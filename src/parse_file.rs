use std::collections::HashMap;
use std::error;
use std::fmt;
use std::fs;
use std::io::{Error, ErrorKind, Read};
use std::mem;
use std::path::Path;
use std::str::FromStr;

use fraction::error::ParseError;
use fraction::Fraction;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StepParseError {
    NonSmFile(String),
    BadOsStr(String),
    UntitledSong,
    UnauthoredSong,
    MissingBpm,
    FractionPairParseIssue,
    FractionParseError(ParseError),
    InvalidSongLevelSpecifier(String),
    HandsRequired(Fraction),
    UnparsableLine(Fraction, String),
    NoteAlreadyPressed(Fraction, Fraction),
    UnopennedHoldClose(Fraction),
}

impl fmt::Display for StepParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StepParseError::NonSmFile(p) => write!(f, "Path isn't an sm file. Path: {}", p),
            StepParseError::BadOsStr(s) => {
                write!(f, "Issues in parsing your file paths. Roughly got: {}", s)
            }
            StepParseError::UntitledSong => write!(f, "Found song without title"),
            StepParseError::UnauthoredSong => write!(f, "Found song without author"),
            StepParseError::MissingBpm => write!(f, "Found song without BPM"),
            StepParseError::FractionPairParseIssue => {
                write!(f, "Can't parse speed changes, invalid pair")
            }
            StepParseError::FractionParseError(fpe) => {
                write!(f, "Can't parse speed changes, invalid fraction: {}", fpe)
            }
            StepParseError::InvalidSongLevelSpecifier(spec) => {
                write!(f, "Can't parse song level specifier: \"{}\"", spec)
            }
            StepParseError::HandsRequired(beat) => {
                write!(f, "Hands required at beat {}", beat)
            }
            StepParseError::UnparsableLine(beat, line) => {
                write!(f, "Beat {} unparsable: \"{}\"", beat, line)
            }
            StepParseError::NoteAlreadyPressed(first_time, second_time) => {
                write!(
                    f,
                    "Note simultaneously: at {} and {}",
                    first_time, second_time
                )
            }
            StepParseError::UnopennedHoldClose(time) => {
                write!(f, "Unopenned hold closed at {}", time)
            }
        }
    }
}

impl error::Error for StepParseError {}

impl From<StepParseError> for Error {
    fn from(item: StepParseError) -> Self {
        Error::new(ErrorKind::Other, item)
    }
}

impl From<ParseError> for StepParseError {
    fn from(item: ParseError) -> Self {
        StepParseError::FractionParseError(item)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub enum Note {
    Up,
    Down,
    Left,
    Right,
    RPadUp,
    RPadDown,
    RPadLeft,
    RPadRight,
}

impl Note {
    pub fn is_doubles(&self) -> bool {
        match self {
            Note::Up | Note::Down | Note::Left | Note::Right => false,
            _ => true,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Duration(Fraction);

// TODO: rolls? mines?
#[derive(Copy, Clone, Debug)]
pub struct StepNote {
    pub note: Note,
    pub hold_duration: Option<Duration>,
}

impl StepNote {
    pub fn is_doubles(&self) -> bool {
        self.note.is_doubles()
    }

    fn adjust_time(&self, timing: Fraction) -> Fraction {
        timing - self.hold_duration.map(|d| d.0).unwrap_or(0.into())
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Step {
    Note(StepNote),
    Chord(StepNote, StepNote),
}

#[derive(Copy, Clone)]
enum LineState {
    Note,
    HoldStart,
    HoldEnd, // TODO: rolls? mines?
}

fn line_state_of_line(s: &str, is_doubles: bool) -> Option<Vec<(Note, LineState)>> {
    const NOTE_POSITIONS: [Note; 8] = [
        Note::Left,
        Note::Down,
        Note::Up,
        Note::Right,
        Note::RPadLeft,
        Note::RPadDown,
        Note::RPadUp,
        Note::RPadRight,
    ];
    const NOTE_STATES: [LineState; 3] = [LineState::Note, LineState::HoldStart, LineState::HoldEnd];

    let n_notes = if is_doubles { NOTE_POSITIONS.len() } else { 4 };
    let v = s
        .chars()
        .enumerate()
        .map(|(pos, state)| {
            let state_idx = (state.to_digit(10)? as usize) - 1;
            if pos >= n_notes {
                return None;
            }
            if state_idx >= NOTE_STATES.len() {
                return None;
            }
            Some((NOTE_POSITIONS[pos], NOTE_STATES[state_idx]))
        })
        .collect::<Option<Vec<(Note, LineState)>>>()?;
    if v.len() == 2 {
        Some(v)
    } else {
        None
    }
}

impl Step {
    pub fn is_doubles(&self) -> bool {
        match self {
            Step::Note(n) => n.is_doubles(),
            Step::Chord(l, r) => l.is_doubles() || r.is_doubles(),
        }
    }

    fn merge_into_chord(self, other: Step) -> Option<Step> {
        if let Step::Note(n1) = self {
            if let Step::Note(n2) = other {
                return Some(Step::Chord(n1, n2));
            }
        }
        None
    }

    fn emplace_into_map(self, timing: Fraction, steps: &mut HashMap<Fraction, Step>) {
        let key = match self {
            Step::Note(n) => n.adjust_time(timing),
            Step::Chord(l, r) => {
                let l_timing = l.adjust_time(timing);
                let r_timing = r.adjust_time(timing);
                // Take the lesser of the two since the point is that every step
                // will properly be updated by read_steps_in_line and a step will only
                // be returned from theronly when both notes are released, by which point the
                // shorter hold may already have been released.
                if l_timing < r_timing {
                    l_timing
                } else {
                    r_timing
                }
            }
        };

        steps
            .entry(key)
            .and_modify(|to_chord| {
                if let Some(ch) = self.merge_into_chord(*to_chord) {
                    mem::replace(to_chord, ch);
                }
            })
            .or_insert(self);
    }
}

enum HoldState {
    NoHolds,
    OneHold(Fraction, Note),
    TwoHolds(Fraction, Note, Fraction, Note),
}

impl HoldState {
    fn new() -> HoldState {
        HoldState::NoHolds
    }

    fn magnitude(&self) -> u8 {
        match self {
            HoldState::NoHolds => 0,
            HoldState::OneHold(_, _) => 1,
            HoldState::TwoHolds(_, _, _, _) => 2,
        }
    }

    fn note_timing(&self, n: &Note) -> Option<Fraction> {
        match self {
            HoldState::NoHolds => None,
            HoldState::OneHold(t, held_n) => {
                if n == held_n {
                    Some(*t)
                } else {
                    None
                }
            }
            HoldState::TwoHolds(t1, held_n1, t2, held_n2) => {
                if n == held_n1 {
                    Some(*t1)
                } else if n == held_n2 {
                    Some(*t2)
                } else {
                    None
                }
            }
        }
    }

    fn promote(&mut self, t: Fraction, n: Note) {
        let new_state = match self {
            HoldState::NoHolds => HoldState::OneHold(t, n),
            HoldState::OneHold(og_t, og_n) => HoldState::TwoHolds(*og_t, *og_n, t, n),
            _ => {
                return;
            }
        };
        mem::replace(self, new_state);
    }

    fn demote(&mut self, n: &Note) -> Option<Fraction> {
        let (new_state, fraction) = match self {
            HoldState::OneHold(og_t, og_n) => {
                if og_n == n {
                    (HoldState::NoHolds, *og_t)
                } else {
                    return None;
                }
            }
            HoldState::TwoHolds(t1, n1, t2, n2) => {
                if n1 == n {
                    (HoldState::OneHold(*t2, *n2), *t1)
                } else if n2 == n {
                    (HoldState::OneHold(*t1, *n1), *t2)
                } else {
                    return None;
                }
            }
            _ => {
                return None;
            }
        };
        mem::replace(self, new_state);
        Some(fraction)
    }

    fn update_holds(
        &mut self,
        time: Fraction,
        line: &str,
        is_doubles: bool,
        steps: &mut HashMap<Fraction, Step>,
    ) -> Option<StepParseError> {
        let state = line_state_of_line(line, is_doubles);
        if state.is_none() {
            return Some(StepParseError::UnparsableLine(time, line.to_string()));
        }
        for (note, state) in state.unwrap() {
            match state {
                LineState::Note => {
                    if let Some(first_time) = self.note_timing(&note) {
                        return Some(StepParseError::NoteAlreadyPressed(first_time, time));
                    }
                    if self.magnitude() > 1 {
                        return Some(StepParseError::HandsRequired(time));
                    }
                    Step::Note(StepNote {
                        note,
                        hold_duration: None,
                    })
                    .emplace_into_map(time, steps);
                }
                LineState::HoldStart => {
                    if let Some(first_time) = self.note_timing(&note) {
                        return Some(StepParseError::NoteAlreadyPressed(first_time, time));
                    }
                    if self.magnitude() > 1 {
                        return Some(StepParseError::HandsRequired(time));
                    }
                    self.promote(time, note);
                }
                LineState::HoldEnd => {
                    if let Some(first_time) = self.demote(&note) {
                        Step::Note(StepNote {
                            note,
                            hold_duration: Some(Duration(time - first_time)),
                        })
                        .emplace_into_map(time, steps);
                    } else {
                        return Some(StepParseError::UnopennedHoldClose(time));
                    }
                }
            }
        }
        None
    }
}

#[derive(Eq, PartialEq, Hash, Debug)]
pub enum SongLevel {
    Single(u8),
    Double(u8),
}

impl SongLevel {
    pub fn is_doubles(&self) -> bool {
        match self {
            SongLevel::Single(_) => false,
            SongLevel::Double(_) => true,
        }
    }

    // We don't implement the trait since we do not actually want to use this as a standard format.
    fn from_str(key: &str) -> Result<Self, StepParseError> {
        let bits: Vec<_> = key.split(':').collect();
        if bits.len() != 6 || bits[0] == "NOTES" {
            return Err(StepParseError::InvalidSongLevelSpecifier(key.to_string()));
        }
        let is_doubles = {
            if bits[1] == "dance-single" {
                false
            } else if bits[1] == "dance-double" {
                true
            } else {
                return Err(StepParseError::InvalidSongLevelSpecifier(key.to_string()));
            }
        };
        let lvl: u8 = bits[4]
            .parse()
            .map_err(|_| StepParseError::InvalidSongLevelSpecifier(key.to_string()))?;
        if is_doubles {
            Ok(SongLevel::Double(lvl))
        } else {
            Ok(SongLevel::Single(lvl))
        }
    }
}

#[derive(Default, Debug)]
pub struct StepChart {
    pub title: String,
    pub artist: String,
    pub bpms: HashMap<Fraction, Fraction>,
    pub stops: HashMap<Fraction, Fraction>,
    pub maps: HashMap<SongLevel, HashMap<Fraction, Step>>,
}

fn GetFilePath(path: &Path) -> Result<String, StepParseError> {
    if let Some(name_os_str) = path.file_name() {
        if let Some(name) = name_os_str.to_str() {
            Result::Ok(name.to_string())
        } else {
            Result::Err(StepParseError::BadOsStr(
                name_os_str.to_string_lossy().into_owned(),
            ))
        }
    } else {
        Result::Ok(String::from(""))
    }
}

impl StepChart {
    pub fn from_file(path: &Path) -> Result<StepChart, Error> {
        if path.extension().map(|e| *e != *".sm").unwrap_or(true) {
            return Result::Err(StepParseError::NonSmFile(path.display().to_string()).into());
        }
        let mut contents = String::new();
        fs::File::open(&path)?.read_to_string(&mut contents)?;
        Result::Ok(contents.parse()?)
    }
}

fn to_fraction_map(val: &str) -> Result<HashMap<Fraction, Fraction>, StepParseError> {
    val.split(',')
        .map(|pair| {
            let delim = pair
                .find('=')
                .ok_or(StepParseError::FractionPairParseIssue)?;
            Ok((pair[..delim].parse()?, pair[(delim + 1)..].parse()?))
        })
        .collect()
}

fn parse_steps(
    steps_str: &str,
    song_level: &SongLevel,
) -> Result<HashMap<Fraction, Step>, StepParseError> {
    let mut steps = HashMap::new();
    let mut hold_state = HoldState::new();
    let mut dist: Fraction = 0.into();
    for measure in steps_str
        .split(',')
        .map(|blk| blk.trim().split('\n').collect::<Vec<_>>())
    {
        let line_rate = Fraction::new(4u64, measure.len() as u64);
        for (measure_idx, line) in measure.into_iter().enumerate() {
            let timing = line_rate * (measure_idx as u64).into() + dist;
            if let Some(err) =
                hold_state.update_holds(timing, line, song_level.is_doubles(), &mut steps)
            {
                return Err(err);
            }
        }
        dist += Fraction::new(4u64, 1u64);
    }
    Ok(steps)
}

fn parse_maps(
    notes: &HashMap<&str, &str>,
) -> Result<HashMap<SongLevel, HashMap<Fraction, Step>>, StepParseError> {
    notes
        .into_iter()
        .filter(|(&k, &v_)| k.starts_with("NOTES"))
        .map(|(k, v)| {
            let lvl = SongLevel::from_str(k)?;
            let steps = parse_steps(v, &lvl)?;
            Ok((lvl, steps))
        })
        .collect()
}

impl FromStr for StepChart {
    type Err = StepParseError;

    fn from_str(s: &str) -> Result<Self, StepParseError> {
        let parts: HashMap<&str, &str> = s
            .split('#')
            .filter_map(|ln| {
                // TODO: detect the comment line at the top of the file
                // so that this can actually produce an error if these
                // tokens are missing.
                let kv = &ln[..ln.find(';')?];
                let last_colon = kv.rfind(':')?;
                Some(kv.split_at(last_colon + 1))
            })
            .collect();
        let mut final_chart: StepChart = Default::default();
        final_chart.title = parts
            .get("TITLE:")
            .ok_or(StepParseError::UntitledSong)?
            .to_string();
        final_chart.artist = parts
            .get("ARTIST:")
            .ok_or(StepParseError::UnauthoredSong)?
            .to_string();
        final_chart.bpms = to_fraction_map(parts.get("BPMS:").ok_or(StepParseError::MissingBpm)?)?;
        if final_chart.bpms.is_empty() {
            return Err(StepParseError::MissingBpm);
        }
        if let Some(&stop_str) = parts.get("STOPS:") {
            final_chart.stops = to_fraction_map(stop_str)?;
        }
        final_chart.maps = parse_maps(&parts)?;
        Ok(final_chart)
    }
}

pub struct SongPack {
    pub name: String,
    pub songs: Vec<StepChart>,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lacking_title() {
        let file = "
//----- song ID: pure -----//
#ARTIST:PHQUASE;
#TITLETRANSLIT:Plain Asia -PHQ remix-;
#BANNER:Plain Asia -PHQ remix-.png;
#BACKGROUND:Plain Asia -PHQ remix--bg.png;
#CDTITLE:./CDTitles/BEMANI x Toho Project Ultimate MasterPieces (2015).png;
#MUSIC:Plain Asia -PHQ remix-.ogg;
#SAMPLESTART:34.935;
#SAMPLELENGTH:15;
#BPMS:0=182;

#NOTES:
     dance-single:
     :
     Beginner:
     5:
     :
0000
0000
0000
0000
";
        let parsed = file.parse::<StepChart>();
        assert_eq!(parsed.unwrap_err(), StepParseError::UntitledSong);
    }

    #[test]
    fn test_lacking_artist() {
        let file = "
//----- song ID: pure -----//
#TITLE:プレインエイジア -PHQ remix-;
#TITLETRANSLIT:Plain Asia -PHQ remix-;
#BANNER:Plain Asia -PHQ remix-.png;
#BACKGROUND:Plain Asia -PHQ remix--bg.png;
#CDTITLE:./CDTitles/BEMANI x Toho Project Ultimate MasterPieces (2015).png;
#MUSIC:Plain Asia -PHQ remix-.ogg;
#SAMPLESTART:34.935;
#SAMPLELENGTH:15;
#BPMS:0=182;

#NOTES:
     dance-single:
     :
     Beginner:
     5:
     :
0000
0000
0000
0000
";
        let parsed = file.parse::<StepChart>();
        assert_eq!(parsed.unwrap_err(), StepParseError::UnauthoredSong);
    }

    #[test]
    fn test_lacking_bpm() {
        let file_1 = "
//----- song ID: pure -----//
#ARTIST:PHQUASE;
#TITLE:プレインエイジア -PHQ remix-;
#TITLETRANSLIT:Plain Asia -PHQ remix-;
#BANNER:Plain Asia -PHQ remix-.png;
#BACKGROUND:Plain Asia -PHQ remix--bg.png;
#CDTITLE:./CDTitles/BEMANI x Toho Project Ultimate MasterPieces (2015).png;
#MUSIC:Plain Asia -PHQ remix-.ogg;
#SAMPLESTART:34.935;
#SAMPLELENGTH:15;

#NOTES:
     dance-single:
     :
     Beginner:
     5:
     :
0000
0000
0000
0000
";
        let parsed_1 = file_1.parse::<StepChart>();
        assert_eq!(parsed_1.unwrap_err(), StepParseError::MissingBpm);
        let file_2 = "
//----- song ID: pure -----//
#ARTIST:PHQUASE;
#TITLE:プレインエイジア -PHQ remix-;
#TITLETRANSLIT:Plain Asia -PHQ remix-;
#BANNER:Plain Asia -PHQ remix-.png;
#BACKGROUND:Plain Asia -PHQ remix--bg.png;
#CDTITLE:./CDTitles/BEMANI x Toho Project Ultimate MasterPieces (2015).png;
#MUSIC:Plain Asia -PHQ remix-.ogg;
#SAMPLESTART:34.935;
#SAMPLELENGTH:15;
#BPMS:;

#NOTES:
     dance-single:
     :
     Beginner:
     5:
     :
0000
0000
0000
0000
";
        let parsed_2 = file_2.parse::<StepChart>();
        assert_eq!(parsed_2.unwrap_err(), StepParseError::FractionPairParseIssue);
    }

    #[test]
    fn test_bad_fraction_pair_cases() {
        let bpm_file = "
//----- song ID: pure -----//
#ARTIST:PHQUASE;
#TITLE:プレインエイジア -PHQ remix-;
#TITLETRANSLIT:Plain Asia -PHQ remix-;
#BANNER:Plain Asia -PHQ remix-.png;
#BACKGROUND:Plain Asia -PHQ remix--bg.png;
#CDTITLE:./CDTitles/BEMANI x Toho Project Ultimate MasterPieces (2015).png;
#MUSIC:Plain Asia -PHQ remix-.ogg;
#SAMPLESTART:34.935;
#SAMPLELENGTH:15;
#BPMS:0$182;

#NOTES:
     dance-single:
     :
     Beginner:
     5:
     :
0000
0000
0000
0000
";
        let bpm_parsed = bpm_file.parse::<StepChart>();
        assert_eq!(
            bpm_parsed.unwrap_err(),
            StepParseError::FractionPairParseIssue
        );

        let stops_file = "
//----- song ID: algo -----//
#TITLE:ALGORITHM;
#ARTIST:SOUND HOLIC feat. Nana Takahashi;
#BANNER:ALGORITHM.png;
#BACKGROUND:ALGORITHM-bg.png;
#CDTITLE:./CDTitles/DDR A.png;
#MUSIC:ALGORITHM.ogg;
#SAMPLESTART:67.833;
#SAMPLELENGTH:15;
#BPMS:0=130;
#STOPS:218$0.231,218.5$0.231;

#NOTES:
     dance-single:
     :
     Beginner:
     5:
     :
0000
0000
0000
0000
";
        let stops_parsed = stops_file.parse::<StepChart>();
        assert_eq!(
            stops_parsed.unwrap_err(),
            StepParseError::FractionPairParseIssue
        );
    }
}
