use std::collections::{BTreeMap, HashMap};
use std::error;
use std::fmt;
use std::fs;
use std::io::{Error, ErrorKind, Read};
use std::mem;
use std::path::Path;
use std::str::FromStr;

use fraction::ParseRatioError;
use fraction::Ratio;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SongSpecifierIssue {
    IncorrectNumberOfParts,
    InvalidFirstPart,
    InvalidMode,
    InvalidLevelNumber,
}

impl fmt::Display for SongSpecifierIssue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SongSpecifierIssue::IncorrectNumberOfParts => {
                write!(f, "Expected 7 colon-delimited parts")
            }
            SongSpecifierIssue::InvalidFirstPart => write!(f, "Expected first bit to be 'NOTES'"),
            SongSpecifierIssue::InvalidMode => write!(f, "Bad song mode"),
            SongSpecifierIssue::InvalidLevelNumber => write!(f, "Unparsable level number"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StepParseError {
    NonSmFile(String),
    BadOsStr(String),
    UntitledSong,
    UnauthoredSong,
    MissingBpm,
    MissingStepCharts,
    FractionPairParseIssue,
    FractionParseError(ParseRatioError),
    InvalidSongLevelSpecifier(String, SongSpecifierIssue),
    HandsRequired(Ratio<u64>),
    UnparsableLine(Ratio<u64>, String),
    NoteAlreadyPressed(Ratio<u64>, Ratio<u64>),
    UnopennedHoldClose(Ratio<u64>),
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
            StepParseError::MissingStepCharts => write!(f, "Found song without any step charts."),
            StepParseError::FractionPairParseIssue => {
                write!(f, "Can't parse speed changes, invalid pair")
            }
            StepParseError::FractionParseError(fpe) => {
                write!(f, "Can't parse speed changes, invalid fraction: {}", fpe)
            }
            StepParseError::InvalidSongLevelSpecifier(spec, error) => {
                write!(
                    f,
                    "Can't parse song level specifier ({}): \"{}\"",
                    error, spec
                )
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

impl From<ParseRatioError> for StepParseError {
    fn from(item: ParseRatioError) -> Self {
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Duration(Ratio<u64>);

impl<T: Into<Ratio<u64>>> From<T> for Duration {
    fn from(item: T) -> Self {
        Duration(item.into())
    }
}

// TODO: rolls? mines?
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct StepNote {
    pub note: Note,
    pub hold_duration: Duration,
}

impl StepNote {
    pub fn is_doubles(&self) -> bool {
        self.note.is_doubles()
    }

    fn adjust_time(&self, timing: Ratio<u64>) -> Ratio<u64> {
        timing - self.hold_duration.0
    }
}

#[derive(Copy, Clone, Debug, Eq)]
pub enum Step {
    Note(StepNote),
    Chord(StepNote, StepNote),
}

impl PartialEq for Step {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Step::Note(sn) => {
                if let Step::Note(on) = other {
                    sn == on
                } else {
                    false
                }
            }
            Step::Chord(sl, sr) => {
                if let Step::Chord(ol, or) = other {
                    (ol == sl && or == sr) || (ol == sr && or == sl)
                } else {
                    false
                }
            }
        }
    }
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
        .filter_map(|(pos, state)| {
            // On a Galois inclusion:
            // filter_map and then the collect makes this return
            // Option<Option<Vec<_>>>, where the outer option is
            // about whether the value should be included, and the
            // inner one is about whether the outer function should
            // report an error in parsing this line.
            let state_idx = match state.to_digit(10) {
                None => return Some(None),
                Some(x) => {
                    if x == 0 {
                        return None;
                    } else {
                        (x as usize) - 1
                    }
                }
            };
            if pos >= n_notes {
                return Some(None);
            }
            if state_idx >= NOTE_STATES.len() {
                return Some(None);
            }
            Some(Some((NOTE_POSITIONS[pos], NOTE_STATES[state_idx])))
        })
        .collect::<Option<Vec<(Note, LineState)>>>()?;
    if v.len() <= 2 {
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

    fn emplace_into_map(self, timing: Ratio<u64>, steps: &mut BTreeMap<Ratio<u64>, Step>) {
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
    OneHold(Ratio<u64>, Note),
    TwoHolds(Ratio<u64>, Note, Ratio<u64>, Note),
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

    fn note_timing(&self, n: &Note) -> Option<Ratio<u64>> {
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

    fn promote(&mut self, t: Ratio<u64>, n: Note) {
        let new_state = match self {
            HoldState::NoHolds => HoldState::OneHold(t, n),
            HoldState::OneHold(og_t, og_n) => HoldState::TwoHolds(*og_t, *og_n, t, n),
            _ => {
                return;
            }
        };
        mem::replace(self, new_state);
    }

    fn demote(&mut self, n: &Note) -> Option<Ratio<u64>> {
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
        time: Ratio<u64>,
        line: &str,
        is_doubles: bool,
        steps: &mut BTreeMap<Ratio<u64>, Step>,
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
                        hold_duration: 0.into(),
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
                            hold_duration: Duration(time - first_time),
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
        let bits: Vec<_> = key.split(':').map(|b| b.trim()).collect();
        // 7 bits:
        // ["NOTES", "dance-{single|double}", "", "<string of level difficulty>", "{\d+}", "", ""]
        if bits.len() != 7 {
            return Err(StepParseError::InvalidSongLevelSpecifier(
                key.to_string(),
                SongSpecifierIssue::IncorrectNumberOfParts,
            ));
        }
        if bits[0] != "NOTES" {
            return Err(StepParseError::InvalidSongLevelSpecifier(
                key.to_string(),
                SongSpecifierIssue::InvalidFirstPart,
            ));
        }
        let is_doubles = {
            if bits[1] == "dance-single" {
                false
            } else if bits[1] == "dance-double" {
                true
            } else {
                return Err(StepParseError::InvalidSongLevelSpecifier(
                    key.to_string(),
                    SongSpecifierIssue::InvalidMode,
                ));
            }
        };
        let lvl: u8 = bits[4].parse().map_err(|_| {
            StepParseError::InvalidSongLevelSpecifier(
                key.to_string(),
                SongSpecifierIssue::InvalidLevelNumber,
            )
        })?;
        if is_doubles {
            Ok(SongLevel::Double(lvl))
        } else {
            Ok(SongLevel::Single(lvl))
        }
    }
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct StepChart {
    pub title: String,
    pub artist: String,
    pub bpms: BTreeMap<Ratio<u64>, Ratio<u64>>,
    pub stops: BTreeMap<Ratio<u64>, Ratio<u64>>,
    pub maps: HashMap<SongLevel, BTreeMap<Ratio<u64>, Step>>,
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

fn to_fraction_map(val: &str) -> Result<BTreeMap<Ratio<u64>, Ratio<u64>>, StepParseError> {
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
) -> Result<BTreeMap<Ratio<u64>, Step>, StepParseError> {
    let mut steps = BTreeMap::new();
    let mut hold_state = HoldState::new();
    let mut dist: Ratio<u64> = 0.into();
    for measure in steps_str
        .split(',')
        .map(|blk| blk.trim().split('\n').collect::<Vec<_>>())
    {
        let line_rate = Ratio::new(4u64, measure.len() as u64);
        for (measure_idx, line) in measure.into_iter().enumerate() {
            let timing = line_rate * Ratio::new(measure_idx as u64, 1) + dist;
            if let Some(err) =
                hold_state.update_holds(timing, line, song_level.is_doubles(), &mut steps)
            {
                return Err(err);
            }
        }
        dist += Ratio::new(4u64, 1u64);
    }
    Ok(steps)
}

fn parse_maps(
    notes: &BTreeMap<&str, &str>,
) -> Result<HashMap<SongLevel, BTreeMap<Ratio<u64>, Step>>, StepParseError> {
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
        let parts: BTreeMap<&str, &str> = s
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
        if final_chart.maps.is_empty() {
            Err(StepParseError::MissingStepCharts)
        } else {
            Ok(final_chart)
        }
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
        assert_eq!(
            parsed_2.unwrap_err(),
            StepParseError::FractionPairParseIssue
        );
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

    #[test]
    fn test_bad_song_levels_colons() {
        let fewer_file = "
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
#BPMS:0=182;

#NOTES:
     dance-single:
     Beginner:
     5:
     :
0000
0000
0000
0000
; ";
        let fewer_parsed = fewer_file.parse::<StepChart>();
        assert_eq!(
            fewer_parsed.unwrap_err(),
            StepParseError::InvalidSongLevelSpecifier(
                "NOTES:
     dance-single:
     Beginner:
     5:
     :"
                .to_string(),
                SongSpecifierIssue::IncorrectNumberOfParts
            )
        );

        let more_file = "
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
#BPMS:0=182;

#NOTES:
     dance-single:
     ::
     Beginner:
     5:
     :
0000
0000
0000
0000
; ";
        let more_parsed = more_file.parse::<StepChart>();
        assert_eq!(
            more_parsed.unwrap_err(),
            StepParseError::InvalidSongLevelSpecifier(
                "NOTES:
     dance-single:
     ::
     Beginner:
     5:
     :"
                .to_string(),
                SongSpecifierIssue::IncorrectNumberOfParts
            )
        );
    }

    #[test]
    fn test_bad_song_levels_bits() {
        let fewer_file = "
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
#BPMS:0=182;

#NOTES:
     dance-triple:
     :
     Beginner:
     5:
     :
0000
0000
0000
0000
; ";
        let fewer_parsed = fewer_file.parse::<StepChart>();
        assert_eq!(
            fewer_parsed.unwrap_err(),
            StepParseError::InvalidSongLevelSpecifier(
                "NOTES:
     dance-triple:
     :
     Beginner:
     5:
     :"
                .to_string(),
                SongSpecifierIssue::InvalidMode
            )
        );

        let more_file = "
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
#BPMS:0=182;

#NOTES:
     dance-single:
     :
     Beginner:
     dog:
     :
0000
0000
0000
0000
; ";
        let more_parsed = more_file.parse::<StepChart>();
        assert_eq!(
            more_parsed.unwrap_err(),
            StepParseError::InvalidSongLevelSpecifier(
                "NOTES:
     dance-single:
     :
     Beginner:
     dog:
     :"
                .to_string(),
                SongSpecifierIssue::InvalidLevelNumber
            )
        );
    }

    #[test]
    fn test_sucessful_header_parse() {
        let fewer_file = "
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
; ";
        let fewer_parsed = fewer_file.parse::<StepChart>();
        assert_eq!(
            fewer_parsed.unwrap(),
            StepChart {
                title: "プレインエイジア -PHQ remix-".to_string(),
                artist: "PHQUASE".to_string(),
                bpms: BTreeMap::from([(0.into(), 182.into())]),
                stops: BTreeMap::new(),
                maps: HashMap::from([(SongLevel::Single(5), BTreeMap::new())])
            }
        );
    }

    #[test]
    fn test_nested_holds() {
        let notes = "
2000
0000
0020
0030
,
0002
0003
0020
3030
,
0002
0000
0020
0030
,
2000
3000
0020
0033
,
0200
0000
0000
0001
0301
0000
0000
0000";
        let parsed = parse_steps(notes, &SongLevel::Single(9));
        assert_eq!(
            parsed.unwrap(),
            BTreeMap::from([(
                0.into(),
                Step::Note(StepNote {
                    note: Note::Left,
                    hold_duration: 7.into()
                })
            ),(
                2.into(),
                Step::Note(StepNote {
                    note: Note::Up,
                    hold_duration: 1.into()
                })
            ),(
                4.into(),
                Step::Note(StepNote {
                    note: Note::Right,
                    hold_duration: 1.into()
                })
            ),(
                6.into(),
                Step::Note(StepNote {
                    note: Note::Up,
                    hold_duration: 1.into()
                })
            ),(
                8.into(),
                Step::Note(StepNote {
                    note: Note::Right,
                    hold_duration: 7.into()
                })
            ),(
                10.into(),
                Step::Note(StepNote {
                    note: Note::Up,
                    hold_duration: 1.into()
                })
            ),(
                12.into(),
                Step::Note(StepNote {
                    note: Note::Left,
                    hold_duration: 1.into()
                })
            ),(
                14.into(),
                Step::Note(StepNote {
                    note: Note::Up,
                    hold_duration: 1.into()
                })
            ),(
                16.into(),
                Step::Note(StepNote {
                    note: Note::Down,
                    hold_duration: 2.into()
                })
            ),(
                Ratio::new(35, 2),
                Step::Note(StepNote {
                    note: Note::Right,
                    hold_duration: 0.into()
                })
            ),(
                18.into(),
                Step::Note(StepNote {
                    note: Note::Right,
                    hold_duration: 0.into()
                })
            )])
        );
    }
}
