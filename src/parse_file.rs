use std::collections::HashMap;
use std::error;
use std::fmt;
use std::fs;
use std::io::{Error, ErrorKind, Read};
use std::path::Path;
use std::str::FromStr;

use fraction::error::ParseError;
use fraction::Fraction;

#[derive(Debug, Clone)]
pub enum StepParseError {
    NonSmFile(String),
    BadOsStr(String),
    UntitledSong,
    UnauthoredSong,
    FractionPairParseIssue,
    FractionParseError(ParseError),
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
            StepParseError::FractionPairParseIssue => {
                write!(f, "Can't parse speed changes, invalid pair")
            }
            StepParseError::FractionParseError(fpe) => {
                write!(f, "Can't parse speed changes, invalid fraction: {}", fpe)
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

pub struct Duration(Fraction);

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

pub enum StepNotes {
    Single(Note),
    Chord(Note, Note),
}

pub struct Step {
    pub steps: StepNotes,
    pub hold_duration: Option<Duration>,
}

#[derive(Eq, PartialEq, Hash)]
pub enum SongLevel {
    Single(u8),
    Double(u8),
}

#[derive(Default)]
pub struct StepChart {
    pub title: String,
    pub artist: String,
    pub bpms: HashMap<Fraction, Fraction>,
    pub stops: HashMap<Fraction, Fraction>,
    pub maps: HashMap<SongLevel, Vec<Step>>,
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
            Ok((pair[..delim].parse()?, pair[delim..].parse()?))
        })
        .collect()
}

fn parse_maps(notes: &HashMap<&str, &str>) -> HashMap<SongLevel, Vec<Step>> {
    unimplemented!();
}

impl FromStr for StepChart {
    type Err = StepParseError;

    fn from_str(s: &str) -> Result<Self, StepParseError> {
        let parts: HashMap<&str, &str> = s
            .split('#')
            .filter_map(|ln| {
                let kv = &ln[..ln.find(';')?];
                let last_colon = kv.rfind(':')?;
                Some(kv.split_at(last_colon + 1))
            })
            .collect();
        let mut final_chart: StepChart = Default::default();
        final_chart.artist = parts
            .get("ARTIST:")
            .ok_or(StepParseError::UnauthoredSong)?
            .to_string();
        final_chart.title = parts
            .get("TITLE:")
            .ok_or(StepParseError::UntitledSong)?
            .to_string();
        if let Some(&bpm_str) = parts.get("BPMS:") {
            final_chart.bpms = to_fraction_map(bpm_str)?;
        }
        if let Some(&stop_str) = parts.get("STOPS:") {
            final_chart.stops = to_fraction_map(stop_str)?;
        }
        Ok(final_chart)
    }
}

pub struct SongPack {
    pub name: String,
    pub songs: Vec<StepChart>,
}
