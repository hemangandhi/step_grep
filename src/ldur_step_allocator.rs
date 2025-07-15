use std::collections::BTreeMap;

use fraction::Ratio;

use crate::parse_file;

pub struct StepAllocation {
    pub left_note: Option<parse_file::Note>,
    pub right_note: Option<parse_file::Note>,
}

impl std::fmt::Display for StepAllocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(l) = self.left_note {
            write!(f, "{}", l.get_char())?;
        }
        if let Some(r) = self.right_note {
            write!(f, "{}", r.get_char().to_ascii_uppercase())?;
        }
        Ok(())
    }
}

pub struct SongStepAllocation(BTreeMap<Ratio<u64>, StepAllocation>);

impl std::fmt::Display for SongStepAllocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for allocation in self.0.values() {
            write!(f, "{}", allocation)?;
        }
        Ok(())
    }
}
