use super::general::*;
use crate::filedb::*;

#[derive(Debug)]
pub struct Error {
    pub info: &'static str,
    pub message: Option<String>,
    pub loc: CodeLoc,
    pub file: u32,
}

impl Error {
    pub fn render(&self, files: &FileDb) -> String {
        use crate::util::term::*;
        use crate::util::*;

        let mut out = StringWriter::new();

        Diagnostic::new()
            .with_labels(vec![Label::new(self.file, self.loc)])
            .with_message(self.info.to_string())
            .with_notes(self.message.clone().map(|a| vec![a]).unwrap_or(Vec::new()))
            .render(files, &mut out)
            .unwrap();

        return out.into_string();
    }
}
