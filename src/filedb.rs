use crate::buckets::*;
use crate::util::*;
use codespan_reporting::files::{Error, Files};
use core::ops::Range;
use core::{fmt, str};

/// The column index at the given byte index in the source file.
/// This is the number of characters to the given byte index.
///
/// If the byte index is smaller than the start of the line, then `0` is returned.
/// If the byte index is past the end of the line, the column index of the last
/// character `+ 1` is returned.
pub fn column_index(source: &str, line_range: Range<usize>, byte_index: usize) -> usize {
    let end_index = core::cmp::min(byte_index, core::cmp::min(line_range.end, source.len()));

    (line_range.start..end_index)
        .filter(|byte_index| source.is_char_boundary(byte_index + 1))
        .count()
}

pub fn line_starts<'source>(source: &'source str) -> impl 'source + Iterator<Item = usize> {
    core::iter::once(0).chain(source.match_indices('\n').map(|(i, _)| i + 1))
}

#[derive(Debug, Clone, Copy)]
pub struct File<'a> {
    pub name: &'a str,
    /// The source code of the file.
    pub source: &'a str,
    /// The starting byte indices in the source code.
    pub line_starts: &'a [usize],
}

impl<'a> File<'a> {
    pub fn new(buckets: impl Allocator<'a>, name: &str, source: &str) -> Self {
        let line_starts: Vec<usize> = line_starts(source).collect();
        File {
            name: buckets.add_str(name),
            source: buckets.add_str(source),
            line_starts: buckets.add_array(line_starts),
        }
    }

    pub fn new_static(
        buckets: impl Allocator<'static>,
        name: &'static str,
        source: &'static str,
    ) -> Self {
        let line_starts: Vec<usize> = line_starts(source).collect();
        File {
            name,
            source,
            line_starts: buckets.add_array(line_starts),
        }
    }

    fn line_index(&self, byte_index: usize) -> Option<usize> {
        match self.line_starts.binary_search(&byte_index) {
            Ok(line) => Some(line),
            Err(next_line) => Some(next_line - 1),
        }
    }

    fn line_start(&self, line_index: usize) -> Result<usize, Error> {
        use core::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => Ok(self
                .line_starts
                .get(line_index)
                .expect("failed despite previous check")
                .clone()),
            Ordering::Equal => Ok(self.source.len()),
            Ordering::Greater => Err(Error::LineTooLarge {
                given: line_index,
                max: self.line_starts.len() - 1,
            }),
        }
    }
}

pub const NO_SYMBOL: u32 = !0;

pub struct FileDb {
    pub buckets: BucketListFactory,
    pub names: HashMap<&'static str, u32>,
    pub files: Vec<File<'static>>,
}

impl Drop for FileDb {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

impl FileDb {
    #[inline]
    pub fn new() -> Self {
        Self {
            buckets: BucketListFactory::new(),
            files: Vec::new(),
            names: map(),
        }
    }

    pub fn file_ids(&self) -> core::ops::Range<u32> {
        let len = self.files.len() as u32;
        return 0..len;
    }

    fn get(&self, file_id: u32) -> Result<&File<'static>, Error> {
        return self.files.get(file_id as usize).ok_or(Error::FileMissing);
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again. Errors if the file already exists in the database.
    pub fn add(&mut self, file_name: &str, source: &str) -> Result<u32, &'static str> {
        if let Some(id) = self.names.get(file_name) {
            return Err("already exists");
        }

        let file_id = self.files.len() as u32;
        let file = File::new(&*self.buckets, file_name, &source);
        self.files.push(file);
        self.names.insert(file.name, file_id);

        Ok(file_id)
    }

    pub fn display_loc(
        &self,
        out: &mut impl fmt::Write,
        loc: Range<u32>,
        file: u32,
    ) -> fmt::Result {
        let file = self.files[file as usize];
        let start_line = file.line_index(loc.start as usize).unwrap();
        let end_line = file.line_index(loc.end as usize).unwrap();

        let start = file.line_start(start_line).unwrap();
        let end = file.line_start(end_line + 1).unwrap();
        let bytes = &file.source.as_bytes()[start..end];

        return write!(out, "{}", unsafe { str::from_utf8_unchecked(bytes) });
    }

    pub fn write_loc(&self, out: &mut impl fmt::Write, loc: Range<u32>, file: u32) -> fmt::Result {
        let file = self.files[file as usize];
        let line = file.line_index(loc.start as usize).unwrap() + 1;
        return write!(out, "{}:{}", file.name, line);
    }

    pub fn loc_to_string(&self, loc: Range<u32>, file: u32) -> String {
        let mut out = StringWriter::new();
        self.write_loc(&mut out, loc, file).unwrap();
        return out.into_string();
    }

    pub fn resolve_include(&self, include: &str, file: u32) -> Result<u32, &'static str> {
        if !include.starts_with("/") {
            let or_else = || -> &'static str { "not found" };
            let mut path =
                parent_if_file(self.files.get(file as usize).ok_or_else(or_else)?.name).to_string();
            if !path.ends_with("/") && path != "" {
                path.push_str("/");
            }
            path.push_str(include);

            if let Some(id) = self.names.get(&*path) {
                return Ok(*id);
            }

            return Err("not found");
        }

        if let Some(id) = self.names.get(include) {
            return Ok(*id);
        }

        return Err("not found");
    }

    pub fn resolve_system_include(&self, include: &str) -> Result<u32, &'static str> {
        if let Some(id) = self.names.get(include) {
            return Ok(*id);
        }

        return Err("not found");
    }
}

impl<'a> Files<'a> for FileDb {
    type FileId = u32;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&'a self, file_id: u32) -> Result<&'a str, Error> {
        Ok(self.get(file_id)?.name)
    }

    fn source(&'a self, file_id: u32) -> Result<&'a str, Error> {
        Ok(self.get(file_id)?.source)
    }

    fn line_index(&self, file_id: u32, byte_index: usize) -> Result<usize, Error> {
        self.get(file_id)?
            .line_starts
            .binary_search(&byte_index)
            .or_else(|next_line| Ok(next_line - 1))
    }

    fn line_range(&self, file_id: u32, line_index: usize) -> Result<Range<usize>, Error> {
        let file = self.get(file_id)?;
        let line_start = file.line_start(line_index)?;
        let next_line_start = file.line_start(line_index + 1)?;

        Ok(line_start..next_line_start)
    }
}

#[cfg(not(target_os = "windows"))]
const PATH_SEP: u8 = b'/';
#[cfg(target_os = "windows")]
const PATH_SEP: u8 = b'\\';

pub fn parent_if_file<'a>(path: &'a str) -> &'a str {
    let bytes = path.as_bytes();
    let mut idx = bytes.len() - 1;
    while bytes[idx] != PATH_SEP {
        if idx == 0 {
            return ""; // idk man this works
        }
        idx -= 1;
    }

    unsafe { str::from_utf8_unchecked(&bytes[..(idx + 1)]) }
}

// https://github.com/danreeves/path-clean/blob/master/src/lib.rs
pub fn path_clean(path: &str) -> String {
    let out = clean_internal(path.as_bytes());
    unsafe { String::from_utf8_unchecked(out) }
}

// https://github.com/danreeves/path-clean/blob/master/src/lib.rs
fn clean_internal(path: &[u8]) -> Vec<u8> {
    static DOT: u8 = b'.';

    if path.is_empty() {
        return vec![DOT];
    }

    let rooted = path[0] == PATH_SEP;
    let n = path.len();

    // Invariants:
    //  - reading from path; r is index of next byte to process.
    //  - dotdot is index in out where .. must stop, either because it is the
    //    leading slash or it is a leading ../../.. prefix.
    //
    // The go code this function is based on handles already-clean paths without
    // an allocation, but I haven't done that here because I think it
    // complicates the return signature too much.
    let mut out: Vec<u8> = Vec::with_capacity(n);
    let mut r = 0;
    let mut dotdot = 0;

    if rooted {
        out.push(PATH_SEP);
        r = 1;
        dotdot = 1
    }

    while r < n {
        if path[r] == PATH_SEP || path[r] == DOT && (r + 1 == n || path[r + 1] == PATH_SEP) {
            // empty path element || . element: skip
            r += 1;
        } else if path[r] == DOT && path[r + 1] == DOT && (r + 2 == n || path[r + 2] == PATH_SEP) {
            // .. element: remove to last separator
            r += 2;
            if out.len() > dotdot {
                // can backtrack, truncate to last separator
                let mut w = out.len() - 1;
                while w > dotdot && out[w] != PATH_SEP {
                    w -= 1;
                }
                out.truncate(w);
            } else if !rooted {
                // cannot backtrack, but not rooted, so append .. element
                if !out.is_empty() {
                    out.push(PATH_SEP);
                }
                out.push(DOT);
                out.push(DOT);
                dotdot = out.len();
            }
        } else {
            // real path element
            // add slash if needed
            if rooted && out.len() != 1 || !rooted && !out.is_empty() {
                out.push(PATH_SEP);
            }
            while r < n && path[r] != PATH_SEP {
                out.push(path[r]);
                r += 1;
            }
        }
    }

    // Turn empty string into "."
    if out.is_empty() {
        out.push(DOT);
    }
    out
}
