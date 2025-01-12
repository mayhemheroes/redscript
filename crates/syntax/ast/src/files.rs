use std::path::{Path, PathBuf};
use std::{fmt, fs, io};

use crate::span::FileId;

#[derive(Debug, Default)]
pub struct SourceMap {
    files: StableDeque<File>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_files(it: impl IntoIterator<Item = impl Into<PathBuf>>) -> io::Result<Self> {
        let mut files = Self::new();
        for path in it {
            let path = path.into();
            let source = fs::read_to_string(&path)?;
            files.push_back(path, source);
        }
        Ok(files)
    }

    #[cfg(feature = "walkdir")]
    pub fn from_paths_recursively(
        it: impl IntoIterator<Item = impl Into<PathBuf>>,
    ) -> io::Result<Self> {
        use std::ffi::OsStr;

        let it = it.into_iter().flat_map(|path| {
            walkdir::WalkDir::new(path.into())
                .follow_links(true)
                .into_iter()
                .filter_map(Result::ok)
                .filter(|entry| entry.path().extension() == Some(OsStr::new("reds")))
                .map(walkdir::DirEntry::into_path)
        });
        Self::from_files(it)
    }

    pub fn push_front(&mut self, path: impl Into<PathBuf>, source: impl Into<String>) -> FileId {
        FileId(self.files.push_front(File::new(path, source)))
    }

    pub fn push_back(&mut self, path: impl Into<PathBuf>, source: impl Into<String>) -> FileId {
        FileId(self.files.push_back(File::new(path, source)))
    }

    #[inline]
    pub fn get(&self, id: FileId) -> Option<&File> {
        self.files.get(id.0)
    }

    pub fn files(&self) -> impl Iterator<Item = (FileId, &File)> {
        self.files.iter().map(|(id, file)| (FileId(id), file))
    }

    pub fn len(&self) -> usize {
        self.files.len()
    }

    pub fn is_empty(&self) -> bool {
        self.files.len() == 0
    }
}

impl fmt::Display for SourceMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.files().enumerate().try_for_each(|(i, (_, file))| {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", file.path.display())
        })
    }
}

#[derive(Debug)]
pub struct File {
    path: PathBuf,
    source: String,
    lines: Vec<u32>,
}

impl File {
    pub fn new(path: impl Into<PathBuf>, source: impl Into<String>) -> Self {
        let mut lines = vec![];
        let source = source.into();
        for (offset, _) in source.match_indices('\n') {
            lines.push(offset as u32 + 1);
        }
        Self {
            path: path.into(),
            source,
            lines,
        }
    }

    #[inline]
    pub fn source(&self) -> &str {
        &self.source
    }

    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn lookup(&self, offset: u32) -> SourceLoc {
        let (line, line_offset) = if self.lines.first().is_some_and(|&p| p > offset) {
            (0, 0)
        } else {
            let line = self
                .lines
                .binary_search(&offset)
                .map(|i| i + 1)
                .unwrap_or_else(|i| i);
            (line, self.lines[line - 1])
        };
        SourceLoc {
            line,
            col: self.source[line_offset as usize..offset as usize]
                .chars()
                .count(),
        }
    }

    pub fn line(&self, idx: usize) -> Option<&str> {
        let start = if idx == 0 {
            0
        } else {
            self.lines.get(idx - 1).copied()?
        };
        let end = self.lines.get(idx).copied().unwrap_or_else(|| {
            u32::try_from(self.source.len()).expect("source size should fit in u32")
        });
        Some(&self.source[start as usize..end as usize])
    }
}

#[derive(Debug, Clone)]
pub struct SourceLoc {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

#[derive(Debug)]
struct StableDeque<T> {
    front: Vec<T>,
    back: Vec<T>,
}

impl<T> StableDeque<T> {
    pub fn get(&self, index: i32) -> Option<&T> {
        if index < 0 {
            self.front.get((-index - 1) as usize)
        } else {
            self.back.get(index as usize)
        }
    }

    fn push_front(&mut self, value: T) -> i32 {
        self.front.push(value);
        -i32::try_from(self.front.len()).expect("deque size overflows i32")
    }

    fn push_back(&mut self, value: T) -> i32 {
        self.back.push(value);
        i32::try_from(self.back.len()).expect("deque size overflows i32") - 1
    }

    pub fn len(&self) -> usize {
        self.front.len() + self.back.len()
    }

    fn iter(&self) -> impl Iterator<Item = (i32, &T)> {
        self.front
            .iter()
            .enumerate()
            .map(|(i, v)| (-(i as i32) - 1, v))
            .chain(self.back.iter().enumerate().map(|(i, v)| (i as i32, v)))
    }
}

impl Default for StableDeque<File> {
    fn default() -> Self {
        Self {
            front: Vec::new(),
            back: Vec::new(),
        }
    }
}
