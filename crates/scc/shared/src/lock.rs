use std::{
    fs, io,
    ops::{Deref, DerefMut},
};

pub struct FileLock(fs::File);

impl FileLock {
    pub fn new(file: fs::File) -> io::Result<Self> {
        file.lock()?;
        Ok(Self(file))
    }
}

impl Deref for FileLock {
    type Target = fs::File;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FileLock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Drop for FileLock {
    fn drop(&mut self) {
        self.0.unlock().ok();
    }
}
