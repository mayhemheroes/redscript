use std::time::SystemTime;
use std::{fs, io};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CompileTimestamp {
    nanos: u128,
}

impl CompileTimestamp {
    pub fn read<R: io::Read + io::Seek>(input: &mut R) -> io::Result<Option<Self>> {
        if input.seek(io::SeekFrom::End(0))? == 0 {
            return Ok(None);
        }
        input.rewind()?;

        let mut buf: [u8; 16] = [0; 16];
        input.read_exact(&mut buf)?;

        let nanos = u128::from_le_bytes(buf);
        Ok(Some(Self { nanos }))
    }

    pub fn write<W: io::Write + io::Seek>(&self, output: &mut W) -> io::Result<()> {
        output.rewind()?;
        output.write_all(&self.nanos.to_le_bytes())?;
        Ok(())
    }
}

impl TryFrom<&fs::Metadata> for CompileTimestamp {
    type Error = io::Error;

    fn try_from(metadata: &fs::Metadata) -> io::Result<Self> {
        let nanos = metadata
            .modified()?
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        Ok(Self { nanos })
    }
}
