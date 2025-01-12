use std::path::Path;

use flexi_logger::{Age, Cleanup, Criterion, Duplicate, FileSpec, LogSpecBuilder, Logger, Naming};
use log::LevelFilter;

pub fn setup(r6_dir: &Path) {
    let file = FileSpec::default()
        .directory(r6_dir.join("logs"))
        .basename("redscript");

    Logger::with(LogSpecBuilder::new().default(LevelFilter::Info).build())
        .log_to_file(file)
        .duplicate_to_stdout(Duplicate::All)
        .rotate(
            Criterion::Age(Age::Day),
            Naming::Timestamps,
            Cleanup::KeepLogFiles(4),
        )
        .format(|out, time, msg| {
            write!(
                out,
                "[{} - {}] {}",
                msg.level(),
                time.now().to_rfc2822(),
                msg.args()
            )
        })
        .start()
        .ok();
}
