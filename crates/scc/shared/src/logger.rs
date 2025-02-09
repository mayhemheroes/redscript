use std::path::Path;

use chrono::SecondsFormat;
use flexi_logger::{Age, Cleanup, Criterion, Duplicate, FileSpec, LogSpecBuilder, Logger, Naming};
use log::LevelFilter;

pub fn setup(root_dir: &Path) {
    let file = FileSpec::default()
        .directory(root_dir.join("r6").join("logs"))
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
            let time = time
                .now()
                .with_timezone(&chrono::Utc)
                .to_rfc3339_opts(SecondsFormat::Secs, true);
            write!(out, "[{} - {time}] {}", msg.level(), msg.args())
        })
        .start()
        .ok();
}
