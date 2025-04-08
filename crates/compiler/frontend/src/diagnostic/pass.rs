use crate::{CompileErrorReporter, LoweredFunction};

mod unused_locals;

pub use unused_locals::UnusedLocals;

pub trait DiagnosticPass<'ctx> {
    fn run(&self, func: &LoweredFunction<'ctx>, reporter: &mut CompileErrorReporter<'ctx>);
}
