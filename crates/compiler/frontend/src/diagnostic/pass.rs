use crate::{CompileErrorReporter, LoweredFunction};

mod unused_locals;

pub use unused_locals::UnusedLocals;

pub trait DiagnosticPass {
    fn run<'ctx>(&self, func: &LoweredFunction<'ctx>, reporter: &mut CompileErrorReporter<'ctx>);
}
