mod infer;
mod resolution;

pub use infer::{LoweredClass, LoweredCompilationUnit, LoweredFunction, TypeInference};
pub use resolution::{FunctionAnnotation, NameResolution, Scope};
