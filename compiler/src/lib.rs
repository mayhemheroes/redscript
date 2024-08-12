pub mod assembler;
pub mod cte;
pub mod diagnostics;
pub mod error;
#[allow(clippy::redundant_closure_call)]
pub mod parser;
pub mod scope;
mod sealed_structs;
pub mod source_map;
pub mod sugar;
pub mod symbol;
pub mod transform;
pub mod typechecker;
pub mod unit;
