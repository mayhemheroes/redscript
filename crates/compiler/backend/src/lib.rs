mod assemble;
mod inputs;
mod monomorph;
mod type_flags;

pub use assemble::AssembleError;
pub use inputs::{CompilationInputs, Error as PoolError, PoolMappings};
pub use monomorph::Monomorphizer;
pub use type_flags::{TypeFlagRegistry, TypeFlags};

type IndexMap<K, V, S = hashbrown::DefaultHashBuilder> = indexmap::IndexMap<K, V, S>;
