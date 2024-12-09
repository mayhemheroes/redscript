use std::borrow::Cow;
use std::str::FromStr;

use itertools::Itertools;
use redscript::ast::{BinOp, Ident, Span, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{AnyDefinition, Class, Enum, Function, Visibility};
use sequence_trie::SequenceTrie;

use crate::error::{Cause, Error, ResultSpan};
use crate::parser::{Annotation, FunctionSource, Qualifier};
use crate::scope::Scope;

#[derive(Debug)]
pub struct SymbolMap {
    symbols: SequenceTrie<Ident, Symbol>,
}

impl SymbolMap {
    pub fn new(pool: &ConstantPool) -> Result<Self, Error> {
        let mut symbols: SequenceTrie<Ident, Symbol> = SequenceTrie::new();

        for (idx, def) in pool.roots() {
            let name = pool.def_name(idx)?;
            let symbol = match def.value {
                AnyDefinition::Class(ref class) if class.flags.is_struct() => {
                    Symbol::Struct(idx.cast(), class.visibility)
                }
                AnyDefinition::Class(ref class) => Symbol::Class(idx.cast(), class.visibility),
                AnyDefinition::Enum(_) => Symbol::Enum(idx.cast()),
                AnyDefinition::Function(ref fun) => Symbol::Functions(vec![(idx.cast(), fun.visibility)]),
                _ => continue,
            };
            let path = ModulePath::parse(&name);
            match (symbols.get_mut(&path), symbol) {
                (Some(Symbol::Functions(existing)), Symbol::Functions(new)) => {
                    existing.extend(new);
                }
                (_, symbol) => {
                    symbols.insert(&path, symbol);
                }
            }
        }

        Ok(Self { symbols })
    }

    pub fn add_class(&mut self, path: &ModulePath, class: PoolIndex<Class>, visibility: Visibility) {
        self.symbols.insert(path, Symbol::Class(class, visibility));
    }

    pub fn add_struct(&mut self, path: &ModulePath, class: PoolIndex<Class>, visibility: Visibility) {
        self.symbols.insert(path, Symbol::Struct(class, visibility));
    }

    pub fn add_enum(&mut self, path: &ModulePath, enum_: PoolIndex<Enum>) {
        self.symbols.insert(path, Symbol::Enum(enum_));
    }

    pub fn add_function(&mut self, path: &ModulePath, index: PoolIndex<Function>, visibility: Visibility) {
        match self.symbols.get_mut(path) {
            Some(Symbol::Functions(existing)) => {
                existing.push((index, visibility));
            }
            _ => {
                self.symbols.insert(path, Symbol::Functions(vec![(index, visibility)]));
            }
        }
    }

    pub fn populate_import(&self, import: Import, scope: &mut Scope, visibility: Visibility) -> Result<(), Error> {
        match import {
            Import::Exact(_, path, span) => {
                if let Some(symbol) = self.get_symbol(&path).and_then(|s| s.visible(visibility)) {
                    scope.add_symbol(path.last().unwrap(), symbol);
                } else {
                    return Err(Cause::UnresolvedImport(path.render()).with_span(span));
                }
            }
            Import::All(_, path, span) => {
                for (ident, symbol) in self.get_direct_children(&path).with_span(span)? {
                    if let Some(symbol) = symbol.clone().visible(visibility) {
                        scope.add_symbol(ident, symbol.clone());
                    }
                }
            }
            Import::Selected(_, path, names, span) => {
                for name in names {
                    let path = path.with_child(name);
                    if let Some(symbol) = self.get_symbol(&path).and_then(|s| s.visible(visibility)) {
                        scope.add_symbol(path.last().unwrap(), symbol);
                    } else {
                        return Err(Cause::UnresolvedImport(path.render()).with_span(span));
                    }
                }
            }
        };
        Ok(())
    }

    #[inline]
    pub fn get_symbol(&self, path: &ModulePath) -> Option<Symbol> {
        self.symbols.get(path).cloned()
    }

    fn get_direct_children(&self, path: &ModulePath) -> Result<impl Iterator<Item = (Ident, &Symbol)>, Cause> {
        let node = self
            .symbols
            .get_node(path)
            .ok_or_else(|| Cause::UnresolvedModule(path.render()))?;
        let res = node
            .children_with_keys()
            .filter_map(|(k, v)| Some((k.clone(), v.value()?)));
        Ok(res)
    }
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Class(PoolIndex<Class>, Visibility),
    Struct(PoolIndex<Class>, Visibility),
    Enum(PoolIndex<Enum>),
    Functions(Vec<(PoolIndex<Function>, Visibility)>),
}

impl Symbol {
    pub fn visible(self, visibility: Visibility) -> Option<Self> {
        match self {
            Self::Class(_, v) if v <= visibility => Some(self),
            Self::Struct(_, v) if v <= visibility => Some(self),
            Self::Enum(_) => Some(self),
            Self::Functions(funs) => {
                let visible_funs: Vec<_> = funs.into_iter().filter(|(_, v)| *v <= visibility).collect();
                if visible_funs.is_empty() {
                    None
                } else {
                    Some(Self::Functions(visible_funs))
                }
            }
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Import {
    Exact(Vec<Annotation>, ModulePath, Span),
    Selected(Vec<Annotation>, ModulePath, Vec<Ident>, Span),
    All(Vec<Annotation>, ModulePath, Span),
}

impl Import {
    pub fn annotations(&self) -> &[Annotation] {
        match self {
            Self::Exact(anns, _, _) | Self::Selected(anns, _, _, _) | Self::All(anns, _, _) => anns,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath {
    pub(crate) parts: Vec<Ident>,
}

impl ModulePath {
    pub const EMPTY: Self = Self { parts: vec![] };

    pub fn new(parts: Vec<Ident>) -> Self {
        Self { parts }
    }

    pub fn parse(str: &str) -> Self {
        let parts = str
            .split('.')
            .map(|str| Ident::from_ref(str.split(';').next().unwrap()))
            .collect();
        Self { parts }
    }

    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    pub fn with_child(&self, child: Ident) -> Self {
        let mut copy = self.clone();
        copy.parts.push(child);
        copy
    }

    pub fn with_function(&self, fun_sig: FunctionSignature<'_>) -> Self {
        let ident = Ident::from_ref(fun_sig);
        self.with_child(ident)
    }

    pub fn last(&self) -> Option<Ident> {
        self.parts.last().cloned()
    }

    pub fn render(&self) -> Ident {
        let str = self.parts.iter().join(".");
        Ident::from(str)
    }
}

impl<'a> IntoIterator for &'a ModulePath {
    type IntoIter = std::slice::Iter<'a, Ident>;
    type Item = &'a Ident;

    fn into_iter(self) -> Self::IntoIter {
        self.parts.iter()
    }
}

pub struct FunctionSignature<'a>(Cow<'a, str>);

impl<'a> FunctionSignature<'a> {
    #[inline]
    pub fn from_raw(name: &'a str) -> Self {
        Self(Cow::Borrowed(name))
    }

    pub fn from_source(source: &'a FunctionSource) -> Self {
        let qs = &source.declaration.qualifiers;
        let name = source.declaration.name.as_ref();
        let is_operator = BinOp::from_str(name).is_ok();
        let is_cast = name == "Cast";

        if !is_operator
            && !is_cast
            && (qs.contain(Qualifier::Callback) || qs.contain(Qualifier::Exec) || qs.contain(Qualifier::Native))
        {
            FunctionSignature(Cow::Borrowed(name))
        } else {
            let builder = source
                .parameters
                .iter()
                .fold(FunctionSignatureBuilder::new(name.to_owned()), |acc, param| {
                    acc.parameter(&param.type_, param.qualifiers.contain(Qualifier::Out) && is_operator)
                });
            if is_operator || is_cast {
                builder.return_type(source.type_.as_ref().unwrap_or(&TypeName::VOID))
            } else {
                builder.build()
            }
        }
    }

    pub fn name(&self) -> &str {
        self.as_ref()
            .split_once(';')
            .map_or(self.as_ref(), |(prefix, _)| prefix)
    }
}

impl AsRef<str> for FunctionSignature<'_> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

pub struct FunctionSignatureBuilder {
    signature: String,
}

impl FunctionSignatureBuilder {
    pub fn new(name: String) -> Self {
        Self { signature: name + ";" }
    }

    pub fn parameter(self, typ: &TypeName, is_out: bool) -> Self {
        let mut signature = self.signature;
        if is_out {
            signature.push_str("Out");
        }
        signature.push_str(typ.mangled().as_ref());
        Self { signature }
    }

    pub fn return_type<'a>(self, typ: &TypeName) -> FunctionSignature<'a> {
        let mut signature = self.signature;
        signature.push(';');
        signature.push_str(typ.mangled().as_ref());
        FunctionSignature(Cow::Owned(signature))
    }

    pub fn build<'a>(self) -> FunctionSignature<'a> {
        FunctionSignature(Cow::Owned(self.signature))
    }
}
