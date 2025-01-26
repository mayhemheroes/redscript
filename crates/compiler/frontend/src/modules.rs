use redscript_ast as ast;
use sequence_trie::SequenceTrie;
use smallvec::smallvec;

use crate::lower::FreeFunctionIndexes;
use crate::{FreeFunctionIndex, QualifiedName, TypeId};

#[derive(Debug, Default)]
pub struct ModuleMap<'ctx> {
    map: SequenceTrie<&'ctx str, Export<'ctx>, hashbrown::DefaultHashBuilder>,
}

impl<'ctx> ModuleMap<'ctx> {
    #[inline]
    pub fn add_type(
        &mut self,
        path: QualifiedName<'ctx>,
        id: TypeId<'ctx>,
    ) -> Result<(), NameRedefinition> {
        if self.map.insert_owned(&path, Export::Type(id)).is_some() {
            return Err(NameRedefinition);
        };
        Ok(())
    }

    pub fn add_function(
        &mut self,
        path: QualifiedName<'ctx>,
        index: FreeFunctionIndex,
    ) -> Result<(), NameRedefinition> {
        match self.map.get_mut(&path) {
            None => {
                self.map
                    .insert_owned(&path, Export::FreeFunction(smallvec![index]));
            }
            Some(Export::FreeFunction(vec)) => vec.push(index),
            Some(_) => return Err(NameRedefinition),
        };
        Ok(())
    }

    pub fn exports(
        &self,
        path: impl AsRef<[&'ctx str]>,
    ) -> Option<impl Iterator<Item = (&'ctx str, &Export<'ctx>)>> {
        let node = self.map.get_node(path.as_ref())?;
        let it = node
            .children_with_keys()
            .filter_map(|(&k, n)| Some((k, n.value()?)));
        Some(it)
    }

    pub fn exists(&self, path: impl IntoIterator<Item = &'ctx str>) -> bool {
        self.map.get_node(path).is_some()
    }

    pub fn visit_import(
        &self,
        import: &ast::Import<'ctx>,
        mut on_type: impl FnMut(&'ctx str, TypeId<'ctx>),
        mut on_function: impl FnMut(&'ctx str, FreeFunctionIndex),
        mut on_not_found: impl FnMut(&'ctx str),
    ) {
        let mut visit_export = |export: &Export<'ctx>, name: &'ctx str| match export {
            Export::Type(id) => on_type(name, *id),
            Export::FreeFunction(vec) => vec.iter().for_each(|&idx| on_function(name, idx)),
        };

        match import {
            ast::Import::Exact(path) => {
                let last = path.as_ref().last().unwrap();
                match self.map.get(path.as_ref()) {
                    Some(export) => visit_export(export, last),
                    None => on_not_found(last),
                }
            }
            ast::Import::Select(path, names) => {
                match self.map.get_node(path.as_ref()) {
                    Some(node) => names.iter().for_each(|&name| match node.get([name]) {
                        Some(export) => visit_export(export, name),
                        None => on_not_found(name),
                    }),
                    None => on_not_found(path.as_ref().last().unwrap()),
                };
            }
            ast::Import::All(path) => match self.exports(path.as_ref()) {
                Some(exports) => {
                    let mut peekable = exports.peekable();
                    if peekable.peek().is_none() {
                        on_not_found(path.as_ref().last().unwrap());
                    } else {
                        peekable.for_each(|(name, export)| visit_export(export, name));
                    }
                }
                None => on_not_found(path.as_ref().last().unwrap()),
            },
        };
    }
}

#[derive(Debug)]
pub enum Export<'ctx> {
    FreeFunction(FreeFunctionIndexes),
    Type(TypeId<'ctx>),
}

#[derive(Debug)]
pub struct NameRedefinition;
