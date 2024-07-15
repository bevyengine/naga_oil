use std::collections::HashMap;

use naga::Handle;

use super::{composer::ModuleName, mangle};

pub trait NagaGlobalHandle {
    fn name<'a>(&self, module: &'a naga::Module) -> Option<&'a str>;
    fn mangled_name(&self, module: &CompiledModule) -> Option<String> {
        self.name(&module.module)
            .map(|name| mangle(&module.name, name))
    }
}

impl NagaGlobalHandle for Handle<naga::Type> {
    fn name<'a>(&self, module: &'a naga::Module) -> Option<&'a str> {
        module
            .types
            .get_handle(*self)
            .ok()
            .and_then(|v| v.name.as_deref())
    }
}

impl NagaGlobalHandle for Handle<naga::Constant> {
    fn name<'a>(&self, module: &'a naga::Module) -> Option<&'a str> {
        module
            .constants
            .try_get(*self)
            .ok()
            .and_then(|v| v.name.as_deref())
    }
}

impl NagaGlobalHandle for Handle<naga::Override> {
    fn name<'a>(&self, module: &'a naga::Module) -> Option<&'a str> {
        module
            .overrides
            .try_get(*self)
            .ok()
            .and_then(|v| v.name.as_deref())
    }
}

impl NagaGlobalHandle for Handle<naga::GlobalVariable> {
    fn name<'a>(&self, module: &'a naga::Module) -> Option<&'a str> {
        module
            .global_variables
            .try_get(*self)
            .ok()
            .and_then(|v| v.name.as_deref())
    }
}

impl NagaGlobalHandle for Handle<naga::Function> {
    fn name<'a>(&self, module: &'a naga::Module) -> Option<&'a str> {
        module
            .functions
            .try_get(*self)
            .ok()
            .and_then(|v| v.name.as_deref())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ModuleGlobal {
    Struct(Handle<naga::Type>),
    Constant(Handle<naga::Constant>),
    Override(Handle<naga::Override>),
    GlobalVariable(Handle<naga::GlobalVariable>),
    Function(Handle<naga::Function>),
    EntryPoint(String),
}

/// A module with mangled names.
/// And fragments of a header.
pub struct CompiledModule {
    pub name: ModuleName,
    /// Module with mangled names.
    pub module: naga::Module,
    /// Exports with the original names.
    pub exports: HashMap<String, ModuleGlobal>,
}
impl CompiledModule {
    pub fn get_export(&self, item_name: &str) -> Option<ModuleGlobal> {
        self.exports.get(item_name).cloned()
    }
}

pub struct FinalModuleData<'a> {
    structs: HashMap<&'a str, Handle<naga::Type>>,
    constants: HashMap<&'a str, Handle<naga::Constant>>,
    overrides: HashMap<&'a str, Handle<naga::Override>>,
    global_variables: HashMap<&'a str, Handle<naga::GlobalVariable>>,
    functions: HashMap<&'a str, Handle<naga::Function>>,
}
