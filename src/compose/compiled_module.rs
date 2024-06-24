use std::collections::HashMap;

use naga::Handle;

use super::{composer::ModuleName, mangle};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ModuleGlobal {
    Struct(Handle<naga::Type>),
    Constant(Handle<naga::Constant>),
    Override(Handle<naga::Override>),
    GlobalVariable(Handle<naga::GlobalVariable>),
    Function(Handle<naga::Function>),
    EntryPoint(String),
}

impl ModuleGlobal {
    pub fn name<'a>(&'a self, module: &'a naga::Module) -> Option<&'a str> {
        match self {
            ModuleGlobal::Struct(h) => module
                .types
                .get_handle(*h)
                .ok()
                .and_then(|v| v.name.as_deref()),
            ModuleGlobal::Constant(h) => module
                .constants
                .try_get(*h)
                .ok()
                .and_then(|v| v.name.as_deref()),
            ModuleGlobal::Override(h) => module
                .overrides
                .try_get(*h)
                .ok()
                .and_then(|v| v.name.as_deref()),
            ModuleGlobal::GlobalVariable(h) => module
                .global_variables
                .try_get(*h)
                .ok()
                .and_then(|v| v.name.as_deref()),
            ModuleGlobal::Function(h) => module
                .functions
                .try_get(*h)
                .ok()
                .and_then(|v| v.name.as_deref()),
            ModuleGlobal::EntryPoint(name) => Some(name),
        }
    }

    pub fn mangled_name(&self, module: &CompiledModule) -> Option<String> {
        self.name(&module.module)
            .map(|name| mangle(&module.name, name))
    }
}

/// A module with mangled names.
/// And fragments of a header.
pub struct CompiledModule {
    pub name: ModuleName,
    pub module: naga::Module,
    pub exports: HashMap<String, ModuleGlobal>,
}
impl CompiledModule {
    pub fn get_item(&self, item_name: &str) -> Option<ModuleGlobal> {
        self.exports.get(item_name).cloned()
    }
}
