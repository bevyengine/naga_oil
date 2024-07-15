use naga::{
    front::wgsl::ParsedWgsl,
    valid::{Capabilities, ShaderStages},
    EntryPoint, Handle,
};
use std::collections::{BTreeMap, HashMap, HashSet};
use tracing::{debug, trace};
use winnow::RecoverableParser;

use crate::{compose::mangle, derive::DerivedModule};

pub use super::error::{ComposerError, ComposerErrorInner, ErrSource};
use super::{
    compiled_module::{CompiledModule, ModuleGlobal},
    preprocess::{self, FlattenedImport},
    CompileId, ComposableModuleDescriptor, ShaderLanguage,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum ShaderDefValue {
    Bool(bool),
    Int(i32),
    UInt(u32),
}

impl Default for ShaderDefValue {
    fn default() -> Self {
        ShaderDefValue::Bool(true)
    }
}

impl ShaderDefValue {
    pub(super) fn value_as_string(&self) -> String {
        match self {
            ShaderDefValue::Bool(val) => val.to_string(),
            ShaderDefValue::Int(val) => val.to_string(),
            ShaderDefValue::UInt(val) => val.to_string(),
        }
    }

    pub(super) fn parse(value: &str) -> Self {
        if let Ok(val) = value.parse::<u32>() {
            ShaderDefValue::UInt(val)
        } else if let Ok(val) = value.parse::<i32>() {
            ShaderDefValue::Int(val)
        } else if let Ok(val) = value.parse::<bool>() {
            ShaderDefValue::Bool(val)
        } else {
            // TODO: Better error handling
            ShaderDefValue::Bool(false) // this error will get picked up when we fully preprocess the module?
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct OwnedShaderDefs(BTreeMap<String, ShaderDefValue>);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct ModuleKey(OwnedShaderDefs);

impl ModuleKey {
    fn from_members(key: &HashMap<String, ShaderDefValue>, universe: &[String]) -> Self {
        let mut acc = OwnedShaderDefs::default();
        for item in universe {
            if let Some(value) = key.get(item) {
                acc.0.insert(item.to_owned(), *value);
            }
        }
        ModuleKey(acc)
    }
}

// a module built with a specific set of shader_defs
#[derive(Default, Debug)]
pub struct ComposableModule {
    // module decoration, prefixed to all items from this module in the final source
    pub decorated_name: String,
    // module names required as imports, optionally with a list of items to import
    pub imports: Vec<ImportDefinition>,
    // types exported
    pub owned_types: HashSet<String>,
    // constants exported
    pub owned_constants: HashSet<String>,
    // vars exported
    pub owned_vars: HashSet<String>,
    // functions exported
    pub owned_functions: HashSet<String>,
    // naga module, built against headers for any imports
    module_ir: naga::Module,
    // headers in different shader languages, used for building modules/shaders that import this module
    // headers contain types, constants, global vars and empty function definitions -
    // just enough to convert source strings that want to import this module into naga IR
    // headers: HashMap<ShaderLanguage, String>,
    header_ir: naga::Module,
    // character offset of the start of the owned module string
    start_offset: usize,
}

// data used to build a ComposableModule
#[derive(Debug)]
pub struct ComposableModuleDefinition {
    pub name: ModuleName,
    // shader text (with auto bindings replaced - we do this on module add as we only want to do it once to avoid burning slots)
    pub source: String,
    // language
    pub language: ShaderLanguage,
    // source path for error display
    pub file_path: String,
    // shader defs that have been defined by this module
    pub shader_defs: HashMap<String, ShaderDefValue>,
    pub(super) parsed: preprocess::Preprocessed,
    // list of shader_defs that can affect this module
    pub(super) used_defs: Vec<String>,
    // full list of possible imports (regardless of shader_def configuration)
    pub(super) shader_imports: Vec<FlattenedImport>,
    // additional imports to add (as though they were included in the source after any other imports)
    pub(super) additional_imports: Vec<(ModuleName, ImportDefinition)>,
    /// Which alias maps to which function/struct/module
    pub(super) alias_to_path: HashMap<String, String>,
}

impl ComposableModuleDefinition {
    pub fn get_imports(
        &self,
        module_sets: &HashMap<ModuleName, ComposableModuleDefinition>,
    ) -> Result<Vec<ImportDefinition>, ComposerError> {
        self.additional_imports
            .iter()
            .map(|(_, import)| Ok(import.clone()))
            .chain(self.shader_imports.iter().map(|import| {
                match get_imported_module(module_sets, import) {
                    Some(v) => Ok(v.definition),
                    None => Err(ComposerError {
                        inner: ComposerErrorInner::ImportNotFound(
                            import.path.to_owned(),
                            import.offset,
                        ),
                        source: ErrSource::Module {
                            name: self.name.0.to_string(),
                            offset: import.offset,
                            defs: self.shader_defs.clone(),
                        },
                    }),
                }
            }))
            .collect()
    }
}

#[derive(Debug)]
pub(super) struct ImportGraph {
    graph: HashMap<ModuleName, Vec<ImportDefinition>>,
}

// TODO: Document that conditional imports are not supported. (have to be at the very top, just like #defines)
// TODO: Verify that ^
// Alternatively, we could support them by changing the #define semantics to no longer be global & time traveling.
impl ImportGraph {
    pub fn new(
        module_sets: &HashMap<ModuleName, ComposableModuleDefinition>,
    ) -> Result<Self, ComposerError> {
        let mut graph = HashMap::new();
        for (module_name, module) in module_sets {
            graph.insert(module_name.clone(), module.get_imports(module_sets)?);
        }
        Ok(Self { graph })
    }

    /// Collects all imports for a given entry point module.
    /// Excludes the entry point module itself.
    pub fn get_reachable(&self, entry_point: &ModuleName) -> HashSet<ModuleName> {
        let mut all_imports = HashSet::new();

        fn collect_imports_rec(
            module: &ModuleName,
            graph: &ImportGraph,
            all_imports: &mut HashSet<ModuleName>,
        ) {
            if let Some(imports) = graph.graph.get(module) {
                for import in imports {
                    if all_imports.insert(import.module_name.clone()) {
                        collect_imports_rec(&import.module_name, graph, all_imports);
                    }
                }
            }
        }
        collect_imports_rec(entry_point, &self, &mut all_imports);

        all_imports
    }

    /// Visits all modules in topological order, ending with the entry point.
    pub fn depth_first_visit(
        &self,
        entry_point: &ModuleName,
        mut callback: impl FnMut(&ModuleName),
    ) {
        fn visit(
            module: &ModuleName,
            graph: &ImportGraph,
            callback: &mut impl FnMut(&ModuleName),
            visited: &mut HashSet<ModuleName>,
        ) {
            if !visited.insert(module.clone()) {
                return;
            }
            if let Some(imports) = graph.graph.get(module) {
                for import in imports {
                    visit(&import.module_name, graph, callback, visited);
                }
                callback(module);
            }
        }

        let mut visited = HashSet::new();
        visit(entry_point, self, &mut callback, &mut visited);
    }

    /// Returns all modules in topological order, ending with the entry point.
    pub fn depth_first_order(&self, entry_point: &ModuleName) -> Vec<ModuleName> {
        let mut items = vec![];
        self.depth_first_visit(entry_point, |name| items.push(name.clone()));
        items
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(pub(super) String);
impl ModuleName {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self(name.into())
    }
}
impl std::fmt::Display for ModuleName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDefinition {
    pub module_name: ModuleName,
    pub item: String,
}

#[derive(Debug, Clone)]
pub struct ImportDefWithOffset {
    pub(super) definition: ImportDefinition,
    pub(super) offset: usize,
}

/// module composer.
/// stores any modules that can be imported into a shader
/// and builds the final shader
#[derive(Debug)]
pub struct Composer {
    pub validate: bool,
    pub module_sets: HashMap<ModuleName, ComposableModuleDefinition>,
    pub capabilities: naga::valid::Capabilities,
    /// The shader stages that the subgroup operations are valid for.
    /// Used when creating a validator for the module.
    /// See https://github.com/gfx-rs/wgpu/blob/d9c054c645af0ea9ef81617c3e762fbf0f3fecda/wgpu-core/src/device/mod.rs#L515
    /// for how to set this for proper subgroup ops support.
    pub subgroup_stages: ShaderStages,
    pub(super) compile_id: CompileId,
}

// shift for module index
// 21 gives
//   max size for shader of 2m characters
//   max 2048 modules
const SPAN_SHIFT: usize = 21;

impl Default for Composer {
    fn default() -> Self {
        Self {
            validate: true,
            capabilities: Default::default(),
            module_sets: Default::default(),
            subgroup_stages: ShaderStages::empty(),
            compile_id: CompileId::new(),
        }
    }
}

struct IrBuildResult {
    module: naga::Module,
    start_offset: usize,
}

impl Composer {
    pub(super) fn mangle_with_uuid(&self, name: &str) -> String {
        format!("{}_{}", self.compile_id, name)
    }
    pub(super) fn unmangle_with_uuid<'a>(&self, name: &'a str) -> &'a str {
        name.split('_').skip(1).next().unwrap_or(name)
    }
    pub(super) fn starts_with_uuid(&self, name: &str) -> bool {
        name.starts_with(&self.compile_id.to_string())
    }

    /// This creates a validator that properly detects subgroup support.
    pub(super) fn create_validator(&self) -> naga::valid::Validator {
        let subgroup_operations = if self.capabilities.contains(Capabilities::SUBGROUP) {
            use naga::valid::SubgroupOperationSet as S;
            S::BASIC | S::VOTE | S::ARITHMETIC | S::BALLOT | S::SHUFFLE | S::SHUFFLE_RELATIVE
        } else {
            naga::valid::SubgroupOperationSet::empty()
        };
        let mut validator =
            naga::valid::Validator::new(naga::valid::ValidationFlags::all(), self.capabilities);
        validator.subgroup_stages(self.subgroup_stages);
        validator.subgroup_operations(subgroup_operations);
        validator
    }

    pub(super) fn compile_to_naga_ir(
        &self,
        input: &ParsedWgsl,
        input_name: &ModuleName,
        imports: &Vec<ImportDefWithOffset>,
        modules: &HashMap<ModuleName, CompiledModule>,
        alias_to_path: &HashMap<String, String>, // TODO: Implement this properly in terms of something else
    ) -> Result<CompiledModule, ComposerError> {
        let mut header_module = DerivedModule::default();
        let mut imports_to_rename = HashMap::new();
        let mut imported_items = HashMap::new();

        let imports = group_to_map(
            imports
                .into_iter()
                .map(|v| (&v.definition.module_name, &v.definition)),
        );

        for (module_name, module_imports) in imports.into_iter() {
            let module: &CompiledModule = modules.get(module_name).unwrap();
            let mut header_module_edit = header_module.set_shader_source(&module.module, 0); // TODO: Set span_offset properly?

            for import in module_imports.iter() {
                imported_items.insert(import.item.as_str(), module_name);
                let item = module.get_export(&import.item).unwrap();
                let header_item = match item {
                    ModuleGlobal::Struct(h) => {
                        ModuleGlobal::Struct(header_module_edit.import_type(&h))
                    }
                    ModuleGlobal::Constant(h) => {
                        ModuleGlobal::Constant(header_module_edit.import_const(&h))
                    }
                    ModuleGlobal::Override(h) => {
                        ModuleGlobal::Override(header_module_edit.import_pipeline_override(&h))
                    }
                    ModuleGlobal::GlobalVariable(h) => {
                        ModuleGlobal::GlobalVariable(header_module_edit.import_global(&h))
                    }
                    ModuleGlobal::Function(h) => {
                        let f = module.module.functions.try_get(h).unwrap();
                        // create dummy header function
                        let header_function = naga::Function {
                            name: f.name.clone(),
                            arguments: f.arguments.clone(),
                            result: f.result.clone(),
                            local_variables: Default::default(),
                            expressions: Default::default(),
                            named_expressions: Default::default(),
                            body: Default::default(),
                        };

                        ModuleGlobal::Function(
                            header_module_edit.import_function(
                                &header_function,
                                module.module.functions.get_span(h),
                            ),
                        )
                    }
                    ModuleGlobal::EntryPoint(_) => panic!("entry points should not be imported"), // TODO: Proper error handling
                };

                imports_to_rename.insert(header_item, import.item.as_str());
            }
        }

        let header_module = rename_globals(header_module.into(), |global_ref| {
            if let Some(&new_name) = imports_to_rename.get(&global_ref.to_module_global()) {
                // We want to use this in the module
                new_name.to_owned()
            } else {
                // Should be invisible to the module
                self.mangle_with_uuid(&global_ref.name())
            }
        });

        let mut exports = HashMap::new();
        let mut module = input.to_module(Some(&header_module)).unwrap(); // TODO: Error handling
        module = rename_globals(module, |global_ref| {
            // Rename everything to have the module name prefix.
            let name = global_ref.name();
            if self.starts_with_uuid(name) {
                // It's a part of the invisible header. Turn from UUID_module_name to module_name.
                self.unmangle_with_uuid(name).to_owned()
            } else if let Some(module_name) = imported_items.get(name) {
                // It's an imported item.
                mangle(module_name, name)
            } else {
                exports.insert(name.to_owned(), global_ref.to_module_global());
                // It's an exported item. Rename it to have the module name prefix.
                mangle(input_name, name)
            }
        });

        Ok(CompiledModule {
            name: input_name.to_owned(),
            module,
            exports,
        })
    }

    pub(super) fn make_composable_module_definition(
        &self,
        desc: ComposableModuleDescriptor,
    ) -> Result<ComposableModuleDefinition, ComposerError> {
        let ComposableModuleDescriptor {
            source,
            file_path,
            language,
            as_name,
            additional_imports,
            mut shader_defs,
        } = desc;

        let (_, parsed, errors) =
            preprocess::preprocess.recoverable_parse(winnow::Located::new(source));

        if !errors.is_empty() {
            return Err(ComposerError {
                inner: ComposerErrorInner::PreprocessorError(
                    // TODO: Prettier error messages
                    errors
                        .into_iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .into(),
                ),
                source: ErrSource::Constructing {
                    path: file_path.to_owned(),
                    source: source.to_owned(),
                    offset: 0,
                },
            });
        }
        let parsed = match parsed {
            Some(parsed) => parsed,
            None => {
                return Err(ComposerError {
                    inner: ComposerErrorInner::PreprocessorError(
                        vec!["preprocessor failed to parse source".to_owned()].into(),
                    ),
                    source: ErrSource::Constructing {
                        path: file_path.to_owned(),
                        source: source.to_owned(),
                        offset: 0,
                    },
                });
            }
        };

        let module_names = as_name
            .into_iter()
            .chain(parsed.get_module_names(source).map(|v| v.to_owned()))
            .collect::<Vec<_>>();
        if module_names.len() == 0 {
            return Err(ComposerError {
                inner: ComposerErrorInner::NoModuleName,
                source: ErrSource::Constructing {
                    path: file_path.to_owned(),
                    source: source.to_owned(),
                    offset: 0,
                },
            });
        }
        if module_names.len() > 1 {
            return Err(ComposerError {
                inner: ComposerErrorInner::MultipleModuleNames(module_names.into()),
                source: ErrSource::Constructing {
                    path: file_path.to_owned(),
                    source: source.to_owned(),
                    offset: 0, // TODO: Return the offset of the second module name
                },
            });
        }
        let module_name = ModuleName(module_names.into_iter().next().unwrap());
        let used_defs = parsed.get_used_defs(source);
        let defined_defs = parsed.get_defined_defs(source);
        shader_defs.extend(defined_defs);

        debug!(
            "adding module definition for {:?} with defs: {:?}",
            module_name, used_defs
        );

        let additional_imports = additional_imports
            .into_iter()
            .flat_map(|v| {
                let items = if v.items.is_empty() {
                    vec![v.module.0.clone()]
                } else {
                    v.items.clone()
                };

                items.into_iter().map(|item| {
                    (
                        v.module.clone(),
                        ImportDefinition {
                            module_name: v.module.clone(),
                            item,
                        },
                    )
                })
            })
            .collect::<Vec<_>>();
        let shader_imports = parsed.get_imports(source);
        let alias_to_path = shader_imports
            .iter()
            .filter_map(|v| {
                v.alias
                    .as_ref()
                    .map(|alias| (alias.clone(), v.path.clone()))
            })
            .collect();

        let module_set = ComposableModuleDefinition {
            name: module_name.clone(),
            source: source.to_owned(),
            file_path: file_path.to_owned(),
            language,
            used_defs: used_defs.into_iter().collect(),
            additional_imports,
            shader_imports,
            shader_defs,
            parsed,
            alias_to_path,
        };

        Ok(module_set)
    }
}

pub(super) fn get_imported_module(
    module_sets: &HashMap<ModuleName, ComposableModuleDefinition>,
    import: &FlattenedImport,
) -> Option<ImportDefWithOffset> {
    let module_path = ModuleName::new(&import.path);
    let splitted_path = import.path.rsplit_once("::");
    let super_module_path: Option<(ModuleName, String)> = splitted_path
        .map(|(module, item)| (ModuleName::new(module), item))
        .and_then(|(module, item)| {
            if module_sets.contains_key(&module) {
                Some((module, item.to_owned()))
            } else {
                None
            }
        });

    let module_exists = module_sets.contains_key(&module_path);

    // TODO: Change the syntax, or add #export s so that I don't need to rely on this hack where I check "which import could be correct".
    let (module, item) = match (module_exists, super_module_path) {
        (true, None) => (
            ModuleName::new(import.path.clone()),
            splitted_path
                .map(|v| v.1)
                .unwrap_or_else(|| &import.path)
                .to_owned(),
        ),
        (true, Some((module, item))) => {
            eprintln!("Ambiguous import: {} could refer to either a module or a function. Please use the syntax `module::function` to disambiguate.", import.path);
            (module, item)
        }
        (false, None) => {
            return None;
        }
        (false, Some((module, item))) => (module, item),
    };

    Some(ImportDefWithOffset {
        definition: ImportDefinition {
            module_name: module,
            item,
        },
        offset: import.offset,
    })
}

#[derive(Debug)]
pub(super) struct ModuleImports<'a> {
    pub(super) entry_name: ModuleName,
    pub(super) modules: &'a HashMap<ModuleName, ComposableModuleDefinition>,
    pub(super) import_graph: ImportGraph,
}

impl<'a> ModuleImports<'a> {
    pub fn new(
        entry_name: ModuleName,
        module_sets: &'a HashMap<ModuleName, ComposableModuleDefinition>,
    ) -> Result<Self, ComposerError> {
        assert!(module_sets.contains_key(&entry_name));
        Ok(Self {
            entry_name,
            modules: module_sets,
            import_graph: ImportGraph::new(module_sets)?,
        })
    }

    pub fn collect_shader_defs(
        &self,
        imports: &HashSet<ModuleName>,
        mut shader_defs: HashMap<String, ShaderDefValue>,
    ) -> Result<HashMap<String, ShaderDefValue>, ComposerError> {
        for import_name in imports {
            // TODO: No unwrap pls
            let module = self.modules.get(import_name).unwrap();
            for (def, value) in module.shader_defs.iter() {
                match shader_defs.insert(def.clone(), value.clone()) {
                    Some(old_value) if &old_value != value => {
                        return Err(ComposerError {
                            inner: ComposerErrorInner::InconsistentShaderDefValue {
                                def: def.clone(),
                            },
                            source: ErrSource::Constructing {
                                path: module.file_path.to_owned(),
                                source: module.source.to_owned(),
                                offset: 0, // TODO: Set this properly
                            },
                        });
                    }
                    _ => {}
                }
            }
        }
        Ok(shader_defs)
    }
}

pub enum ModuleGlobalRef<'a> {
    Struct(Handle<naga::Type>, &'a naga::Type),
    Constant(Handle<naga::Constant>, &'a naga::Constant),
    Override(Handle<naga::Override>, &'a naga::Override),
    GlobalVariable(Handle<naga::GlobalVariable>, &'a naga::GlobalVariable),
    Function(Handle<naga::Function>, &'a naga::Function),
    EntryPoint(&'a naga::EntryPoint),
}

impl<'a> ModuleGlobalRef<'a> {
    pub fn to_module_global(&self) -> ModuleGlobal {
        match *self {
            ModuleGlobalRef::Struct(handle, _) => ModuleGlobal::Struct(handle),
            ModuleGlobalRef::Constant(handle, _) => ModuleGlobal::Constant(handle),
            ModuleGlobalRef::Override(handle, _) => ModuleGlobal::Override(handle),
            ModuleGlobalRef::GlobalVariable(handle, _) => ModuleGlobal::GlobalVariable(handle),
            ModuleGlobalRef::Function(handle, _) => ModuleGlobal::Function(handle),
            ModuleGlobalRef::EntryPoint(entry_point) => {
                ModuleGlobal::EntryPoint(entry_point.name.clone())
            }
        }
    }
    pub fn name(&self) -> &'a str {
        match *self {
            ModuleGlobalRef::Struct(_, ty) => ty.name.as_deref().unwrap(),
            ModuleGlobalRef::Constant(_, c) => c.name.as_deref().unwrap(),
            ModuleGlobalRef::Override(_, o) => o.name.as_deref().unwrap(),
            ModuleGlobalRef::GlobalVariable(_, gv) => gv.name.as_deref().unwrap(),
            ModuleGlobalRef::Function(_, f) => f.name.as_deref().unwrap(),
            ModuleGlobalRef::EntryPoint(entry_point) => entry_point.name.as_str(),
        }
    }
}

fn iter_globals(module: &naga::Module) -> impl Iterator<Item = ModuleGlobalRef> + '_ {
    let struct_type_handles = module.types.iter().filter_map(|(h, ty)| match &ty.inner {
        naga::TypeInner::Struct { .. } => Some(ModuleGlobalRef::Struct(h, ty)),
        _ => None,
    });
    let constant_handles = module
        .constants
        .iter()
        .map(|(h, c)| ModuleGlobalRef::Constant(h, c));
    let override_handles = module
        .overrides
        .iter()
        .map(|(h, o)| ModuleGlobalRef::Override(h, o));
    let global_variable_handles = module
        .global_variables
        .iter()
        .map(|(h, gv)| ModuleGlobalRef::GlobalVariable(h, gv));
    let function_handles = module
        .functions
        .iter()
        .map(|(h, f)| ModuleGlobalRef::Function(h, f));
    let entry_points = module.entry_points.iter().map(ModuleGlobalRef::EntryPoint);

    struct_type_handles
        .chain(constant_handles)
        .chain(override_handles)
        .chain(global_variable_handles)
        .chain(function_handles)
        .chain(entry_points)
}

fn rename_globals<Rename>(mut module: naga::Module, mut rename: Rename) -> naga::Module
where
    Rename: FnMut(ModuleGlobalRef) -> String,
{
    let struct_type_handles = module
        .types
        .iter()
        .filter_map(|(h, ty)| match &ty.inner {
            naga::TypeInner::Struct { .. } => Some(h),
            _ => None,
        })
        .collect::<Vec<_>>();
    for handle in struct_type_handles.into_iter() {
        let v = module.types.get_handle(handle.clone()).unwrap();
        if v.name.is_some() {
            let name = rename(ModuleGlobalRef::Struct(handle, v));
            let mut new_value = v.to_owned();
            new_value.name = Some(name);
            // Replaces the value, and keeps the same handle
            module.types.replace(handle, new_value);
        }
    }
    for (handle, v) in module.constants.iter_mut() {
        if v.name.is_none() {
            v.name = Some(rename(ModuleGlobalRef::Constant(handle, v)));
        }
    }
    for (handle, v) in module.overrides.iter_mut() {
        if v.name.is_none() {
            v.name = Some(rename(ModuleGlobalRef::Override(handle, v)));
        }
    }
    for (handle, v) in module.global_variables.iter_mut() {
        if v.name.is_none() {
            v.name = Some(rename(ModuleGlobalRef::GlobalVariable(handle, v)));
        }
    }
    for (handle, v) in module.functions.iter_mut() {
        if v.name.is_some() {
            v.name = Some(rename(ModuleGlobalRef::Function(handle, v)));
        }
    }
    for v in module.entry_points.iter_mut() {
        v.name = rename(ModuleGlobalRef::EntryPoint(v));
    }

    module
}

// From https://users.rust-lang.org/t/group-iterator-into-hashmap-k-vec-v/31727/2
fn group_to_map<Key, Value, Iter>(iter: Iter) -> HashMap<Key, Vec<Value>>
where
    Key: Eq + std::hash::Hash,
    Iter: Iterator<Item = (Key, Value)>,
{
    let mut hash_map = match iter.size_hint() {
        (_, Some(len)) => HashMap::with_capacity(len),
        (len, None) => HashMap::with_capacity(len),
    };
    for (key, value) in iter {
        hash_map
            .entry(key)
            .or_insert_with(|| Vec::with_capacity(1))
            .push(value);
    }
    hash_map
}
