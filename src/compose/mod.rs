use std::collections::{hash_map, HashMap};

use compiled_module::{CompiledModule, ModuleGlobal, NagaGlobalHandle};
use naga::Handle;

use crate::derive::{BasicGlobalCache, DerivedModule, GlobalCacher};

use self::composer::{
    ComposableModuleDefinition, ComposerErrorInner, ErrSource, ModuleImports, ModuleName,
    ShaderDefValue,
};
pub use self::composer::{Composer, ComposerError};

mod compiled_module;
mod compose_parser;
/// the compose module allows construction of shaders from modules (which are themselves shaders).
///
/// it does this by treating shaders as modules, and
/// - building each module independently to naga IR
/// - creating "header" files for each supported language, which are used to build dependent modules/shaders
/// - making final shaders by combining the shader IR with the IR for imported modules
///
/// for multiple small shaders with large common imports, this can be faster than parsing the full source for each shader, and it allows for constructing shaders in a cleaner modular manner with better scope control.
///
/// ## imports
///
/// shaders can be added to the composer as modules. this makes their types, constants, variables and functions available to modules/shaders that import them. note that importing a module will affect the final shader's global state if the module defines globals variables with bindings.
///
/// modules must include a `#define_import_path` directive that names the module.
///
/// ```ignore
/// #define_import_path my_module
///
/// fn my_func() -> f32 {
///     return 1.0;
/// }
/// ```
///
/// shaders can then import the module with an `#import` directive (with an optional `as` name). at point of use, imported items must be qualified:
///
/// ```ignore
/// #import my_module
/// #import my_other_module as Mod2
///
/// fn main() -> f32 {
///     let x = my_module::my_func();
///     let y = Mod2::my_other_func();
///     return x*y;
/// }
/// ```
///
/// or import a comma-separated list of individual items with a `#from` directive. at point of use, imported items must be prefixed with `::` :
///
/// ```ignore
/// #from my_module import my_func, my_const
///
/// fn main() -> f32 {
///     return ::my_func(::my_const);
/// }
/// ```
///
/// imports can be nested - modules may import other modules, but not recursively. when a new module is added, all its `#import`s must already have been added.
/// the same module can be imported multiple times by different modules in the import tree.
/// there is no overlap of namespaces, so the same function names (or type, constant, or variable names) may be used in different modules.
///
/// note: when importing an item with the `#from` directive, the final shader will include the required dependencies (bindings, globals, consts, other functions) of the imported item, but will not include the rest of the imported module. it will however still include all of any modules imported by the imported module. this is probably not desired in general and may be fixed in a future version. currently for a more complete culling of unused dependencies the `prune` module can be used.
///
/// ## overriding functions
///
/// virtual functions can be declared with the `virtual` keyword:
/// ```ignore
/// virtual fn point_light(world_position: vec3<f32>) -> vec3<f32> { ... }
/// ```
/// virtual functions defined in imported modules can then be overridden using the `override` keyword:
///
/// ```ignore
/// #import bevy_pbr::lighting as Lighting
///
/// override fn Lighting::point_light (world_position: vec3<f32>) -> vec3<f32> {
///     let original = Lighting::point_light(world_position);
///     let quantized = vec3<u32>(original * 3.0);
///     return vec3<f32>(quantized) / 3.0;
/// }
/// ```
///
/// override function definitions cause *all* calls to the original function in the entire shader scope to be replaced by calls to the new function, with the exception of calls within the override function itself.
///
/// the function signature of the override must match the base function.
///
/// overrides can be specified at any point in the final shader's import tree.
///
/// multiple overrides can be applied to the same function. for example, given :
/// - a module `a` containing a function `f`,
/// - a module `b` that imports `a`, and containing an `override a::f` function,
/// - a module `c` that imports `a` and `b`, and containing an `override a::f` function,
/// then b and c both specify an override for `a::f`.
/// the `override fn a::f` declared in module `b` may call to `a::f` within its body.
/// the `override fn a::f` declared in module 'c' may call to `a::f` within its body, but the call will be redirected to `b::f`.
/// any other calls to `a::f` (within modules 'a' or `b`, or anywhere else) will end up redirected to `c::f`
/// in this way a chain or stack of overrides can be applied.
///
/// different overrides of the same function can be specified in different import branches. the final stack will be ordered based on the first occurrence of the override in the import tree (using a depth first search).
///
/// note that imports into a module/shader are processed in order, but are processed before the body of the current shader/module regardless of where they occur in that module, so there is no way to import a module containing an override and inject a call into the override stack prior to that imported override. you can instead create two modules each containing an override and import them into a parent module/shader to order them as required.
/// override functions can currently only be defined in wgsl.
///
/// if the `override_any` crate feature is enabled, then the `virtual` keyword is not required for the function being overridden.
///
/// ## languages
///
/// modules can we written in GLSL or WGSL. shaders with entry points can be imported as modules (provided they have a `#define_import_path` directive). entry points are available to call from imported modules either via their name (for WGSL) or via `module::main` (for GLSL).
///
/// final shaders can also be written in GLSL or WGSL. for GLSL users must specify whether the shader is a vertex shader or fragment shader via the `ShaderType` argument (GLSL compute shaders are not supported).
///
/// ## preprocessing
///
/// when generating a final shader or adding a composable module, a set of `shader_def` string/value pairs must be provided. The value can be a bool (`ShaderDefValue::Bool`), an i32 (`ShaderDefValue::Int`) or a u32 (`ShaderDefValue::UInt`).
///
/// these allow conditional compilation of parts of modules and the final shader. conditional compilation is performed with `#if` / `#ifdef` / `#ifndef`, `#else` and `#endif` preprocessor directives:
///
/// ```ignore
/// fn get_number() -> f32 {
///     #ifdef BIG_NUMBER
///         return 999.0;
///     #else
///         return 0.999;
///     #endif
/// }
/// ```
/// the `#ifdef` directive matches when the def name exists in the input binding set (regardless of value). the `#ifndef` directive is the reverse.
///
/// the `#if` directive requires a def name, an operator, and a value for comparison:
/// - the def name must be a provided `shader_def` name.
/// - the operator must be one of `==`, `!=`, `>=`, `>`, `<`, `<=`
/// - the value must be an integer literal if comparing to a `ShaderDef::Int`, or `true` or `false` if comparing to a `ShaderDef::Bool`.
///
/// shader defs can also be used in the shader source with `#SHADER_DEF` or `#{SHADER_DEF}`, and will be substituted for their value.
///
/// ## error reporting
///
/// codespan reporting for errors is available using the error `emit_to_string` method. this requires validation to be enabled, which is true by default. `Composer::non_validating()` produces a non-validating composer that is not able to give accurate error reporting.
///
pub mod composer;
pub mod error;
pub mod preprocess;
mod test;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompileId(String);

impl CompileId {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4().to_string())
    }
}

impl Default for CompileId {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for CompileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdditionalImport {
    // TODO: Support aliases?
    pub module: ModuleName,
    pub items: Vec<String>,
}

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug, Default)]
pub enum ShaderLanguage {
    #[default]
    Wgsl,
    #[cfg(feature = "glsl")]
    Glsl,
}

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug, Default)]
pub enum ShaderType {
    #[default]
    Wgsl,
    #[cfg(feature = "glsl")]
    GlslVertex,
    #[cfg(feature = "glsl")]
    GlslFragment,
}

impl From<ShaderType> for ShaderLanguage {
    fn from(ty: ShaderType) -> Self {
        match ty {
            ShaderType::Wgsl => ShaderLanguage::Wgsl,
            #[cfg(feature = "glsl")]
            ShaderType::GlslVertex | ShaderType::GlslFragment => ShaderLanguage::Glsl,
        }
    }
}

#[derive(Default)]
pub struct ComposableModuleDescriptor<'a> {
    pub source: &'a str,
    pub file_path: &'a str,
    pub language: ShaderLanguage,
    pub as_name: Option<String>,
    pub additional_imports: &'a [AdditionalImport],
    pub shader_defs: HashMap<String, ShaderDefValue>,
}

#[derive(Default)]
pub struct NagaModuleDescriptor<'a> {
    pub source: &'a str,
    pub file_path: &'a str,
    pub shader_type: ShaderType,
    pub additional_imports: &'a [AdditionalImport],
    pub shader_defs: HashMap<String, ShaderDefValue>,
}

pub fn mangle(module: &ModuleName, name: &str) -> String {
    format!("{}_{}", module.0, name)
}
pub fn unmangle(name: &str) -> (ModuleName, String) {
    let mut parts = name.split('_');
    let module = ModuleName(parts.next().unwrap().to_owned());
    let name = parts.collect();
    (module, name)
}
/// Unmangle a name into a module and a name
pub fn unmangle_ref(name: &str) -> (&str, &str) {
    let mut parts = name.split('_');
    let module = parts.next().unwrap();
    let name = parts.next().unwrap();
    (module, name)
}

// public api
impl Composer {
    /// create a non-validating composer.
    /// validation errors in the final shader will not be caught, and errors resulting from their
    /// use will have bad span data, so codespan reporting will fail.
    /// use default() to create a validating composer.
    pub fn non_validating() -> Self {
        Self {
            validate: false,
            ..Default::default()
        }
    }

    /// specify capabilities to be used for naga module generation.
    /// purges any existing modules
    /// See https://github.com/gfx-rs/wgpu/blob/d9c054c645af0ea9ef81617c3e762fbf0f3fecda/wgpu-core/src/device/mod.rs#L515
    /// for how to set the subgroup_stages value.
    pub fn with_capabilities(
        self,
        capabilities: naga::valid::Capabilities,
        subgroup_stages: naga::valid::ShaderStages,
    ) -> Self {
        Self {
            capabilities,
            validate: self.validate,
            subgroup_stages,
            ..Default::default()
        }
    }

    /// check if a module with the given name has been added
    pub fn contains_module(&self, module_name: &ModuleName) -> bool {
        self.module_sets.contains_key(module_name)
    }

    /// add a composable module to the composer
    pub fn add_composable_module(
        &mut self,
        desc: ComposableModuleDescriptor,
    ) -> Result<&ComposableModuleDefinition, ComposerError> {
        let module_set = self.make_composable_module_definition(desc)?;

        match self.module_sets.entry(module_set.name.clone()) {
            hash_map::Entry::Occupied(_) => Err(ComposerError {
                inner: ComposerErrorInner::ModuleAlreadyExists(module_set.name.0.clone()),
                source: ErrSource::Constructing {
                    path: module_set.file_path.to_owned(),
                    source: module_set.source.to_owned(),
                    offset: 0,
                },
            }),
            hash_map::Entry::Vacant(slot) => Ok(&*slot.insert(module_set)),
        }
    }

    /// remove a composable module
    pub fn remove_composable_module(&mut self, module_name: &ModuleName) {
        self.module_sets.remove(module_name);
    }

    /// TODO:
    /// - @binding(auto) for auto-binding
    /// - virtual and override
    /// - mod::func in the source code
    ///
    /// Builds a naga shader module
    pub fn make_naga_module(
        &mut self,
        desc: NagaModuleDescriptor,
    ) -> Result<naga::Module, ComposerError> {
        let definition = self.add_composable_module(ComposableModuleDescriptor {
            source: desc.source,
            file_path: desc.file_path,
            language: desc.shader_type.into(),
            additional_imports: desc.additional_imports,
            shader_defs: desc.shader_defs,
            // We pick a unique name for the main module
            as_name: Some(format!("main_module_{}", uuid::Uuid::new_v4())),
        })?;
        let name = definition.name.clone();
        let result = self.make_naga_module_inner(name.clone());
        self.remove_composable_module(&name);
        result
    }

    fn make_naga_module_inner(
        &mut self,
        entry_point_name: ModuleName,
    ) -> Result<naga::Module, ComposerError> {
        let module_imports = ModuleImports::new(entry_point_name.clone(), &self.module_sets)?;
        let reachable_modules = module_imports.import_graph.get_reachable(&entry_point_name);
        let shader_defs = module_imports.collect_shader_defs(&reachable_modules, HashMap::new())?;

        let mut naga_parser = naga::front::wgsl::Frontend::new();
        let mut processed_sources = HashMap::with_capacity(reachable_modules.len());
        let mut processed_ast = HashMap::with_capacity(reachable_modules.len());

        for module in reachable_modules
            .iter()
            .map(|name| module_imports.modules.get(name).unwrap())
        {
            let preprocessed_source =
                compose_parser::preprocess(&module_imports, module, &shader_defs)?;
            processed_sources.insert(module.name.clone(), preprocessed_source);
        }
        for module_name in reachable_modules.iter() {
            let ast = naga_parser
                .parse_to_ast(&processed_sources[module_name].source)
                .unwrap(); // TODO: Handle errors
            processed_ast.insert(module_name.clone(), ast);
        }

        let mut compiled = HashMap::with_capacity(reachable_modules.len());
        let module_order = module_imports
            .import_graph
            .depth_first_order(&entry_point_name);
        for module_name in module_order.iter() {
            let result = self
                .compile_to_naga_ir(
                    &processed_ast[module_name],
                    &module_name,
                    &processed_sources[module_name].imports,
                    &compiled,
                    &Default::default(),
                )
                .unwrap();
            compiled.insert(module_name.to_owned(), result);
        }

        // Now I have a ton of modules with properly mangled names, and header fragments.
        // Then I need to iterate over them in the import graph order, and copy everything to the final module.
        // We also attempt to preserve the order of the modules in the final module.

        /*TODO:
        let module_span_shifts = HashMap::with_capacity(reachable_modules.len());
        module_imports
            .import_graph
            .depth_first_visit(&entry_point_name, |module_name| {
                compiled.insert(module_name.to_owned(), result);
            }); */

        let mut naga_module = DerivedModule::default();

        {
            let mut final_structs: HashMap<&str, Handle<naga::Type>> = HashMap::default();
            let mut final_constants: HashMap<&str, Handle<naga::Constant>> = HashMap::default();
            let mut final_overrides: HashMap<&str, Handle<naga::Override>> = HashMap::default();
            let mut final_global_variables: HashMap<&str, Handle<naga::GlobalVariable>> =
                HashMap::default();
            let mut final_functions: HashMap<&str, Handle<naga::Function>> = HashMap::default();

            for module_name in module_order.iter() {
                let module = compiled.get(module_name).unwrap();
                let mut module_edit = naga_module
                    .set_shader_source(&module.module, 0) // TODO: Set span offset
                    .with_cache(FinalModuleCache {
                        module_name: module_name.clone(),
                        module,
                        structs: std::mem::take(&mut final_structs),
                        constants: std::mem::take(&mut final_constants),
                        overrides: std::mem::take(&mut final_overrides),
                        global_variables: std::mem::take(&mut final_global_variables),
                        functions: std::mem::take(&mut final_functions),
                        cache: Default::default(),
                    });

                for (handle, _) in module.module.types.iter() {
                    module_edit.import_type(&handle);
                }
                for (handle, _) in module.module.constants.iter() {
                    module_edit.import_const(&handle);
                }
                for (handle, _) in module.module.overrides.iter() {
                    module_edit.import_pipeline_override(&handle);
                }
                for (handle, _) in module.module.global_variables.iter() {
                    module_edit.import_global(&handle);
                }
                for (_, function) in module.module.functions.iter() {
                    module_edit.import_function(&function, Default::default()); // TODO: Set span
                }

                final_structs = std::mem::take(&mut module_edit.cache.structs);
                final_constants = std::mem::take(&mut module_edit.cache.constants);
                final_overrides = std::mem::take(&mut module_edit.cache.overrides);
                final_global_variables = std::mem::take(&mut module_edit.cache.global_variables);
                final_functions = std::mem::take(&mut module_edit.cache.functions);
            }
        }

        let naga_module = naga_module.into();

        // validation
        if self.validate {
            let info = self.create_validator().validate(&naga_module);
            match info {
                Ok(_) => Ok(naga_module),
                Err(e) => {
                    panic!("Validation failed: {:?}", e);
                    /* TODO: Error handling
                    let original_span = e.spans().last();
                    let err_source = match original_span.and_then(|(span, _)| span.to_range()) {
                        Some(rng) => {
                            let module_index = rng.start >> SPAN_SHIFT;
                            match module_index {
                                0 => ErrSource::Constructing {
                                    path: file_path.to_owned(),
                                    source: preprocessed_source.clone(),
                                    offset: composable.start_offset,
                                },
                                _ => {
                                    let module_name =
                                        self.module_index.get(&module_index).unwrap().clone();
                                    let offset = self
                                        .module_sets
                                        .get(&module_name)
                                        .unwrap()
                                        .get_module(&shader_defs)
                                        .unwrap()
                                        .start_offset;
                                    ErrSource::Module {
                                        name: module_name,
                                        offset,
                                        defs: shader_defs.clone(),
                                    }
                                }
                            }
                        }
                        None => ErrSource::Constructing {
                            path: file_path.to_owned(),
                            source: preprocessed_source.clone(),
                            offset: composable.start_offset,
                        },
                    };

                    Err(ComposerError {
                        inner: ComposerErrorInner::ShaderValidationError(e),
                        source: err_source,
                    }) */
                }
            }
        } else {
            Ok(naga_module)
        }
    }
}

struct FinalModuleCache<'a> {
    module_name: ModuleName,
    module: &'a CompiledModule,
    structs: HashMap<&'a str, Handle<naga::Type>>,
    constants: HashMap<&'a str, Handle<naga::Constant>>,
    overrides: HashMap<&'a str, Handle<naga::Override>>,
    global_variables: HashMap<&'a str, Handle<naga::GlobalVariable>>,
    functions: HashMap<&'a str, Handle<naga::Function>>,
    cache: BasicGlobalCache,
}

impl<'a> FinalModuleCache<'a> {
    fn is_header_part<T: NagaGlobalHandle>(&self, item: T) -> bool {
        item.name(&self.module.module)
            .map(|name| unmangle_ref(&name).0 != self.module_name.0)
            .unwrap_or(false)
    }

    fn is_exported_item<T: NagaGlobalHandle>(&self, item: T) -> bool {
        if let Some(name) = item.name(&self.module.module) {
            if self.module.get_export(unmangle_ref(&name).1).is_some() {
                return true;
            }
        }
        false
    }
}

impl<'a> GlobalCacher for FinalModuleCache<'a> {
    fn get_type(&self, handle: Handle<naga::Type>) -> Option<Handle<naga::Type>> {
        if self.is_header_part(handle) {
            return Some(
                *self
                    .structs
                    .get(handle.name(&self.module.module).unwrap())
                    .expect("Imported item should have been added."),
            );
        }

        self.cache.get_type(handle)
    }

    fn set_type(&mut self, handle: Handle<naga::Type>, new_handle: Handle<naga::Type>) {
        if self.is_header_part(handle) {
            return;
        }
        self.cache.set_type(handle, new_handle);
        if self.is_exported_item(handle) {
            self.structs
                .insert(handle.name(&self.module.module).unwrap(), new_handle);
        }
    }

    fn get_constant(&self, handle: Handle<naga::Constant>) -> Option<Handle<naga::Constant>> {
        todo!()
    }

    fn set_constant(&mut self, handle: Handle<naga::Constant>, new_handle: Handle<naga::Constant>) {
        todo!()
    }

    fn get_override(&self, handle: Handle<naga::Override>) -> Option<Handle<naga::Override>> {
        todo!()
    }

    fn set_override(&mut self, handle: Handle<naga::Override>, new_handle: Handle<naga::Override>) {
        todo!()
    }

    fn get_global_variable(
        &self,
        handle: Handle<naga::GlobalVariable>,
    ) -> Option<Handle<naga::GlobalVariable>> {
        todo!()
    }

    fn set_global_variable(
        &mut self,
        handle: Handle<naga::GlobalVariable>,
        new_handle: Handle<naga::GlobalVariable>,
    ) {
        todo!()
    }

    fn get_function(&self, handle: &str) -> Option<Handle<naga::Function>> {
        todo!()
    }

    fn set_function(&mut self, handle: &str, new_handle: Handle<naga::Function>) {
        todo!()
    }
}

/* TODO: Implement this
/// Get module name and all required imports (ignoring shader_defs) from a shader string
pub fn get_preprocessor_data(
    source: &str,
) -> (
    Option<String>,
    Vec<ImportDefinition>,
    HashMap<String, ShaderDefValue>,
) {
    let (_, parsed, errors) =
        preprocess1::preprocess.recoverable_parse(winnow::Located::new(source));

    // Returning the defines correctly is impossible at the moment.
    todo!()


    if let Ok(PreprocessorMetaData {
        name,
        imports,
        defines,
        ..
    }) = Preprocessor.get_preprocessor_metadata(source, true)
    {
        (
            name,
            imports
                .into_iter()
                .map(|import_with_offset| import_with_offset.definition)
                .collect(),
            defines,
        )
    } else {
        // if errors occur we return nothing; the actual error will be displayed when the caller attempts to use the shader
        Default::default()
    }
}*/
