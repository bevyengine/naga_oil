use std::collections::{HashMap, HashSet};

use naga::EntryPoint;

use crate::{derive::DerivedModule, redirect::Redirector};

use self::composer::{
    ComposableModuleDefinition, Composer, ComposerError, ComposerErrorInner, ErrSource, ModuleName,
    ShaderDefValue,
};

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
pub mod tokenizer;
pub mod util;

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
    pub fn with_capabilities(self, capabilities: naga::valid::Capabilities) -> Self {
        Self {
            capabilities,
            validate: self.validate,
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
        let module_set = self.make_composable_module(desc)?;

        if self.module_sets.contains_key(&module_set.name) {
            return Err(ComposerError {
                inner: ComposerErrorInner::ModuleAlreadyExists(module_set.name.0.clone()),
                source: ErrSource::Constructing {
                    path: module_set.file_path.to_owned(),
                    source: module_set.source.to_owned(),
                    offset: 0,
                },
            });
        }

        let name = module_set.name.clone();
        self.module_sets.insert(name.clone(), module_set);
        Ok(self.module_sets.get(&name).unwrap())
    }

    /// remove a composable module
    pub fn remove_composable_module(&mut self, module_name: &ModuleName) {
        self.module_sets.remove(module_name);
    }

    /// TODO:
    /// - @binding(auto) for auto-binding
    /// - virtual and override
    /// build a naga shader module
    pub fn make_naga_module(
        &mut self,
        desc: NagaModuleDescriptor,
    ) -> Result<naga::Module, ComposerError> {
        let definition = self.make_composable_module(ComposableModuleDescriptor {
            source: desc.source,
            file_path: desc.file_path,
            language: desc.shader_type.into(),
            as_name: None,
            additional_imports: desc.additional_imports,
            shader_defs: desc.shader_defs,
        })?;

        let shader_defs = {
            let defs = definition.shader_defs.clone();
            let mut all_imported_modules = HashSet::new();
            self.collect_all_imports(&definition.name, &mut all_imported_modules)?;
            self.collect_shader_defs(&all_imported_modules, &mut defs);
            defs
        };

        let processed = compose_parser::preprocess(self, &definition, &shader_defs)?;

        // TODO:
        // - Replace all :: names with randomly generated names. Merge that logic into compose_parser::preprocess
        // - Build naga modules, starting with the ones that have zero dependencies.
        // - Then construct a header, and build the next module. Use the aliases here.
        // - Name mangling.

        self.ensure_imports(
            imports.iter().map(|import| &import.definition),
            &shader_defs,
        )?;
        self.ensure_imports(additional_imports, &shader_defs)?;

        let PreprocessOutput {
            preprocessed_source,
            imports,
        } = self
            .preprocessor
            .preprocess(&sanitized_source, &shader_defs, self.validate)
            .map_err(|inner| ComposerError {
                inner,
                source: ErrSource::Constructing {
                    path: file_path.to_owned(),
                    source: sanitized_source,
                    offset: 0,
                },
            })?;

        let composable = self
            .create_composable_module(
                &definition,
                String::from(""),
                &shader_defs,
                false,
                false,
                &preprocessed_source,
                imports,
            )
            .map_err(|e| ComposerError {
                inner: e.inner,
                source: ErrSource::Constructing {
                    path: definition.file_path.to_owned(),
                    source: preprocessed_source.clone(),
                    offset: e.source.offset(),
                },
            })?;

        let mut derived = DerivedModule::default();

        let mut already_added = Default::default();
        for import in &composable.imports {
            self.add_import(
                &mut derived,
                import,
                &shader_defs,
                false,
                &mut already_added,
            );
        }

        Self::add_composable_data(&mut derived, &composable, None, 0, false);

        let stage = match desc.shader_type {
            #[cfg(feature = "glsl")]
            ShaderType::GlslVertex => Some(naga::ShaderStage::Vertex),
            #[cfg(feature = "glsl")]
            ShaderType::GlslFragment => Some(naga::ShaderStage::Fragment),
            _ => None,
        };

        let mut entry_points = Vec::default();
        derived.set_shader_source(&composable.module_ir, 0);
        for ep in &composable.module_ir.entry_points {
            let mapped_func = derived.localize_function(&ep.function);
            entry_points.push(EntryPoint {
                name: ep.name.clone(),
                function: mapped_func,
                stage: stage.unwrap_or(ep.stage),
                early_depth_test: ep.early_depth_test,
                workgroup_size: ep.workgroup_size,
            });
        }

        let mut naga_module = naga::Module {
            entry_points,
            ..derived.into()
        };

        // apply overrides
        if !composable.override_functions.is_empty() {
            let mut redirect = Redirector::new(naga_module);

            for (base_function, overrides) in composable.override_functions {
                let mut omit = HashSet::default();

                let mut original = base_function;
                for replacement in overrides {
                    let (_h_orig, _h_replace) = redirect
                        .redirect_function(&original, &replacement, &omit)
                        .map_err(|e| ComposerError {
                            inner: e.into(),
                            source: ErrSource::Constructing {
                                path: file_path.to_owned(),
                                source: preprocessed_source.clone(),
                                offset: composable.start_offset,
                            },
                        })?;
                    omit.insert(replacement.clone());
                    original = replacement;
                }
            }

            naga_module = redirect.into_module().map_err(|e| ComposerError {
                inner: e.into(),
                source: ErrSource::Constructing {
                    path: file_path.to_owned(),
                    source: preprocessed_source.clone(),
                    offset: composable.start_offset,
                },
            })?;
        }

        // validation
        if self.validate {
            let info =
                naga::valid::Validator::new(naga::valid::ValidationFlags::all(), self.capabilities)
                    .validate(&naga_module);
            match info {
                Ok(_) => Ok(naga_module),
                Err(e) => {
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
                    })
                }
            }
        } else {
            Ok(naga_module)
        }
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
