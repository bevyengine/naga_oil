use indexmap::IndexMap;

use naga::EntryPoint;
use regex::Regex;
use std::{
    collections::{hash_map::Entry, BTreeMap, HashMap, HashSet},
    ops::Range,
};
use tracing::{debug, trace};
use winnow::RecoverableParser;

use crate::{
    compose::preprocess::{DefineImportPath, PreprocessorPart},
    derive::DerivedModule,
    redirect::Redirector,
};

pub use super::error::{ComposerError, ComposerErrorInner, ErrSource};
use super::{
    compose_parser::PreprocessOutput,
    error::StringsWithNewlines,
    preprocess::{self, FlattenedImport, UseDefineDirective},
    ComposableModuleDescriptor, ShaderLanguage,
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
    // local functions that can be overridden
    pub virtual_functions: HashSet<String>,
    // overriding functions defined in this module
    // target function -> Vec<replacement functions>
    pub override_functions: IndexMap<String, Vec<String>>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(pub(super) String);
impl ModuleName {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self(name.into())
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDefinition {
    pub module: ModuleName,
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
    check_decoration_regex: Regex,
    undecorate_regex: Regex,
    undecorate_override_regex: Regex,
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
            check_decoration_regex: Regex::new(
                format!(
                    "({}|{})",
                    regex_syntax::escape(DECORATION_PRE),
                    regex_syntax::escape(DECORATION_OVERRIDE_PRE)
                )
                .as_str(),
            )
            .unwrap(),
            undecorate_regex: Regex::new(
                format!(
                    r"(\x1B\[\d+\w)?([\w\d_]+){}([A-Z0-9]*){}",
                    regex_syntax::escape(DECORATION_PRE),
                    regex_syntax::escape(DECORATION_POST)
                )
                .as_str(),
            )
            .unwrap(),
            undecorate_override_regex: Regex::new(
                format!(
                    "{}([A-Z0-9]*){}",
                    regex_syntax::escape(DECORATION_OVERRIDE_PRE),
                    regex_syntax::escape(DECORATION_POST)
                )
                .as_str(),
            )
            .unwrap(),
        }
    }
}

// TODO: Change the mangling scheme
const DECORATION_PRE: &str = "X_naga_oil_mod_X";
const DECORATION_POST: &str = "X";

// must be same length as DECORATION_PRE for spans to work
const DECORATION_OVERRIDE_PRE: &str = "X_naga_oil_vrt_X";

struct IrBuildResult {
    module: naga::Module,
    start_offset: usize,
    override_functions: IndexMap<String, Vec<String>>,
}

impl Composer {
    // TODO: Change the mangling scheme
    pub fn decorated_name(module_name: Option<&str>, item_name: &str) -> String {
        match module_name {
            Some(module_name) => format!("{}{}", item_name, Self::decorate(module_name)),
            None => item_name.to_owned(),
        }
    }

    pub(super) fn decorate(module: &str) -> String {
        let encoded = data_encoding::BASE32_NOPAD.encode(module.as_bytes());
        format!("{DECORATION_PRE}{encoded}{DECORATION_POST}")
    }

    fn decode(from: &str) -> String {
        String::from_utf8(data_encoding::BASE32_NOPAD.decode(from.as_bytes()).unwrap()).unwrap()
    }

    pub(super) fn undecorate(&self, string: &str) -> String {
        let undecor = self
            .undecorate_regex
            .replace_all(string, |caps: &regex::Captures| {
                format!(
                    "{}{}::{}",
                    caps.get(1).map(|cc| cc.as_str()).unwrap_or(""),
                    Self::decode(caps.get(3).unwrap().as_str()),
                    caps.get(2).unwrap().as_str()
                )
            });

        let undecor =
            self.undecorate_override_regex
                .replace_all(&undecor, |caps: &regex::Captures| {
                    format!(
                        "override fn {}::",
                        Self::decode(caps.get(1).unwrap().as_str())
                    )
                });

        undecor.to_string()
    }

    fn naga_to_string(
        &self,
        naga_module: &mut naga::Module,
        language: ShaderLanguage,
        #[allow(unused)] header_for: &str, // Only used when GLSL is enabled
    ) -> Result<String, ComposerErrorInner> {
        // TODO: cache headers again
        let info =
            naga::valid::Validator::new(naga::valid::ValidationFlags::all(), self.capabilities)
                .validate(naga_module)
                .map_err(ComposerErrorInner::HeaderValidationError)?;

        match language {
            ShaderLanguage::Wgsl => naga::back::wgsl::write_string(
                naga_module,
                &info,
                naga::back::wgsl::WriterFlags::EXPLICIT_TYPES,
            )
            .map_err(ComposerErrorInner::WgslBackError),
            #[cfg(feature = "glsl")]
            ShaderLanguage::Glsl => {
                let vec4 = naga_module.types.insert(
                    naga::Type {
                        name: None,
                        inner: naga::TypeInner::Vector {
                            size: naga::VectorSize::Quad,
                            scalar: naga::Scalar::F32,
                        },
                    },
                    naga::Span::UNDEFINED,
                );
                // add a dummy entry point for glsl headers
                let dummy_entry_point = "dummy_module_entry_point".to_owned();
                let func = naga::Function {
                    name: Some(dummy_entry_point.clone()),
                    arguments: Default::default(),
                    result: Some(naga::FunctionResult {
                        ty: vec4,
                        binding: Some(naga::Binding::BuiltIn(naga::BuiltIn::Position {
                            invariant: false,
                        })),
                    }),
                    local_variables: Default::default(),
                    expressions: Default::default(),
                    named_expressions: Default::default(),
                    body: Default::default(),
                };
                let ep = EntryPoint {
                    name: dummy_entry_point.clone(),
                    stage: naga::ShaderStage::Vertex,
                    function: func,
                    early_depth_test: None,
                    workgroup_size: [0, 0, 0],
                };

                naga_module.entry_points.push(ep);

                let info = naga::valid::Validator::new(
                    naga::valid::ValidationFlags::all(),
                    self.capabilities,
                )
                .validate(naga_module)
                .map_err(ComposerErrorInner::HeaderValidationError)?;

                let mut string = String::new();
                let options = naga::back::glsl::Options {
                    version: naga::back::glsl::Version::Desktop(450),
                    writer_flags: naga::back::glsl::WriterFlags::INCLUDE_UNUSED_ITEMS,
                    ..Default::default()
                };
                let pipeline_options = naga::back::glsl::PipelineOptions {
                    shader_stage: naga::ShaderStage::Vertex,
                    entry_point: dummy_entry_point,
                    multiview: None,
                };
                let mut writer = naga::back::glsl::Writer::new(
                    &mut string,
                    naga_module,
                    &info,
                    &options,
                    &pipeline_options,
                    naga::proc::BoundsCheckPolicies::default(),
                )
                .map_err(ComposerErrorInner::GlslBackError)?;

                writer.write().map_err(ComposerErrorInner::GlslBackError)?;

                // strip version decl and main() impl
                let lines: Vec<_> = string.lines().collect();
                let string = lines[1..lines.len() - 3].join("\n");
                trace!("glsl header for {}:\n\"\n{:?}\n\"", header_for, string);

                Ok(string)
            }
        }
    }

    // build naga module for a given shader_def configuration. builds a minimal self-contained module built against headers for imports
    fn create_module_ir(
        &self,
        name: &str,
        source: String,
        language: ShaderLanguage,
        imports: &[ImportDefinition],
        shader_defs: &HashMap<String, ShaderDefValue>,
    ) -> Result<IrBuildResult, ComposerError> {
        debug!("creating IR for {} with defs: {:?}", name, shader_defs);

        let mut module_string = match language {
            ShaderLanguage::Wgsl => String::new(),
            #[cfg(feature = "glsl")]
            ShaderLanguage::Glsl => String::from("#version 450\n"),
        };

        let mut override_functions: IndexMap<String, Vec<String>> = IndexMap::default();
        let mut added_imports: HashSet<String> = HashSet::new();
        let mut header_module = DerivedModule::default();

        for import in imports {
            if added_imports.contains(&import.module) {
                continue;
            }
            // add to header module
            self.add_import(
                &mut header_module,
                import,
                shader_defs,
                true,
                &mut added_imports,
            );

            // // we must have ensured these exist with Composer::ensure_imports()
            trace!("looking for {}", import.module);
            let import_module_set = self.module_sets.get(&import.module).unwrap();
            trace!("with defs {:?}", shader_defs);
            let module = import_module_set.get_module(shader_defs).unwrap();
            trace!("ok");

            // gather overrides
            if !module.override_functions.is_empty() {
                for (original, replacements) in &module.override_functions {
                    match override_functions.entry(original.clone()) {
                        indexmap::map::Entry::Occupied(o) => {
                            let existing = o.into_mut();
                            let new_replacements: Vec<_> = replacements
                                .iter()
                                .filter(|rep| !existing.contains(rep))
                                .cloned()
                                .collect();
                            existing.extend(new_replacements);
                        }
                        indexmap::map::Entry::Vacant(v) => {
                            v.insert(replacements.clone());
                        }
                    }
                }
            }
        }

        let composed_header = self
            .naga_to_string(&mut header_module.into(), language, name)
            .map_err(|inner| ComposerError {
                inner,
                source: ErrSource::Module {
                    name: name.to_owned(),
                    offset: 0,
                    defs: shader_defs.clone(),
                },
            })?;
        module_string.push_str(&composed_header);

        let start_offset = module_string.len();

        module_string.push_str(&source);

        trace!(
            "parsing {}: {}, header len {}, total len {}",
            name,
            module_string,
            start_offset,
            module_string.len()
        );
        let module = match language {
            ShaderLanguage::Wgsl => naga::front::wgsl::parse_str(&module_string).map_err(|e| {
                debug!("full err'd source file: \n---\n{}\n---", module_string);
                ComposerError {
                    inner: ComposerErrorInner::WgslParseError(e),
                    source: ErrSource::Module {
                        name: name.to_owned(),
                        offset: start_offset,
                        defs: shader_defs.clone(),
                    },
                }
            })?,
            #[cfg(feature = "glsl")]
            ShaderLanguage::Glsl => naga::front::glsl::Frontend::default()
                .parse(
                    &naga::front::glsl::Options {
                        stage: naga::ShaderStage::Vertex,
                        defines: Default::default(),
                    },
                    &module_string,
                )
                .map_err(|e| {
                    debug!("full err'd source file: \n---\n{}\n---", module_string);
                    ComposerError {
                        inner: ComposerErrorInner::GlslParseError(e),
                        source: ErrSource::Module {
                            name: name.to_owned(),
                            offset: start_offset,
                            defs: shader_defs.clone(),
                        },
                    }
                })?,
        };

        Ok(IrBuildResult {
            module,
            start_offset,
            override_functions,
        })
    }

    // check that identifiers exported by a module do not get modified in string export
    fn validate_identifiers(
        source_ir: &naga::Module,
        lang: ShaderLanguage,
        header: &str,
        module_decoration: &str,
        owned_types: &HashSet<String>,
    ) -> Result<(), ComposerErrorInner> {
        // TODO: remove this once glsl front support is complete
        #[cfg(feature = "glsl")]
        if lang == ShaderLanguage::Glsl {
            return Ok(());
        }

        let recompiled = match lang {
            ShaderLanguage::Wgsl => naga::front::wgsl::parse_str(header).unwrap(),
            #[cfg(feature = "glsl")]
            ShaderLanguage::Glsl => naga::front::glsl::Frontend::default()
                .parse(
                    &naga::front::glsl::Options {
                        stage: naga::ShaderStage::Vertex,
                        defines: Default::default(),
                    },
                    &format!("{}\n{}", header, "void main() {}"),
                )
                .map_err(|e| {
                    debug!("full err'd source file: \n---\n{header}\n---");
                    ComposerErrorInner::GlslParseError(e)
                })?,
        };

        let recompiled_types: IndexMap<_, _> = recompiled
            .types
            .iter()
            .flat_map(|(h, ty)| ty.name.as_deref().map(|name| (name, h)))
            .collect();
        for (h, ty) in source_ir.types.iter() {
            if let Some(name) = &ty.name {
                let decorated_type_name = format!("{name}{module_decoration}");
                if !owned_types.contains(&decorated_type_name) {
                    continue;
                }
                match recompiled_types.get(decorated_type_name.as_str()) {
                    Some(recompiled_h) => {
                        if let naga::TypeInner::Struct { members, .. } = &ty.inner {
                            let recompiled_ty = recompiled.types.get_handle(*recompiled_h).unwrap();
                            let naga::TypeInner::Struct {
                                members: recompiled_members,
                                ..
                            } = &recompiled_ty.inner
                            else {
                                panic!();
                            };
                            for (member, recompiled_member) in
                                members.iter().zip(recompiled_members)
                            {
                                if member.name != recompiled_member.name {
                                    return Err(ComposerErrorInner::InvalidIdentifier {
                                        original: member.name.clone().unwrap_or_default(),
                                        at: source_ir.types.get_span(h),
                                    });
                                }
                            }
                        }
                    }
                    None => {
                        return Err(ComposerErrorInner::InvalidIdentifier {
                            original: name.clone(),
                            at: source_ir.types.get_span(h),
                        })
                    }
                }
            }
        }

        let recompiled_consts: HashSet<_> = recompiled
            .constants
            .iter()
            .flat_map(|(_, c)| c.name.as_deref())
            .filter(|name| name.ends_with(module_decoration))
            .collect();
        for (h, c) in source_ir.constants.iter() {
            if let Some(name) = &c.name {
                if name.ends_with(module_decoration) && !recompiled_consts.contains(name.as_str()) {
                    return Err(ComposerErrorInner::InvalidIdentifier {
                        original: name.clone(),
                        at: source_ir.constants.get_span(h),
                    });
                }
            }
        }

        let recompiled_globals: HashSet<_> = recompiled
            .global_variables
            .iter()
            .flat_map(|(_, c)| c.name.as_deref())
            .filter(|name| name.ends_with(module_decoration))
            .collect();
        for (h, gv) in source_ir.global_variables.iter() {
            if let Some(name) = &gv.name {
                if name.ends_with(module_decoration) && !recompiled_globals.contains(name.as_str())
                {
                    return Err(ComposerErrorInner::InvalidIdentifier {
                        original: name.clone(),
                        at: source_ir.global_variables.get_span(h),
                    });
                }
            }
        }

        let recompiled_fns: HashSet<_> = recompiled
            .functions
            .iter()
            .flat_map(|(_, c)| c.name.as_deref())
            .filter(|name| name.ends_with(module_decoration))
            .collect();
        for (h, f) in source_ir.functions.iter() {
            if let Some(name) = &f.name {
                if name.ends_with(module_decoration) && !recompiled_fns.contains(name.as_str()) {
                    return Err(ComposerErrorInner::InvalidIdentifier {
                        original: name.clone(),
                        at: source_ir.functions.get_span(h),
                    });
                }
            }
        }

        Ok(())
    }

    // build a ComposableModule from a ComposableModuleDefinition, for a given set of shader defs
    // - build the naga IR (against headers)
    // - record any types/vars/constants/functions that are defined within this module
    // - build headers for each supported language
    #[allow(clippy::too_many_arguments)]
    pub fn create_composable_module(
        &mut self,
        module_definition: &ComposableModuleDefinition,
        module_decoration: String,
        shader_defs: &HashMap<String, ShaderDefValue>,
        create_headers: bool,
        demote_entrypoints: bool,
        source: &str,
        imports: Vec<ImportDefWithOffset>,
    ) -> Result<ComposableModule, ComposerError> {
        let mut imports: Vec<_> = imports
            .into_iter()
            .map(|import_with_offset| import_with_offset.definition)
            .collect();
        imports.extend(module_definition.additional_imports.to_vec());

        trace!(
            "create composable module {}: source len {}",
            module_definition.name.0,
            source.len()
        );

        trace!(
            "create composable module {}: source len {}",
            module_definition.name.0,
            source.len()
        );

        let IrBuildResult {
            module: mut source_ir,
            start_offset,
            mut override_functions,
        } = self.create_module_ir(
            &module_definition.name.0,
            source,
            module_definition.language,
            &imports,
            shader_defs,
        )?;

        // from here on errors need to be reported using the modified source with start_offset
        let wrap_err = |inner: ComposerErrorInner| -> ComposerError {
            ComposerError {
                inner,
                source: ErrSource::Module {
                    name: module_definition.name.0.to_owned(),
                    offset: start_offset,
                    defs: shader_defs.clone(),
                },
            }
        };

        // rename and record owned items (except types which can't be mutably accessed)
        let mut owned_constants = IndexMap::new();
        for (h, c) in source_ir.constants.iter_mut() {
            if let Some(name) = c.name.as_mut() {
                if !name.contains(DECORATION_PRE) {
                    *name = format!("{name}{module_decoration}");
                    owned_constants.insert(name.clone(), h);
                }
            }
        }

        let mut owned_vars = IndexMap::new();
        for (h, gv) in source_ir.global_variables.iter_mut() {
            if let Some(name) = gv.name.as_mut() {
                if !name.contains(DECORATION_PRE) {
                    *name = format!("{name}{module_decoration}");

                    owned_vars.insert(name.clone(), h);
                }
            }
        }

        let mut owned_functions = IndexMap::new();
        for (h_f, f) in source_ir.functions.iter_mut() {
            if let Some(name) = f.name.as_mut() {
                if !name.contains(DECORATION_PRE) {
                    *name = format!("{name}{module_decoration}");

                    // create dummy header function
                    let header_function = naga::Function {
                        name: Some(name.clone()),
                        arguments: f.arguments.to_vec(),
                        result: f.result.clone(),
                        local_variables: Default::default(),
                        expressions: Default::default(),
                        named_expressions: Default::default(),
                        body: Default::default(),
                    };

                    // record owned function
                    owned_functions.insert(name.clone(), (Some(h_f), header_function));
                }
            }
        }

        if demote_entrypoints {
            // make normal functions out of the source entry points
            for ep in &mut source_ir.entry_points {
                ep.function.name = Some(format!(
                    "{}{}",
                    ep.function.name.as_deref().unwrap_or("main"),
                    module_decoration,
                ));
                let header_function = naga::Function {
                    name: ep.function.name.clone(),
                    arguments: ep
                        .function
                        .arguments
                        .iter()
                        .cloned()
                        .map(|arg| naga::FunctionArgument {
                            name: arg.name,
                            ty: arg.ty,
                            binding: None,
                        })
                        .collect(),
                    result: ep.function.result.clone().map(|res| naga::FunctionResult {
                        ty: res.ty,
                        binding: None,
                    }),
                    local_variables: Default::default(),
                    expressions: Default::default(),
                    named_expressions: Default::default(),
                    body: Default::default(),
                };

                owned_functions.insert(ep.function.name.clone().unwrap(), (None, header_function));
            }
        };

        let mut module_builder = DerivedModule::default();
        let mut header_builder = DerivedModule::default();
        module_builder.set_shader_source(&source_ir, 0);
        header_builder.set_shader_source(&source_ir, 0);

        let mut owned_types = HashSet::new();
        for (h, ty) in source_ir.types.iter() {
            if let Some(name) = &ty.name {
                // we need to exclude autogenerated struct names, i.e. those that begin with "__"
                // "__" is a reserved prefix for naga so user variables cannot use it.
                if !name.contains(DECORATION_PRE) && !name.starts_with("__") {
                    let name = format!("{name}{module_decoration}");
                    owned_types.insert(name.clone());
                    // copy and rename types
                    module_builder.rename_type(&h, Some(name.clone()));
                    header_builder.rename_type(&h, Some(name));
                    continue;
                }
            }

            // copy all required types
            module_builder.import_type(&h);
        }

        // copy owned types into header and module
        for h in owned_constants.values() {
            header_builder.import_const(h);
            module_builder.import_const(h);
        }

        for h in owned_vars.values() {
            header_builder.import_global(h);
            module_builder.import_global(h);
        }

        // only stubs of owned functions into the header
        for (h_f, f) in owned_functions.values() {
            let span = h_f
                .map(|h_f| source_ir.functions.get_span(h_f))
                .unwrap_or(naga::Span::UNDEFINED);
            header_builder.import_function(f, span); // header stub function
        }
        // all functions into the module (note source_ir only contains stubs for imported functions)
        for (h_f, f) in source_ir.functions.iter() {
            let span = source_ir.functions.get_span(h_f);
            module_builder.import_function(f, span);
        }
        // // including entry points as vanilla functions if required
        if demote_entrypoints {
            for ep in &source_ir.entry_points {
                let mut f = ep.function.clone();
                f.arguments = f
                    .arguments
                    .into_iter()
                    .map(|arg| naga::FunctionArgument {
                        name: arg.name,
                        ty: arg.ty,
                        binding: None,
                    })
                    .collect();
                f.result = f.result.map(|res| naga::FunctionResult {
                    ty: res.ty,
                    binding: None,
                });

                module_builder.import_function(&f, naga::Span::UNDEFINED);
                // todo figure out how to get span info for entrypoints
            }
        }

        let module_ir = module_builder.into_module_with_entrypoints();
        let mut header_ir: naga::Module = header_builder.into();

        if self.validate && create_headers {
            // check that identifiers haven't been renamed
            #[allow(clippy::single_element_loop)]
            for language in [
                ShaderLanguage::Wgsl,
                #[cfg(feature = "glsl")]
                ShaderLanguage::Glsl,
            ] {
                let header = self
                    .naga_to_string(&mut header_ir, language, &module_definition.name.0)
                    .map_err(wrap_err)?;
                Self::validate_identifiers(
                    &source_ir,
                    language,
                    &header,
                    &module_decoration,
                    &owned_types,
                )
                .map_err(wrap_err)?;
            }
        }

        let composable_module = ComposableModule {
            decorated_name: module_decoration,
            imports,
            owned_types,
            owned_constants: owned_constants.into_keys().collect(),
            owned_vars: owned_vars.into_keys().collect(),
            owned_functions: owned_functions.into_keys().collect(),
            virtual_functions,
            override_functions,
            module_ir,
            header_ir,
            start_offset,
        };

        Ok(composable_module)
    }

    // shunt all data owned by a composable into a derived module
    fn add_composable_data<'a>(
        derived: &mut DerivedModule<'a>,
        composable: &'a ComposableModule,
        items: Option<&Vec<String>>,
        span_offset: usize,
        header: bool,
    ) {
        let items: Option<HashSet<String>> = items.map(|items| {
            items
                .iter()
                .map(|item| format!("{}{}", item, composable.decorated_name))
                .collect()
        });
        let items = items.as_ref();

        let source_ir = match header {
            true => &composable.header_ir,
            false => &composable.module_ir,
        };

        derived.set_shader_source(source_ir, span_offset);

        for (h, ty) in source_ir.types.iter() {
            if let Some(name) = &ty.name {
                if composable.owned_types.contains(name)
                    && items.map_or(true, |items| items.contains(name))
                {
                    derived.import_type(&h);
                }
            }
        }

        for (h, c) in source_ir.constants.iter() {
            if let Some(name) = &c.name {
                if composable.owned_constants.contains(name)
                    && items.map_or(true, |items| items.contains(name))
                {
                    derived.import_const(&h);
                }
            }
        }

        for (h, v) in source_ir.global_variables.iter() {
            if let Some(name) = &v.name {
                if composable.owned_vars.contains(name)
                    && items.map_or(true, |items| items.contains(name))
                {
                    derived.import_global(&h);
                }
            }
        }

        for (h_f, f) in source_ir.functions.iter() {
            if let Some(name) = &f.name {
                if composable.owned_functions.contains(name)
                    && (items.map_or(true, |items| items.contains(name))
                        || composable
                            .override_functions
                            .values()
                            .any(|v| v.contains(name)))
                {
                    let span = composable.module_ir.functions.get_span(h_f);
                    derived.import_function_if_new(f, span);
                }
            }
        }

        derived.clear_shader_source();
    }

    // add an import (and recursive imports) into a derived module
    fn add_import<'a>(
        &'a self,
        derived: &mut DerivedModule<'a>,
        import: &ImportDefinition,
        shader_defs: &HashMap<String, ShaderDefValue>,
        header: bool,
        already_added: &mut HashSet<String>,
    ) {
        if already_added.contains(&import.module) {
            trace!("skipping {}, already added", import.module);
            return;
        }

        let import_module_set = self.module_sets.get(&import.module).unwrap();
        let module = import_module_set.get_module(shader_defs).unwrap();

        for import in &module.imports {
            self.add_import(derived, import, shader_defs, header, already_added);
        }

        Self::add_composable_data(
            derived,
            module,
            Some(&import.items),
            import_module_set.module_index << SPAN_SHIFT,
            header,
        );
    }

    fn ensure_import(
        &mut self,
        module_set: &ComposableModuleDefinition,
        shader_defs: &HashMap<String, ShaderDefValue>,
    ) -> Result<ComposableModule, ComposerError> {
        let PreprocessOutput {
            preprocessed_source,
            imports,
        } = self
            .preprocessor
            .preprocess(&module_set.source, shader_defs, self.validate)
            .map_err(|inner| ComposerError {
                inner,
                source: ErrSource::Module {
                    name: module_set.name.to_owned(),
                    offset: 0,
                    defs: shader_defs.clone(),
                },
            })?;

        self.ensure_imports(imports.iter().map(|import| &import.definition), shader_defs)?;
        self.ensure_imports(&module_set.additional_imports, shader_defs)?;

        self.create_composable_module(
            module_set,
            Self::decorate(&module_set.name),
            shader_defs,
            true,
            true,
            &preprocessed_source,
            imports,
        )
    }

    pub(super) fn get_imported_module(&self, import: &FlattenedImport) -> Option<ModuleName> {
        let module_exists = self
            .module_sets
            .contains_key(&ModuleName(import.path.clone()));
        let splitted_module_path = import.path.rsplit_once("::");
        // TODO: Change the syntax, or add #export s so that I don't need to rely on this hack where I check "which import could be correct".
        let module = match (module_exists, splitted_module_path) {
            (true, None) => ModuleName::new(import.path.clone()),
            (true, Some((module, _item))) => {
                eprintln!("Ambiguous import: {} could refer to either a module or a function. Please use the syntax `module::function` to disambiguate.", import.path);
                ModuleName::new(module)
            }
            (false, None) => {
                return None;
            }
            (false, Some((module, _item))) => ModuleName::new(module),
        };

        Some(module)
    }

    pub(super) fn collect_all_imports(
        &self,
        entry_point: &ModuleName,
        imports: &mut HashSet<ModuleName>,
    ) -> Result<(), ComposerError> {
        let entry_module = self.module_sets.get(entry_point).unwrap();

        // TODO: Document that conditional imports are not supported. (have to be at the very top, just like #defines)
        // TODO: Verify that ^
        // Alternatively, we could support them by changing the #define semantics to no longer be global & time traveling.

        for import in entry_module.shader_imports.iter() {
            let module = match self.get_imported_module(import) {
                Some(v) => v,
                None => {
                    return Err(ComposerError {
                        inner: ComposerErrorInner::ImportNotFound(
                            import.path.to_owned(),
                            import.offset,
                        ),
                        source: ErrSource::Module {
                            name: entry_point.0.to_string(),
                            offset: 0,
                            defs: Default::default(), // TODO: Set this properly
                        },
                    });
                }
            };

            let is_new_import = imports.insert(module.clone());
            if is_new_import {
                self.collect_all_imports(&module, imports)?;
            }
        }

        for (additional_module, _) in entry_module.additional_imports.iter() {
            let is_new_import = imports.insert(additional_module.clone());
            if is_new_import {
                self.collect_all_imports(&additional_module, imports)?;
            }
        }
        Ok(())
    }

    pub(super) fn collect_shader_defs(
        &self,
        imports: &HashSet<ModuleName>,
        shader_defs: &mut HashMap<String, ShaderDefValue>,
    ) -> Result<(), ComposerError> {
        for import_name in imports {
            // TODO: No unwrap pls
            let module = self.module_sets.get(import_name).unwrap();
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

        Ok(())
    }

    pub(super) fn make_composable_module(
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

        // reject a module containing the DECORATION strings
        if let Some(decor) = self.check_decoration_regex.find(source) {
            return Err(ComposerError {
                inner: ComposerErrorInner::DecorationInSource(decor.range()),
                source: ErrSource::Constructing {
                    path: file_path.to_owned(),
                    source: source.to_owned(),
                    offset: 0,
                },
            });
        }

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
                            module: v.module.clone(),
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
