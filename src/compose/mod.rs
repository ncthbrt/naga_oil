use indexmap::{IndexMap, IndexSet};
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
/// shaders can then import the module with an `use` directive (with an optional `as` name). at point of use, imported items must be qualified:
///
/// ```ignore
/// use my_module
/// use my_other_module as Mod2
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
/// imports can be nested - modules may import other modules, but not recursively. when a new module is added, all its `use`s must already have been added.
/// the same module can be imported multiple times by different modules in the import tree.
/// there is no overlap of namespaces, so the same function names (or type, constant, or variable names) may be used in different modules.
///
/// note: when importing an item with the `#from` directive, the final shader will include the required dependencies (bindings, globals, consts, other functions) of the imported item, but will not include the rest of the imported module. it will however still include all of any modules imported by the imported module. this is probably not desired in general and may be fixed in a future version. currently for a more complete culling of unused dependencies the `prune` module can be used.
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
use naga::EntryPoint;
use preprocess::PreprocessResolver;
use regex::Regex;
use std::{
    borrow::BorrowMut,
    collections::{hash_map::Entry, BTreeMap, HashMap, HashSet},
};
use tracing::{debug, trace};

use crate::{compose::preprocess::PreprocessOutput, derive::DerivedModule, redirect::Redirector};

pub use self::error::{ComposerError, ComposerErrorInner, ErrSource};
use self::preprocess::Preprocessor;

pub mod comment_strip_iter;
pub mod error;
pub mod parse_usages;
pub mod preprocess;
mod test;
pub mod tokenizer;

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
    fn value_as_string(&self) -> String {
        match self {
            ShaderDefValue::Bool(val) => val.to_string(),
            ShaderDefValue::Int(val) => val.to_string(),
            ShaderDefValue::UInt(val) => val.to_string(),
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

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Visibility {
    #[default]
    Public,
}

// a module built with a specific set of shader_defs
#[derive(Default, Debug)]
pub struct ComposableModule {
    // module names required as imports, optionally with a list of items to import
    pub usages: IndexMap<String, UseDefinition>,
    // module exports
    pub exports: IndexMap<String, (Export, Visibility)>,
    // types exported
    pub owned_types: HashSet<String>,
    // constants exported
    pub owned_constants: HashSet<String>,
    // pipeline override exported
    pub owned_pipeline_overrides: HashSet<String>,
    // vars exported
    pub owned_vars: HashSet<String>,
    // functions exported
    pub owned_functions: HashSet<String>,
    // local functions that can be overridden
    pub virtual_functions: HashMap<String, Visibility>,
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

// data used to build a Crate
#[derive(Debug)]
pub struct CrateDefinition {
    name: String,
    // Modules defined in this crate
    modules: IndexMap<String, ResolvedCrateModule>,
}

// data used to build a ComposableModule
#[derive(Debug)]
pub struct ComposableModuleDefinition {
    pub name: String,
    pub is_crate_module: bool,
    pub module_usages: HashSet<String>,
    // shader text (with auto bindings replaced - we do this on module add as we only want to do it once to avoid burning slots)
    pub sanitized_source: String,
    // language
    pub language: ShaderLanguage,
    // source path for error display
    pub file_path: String,
    // shader def values bound to this module
    pub shader_defs: HashMap<String, ShaderDefValue>,
    // default module visibility, set when adding the module to the composer
    pub module_visibility: Option<Option<Visibility>>,
    // list of shader_defs that can affect this module
    effective_defs: Vec<String>,
    // built composable modules for a given set of shader defs
    modules: HashMap<ModuleKey, ComposableModule>,
    // used in spans when this module is included
    module_index: usize,
}

impl ComposableModuleDefinition {
    fn get_module(
        &self,
        shader_defs: &HashMap<String, ShaderDefValue>,
    ) -> Option<&ComposableModule> {
        self.modules
            .get(&ModuleKey::from_members(shader_defs, &self.effective_defs))
    }

    fn insert_module(
        &mut self,
        shader_defs: &HashMap<String, ShaderDefValue>,
        module: ComposableModule,
    ) -> &ComposableModule {
        match self
            .modules
            .entry(ModuleKey::from_members(shader_defs, &self.effective_defs))
        {
            Entry::Occupied(_) => panic!("entry already populated"),
            Entry::Vacant(v) => v.insert(module),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct UseDefinition {
    pub module: String,
    pub items: Vec<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CanonicalImportDefinition {
    pub module: String,
    pub items: IndexSet<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OverrideMode {
    Virtual,
    Static,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum UseBehaviour {
    #[default]
    ModuleSystem,
    Legacy,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Export {
    Function(OverrideMode),
    Variable,
    Constant,
    Alias,
    Struct,
    Override,
    Use(String, Option<String>, UseBehaviour),
    Module,
}

#[derive(Debug, Clone)]
pub struct UseDefWithOffset {
    definition: UseDefinition,
    offset: usize,
}

/// module composer.
/// stores any modules that can be imported into a shader
/// and builds the final shader
#[derive(Debug)]
pub struct Composer {
    pub validate: bool,
    pub module_sets: HashMap<String, ComposableModuleDefinition>,
    pub crates: HashMap<String, CrateDefinition>,
    pub module_index: HashMap<usize, String>,
    pub capabilities: naga::valid::Capabilities,
    preprocessor: Preprocessor,
    check_decoration_regex: Regex,
    undecorate_regex: Regex,
    auto_binding_regex: Regex,
    auto_binding_index: u32,
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
            module_index: Default::default(),
            preprocessor: Preprocessor::default(),
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
            auto_binding_regex: Regex::new(r"@binding\(auto\)").unwrap(),
            auto_binding_index: 0,
            crates: HashMap::new(),
        }
    }
}

pub const DECORATION_PRE: &str = "X_naga_oil_mod_X";
pub const DECORATION_POST: &str = "X";

// must be same length as DECORATION_PRE for spans to work
pub const DECORATION_OVERRIDE_PRE: &str = "X_naga_oil_vrt_X";

struct IrBuildResult {
    module: naga::Module,
    start_offset: usize,
}

fn decode(from: &str) -> String {
    String::from_utf8(data_encoding::BASE32_NOPAD.decode(from.as_bytes()).unwrap()).unwrap()
}

impl Composer {
    pub fn decorated_name(module_name: Option<&str>, item_name: &str) -> String {
        match module_name {
            Some(module_name) => format!("{}{}", item_name, Self::decorate(module_name)),
            None => item_name.to_owned(),
        }
    }

    pub fn decorate(module: &str) -> String {
        let encoded = data_encoding::BASE32_NOPAD.encode(module.as_bytes());
        format!("{DECORATION_PRE}{encoded}{DECORATION_POST}")
    }

    pub fn decorate_vrt(module: &str) -> String {
        let encoded = data_encoding::BASE32_NOPAD.encode(module.as_bytes());
        format!("{DECORATION_OVERRIDE_PRE}{encoded}{DECORATION_POST}")
    }

    /// Shorthand for creating a naga validator.
    fn create_validator(&self) -> naga::valid::Validator {
        naga::valid::Validator::new(naga::valid::ValidationFlags::all(), self.capabilities)
    }

    fn undecorate(&self, string: &str) -> String {
        let undecor = self
            .undecorate_regex
            .replace_all(string, |caps: &regex::Captures| {
                format!(
                    "{}{}::{}",
                    caps.get(1).map(|cc| cc.as_str()).unwrap_or(""),
                    decode(caps.get(3).unwrap().as_str()),
                    caps.get(2).unwrap().as_str()
                )
            });

        undecor.to_string()
    }

    fn sanitize_and_set_auto_bindings(&mut self, source: &str) -> String {
        let mut substituted_source = source.replace("\r\n", "\n").replace('\r', "\n");
        if !substituted_source.ends_with('\n') {
            substituted_source.push('\n');
        }

        // replace @binding(auto) with an incrementing index
        struct AutoBindingReplacer<'a> {
            auto: &'a mut u32,
        }

        impl<'a> regex::Replacer for AutoBindingReplacer<'a> {
            fn replace_append(&mut self, _: &regex::Captures<'_>, dst: &mut String) {
                dst.push_str(&format!("@binding({})", self.auto));
                *self.auto += 1;
            }
        }

        let substituted_source = self.auto_binding_regex.replace_all(
            &substituted_source,
            AutoBindingReplacer {
                auto: &mut self.auto_binding_index,
            },
        );

        substituted_source.into_owned()
    }

    fn naga_to_string(
        &self,
        naga_module: &mut naga::Module,
        language: ShaderLanguage,
        #[allow(unused)] header_for: &str, // Only used when GLSL is enabled
    ) -> Result<String, ComposerErrorInner> {
        // TODO: cache headers again
        let info = self
            .create_validator()
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

                let info = self
                    .create_validator()
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
        usages: &[UseDefinition],
        shader_defs: &HashMap<String, ShaderDefValue>,
    ) -> Result<IrBuildResult, ComposerError> {
        debug!("creating IR for {} with defs: {:?}", name, shader_defs);

        let mut module_string = match language {
            ShaderLanguage::Wgsl => String::new(),
            #[cfg(feature = "glsl")]
            ShaderLanguage::Glsl => String::from("#version 450\n"),
        };

        let mut header_module = DerivedModule::default();
        let mut already_added = Default::default();
        for usage in usages {
            // add to header module
            self.add_usage(
                &mut header_module,
                usage,
                shader_defs,
                Some(usage.module.clone()),
                true,
                &mut already_added,
            );
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
        })
    }

    // build a ComposableModule from a ComposableModuleDefinition, for a given set of shader defs
    // - build the naga IR (against headers)
    // - record any types/vars/constants/functions that are defined within this module
    // - build headers for each supported language
    #[allow(clippy::too_many_arguments)]
    fn create_composable_module(
        &mut self,
        module_definition: &ComposableModuleDefinition,
        shader_defs: &HashMap<String, ShaderDefValue>,
        demote_entrypoints: bool,
        preprocess_output: &PreprocessOutput,
        module_mappings: &mut HashMap<String, IndexMap<String, Option<Visibility>>>,
    ) -> Result<ComposableModule, ComposerError> {
        let usages: Vec<_> = preprocess_output
            .usages
            .iter()
            .map(|import_with_offset| import_with_offset.definition.clone())
            .collect();

        if let Some(visibility) = module_definition.module_visibility {
            module_mappings
                .entry(module_definition.name.to_string())
                .or_default()
                .insert(module_definition.name.to_string(), visibility);
        }

        trace!(
            "create composable module {}: source len {}",
            module_definition.name,
            &preprocess_output.preprocessed_source.len()
        );

        let source = preprocess_output.preprocessed_source.clone();

        let virtual_functions: HashMap<String, Visibility> = preprocess_output
            .exports
            .iter()
            .filter(|(_, (export, _))| matches!(export, Export::Function(OverrideMode::Virtual)))
            .map(|(symbol, (_, visibility))| (symbol.to_string(), *visibility))
            .collect();

        trace!(
            "create composable module {}: source len {}",
            module_definition.name,
            source.len()
        );

        let IrBuildResult {
            module: mut source_ir,
            start_offset,
        } = self.create_module_ir(
            &module_definition.name,
            source.to_string(),
            module_definition.language,
            &usages,
            shader_defs,
        )?;

        // rename and record owned items (except types which can't be mutably accessed)
        let mut owned_constants = IndexMap::new();
        for (h, c) in source_ir.constants.iter_mut() {
            if let Some(name) = c.name.as_mut() {
                if !name.contains(DECORATION_PRE) {
                    // *name = format!("{name}{module_decoration}");
                    owned_constants.insert(name.clone(), h);
                }
            }
        }

        // These are naga/wgpu's pipeline override constants, not naga_oil's overrides
        let mut owned_pipeline_overrides = IndexMap::new();
        for (h, po) in source_ir.overrides.iter_mut() {
            if let Some(name) = po.name.as_mut() {
                if !name.contains(DECORATION_PRE) {
                    // *name = format!("{name}{module_decoration}");
                    owned_pipeline_overrides.insert(name.clone(), h);
                }
            }
        }

        let mut owned_vars = IndexMap::new();
        for (h, gv) in source_ir.global_variables.iter_mut() {
            if let Some(name) = gv.name.as_mut() {
                if !name.contains(DECORATION_PRE) {
                    // *name = format!("{name}{module_decoration}");
                    owned_vars.insert(name.clone(), h);
                }
            }
        }

        let mut owned_functions = IndexMap::new();
        for (h_f, f) in source_ir.functions.iter_mut() {
            if let Some(name) = f.name.as_mut() {
                if !name.contains(DECORATION_PRE) {
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
                ep.function.name = Some(ep.function.name.as_deref().unwrap_or("main").to_string());
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
                    // let name = format!("{name}{module_decoration}");
                    owned_types.insert(name.clone());
                    // copy types
                    header_builder.import_type(&h);
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

        for h in owned_pipeline_overrides.values() {
            header_builder.import_pipeline_override(h);
            module_builder.import_pipeline_override(h);
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
        let header_ir: naga::Module = header_builder.into();

        let composable_module = ComposableModule {
            usages: usages.into_iter().map(|x| (x.module.clone(), x)).collect(),
            exports: preprocess_output.exports.clone(),
            owned_types,
            owned_pipeline_overrides: owned_pipeline_overrides.into_keys().collect(),
            owned_constants: owned_constants.into_keys().collect(),
            owned_vars: owned_vars.into_keys().collect(),
            owned_functions: owned_functions.into_keys().collect(),
            virtual_functions,
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
        module_name: Option<String>,
    ) {
        let module_decoration = module_name.map(|x: String| Self::decorate(&x));

        let items: Option<HashSet<String>> = items.map(|items| items.iter().cloned().collect());
        let items = items.as_ref();
        let source_ir = match header {
            true => &composable.header_ir,
            false => &composable.module_ir,
        };

        derived.set_shader_source(source_ir, span_offset);

        for (h, ty) in source_ir.types.iter() {
            if let Some(name) = &ty.name {
                // We need to add all the types for now, even if they weren't specified in items
                // This is because functions may depend of them
                if composable.owned_types.contains(name) {
                    let new_name = module_decoration.as_ref().map(|x| format!("{}{}", name, x));
                    derived.rename_type(&h, new_name);
                }
            }
        }

        for (h, c) in source_ir.constants.iter() {
            if let Some(name) = &c.name {
                // We need to add all the constants for now, even if they weren't specified in items
                // This is because functions may depend of them
                if composable.owned_constants.contains(name) {
                    let new_name = module_decoration.as_ref().map(|x| format!("{}{}", name, x));
                    derived.rename_const(&h, new_name);
                }
            }
        }

        for (h, po) in source_ir.overrides.iter() {
            if let Some(name) = &po.name {
                // We need to add all the overrides for now, even if they weren't specified in items
                // This is because functions may depend of them
                if composable.owned_pipeline_overrides.contains(name) {
                    let new_name = module_decoration.as_ref().map(|x| format!("{}{}", name, x));
                    derived.rename_pipeline_override(&h, new_name);
                }
            }
        }

        for (h, v) in source_ir.global_variables.iter() {
            // We need to add all the vars for now, even if they weren't specified in items
            // This is because functions may depend of them
            if let Some(name) = &v.name {
                if composable.owned_vars.contains(name) {
                    let new_name = module_decoration.as_ref().map(|x| format!("{}{}", name, x));
                    derived.rename_global(&h, new_name);
                }
            }
        }

        for (h_f, f) in source_ir.functions.iter() {
            if let Some(name) = &f.name {
                // We need to add all the vars for now, even if they weren't specified in items
                // This is because other functions may depend of them
                if composable.owned_functions.contains(name) {
                    // TODO Evaluate visibility properly
                    let span: naga::Span = composable.module_ir.functions.get_span(h_f);
                    let new_name = module_decoration.as_ref().map(|x| format!("{}{}", name, x));
                    let mut f = f.clone();
                    f.name = Some(new_name.unwrap_or_else(|| name.to_string()));
                    derived.import_function_if_new(&f, span);
                }
            }
        }

        derived.clear_shader_source();
    }

    // add a usage (and recursive usages) into a derived module
    fn add_usage<'a>(
        &'a self,
        derived: &mut DerivedModule<'a>,
        use_definition: &UseDefinition,
        shader_defs: &HashMap<String, ShaderDefValue>,
        module_name: Option<String>,
        header: bool,
        already_added: &mut HashSet<String>,
    ) {
        // TODO: Should also test items for addition!
        if already_added.contains(&use_definition.module) {
            trace!("skipping {}, already added", use_definition.module);
            return;
        }

        let usage_module_set = self.module_sets.get(&use_definition.module).unwrap();
        let module = usage_module_set.get_module(shader_defs).unwrap();

        for usage in module.usages.values() {
            self.add_usage(
                derived,
                usage,
                shader_defs,
                Some(usage.module.clone()),
                header,
                already_added,
            );
        }

        Self::add_composable_data(
            derived,
            module,
            Some(&use_definition.items),
            usage_module_set.module_index << SPAN_SHIFT,
            header,
            module_name,
        );
    }

    #[allow(clippy::type_complexity)]
    fn build_usage(
        &mut self,
        module_set: &ComposableModuleDefinition,
        preprocess_outputs: &HashMap<Option<String>, PreprocessOutput>,
        shader_defs: &HashMap<String, ShaderDefValue>,
        module_exports: &mut HashMap<String, IndexMap<String, Vec<(Export, Visibility)>>>,
        module_mappings: &mut HashMap<String, IndexMap<String, Option<Visibility>>>,
    ) -> Result<ComposableModule, ComposerError> {
        let preprocess_output = preprocess_outputs
            .get(&Some(module_set.name.clone()))
            .expect("Expected to have errored before reaching this point");

        let extension_usages = preprocess_output
            .extensions
            .iter()
            .map(|extension| UseDefinition {
                module: extension.to_string(),
                items: vec![],
            })
            .collect::<Vec<UseDefinition>>();

        self.build_usages(
            preprocess_output
                .usages
                .iter()
                .map(|usage| &usage.definition),
            shader_defs,
            preprocess_outputs,
            module_exports,
            module_mappings,
        )?;
        self.build_usages(
            &extension_usages,
            shader_defs,
            preprocess_outputs,
            module_exports,
            module_mappings,
        )?;

        self.create_composable_module(
            module_set,
            shader_defs,
            true,
            preprocess_output,
            module_mappings,
        )
    }

    // build required ComposableModules for a given set of shader_defs
    #[allow(clippy::type_complexity)]
    fn build_usages<'a>(
        &mut self,
        usages: impl IntoIterator<Item = &'a UseDefinition>,
        shader_defs: &HashMap<String, ShaderDefValue>,
        preprocess_outputs: &HashMap<Option<String>, PreprocessOutput>,
        module_exports: &mut HashMap<String, IndexMap<String, Vec<(Export, Visibility)>>>,
        module_mappings: &mut HashMap<String, IndexMap<String, Option<Visibility>>>,
    ) -> Result<(), ComposerError> {
        let usages = usages.into_iter().cloned().collect::<Vec<UseDefinition>>();

        for UseDefinition { module, .. } in usages.into_iter() {
            // we've already ensured usages exist when they were added

            let module_set = self.module_sets.get(&module).unwrap();
            if module_set.get_module(shader_defs).is_some() {
                continue;
            }
            // we need to build the module
            // take the set so we can recurse without borrowing
            let (set_key, mut module_set) = self.module_sets.remove_entry(&module).unwrap();

            match self.build_usage(
                &module_set,
                &preprocess_outputs,
                shader_defs,
                module_exports,
                module_mappings,
            ) {
                Ok(module) => {
                    module_set.insert_module(shader_defs, module);
                    self.module_sets.insert(set_key, module_set);
                }
                Err(e) => {
                    self.module_sets.insert(set_key, module_set);
                    return Err(e);
                }
            }
        }

        Ok(())
    }
}

pub struct ComposableModuleDescriptor<'a> {
    pub source: &'a str,
    pub file_path: &'a str,
    pub language: ShaderLanguage,
    pub as_name: Option<String>,
    pub shader_defs: HashMap<String, ShaderDefValue>,
    pub module_visibility: Option<Option<Visibility>>,
}

impl<'a> Default for ComposableModuleDescriptor<'a> {
    fn default() -> Self {
        Self {
            source: Default::default(),
            file_path: Default::default(),
            language: ShaderLanguage::Wgsl,
            as_name: None,
            shader_defs: Default::default(),
            module_visibility: Some(Some(Visibility::Public)),
        }
    }
}

#[derive(Debug, Default)]
pub struct CrateRootDescriptor<'a> {
    pub root_source: &'a str,
    pub root_file_path: String,
    pub language: ShaderLanguage,
    pub crate_name: String,
    pub shader_defs: HashMap<String, ShaderDefValue>,
}

#[derive(Debug, Default)]
pub struct CrateSubmoduleDescriptor {
    pub parent: String,
    pub submodule_name: String,
    pub source: String,
    pub file_path: String,
    pub language: ShaderLanguage,
}

pub struct CrateResolver<'a> {
    composer: &'a mut Composer,
    crate_name: String,
    shader_defs: HashMap<String, ShaderDefValue>,
    resolved_modules: IndexMap<String, ResolvedCrateModule>,
    unresolved_modules: IndexMap<String, UnresolvedCrateModule>,
}

#[derive(Debug, Clone)]
struct ResolvedCrateModule {
    module_path: String,
    source: String,
    file_path: String,
    language: ShaderLanguage,
    all_submodules: IndexSet<String>,
}

#[derive(Debug)]
struct CrateModuleDefinition {
    module_name: String,
    file_path: String,
    language: ShaderLanguage,
    all_submodules: IndexSet<String>,
}

#[derive(Debug)]
struct UnresolvedCrateModule {
    parent: Option<String>,
    submodule_name: String,
    source: String,
    file_path: String,
    language: ShaderLanguage,
}

#[derive(Debug)]
pub struct ResolvedCrate {
    crate_name: String,
    modules: IndexMap<String, ResolvedCrateModule>,
    shader_defs: HashMap<String, ShaderDefValue>,
}

#[derive(Debug)]
pub struct MissingSubmodule {
    /// Full crate relative name of the parent module (e.g. crate_name::xyz)
    pub parent: String,
    /// Submodule name relative to the parent (e.g. abc)
    pub submodule_name: String,
}

#[derive(Debug)]
pub enum CrateResolutionMiss {
    MissingSubmodules(Vec<MissingSubmodule>),
}

impl<'a> CrateResolver<'a> {
    pub fn add_submodule(&mut self, desc: CrateSubmoduleDescriptor) -> Result<(), ComposerError> {
        // TODO: Add some validation here to test that the parent follows the crate naming scheme
        // and that the submodule hasn't already been added
        let CrateSubmoduleDescriptor {
            parent,
            submodule_name,
            source,
            file_path,
            language,
        } = desc;

        self.unresolved_modules.insert(
            format!("{}::{}", parent, submodule_name),
            UnresolvedCrateModule {
                parent: Some(parent),
                submodule_name,
                source,
                file_path,
                language,
            },
        );
        Ok(())
    }

    pub fn next(&mut self) -> Result<CrateResolutionStep, ComposerError> {
        while let Some((module_name, module)) = self.unresolved_modules.pop() {
            let submodules = self
                .composer
                .preprocessor
                .get_submodules(&module.source, module.language);
            let module_path = module_name.clone();
            self.resolved_modules.insert(
                module_path.clone(),
                ResolvedCrateModule {
                    module_path: module_path.clone(),
                    source: module.source,
                    file_path: module.file_path,
                    language: module.language,
                    all_submodules: submodules.clone(),
                },
            );
            return Ok(CrateResolutionStep::Miss(
                CrateResolutionMiss::MissingSubmodules(
                    submodules
                        .into_iter()
                        .map(|x: String| MissingSubmodule {
                            parent: module_name.clone(),
                            submodule_name: x,
                        })
                        .collect(),
                ),
            ));
        }
        Ok(CrateResolutionStep::Done(ResolvedCrate {
            modules: self.resolved_modules.clone(),
            crate_name: self.crate_name.clone(),
            shader_defs: self.shader_defs.clone(),
        }))
    }
}

#[derive(Debug)]
pub enum CrateResolutionStep {
    Done(ResolvedCrate),
    Miss(CrateResolutionMiss),
}

#[derive(Default)]
pub struct NagaModuleDescriptor<'a> {
    pub source: &'a str,
    pub file_path: &'a str,
    pub shader_type: ShaderType,
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
    /// See https://github.com/gfx-rs/wgpu/blob/d9c054c645af0ea9ef81617c3e762fbf0f3fecda/wgpu-core/src/device/mod.rs#L515
    /// for how to set the subgroup_stages value.
    pub fn with_capabilities(self, capabilities: naga::valid::Capabilities) -> Self {
        Self {
            capabilities,
            validate: self.validate,
            ..Default::default()
        }
    }

    /// check if a module with the given name has been added
    pub fn contains_module(&self, module_name: &str) -> bool {
        self.module_sets.contains_key(module_name)
    }

    /// starts adding a composable crate to the composer.
    /// all modules/crates required by the root module must already have been added, with the exception of submodules
    /// returns a CreateResolver which can be used to build the final crate
    pub fn start_crate<'a>(
        &'a mut self,
        desc: CrateRootDescriptor<'a>,
    ) -> Result<CrateResolver<'a>, ComposerError> {
        let CrateRootDescriptor {
            root_source,
            root_file_path,
            language,
            crate_name,
            shader_defs,
        } = desc;
        Ok(CrateResolver {
            crate_name: crate_name.clone(),
            shader_defs,
            composer: self,
            resolved_modules: Default::default(),
            unresolved_modules: IndexMap::from([(
                crate_name.clone(),
                UnresolvedCrateModule {
                    parent: None,
                    submodule_name: crate_name.clone(),
                    source: root_source.to_string(),
                    file_path: root_file_path.to_string(),
                    language,
                },
            )]),
        })
    }

    pub fn add_resolved_crate(
        &mut self,
        resolved_crate: ResolvedCrate,
    ) -> Result<(), ComposerError> {
        let ResolvedCrate {
            crate_name,
            modules,
            shader_defs,
        } = resolved_crate;
        self.crates.insert(
            crate_name.to_owned(),
            CrateDefinition {
                name: crate_name.to_owned(),
                modules: modules.clone(),
            },
        );
        for (module_path, module) in modules.iter() {
            // can't gracefully report errors for more modules. perhaps this should be a warning
            assert!((self.module_sets.len() as u32) < u32::MAX >> SPAN_SHIFT);

            let module_index = self.module_sets.len() + 1;
            let module_set: ComposableModuleDefinition = ComposableModuleDefinition {
                is_crate_module: true,
                name: module_path.clone(),
                sanitized_source: module.source.clone(),
                file_path: module.file_path.to_owned(),
                language: module.language,
                shader_defs: shader_defs.clone(),
                // Only the root module has a default visibility. The other modules visibility will be resolved during
                // preprocessing
                module_visibility: if &crate_name == module_path {
                    Some(Some(Visibility::Public))
                } else {
                    Some(None)
                },
                module_index,
                // These will all be initialized later
                effective_defs: Default::default(),
                modules: Default::default(),
                module_usages: Default::default(),
            };
            // invalidate dependent modules if this module already exists
            self.remove_composable_module(&module_path);
            self.module_sets.insert(module_path.clone(), module_set);
            self.module_index.insert(module_index, module_path.clone());
        }

        let root_module = modules.get(&crate_name).unwrap();
        let base_resolver = self.preprocessor.preprocess(
            &root_module.source,
            None,
            root_module.language,
            Some(crate_name.clone()),
            Some(crate_name.clone()),
            root_module.file_path.clone(),
        );
        let mut module_exports = HashMap::default();
        let mut module_mappings: HashMap<String, IndexMap<String, Option<Visibility>>> =
            HashMap::default();
        let mut module_wildcards = HashMap::default();
        let mut preprocess_outputs = HashMap::default();

        module_mappings
            .entry(crate_name.clone())
            .or_default()
            .insert(crate_name.clone(), Some(Visibility::Public));

        let PreprocessOutput {
            shader_defs,
            effective_defs,
            ..
        } = self.preprocess_dependency_tree(
            base_resolver,
            None,
            &mut preprocess_outputs,
            &mut module_exports,
            &mut module_mappings,
            &mut module_wildcards,
        )?;

        for (module_path, _) in modules {
            let module_set = self
                .module_sets
                .get_mut(&module_path)
                .expect("Module to have been added");
            let PreprocessOutput { usages, .. } = preprocess_outputs
                .remove(&Some(module_path.to_string()))
                .expect(&format!(
                    "Expected module {} to have been included in preprocess output",
                    module_path
                ));
            module_set.effective_defs = effective_defs.iter().cloned().collect();
            module_set.shader_defs = shader_defs.clone();
            module_set.module_usages = usages.into_iter().map(|x| x.definition.module).collect();
        }

        Ok(())
    }

    /// add a composable module to the composer.
    /// all modules imported by this module must already have been added
    pub fn add_composable_module(
        &mut self,
        desc: ComposableModuleDescriptor,
    ) -> Result<&ComposableModuleDefinition, ComposerError> {
        let ComposableModuleDescriptor {
            source,
            file_path,
            language,
            as_name,
            mut shader_defs,
            module_visibility,
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

        let substituted_source = self.sanitize_and_set_auto_bindings(source);

        let base_resolver = self.preprocessor.preprocess(
            &substituted_source,
            None,
            language,
            None,
            as_name.clone(),
            file_path.to_string(),
        );

        let mut module_exports = HashMap::default();
        let mut module_mappings = HashMap::default();
        let mut module_wildcards = HashMap::default();
        let mut preprocess_outputs = HashMap::default();
        let PreprocessOutput {
            usages,
            module_name,
            extensions,
            shader_defs: defines,
            mut effective_defs,
            ..
        } = self
            .preprocess_dependency_tree(
                base_resolver,
                None,
                &mut preprocess_outputs,
                &mut module_exports,
                &mut module_mappings,
                &mut module_wildcards,
            )
            .map_err(|mut err| match &err.source {
                ErrSource::Module { name, offset, .. } if name == "" => {
                    err.source = ErrSource::Constructing {
                        path: file_path.to_string(),
                        source: substituted_source.clone(),
                        offset: *offset,
                    };
                    err
                }
                _ => err,
            })?;

        if module_name.is_none() {
            return Err(ComposerError {
                inner: ComposerErrorInner::NoModuleName,
                source: ErrSource::Constructing {
                    path: file_path.to_owned(),
                    source: source.to_owned(),
                    offset: 0,
                },
            });
        }
        shader_defs.extend(defines);
        let usages = usages
            .into_iter()
            .map(|x| x.definition.module)
            .chain(extensions)
            .collect();

        let module_name = module_name.unwrap();
        debug!(
            "adding module definition for {} with defs: {:?}",
            module_name, shader_defs
        );

        // remove defs that are already specified through our imports
        effective_defs.retain(|name| !shader_defs.contains_key(name));
        // can't gracefully report errors for more modules. perhaps this should be a warning
        assert!((self.module_sets.len() as u32) < u32::MAX >> SPAN_SHIFT);
        let module_index = self.module_sets.len() + 1;
        let module_set: ComposableModuleDefinition = ComposableModuleDefinition {
            is_crate_module: false,
            name: module_name.clone(),
            sanitized_source: substituted_source,
            file_path: file_path.to_owned(),
            language,
            effective_defs: effective_defs.into_iter().collect(),
            shader_defs,
            module_index,
            modules: Default::default(),
            module_visibility,
            module_usages: usages,
        };

        // invalidate dependent modules if this module already exists
        self.remove_composable_module(&module_name);

        self.module_sets.insert(module_name.clone(), module_set);
        self.module_index.insert(module_index, module_name.clone());
        Ok(self.module_sets.get(&module_name).unwrap())
    }

    /// remove a composable module. also removes modules that depend on this module, as we cannot be sure about
    /// the completeness of their effective shader defs any more...
    pub fn remove_composable_module(&mut self, module_name: &str) {
        // todo this could be improved by making effective defs an Option<HashSet> and populating on demand?
        let mut dependent_sets = Vec::new();

        if self.module_sets.remove(module_name).is_some() {
            dependent_sets.extend(self.module_sets.iter().filter_map(|(dependent_name, set)| {
                if set.module_usages.contains(module_name) {
                    Some(dependent_name.clone())
                } else {
                    None
                }
            }));
        }

        for dependent_set in dependent_sets {
            self.remove_composable_module(&dependent_set);
        }
    }

    fn get_visibility_unsupported_modules(&self) -> HashSet<String> {
        self.module_sets
            .iter()
            .filter_map(|(k, module)| match module.language {
                ShaderLanguage::Wgsl => None,
                ShaderLanguage::Glsl => Some(k.to_string()),
            })
            .collect()
    }

    #[allow(clippy::type_complexity)]
    fn preprocess_dependency_tree(
        &mut self,
        base_resolver: PreprocessResolver,
        shader_defs: Option<HashMap<String, ShaderDefValue>>,
        preprocess_outputs: &mut HashMap<Option<String>, PreprocessOutput>,
        module_exports: &mut HashMap<String, IndexMap<String, Vec<(Export, Visibility)>>>,
        module_mappings: &mut HashMap<String, IndexMap<String, Option<Visibility>>>,
        module_wildcards: &mut HashMap<String, IndexSet<(String, Option<Visibility>)>>,
    ) -> Result<PreprocessOutput, ComposerError> {
        let mut total_shader_defs: HashMap<String, ShaderDefValue> =
            shader_defs.clone().unwrap_or_default();
        let mut total_effective_defs: HashSet<String> = HashSet::default();

        let mut modules_in_stack = HashSet::new();

        let visibility_unsupported = self.get_visibility_unsupported_modules();

        let mut resolver_stack = IndexMap::new();
        let base_crate_name: Option<String> = base_resolver.crate_name().clone();
        let base_key = if let Some(base_crate_name) = &base_crate_name {
            modules_in_stack.insert(base_crate_name.to_owned());
            resolver_stack.insert(base_crate_name.to_string(), base_resolver);
            base_crate_name.clone()
        } else {
            // Forces the base resolver be anonymous and immovable.
            resolver_stack.insert("".to_string(), base_resolver);
            "".to_string()
        };

        loop {
            let (current_resolver_name, mut current_resolver) = resolver_stack.pop().unwrap();
            let next = current_resolver.next(
                module_exports,
                module_mappings,
                module_wildcards,
                &visibility_unsupported,
            )?;
            match next {
                preprocess::PreprocessStep::Done(mut result) => {
                    if resolver_stack.is_empty() {
                        result.shader_defs.extend(total_shader_defs);
                        result.effective_defs.extend(total_effective_defs);
                        if let Some(base_crate_name) = &base_crate_name {
                            preprocess_outputs
                                .insert(Some(base_crate_name.clone()), *result.clone());
                        } else {
                            preprocess_outputs.insert(None, *result.clone());
                        }
                        // Testing that the right preprocess output is returned
                        debug_assert_eq!(base_key, current_resolver_name);
                        return Ok(*result.clone());
                    } else if let Some(module_set) =
                        self.module_sets.remove(current_resolver.module_name())
                    {
                        if let Some(default_visiblity) = &module_set.module_visibility {
                            module_mappings
                                .entry(module_set.name.to_string())
                                .or_default()
                                .insert(module_set.name.to_string(), default_visiblity.clone());
                        }

                        preprocess_outputs.insert(result.module_name.clone(), *result.clone());
                        self.module_sets
                            .insert(current_resolver.module_name().to_string(), module_set);
                        if current_resolver_name == base_key {
                            // Insert the base resolver at the bottom of the stack
                            // Assertion to check that there isn't two of the same resolver in the stack
                            assert!(matches!(
                                resolver_stack.shift_insert(
                                    0,
                                    current_resolver_name.clone(),
                                    current_resolver
                                ),
                                None
                            ));
                        }
                    } else {
                        panic!("ModuleSet {} Not Found", current_resolver.module_name());
                    }
                }
                preprocess::PreprocessStep::Miss(preprocess::PreprocessMiss::MissingModules(
                    modules,
                )) => {
                    // Assertion to check that there isn't two of the same resolver in the stack
                    assert!(matches!(
                        resolver_stack.insert(current_resolver_name.clone(), current_resolver),
                        None
                    ));
                    for (module, offset) in modules {
                        let (maybe_crate_name, _) =
                            module.split_once("::").unwrap_or_else(|| (&module, ""));
                        let part_of_crate = self.crates.contains_key(maybe_crate_name);
                        let mut crate_root_in_stack = false;
                        if part_of_crate {
                            crate_root_in_stack = modules_in_stack.contains(maybe_crate_name);
                        }
                        if part_of_crate && !crate_root_in_stack {
                            // If is part of a crate, we need to start at the module root to resolve this dependency
                            if let Some(module_set) = self.module_sets.get_mut(maybe_crate_name) {
                                total_shader_defs.extend(module_set.shader_defs.clone());
                                total_effective_defs.extend(module_set.effective_defs.clone());
                                modules_in_stack.insert(maybe_crate_name.to_string());
                                let resolver = self.preprocessor.preprocess(
                                    &module_set.sanitized_source,
                                    shader_defs.as_ref().map(|_| {
                                        let mut clone = total_shader_defs.clone();
                                        clone.extend(module_set.shader_defs.clone());
                                        clone
                                    }),
                                    module_set.language,
                                    Some(maybe_crate_name.to_string()),
                                    Some(maybe_crate_name.to_string()),
                                    module_set.file_path.clone(),
                                );
                                // Assertion to check that the crate root hasn't been added twice
                                assert!(matches!(
                                    resolver_stack.insert(maybe_crate_name.to_string(), resolver),
                                    None
                                ));
                                continue;
                            } else {
                                return Err(ComposerError {
                                    inner: ComposerErrorInner::ModuleNotFound(
                                        maybe_crate_name.to_string(),
                                        offset,
                                    ),
                                    source: ErrSource::Module {
                                        name: current_resolver_name.to_string(),
                                        offset,
                                        defs: total_shader_defs.clone(),
                                    },
                                });
                            }
                        } else if let Some(module_set) = self.module_sets.get_mut(&module) {
                            if !modules_in_stack.contains(&module) {
                                modules_in_stack.insert(module.to_string());
                                total_shader_defs.extend(module_set.shader_defs.clone());
                                total_effective_defs.extend(module_set.effective_defs.clone());
                                let resolver = self.preprocessor.preprocess(
                                    &module_set.sanitized_source,
                                    shader_defs.as_ref().map(|_| {
                                        let mut clone = total_shader_defs.clone();
                                        clone.extend(module_set.shader_defs.clone());
                                        clone
                                    }),
                                    module_set.language,
                                    if part_of_crate {
                                        Some(maybe_crate_name.to_string())
                                    } else {
                                        None
                                    },
                                    Some(module_set.name.to_string()),
                                    module_set.file_path.clone(),
                                );

                                // Assertion to check that the resolver hasn't been added
                                // twice
                                assert!(matches!(
                                    resolver_stack.insert(module.to_string(), resolver),
                                    None
                                ));
                                continue;
                            } else if !part_of_crate
                                || !current_resolver_name.starts_with(maybe_crate_name)
                            {
                                return Err(ComposerError {
                                    // TODO: Replace with cyclic dependency warning
                                    inner: ComposerErrorInner::ModuleNotFound(module, offset),
                                    source: ErrSource::Module {
                                        name: current_resolver_name.to_string(),
                                        offset,
                                        defs: shader_defs.clone().unwrap_or_default(),
                                    },
                                });
                            } else {
                                // Relocate the dependency to the top of the stack.
                                // This shouldn't result in an infinite loop
                                // as wildcards are evaluated using a set
                                // However it may be a good idea to think
                                // about how to prove that invariant
                                resolver_stack.move_index(
                                    resolver_stack
                                        .get_index_of(&module)
                                        .expect("EXPECTED VALID INDEX"),
                                    resolver_stack.len() - 1,
                                );
                                continue;
                            }
                        } else {
                            return Err(ComposerError {
                                inner: ComposerErrorInner::ModuleNotFound(module, offset),
                                source: ErrSource::Module {
                                    name: current_resolver_name.to_string(),
                                    offset,
                                    defs: shader_defs.clone().unwrap_or_default(),
                                },
                            });
                        }
                    }
                }
            }
        }
    }

    /// build a naga shader module
    pub fn make_naga_module(
        &mut self,
        desc: NagaModuleDescriptor,
    ) -> Result<naga::Module, ComposerError> {
        let NagaModuleDescriptor {
            source,
            file_path,
            shader_type,
            shader_defs,
        } = desc;
        let sanitized_source = self.sanitize_and_set_auto_bindings(source);

        let mut module_exports = HashMap::default();
        let mut module_mappings = HashMap::default();
        let mut module_wildcards = HashMap::default();
        let mut preprocess_outputs = HashMap::default();

        let base_preprocessor_resolver = self.preprocessor.preprocess(
            &sanitized_source,
            Some(shader_defs.clone()),
            shader_type.into(),
            None,
            None,
            file_path.to_string(),
        );
        let preprocess_output = self
            .preprocess_dependency_tree(
                base_preprocessor_resolver,
                Some(shader_defs.clone()),
                &mut preprocess_outputs,
                &mut module_exports,
                &mut module_mappings,
                &mut module_wildcards,
            )
            .map_err(|mut err| match &err.source {
                ErrSource::Module { name, offset, .. } if name == "" => {
                    err.source = ErrSource::Constructing {
                        path: file_path.to_string(),
                        source: sanitized_source.clone(),
                        offset: *offset,
                    };
                    err
                }
                _ => err,
            })?;

        let usages: Vec<UseDefinition> = preprocess_output
            .usages
            .iter()
            .map(|usage| &usage.definition)
            .cloned()
            .collect();

        self.build_usages(
            &usages,
            &preprocess_output.shader_defs,
            &preprocess_outputs,
            &mut module_exports,
            &mut module_mappings,
        )?;

        let extensions: Vec<UseDefinition> = preprocess_output
            .extensions
            .iter()
            .map(|module| UseDefinition {
                module: module.to_string(),
                items: vec![],
            })
            .collect();

        self.build_usages(
            &extensions,
            &preprocess_output.shader_defs,
            &preprocess_outputs,
            &mut module_exports,
            &mut module_mappings,
        )?;

        let definition = ComposableModuleDefinition {
            name: "".to_string(),
            sanitized_source: sanitized_source.clone(),
            language: shader_type.into(),
            file_path: file_path.to_owned(),
            module_index: 0,
            // we don't care about these for creating a top-level module
            effective_defs: Default::default(),
            shader_defs: Default::default(),
            modules: Default::default(),
            module_visibility: Some(Some(Visibility::Public)),
            is_crate_module: false,
            module_usages: usages
                .into_iter()
                .chain(extensions)
                .map(|x| x.module)
                .collect(),
        };
        let composable = self
            .create_composable_module(
                &definition,
                &preprocess_output.shader_defs,
                false,
                &preprocess_output,
                &mut module_mappings,
            )
            .map_err(|e| ComposerError {
                inner: e.inner,
                source: ErrSource::Constructing {
                    path: definition.file_path.to_owned(),
                    source: sanitized_source.clone(),
                    offset: e.source.offset(),
                },
            })?;

        let mut derived = DerivedModule::default();

        let mut already_added = Default::default();

        for usage in composable.usages.values() {
            self.add_usage(
                &mut derived,
                usage,
                &preprocess_output.shader_defs,
                Some(usage.module.to_owned()),
                false,
                &mut already_added,
            );
        }

        Self::add_composable_data(&mut derived, &composable, None, 0, false, None);

        let stage = match shader_type {
            #[cfg(feature = "glsl")]
            ShaderType::GlslVertex => Some(naga::ShaderStage::Vertex),
            #[cfg(feature = "glsl")]
            ShaderType::GlslFragment => Some(naga::ShaderStage::Fragment),
            _ => None,
        };

        let mut entry_points = Vec::default();
        derived.set_shader_source(&composable.module_ir, 0);
        for ep in &composable.module_ir.entry_points {
            if let Some((Export::Function(_), _)) = composable.exports.get(&ep.name) {
                let mapped_func = derived.localize_function(&ep.function);
                entry_points.push(EntryPoint {
                    name: ep.name.clone(),
                    function: mapped_func,
                    stage: stage.unwrap_or(ep.stage),
                    early_depth_test: ep.early_depth_test,
                    workgroup_size: ep.workgroup_size,
                });
            }
        }

        let naga_module = naga::Module {
            entry_points,
            ..derived.into()
        };

        // validation
        if self.validate {
            let info = self.create_validator().validate(&naga_module);
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
                                    source: sanitized_source.clone(),
                                    offset: composable.start_offset,
                                },
                                _ => {
                                    let module_name =
                                        self.module_index.get(&module_index).unwrap().clone();
                                    let offset = self
                                        .module_sets
                                        .get(&module_name)
                                        .unwrap()
                                        .get_module(&preprocess_output.shader_defs)
                                        .unwrap()
                                        .start_offset;
                                    ErrSource::Module {
                                        name: module_name,
                                        offset,
                                        defs: preprocess_output.shader_defs.clone(),
                                    }
                                }
                            }
                        }
                        None => ErrSource::Constructing {
                            path: file_path.to_owned(),
                            source: sanitized_source.clone(),
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
