use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    sync::Arc,
};

use indexmap::{IndexMap, IndexSet};
use regex::{Match, Regex};

use crate::compose::OverrideMode;

use super::{
    comment_strip_iter::CommentReplaceExt,
    parse_usages::{
        self, canonicalize_usage, flatten_wildcard_use, parse_uses,
        substitute_with_canonical_identifiers, CanonicalizationFault, UsageFault,
    },
    Composer, ComposerError, ComposerErrorInner, ErrSource, Export, ShaderDefValue, ShaderLanguage,
    UseBehaviour, UseDefWithOffset, UseDefinition, Visibility,
};

#[derive(Debug)]
struct PreprocessorRegexes {
    version_regex: Regex,
    ifdef_regex: Regex,
    ifndef_regex: Regex,
    ifop_regex: Regex,
    else_regex: Regex,
    endif_regex: Regex,
    def_regex: Regex,
    def_regex_delimited: Regex,
    use_regex: Regex,
    define_import_path_regex: Regex,
    define_shader_def_regex: Regex,
    pub_fn_regex: Regex,
    pub_var_regex: Regex,
    pub_constant_regex: Regex,
    pub_struct_regex: Regex,
    pub_alias_regex: Regex,
    pub_override_regex: Regex,
    module_regex: Regex,
}

#[derive(Debug)]
pub struct Preprocessor {
    regexes: Arc<PreprocessorRegexes>,
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self {
            regexes: Arc::new(PreprocessorRegexes {
            version_regex: Regex::new(r"^\s*#version\s+([0-9]+)").unwrap(),
            ifdef_regex: Regex::new(r"^\s*#\s*(else\s+)?\s*ifdef\s+([\w|\d|_]+)").unwrap(),
            ifndef_regex: Regex::new(r"^\s*#\s*(else\s+)?\s*ifndef\s+([\w|\d|_]+)").unwrap(),
            ifop_regex: Regex::new(
                r"^\s*#\s*(else\s+)?\s*if\s+([\w|\d|_]+)\s*([=!<>]*)\s*([-\w|\d]+)",
            )
            .unwrap(),
            else_regex: Regex::new(r"^\s*#\s*else").unwrap(),
            endif_regex: Regex::new(r"^\s*#\s*endif").unwrap(),
            def_regex: Regex::new(r"#\s*([\w|\d|_]+)").unwrap(),
            def_regex_delimited: Regex::new(r"#\s*\{([\w|\d|_]+)\}").unwrap(),
            use_regex: Regex::new(
                r"^(\s*(pub\s+)?use\s)|(\s*extend\s)",
            )
            .unwrap(),
            define_import_path_regex: Regex::new(r"^\s*#\s*define_import_path\s+(?P<import_path>[^\s]+)").unwrap(),
            define_shader_def_regex: Regex::new(r"^\s*#\s*define\s+([\w|\d|_]+)\s*([-\w|\d]+)?")
                .unwrap(),
            pub_fn_regex: Regex::new(r"(?P<lead>(^|\n)\s*)(?P<pub_keyword>pub\s+)((?P<virtual_keyword>virtual\s+)|(?P<extend_keyword>override\s+))?(?P<fn_keyword>fn\s+)(?<target_function>[^\s]+)(?P<trail>\s*)\(",).unwrap(),
            pub_var_regex: Regex::new(r"(?P<lead>(^|\n)\s*)(?P<pub_keyword>pub\s+)(?P<var_keyword>var\s*)(?P<var_arguments><.+?>\s*)?(?P<target_var>[^\:\s]+)").unwrap(),
            pub_struct_regex: Regex::new(r"(?P<lead>(^|\n)\s*)(?P<pub_keyword>pub\s+)(?P<struct_keyword>struct\s+)(?P<target_struct>[^\s]+)").unwrap(),
            pub_alias_regex: Regex::new(r"(?P<lead>(^|\n)\s*)(?P<pub_keyword>pub\s+)(?P<alias_keyword>alias\s+)(?P<target_alias>[^\s]+)(\s*)\=").unwrap(),
            pub_constant_regex: Regex::new(r"(?P<lead>(^|\n)\s*)(?P<pub_keyword>pub\s+)(?P<constant_keyword>const\s+)(?P<target_const>[^\s\:]+)").unwrap(),
            module_regex: Regex::new(
                r"(?P<lead>(^|\n)\s*)(?P<pub_keyword>pub\s+)?(?P<mod_keyword>mod\s+)(?P<module_name>[^\s]+)(?P<trail>\s*);".to_string()
                .as_str(),
            )
            .unwrap(),
            pub_override_regex: Regex::new(
                    r"(?P<lead>(^|\n)\s*)(?P<pub_keyword>pub\s+)(?P<override_keyword>override\s+)(?P<target_override>[^\s:]+)",
            )
            .unwrap(),
            })
        }
    }
}

#[derive(Debug)]
enum ScopeLevel {
    Active,           // conditions have been met
    PreviouslyActive, // conditions have previously been met
    NotActive,        // no conditions yet met
}

#[derive(Debug)]
struct Scope(Vec<ScopeLevel>);

impl Scope {
    fn new() -> Self {
        Self(vec![ScopeLevel::Active])
    }

    fn branch(
        &mut self,
        is_else: bool,
        condition: bool,
        offset: usize,
    ) -> Result<(), ComposerErrorInner> {
        if is_else {
            let prev_scope = self.0.pop().unwrap();
            let parent_scope = self
                .0
                .last()
                .ok_or(ComposerErrorInner::ElseWithoutCondition(offset))?;
            let new_scope = if !matches!(parent_scope, ScopeLevel::Active) {
                ScopeLevel::NotActive
            } else if !matches!(prev_scope, ScopeLevel::NotActive) {
                ScopeLevel::PreviouslyActive
            } else if condition {
                ScopeLevel::Active
            } else {
                ScopeLevel::NotActive
            };

            self.0.push(new_scope);
        } else {
            let parent_scope = self.0.last().unwrap_or(&ScopeLevel::Active);
            let new_scope = if matches!(parent_scope, ScopeLevel::Active) && condition {
                ScopeLevel::Active
            } else {
                ScopeLevel::NotActive
            };

            self.0.push(new_scope);
        }

        Ok(())
    }

    fn pop(&mut self, offset: usize) -> Result<(), ComposerErrorInner> {
        self.0.pop();
        if self.0.is_empty() {
            Err(ComposerErrorInner::TooManyEndIfs(offset))
        } else {
            Ok(())
        }
    }

    fn active(&self) -> bool {
        matches!(self.0.last().unwrap(), ScopeLevel::Active)
    }

    fn finish(&self, offset: usize) -> Result<(), ComposerErrorInner> {
        if self.0.len() != 1 {
            Err(ComposerErrorInner::NotEnoughEndIfs(offset))
        } else {
            Ok(())
        }
    }
}

fn check_scope<'a>(
    regexes: &PreprocessorRegexes,
    shader_defs: Option<&HashMap<String, ShaderDefValue>>,
    line: &'a str,
    scope: Option<&mut Scope>,
    offset: usize,
) -> Result<(bool, Option<&'a str>), ComposerErrorInner> {
    if let Some(cap) = regexes.ifdef_regex.captures(line) {
        let is_else = cap.get(1).is_some();
        let def = cap.get(2).unwrap().as_str();
        let Some(shader_defs) = shader_defs else {
            return Ok((true, Some(def)));
        };
        let cond = shader_defs.contains_key(def);
        scope.map_or(Ok(()), |scope| scope.branch(is_else, cond, offset))?;
        return Ok((true, Some(def)));
    } else if let Some(cap) = regexes.ifndef_regex.captures(line) {
        let is_else = cap.get(1).is_some();
        let def = cap.get(2).unwrap().as_str();
        let Some(shader_defs) = shader_defs else {
            return Ok((true, Some(def)));
        };
        let cond = !shader_defs.contains_key(def);
        scope.map_or(Ok(()), |scope| scope.branch(is_else, cond, offset))?;
        return Ok((true, Some(def)));
    } else if let Some(cap) = regexes.ifop_regex.captures(line) {
        let is_else = cap.get(1).is_some();
        let def = cap.get(2).unwrap().as_str();
        let Some(shader_defs) = shader_defs else {
            return Ok((true, Some(def)));
        };
        let op = cap.get(3).unwrap();
        let val = cap.get(4).unwrap();

        if scope.is_none() {
            // don't try to evaluate if we don't have a scope
            return Ok((true, Some(def)));
        }

        fn act_on<T: Eq + Ord>(
            a: T,
            b: T,
            op: &str,
            pos: usize,
        ) -> Result<bool, ComposerErrorInner> {
            match op {
                "==" => Ok(a == b),
                "!=" => Ok(a != b),
                ">" => Ok(a > b),
                ">=" => Ok(a >= b),
                "<" => Ok(a < b),
                "<=" => Ok(a <= b),
                _ => Err(ComposerErrorInner::UnknownShaderDefOperator {
                    pos,
                    operator: op.to_string(),
                }),
            }
        }

        let def_value = shader_defs
            .get(def)
            .ok_or(ComposerErrorInner::UnknownShaderDef {
                pos: offset,
                shader_def_name: def.to_string(),
            })?;

        let invalid_def = |ty: &str| ComposerErrorInner::InvalidShaderDefComparisonValue {
            pos: offset,
            shader_def_name: def.to_string(),
            value: val.as_str().to_string(),
            expected: ty.to_string(),
        };

        let new_scope = match def_value {
            ShaderDefValue::Bool(def_value) => {
                let val = val.as_str().parse().map_err(|_| invalid_def("bool"))?;
                act_on(*def_value, val, op.as_str(), offset)?
            }
            ShaderDefValue::Int(def_value) => {
                let val = val.as_str().parse().map_err(|_| invalid_def("int"))?;
                act_on(*def_value, val, op.as_str(), offset)?
            }
            ShaderDefValue::UInt(def_value) => {
                let val = val.as_str().parse().map_err(|_| invalid_def("uint"))?;
                act_on(*def_value, val, op.as_str(), offset)?
            }
        };
        scope.map_or(Ok(()), |scope| scope.branch(is_else, new_scope, offset))?;
        return Ok((true, Some(def)));
    } else if regexes.else_regex.is_match(line) {
        let Some(_) = shader_defs else {
            return Ok((true, None));
        };
        scope.map_or(Ok(()), |scope| scope.branch(true, true, offset))?;
        return Ok((true, None));
    } else if regexes.endif_regex.is_match(line) {
        let Some(_) = shader_defs else {
            return Ok((true, None));
        };
        scope.map_or(Ok(()), |scope| scope.pop(offset))?;
        return Ok((true, None));
    }

    Ok((false, None))
}

#[derive(Debug, Clone)]
pub struct PreprocessOutput {
    pub module_name: Option<String>,
    pub preprocessed_source: String,
    pub exports: IndexMap<String, (Export, Visibility)>,
    pub submodules: IndexSet<String>,
    pub extensions: IndexSet<String>,
    pub usages: Vec<UseDefWithOffset>,
    pub extended_functions: Vec<(Option<String>, String)>,
    // shader def values bound to this module
    pub shader_defs: HashMap<String, ShaderDefValue>,
    // list of shader_defs that can affect this crate
    pub effective_defs: HashSet<String>,
}

#[derive(Debug)]
enum PreprocessState {
    ProcessDirectives,
    DiscoveringSubmodulesAndExports,
    StartProcessingUsages,
    ParsingUsages,
    ParsingUseStatement {
        initial_offset: usize,
        usage_lines: String,
    },
    StartProcessingWildcards,
    ProcessingWildcards,
    ProcessingWildcard {
        module_name: String,
    },
    CanonicalizingExtendedFunctions,
    StartProcessingIdentifierSubstitution,
    ProcessIdentifierSubstitution,
    SubstituteIdentifiers {
        original_line: String,
        decommented_line: String,
    },
    Done(Box<PreprocessOutput>),
}

pub struct PreprocessResolver {
    lines: Vec<(String, String)>,
    current_line: usize,
    shader_defs: Option<HashMap<String, ShaderDefValue>>,
    scope: Scope,
    language: ShaderLanguage,
    crate_name: Option<String>,
    module_name: Option<String>,
    module_path: String,
    regexes: Arc<PreprocessorRegexes>,
    original_string: String,
    final_string: String,
    offset: usize,
    unprocessed_extended_functions: Vec<(String, usize)>,
    extensions: IndexSet<String>,
    extended_functions: Vec<(Option<String>, String)>,
    unprocessed_wildcards: IndexSet<(String, Option<Visibility>)>,
    usages: IndexMap<String, UseDefWithOffset>,
    declared_usages: IndexMap<String, Vec<(String, Option<Visibility>, UseBehaviour)>>,
    exports: IndexMap<String, (Export, Visibility)>,
    submodules: IndexSet<String>,
    state: PreprocessState,
    // the input shader def values plus the shader defs defined in this module
    derived_shader_defs: HashMap<String, ShaderDefValue>,
    // list of shader_defs that can affect this crate
    effective_defs: HashSet<String>,
}

#[derive(Debug)]
pub enum PreprocessMiss {
    MissingModules(Vec<(String, usize)>),
}

#[derive(Debug)]
pub enum PreprocessStep {
    Done(Box<PreprocessOutput>),
    Miss(PreprocessMiss),
}

fn map_err(inner: ComposerErrorInner, source: ErrSource) -> ComposerError {
    ComposerError { inner, source }
}

impl PreprocessResolver {
    pub fn module_name(&self) -> &str {
        self.module_name.as_deref().unwrap_or_default()
    }

    pub fn crate_name(&self) -> &Option<String> {
        &self.crate_name
    }

    pub fn module_path(&self) -> &str {
        &self.module_path.as_ref()
    }

    pub fn original_source(&self) -> &str {
        &self.original_string
    }

    fn process_directives(&mut self) -> Result<(), ComposerError> {
        while let Some((line, original_line)) = self.lines.get(self.current_line).cloned() {
            let mut output = false;
            if let Some(cap) = self.regexes.version_regex.captures(&line) {
                let v = cap.get(1).unwrap().as_str();
                if v != "440" && v != "450" {
                    return Err(map_err(
                        ComposerErrorInner::GlslInvalidVersion(self.offset),
                        ErrSource::Constructing {
                            offset: self.offset,
                            path: self.module_path.clone(),
                            source: self.original_source().to_string(),
                        },
                    ));
                }
            } else if let Some(cap) = self.regexes.define_import_path_regex.captures(&line) {
                self.module_name = self
                    .module_name
                    .clone()
                    .or_else(|| Some(cap.name("import_path").unwrap().as_str().to_string()));
            } else if let Some(captures) = self.regexes.define_shader_def_regex.captures(&line) {
                let def = captures.get(1).unwrap();
                let name = def.as_str().to_string();
                let value = if let Some(val) = captures.get(2) {
                    if let Ok(val) = val.as_str().parse::<u32>() {
                        ShaderDefValue::UInt(val)
                    } else if let Ok(val) = val.as_str().parse::<i32>() {
                        ShaderDefValue::Int(val)
                    } else if let Ok(val) = val.as_str().parse::<bool>() {
                        ShaderDefValue::Bool(val)
                    } else {
                        ShaderDefValue::Bool(false) // this error will get picked up when we fully preprocess the module
                    }
                } else {
                    ShaderDefValue::Bool(true)
                };
                self.derived_shader_defs.insert(name, value);
            } else if let (true, maybe_def) = check_scope(
                &self.regexes,
                if self.shader_defs.is_some() {
                    Some(&self.derived_shader_defs)
                } else {
                    None
                },
                &line,
                Some(&mut self.scope),
                self.offset,
            )
            .map_err(|inner| {
                map_err(
                    inner,
                    ErrSource::Constructing {
                        path: self.module_path.to_string(),
                        source: self.original_string.to_owned(),
                        offset: self.offset,
                    },
                )
            })? {
                if let Some(def) = maybe_def {
                    self.effective_defs.insert(def.to_owned());
                }
            } else if self.scope.active() {
                if self.regexes.use_regex.is_match(&line) {
                    let _ = self.collect_usage_lines(false);
                    output = true;
                    // ignore
                } else {
                    let mut replaced_line = original_line.to_string();
                    for capture in self.regexes.def_regex.captures_iter(&original_line) {
                        let def = capture.get(1).unwrap();
                        self.effective_defs
                            .insert(capture.get(1).unwrap().as_str().to_owned());
                        if let Some(def) = self.derived_shader_defs.get(def.as_str()) {
                            replaced_line = self
                                .regexes
                                .def_regex
                                .replace(&replaced_line, def.value_as_string())
                                .to_string();
                        }
                    }
                    for capture in self
                        .regexes
                        .def_regex_delimited
                        .captures_iter(&original_line)
                    {
                        let def = capture.get(1).unwrap();
                        if let Some(def) = self.derived_shader_defs.get(def.as_str()) {
                            replaced_line = self
                                .regexes
                                .def_regex_delimited
                                .replace(&replaced_line, def.value_as_string())
                                .to_string();
                        }
                    }

                    self.final_string.push_str(replaced_line.as_str());
                    let diff = original_line.len().saturating_sub(replaced_line.len());
                    self.final_string.extend(std::iter::repeat(" ").take(diff));
                    self.offset += original_line.len() + 1;
                    output = true;
                }
            }
            self.current_line += 1;

            if !output {
                // output spaces for removed lines to keep spans consistent (errors report against substituted_source, which is not preprocessed)
                self.final_string
                    .extend(std::iter::repeat(" ").take(original_line.len()));
                self.offset += original_line.len() + 1;
            }
            self.final_string.push('\n');
        }

        self.scope.finish(self.offset).map_err(|inner| {
            map_err(
                inner,
                ErrSource::Constructing {
                    offset: self.offset,
                    path: self.module_path.clone(),
                    source: self.original_source().to_string(),
                },
            )
        })?;
        self.state = PreprocessState::DiscoveringSubmodulesAndExports;
        Ok(())
    }

    fn collect_usage_lines(&mut self, replace_with_whitespace: bool) -> String {
        let mut usage_lines = String::default();
        let mut open_count = 0;

        loop {
            // output spaces for removed lines to keep spans consistent (errors report against substituted_source, which is not preprocessed)
            let (current_line, current_original_line) = &self.lines[self.current_line];
            if replace_with_whitespace {
                self.final_string
                    .extend(std::iter::repeat(" ").take(current_original_line.len()));
            } else {
                self.final_string.push_str(current_original_line);
            }
            self.offset += current_original_line.len() + 1;

            // PERF: Ideally we don't do multiple `match_indices` passes over `line`
            // in addition to the final pass for the import parse
            open_count += current_line.match_indices('{').count();
            open_count = open_count.saturating_sub(current_line.match_indices('}').count());

            // PERF: it's bad that we allocate here. ideally we would use something like
            //     let import_lines = &shader_str[initial_offset..offset]
            // but we need the comments removed, and the iterator approach doesn't make that easy
            usage_lines.push_str(current_line);
            usage_lines.push('\n');
            self.final_string.push('\n');
            if open_count == 0 || self.lines.get(self.current_line + 1).is_none() {
                break;
            }
            self.current_line += 1;
        }

        usage_lines
    }

    fn insert_reexports(
        &mut self,
        declared_usages: &IndexMap<String, Vec<(String, Option<Visibility>, UseBehaviour)>>,
        module_exports: &mut HashMap<String, IndexMap<String, Vec<(Export, Visibility)>>>,
        module_mappings: &mut HashMap<String, IndexMap<String, Option<Visibility>>>,
    ) {
        for (symbol_name, usages) in declared_usages.iter() {
            if let Some((full_path, Some(visibility), use_behaviour)) = usages.first() {
                let (module, symbol) = full_path
                    .rsplit_once("::")
                    .map(|(module, canonical)| (module.to_string(), Some(canonical.to_string())))
                    .unwrap_or_else(|| (full_path.to_string(), None));
                if self.module_name.as_deref().unwrap_or_default() != &module {
                    self.exports.insert(
                        symbol_name.to_string(),
                        (
                            Export::Use(module.clone(), symbol.clone(), use_behaviour.clone()),
                            *visibility,
                        ),
                    );
                    self.usages
                        .entry(full_path.clone())
                        .or_insert_with(|| UseDefWithOffset {
                            definition: UseDefinition {
                                module: module.clone(),
                                items: vec![],
                            },
                            offset: 0,
                        })
                        .definition
                        .items
                        .push(symbol.as_ref().unwrap_or(&symbol_name).clone());
                }

                module_exports
                    .entry(self.module_name.clone().unwrap_or_default())
                    .or_default()
                    .entry(symbol_name.to_string())
                    .or_default()
                    .push((
                        Export::Use(module, symbol.clone(), use_behaviour.clone()),
                        *visibility,
                    ));
                if let Some((canonical_module, aliased_visibility)) = module_mappings
                    .get(full_path)
                    .map(|mappings: &IndexMap<String, Option<Visibility>>| {
                        let (module, viz) = mappings.first().unwrap();
                        (module.clone(), *viz)
                    })
                {
                    // TODO Evaluate visibility
                    // TODO: Evaluate using relative modules and inscope imports
                    module_mappings
                        .entry(full_path.to_string())
                        .or_default()
                        .insert(canonical_module.to_string(), Some(*visibility));
                }
            }
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn next(
        &mut self,
        module_exports: &mut HashMap<String, IndexMap<String, Vec<(Export, Visibility)>>>,
        module_mappings: &mut HashMap<String, IndexMap<String, Option<Visibility>>>,
        module_wildcards: &mut HashMap<String, IndexSet<(String, Option<Visibility>)>>,
        visibility_unsupported_modules: &HashSet<String>,
    ) -> Result<PreprocessStep, ComposerError> {
        'processor: loop {
            match &self.state {
                PreprocessState::ProcessDirectives => {
                    self.process_directives()?;
                    continue 'processor;
                }
                PreprocessState::DiscoveringSubmodulesAndExports => {
                    let ParseExportsAndSubmodulesOutput {
                        final_string,
                        exports,
                        submodules,
                        extended_functions,
                    } = parse_exports_and_submodules(
                        &self.regexes,
                        self.language,
                        &self.final_string,
                    )?;
                    self.final_string = final_string;
                    self.unprocessed_extended_functions = extended_functions;
                    self.submodules.extend(submodules);
                    let module_name = self.module_name.clone().unwrap_or_default();

                    let module_export_entry =
                        module_exports.entry(module_name.clone()).or_default();

                    for (symbol, symbol_export) in exports {
                        if let (Some(_), true) = (&self.shader_defs, symbol_export.len() > 1) {
                            // TODO: Add better error handling (specifically include symbol offsets)
                            return Err(map_err(
                                ComposerErrorInner::AmbiguousExportError(symbol),
                                ErrSource::Constructing {
                                    offset: self.offset,
                                    path: self.module_path.clone(),
                                    source: self.original_source().to_string(),
                                },
                            ));
                        } else {
                            let export = symbol_export.first().unwrap().clone();
                            if let (Export::Module, visibility) = &export {
                                let submodule_name = format!("{module_name}::{symbol}").to_string();
                                module_mappings
                                    .entry(submodule_name.clone())
                                    .or_default()
                                    .insert(submodule_name, Some(*visibility));
                            };
                            self.exports.insert(symbol.clone(), export);
                            module_export_entry.insert(symbol, symbol_export);
                        }
                    }

                    self.state = PreprocessState::StartProcessingUsages;
                    let unprocessed_submodules = self
                        .submodules
                        .iter()
                        .map(|x| format!("{module_name}::{x}").to_string())
                        .filter(|x| {
                            !module_mappings.contains_key(x.as_str())
                                || !module_exports.contains_key(x.as_str())
                        })
                        .collect::<IndexSet<String>>();

                    if !unprocessed_submodules.is_empty() {
                        return Ok(PreprocessStep::Miss(PreprocessMiss::MissingModules(
                            unprocessed_submodules.into_iter().map(|x| (x, 0)).collect(),
                        )));
                    }
                    continue 'processor;
                }
                PreprocessState::StartProcessingUsages => {
                    self.lines = self
                        .final_string
                        .lines()
                        .replace_comments()
                        .zip(self.final_string.lines())
                        .map(|(replaced, original)| (replaced.to_string(), original.to_string()))
                        .collect();
                    self.current_line = 0;
                    self.offset = 0;
                    self.final_string.clear();
                    self.state = PreprocessState::ParsingUsages;
                    continue 'processor;
                }
                PreprocessState::ParsingUsages => {
                    while let Some((line, original_line)) =
                        self.lines.get(self.current_line).cloned()
                    {
                        if self.regexes.use_regex.is_match(&line) {
                            let initial_offset = self.offset;
                            let usage_lines = self.collect_usage_lines(true);
                            self.current_line += 1;
                            self.state = PreprocessState::ParsingUseStatement {
                                usage_lines: usage_lines.clone(),
                                initial_offset,
                            };
                            continue 'processor;
                        } else {
                            self.final_string.push_str(&original_line);
                            self.final_string.push('\n');
                            self.current_line += 1;
                            self.offset += original_line.len() + 1;
                        }
                    }
                    self.state = PreprocessState::StartProcessingWildcards;
                    continue 'processor;
                }
                PreprocessState::ParsingUseStatement {
                    initial_offset,
                    usage_lines: import_lines,
                } => {
                    let mut declared_usages = IndexMap::default();
                    let mut declared_wildcard_usages = IndexSet::default();

                    let uses_result = parse_uses(
                        import_lines.as_str(),
                        &self.crate_name,
                        &self.module_name,
                        &self.submodules,
                        &mut declared_usages,
                        &mut declared_wildcard_usages,
                        &mut self.extensions,
                    );
                    match uses_result {
                        Ok(()) => {
                            self.insert_reexports(
                                &declared_usages,
                                module_exports,
                                module_mappings,
                            );
                            module_wildcards
                                .entry(self.module_name.clone().unwrap_or_default())
                                .or_default()
                                .extend(declared_wildcard_usages.iter().cloned());
                            self.unprocessed_wildcards.extend(declared_wildcard_usages);
                            self.declared_usages.extend(declared_usages);
                        }
                        Err(UsageFault::UsageParseError(err)) => {
                            return Err(map_err(
                                ComposerErrorInner::UsageParseError(
                                    err.to_owned(),
                                    *initial_offset,
                                ),
                                ErrSource::Constructing {
                                    offset: self.offset,
                                    path: self.module_path.clone(),
                                    source: self.original_source().to_string(),
                                },
                            ));
                        }
                        Err(UsageFault::UsageParseErrorWithOffset(err, additional_offset)) => {
                            return Err(map_err(
                                ComposerErrorInner::UsageParseError(
                                    err.to_owned(),
                                    initial_offset + additional_offset,
                                ),
                                ErrSource::Constructing {
                                    offset: self.offset,
                                    path: self.module_path.clone(),
                                    source: self.original_source().to_string(),
                                },
                            ));
                        }
                        Err(UsageFault::ComposerError(err)) => {
                            return Err(map_err(
                                err,
                                ErrSource::Constructing {
                                    offset: self.offset,
                                    path: self.module_path.clone(),
                                    source: self.original_source().to_string(),
                                },
                            ));
                        }
                    };
                    self.current_line += 1;
                    self.final_string.push('\n');
                    self.state = PreprocessState::ParsingUsages;
                    continue 'processor;
                }
                PreprocessState::StartProcessingWildcards => {
                    self.state = PreprocessState::ProcessingWildcards;
                    continue 'processor;
                }
                PreprocessState::ProcessingWildcards => {
                    if let Some((module, _)) = self.unprocessed_wildcards.pop() {
                        self.state = PreprocessState::ProcessingWildcard {
                            module_name: module.clone(),
                        };
                    } else {
                        self.state = PreprocessState::CanonicalizingExtendedFunctions;
                    }
                    continue 'processor;
                }
                PreprocessState::ProcessingWildcard { module_name } => {
                    let mut declared_usages: IndexMap<
                        String,
                        Vec<(String, Option<Visibility>, UseBehaviour)>,
                    > = IndexMap::default();
                    match flatten_wildcard_use(
                        module_exports,
                        module_mappings,
                        module_wildcards,
                        visibility_unsupported_modules,
                        &self.crate_name,
                        &self.submodules,
                        &self.module_name,
                        &module_name,
                        &mut declared_usages,
                        self.shader_defs.is_none(),
                    ) {
                        Ok(()) => {
                            self.insert_reexports(
                                &declared_usages,
                                module_exports,
                                module_mappings,
                            );
                            self.declared_usages.extend(declared_usages);
                            self.state = PreprocessState::ProcessingWildcards;
                            continue 'processor;
                        }
                        Err(CanonicalizationFault::MissingModules(modules)) => {
                            return Ok(PreprocessStep::Miss(PreprocessMiss::MissingModules(
                                modules,
                            )));
                        }
                        Err(CanonicalizationFault::UsageParseError(err)) => {
                            return Err(map_err(
                                ComposerErrorInner::UsageParseError(err.to_owned(), 0),
                                ErrSource::Constructing {
                                    path: self.module_path.to_string(),
                                    source: self.original_string.to_string(),
                                    offset: 0,
                                },
                            ));
                        }
                        Err(CanonicalizationFault::UsageParseErrorWithOffset(err, offset)) => {
                            return Err(map_err(
                                ComposerErrorInner::UsageParseError(err.to_owned(), offset),
                                ErrSource::Constructing {
                                    path: self.module_path.to_string(),
                                    source: self.original_string.to_string(),
                                    offset: 0,
                                },
                            ));
                        }
                        Err(CanonicalizationFault::ComposerError(err)) => {
                            return Err(map_err(
                                err,
                                ErrSource::Constructing {
                                    path: self.module_path.to_string(),
                                    source: self.original_string.to_string(),
                                    offset: 0,
                                },
                            ));
                        }
                    }
                }
                PreprocessState::CanonicalizingExtendedFunctions => {
                    self.state = PreprocessState::StartProcessingIdentifierSubstitution;
                    for (idx, (extended_function, _)) in self
                        .unprocessed_extended_functions
                        .clone()
                        .iter_mut()
                        .enumerate()
                        .rev()
                    {
                        let full_path = match &self.module_name {
                            Some(module_name) => format!("{}::{}", module_name, extended_function),
                            None => extended_function.to_owned(),
                        };
                        let (module, fn_name) = full_path
                            .rsplit_once("::")
                            .map(|(module, fn_name)| (Some(module.to_string()), fn_name))
                            .unwrap_or_else(|| (None, &full_path));

                        self.unprocessed_extended_functions.remove(idx);
                        self.extended_functions.push((module, fn_name.to_string()));
                    }
                    self.state = PreprocessState::StartProcessingIdentifierSubstitution;
                }
                PreprocessState::StartProcessingIdentifierSubstitution => {
                    self.lines = self
                        .final_string
                        .lines()
                        .replace_comments()
                        .zip(self.final_string.lines())
                        .map(|(replaced, original)| (replaced.to_string(), original.to_string()))
                        .collect();
                    self.offset = 0;
                    self.current_line = 0;
                    self.final_string.clear();
                    self.state = PreprocessState::ProcessIdentifierSubstitution;
                }
                PreprocessState::ProcessIdentifierSubstitution => {
                    if let Some((line, original_line)) = self.lines.get(self.current_line).cloned()
                    {
                        self.state = PreprocessState::SubstituteIdentifiers {
                            original_line,
                            decommented_line: line,
                        };
                        continue 'processor;
                    }

                    let output = PreprocessOutput {
                        module_name: self.module_name.clone(),
                        preprocessed_source: self.final_string.clone(),
                        exports: self.exports.clone(),
                        submodules: self.submodules.clone(),
                        extensions: self.extensions.clone(),
                        usages: self.usages.values().cloned().collect(),
                        extended_functions: self.extended_functions.clone(),
                        shader_defs: self.derived_shader_defs.clone(),
                        effective_defs: self.effective_defs.clone(),
                    };
                    self.state = PreprocessState::Done(Box::new(output));

                    continue 'processor;
                }
                PreprocessState::SubstituteIdentifiers {
                    original_line,
                    decommented_line,
                } => {
                    // we don't want to capture imports from comments so we run using a dummy used_imports, and disregard any errors
                    let item_replaced_line = substitute_with_canonical_identifiers(
                        original_line,
                        self.offset,
                        &self.crate_name,
                        &self.module_name,
                        &self.submodules,
                        module_exports,
                        module_mappings,
                        visibility_unsupported_modules,
                        &self.declared_usages,
                        &mut Default::default(),
                        self.shader_defs.is_none(),
                    )
                    .unwrap_or_else(|_| original_line.clone());
                    // we run against the de-commented line to replace real imports, and throw an error if appropriate
                    match substitute_with_canonical_identifiers(
                        decommented_line,
                        self.offset,
                        &self.crate_name,
                        &self.module_name,
                        &self.submodules,
                        module_exports,
                        module_mappings,
                        visibility_unsupported_modules,
                        &self.declared_usages,
                        &mut self.usages,
                        self.shader_defs.is_none(),
                    ) {
                        Ok(_) => {}
                        Err(CanonicalizationFault::MissingModules(modules)) => {
                            return Ok(PreprocessStep::Miss(PreprocessMiss::MissingModules(
                                modules,
                            )));
                        }
                        Err(CanonicalizationFault::UsageParseError(err)) => {
                            return Err(map_err(
                                ComposerErrorInner::UsageParseError(err.to_owned(), self.offset),
                                ErrSource::Constructing {
                                    path: self.module_path.to_owned(),
                                    source: self.original_string.to_owned(),
                                    offset: self.offset,
                                },
                            ));
                        }
                        Err(CanonicalizationFault::UsageParseErrorWithOffset(err, offset)) => {
                            return Err(map_err(
                                ComposerErrorInner::UsageParseError(err.to_owned(), offset),
                                ErrSource::Constructing {
                                    path: self.module_path.to_owned(),
                                    source: self.original_string.to_owned(),
                                    offset,
                                },
                            ));
                        }
                        Err(CanonicalizationFault::ComposerError(err)) => {
                            return Err(map_err(
                                err,
                                ErrSource::Constructing {
                                    path: self.module_path.to_owned(),
                                    source: self.original_string.to_owned(),
                                    offset: self.offset,
                                },
                            ));
                        }
                    };

                    self.final_string.push_str(&item_replaced_line);
                    let diff = original_line.len().saturating_sub(item_replaced_line.len());
                    self.final_string.extend(std::iter::repeat(" ").take(diff));
                    self.offset += original_line.len() + 1;
                    self.final_string.push('\n');
                    self.current_line += 1;
                    self.state = PreprocessState::ProcessIdentifierSubstitution;
                    continue 'processor;
                }
                PreprocessState::Done(preprocess_output) => {
                    return Ok(PreprocessStep::Done(preprocess_output.clone()));
                }
            };
        }
    }
}

struct ParseExportsAndSubmodulesOutput {
    final_string: String,
    exports: IndexMap<String, Vec<(Export, Visibility)>>,
    submodules: IndexSet<String>,
    extended_functions: Vec<(String, usize)>,
}

fn get_submodules(
    regexes: &PreprocessorRegexes,
    language: ShaderLanguage,
    source: &str,
) -> IndexSet<String> {
    if language == ShaderLanguage::Wgsl {
        let mut submodules: IndexSet<String> = Default::default();
        for cap in regexes.module_regex.captures_iter(source) {
            let module_name = cap.name("module_name").unwrap().as_str();
            submodules.insert(module_name.to_string());
        }
        submodules
    } else {
        return IndexSet::new();
    }
}

// TODO: Make spans consistent
fn parse_exports_and_submodules(
    regexes: &PreprocessorRegexes,
    language: ShaderLanguage,
    source: &str,
) -> Result<ParseExportsAndSubmodulesOutput, ComposerError> {
    let mut exports: IndexMap<String, Vec<(Export, Visibility)>> = Default::default();
    let mut extended_functions: Vec<(String, usize)> = Default::default();
    let source = Cow::Borrowed(source);
    let source = if language == ShaderLanguage::Wgsl {
        regexes
            .pub_fn_regex
            .replace_all(&source, |cap: &regex::Captures| {
                let target_function = cap.name("target_function").unwrap().as_str();

                let extend_cap = cap.name("extend_keyword");

                let override_mode = if cap.name("virtual_keyword").is_some() || extend_cap.is_some()
                {
                    OverrideMode::Virtual
                } else {
                    OverrideMode::Static
                };

                if extend_cap.is_some() {
                    extended_functions.push((
                        target_function.to_string(),
                        cap.name("target_function").unwrap().start(),
                    ));
                }

                exports
                    .entry(target_function.to_string())
                    .or_default()
                    .push((
                        Export::Function(override_mode),
                        parse_visibility(cap.name("pub_keyword")).unwrap(),
                    ));
                format!(
                    "{}{}{}{}{}{}{}(",
                    cap.name("lead").unwrap().as_str(),
                    " ".repeat(cap.name("pub_keyword").map(|x| x.as_str()).unwrap().len()),
                    " ".repeat(
                        cap.name("extend_keyword")
                            .map(|x| x.as_str())
                            .unwrap_or_default()
                            .len()
                    ),
                    " ".repeat(
                        cap.name("virtual_keyword")
                            .map(|x| x.as_str())
                            .unwrap_or_default()
                            .len()
                    ),
                    cap.name("fn_keyword").unwrap().as_str(),
                    target_function,
                    cap.name("trail").unwrap().as_str()
                )
            })
    } else {
        source
    };

    let source = if language == ShaderLanguage::Wgsl {
        regexes
            .pub_var_regex
            .replace_all(&source, |cap: &regex::Captures| {
                let target_var = cap.name("target_var").unwrap().as_str();
                exports.entry(target_var.to_string()).or_default().push((
                    Export::Variable,
                    parse_visibility(cap.name("pub_keyword")).unwrap(),
                ));
                format!(
                    "{}{}{}{}{}",
                    cap.name("lead").unwrap().as_str(),
                    " ".repeat(
                        cap.name("pub_keyword")
                            .map(|x| x.as_str())
                            .unwrap_or_default()
                            .len()
                    ),
                    cap.name("var_keyword").unwrap().as_str(),
                    cap.name("var_arguments")
                        .map(|x| x.as_str())
                        .unwrap_or_default(),
                    target_var
                )
            })
    } else {
        source
    };

    let source = if language == ShaderLanguage::Wgsl {
        regexes
            .pub_struct_regex
            .replace_all(&source, |cap: &regex::Captures| {
                let target_struct = cap.name("target_struct").unwrap().as_str();
                exports.entry(target_struct.to_string()).or_default().push((
                    Export::Struct,
                    parse_visibility(cap.name("pub_keyword")).unwrap(),
                ));
                format!(
                    "{}{}{}{}",
                    cap.name("lead").unwrap().as_str(),
                    " ".repeat(
                        cap.name("pub_keyword")
                            .map(|x| x.as_str())
                            .unwrap_or_default()
                            .len()
                    ),
                    cap.name("struct_keyword").unwrap().as_str(),
                    target_struct,
                )
            })
    } else {
        source
    };

    let source = if language == ShaderLanguage::Wgsl {
        regexes
            .pub_alias_regex
            .replace_all(&source, |cap: &regex::Captures| {
                let target_alias = cap.name("target_alias").unwrap().as_str();
                exports.entry(target_alias.to_string()).or_default().push((
                    Export::Alias,
                    parse_visibility(cap.name("pub_keyword")).unwrap(),
                ));
                format!(
                    "{}{}{}{}",
                    cap.name("lead").unwrap().as_str(),
                    " ".repeat(
                        cap.name("pub_keyword")
                            .map(|x| x.as_str())
                            .unwrap_or_default()
                            .len()
                    ),
                    cap.name("alias_keyword").unwrap().as_str(),
                    target_alias,
                )
            })
    } else {
        source
    };

    let source = if language == ShaderLanguage::Wgsl {
        regexes
            .pub_constant_regex
            .replace_all(&source, |cap: &regex::Captures| {
                let target_const = cap.name("target_const").unwrap().as_str();
                exports.entry(target_const.to_string()).or_default().push((
                    Export::Constant,
                    parse_visibility(cap.name("pub_keyword")).unwrap(),
                ));
                format!(
                    "{}{}{}{}",
                    cap.name("lead").unwrap().as_str(),
                    " ".repeat(
                        cap.name("pub_keyword")
                            .map(|x| x.as_str())
                            .unwrap_or_default()
                            .len()
                    ),
                    cap.name("constant_keyword").unwrap().as_str(),
                    target_const,
                )
            })
    } else {
        source
    };

    let source = if language == ShaderLanguage::Wgsl {
        regexes
            .pub_override_regex
            .replace_all(&source, |cap: &regex::Captures| {
                let target_override = cap.name("target_alias").unwrap().as_str();
                exports
                    .entry(target_override.to_string())
                    .or_default()
                    .push((
                        Export::Override,
                        parse_visibility(cap.name("pub_keyword")).unwrap(),
                    ));
                format!(
                    "{}{}",
                    cap.name("override_keyword").unwrap().as_str(),
                    target_override,
                )
            })
    } else {
        source
    };

    let mut submodules: IndexSet<String> = Default::default();
    let source = if language == ShaderLanguage::Wgsl {
        regexes
            .module_regex
            .replace_all(&source, |cap: &regex::Captures| {
                let module_name = cap.name("module_name").unwrap().as_str();

                let visibility = parse_visibility(cap.name("pub_keyword"));

                if let Some(visibility) = visibility {
                    exports
                        .entry(module_name.to_string())
                        .or_default()
                        .push((Export::Module, visibility));
                }
                submodules.insert(module_name.to_string());
                format!(
                    "\n{}{}{}{}{} ",
                    " ".repeat(
                        cap.name("pub_keyword")
                            .map(|x| x.as_str())
                            .unwrap_or_default()
                            .len()
                            .saturating_sub(1)
                    ),
                    cap.name("lead").unwrap().as_str(),
                    " ".repeat(cap.name("mod_keyword").unwrap().as_str().len()),
                    " ".repeat(module_name.len()),
                    cap.name("trail").map(|x| x.as_str()).unwrap_or_default(),
                )
            })
    } else {
        source
    };
    Ok(ParseExportsAndSubmodulesOutput {
        final_string: source.to_string(),
        exports,
        submodules,
        extended_functions,
    })
}

fn parse_visibility(visibility: Option<Match>) -> Option<Visibility> {
    if visibility.is_some() {
        Some(Visibility::Public)
    } else {
        None
    }
}

impl Preprocessor {
    // process #if[(n)?def]? / #else / #endif preprocessor directives,
    // strip module name and imports
    // also strip "#version xxx"
    // replace items with resolved decorated names
    #[allow(clippy::too_many_arguments)]
    pub fn preprocess(
        &self,
        shader_str: &str,
        shader_defs: Option<HashMap<String, ShaderDefValue>>,
        language: ShaderLanguage,
        crate_name: Option<String>,
        module_name: Option<String>,
        module_path: String,
    ) -> PreprocessResolver {
        let declared_usages = IndexMap::new();
        let extensions = IndexSet::new();
        let scope = Scope::new();
        let final_string = String::new();
        let offset = 0;

        let lines: Vec<(String, String)> = shader_str
            .lines()
            .replace_comments()
            .zip(shader_str.lines())
            .map(|(replaced, original)| (replaced.to_string(), original.to_string()))
            .collect();

        return PreprocessResolver {
            lines,
            current_line: 0,
            shader_defs: shader_defs.clone(),
            language,
            crate_name,
            module_name,
            module_path,
            regexes: self.regexes.clone(),
            scope,
            final_string,
            offset,
            extensions,
            usages: Default::default(),
            declared_usages,
            state: PreprocessState::ProcessDirectives,
            exports: Default::default(),
            unprocessed_extended_functions: Default::default(),
            extended_functions: Default::default(),
            submodules: Default::default(),
            original_string: shader_str.to_string(),
            unprocessed_wildcards: Default::default(),
            derived_shader_defs: shader_defs.unwrap_or_default(),
            effective_defs: Default::default(),
        };
    }

    pub fn get_submodules(&self, shader_str: &str, language: ShaderLanguage) -> IndexSet<String> {
        get_submodules(&self.regexes, language, shader_str)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[rustfmt::skip]
    const WGSL_ELSE_IFDEF: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;

#ifdef TEXTURE
// Main texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else ifdef SECOND_TEXTURE
// Second texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else ifdef THIRD_TEXTURE
// Third texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else
@group(1) @binding(0)
var sprite_texture: texture_2d_array<f32>;
#endif

struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

    //preprocessor tests
    #[test]
    fn process_shader_def_unknown_operator() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE !! true
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        let processor = Preprocessor::default();

        let result_missing = processor
            .preprocess(
                WGSL,
                Some([("TEXTURE".to_owned(), ShaderDefValue::Bool(true))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            )
            .unwrap_err()
            .inner;

        let expected: ComposerErrorInner = ComposerErrorInner::UnknownShaderDefOperator {
            pos: 124,
            operator: "!!".to_string(),
        };

        assert_eq!(format!("{result_missing:?}"), format!("{expected:?}"),);
    }

    #[test]
    fn process_shader_def_equal_int() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE == 3
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_EQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;

@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;

struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_NEQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;




struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL,
                Some([("TEXTURE".to_string(), ShaderDefValue::Int(3))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
            )
            .unwrap()
        {
            PreprocessStep::Done(result) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED_EQ
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            not_good => panic!("ERR: {:?}", not_good),
        };

        match processor
            .preprocess(
                WGSL,
                Some([("TEXTURE".to_string(), ShaderDefValue::Int(7))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
            )
            .unwrap()
        {
            PreprocessStep::Done(result) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED_NEQ
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            not_good => panic!("ERR: {:?}", not_good),
        };

        let result_missing = processor
            .preprocess(
                WGSL,
                Some(Default::default()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            )
            .unwrap_err()
            .inner;

        let expected_err = ComposerErrorInner::UnknownShaderDef {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
        };
        assert_eq!(format!("{result_missing:?}"), format!("{expected_err:?}"),);

        let result_wrong_type = processor
            .preprocess(
                WGSL,
                Some([("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            )
            .unwrap_err()
            .inner;

        let expected_err = ComposerErrorInner::InvalidShaderDefComparisonValue {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
            expected: "bool".to_string(),
            value: "3".to_string(),
        };

        assert_eq!(
            format!("{result_wrong_type:?}"),
            format!("{expected_err:?}")
        );
    }

    #[test]
    fn process_shader_def_equal_bool() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE == true
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_EQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;

@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;

struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_NEQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;




struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL,
                Some([("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED_EQ
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };

        match processor
            .preprocess(
                WGSL,
                Some([("TEXTURE".to_string(), ShaderDefValue::Bool(false))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED_NEQ
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_def_not_equal_bool() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE != false
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_EQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;

@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;

struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_NEQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;




struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL,
                Some([("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_owned(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED_EQ
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };

        match processor
            .preprocess(
                WGSL,
                Some([("TEXTURE".to_string(), ShaderDefValue::Bool(false))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED_NEQ
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };

        let result_missing = processor
            .preprocess(
                WGSL,
                Some([].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            )
            .unwrap_err()
            .inner;
        let expected_err = ComposerErrorInner::UnknownShaderDef {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
        };
        assert_eq!(format!("{result_missing:?}"), format!("{expected_err:?}"),);

        let result_wrong_type = processor
            .preprocess(
                WGSL,
                Some([("TEXTURE".to_string(), ShaderDefValue::Int(7))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            )
            .unwrap_err()
            .inner;

        let expected_err = ComposerErrorInner::InvalidShaderDefComparisonValue {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
            expected: "int".to_string(),
            value: "false".to_string(),
        };
        assert_eq!(
            format!("{result_wrong_type:?}"),
            format!("{expected_err:?}"),
        );
    }

    #[test]
    fn process_shader_def_replace() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    var a: i32 = #FIRST_VALUE;
    var b: i32 = #FIRST_VALUE * #SECOND_VALUE;
    var c: i32 = #MISSING_VALUE;
    var d: bool = #BOOL_VALUE;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_REPLACED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    var a: i32 = 5;
    var b: i32 = 5 * 3;
    var c: i32 = #MISSING_VALUE;
    var d: bool = true;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL,
                Some(
                    [
                        ("BOOL_VALUE".to_string(), ShaderDefValue::Bool(true)),
                        ("FIRST_VALUE".to_string(), ShaderDefValue::Int(5)),
                        ("SECOND_VALUE".to_string(), ShaderDefValue::Int(3)),
                    ]
                    .into(),
                ),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED_REPLACED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_define_in_shader() {
        #[rustfmt::skip]
        const WGSL: &str = r"
#define NOW_DEFINED
#ifdef NOW_DEFINED
defined
#endif
";

        #[rustfmt::skip]
        const EXPECTED: &str = r"


defined

";
        let processor = Preprocessor::default();

        match processor
            .preprocess(
                WGSL,
                Some([].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_define_in_shader_with_value() {
        #[rustfmt::skip]
        const WGSL: &str = r"
#define DEFUINT 1
#define DEFINT -1
#define DEFBOOL false
#if DEFUINT == 1
uint: #DEFUINT
#endif
#if DEFINT == -1
int: #DEFINT
#endif
#if DEFBOOL == false
bool: #DEFBOOL
#endif
";

        #[rustfmt::skip]
        const EXPECTED: &str = r"




uint: 1


int: -1


bool: false

";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL,
                Some([].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_owned(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_else() {
        #[rustfmt::skip]
        const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
@group(1) @binding(0)
var sprite_texture: texture_2d_array<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL_ELSE_IFDEF,
                Some([].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_def_else_ifdef_no_match_and_no_fallback_else() {
        #[rustfmt::skip]
        const WGSL_ELSE_IFDEF_NO_ELSE_FALLBACK: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;

#ifdef TEXTURE
// Main texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else ifdef OTHER_TEXTURE
// Other texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif

struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL_ELSE_IFDEF_NO_ELSE_FALLBACK,
                Some([].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_first_clause() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;

// Main texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;

struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL_ELSE_IFDEF,
                Some([("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_second_clause() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
// Second texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let shader_defs = [("SECOND_TEXTURE".to_string(), ShaderDefValue::Bool(true))].into();
        match processor
            .preprocess(
                WGSL_ELSE_IFDEF,
                Some(shader_defs),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_third_clause() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
// Third texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL_ELSE_IFDEF,
                Some([("THIRD_TEXTURE".to_string(), ShaderDefValue::Bool(true))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_def_else_ifdef_only_accepts_one_valid_else_ifdef() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
// Second texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL_ELSE_IFDEF,
                Some(
                    [
                        ("SECOND_TEXTURE".to_string(), ShaderDefValue::Bool(true)),
                        ("THIRD_TEXTURE".to_string(), ShaderDefValue::Bool(true)),
                    ]
                    .into(),
                ),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_def_else_ifdef_complicated_nesting() {
        // Test some nesting including #else ifdef statements
        // 1. Enter an #else ifdef
        // 2. Then enter an #else
        // 3. Then enter another #else ifdef

        #[rustfmt::skip]
        const WGSL_COMPLICATED_ELSE_IFDEF: &str = r"
#ifdef NOT_DEFINED
// not defined
#else ifdef IS_DEFINED
// defined 1
#ifdef NOT_DEFINED
// not defined
#else
// should be here
#ifdef NOT_DEFINED
// not defined
#else ifdef ALSO_NOT_DEFINED
// not defined
#else ifdef IS_DEFINED
// defined 2
#endif
#endif
#endif
";

        #[rustfmt::skip]
        const EXPECTED: &str = r"
// defined 1
// should be here
// defined 2
";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                WGSL_COMPLICATED_ELSE_IFDEF,
                Some([("IS_DEFINED".to_string(), ShaderDefValue::Bool(true))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_def_else_ifndef() {
        #[rustfmt::skip]
        const INPUT: &str = r"
#ifdef NOT_DEFINED
fail 1
#else ifdef ALSO_NOT_DEFINED
fail 2
#else ifndef ALSO_ALSO_NOT_DEFINED
ok
#else
fail 3
#endif
";

        const EXPECTED: &str = r"ok";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                INPUT,
                Some([].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }

    #[test]
    fn process_shader_def_else_if() {
        #[rustfmt::skip]
        const INPUT: &str = r"
#ifdef NOT_DEFINED
fail 1
#else if x == 1
fail 2
#else if x == 2
ok
#else
fail 3
#endif
";

        const EXPECTED: &str = r"ok";
        let processor = Preprocessor::default();
        match processor
            .preprocess(
                INPUT,
                Some([("x".to_owned(), ShaderDefValue::Int(2))].into()),
                ShaderLanguage::Wgsl,
                None,
                None,
                "/tests/abc.wgsl".to_string(),
            )
            .next(
                &mut Default::default(),
                &mut Default::default(),
                &mut Default::default(),
                &Default::default(),
            ) {
            Ok(PreprocessStep::Done(result)) => {
                assert_eq!(
                    result
                        .preprocessed_source
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', ""),
                    EXPECTED
                        .replace(' ', "")
                        .replace('\n', "")
                        .replace('\r', "")
                );
            }
            Ok(not_good) => panic!("ERR: {:?}", not_good),
            Err(err) => panic!("ERR: {:?}", err),
        };
    }
}
