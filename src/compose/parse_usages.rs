use std::collections::{HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};

use super::{
    tokenizer::{Token, Tokenizer},
    Composer, ComposerErrorInner, Export, UseBehaviour, UseDefWithOffset, UseDefinition,
    Visibility,
};

#[allow(clippy::type_complexity)]

pub fn parse_uses(
    input: &str,
    crate_name: &Option<String>,
    from_module: &Option<String>,
    submodules: &IndexSet<String>,
    declared_usages: &mut IndexMap<String, Vec<(String, Option<Visibility>, UseBehaviour)>>,
    declared_wildcard_usages: &mut IndexSet<(String, Option<Visibility>)>,
    declared_extensions: &mut IndexSet<String>,
) -> Result<(), UsageFault> {
    let mut tokens = Tokenizer::new(input, false).peekable();

    let use_behaviour = match tokens.peek() {
        Some(Token::Other('#', _)) => {
            tokens.next();
            UseBehaviour::ModuleSystem
        }
        _ => UseBehaviour::ModuleSystem,
    };

    let visibility = match tokens.peek() {
        Some(Token::Identifier("pub", _)) => {
            tokens.next();
            Some(Visibility::Public)
        }
        _ => None,
    };

    let mut extension_import = false;

    match tokens.next() {
        Some(Token::Identifier("use", _)) => (),
        Some(Token::Identifier("extend", pos)) if visibility == Some(Visibility::Public) => {
            return Err(UsageFault::UsageParseErrorWithOffset(
                "`extend` does not support the `pub` keyword".to_string(),
                pos,
            ))
        }
        Some(Token::Identifier("extend", _)) => {
            extension_import = true;
        }
        Some(other) => {
            return Err(UsageFault::UsageParseErrorWithOffset(
                "expected `use`".to_string(),
                other.pos(),
            ))
        }
        None => {
            return Err(UsageFault::UsageParseErrorWithOffset(
                "expected `use`".to_string(),
                input.len(),
            ))
        }
    };

    let mut stack = Vec::default();
    let mut current = String::default();
    let mut as_name = None;
    let mut is_deprecated_itemlist = false;

    loop {
        match tokens.peek() {
            Some(Token::Identifier(ident, _)) => {
                let mut ident = ident.to_string();
                if stack.is_empty() {
                    if let Some((start, residual)) = ident.split_once("::") {
                        if start == "crate" && crate_name.is_some() {
                            ident = format!(
                                "{}::{residual}",
                                crate_name.as_deref().unwrap().to_string()
                            );
                        } else if start == "self" && from_module.is_some() {
                            ident = format!(
                                "{}::{residual}",
                                from_module.as_deref().unwrap().to_string()
                            );
                        } else if start == "super" && from_module.is_some() {
                            let (spr, _) =
                                from_module.as_deref().unwrap().rsplit_once("::").unwrap();
                            ident = format!("{spr}::{residual}");
                        } else if submodules.contains(start) && from_module.is_some() {
                            ident = format!(
                                "{}::{}::{}",
                                from_module.as_deref().unwrap(),
                                start,
                                residual
                            );
                        }
                    }
                }
                let ident = &ident;
                current.push_str(ident);
                tokens.next();

                if tokens.peek().and_then(Token::identifier) == Some("as") {
                    let pos = tokens.next().unwrap().pos();
                    if extension_import {
                        return Err(UsageFault::UsageParseErrorWithOffset(
                            "extensions do not support aliasing using `as`".to_string(),
                            pos,
                        ));
                    }
                    let Some(Token::Identifier(name, _)) = tokens.next() else {
                        return Err(UsageFault::UsageParseErrorWithOffset(
                            "expected identifier after `as`".to_string(),
                            pos,
                        ));
                    };
                    as_name = Some(name);
                } else if tokens.peek().and_then(Token::other) == Some(&'*') {
                    if !current.is_empty() {
                        let joined = format!("{}{}", stack.join(""), current);
                        let module_name = joined.strip_suffix("::").unwrap_or_else(|| &joined);
                        declared_wildcard_usages.insert((module_name.to_owned(), visibility));
                        current = String::default();
                        as_name = None;
                        tokens.next();
                    }
                }
                // support deprecated use mod item
                else if let Some(Token::Identifier(..)) = tokens.peek() {
                    #[cfg(not(feature = "allow_deprecated"))]
                    tracing::warn!("item list imports are deprecated, please use `rust::style::item_imports` (or use feature `allow_deprecated`)`\n| {}", input);

                    is_deprecated_itemlist = true;
                    stack.push(format!("{}::", current));
                    current = String::default();
                    as_name = None;
                }

                continue;
            }
            Some(Token::Other('{', pos)) => {
                if !current.ends_with("::") {
                    return Err(UsageFault::UsageParseErrorWithOffset(
                        "open brace must follow `::`".to_string(),
                        *pos,
                    ));
                }
                stack.push(current);
                current = String::default();
                as_name = None;
            }
            Some(Token::Other(',', _))
            | Some(Token::Other('}', _))
            | Some(Token::Other('\n', _))
            | None => {
                if !current.is_empty() {
                    let import_name = current
                        .rsplit_once("::")
                        .map(|(_, name)| name.to_owned())
                        .unwrap_or(current.clone());
                    let used_name = as_name
                        .map(ToString::to_string)
                        .unwrap_or_else(|| import_name);

                    if extension_import {
                        let full_path = format!("{}{}", stack.join(""), current);
                        declared_extensions.insert(full_path);
                    } else {
                        let full_path = format!("{}{}", stack.join(""), current);
                        declared_usages.entry(used_name.clone()).or_default().push((
                            full_path,
                            visibility,
                            use_behaviour.clone(),
                        ));
                    }

                    current = String::default();
                    as_name = None;
                }

                if let Some(Token::Other('}', pos)) = tokens.peek() {
                    if stack.pop().is_none() {
                        return Err(UsageFault::UsageParseErrorWithOffset(
                            "close brace without open".to_string(),
                            *pos,
                        ));
                    }
                }

                if tokens.peek().is_none() {
                    break;
                }
            }
            Some(Token::Other(';', _)) => {
                tokens.next();
                if let Some(token) = tokens.peek() {
                    return Err(UsageFault::UsageParseErrorWithOffset(
                        "unexpected token after ';'".to_string(),
                        token.pos(),
                    ));
                }
            }
            Some(Token::Other(token, pos)) => {
                return Err(UsageFault::UsageParseErrorWithOffset(
                    format!("unexpected token '{}'", token),
                    *pos,
                ))
            }
            Some(Token::Whitespace(..)) => unreachable!(),
        }

        tokens.next();
    }

    if !(stack.is_empty() || is_deprecated_itemlist && stack.len() == 1) {
        return Err(UsageFault::UsageParseErrorWithOffset(
            "missing close brace".to_string(),
            input.len(),
        ));
    }

    Ok(())
}

#[derive(Debug)]
pub enum UsageFault {
    // TODO Improve error handling
    UsageParseError(String),
    UsageParseErrorWithOffset(String, usize),
    ComposerError(ComposerErrorInner),
}

#[derive(Debug)]
pub enum CanonicalizationFault {
    // TODO Improve error handling
    UsageParseError(String),
    UsageParseErrorWithOffset(String, usize),
    MissingModules(Vec<(String, usize)>),
    ComposerError(ComposerErrorInner),
}

#[allow(clippy::too_many_arguments, clippy::type_complexity)]
pub fn canonicalize_usage(
    module_exports: &HashMap<String, IndexMap<String, Vec<(Export, Visibility)>>>,
    module_mappings: &HashMap<String, IndexMap<String, Option<Visibility>>>,
    visibility_unsupported_modules: &HashSet<String>,
    from_module: &Option<String>,
    use_module_name: &str,
    use_item: &str,
    allow_ambiguous: bool,
    result: &mut Vec<(String, String, Option<Export>)>,
    seen_modules: &mut HashSet<String>,
) -> Result<(), CanonicalizationFault> {
    // TODO: Introduce backtracking to allow for recursive exports
    if let Some(mapping) = module_mappings.get(use_module_name) {
        // TODO: Evaluate Visibility
        for (concrete_module, concrete_module_visibility) in mapping {
            if visibility_unsupported_modules.contains(concrete_module) {
                result.push((concrete_module.to_string(), use_item.to_string(), None));
            }
            if let Some(exports) = module_exports.get(concrete_module) {
                if let Some(export) = exports.get(use_item) {
                    for (export, visibility) in export.iter() {
                        match export {
                            Export::Function(_)
                            | Export::Variable
                            | Export::Constant
                            | Export::Alias
                            | Export::Struct
                            | Export::Override
                            | Export::Module => {
                                if result
                                    .iter()
                                    .all(|(mdl, itm, _)| use_module_name != mdl || itm != use_item)
                                {
                                    result.push((
                                        use_module_name.to_string(),
                                        use_item.to_string(),
                                        Some(export.clone()),
                                    ));
                                }
                            }
                            Export::Use(module_name, original_name, _)
                                if !seen_modules.contains(module_name) =>
                            {
                                seen_modules.insert(module_name.clone());
                                // TODO: Add visibility modifiers
                                canonicalize_usage(
                                    module_exports,
                                    module_mappings,
                                    visibility_unsupported_modules,
                                    &Some(concrete_module.to_string()),
                                    module_name,
                                    original_name.as_deref().unwrap_or(use_item),
                                    allow_ambiguous,
                                    result,
                                    seen_modules,
                                )?;
                            }
                            Export::Use(_, _, _) => {}
                        }
                    }
                }
            } else {
                return Err(CanonicalizationFault::MissingModules(vec![(
                    concrete_module.to_string(),
                    0,
                )]));
            }
        }
    } else {
        return Err(CanonicalizationFault::MissingModules(vec![(
            use_module_name.to_string(),
            0,
        )]));
    }

    if result.is_empty() {
        return Err(CanonicalizationFault::UsageParseError(
            format!("module {} item {} not found", use_module_name, use_item).to_string(),
        ));
    }
    if !allow_ambiguous && result.len() > 1 {
        return Err(CanonicalizationFault::UsageParseError(
            format!(
                "module {} has an ambigous export for item {}",
                use_module_name, use_item
            )
            .to_string(),
        ));
    }

    Ok(())
}

pub(crate) fn flatten_wildcard_use<'a>(
    module_exports: &HashMap<String, IndexMap<String, Vec<(Export, Visibility)>>>,
    module_mappings: &HashMap<String, IndexMap<String, Option<Visibility>>>,
    module_wildcards: &HashMap<String, IndexSet<(String, Option<Visibility>)>>,
    visibility_unsupported_modules: &HashSet<String>,
    crate_name: &Option<String>,
    submodules: &IndexSet<String>,
    from_module: &Option<String>,
    module_name: &'a str,
    declared_usages: &mut IndexMap<String, Vec<(String, Option<Visibility>, UseBehaviour)>>,
    allow_ambiguous: bool,
) -> Result<(), CanonicalizationFault> {
    let full_module_paths = get_full_paths(
        module_name,
        declared_usages,
        crate_name,
        from_module,
        submodules,
    );
    if !allow_ambiguous && full_module_paths.len() > 1 {
        return Err(CanonicalizationFault::UsageParseError(format!(
            "Ambiguous paths {:?} for {}",
            full_module_paths, module_name
        )));
    }

    let mut seen_wildcards: HashSet<String> = HashSet::new();

    for module_name in full_module_paths {
        let mut add_exports = |exports: &IndexMap<String, Vec<(Export, Visibility)>> | -> Result<(), CanonicalizationFault> {
        // TODO: Add visibility modifiers
        if !allow_ambiguous && exports.values().any(|x| x.len() > 1) {
            return Err(CanonicalizationFault::UsageParseError(
                format!("found ambiguity when importing module {}", module_name).to_string(),
            ));
        }
        for (exported_name, export) in exports.iter() {
            for (e, visibility) in export {
                // TODO evalutate visibility
                let (module_name, name, use_behaviour) = match e {
                    Export::Function(_)
                    | Export::Variable
                    | Export::Constant
                    | Export::Alias
                    | Export::Struct
                    | Export::Override
                    | Export::Module => (
                        module_name.to_string(),
                        exported_name.to_string(),
                        UseBehaviour::ModuleSystem,
                    ),
                    Export::Use(module_name, canonical_name, use_behaviour) => (
                        module_name.to_string(),
                        canonical_name.as_ref().unwrap_or(exported_name).to_string(),
                        use_behaviour.clone(),
                    ),
                };
                declared_usages
                    .entry(exported_name.to_string())
                    .or_default()
                    .push((
                        format!("{}::{}", module_name, name),
                        Some(visibility.clone()),
                        use_behaviour,
                    ));
            }
        }
        return Ok(());
    };
        if visibility_unsupported_modules.contains(&module_name) {
            return Err(CanonicalizationFault::UsageParseError(
                format!(
                    "Cannot use wildcard imports when using module {} as it does not yet support visibility modifiers",
                    module_name
                )
                .to_string(),
            ));
        };
        if let Some(mapping) = module_mappings.get(&module_name) {
            for (concrete_module, concrete_module_visibility) in mapping {
                if let Some(exports) = module_exports.get(concrete_module) {
                    add_exports(
                        &exports
                            .iter()
                            .map(|(symbol_name, export)| {
                                (
                                    symbol_name.to_owned(),
                                    export
                                        .iter()
                                        .filter(|(export, _)| match export {
                                            Export::Use(m, _, _)
                                                if Some(m) == from_module.as_ref() =>
                                            {
                                                false
                                            }
                                            _ => true,
                                        })
                                        .cloned()
                                        .collect(),
                                )
                            })
                            .collect(),
                    )?;
                    let mut stack: Vec<(String, Option<Visibility>)> = module_wildcards
                        .get(concrete_module)
                        .cloned()
                        .unwrap_or_else(|| IndexSet::new())
                        .into_iter()
                        .collect();
                    loop {
                        if let Some((wildcard_module, visiblity)) = stack.pop() {
                            if Some(&wildcard_module) == from_module.as_ref() {
                                continue;
                            }
                            // TODO Evaluate visibility
                            if let Some(mapping) = module_mappings.get(&wildcard_module) {
                                if matches!(visiblity, Some(_)) {
                                    for (mapping, _) in mapping.iter() {
                                        // TODO Evaluate visibility
                                        if !seen_wildcards.contains(mapping) {
                                            seen_wildcards.insert(mapping.to_string());
                                            if let Some(exports) = module_exports.get(mapping) {
                                                add_exports(exports)?;
                                                stack.extend(
                                                module_wildcards
                                                    .get(mapping)
                                                    .map(|x| {
                                                        x.iter().cloned().filter(|(_, viz)| matches!(viz, Some(_))).collect::<Vec<(
                                                            String,
                                                            Option<Visibility>,
                                                        )>>(
                                                        )
                                                    })
                                                    .unwrap_or_default(),
                                            );
                                            } else {
                                                return Err(CanonicalizationFault::MissingModules(
                                                    vec![(mapping.to_string(), 0)],
                                                ));
                                            }
                                        }
                                    }
                                }
                            } else {
                                return Err(CanonicalizationFault::MissingModules(vec![(
                                    module_name.to_string(),
                                    0,
                                )]));
                            }
                        } else {
                            break;
                        }
                    }
                } else {
                    return Err(CanonicalizationFault::MissingModules(vec![(
                        concrete_module.to_string(),
                        0,
                    )]));
                }
            }
            return Ok(());
        } else {
            return Err(CanonicalizationFault::MissingModules(vec![(
                module_name.to_string(),
                0,
            )]));
        }
    }

    return Err(CanonicalizationFault::UsageParseError(format!(
        "No valid canonical paths for {}",
        module_name
    )));
}

#[allow(clippy::type_complexity)]
pub fn get_full_paths(
    ident: &str,
    declared_usages: &IndexMap<String, Vec<(String, Option<Visibility>, UseBehaviour)>>,
    crate_name: &Option<String>,
    from_module: &Option<String>,
    submodules: &IndexSet<String>,
) -> Vec<String> {
    let (first, residual) = ident.split_once("::").unwrap_or((ident, ""));
    let full_paths = if first == "self" && from_module.is_some() {
        if residual.is_empty() {
            get_full_paths(
                &from_module.clone().unwrap(),
                declared_usages,
                crate_name,
                from_module,
                submodules,
            )
        } else {
            get_full_paths(
                &format!("{}::{}", from_module.as_deref().unwrap(), residual),
                declared_usages,
                crate_name,
                from_module,
                submodules,
            )
        }
    } else if first == "crate" && crate_name.is_some() {
        if residual.is_empty() {
            get_full_paths(
                &crate_name.clone().unwrap(),
                declared_usages,
                crate_name,
                from_module,
                submodules,
            )
        } else {
            get_full_paths(
                &format!("{}::{}", crate_name.as_deref().unwrap(), residual),
                declared_usages,
                crate_name,
                from_module,
                submodules,
            )
        }
    } else if first == "super" && from_module.is_some() {
        // TODO RETURN AN ERROR IF NO SUPER MODULE
        let (spr, _) = from_module.as_deref().unwrap().rsplit_once("::").unwrap();
        if residual.is_empty() {
            get_full_paths(spr, declared_usages, crate_name, from_module, submodules)
        } else {
            get_full_paths(
                &format!("{}::{}", spr, residual),
                declared_usages,
                crate_name,
                from_module,
                submodules,
            )
        }
    } else {
        if submodules.contains(first) && from_module.is_some() {
            if residual.is_empty() {
                vec![format!("{}::{}", from_module.as_deref().unwrap(), first)]
            } else {
                vec![format!(
                    "{}::{}::{}",
                    from_module.as_deref().unwrap(),
                    first,
                    residual
                )]
            }
        } else {
            let result = declared_usages
                .get(first)
                .map(|items| {
                    if residual.is_empty() {
                        items.iter().map(|(path, _, _)| path).cloned().collect()
                    } else {
                        items
                            .iter()
                            .map(|(path, _, _)| format!("{}::{}", path, residual))
                            .collect()
                    }
                })
                .unwrap_or_else(|| {
                    if residual.is_empty() {
                        vec![first.to_owned()]
                    } else {
                        vec![format!("{}::{}", first.to_owned(), residual)]
                    }
                });
            result
        }
    };
    full_paths
}

#[allow(clippy::type_complexity, clippy::too_many_arguments)]
pub fn substitute_with_canonical_identifiers(
    input: &str,
    offset: usize,
    crate_name: &Option<String>,
    from_module: &Option<String>,
    submodules: &IndexSet<String>,
    module_exports: &HashMap<String, IndexMap<String, Vec<(Export, Visibility)>>>,
    module_mappings: &HashMap<String, IndexMap<String, Option<Visibility>>>,
    visibility_unsupported_modules: &HashSet<String>,
    declared_usages: &IndexMap<String, Vec<(String, Option<Visibility>, UseBehaviour)>>,
    used_usages: &mut IndexMap<String, UseDefWithOffset>,
    allow_ambiguous: bool,
) -> Result<String, CanonicalizationFault> {
    let tokens = Tokenizer::new(input, true);
    let mut output = String::with_capacity(input.len());
    let mut in_substitution_position = true;

    for token in tokens {
        match token {
            Token::Identifier(ident, token_pos) => {
                if in_substitution_position {
                    let full_paths =
                        get_full_paths(ident, declared_usages, crate_name, from_module, submodules);

                    if !allow_ambiguous && full_paths.len() > 1 {
                        return Err(CanonicalizationFault::UsageParseErrorWithOffset(
                            "Ambiguous paths".to_string(),
                            offset + token_pos,
                        ));
                    }

                    for full_path in full_paths {
                        if let Some((module, item)) = full_path.rsplit_once("::") {
                            let mut canonicalized_result = Default::default();
                            let mut seen_modules = HashSet::new();
                            canonicalize_usage(
                                module_exports,
                                module_mappings,
                                visibility_unsupported_modules,
                                from_module,
                                module,
                                item,
                                allow_ambiguous,
                                &mut canonicalized_result,
                                &mut seen_modules,
                            )?;

                            for (canonical_module, canonical_item, _) in canonicalized_result {
                                if Some(canonical_module.as_ref()) == from_module.as_deref() {
                                    output.push_str(&canonical_item);
                                } else {
                                    used_usages
                                        .entry(module.to_owned())
                                        .or_insert_with(|| UseDefWithOffset {
                                            definition: UseDefinition {
                                                module: module.to_owned(),
                                                ..Default::default()
                                            },
                                            offset: offset + token_pos,
                                        })
                                        .definition
                                        .items
                                        .push(item.to_string());
                                    output.push_str(&canonical_item);
                                    output.push_str(&Composer::decorate(&canonical_module));
                                }
                            }
                        } else if full_path.find('"').is_some() {
                            // we don't want to replace local variables that shadow quoted module imports with the
                            // quoted name as that won't compile.
                            // since quoted items always refer to modules, we can just emit the original ident
                            // in this case
                            output.push_str(ident);
                        } else {
                            // if there are no quotes we do the replacement. this means that individually imported
                            // items can be used, and any shadowing local variables get harmlessly renamed.
                            // TODO: it can lead to weird errors, but such is life
                            output.push_str(&full_path);
                        }
                    }
                } else {
                    output.push_str(ident);
                }
            }
            Token::Other(other, _) => {
                output.push(other);
                if other == '.' || other == '@' {
                    in_substitution_position = false;
                    continue;
                }
            }
            Token::Whitespace(ws, _) => output.push_str(ws),
        }

        in_substitution_position = true;
    }

    Ok(output)
}

#[cfg(test)]
fn test_parse(
    input: &str,
) -> Result<IndexMap<String, Vec<(String, Option<Visibility>, UseBehaviour)>>, String> {
    let mut declared_imports = IndexMap::default();
    let mut declared_extensions = IndexSet::default();
    let mut declared_wildcard_usages = IndexSet::default();
    let submodules = IndexSet::default();

    parse_uses(
        input,
        &None,
        &None,
        &submodules,
        &mut declared_imports,
        &mut declared_wildcard_usages,
        &mut declared_extensions,
    )
    .map_err(|err| match err {
        UsageFault::UsageParseError(err) => ComposerErrorInner::UsageParseError(err, 0).to_string(),
        UsageFault::UsageParseErrorWithOffset(err, offset) => {
            ComposerErrorInner::UsageParseError(err, offset).to_string()
        }
        UsageFault::ComposerError(err) => err.to_string(),
    })?;
    Ok(declared_imports)
}

#[test]
fn import_tokens() {
    let input = r"
        use a::b
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([(
            "b".to_owned(),
            vec!(("a::b".to_owned(), None, UseBehaviour::ModuleSystem))
        )]))
    );

    let input = r"
        use a::{b, c}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "b".to_owned(),
                vec!(("a::b".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "c".to_owned(),
                vec!(("a::c".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
        ]))
    );

    let input = r"
        pub use a::{b as d, c}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "d".to_owned(),
                vec!((
                    "a::b".to_owned(),
                    Some(Visibility::Public),
                    UseBehaviour::ModuleSystem
                ))
            ),
            (
                "c".to_owned(),
                vec!((
                    "a::c".to_owned(),
                    Some(Visibility::Public),
                    UseBehaviour::ModuleSystem
                ))
            ),
        ]))
    );

    let input = r"
        use a::{b::{c, d}, e}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "c".to_owned(),
                vec!(("a::b::c".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "d".to_owned(),
                vec!(("a::b::d".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "e".to_owned(),
                vec!(("a::e".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
        ]))
    );

    let input = r"
        use a::b::{c, d}, e
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "c".to_owned(),
                vec!(("a::b::c".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "d".to_owned(),
                vec!(("a::b::d".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "e".to_owned(),
                vec!(("e".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
        ]))
    );

    let input = r"
        use a, b
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "a".to_owned(),
                vec!(("a".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "b".to_owned(),
                vec!(("b".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
        ]))
    );

    let input = r"
        use a::b c, d
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "c".to_owned(),
                vec!(("a::b::c".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "d".to_owned(),
                vec!(("a::b::d".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
        ]))
    );

    let input = r"
        use a::b c
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([(
            "c".to_owned(),
            vec!(("a::b::c".to_owned(), None, UseBehaviour::ModuleSystem))
        ),]))
    );

    let input = r"
        use a::b::{c::{d, e}, f, g::{h as i, j}}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "d".to_owned(),
                vec!(("a::b::c::d".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "e".to_owned(),
                vec!(("a::b::c::e".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "f".to_owned(),
                vec!(("a::b::f".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "i".to_owned(),
                vec!(("a::b::g::h".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "j".to_owned(),
                vec!(("a::b::g::j".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
        ]))
    );

    let input = r"
        use a::b::{
            c::{d, e},
            f,
            g::{
                h as i,
                j::k::l as m,
            }
        }
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "d".to_owned(),
                vec!(("a::b::c::d".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "e".to_owned(),
                vec!(("a::b::c::e".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "f".to_owned(),
                vec!(("a::b::f".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "i".to_owned(),
                vec!(("a::b::g::h".to_owned(), None, UseBehaviour::ModuleSystem))
            ),
            (
                "m".to_owned(),
                vec!((
                    "a::b::g::j::k::l".to_owned(),
                    None,
                    UseBehaviour::ModuleSystem
                ))
            ),
        ]))
    );

    let input = r#"
        use "path//with\ all sorts of .stuff"::{a, b}
    "#;
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "a".to_owned(),
                (vec!((
                    r#""path//with\ all sorts of .stuff"::a"#.to_owned(),
                    None,
                    UseBehaviour::ModuleSystem
                )))
            ),
            (
                "b".to_owned(),
                vec!((
                    r#""path//with\ all sorts of .stuff"::b"#.to_owned(),
                    None,
                    UseBehaviour::ModuleSystem
                ))
            ),
        ]))
    );

    let input = r"
        use a::b::{
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        use a::b::{{c}
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        use a::b::{c}}
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        use a::b{{c,d}}
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        use a:b
    ";
    assert!(test_parse(input).is_err());
}
