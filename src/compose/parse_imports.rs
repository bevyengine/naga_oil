use std::ops::Range;

use indexmap::IndexMap;
use winnow::{stream::Recoverable, Located, Parser};

use crate::compose::preprocess1::{import_directive, ImportTree};

use super::{
    tokenizer::{Token, Tokenizer},
    Composer, ImportDefWithOffset, ImportDefinition,
};

pub fn parse_imports<'a>(
    input: &'a str,
    declared_imports: &mut IndexMap<String, Vec<String>>,
) -> Result<(), (&'a str, usize)> {
    let input = input.trim();
    let imports = import_directive
        .parse(Recoverable::new(Located::new(input)))
        .map_err(|_v| {
            // panic!("{:#?}", _v);
            ("failed to parse imports", 0)
        })?;

    fn to_stack<'a>(input: &'a str, ranges: &[Range<usize>]) -> Vec<&'a str> {
        ranges
            .iter()
            .map(|range| &input[range.clone()])
            .collect::<Vec<_>>()
    }

    fn walk_import_tree<'a>(
        input: &'a str,
        tree: &ImportTree,
        stack: &[&'a str],
        declared_imports: &mut IndexMap<String, Vec<String>>,
    ) {
        let (name_range, path_ranges) = match tree {
            ImportTree::Path(path_ranges) => (path_ranges.last().unwrap().clone(), path_ranges),
            ImportTree::Alias {
                path: path_ranges,
                alias: alias_range,
            } => (alias_range.clone(), path_ranges),
            ImportTree::Children { path, children } => {
                let extended_stack = [stack, &to_stack(input, path)].concat();
                for child in children {
                    walk_import_tree(input, child, &extended_stack, declared_imports);
                }
                return;
            }
        };

        let name = input[name_range].to_string();
        let extended_stack = [stack, &to_stack(input, &path_ranges)].concat();
        declared_imports
            .entry(name)
            .or_default()
            .push(extended_stack.join("::"));
    }

    walk_import_tree(input, &imports.tree, &[], declared_imports);

    Ok(())
}

pub fn substitute_identifiers(
    input: &str,
    offset: usize,
    declared_imports: &IndexMap<String, Vec<String>>,
    used_imports: &mut IndexMap<String, ImportDefWithOffset>,
    allow_ambiguous: bool,
) -> Result<String, usize> {
    let tokens = Tokenizer::new(input, true);
    let mut output = String::with_capacity(input.len());
    let mut in_substitution_position = true;

    for token in tokens {
        match token {
            Token::Identifier(ident, token_pos) => {
                if in_substitution_position {
                    let (first, residual) = ident.split_once("::").unwrap_or((ident, ""));
                    let full_paths = declared_imports
                        .get(first)
                        .cloned()
                        .unwrap_or(vec![first.to_owned()]);

                    if !allow_ambiguous && full_paths.len() > 1 {
                        return Err(offset + token_pos);
                    }

                    for mut full_path in full_paths {
                        if !residual.is_empty() {
                            full_path.push_str("::");
                            full_path.push_str(residual);
                        }

                        if let Some((module, item)) = full_path.rsplit_once("::") {
                            used_imports
                                .entry(module.to_owned())
                                .or_insert_with(|| ImportDefWithOffset {
                                    definition: ImportDefinition {
                                        import: module.to_owned(),
                                        ..Default::default()
                                    },
                                    offset: offset + token_pos,
                                })
                                .definition
                                .items
                                .push(item.to_owned());
                            output.push_str(item);
                            output.push_str(&Composer::decorate(module));
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
fn test_parse(input: &str) -> Result<IndexMap<String, Vec<String>>, (&str, usize)> {
    let mut declared_imports = IndexMap::default();
    parse_imports(input, &mut declared_imports)?;
    Ok(declared_imports)
}

#[test]
fn import_tokens() {
    let input = r"
        #import a::b
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([(
            "b".to_owned(),
            vec!("a::b".to_owned())
        )]))
    );

    let input = r"
        #import a::{b, c}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("b".to_owned(), vec!("a::b".to_owned())),
            ("c".to_owned(), vec!("a::c".to_owned())),
        ]))
    );

    let input = r"
        #import a::{b as d, c}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("d".to_owned(), vec!("a::b".to_owned())),
            ("c".to_owned(), vec!("a::c".to_owned())),
        ]))
    );

    let input = r"
        #import a::{b::{c, d}, e}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("c".to_owned(), vec!("a::b::c".to_owned())),
            ("d".to_owned(), vec!("a::b::d".to_owned())),
            ("e".to_owned(), vec!("a::e".to_owned())),
        ]))
    );

    let input = r"
        #import a::b::{c, d}, e
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("c".to_owned(), vec!("a::b::c".to_owned())),
            ("d".to_owned(), vec!("a::b::d".to_owned())),
            ("e".to_owned(), vec!("e".to_owned())),
        ]))
    );

    let input = r"
        #import a, b
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("a".to_owned(), vec!("a".to_owned())),
            ("b".to_owned(), vec!("b".to_owned())),
        ]))
    );

    let input = r"
        #import a::b c, d
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("c".to_owned(), vec!("a::b::c".to_owned())),
            ("d".to_owned(), vec!("a::b::d".to_owned())),
        ]))
    );

    let input = r"
        #import a::b c
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([(
            "c".to_owned(),
            vec!("a::b::c".to_owned())
        ),]))
    );

    let input = r"
        #import a::b::{c::{d, e}, f, g::{h as i, j}}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("d".to_owned(), vec!("a::b::c::d".to_owned())),
            ("e".to_owned(), vec!("a::b::c::e".to_owned())),
            ("f".to_owned(), vec!("a::b::f".to_owned())),
            ("i".to_owned(), vec!("a::b::g::h".to_owned())),
            ("j".to_owned(), vec!("a::b::g::j".to_owned())),
        ]))
    );

    let input = r"
        #import a::b::{
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
            ("d".to_owned(), vec!("a::b::c::d".to_owned())),
            ("e".to_owned(), vec!("a::b::c::e".to_owned())),
            ("f".to_owned(), vec!("a::b::f".to_owned())),
            ("i".to_owned(), vec!("a::b::g::h".to_owned())),
            ("m".to_owned(), vec!("a::b::g::j::k::l".to_owned())),
        ]))
    );

    let input = r#"
        #import "path//with\ all sorts of .stuff"::{a, b}
    "#;
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "a".to_owned(),
                vec!(r#""path//with\ all sorts of .stuff"::a"#.to_owned())
            ),
            (
                "b".to_owned(),
                vec!(r#""path//with\ all sorts of .stuff"::b"#.to_owned())
            ),
        ]))
    );

    let input = r"
        #import a::b::{
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        #import a::b::{{c}
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        #import a::b::{c}}
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        #import a::b{{c,d}}
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        #import a:b
    ";
    assert!(test_parse(input).is_err());
}
