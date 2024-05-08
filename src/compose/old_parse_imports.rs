use std::ops::Range;

use indexmap::IndexMap;
use winnow::{stream::Recoverable, Located, Parser};

use crate::compose::preprocess1::{import_directive, ImportTree};

use super::{
    composer::{ImportDefWithOffset, ImportDefinition},
    tokenizer::{Token, Tokenizer},
    Composer,
};

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
