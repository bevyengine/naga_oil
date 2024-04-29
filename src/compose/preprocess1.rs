use std::ops::Range;

use winnow::{
    combinator::{alt, cut_err, delimited, opt, preceded, repeat, separated, terminated},
    error::{ContextError, ParserError, StrContext},
    token::{any, none_of, one_of, take_while},
    Located, PResult, Parser,
};

use super::comment_strip_iter::{multi_line_comment, quoted_string, single_line_comment};

/**
 * The abstract syntax trees do not include spaces or comments. They are implicity there between adjacent tokens.
 * It is also missing a lot of filler tokens, like semicolons, commas, and braces.
 * The syntax tree only has ranges, and needs the original source code to extract the actual text.
 *
 * If we ever want to have a full concrete syntax tree, we should look into https://github.com/domenicquirl/cstree
 */

// TODO: Count line breaks *after the fact* https://www.w3.org/TR/WGSL/#line-break
// TODO: Strategically add "cut_error" for better error messages

pub struct VersionDirective {
    pub version_number: u32,
    start: Range<usize>,
    version: Range<usize>,
}

pub fn version(input: &mut Located<&str>) -> PResult<VersionDirective> {
    let start = "#version".span().parse_next(input)?;
    let _ = spaces.parse_next(input)?; // We expect at least one space
    let (version_number, version) = take_while(1.., |c: char| c.is_ascii_digit())
        .verify_map(|v: &str| v.parse::<u32>().ok())
        .with_span()
        .parse_next(input)?;
    Ok(VersionDirective {
        version_number,
        start,
        version,
    })
}

/// Formal grammar
/// ```ebnf
/// <import> ::= "#import" <s> "::"? <import_tree> ";"?
///
/// <import_tree> ::= <path> (<s> "as" <s> <identifier> | "::" "{" <import_trees> "}")?
/// <import_trees> ::= <import_tree> ("," <import_tree>)* ","?
///
/// <path> ::= (<identifier> | <nonempty_string>) ("::" (<identifier> | <nonempty_string>) )*
/// <identifier> ::= ([a-z]) ([a-z] | [0-9])*
/// <nonempty_string> ::= "\"" <string_char>+ "\""
/// <string_char> ::= [a-z]
///
/// <s> ::= " "+
/// ```
///
/// Can be tested on https://bnfplayground.pauliankline.com/
///
/// Except that
/// - `<s>` should be Unicode aware
/// - `<identifier>` should use the XID rules instead of only allowing lowercase letters
/// - `<nonempty_string>` should be a string with at least one character, and follow the usual "quotes and \\ backslash for escaping" rules
/// - spaces are allowed between every token
/// ```
pub struct ImportDirective {
    range: Range<usize>,
    pub root_specifier: Option<Range<usize>>,
    pub tree: ImportTree,
}

// TODO: Remove this function
pub fn import_start(input: &mut Located<&str>) -> PResult<Range<usize>> {
    let _ = opt(spaces).parse_next(input)?;
    let start = "#import".span().parse_next(input);
    let _ = cut_err(spaces)
        .context(StrContext::Label("space after #import"))
        .parse_next(input)?;
    let _ = repeat(0.., any).parse_next(input)?;
    start
}

pub fn import_directive(input: &mut Located<&str>) -> PResult<ImportDirective> {
    let start = "#import".span().parse_next(input)?;
    let _ = cut_err(spaces)
        .context(StrContext::Label("space after #import"))
        .parse_next(input)?;
    let root_specifier = opt("::".span()).parse_next(input)?;
    let (tree, tree_range) = cut_err(import_tree)
        .context(StrContext::Label("imports"))
        .with_span()
        .parse_next(input)?;
    let semicolon = opt(token(";")).parse_next(input)?;
    let end = semicolon.map(|v| v.end).unwrap_or(tree_range.end);
    Ok(ImportDirective {
        range: start.start..end,
        root_specifier,
        tree,
    })
}

#[test]
fn testx() {
    let code = "#import a::{b, c}";
    let input = Located::new(code);
    let result = import_directive.parse(input).map(|_| ());
    assert_eq!(result, Ok(()));
}

#[derive(Debug, Clone)]
pub enum ImportTree {
    Path(Vec<Range<usize>>),
    Alias {
        path: Vec<Range<usize>>,
        alias: Range<usize>,
    },
    Children {
        path: Vec<Range<usize>>,
        children: Vec<ImportTree>,
    },
}

fn import_tree(input: &mut Located<&str>) -> PResult<ImportTree> {
    let path = path.parse_next(input)?;

    if let Some(alias) = opt(preceded((spaces, "as", spaces), identifier)).parse_next(input)? {
        return Ok(ImportTree::Alias { path, alias });
    }
    if let Some(children) = opt(delimited(
        (opt(spaces), "::", "{", opt(spaces)),
        import_trees,
        (opt(spaces), "}", opt(spaces)),
    ))
    .parse_next(input)?
    {
        return Ok(ImportTree::Children { path, children });
    }

    Ok(ImportTree::Path(path))
}

fn import_trees(input: &mut Located<&str>) -> PResult<Vec<ImportTree>> {
    separated(1.., import_tree, (token(","), opt(spaces))).parse_next(input)
}

fn path(input: &mut Located<&str>) -> PResult<Vec<Range<usize>>> {
    separated(
        1..,
        alt((nonempty_string, identifier)),
        (token("::"), opt(spaces)),
    )
    .parse_next(input)
}

fn identifier(input: &mut Located<&str>) -> PResult<Range<usize>> {
    (
        one_of(|c: char| unicode_ident::is_xid_start(c)),
        take_while(0.., |c: char| unicode_ident::is_xid_continue(c)),
    )
        .span()
        .parse_next(input)
}

fn nonempty_string(input: &mut Located<&str>) -> PResult<Range<usize>> {
    quoted_string.verify(|s| s.len() > 2).parse_next(input)
}

pub struct SimplePath {}

pub struct DefineImportPath {
    start: Range<usize>,
    pub path: Range<usize>,
}

pub fn define_import_path(input: &mut Located<&str>) -> PResult<DefineImportPath> {
    let start = "#define_import_path".span().parse_next(input)?;
    let _ = spaces.parse_next(input)?;
    let path = take_while(1.., |c: char| !c.is_whitespace())
        .span()
        .parse_next(input)?;
    Ok(DefineImportPath { start, path })
}

pub struct DefineShaderDef {
    start: Range<usize>,
    pub name: Range<usize>,
    pub value: Option<Range<usize>>,
}

pub fn define_shader_def(input: &mut Located<&str>) -> PResult<DefineShaderDef> {
    let start = "#define".span().parse_next(input)?;
    let _ = spaces.parse_next(input)?;
    let name = take_while(1.., |c: char| c.is_alphanumeric() || c == '_')
        .span()
        .parse_next(input)?;
    let value = opt(preceded(
        spaces,
        take_while(1.., |c: char| c.is_alphanumeric() || c == '-').span(),
    ))
    .parse_next(input)?;

    Ok(DefineShaderDef { start, name, value })
}

struct Spaces {
    range: Range<usize>,
    /// Comments that are in this "spaces" block
    comments: Vec<Range<usize>>,
}

/// Parses at least one whitespace or comment.
fn spaces(input: &mut Located<&str>) -> PResult<Spaces> {
    repeat(
        1..,
        alt((
            take_while(1.., |c: char| c.is_whitespace()).map(|_| None),
            single_line_comment.map(|c| Some(c)),
            multi_line_comment.map(|c| Some(c)),
        )),
    )
    .fold(
        || Vec::new(),
        |mut comments, comment| {
            if let Some(comment) = comment {
                comments.push(comment);
            }
            comments
        },
    )
    .with_span()
    .map(|(comments, range)| Spaces { range, comments })
    .parse_next(input)
}

/// Parses a token and ignores any spaces or comments before it.
pub fn token<'a, O, P>(parser: P) -> impl Parser<Located<&'a str>, Range<usize>, ContextError>
where
    P: Parser<Located<&'a str>, O, ContextError>,
{
    let mut span_parser = parser.span();
    move |input: &mut Located<&'a str>| {
        // Optional spaces and comments before the token are ignored
        let _ = opt(spaces).parse_next(input)?;
        span_parser.parse_next(input)
    }
}
