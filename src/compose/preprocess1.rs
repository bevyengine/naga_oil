use std::ops::Range;

use winnow::{
    combinator::{
        alt, cut_err, delimited, empty, not, opt, preceded, repeat, separated, seq, terminated,
    },
    error::{ContextError, ParserError, StrContext, StrContextValue},
    stream::{AsChar, Recover, Recoverable},
    token::{any, none_of, one_of, take_till, take_until, take_while},
    Located, PResult, Parser, RecoverableParser, Stateful,
};

/**
 * The abstract syntax trees do not include spaces or comments. They are implicity there between adjacent tokens.
 * It is also missing a lot of filler tokens, like semicolons, commas, and braces.
 * The syntax tree only has ranges, and needs the original source code to extract the actual text.
 *
 * If we ever want to have a full concrete syntax tree, we should look into https://github.com/domenicquirl/cstree
 */

pub type Input<'a> = Recoverable<Located<&'a str>, ContextError>;
pub fn input_new(input: &str) -> Input {
    Recoverable::new(Located::new(input))
}
// Kinda useless, cause we got .span()
pub struct Token<T> {
    pub range: Range<usize>,
    pub inner: T,
}
impl<T> Token<T> {
    pub fn parse_span<'a, P>(parser: P) -> impl Parser<Input<'a>, Token<T>, ContextError>
    where
        P: Parser<Input<'a>, T, ContextError>,
    {
        let mut spanned_parser = parser.with_span();
        move |input: &mut Input<'a>| {
            let (inner, range) = spanned_parser.parse_next(input)?;
            Ok(Token { range, inner })
        }
    }
}
pub fn token<'a, T, P>(parser: P) -> impl Parser<Input<'a>, Token<T>, ContextError>
where
    P: Parser<Input<'a>, T, ContextError>,
{
    Token::<T>::parse_span(parser)
}

pub struct VersionDirective {
    version: Option<Range<usize>>,
}
impl VersionDirective {
    pub fn version_number(&self, input: &str) -> Option<u32> {
        self.version
            .as_ref()
            .and_then(|v| (&input[v.clone()]).parse::<u32>().ok())
    }
}

pub fn version(input: &mut Input<'_>) -> PResult<VersionDirective> {
    seq! {VersionDirective{
        _: "#version".span(),
        _: spaces.resume_after(empty),
        version: take_while(1.., |c:char| c.is_ascii_digit()).span().resume_after(empty),
        _: take_until_new_line
    }}
    .parse_next(input)
}

pub struct IfDefDirective {
    name: Range<usize>,
}

pub struct IfNDefDirective {
    name: Range<usize>,
}

pub struct IfOpDirective {
    name: Range<usize>,
    op: Range<usize>,
    value: Option<Range<usize>>,
}

pub struct ElseDirective;

pub struct EndIfDirective;

// TODO: Remove this function
pub fn import_start(input: &mut Input<'_>) -> PResult<Range<usize>> {
    let _ = opt(spaces).parse_next(input)?;
    let start = "#import".span().parse_next(input);
    let _ = cut_err(spaces)
        .context(StrContext::Label("space after #import"))
        .parse_next(input)?;
    let _ = repeat(0.., any).parse_next(input)?;
    start
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
    pub root_specifier: Option<Range<usize>>,
    pub tree: ImportTree,
}

pub fn import_directive(input: &mut Input<'_>) -> PResult<ImportDirective> {
    let start = token1("#import".span()).parse_next(input)?;
    let root_specifier = opt(token0("::".span())).parse_next(input)?;
    let (tree, tree_range) = cut_err(token0(import_tree))
        .context(StrContext::Label("imports"))
        .with_span()
        .parse_next(input)?;
    let semicolon = opt(";".span()).parse_next(input)?;
    let end = semicolon.map(|v| v.end).unwrap_or(tree_range.end);
    Ok(ImportDirective {
        root_specifier,
        tree,
    })
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

fn import_tree(input: &mut Input<'_>) -> PResult<ImportTree> {
    let path = path.parse_next(input)?;
    let s = opt(spaces).parse_next(input)?;

    if s.is_some() {
        if let Some(alias) = opt(preceded(token1("as"), identifier)).parse_next(input)? {
            return Ok(ImportTree::Alias { path, alias });
        }
    }
    if let Some(children) = opt(delimited(
        (token0("::"), token0("{")),
        token0(import_trees),
        "}",
    ))
    .parse_next(input)?
    {
        return Ok(ImportTree::Children { path, children });
    }

    Ok(ImportTree::Path(path))
}

fn import_trees(input: &mut Input<'_>) -> PResult<Vec<ImportTree>> {
    terminated(
        separated(1.., import_tree, (opt(spaces), ",", opt(spaces))),
        (opt(spaces), ","),
    )
    .parse_next(input)
}

fn path(input: &mut Input<'_>) -> PResult<Vec<Range<usize>>> {
    separated(
        1..,
        alt((nonempty_string, identifier)),
        (opt(spaces), "::", opt(spaces)),
    )
    .parse_next(input)
}

fn identifier(input: &mut Input<'_>) -> PResult<Range<usize>> {
    (
        one_of(|c: char| unicode_ident::is_xid_start(c)),
        take_while(0.., |c: char| unicode_ident::is_xid_continue(c)),
    )
        .span()
        .parse_next(input)
}

fn nonempty_string(input: &mut Input<'_>) -> PResult<Range<usize>> {
    quoted_string.verify(|s| s.len() > 2).parse_next(input)
}

pub fn quoted_string(input: &mut Input<'_>) -> PResult<Range<usize>> {
    // See https://docs.rs/winnow/latest/winnow/_topic/json/index.html
    preceded(
        '\"',
        cut_err(terminated(
            repeat(0.., string_character).fold(|| (), |a, _| a),
            '\"'.resume_after(empty),
        )),
    )
    .span()
    .parse_next(input)
}
fn string_character(input: &mut Input<'_>) -> PResult<()> {
    let c = none_of('\"').parse_next(input)?;
    if c == '\\' {
        let _ = any.parse_next(input)?;
    }
    Ok(())
}

pub struct DefineImportPath {
    pub path: Option<Range<usize>>,
}

pub fn define_import_path(input: &mut Input<'_>) -> PResult<DefineImportPath> {
    seq! {DefineImportPath{
        _: "#define_import_path",
        _: spaces.resume_after(empty),
        path: take_while(1.., |c: char| !c.is_whitespace()).span().resume_after(empty),
        _: take_until_new_line
    }}
    .parse_next(input)
}

pub struct DefineShaderDef {
    pub name: Option<Range<usize>>,
    pub value: Option<Range<usize>>,
}

pub fn define_shader_def(input: &mut Input<'_>) -> PResult<DefineShaderDef> {
    // Technically I'm changing the #define behaviour
    // I'm no longer allowing redefining numbers, like #define 3 a|b
    seq! {DefineShaderDef{
        _: "#define",
        _: spaces.resume_after(empty),
        name: (one_of(|c: char| c.is_ascii_alphabetic() || c == '_'), take_while(0.., |c: char| c.is_ascii_alphanumeric() || c == '_')).span().resume_after(empty),
        _: spaces.resume_after(empty),
        value: opt(take_while(1.., |c: char| c.is_ascii_alphanumeric()  || c == '_' || c == '-').span()),
        _: take_until_new_line
    }}
    .parse_next(input)
}

struct Spaces {
    /// Comments that are in this "spaces" block
    comments: Vec<Range<usize>>,
}

/// Parses at least one whitespace or comment.
fn spaces(input: &mut Input<'_>) -> PResult<Spaces> {
    repeat(
        1..,
        alt((
            take_while(1.., |c: char| c.is_whitespace()).map(|_| None),
            single_line_comment.span().map(|c| Some(c)),
            multi_line_comment.span().map(|c| Some(c)),
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
    .map(|comments| Spaces { comments })
    .parse_next(input)
}

/// Takes tokens until a WGSL newline is found. Also takes the newline.
pub fn take_until_new_line(input: &mut Input<'_>) -> PResult<()> {
    /// Checks if it's a Unicode line break, according to https://www.w3.org/TR/WGSL/#line-break
    fn is_newline(c: char) -> bool {
        c == '\u{000A}'
            || c == '\u{000B}'
            || c == '\u{000C}'
            || c == '\u{000D}'
            || c == '\u{0085}'
            || c == '\u{2028}'
            || c == '\u{2029}'
    }

    let _text = take_till(0.., is_newline).parse_next(input)?;
    let newline = one_of(is_newline).parse_next(input)?;
    // carriage return (U+000D) followed by line feed (U+000A) is treated as a single line terminator
    if newline == '\u{000D}' {
        let _lf = opt(one_of('\u{000A}')).parse_next(input)?;
    }
    Ok(())
}

pub fn single_line_comment(input: &mut Input<'_>) -> PResult<()> {
    let _start = "//".parse_next(input)?;
    let _text = take_until_new_line.parse_next(input)?;
    Ok(())
}

pub fn multi_line_comment(input: &mut Input<'_>) -> PResult<()> {
    let _start = "/*".parse_next(input)?;
    loop {
        if let Some(_end) = opt("*/").parse_next(input)? {
            return Ok(());
        } else if let Some(_) = opt(multi_line_comment).parse_next(input)? {
            // We found a nested comment, skip it
        } else {
            // Skip any other character
            let _ = take_till(1.., ('*', '/')).parse_next(input)?;
        }
    }
}

/// Parses a token and ignores any spaces or comments after it.
pub fn token0<'a, O, P>(mut parser: P) -> impl Parser<Input<'a>, O, ContextError>
where
    P: Parser<Input<'a>, O, ContextError>,
{
    move |input: &mut Input<'a>| {
        // Spaces and comments after the token are ignored
        // We have to parse the ones afterwards, because parsing the ones before & backtracking would be slow
        let result = parser.parse_next(input);
        let _ = opt(spaces).parse_next(input)?;

        result
    }
}

/// Parses a token and needs at least one space or comment after it.
pub fn token1<'a, O, P>(mut parser: P) -> impl Parser<Input<'a>, O, ContextError>
where
    P: Parser<Input<'a>, O, ContextError>,
{
    move |input: &mut Input<'a>| {
        // Spaces and comments after the token are ignored
        // We have to parse the ones afterwards, because parsing the ones before & backtracking would be slow
        let result = parser.parse_next(input);
        let _ = cut_err(spaces).parse_next(input)?;
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_simple() {
        let input = "a::b::c";
        assert_eq!(
            path.parse(input_new(input)).ok(),
            Some(vec![0..1, 3..4, 6..7])
        );
    }
    #[test]
    fn test_path_trailing() {
        // It shouldn't eat the trailing character
        let input = "a::b::c::";
        assert_eq!(path.parse(input_new(input)).ok(), None);
        let mut inp = input_new(input);
        assert_eq!(
            path.with_span().parse_next(&mut inp).ok(),
            Some((vec![0..1, 3..4, 6..7], 0..7,))
        );
        // assert_eq!("::".parse_next(&mut inp).ok(), Some("::"));
    }
}
