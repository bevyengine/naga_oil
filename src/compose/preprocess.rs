use std::{collections::HashSet, ops::Range};

use winnow::{
    combinator::{alt, empty, eof, fail, opt, peek, preceded, repeat, separated, seq, terminated},
    error::{ContextError, StrContext},
    stream::Recoverable,
    token::{any, none_of, one_of, take_till, take_while},
    Located, PResult, Parser,
};

use super::{composer::ImportDefWithOffset, ShaderDefValue};

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

#[derive(Debug)]
pub struct Preprocessed {
    pub parts: Vec<PreprocessorPart>,
}
impl Preprocessed {
    pub fn get_module_names<'a, 'b>(&'b self, input: &'a str) -> impl Iterator<Item = &'a str> + 'b
    where
        'a: 'b,
    {
        self.parts
            .iter()
            .filter_map(|v: &PreprocessorPart| match v {
                PreprocessorPart::DefineImportPath(DefineImportPath { path }) => {
                    path.as_ref().map(|v| &input[v.clone()])
                }
                _ => None,
            })
    }
    pub fn get_imports(&self, input: &str) -> Vec<FlattenedImport> {
        self.parts
            .iter()
            .filter_map(move |v| match v {
                PreprocessorPart::Import(v) => v.get_import(input),
                _ => None,
            })
            .flatten()
            .collect::<Vec<_>>()
    }

    pub fn get_used_defs(&self, input: &str) -> HashSet<String> {
        self.parts
            .iter()
            .filter_map(|v| match v {
                PreprocessorPart::If(v) => v.name.as_ref(),
                PreprocessorPart::IfOp(v) => v.name.as_ref(),
                PreprocessorPart::UseDefine(v) => v.name.as_ref(),
                _ => None,
            })
            .map(|v| input[v.clone()].to_owned())
            .collect()
    }

    pub fn get_defined_defs<'a>(
        &'a self,
        input: &'a str,
    ) -> impl Iterator<Item = (String, ShaderDefValue)> + 'a {
        self.parts.iter().filter_map(|v| match v {
            PreprocessorPart::DefineShaderDef(v) => {
                let name = match v.name.as_ref() {
                    Some(v) => input[v.clone()].to_owned(),
                    None => return None,
                };
                let value = match v.value.as_ref() {
                    Some(v) => ShaderDefValue::parse(&input[v.clone()]),
                    None => ShaderDefValue::default(),
                };
                Some((name, value))
            }
            _ => None,
        })
    }
}

impl ImportDirective {
    pub fn get_import(&self, input: &str) -> Option<Vec<FlattenedImport>> {
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
            offset: usize,
        ) -> Vec<FlattenedImport> {
            let (alias_range, path_ranges) = match tree {
                ImportTree::Path(path_ranges) => (None, path_ranges),
                ImportTree::Alias { path, alias } => (alias.clone(), path),
                ImportTree::Children { path, children } => {
                    let extended_stack = [stack, &to_stack(input, path)].concat();
                    return children
                        .iter()
                        .flat_map(|child| walk_import_tree(input, child, &extended_stack, offset))
                        .collect();
                }
            };

            let alias = alias_range.map(|v| input[v.clone()].to_owned());
            let path = [stack, &to_stack(input, &path_ranges)].concat().join("::");
            vec![FlattenedImport {
                alias,
                path,
                offset,
            }]
        }

        match &self.tree {
            Some((tree, range)) => Some(walk_import_tree(input, tree, &[], range.start)),
            None => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedIfOp<'a> {
    pub is_else_if: bool,
    pub name: &'a str,
    pub op: &'a str,
    pub value: &'a str,
}

impl IfOpDirective {
    pub fn resolve<'a>(&self, input: &'a str) -> Option<ResolvedIfOp<'a>> {
        Some(ResolvedIfOp {
            is_else_if: self.is_else_if,
            name: &input[self.name.as_ref()?.clone()],
            op: &input[self.op.as_ref()?.clone()],
            value: &input[self.value.as_ref()?.clone()],
        })
    }
}

#[derive(Debug)]
pub struct FlattenedImport {
    pub offset: usize,
    pub path: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone)]
pub enum PreprocessorPart {
    Version(VersionDirective),
    If(IfDefDirective),
    IfOp(IfOpDirective),
    Else(ElseDirective),
    EndIf(EndIfDirective),
    UseDefine(UseDefineDirective),
    DefineShaderDef(DefineShaderDef),
    DefineImportPath(DefineImportPath),
    Import(ImportDirective),
    UnknownDirective(Range<usize>),
    /// Normal shader code
    Text(Range<usize>),
}

// Note: This is a public API that lower level tools may use. It's a recoverable parser.
pub fn preprocess(input: &mut Input<'_>) -> PResult<Preprocessed> {
    // All of the directives start with a #.
    // And most of the directives have to be on their own line.
    let mut parts = Vec::new();
    let mut start_text = empty.span().parse_next(input)?.start;
    loop {
        // I'm at the start of a line. Let's try parsing a preprocessor directive.
        if let Some(_) = opt(spaces_single_line).parse_next(input)? {
            if let Some(_) = opt(peek('#').span()).parse_next(input)? {
                // It's a preprocessor directive
                let (part, span): (Option<_>, _) = alt((
                    version.map(PreprocessorPart::Version),
                    if_directive.map(|v| match v {
                        IfDirective::If(v) => PreprocessorPart::If(v),
                        IfDirective::IfOp(v) => PreprocessorPart::IfOp(v),
                        IfDirective::Else(v) => PreprocessorPart::Else(v),
                    }),
                    end_if_directive.map(PreprocessorPart::EndIf),
                    use_define_directive.map(PreprocessorPart::UseDefine),
                    define_import_path.map(PreprocessorPart::DefineImportPath),
                    define_shader_def.map(PreprocessorPart::DefineShaderDef),
                    import_directive.map(PreprocessorPart::Import),
                    fail.context(StrContext::Label("Unknown directive")),
                ))
                .resume_after(take_till(0.., is_newline_start).map(|_| ()))
                .with_span()
                .parse_next(input)?;
                parts.push(PreprocessorPart::Text(start_text..span.start));
                start_text = span.end;
                parts.push(part.unwrap_or_else(move || PreprocessorPart::UnknownDirective(span)));
                continue;
            }
        }

        // Normal line
        loop {
            let text = take_till(1.., |c: char| is_newline_start(c) || c == '#')
                .span()
                .parse_next(input)?;

            if let Some(_) = opt(new_line).parse_next(input)? {
                // Nice, we finished a line
                break;
            } else if let Some((use_define, span)) =
                opt(use_define_directive.with_span()).parse_next(input)?
            {
                parts.push(PreprocessorPart::Text(start_text..span.start));
                start_text = span.end;
                parts.push(PreprocessorPart::UseDefine(use_define));
                // Continue parsing the line
            } else if let Some(_) = opt(eof).parse_next(input)? {
                // We reached the end of the file
                parts.push(PreprocessorPart::Text(start_text..text.end));
                return Ok(Preprocessed { parts });
            } else {
                // It's a # that we don't care about
                // Skip it and continue parsing the line
                let _ = any.parse_next(input)?;
            }
        }
    }
}

#[derive(Debug, Clone)]
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
        _: spaces_single_line.resume_after(empty),
        version: take_while(1.., |c:char| c.is_ascii_digit()).span().resume_after(empty),
        _: spaces_until_new_line
    }}
    .parse_next(input)
}

/// Note: We're disallowing spaces between the `#` and the `ifdef`.
/// `#ifdef {name}` or `#else ifdef {name}` or `#ifndef {name}` or `#else ifndef {name`
#[derive(Debug, Clone)]
pub struct IfDefDirective {
    pub is_else_if: bool,
    pub is_not: bool,
    pub name: Option<Range<usize>>,
}

impl IfDefDirective {
    pub fn name<'a>(&'a self, input: &'a str) -> Option<&'a str> {
        self.name.as_ref().map(|v| &input[v.clone()])
    }
}

/// `#ifop {name} {op} {value}` or `#else ifop {name} {op} {value}`
#[derive(Debug, Clone)]
pub struct IfOpDirective {
    pub is_else_if: bool,
    pub name: Option<Range<usize>>,
    pub op: Option<Range<usize>>,
    pub value: Option<Range<usize>>,
}

pub enum IfDirective {
    If(IfDefDirective),
    IfOp(IfOpDirective),
    Else(ElseDirective),
}

pub fn if_directive(input: &mut Input<'_>) -> PResult<IfDirective> {
    #[derive(PartialEq, Eq)]
    enum Start {
        IfDef,
        IfNotDef,
        IfOp,
        Else,
    }
    let (start, is_else) = alt((
        "#ifop".map(|_| (Start::IfOp, false)),
        "#ifdef".map(|_| (Start::IfDef, false)),
        "#ifndef".map(|_| (Start::IfNotDef, false)),
        (
            "#else",
            spaces_single_line.resume_after(empty),
            alt((
                "ifdef".map(|_| Start::IfDef),
                "ifndef".map(|_| Start::IfNotDef),
                "ifop".map(|_| Start::IfOp),
                spaces_until_new_line.map(|_| Start::Else),
            )),
        )
            .map(|(_, _, next)| (next, true)),
    ))
    .parse_next(input)?;

    match start {
        Start::IfDef | Start::IfNotDef => {
            let _ = spaces_single_line.resume_after(empty).parse_next(input)?;
            let name = shader_def_name.resume_after(empty).parse_next(input)?;
            let _ = spaces_until_new_line.parse_next(input)?;
            Ok(IfDirective::If(IfDefDirective {
                is_else_if: is_else,
                is_not: start == Start::IfNotDef,
                name,
            }))
        }
        Start::IfOp => {
            let _ = spaces_single_line.resume_after(empty).parse_next(input)?;
            let name = shader_def_name.resume_after(empty).parse_next(input)?;
            let _ = opt(spaces_single_line).parse_next(input)?;
            let op = alt(("==", "!=", "<", "<=", ">", ">="))
                .span()
                .resume_after(empty)
                .parse_next(input)?;
            let _ = opt(spaces_single_line).parse_next(input)?;
            let value = shader_def_value.resume_after(empty).parse_next(input)?;
            let _ = spaces_until_new_line.parse_next(input)?;
            Ok(IfDirective::IfOp(IfOpDirective {
                is_else_if: is_else,
                name,
                op,
                value,
            }))
        }
        Start::Else => Ok(IfDirective::Else(ElseDirective)),
    }
}

/// `#else`
#[derive(Debug, Clone)]
pub struct ElseDirective;

/// `#endif`
#[derive(Debug, Clone)]
pub struct EndIfDirective;

pub fn end_if_directive(input: &mut Input<'_>) -> PResult<EndIfDirective> {
    seq! {EndIfDirective{
        _: "#endif",
        _: spaces_single_line.resume_after(empty),
        _: spaces_until_new_line
    }}
    .parse_next(input)
}

/// Note: We're disallowing the previous `#ANYTHING` syntax, since it's rarely used and error prone     
/// (a misspelled `#inport` would get mistaken for a `#ANYTHING``).
/// `#{name of defined value}``
#[derive(Debug, Clone)]
pub struct UseDefineDirective {
    pub name: Option<Range<usize>>,
}

impl UseDefineDirective {
    pub fn name<'a>(&'a self, input: &'a str) -> Option<&'a str> {
        self.name.as_ref().map(|v| &input[v.clone()])
    }
}

/// Remember that this one doesn't need to be on its own line
pub fn use_define_directive(input: &mut Input<'_>) -> PResult<UseDefineDirective> {
    seq! {UseDefineDirective{
        _: "#{",
        _: spaces_single_line.resume_after(empty),
        name: shader_def_name.resume_after(empty),
        _: spaces_single_line.resume_after(empty),
        _: "}".resume_after(empty)
    }}
    .parse_next(input)
}

/// `#define {name} {value}`, except it can only be used with other preprocessor macros.
/// Unlike its C cousin, it doesn't aggressively replace text.
#[derive(Debug, Clone)]
pub struct DefineShaderDef {
    pub name: Option<Range<usize>>,
    pub value: Option<Range<usize>>,
}

pub fn define_shader_def(input: &mut Input<'_>) -> PResult<DefineShaderDef> {
    // Technically I'm changing the #define behaviour
    // I'm no longer allowing redefining numbers, like #define 3 a|b
    seq! {DefineShaderDef{
        _: "#define",
        _: spaces_single_line.resume_after(empty),
        name: shader_def_name.resume_after(empty),
        _: spaces_single_line.resume_after(empty),
        value: opt(shader_def_value),
        _: spaces_until_new_line
    }}
    .parse_next(input)
}

fn shader_def_name(input: &mut Input<'_>) -> PResult<Range<usize>> {
    (
        one_of(|c: char| c.is_ascii_alphabetic() || c == '_'),
        take_while(0.., |c: char| c.is_ascii_alphanumeric() || c == '_'),
    )
        .span()
        .parse_next(input)
}

fn shader_def_value(input: &mut Input<'_>) -> PResult<Range<usize>> {
    take_while(1.., |c: char| {
        c.is_ascii_alphanumeric() || c == '_' || c == '-'
    })
    .span()
    .parse_next(input)
}

#[derive(Debug, Clone)]
pub struct DefineImportPath {
    pub path: Option<Range<usize>>,
}

pub fn define_import_path(input: &mut Input<'_>) -> PResult<DefineImportPath> {
    seq! {DefineImportPath{
        _: "#define_import_path",
        _: spaces_single_line.resume_after(empty),
        path: take_while(1.., |c: char| !c.is_whitespace()).span().resume_after(empty),
        _: spaces_until_new_line
    }}
    .parse_next(input)
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
#[derive(Debug, Clone)]
pub struct ImportDirective {
    pub root_specifier: Option<Range<usize>>,
    pub tree: Option<(ImportTree, Range<usize>)>,
}

pub fn import_directive(input: &mut Input<'_>) -> PResult<ImportDirective> {
    seq! {ImportDirective {
        _ : "#import",
        _: spaces.resume_after(empty),
        root_specifier: opt("::".span()),
        tree: import_tree.with_span().resume_after(empty),
        _: opt(spaces),
        _: opt(";"),
    }}
    .parse_next(input)
}

#[derive(Debug, Clone)]
pub enum ImportTree {
    Path(Vec<Range<usize>>),
    Alias {
        path: Vec<Range<usize>>,
        alias: Option<Range<usize>>,
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
        let as_token = opt("as").parse_next(input)?;
        if as_token.is_some() {
            let _ = spaces.resume_after(empty).parse_next(input)?;
            let alias = identifier.resume_after(empty).parse_next(input)?;
            return Ok(ImportTree::Alias { path, alias });
        }
    }

    if let Some(_) = opt("::{").parse_next(input)? {
        let _ = opt(spaces).parse_next(input)?;
        let children = import_trees
            .retry_after(
                // TODO: This recovery can explode and eat the whole file
                (take_till(0.., (',', '}')), ",").map(|_| ()),
            )
            // TODO: This recovery can explode and eat the whole file
            .resume_after((take_till(0.., '}'), '}').map(|_| ()))
            .parse_next(input)?
            .unwrap_or_default();

        let _ = opt(spaces).parse_next(input)?;
        let _ = "}".resume_after(empty).parse_next(input)?;
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

fn quoted_string(input: &mut Input<'_>) -> PResult<Range<usize>> {
    // See https://docs.rs/winnow/latest/winnow/_topic/json/index.html
    preceded(
        '\"',
        terminated(
            repeat(0.., string_character).fold(|| (), |a, _| a),
            '\"'.resume_after(empty),
        ),
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

pub struct Spaces {
    /// Comments that are in this "spaces" block
    pub comments: Vec<Range<usize>>,
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

/// Parses at least one non-newline whitespace or comment.
/// Preprocessor directives are always on their own line. So they need a slightly different spaces parser.
fn spaces_single_line(input: &mut Input<'_>) -> PResult<Spaces> {
    repeat(
        1..,
        alt((
            take_while(1.., |c: char| c.is_whitespace() && !is_newline_start(c)).map(|_| None),
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

/// Checks if it's part of a Unicode line break, according to https://www.w3.org/TR/WGSL/#line-break
fn is_newline_start(c: char) -> bool {
    c == '\u{000A}'
        || c == '\u{000B}'
        || c == '\u{000C}'
        || c == '\u{000D}'
        || c == '\u{0085}'
        || c == '\u{2028}'
        || c == '\u{2029}'
}

fn spaces_until_new_line(input: &mut Input<'_>) -> PResult<Spaces> {
    let spaces = opt(spaces_single_line).parse_next(input)?;
    let _newline = new_line
        .retry_after(take_till(0.., is_newline_start).map(|_| ()))
        .parse_next(input)?;
    Ok(spaces.unwrap_or(Spaces {
        comments: Vec::new(),
    }))
}

fn new_line(input: &mut Input<'_>) -> PResult<()> {
    alt((
        "\u{000D}\u{000A}".map(|_| ()),
        one_of(is_newline_start).map(|_| ()),
        eof.map(|_| ()),
    ))
    .parse_next(input)
}

fn single_line_comment(input: &mut Input<'_>) -> PResult<()> {
    let _start = "//".parse_next(input)?;
    let _text = take_till(0.., is_newline_start).parse_next(input)?;
    let _newline = new_line.parse_next(input)?;
    Ok(())
}

fn multi_line_comment(input: &mut Input<'_>) -> PResult<()> {
    let _start = "/*".parse_next(input)?;
    loop {
        if let Some(_end) = opt("*/").parse_next(input)? {
            return Ok(());
        } else if let Some(_) = opt(multi_line_comment).parse_next(input)? {
            // We found a nested comment, skip it
        } else {
            // Skip any other character
            // TODO: Eof error recovery
            let _ = take_till(1.., ('*', '/')).parse_next(input)?;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn replace_comments(input: &str) -> String {
        let mut input = input_new(input);
        let mut output = String::new();
        loop {
            if let Some(span) = opt(single_line_comment.span())
                .parse_next(&mut input)
                .unwrap()
            {
                output.push_str(&" ".repeat(span.len()));
            } else if let Some(span) = opt(multi_line_comment.span())
                .parse_next(&mut input)
                .unwrap()
            {
                output.push_str(&" ".repeat(span.len()));
            } else if let Some(v) = opt(any::<_, ContextError>).parse_next(&mut input).unwrap() {
                output.push(v);
            } else {
                let _ = eof::<_, ContextError>.parse_next(&mut input).unwrap();
                break;
            }
        }
        output
    }

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

    #[test]
    fn comment_test() {
        let input = r"
not commented
// line commented
not commented
/* block commented on a line */
not commented
// line comment with a /* block comment unterminated
not commented
/* block comment
   spanning lines */
not commented
/* block comment
   spanning lines and with // line comments
   even with a // line commented terminator */
not commented
";

        let replaced = replace_comments(input);
        assert_eq!(replaced.len(), input.len());
        assert_eq!(
            replaced
                .lines()
                .zip(input.lines())
                .find(|(line, original)| {
                    (*line != "not commented" && !line.chars().all(|c| c == ' '))
                        || line.len() != original.len()
                }),
            None
        );

        let partial_tests = [
            (
                "1.0 /* block comment with a partial line comment on the end *// 2.0",
                "1.0                                                           / 2.0",
            ),
            (
                "1.0 /* block comment with a partial block comment on the end */* 2.0",
                "1.0                                                            * 2.0",
            ),
            (
                "1.0 /* block comment 1 *//* block comment 2 */ * 2.0",
                "1.0                                            * 2.0",
            ),
            (
                "1.0 /* block comment with real line comment after */// line comment",
                "1.0                                                                ",
            ),
        ];

        for &(input, expected) in partial_tests.iter() {
            assert_eq!(&replace_comments(input), expected);
        }
    }
}
