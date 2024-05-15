use std::collections::HashMap;

use winnow::{
    combinator::{alt, eof, fail, opt, peek},
    error::StrContext,
    token::any,
    PResult, Parser, Stateful,
};

use crate::compose::preprocess::ResolvedIfOp;

use super::{
    composer::{
        get_imported_module, ComposableModuleDefinition, ComposerError, ComposerErrorInner,
        ErrSource, ImportDefWithOffset, ModuleImports, ShaderDefValue,
    },
    preprocess::{IfDefDirective, IfOpDirective, PreprocessorPart},
};

type Stream<'a> = Stateful<&'a [PreprocessorPart], PreprocessingState<'a>>;

pub(super) fn preprocess<'a>(
    module_sets: &'a ModuleImports<'a>,
    module: &'a ComposableModuleDefinition,
    shader_defs: &'a HashMap<String, ShaderDefValue>,
) -> Result<PreprocessOutput, ComposerError> {
    let state = PreprocessingState {
        module_sets,
        module,
        shader_defs,
    };
    let stream = Stream {
        input: &module.parsed.parts,
        state,
    };

    top_level_preprocessor
        .parse(stream)
        .map_err(|inner| ComposerError {
            inner: ComposerErrorInner::PreprocessorError(
                vec![inner.into_inner().to_string()].into(),
            ),
            source: ErrSource::Module {
                name: module.name.0.to_owned(),
                offset: 0,
                defs: shader_defs.clone(),
            },
        })
}

#[derive(Debug, Clone)]
struct PreprocessingState<'a> {
    module_sets: &'a ModuleImports<'a>,
    module: &'a ComposableModuleDefinition,
    shader_defs: &'a HashMap<String, ShaderDefValue>,
}

// TODO: Check what the error messages are like (do they include a location?)
fn top_level_preprocessor<'a>(input: &mut Stream<'a>) -> PResult<PreprocessOutput> {
    let mut source = String::new();
    let mut imports = Vec::new();
    loop {
        if let Some(result) = opt(if_statement).parse_next(input)? {
            source += &result;
            continue;
        }

        let token = match opt(any).parse_next(input)? {
            Some(v) => v,
            None => break,
        };
        match token {
            PreprocessorPart::Version(_) => { /* ignore */ }
            // TODO: Can I get away without the context?
            PreprocessorPart::If(_) => {
                return fail
                    .context(StrContext::Label("unexpected #ifdef"))
                    .parse_next(input)?;
            }
            PreprocessorPart::IfOp(_) => {
                return fail
                    .context(StrContext::Label("unexpected #if"))
                    .parse_next(input)?;
            }
            PreprocessorPart::Else(_) => {
                return fail
                    .context(StrContext::Label("unmatched else"))
                    .parse_next(input)?;
            }
            PreprocessorPart::EndIf(_) => {
                return fail
                    .context(StrContext::Label("unmatched end-if"))
                    .parse_next(input)?;
            }
            PreprocessorPart::UseDefine(def) => {
                let define = def.name(&input.state.module.source).unwrap();
                let value = input.state.shader_defs.get(define).unwrap();
                source += &value.value_as_string();
            }
            PreprocessorPart::DefineShaderDef(_) => { /* ignore */ }
            PreprocessorPart::DefineImportPath(_) => { /* ignore */ }
            PreprocessorPart::Import(directive) => {
                for import in directive
                    .get_import(&input.state.module.source)
                    .unwrap()
                    .into_iter()
                {
                    let import =
                        get_imported_module(&input.state.module_sets.modules, &import).unwrap(); // TODO: Error handling

                    imports.push(import)
                }
            }
            PreprocessorPart::UnknownDirective(_) => {
                return fail
                    .context(StrContext::Label("unknown directive"))
                    .parse_next(input)?;
            }
            PreprocessorPart::Text(range) => {
                source += &input.state.module.source[range.clone()];
            }
        }
    }
    Ok(PreprocessOutput { source, imports })
}

#[derive(Debug, Clone)]
enum IfOrIfOp {
    If(IfDefDirective),
    IfOp(IfOpDirective),
}

#[derive(Debug)]
enum IfEnd {
    ElseIf(IfDefDirective),
    ElseIfOp(IfOpDirective),
    Else,
    EndIf,
    Eof,
}

fn if_statement<'a>(input: &mut Stream<'a>) -> PResult<String> {
    let start = any
        .verify_map(|token| match token {
            PreprocessorPart::If(if_def) if !if_def.is_else_if => Some(IfOrIfOp::If(if_def)),
            PreprocessorPart::IfOp(if_def) if !if_def.is_else_if => Some(IfOrIfOp::IfOp(if_def)),
            _ => None,
        })
        .parse_next(input)?;

    let mut source = String::new();
    let is_true_branch = match start {
        IfOrIfOp::If(if_def) => {
            let define = if_def.name(&input.state.module.source).unwrap();
            let mut result = input.state.shader_defs.contains_key(define);
            if if_def.is_not {
                result = !result;
            }
            result
        }
        IfOrIfOp::IfOp(if_op) => {
            let ResolvedIfOp {
                name, op, value, ..
            } = if_op.resolve(&input.state.module.source).unwrap();
            let name_value = input.state.shader_defs.get(name).unwrap();
            act_on(name_value.value_as_string().as_str(), value, op).unwrap() // TODO: Error handling
        }
    };

    if is_true_branch {
        let (source_add, _) = block.parse_next(input)?;
        source += &source_add;
        // Skip all the next blocks until we reach the end
        loop {
            let next_block = block_end.parse_next(input)?.unwrap();
            match next_block {
                IfEnd::ElseIf(_) | IfEnd::ElseIfOp(_) | IfEnd::Else => {
                    let _ = skip_block.parse_next(input)?;
                }
                IfEnd::EndIf => break,
                IfEnd::Eof => fail
                    .context(StrContext::Label("expected #endif"))
                    .parse_next(input)?,
            }
        }
    } else {
        let peek_next_block = skip_block.parse_next(input)?;
        // And handle the various else cases
        match peek_next_block {
            IfEnd::ElseIf(_) => source += &if_statement.parse_next(input)?,
            IfEnd::ElseIfOp(_) => source += &if_statement.parse_next(input)?,
            IfEnd::Else => {
                let _ = block_end.parse_next(input)?;
                let (source_add, peeked_block_end) = block.parse_next(input)?;
                source += &source_add;
                let _ = block_end.parse_next(input)?;
                if matches!(peeked_block_end, IfEnd::EndIf) {
                    return fail
                        .context(StrContext::Label("else block must end with #endif"))
                        .parse_next(input)?;
                }
            }
            IfEnd::EndIf => { /* done */ }
            IfEnd::Eof => fail
                .context(StrContext::Label("expected #endif"))
                .parse_next(input)?,
        };
    }
    Ok(source)
}

fn block_end<'a>(input: &mut Stream<'a>) -> PResult<Option<IfEnd>> {
    alt((
        eof.map(|_| Some(IfEnd::Eof)),
        any.map(|token| match token {
            PreprocessorPart::If(if_def) if if_def.is_else_if => Some(IfEnd::ElseIf(if_def)),
            PreprocessorPart::IfOp(if_op) if if_op.is_else_if => Some(IfEnd::ElseIfOp(if_op)),
            PreprocessorPart::Else(_) => Some(IfEnd::Else),
            PreprocessorPart::EndIf(_) => Some(IfEnd::EndIf),
            _ => None,
        }),
    ))
    .parse_next(input)
}

fn block<'a>(input: &mut Stream<'a>) -> PResult<(String, IfEnd)> {
    let mut source = String::new();
    loop {
        if let Some(block_end) = peek(block_end).parse_next(input)? {
            return Ok((source, block_end));
        }

        if let Some(result) = opt(if_statement).parse_next(input)? {
            source += &result;
            continue;
        }

        let token = opt(any).parse_next(input)?.unwrap();
        match token {
            PreprocessorPart::Version(_) => {
                return fail
                    .context(StrContext::Label("#version must be at the top of the file"))
                    .parse_next(input)?;
            }
            PreprocessorPart::If(_) => {
                return fail
                    .context(StrContext::Label("unexpected #ifdef"))
                    .parse_next(input)?;
            }
            PreprocessorPart::IfOp(_) => {
                return fail
                    .context(StrContext::Label("unexpected #if"))
                    .parse_next(input)?;
            }
            PreprocessorPart::Else(_) => {
                return fail
                    .context(StrContext::Label("unmatched else"))
                    .parse_next(input)?;
            }
            PreprocessorPart::EndIf(_) => {
                return fail
                    .context(StrContext::Label("unmatched end-if"))
                    .parse_next(input)?;
            }
            PreprocessorPart::UseDefine(def) => {
                let define = def.name(&input.state.module.source).unwrap();
                let value = input.state.shader_defs.get(define).unwrap();
                source += &value.value_as_string();
            }
            PreprocessorPart::DefineShaderDef(_) => {
                return fail
                    .context(StrContext::Label("#define must be at the top of the file"))
                    .parse_next(input)?;
            }
            PreprocessorPart::DefineImportPath(_) => {
                return fail
                    .context(StrContext::Label(
                        "#define_import_path must be at the top of the file",
                    ))
                    .parse_next(input)?;
            }
            PreprocessorPart::Import(_) => {
                return fail
                    .context(StrContext::Label("only top-level imports are allowed"))
                    .parse_next(input)?;
            }
            PreprocessorPart::UnknownDirective(_) => {
                return fail
                    .context(StrContext::Label("unknown directive"))
                    .parse_next(input)?;
            }
            PreprocessorPart::Text(range) => {
                source += &input.state.module.source[range.clone()];
            }
        }
    }
}

fn skip_block<'a>(input: &mut Stream<'a>) -> PResult<IfEnd> {
    loop {
        if let Some(if_end) = peek(block_end).parse_next(input)? {
            return Ok(if_end);
        }
        any.parse_next(input)?;
    }
}

fn act_on(a: &str, b: &str, op: &str) -> Result<bool, ()> {
    match op {
        "==" => Ok(a == b),
        "!=" => Ok(a != b),
        ">" => Ok(a > b),
        ">=" => Ok(a >= b),
        "<" => Ok(a < b),
        "<=" => Ok(a <= b),
        _ => Err(()),
    }
}

#[derive(Debug)]
pub struct PreprocessOutput {
    pub source: String,
    pub imports: Vec<ImportDefWithOffset>,
    // TODO: Implement this, or justify why it's shouldn't be a thing yet
    // When the user uses `a::b` in the source code, we mangle it to a random name before parsing.
    // This map is used to resolve the mangled names back to the original names.
    // pub source_alias_to_path: HashMap<String, String>,
}
