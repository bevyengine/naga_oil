use std::{borrow::Cow, ops::Range};

use winnow::{
    ascii::till_line_ending,
    combinator::{cut_err, opt},
    error::StrContext,
    token::any,
    Located, PResult, Parser,
};

struct SourceCode {
    /** Sorted pieces of the source code without any gaps */
    parts: Vec<SourceCodePart>,
}

enum SourceCodePart {
    Text(Range<usize>),
    SingleLineComment(SingleLineComment),
    MultiLineComment(MultiLineComment),
}

impl SourceCodePart {
    fn span(&self) -> Range<usize> {
        match self {
            SourceCodePart::Text(span) => span.clone(),
            SourceCodePart::SingleLineComment(comment) => comment.span.clone(),
            SourceCodePart::MultiLineComment(comment) => comment.span.clone(),
        }
    }
}

pub struct SingleLineComment {
    pub span: Range<usize>,
}
pub struct MultiLineComment {
    pub span: Range<usize>,
}

fn parse_source(input: &mut Located<&str>) -> PResult<SourceCode> {
    let mut parts = Vec::new();
    loop {
        if input.is_empty() {
            break;
        }
        if let Some(part) = opt(single_line_comment).parse_next(input)? {
            parts.push(SourceCodePart::SingleLineComment(part));
        } else if let Some(part) = opt(multi_line_comment).parse_next(input)? {
            parts.push(SourceCodePart::MultiLineComment(part));
        } else {
            let text_span = any.span().parse_next(input)?;
            if let Some(SourceCodePart::Text(last_span)) = parts.last_mut() {
                last_span.end = text_span.end;
            } else {
                parts.push(SourceCodePart::Text(text_span));
            }
        }
    }
    Ok(SourceCode { parts })
}

fn single_line_comment(input: &mut Located<&str>) -> PResult<SingleLineComment> {
    let start_span = "//".span().parse_next(input)?;
    let text_span = till_line_ending.span().parse_next(input)?;
    Ok(SingleLineComment {
        span: start_span.start..text_span.end,
    })
}
fn multi_line_comment(input: &mut Located<&str>) -> PResult<MultiLineComment> {
    let start_span = "/*".span().parse_next(input)?;
    loop {
        if let Some(end_span) = opt("*/".span()).parse_next(input)? {
            return Ok(MultiLineComment {
                span: start_span.start..end_span.end,
            });
        } else if let Some(_) = opt(multi_line_comment).parse_next(input)? {
            // We found a nested comment, skip it
        } else {
            // Skip a single character
            let _ = cut_err(any)
                .context(StrContext::Label("multiline comment"))
                .parse_next(input)?;
        }
    }
}

pub struct CommentReplaceIter<'a> {
    text: &'a str,
    text_index: usize,
    parsed: SourceCode,
    parsed_index: usize,
}

fn clamp_range(range: Range<usize>, min: usize, max: usize) -> Range<usize> {
    range.start.clamp(min, max)..range.end.clamp(min, max)
}

impl<'a> Iterator for CommentReplaceIter<'a> {
    type Item = (Cow<'a, str>, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        if self.text_index >= self.text.len() {
            return None;
        }

        let line_start = self.text_index;
        let line_end = self.text[line_start..]
            .find('\n') // TODO: Handle \r\n
            .map(|i| line_start + i + 1)
            .unwrap_or_else(|| self.text.len());
        self.text_index = line_end;

        let mut parts = Vec::new();
        for (i, parsed_part) in self.parsed.parts.iter().enumerate().skip(self.parsed_index) {
            let span = parsed_part.span();
            if span.start >= line_end {
                break;
            }
            if span.end <= line_start {
                self.parsed_index = i + 1;
                continue;
            }
            parts.push((parsed_part, clamp_range(span, line_start, line_end)));
        }

        assert!(parts.len() > 0);

        // Fast path
        if parts.len() == 1 {
            match parts.into_iter().next().unwrap() {
                (SourceCodePart::Text(_), span) => {
                    return Some((
                        Cow::Borrowed(&self.text[span]),
                        &self.text[line_start..line_end],
                    ));
                }
                (
                    SourceCodePart::SingleLineComment(_) | SourceCodePart::MultiLineComment(_),
                    span,
                ) => {
                    let spaces = " ".repeat(span.len());
                    return Some((Cow::Owned(spaces), &self.text[line_start..line_end]));
                }
            }
        }

        let mut output = String::new();
        let mut last_end = line_start;
        for (part, span) in parts.into_iter() {
            output.push_str(&self.text[last_end..span.start]);
            last_end = span.end;
            match part {
                SourceCodePart::Text(_) => {
                    output.push_str(&self.text[span]);
                }
                SourceCodePart::SingleLineComment(_) | SourceCodePart::MultiLineComment(_) => {
                    output.extend(std::iter::repeat(' ').take(span.len()));
                }
            }
        }

        assert!(last_end == line_end);
        Some((Cow::Owned(output), &self.text[line_start..line_end]))
    }
}

/// Gives you an iterator that replaces comments in the input text with spaces.
/// The iterator will yield the same lines as the input text, but with comments replaced.
/// Lines will include the newline character at the end!
pub fn replace_comments(input: &str) -> CommentReplaceIter {
    let parsed = parse_source(&mut Located::new(input)).unwrap();
    CommentReplaceIter {
        text: input,
        text_index: 0,
        parsed,
        parsed_index: 0,
    }
}

#[test]
fn comment_test() {
    const INPUT: &str = r"
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

    assert_eq!(
        replace_comments(INPUT).find(|(line, original)| {
            (line.trim_end() != "not commented" && !line.chars().all(|c| c == ' ' || c == '\n'))
                || line.len() != original.len()
        }),
        None
    );

    const PARTIAL_TESTS: [(&str, &str); 4] = [
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

    for &(input, expected) in PARTIAL_TESTS.iter() {
        let nasty_processed = replace_comments(input).next().unwrap().0;
        assert_eq!(&nasty_processed, expected);
    }
}
