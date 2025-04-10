use nom::error::{ErrorKind, Error};

#[derive(Debug, Clone)]
pub enum TokenizerError<'a> {
    Failure{
        input: &'a str,
        fail_idx: usize,
        kind: ErrorKind,
    },
    Incomplete,
}

impl<'a> TokenizerError<'a> {

    // Create tokenizer error from a nom error.
    // Needs also the original input to calculate what index the fail happened
    pub fn from_nom_err(value: nom::Err<Error<&'a str>>, original_input: &'a str) -> Self {
        use nom::Err::*;
        use nom::Offset;

        match value {
            Error(e) | Failure(e) => {
                let fail_idx = original_input.offset(e.input);
                Self::Failure {
                    input: original_input,
                    fail_idx,
                    kind: e.code
                }
            }
            _ => Self::Incomplete,
        }
    }
}



impl<'a> std::fmt::Display for TokenizerError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self::Failure{ input, fail_idx, kind: _ } = self else {
            return write!(f, "Tokenizer error: incomplete input")
        };

        // Line number of where parse failed and
        // index at which aforementioned line starts
        let (failing_line_number, failing_line_idx) = 
            input
            .chars()
            .take(fail_idx + 1)
            .enumerate()
            .fold((0, 0), |(line_num, line_idx), (i, c)| {
                if c != '\n' { return (line_num, line_idx)}
                (line_num + 1, i + 1)
            });

        // Column at which the parsing failed
        let fail_column_idx = fail_idx - failing_line_idx;

        let Some(fail_line_string) = input.lines().nth(failing_line_number) else {
            return write!(f, "Tokernizer error: failure") // Should be unreachable
        };

        // Amount of spaces for caret to be on the start of the failed parse
        let spaces = str::repeat(" ", fail_column_idx);

        let output = format!(
"Tokenizing failed: line {} col {}
{fail_line_string}
{spaces}^", failing_line_number + 1, fail_column_idx + 1);
        write!(f, "{output}")
    }
}
