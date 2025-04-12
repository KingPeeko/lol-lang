use crate::lexer::tokens::*;

#[derive(Debug, Clone)]
pub enum ParseError {
    Eof,
    DeclError,
    UnexpectedToken,
    ParseLitError,
}
