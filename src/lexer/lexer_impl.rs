use super::Token::{self, *};
use nom;

pub struct Lexer {
    input: String,
    position: usize,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self { input, position: 0, tokens: Vec::new() }
    }
}
