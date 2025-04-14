use crate::lexer::tokens::*;

use super::error::ParseError;

pub struct TokenStream<'a> {
    tokens: &'a [Token],
    pos: usize,
    saved_poses: Vec<usize>,
}

type Err = ParseError;

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            pos: 0,
            saved_poses: Vec::new(),
        }
    }

    // Get current token
    pub fn peek(&self) -> Result<&'a Token, Err> {
        self.tokens.get(self.pos).ok_or(ParseError::Eof)
    }

    pub fn peek_ahead(&self, ahead_by: usize) -> Result<&'a Token, Err> {
        self.tokens.get(self.pos + ahead_by).ok_or(ParseError::Eof)
    }

    // Save pos to saved_poses stack
    pub fn save_pos(&mut self) {
        self.saved_poses.push(self.pos);
    }

    // Load the last saved position from stack, must also call pop_saved_pos after if the loaded
    // pos should be removed from the stack
    pub fn load_pos(&mut self) -> Result<(), Err> {
        let last_pos = self.saved_poses.last();

        match last_pos {
            Some(n) => {
                self.pos = *n;
                Ok(())
            } 

            None => Err(ParseError::NoSavedPos),
        }
    }

    // Pop the last saved position from stack
    pub fn pop_saved_pos(&mut self) -> Result<usize, Err> {
        self.saved_poses.pop().ok_or(ParseError::NoSavedPos)
    }

    // Get current token and move forward
    pub fn advance(&mut self) -> Result<&'a Token, Err> {
        let token = self.peek();
        if token.is_ok() {
            self.pos += 1;
        }
        token
    }

    // Move pos forward by `move_by`
    pub fn move_forward(&mut self, move_by: usize) {
        self.pos += move_by;
    }

    // Get current token, move forward if the predicate evaluates to true
    pub fn expect<F>(&mut self, predicate: F) -> Result<&'a Token, ParseError>
    where
        F: FnOnce(&Token) -> bool,
    {
        let token = self.peek()?;

        if predicate(token) {
            let _ = self.advance();
            return Ok(token);
        }

        Err(ParseError::UnexpectedToken)
    }

    pub fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_stream() {
        let input = r#"
nexus() {
/all noob team go die
    buy my_variable = "Hello world!";
    ping(my_variable); /all this is a print statement; buy x = 5;
    recall 5 + 9;
} /all This is the best language ever!
"#;
        let tokens = crate::lexer::tokenize(input);

        let binding = tokens.unwrap();
        let mut stream = TokenStream::new(&binding);

        use crate::lexer::util::*;
        let should_be = vec![
            keyword("nexus"),
            sym("("),
            sym(")"),
            sym("{"),
            keyword("buy"),
            ident("my_variable"),
            op("="),
            chat_lit("Hello world!"),
            sym(";"),
            keyword("ping"),
            sym("("),
            ident("my_variable"),
            sym(")"),
            sym(";"),
            keyword("recall"),
            gold_lit(5),
            op("+"),
            gold_lit(9),
            sym(";"),
            sym("}"),
            Token::Eof,
        ];

        should_be.into_iter().for_each(|tok| {
            assert!(stream.expect(|token| matches!(token, tok)).is_ok());
        })
    }
}
