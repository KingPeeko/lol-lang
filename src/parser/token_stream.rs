use crate::lexer::tokens::*;

#[derive(Debug, Clone, Copy)]
pub struct TokenStream<'a> {
    tokens: &'a [Token],
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }
}

impl<'a> nom::Input for TokenStream<'a> {
    type Item = &'a Token;

    type Iter = std::slice::Iter<'a, Token>;

    type IterIndices = std::iter::Enumerate<Self::Iter>;

    fn input_len(&self) -> usize {
        self.tokens.len()
    }

    fn take(&self, index: usize) -> Self {
        Self::new(&self.tokens[..index])
    }

    fn take_from(&self, index: usize) -> Self {
        Self::new(&self.tokens[index..])
    }

    fn take_split(&self, index: usize) -> (Self, Self) {
        (self.take(index), self.take_from(index))
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tokens.iter().position(predicate)
    }

    fn iter_elements(&self) -> Self::Iter {
        self.tokens.iter()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.tokens.iter().enumerate()
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if count <= self.input_len() {
            Ok(count)
        } else {
            Err(nom::Needed::new(count - self.input_len()))
        }
    }
}
