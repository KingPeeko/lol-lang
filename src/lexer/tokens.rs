// Tokens for the Summoner's Code lexer

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Literal(Literal),
    Type(Type),
    Operator(Operator),
    Symbol(Symbol),
    Eof, // End of file
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Nexus,
    Ability,
    Buy,
    Coinflip,
    Ff15,
    GoNext,
    Ping,
    Recall,
    True,
    False,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    GoldLit(String),
    ChatLit(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Gold,
    Status,
    Chat,
    Void,
    Duo,
    Inventory,
    Shop,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Assignment, // as in 'buy a = 5;'
    // Binary operators
    Plus,
    Minus, // This is also the unary operator for negative numbers, ie '-53'
    Mult,
    Divide,
    Modulo,
    Equals,
    NotEquals,
    // LessThan,
    LessEquals,
    // GreaterThan,
    GreaterEquals,
    And,
    Or,
    // Unary operators
    Negate, // as in '!true'
}

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    SquareClose,
    AngleOpen,
    AngleClose,

    Semicolon,
    Colon,
    Comma,
    Arrow, // '->'
}


use Token::*;
impl Token {
    pub fn is_keyword(&self) -> bool {
        matches!(self, Keyword(..))
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Literal(..))
    }

    pub fn is_type(&self) -> bool {
        matches!(self, Type(..))
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, Operator(..))
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self, Symbol(..))
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Identifier(..))
    }

    pub fn is_eof(&self) -> bool {
        matches!(self, Eof)
    }
}
