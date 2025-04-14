use super::tokens::*;
use std::str::FromStr;

pub struct ParseKeywordError;

impl FromStr for Keyword {
    type Err = ParseKeywordError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Keyword::*;
        match s {
            "nexus" => Ok(Nexus),
            "ability" => Ok(Ability),
            "buy" => Ok(Buy),
            "coinflip" => Ok(Coinflip),
            "ff15" => Ok(Ff15),
            "go next" => Ok(GoNext),
            "ping" => Ok(Ping),
            "recall" => Ok(Recall),
            "true" => Ok(True),
            "false" => Ok(False),

            _ => Err(ParseKeywordError),
        }
    }
}

pub struct ParseTypeError;

impl FromStr for Type {
    type Err = ParseTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Type::*;
        match s {
            "Gold" => Ok(Gold),
            "Status" => Ok(Status),
            "Chat" => Ok(Chat),
            "Void" => Ok(Void),
            "Duo" => Ok(Duo),
            "Inventory" => Ok(Inventory),
            "Shop" => Ok(Shop),

            _ => Err(ParseTypeError),
        }
    }
}

pub struct ParseSymbolError;

impl FromStr for Symbol {
    type Err = ParseSymbolError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Symbol::*;
        match s {
            "(" => Ok(ParenOpen),
            ")" => Ok(ParenClose),
            "{" => Ok(CurlyOpen),
            "}" => Ok(CurlyClose),
            "[" => Ok(SquareOpen),
            "]" => Ok(SquareClose),
            "<" => Ok(AngleOpen),
            ">" => Ok(AngleClose),
            ";" => Ok(Semicolon),
            ":" => Ok(Colon),
            "," => Ok(Comma),
            "->" => Ok(Arrow),

            _ => Err(ParseSymbolError),
        }
    }
}

pub struct ParseOperatorError;

impl FromStr for Operator {
    type Err = ParseOperatorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Operator::*;
        match s {
            "=" => Ok(Assignment),
            "+" => Ok(Plus),
            "-" => Ok(Minus),
            "*" => Ok(Mult),
            "/" => Ok(Divide),
            "%" => Ok(Modulo),
            "==" => Ok(Equals),
            "!=" => Ok(NotEquals),
            "<=" => Ok(LessEquals),
            ">=" => Ok(GreaterEquals),
            "&&" => Ok(And),
            "||" => Ok(Or),
            "!" => Ok(Negate),

            _ => Err(ParseOperatorError),
        }
    }
}

pub fn chat_lit(s: &str) -> Token {
    Token::Literal(Literal::ChatLit(s.to_string()))
}

pub fn gold_lit(n: i64) -> Token {
    Token::Literal(Literal::GoldLit(n.to_string()))
}

pub fn op(s: &str) -> Token {
    let Ok(operator) = Operator::from_str(s) else {
        panic!("Invalid operator");
    };

    Token::Operator(operator)
}

pub fn sym(s: &str) -> Token {
    let Ok(symbol) = Symbol::from_str(s) else {
        panic!("Invalid symbol");
    };

    Token::Symbol(symbol)
}

pub fn keyword(s: &str) -> Token {
    let Ok(keyword) = Keyword::from_str(s) else {
        panic!("Invalid keyword");
    };

    Token::Keyword(keyword)
}

pub fn ty(s: &str) -> Token {
    let Ok(ty) = Type::from_str(s) else {
        panic!("Invalid type");
    };

    Token::Type(ty)
}

pub fn ident(s: &str) -> Token {
    Token::Identifier(s.to_string())
}
