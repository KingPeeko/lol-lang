//  AST for Summoner's Code.

// All expressions of the language. Derives Debug and Clone.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Unit,
    Identifier(String),
    Binary {
        left: Box<Expr>,
        operator: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        operator: UnaryOp,
        right: Box<Expr>,
    },
    Call {
        callee: String,
        args: Vec<Expr>,
    },
    Group(Box<Expr>),
    Duo(Box<Expr>, Box<Expr>),
    Inventory(Vec<Expr>),
    Shop(Vec<(Expr, Expr)>),
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
    },
}

// Binary operators.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,          // +
    Subtract,     // -
    Multiply,     // *
    Divide,       // /
    Modulo,       // %
    Equal,        // ==
    NotEqual,     // !=
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
    And,          // &&
    Or,           // ||
}

// Unary operators.
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate, // -
    Not,    // !
}

// All types of the language. Derives PartialEq so they can be compared.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Gold,                       // integer
    Status,                     // boolean
    Chat,                       // string
    Void,                       // unit
    Duo(Box<Type>, Box<Type>),  // tuple
    Inventory(Box<Type>),       // list
    Shop(Box<Type>, Box<Type>), // map
}

// Statements of the language.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Vec<Statement>),
    ItemDecl {
        name: String,
        ty: Type,
        initializer: Expr,
    },
    Assignment {
        name: String,
        value: Expr,
    },
    Coinflip {
        condition: Expr,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    GoNext {
        condition: Expr,
        body: Box<Statement>,
    },
    Ping {
        value: Expr,
    },
    Recall {
        value: Option<Expr>,
    },
    Expression(Expr),
}

//  Parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type, // Parameter type, cannot be named type because of syntax.
}

// Declarations of the language.
#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Ability {
        name: String,
        params: Vec<Param>,
        return_type: Type,
        body: Vec<Statement>,
    },
    Item {
        name: String,
        ty: Type,
        initializer: Expr,
    },
    Nexus {
        body: Vec<Statement>,
    },
}

// The program itself.
#[derive(Debug, PartialEq)]
pub struct Program {
    pub declarations: Vec<Decl>,
}

pub struct InvalidBinaryOp;

impl TryFrom<crate::lexer::Token> for BinaryOp {
    type Error = InvalidBinaryOp;

    fn try_from(value: crate::lexer::Token) -> Result<Self, Self::Error> {
        use crate::lexer::{Operator::*, Symbol::*, Token};

        match value {
            Token::Operator(Plus) => Ok(Self::Add),
            Token::Operator(Minus) => Ok(Self::Subtract),
            Token::Operator(Mult) => Ok(Self::Multiply),
            Token::Operator(Divide) => Ok(Self::Divide),
            Token::Operator(Modulo) => Ok(Self::Modulo),
            Token::Operator(Equals) => Ok(Self::Equal),
            Token::Operator(NotEquals) => Ok(Self::NotEqual),
            Token::Operator(LessEquals) => Ok(Self::LessEqual),
            Token::Operator(GreaterEquals) => Ok(Self::GreaterEqual),
            Token::Operator(And) => Ok(Self::And),
            Token::Operator(Or) => Ok(Self::Or),

            Token::Symbol(AngleOpen) => Ok(Self::Less),
            Token::Symbol(AngleClose) => Ok(Self::Greater),

            _ => Err(InvalidBinaryOp),
        }
    }
}

pub fn operator_precedence(op: &BinaryOp) -> u8 {
    use BinaryOp::*;
    match op {
        Multiply | Divide | Modulo => 4,
        Add | Subtract => 3,
        Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual => 2,
        And => 1,
        Or => 0,
    }
}

pub struct InvalidUnaryOp;
impl TryFrom<crate::lexer::Token> for UnaryOp {
    type Error = InvalidUnaryOp;
    fn try_from(value: crate::lexer::Token) -> Result<Self, Self::Error> {
        use crate::lexer::{Operator::*, Token};
        match value {
            Token::Operator(Minus) => Ok(Self::Negate),
            Token::Operator(Not) => Ok(Self::Not),

            _ => Err(InvalidUnaryOp),
        }
    }
}
