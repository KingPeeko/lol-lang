use super::token_stream::TokenStream;
use crate::ast::*;
use crate::lexer::tokens::{Keyword, Literal, Operator, Symbol, Token};

use nom::combinator::opt;
use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::value,
    error::{ErrorKind, ParseError},
    multi::{fold_many1, many0, separated_list0},
    Err::Error,
    IResult, Input, Parser,
};

// type TokenStream<'a> = &'a [Token];

type TokType = crate::lexer::tokens::Type;
type AstType = crate::ast::Type;

///////////////////////////////////////////////UTIL FUNCTIONS///////////////////////////////////////////////////
// Utility function for creating a nom error
fn create_error(input: TokenStream, kind: ErrorKind) -> nom::Err<nom::error::Error<TokenStream>> {
    Error(ParseError::from_error_kind(input, kind))
}

// Parses any token, only fails if input is empty
fn any_token(input: TokenStream) -> IResult<TokenStream, Token> {
    let (rest, first) = take(1usize).parse(input)?;

    let Some(first_token) = first.iter_elements().next() else {
        return Err(create_error(input, ErrorKind::Eof));
    };

    Ok((rest, first_token.clone()))
}

// Parses identifier and returns its contained String
fn identifier(input: TokenStream) -> IResult<TokenStream, String> {
    let (rest, first) = any_token.parse(input)?;

    match first {
        Token::Identifier(s) => Ok((rest, s.clone())),
        _ => Err(create_error(input, ErrorKind::Tag)),
    }
}

// Parses the parameter keyword
fn keyword(expected: Keyword) -> impl Fn(TokenStream) -> IResult<TokenStream, Keyword> {
    move |input: TokenStream| {
        let (rest, first) = any_token.parse(input)?;

        match first {
            Token::Keyword(kw) if kw == expected => Ok((rest, kw.clone())),
            _ => Err(create_error(input, ErrorKind::Tag)),
        }
    }
}

// Parses a gold literal and returns the i64 it contains
fn gold_lit(input: TokenStream) -> IResult<TokenStream, i64> {
    let (rest, first) = any_token.parse(input)?;

    match first {
        Token::Literal(Literal::GoldLit(n)) => match n.parse::<i64>() {
            Ok(num) => Ok((rest, num)),
            Err(_) => Err(create_error(input, ErrorKind::Fail)),
        },

        _ => Err(create_error(input, ErrorKind::Tag)),
    }
}

// Parse a chat literal and return the String it contains
fn chat_lit(input: TokenStream) -> IResult<TokenStream, String> {
    let (rest, first) = any_token.parse(input)?;

    match first {
        Token::Literal(Literal::ChatLit(s)) => Ok((rest, s.clone())),
        _ => Err(create_error(input, ErrorKind::Tag)),
    }
}

// Parse the parameter type token
fn typ(expected: TokType) -> impl Fn(TokenStream) -> IResult<TokenStream, TokType> {
    move |input: TokenStream| {
        let (rest, first) = any_token.parse(input)?;

        match first {
            Token::Type(typ) if typ == expected => Ok((rest, typ.clone())),
            _ => Err(create_error(input, ErrorKind::Tag)),
        }
    }
}

// Parse any type token
fn typ_general(input: TokenStream) -> IResult<TokenStream, TokType> {
    let (rest, first) = any_token.parse(input)?;

    match first {
        Token::Type(typ_token) => Ok((rest, typ_token.clone())),
        _ => Err(create_error(input, ErrorKind::Tag)),
    }
}

// Parse the given operator token
fn operator(expected: Operator) -> impl Fn(TokenStream) -> IResult<TokenStream, Operator> {
    move |input: TokenStream| {
        let (rest, first) = any_token.parse(input)?;

        match first {
            Token::Operator(op) if op == expected => Ok((rest, op.clone())),
            _ => Err(create_error(input, ErrorKind::Tag)),
        }
    }
}

// Parse any operator token
fn operator_general(input: TokenStream) -> IResult<TokenStream, Operator> {
    let (rest, first) = any_token.parse(input)?;

    match first {
        Token::Operator(op) => Ok((rest, op.clone())),
        _ => Err(create_error(input, ErrorKind::Tag)),
    }
}

// Parse the parameter symbol token
fn symbol(expected: Symbol) -> impl Fn(TokenStream) -> IResult<TokenStream, Symbol> {
    move |input: TokenStream| {
        let (rest, first) = any_token.parse(input)?;

        match first {
            Token::Symbol(sym) if sym == expected => Ok((rest, sym.clone())),
            _ => Err(create_error(input, ErrorKind::Tag)),
        }
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Parse into a type AST
fn parse_type(input: TokenStream) -> IResult<TokenStream, AstType> {
    alt((
        parse_simple_type,
        parse_shop_type,
        parse_inventory_type,
        parse_duo_type,
    ))
    .parse(input)
}

// Parses into type AST, which doesn't contain other types
fn parse_simple_type(input: TokenStream) -> IResult<TokenStream, AstType> {
    let (rest, typ_token) = typ_general.parse(input)?;

    let ast_type = match typ_token {
        TokType::Gold => AstType::Gold,
        TokType::Chat => AstType::Chat,
        TokType::Void => AstType::Void,
        TokType::Status => AstType::Status,
        _ => return Err(create_error(input, ErrorKind::Tag)),
    };

    Ok((rest, ast_type))
}

// Parses a Shop type into AST
fn parse_shop_type(input: TokenStream) -> IResult<TokenStream, AstType> {
    (
        typ(TokType::Shop),
        symbol(Symbol::AngleOpen),
        parse_type,
        symbol(Symbol::Comma),
        parse_type,
        symbol(Symbol::AngleClose),
    )
        .map(|(_, _, typ1, _, typ2, _)| AstType::Shop(Box::new(typ1), Box::new(typ2)))
        .parse(input)
}

// Parses a Duo type into AST
fn parse_duo_type(input: TokenStream) -> IResult<TokenStream, AstType> {
    (
        typ(TokType::Duo),
        symbol(Symbol::AngleOpen),
        parse_type,
        symbol(Symbol::Comma),
        parse_type,
        symbol(Symbol::AngleClose),
    )
        .map(|(_, _, typ1, _, typ2, _)| AstType::Duo(Box::new(typ1), Box::new(typ2)))
        .parse(input)
}

// Parses an Inventory type into AST
fn parse_inventory_type(input: TokenStream) -> IResult<TokenStream, AstType> {
    (
        typ(TokType::Inventory),
        symbol(Symbol::AngleOpen),
        parse_type,
        symbol(Symbol::AngleClose),
    )
        .map(|(_, _, typ, _)| AstType::Inventory(Box::new(typ)))
        .parse(input)
}

// Parse into an Expr AST
fn parse_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    alt((parse_index_expr, parse_primary_expr, parse_binary_expr)).parse(input)
}

// Parse primary expressions, AKA expressions that don't have an expr as their first part
fn parse_primary_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    alt((
        parse_group_expr,
        parse_duo_expr,
        parse_inventory_expr,
        parse_shop_expr,
        parse_unary_expr,
        parse_call_expr,
        parse_literal_expr,
        parse_identifier,
    ))
    .parse(input)
}

// Parse into Expr::Identifier
fn parse_identifier(input: TokenStream) -> IResult<TokenStream, Expr> {
    identifier.map(Expr::Identifier).parse(input)
}

// Parse into Expr literal type
fn parse_literal_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    alt((
        parse_status_lit,
        parse_void_lit,
        parse_gold_lit,
        parse_chat_lit,
    ))
    .parse(input)
}

// Parse into Expr::Unit
fn parse_void_lit(input: TokenStream) -> IResult<TokenStream, Expr> {
    value(
        Expr::Unit,
        (symbol(Symbol::ParenOpen), symbol(Symbol::ParenClose)),
    )
    .parse(input)
}

// Parse into Expr::Boolean
fn parse_status_lit(input: TokenStream) -> IResult<TokenStream, Expr> {
    alt((
        value(Expr::Boolean(true), keyword(Keyword::True)),
        value(Expr::Boolean(false), keyword(Keyword::False)),
    ))
    .parse(input)
}

// Parse into Expr::String
fn parse_chat_lit(input: TokenStream) -> IResult<TokenStream, Expr> {
    chat_lit.map(Expr::String).parse(input)
}

// Parse into Expr::Integer
fn parse_gold_lit(input: TokenStream) -> IResult<TokenStream, Expr> {
    gold_lit.map(Expr::Integer).parse(input)
}

// Parse into Expr::Group
fn parse_group_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    (
        symbol(Symbol::ParenOpen),
        parse_expr,
        symbol(Symbol::ParenClose),
    )
        .map(|(_, expr, _)| Expr::Group(Box::new(expr)))
        .parse(input)
}

// Parse into Expr::Binary
fn parse_binary_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    let (rest, expr) = parse_primary_expr.parse(input)?;
    parse_binary_expr_prec(expr, 0).parse(rest)
}

// Recursive function called in parse_binary_expr
// Parses binary expressions while respecting operator precedence, ie. multiplication binds
// stronger than addition or subtraction
fn parse_binary_expr_prec(
    lhs: Expr,
    min_precedence: u8,
) -> impl Fn(TokenStream) -> IResult<TokenStream, Expr> {
    move |input: TokenStream| {
        let Ok((rest, op)) = parse_binary_op.parse(input) else {
            return Ok((input, lhs.clone()));
        };

        // If the parsed operators precedence is less then minimum, return the accumulated
        // expression `lhs`
        let op_precedence = operator_precedence(&op);
        if op_precedence < min_precedence {
            return Ok((input, lhs.clone()));
        }

        // Otherwise parse a new rhs
        let (rest, rhs) = parse_primary_expr.parse(rest)?;

        // Check if parsing further produces an even higher precedence binary operator, and if so,
        // make that the new rhs. Needed for left-associativeness
        let next_min_precedence = op_precedence + 1;
        let (rest, rhs) = parse_binary_expr_prec(rhs, next_min_precedence).parse(rest)?;

        // Create a new lhs for parsing with the original precedence
        let lhs = Expr::Binary {
            left: Box::new(lhs.clone()),
            operator: op,
            right: Box::new(rhs),
        };

        parse_binary_expr_prec(lhs, min_precedence).parse(rest)
    }
}

// Parse into BinaryOp
fn parse_binary_op(input: TokenStream) -> IResult<TokenStream, BinaryOp> {
    any_token.map_res(BinaryOp::try_from).parse(input)
}

// Parse into Expr::Unary
fn parse_unary_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    (parse_unary_op, parse_expr)
        .map(|(op, expr)| Expr::Unary {
            operator: op,
            right: Box::new(expr),
        })
        .parse(input)
}

// Parse into UnaryOp
fn parse_unary_op(input: TokenStream) -> IResult<TokenStream, UnaryOp> {
    any_token.map_res(UnaryOp::try_from).parse(input)
}

// Parse into Expr::Inventory
fn parse_inventory_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    (
        symbol(Symbol::SquareOpen),
        separated_list0(symbol(Symbol::Comma), parse_expr),
        symbol(Symbol::SquareClose),
    )
        .map(|(_, items, _)| Expr::Inventory(items))
        .parse(input)
}

// Parse Expr::Duo
fn parse_duo_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    (
        symbol(Symbol::ParenOpen),
        parse_expr,
        symbol(Symbol::Comma),
        parse_expr,
        symbol(Symbol::ParenClose),
    )
        .map(|(_, expr1, _, expr2, _)| Expr::Duo(Box::new(expr1), Box::new(expr2)))
        .parse(input)
}

// Parse Expr::Shop
fn parse_shop_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    (
        symbol(Symbol::CurlyOpen),
        separated_list0(symbol(Symbol::Comma), parse_shop_item),
        symbol(Symbol::CurlyClose),
    )
        .map(|(_, vec, _)| Expr::Shop(vec))
        .parse(input)
}

// Parse Expr::Shop's inner item
fn parse_shop_item(input: TokenStream) -> IResult<TokenStream, (Expr, Expr)> {
    (
        symbol(Symbol::ParenOpen),
        parse_expr,
        symbol(Symbol::Comma),
        parse_expr,
        symbol(Symbol::ParenClose),
    )
        .map(|(_, expr1, _, expr2, _)| (expr1, expr2))
        .parse(input)
}

// Parsing Expr::Call
fn parse_call_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    (
        identifier,
        symbol(Symbol::ParenOpen),
        separated_list0(symbol(Symbol::Comma), parse_expr),
        symbol(Symbol::ParenClose),
    )
        .map(|(func_name, _, args, _)| Expr::Call {
            callee: func_name,
            args,
        })
        .parse(input)
}

// Parse an Expr::Index
fn parse_index_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    let (rest, expr) = parse_primary_expr.parse(input)?;

    // May parse multiple indexing expressions in a row
    fold_many1(
        (
            symbol(Symbol::SquareOpen),
            parse_expr,
            symbol(Symbol::SquareClose),
        ),
        move || expr.clone(),
        |acc, (_, idx, _)| Expr::Index {
            object: Box::new(acc),
            index: Box::new(idx),
        },
    )
    .parse(rest)
}

// STATEMENT PRASING SECTION

// Parse statements
fn parse_statement(input: TokenStream) -> IResult<TokenStream, Statement> {
    alt((
        parse_ping_statement,
        parse_gonext_statement,
        parse_block_statement,
        parse_coinflip_statement,
        parse_item.map(|i| i.into()),
        parse_recall_statement,
    ))
    .parse(input)
}

// Parse into Stmt::Block
fn parse_block_statement(input: TokenStream) -> IResult<TokenStream, Statement> {
    (
        symbol(Symbol::CurlyOpen),
        many0(parse_statement),
        symbol(Symbol::CurlyClose),
    )
        .map(|(_, statements, _)| Statement::Block(statements))
        .parse(input)
}

// Parse into Statement::Ping
fn parse_ping_statement(input: TokenStream) -> IResult<TokenStream, Statement> {
    (
        keyword(Keyword::Ping),
        symbol(Symbol::ParenOpen),
        parse_expr,
        symbol(Symbol::ParenClose),
        symbol(Symbol::Semicolon),
    )
        .map(|(_, _, printed_value, _, _)| Statement::Ping {
            value: (printed_value),
        })
        .parse(input)
}

// Parse into Statement::GoNext
fn parse_gonext_statement(input: TokenStream) -> IResult<TokenStream, Statement> {
    (
        keyword(Keyword::GoNext),
        symbol(Symbol::ParenOpen),
        parse_expr,
        symbol(Symbol::ParenClose),
        parse_block_statement,
    )
        .map(|(_, _, condition, _, block)| Statement::GoNext {
            condition: (condition),
            body: Box::new(block),
        })
        .parse(input)
}

fn parse_coinflip_statement(input: TokenStream) -> IResult<TokenStream, Statement> {
    (
        keyword(Keyword::Coinflip),
        symbol(Symbol::ParenOpen),
        parse_expr,
        symbol(Symbol::ParenClose),
        parse_block_statement,
        opt((keyword(Keyword::Ff15), parse_block_statement)),
    )
        .map(
            |(_, _, condition, _, then_branch, ff15)| Statement::Coinflip {
                condition,
                then_branch: Box::new(then_branch),
                else_branch: ff15.map(|(_, else_branch)| Box::new(else_branch)),
            },
        )
        .parse(input)
}

fn parse_recall_statement(input: TokenStream) -> IResult<TokenStream, Statement> {
    (
        keyword(Keyword::Recall),
        opt(parse_expr),
        symbol(Symbol::Semicolon),
    )
        .map(|(_, value, _)| Statement::Recall { value })
        .parse(input)
}

// DECLARATION PARSING SECTION

/// Try to parse input into a declaration in order of nexus -> ability -> item
fn parse_decl(input: TokenStream) -> IResult<TokenStream, Decl> {
    alt((parse_nexus, parse_ability, parse_item.map(|i| i.into()))).parse(input)
}

/// Parse the nexus function (essentially main function) to Decl::Nexus
///
/// nexus() { <statement> )
fn parse_nexus(input: TokenStream) -> IResult<TokenStream, Decl> {
    let (rest, (_, _, _, _, statements, _)) = (
        keyword(Keyword::Nexus),
        symbol(Symbol::ParenOpen),
        symbol(Symbol::ParenClose),
        symbol(Symbol::CurlyOpen),
        many0(parse_statement),
        symbol(Symbol::CurlyClose),
    )
        .parse(input)?;

    Ok((rest, Decl::Nexus { body: statements }))
}

/// Parse abilities (non-main functions) to Decl::Ability
///
/// ability <name>(<params>) -> <return type> { <statement> }
fn parse_ability(input: TokenStream) -> IResult<TokenStream, Decl> {
    (
        keyword(Keyword::Ability),
        identifier,
        symbol(Symbol::ParenOpen),
        separated_list0(symbol(Symbol::Comma), parse_param),
        symbol(Symbol::ParenClose),
        symbol(Symbol::Arrow),
        parse_type,
        symbol(Symbol::CurlyOpen),
        many0(parse_statement),
        symbol(Symbol::CurlyClose),
    )
        .map(
            |(_, name, _, params, _, _, return_type, _, statements, _)| Decl::Ability {
                name,
                params,
                return_type,
                body: statements,
            },
        )
        .parse(input)
}

/// Parse function parameters to Param
///
/// <name>: <type>
fn parse_param(input: TokenStream) -> IResult<TokenStream, Param> {
    (identifier, symbol(Symbol::Colon), parse_type)
        .map(|(name, _, ty)| Param { name, ty })
        .parse(input)
}

struct Item {
    pub name: String,
    pub ty: Type,
    pub initializer: Expr,
}

impl Into<Decl> for Item {
    fn into(self) -> Decl {
        Decl::Item {
            name: self.name,
            ty: self.ty,
            initializer: self.initializer,
        }
    }
}

impl Into<Statement> for Item {
    fn into(self) -> Statement {
        Statement::ItemDecl {
            name: self.name,
            ty: self.ty,
            initializer: self.initializer,
        }
    }
}

/// Parse items (variable) to Decl::Item
///
/// buy <name>: <type> = <expr>
fn parse_item(input: TokenStream) -> IResult<TokenStream, Item> {
    (
        keyword(Keyword::Buy),
        identifier,
        symbol(Symbol::Colon),
        parse_type,
        operator(Operator::Assignment),
        parse_expr,
        symbol(Symbol::Semicolon),
    )
        .map(|(_, name, _, ty, _, initializer, _)| Item {
            name,
            ty,
            initializer,
        })
        .parse(input)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::util::*;
    use nom::multi::many;

    #[test]
    fn test_parse_type() {
        use crate::ast::*;
        let tokens = [
            ty("Shop"),
            sym("<"),
            ty("Duo"),
            sym("<"),
            ty("Chat"),
            sym(","),
            ty("Gold"),
            sym(">"),
            sym(","),
            ty("Inventory"),
            sym("<"),
            ty("Gold"),
            sym(">"),
            sym(">"),
        ];

        let should_be = Type::Shop(
            Box::new(Type::Duo(Box::new(Type::Chat), Box::new(Type::Gold))),
            Box::new(Type::Inventory(Box::new(Type::Gold))),
        );

        let (_rest, typ) = parse_type(TokenStream::new(&tokens)).unwrap();

        assert_eq!(typ, should_be);
    }

    #[test]
    fn test_parse_lit() {
        use crate::lexer::util::*;
        let tokens = [
            gold_lit(2938),
            chat_lit("hello world"),
            keyword("true"),
            sym("("),
            sym(")"),
        ];

        // let (_, (res1, res2, res3, res4)) = (parse_lit, parse_lit, parse_lit, parse_lit).parse(TokenStream::new(&tokens)).unwrap();
        let (_, res): (_, Vec<Expr>) = many(4, parse_literal_expr)
            .parse(TokenStream::new(&tokens))
            .unwrap();

        assert_eq!(res[0], Expr::Integer(2938));
        assert_eq!(res[1], Expr::String("hello world".to_string()));
        assert_eq!(res[2], Expr::Boolean(true));
        assert_eq!(res[3], Expr::Unit);
    }

    #[test]
    fn test_parse_inventory_expr() {
        use crate::lexer::util::*;
        let tokens = [
            sym("["),
            chat_lit("hello world"),
            sym(","),
            keyword("true"),
            sym(","),
            ident("what_up"),
            sym("]"),
        ];

        let should_be = Expr::Inventory(vec![
            Expr::String("hello world".to_string()),
            Expr::Boolean(true),
            Expr::Identifier("what_up".to_string()),
        ]);

        let (_, inv) = parse_inventory_expr
            .parse(TokenStream::new(&tokens))
            .unwrap();

        assert_eq!(inv, should_be);
    }

    #[test]
    fn test_parse_duo_expr() {
        use crate::lexer::util::*;
        let tokens = [
            sym("("),
            chat_lit("hello world"),
            sym(","),
            keyword("true"),
            sym(")"),
        ];

        let should_be = Expr::Duo(
            Box::new(Expr::String("hello world".to_string())),
            Box::new(Expr::Boolean(true)),
        );

        let (_, inv) = parse_duo_expr.parse(TokenStream::new(&tokens)).unwrap();

        assert_eq!(inv, should_be);
    }

    #[test]
    fn test_parse_shop_expr() {
        use crate::lexer::util::*;
        let tokens = [
            sym("{"),
            sym("("),
            chat_lit("hello world"),
            sym(","),
            keyword("true"),
            sym(")"),
            sym("}"),
        ];

        let should_be = Expr::Shop(vec![(
            Expr::String("hello world".to_string()),
            Expr::Boolean(true),
        )]);

        let (_, inv) = parse_shop_expr.parse(TokenStream::new(&tokens)).unwrap();

        assert_eq!(inv, should_be);
    }

    #[test]
    fn test_parse_index_expr() {
        use crate::lexer::util::*;
        let tokens = [
            ident("my_list"),
            sym("["),
            gold_lit(2),
            sym("]"),
            sym("["),
            gold_lit(2),
            sym("]"),
        ];

        let should_be = Expr::Index {
            object: Box::new(Expr::Index {
                object: Box::new(Expr::Identifier("my_list".to_string())),
                index: Box::new(Expr::Integer(2)),
            }),
            index: Box::new(Expr::Integer(2)),
        };

        let (_, result) = parse_index_expr.parse(TokenStream::new(&tokens)).unwrap();

        assert_eq!(result, should_be);
    }

    #[test]
    fn test_parse_call_expr() {
        use crate::lexer::util::*;
        let tokens = [
            ident("yap"),
            sym("("),
            chat_lit("hello world"),
            sym(","),
            keyword("true"),
            sym(","),
            ident("what_up"),
            sym(")"),
        ];

        let should_be = Expr::Call {
            callee: "yap".to_string(),
            args: vec![
                Expr::String("hello world".to_string()),
                Expr::Boolean(true),
                Expr::Identifier("what_up".to_string()),
            ],
        };

        let (_, result) = parse_call_expr.parse(TokenStream::new(&tokens)).unwrap();

        assert_eq!(result, should_be);
    }

    #[test]
    fn test_parse_binary_expr() {
        use crate::lexer::util::*;
        let tokens = [
            gold_lit(1000),
            op("+"),
            op("-"),
            gold_lit(5000),
            op("*"),
            gold_lit(5000),
        ];

        let (_, result) = parse_binary_expr.parse(TokenStream::new(&tokens)).unwrap();

        let should_be = Expr::Binary {
            left: Box::new(Expr::Integer(1000)),
            operator: BinaryOp::Add,
            right: Box::new(Expr::Binary {
                left: Box::new(Expr::Unary {
                    operator: UnaryOp::Negate,
                    right: Box::new(Expr::Integer(5000)),
                }),
                operator: BinaryOp::Multiply,
                right: Box::new(Expr::Integer(5000)),
            }),
        };

        assert_eq!(result, should_be);
    }
}
