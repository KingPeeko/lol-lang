use super::token_stream::TokenStream;
use crate::ast::*;
use crate::lexer::tokens::{Keyword, Literal, Operator, Symbol, Token};

use nom::Input;
use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::value,
    error::{ErrorKind, ParseError},
    multi::fold_many0,
    Err::Error,
    IResult, Parser,
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
    todo!();
}

// Parse into Expr::Identifier
fn parse_identifier(input: TokenStream) -> IResult<TokenStream, Expr> {
    identifier.map(Expr::Identifier).parse(input)
}

// Parse into Expr literal type
fn parse_lit(input: TokenStream) -> IResult<TokenStream, Expr> {
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
        .map(|(_, expr, _)| expr)
        .parse(input)
}

// Parse into Expr::Binary
fn parse_binary_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    (parse_expr, parse_binary_op, parse_expr)
        .map(|(expr1, op, expr2)| Expr::Binary {
            left: Box::new(expr1),
            operator: op,
            right: Box::new(expr2),
        })
        .parse(input)
}

// Parse into BinaryOp
fn parse_binary_op(input: TokenStream) -> IResult<TokenStream, BinaryOp> {
    any_token.map_res(BinaryOp::try_from).parse(input)
}

// Parse into Expr::Inventory
fn parse_inventory_expr(input: TokenStream) -> IResult<TokenStream, Expr> {
    (
        symbol(Symbol::SquareOpen),
        parse_inventory_items,
        symbol(Symbol::SquareClose),
    )
        .map(|(_, items, _)| Expr::Inventory(items))
        .parse(input)
}

// Parse the items that go in Expr::Inventory
fn parse_inventory_items(input: TokenStream) -> IResult<TokenStream, Vec<Expr>> {
    separated_list0(symbol(Symbol::Comma), parse_expr).parse(input)
}


// STATEMENT PRASING SECTION

fn parse_ping_stmt(input: TokenStream) -> IResult<TokenStream, Stmt> {
    (
        keyword(Keyword::Ping),
        symbol(Symbol::ParenOpen),
        parse_expr,
        symbol(Symbol::ParenClose),
        symbol(Symbol::Semicolon),
    )
    .map(|(_, _, printed_value, _, _)| Stmt::Ping { value: (printed_value) })
    .parse(input)
}

//     fn parse_decl(&mut self) -> Result<Decl, Err> {
//         todo!();
//         let kw = expect_unwrap!(self.stream, Token::Keyword)?;
//
//         match kw {
//             Keyword::Ability => self.parse_ability(),
//             Keyword::Buy => self.parse_item_decl(),
//             Keyword::Nexus => self.parse_nexus(),
//
//             _ => Err(ParseError::UnexpectedToken),
//         }
//     }
//
//     fn parse_nexus(&mut self) -> Result<Decl, Err> {
//         todo!();
//     }
//
//     fn parse_ability(&mut self) -> Result<Decl, Err> {
//         todo!();
//     }
//
//     fn parse_item_decl(&mut self) -> Result<Decl, Err> {
//         todo!();
//         // Item decl needs tokens 'buy', 'identifier', 'colon', 'type', 'assignment', 'expression', 'semicolon'
//         expect!(self.stream, Token::Keyword(Keyword::Buy))?;
//
//         let variable_name = expect_unwrap!(self.stream, Token::Identifier)?;
//
//         expect!(self.stream, Token::Symbol(Symbol::Colon))?;
//
//         let ty = self.parse_type()?;
//
//         expect!(self.stream, Token::Operator(Operator::Assignment))?;
//
//         let expr = self.parse_expr()?;
//
//         expect!(self.stream, Token::Symbol(Symbol::Semicolon))?;
//
//         Ok(Decl::Item {
//             name: variable_name.to_string(),
//             ty,
//             initializer: expr,
//         })
//     }
//
//     // Parses the Inventory expression, aka. a list. Returns an Ok with the Inventory if syntax is correct.
//     fn parse_inventory_expr(&mut self) -> Result<Expr, Err> {
//         expect!(self.stream, Token::Symbol(Symbol::SquareOpen))?;
//
//         let mut items = Vec::new();
//
//         if let _ = peek_validate!(self.stream, Token::Symbol(Symbol::SquareClose)) {
//             self.stream.move_forward(1);
//             return Ok(Expr::Inventory(items));
//         }
//
//         loop {
//             items.push(self.parse_expr()?);
//             if let _ = peek_validate!(self.stream, Token::Symbol(Symbol::Comma)) {
//                 self.stream.move_forward(1);
//             } else {
//                 break;
//             }
//         }
//
//         expect!(self.stream, Token::Symbol(Symbol::SquareClose))?;
//
//         return Ok(Expr::Inventory(items));
//     }
// }
//
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
        let (_, res): (_, Vec<Expr>) = many(4, parse_lit).parse(TokenStream::new(&tokens)).unwrap();

        assert_eq!(res[0], Expr::Integer(2938));
        assert_eq!(res[1], Expr::String("hello world".to_string()));
        assert_eq!(res[2], Expr::Boolean(true));
        assert_eq!(res[3], Expr::Unit);
    }

    #[test]
    #[ignore = "needs parse_expr to be complete"]
    fn test_parse_inventory_expr() {
        use crate::lexer::util::*;
        let tokens = [
            sym("["),
            chat_lit("hello world"),
            sym(","),
            keyword("true"),
            sym(","),
            ident("what_up"),
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

    //
    // #[test]
    // fn test_parse_binary_expr() {
    //     todo!(); // Needs parse_expr first
    //     let tokens = [gold_lit(5000), op("*"), op("-"), gold_lit(1)];
    //
    //     let mut parser = Parser::new(&tokens);
    //     let result = parser.parse_binary_expr().unwrap();
    //
    //     let should_be = Expr::Binary {
    //         left: Box::new(Expr::Integer(5000)),
    //         operator: BinaryOp::Multiply,
    //         right: Box::new(Expr::Unary {
    //             operator: UnaryOp::Negate,
    //             right: Box::new(Expr::Integer(1)),
    //         }),
    //     };
    //
    //     assert_eq!(result, should_be);
    // }
}
