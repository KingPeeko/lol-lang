use super::tokens::*;
use std::str::FromStr;


use nom::{
    sequence::{pair, preceded, delimited},
    branch::alt,
    bytes::complete::{tag, is_not, take, escaped_transform},
    combinator::{recognize, value, all_consuming},
    character::complete::{alpha1, alphanumeric1, digit1, multispace1},
    multi::{many0_count, many0},
    IResult, Parser
};


fn parse_comment(input: &str) -> IResult<&str, &str> {
    preceded(
        tag("/all "), //first find '/all '
        is_not("\n") // Then parse till new line.
    ).parse(input)
}

fn parse_keyword(input: &str) -> IResult<&str, Token> {

    // Either parse a one word keyword, then try two words (go next)
    let one_word = alphanumeric1.map_res(Keyword::from_str);
    let two_words = recognize(pair(alphanumeric1, pair(multispace1, alphanumeric1))).map_res(Keyword::from_str);

    alt((
        one_word,
        two_words,
    )).map(Token::Keyword).parse(input)
}

fn parse_type(input: &str) -> IResult<&str, Token> {
    alpha1
        .map_res(Type::from_str) // Parse any alphabet characters and check if it matches a type
        .map(Token::Type)
        .parse(input)
}

fn parse_symbol(input: &str) -> IResult<&str, Token> {
    // the '->' consists of two characters, which needs to be parsed as a special case, since
    // symbols are allowed to have things follow them immediately afterwards
    let arrow = tag("->").map_res(Symbol::from_str);
    let others = take(1usize).map_res(Symbol::from_str);

    alt((
        arrow,
        others
    )).map(Token::Symbol)
        .parse(input)
}

fn parse_operator(input: &str) -> IResult<&str, Token> {

    // Some operators are two characters long, and must be checked before the one character long
    // ones, since some share the same first character
    let two_char_op = take(2usize).map_res(Operator::from_str);
    let one_char_op = take(1usize).map_res(Operator::from_str);

    alt((
        two_char_op,
        one_char_op
    )).map(Token::Operator)
        .parse(input)
}

fn parse_gold_literal(input: &str) -> IResult<&str, Token> {
    digit1
        .map( |s: &str| Literal::GoldLit(s.to_string()) )
        .map(Token::Literal)
        .parse(input)
}

fn parse_chat_literal(input: &str) -> IResult<&str, Token> {

    let parse_escaped = alt((
        value("\n", tag("n")), //convert the escaped tag to the LHS value
        value("\t", tag("t")),

        take(1usize) //otherwise just keep it as is
    ));

    // This allows escaped double quotes and such in our strings
    let string_content = escaped_transform(
        is_not("\\\""), // Parse everything but backslash and "
        '\\',         // Upon finding a backslash
        parse_escaped // parse it with this
    );

    delimited(
        tag("\""),
        string_content,
        tag("\"")
    ).map(Literal::ChatLit)
        .map(Token::Literal)
        .parse(input)
}

fn parse_literal(input: &str) -> IResult<&str, Token> {
    alt((
        parse_gold_literal,
        parse_chat_literal
    )).parse(input)
}

// Note that this WILL parse keywords, which is why keywords must have higher priority in parsing.
// Identifiers should have the least priority, since it can catch a lot of stuff
fn parse_identifier(input: &str) -> IResult<&str, Token> {
    recognize(
        pair(
            alpha1,                                     // Parse an alphabet character
            many0_count(alt((alphanumeric1, tag("_")))) // then alphanumeric characters or _
        )
    ).map( |s: &str| Token::Identifier(s.to_string()) )
        .parse(input)
}

// Parsing order:
// Comment
// Keyword
// Type
// Symbol
// Operator
// Literal
// Identifier
// Eof

fn next_token(input: &str) -> IResult<&str, Token> {

    // Things to ignore while parsing: whitespace and comments
    // ignore them many times, until we don't find them anymore (many0_count)
    fn ignore(input: &str) -> IResult<&str, usize> {
        many0_count(
            alt((
                multispace1,
                parse_comment
            ))
        ).parse(input)
    }

    delimited(
        ignore, // This is discarded
        alt((   // Parse in priority order
            parse_keyword,
            parse_type,
            parse_symbol,
            parse_operator,
            parse_literal,
            parse_identifier,
        )),
        ignore
    ).parse(input)
}

fn parse_whole_input(input: &str) -> IResult<&str, Vec<Token>> {
    // Parse next_token as many times as possible but require that the input string gets completely
    // consumed, otherwise parsing has failed somewhere
    all_consuming(
        many0(next_token)
    ).map( |mut tokens| { // Add an Eof token at the end
            tokens.push(Token::Eof);
            tokens
        })
        .parse(input)
}

#[derive(Debug, Clone, Copy)]
pub struct TokenizerError;

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenizerError> {
    match parse_whole_input(input) {
        Ok(( _, tokens )) => Ok(tokens),
        Err(_e) => {
            Err(TokenizerError)
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::super::util::*;

    // Macro which tests that all elements in $strs parse to the corresponding token in $enums when
    // parsed with $parser
    macro_rules! test_pair {
        ($strs:expr, $enums:expr, $parser:ident, $token:path) => {
            $strs.iter().zip($enums).for_each(
                | (tstr, tenum) | {
                    let Ok(( _, token )) = $parser(tstr) else {
                        panic!("'{tstr}' should have parsed to {}({tenum:?}), but parsing failed.", stringify!($token));
                    };
                    
                    assert_eq!(token, $token(tenum));
                }
            )
        }
    }

    // Macro to test that parsing each string in $strs should fail with $parser
    macro_rules! test_fail {
        ($strs:expr, $parser:ident) => {
            $strs.iter().for_each(
                |s| {
                    let result = $parser(s);
                    assert!(result.is_err());
                }
            )
        }
    }

    // Comment parsing
    #[test]
    fn test_comment() {
        let input = "/all noob team x9 jungle";
        let result = parse_comment(input);
        assert_eq!( result, Ok(("", "noob team x9 jungle") )); // Should parse the whole line and return ()
    }

    #[test]
    fn test_comment_stop_on_newline() {
        let input = "/all noob team x9 jungle\nbuy x = 5;";
        let result = parse_comment(input);
        assert_eq!( result, Ok(("\nbuy x = 5;", "noob team x9 jungle") )); // Should parse until \n, then stop
    }

    // Keywords
    #[test]
    fn test_keyword() {
        use Keyword::*;

        let keywords = ["nexus", "ability", "buy", "coinflip", "ff15", "go next", "ping", "recall", "true", "false"];
        let keyword_enums = [Nexus, Ability, Buy, Coinflip, Ff15, GoNext, Ping, Recall, True, False];
        test_pair!(keywords, keyword_enums, parse_keyword, Token::Keyword);
    }

    #[test]
    fn test_keyword_fail() {
        let input = "nexuus fodsfds";
        let result = parse_keyword(input);
        assert!(result.is_err());
    }

    // Identifiers
    #[test]
    fn test_identifier() {
        let input = "the_best_v4r14bl3_name420 * 50";
        let result = parse_identifier(input);
        assert_eq!(
            result,
            Ok( (" * 50", Token::Identifier("the_best_v4r14bl3_name420".to_string())) )
        )
    }

    #[test]
    fn test_identifier_fail() {
        let input = "_yay";
        let result = parse_identifier(input);
        assert!(result.is_err());
    }

    // Types
    #[test]
    fn test_types() {
        use Type::*;
        let types = ["Gold", "Status", "Chat", "Void", "Duo", "Inventory", "Shop"];
        let type_enums = [Gold, Status, Chat, Void, Duo, Inventory, Shop];

        test_pair!(types, type_enums, parse_type, Token::Type);
    }

    #[test]
    fn test_type_fail() {
        let input = "Nexus";
        let result = parse_type(input);
        assert!(result.is_err());
    }

    // Symbols
    #[test]
    fn test_symbols() {
        use Symbol::*;
        let symbols = ["(", ")", "{", "}", "[", "]", "<", ">", ";", ":", ",", "->"];
        let symbol_enums = [ParenOpen, ParenClose, CurlyOpen, CurlyClose, SquareOpen, SquareClose, AngleOpen, AngleClose, Semicolon, Colon, Comma, Arrow];
        test_pair!(symbols, symbol_enums, parse_symbol, Token::Symbol);
        
    }

    #[test]
    fn test_symbol_fail() {
        let input = ".huehue";
        let result = parse_symbol(input);
        assert!(result.is_err());
    }

    // Operators
    #[test]
    fn test_operators() {
        use Operator::*;
        let operators = ["=", "+", "-", "*", "/", "%", "==", "!=", "<=", ">=", "&&", "||", "!"];
        let operator_enums = [Assignment, Plus, Minus, Mult, Divide, Modulo, Equals, NotEquals, LessEquals, GreaterEquals, And, Or, Negate];
        test_pair!(operators, operator_enums, parse_operator, Token::Operator);
    }

    #[test]
    fn test_operator_fail() {
        let input = "< 5";
        let result = parse_operator(input);
        assert!(result.is_err());
    }

    // Chat literals
    #[test]
    fn test_chat_literal() {

        let inputs = [
            r#""Hello, world!""#,
            r#""Hello, \"world\"...""#,
            r#""This is a\nnew line!""#,
            r#""nexus() {\n\tbuy x = 5;\n\tping(x);\n}""#
        ];
        let outputs = [
            "Hello, world!",
            r#"Hello, "world"..."#,
            "This is a\nnew line!",
            concat!(
                "nexus() {\n",
                "\tbuy x = 5;\n",
                "\tping(x);\n",
                "}"
            )
        ];

        test_pair!(inputs, outputs, parse_literal, chat_lit)
    }

    #[test]
    fn test_chat_fail() {
        let input = "Hello, world!";
        let result = parse_literal(input);
        assert!(result.is_err());
    }

    // Gold literals
    #[test]
    fn test_gold_literal() {
        let nums = [34795, 12034, 0, 1293, 5892];
        let inputs = nums.map( |n| n.to_string() );
        let outputs = nums;

        test_pair!(inputs, outputs, parse_literal, gold_lit);
    }

    #[test]
    fn test_gold_fail() {
        let inputs = ["-32498", "ahaha", "Hello, world!"];
        test_fail!(inputs, parse_literal)
    }

    #[test]
    fn test_tokenizer() {
        let input = r#"
          
nexus() {
/all noob team go die
    buy my_variable = "Hello world!";
    ping(my_variable); /all this is a print statement; buy x = 5;
    recall 5 + 9;
} /all This is the best language ever!

   
   "#;

        let tokens = vec![
            keyword("nexus"), sym("("), sym(")"), sym("{"),
            keyword("buy"), ident("my_variable"), op("="), chat_lit("Hello world!"), sym(";"),
            keyword("ping"), sym("("), ident("my_variable"), sym(")"), sym(";"),
            keyword("recall"), gold_lit(5), op("+"), gold_lit(9), sym(";"),
            sym("}"),
            Token::Eof
        ];

        let Ok(result) = tokenize(input) else {
            panic!("Tokenizing failed, it shouldn't have");
        };


        assert_eq!(result, tokens);
    }
}


