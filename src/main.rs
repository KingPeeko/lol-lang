mod ast;
mod lexer;
mod parser;

fn main() {
    let input = r#"
    ability sum(a: Gold, b: Gold, c: Gold, d: Gold) -> Gold {
        recall 1 + 2 + 3 + 4;
    }
nexus() {
    /all noob team go die
    buy my_variable: Chat = "Hello world!";
    recall sum(1, 2, 3, 4);
} /all This is the best language ever!
"#;

    let tokens = lexer::tokenize(input).unwrap();
    println!("{tokens:?}");
    let program = parser::parse(&tokens).unwrap();

    println!("{program:?}");
}
