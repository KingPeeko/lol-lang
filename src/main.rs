mod lexer;
mod parser;
mod ast;

fn main() {
    let a = lexer::tokenize("buy buy buy _");
    if let Err(e) = a {
        println!("{e}");
    }
}
