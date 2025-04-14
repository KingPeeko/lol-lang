mod ast;
mod lexer;
mod parser;

fn main() {
    let a = lexer::tokenize("buy buy buy _");
    if let Err(e) = a {
        println!("{e}");
    }
}
