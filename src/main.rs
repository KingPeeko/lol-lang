mod lexer;

fn main() {
    let a = lexer::tokenize("hellosdfjodsfdjfoajo3o2j432423___324324o");
    println!("{a:?}");
}
