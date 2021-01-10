use lex_lua::Lexer;

fn main() {
    let bytes = include_bytes!("./readme.lua");
    let l = Lexer::new(bytes);
    for (i, token) in l.enumerate() {
        println!("{}: {:?}", i, token);
    }
}
