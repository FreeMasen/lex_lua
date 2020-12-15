# Lex Lua

A Lua lexer written in Rust.

## Example

```rust
use lex_lua::Lexer;

fn main() {
    let bytes = std::fs::read("./readme.lua").unwrap();
    let l = Lexer::new(bytes.as_slice());
    for (i, token) in l.enumerate() {
        println!("{}: {:?}", i, token);
    }
}
```


```sh
$ cargo run --example readme
0: Keyword(Function)
1: Name("say_hello")
2: Punct(OpenParen)
3: Punct(CloseParen)
4: Name("print")
5: Punct(OpenParen)
6: LiteralString("\'Hi!\'")
7: Punct(CloseParen)
8: Keyword(End)
9: Name("say_hello")
10: Punct(OpenParen)
11: Punct(CloseParen)
```