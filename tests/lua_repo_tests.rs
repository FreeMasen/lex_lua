use std::path::{Path, PathBuf};

use lex_lua::{Token, Lexer};

#[test]
fn try_all() {
    let lua_repo_dir = std::env::var("LUA_REPO_DIR").unwrap_or_else(|_| "./lua".to_string());
    let lua_repo_path = PathBuf::from(lua_repo_dir);
    assert!(
        lua_repo_path.exists(),
        "Unable to find lua repo directory, try setting LUA_REPO_DIR"
    );
    let lua_tests_path = lua_repo_path.join("testes");
    assert!(
        lua_tests_path.exists(),
        "Unable to find the lua testes directory at {}",
        lua_tests_path.display()
    );
    for ent in std::fs::read_dir(&lua_tests_path).unwrap() {
        let entry = ent.unwrap();
        if entry
            .path()
            .extension()
            .as_ref()
            .map(|e| *e == "lua")
            .unwrap_or(false)
        {
            println!("trying {}", entry.path().display());
            let text = std::fs::read(entry.path()).unwrap();
            let mut t = Lexer::new(text.as_slice());
            let mut tok_ct = 0;
            while let Some(tok) = t.next_token() {
                tok_ct += 1;
                println!("{} {:?}", tok_ct, tok);
                if matches!(tok, Token::Unknown(_)) {
                    let (_, full_line) =
                        text.split(|c| *c == b'\n').fold((0, None), |mut acc, l| {
                            acc.0 += l.len();
                            if acc.0 >= t.current_pos() && acc.1.is_none() {
                                acc.1 = Some(String::from_utf8_lossy(l))
                            }
                            acc
                        });

                    let (line_idx, line) = text[0..t.current_pos()]
                        .split(|c| *c == b'\n')
                        .enumerate()
                        .last()
                        .unwrap();
                    let (behind_current, _) = text[..t.current_pos()]
                        .iter()
                        .enumerate()
                        .rfind(|(_, &b)| b == b'\n')
                        .unwrap();
                    panic!(
                        "{} unknown token in {}:{}:0 {:?}\n{}\n{}",
                        tok_ct,
                        entry.path().display(),
                        line_idx + 1,
                        tok,
                        String::from_utf8_lossy(line),
                        "^".repeat(full_line.map(|l| l.len()).unwrap_or(0))
                    )
                }
            }
        }
    }
}

fn try_read(p: impl AsRef<Path>) -> String {
    let bytes = std::fs::read(p.as_ref()).unwrap_or_else(|e| {
        panic!(
            "failed to read file as bytes {}: {}",
            p.as_ref().display(),
            e
        )
    });
    let mut s = String::with_capacity(bytes.len());
    for (idx, line) in bytes.split(|b| *b == b'\n').enumerate() {
        match String::from_utf8(line.to_vec()) {
            Ok(l) => {
                s.push_str(&l);
                s.push('\n');
            }
            Err(_) => {
                println!("Invalid utf8 {}:{}:0", p.as_ref().display(), idx + 1)
            }
        }
    }

    s
}
