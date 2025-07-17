use std::{fs::File, io, io::Read};

pub mod lexer;
pub mod token;

fn read_file_contents(path: &str) -> Result<String, io::Error> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    return Ok(contents);
}

fn main() -> Result<(), io::Error> {
    let contents = read_file_contents("examples/add.mk")?;
    println!("File contents: {}", contents);

    let tokens = lexer::lex(&contents);
    println!("{:#?}", tokens);

    Ok(())
}
