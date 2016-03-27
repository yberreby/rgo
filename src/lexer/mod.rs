mod token;
pub use self::token::*;

#[cfg(test)]
mod test;

pub fn tokenize(s: &str) -> Vec<Token> {
    unimplemented!()
}
