use std::{fmt::Display, iter::Peekable, str::Chars};

use crate::interpreter::Expression;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    start: usize,
    end: usize,
    contents: T,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Eof,
    LParen,
    RParen,
    Plus,
    QuestionMark,
    Regex(String),
    Str(String),
    Ident(String),
    Def,
    Let,
    End,
    Equals,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Eof => write!(f, ""),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Plus => write!(f, "+"),
            Token::Equals => write!(f, "="),
            Token::QuestionMark => write!(f, "?"),
            Token::Regex(s) => write!(f, "'{s}'"),
            Token::Str(s) => write!(f, "\"{s}\""),
            Token::Ident(s) => write!(f, "{s}"),
            Token::Def => write!(f, "def"),
            Token::Let => write!(f, "let"),
            Token::End => write!(f, "end"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'input> {
    chars: Peekable<Chars<'input>>,
    position: usize,
    eof: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            chars: input.chars().peekable(),
            position: 0,
            eof: false,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Spanned<Token>> {
        self.collect()
    }

    fn next_char(&mut self) -> Option<char> {
        self.position += 1;
        self.chars.next()
    }

    fn assert_next_char(&mut self) -> Option<char> {
        self.position += 1;
        self.chars.next()
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            return None;
        }
        while let Some(c) = self.peek_char() {
            if !c.is_whitespace() {
                break;
            }
            self.next_char();
        }

        let start_pos = self.position;

        let Some(next) = self.next_char() else {
            self.eof = true;
            return Some(Spanned {
                contents: Token::Eof,
                start: start_pos,
                end: start_pos,
            });
        };

        let token = match next {
            '(' => Token::LParen,
            ')' => Token::RParen,
            '+' => Token::Plus,
            '?' => Token::QuestionMark,
            '=' => Token::Equals,
            d if d == '\'' || d == '"' => {
                let mut conts = String::new();
                while let Some(c) = self.assert_next_char() {
                    if c == d {
                        break;
                    }
                    conts.push(c);
                }
                if d == '\'' {
                    Token::Regex(conts)
                } else {
                    Token::Str(conts)
                }
            }
            c => {
                let mut conts = String::from(c);
                while let Some(c) = self.assert_next_char() {
                    if !c.is_alphanumeric() {
                        break;
                    }
                    conts.push(c);
                }
                match conts.as_str() {
                    "def" => Token::Def,
                    "end" => Token::End,
                    "let" => Token::Let,
                    _ => Token::Ident(conts),
                }
            }
        };
        Some(Spanned {
            start: start_pos,
            end: self.position,
            contents: token,
        })
    }
}

#[cfg(test)]
mod test {
    use super::Lexer;

    #[test]
    fn test_lexing_basic() {
        insta::assert_debug_snapshot!(Lexer::new("let  (+)?").tokenize())
    }

    #[test]
    fn test_lexing_multiple_keywords() {
        insta::assert_debug_snapshot!(Lexer::new(
            "let

    def   

            end   let   def end defendletdef"
        )
        .tokenize())
    }
}
