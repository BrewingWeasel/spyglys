use std::{fmt::Display, iter::Peekable, str::Chars};

use crate::interpreter::{Expression, Statement};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub start: usize,
    pub end: usize,
    pub contents: T,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Eof,
    LParen,
    RParen,
    Plus,
    QuestionMark,
    Dollars,
    Regex(String),
    Str(String),
    Ident(String),
    Def,
    Let,
    Equals,
    Semicolon,
    Comma,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Eof => write!(f, ""),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Plus => write!(f, "+"),
            Token::Equals => write!(f, "="),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::QuestionMark => write!(f, "?"),
            Token::Dollars => write!(f, "$"),
            Token::Regex(s) => write!(f, "'{s}'"),
            Token::Str(s) => write!(f, "\"{s}\""),
            Token::Ident(s) => write!(f, "{s}"),
            Token::Def => write!(f, "def"),
            Token::Let => write!(f, "let"),
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
            '$' => Token::Dollars,
            ',' => Token::Comma,
            '=' => Token::Equals,
            ';' => Token::Semicolon,
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
                while let Some(c) = self.peek_char() {
                    if !c.is_alphanumeric() && c != '_' && c != '?' {
                        break;
                    }
                    self.next_char();
                    conts.push(c);
                }
                match conts.as_str() {
                    "def" => Token::Def,
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

pub fn parse_statement(tokens: &mut Peekable<Lexer>) -> Option<Statement> {
    if let Some(t) = tokens.next() {
        match t.contents {
            Token::Let => {
                let Some(ident) = tokens.next() else {
                    todo!();
                    return None;
                };
                let Token::Ident(var_name) = ident.contents else {
                    return None;
                };
                tokens.next();
                let value = parse_expression(tokens, Token::Semicolon)?;
                Some(Statement::Let(var_name, value))
            }
            Token::Def => {
                let Some(ident) = tokens.next() else {
                    todo!();
                    return None;
                };
                let Token::Ident(func_name) = ident.contents else {
                    return None;
                };
                tokens.next();
                let matching = parse_expression(tokens, Token::RParen)?;
                tokens.next();
                let handler = parse_expression(tokens, Token::Semicolon)?;
                Some(Statement::Def(func_name, matching, handler))
            }
            _ => None,
        }
    } else {
        None
    }
}

pub fn parse_expression(tokens: &mut Peekable<Lexer>, ending: Token) -> Option<Expression> {
    let mut previous = None;
    while let Some(t) = tokens.peek() {
        if t.contents == ending {
            break;
        }
        let new_expr = parse_partial_expression(tokens, previous);
        previous = new_expr
    }
    previous
}

fn parse_partial_expression(
    tokens: &mut Peekable<Lexer>,
    previous: Option<Expression>,
) -> Option<Expression> {
    if let Some(t) = tokens.next() {
        match t.contents {
            Token::Regex(r) => Some(Expression::Regex(r)),
            Token::Str(s) => Some(Expression::String(s)),
            Token::Ident(name) => {
                if let Some(t) = tokens.peek() {
                    if t.contents == Token::LParen {
                        tokens.next();
                        let inner = parse_expression(tokens, Token::RParen)?;
                        tokens.next();
                        return Some(Expression::Call(name, Box::new(inner)));
                    }
                }
                Some(Expression::Variable(name))
            }
            Token::Plus => {
                let next = parse_partial_expression(tokens, previous.clone());
                Some(Expression::Plus(Box::new(previous?), Box::new(next?)))
            }
            Token::Dollars => {
                let Some(ident) = tokens.next() else { todo!() };
                let Token::Ident(name) = ident.contents else {
                    todo!()
                };
                tokens.next();
                let mut contents = Vec::new();
                while let Some(t) = tokens.peek() {
                    if t.contents == Token::RParen {
                        tokens.next();
                        break;
                    }
                    contents.push(Box::new(parse_expression(tokens, Token::Comma)?));
                    tokens.next();
                }
                Some(Expression::Builtin(name, contents))
            }
            _ => None,
        }
    } else {
        None
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
