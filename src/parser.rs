use std::{fmt::Display, iter::Peekable, str::Chars};

use crate::interpreter::{Expression, Statement, TestRule};

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
    LBracket,
    RBracket,
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
    Comment(String),
    NewLine,
    End,
    For,
    In,
    // RESERVED (not all)
    Where,
    When,
    Rule,
    Arrow,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, ""),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::Plus => write!(f, "+"),
            Self::Equals => write!(f, "="),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::QuestionMark => write!(f, "?"),
            Self::Dollars => write!(f, "$"),
            Self::Arrow => write!(f, "->"),
            Self::Regex(s) => write!(f, "'{s}'"),
            Self::Str(s) => write!(f, "\"{s}\""),
            Self::Ident(s) => write!(f, "{s}"),
            Self::Def => write!(f, "def"),
            Self::Let => write!(f, "let"),
            Self::End => write!(f, "end"),
            Self::Where => write!(f, "where"),
            Self::For => write!(f, "for"),
            Self::In => write!(f, "in"),
            Self::When => write!(f, "when"),
            Self::Rule => write!(f, "rule"),
            Self::Comment(c) => write!(f, "#{c}"),
            Self::NewLine => writeln!(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LexingError {
    pub err_type: LexingErrorType,
    pub position: usize,
}

#[derive(Debug, Clone)]
pub enum LexingErrorType {
    UnexpectedEof,
    UnexpectedChar(char),
}

#[derive(Debug, Clone)]
pub struct Lexer<'input> {
    chars: Peekable<Chars<'input>>,
    position: usize,
    eof: bool,
    pub errors: Vec<LexingError>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            chars: input.chars().peekable(),
            position: 0,
            eof: false,
            errors: Vec::new(),
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
        if self.chars.peek().is_none() {
            self.errors.push(LexingError {
                err_type: LexingErrorType::UnexpectedEof,
                position: self.position,
            })
        }
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
            if c == '\n' {
                return Some(Spanned {
                    start: self.position - 1,
                    end: self.position,
                    contents: Token::NewLine,
                });
            }
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
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '+' => Token::Plus,
            '?' => Token::QuestionMark,
            '$' => Token::Dollars,
            ',' => Token::Comma,
            '=' => Token::Equals,
            ';' => Token::Semicolon,
            '-' => match self.next_char() {
                Some('>') => Token::Arrow,
                _ => todo!(),
            },
            '#' => {
                let mut conts = String::new();
                while let Some(c) = self.peek_char() {
                    if c == '\n' {
                        break;
                    }
                    conts.push(c);
                    self.next_char();
                }
                Token::Comment(conts)
            }
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
                    if c.is_whitespace() {
                        break;
                    }
                    if !c.is_alphanumeric() && c != '_' && c != '?' {
                        self.errors.push(LexingError {
                            err_type: LexingErrorType::UnexpectedChar(c),
                            position: self.position + 1,
                        });
                        break;
                    }
                    self.next_char();
                    conts.push(c);
                }
                match conts.as_str() {
                    "def" => Token::Def,
                    "let" => Token::Let,
                    "end" => Token::End,
                    "where" => Token::Where,
                    "when" => Token::When,
                    "rule" => Token::Rule,
                    "for" => Token::For,
                    "in" => Token::In,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsingErrorType {
    UnexpectedEOF,
    ExpectedIdent(Token),
    ExpectedToken(Token, Token),
    InvalidStatement,
    InvalidExpression,
}

#[derive(Debug, Clone)]
pub struct ParsingError {
    pub err_type: ParsingErrorType,
    pub start: usize,
    pub end: usize,
}

pub fn parse_statement(tokens: &mut Peekable<Lexer>) -> Result<Statement, ParsingError> {
    if let Some(t) = tokens.next() {
        match t.contents {
            Token::Let => {
                let Some(ident) = tokens.next() else {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::UnexpectedEOF,
                        start: t.end,
                        end: t.end,
                    });
                };
                let Token::Ident(var_name) = ident.contents else {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::ExpectedIdent(ident.contents),
                        start: ident.start,
                        end: ident.end,
                    });
                };
                let Some(t) = tokens.next() else {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::UnexpectedEOF,
                        start: ident.end,
                        end: ident.end,
                    });
                };
                if t.contents != Token::Equals {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::ExpectedToken(Token::Equals, t.contents),
                        start: t.start,
                        end: t.end,
                    });
                }
                let value = parse_expression(tokens, &[Token::Semicolon])?;
                tokens.next();
                Ok(Statement::Let(var_name, value))
            }
            Token::Def => {
                // func
                let Some(ident) = tokens.next() else {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::UnexpectedEOF,
                        start: t.end,
                        end: t.end,
                    });
                };
                let Token::Ident(func_name) = ident.contents else {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::ExpectedIdent(ident.contents),
                        start: ident.start,
                        end: ident.end,
                    });
                };

                // (
                let Some(t) = tokens.next() else {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::UnexpectedEOF,
                        start: ident.end,
                        end: ident.end,
                    });
                };
                if t.contents != Token::LParen {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::ExpectedToken(Token::LParen, t.contents),
                        start: t.start,
                        end: t.end,
                    });
                }

                let matching = parse_expression(tokens, &[Token::RParen])?;

                // )
                tokens.next();
                // :
                tokens.next();
                let handler = parse_expression(tokens, &[Token::End, Token::Where])?;
                let ending = tokens.next();
                let mut rules = Vec::new();
                if matches!(
                    ending,
                    Some(Spanned {
                        contents: Token::Where,
                        ..
                    })
                ) {
                    while let Some(mut t) = tokens.peek() {
                        while matches!(t.contents, Token::NewLine | Token::Comment(_)) {
                            let location = t.end;
                            tokens.next();
                            t = tokens.peek().ok_or(ParsingError {
                                start: location,
                                end: location,
                                err_type: ParsingErrorType::UnexpectedEOF,
                            })?;
                        }
                        if t.contents == Token::End {
                            tokens.next();
                            break;
                        }
                        let input = parse_expression(tokens, &[Token::Arrow])?;
                        tokens.next();
                        let expected_output =
                            parse_expression(tokens, &[Token::Semicolon, Token::For])?;
                        let mut for_vars = Vec::new();
                        if matches!(
                            tokens.next(),
                            Some(Spanned {
                                contents: Token::For,
                                ..
                            })
                        ) {
                            let Some(possible_var) = tokens.next() else {
                                todo!()
                            };
                            let Token::Ident(var_name) = possible_var.contents else {
                                todo!()
                            };
                            if !matches!(
                                tokens.next(),
                                Some(Spanned {
                                    contents: Token::In,
                                    ..
                                })
                            ) {
                                todo!()
                            }
                            let variable_value_options =
                                parse_expression(tokens, &[Token::Semicolon])?;
                            tokens.next();
                            for_vars.push((var_name, variable_value_options))
                        }
                        rules.push(TestRule {
                            input,
                            expected_output,
                            for_vars,
                        });
                    }
                }
                Ok(Statement::Def(func_name, matching, handler, rules))
            }
            Token::Comment(c) => Ok(Statement::Comment(c)),
            Token::NewLine => {
                if matches!(
                    tokens.peek(),
                    Some(Spanned {
                        contents: Token::NewLine | Token::Eof,
                        ..
                    })
                ) {
                    Ok(Statement::NewLine)
                } else {
                    parse_statement(tokens)
                }
            }
            _ => Err(ParsingError {
                err_type: ParsingErrorType::InvalidStatement,
                start: t.start,
                end: t.end,
            }),
        }
    } else {
        Err(ParsingError {
            err_type: ParsingErrorType::InvalidStatement,
            start: 0,
            end: 0,
        })
    }
}

pub fn parse_expression(
    tokens: &mut Peekable<Lexer>,
    endings: &[Token],
) -> Result<Expression, ParsingError> {
    let mut previous = None;
    while let Some(t) = tokens.peek() {
        if endings.contains(&t.contents) {
            break;
        }
        if t.contents == Token::NewLine {
            tokens.next();
            continue;
        }
        let new_expr = parse_partial_expression(tokens, previous)?;
        previous = Some(new_expr);
    }
    previous.ok_or(ParsingError {
        err_type: ParsingErrorType::InvalidExpression,
        start: 0,
        end: 0,
    })
}

fn parse_partial_expression(
    tokens: &mut Peekable<Lexer>,
    previous: Option<Expression>,
) -> Result<Expression, ParsingError> {
    if let Some(t) = tokens.next() {
        match t.contents {
            Token::Regex(r) => Ok(Expression::Regex(r)),
            Token::Str(s) => Ok(Expression::String(s)),
            Token::Ident(name) => {
                if let Some(t) = tokens.peek() {
                    if t.contents == Token::LParen {
                        tokens.next();
                        let inner = parse_expression(tokens, &[Token::RParen])?;
                        tokens.next();
                        return Ok(Expression::Call(name, Box::new(inner)));
                    }
                }
                Ok(Expression::Variable(name))
            }
            Token::Plus => {
                let next = parse_partial_expression(tokens, previous.clone())?;
                Ok(Expression::Plus(
                    Box::new(previous.ok_or(ParsingError {
                        err_type: ParsingErrorType::InvalidExpression,
                        start: 0,
                        end: 0,
                    })?),
                    Box::new(next),
                ))
            }
            Token::LParen => {
                let Some(t) = tokens.next() else {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::UnexpectedEOF,
                        start: t.end,
                        end: t.end,
                    });
                };
                if t.contents == Token::RParen {
                    Ok(Expression::Empty)
                } else {
                    Err(ParsingError {
                        err_type: ParsingErrorType::ExpectedToken(Token::RParen, t.contents),
                        start: t.start,
                        end: t.end,
                    })
                }
            }
            Token::LBracket => {
                let mut contents = Vec::new();
                while let Some(t) = tokens.peek() {
                    if t.contents == Token::RBracket {
                        tokens.next();
                        break;
                    }
                    contents.push(parse_expression(tokens, &[Token::Comma, Token::RBracket])?);
                    if matches!(
                        tokens.peek(),
                        Some(Spanned {
                            contents: Token::Comma,
                            ..
                        }),
                    ) {
                        tokens.next();
                    }
                }
                Ok(Expression::Iterator(contents))
            }
            Token::Dollars => {
                // ident
                let Some(ident) = tokens.next() else {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::UnexpectedEOF,
                        start: t.end,
                        end: t.end,
                    });
                };
                let Token::Ident(name) = ident.contents else {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::ExpectedIdent(ident.contents),
                        start: ident.start,
                        end: ident.end,
                    });
                };

                // (
                let Some(t) = tokens.next() else {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::UnexpectedEOF,
                        start: ident.end,
                        end: ident.end,
                    });
                };
                if t.contents != Token::LParen {
                    return Err(ParsingError {
                        err_type: ParsingErrorType::ExpectedToken(Token::LParen, t.contents),
                        start: t.start,
                        end: t.end,
                    });
                }

                let mut contents = Vec::new();
                while let Some(t) = tokens.peek() {
                    if t.contents == Token::RParen {
                        tokens.next();
                        break;
                    }
                    contents.push(Box::new(parse_expression(
                        tokens,
                        &[Token::Comma, Token::RParen],
                    )?));
                    if matches!(
                        tokens.peek(),
                        Some(Spanned {
                            contents: Token::Comma,
                            ..
                        }),
                    ) {
                        tokens.next();
                    }
                }
                Ok(Expression::Builtin(name, contents))
            }
            Token::NewLine => parse_partial_expression(tokens, previous),
            _ => Err(ParsingError {
                err_type: ParsingErrorType::InvalidExpression,
                start: t.start,
                end: t.end,
            }),
        }
    } else {
        Err(ParsingError {
            err_type: ParsingErrorType::InvalidExpression,
            start: 0,
            end: 0,
        })
    }
}

#[cfg(test)]
mod test {
    use super::parse_expression;
    use super::parse_statement;
    use super::Lexer;
    use super::Token;

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

    #[test]
    fn test_parsing_simple_call() {
        let mut tokens = Lexer::new("run_thing('v')").peekable();
        insta::assert_debug_snapshot!(parse_expression(&mut tokens, &[Token::Eof]))
    }

    #[test]
    fn test_parsing_complex_call() {
        let mut tokens =
            Lexer::new("run_thing?('v' + '$#@!@' + '#$@$@#$@#$@#$' + other + another)").peekable();
        insta::assert_debug_snapshot!(parse_expression(&mut tokens, &[Token::Eof]))
    }

    #[test]
    fn test_parsing_unicode_ident() {
        let mut tokens = Lexer::new("lietuviškasKintamasis").peekable();
        insta::assert_debug_snapshot!(parse_expression(&mut tokens, &[Token::Eof]))
    }

    #[test]
    fn test_parsing_iterator_no_trailing_comma() {
        let mut tokens = Lexer::new(r#"["hi", variable, "hello " + "there"]"#).peekable();
        insta::assert_debug_snapshot!(parse_expression(&mut tokens, &[Token::Eof]))
    }

    #[test]
    fn test_parsing_iterator_trailing_comma() {
        let mut tokens = Lexer::new(r#"["hi", variable, "hello " + "there",]"#).peekable();
        insta::assert_debug_snapshot!(parse_expression(&mut tokens, &[Token::Eof]))
    }

    #[test]
    fn test_parsing_iterator_empty() {
        let mut tokens = Lexer::new("[]").peekable();
        insta::assert_debug_snapshot!(parse_expression(&mut tokens, &[Token::Eof]))
    }

    #[test]
    fn test_parsing_adding_strings() {
        let mut tokens = Lexer::new(r#" "pirmas" + "antras" "#).peekable();
        insta::assert_debug_snapshot!(parse_expression(&mut tokens, &[Token::Eof]))
    }

    #[test]
    fn test_parsing_simple_function_def() {
        let mut tokens = Lexer::new(
            r#"
def do_thing('hi (?<name>.*)'):
    "hello " + name
end
"#,
        )
        .peekable();
        insta::assert_debug_snapshot!(parse_statement(&mut tokens))
    }

    #[test]
    fn test_parsing_simple_function_where() {
        let mut tokens = Lexer::new(
            r#"
def do_thing('hi (?<name>.*)'):
    "hello " + name
where
    "hi joe" -> "hello joe";
    "hey john" -> ();
end
"#,
        )
        .peekable();
        insta::assert_debug_snapshot!(parse_statement(&mut tokens))
    }

    #[test]
    fn test_parsing_function_where_loop() {
        let mut tokens = Lexer::new(
            r#"
def do_thing('hi (?<name>.*)'):
    "hello " + name
where
    "hi " + name -> "hello " + name for name in ["Elisa", "Jonas", "Hubert Blaine Wolfeschlegelsteinhausenbergerdorff"];
end
"#,
        )
        .peekable();
        insta::assert_debug_snapshot!(parse_statement(&mut tokens))
    }

    #[test]
    fn test_parsing_complex_function_def() {
        let mut tokens = Lexer::new(
            r#"
def      do_thing('hi (?<name>.*)' + other + variables + and + stuff):
    "hello " + name + $if_else(other, ", how are you?", "!",)
end
"#,
        )
        .peekable();
        insta::assert_debug_snapshot!(parse_statement(&mut tokens))
    }

    #[test]
    fn test_parsing_builtin_in_func() {
        let mut tokens = Lexer::new(
            r#"
def do_thing('parse_(?<name>.)'):
    $if_else(name, "asdfkl" + "asdklfj" + "aksldfasdfj", "yes",)
end
"#,
        )
        .peekable();
        insta::assert_debug_snapshot!(parse_statement(&mut tokens))
    }

    #[test]
    fn test_parsing_builtin_in_func_without_comma() {
        let mut tokens = Lexer::new(
            r#"
def do_thing('parse_(?<name>.)'):
    $if_else(name, "asdfkl" + "asdklfj" + "aksldfasdfj", "yes")
end
"#,
        )
        .peekable();
        insta::assert_debug_snapshot!(parse_statement(&mut tokens))
    }
}
