use std::{error::Error, fmt::Display};

use interpreter::{CompileTimeError, Interpreter, RuntimeError, Statement, Value};
use parser::{Lexer, LexingError, LexingErrorType, ParsingError, ParsingErrorType, Token};

pub mod formatter;
pub mod interpreter;
pub mod parser;

#[derive(Clone, Debug)]
pub enum SpyglysError {
    Lexing(LexingError),
    Parsing(ParsingError),
    Runtime(RuntimeError),
    CompileTime(CompileTimeError),
}

impl Error for SpyglysError {}

impl Display for SpyglysError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lexing(LexingError { err_type, position }) => {
                write!(f, "Lexing error at {position}: ")?;
                match err_type {
                    LexingErrorType::UnexpectedEof => write!(f, "Unexpected EOF"),
                    LexingErrorType::UnexpectedChar(actual) => {
                        write!(f, "Expected a valid character for an ident; found {actual}")
                    }
                }
            }
            Self::Parsing(ParsingError {
                err_type,
                start,
                end,
            }) => {
                write!(f, "Parsing error at at {start}..{end}: ")?;
                match err_type {
                    ParsingErrorType::UnexpectedEOF => write!(f, "Unexpected EOF"),
                    ParsingErrorType::InvalidExpression => write!(f, "Invalid expression"),
                    ParsingErrorType::InvalidStatement => write!(f, "Invalid statement"),
                    ParsingErrorType::ExpectedIdent(g) => write!(
                        f,
                        "Expected an identifier for this object; found token `{g}`"
                    ),
                    ParsingErrorType::ExpectedToken(wanted, got) => {
                        write!(f, "Expected token `{wanted}`; got `{got}`")
                    }
                }
            }
            Self::CompileTime(e) => {
                write!(f, "Compiletime error: {e}")
            }
            Self::Runtime(r) => {
                write!(f, "{r}")
            }
        }
    }
}

pub fn parse_string(input: &str) -> Result<Vec<Statement>, SpyglysError> {
    let mut lexed = Lexer::new(input).peekable();

    let mut statements = Vec::new();
    while let Some(t) = lexed.peek() {
        if t.contents == Token::Eof {
            break;
        }
        statements.push(parser::parse_statement(&mut lexed).map_err(SpyglysError::Parsing)?);
    }
    Ok(statements)
}

pub fn contents_to_interpreter(input: &str) -> Result<Interpreter, SpyglysError> {
    let statements = parse_string(input)?;
    let mut i = Interpreter::new();
    i.run_statements(statements)
        .map_err(SpyglysError::Runtime)?;
    i.run_tests().map_err(SpyglysError::CompileTime)?;
    Ok(i)
}

pub fn run_input(input: &str, interpreter: &mut Interpreter) -> Result<Value, SpyglysError> {
    let mut lexed = Lexer::new(input).peekable();

    match parser::parse_expression(&mut lexed, &[Token::Eof]) {
        Ok(expr) => {
            let resp = interpreter.eval(&expr, None);
            return resp.map_err(SpyglysError::Runtime);
        }
        Err(e) => {
            if e.err_type != ParsingErrorType::InvalidExpression {
                return Err(SpyglysError::Parsing(e));
            }
        }
    }
    match parser::parse_statement(&mut lexed) {
        Ok(statement) => {
            interpreter
                .run_statements(vec![statement])
                .map_err(SpyglysError::Runtime)?;
            Ok(Value::Empty)
        }
        Err(e) => Err(SpyglysError::Parsing(e)),
    }
}
