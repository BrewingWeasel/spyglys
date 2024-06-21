use interpreter::{Interpreter, Statement, Value};
use parser::{Lexer, Token};

pub mod formatter;
pub mod interpreter;
pub mod parser;

pub fn parse_string(input: &str) -> Vec<Statement> {
    let mut lexed = Lexer::new(input).peekable();

    let mut statements = Vec::new();
    while let Some(t) = lexed.peek() {
        if t.contents == Token::Eof {
            break;
        }
        let Some(statement) = parser::parse_statement(&mut lexed) else {
            continue;
        };
        statements.push(statement);
    }
    statements
}

pub fn contents_to_interpreter(input: &str) -> Interpreter {
    let statements = parse_string(input);
    let mut i = Interpreter::new();
    i.run_statements(statements);
    i
}

pub fn run_input(input: &str, interpreter: &mut Interpreter) -> Value {
    let mut lexed = Lexer::new(input).peekable();

    if let Some(expr) = parser::parse_expression(&mut lexed, Token::Eof) {
        let resp = interpreter.eval(&expr, None);
        return resp;
    }
    if let Some(statement) = parser::parse_statement(&mut lexed) {
        interpreter.run_statements(vec![statement]);
    };
    Value::Empty
}
