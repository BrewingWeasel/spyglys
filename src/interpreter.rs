use std::{collections::HashMap, fmt::Display};

use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Regex(String),
    String(String),
    Variable(String),
    Plus(Box<Expression>, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(String, Expression),
    Def(String, Expression, Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Str(String),
    Regex(String),
    Empty,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "()"),
            Self::Regex(r) => write!(f, "'{}'", r),
            Self::Str(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Default)]
pub struct Interpreter {
    scope: Scope,
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    variables: HashMap<String, Value>,
    functions: HashMap<String, (Expression, Expression)>,
}

impl Interpreter {
    pub fn eval(&self, expr: &Expression, additional: Option<&[Scope]>) -> Value {
        match expr {
            Expression::Plus(v1, v2) => {
                let evaled = (self.eval(v1, additional), self.eval(v2, additional));
                match evaled {
                    (Value::Str(mut s1), Value::Str(s2)) => {
                        s1.push_str(&s2);
                        Value::Str(s1)
                    }
                    (Value::Regex(mut r1), Value::Regex(r2)) => {
                        r1.push_str(&r2);
                        Value::Regex(r1)
                    }
                    _ => {
                        println!("{:?} {:?}", v1, v2);
                        println!("{:?}", evaled);
                        todo!()
                    }
                }
            }
            Expression::Ternary(cond, v1, v2) => {
                let option = self.eval(cond, additional);
                if option != Value::Empty {
                    self.eval(v1, additional)
                } else {
                    self.eval(v2, additional)
                }
            }
            Expression::Regex(r) => Value::Regex((*r).to_owned()),
            Expression::String(s) => Value::Str((*s).to_owned()),
            Expression::Variable(var) => {
                if let Some(scopes) = additional {
                    for scope in scopes.iter().rev() {
                        if let Some(v) = scope.variables.get(var.as_str()) {
                            return v.clone();
                        }
                    }
                }
                if let Some(v) = self.scope.variables.get(var.as_str()) {
                    return v.clone();
                }
                println!("{:?} {:?}", var, self.scope.variables);
                todo!();
            }
            Expression::Empty => Value::Empty,
        }
    }

    pub fn new() -> Self {
        Self {
            scope: Default::default(),
        }
    }

    pub fn run_statements(&mut self, statements: Vec<Statement>) {
        for statement in statements {
            match statement {
                Statement::Let(var, value) => {
                    let v = self.eval(&value, None);
                    self.scope.variables.insert(var.to_owned(), v);
                }
                Statement::Def(func_name, pattern, handler) => {
                    self.scope.functions.insert(func_name, (pattern, handler));
                }
            }
        }
    }

    pub fn run_function(&self, function: &str, input: &str) -> String {
        if let Some((matcher, handler)) = self.scope.functions.get(function) {
            self.eval_function(matcher, handler, input)
        } else {
            println!("{:?} {:?}", function, self.scope.functions);
            todo!()
        }
    }

    fn eval_function(&self, matcher: &Expression, handler: &Expression, input: &str) -> String {
        let Value::Regex(regex_matching) = self.eval(matcher, None) else {
            todo!()
        };
        let re = Regex::new(&regex_matching).unwrap();
        let captures = re.captures(input).unwrap();
        let mut variables = HashMap::new();
        for name in re.capture_names() {
            let Some(var_name) = name else {
                continue;
            };
            variables.insert(
                var_name.to_owned(),
                captures
                    .name(var_name)
                    .map_or(Value::Empty, |v| Value::Str(v.as_str().to_string())),
            );
        }
        if let Value::Str(r) = self.eval(
            handler,
            Some(&[Scope {
                variables,
                ..Default::default()
            }]),
        ) {
            r
        } else {
            todo!();
        }
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::*;

    #[test]
    fn test_ternary_true() {
        let interpreter: Interpreter = Default::default();
        let v = interpreter.eval(
            &Expression::Ternary(
                Box::new(Expression::String("exists".to_owned())),
                Box::new(Expression::String("t".to_owned())),
                Box::new(Expression::String("f".to_owned())),
            ),
            None,
        );
        assert_eq!(v, Value::Str("t".to_owned()));
    }

    #[test]
    fn test_ternary_false() {
        let interpreter: Interpreter = Default::default();
        let v = interpreter.eval(
            &Expression::Ternary(
                Box::new(Expression::Empty),
                Box::new(Expression::String("t".to_owned())),
                Box::new(Expression::String("f".to_owned())),
            ),
            None,
        );
        assert_eq!(v, Value::Str("f".to_owned()));
    }

    #[test]
    fn test_run_function() {
        let interpreter: Interpreter = Default::default();
        let v = interpreter.eval_function(
            &Expression::Plus(
                Box::new(Expression::Regex("^per(?<reflexive>si)".to_owned())),
                Box::new(Expression::Regex("(?<stem>\\w*)ti$".to_owned())),
            ),
            &Expression::Plus(
                Box::new(Expression::Variable("stem".to_owned())),
                Box::new(Expression::Plus(
                    Box::new(Expression::String("ti".to_owned())),
                    Box::new(Expression::Ternary(
                        Box::new(Expression::Variable("reflexive".to_owned())),
                        Box::new(Expression::String("s".to_owned())),
                        Box::new(Expression::String("".to_owned())),
                    )),
                )),
            ),
            "persikelti",
        );
        assert_eq!(v, "keltis");
    }
}
