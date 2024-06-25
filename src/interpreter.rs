use std::{collections::HashMap, fmt::Display};

use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Regex(String),
    String(String),
    Variable(String),
    Plus(Box<Expression>, Box<Expression>),
    Empty,
    Call(String, Box<Expression>),
    Builtin(String, Vec<Box<Expression>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(String, Expression),
    Def(String, Expression, Expression),
    Comment(String),
    NewLine,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Str,
    Regex,
    Empty,
}

impl From<Value> for Type {
    fn from(value: Value) -> Self {
        match value {
            Value::Empty => Type::Empty,
            Value::Regex(_) => Type::Regex,
            Value::Str(_) => Type::Str,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeErrorType {
    TypeError(TypeErrorType),
    NonExistentVariable(String),
    WrongNumberOfArgs(usize, usize),
    NonExistentBuiltin(String),
    NonExistentFunction(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeErrorType {
    Adding(Type, Type),
    ExpectedType(Type, Value),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeError {
    pub when_evaluating: Expression,
    pub error_type: RuntimeErrorType,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut expr = Vec::new();
        self.when_evaluating.to_doc().render(80, &mut expr).unwrap();
        write!(
            f,
            "Runtime error while evaluating `{}`: ",
            String::from_utf8(expr).unwrap()
        )?;
        match &self.error_type {
            RuntimeErrorType::TypeError(t) => {
                write!(f, "(type error) ")?;
                match t {
                    TypeErrorType::Adding(t1, t2) => {
                        write!(f, "Cannot add types {:?} and {:?}", t1, t2)
                    }
                    TypeErrorType::ExpectedType(t, got) => {
                        write!(
                            f,
                            "Expected type {:?}; got {:?} (type {:?})",
                            t,
                            got,
                            std::convert::Into::<Type>::into(got.clone())
                        )
                    }
                }
            }
            RuntimeErrorType::WrongNumberOfArgs(expected, got) => write!(
                f,
                "Incorrect number of arguments; expected {expected}, got {got}"
            ),
            RuntimeErrorType::NonExistentBuiltin(builtin) => {
                write!(f, "Builtin {builtin} does not exist")
            }
            RuntimeErrorType::NonExistentFunction(function) => {
                write!(f, "Function {function} does not exist")
            }
            RuntimeErrorType::NonExistentVariable(variable) => {
                write!(f, "Variable {variable} does not exist")
            }
        }
    }
}

impl Interpreter {
    pub fn eval(
        &self,
        expr: &Expression,
        additional: Option<&[Scope]>,
    ) -> Result<Value, RuntimeError> {
        match expr {
            Expression::Plus(e1, e2) => {
                let evaled = (self.eval(e1, additional)?, self.eval(e2, additional)?);
                match evaled {
                    (Value::Str(mut s1), Value::Str(s2)) => {
                        s1.push_str(&s2);
                        Ok(Value::Str(s1))
                    }
                    (Value::Regex(mut r1), Value::Regex(r2)) => {
                        r1.push_str(&r2);
                        Ok(Value::Regex(r1))
                    }
                    (v1, v2) => Err(RuntimeError {
                        when_evaluating: expr.clone(),
                        error_type: RuntimeErrorType::TypeError(TypeErrorType::Adding(
                            v1.into(),
                            v2.into(),
                        )),
                    }),
                }
            }
            Expression::Regex(r) => Ok(Value::Regex((*r).to_owned())),
            Expression::String(s) => Ok(Value::Str((*s).to_owned())),
            Expression::Variable(var) => {
                if let Some(scopes) = additional {
                    for scope in scopes.iter().rev() {
                        if let Some(v) = scope.variables.get(var.as_str()) {
                            return Ok(v.clone());
                        }
                    }
                }
                if let Some(v) = self.scope.variables.get(var.as_str()) {
                    return Ok(v.clone());
                }
                Err(RuntimeError {
                    when_evaluating: expr.clone(),
                    error_type: RuntimeErrorType::NonExistentVariable(var.to_owned()),
                })
            }
            Expression::Empty => Ok(Value::Empty),
            Expression::Call(func, expr) => {
                let call_value = self.eval(expr, additional)?;
                if let Value::Str(input) = call_value {
                    self.run_function(func, &input)
                } else {
                    Err(RuntimeError {
                        when_evaluating: *expr.clone(),
                        error_type: RuntimeErrorType::TypeError(TypeErrorType::ExpectedType(
                            Type::Str,
                            call_value,
                        )),
                    })
                }
            }
            Expression::Builtin(func, exprs) => {
                let mut values = exprs.iter().map(|expr| self.eval(expr, additional));
                match func.as_str() {
                    "if_else" => {
                        if exprs.len() != 3 {
                            Err(RuntimeError {
                                when_evaluating: expr.clone(),
                                error_type: RuntimeErrorType::WrongNumberOfArgs(values.len(), 3),
                            })
                        } else if values
                            .next()
                            .expect("values count has already been determined")?
                            != Value::Empty
                        {
                            values
                                .next()
                                .expect("values count has already been determined")
                        } else {
                            values.next();
                            values
                                .next()
                                .expect("values count has already been determined")
                        }
                    }
                    "map" => {
                        if exprs.len() != 2 {
                            Err(RuntimeError {
                                when_evaluating: expr.clone(),
                                error_type: RuntimeErrorType::WrongNumberOfArgs(values.len(), 2),
                            })
                        } else if values
                            .next()
                            .expect("values count has already been determined")?
                            != Value::Empty
                        {
                            values
                                .next()
                                .expect("values count has already been determined")
                        } else {
                            Ok(Value::Empty)
                        }
                    }
                    "unwrap_empty" => {
                        if exprs.len() != 2 {
                            return Err(RuntimeError {
                                when_evaluating: expr.clone(),
                                error_type: RuntimeErrorType::WrongNumberOfArgs(values.len(), 2),
                            });
                        }
                        let initial = values
                            .next()
                            .expect("values count has already been determined")?;
                        if initial != Value::Empty {
                            values
                                .next()
                                .expect("values count has already been determined")
                        } else {
                            Ok(initial)
                        }
                    }
                    v => Err(RuntimeError {
                        when_evaluating: expr.clone(),
                        error_type: RuntimeErrorType::NonExistentBuiltin(v.to_owned()),
                    }),
                }
            }
        }
    }

    pub fn new() -> Self {
        Self {
            scope: Default::default(),
        }
    }

    pub fn run_statements(&mut self, statements: Vec<Statement>) -> Result<(), RuntimeError> {
        for statement in statements {
            match statement {
                Statement::Let(var, value) => {
                    let v = self.eval(&value, None)?;
                    self.scope.variables.insert(var.to_owned(), v);
                }
                Statement::Def(func_name, pattern, handler) => {
                    self.scope.functions.insert(func_name, (pattern, handler));
                }
                Statement::Comment(_) | Statement::NewLine => {}
            }
        }
        Ok(())
    }

    pub fn run_function(&self, function: &str, input: &str) -> Result<Value, RuntimeError> {
        if let Some((matcher, handler)) = self.scope.functions.get(function) {
            self.eval_function(matcher, handler, input)
        } else {
            Err(RuntimeError {
                when_evaluating: Expression::Call(
                    function.to_owned(),
                    Box::new(Expression::String(input.to_owned())),
                ),
                error_type: RuntimeErrorType::NonExistentFunction(function.to_owned()),
            })
        }
    }

    fn eval_function(
        &self,
        matcher: &Expression,
        handler: &Expression,
        input: &str,
    ) -> Result<Value, RuntimeError> {
        let matched = self.eval(matcher, None)?;
        let Value::Regex(regex_matching) = matched else {
            return Err(RuntimeError {
                when_evaluating: matcher.clone(),
                error_type: RuntimeErrorType::TypeError(TypeErrorType::ExpectedType(
                    Type::Regex,
                    matched,
                )),
            });
        };

        let re = Regex::new(&regex_matching).unwrap();
        let Some(captures) = re.captures(input) else {
            return Ok(Value::Empty);
        };
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

        let handled = self.eval(
            handler,
            Some(&[Scope {
                variables,
                ..Default::default()
            }]),
        )?;

        if let Value::Str(_) = handled {
            Ok(handled)
        } else {
            Err(RuntimeError {
                when_evaluating: matcher.clone(),
                error_type: RuntimeErrorType::TypeError(TypeErrorType::ExpectedType(
                    Type::Str,
                    handled,
                )),
            })
        }
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::*;

    #[test]
    fn test_if_else_true_from_expression() {
        let interpreter: Interpreter = Default::default();
        let v = interpreter.eval(
            &Expression::Builtin(
                "if_else".to_owned(),
                vec![
                    Box::new(Expression::String("exists".to_owned())),
                    Box::new(Expression::String("t".to_owned())),
                    Box::new(Expression::String("f".to_owned())),
                ],
            ),
            None,
        );
        assert_eq!(v, Ok(Value::Str("t".to_owned())));
    }

    #[test]
    fn test_if_else_false_from_expression() {
        let interpreter: Interpreter = Default::default();
        let v = interpreter.eval(
            &Expression::Builtin(
                "if_else".to_owned(),
                vec![
                    Box::new(Expression::Empty),
                    Box::new(Expression::String("t".to_owned())),
                    Box::new(Expression::String("f".to_owned())),
                ],
            ),
            None,
        );
        assert_eq!(v, Ok(Value::Str("f".to_owned())));
    }

    #[test]
    fn test_run_function_from_expression() {
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
                    Box::new(Expression::Builtin(
                        "if_else".to_owned(),
                        vec![
                            Box::new(Expression::Variable("reflexive".to_owned())),
                            Box::new(Expression::String("s".to_owned())),
                            Box::new(Expression::String("".to_owned())),
                        ],
                    )),
                )),
            ),
            "persikelti",
        );
        assert_eq!(v, Ok("keltis".to_owned()));
    }
}
