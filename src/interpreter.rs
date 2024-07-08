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
    Iterator(Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(String, Expression),
    Def(String, Expression, Expression, Vec<TestRule>),
    Comment(String),
    NewLine,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestRule {
    pub input: Expression,
    pub expected_output: Expression,
    pub for_vars: Vec<(String, Expression)>,
}

impl Display for TestRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut expr_str = Vec::new();
        self.to_doc()
            .render(80, &mut expr_str)
            .expect("rendering to work");
        write!(
            f,
            "{}",
            &String::from_utf8(expr_str).expect("formatting to generate valid utf8")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Str(String),
    Regex(String),
    Empty,
    Iterator(Vec<Expression>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "()"),
            Self::Regex(r) => write!(f, "'{}'", r),
            Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Iterator(exprs) => write!(
                f,
                "[{}]",
                exprs.iter().fold(String::new(), |mut acc, e| {
                    let mut expr_str = Vec::new();
                    e.to_doc()
                        .render(80, &mut expr_str)
                        .expect("rendering to work");
                    acc.push_str(&String::from_utf8(expr_str).unwrap());
                    acc
                })
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Str,
    Regex,
    Empty,
    Iterator(Option<Box<Type>>),
}

pub struct BuiltinFunction {
    handler: Box<
        dyn (Fn(
                &mut dyn Iterator<Item = Result<Value, RuntimeError>>,
                &Interpreter,
            ) -> Result<Value, RuntimeError>)
            + Sync,
    >,
    num_args: usize,
}

#[derive(Default)]
pub struct Interpreter {
    scope: Scope,
    builtins: HashMap<String, BuiltinFunction>,
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    variables: HashMap<String, Value>,
    functions: HashMap<String, (Expression, Expression, Vec<TestRule>)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeErrorType {
    Adding(Type, Type),
    ExpectedType(Type, Value, Type),
    NonExistentVariable,
}

impl Display for TypeErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Adding(t1, t2) => {
                write!(f, "Cannot add types {:?} and {:?}", t1, t2)
            }
            Self::ExpectedType(t, got, got_type) => {
                write!(
                    f,
                    "Expected type {:?}; got {:?} (type {:?})",
                    t, got, got_type
                )
            }
            Self::NonExistentVariable => {
                write!(f, "Unable to find variable when determining type")
            }
        }
    }
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
                write!(f, "(type error) {t}")
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompileTimeError {
    pub in_function: String,
    pub in_test: String,
    pub error_type: CompileTimeErrorType,
}

impl Display for CompileTimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Compiletime error in function `{}`:
in test `{}`:
",
            self.in_function, self.in_test
        )?;
        match &self.error_type {
            CompileTimeErrorType::TypeError(t) => write!(f, "{t}"),
            CompileTimeErrorType::RuntimeErrorInTest(t) => write!(f, "run time error in test: {t}"),
            CompileTimeErrorType::TestFailed(expected, got) => {
                write!(f, "expected value {expected} did not match {got}")
            }
            CompileTimeErrorType::IncorrectTestType(val, val_type) => {
                write!(f, "incorrect type: value {val} (type: {val_type:?})",)
            }
            CompileTimeErrorType::ExpectedIterator(val) => {
                write!(f, "expected an iterator but found {val}",)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileTimeErrorType {
    TestFailed(Value, Value),
    RuntimeErrorInTest(RuntimeError),
    IncorrectTestType(Value, Type),
    TypeError(TypeErrorType),
    ExpectedIterator(Value),
}

macro_rules! compile_time_error {
    ($expr:expr, $func:ident, $test:ident) => {
        CompileTimeError {
            error_type: $expr,
            in_function: $func.to_owned(),
            in_test: $test.to_string(),
        }
    };
}

macro_rules! try_or_compile_time_error {
    ($expr:expr, $func:ident, $test:ident) => {
        $expr.map_err(|e| CompileTimeError {
            error_type: CompileTimeErrorType::RuntimeErrorInTest(e),
            in_function: $func.to_owned(),
            in_test: $test.to_string(),
        })?
    };
    ($expr:expr, $func:ident, $test:ident, $handler:expr) => {
        $expr.map_err(|e| CompileTimeError {
            error_type: $handler(e),
            in_function: $func.to_owned(),
            in_test: $test.to_string(),
        })?
    };
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
                    (Value::Str(s), Value::Empty) | (Value::Empty, Value::Str(s)) => {
                        Ok(Value::Str(s))
                    }
                    (Value::Regex(mut r1), Value::Regex(r2)) => {
                        r1.push_str(&r2);
                        Ok(Value::Regex(r1))
                    }
                    (v1, v2) => Err(RuntimeError {
                        when_evaluating: expr.clone(),
                        error_type: RuntimeErrorType::TypeError(TypeErrorType::Adding(
                            self.value_to_type(&v1).map_err(|error_type| RuntimeError {
                                when_evaluating: *e1.to_owned(),
                                error_type: RuntimeErrorType::TypeError(error_type),
                            })?,
                            self.value_to_type(&v2).map_err(|error_type| RuntimeError {
                                when_evaluating: *e2.to_owned(),
                                error_type: RuntimeErrorType::TypeError(error_type),
                            })?,
                        )),
                    }),
                }
            }
            Expression::Regex(r) => Ok(Value::Regex((*r).to_owned())),
            Expression::String(s) => Ok(Value::Str((*s).to_owned())),
            Expression::Iterator(xs) => Ok(Value::Iterator(xs.to_owned())),
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
            Expression::Call(func, argument_expr) => {
                let call_value = self.eval(argument_expr, additional)?;
                if let Value::Str(input) = call_value {
                    self.run_function(func, &input)
                } else {
                    let call_type =
                        self.value_to_type(&call_value)
                            .map_err(|error_type| RuntimeError {
                                when_evaluating: *argument_expr.to_owned(),
                                error_type: RuntimeErrorType::TypeError(error_type),
                            })?;
                    Err(RuntimeError {
                        when_evaluating: *argument_expr.clone(),
                        error_type: RuntimeErrorType::TypeError(TypeErrorType::ExpectedType(
                            Type::Str,
                            call_value,
                            call_type,
                        )),
                    })
                }
            }
            Expression::Builtin(func, exprs) => {
                let mut values = exprs.iter().map(|expr| self.eval(expr, additional));
                if let Some(func_details) = self.builtins.get(func) {
                    if exprs.len() != func_details.num_args {
                        Err(RuntimeError {
                            when_evaluating: expr.clone(),
                            error_type: RuntimeErrorType::WrongNumberOfArgs(
                                values.len(),
                                func_details.num_args,
                            ),
                        })
                    } else {
                        (func_details.handler)(&mut values, self)
                    }
                } else {
                    Err(RuntimeError {
                        when_evaluating: expr.clone(),
                        error_type: RuntimeErrorType::NonExistentBuiltin(func.to_owned()),
                    })
                }
            }
        }
    }

    pub fn new() -> Self {
        Self {
            scope: Default::default(),
            builtins: HashMap::from([
                (
                    String::from("if_else"),
                    BuiltinFunction {
                        handler: Box::new(
                            |values: &mut dyn Iterator<Item = Result<Value, RuntimeError>>,
                             _: &Interpreter| {
                                if values
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
                            },
                        ) as _,
                        num_args: 3,
                    },
                ),
                (
                    String::from("map"),
                    BuiltinFunction {
                        handler: Box::new(
                            |values: &mut dyn Iterator<Item = Result<Value, RuntimeError>>,
                             _: &Interpreter| {
                                if values
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
                            },
                        ) as _,
                        num_args: 2,
                    },
                ),
                (
                    String::from("unwrap_empty"),
                    BuiltinFunction {
                        handler: Box::new(
                            |values: &mut dyn Iterator<Item = Result<Value, RuntimeError>>,
                             _: &Interpreter| {
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
                            },
                        ) as _,
                        num_args: 2,
                    },
                ),
                (
                    String::from("re:only"),
                    BuiltinFunction {
                        handler: Box::new(
                            |values: &mut dyn Iterator<Item = Result<Value, RuntimeError>>,
                             interpreter: &Interpreter| {
                                let value = values
                                    .next()
                                    .expect("values count has already been determined")?;
                                match value {
                                    Value::Str(s) => Ok(Value::Regex(format!("^({s})$"))),
                                    Value::Regex(r) => Ok(Value::Regex(format!("^({r})$"))),
                                    _ => Err(RuntimeError {
                                        when_evaluating: Expression::Empty,
                                        error_type: RuntimeErrorType::TypeError(
                                            TypeErrorType::ExpectedType(
                                                // TODO: multiple expected
                                                // types
                                                Type::Str,
                                                value.clone(),
                                                interpreter.value_to_type(&value).map_err(|v| {
                                                    RuntimeError {
                                                        when_evaluating: Expression::Empty,
                                                        error_type: RuntimeErrorType::TypeError(v),
                                                    }
                                                })?,
                                            ),
                                        ),
                                    }),
                                }
                            },
                        ) as _,
                        num_args: 1,
                    },
                ),
                (
                    String::from("re:any"),
                    BuiltinFunction {
                        handler: Box::new(
                            |values: &mut dyn Iterator<Item = Result<Value, RuntimeError>>,
                             interpreter: &Interpreter| {
                                let input = values
                                    .next()
                                    .expect("values count has already been determined")?;
                                let Value::Iterator(values_inside) = input else {
                                    return Err(RuntimeError {
                                        when_evaluating: Expression::Empty,
                                        error_type: RuntimeErrorType::TypeError(
                                            TypeErrorType::ExpectedType(
                                                Type::Iterator(None),
                                                input,
                                                Type::Empty,
                                            ),
                                        ),
                                    });
                                };
                                Ok(Value::Regex(
                                    values_inside
                                        .iter()
                                        .map(|expr| {
                                            let value = interpreter.eval(expr, None)?;
                                            match value {
                                                Value::Regex(r) => Ok(r),
                                                Value::Str(s) => Ok(s),
                                                v => Err(RuntimeError {
                                                    when_evaluating: Expression::Empty, // TODO: figure
                                                    // out how to do this better
                                                    error_type: RuntimeErrorType::TypeError(
                                                        TypeErrorType::ExpectedType(
                                                            Type::Regex,
                                                            v,
                                                            Type::Str,
                                                        ),
                                                    ),
                                                }),
                                            }
                                        })
                                        .collect::<Result<Vec<String>, RuntimeError>>()?
                                        .join("|"),
                                ))
                            },
                        ) as _,
                        num_args: 1,
                    },
                ),
            ]),
        }
    }

    pub fn expression_to_type(&self, expr: &Expression) -> Result<Type, TypeErrorType> {
        match expr {
            Expression::Empty => Ok(Type::Empty),
            Expression::String(_) => Ok(Type::Str),
            Expression::Regex(_) => Ok(Type::Regex),
            Expression::Plus(e, _) => self.expression_to_type(e),
            Expression::Iterator(conts) => {
                if let Some(expr) = conts.first() {
                    Ok(Type::Iterator(Some(Box::new(
                        self.expression_to_type(expr)?,
                    ))))
                } else {
                    Ok(Type::Iterator(None))
                }
            }
            Expression::Call(_, _) => Ok(Type::Str),
            Expression::Variable(var) => self
                .scope
                .variables
                .get(var)
                .map(|v| self.value_to_type(v))
                .ok_or(TypeErrorType::NonExistentVariable)?,
            Expression::Builtin(_, _) => todo!(),
        }
    }

    fn value_to_type(&self, value: &Value) -> Result<Type, TypeErrorType> {
        match value {
            Value::Empty => Ok(Type::Empty),
            Value::Regex(_) => Ok(Type::Regex),
            Value::Str(_) => Ok(Type::Str),
            Value::Iterator(conts) => {
                if let Some(expr) = conts.first() {
                    Ok(Type::Iterator(Some(Box::new(
                        self.expression_to_type(expr)?,
                    ))))
                } else {
                    Ok(Type::Iterator(None))
                }
            }
        }
    }

    pub fn run_tests(&self) -> Result<(), CompileTimeError> {
        for (func, (matcher, handler, tests)) in &self.scope.functions {
            for test in tests {
                if test.for_vars.is_empty() {
                    self.run_individual_test(func, test, matcher, handler, None)?;
                } else {
                    for (var, iterator_expr_for_values) in &test.for_vars {
                        let iterator_val = try_or_compile_time_error!(
                            self.eval(iterator_expr_for_values, None),
                            func,
                            test
                        );
                        let Value::Iterator(all_variable_expressions) = iterator_val else {
                            return Err(compile_time_error!(
                                CompileTimeErrorType::ExpectedIterator(iterator_val),
                                func,
                                test
                            ));
                        };
                        for specific_variable_expression in all_variable_expressions {
                            let test_scope = Some(vec![Scope {
                                variables: HashMap::from([(
                                    var.to_owned(),
                                    try_or_compile_time_error!(
                                        self.eval(&specific_variable_expression, None),
                                        func,
                                        test
                                    ),
                                )]),
                                ..Default::default()
                            }]);
                            self.run_individual_test(
                                func,
                                test,
                                matcher,
                                handler,
                                test_scope.as_deref(),
                            )?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn run_individual_test(
        &self,
        func: &str,
        test: &TestRule,
        matcher: &Expression,
        handler: &Expression,
        test_scope: Option<&[Scope]>,
    ) -> Result<(), CompileTimeError> {
        let input_value =
            try_or_compile_time_error!(self.eval(&test.input, test_scope), func, test);
        let Value::Str(input) = input_value else {
            let input_type = try_or_compile_time_error!(
                self.value_to_type(&input_value),
                func,
                test,
                CompileTimeErrorType::TypeError
            );
            return Err(compile_time_error!(
                CompileTimeErrorType::IncorrectTestType(input_value, input_type),
                func,
                test
            ));
        };

        let expected_value =
            try_or_compile_time_error!(self.eval(&test.expected_output, test_scope), func, test);

        let output =
            try_or_compile_time_error!(self.eval_function(matcher, handler, &input), func, test);
        if output != expected_value {
            return Err(compile_time_error!(
                CompileTimeErrorType::TestFailed(output, expected_value,),
                func,
                test
            ));
        }
        Ok(())
    }

    pub fn run_statements(&mut self, statements: Vec<Statement>) -> Result<(), RuntimeError> {
        for statement in statements {
            match statement {
                Statement::Let(var, value) => {
                    let v = self.eval(&value, None)?;
                    self.scope.variables.insert(var.to_owned(), v);
                }
                Statement::Def(func_name, pattern, handler, test_rules) => {
                    self.scope
                        .functions
                        .insert(func_name, (pattern, handler, test_rules));
                }
                Statement::Comment(_) | Statement::NewLine => {}
            }
        }
        Ok(())
    }

    pub fn run_function(&self, function: &str, input: &str) -> Result<Value, RuntimeError> {
        if let Some((matcher, handler, _test_rules)) = self.scope.functions.get(function) {
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
            let matched_type = self
                .value_to_type(&matched)
                .map_err(|error_type| RuntimeError {
                    when_evaluating: matcher.to_owned(),
                    error_type: RuntimeErrorType::TypeError(error_type),
                })?;
            return Err(RuntimeError {
                when_evaluating: matcher.clone(),
                error_type: RuntimeErrorType::TypeError(TypeErrorType::ExpectedType(
                    Type::Regex,
                    matched,
                    matched_type,
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
            let handled_type = self
                .value_to_type(&handled)
                .map_err(|error_type| RuntimeError {
                    when_evaluating: handler.to_owned(),
                    error_type: RuntimeErrorType::TypeError(error_type),
                })?;
            Err(RuntimeError {
                when_evaluating: handler.to_owned(),
                error_type: RuntimeErrorType::TypeError(TypeErrorType::ExpectedType(
                    Type::Str,
                    handled,
                    handled_type,
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
        let interpreter = Interpreter::new();
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
        let interpreter = Interpreter::new();
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
        let interpreter = Interpreter::new();
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
        assert_eq!(v, Ok(Value::Str("keltis".to_owned())));
    }
}
