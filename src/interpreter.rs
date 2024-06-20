use std::collections::HashMap;

use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expression<'a> {
    Regex(&'a str),
    String(&'a str),
    Variable(&'a str),
    Plus(Box<Expression<'a>>, Box<Expression<'a>>),
    Ternary(
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
    ),
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Statement<'a> {
    Let(&'a str, Expression<'a>),
    Def(&'a str, Expression<'a>, Expression<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    Str(String),
    Regex(String),
    Empty,
}

#[derive(Default)]
struct Interpreter<'a> {
    scope: Scope<'a>,
    statements: Vec<Statement<'a>>,
}

#[derive(Debug, Clone, Default)]
struct Scope<'a> {
    variables: HashMap<&'a str, Value>,
    functions: HashMap<&'a str, (&'a Expression<'a>, &'a Expression<'a>)>,
}

impl<'a> Interpreter<'a> {
    fn eval(&self, expr: &Expression, additional: Option<&[Scope]>) -> Value {
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
                    _ => todo!(),
                }
            }
            Expression::Ternary(cond, v1, v2) => {
                let option = self.eval(cond, additional.clone());
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
                        if let Some(v) = scope.variables.get(var) {
                            return v.clone();
                        }
                    }
                }
                if let Some(v) = self.scope.variables.get(var) {
                    return v.clone();
                }
                todo!();
            }
            Expression::Empty => Value::Empty,
        }
    }

    fn new_from_statements(statements: Vec<Statement<'a>>) -> Self {
        Self {
            scope: Default::default(),
            statements,
        }
    }

    fn prepare_statements(&'a mut self) {
        for statement in &self.statements {
            match statement {
                Statement::Let(var, value) => {
                    let v = self.eval(value, None);
                    self.scope.variables.insert(var, v);
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
                var_name,
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
                Box::new(Expression::String("exists")),
                Box::new(Expression::String("t")),
                Box::new(Expression::String("f")),
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
                Box::new(Expression::String("t")),
                Box::new(Expression::String("f")),
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
                Box::new(Expression::Regex("^per(?<reflexive>si)")),
                Box::new(Expression::Regex("(?<stem>\\w*)ti$")),
            ),
            &Expression::Plus(
                Box::new(Expression::Variable("stem")),
                Box::new(Expression::Plus(
                    Box::new(Expression::String("ti")),
                    Box::new(Expression::Ternary(
                        Box::new(Expression::Variable("reflexive")),
                        Box::new(Expression::String("s")),
                        Box::new(Expression::String("")),
                    )),
                )),
            ),
            "persikelti",
        );
        assert_eq!(v, "keltis");
    }
}
