use pretty::RcDoc;

use crate::interpreter::{Expression, Statement};

impl Expression {
    pub fn to_doc(&self) -> RcDoc<()> {
        match self {
            Expression::String(x) => RcDoc::as_string(format!("\"{x}\"")),
            Expression::Regex(x) => RcDoc::as_string(format!("'{x}'")),
            Expression::Variable(x) => RcDoc::as_string(x),
            Expression::Call(func, arg) => RcDoc::as_string(format!("{func}("))
                .append(
                    RcDoc::softline()
                        .append(arg.to_doc())
                        .nest(INDENT_SIZE)
                        .group(),
                )
                .append(RcDoc::text(")")),
            Expression::Builtin(func, args) => RcDoc::as_string(format!("${func}("))
                .append(
                    RcDoc::intersperse(
                        args.iter().map(|x| x.to_doc().append(RcDoc::text(","))),
                        RcDoc::line(),
                    )
                    .append(RcDoc::line())
                    .nest(INDENT_SIZE)
                    .group(),
                )
                .append(RcDoc::text(")")),
            Expression::Plus(expr1, expr2) => expr1
                .to_doc()
                .append(RcDoc::space())
                .append(RcDoc::text("+"))
                .append(RcDoc::line().append(expr2.to_doc()).group()),
            Expression::Empty => RcDoc::text("()"),
        }
    }
}

impl Statement {
    pub fn to_doc(&self) -> RcDoc<()> {
        match self {
            Statement::Let(var, contents) => RcDoc::text("let")
                .append(RcDoc::line())
                .append(RcDoc::text(var))
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(
                    RcDoc::line()
                        .append(contents.to_doc())
                        .append(RcDoc::text(";"))
                        .nest(INDENT_SIZE)
                        .group(),
                )
                .nest(INDENT_SIZE)
                .group(),
            Statement::Def(func, matcher, handler) => RcDoc::line().append(
                RcDoc::text("def")
                    .append(RcDoc::space())
                    .append(RcDoc::text(func))
                    .append(RcDoc::space())
                    .append(RcDoc::text("("))
                    .append(
                        RcDoc::line_()
                            .append(matcher.to_doc())
                            .append(RcDoc::line_())
                            .nest(INDENT_SIZE)
                            .group(),
                    )
                    .append(RcDoc::text(")"))
                    .group()
                    .append(RcDoc::text(":"))
                    .append(
                        RcDoc::line()
                            .append(handler.to_doc())
                            .append(RcDoc::text(";"))
                            .nest(INDENT_SIZE),
                    ),
            ),
        }
        .append(RcDoc::line())
    }
}

const WIDTH: usize = 90;
const INDENT_SIZE: isize = 4;

pub fn pretty_file(statements: &[Statement]) -> String {
    let mut w = Vec::new();
    for statement in statements {
        statement.to_doc().render(WIDTH, &mut w).unwrap();
    }
    String::from_utf8(w).unwrap()
}