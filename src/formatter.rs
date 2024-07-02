use pretty::RcDoc;

use crate::interpreter::{Expression, Statement, TestRule};

impl Expression {
    pub fn to_doc(&self) -> RcDoc<()> {
        match self {
            Self::String(x) => RcDoc::as_string(format!("\"{x}\"")),
            Self::Regex(x) => RcDoc::as_string(format!("'{x}'")),
            Self::Variable(x) => RcDoc::as_string(x),
            Self::Call(func, arg) => RcDoc::as_string(format!("{func}("))
                .append(
                    RcDoc::softline()
                        .append(arg.to_doc())
                        .nest(INDENT_SIZE)
                        .group(),
                )
                .append(RcDoc::text(")")),
            Self::Builtin(func, args) => RcDoc::as_string(format!("${func}("))
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
            Self::Plus(expr1, expr2) => expr1
                .to_doc()
                .append(RcDoc::space())
                .append(RcDoc::text("+"))
                .append(RcDoc::line().append(expr2.to_doc()).group()),
            Self::Empty => RcDoc::text("()"),
            Self::Iterator(v) => RcDoc::text("[")
                .append(RcDoc::intersperse(
                    v.iter().map(|v| v.to_doc().append(RcDoc::text(","))),
                    RcDoc::line(),
                ))
                .append(RcDoc::text("]"))
                .group(),
        }
    }
}

impl Statement {
    pub fn to_doc(&self) -> RcDoc<()> {
        match self {
            Self::Let(var, contents) => RcDoc::text("let")
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
            Self::Def(func, matcher, handler, rules) => {
                let main = RcDoc::text("def")
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
                    .append(RcDoc::line().append(handler.to_doc()).nest(INDENT_SIZE))
                    .append(RcDoc::line());

                if rules.is_empty() {
                    main
                } else {
                    let where_section = RcDoc::text("where")
                        .append(RcDoc::line())
                        .append(RcDoc::intersperse(
                            rules.iter().map(|r| r.to_doc()),
                            RcDoc::line(),
                        ))
                        .nest(INDENT_SIZE)
                        .append(RcDoc::line());
                    main.append(where_section)
                }
                .append(RcDoc::text("end"))
            }
            Self::Comment(c) => RcDoc::as_string(format!("#{c}")),
            Self::NewLine => RcDoc::text(""),
        }
        .append(RcDoc::line())
    }
}

impl TestRule {
    pub fn to_doc(&self) -> RcDoc<()> {
        let test_body = self
            .input
            .to_doc()
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(
                RcDoc::line()
                    .append(self.expected_output.to_doc())
                    .nest(INDENT_SIZE),
            );

        if self.for_vars.is_empty() {
            test_body
        } else {
            test_body.append(
                RcDoc::line()
                    .append(
                        RcDoc::text("for")
                            .append(RcDoc::space())
                            .append(RcDoc::text(&self.for_vars[0].0))
                            .append(RcDoc::space())
                            .append(RcDoc::text("in"))
                            .append(RcDoc::space())
                            .append(self.for_vars[0].1.to_doc())
                            .group(),
                    )
                    .nest(INDENT_SIZE),
            )
        }
        .append(RcDoc::text(";"))
        .group()
    }
}

const WIDTH: usize = 90;
const INDENT_SIZE: isize = 4;

pub fn pretty_file(statements: &[Statement]) -> String {
    let mut w = Vec::new();
    let mut last = None;
    for statement in statements {
        if matches!(statement, Statement::Def(_, _, _, _))
            && !matches!(
                last,
                Some(&Statement::Comment(_)) | Some(Statement::NewLine)
            )
        {
            RcDoc::<()>::line().render(WIDTH, &mut w).unwrap();
        }
        if matches!(last, Some(Statement::NewLine)) && statement == &Statement::NewLine {
            continue;
        }
        statement.to_doc().render(WIDTH, &mut w).unwrap();
        last = Some(statement);
    }
    String::from_utf8(w).unwrap().trim().to_owned()
}
