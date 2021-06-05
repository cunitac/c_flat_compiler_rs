use nom::branch::alt;
use nom::bytes::complete::{tag, take_while_m_n};
use nom::character::complete::{alpha1, alphanumeric1, char, digit1, one_of, satisfy};
use nom::combinator::{all_consuming, map, map_res, opt, recognize, value};
use nom::error::ParseError;
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::{IResult, Parser};

type Span<'a> = nom_locate::LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
pub struct ASTree {
    root: Node,
}

#[derive(Debug, PartialEq)]
enum Node {
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Literal(Literal),
    Identifier(String),
    BinaryOp {
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Cond {
        cond: Box<Expr>,
        then: Box<Expr>,
        r#else: Box<Expr>,
    },
    LogicalOr {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    LogicalAnd {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

impl Expr {
    fn binary_op(op: &str, lhs: Expr, rhs: Expr) -> Expr {
        Expr::BinaryOp {
            op: op.to_string(),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    fn cond(cond: Expr, then: Expr, r#else: Expr) -> Expr {
        Expr::Cond {
            cond: Box::new(cond),
            then: Box::new(then),
            r#else: Box::new(r#else),
        }
    }
    fn logical_or(lhs: Expr, rhs: Expr) -> Expr {
        Expr::LogicalOr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    fn logical_and(lhs: Expr, rhs: Expr) -> Expr {
        Expr::LogicalAnd {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Literal {
    Integer(i64),
    Character(u8),
}

pub fn parse(source: &str) -> Result<ASTree, nom::Err<nom::error::Error<Span>>> {
    let source = Span::new(source);
    let (source, _) = skip(source)?;
    let (_, root) = map(all_consuming(expr), Node::Expr)(source)?;
    Ok(ASTree { root })
}

fn expr(source: Span) -> IResult<Span, Expr> {
    expr10(source)
}

fn expr10(source: Span) -> IResult<Span, Expr> {
    let (source, init) = expr9(source)?;
    let (source, qtce) = opt(tuple((
        skipped(char('?')),
        skipped(expr),
        skipped(char(':')),
        skipped(expr10),
    )))(source)?;
    Ok((
        source,
        if let Some((_question, then, _colon, r#else)) = qtce {
            Expr::cond(init, then, r#else)
        } else {
            init
        },
    ))
}
fn expr9(source: Span) -> IResult<Span, Expr> {
    let (source, init) = expr8(source)?;
    fold_many0(
        preceded(skipped(tag("||")), skipped(expr8)),
        init,
        Expr::logical_or,
    )(source)
}
fn expr8(source: Span) -> IResult<Span, Expr> {
    let (source, init) = expr7(source)?;
    fold_many0(
        preceded(skipped(tag("&&")), skipped(expr7)),
        init,
        Expr::logical_and,
    )(source)
}
fn expr7(source: Span) -> IResult<Span, Expr> {
    let (source, init) = expr6(source)?;
    let cmp = alt((
        tag(">="),
        tag("<="),
        tag("=="),
        tag("!="),
        tag("<"),
        tag(">"),
    ));
    fold_many0(
        pair(skipped(cmp), skipped(expr6)),
        init,
        |lhs, (op, rhs)| Expr::binary_op(op.fragment(), lhs, rhs),
    )(source)
}
fn expr6(source: Span) -> IResult<Span, Expr> {
    let (source, init) = expr5(source)?;
    fold_many0(
        pair(skipped(tag("|")), skipped(expr5)),
        init,
        |lhs, (op, rhs)| Expr::binary_op(op.fragment(), lhs, rhs),
    )(source)
}
fn expr5(source: Span) -> IResult<Span, Expr> {
    let (source, init) = expr4(source)?;
    fold_many0(
        pair(skipped(tag("^")), skipped(expr4)),
        init,
        |lhs, (op, rhs)| Expr::binary_op(op.fragment(), lhs, rhs),
    )(source)
}
fn expr4(source: Span) -> IResult<Span, Expr> {
    let (source, init) = expr3(source)?;
    fold_many0(
        pair(skipped(tag("&")), skipped(expr3)),
        init,
        |lhs, (op, rhs)| Expr::binary_op(&op.to_string(), lhs, rhs),
    )(source)
}
fn expr3(source: Span) -> IResult<Span, Expr> {
    let (source, init) = expr2(source)?;
    fold_many0(
        pair(skipped(alt((tag(">>"), tag("<<")))), skipped(expr2)),
        init,
        |lhs, (op, rhs)| Expr::binary_op(&op.to_string(), lhs, rhs),
    )(source)
}
fn expr2(source: Span) -> IResult<Span, Expr> {
    let (source, init) = expr1(source)?;
    fold_many0(
        pair(skipped(one_of("+-")), skipped(expr1)),
        init,
        |lhs, (op, rhs)| Expr::binary_op(&op.to_string(), lhs, rhs),
    )(source)
}
fn expr1(source: Span) -> IResult<Span, Expr> {
    let (source, init) = term(source)?;
    fold_many0(
        pair(skipped(one_of("*/%")), skipped(term)),
        init,
        |lhs, (op, rhs)| Expr::binary_op(&op.to_string(), lhs, rhs),
    )(source)
}
fn term(source: Span) -> IResult<Span, Expr> {
    primary(source)
}
fn primary(source: Span) -> IResult<Span, Expr> {
    alt((
        map(identifier, Expr::Identifier),
        map(literal, Expr::Literal),
        delimited(char('('), skipped(expr), skipped(char(')'))),
    ))(source)
}

fn identifier(source: Span) -> IResult<Span, String> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |span: Span| span.to_string(),
    )(source)
}

fn literal(source: Span) -> IResult<Span, Literal> {
    alt((
        map(integer, Literal::Integer),
        map(character, Literal::Character),
    ))(source)
}

fn integer(source: Span) -> IResult<Span, i64> {
    let (source, val) = digit1(source)?;
    Ok((source, val.parse().unwrap()))
}

fn character(source: Span) -> IResult<Span, u8> {
    let simple_char = map(satisfy(|c| c != '\\' && c.is_ascii()), |ch| ch as u8);
    let escaped_char = preceded(
        char('\\'),
        alt((
            value(b'\'', char('\'')),
            value(b'\\', char('\\')),
            value(b'\n', char('n')),
            value(b'\r', char('r')),
        )),
    );
    let code_char = preceded(
        char('\\'),
        map_res(take_while_m_n(3, 3, |c: char| c.is_digit(8)), |s: Span| {
            u8::from_str_radix(s.fragment(), 8)
        }),
    );
    delimited(
        char('\''),
        alt((simple_char, escaped_char, code_char)),
        char('\''),
    )(source)
}

fn skipped<'a, O, E: ParseError<Span<'a>>>(
    f: impl Parser<Span<'a>, O, E>,
) -> impl FnMut(Span<'a>) -> IResult<Span, O, E> {
    preceded(skip, f)
}

fn skip<'a, E: ParseError<Span<'a>>>(source: Span<'a>) -> IResult<Span, Span, E> {
    use nom::InputTakeAtPosition as _;
    source.split_at_position_complete(|item| !item.is_ascii_whitespace())
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_parse {
        ($parse:ident($src:expr), $rest:expr, $ast:expr $(,)?) => {
            assert_eq!($parse(Span::new($src)).unwrap(), ($rest, $ast))
        };
        ($parse:ident($src:expr), $ast:expr $(,)?) => {
            assert_eq!($parse(Span::new($src)).unwrap(), ("", $ast))
        };
    }

    #[test]
    fn test_expr10() {
        assert_parse!(expr10("4 ?\t 2 :\n  0"), Expr::cond(int(4), int(2), int(0)));
        assert_parse!(
            expr10("0 ? 1 ? 2 : 3 : 4 ? 5 : 6"),
            Expr::cond(
                int(0),
                Expr::cond(int(1), int(2), int(3)),
                Expr::cond(int(4), int(5), int(6))
            )
        );
        assert_parse!(
            expr10("0 ? 1 : 2 ? 3 : 4 ? 5 : 6"),
            Expr::cond(
                int(0),
                int(1),
                Expr::cond(int(2), int(3), Expr::cond(int(4), int(5), int(6)))
            )
        );
    }
    #[test]
    fn test_expr9() {
        assert_parse!(
            expr9("4 || 5 || 99"),
            Expr::logical_or(Expr::logical_or(int(4), int(5)), int(99))
        );
        assert_parse!(
            expr9("4 || 5 && 99"),
            Expr::logical_or(int(4), Expr::logical_and(int(5), int(99)))
        );
    }
    #[test]
    fn test_expr8() {
        assert_parse!(
            expr8("4 && 5 && 99"),
            Expr::logical_and(Expr::logical_and(int(4), int(5)), int(99))
        );
        assert_parse!(
            expr8("4 + 5 && 99"),
            Expr::logical_and(Expr::binary_op("+", int(4), int(5)), int(99))
        );
    }
    #[test]
    fn test_expr7() {
        assert_parse!(
            expr7("4 > 5 <= 99"),
            Expr::binary_op("<=", Expr::binary_op(">", int(4), int(5)), int(99))
        );
        assert_parse!(
            expr7("4 + 5 == 99"),
            Expr::binary_op("==", Expr::binary_op("+", int(4), int(5)), int(99))
        );
    }
    #[test]
    fn test_expr6() {
        assert_parse!(
            expr6("4 | 5 | 99"),
            Expr::binary_op("|", Expr::binary_op("|", int(4), int(5)), int(99))
        );
        assert_parse!(
            expr6("4 | 5 ^ 99"),
            Expr::binary_op("|", int(4), Expr::binary_op("^", int(5), int(99)))
        );
    }
    #[test]
    fn test_expr5() {
        assert_parse!(
            expr5("4 ^ 5 ^ 99"),
            Expr::binary_op("^", Expr::binary_op("^", int(4), int(5)), int(99))
        );
        assert_parse!(
            expr5("4 ^ 5 & 99"),
            Expr::binary_op("^", int(4), Expr::binary_op("&", int(5), int(99)))
        );
    }
    #[test]
    fn test_expr4() {
        assert_parse!(
            expr4("4 & 5 & 99"),
            Expr::binary_op("&", Expr::binary_op("&", int(4), int(5)), int(99))
        );
        assert_parse!(
            expr4("4 + 5 & 99"),
            Expr::binary_op("&", Expr::binary_op("+", int(4), int(5)), int(99))
        );
    }
    #[test]
    fn test_expr3() {
        assert_parse!(
            expr4("4 >> 5 << 99"),
            Expr::binary_op("<<", Expr::binary_op(">>", int(4), int(5)), int(99))
        );
        assert_parse!(
            expr4("4 << 5 - 99"),
            Expr::binary_op("<<", int(4), Expr::binary_op("-", int(5), int(99)))
        );
    }
    #[test]
    fn test_expr2() {
        assert_parse!(
            expr2("3 * 9 - 4"),
            Expr::binary_op("-", Expr::binary_op("*", int(3), int(9)), int(4)),
        );
        assert_parse!(
            expr2("4\n+\r3 *     \n\t\r 2"),
            Expr::binary_op("+", int(4), Expr::binary_op("*", int(3), int(2)))
        );
    }
    #[test]
    fn test_expr1() {
        assert_parse!(
            expr2("3 + 9 - 4"),
            Expr::binary_op("-", Expr::binary_op("+", int(3), int(9)), int(4)),
        );
        assert_parse!(
            expr2("4\n-\r3 +     \n\t\r 2"),
            Expr::binary_op("+", Expr::binary_op("-", int(4), int(3)), int(2))
        );
    }
    #[test]
    fn test_term() {
        assert_parse!(term("42"), Expr::Literal(Literal::Integer(42)));
    }

    #[test]
    fn test_character() {
        assert_parse!(character("'a'"), b'a');
        assert_parse!(character("'\\n'"), b'\n');
        assert_parse!(character("'\\060'"), b'0');
    }

    fn int(val: i64) -> Expr {
        Expr::Literal(Literal::Integer(val))
    }
}
