mod ast;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while_m_n};
use nom::character::complete::{alpha1, alphanumeric1, char, digit1, one_of, satisfy};
use nom::combinator::{all_consuming, map, map_res, opt, recognize, value};
use nom::error::ParseError;
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::{IResult, Parser};

use ast::{ASTree, Expr, Literal, Node};

type Span<'a> = nom_locate::LocatedSpan<&'a str>;

pub fn parse(source: &str) -> Result<ASTree, nom::Err<nom::error::Error<Span>>> {
    let source = Span::new(source);
    let (source, _) = skip(source)?;
    let (_, root) = map(all_consuming(expr), Node::Expr)(source)?;
    Ok(ASTree::new(root))
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
mod tests;
