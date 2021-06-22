mod ast;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while_m_n};
use nom::character::complete::{alpha1, alphanumeric1, char, digit1, one_of, satisfy};
use nom::combinator::{all_consuming, map, map_res, opt, recognize, value};
use nom::multi::{fold_many0, many0, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::Parser;
use nom_locate::position;

type IResult<'a, O> = nom::IResult<Span<'a>, O>;

use ast::{
    ASTree, Block, Declarations, DefinedFunction, DefinedVariable, Expr, Identifier, Literal,
    Param, Stmt, TypeRef,
};

type Span<'a> = nom_locate::LocatedSpan<&'a str>;

pub fn parse(source: &'static str) -> anyhow::Result<ASTree> {
    let source = Span::new(source);
    let (_, ast) = all_consuming(ast)(source)?;
    Ok(ast)
}

fn ast(source: Span) -> IResult<ASTree> {
    let (source, position) = position(source)?;
    let (source, declarations) = declarations(source)?;
    let (source, _skip) = skip(source)?;
    Ok((
        source,
        ASTree {
            position,
            declarations,
        },
    ))
}

fn declarations(source: Span) -> IResult<Declarations> {
    let mut ret = Declarations::default();
    enum Declaration<'a> {
        Defun(DefinedFunction<'a>),
        Defvars(Vec<DefinedVariable<'a>>),
    }
    use Declaration::*;
    let declaration = alt((map(defun, Defun), map(defvar, Defvars)));
    let declaration = map(s(declaration), |decl| match decl {
        Defun(defun) => ret.add_defun(defun),
        Defvars(defvars) => ret.add_defvars(defvars),
    });
    let (source, _declarations) = many0(declaration)(source)?;
    Ok((source, ret))
}

fn defun(source: Span) -> IResult<DefinedFunction> {
    let (source, ret_type) = type_ref(source)?;
    let (source, name) = s(identifier)(source)?;
    let params = separated_list0(s(tag(",")), s(param));
    let (source, params) = paren(params)(source)?;
    let (source, body) = s(block)(source)?;
    Ok((
        source,
        DefinedFunction {
            ret_type,
            name: Identifier(name),
            params,
            body,
        },
    ))
}

fn defvar(source: Span) -> IResult<Vec<DefinedVariable>> {
    let (source, r#type) = type_ref(source)?;
    let (source, vars) = separated_list1(s(tag(",")), s(defvar_type(r#type)))(source)?;
    let (source, _semicolon) = s(tag(";"))(source)?;
    Ok((source, vars))
}

fn defvar_type<'a>(r#type: TypeRef<'a>) -> impl FnMut(Span<'a>) -> IResult<DefinedVariable<'a>> {
    move |source| {
        let (source, name) = identifier(source)?;
        Ok((
            source,
            DefinedVariable {
                r#type,
                name: Identifier(name),
            },
        ))
    }
}

fn defvars(source: Span) -> IResult<Vec<DefinedVariable>> {
    let (source, defvars) = separated_list0(skip, defvar)(source)?;
    let defvars = defvars.into_iter().flatten().collect();
    Ok((source, defvars))
}

fn type_ref(source: Span) -> IResult<TypeRef> {
    let (source, name) = identifier(source)?;
    Ok((source, TypeRef { name }))
}

fn param(source: Span) -> IResult<Param> {
    let (source, r#type) = identifier(source)?;
    let (source, name) = s(identifier)(source)?;
    Ok((source, Param::new(r#type, name)))
}

fn block(source: Span) -> IResult<Block> {
    let (source, _open) = char('{')(source)?;
    let (source, defvars) = s(defvars)(source)?;
    let (source, stmts) = many0(s(stmt))(source)?;
    let (source, _close) = s(char('}'))(source)?;
    Ok((source, Block { defvars, stmts }))
}

fn stmt(source: Span) -> IResult<Stmt> {
    alt((expr_stmt, map(block, Stmt::Block), r#if))(source)
}

fn r#if(source: Span) -> IResult<Stmt> {
    let (source, _if) = tag("if")(source)?;
    let (source, cond) = paren(expr)(source)?;
    let (source, then) = s(stmt)(source)?;
    let (source, r#else) = opt(preceded(s(tag("else")), s(stmt)))(source)?;
    Ok((source, Stmt::r#if(cond, then, r#else)))
}

fn expr_stmt(source: Span) -> IResult<Stmt> {
    let (source, expr) = expr(source)?;
    let (source, _) = s(tag(";"))(source)?;
    Ok((source, Stmt::Expr(expr)))
}

fn expr(source: Span) -> IResult<Expr> {
    expr10(source)
}

fn expr10(source: Span) -> IResult<Expr> {
    let (source, init) = expr9(source)?;
    let (source, qtce) = opt(tuple((s(char('?')), s(expr), s(char(':')), s(expr10))))(source)?;
    Ok((
        source,
        if let Some((_question, then, _colon, r#else)) = qtce {
            Expr::cond(init, then, r#else)
        } else {
            init
        },
    ))
}

fn expr9(source: Span) -> IResult<Expr> {
    let (source, init) = expr8(source)?;
    fold_many0(preceded(s(tag("||")), s(expr8)), init, Expr::logical_or)(source)
}

fn expr8(source: Span) -> IResult<Expr> {
    let (source, init) = expr7(source)?;
    fold_many0(preceded(s(tag("&&")), s(expr7)), init, Expr::logical_and)(source)
}

fn expr7(source: Span) -> IResult<Expr> {
    let (source, init) = expr6(source)?;
    let cmp = alt((
        tag(">="),
        tag("<="),
        tag("=="),
        tag("!="),
        tag("<"),
        tag(">"),
    ));
    fold_many0(pair(s(cmp), s(expr6)), init, |lhs, (op, rhs)| {
        Expr::binary_op(op.fragment(), lhs, rhs)
    })(source)
}

fn expr6(source: Span) -> IResult<Expr> {
    let (source, init) = expr5(source)?;
    fold_many0(pair(s(tag("|")), s(expr5)), init, |lhs, (op, rhs)| {
        Expr::binary_op(op.fragment(), lhs, rhs)
    })(source)
}

fn expr5(source: Span) -> IResult<Expr> {
    let (source, init) = expr4(source)?;
    fold_many0(pair(s(tag("^")), s(expr4)), init, |lhs, (op, rhs)| {
        Expr::binary_op(op.fragment(), lhs, rhs)
    })(source)
}

fn expr4(source: Span) -> IResult<Expr> {
    let (source, init) = expr3(source)?;
    fold_many0(pair(s(tag("&")), s(expr3)), init, |lhs, (op, rhs)| {
        Expr::binary_op(&op.to_string(), lhs, rhs)
    })(source)
}

fn expr3(source: Span) -> IResult<Expr> {
    let (source, init) = expr2(source)?;
    fold_many0(
        pair(s(alt((tag(">>"), tag("<<")))), s(expr2)),
        init,
        |lhs, (op, rhs)| Expr::binary_op(&op.to_string(), lhs, rhs),
    )(source)
}

fn expr2(source: Span) -> IResult<Expr> {
    let (source, init) = expr1(source)?;
    fold_many0(pair(s(one_of("+-")), s(expr1)), init, |lhs, (op, rhs)| {
        Expr::binary_op(&op.to_string(), lhs, rhs)
    })(source)
}

fn expr1(source: Span) -> IResult<Expr> {
    let (source, init) = term(source)?;
    fold_many0(pair(s(one_of("*/%")), s(term)), init, |lhs, (op, rhs)| {
        Expr::binary_op(&op.to_string(), lhs, rhs)
    })(source)
}

fn term(source: Span) -> IResult<Expr> {
    primary(source)
}

fn primary(source: Span) -> IResult<Expr> {
    alt((
        map(identifier, Expr::identifier),
        map(literal, Expr::Literal),
        paren(expr),
    ))(source)
}

fn identifier(source: Span) -> IResult<Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(source)
}

fn literal(source: Span) -> IResult<Literal> {
    alt((integer, character))(source)
}

fn integer(source: Span) -> IResult<Literal> {
    let (source, position) = position(source)?;
    let (source, val) = digit1(source)?;
    let val = val.parse().unwrap();
    Ok((source, Literal::Integer { position, val }))
}

fn character(source: Span) -> IResult<Literal> {
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
    let character = alt((simple_char, escaped_char, code_char));

    let (source, position) = position(source)?;
    let (source, val) = delimited(char('\''), character, char('\''))(source)?;
    Ok((source, Literal::Character { position, val }))
}

/// <SPACES> <OUTPUT>
fn s<'a, O>(
    f: impl Parser<Span<'a>, O, nom::error::Error<Span<'a>>>,
) -> impl FnMut(Span<'a>) -> IResult<O> {
    preceded(skip, f)
}

fn skip(source: Span) -> IResult<Span> {
    use nom::InputTakeAtPosition as _;
    source.split_at_position_complete(|item| !item.is_ascii_whitespace())
}

/// "(" <SPACES> <OUTPUT> <SPACES> ")"
fn paren<'a, O>(
    f: impl Parser<Span<'a>, O, nom::error::Error<Span<'a>>>,
) -> impl FnMut(Span<'a>) -> IResult<O> {
    delimited(tag("("), s(f), s(tag(")")))
}

#[cfg(test)]
mod tests;
