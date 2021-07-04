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

pub fn parse(src: &'static str) -> anyhow::Result<ASTree> {
    let src = Span::new(src);
    let (_, ast) = all_consuming(ast)(src)?;
    Ok(ast)
}

fn ast(src: Span) -> IResult<ASTree> {
    let (src, position) = position(src)?;
    let (src, declarations) = declarations(src)?;
    let (src, _skip) = skip(src)?;
    Ok((
        src,
        ASTree {
            position,
            declarations,
        },
    ))
}

fn declarations(src: Span) -> IResult<Declarations> {
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
    let (src, _declarations) = many0(declaration)(src)?;
    Ok((src, ret))
}

fn defun(src: Span) -> IResult<DefinedFunction> {
    let (src, ret_type) = type_ref(src)?;
    let (src, name) = s(identifier)(src)?;
    let params = separated_list0(s(tag(",")), s(param));
    let (src, params) = paren(params)(src)?;
    let (src, body) = s(block)(src)?;
    Ok((
        src,
        DefinedFunction {
            ret_type,
            name: Identifier(name),
            params,
            body,
        },
    ))
}

fn defvar(src: Span) -> IResult<Vec<DefinedVariable>> {
    let (src, r#type) = type_ref(src)?;
    let (src, vars) = separated_list1(s(tag(",")), s(defvar_type(r#type)))(src)?;
    let (src, _semicolon) = s(tag(";"))(src)?;
    Ok((src, vars))
}

fn defvar_type<'a>(r#type: TypeRef<'a>) -> impl FnMut(Span<'a>) -> IResult<DefinedVariable<'a>> {
    move |src| {
        let (src, name) = identifier(src)?;
        Ok((
            src,
            DefinedVariable {
                r#type,
                name: Identifier(name),
            },
        ))
    }
}

fn defvars(src: Span) -> IResult<Vec<DefinedVariable>> {
    let (src, defvars) = separated_list0(skip, defvar)(src)?;
    let defvars = defvars.into_iter().flatten().collect();
    Ok((src, defvars))
}

fn type_ref(src: Span) -> IResult<TypeRef> {
    let (src, name) = identifier(src)?;
    Ok((src, TypeRef { name }))
}

fn param(src: Span) -> IResult<Param> {
    let (src, r#type) = identifier(src)?;
    let (src, name) = s(identifier)(src)?;
    Ok((src, Param::new(r#type, name)))
}

fn block(src: Span) -> IResult<Block> {
    let (src, _open) = char('{')(src)?;
    let (src, defvars) = s(defvars)(src)?;
    let (src, stmts) = many0(s(stmt))(src)?;
    let (src, _close) = s(char('}'))(src)?;
    Ok((src, Block { defvars, stmts }))
}

fn stmt(src: Span) -> IResult<Stmt> {
    alt((expr_stmt, map(block, Stmt::Block), r#if, r#while, r#return))(src)
}

fn r#if(src: Span) -> IResult<Stmt> {
    let (src, _if) = tag("if")(src)?;
    let (src, cond) = s(paren(expr))(src)?;
    let (src, then) = s(stmt)(src)?;
    let (src, r#else) = opt(preceded(s(tag("else")), s(stmt)))(src)?;
    Ok((src, Stmt::r#if(cond, then, r#else)))
}

fn r#while(src: Span) -> IResult<Stmt> {
    let (src, _while) = tag("while")(src)?;
    let (src, cond) = s(paren(expr))(src)?;
    let (src, body) = s(stmt)(src)?;
    Ok((src, Stmt::r#while(cond, body)))
}

fn r#return(src: Span) -> IResult<Stmt> {
    let (src, _return) = tag("return")(src)?;
    let (src, expr) = s(expr)(src)?;
    let (src, _semicolon) = s(char(';'))(src)?;
    Ok((src, Stmt::Return(expr)))
}

fn expr_stmt(src: Span) -> IResult<Stmt> {
    let (src, expr) = expr(src)?;
    let (src, _) = s(tag(";"))(src)?;
    Ok((src, Stmt::Expr(expr)))
}

fn expr(src: Span) -> IResult<Expr> {
    expr10(src)
}

fn expr10(src: Span) -> IResult<Expr> {
    let (src, init) = expr9(src)?;
    let (src, qtce) = opt(tuple((s(char('?')), s(expr), s(char(':')), s(expr10))))(src)?;
    Ok((
        src,
        if let Some((_question, then, _colon, r#else)) = qtce {
            Expr::cond(init, then, r#else)
        } else {
            init
        },
    ))
}

fn expr9(src: Span) -> IResult<Expr> {
    let (src, init) = expr8(src)?;
    fold_many0(preceded(s(tag("||")), s(expr8)), init, Expr::logical_or)(src)
}

fn expr8(src: Span) -> IResult<Expr> {
    let (src, init) = expr7(src)?;
    fold_many0(preceded(s(tag("&&")), s(expr7)), init, Expr::logical_and)(src)
}

fn expr7(src: Span) -> IResult<Expr> {
    let (src, init) = expr6(src)?;
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
    })(src)
}

fn expr6(src: Span) -> IResult<Expr> {
    let (src, init) = expr5(src)?;
    fold_many0(pair(s(tag("|")), s(expr5)), init, |lhs, (op, rhs)| {
        Expr::binary_op(op.fragment(), lhs, rhs)
    })(src)
}

fn expr5(src: Span) -> IResult<Expr> {
    let (src, init) = expr4(src)?;
    fold_many0(pair(s(tag("^")), s(expr4)), init, |lhs, (op, rhs)| {
        Expr::binary_op(op.fragment(), lhs, rhs)
    })(src)
}

fn expr4(src: Span) -> IResult<Expr> {
    let (src, init) = expr3(src)?;
    fold_many0(pair(s(tag("&")), s(expr3)), init, |lhs, (op, rhs)| {
        Expr::binary_op(&op.to_string(), lhs, rhs)
    })(src)
}

fn expr3(src: Span) -> IResult<Expr> {
    let (src, init) = expr2(src)?;
    fold_many0(
        pair(s(alt((tag(">>"), tag("<<")))), s(expr2)),
        init,
        |lhs, (op, rhs)| Expr::binary_op(&op.to_string(), lhs, rhs),
    )(src)
}

fn expr2(src: Span) -> IResult<Expr> {
    let (src, init) = expr1(src)?;
    fold_many0(pair(s(one_of("+-")), s(expr1)), init, |lhs, (op, rhs)| {
        Expr::binary_op(&op.to_string(), lhs, rhs)
    })(src)
}

fn expr1(src: Span) -> IResult<Expr> {
    let (src, init) = term(src)?;
    fold_many0(pair(s(one_of("*/%")), s(term)), init, |lhs, (op, rhs)| {
        Expr::binary_op(&op.to_string(), lhs, rhs)
    })(src)
}

fn term(src: Span) -> IResult<Expr> {
    primary(src)
}

fn primary(src: Span) -> IResult<Expr> {
    alt((
        map(identifier, Expr::identifier),
        map(literal, Expr::Literal),
        paren(expr),
    ))(src)
}

fn identifier(src: Span) -> IResult<Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(src)
}

fn literal(src: Span) -> IResult<Literal> {
    alt((integer, character))(src)
}

fn integer(src: Span) -> IResult<Literal> {
    let (src, position) = position(src)?;
    let (src, val) = digit1(src)?;
    let val = val.parse().unwrap();
    Ok((src, Literal::Integer { position, val }))
}

fn character(src: Span) -> IResult<Literal> {
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

    let (src, position) = position(src)?;
    let (src, val) = delimited(char('\''), character, char('\''))(src)?;
    Ok((src, Literal::Character { position, val }))
}

/// <SPACES> <OUTPUT>
fn s<'a, O>(
    f: impl Parser<Span<'a>, O, nom::error::Error<Span<'a>>>,
) -> impl FnMut(Span<'a>) -> IResult<O> {
    preceded(skip, f)
}

fn skip(src: Span) -> IResult<Span> {
    use nom::InputTakeAtPosition as _;
    src.split_at_position_complete(|item| !item.is_ascii_whitespace())
}

/// "(" <SPACES> <OUTPUT> <SPACES> ")"
fn paren<'a, O>(
    f: impl Parser<Span<'a>, O, nom::error::Error<Span<'a>>>,
) -> impl FnMut(Span<'a>) -> IResult<O> {
    delimited(tag("("), s(f), s(tag(")")))
}

#[cfg(test)]
mod tests;
