use nom::character::complete as character;
use nom::IResult;

#[derive(Debug)]
pub struct ASTree {
    root: Node,
}

#[derive(Debug)]
enum Node {
    Expr(Expr),
}

#[derive(Debug)]
enum Expr {
    Literal(Literal),
}

#[derive(Debug)]
enum Literal {
    Integer(i64),
}

pub fn parse<'a>(source: &'a str) -> Result<ASTree, nom::Err<nom::error::Error<&'a str>>> {
    let (_, root) = nom::alt!(
        source,
        expr => { |expr| Node::Expr(expr) }
    )?;
    Ok(ASTree { root })
}

fn expr(source: &str) -> IResult<&str, Expr> {
    nom::alt!(
        source,
        literal => { |literal| Expr::Literal(literal) }
    )
}

fn literal(source: &str) -> IResult<&str, Literal> {
    nom::alt!(
        source,
        integer => { |integer| Literal::Integer(integer) }
    )
}

fn integer(source: &str) -> IResult<&str, i64> {
    let (source, val) = character::digit1(source)?;
    Ok((source, val.parse().unwrap()))
}
