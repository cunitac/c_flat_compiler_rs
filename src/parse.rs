use anyhow::anyhow;
use anyhow::Context as _;
use nom::character::complete as character;
use nom::IResult;
use std::path::Path;

#[derive(Debug)]
pub struct ASTree {
    root: Node,
}

#[derive(Debug)]
pub enum Node {
    Root { node: Box<Node> },
    IntegerLiteral(String),
}

pub fn parse_file(path: impl AsRef<Path>) -> anyhow::Result<ASTree> {
    let source = std::fs::read_to_string(&path)
        .with_context(|| format!("failed to read file {}", path.as_ref().display()))?;
    parse(&source).map_err(|_| anyhow!("failed to parse file {}", path.as_ref().display()))
}

fn parse(source: &str) -> Result<ASTree, nom::Err<nom::error::Error<&str>>> {
    let (_, integer) = parse_integer(source)?;

    Ok(ASTree {
        root: Node::Root {
            node: Box::new(Node::IntegerLiteral(integer)),
        },
    })
}

fn parse_integer(source: &str) -> IResult<&str, String> {
    let (source, _) = character::space0(source)?;
    let (source, integer) = character::digit1(source)?;
    Ok((source, integer.to_string()))
}
