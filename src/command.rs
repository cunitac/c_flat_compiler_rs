pub mod ast;

use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub enum Opt {
    /// Show AST (Abstruct Syntax Tree)
    Ast(ast::Opt),
}
