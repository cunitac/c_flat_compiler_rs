mod command;
mod parse;

pub use command::Opt;

pub fn run(opt: Opt) -> anyhow::Result<()> {
    use command::Opt::*;
    match opt {
        Ast(opt) => command::ast::run(opt),
    }
}
