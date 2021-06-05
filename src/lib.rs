mod command;
mod parse;

pub use command::Opt;

pub fn run(opt: Opt) {
    use command::Opt::*;
    match opt {
        Ast(opt) => command::ast::run(opt),
    }
}
