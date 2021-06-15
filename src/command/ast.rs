use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub struct Opt {
    source: String,
}

pub fn run(opt: Opt) {
    let source = std::fs::read_to_string(&opt.source).unwrap();

    let ast = crate::parse::parse(Box::leak(source.into_boxed_str())).unwrap();

    println!("{:#?}", ast);
}
