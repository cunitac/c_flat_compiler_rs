use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub struct Opt {
    source: String,
}

pub fn run(opt: Opt) -> anyhow::Result<()> {
    let ast = crate::parse::parse_file(&opt.source)?;

    println!("{:#?}", ast);

    Ok(())
}
