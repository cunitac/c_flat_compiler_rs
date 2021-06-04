use anyhow::anyhow;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub struct Opt {
    source: String,
}

pub fn run(opt: Opt) -> anyhow::Result<()> {
    let source = std::fs::read_to_string(&opt.source)?;

    let ast = crate::parse::parse(&source)
        .map_err(|_| anyhow!("failed to parse file {}", &opt.source))?;

    println!("{:#?}", ast);

    Ok(())
}
