#![warn(clippy::all)]

use structopt::StructOpt as _;

fn main() -> anyhow::Result<()> {
    let opt = cbc::Opt::from_args();

    cbc::run(opt)
}
