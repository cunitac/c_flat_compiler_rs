#![warn(clippy::all)]

use structopt::StructOpt as _;

fn main() {
    let opt = cbc::Opt::from_args();

    cbc::run(opt)
}
