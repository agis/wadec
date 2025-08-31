use std::fs::File;
use std::io::BufReader;

fn main() {
    let path = std::env::args().nth(1).expect("usage: wadec <FILE>");
    let f = BufReader::new(File::open(path).expect("failed to open file"));
    wadec::decode(f).unwrap();
}
