use anyhow::{bail, Result};
use googletest::prelude::*;
use serde::Deserialize;
use std::fs;
use std::fs::File;
use std::io::{self, Write};
use std::process::Command;
use wadec::*;

// .wasm and .json fixtures generated via wast2json
const FIXTURE_PATH: &str = "./tests/fixtures/spec/";

#[derive(Deserialize, Debug)]
struct TestScript {
    commands: Vec<Assertion>,
}

impl TestScript {
    fn execute(self) -> Vec<Result<()>> {
        self.commands
            .into_iter()
            .filter(|c| *c != Assertion::Other)
            .map(|c| c.execute())
            .collect()
    }
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all(deserialize = "lowercase"))]
enum ModuleType {
    Binary,
    Text,
}

// TODO: also map for errors
#[derive(Deserialize, Debug)]
#[serde(tag = "type")]
#[serde(rename_all(deserialize = "snake_case"))]
#[derive(PartialEq)]
enum Assertion {
    #[serde(alias = "module")]
    AssertValid { line: u64, filename: String },
    AssertMalformed {
        line: u64,
        filename: String,
        text: String,
        module_type: ModuleType,
    },
    #[serde(other)]
    Other,
}

impl Assertion {
    const EXCLUDED: [&str; 5] = [
        // these tests are bogus in the upstream v2.0 branch
        // (see https://github.com/WebAssembly/spec/issues/2012).
        //
        // TODO: re-enable them when we migrate to the v3.0, since they're fixed
        // in that one
        "align.109.wasm",
        "align.110.wasm",
        "align.111.wasm",
        "align.112.wasm",
        "align.113.wasm",
    ];

    fn is_binary(&self) -> bool {
        matches!(
            self,
            Assertion::AssertValid { .. }
                | Assertion::AssertMalformed {
                    module_type: ModuleType::Binary,
                    ..
                }
        )
    }

    fn execute(&self) -> Result<()> {
        if !self.is_binary() {
            return Ok(());
        }

        match self {
            Self::AssertValid { filename, .. } => {
                if Self::EXCLUDED.contains(&filename.as_str()) {
                    return Ok(());
                }
                match decode(File::open(resolve_fixture(filename)).expect(filename)) {
                    Ok(_) => Ok(()),
                    Err(e) => bail!("expected {filename} to be valid; got: {e}"),
                }
            }
            Self::AssertMalformed { filename, .. } => {
                if Self::EXCLUDED.contains(&filename.as_str()) {
                    return Ok(());
                }
                if decode(File::open(resolve_fixture(filename)).expect(filename)).is_ok() {
                    bail!("expected {} to be malformed", filename);
                }
                Ok(())
            }
            _ => unreachable!(),
        }
    }
}

fn resolve_fixture(filename: &str) -> String {
    FIXTURE_PATH.to_owned() + filename
}

#[gtest]
fn it_passes_upstream_spec_tests() {
    setup();

    for fname in files_with_ext(FIXTURE_PATH, ".json") {
        let f = File::open(fname).unwrap();
        let test_script: TestScript = serde_json::from_reader(f).unwrap();
        for result in test_script.execute() {
            expect_that!(result, ok(anything()));
        }
    }
}

fn setup() {
    const TEST_SCRIPTS_PATHS: [&str; 2] = ["spec/test/core/", "spec/test/core/simd/"];

    TEST_SCRIPTS_PATHS.iter().for_each(|path| {
        for fname in files_with_ext(path, ".wast") {
            let output = Command::new("wast2json")
                .current_dir(FIXTURE_PATH)
                .arg("../../../".to_owned() + fname.to_str().unwrap())
                .output()
                .unwrap();

            if !output.status.success() {
                io::stderr().write_all(&output.stderr).unwrap();
            }
            io::stdout().write_all(&output.stdout).unwrap();
        }
    })
}

fn files_with_ext(path: &str, ext: &str) -> impl Iterator<Item = std::path::PathBuf> {
    fs::read_dir(path)
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().unwrap().is_file())
        .filter_map(move |entry| {
            let filename = entry.file_name().into_string().unwrap();
            filename.ends_with(ext).then_some(entry.path())
        })
}
