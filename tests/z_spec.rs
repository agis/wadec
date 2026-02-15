use anyhow::{Result, bail};
use googletest::prelude::*;
use serde::Deserialize;
use std::fs;
use std::fs::File;
use std::io::{self, Write};
use std::process::Command;
use wadec::decode_module;

// Contains fixtures (.wasm and .json files) generated via
// `wasm-tools json-from-wast` from the upstream spec suite.
const FIXTURE_PATH: &str = "./tests/fixtures/spec/";

#[derive(Deserialize, Debug)]
struct TestScript {
    commands: Vec<Assertion>,
}

// A TestScript maps to a JSON file generated via a .wast (aka. test script) file.
impl TestScript {
    fn execute(self) -> Vec<Result<()>> {
        self.commands
            .into_iter()
            .filter(|c| *c != Assertion::Other)
            .map(|c| c.execute())
            .collect()
    }
}

// We only care about binary modules (since we're a binary decoder after all).
#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all(deserialize = "lowercase"))]
enum ModuleType {
    Binary,
    Text,
}

#[derive(Deserialize, Debug)]
#[serde(tag = "type")]
#[serde(rename_all(deserialize = "snake_case"))]
#[derive(PartialEq)]
enum Assertion {
    // A valid module is one that both its Decoding and Validation phases
    // succeed (we only care about the former).
    #[serde(alias = "module")]
    AssertValid {
        line: u64,
        filename: String,
        module_type: ModuleType,
    },

    // 'malformed' means the module is not well-formed according to the spec
    // (Binary format), so the Decoding phase itself must fail.
    AssertMalformed {
        line: u64,
        filename: String,
        text: String,
        module_type: ModuleType,
    },

    // 'invalid' means the Validation phase rejected the module, which however
    // implies that Decoding phase (which comes before the Validation phase)
    // succeeded.
    AssertInvalid {
        line: u64,
        filename: String,
        text: String,
        module_type: ModuleType,
    },

    // 'unlinkable' means the module failed to be instatiated/linked, which
    // however implies that both Decoding and Validation phases succeeded.
    AssertUnlinkable {
        line: u64,
        filename: String,
        text: String,
        module_type: ModuleType,
    },

    #[serde(other)]
    Other,
}

impl Assertion {
    fn is_binary(&self) -> bool {
        matches!(
            self,
            Assertion::AssertValid {
                module_type: ModuleType::Binary,
                ..
            } | Assertion::AssertInvalid {
                module_type: ModuleType::Binary,
                ..
            } | Assertion::AssertMalformed {
                module_type: ModuleType::Binary,
                ..
            } | Assertion::AssertUnlinkable {
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
            Self::AssertValid { filename, .. }
            | Self::AssertInvalid { filename, .. }
            | Self::AssertUnlinkable { filename, .. } => {
                match decode_module(File::open(resolve_fixture(filename)).expect(filename)) {
                    Ok(_) => Ok(()),
                    Err(e) => bail!("expected {filename} to be considered well-formed; got: {e}"),
                }
            }
            Self::AssertMalformed { filename, .. } => {
                if decode_module(File::open(resolve_fixture(filename)).expect(filename)).is_ok() {
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
fn upstream_spec_tests() {
    setup();

    for path in files_with_ext(FIXTURE_PATH, ".json") {
        println!("Executing {:?}", path.file_name().unwrap());
        let f = File::open(path).unwrap();
        let test_script: TestScript = serde_json::from_reader(f).unwrap();
        for result in test_script.execute() {
            expect_that!(result, ok(anything()));
        }
    }
}

fn setup() {
    const TEST_SCRIPTS_PATHS: [&str; 2] = ["spec/test/core/", "spec/test/core/simd/"];

    // clean any leftovers from previous runs; we're going to re-generate them
    // anyway
    Command::new("git")
        .current_dir(FIXTURE_PATH)
        .arg("clean")
        .arg("-fdx")
        .output()
        .unwrap();

    TEST_SCRIPTS_PATHS.iter().for_each(|path| {
        for path in files_with_ext(path, ".wast") {
            let output = Command::new("wasm-tools")
                .current_dir(FIXTURE_PATH)
                .arg("json-from-wast")
                .arg("--output")
                .arg(format!(
                    "{}.json",
                    path.file_name().unwrap().to_str().unwrap()
                ))
                .arg("../../../".to_owned() + path.to_str().unwrap())
                .output()
                .unwrap();

            if !output.status.success() {
                io::stderr().write_all(&output.stderr).unwrap();
            }
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
