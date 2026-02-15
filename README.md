# wadec [![Latest Version]][crates.io] [![Documentation](https://docs.rs/wadec/badge.svg)](https://docs.rs/wadec) ![License](https://img.shields.io/crates/l/wadec.svg)

[Latest Version]: https://img.shields.io/crates/v/wadec.svg
[crates.io]: https://crates.io/crates/wadec

wadec is a decoder for WebAssembly modules, focusing on developer experience,
informative errors and helpful diagnostics.

It can be used as a library or a command-line tool and is [fully conforming to the specification](#specification-conformance-testing).

## Status

This project is currently in early development stage of development. The API is
unstable and subject to change.

## Rationale

wadec aims to serve as an educational tool for those looking to understand the
Wasm binary format, or for developers debugging Wasm modules.

wadec does _not_ aim to be a highly-efficient decoder for
performance-critical tasks. For such use-cases, consider using a streaming
decoder.


## Specification conformance

wadec is 100% conforming to the official WebAssembly specification, [version
3.0](https://www.w3.org/TR/wasm-core-2/).

This is ensured by running the official specification [test
suite](https://github.com/WebAssembly/spec/tree/main/test/core)
against our decoder. Specifically, we go through all the test scripts (`.wast`)
and ensure that:

✔️ all modules marked with `assert_malformed` are *rejected* by our decoder

✔️ all modules marked with `assert_invalid` are *accepted* (note: that 'invalid' modules are those rejected during the Validation phase, which implies that the Decoding phase has succeeded first.)

✔️ all modules marked with `module` are *accepted*

✔️ all modules marked with `assert_unlinkable` are *accepted*

The relevant tests can be found at [tests/z_spec.rs](tests/z_spec.rs).

## Installation

You can install the CLI using [cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html):

```shell
$ cargo install wadec-cli
```

Note: We do not distribute binaries yet, but we may do so in the future.

To use the library, similarly:

```shell
$ cargo add wadec
```

## Usage

```bash
# a module that encodes an invalid Value Type marker byte (0xAA) in its
# Type section
$ hexdump -C type_section_invalid_valtype_marker.wasm
00000000  00 61 73 6d 01 00 00 00  01 05 01 60 01 aa 00     |.asm.......`...|
0000000f

$ wadec --verbose type_section_invalid_valtype_marker.wasm
ERROR: failed decoding Type section

Caused by:
    0: failed parsing vector element at position 0
    1: failed decoding Function type parameters
    2: failed parsing vector element at position 0
    3: invalid ValType marker byte - expected one of 0x7F (Num(Int32)), 0x7E (Num(Int64)), 0x7D (Num(Float32)), 0x7C (Num(Float64)), 0x7B (Vec(V128)), 0x70 (Ref(Func)), 0x6F (Ref(Extern)); got 0xAA

DEBUG OUTPUT:
DecodeTypeSection(
    DecodeList(
        ParseElement {
            position: 0,
            source: DecodeCompTypeError::DecodeFuncParameters(
                DecodeList(
                    ParseElement {
                        position: 0,
                        source: DecodeValTypeError::InvalidMarkerByte(
                            InvalidValTypeMarkerError(
                                0xAA,
                            ),
                        ),
                    },
                ),
            ),
        },
    ),
)
```


## Roadmap

- [x] Implement the Decoding phase for specification [version 2](https://www.w3.org/TR/wasm-core-2/)
- [x] Implement the Decoding phase for specification [version 3](https://webassembly.github.io/spec/core/)
- [ ] Implement the [Validation phase](https://webassembly.github.io/spec/core/valid/index.html)
- [ ] Optional support for WebAssembly extensions

## License

wadec is licensed under the [Apache 2.0 license](LICENSE).
