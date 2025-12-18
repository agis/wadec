# wadec [![Latest Version]][crates.io]

[Latest Version]: https://img.shields.io/crates/v/wadec.svg
[crates.io]: https://crates.io/crates/wadec

wadec is a decoder for WebAssembly modules, focusing on developer experience,
informative errors and other helpful diagnostics.

It can be used as a library or a command-line tool.

## Status

This project is currently in early development stage of development. It passes
100% of the official suite for [version
2](https://webassembly.github.io/spec/versions/core/WebAssembly-2.0.pdf) of the
specification. (Support for version 3 is planned.)

The API is still unstable and subject to change.

## Rationale

wadec aims to serve as an educational tool for those looking to understand the
Wasm binary format, or for developers debugging Wasm modules.

wadec does _not_ aim to be a highly-efficient decoder for
performance-critical tasks. For such use-cases, consider using a streaming
decoder.

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
    1: failed decoding Parameters
    2: failed parsing vector element at position 0
    3: invalid ValType marker byte - expected one of 0x7F (Num(Int32)), 0x7E (Num(Int64)), 0x7D (Num(Float32)), 0x7C (Num(Float64)), 0x7B (Vec(V128)), 0x70 (Ref(Func)), 0x6F (Ref(Extern)); got 0xAA

DEBUG OUTPUT:
DecodeTypeSection(
    DecodeVector(
        ParseElement {
            position: 0,
            source: DecodeFuncTypeError::DecodeParameterTypes(
                DecodeVector(
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

- [ ] Support for [spec version 3](https://webassembly.github.io/spec/core/)
- [ ] Support for performing the [Validation phase](https://webassembly.github.io/spec/core/valid/index.html)
- [ ] Optional support for WebAssembly extensions

## License

wadec is licensed under the [Apache 2.0 license](LICENSE).
