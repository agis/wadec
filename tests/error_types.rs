use std::fs::File;
use wadec::core::SectionKind;
use wadec::decode::sections::DecodeTagSectionError;
use wadec::decode::sections::table::DecodeTableError;
use wadec::decode::types::{DecodeExternTypeError, DecodeTagTypeError};
use wadec::decode::instructions::CatchError;
use wadec::decode_errors::*;
use wadec::decode_module;

#[test]
fn type_idx_error_for_overlong_function_index() {
    // Sections: Type, Function.
    // Fixture: function section declares a type index encoded with overlong LEB128.
    // Spec 5.5.6 (Function Section) and 5.5.1 (Indices): typeidx is a u32; overlong LEB128 violates 5.2.2.
    // function section declares one function whose type index uses an overlong
    // unsigned LEB128 encoding, which should be rejected.
    let wasm =
        File::open("tests/fixtures/malformed/function_section_typeidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong type index should fail to decode");

    match err {
        DecodeModuleError::DecodeFunctionSection(DecodeFunctionSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeTypeIdxError(DecodeU32Error::RepresentationTooLong),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_u32_error_io_for_section_size() {
    // Sections: Type (size truncated).
    // Fixture: section header size field is truncated.
    // Spec 5.5.2 (Sections): section header includes a u32 size; the LEB128 is truncated.
    // Section header size field is truncated, so reading the u32 length hits an IO error.
    let wasm = File::open("tests/fixtures/malformed/section_header_size_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated section size should fail to decode");

    match err {
        DecodeModuleError::DecodeSectionHeader(DecodeSectionHeaderError::DecodeSectionSize(
            u32_err,
        )) => match u32_err {
            DecodeU32Error::Io(io_err) => {
                assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
            }
            other => panic!("expected Io error, got {other:?}"),
        },
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_i32_error_too_large() {
    // Sections: Type, Function, Code.
    // Fixture: single function with i32.const encoding a value outside i32 range.
    // Spec 5.2.2 (Integers) and 5.4.7 (Numeric Instructions): i32.const value is outside the s32 range.
    // i32.const immediate encodes 0x8000_0000, which is outside the i32 range.
    let wasm = File::open("tests/fixtures/malformed/i32_const_too_large.wasm").unwrap();

    let err = decode_module(wasm).expect_err("too-large i32 const should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Numeric(NumericError::DecodeI32(i32_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert!(matches!(i32_err, DecodeI32Error::TooLarge));
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_i32_error_representation_too_long() {
    // Sections: Type, Function, Code.
    // Fixture: single function with i32.const encoded using more than 5 bytes.
    // Spec 5.2.2 (Integers) and 5.4.7 (Numeric Instructions): i32.const uses signed LEB128; encoding exceeds 5 bytes.
    // i32.const immediate uses more than 5 bytes of LEB128, triggering the representation-too-long check.
    let wasm =
        File::open("tests/fixtures/malformed/i32_const_representation_too_long.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong i32 const should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Numeric(NumericError::DecodeI32(i32_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert!(matches!(i32_err, DecodeI32Error::RepresentationTooLong));
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_i32_error_io() {
    // Sections: Type, Function, Code.
    // Fixture: single function with i32.const missing its immediate bytes.
    // Spec 5.2.2 (Integers) and 5.4.7 (Numeric Instructions): i32.const immediate is truncated mid-LEB128.
    // i32.const is missing its immediate entirely.
    let wasm = File::open("tests/fixtures/malformed/i32_const_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated i32 const should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Numeric(NumericError::DecodeI32(i32_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            match i32_err {
                DecodeI32Error::Io(io_err) => {
                    assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
                }
                other => panic!("expected Io error, got {other:?}"),
            }
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_i64_error_representation_too_long() {
    // Sections: Type, Function, Code.
    // Fixture: single function with i64.const encoded using too many bytes.
    // Spec 5.2.2 (Integers) and 5.4.7 (Numeric Instructions): i64.const encoding exceeds the s64 byte limit.
    // i64.const immediate encoded with 10 continuation bytes.
    let wasm =
        File::open("tests/fixtures/malformed/i64_const_representation_too_long.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong i64 const should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Numeric(NumericError::DecodeI64(i64_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert!(matches!(i64_err, DecodeI64Error::RepresentationTooLong));
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_i64_error_incorrect_sign_extension() {
    // Sections: Type, Function, Code.
    // Fixture: single function with i64.const whose final byte has invalid sign extension.
    // Spec 5.2.2 (Integers): signed LEB128 requires correct sign extension; the final byte padding is invalid.
    // 10th byte terminates with non-zero padding for a positive number.
    let wasm =
        File::open("tests/fixtures/malformed/i64_const_incorrect_sign_extension.wasm").unwrap();

    let err = decode_module(wasm).expect_err("bad i64 sign extension should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Numeric(NumericError::DecodeI64(i64_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert!(matches!(i64_err, DecodeI64Error::IncorrectSignExtension));
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_i64_error_io() {
    // Sections: Type, Function, Code.
    // Fixture: single function with i64.const missing its immediate bytes.
    // Spec 5.2.2 (Integers) and 5.4.7 (Numeric Instructions): i64.const immediate is truncated.
    // i64.const is missing its immediate entirely.
    let wasm = File::open("tests/fixtures/malformed/i64_const_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated i64 const should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Numeric(NumericError::DecodeI64(i64_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            match i64_err {
                DecodeI64Error::Io(io_err) => {
                    assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
                }
                other => panic!("expected Io error, got {other:?}"),
            }
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_read_opcode() {
    // Sections: Type, Function, Code.
    // Fixture: single function body missing its first opcode.
    // Spec 5.5.13 (Code Section) and 5.4.9 (Expressions): function bodies are locals vec + expr; the first opcode is missing.
    // Code body declares zero locals but provides no opcode bytes.
    let wasm = File::open("tests/fixtures/malformed/code_body_missing_opcode.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing opcode should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::ReadOpcode(io_err),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_control() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a br instruction using an overlong label index.
    // Spec 5.4.1 (Control Instructions) and 5.5.1 (Indices): br expects a labelidx u32; overlong LEB128 violates 5.2.2.
    // branch instruction with overlong label index, surfacing as a Control error.
    let wasm = File::open("tests/fixtures/malformed/br_labelidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid label index should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::LabelIdx(DecodeLabelIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_reference() {
    // Sections: Type, Function, Code.
    // Fixture: single function with ref.null using an invalid heap type.
    // Spec 5.4.6 (Reference Instructions) and 5.3.3 (Heap Types): ref.null heaptype must be non-negative.
    // ref.null followed by a negative type index (s33 = -1).
    let wasm = File::open("tests/fixtures/malformed/ref_null_invalid_marker.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid heap type should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Reference(ReferenceError::HeapType(
                            DecodeHeapTypeError::NegativeTypeIndex(n),
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(n, -1);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_reference_invalid_subopcode() {
    // Sections: Type, Function, Code.
    // Fixture: single function with 0xFB-prefixed instruction using an invalid sub-opcode.
    // Spec binary instruction table: sub-opcodes 0..=30 (0x00..=0x1E) are defined; 31 is invalid.
    // 0xFB followed by sub-opcode 31.
    let wasm = File::open("tests/fixtures/malformed/ref_test_invalid_subopcode.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid sub-opcode should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::InvalidMarkerByteAfterFB(op),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(op, 31);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_control_invalid_cast_nullability_marker() {
    // Sections: Type, Function, Code.
    // Fixture: single function with br_on_cast using an invalid cast nullability marker.
    // Spec 5.4.1 (Control Instructions): cast nullability marker must be 0x00..=0x03.
    // br_on_cast followed by invalid marker byte 0x04.
    let wasm =
        File::open("tests/fixtures/malformed/br_on_cast_invalid_nullability.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid cast nullability marker should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::InvalidCastNullabilityMarker(marker)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(marker, 0x04);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_reference_ref_test_negative_heaptype() {
    // Sections: Type, Function, Code.
    // Fixture: single function with ref.test using a negative heap type index.
    // Spec 5.4.6 (Reference Instructions) and 5.3.3 (Heap Types): heaptype must be non-negative.
    // ref.test followed by a negative type index (s33 = -1).
    let wasm = File::open("tests/fixtures/malformed/ref_test_negative_heaptype.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid heap type should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Reference(ReferenceError::HeapType(
                            DecodeHeapTypeError::NegativeTypeIndex(n),
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(n, -1);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_parametric() {
    // Sections: Type, Function, Code.
    // Fixture: single function with select using an invalid value type marker.
    // Spec 5.4.3 (Parametric Instructions) and 5.3.4 (Value Types): typed select carries an invalid valtype marker.
    // select with a type list containing an invalid ValType marker.
    let wasm = File::open("tests/fixtures/malformed/select_invalid_valtype.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid valtype in select should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Parametric(ParametricError::DecodeVector(
                            DecodeListError::ParseElement {
                                position: vec_pos,
                                source:
                                    DecodeValTypeError::DecodeRefType(
                                        DecodeRefTypeError::InvalidMarkerByte(
                                            DecodeAbsHeapTypeError::InvalidMarkerByte(err),
                                        ),
                                    ),
                            },
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(vec_pos, 0);
            assert_eq!(err.0, 0xFF);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_variable() {
    // Sections: Type, Function, Code.
    // Fixture: single function with global.set missing its index immediate.
    // Spec 5.4.4 (Variable Instructions) and 5.5.1 (Indices): global.set expects a globalidx u32; the immediate is truncated.
    // global.set without a following global index.
    let wasm = File::open("tests/fixtures/malformed/global_set_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing global index should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Variable(VariableError::GlobalIdx(DecodeGlobalIdxError(
                            DecodeU32Error::Io(io_err),
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_table() {
    // Sections: Type, Function, Code.
    // Fixture: single function with table.get using an overlong table index.
    // Spec 5.4.5 (Table Instructions) and 5.5.1 (Indices): table.get uses tableidx u32; overlong LEB128 violates 5.2.2.
    // table.get with overlong table index.
    let wasm = File::open("tests/fixtures/malformed/table_get_tableidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid table index should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Table(TableError::TableIdx(DecodeTableIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_numeric() {
    // Sections: Type, Function, Code.
    // Fixture: single function with f32.const missing its 4-byte payload.
    // Spec 5.4.7 (Numeric Instructions) and 5.2.3 (Floating-Point): f32.const requires 4 bytes; payload is truncated.
    // f32.const with truncated payload.
    let wasm = File::open("tests/fixtures/malformed/f32_const_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated f32 const should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Numeric(NumericError::DecodeF32(
                            DecodeFloat32Error::ReadPayload(io_err),
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_vector() {
    // Sections: Type, Function, Code.
    // Fixture: single function with an invalid SIMD opcode.
    // Spec 5.4.8 (Vector Instructions): SIMD subopcode is not defined; invalid opcode.
    // 0xFD-prefixed instruction with an invalid sub-opcode.
    let wasm = File::open("tests/fixtures/malformed/vector_invalid_opcode.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid vector opcode should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Vector(VectorError::InvalidOpcode(op)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(op, 65535);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_invalid_opcode() {
    // Sections: Type, Function, Code.
    // Fixture: single function containing the invalid 0xFF opcode.
    // Spec 5.4 (Instructions): opcode 0xFF is not assigned to any instruction.
    // Unknown primary opcode 0xFF.
    let wasm = File::open("tests/fixtures/malformed/invalid_opcode_ff.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid opcode should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::InvalidOpcode(0xFF),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_error_invalid_marker_after_fc() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a 0xFC-prefixed opcode using an invalid subopcode.
    // Spec 5.4.7 (Numeric Instructions): 0xFC prefix requires a defined subopcode; this subopcode is invalid.
    // 0xFC prefix with an unsupported secondary opcode.
    let wasm = File::open("tests/fixtures/malformed/fc_invalid_marker.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid FC marker should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::InvalidMarkerByteAfterFC(op),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(op, 0xFF);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn control_error_decode_label_idx_vector() {
    // Sections: Type, Function, Code.
    // Fixture: single function with br_table whose label index element uses an overlong LEB128 encoding.
    // Spec 5.4.1 (Control Instructions) and 5.5.1 (Indices): labelidx is a u32; overlong LEB128 violates 5.2.2.
    // br_table with a label index encoded in more than 5 bytes.
    let wasm =
        File::open("tests/fixtures/malformed/br_table_labelidxvector_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong label index should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::DecodeLabelIdxVector(
                            DecodeListError::ParseElement {
                                position: vec_pos,
                                source: DecodeLabelIdxError(DecodeU32Error::RepresentationTooLong),
                            },
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(vec_pos, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
// # spec version: 3
fn control_error_decode_catch_invalid_marker() {
    // Sections: Type, Function, Code.
    // Fixture: try_table with a catch entry using an invalid marker byte.
    // Spec 5.4.1 (Control Instructions): catch marker must be 0x00, 0x01, 0x02, or 0x03.
    let wasm =
        File::open("tests/fixtures/malformed/try_table_invalid_catch_marker.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid catch marker should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::DecodeCatchListError(
                            DecodeListError::ParseElement {
                                position: catch_pos,
                                source: CatchError::InvalidMarkerByte(marker),
                            },
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(catch_pos, 0);
            assert_eq!(marker, 0x04);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn control_error_table_idx() {
    // Sections: Type, Function, Code.
    // Fixture: single function with call_indirect using an overlong table index.
    // Spec 5.4.1 (Control Instructions) and 5.5.1 (Indices): call_indirect tableidx is a u32; overlong LEB128 violates 5.2.2.
    // call_indirect with an overlong table index.
    let wasm = File::open("tests/fixtures/malformed/call_indirect_tableidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong table index should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::TableIdx(DecodeTableIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn control_error_type_idx() {
    // Sections: Type, Function, Code.
    // Fixture: single function with call_indirect using an overlong type index.
    // Spec 5.4.1 (Control Instructions) and 5.5.1 (Indices): call_indirect typeidx is a u32; overlong LEB128 violates 5.2.2.
    // call_indirect with an overlong type index.
    let wasm = File::open("tests/fixtures/malformed/call_indirect_typeidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong type index should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::TypeIdx(DecodeTypeIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn control_error_block_type() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a block instruction missing its block type.
    // Spec 5.4.1 (Control Instructions): block/loop/if must be followed by a blocktype byte; the immediate is missing.
    // block opcode without a following block type byte.
    let wasm = File::open("tests/fixtures/malformed/block_missing_blocktype.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing block type should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::BlockType(
                            BlockTypeError::ReadMarkerByte(io_err),
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn control_error_unexpected_else() {
    // Sections: Type, Function, Code.
    // Fixture: single function with an if construct containing an unexpected else.
    // Spec 5.4.1 (Control Instructions): else is only allowed in if; an unexpected else opcode appears.
    // if body contains an extra `else`, leading to an unexpected Else inside the else-branch parser.
    let wasm = File::open("tests/fixtures/malformed/if_unexpected_else.wasm").unwrap();

    let err = decode_module(wasm).expect_err("double else should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::UnexpectedElse),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn reference_error_func_idx() {
    // Sections: Type, Function, Code.
    // Fixture: single function with ref.func using an overlong function index.
    // Spec 5.4.2 (Reference Instructions) and 5.5.1 (Indices): ref.func uses funcidx u32; overlong LEB128 violates 5.2.2.
    // ref.func with an overlong function index.
    let wasm = File::open("tests/fixtures/malformed/ref_func_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong ref.func index should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Reference(ReferenceError::FuncIdx(DecodeFuncIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn func_idx_error_for_overlong_call_instruction() {
    // Sections: Type, Function, Code.
    // Fixture: single function with call using an overlong function index.
    // Spec 5.4.1 (Control Instructions) and 5.5.1 (Indices): call uses funcidx u32; overlong LEB128 violates 5.2.2.
    // call instruction uses a function index with an overlong unsigned LEB128 encoding.
    let wasm = File::open("tests/fixtures/malformed/function_call_funcidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong func idx should fail to decode");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::FuncIdx(DecodeFuncIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn table_idx_error_for_overlong_table_get_instruction() {
    // Sections: Type, Function, Code.
    // Fixture: single function with table.get using an overlong table index.
    // Spec 5.4.5 (Table Instructions) and 5.5.1 (Indices): table.get uses tableidx u32; overlong LEB128 violates 5.2.2.
    // table.get uses an overlong table index.
    let wasm = File::open("tests/fixtures/malformed/table_get_tableidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong table idx should fail to decode");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Table(TableError::TableIdx(DecodeTableIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn mem_idx_error_for_overlong_data_segment_memory_index() {
    // Sections: Data.
    // Fixture: data segment uses an overlong memory index for an active segment.
    // Spec 5.5.14 (Data Section) and 5.5.1 (Indices): active segment memidx is a u32; overlong LEB128 violates 5.2.2.
    // data segment with bitfield 2 carries a memory index encoded with too many bytes.
    let wasm = File::open("tests/fixtures/malformed/data_section_memidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong mem idx should fail to decode");

    match err {
        DecodeModuleError::DecodeDataSection(DecodeDataSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeDataSegmentError::DecodeMemIdx(DecodeMemIdxError(
                        DecodeU32Error::RepresentationTooLong,
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn global_idx_error_for_overlong_global_get_instruction() {
    // Sections: Type, Function, Code.
    // Fixture: single function with global.get using an overlong global index.
    // Spec 5.4.4 (Variable Instructions) and 5.5.1 (Indices): global.get uses globalidx u32; overlong LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/global_get_globalidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong global idx should fail to decode");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Variable(VariableError::GlobalIdx(DecodeGlobalIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn elem_idx_error_for_overlong_elem_drop_instruction() {
    // Sections: Type, Function, Code.
    // Fixture: single function with elem.drop using an overlong element index.
    // Spec 5.4.5 (Table Instructions) and 5.5.1 (Indices): elem.drop uses elemidx u32; overlong LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/elem_drop_elemidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong elem idx should fail to decode");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Table(TableError::ElemIdx(DecodeElemIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn data_idx_error_for_overlong_data_drop_instruction() {
    // Sections: Type, Function, Code.
    // Fixture: single function with data.drop using an overlong data index.
    // Spec 5.4.6 (Memory Instructions) and 5.5.1 (Indices): data.drop uses a dataidx u32; overlong LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/data_drop_dataidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong data idx should fail to decode");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Memory(MemoryError::DecodeDataIdx(DecodeDataIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn local_idx_error_for_overlong_local_get_instruction() {
    // Sections: Type, Function, Code.
    // Fixture: single function with local.get using an overlong local index.
    // Spec 5.4.4 (Variable Instructions) and 5.5.1 (Indices): local.get uses localidx u32; overlong LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/local_get_localidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong local idx should fail to decode");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Variable(VariableError::LocalIdx(DecodeLocalIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn label_idx_error_for_overlong_br_instruction() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a br instruction using an overlong label index.
    // Spec 5.4.1 (Control Instructions) and 5.5.1 (Indices): br expects a labelidx u32; overlong LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/br_labelidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong label idx should fail to decode");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::LabelIdx(DecodeLabelIdxError(
                            DecodeU32Error::RepresentationTooLong,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_u32_error_too_large_for_type_section_length() {
    // Sections: Type.
    // Fixture: type section length LEB128 encodes a value too large for u32.
    // Spec 5.5.4 (Type Section) and 5.1.3 (Vectors): type vector length is a u32; encoded value is too large.
    let wasm = File::open("tests/fixtures/malformed/type_section_length_too_large.wasm").unwrap();

    let err = decode_module(wasm).expect_err("too-large vector length should fail to decode");

    match err {
        DecodeModuleError::DecodeTypeSection(DecodeTypeSectionError::DecodeVector(
            DecodeListError::DecodeLength(u32_err),
        )) => {
            assert!(
                matches!(u32_err, DecodeU32Error::TooLarge),
                "expected TooLarge variant, got {u32_err:?}"
            );
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_u32_error_representation_too_long_for_section_size() {
    // Sections: Type (size overlong).
    // Fixture: section size encoded with overlong LEB128.
    // Spec 5.5.2 (Sections): section size is a u32; the LEB128 encoding is overlong (5.2.2).
    let wasm =
        File::open("tests/fixtures/malformed/section_size_representation_too_long.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong section size should fail to decode");

    match err {
        DecodeModuleError::DecodeSectionHeader(DecodeSectionHeaderError::DecodeSectionSize(
            u32_err,
        )) => assert!(
            matches!(u32_err, DecodeU32Error::RepresentationTooLong),
            "expected RepresentationTooLong variant, got {u32_err:?}"
        ),
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parametric_error_decode_vector_invalid_valtype() {
    // Sections: Type, Function, Code.
    // Fixture: single function with typed select containing an invalid value type marker.
    // Spec 5.4.3 (Parametric Instructions) and 5.1.3 (Vectors): typed select valtype vector contains an invalid marker.
    // typed select carries a type vector containing an invalid valtype marker.
    let wasm =
        File::open("tests/fixtures/malformed/select_type_vector_invalid_valtype.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid type vector in select should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Parametric(ParametricError::DecodeVector(
                            DecodeListError::ParseElement {
                                position: vec_pos,
                                source:
                                    DecodeValTypeError::DecodeRefType(
                                        DecodeRefTypeError::InvalidMarkerByte(
                                            DecodeAbsHeapTypeError::InvalidMarkerByte(err),
                                        ),
                                    ),
                            },
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(vec_pos, 0);
            assert_eq!(err.0, 0xFF);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn memory_error_decode_memarg_missing_offset() {
    // Sections: Type, Function, Code.
    // Fixture: single function with i32.load memarg missing its offset bytes.
    // Spec 5.4.6 (Memory Instructions): memarg encodes align and offset as u32; the offset is truncated.
    // i32.load with only an alignment byte present; offset decoding hits EOF.
    let wasm =
        File::open("tests/fixtures/malformed/i32_load_memarg_truncated_offset.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing memarg offset should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Memory(MemoryError::DecodeMemarg(memarg_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            match memarg_err {
                MemargError::Offset(DecodeU64Error::Io(io_err)) => {
                    assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
                }
                other => panic!("expected offset decoding failure, got {other:?}"),
            }
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn numeric_error_read_opcode_truncated_fc_prefix() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a 0xFC prefix missing the subopcode byte.
    // Spec 5.4.7 (Numeric Instructions): 0xFC prefix requires a subopcode byte; it is truncated.
    let wasm = File::open("tests/fixtures/malformed/numeric_fc_opcode_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated 0xFC opcode should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Numeric(NumericError::ReadOpcode(u32_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            match u32_err {
                DecodeU32Error::Io(io_err) => {
                    assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
                }
                other => panic!("expected Io error, got {other:?}"),
            }
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn numeric_error_decode_f32_truncated_payload() {
    // Sections: Type, Function, Code.
    // Fixture: single function with f32.const missing its 4-byte payload.
    // Spec 5.4.7 (Numeric Instructions) and 5.2.3 (Floating-Point): f32.const requires 4 bytes; payload is truncated.
    let wasm = File::open("tests/fixtures/malformed/f32_const_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated f32 const should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Numeric(NumericError::DecodeF32(f32_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            match f32_err {
                DecodeFloat32Error::ReadPayload(io_err) => {
                    assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
                }
            }
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn numeric_error_decode_f64_truncated_payload() {
    // Sections: Type, Function, Code.
    // Fixture: single function with f64.const missing its 8-byte payload.
    // Spec 5.4.7 (Numeric Instructions) and 5.2.3 (Floating-Point): f64.const requires 8 bytes; payload is truncated.
    let wasm = File::open("tests/fixtures/malformed/numeric_f64_const_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated f64 const should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Numeric(NumericError::DecodeF64(f64_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            match f64_err {
                DecodeFloat64Error::ReadPayload(io_err) => {
                    assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
                }
            }
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn vector_error_read_opcode_truncated() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a vector opcode byte truncated.
    // Spec 5.4.8 (Vector Instructions): SIMD opcode prefix/subopcode bytes are truncated.
    let wasm = File::open("tests/fixtures/malformed/vector_opcode_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated vector opcode should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Vector(VectorError::ReadOpcode(u32_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            match u32_err {
                DecodeU32Error::Io(io_err) => {
                    assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
                }
                other => panic!("expected Io error, got {other:?}"),
            }
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn vector_error_memarg_offset_missing() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a vector memory instruction missing its memarg offset.
    // Spec 5.4.8 (Vector Instructions): vector memory ops use memarg; offset is truncated.
    let wasm = File::open("tests/fixtures/malformed/vector_memarg_offset_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing memarg offset should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Vector(VectorError::Memarg(MemargError::Offset(
                            DecodeU64Error::Io(io_err),
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn vector_error_laneidx_missing() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a vector instruction missing its lane index.
    // Spec 5.4.8 (Vector Instructions): lane index immediate byte is missing.
    let wasm = File::open("tests/fixtures/malformed/vector_laneidx_missing.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing lane idx should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Vector(VectorError::LaneIdx(LaneIdxError(io_err))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn vector_error_read_immediate_bytes() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a vector instruction missing immediate bytes.
    // Spec 5.4.8 (Vector Instructions): SIMD instruction immediates are truncated.
    let wasm = File::open("tests/fixtures/malformed/vector_immediates_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated vector immediate should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Vector(VectorError::ReadImmediateBytes(io_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn vector_error_invalid_opcode() {
    // Sections: Type, Function, Code.
    // Fixture: single function with an invalid SIMD opcode.
    // Spec 5.4.8 (Vector Instructions): SIMD subopcode is not defined; invalid opcode.
    let wasm = File::open("tests/fixtures/malformed/vector_invalid_opcode.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid vector opcode should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Vector(VectorError::InvalidOpcode(op)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(op, 65535);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn memarg_error_align_missing() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a memory instruction missing its memarg alignment.
    // Spec 5.4.6 (Memory Instructions): memarg encodes align and offset as u32; align is missing.
    let wasm = File::open("tests/fixtures/malformed/memarg_align_missing.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing memarg align should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Memory(MemoryError::DecodeMemarg(MemargError::Align(
                            DecodeU32Error::Io(io_err),
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
// # spec version: 3
fn memarg_error_invalid_flags_bit() {
    // Sections: Type, Function, Memory, Code.
    // Fixture: single function with a memory instruction whose memarg flags use a reserved bit.
    // Spec 5.4.6 (Memory Instructions): memarg flags must fit within 7 bits; bit 7 is invalid.
    let wasm = File::open("tests/fixtures/malformed/memarg_invalid_flags_bit.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid memarg flags should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Memory(MemoryError::DecodeMemarg(MemargError::InvalidFlagsBit(
                            flags,
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(flags, 128);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn lane_idx_error_missing_byte() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a vector instruction missing its lane index.
    // Spec 5.4.8 (Vector Instructions): lane index immediate byte is missing.
    let wasm = File::open("tests/fixtures/malformed/vector_laneidx_missing.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing lane idx should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Vector(VectorError::LaneIdx(LaneIdxError(io_err))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn block_type_error_read_marker_byte() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a block instruction missing its block type.
    // Spec 5.4.1 (Control Instructions): block/loop/if must be followed by a blocktype byte; the immediate is missing.
    let wasm = File::open("tests/fixtures/malformed/block_missing_blocktype.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing block type should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::BlockType(
                            BlockTypeError::ReadMarkerByte(io_err),
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn block_type_error_decode_index_truncated() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a block type index truncated mid-LEB128.
    // Spec 5.4.1 (Control Instructions): blocktype uses a signed LEB128 s33; the encoding is truncated.
    let wasm = File::open("tests/fixtures/malformed/blocktype_index_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated block type index should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::BlockType(BlockTypeError::DecodeIndex(
                            DecodeS33Error::Io(io_err),
                        ))),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn block_type_error_negative_type_index() {
    // Sections: Type, Function, Code.
    // Fixture: single function with a block type using a negative type index encoding.
    // Spec 5.4.1 (Control Instructions): a blocktype type index must be a positive s33; this encoding is negative.
    let wasm = File::open("tests/fixtures/malformed/blocktype_negative_typeidx.wasm").unwrap();

    let err = decode_module(wasm).expect_err("negative block type index should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeFunctionBody(ParseExpressionError::ParseInstruction(
                        ParseError::Control(ControlError::BlockType(
                            BlockTypeError::NegativeTypeIndex(n),
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert!(n < 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_rec_type_error_read_marker_byte() {
    // Sections: Type.
    // Fixture: type section recursive type marker byte is missing.
    // Spec 5.5.4 (Type Section): rectype entry marker byte is missing.
    let wasm =
        File::open("tests/fixtures/malformed/type_section_comptype_marker_missing.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing rectype marker should fail");

    match err {
        DecodeModuleError::DecodeTypeSection(DecodeTypeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeRecTypeError::Io(io_err),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_rec_type_error_invalid_marker_byte() {
    // Sections: Type.
    // Fixture: type section composite type marker is invalid.
    // Spec 5.5.4 (Type Section) and 5.3.7 (Composite Types): composite type entries must start
    // with 0x5E/0x5F/0x60; this marker is invalid.
    let wasm =
        File::open("tests/fixtures/malformed/type_section_comptype_invalid_marker.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid composite type marker should fail");

    match err {
        DecodeModuleError::DecodeTypeSection(DecodeTypeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeRecTypeError::DecodeSubtype(DecodeSubTypeError::DecodeCompType(
                    DecodeCompTypeError::InvalidMarkerByte(b),
                )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(b, 0x00);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_rec_type_error_parameter_types_invalid_valtype() {
    // Sections: Type.
    // Fixture: type section parameter list contains an invalid value type.
    // Spec 5.5.4 (Type Section), 5.3.7 (Composite Types), and 5.3.5 (Value Types):
    // parameter valtype marker is invalid.
    let wasm =
        File::open("tests/fixtures/malformed/type_section_param_invalid_valtype.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid parameter valtype should fail");

    match err {
        DecodeModuleError::DecodeTypeSection(DecodeTypeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeRecTypeError::DecodeSubtype(DecodeSubTypeError::DecodeCompType(
                    DecodeCompTypeError::DecodeFuncParameters(DecodeResultTypeError::DecodeVector(
                        DecodeListError::ParseElement {
                            position: inner_pos,
                            source:
                                DecodeValTypeError::DecodeRefType(
                                    DecodeRefTypeError::InvalidMarkerByte(
                                        DecodeAbsHeapTypeError::InvalidMarkerByte(err),
                                    ),
                                ),
                        },
                    )),
                )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(inner_pos, 0);
            assert_eq!(err.0, 0xFF);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_rec_type_error_result_types_truncated() {
    // Sections: Type.
    // Fixture: type section result type marker is truncated.
    // Spec 5.5.4 (Type Section), 5.3.7 (Composite Types), and 5.3.5 (Value Types):
    // result valtype marker is truncated.
    let wasm =
        File::open("tests/fixtures/malformed/type_section_result_valtype_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated result valtype should fail");

    match err {
        DecodeModuleError::DecodeTypeSection(DecodeTypeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeRecTypeError::DecodeSubtype(DecodeSubTypeError::DecodeCompType(
                    DecodeCompTypeError::DecodeFuncResults(DecodeResultTypeError::DecodeVector(
                        DecodeListError::ParseElement {
                            position: inner_pos,
                            source: DecodeValTypeError::Io(io_err),
                        },
                    )),
                )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(inner_pos, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_table_error_invalid_reftype_marker() {
    // Sections: Table.
    // Fixture: table section uses an invalid reference type marker.
    // Spec 5.5.7 (Table Section) and 5.3.3 (Reference Types): table reftype marker is invalid.
    let wasm = File::open("tests/fixtures/malformed/table_section_invalid_reftype.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid table reftype should fail");

    match err {
        DecodeModuleError::DecodeTableSection(DecodeTableSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeTableError::DecodeTableType(
                    DecodeTableTypeError::DecodeRefType(DecodeRefTypeError::InvalidMarkerByte(
                        DecodeAbsHeapTypeError::InvalidMarkerByte(err),
                    )),
                ),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(err.0, 0x00);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_table_error_reftype_truncated() {
    // Sections: Table.
    // Fixture: table section reference type byte is truncated.
    // Spec 5.5.7 (Table Section): reftype byte is truncated.
    let wasm = File::open("tests/fixtures/malformed/table_section_reftype_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated table reftype should fail");

    match err {
        DecodeModuleError::DecodeTableSection(DecodeTableSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeTableError::Io(io_err),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_table_error_limits_invalid_flag() {
    // Sections: Table.
    // Fixture: table section uses an invalid limits flag.
    // Spec 5.3.7 (Limits) and 5.5.7 (Table Section): limits flag must be 0x00 or 0x01; invalid value.
    let wasm =
        File::open("tests/fixtures/malformed/table_section_limits_invalid_flag.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid table limits flag should fail");

    match err {
        DecodeModuleError::DecodeTableSection(DecodeTableSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeTableError::DecodeTableType(DecodeTableTypeError::DecodeLimits(
                    ParseLimitsError::UnexpectedFlagByte(0x02),
                )),
            },
        )) => assert_eq!(position, 0),
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_memory_type_error_missing_limits_byte() {
    // Sections: Memory.
    // Fixture: memory section is missing its limits flag byte.
    // Spec 5.3.7 (Limits) and 5.5.8 (Memory Section): limits flag byte is missing.
    let wasm = File::open("tests/fixtures/malformed/memory_section_missing_limits.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing limits should fail");

    match err {
        DecodeModuleError::DecodeMemorySection(DecodeMemorySectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeMemoryTypeError(ParseLimitsError::ReadFlagByte(io_err)),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_memory_type_error_unexpected_max_limit_byte() {
    // Sections: Memory.
    // Fixture: memory section uses an invalid limits flag.
    // Spec 5.3.7 (Limits) and 5.5.8 (Memory Section): limits flag must be 0x00 or 0x01; invalid value.
    let wasm =
        File::open("tests/fixtures/malformed/memory_section_invalid_limits_flag.wasm").unwrap();

    let err = decode_module(wasm).expect_err("unexpected limits flag should fail");

    match err {
        DecodeModuleError::DecodeMemorySection(DecodeMemorySectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeMemoryTypeError(ParseLimitsError::UnexpectedFlagByte(0x02)),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_memory_type_error_missing_min_limit() {
    // Sections: Memory.
    // Fixture: memory section limits are missing the min bound.
    // Spec 5.3.7 (Limits) and 5.5.8 (Memory Section): min bound is required; missing.
    let wasm = File::open("tests/fixtures/malformed/memory_section_missing_min.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing min limit should fail");

    match err {
        DecodeModuleError::DecodeMemorySection(DecodeMemorySectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeMemoryTypeError(ParseLimitsError::ReadMinLimit(DecodeU64Error::Io(io_err))),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_memory_type_error_missing_max_limit() {
    // Sections: Memory.
    // Fixture: memory section limits are missing the max bound.
    // Spec 5.3.7 (Limits) and 5.5.8 (Memory Section): max bound is required when flag indicates; missing.
    let wasm = File::open("tests/fixtures/malformed/memory_section_missing_max.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing max limit should fail");

    match err {
        DecodeModuleError::DecodeMemorySection(DecodeMemorySectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeMemoryTypeError(ParseLimitsError::ReadMaxLimit(DecodeU64Error::Io(io_err))),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_global_type_error_invalid_valtype() {
    // Sections: Global.
    // Fixture: global type value type marker is invalid.
    // Spec 5.3.10 (Global Types) and 5.3.4 (Value Types): global valtype marker is invalid.
    let wasm = File::open("tests/fixtures/malformed/global_section_invalid_valtype.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid global valtype should fail");

    match err {
        DecodeModuleError::DecodeGlobalSection(DecodeGlobalSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeGlobalError::DecodeGlobalType(DecodeGlobalTypeError::DecodeValueType(
                        DecodeValTypeError::DecodeRefType(DecodeRefTypeError::InvalidMarkerByte(
                            DecodeAbsHeapTypeError::InvalidMarkerByte(err),
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(err.0, 0xFF);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_global_type_error_missing_mutability() {
    // Sections: Global.
    // Fixture: global type is missing its mutability byte.
    // Spec 5.3.10 (Global Types): globaltype encodes mutability after valtype; the mutability byte is missing.
    let wasm =
        File::open("tests/fixtures/malformed/global_section_missing_mutability.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing mutability should fail");

    match err {
        DecodeModuleError::DecodeGlobalSection(DecodeGlobalSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeGlobalError::DecodeGlobalType(DecodeGlobalTypeError::DecodeMutability(io_err)),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_global_type_error_invalid_mutability() {
    // Sections: Global.
    // Fixture: global type mutability byte is invalid.
    // Spec 5.3.10 (Global Types): mutability must be 0x00 or 0x01; this value is invalid.
    let wasm =
        File::open("tests/fixtures/malformed/global_section_invalid_mutability.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid mutability should fail");

    match err {
        DecodeModuleError::DecodeGlobalSection(DecodeGlobalSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeGlobalError::DecodeGlobalType(DecodeGlobalTypeError::InvalidMutability(err)),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(err.0, 0x02);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_preamble_error_io() {
    // Sections: none (preamble truncated).
    // Fixture: module preamble is truncated before the full magic/version.
    // Spec 5.5.16 (Modules): the preamble (magic + version) is truncated before all 8 bytes.
    let wasm = File::open("tests/fixtures/malformed/preamble_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated preamble should fail");

    match err {
        DecodeModuleError::ParsePreamble(ParsePreambleError::Io(io_err)) => {
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_preamble_error_unexpected() {
    // Sections: none (preamble invalid).
    // Fixture: module preamble bytes do not match the expected magic/version.
    // Spec 5.5.16 (Modules): the preamble magic/version bytes do not match the required values.
    let wasm = File::open("tests/fixtures/malformed/preamble_unexpected.wasm").unwrap();

    let err = decode_module(wasm).expect_err("wrong preamble should fail");

    match err {
        DecodeModuleError::ParsePreamble(ParsePreambleError::Unexpected(bytes)) => {
            assert_ne!(bytes, *b"\0asm\x01\0\0\0");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_section_header_error_invalid_section_id() {
    // Sections: invalid section id 0x0e.
    // Fixture: module starts with an invalid section id.
    // Spec 5.5.2 (Sections): section id must be one of the defined values; this id is invalid.
    let wasm = File::open("tests/fixtures/malformed/invalid_section_id.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid section id should fail");

    match err {
        DecodeModuleError::DecodeSectionHeader(DecodeSectionHeaderError::InvalidSectionId(err)) => {
            assert_eq!(err.0, 0x0E);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_module_section_out_of_order() {
    // Sections: Memory, Type.
    // Fixture: module sections appear out of the required order.
    // Spec 5.5.16 (Modules): non-custom sections must appear in the prescribed order; this module is out of order.
    let wasm = File::open("tests/fixtures/malformed/section_out_of_order.wasm").unwrap();

    let err = decode_module(wasm).expect_err("out-of-order sections should fail");

    match err {
        DecodeModuleError::SectionOutOfOrder { current, previous } => {
            assert_eq!(current, SectionKind::Type);
            assert_eq!(previous, SectionKind::Memory);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_module_duplicate_section() {
    // Sections: Type, Type.
    // Fixture: module contains a duplicate type section.
    // Spec 5.5.16 (Modules): non-custom sections occur at most once; type section is duplicated.
    let wasm = File::open("tests/fixtures/malformed/duplicate_type_section.wasm").unwrap();

    let err = decode_module(wasm).expect_err("duplicate type section should fail");

    match err {
        DecodeModuleError::DuplicateSection(kind) => assert_eq!(kind, SectionKind::Type),
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_module_section_size_mismatch() {
    // Sections: Type.
    // Fixture: type section declared size does not match payload length.
    // Spec 5.5.2 (Sections): section payload length must match declared size; type section mismatches.
    let wasm = File::open("tests/fixtures/malformed/type_section_size_mismatch.wasm").unwrap();

    let err = decode_module(wasm).expect_err("section size mismatch should fail");

    match err {
        DecodeModuleError::SectionSizeMismatch {
            section_kind,
            declared,
            got,
        } => {
            assert_eq!(section_kind, SectionKind::Type);
            assert!(u64::from(declared) > got);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_module_code_func_entries_len_mismatch() {
    // Sections: Type, Function, Code.
    // Fixture: function section declares one function but code section has zero bodies.
    // Spec 5.5.16 (Modules): function and code section vector lengths must match; funcs=1, codes=0.
    let wasm = File::open("tests/fixtures/malformed/code_func_len_mismatch.wasm").unwrap();

    let err = decode_module(wasm).expect_err("function/code length mismatch should fail");

    match err {
        DecodeModuleError::CodeFuncEntriesLenMismatch {
            codes_len,
            funcs_len,
        } => {
            assert_eq!(codes_len, 0);
            assert_eq!(funcs_len, 1);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_module_data_count_mismatch() {
    // Sections: DataCount, Data.
    // Fixture: data count declares 1 segment but data section has 0 segments.
    // Spec 5.5.15 (Data Count Section) and 5.5.16 (Modules): data count must equal data segment count; mismatch.
    let wasm = File::open("tests/fixtures/malformed/data_count_mismatch.wasm").unwrap();

    let err = decode_module(wasm).expect_err("data count mismatch should fail");

    match err {
        DecodeModuleError::DataCountMismatch {
            datas_len,
            data_count,
        } => {
            assert_eq!(datas_len, 0);
            assert_eq!(data_count, 1);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_module_data_count_datas_len_mismatch() {
    // Sections: DataCount.
    // Fixture: data count declares 1 segment but data section has 0 segments.
    // Spec 5.5.15 (Data Count Section) and 5.5.16 (Modules): data count must equal data segment count; mismatch.
    let wasm = File::open("tests/fixtures/malformed/data_count_datas_len_mismatch.wasm").unwrap();

    let err = decode_module(wasm).expect_err("data count datas len mismatch should fail");

    match err {
        DecodeModuleError::DataCountDatasLenMismatch {
            datas_len,
            data_count,
        } => {
            assert_eq!(datas_len, 0);
            assert_eq!(data_count, 1);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_module_data_index_without_data_count() {
    // Sections: Type, Function, Code.
    // Fixture: single function uses data.init/data.drop without a data count section.
    // Spec 5.5.16 (Modules): a data count section is required if data indices are used (memory.init/data.drop).
    let wasm = File::open("tests/fixtures/malformed/data_index_without_data_count.wasm").unwrap();

    let err = decode_module(wasm).expect_err("data index without data count should fail");

    match err {
        DecodeModuleError::DataIndexWithoutDataCount => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_custom_section_error_decode_name_length() {
    // Sections: Custom.
    // Fixture: custom section name length encoded with overlong LEB128.
    // Spec 5.2.4 (Names): name length is a u32; overlong LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/custom_name_length_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong custom name length should fail");

    match err {
        DecodeModuleError::DecodeCustomSection(DecodeCustomSectionError::DecodeName(
            DecodeNameError::DecodeByteVector(DecodeByteVectorError::DecodeLength(
                DecodeU32Error::RepresentationTooLong,
            )),
        )) => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_custom_section_error_decode_name_truncated() {
    // Sections: Custom.
    // Fixture: custom section name bytes are truncated.
    // Spec 5.2.4 (Names): name is length-prefixed; the name bytes are truncated.
    let wasm = File::open("tests/fixtures/malformed/custom_name_truncated_bytes.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated custom name should fail");

    match err {
        DecodeModuleError::DecodeCustomSection(DecodeCustomSectionError::DecodeName(
            DecodeNameError::DecodeByteVector(DecodeByteVectorError::ReadElements(io_err)),
        )) => assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof),
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_custom_section_error_invalid_utf8_name() {
    // Sections: Custom.
    // Fixture: custom section name bytes are invalid UTF-8.
    // Spec 5.2.4 (Names): custom section name bytes must be valid UTF-8; this name is not.
    let wasm = File::open("tests/fixtures/malformed/custom_name_invalid_utf8.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid utf8 in custom name should fail");

    match err {
        DecodeModuleError::DecodeCustomSection(DecodeCustomSectionError::DecodeName(
            DecodeNameError::Utf8(err),
        )) => {
            let utf8_err = err.utf8_error();
            assert_eq!(utf8_err.valid_up_to(), 0);
            assert_eq!(utf8_err.error_len(), Some(1));
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_import_section_error_module_name() {
    // Sections: Import.
    // Fixture: import entry module name length or bytes are malformed.
    // Spec 5.5.5 (Import Section) and 5.2.4 (Names): module name is malformed (length or bytes).
    let wasm = File::open("tests/fixtures/malformed/import_module_name_decode_error.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid import module name should fail");

    match err {
        DecodeModuleError::DecodeImportSection(DecodeImportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeImportError::DecodeModuleName(DecodeNameError::DecodeByteVector(
                        DecodeByteVectorError::DecodeLength(DecodeU32Error::Io(io_err)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_import_section_error_entity_name_utf8() {
    // Sections: Import.
    // Fixture: import entry entity name bytes are invalid UTF-8.
    // Spec 5.5.5 (Import Section) and 5.2.4 (Names): import name bytes are not valid UTF-8.
    let wasm = File::open("tests/fixtures/malformed/import_entity_name_utf8_error.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid import entity name should fail");

    match err {
        DecodeModuleError::DecodeImportSection(DecodeImportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeImportError::DecodeItemName(DecodeNameError::Utf8(err)),
            },
        )) => {
            assert_eq!(position, 0);
            let utf8_err = err.utf8_error();
            assert_eq!(utf8_err.valid_up_to(), 0);
            assert_eq!(utf8_err.error_len(), Some(1));
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_import_error_descriptor_missing_byte() {
    // Sections: Import.
    // Fixture: import entry is missing its descriptor byte.
    // Spec 5.5.5 (Import Section): importdesc tag byte is missing.
    let wasm = File::open("tests/fixtures/malformed/import_descriptor_missing_byte.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing import descriptor should fail");

    match err {
        DecodeModuleError::DecodeImportSection(DecodeImportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeImportError::DecodeExternType(DecodeExternTypeError::ReadMarkerByte(io_err)),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_import_error_invalid_descriptor() {
    // Sections: Import.
    // Fixture: import entry descriptor byte is invalid.
    // Spec 5.5.5 (Import Section): importdesc tag must be a defined kind; this value is invalid.
    let wasm = File::open("tests/fixtures/malformed/import_invalid_descriptor.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid import descriptor should fail");

    match err {
        DecodeModuleError::DecodeImportSection(DecodeImportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeImportError::DecodeExternType(DecodeExternTypeError::InvalidMarkerByte(b)),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(b, 0xFF);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_import_error_typeidx_overlong() {
    // Sections: Import.
    // Fixture: import entry function type index uses an overlong LEB128 encoding.
    // Spec 5.5.5 (Import Section) and 5.5.1 (Indices): typeidx is a u32; overlong LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/import_typeidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong import type idx should fail");

    match err {
        DecodeModuleError::DecodeImportSection(DecodeImportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeImportError::DecodeExternType(DecodeExternTypeError::DecodeTypeIndex(
                        DecodeTypeIdxError(DecodeU32Error::TooLarge),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_import_error_table_invalid_reftype() {
    // Sections: Import.
    // Fixture: imported table type uses an invalid reference type marker.
    // Spec 5.5.5 (Import Section) and 5.3.9 (Table Types): reftype marker is invalid.
    let wasm = File::open("tests/fixtures/malformed/import_table_invalid_reftype.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid import table reftype should fail");

    match err {
        DecodeModuleError::DecodeImportSection(DecodeImportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeImportError::DecodeExternType(DecodeExternTypeError::DecodeTableType(
                        DecodeTableTypeError::DecodeRefType(DecodeRefTypeError::InvalidMarkerByte(
                            DecodeAbsHeapTypeError::InvalidMarkerByte(err),
                        )),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(err.0, 0x00);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_import_error_memory_invalid_limits() {
    // Sections: Import.
    // Fixture: imported memory type has invalid limits flags or bounds.
    // Spec 5.5.5 (Import Section) and 5.3.7 (Limits): memory limits flags or bounds are invalid.
    let wasm = File::open("tests/fixtures/malformed/import_memory_invalid_limits.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid import memory limits should fail");

    match err {
        DecodeModuleError::DecodeImportSection(DecodeImportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeImportError::DecodeExternType(DecodeExternTypeError::DecodeMemoryType(
                        DecodeMemoryTypeError(ParseLimitsError::UnexpectedFlagByte(0x02)),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_import_error_global_invalid_mutability() {
    // Sections: Import.
    // Fixture: imported global type has an invalid mutability byte.
    // Spec 5.5.5 (Import Section) and 5.3.10 (Global Types): mutability must be 0x00 or 0x01; invalid.
    let wasm =
        File::open("tests/fixtures/malformed/import_global_invalid_mutability.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid import global mutability should fail");

    match err {
        DecodeModuleError::DecodeImportSection(DecodeImportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeImportError::DecodeExternType(DecodeExternTypeError::DecodeGlobalType(
                        DecodeGlobalTypeError::InvalidMutability(err),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(err.0, 0x02);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_export_section_error_name_decode() {
    // Sections: Export.
    // Fixture: export entry name length or bytes are malformed.
    // Spec 5.5.10 (Export Section) and 5.2.4 (Names): export name is malformed (length or bytes).
    let wasm = File::open("tests/fixtures/malformed/export_name_decode_error.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid export name should fail");

    match err {
        DecodeModuleError::DecodeExportSection(DecodeExportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeExportError::DecodeName(DecodeNameError::DecodeByteVector(
                        DecodeByteVectorError::DecodeLength(DecodeU32Error::RepresentationTooLong),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_export_error_descriptor_missing_byte() {
    // Sections: Export.
    // Fixture: export entry is missing its descriptor byte.
    // Spec 5.5.10 (Export Section): exportdesc tag byte is missing.
    let wasm = File::open("tests/fixtures/malformed/export_descriptor_missing_byte.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing export descriptor should fail");

    match err {
        DecodeModuleError::DecodeExportSection(DecodeExportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeExportError::ReadDescriptorMarkerByte(io_err),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_export_error_index_decode() {
    // Sections: Export.
    // Fixture: export entry index uses an invalid LEB128 encoding.
    // Spec 5.5.10 (Export Section) and 5.5.1 (Indices): export index is a u32; malformed LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/export_index_decode_error.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong export index should fail");

    match err {
        DecodeModuleError::DecodeExportSection(DecodeExportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeExportError::DecodeIndex(u32_err),
            },
        )) => {
            assert_eq!(position, 0);
            assert!(
                matches!(u32_err, DecodeU32Error::TooLarge),
                "unexpected export index error: {u32_err:?}"
            );
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_export_error_invalid_descriptor() {
    // Sections: Export.
    // Fixture: export entry descriptor byte is invalid.
    // Spec 5.5.10 (Export Section): exportdesc tag must be a defined kind; this value is invalid.
    let wasm = File::open("tests/fixtures/malformed/export_invalid_descriptor.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid export descriptor should fail");

    match err {
        DecodeModuleError::DecodeExportSection(DecodeExportSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeExportError::InvalidDescriptorMarkerByte(err),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(err.0, 0xFF);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_start_section_error_func_idx() {
    // Sections: Start.
    // Fixture: start section function index encoded with overlong LEB128.
    // Spec 5.5.11 (Start Section) and 5.5.1 (Indices): funcidx is a u32; overlong LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/start_section_funcidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong start func idx should fail");

    match err {
        DecodeModuleError::DecodeStartSection(DecodeStartSectionError(DecodeFuncIdxError(
            DecodeU32Error::TooLarge,
        ))) => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_code_error_function_size_truncated() {
    // Sections: Type, Function, Code.
    // Fixture: code section entry size present but function body bytes are truncated.
    // Spec 5.5.13 (Code Section): body size is present but the function body bytes are truncated.
    let wasm =
        File::open("tests/fixtures/malformed/code_section_function_size_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing code size should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeCodeError::DecodeFunctionSize(u32_err),
            },
        )) => {
            assert_eq!(position, 0);
            match u32_err {
                DecodeU32Error::Io(io_err) => {
                    assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
                }
                other => panic!("expected Io error, got {other:?}"),
            }
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_code_error_locals_vector_length_overlong() {
    // Sections: Type, Function, Code.
    // Fixture: function locals vector length encoded with overlong LEB128.
    // Spec 5.5.13 (Code Section) and 5.1.3 (Vectors): locals vector length is a u32; overlong LEB128 violates 5.2.2.
    let wasm =
        File::open("tests/fixtures/malformed/code_section_locals_vector_length_overlong.wasm")
            .unwrap();

    let err = decode_module(wasm).expect_err("overlong locals vector length should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeLocalsVector(DecodeListError::DecodeLength(
                        DecodeU32Error::RepresentationTooLong,
                    )),
            },
        )) => assert_eq!(position, 0),
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_code_locals_error_count_out_of_bounds() {
    // Sections: Type, Function, Code.
    // Fixture: function locals count exceeds remaining bytes in the body.
    // Spec 5.5.13 (Code Section): local decl count claims more bytes than remain, so the locals vector is truncated.
    let wasm = File::open("tests/fixtures/malformed/code_section_locals_count_out_of_bounds.wasm")
        .unwrap();

    let err = decode_module(wasm).expect_err("too many locals should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeLocalsVector(DecodeListError::ParseElement {
                        position: locals_position,
                        source:
                            DecodeCodeLocalsError::LocalsCountOutOfBound {
                                max_locals,
                                actual_locals,
                            },
                    }),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(locals_position, 1);
            assert_eq!(max_locals, u64::from(u32::MAX));
            assert_eq!(actual_locals, u64::from(u32::MAX) + 1);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_code_locals_error_decode_count() {
    // Sections: Type, Function, Code.
    // Fixture: function locals count uses an invalid LEB128 encoding.
    // Spec 5.5.13 (Code Section): local decl count is a u32; malformed LEB128 violates 5.2.2.
    let wasm =
        File::open("tests/fixtures/malformed/code_section_locals_count_decode_error.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated locals count should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeLocalsVector(DecodeListError::ParseElement {
                        position: locals_position,
                        source: DecodeCodeLocalsError::DecodeLocalsCount(DecodeU32Error::Io(io_err)),
                    }),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(locals_position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_code_error_local_valtype_invalid() {
    // Sections: Type, Function, Code.
    // Fixture: function locals vector contains an invalid value type marker.
    // Spec 5.5.13 (Code Section) and 5.3.4 (Value Types): a local decl uses an invalid valtype marker.
    let wasm =
        File::open("tests/fixtures/malformed/code_section_local_valtype_invalid.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid local valtype should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::DecodeLocalsVector(DecodeListError::ParseElement {
                        position: locals_position,
                        source:
                            DecodeCodeLocalsError::DecodeLocalValType(
                                DecodeValTypeError::DecodeRefType(
                                    DecodeRefTypeError::InvalidMarkerByte(
                                        DecodeAbsHeapTypeError::InvalidMarkerByte(err),
                                    ),
                                ),
                            ),
                    }),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(locals_position, 0);
            assert_eq!(err.0, 0xFF);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_code_error_entry_size_mismatch() {
    // Sections: Type, Function, Code.
    // Fixture: code section entry declares a size that does not match the body length.
    // Spec 5.5.13 (Code Section): each body has a size prefix that must match the body length; the sizes disagree.
    let wasm =
        File::open("tests/fixtures/malformed/code_section_entry_size_mismatch.wasm").unwrap();

    let err = decode_module(wasm).expect_err("entry size mismatch should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeCodeError::EntrySizeMismatch {
                        declared_bytes,
                        leftover_bytes,
                        consumed_bytes,
                    },
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(declared_bytes, 3);
            assert_eq!(leftover_bytes, 1);
            assert_eq!(consumed_bytes, 2);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn parse_expression_error_unexpected_else() {
    // Sections: Type, Function, Code.
    // Fixture: function expression contains an unexpected else opcode.
    // Spec 5.4.9 (Expressions): else is only valid inside an if; encountering else here is invalid.
    let wasm = File::open("tests/fixtures/malformed/expression_unexpected_else.wasm").unwrap();

    let err = decode_module(wasm).expect_err("unexpected else should fail");

    match err {
        DecodeModuleError::DecodeCodeSection(DecodeCodeSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeCodeError::DecodeFunctionBody(ParseExpressionError::UnexpectedElse),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_element_section_error_bitfield_decode() {
    // Sections: Element.
    // Fixture: element segment bitfield uses an invalid LEB128 encoding.
    // Spec 5.5.12 (Element Section): segment flags are encoded as a u32; malformed LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/element_bitfield_decode_error.wasm").unwrap();

    let err = decode_module(wasm).expect_err("element bitfield decode should fail");

    match err {
        DecodeModuleError::DecodeElementSection(DecodeElementSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeElementError::DecodeBitfield(DecodeU32Error::RepresentationTooLong),
            },
        )) => assert_eq!(position, 0),
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_element_error_invalid_bitfield() {
    // Sections: Element.
    // Fixture: element segment bitfield contains an invalid value.
    // Spec 5.5.12 (Element Section): segment flags must be defined values; this bitfield is invalid.
    let wasm = File::open("tests/fixtures/malformed/element_invalid_bitfield.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid element bitfield should fail");

    match err {
        DecodeModuleError::DecodeElementSection(DecodeElementSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeElementError::InvalidBitfield(n),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(n, 9);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_element_error_offset_expression() {
    // Sections: Element.
    // Fixture: element segment offset expression is missing its opcode.
    // Spec 5.5.12 (Element Section) and 5.4.9 (Expressions): active segment offset expr is missing its opcode.
    let wasm = File::open("tests/fixtures/malformed/element_offset_expr_missing.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing offset expr should fail");

    match err {
        DecodeModuleError::DecodeElementSection(DecodeElementSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeElementError::DecodeOffsetExpression(ParseExpressionError::ParseInstruction(
                        ParseError::ReadOpcode(io_err),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_element_error_elemkind_invalid() {
    // Sections: Element.
    // Fixture: element segment elemkind byte has an invalid value.
    // Spec 5.5.12 (Element Section): elemkind must be 0x00 for function elements; this value is invalid.
    let wasm = File::open("tests/fixtures/malformed/element_elemkind_invalid.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid element kind should fail");

    match err {
        DecodeModuleError::DecodeElementSection(DecodeElementSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeElementError::DecodeElementKind(DecodeElementKindError::InvalidElemKind(0x01)),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_element_error_elemkind_io() {
    // Sections: Element.
    // Fixture: element segment elemkind byte is truncated or missing.
    // Spec 5.5.12 (Element Section): elemkind byte is required; the byte is missing/truncated.
    let wasm = File::open("tests/fixtures/malformed/element_elemkind_io.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing elemkind byte should fail");

    match err {
        DecodeModuleError::DecodeElementSection(DecodeElementSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeElementError::DecodeElementKind(DecodeElementKindError::Io(io_err)),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_element_error_reference_type_invalid() {
    // Sections: Element.
    // Fixture: element segment reference type marker is invalid.
    // Spec 5.5.12 (Element Section) and 5.3.3 (Reference Types): reftype marker is invalid.
    let wasm = File::open("tests/fixtures/malformed/element_reftype_invalid.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid element ref type should fail");

    match err {
        DecodeModuleError::DecodeElementSection(DecodeElementSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeElementError::DecodeReferenceType(DecodeRefTypeError::InvalidMarkerByte(
                        DecodeAbsHeapTypeError::InvalidMarkerByte(err),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(err.0, 0x00);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_element_error_init_decode_length() {
    // Sections: Element.
    // Fixture: element segment init vector length uses an invalid LEB128 encoding.
    // Spec 5.5.12 (Element Section): init vector length is a u32; malformed LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/element_init_decode_length.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid element init vector should fail");

    match err {
        DecodeModuleError::DecodeElementSection(DecodeElementSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeElementError::DecodeInit(DecodeListError::DecodeLength(
                        DecodeU32Error::RepresentationTooLong,
                    )),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_element_error_funcidx_vector_overlong() {
    // Sections: Element.
    // Fixture: element segment function index vector length is overlong.
    // Spec 5.5.12 (Element Section) and 5.1.3 (Vectors): funcidx vector length is overlong LEB128.
    let wasm = File::open("tests/fixtures/malformed/element_funcidx_vector_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong funcidx vector should fail");

    match err {
        DecodeModuleError::DecodeElementSection(DecodeElementSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeElementError::DecodeFuncIdxVector(DecodeListError::ParseElement {
                        position: func_pos,
                        source: DecodeFuncIdxError(DecodeU32Error::TooLarge),
                    }),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(func_pos, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_element_error_table_idx_overlong() {
    // Sections: Element.
    // Fixture: element segment table index uses an overlong LEB128 encoding.
    // Spec 5.5.12 (Element Section) and 5.5.1 (Indices): tableidx is a u32; overlong LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/element_tableidx_overlong.wasm").unwrap();

    let err = decode_module(wasm).expect_err("overlong table idx should fail");

    match err {
        DecodeModuleError::DecodeElementSection(DecodeElementSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeElementError::DecodeTableIdx(DecodeTableIdxError(DecodeU32Error::TooLarge)),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_element_error_expression_missing_opcode() {
    // Sections: Element.
    // Fixture: element segment initializer expression is missing its opcode.
    // Spec 5.5.12 (Element Section) and 5.4.9 (Expressions): init expr is missing its first opcode.
    let wasm =
        File::open("tests/fixtures/malformed/element_expression_missing_opcode.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing element expression should fail");

    match err {
        DecodeModuleError::DecodeElementSection(DecodeElementSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeElementError::DecodeElementExpression(ParseExpressionError::ParseInstruction(
                        ParseError::ReadOpcode(io_err),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_data_section_error_bitfield_decode() {
    // Sections: Data.
    // Fixture: data segment bitfield uses an invalid LEB128 encoding.
    // Spec 5.5.14 (Data Section): segment flags are encoded as a u32; malformed LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/data_bitfield_decode_error.wasm").unwrap();

    let err = decode_module(wasm).expect_err("data bitfield decode should fail");

    match err {
        DecodeModuleError::DecodeDataSection(DecodeDataSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeDataSegmentError::DecodeBitfield(DecodeU32Error::RepresentationTooLong),
            },
        )) => {
            assert_eq!(position, 0);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_data_section_error_invalid_bitfield() {
    // Sections: Data.
    // Fixture: data segment bitfield contains an invalid value.
    // Spec 5.5.14 (Data Section): segment flags must be one of the defined values; this bitfield is invalid.
    let wasm = File::open("tests/fixtures/malformed/data_invalid_bitfield.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid data bitfield should fail");

    match err {
        DecodeModuleError::DecodeDataSection(DecodeDataSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeDataSegmentError::InvalidBitfield(n),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(n, 3);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_data_section_error_offset_expr_missing() {
    // Sections: Data.
    // Fixture: data segment offset expression is missing its opcode.
    // Spec 5.5.14 (Data Section) and 5.4.9 (Expressions): active segments include an offset expr; the opcode is missing.
    let wasm = File::open("tests/fixtures/malformed/data_offset_expr_missing.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing data offset expr should fail");

    match err {
        DecodeModuleError::DecodeDataSection(DecodeDataSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeDataSegmentError::DecodeOffsetExpr(ParseExpressionError::ParseInstruction(
                        ParseError::ReadOpcode(io_err),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_data_section_error_init_vector_truncated() {
    // Sections: Data.
    // Fixture: data segment init byte vector is truncated.
    // Spec 5.5.14 (Data Section): init bytes are a length-prefixed byte vector; the payload is truncated.
    let wasm = File::open("tests/fixtures/malformed/data_init_vector_truncated.wasm").unwrap();

    let err = decode_module(wasm).expect_err("truncated data init vector should fail");

    match err {
        DecodeModuleError::DecodeDataSection(DecodeDataSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeDataSegmentError::DecodeInitVector(DecodeByteVectorError::ReadElements(
                        io_err,
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_datacount_section_error_decode_segment_count() {
    // Sections: DataCount.
    // Fixture: data count section segment count uses an invalid LEB128 encoding.
    // Spec 5.5.15 (Data Count Section): segment count is a u32; malformed LEB128 violates 5.2.2.
    let wasm = File::open("tests/fixtures/malformed/data_count_decode_error.wasm").unwrap();

    let err = decode_module(wasm).expect_err("data count decode should fail");

    match err {
        DecodeModuleError::DecodeDatacountSection(
            DecodeDataCountSectionError::DecodeDataSegmentCount(
                DecodeU32Error::RepresentationTooLong,
            ),
        ) => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_global_error_init_missing_expr() {
    // Sections: Global.
    // Fixture: global section entry missing its initializer expression opcode.
    // Spec 5.5.9 (Global Section) and 5.4.9 (Expressions): global initializer expr is missing its opcode.
    let wasm =
        File::open("tests/fixtures/malformed/global_section_init_missing_expr.wasm").unwrap();

    let err = decode_module(wasm).expect_err("missing global init should fail");

    match err {
        DecodeModuleError::DecodeGlobalSection(DecodeGlobalSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source:
                    DecodeGlobalError::DecodeInit(ParseExpressionError::ParseInstruction(
                        ParseError::ReadOpcode(io_err),
                    )),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn decode_tag_section_error_invalid_marker() {
    // Sections: Type, Tag.
    // Fixture: tag section entry has an invalid tagtype marker byte.
    // Spec 5.3.10 (Tag Types): tagtype must begin with 0x00.
    let wasm = File::open("tests/fixtures/malformed/tag_section_invalid_marker.wasm").unwrap();

    let err = decode_module(wasm).expect_err("invalid tagtype marker should fail");

    match err {
        DecodeModuleError::DecodeTagSection(DecodeTagSectionError::DecodeVector(
            DecodeListError::ParseElement {
                position,
                source: DecodeTagTypeError::InvalidMarkerByte(byte),
            },
        )) => {
            assert_eq!(position, 0);
            assert_eq!(byte, 0x01);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}
