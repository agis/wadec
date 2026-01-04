use pretty_assertions::assert_eq;
use std::collections::HashSet;
use std::fs::File;
use wadec::core::indices::*;
use wadec::core::instruction::*;
use wadec::core::types::*;
use wadec::core::*;
use wadec::decode_errors::*;
use wadec::*;

#[allow(dead_code)]
#[test]
fn it_parses_preamble() {
    let mut input: &[u8] = &[];
    let err = decode_module(input).expect_err("empty input should fail");
    match err {
        DecodeModuleError::ParsePreamble(ParsePreambleError::Io(io_err)) => {
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }

    input = &[0xD3, 0xAD, 0xBE, 0xEF];
    let err = decode_module(input).expect_err("short preamble should fail");
    match err {
        DecodeModuleError::ParsePreamble(ParsePreambleError::Io(io_err)) => {
            assert_eq!(io_err.kind(), std::io::ErrorKind::UnexpectedEof);
        }
        other => panic!("unexpected error: {other:?}"),
    }

    input = &[0xD3, 0xAD, 0xBE, 0xEF, 0x00, 0x00, 0x00, 0x00];
    let err = decode_module(input).expect_err("wrong preamble should fail");
    match err {
        DecodeModuleError::ParsePreamble(ParsePreambleError::Unexpected(preamble)) => {
            assert_eq!(preamble, [0xD3, 0xAD, 0xBE, 0xEF, 0x00, 0x00, 0x00, 0x00]);
        }
        other => panic!("unexpected error: {other:?}"),
    }

    // just the preamble
    input = &[0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00];
    assert_eq!(decode_module(input).unwrap(), Module::default());
}

#[test]
fn it_accepts_empty_module() {
    // (module)
    let f = File::open("./tests/fixtures/empty.wasm").unwrap();

    assert_eq!(decode_module(f).unwrap(), Module::default(),)
}

#[test]
fn it_accepts_add_sample() {
    let f = File::open("./tests/fixtures/add.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Export,
        SectionKind::Code,
    ];

    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 2,
        },
        SectionHeader {
            kind: SectionKind::Export,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 9,
        },
    ];

    let types = vec![FuncType {
        parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
        results: vec![ValType::Num(NumType::Int32)],
    }];

    let funcs = vec![Func {
        r#type: TypeIdx(0),
        locals: Vec::new(),
        body: vec![
            Instruction::LocalGet(LocalIdx(0)),
            Instruction::LocalGet(LocalIdx(1)),
            Instruction::I32Add,
        ],
    }];

    let exports = vec![Export {
        name: "add".to_owned(),
        externidx: ExternIdx::Func(FuncIdx(0)),
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            exports,
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_tag_section() {
    let f = File::open("tests/fixtures/tag_section.wasm").unwrap();

    let parsed_section_kinds = vec![SectionKind::Type, SectionKind::Tag];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 4,
        },
        SectionHeader {
            kind: SectionKind::Tag,
            size: 3,
        },
    ];

    let types = vec![FuncType {
        parameters: vec![],
        results: vec![],
    }];

    let tags = vec![TagType(TypeIdx(0))];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            tags,
            ..Default::default()
        }
    )
}

#[test]
// # spec version: 3
fn it_decodes_tag_section_multiple_entries() {
    let f = File::open("tests/fixtures/tag_section_multi.wasm").unwrap();

    let parsed_section_kinds = vec![SectionKind::Type, SectionKind::Tag];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 9,
        },
        SectionHeader {
            kind: SectionKind::Tag,
            size: 5,
        },
    ];

    let types = rectypes(vec![
        CompType::Func {
            parameters: vec![],
            results: vec![],
        },
        CompType::Func {
            parameters: vec![
                ValType::Num(NumType::Int32),
                ValType::Num(NumType::Int64),
            ],
            results: vec![],
        },
    ]);

    let tags = vec![TagType(TypeIdx(0)), TagType(TypeIdx(1))];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            tags,
            ..Default::default()
        }
    )
}

#[test]
// # spec version: 3
fn it_decodes_tag_export() {
    let f = File::open("tests/fixtures/tag_export.wasm").unwrap();
    let module = decode_module(f).unwrap();

    assert_eq!(
        module.parsed_section_kinds,
        vec![SectionKind::Type, SectionKind::Tag, SectionKind::Export]
    );
    assert_eq!(
        module.types,
        rectypes(vec![CompType::Func {
            parameters: vec![],
            results: vec![],
        }])
    );
    assert_eq!(module.tags, vec![TagType(TypeIdx(0))]);
    assert_eq!(
        module.exports,
        vec![Export {
            name: "tag0".to_owned(),
            externidx: ExternIdx::Tag(TagIdx(0)),
        }]
    );
}

#[test]
fn it_accepts_two_funcs_exporting_second() {
    // two funcs (i32,i32)->i32; exports func 1 as "add2"
    // Body: local.get 0, local.get 1, i32.add, end.
    let f = File::open("tests/fixtures/two_funcs_add2.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Export,
        SectionKind::Code,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 3,
        },
        SectionHeader {
            kind: SectionKind::Export,
            size: 8,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 17,
        },
    ];

    let types = vec![FuncType {
        parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
        results: vec![ValType::Num(NumType::Int32)],
    }];

    let add_body = vec![
        Instruction::LocalGet(LocalIdx(0)),
        Instruction::LocalGet(LocalIdx(1)),
        Instruction::I32Add,
    ];

    let funcs = vec![
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: add_body.clone(),
        },
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: add_body.clone(),
        },
    ];

    let exports = vec![Export {
        name: "add2".to_owned(),
        externidx: ExternIdx::Func(FuncIdx(1)),
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            exports,
            ..Default::default()
        }
    )
}

#[test]
fn it_accepts_imports_of_tables_memories_and_globals() {
    let f = File::open("tests/fixtures/imports_table_mem_global.wasm").unwrap();

    let section_headers = vec![SectionHeader {
        kind: SectionKind::Import,
        size: 63,
    }];

    let imports = vec![
        Import {
            module_name: "env".to_owned(),
            item_name: "table".to_owned(),
            extern_type: ExternType::Table(TableType {
                limits: Limits {
                    address_type: AddrType::I32,
                    min: 1,
                    max: None,
                },
                reftype: RefType::Func,
            }),
        },
        Import {
            module_name: "env".to_owned(),
            item_name: "memory".to_owned(),
            extern_type: ExternType::Mem(MemType {
                limits: Limits {
                    address_type: AddrType::I32,
                    min: 1,
                    max: None,
                },
            }),
        },
        Import {
            module_name: "env".to_owned(),
            item_name: "global_i".to_owned(),
            extern_type: ExternType::Global(GlobalType(Mut::Const, ValType::Num(NumType::Int32))),
        },
        Import {
            module_name: "env".to_owned(),
            item_name: "global_mut".to_owned(),
            extern_type: ExternType::Global(GlobalType(Mut::Var, ValType::Num(NumType::Int64))),
        },
    ];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds: vec![SectionKind::Import],
            section_headers,
            imports,
            ..Default::default()
        }
    )
}

#[test]
// # spec version: 3
fn it_decodes_imports_with_tag_and_func() {
    let f = File::open("tests/fixtures/imports_with_tag_func.wasm").unwrap();
    let module = decode_module(f).unwrap();

    let types = rectypes(vec![
        CompType::Func {
            parameters: vec![ValType::Num(NumType::Int32)],
            results: vec![ValType::Num(NumType::Int32)],
        },
        CompType::Func {
            parameters: vec![ValType::Num(NumType::Int32)],
            results: vec![],
        },
    ]);

    let imports = vec![
        Import {
            module_name: "env".to_owned(),
            item_name: "func0".to_owned(),
            extern_type: ExternType::Func(TypeIdx(0)),
        },
        Import {
            module_name: "env".to_owned(),
            item_name: "tag0".to_owned(),
            extern_type: ExternType::Tag(TagType(TypeIdx(1))),
        },
    ];

    assert_eq!(module.types, types);
    assert_eq!(module.imports, imports);
}

#[test]
fn it_accepts_module_without_exports() {
    // single func (i32,i32)->i32; no Export section
    // Body: local.get 0, local.get 1, i32.add
    let f = File::open("tests/fixtures/no_export.wasm").unwrap();

    let parsed_section_kinds = vec![SectionKind::Type, SectionKind::Function, SectionKind::Code];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 2,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 9,
        },
    ];

    let types = vec![FuncType {
        parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
        results: vec![ValType::Num(NumType::Int32)],
    }];

    let funcs = vec![Func {
        r#type: TypeIdx(0),
        locals: Vec::new(),
        body: vec![
            Instruction::LocalGet(LocalIdx(0)),
            Instruction::LocalGet(LocalIdx(1)),
            Instruction::I32Add,
        ],
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_start_section() {
    let f = File::open("tests/fixtures/start_section.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Start,
        SectionKind::Code,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 4,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 2,
        },
        SectionHeader {
            kind: SectionKind::Start,
            size: 1,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 4,
        },
    ];

    let types = vec![FuncType {
        parameters: Vec::new(),
        results: Vec::new(),
    }];

    let funcs = vec![Func {
        r#type: TypeIdx(0),
        locals: Vec::new(),
        body: Vec::new(),
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            start: Some(FuncIdx(0)),
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_control_instructions() {
    let f = File::open("tests/fixtures/control_instructions.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Table,
        SectionKind::Export,
        SectionKind::Code,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 9,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 3,
        },
        SectionHeader {
            kind: SectionKind::Table,
            size: 4,
        },
        SectionHeader {
            kind: SectionKind::Export,
            size: 11,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 64,
        },
    ];

    let types = vec![
        FuncType {
            parameters: Vec::new(),
            results: Vec::new(),
        },
        FuncType {
            parameters: Vec::new(),
            results: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
        },
    ];

    let funcs = vec![
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: Vec::new(),
        },
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![
                Instruction::Nop,
                Instruction::Block(BlockType::Empty, vec![Instruction::Unreachable]),
                Instruction::Block(
                    BlockType::Empty,
                    vec![Instruction::Loop(
                        BlockType::Empty,
                        vec![Instruction::Br(LabelIdx(1))],
                    )],
                ),
                Instruction::Block(
                    BlockType::Empty,
                    vec![Instruction::I32Const(0), Instruction::BrIf(LabelIdx(0))],
                ),
                Instruction::Block(
                    BlockType::Empty,
                    vec![
                        Instruction::I32Const(0),
                        Instruction::BrTable(vec![LabelIdx(0)], LabelIdx(0)),
                    ],
                ),
                Instruction::Block(
                    BlockType::Empty,
                    vec![
                        Instruction::I32Const(0),
                        Instruction::If(
                            BlockType::Empty,
                            vec![Instruction::Unreachable],
                            Some(vec![Instruction::Nop]),
                        ),
                    ],
                ),
                Instruction::Block(
                    BlockType::X(1),
                    vec![Instruction::I32Const(42), Instruction::I32Const(7)],
                ),
                Instruction::Drop,
                Instruction::Drop,
                Instruction::Call(FuncIdx(0)),
                Instruction::I32Const(0),
                Instruction::CallIndirect(TableIdx(0), TypeIdx(0)),
                Instruction::Return,
            ],
        },
    ];

    let tables = vec![TableType {
        limits: Limits {
            address_type: AddrType::I32,
            min: 1,
            max: None,
        },
        reftype: RefType::Func,
    }];

    let exports = vec![Export {
        name: "control".to_owned(),
        externidx: ExternIdx::Func(FuncIdx(1)),
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            tables,
            exports,
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_return_call_instructions() {
    let f = File::open("tests/fixtures/return_call_instructions.wasm").unwrap();
    let module = decode_module(f).unwrap();

    let types = rectypes(vec![
        CompType::Func {
            parameters: Vec::new(),
            results: Vec::new(),
        },
        CompType::Func {
            parameters: Vec::new(),
            results: Vec::new(),
        },
    ]);

    let tables = vec![Table(
        TableType {
            limits: Limits {
                address_type: AddrType::I32,
                min: 1,
                max: None,
            },
            reftype: ref_null_func(),
        },
        vec![],
    )];

    let funcs = vec![
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![Instruction::Nop],
        },
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![Instruction::ReturnCall(FuncIdx(0))],
        },
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![
                Instruction::I32Const(0),
                Instruction::ReturnCallIndirect(TableIdx(0), TypeIdx(1)),
            ],
        },
    ];

    assert_eq!(module.types, types);
    assert_eq!(module.tables, tables);
    assert_eq!(module.funcs, funcs);
}
#[test]
fn it_decodes_element_section_all_alts() {
    let f = File::open("tests/fixtures/element_section_all_alts.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Table,
        SectionKind::Element,
        SectionKind::Code,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 4,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Table,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Element,
            size: 67,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 19,
        },
    ];

    let types = vec![FuncType {
        parameters: vec![],
        results: vec![],
    }];

    let funcs = vec![
        Func {
            r#type: TypeIdx(0),
            locals: vec![],
            body: vec![],
        },
        Func {
            r#type: TypeIdx(0),
            locals: vec![],
            body: vec![],
        },
        Func {
            r#type: TypeIdx(0),
            locals: vec![],
            body: vec![],
        },
        Func {
            r#type: TypeIdx(0),
            locals: vec![],
            body: vec![],
        },
        Func {
            r#type: TypeIdx(0),
            locals: vec![],
            body: vec![],
        },
        Func {
            r#type: TypeIdx(0),
            locals: vec![],
            body: vec![],
        },
    ];

    let tables = vec![
        TableType {
            limits: Limits {
                address_type: AddrType::I32,
                min: 12,
                max: None,
            },
            reftype: RefType::Func,
        },
        TableType {
            limits: Limits {
                address_type: AddrType::I32,
                min: 12,
                max: None,
            },
            reftype: RefType::Func,
        },
    ];

    let elems = vec![
        Elem {
            r#type: RefType::Func,
            init: vec![
                vec![Instruction::RefFunc(FuncIdx(0))],
                vec![Instruction::RefFunc(FuncIdx(1))],
            ],
            mode: ElemMode::Active {
                table: TableIdx(0),
                offset: vec![Instruction::I32Const(0)],
            },
        },
        Elem {
            r#type: RefType::Func,
            init: vec![
                vec![Instruction::RefFunc(FuncIdx(2))],
                vec![Instruction::RefFunc(FuncIdx(3))],
            ],
            mode: ElemMode::Passive,
        },
        Elem {
            r#type: RefType::Func,
            init: vec![vec![Instruction::RefFunc(FuncIdx(4))]],
            mode: ElemMode::Active {
                table: TableIdx(1),
                offset: vec![Instruction::I32Const(1)],
            },
        },
        Elem {
            r#type: RefType::Func,
            init: vec![vec![Instruction::RefFunc(FuncIdx(5))]],
            mode: ElemMode::Declarative,
        },
        Elem {
            r#type: RefType::Func,
            init: vec![
                vec![Instruction::RefFunc(FuncIdx(0))],
                vec![Instruction::RefNull(RefType::Func)],
            ],
            mode: ElemMode::Active {
                table: TableIdx(0),
                offset: vec![Instruction::I32Const(6)],
            },
        },
        Elem {
            r#type: RefType::Func,
            init: vec![
                vec![Instruction::RefFunc(FuncIdx(1))],
                vec![Instruction::RefNull(RefType::Func)],
            ],
            mode: ElemMode::Passive,
        },
        Elem {
            r#type: RefType::Func,
            init: vec![
                vec![Instruction::RefFunc(FuncIdx(2))],
                vec![Instruction::RefNull(RefType::Func)],
            ],
            mode: ElemMode::Active {
                table: TableIdx(1),
                offset: vec![Instruction::I32Const(3)],
            },
        },
        Elem {
            r#type: RefType::Func,
            init: vec![
                vec![Instruction::RefNull(RefType::Func)],
                vec![Instruction::RefFunc(FuncIdx(3))],
            ],
            mode: ElemMode::Declarative,
        },
    ];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            tables,
            elems,
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_reference_instructions() {
    let f = File::open("tests/fixtures/reference_instructions.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Table,
        SectionKind::Export,
        SectionKind::Element,
        SectionKind::Code,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 4,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 3,
        },
        SectionHeader {
            kind: SectionKind::Table,
            size: 4,
        },
        SectionHeader {
            kind: SectionKind::Export,
            size: 8,
        },
        SectionHeader {
            kind: SectionKind::Element,
            size: 5,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 14,
        },
    ];

    let types = vec![FuncType {
        parameters: Vec::new(),
        results: Vec::new(),
    }];

    let funcs = vec![
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: Vec::new(),
        },
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![
                Instruction::RefNull(RefType::Func),
                Instruction::RefIsNull,
                Instruction::Drop,
                Instruction::RefFunc(FuncIdx(0)),
                Instruction::Drop,
            ],
        },
    ];

    let tables = vec![TableType {
        limits: Limits {
            address_type: AddrType::I32,
            min: 1,
            max: None,
        },
        reftype: RefType::Func,
    }];

    let exports = vec![Export {
        name: "refs".to_owned(),
        externidx: ExternIdx::Func(FuncIdx(1)),
    }];

    let elems = vec![Elem {
        r#type: RefType::Func,
        init: vec![vec![Instruction::RefFunc(FuncIdx(0))]],
        mode: ElemMode::Declarative,
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            tables,
            elems,
            exports,
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_variable_instructions() {
    let f = File::open("tests/fixtures/variable_instructions.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Global,
        SectionKind::Export,
        SectionKind::Code,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 6,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 2,
        },
        SectionHeader {
            kind: SectionKind::Global,
            size: 6,
        },
        SectionHeader {
            kind: SectionKind::Export,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 20,
        },
    ];

    let types = vec![FuncType {
        parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
        results: Vec::new(),
    }];

    let funcs = vec![Func {
        r#type: TypeIdx(0),
        locals: Vec::new(),
        body: vec![
            Instruction::LocalGet(LocalIdx(0)),
            Instruction::LocalSet(LocalIdx(1)),
            Instruction::LocalGet(LocalIdx(1)),
            Instruction::LocalTee(LocalIdx(0)),
            Instruction::Drop,
            Instruction::GlobalGet(GlobalIdx(0)),
            Instruction::Drop,
            Instruction::LocalGet(LocalIdx(1)),
            Instruction::GlobalSet(GlobalIdx(0)),
        ],
    }];

    let globals = vec![Global {
        r#type: GlobalType(Mut::Var, ValType::Num(NumType::Int32)),
        init: vec![Instruction::I32Const(0)],
    }];

    let exports = vec![Export {
        name: "var".to_owned(),
        externidx: ExternIdx::Func(FuncIdx(0)),
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            globals,
            exports,
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_parametric_instructions() {
    let f = File::open("tests/fixtures/parametric_instructions.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Export,
        SectionKind::Code,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 8,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 2,
        },
        SectionHeader {
            kind: SectionKind::Export,
            size: 9,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 24,
        },
    ];

    let types = vec![FuncType {
        parameters: vec![
            ValType::Num(NumType::Int32),
            ValType::Num(NumType::Int32),
            ValType::Num(NumType::Int32),
        ],
        results: vec![ValType::Num(NumType::Int32)],
    }];

    let funcs = vec![Func {
        r#type: TypeIdx(0),
        locals: Vec::new(),
        body: vec![
            Instruction::LocalGet(LocalIdx(0)),
            Instruction::LocalGet(LocalIdx(1)),
            Instruction::LocalGet(LocalIdx(2)),
            Instruction::Select(None),
            Instruction::Drop,
            Instruction::LocalGet(LocalIdx(0)),
            Instruction::LocalGet(LocalIdx(1)),
            Instruction::LocalGet(LocalIdx(2)),
            Instruction::Select(Some(vec![ValType::Num(NumType::Int32)])),
            Instruction::Drop,
            Instruction::LocalGet(LocalIdx(0)),
        ],
    }];

    let exports = vec![Export {
        name: "param".to_owned(),
        externidx: ExternIdx::Func(FuncIdx(0)),
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            exports,
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_table_instructions() {
    let f = File::open("tests/fixtures/table_instructions.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Table,
        SectionKind::Export,
        SectionKind::Element,
        SectionKind::Code,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 9,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 3,
        },
        SectionHeader {
            kind: SectionKind::Table,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Export,
            size: 13,
        },
        SectionHeader {
            kind: SectionKind::Element,
            size: 9,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 66,
        },
    ];

    let types = vec![
        FuncType {
            parameters: Vec::new(),
            results: Vec::new(),
        },
        FuncType {
            parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
            results: Vec::new(),
        },
    ];

    let funcs = vec![
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: Vec::new(),
        },
        Func {
            r#type: TypeIdx(1),
            locals: Vec::new(),
            body: vec![
                Instruction::LocalGet(LocalIdx(0)),
                Instruction::TableGet(TableIdx(0)),
                Instruction::Drop,
                Instruction::LocalGet(LocalIdx(0)),
                Instruction::RefNull(RefType::Func),
                Instruction::TableSet(TableIdx(0)),
                Instruction::I32Const(0),
                Instruction::I32Const(0),
                Instruction::I32Const(1),
                Instruction::TableInit(TableIdx(0), ElemIdx(1)),
                Instruction::ElemDrop(ElemIdx(1)),
                Instruction::I32Const(0),
                Instruction::RefNull(RefType::Func),
                Instruction::I32Const(1),
                Instruction::TableFill(TableIdx(0)),
                Instruction::TableSize(TableIdx(0)),
                Instruction::Drop,
                Instruction::RefNull(RefType::Func),
                Instruction::LocalGet(LocalIdx(1)),
                Instruction::TableGrow(TableIdx(0)),
                Instruction::Drop,
                Instruction::I32Const(0),
                Instruction::I32Const(0),
                Instruction::I32Const(1),
                Instruction::TableCopy(TableIdx(0), TableIdx(1)),
                Instruction::TableSize(TableIdx(0)),
                Instruction::Drop,
            ],
        },
    ];

    let tables = vec![
        TableType {
            limits: Limits {
                address_type: AddrType::I32,
                min: 4,
                max: None,
            },
            reftype: RefType::Func,
        },
        TableType {
            limits: Limits {
                address_type: AddrType::I32,
                min: 4,
                max: None,
            },
            reftype: RefType::Func,
        },
    ];

    let elems = vec![
        Elem {
            r#type: RefType::Func,
            init: vec![vec![Instruction::RefFunc(FuncIdx(0))]],
            mode: ElemMode::Passive,
        },
        Elem {
            r#type: RefType::Func,
            init: vec![vec![Instruction::RefFunc(FuncIdx(0))]],
            mode: ElemMode::Passive,
        },
    ];

    let exports = vec![Export {
        name: "table_ops".to_owned(),
        externidx: ExternIdx::Func(FuncIdx(1)),
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            tables,
            elems,
            exports,
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_memory_instructions() {
    let f = File::open("tests/fixtures/memory_instructions.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Memory,
        SectionKind::Export,
        SectionKind::DataCount,
        SectionKind::Code,
        SectionKind::Data,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 4,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 2,
        },
        SectionHeader {
            kind: SectionKind::Memory,
            size: 3,
        },
        SectionHeader {
            kind: SectionKind::Export,
            size: 20,
        },
        SectionHeader {
            kind: SectionKind::DataCount,
            size: 1,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 201,
        },
        SectionHeader {
            kind: SectionKind::Data,
            size: 46,
        },
    ];

    let types = vec![FuncType {
        parameters: Vec::new(),
        results: Vec::new(),
    }];

    let funcs = vec![Func {
        r#type: TypeIdx(0),
        locals: vec![
            ValType::Num(NumType::Float32),
            ValType::Num(NumType::Float64),
        ],
        body: vec![
            Instruction::I32Const(0),
            Instruction::I32Load(Memarg {
                mem_idx: MemIdx(0),
                align: 2,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load(Memarg {
                mem_idx: MemIdx(0),
                align: 3,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::F32Load(Memarg {
                mem_idx: MemIdx(0),
                align: 2,
                offset: 0,
            }),
            Instruction::LocalTee(LocalIdx(0)),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::F64Load(Memarg {
                mem_idx: MemIdx(0),
                align: 3,
                offset: 0,
            }),
            Instruction::LocalTee(LocalIdx(1)),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I32Load8s(Memarg {
                mem_idx: MemIdx(0),
                align: 0,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I32Load8u(Memarg {
                mem_idx: MemIdx(0),
                align: 0,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I32Load16s(Memarg {
                mem_idx: MemIdx(0),
                align: 1,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I32Load16u(Memarg {
                mem_idx: MemIdx(0),
                align: 1,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load8s(Memarg {
                mem_idx: MemIdx(0),
                align: 0,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load8u(Memarg {
                mem_idx: MemIdx(0),
                align: 0,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load16s(Memarg {
                mem_idx: MemIdx(0),
                align: 1,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load16u(Memarg {
                mem_idx: MemIdx(0),
                align: 1,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load32s(Memarg {
                mem_idx: MemIdx(0),
                align: 2,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load32u(Memarg {
                mem_idx: MemIdx(0),
                align: 2,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(4),
            Instruction::I32Const(1),
            Instruction::I32Store(Memarg {
                mem_idx: MemIdx(0),
                align: 2,
                offset: 0,
            }),
            Instruction::I32Const(8),
            Instruction::I64Const(2),
            Instruction::I64Store(Memarg {
                mem_idx: MemIdx(0),
                align: 3,
                offset: 0,
            }),
            Instruction::I32Const(16),
            Instruction::LocalGet(LocalIdx(0)),
            Instruction::F32Store(Memarg {
                mem_idx: MemIdx(0),
                align: 2,
                offset: 0,
            }),
            Instruction::I32Const(24),
            Instruction::LocalGet(LocalIdx(1)),
            Instruction::F64Store(Memarg {
                mem_idx: MemIdx(0),
                align: 3,
                offset: 0,
            }),
            Instruction::I32Const(32),
            Instruction::I32Const(5),
            Instruction::I32Store8(Memarg {
                mem_idx: MemIdx(0),
                align: 0,
                offset: 0,
            }),
            Instruction::I32Const(34),
            Instruction::I32Const(6),
            Instruction::I32Store16(Memarg {
                mem_idx: MemIdx(0),
                align: 1,
                offset: 0,
            }),
            Instruction::I32Const(36),
            Instruction::I64Const(7),
            Instruction::I64Store8(Memarg {
                mem_idx: MemIdx(0),
                align: 0,
                offset: 0,
            }),
            Instruction::I32Const(38),
            Instruction::I64Const(8),
            Instruction::I64Store16(Memarg {
                mem_idx: MemIdx(0),
                align: 1,
                offset: 0,
            }),
            Instruction::I32Const(40),
            Instruction::I64Const(9),
            Instruction::I64Store32(Memarg {
                mem_idx: MemIdx(0),
                align: 2,
                offset: 0,
            }),
            Instruction::MemorySize(MemIdx(0)),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::MemoryGrow(MemIdx(0)),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I32Const(0),
            Instruction::I32Const(4),
            Instruction::MemoryInit(MemIdx(0), DataIdx(1)),
            Instruction::DataDrop(DataIdx(1)),
            Instruction::I32Const(8),
            Instruction::I32Const(0),
            Instruction::I32Const(4),
            Instruction::MemoryCopy(MemIdx(0), MemIdx(0)),
            Instruction::I32Const(12),
            Instruction::I32Const(255),
            Instruction::I32Const(4),
            Instruction::MemoryFill(MemIdx(0)),
        ],
    }];

    let mems = vec![MemType {
        limits: Limits {
            address_type: AddrType::I32,
            min: 1,
            max: None,
        },
    }];

    let exports = vec![
        Export {
            name: "mem".to_owned(),
            externidx: ExternIdx::Mem(MemIdx(0)),
        },
        Export {
            name: "use-memory".to_owned(),
            externidx: ExternIdx::Func(FuncIdx(0)),
        },
    ];

    let datas = vec![
        Data {
            init: vec![
                0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D,
                0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B,
                0x1C, 0x1D, 0x1E, 0x1F,
            ],
            mode: DataMode::Active {
                memory: MemIdx(0),
                offset: vec![Instruction::I32Const(0)],
            },
        },
        Data {
            init: vec![0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF],
            mode: DataMode::Passive,
        },
    ];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            mems,
            datas,
            exports,
            data_count: Some(2),
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_multi_memory_immediates() {
    let f = File::open("tests/fixtures/memory_instructions_multi_memidx.wasm").unwrap();

    let module = decode_module(f).unwrap();
    assert_eq!(module.mems.len(), 2);
    assert_eq!(
        module.mems,
        vec![
            MemType {
                limits: Limits {
                    address_type: AddrType::I32,
                    min: 1,
                    max: None,
                },
            },
            MemType {
                limits: Limits {
                    address_type: AddrType::I32,
                    min: 1,
                    max: None,
                },
            },
        ],
    );

    assert_eq!(module.funcs.len(), 1);
    let func = &module.funcs[0];
    let expected_body = vec![
        Instruction::I32Const(0),
        Instruction::I32Load(Memarg {
            mem_idx: MemIdx(1),
            align: 2,
            offset: 0,
        }),
        Instruction::Drop,
        Instruction::MemorySize(MemIdx(1)),
        Instruction::Drop,
        Instruction::I32Const(1),
        Instruction::MemoryGrow(MemIdx(1)),
        Instruction::Drop,
        Instruction::I32Const(0),
        Instruction::I32Const(0),
        Instruction::I32Const(2),
        Instruction::MemoryInit(MemIdx(1), DataIdx(0)),
        Instruction::I32Const(0),
        Instruction::I32Const(0),
        Instruction::I32Const(2),
        Instruction::MemoryCopy(MemIdx(1), MemIdx(0)),
        Instruction::I32Const(0),
        Instruction::I32Const(7),
        Instruction::I32Const(2),
        Instruction::MemoryFill(MemIdx(1)),
    ];
    assert_eq!(func.body, expected_body);

    assert_eq!(module.datas.len(), 1);
    assert_eq!(module.datas[0].init, b"hi".to_vec());
    assert_eq!(module.datas[0].mode, DataMode::Passive);
}

#[test]
// # spec version: 3
fn it_decodes_multi_memory_kitchensink() {
    let f = File::open("tests/fixtures/multi_memory_kitchensink.wasm").unwrap();
    let module = decode_module(f).unwrap();

    assert_eq!(module.mems.len(), 2);
    assert_eq!(
        module.mems,
        vec![
            MemType {
                limits: Limits {
                    address_type: AddrType::I32,
                    min: 1,
                    max: None,
                },
            },
            MemType {
                limits: Limits {
                    address_type: AddrType::I32,
                    min: 1,
                    max: None,
                },
            },
        ]
    );
    assert_eq!(module.data_count, Some(3));

    assert_eq!(module.datas.len(), 3);
    assert_eq!(module.datas[0].init, b"A".to_vec());
    assert_eq!(
        module.datas[0].mode,
        DataMode::Active {
            memory: MemIdx(0),
            offset: vec![Instruction::I32Const(0)],
        }
    );
    assert_eq!(module.datas[1].init, b"BC".to_vec());
    assert_eq!(module.datas[1].mode, DataMode::Passive);
    assert_eq!(module.datas[2].init, b"DEF".to_vec());
    assert_eq!(
        module.datas[2].mode,
        DataMode::Active {
            memory: MemIdx(1),
            offset: vec![Instruction::I32Const(4)],
        }
    );

    assert_eq!(module.funcs.len(), 1);
    let func = &module.funcs[0];
    let expected_body = vec![
        Instruction::I32Const(0),
        Instruction::I32Load(Memarg {
            mem_idx: MemIdx(0),
            align: 2,
            offset: 0,
        }),
        Instruction::Drop,
        Instruction::I32Const(0),
        Instruction::I32Load(Memarg {
            mem_idx: MemIdx(1),
            align: 2,
            offset: 0,
        }),
        Instruction::Drop,
        Instruction::I32Const(0),
        Instruction::I64Const(1),
        Instruction::I64Store(Memarg {
            mem_idx: MemIdx(1),
            align: 3,
            offset: 0,
        }),
        Instruction::I32Const(0),
        Instruction::I32Load(Memarg {
            mem_idx: MemIdx(0),
            align: 0,
            offset: 16,
        }),
        Instruction::Drop,
        Instruction::MemorySize(MemIdx(0)),
        Instruction::Drop,
        Instruction::MemorySize(MemIdx(1)),
        Instruction::Drop,
        Instruction::I32Const(0),
        Instruction::MemoryGrow(MemIdx(0)),
        Instruction::Drop,
        Instruction::I32Const(0),
        Instruction::MemoryGrow(MemIdx(1)),
        Instruction::Drop,
        Instruction::I32Const(0),
        Instruction::I32Const(0),
        Instruction::I32Const(1),
        Instruction::MemoryInit(MemIdx(0), DataIdx(1)),
        Instruction::DataDrop(DataIdx(1)),
        Instruction::I32Const(0),
        Instruction::I32Const(0),
        Instruction::I32Const(1),
        Instruction::MemoryInit(MemIdx(1), DataIdx(1)),
        Instruction::I32Const(0),
        Instruction::I32Const(0),
        Instruction::I32Const(1),
        Instruction::MemoryCopy(MemIdx(0), MemIdx(1)),
        Instruction::I32Const(0),
        Instruction::I32Const(0),
        Instruction::I32Const(1),
        Instruction::MemoryCopy(MemIdx(1), MemIdx(0)),
        Instruction::I32Const(0),
        Instruction::I32Const(255),
        Instruction::I32Const(1),
        Instruction::MemoryFill(MemIdx(0)),
        Instruction::I32Const(0),
        Instruction::I32Const(7),
        Instruction::I32Const(1),
        Instruction::MemoryFill(MemIdx(1)),
    ];
    assert_eq!(func.body, expected_body);
}

#[test]
fn it_accepts_export_with_locals() {
    // single func with 2 i32 locals; exported "add_locals"
    // Body: local.get 0, local.get 1, i32.add, end.
    let f = File::open("tests/fixtures/with_locals_exported.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Function,
        SectionKind::Export,
        SectionKind::Code,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Type,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 2,
        },
        SectionHeader {
            kind: SectionKind::Export,
            size: 14,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 11,
        },
    ];

    let types = vec![FuncType {
        parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
        results: vec![ValType::Num(NumType::Int32)],
    }];

    let funcs = vec![Func {
        r#type: TypeIdx(0),
        locals: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
        body: vec![
            Instruction::LocalGet(LocalIdx(0)),
            Instruction::LocalGet(LocalIdx(1)),
            Instruction::I32Add,
        ],
    }];

    let exports = vec![Export {
        name: "add_locals".to_owned(),
        externidx: ExternIdx::Func(FuncIdx(0)),
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            types,
            funcs,
            exports,
            ..Default::default()
        }
    )
}

#[test]
fn it_accepts_kitchensink() {
    let f = File::open("./tests/fixtures/kitchensink.wasm").unwrap();

    let custom_sections = vec![CustomSection {
        name: "note".to_owned(),
        contents: vec![0x68, 0x69], // hi
    }];

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Import,
        SectionKind::Function,
        SectionKind::Table,
        SectionKind::Memory,
        SectionKind::Global,
        SectionKind::Export,
        SectionKind::Start,
        SectionKind::Element,
        SectionKind::Code,
        SectionKind::Data,
    ];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Custom,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Type,
            size: 10,
        },
        SectionHeader {
            kind: SectionKind::Import,
            size: 16,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 3,
        },
        SectionHeader {
            kind: SectionKind::Table,
            size: 4,
        },
        SectionHeader {
            kind: SectionKind::Memory,
            size: 3,
        },
        SectionHeader {
            kind: SectionKind::Global,
            size: 6,
        },
        SectionHeader {
            kind: SectionKind::Export,
            size: 24,
        },
        SectionHeader {
            kind: SectionKind::Start,
            size: 1,
        },
        SectionHeader {
            kind: SectionKind::Element,
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 14,
        },
        SectionHeader {
            kind: SectionKind::Data,
            size: 10,
        },
    ];

    let types = vec![
        FuncType {
            parameters: Vec::new(),
            results: Vec::new(),
        },
        FuncType {
            parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
            results: vec![ValType::Num(NumType::Int32)],
        },
    ];

    let funcs = vec![
        Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![Instruction::Call(FuncIdx(0))],
        },
        Func {
            r#type: TypeIdx(1),
            locals: Vec::new(),
            body: vec![
                Instruction::LocalGet(LocalIdx(0)),
                Instruction::LocalGet(LocalIdx(1)),
                Instruction::I32Add,
            ],
        },
    ];

    let tables = vec![TableType {
        limits: Limits {
            address_type: AddrType::I32,
            min: 1,
            max: None,
        },
        reftype: RefType::Func,
    }];

    let mems = vec![MemType {
        limits: Limits {
            address_type: AddrType::I32,
            min: 1,
            max: None,
        },
    }];

    let globals = vec![Global {
        r#type: GlobalType(Mut::Var, ValType::Num(NumType::Int32)),
        init: vec![Instruction::I32Const(42)],
    }];

    let elems = vec![Elem {
        r#type: RefType::Func,
        init: vec![vec![Instruction::RefFunc(FuncIdx(2))]],
        mode: ElemMode::Active {
            table: TableIdx(0),
            offset: vec![Instruction::I32Const(0)],
        },
    }];

    let datas = vec![Data {
        init: vec![0, 17, 34, 51],
        mode: DataMode::Active {
            memory: MemIdx(0),
            offset: vec![Instruction::I32Const(0)],
        },
    }];

    let imports = vec![Import {
        module_name: "env".to_owned(),
        item_name: "impstart".to_owned(),
        extern_type: ExternType::Func(TypeIdx(0)),
    }];

    let exports = vec![
        Export {
            name: "add".to_owned(),
            externidx: ExternIdx::Func(FuncIdx(2)),
        },
        Export {
            name: "mem".to_owned(),
            externidx: ExternIdx::Mem(MemIdx(0)),
        },
        Export {
            name: "tab".to_owned(),
            externidx: ExternIdx::Table(TableIdx(0)),
        },
        Export {
            name: "g0".to_owned(),
            externidx: ExternIdx::Global(GlobalIdx(0)),
        },
    ];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            custom_sections,
            types,
            funcs,
            tables,
            mems,
            globals,
            elems,
            datas,
            imports,
            exports,
            start: Some(FuncIdx(1)),
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_data_section_multiple_segments() {
    let f = File::open("tests/fixtures/data_section_multi_segment.wasm").unwrap();

    let parsed_section_kinds = vec![SectionKind::Memory, SectionKind::Data];
    let section_headers = vec![
        SectionHeader {
            kind: SectionKind::Memory,
            size: 3,
        },
        SectionHeader {
            kind: SectionKind::Data,
            size: 14,
        },
    ];

    let mems = vec![MemType {
        limits: Limits {
            address_type: AddrType::I32,
            min: 1,
            max: None,
        },
    }];

    let datas = vec![
        Data {
            init: vec![0x41],
            mode: DataMode::Active {
                memory: MemIdx(0),
                offset: vec![Instruction::I32Const(0)],
            },
        },
        Data {
            init: vec![0x42],
            mode: DataMode::Active {
                memory: MemIdx(0),
                offset: vec![Instruction::I32Const(1)],
            },
        },
    ];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            mems,
            datas,
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_numeric_instructions() {
    let f = File::open("./tests/fixtures/numeric_instructions.wasm").unwrap();

    use std::mem::discriminant;

    let module = decode_module(f).unwrap();
    let mut seen = HashSet::new();

    for func in &module.funcs {
        for instr in &func.body {
            match instr {
                Instruction::I32Const(_)
                | Instruction::I64Const(_)
                | Instruction::F32Const(_)
                | Instruction::F64Const(_)
                | Instruction::I32Eqz
                | Instruction::I32Eq
                | Instruction::I32Ne
                | Instruction::I32LtS
                | Instruction::I32LtU
                | Instruction::I32GtS
                | Instruction::I32GtU
                | Instruction::I32LeS
                | Instruction::I32LeU
                | Instruction::I32GeS
                | Instruction::I32GeU
                | Instruction::I64Eqz
                | Instruction::I64Eq
                | Instruction::I64Ne
                | Instruction::I64LtS
                | Instruction::I64LtU
                | Instruction::I64GtS
                | Instruction::I64GtU
                | Instruction::I64LeS
                | Instruction::I64LeU
                | Instruction::I64GeS
                | Instruction::I64GeU
                | Instruction::F32Eq
                | Instruction::F32Ne
                | Instruction::F32Lt
                | Instruction::F32Gt
                | Instruction::F32Le
                | Instruction::F32Ge
                | Instruction::F64Eq
                | Instruction::F64Ne
                | Instruction::F64Lt
                | Instruction::F64Gt
                | Instruction::F64Le
                | Instruction::F64Ge
                | Instruction::I32Clz
                | Instruction::I32Ctz
                | Instruction::I32Popcnt
                | Instruction::I32Add
                | Instruction::I32Sub
                | Instruction::I32Mul
                | Instruction::I32DivS
                | Instruction::I32DivU
                | Instruction::I32RemS
                | Instruction::I32RemU
                | Instruction::I32And
                | Instruction::I32Or
                | Instruction::I32Xor
                | Instruction::I32Shl
                | Instruction::I32ShrS
                | Instruction::I32ShrU
                | Instruction::I32Rotl
                | Instruction::I32Rotr
                | Instruction::I64Clz
                | Instruction::I64Ctz
                | Instruction::I64Popcnt
                | Instruction::I64Add
                | Instruction::I64Sub
                | Instruction::I64Mul
                | Instruction::I64DivS
                | Instruction::I64DivU
                | Instruction::I64RemS
                | Instruction::I64RemU
                | Instruction::I64And
                | Instruction::I64Or
                | Instruction::I64Xor
                | Instruction::I64Shl
                | Instruction::I64ShrS
                | Instruction::I64ShrU
                | Instruction::I64Rotl
                | Instruction::I64Rotr
                | Instruction::F32Abs
                | Instruction::F32Neg
                | Instruction::F32Ceil
                | Instruction::F32Floor
                | Instruction::F32Trunc
                | Instruction::F32Nearest
                | Instruction::F32Sqrt
                | Instruction::F32Add
                | Instruction::F32Sub
                | Instruction::F32Mul
                | Instruction::F32Div
                | Instruction::F32Min
                | Instruction::F32Max
                | Instruction::F32Copysign
                | Instruction::F64Abs
                | Instruction::F64Neg
                | Instruction::F64Ceil
                | Instruction::F64Floor
                | Instruction::F64Trunc
                | Instruction::F64Nearest
                | Instruction::F64Sqrt
                | Instruction::F64Add
                | Instruction::F64Sub
                | Instruction::F64Mul
                | Instruction::F64Div
                | Instruction::F64Min
                | Instruction::F64Max
                | Instruction::F64Copysign
                | Instruction::I32WrapI64
                | Instruction::I32TruncF32S
                | Instruction::I32TruncF32U
                | Instruction::I32TruncF64S
                | Instruction::I32TruncF64U
                | Instruction::I64ExtendI32S
                | Instruction::I64ExtendI32U
                | Instruction::I64TruncF32S
                | Instruction::I64TruncF32U
                | Instruction::I64TruncF64S
                | Instruction::I64TruncF64U
                | Instruction::F32ConvertI32S
                | Instruction::F32ConvertI32U
                | Instruction::F32ConvertI64S
                | Instruction::F32ConvertI64U
                | Instruction::F32DemoteF64
                | Instruction::F64ConvertI32S
                | Instruction::F64ConvertI32U
                | Instruction::F64ConvertI64S
                | Instruction::F64ConvertI64U
                | Instruction::F64PromoteF32
                | Instruction::I32ReinterpretF32
                | Instruction::I64ReinterpretF64
                | Instruction::F32ReinterpretI32
                | Instruction::F64ReinterpretI64
                | Instruction::I32Extend8S
                | Instruction::I32Extend16S
                | Instruction::I64Extend8S
                | Instruction::I64Extend16S
                | Instruction::I64Extend32S
                | Instruction::I32TruncSatF32S
                | Instruction::I32TruncSatF32U
                | Instruction::I32TruncSatF64S
                | Instruction::I32TruncSatF64U
                | Instruction::I64TruncSatF32S
                | Instruction::I64TruncSatF32U
                | Instruction::I64TruncSatF64S
                | Instruction::I64TruncSatF64U => {
                    seen.insert(discriminant(instr));
                }
                _ => {}
            }
        }
    }

    let expected: HashSet<_> = [
        discriminant(&Instruction::I32Const(0)),
        discriminant(&Instruction::I64Const(0)),
        discriminant(&Instruction::F32Const(0.0_f32)),
        discriminant(&Instruction::F64Const(0.0)),
        discriminant(&Instruction::I32Eqz),
        discriminant(&Instruction::I32Eq),
        discriminant(&Instruction::I32Ne),
        discriminant(&Instruction::I32LtS),
        discriminant(&Instruction::I32LtU),
        discriminant(&Instruction::I32GtS),
        discriminant(&Instruction::I32GtU),
        discriminant(&Instruction::I32LeS),
        discriminant(&Instruction::I32LeU),
        discriminant(&Instruction::I32GeS),
        discriminant(&Instruction::I32GeU),
        discriminant(&Instruction::I64Eqz),
        discriminant(&Instruction::I64Eq),
        discriminant(&Instruction::I64Ne),
        discriminant(&Instruction::I64LtS),
        discriminant(&Instruction::I64LtU),
        discriminant(&Instruction::I64GtS),
        discriminant(&Instruction::I64GtU),
        discriminant(&Instruction::I64LeS),
        discriminant(&Instruction::I64LeU),
        discriminant(&Instruction::I64GeS),
        discriminant(&Instruction::I64GeU),
        discriminant(&Instruction::F32Eq),
        discriminant(&Instruction::F32Ne),
        discriminant(&Instruction::F32Lt),
        discriminant(&Instruction::F32Gt),
        discriminant(&Instruction::F32Le),
        discriminant(&Instruction::F32Ge),
        discriminant(&Instruction::F64Eq),
        discriminant(&Instruction::F64Ne),
        discriminant(&Instruction::F64Lt),
        discriminant(&Instruction::F64Gt),
        discriminant(&Instruction::F64Le),
        discriminant(&Instruction::F64Ge),
        discriminant(&Instruction::I32Clz),
        discriminant(&Instruction::I32Ctz),
        discriminant(&Instruction::I32Popcnt),
        discriminant(&Instruction::I32Add),
        discriminant(&Instruction::I32Sub),
        discriminant(&Instruction::I32Mul),
        discriminant(&Instruction::I32DivS),
        discriminant(&Instruction::I32DivU),
        discriminant(&Instruction::I32RemS),
        discriminant(&Instruction::I32RemU),
        discriminant(&Instruction::I32And),
        discriminant(&Instruction::I32Or),
        discriminant(&Instruction::I32Xor),
        discriminant(&Instruction::I32Shl),
        discriminant(&Instruction::I32ShrS),
        discriminant(&Instruction::I32ShrU),
        discriminant(&Instruction::I32Rotl),
        discriminant(&Instruction::I32Rotr),
        discriminant(&Instruction::I64Clz),
        discriminant(&Instruction::I64Ctz),
        discriminant(&Instruction::I64Popcnt),
        discriminant(&Instruction::I64Add),
        discriminant(&Instruction::I64Sub),
        discriminant(&Instruction::I64Mul),
        discriminant(&Instruction::I64DivS),
        discriminant(&Instruction::I64DivU),
        discriminant(&Instruction::I64RemS),
        discriminant(&Instruction::I64RemU),
        discriminant(&Instruction::I64And),
        discriminant(&Instruction::I64Or),
        discriminant(&Instruction::I64Xor),
        discriminant(&Instruction::I64Shl),
        discriminant(&Instruction::I64ShrS),
        discriminant(&Instruction::I64ShrU),
        discriminant(&Instruction::I64Rotl),
        discriminant(&Instruction::I64Rotr),
        discriminant(&Instruction::F32Abs),
        discriminant(&Instruction::F32Neg),
        discriminant(&Instruction::F32Ceil),
        discriminant(&Instruction::F32Floor),
        discriminant(&Instruction::F32Trunc),
        discriminant(&Instruction::F32Nearest),
        discriminant(&Instruction::F32Sqrt),
        discriminant(&Instruction::F32Add),
        discriminant(&Instruction::F32Sub),
        discriminant(&Instruction::F32Mul),
        discriminant(&Instruction::F32Div),
        discriminant(&Instruction::F32Min),
        discriminant(&Instruction::F32Max),
        discriminant(&Instruction::F32Copysign),
        discriminant(&Instruction::F64Abs),
        discriminant(&Instruction::F64Neg),
        discriminant(&Instruction::F64Ceil),
        discriminant(&Instruction::F64Floor),
        discriminant(&Instruction::F64Trunc),
        discriminant(&Instruction::F64Nearest),
        discriminant(&Instruction::F64Sqrt),
        discriminant(&Instruction::F64Add),
        discriminant(&Instruction::F64Sub),
        discriminant(&Instruction::F64Mul),
        discriminant(&Instruction::F64Div),
        discriminant(&Instruction::F64Min),
        discriminant(&Instruction::F64Max),
        discriminant(&Instruction::F64Copysign),
        discriminant(&Instruction::I32WrapI64),
        discriminant(&Instruction::I32TruncF32S),
        discriminant(&Instruction::I32TruncF32U),
        discriminant(&Instruction::I32TruncF64S),
        discriminant(&Instruction::I32TruncF64U),
        discriminant(&Instruction::I64ExtendI32S),
        discriminant(&Instruction::I64ExtendI32U),
        discriminant(&Instruction::I64TruncF32S),
        discriminant(&Instruction::I64TruncF32U),
        discriminant(&Instruction::I64TruncF64S),
        discriminant(&Instruction::I64TruncF64U),
        discriminant(&Instruction::F32ConvertI32S),
        discriminant(&Instruction::F32ConvertI32U),
        discriminant(&Instruction::F32ConvertI64S),
        discriminant(&Instruction::F32ConvertI64U),
        discriminant(&Instruction::F32DemoteF64),
        discriminant(&Instruction::F64ConvertI32S),
        discriminant(&Instruction::F64ConvertI32U),
        discriminant(&Instruction::F64ConvertI64S),
        discriminant(&Instruction::F64ConvertI64U),
        discriminant(&Instruction::F64PromoteF32),
        discriminant(&Instruction::I32ReinterpretF32),
        discriminant(&Instruction::I64ReinterpretF64),
        discriminant(&Instruction::F32ReinterpretI32),
        discriminant(&Instruction::F64ReinterpretI64),
        discriminant(&Instruction::I32Extend8S),
        discriminant(&Instruction::I32Extend16S),
        discriminant(&Instruction::I64Extend8S),
        discriminant(&Instruction::I64Extend16S),
        discriminant(&Instruction::I64Extend32S),
        discriminant(&Instruction::I32TruncSatF32S),
        discriminant(&Instruction::I32TruncSatF32U),
        discriminant(&Instruction::I32TruncSatF64S),
        discriminant(&Instruction::I32TruncSatF64U),
        discriminant(&Instruction::I64TruncSatF32S),
        discriminant(&Instruction::I64TruncSatF32U),
        discriminant(&Instruction::I64TruncSatF64S),
        discriminant(&Instruction::I64TruncSatF64U),
    ]
    .into_iter()
    .collect();

    assert_eq!(seen, expected);
}

#[test]
fn it_decodes_vector_instructions() {
    let f = File::open("./tests/fixtures/vector_instructions.wasm").unwrap();

    let module = decode_module(f).unwrap();

    assert_eq!(
        module.parsed_section_kinds,
        vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Memory,
            SectionKind::Export,
            SectionKind::Code,
            SectionKind::Data,
        ]
    );

    assert_eq!(
        module.section_headers,
        vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 4,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 2,
            },
            SectionHeader {
                kind: SectionKind::Memory,
                size: 3,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 15,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 204,
            },
            SectionHeader {
                kind: SectionKind::Data,
                size: 70,
            },
        ]
    );

    assert_eq!(
        module.types,
        vec![FuncType {
            parameters: Vec::new(),
            results: Vec::new(),
        }]
    );

    assert_eq!(
        module.mems,
        vec![MemType {
            limits: Limits {
                address_type: AddrType::I32,
                min: 1,
                max: None,
            },
        }]
    );

    assert_eq!(
        module.exports,
        vec![Export {
            name: "use_vectors".to_owned(),
            externidx: ExternIdx::Func(FuncIdx(0)),
        }]
    );

    assert_eq!(module.funcs.len(), 1);
    let func = &module.funcs[0];
    assert_eq!(func.r#type, TypeIdx(0));
    assert!(func.locals.is_empty());
    assert_eq!(func.body.len(), 26);
    let expected_body = vec![
        Instruction::I32Const(0),
        Instruction::V128Load(Memarg {
            mem_idx: MemIdx(0),
            align: 4,
            offset: 0,
        }),
        Instruction::I32Const(16),
        Instruction::V128Load(Memarg {
            mem_idx: MemIdx(0),
            align: 4,
            offset: 0,
        }),
        Instruction::I8x16Add,
        Instruction::V128Const([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]),
        Instruction::V128Const([
            16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
        ]),
        Instruction::V128Bitselect,
        Instruction::Drop,
        Instruction::V128Const([
            32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
        ]),
        Instruction::I8x16ExtractLaneS(LaneIdx(0)),
        Instruction::Drop,
        Instruction::V128Const([
            48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
        ]),
        Instruction::I32Const(7),
        Instruction::I8x16ReplaceLane(LaneIdx(5)),
        Instruction::Drop,
        Instruction::V128Const([
            64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
        ]),
        Instruction::V128Const([
            80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
        ]),
        Instruction::I8x16Shuffle([
            LaneIdx(0),
            LaneIdx(16),
            LaneIdx(1),
            LaneIdx(17),
            LaneIdx(2),
            LaneIdx(18),
            LaneIdx(3),
            LaneIdx(19),
            LaneIdx(4),
            LaneIdx(20),
            LaneIdx(5),
            LaneIdx(21),
            LaneIdx(6),
            LaneIdx(22),
            LaneIdx(7),
            LaneIdx(23),
        ]),
        Instruction::Drop,
        Instruction::V128Const([
            96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
        ]),
        Instruction::V128AnyTrue,
        Instruction::Drop,
        Instruction::I32Const(0),
        Instruction::V128Const([
            112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
        ]),
        Instruction::V128Store(Memarg {
            mem_idx: MemIdx(0),
            align: 4,
            offset: 0,
        }),
    ];
    assert_eq!(func.body, expected_body);

    assert_eq!(module.datas.len(), 1);
    let data = &module.datas[0];
    assert_eq!(data.init, (0..64).map(|b| b as u8).collect::<Vec<_>>());
    match &data.mode {
        DataMode::Active { memory, offset } => {
            assert_eq!(*memory, MemIdx(0));
            assert_eq!(offset, &vec![Instruction::I32Const(0)]);
        }
        other => panic!("unexpected data mode: {:?}", other),
    }
}

#[test]
fn it_respects_mem_limits() {
    let f = File::open("tests/fixtures/mem_limits_bug.wasm").unwrap();

    let section_headers = vec![SectionHeader {
        kind: SectionKind::Memory,
        size: 5,
    }];

    let mems = vec![MemType {
        limits: Limits {
            address_type: AddrType::I32,
            min: 1,
            max: Some(129),
        },
    }];

    assert_eq!(
        decode_module(f).unwrap(),
        Module {
            parsed_section_kinds: vec![SectionKind::Memory],
            section_headers,
            mems,
            ..Default::default()
        }
    )
}

#[test]
fn it_decodes_memory64_limits_flags() {
    let module =
        decode_module(File::open("tests/fixtures/memory64_limits_min.wasm").unwrap()).unwrap();
    assert_eq!(
        module.mems,
        vec![MemType {
            limits: Limits {
                address_type: AddrType::I64,
                min: 1,
                max: None,
            },
        }]
    );

    let module =
        decode_module(File::open("tests/fixtures/memory64_limits_min_max.wasm").unwrap()).unwrap();
    assert_eq!(
        module.mems,
        vec![MemType {
            limits: Limits {
                address_type: AddrType::I64,
                min: 1,
                max: Some(2),
            },
        }]
    );
}

#[test]
// # spec version: 3
fn it_decodes_memory64_limits_large_values() {
    let module =
        decode_module(File::open("tests/fixtures/memory64_limits_large.wasm").unwrap()).unwrap();
    assert_eq!(
        module.mems,
        vec![MemType {
            limits: Limits {
                address_type: AddrType::I64,
                min: 1u64 << 32,
                max: Some((1u64 << 32) + 5),
            },
        }]
    );
}

#[test]
fn it_fails_on_code_size_mismatch() {
    let f = File::open("tests/fixtures/code_section_size_underreported.wasm").unwrap();
    let err = decode_module(f).expect_err("underreported code section should fail");
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

    let f = File::open("tests/fixtures/custom_section_size_overreported.wasm").unwrap();
    let err = decode_module(f).expect_err("overreported custom section should fail");
    match err {
        DecodeModuleError::SectionSizeMismatch {
            section_kind,
            declared,
            got,
        } => {
            assert_eq!(section_kind, SectionKind::Custom);
            assert_eq!(declared, 5);
            assert_eq!(got, 3);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn it_rejects_overlong_type_index_encoding() {
    // Construct a minimal module containing a function section whose type index
    // is encoded with five continuation bytes, which exceeds the allowed
    // length for a u32 LEB128.
    let module: &[u8] = &[
        0x00, 0x61, 0x73, 0x6D, // magic
        0x01, 0x00, 0x00, 0x00, // version
        0x01, 0x04, // type section id + size
        0x01, // type vector length
        0x60, // functype tag
        0x00, // param count
        0x00, // result count
        0x03, 0x06, // function section id + size
        0x01, // function count
        0x80, 0x80, 0x80, 0x80, 0x80, // overlong type index encoding
    ];

    let err = decode_module(module).expect_err("module should fail while reading type index");

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
