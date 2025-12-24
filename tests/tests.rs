use pretty_assertions::assert_eq;
use std::collections::BTreeSet;
use std::fs::File;
use wadec::decode::DecodeVectorError;
use wadec::decode::sections::{
    custom::CustomSection,
    data::{Data, DataMode},
    element::{Elem, ElemMode},
    export::{Export, ExportDesc},
    function::Func,
    global::Global,
    import::{Import, ImportDesc},
    r#type::DecodeTypeSectionError,
};
use wadec::decode::types::{DecodeFuncTypeError, DecodeResultTypeError, DecodeValTypeError};
use wadec::indices::*;
use wadec::types::{
    functype::FuncType,
    globaltype::{GlobalType, Mut},
    limits::Limits,
    memtype::MemType,
    numtype::NumType,
    reftype::RefType,
    tabletype::TableType,
    valtype::ValType,
};
use wadec::*;

#[allow(dead_code)]
#[test]
fn it_parses_preamble() {
    let mut input: &[u8] = &[];
    assert!(decode(input).is_err());

    input = &[0xD3, 0xAD, 0xBE, 0xEF];
    assert!(decode(input).is_err());

    input = &[0xD3, 0xAD, 0xBE, 0xEF, 0x00, 0x00, 0x00, 0x00];
    assert!(decode(input).is_err());

    // just the preamble
    input = &[0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00];
    assert_eq!(decode(input).unwrap(), Module::default());
}

#[test]
fn it_accepts_empty_module() {
    // (module)
    let f = File::open("./tests/fixtures/empty.wasm").unwrap();

    assert_eq!(decode(f).unwrap(), Module::default(),)
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
        desc: ExportDesc::Func(FuncIdx(0)),
    }];

    assert_eq!(
        decode(f).unwrap(),
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
        desc: ExportDesc::Func(FuncIdx(1)),
    }];

    assert_eq!(
        decode(f).unwrap(),
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
            module: "env".to_owned(),
            name: "table".to_owned(),
            desc: ImportDesc::Table(TableType {
                limits: Limits { min: 1, max: None },
                reftype: RefType::Func,
            }),
        },
        Import {
            module: "env".to_owned(),
            name: "memory".to_owned(),
            desc: ImportDesc::Mem(MemType {
                limits: Limits { min: 1, max: None },
            }),
        },
        Import {
            module: "env".to_owned(),
            name: "global_i".to_owned(),
            desc: ImportDesc::Global(GlobalType(Mut::Const, ValType::Num(NumType::Int32))),
        },
        Import {
            module: "env".to_owned(),
            name: "global_mut".to_owned(),
            desc: ImportDesc::Global(GlobalType(Mut::Var, ValType::Num(NumType::Int64))),
        },
    ];

    assert_eq!(
        decode(f).unwrap(),
        Module {
            parsed_section_kinds: vec![SectionKind::Import],
            section_headers,
            imports,
            ..Default::default()
        }
    )
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
        decode(f).unwrap(),
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
        decode(f).unwrap(),
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
        limits: Limits { min: 1, max: None },
        reftype: RefType::Func,
    }];

    let exports = vec![Export {
        name: "control".to_owned(),
        desc: ExportDesc::Func(FuncIdx(1)),
    }];

    assert_eq!(
        decode(f).unwrap(),
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
            limits: Limits { min: 12, max: None },
            reftype: RefType::Func,
        },
        TableType {
            limits: Limits { min: 12, max: None },
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
        decode(f).unwrap(),
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
        limits: Limits { min: 1, max: None },
        reftype: RefType::Func,
    }];

    let exports = vec![Export {
        name: "refs".to_owned(),
        desc: ExportDesc::Func(FuncIdx(1)),
    }];

    let elems = vec![Elem {
        r#type: RefType::Func,
        init: vec![vec![Instruction::RefFunc(FuncIdx(0))]],
        mode: ElemMode::Declarative,
    }];

    assert_eq!(
        decode(f).unwrap(),
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
        desc: ExportDesc::Func(FuncIdx(0)),
    }];

    assert_eq!(
        decode(f).unwrap(),
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
        desc: ExportDesc::Func(FuncIdx(0)),
    }];

    assert_eq!(
        decode(f).unwrap(),
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
            limits: Limits { min: 4, max: None },
            reftype: RefType::Func,
        },
        TableType {
            limits: Limits { min: 4, max: None },
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
        desc: ExportDesc::Func(FuncIdx(1)),
    }];

    assert_eq!(
        decode(f).unwrap(),
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
                align: 2,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load(Memarg {
                align: 3,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::F32Load(Memarg {
                align: 2,
                offset: 0,
            }),
            Instruction::LocalTee(LocalIdx(0)),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::F64Load(Memarg {
                align: 3,
                offset: 0,
            }),
            Instruction::LocalTee(LocalIdx(1)),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I32Load8s(Memarg {
                align: 0,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I32Load8u(Memarg {
                align: 0,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I32Load16s(Memarg {
                align: 1,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I32Load16u(Memarg {
                align: 1,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load8s(Memarg {
                align: 0,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load8u(Memarg {
                align: 0,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load16s(Memarg {
                align: 1,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load16u(Memarg {
                align: 1,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load32s(Memarg {
                align: 2,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I64Load32u(Memarg {
                align: 2,
                offset: 0,
            }),
            Instruction::Drop,
            Instruction::I32Const(4),
            Instruction::I32Const(1),
            Instruction::I32Store(Memarg {
                align: 2,
                offset: 0,
            }),
            Instruction::I32Const(8),
            Instruction::I64Const(2),
            Instruction::I64Store(Memarg {
                align: 3,
                offset: 0,
            }),
            Instruction::I32Const(16),
            Instruction::LocalGet(LocalIdx(0)),
            Instruction::F32Store(Memarg {
                align: 2,
                offset: 0,
            }),
            Instruction::I32Const(24),
            Instruction::LocalGet(LocalIdx(1)),
            Instruction::F64Store(Memarg {
                align: 3,
                offset: 0,
            }),
            Instruction::I32Const(32),
            Instruction::I32Const(5),
            Instruction::I32Store8(Memarg {
                align: 0,
                offset: 0,
            }),
            Instruction::I32Const(34),
            Instruction::I32Const(6),
            Instruction::I32Store16(Memarg {
                align: 1,
                offset: 0,
            }),
            Instruction::I32Const(36),
            Instruction::I64Const(7),
            Instruction::I64Store8(Memarg {
                align: 0,
                offset: 0,
            }),
            Instruction::I32Const(38),
            Instruction::I64Const(8),
            Instruction::I64Store16(Memarg {
                align: 1,
                offset: 0,
            }),
            Instruction::I32Const(40),
            Instruction::I64Const(9),
            Instruction::I64Store32(Memarg {
                align: 2,
                offset: 0,
            }),
            Instruction::MemorySize,
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::MemoryGrow,
            Instruction::Drop,
            Instruction::I32Const(0),
            Instruction::I32Const(0),
            Instruction::I32Const(4),
            Instruction::MemoryInit(DataIdx(1)),
            Instruction::DataDrop(DataIdx(1)),
            Instruction::I32Const(8),
            Instruction::I32Const(0),
            Instruction::I32Const(4),
            Instruction::MemoryCopy,
            Instruction::I32Const(12),
            Instruction::I32Const(255),
            Instruction::I32Const(4),
            Instruction::MemoryFill,
        ],
    }];

    let mems = vec![MemType {
        limits: Limits { min: 1, max: None },
    }];

    let exports = vec![
        Export {
            name: "mem".to_owned(),
            desc: ExportDesc::Mem(MemIdx(0)),
        },
        Export {
            name: "use-memory".to_owned(),
            desc: ExportDesc::Func(FuncIdx(0)),
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
        decode(f).unwrap(),
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
        desc: ExportDesc::Func(FuncIdx(0)),
    }];

    assert_eq!(
        decode(f).unwrap(),
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
fn it_accepts_foo() {
    let f = File::open("./tests/fixtures/foo.wasm").unwrap();

    let parsed_section_kinds = vec![
        SectionKind::Type,
        SectionKind::Import,
        SectionKind::Function,
        SectionKind::Table,
        SectionKind::Memory,
        SectionKind::Global,
        SectionKind::Export,
        SectionKind::DataCount,
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
            size: 7,
        },
        SectionHeader {
            kind: SectionKind::Import,
            size: 11,
        },
        SectionHeader {
            kind: SectionKind::Function,
            size: 2,
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
            size: 23,
        },
        SectionHeader {
            kind: SectionKind::DataCount,
            size: 1,
        },
        SectionHeader {
            kind: SectionKind::Code,
            size: 9,
        },
        SectionHeader {
            kind: SectionKind::Data,
            size: 7,
        },
    ];

    let custom_sections = vec![CustomSection {
        name: "note".to_owned(),
        contents: vec![0x68, 0x69],
    }];

    let types = vec![FuncType {
        parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
        results: vec![ValType::Num(NumType::Int32)],
    }];

    let imports = vec![Import {
        module: "env".to_owned(),
        name: "imp".to_owned(),
        desc: ImportDesc::Type(TypeIdx(0)),
    }];

    let tables = vec![TableType {
        limits: Limits { min: 1, max: None },
        reftype: RefType::Func,
    }];

    let mems = vec![MemType {
        limits: Limits { min: 1, max: None },
    }];

    let globals = vec![Global {
        r#type: GlobalType(Mut::Const, ValType::Num(NumType::Int32)),
        init: vec![Instruction::I32Const(42)],
    }];

    let exports = vec![
        Export {
            name: "add".to_owned(),
            desc: ExportDesc::Func(FuncIdx(1)),
        },
        Export {
            name: "mem".to_owned(),
            desc: ExportDesc::Mem(MemIdx(0)),
        },
        Export {
            name: "tab".to_owned(),
            desc: ExportDesc::Table(TableIdx(0)),
        },
        Export {
            name: "g".to_owned(),
            desc: ExportDesc::Global(GlobalIdx(0)),
        },
    ];

    let funcs = vec![Func {
        r#type: TypeIdx(0),
        locals: Vec::new(),
        body: vec![
            Instruction::LocalGet(LocalIdx(0)),
            Instruction::LocalGet(LocalIdx(1)),
            Instruction::I32Add,
        ],
    }];

    let datas = vec![Data {
        init: vec![0x58],
        mode: DataMode::Active {
            memory: MemIdx(0),
            offset: vec![Instruction::I32Const(0)],
        },
    }];

    assert_eq!(
        decode(f).unwrap(),
        Module {
            parsed_section_kinds,
            section_headers,
            custom_sections,
            types,
            funcs,
            tables,
            mems,
            globals,
            datas,
            imports,
            exports,
            data_count: Some(1),
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
        limits: Limits { min: 1, max: None },
        reftype: RefType::Func,
    }];

    let mems = vec![MemType {
        limits: Limits { min: 1, max: None },
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
        module: "env".to_owned(),
        name: "impstart".to_owned(),
        desc: ImportDesc::Type(TypeIdx(0)),
    }];

    let exports = vec![
        Export {
            name: "add".to_owned(),
            desc: ExportDesc::Func(FuncIdx(2)),
        },
        Export {
            name: "mem".to_owned(),
            desc: ExportDesc::Mem(MemIdx(0)),
        },
        Export {
            name: "tab".to_owned(),
            desc: ExportDesc::Table(TableIdx(0)),
        },
        Export {
            name: "g0".to_owned(),
            desc: ExportDesc::Global(GlobalIdx(0)),
        },
    ];

    assert_eq!(
        decode(f).unwrap(),
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
        limits: Limits { min: 1, max: None },
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
        decode(f).unwrap(),
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

    let module = decode(f).unwrap();
    let mut seen = BTreeSet::new();

    for func in &module.funcs {
        for instr in &func.body {
            match instr {
                Instruction::I32Const(_) => {
                    seen.insert("i32.const");
                }
                Instruction::I64Const(_) => {
                    seen.insert("i64.const");
                }
                Instruction::F32Const(_) => {
                    seen.insert("f32.const");
                }
                Instruction::F64Const(_) => {
                    seen.insert("f64.const");
                }
                Instruction::I32Eqz => {
                    seen.insert("i32.eqz");
                }
                Instruction::I32Eq => {
                    seen.insert("i32.eq");
                }
                Instruction::I32Ne => {
                    seen.insert("i32.ne");
                }
                Instruction::I32LtS => {
                    seen.insert("i32.lt_s");
                }
                Instruction::I32LtU => {
                    seen.insert("i32.lt_u");
                }
                Instruction::I32GtS => {
                    seen.insert("i32.gt_s");
                }
                Instruction::I32GtU => {
                    seen.insert("i32.gt_u");
                }
                Instruction::I32LeS => {
                    seen.insert("i32.le_s");
                }
                Instruction::I32LeU => {
                    seen.insert("i32.le_u");
                }
                Instruction::I32GeS => {
                    seen.insert("i32.ge_s");
                }
                Instruction::I32GeU => {
                    seen.insert("i32.ge_u");
                }
                Instruction::I64Eqz => {
                    seen.insert("i64.eqz");
                }
                Instruction::I64Eq => {
                    seen.insert("i64.eq");
                }
                Instruction::I64Ne => {
                    seen.insert("i64.ne");
                }
                Instruction::I64LtS => {
                    seen.insert("i64.lt_s");
                }
                Instruction::I64LtU => {
                    seen.insert("i64.lt_u");
                }
                Instruction::I64GtS => {
                    seen.insert("i64.gt_s");
                }
                Instruction::I64GtU => {
                    seen.insert("i64.gt_u");
                }
                Instruction::I64LeS => {
                    seen.insert("i64.le_s");
                }
                Instruction::I64LeU => {
                    seen.insert("i64.le_u");
                }
                Instruction::I64GeS => {
                    seen.insert("i64.ge_s");
                }
                Instruction::I64GeU => {
                    seen.insert("i64.ge_u");
                }
                Instruction::F32Eq => {
                    seen.insert("f32.eq");
                }
                Instruction::F32Ne => {
                    seen.insert("f32.ne");
                }
                Instruction::F32Lt => {
                    seen.insert("f32.lt");
                }
                Instruction::F32Gt => {
                    seen.insert("f32.gt");
                }
                Instruction::F32Le => {
                    seen.insert("f32.le");
                }
                Instruction::F32Ge => {
                    seen.insert("f32.ge");
                }
                Instruction::F64Eq => {
                    seen.insert("f64.eq");
                }
                Instruction::F64Ne => {
                    seen.insert("f64.ne");
                }
                Instruction::F64Lt => {
                    seen.insert("f64.lt");
                }
                Instruction::F64Gt => {
                    seen.insert("f64.gt");
                }
                Instruction::F64Le => {
                    seen.insert("f64.le");
                }
                Instruction::F64Ge => {
                    seen.insert("f64.ge");
                }
                Instruction::I32Clz => {
                    seen.insert("i32.clz");
                }
                Instruction::I32Ctz => {
                    seen.insert("i32.ctz");
                }
                Instruction::I32Popcnt => {
                    seen.insert("i32.popcnt");
                }
                Instruction::I32Add => {
                    seen.insert("i32.add");
                }
                Instruction::I32Sub => {
                    seen.insert("i32.sub");
                }
                Instruction::I32Mul => {
                    seen.insert("i32.mul");
                }
                Instruction::I32DivS => {
                    seen.insert("i32.div_s");
                }
                Instruction::I32DivU => {
                    seen.insert("i32.div_u");
                }
                Instruction::I32RemS => {
                    seen.insert("i32.rem_s");
                }
                Instruction::I32RemU => {
                    seen.insert("i32.rem_u");
                }
                Instruction::I32And => {
                    seen.insert("i32.and");
                }
                Instruction::I32Or => {
                    seen.insert("i32.or");
                }
                Instruction::I32Xor => {
                    seen.insert("i32.xor");
                }
                Instruction::I32Shl => {
                    seen.insert("i32.shl");
                }
                Instruction::I32ShrS => {
                    seen.insert("i32.shr_s");
                }
                Instruction::I32ShrU => {
                    seen.insert("i32.shr_u");
                }
                Instruction::I32Rotl => {
                    seen.insert("i32.rotl");
                }
                Instruction::I32Rotr => {
                    seen.insert("i32.rotr");
                }
                Instruction::I64Clz => {
                    seen.insert("i64.clz");
                }
                Instruction::I64Ctz => {
                    seen.insert("i64.ctz");
                }
                Instruction::I64Popcnt => {
                    seen.insert("i64.popcnt");
                }
                Instruction::I64Add => {
                    seen.insert("i64.add");
                }
                Instruction::I64Sub => {
                    seen.insert("i64.sub");
                }
                Instruction::I64Mul => {
                    seen.insert("i64.mul");
                }
                Instruction::I64DivS => {
                    seen.insert("i64.div_s");
                }
                Instruction::I64DivU => {
                    seen.insert("i64.div_u");
                }
                Instruction::I64RemS => {
                    seen.insert("i64.rem_s");
                }
                Instruction::I64RemU => {
                    seen.insert("i64.rem_u");
                }
                Instruction::I64And => {
                    seen.insert("i64.and");
                }
                Instruction::I64Or => {
                    seen.insert("i64.or");
                }
                Instruction::I64Xor => {
                    seen.insert("i64.xor");
                }
                Instruction::I64Shl => {
                    seen.insert("i64.shl");
                }
                Instruction::I64ShrS => {
                    seen.insert("i64.shr_s");
                }
                Instruction::I64ShrU => {
                    seen.insert("i64.shr_u");
                }
                Instruction::I64Rotl => {
                    seen.insert("i64.rotl");
                }
                Instruction::I64Rotr => {
                    seen.insert("i64.rotr");
                }
                Instruction::F32Abs => {
                    seen.insert("f32.abs");
                }
                Instruction::F32Neg => {
                    seen.insert("f32.neg");
                }
                Instruction::F32Ceil => {
                    seen.insert("f32.ceil");
                }
                Instruction::F32Floor => {
                    seen.insert("f32.floor");
                }
                Instruction::F32Trunc => {
                    seen.insert("f32.trunc");
                }
                Instruction::F32Nearest => {
                    seen.insert("f32.nearest");
                }
                Instruction::F32Sqrt => {
                    seen.insert("f32.sqrt");
                }
                Instruction::F32Add => {
                    seen.insert("f32.add");
                }
                Instruction::F32Sub => {
                    seen.insert("f32.sub");
                }
                Instruction::F32Mul => {
                    seen.insert("f32.mul");
                }
                Instruction::F32Div => {
                    seen.insert("f32.div");
                }
                Instruction::F32Min => {
                    seen.insert("f32.min");
                }
                Instruction::F32Max => {
                    seen.insert("f32.max");
                }
                Instruction::F32Copysign => {
                    seen.insert("f32.copysign");
                }
                Instruction::F64Abs => {
                    seen.insert("f64.abs");
                }
                Instruction::F64Neg => {
                    seen.insert("f64.neg");
                }
                Instruction::F64Ceil => {
                    seen.insert("f64.ceil");
                }
                Instruction::F64Floor => {
                    seen.insert("f64.floor");
                }
                Instruction::F64Trunc => {
                    seen.insert("f64.trunc");
                }
                Instruction::F64Nearest => {
                    seen.insert("f64.nearest");
                }
                Instruction::F64Sqrt => {
                    seen.insert("f64.sqrt");
                }
                Instruction::F64Add => {
                    seen.insert("f64.add");
                }
                Instruction::F64Sub => {
                    seen.insert("f64.sub");
                }
                Instruction::F64Mul => {
                    seen.insert("f64.mul");
                }
                Instruction::F64Div => {
                    seen.insert("f64.div");
                }
                Instruction::F64Min => {
                    seen.insert("f64.min");
                }
                Instruction::F64Max => {
                    seen.insert("f64.max");
                }
                Instruction::F64Copysign => {
                    seen.insert("f64.copysign");
                }
                Instruction::I32WrapI64 => {
                    seen.insert("i32.wrap_i64");
                }
                Instruction::I32TruncF32S => {
                    seen.insert("i32.trunc_f32_s");
                }
                Instruction::I32TruncF32U => {
                    seen.insert("i32.trunc_f32_u");
                }
                Instruction::I32TruncF64S => {
                    seen.insert("i32.trunc_f64_s");
                }
                Instruction::I32TruncF64U => {
                    seen.insert("i32.trunc_f64_u");
                }
                Instruction::I64ExtendI32S => {
                    seen.insert("i64.extend_i32_s");
                }
                Instruction::I64ExtendI32U => {
                    seen.insert("i64.extend_i32_u");
                }
                Instruction::I64TruncF32S => {
                    seen.insert("i64.trunc_f32_s");
                }
                Instruction::I64TruncF32U => {
                    seen.insert("i64.trunc_f32_u");
                }
                Instruction::I64TruncF64S => {
                    seen.insert("i64.trunc_f64_s");
                }
                Instruction::I64TruncF64U => {
                    seen.insert("i64.trunc_f64_u");
                }
                Instruction::F32ConvertI32S => {
                    seen.insert("f32.convert_i32_s");
                }
                Instruction::F32ConvertI32U => {
                    seen.insert("f32.convert_i32_u");
                }
                Instruction::F32ConvertI64S => {
                    seen.insert("f32.convert_i64_s");
                }
                Instruction::F32ConvertI64U => {
                    seen.insert("f32.convert_i64_u");
                }
                Instruction::F32DemoteF64 => {
                    seen.insert("f32.demote_f64");
                }
                Instruction::F64ConvertI32S => {
                    seen.insert("f64.convert_i32_s");
                }
                Instruction::F64ConvertI32U => {
                    seen.insert("f64.convert_i32_u");
                }
                Instruction::F64ConvertI64S => {
                    seen.insert("f64.convert_i64_s");
                }
                Instruction::F64ConvertI64U => {
                    seen.insert("f64.convert_i64_u");
                }
                Instruction::F64PromoteF32 => {
                    seen.insert("f64.promote_f32");
                }
                Instruction::I32ReinterpretF32 => {
                    seen.insert("i32.reinterpret_f32");
                }
                Instruction::I64ReinterpretF64 => {
                    seen.insert("i64.reinterpret_f64");
                }
                Instruction::F32ReinterpretI32 => {
                    seen.insert("f32.reinterpret_i32");
                }
                Instruction::F64ReinterpretI64 => {
                    seen.insert("f64.reinterpret_i64");
                }
                Instruction::I32Extend8S => {
                    seen.insert("i32.extend8_s");
                }
                Instruction::I32Extend16S => {
                    seen.insert("i32.extend16_s");
                }
                Instruction::I64Extend8S => {
                    seen.insert("i64.extend8_s");
                }
                Instruction::I64Extend16S => {
                    seen.insert("i64.extend16_s");
                }
                Instruction::I64Extend32S => {
                    seen.insert("i64.extend32_s");
                }
                Instruction::I32TruncSatF32S => {
                    seen.insert("i32.trunc_sat_f32_s");
                }
                Instruction::I32TruncSatF32U => {
                    seen.insert("i32.trunc_sat_f32_u");
                }
                Instruction::I32TruncSatF64S => {
                    seen.insert("i32.trunc_sat_f64_s");
                }
                Instruction::I32TruncSatF64U => {
                    seen.insert("i32.trunc_sat_f64_u");
                }
                Instruction::I64TruncSatF32S => {
                    seen.insert("i64.trunc_sat_f32_s");
                }
                Instruction::I64TruncSatF32U => {
                    seen.insert("i64.trunc_sat_f32_u");
                }
                Instruction::I64TruncSatF64S => {
                    seen.insert("i64.trunc_sat_f64_s");
                }
                Instruction::I64TruncSatF64U => {
                    seen.insert("i64.trunc_sat_f64_u");
                }
                _ => {}
            }
        }
    }

    let expected: BTreeSet<&'static str> = [
        "i32.const",
        "i64.const",
        "f32.const",
        "f64.const",
        "i32.eqz",
        "i32.eq",
        "i32.ne",
        "i32.lt_s",
        "i32.lt_u",
        "i32.gt_s",
        "i32.gt_u",
        "i32.le_s",
        "i32.le_u",
        "i32.ge_s",
        "i32.ge_u",
        "i64.eqz",
        "i64.eq",
        "i64.ne",
        "i64.lt_s",
        "i64.lt_u",
        "i64.gt_s",
        "i64.gt_u",
        "i64.le_s",
        "i64.le_u",
        "i64.ge_s",
        "i64.ge_u",
        "f32.eq",
        "f32.ne",
        "f32.lt",
        "f32.gt",
        "f32.le",
        "f32.ge",
        "f64.eq",
        "f64.ne",
        "f64.lt",
        "f64.gt",
        "f64.le",
        "f64.ge",
        "i32.clz",
        "i32.ctz",
        "i32.popcnt",
        "i32.add",
        "i32.sub",
        "i32.mul",
        "i32.div_s",
        "i32.div_u",
        "i32.rem_s",
        "i32.rem_u",
        "i32.and",
        "i32.or",
        "i32.xor",
        "i32.shl",
        "i32.shr_s",
        "i32.shr_u",
        "i32.rotl",
        "i32.rotr",
        "i64.clz",
        "i64.ctz",
        "i64.popcnt",
        "i64.add",
        "i64.sub",
        "i64.mul",
        "i64.div_s",
        "i64.div_u",
        "i64.rem_s",
        "i64.rem_u",
        "i64.and",
        "i64.or",
        "i64.xor",
        "i64.shl",
        "i64.shr_s",
        "i64.shr_u",
        "i64.rotl",
        "i64.rotr",
        "f32.abs",
        "f32.neg",
        "f32.ceil",
        "f32.floor",
        "f32.trunc",
        "f32.nearest",
        "f32.sqrt",
        "f32.add",
        "f32.sub",
        "f32.mul",
        "f32.div",
        "f32.min",
        "f32.max",
        "f32.copysign",
        "f64.abs",
        "f64.neg",
        "f64.ceil",
        "f64.floor",
        "f64.trunc",
        "f64.nearest",
        "f64.sqrt",
        "f64.add",
        "f64.sub",
        "f64.mul",
        "f64.div",
        "f64.min",
        "f64.max",
        "f64.copysign",
        "i32.wrap_i64",
        "i32.trunc_f32_s",
        "i32.trunc_f32_u",
        "i32.trunc_f64_s",
        "i32.trunc_f64_u",
        "i64.extend_i32_s",
        "i64.extend_i32_u",
        "i64.trunc_f32_s",
        "i64.trunc_f32_u",
        "i64.trunc_f64_s",
        "i64.trunc_f64_u",
        "f32.convert_i32_s",
        "f32.convert_i32_u",
        "f32.convert_i64_s",
        "f32.convert_i64_u",
        "f32.demote_f64",
        "f64.convert_i32_s",
        "f64.convert_i32_u",
        "f64.convert_i64_s",
        "f64.convert_i64_u",
        "f64.promote_f32",
        "i32.reinterpret_f32",
        "i64.reinterpret_f64",
        "f32.reinterpret_i32",
        "f64.reinterpret_i64",
        "i32.extend8_s",
        "i32.extend16_s",
        "i64.extend8_s",
        "i64.extend16_s",
        "i64.extend32_s",
        "i32.trunc_sat_f32_s",
        "i32.trunc_sat_f32_u",
        "i32.trunc_sat_f64_s",
        "i32.trunc_sat_f64_u",
        "i64.trunc_sat_f32_s",
        "i64.trunc_sat_f32_u",
        "i64.trunc_sat_f64_s",
        "i64.trunc_sat_f64_u",
    ]
    .into_iter()
    .collect();

    assert_eq!(seen, expected);
}

#[test]
fn it_decodes_vector_instructions() {
    let f = File::open("./tests/fixtures/vector_instructions.wasm").unwrap();

    let module = decode(f).unwrap();

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
            limits: Limits { min: 1, max: None },
        }]
    );

    assert_eq!(
        module.exports,
        vec![Export {
            name: "use_vectors".to_owned(),
            desc: ExportDesc::Func(FuncIdx(0)),
        }]
    );

    assert_eq!(module.funcs.len(), 1);
    let func = &module.funcs[0];
    assert_eq!(func.r#type, TypeIdx(0));
    assert!(func.locals.is_empty());
    assert_eq!(func.body.len(), 26);
    let actual_body: Vec<String> = func.body.iter().map(|instr| format!("{instr:?}")).collect();
    let expected_body: Vec<String> = vec![
        "I32Const(0)",
        "V128Load(Memarg { align: 4, offset: 0 })",
        "I32Const(16)",
        "V128Load(Memarg { align: 4, offset: 0 })",
        "I8x16Add",
        "V128Const([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15])",
        "V128Const([16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31])",
        "V128Bitselect",
        "Drop",
        "V128Const([32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47])",
        "I8x16ExtractLaneS(LaneIdx(0))",
        "Drop",
        "V128Const([48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63])",
        "I32Const(7)",
        "I8x16ReplaceLane(LaneIdx(5))",
        "Drop",
        "V128Const([64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79])",
        "V128Const([80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95])",
        "I8x16Shuffle([LaneIdx(0), LaneIdx(16), LaneIdx(1), LaneIdx(17), LaneIdx(2), LaneIdx(18), LaneIdx(3), LaneIdx(19), LaneIdx(4), LaneIdx(20), LaneIdx(5), LaneIdx(21), LaneIdx(6), LaneIdx(22), LaneIdx(7), LaneIdx(23)])",
        "Drop",
        "V128Const([96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111])",
        "V128AnyTrue",
        "Drop",
        "I32Const(0)",
        "V128Const([112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127])",
        "V128Store(Memarg { align: 4, offset: 0 })",
    ]
    .into_iter()
    .map(String::from)
    .collect();
    assert_eq!(actual_body, expected_body);

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
            min: 1,
            max: Some(129),
        },
    }];

    assert_eq!(
        decode(f).unwrap(),
        Module {
            parsed_section_kinds: vec![SectionKind::Memory],
            section_headers,
            mems,
            ..Default::default()
        }
    )
}

#[test]
fn it_fails_on_code_size_mismatch() {
    let f = File::open("tests/fixtures/code_section_size_underreported.wasm").unwrap();
    assert!(decode(f).is_err());

    let f = File::open("tests/fixtures/custom_section_size_overreported.wasm").unwrap();
    assert!(decode(f).is_err())
}

#[test]
fn it_fails_on_code_entry_size_mismatch() {
    let f = File::open("tests/fixtures/code_entry_size_overreported.wasm").unwrap();
    assert!(decode(f).is_err());
}

#[test]
fn it_enforces_section_ordering() {
    let f = File::open("tests/fixtures/invalid_section_order.wasm").unwrap();
    assert!(decode(f).is_err());
}

#[test]
fn it_surfaces_vector_element_index_on_type_decode_failure() {
    let f = File::open("tests/fixtures/type_section_invalid_functype.wasm").unwrap();

    let err = decode(f).expect_err("type section should fail on bad functype marker");

    match err {
        DecodeModuleError::DecodeTypeSection(DecodeTypeSectionError::DecodeVector(
            DecodeVectorError::ParseElement {
                position: nth_element,
                source,
            },
        )) => {
            assert_eq!(nth_element, 1);
            assert!(matches!(
                source,
                DecodeFuncTypeError::InvalidMarkerByte(0x00)
            ));
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn it_surfaces_invalid_valtype_marker() {
    let f = File::open("tests/fixtures/type_section_invalid_valtype_marker.wasm").unwrap();

    let err = decode(f).expect_err("type section should fail on bad value type marker");

    match err {
        DecodeModuleError::DecodeTypeSection(DecodeTypeSectionError::DecodeVector(
            DecodeVectorError::ParseElement {
                position: 0,
                source:
                    DecodeFuncTypeError::DecodeParameterTypes(DecodeResultTypeError::DecodeVector(
                        DecodeVectorError::ParseElement {
                            position: 0,
                            source: DecodeValTypeError::InvalidMarkerByte(err),
                        },
                    )),
            },
        )) => assert!(err.to_string().contains("0xAA")),
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

    let err = decode(module).expect_err("module should fail while reading type index");

    assert!(matches!(err, DecodeModuleError::DecodeFunctionSection(_)));
}
