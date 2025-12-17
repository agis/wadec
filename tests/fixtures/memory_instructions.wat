(module
  (memory (export "mem") 1)
  (data (memory 0) (offset (i32.const 0))
    "\00\01\02\03\04\05\06\07\08\09\0A\0B\0C\0D\0E\0F\10\11\12\13\14\15\16\17\18\19\1A\1B\1C\1D\1E\1F")
  (data "\AA\BB\CC\DD\EE\FF")
  (func (export "use-memory")
    (local f32 f64)
    ;; Loads
    i32.const 0
    i32.load
    drop

    i32.const 0
    i64.load
    drop

    i32.const 0
    f32.load
    local.tee 0
    drop

    i32.const 0
    f64.load
    local.tee 1
    drop

    i32.const 0
    i32.load8_s
    drop

    i32.const 0
    i32.load8_u
    drop

    i32.const 0
    i32.load16_s
    drop

    i32.const 0
    i32.load16_u
    drop

    i32.const 0
    i64.load8_s
    drop

    i32.const 0
    i64.load8_u
    drop

    i32.const 0
    i64.load16_s
    drop

    i32.const 0
    i64.load16_u
    drop

    i32.const 0
    i64.load32_s
    drop

    i32.const 0
    i64.load32_u
    drop

    ;; Stores
    i32.const 4
    i32.const 1
    i32.store

    i32.const 8
    i64.const 2
    i64.store

    i32.const 16
    local.get 0
    f32.store

    i32.const 24
    local.get 1
    f64.store

    i32.const 32
    i32.const 5
    i32.store8

    i32.const 34
    i32.const 6
    i32.store16

    i32.const 36
    i64.const 7
    i64.store8

    i32.const 38
    i64.const 8
    i64.store16

    i32.const 40
    i64.const 9
    i64.store32

    ;; Memory size
    memory.size
    drop

    ;; Memory grow
    i32.const 0
    memory.grow
    drop

    ;; Memory bulk operations
    i32.const 0
    i32.const 0
    i32.const 4
    memory.init 1

    data.drop 1

    i32.const 8
    i32.const 0
    i32.const 4
    memory.copy

    i32.const 12
    i32.const 255
    i32.const 4
    memory.fill
  )
)
