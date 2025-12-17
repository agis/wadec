(module
  (func $consts
    i32.const 1
    drop
    i64.const 1
    drop
    f32.const 1
    drop
    f64.const 1
    drop)

  (func $comparisons
    ;; i32 tests
    i32.const 0
    i32.eqz
    drop
    i32.const 1
    i32.const 1
    i32.eq
    drop
    i32.const 1
    i32.const 0
    i32.ne
    drop
    i32.const -1
    i32.const 1
    i32.lt_s
    drop
    i32.const 1
    i32.const 2
    i32.lt_u
    drop
    i32.const 2
    i32.const 1
    i32.gt_s
    drop
    i32.const 2
    i32.const 1
    i32.gt_u
    drop
    i32.const -1
    i32.const -1
    i32.le_s
    drop
    i32.const 1
    i32.const 2
    i32.le_u
    drop
    i32.const -1
    i32.const -1
    i32.ge_s
    drop
    i32.const 2
    i32.const 1
    i32.ge_u
    drop

    ;; i64 tests
    i64.const 0
    i64.eqz
    drop
    i64.const 1
    i64.const 1
    i64.eq
    drop
    i64.const 1
    i64.const 0
    i64.ne
    drop
    i64.const -1
    i64.const 1
    i64.lt_s
    drop
    i64.const 1
    i64.const 2
    i64.lt_u
    drop
    i64.const 2
    i64.const 1
    i64.gt_s
    drop
    i64.const 2
    i64.const 1
    i64.gt_u
    drop
    i64.const -1
    i64.const -1
    i64.le_s
    drop
    i64.const 1
    i64.const 2
    i64.le_u
    drop
    i64.const -1
    i64.const -1
    i64.ge_s
    drop
    i64.const 2
    i64.const 1
    i64.ge_u
    drop

    ;; f32 comparisons
    f32.const 1
    f32.const 1
    f32.eq
    drop
    f32.const 1
    f32.const 0
    f32.ne
    drop
    f32.const 1
    f32.const 2
    f32.lt
    drop
    f32.const 2
    f32.const 1
    f32.gt
    drop
    f32.const 1
    f32.const 2
    f32.le
    drop
    f32.const 2
    f32.const 1
    f32.ge
    drop

    ;; f64 comparisons
    f64.const 1
    f64.const 1
    f64.eq
    drop
    f64.const 1
    f64.const 0
    f64.ne
    drop
    f64.const 1
    f64.const 2
    f64.lt
    drop
    f64.const 2
    f64.const 1
    f64.gt
    drop
    f64.const 1
    f64.const 2
    f64.le
    drop
    f64.const 2
    f64.const 1
    f64.ge
    drop)

  (func $i32_numeric
    i32.const 8
    i32.clz
    drop
    i32.const 8
    i32.ctz
    drop
    i32.const 255
    i32.popcnt
    drop
    i32.const 1
    i32.const 2
    i32.add
    drop
    i32.const 3
    i32.const 1
    i32.sub
    drop
    i32.const 3
    i32.const 4
    i32.mul
    drop
    i32.const 7
    i32.const 2
    i32.div_s
    drop
    i32.const 7
    i32.const 2
    i32.div_u
    drop
    i32.const 7
    i32.const 3
    i32.rem_s
    drop
    i32.const 7
    i32.const 3
    i32.rem_u
    drop
    i32.const 6
    i32.const 3
    i32.and
    drop
    i32.const 1
    i32.const 2
    i32.or
    drop
    i32.const 1
    i32.const 3
    i32.xor
    drop
    i32.const 1
    i32.const 2
    i32.shl
    drop
    i32.const -8
    i32.const 1
    i32.shr_s
    drop
    i32.const 8
    i32.const 1
    i32.shr_u
    drop
    i32.const 1
    i32.const 2
    i32.rotl
    drop
    i32.const 1
    i32.const 2
    i32.rotr
    drop)

  (func $i64_numeric
    i64.const 8
    i64.clz
    drop
    i64.const 8
    i64.ctz
    drop
    i64.const 255
    i64.popcnt
    drop
    i64.const 1
    i64.const 2
    i64.add
    drop
    i64.const 3
    i64.const 1
    i64.sub
    drop
    i64.const 3
    i64.const 4
    i64.mul
    drop
    i64.const 7
    i64.const 2
    i64.div_s
    drop
    i64.const 7
    i64.const 2
    i64.div_u
    drop
    i64.const 7
    i64.const 3
    i64.rem_s
    drop
    i64.const 7
    i64.const 3
    i64.rem_u
    drop
    i64.const 6
    i64.const 3
    i64.and
    drop
    i64.const 1
    i64.const 2
    i64.or
    drop
    i64.const 1
    i64.const 3
    i64.xor
    drop
    i64.const 1
    i64.const 2
    i64.shl
    drop
    i64.const -8
    i64.const 1
    i64.shr_s
    drop
    i64.const 8
    i64.const 1
    i64.shr_u
    drop
    i64.const 1
    i64.const 2
    i64.rotl
    drop
    i64.const 1
    i64.const 2
    i64.rotr
    drop)

  (func $f32_numeric
    f32.const -1
    f32.abs
    drop
    f32.const 1
    f32.neg
    drop
    f32.const 1.2
    f32.ceil
    drop
    f32.const 1.8
    f32.floor
    drop
    f32.const 1.7
    f32.trunc
    drop
    f32.const 1.3
    f32.nearest
    drop
    f32.const 4
    f32.sqrt
    drop
    f32.const 1
    f32.const 2
    f32.add
    drop
    f32.const 3
    f32.const 1
    f32.sub
    drop
    f32.const 2
    f32.const 3
    f32.mul
    drop
    f32.const 6
    f32.const 2
    f32.div
    drop
    f32.const 1
    f32.const 2
    f32.min
    drop
    f32.const 1
    f32.const 2
    f32.max
    drop
    f32.const 1
    f32.const -2
    f32.copysign
    drop)

  (func $f64_numeric
    f64.const -1
    f64.abs
    drop
    f64.const 1
    f64.neg
    drop
    f64.const 1.2
    f64.ceil
    drop
    f64.const 1.8
    f64.floor
    drop
    f64.const 1.7
    f64.trunc
    drop
    f64.const 1.3
    f64.nearest
    drop
    f64.const 4
    f64.sqrt
    drop
    f64.const 1
    f64.const 2
    f64.add
    drop
    f64.const 3
    f64.const 1
    f64.sub
    drop
    f64.const 2
    f64.const 3
    f64.mul
    drop
    f64.const 6
    f64.const 2
    f64.div
    drop
    f64.const 1
    f64.const 2
    f64.min
    drop
    f64.const 1
    f64.const 2
    f64.max
    drop
    f64.const 1
    f64.const -2
    f64.copysign
    drop)

  (func $conversions
    i64.const 42
    i32.wrap_i64
    drop
    f32.const 3.5
    i32.trunc_f32_s
    drop
    f32.const 3.5
    i32.trunc_f32_u
    drop
    f64.const -3.5
    i32.trunc_f64_s
    drop
    f64.const 3.5
    i32.trunc_f64_u
    drop
    i32.const -1
    i64.extend_i32_s
    drop
    i32.const 1
    i64.extend_i32_u
    drop
    f32.const -3.5
    i64.trunc_f32_s
    drop
    f32.const 3.5
    i64.trunc_f32_u
    drop
    f64.const -3.5
    i64.trunc_f64_s
    drop
    f64.const 3.5
    i64.trunc_f64_u
    drop
    i32.const -1
    f32.convert_i32_s
    drop
    i32.const 1
    f32.convert_i32_u
    drop
    i64.const -1
    f32.convert_i64_s
    drop
    i64.const 1
    f32.convert_i64_u
    drop
    f64.const 4.5
    f32.demote_f64
    drop
    i32.const -1
    f64.convert_i32_s
    drop
    i32.const 1
    f64.convert_i32_u
    drop
    i64.const -1
    f64.convert_i64_s
    drop
    i64.const 1
    f64.convert_i64_u
    drop
    f32.const 2.5
    f64.promote_f32
    drop)

  (func $reinterpret_and_extend
    f32.const 1
    i32.reinterpret_f32
    drop
    f64.const 1
    i64.reinterpret_f64
    drop
    i32.const 1
    f32.reinterpret_i32
    drop
    i64.const 1
    f64.reinterpret_i64
    drop
    i32.const 0x80
    i32.extend8_s
    drop
    i32.const 0x8000
    i32.extend16_s
    drop
    i64.const 0x80
    i64.extend8_s
    drop
    i64.const 0x8000
    i64.extend16_s
    drop
    i64.const 0x80000000
    i64.extend32_s
    drop)

  (func $saturating
    f32.const -3.5
    i32.trunc_sat_f32_s
    drop
    f32.const 3.5
    i32.trunc_sat_f32_u
    drop
    f64.const -3.5
    i32.trunc_sat_f64_s
    drop
    f64.const 3.5
    i32.trunc_sat_f64_u
    drop
    f32.const -3.5
    i64.trunc_sat_f32_s
    drop
    f32.const 3.5
    i64.trunc_sat_f32_u
    drop
    f64.const -3.5
    i64.trunc_sat_f64_s
    drop
    f64.const 3.5
    i64.trunc_sat_f64_u
    drop)

  (func (export "use_all")
    call $consts
    call $comparisons
    call $i32_numeric
    call $i64_numeric
    call $f32_numeric
    call $f64_numeric
    call $conversions
    call $reinterpret_and_extend
    call $saturating))
