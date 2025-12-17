(module
  (type $t0 (func))
  (func $dummy (type $t0))
  (table $table0 4 funcref)
  (table $table1 4 funcref)
  (elem $seg0 func $dummy)
  (elem $seg1 func $dummy)
  (func (export "table_ops") (param $idx i32) (param $grow i32)
    local.get $idx
    table.get $table0
    drop

    local.get $idx
    ref.null func
    table.set $table0

    i32.const 0
    i32.const 0
    i32.const 1
    table.init $table0 $seg1

    elem.drop $seg1

    i32.const 0
    ref.null func
    i32.const 1
    table.fill $table0

    table.size $table0
    drop

    ref.null func
    local.get $grow
    table.grow $table0
    drop

    i32.const 0
    i32.const 0
    i32.const 1
    table.copy $table0 $table1

    table.size $table0
    drop)
)
