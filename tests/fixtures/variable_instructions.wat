(module
  (global $g0 (mut i32) (i32.const 0))
  (func (export "var") (param i32 i32)
    local.get 0
    local.set 1
    local.get 1
    local.tee 0
    drop
    global.get $g0
    drop
    local.get 1
    global.set $g0)
)
