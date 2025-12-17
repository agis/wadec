(module
  (func (export "param") (param i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    select
    drop
    local.get 0
    local.get 1
    local.get 2
    select (result i32)
    drop
    local.get 0)
)
