(module
  (type $t0 (func))
  (func $dummy (type $t0))
  (table 1 funcref)
  (elem declare func $dummy)
  (func (export "refs")
    ref.null func
    ref.is_null
    drop
    ref.func $dummy
    drop)
)
