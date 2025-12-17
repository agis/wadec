(module
  (type $t0 (func))
  (type $pair (func (result i32 i32)))
  (func $callee (type $t0))
  (table 1 funcref)

  (func (export "control")
    nop

    block
      unreachable
    end

    block
      loop
        br 1
      end
    end

    block
      i32.const 0
      br_if 0
    end

    block
      i32.const 0
      br_table 0 0
    end

    block
      i32.const 0
      if
        unreachable
      else
        nop
      end
    end

    block (type $pair)
      i32.const 42
      i32.const 7
    end
    drop
    drop

    call $callee
    i32.const 0
    call_indirect (type $t0)
    return
  )
)
