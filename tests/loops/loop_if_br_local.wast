(module
  (type (func (result i32)))
  (type (func (param i32) (param i32) (result i32)))

  (func (type 0) (local i32)
    i32.const 0
    local.set 0

    i32.const 0
    i32.const 1

    loop (type 1)
      local.get 0
      i32.const 1
      i32.add
      local.set 0

      i32.lt_u

      if (type 0)
        i32.const 1
        i32.const 0
        br 1
      else
        local.get 0
      end
    end)

  (start 0))
