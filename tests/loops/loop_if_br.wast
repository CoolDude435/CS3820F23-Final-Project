(module
  (type (func (result i32)))
  (type (func (param i32) (result i32)))  
  (type (func (param i32) (param i32) (result i32)))

  (func (type 1)
    i32.const 0
    i32.const 1

    loop (type 2)
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

  (func (type 0)
    i32.const 0
    call 0)

  (start 1))
