(module
  (type (func (result i32)))
  (type (func (param i32) (result i32)))
  (func (type 1)
    local.get 0
    i32.eqz
    if (type 0)
      i32.const 0
      return
    else
      local.get 0
      i32.const 1
      i32.eq
      if (type 0)
        i32.const 1
        return
      else
        local.get 0
        i32.const 1
        i32.sub
        call 0
        local.get 0
        i32.const 2
        i32.sub
        call 0
        i32.add
      end
    end)
  (func (type 1)
    local.get 0
    call 0)
  (start 1))
