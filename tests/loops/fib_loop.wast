(module
  (type (func (result i32)))
  (type (func (param i32) (result i32)))
  (func (type 1) (local i32)(local i32)
    i32.const 0
    local.set 1
    i32.const 1
    local.set 2
    loop (type 0)
      local.get 0
      i32.eqz
      if (type 0)
        local.get 1
        return
      else
        local.get 2
        local.get 1
        local.get 2
        i32.add
        local.set 2
        local.set 1
        local.get 0
        i32.const 1
        i32.sub
        local.set 0
        br 1
      end
    end)
  (func (type 1)
    local.get 0
    call 0)
  (start 1))
