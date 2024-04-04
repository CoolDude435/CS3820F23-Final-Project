(module
  (type (func (result i32)))
  (type (func (param i32) (param i32) (result i32)))
  (func (type 1)
    local.get 0
    i32.eqz
    if (type 0)
      local.get 1
      return
    else
      local.get 0
      i32.const 1
      i32.sub
      local.get 1
      i32.const 1
      i32.add
      call 0
    end)
  (start 0))
