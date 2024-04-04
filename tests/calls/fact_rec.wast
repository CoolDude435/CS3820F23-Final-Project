(module
  (type (func (param i32) (result i32)))
  (type (func (result i32)))
  (func (type 0)
    local.get 0
    i32.eqz
    if (type 1)
      i32.const 1
      return
    else
      local.get 0
      i32.const 1
      i32.sub
      call 0
      local.get 0
      i32.mul
      return
    end)
  (func (type 0)
    local.get 0
    call 0)
  (start 1))
