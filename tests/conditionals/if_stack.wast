(module
  (type (func (param i32) (result i32)))
  (type (func (param i32) (param i32) (result i32)))

  (func (type 1)
    i32.const 42
    local.get 0
    local.get 1
    i32.lt_u
    if (type 0)
      local.get 0
      i32.add
    else
      local.get 1
      i32.add
    end)
  (start 0))
