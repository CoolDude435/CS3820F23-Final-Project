(module 
  (type (func (param i32) (param i32) (result i32)))
  (type (func (param i32) (result i32)))
  (func (type 1)
    local.get 0
    i32.const 5
    i32.mul
    local.get 0
    local.get 0
    local.get 0
    local.get 0
    local.get 0
    i32.add
    i32.add
    i32.add
    i32.add
    i32.eq)
  (start 0))
