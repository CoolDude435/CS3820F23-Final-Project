(module 
  (type (func (param i32) (param i32) (result i32)))
  (type (func (result i32)))
  (func (type 0)
    local.get 0
    local.get 1
    i32.add)
  (func (type 1)
    i32.const 14
    i32.const 12
    call 0)
  (start 1))
  
