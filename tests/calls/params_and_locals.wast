(module 
  (type (func (param i32) (result i32)))
  (func (type 0) (local i32)
    local.get 0
    i32.const 1
    i32.add
    local.tee 1
    call 1
    local.get 1)
  (func (type 0) (local i32)
    local.get 0
    i32.const 1
    i32.add
    local.tee 1
    call 2
    local.get 1)    
  (func (type 0) (local i32)
    local.get 0
    i32.const 1
    i32.add
    local.tee 1)        
  (func (type 0)
    local.get 0
    call 0)
  (start 3))
