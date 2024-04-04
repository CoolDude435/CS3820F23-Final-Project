(module 
  (type (func (param i32) (result i32)))
  (type (func (result i32)))
  (func (type 0)
    local.get 0
    i32.eqz
    if (type 1)
      i32.const 42
      return
    else
      local.get 0
      i32.const 1
      i32.sub
      call 1
      i32.const 1
      i32.add
      return
    end
    i32.const 3)
  (func (type 0)
    local.get 0
    i32.eqz
    if (type 1)
      i32.const 42
      return
    else
      local.get 0
      i32.const 1
      i32.sub
      call 2
      i32.const 1
      i32.add
      return
    end
    i32.const 3)
  (func (type 0)
    local.get 0
    i32.eqz
    if (type 1)
      i32.const 42
      return
    else
      local.get 0
      i32.const 1
      i32.sub
      call 3
      i32.const 1
      i32.add
      return
    end
    i32.const 3)    
  (func (type 0)
    local.get 0
    i32.eqz
    if (type 1)
      i32.const 42
      return
    else
      i32.const 1
      return
    end
    i32.const 3)    
  (func (type 0)
    local.get 0
    call 0)
  (start 4))
