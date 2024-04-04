(module
  (global i32 
    i32.const 1)

  (global i32 
    i32.const 3)

  (global i32 
    i32.const 5)

  (global i32
    i32.const 7)

  (global i32 
    i32.const 8)

  (global i32 
    i32.const 11)

  (type (func (param i32) (param i32) (param i32) (result i32)))
  (func (type 0) (local i32)(local i32)(local i32)
    global.get 0
    global.get 1
    global.get 2
    local.set 5
    local.set 4
    local.set 3
    local.get 2
    local.get 1
    local.get 0
    global.set 3
    global.set 4
    global.set 5
    local.get 0
    global.get 0
    i32.sub
    local.get 1
    global.get 1
    i32.sub
    local.get 2
    global.get 2
    i32.sub
    local.get 3
    global.get 3
    i32.sub
    local.get 4
    global.get 4
    i32.sub
    local.get 5
    global.get 5
    i32.sub
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add)
   
  (start 0))
