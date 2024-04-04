(module
  (global i32 
    i32.const 6)

  (global i32 
    i32.const 5)

  (global i32 
    i32.const 4)

  (global i32
    i32.const 3)

  (global i32 
    i32.const 2)

  (global i32 
    i32.const 1)

  (type (func (result i32)))
  (func (type 0)
    global.get 1
    global.set 0
    global.get 3
    global.set 2
    global.get 5
    global.set 4
    global.get 0
    global.get 1
    global.get 2
    global.get 3
    global.get 4
    global.get 5
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add)
    
  (start 0))
