(module
  (global i32 
    i32.const 1)

  (global i32 
    i32.const 2)

  (global i32 
    i32.const 3)

  (global i32
    i32.const 4)

  (global i32 
    i32.const 5)

  (global i32 
    i32.const 6)

  (type (func (result i32)))
  (func (type 0)
    global.get 0
    global.get 1
    i32.add
    global.get 2
    i32.add
    global.get 3
    i32.add
    global.get 4
    i32.add
    global.get 5
    i32.add)
    
  (start 0))
