(module
  (type (func (param i32) (result i32)))

  (func (type 0) (local i32)(local i32)(local i32)(local i32)(local i32)(local i32)
    local.get 0
    i32.const 1
    i32.add
    local.tee 1
    i32.const 1
    i32.add
    local.tee 2
    i32.const 1
    i32.add
    local.tee 3
    i32.const 1
    i32.add
    local.tee 4
    i32.const 1
    i32.add
    local.tee 5
    i32.const 1
    i32.add
    local.tee 6
    local.get 5
    local.get 4
    local.get 3
    local.get 2
    local.get 1
    local.get 0
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add)

  (start 0))
