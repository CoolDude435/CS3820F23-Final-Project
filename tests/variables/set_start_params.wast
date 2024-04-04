(module
  (type (func (param i32) (param i32) (param i32) (result i32)))

  (func (type 0)
    local.get 1
    local.set 0
    local.get 2
    local.set 1
    local.get 2
    local.get 1
    local.get 0
    i32.add
    i32.add)

  (start 0))
