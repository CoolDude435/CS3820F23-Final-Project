(module
  (type (func (param i32) (result i32)))
  (func (type 0)
    local.get 0
    i32.popcnt
    local.get 0
    i32.const 0
    i32.const 1
    i32.sub
    i32.xor
    i32.popcnt
    i32.add)
  (start 0))
