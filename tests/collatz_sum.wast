(module
  (type (func (result i32)))              ;; [] -> [i32]
  (type (func (param i32) (result i32)))  ;; [i32] -> [i32]
  (type (func (param i32) (result i32) (result i32)))
                                          ;; [i32] -> [i32, i32]
  (func (type 2) ;; dup : [i32] -> [i32, i32]
    local.get 0
    local.get 0)
  (func (type 1) ;; step :: [i32] -> [i32]
    local.get 0
    i32.const 2
    i32.rem_u
    i32.eqz
    if (type 0)
      local.get 0
      i32.const 2
      i32.div_u
    else
      local.get 0
      i32.const 3
      i32.mul
      i32.const 1
      i32.add
    end)
  (func (type 1) (local i32) ;; loop :: [i32] -> [i32]
    local.get 0
    local.set 1
    loop (type 0)
      local.get 0
      call 1 ;; step
      local.tee 0
      local.get 1
      i32.add
      local.set 1
      local.get 0
      i32.const 1
      i32.ne
      br_if 0
    end
    local.get 1)
  (func (type 1)
    local.get 0
    call 2)
  (start 3))
