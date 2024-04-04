(module
  (type (func (result i32)))
  (type (func (param i32) (result i32)))
  (type (func (param i32) (result i32) (result i32)))
  (func (type 2) (local i32)
    local.get 0
    local.get 0)
  (func (type 1) (local i32) 
    i32.const 1 
    local.set 1 ;; y = 1
    local.get 0 ;; stack: x
    loop (type 1)    
      call 0  ;; stack: x x
      i32.eqz ;; stack: x=0 x
      if (type 1)
        local.get 1 ;; stack: y x
        return      ;; return y
      else
        call 0      ;; stack: x x
        local.get 1 ;; stack: y x x
        i32.mul     ;; stack: y*x x
        local.set 1 ;; stack x
        i32.const 1 ;; stack 1 x
        i32.sub     ;; stack x-1
        br 1        ;; block 0 is the if; block 1 is the loop
      end      
    end)
  (func (type 1)
    local.get 0
    call 1)
  (start 2))
