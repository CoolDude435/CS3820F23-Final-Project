(module
  (type (func (param i32) (param i32)))
  (type (func (param i32) (result i32)))

  (func (type 1)
    local.get 0   ;; [10],       #0 = 10
    i32.const 236 ;; [236, 10],  #0 = 10
    
    i32.const 1   ;; [1,236,10], #0 = 10
    
    if (type 0)
                  ;; [236, 10],  #0 = 10
      i32.add     ;; [246],      #0 = 10
      local.set 0 ;; [],         #0 = 246
    else
                  ;; note: unreachable
                  ;; [236, 10],  #0 = 10
      return      ;; [],         #0 = 10
    end

    local.get 0)  ;; [246],      #0 = 246

  (start 0))
