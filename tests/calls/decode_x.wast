(module 
  (type (func (result i32))) ;; TYPE 0

  ;; (define (decode_x_helper n x y m)
  ;;   (cond
  ;;     ((= m n)
  ;;      x)
  ;;     ((= y 0)
  ;;      (decode_x_helper n y (add1 x) (add1 m)))
  ;;     ((< x y)
  ;;      (decode_x_helper n (add1 x) y (add1 m)))
  ;;     (else
  ;;      (decode_x_helper n x (sub1 y) (add1 m)))))
  ;;
  (type (func (param i32) (param i32) (param i32) (param i32) (result i32))) ;; TYPE 1
  (func (type 1) ;; FUNC 0
    local.get 3
    local.get 0
    i32.eq
    if (type 0)
      local.get 1
      return
    else
      local.get 2
      i32.eqz
      if (type 0)
        local.get 0
        local.get 2
        local.get 1
        i32.const 1
        i32.add
        local.get 3
        i32.const 1
        i32.add
        call 0
      else
        local.get 1
        local.get 2
        i32.lt_u
        if (type 0)
          local.get 0
          local.get 1
          i32.const 1
          i32.add
          local.get 2
          local.get 3
          i32.const 1
          i32.add
          call 0
        else
          local.get 0
          local.get 1
          local.get 2
          i32.const 1
          i32.sub
          local.get 3
          i32.const 1
          i32.add
          call 0
        end
      end
    end)

  ;; (define (decode_x n)
  ;;   (decode_x_helper n 0 0 0))
  ;;
  (type (func (param i32) (result i32))) ;; TYPE 2
  (func (type 2) ;; FUNC 1
    local.get 0
    i32.const 0
    i32.const 0
    i32.const 0
    call 0)
  
  (start 1))
