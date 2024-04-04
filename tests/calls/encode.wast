(module
  (type (func (result i32)))

  ;; (define (encode_helper x y n)
  ;;   (cond
  ;;     ((and (zero? x) (zero? y))
  ;;      n)
  ;;     ((zero? x)
  ;;      (encode_helper (sub1 y) x (add1 n)))
  ;;     ((<= x y)
  ;;      (encode_helper (sub1 x) y (add1 n)))
  ;;     (else
  ;;      (encode_helper x (add1 y) (add1 n)))))
  ;;
  (type (func (param i32) (param i32) (param i32) (result i32)))
  (func (type 1)
    local.get 0
    i32.eqz
    if (type 0)
      local.get 1
      i32.eqz
      if (type 0)
        local.get 2
        return
      else
        local.get 1
        i32.const 1
        i32.sub
        local.get 0
        local.get 2
        i32.const 1
        i32.add
        call 0
      end
    else
      local.get 0
      local.get 1
      i32.le_u
      if (type 0)
        local.get 0
        i32.const 1
        i32.sub
        local.get 1
        local.get 2
        i32.const 1
        i32.add
        call 0
      else
        local.get 0
        local.get 1
        i32.const 1
        i32.add
        local.get 2
        i32.const 1
        i32.add
        call 0
      end
    end)

  ;; (define (encode x y)
  ;;   (encode_helper x y 0))
  ;;
  (type (func (param i32) (param i32) (result i32)))
  (func (type 2)
    local.get 0
    local.get 1
    i32.const 0
    call 0)

  (start 1))
