(module
  (type (func (result i32))) ;; TYPE 0

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
  (type (func (param i32) (param i32) (param i32) (result i32))) ;; TYPE 1
  (func (type 1) ;; FUNC 0
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
  (type (func (param i32) (param i32) (result i32))) ;; TYPE 2
  (func (type 2) ;; FUNC 1
    local.get 0
    local.get 1
    i32.const 0
    call 0)

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
  (type (func (param i32) (param i32) (param i32) (param i32) (result i32))) ;; TYPE 3
  (func (type 3) ;; FUNC 2
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
        call 2
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
          call 2
        else
          local.get 0
          local.get 1
          local.get 2
          i32.const 1
          i32.sub
          local.get 3
          i32.const 1
          i32.add
          call 2
        end
      end
    end)

  ;; (define (decode_x n)
  ;;   (decode_x_helper n 0 0 0))
  ;;
  (type (func (param i32) (result i32))) ;; TYPE 4
  (func (type 4) ;; FUNC 3
    local.get 0
    i32.const 0
    i32.const 0
    i32.const 0
    call 2)
  
  ;; (define (divmod_encode n d q)
  ;;   ...)
  ;;
  (type (func (param i32) (param i32) (param i32) (result i32))) ;; TYPE 5
  (func (type 5) ;; FUNC 4
    local.get 0
    local.get 1
    i32.lt_u
    if (type 0)
      local.get 2
      local.get 0
      call 1
    else
      local.get 0
      local.get 1
      i32.sub
      local.get 1
      local.get 2
      i32.const 1
      i32.add
      call 4
    end)

  ;; (define (divmod_div n d)
  ;;   (= (decode_x (divmod_encode n d 0))
  ;;      (quotient n d)))
  ;;
  (type (func (param i32) (param i32) (result i32))) ;; TYPE 6
  (func (type 6) ;; FUNC 5
    local.get 0
    local.get 1
    i32.const 0
    call 4
    call 3
    local.get 0
    local.get 1
    i32.div_u
    i32.eq)

  (start 5))
