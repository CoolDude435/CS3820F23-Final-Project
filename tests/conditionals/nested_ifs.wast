(module
  (type (func))
  (type (func (param i32) (result i32)))

  (func (type 1)
    local.get 0
    local.get 0
    i32.const 0
    i32.ne
    if (type 0)
      local.get 0
      i32.const 1
      i32.sub
      local.tee 0
      local.get 0
      i32.const 0
      i32.ne
      if (type 0)
        local.get 0
        i32.const 1
        i32.sub
        local.tee 0
        local.get 0
        i32.const 0
        i32.ne
        if (type 0)
          local.get 0
          i32.const 1
          i32.sub
          local.tee 0
          local.get 0
          i32.const 0
          i32.ne
          if (type 0)
            local.get 0
            i32.const 1
            i32.sub
            local.tee 0
            local.get 0
            i32.const 0
            i32.ne
            if (type 0)
              local.get 0
              i32.const 1
              i32.sub
              local.tee 0
              local.get 0
              i32.const 0
              i32.ne
              if (type 0)
                local.get 0
                i32.const 1
                i32.sub
                local.tee 0
              else
                i32.const 42
              end        
            else
              i32.const 42
            end      
          else
            i32.const 42
          end    
        else
          i32.const 42
        end  
      else
        i32.const 42
      end
    else
      i32.const 42
    end)

  (start 0))
