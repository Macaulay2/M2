net PolyhedralObject := X -> (
   properties := getAvailableProperties X;
   horizontalJoin flatten (
      "{",
      -- prints the parts vertically
      stack (horizontalJoin \ sort apply(toList properties, 
         property -> (
            val := getProperty(X, property);
            local rhs;
            -- Avoid recursion, e.g. for normalFans
            if not (instance(val, Matrix) or 
                     instance(val, Vector) or 
                     instance(val, ZZ) or 
                     instance(val, List) or 
                     instance(val, Sequence)) then (
               rhs = class val
            )
            else
               rhs = val;
            (net property, " => ", net rhs)
         )
      )),
      "}" 
   )
)

