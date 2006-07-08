--		Copyright 1993-1999 by Daniel R. Grayson

positions = method()
positions(VisibleList,Function) := (v,f) -> (
     apply(select(pack(2, mingle{v,0 .. #v-1}), p -> f p#0), p -> p#1)
     )

position = method(Options => {Reverse => false})
position(VisibleList,Function) := o -> (v,f) -> (
     if o.Reverse
     then ( n := #v; for i to #v-1 do if f v#(n-1-i) then return n-1-i)
     else (for i to #v-1 do if f v#i then return i)
     )
position(VisibleList,VisibleList,Function) := o -> (v,w,f) -> (
     if #v != #w then error "expected lists of the same length";
     if o.Reverse
     then ( n := #v; for i to #v-1 do if f(v#(n-1-i),w#(n-1-i)) then return n-1-i)
     else (for i to #v-1 do if f(v#i,w#i) then return i)
     )

delete = (x,v) -> select(v, i -> i =!= x)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
