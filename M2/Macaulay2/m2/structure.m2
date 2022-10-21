--		Copyright 1993-1999 by Daniel R. Grayson

needs "methods.m2"

positions = method()
positions(MutableList,Function) := 
positions(VisibleList,Function) := (v,f) -> for i from 0 to #v-1 list if f v#i then i else continue

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

delete = method()
delete(Thing, VisibleList) := (x,v) -> select(v, i -> i =!= x)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
