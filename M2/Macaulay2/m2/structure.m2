--		Copyright 1993-1999 by Daniel R. Grayson

positions = method()
positions(VisibleList,Function) := (v,f) -> (
     apply(select(pack(2, mingle{v,0 .. #v-1}), p -> f p#0), p -> p#1)
     )

position = method()
position(VisibleList,Function) := (v,f) -> (
     ret := null;
     select(
	  1, 
	  apply(#v, i -> (i,v#i)), 
	  (i,x) -> f(x) and (ret = i; true));
     ret)

delete = (x,v) -> select(v, i -> i != x)
