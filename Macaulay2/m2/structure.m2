--		Copyright 1994 by Daniel R. Grayson

first = x -> x#0

last = x -> x#-1

positions = (v,f) -> (
     apply(select(pack(mingle{v,0 .. #v-1},2), p -> f p#0), p -> p#1)
     )

position = (v,f) -> (
     ret := null;
     select(
	  1, 
	  apply(#v, i -> (i,v#i)), 
	  (i,x) -> f(x) and (ret = i; true));
     ret)

delete = (x,v) -> select(v, i -> i != x)
