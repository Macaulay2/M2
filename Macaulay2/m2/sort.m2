--		Copyright 1993-2002 by Daniel R. Grayson

-- Jun 25, 2002: these default comparisons are confusing!

-- Type ? Type := (x,y) -> hash x ? hash y
-- Thing ? Thing := (x,y) -> (
--      if x === y then symbol ==
--      else (
-- 	  s := class x ? class y;
-- 	  if s =!= symbol == then s
-- 	  else (
-- 	       t := parent x ? parent y;
-- 	       if t =!= symbol == then t
-- 	       else hash x ? hash y
-- 	       )))
-- 

whichway := symbol >
compare := (a,b) -> (
     c := (try a ? b else 
     	  if class a === class b 
     	  then hash a ? hash b 
     	  else try toString class a ? toString class b 
	  else hash class a ? hash class b
	  );
     if c === incomparable then error "incomparable elements encountered in sort";
     c =!= whichway			-- this relation better be transitive
     )

basicsort := (v) -> if #v <= 1 then v else (	      -- quick sort algorithm
     v = newClass(MutableList, v);
     subsort := (l,r) -> (			     -- sort l .. r inclusive
	  p := l + random(r+1-l);
	  pivot := v#p;
	  v#p = v#l;
	  v#l = pivot;
	  i := l+1;
	  j := r;
	  while i <= j do (
	       -- spots 1 .. i-1 contain elements less or equal to the pivot
	       -- spots j+1 .. r contain elements greater or equal to the pivot
	       -- when i > j we've partitioned all the elements into two parts
	       if compare(v#i,pivot) then i = i+1
	       else if compare(pivot,v#j) then j = j-1
	       else ( tmp := v#i; v#i = v#j; v#j = tmp; i = i+1; j = j-1 ));
	  if l+1 < j then subsort(l+1,j);
	  if j+1 < r then subsort(j+1,r);
	  scan(l+1 .. j, k-> v#(k-1) = v#k);
	  v#j = pivot;
	  );
     -- subsort = on subsort;				    -- debugging
     subsort(0,#v - 1);
     toList v)

rsort = (v) -> (
     whichway = symbol <;
     basicsort v)

sort = (v) -> (
     whichway = symbol >;
     basicsort v)

lexcompare := (v,w,i) -> (
     if i === # v
     then (
	  if i === # w
     	  then symbol ==
     	  else symbol <)
     else (
	  if i === # w
	  then symbol >
	  else (
     	       s := v#i ? w#i;
     	       if s === symbol < then symbol <
     	       else if s === symbol > then symbol >
     	       else lexcompare(v,w,i+1))))


BasicList ? BasicList := (v,w) -> lexcompare(v,w,0)
Sequence ? Sequence := (v,w) -> lexcompare(v,w,0)
