--		Copyright 1993-2002 by Daniel R. Grayson

-- this code has been moved to actors3.d

whichway := symbol >
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
	       if (v#i ? pivot) =!= whichway then i = i+1
	       else if (pivot ? v#j) =!= whichway then j = j-1
	       else ( tmp := v#i; v#i = v#j; v#j = tmp; i = i+1; j = j-1 ));
	  if l+1 < j then subsort(l+1,j);
	  if j+1 < r then subsort(j+1,r);
	  scan(l+1 .. j, k-> v#(k-1) = v#k);
	  v#j = pivot;
	  );
     -- subsort = on subsort;				    -- debugging
     subsort(0,#v - 1);
     toList v)

oldrsort = (v) -> (
     whichway = symbol <;
     basicsort v)

oldsort = (v) -> (
     whichway = symbol >;
     basicsort v)

-- this code mode to actors3.d in compare(left:Expr,right:Expr):Expr
-- lexcompare := (v,w,i) -> (
--      if i === # v
--      then (
-- 	  if i === # w
--      	  then symbol ==
--      	  else symbol <)
--      else (
-- 	  if i === # w
-- 	  then symbol >
-- 	  else (
--      	       s := v#i ? w#i;
--      	       if s === symbol < then symbol <
--      	       else if s === symbol > then symbol >
--      	       else lexcompare(v,w,i+1))))

List ? List := (s,t) -> toSequence s ? toSequence t

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
