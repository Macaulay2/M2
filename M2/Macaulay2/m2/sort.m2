--		Copyright 1994 by Daniel R. Grayson

le := (a,b) -> (
     c := a ? b;
     if c === incomparable then error "incomparable elements encountered in sort";
     c =!= symbol >
     )

sort = (v) -> (
     -- quick sort routine
     if #v <= 1
     then v
     else (
	  v = newClass(MutableList, v);
	  subsort := (l,r) -> (
	       c := v#l;
	       i := l+1;
	       j := r;
	       while i <= j do (
		    if le(v#i,c) then i=i+1
		    else if le(c,v#j) then j=j-1
		    else ( w := v#i; v#i = v#j; v#j = w ));
	       if l<j then subsort(l+1,j);
	       if i<=r then subsort(i,r);
	       scan(l+1 .. j, k-> v#(k-1) = v#k);
	       v#j = c;
	       );
	  subsort(0,#v - 1);
	  toList v))

rsort = (v) -> (
     -- quick rsort routine
     if # v <= 1
     then v
     else (
	  v = newClass(MutableList, v);
	  subrsort := (l,r) -> (
	       c := v#l;
	       i := l+1;
	       j := r;
	       while i <= j do (
		    if v#i >= c then i=i+1
		    else if c >= v#j then j=j-1
		    else ( w := v#i; v#i = v#j; v#j = w ));
	       if l<j then subrsort(l+1,j);
	       if i<=r then subrsort(i,r);
	       scan(l+1 .. j, k-> v#(k-1) = v#k);
	       v#j = c;
	       );
	  subrsort(0,#v - 1);
	  toList v));

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
Type ? Type := (x,y) -> hash x ? hash y
Thing ? Thing := (x,y) -> (
     if x === y then symbol ==
     else (
	  s := class x ? class y;
	  if s =!= symbol == then s
	  else (
	       t := parent x ? parent y;
	       if t =!= symbol == then t
	       else hash x ? hash y
	       )))

