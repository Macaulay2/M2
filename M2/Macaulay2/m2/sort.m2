--		Copyright 1994 by Daniel R. Grayson

le := (a,b) -> (
     c := a ? b;
     if c === incomparable then error "incomparable elements encountered in sort";
     c =!= quote >
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

document { quote sort,
     TT "sort v", " -- produces a sorted version of the list v.",
     PARA,
     "The sort function uses ", TO "<=", " to compare elements of the
     list, which in turn calls upon ", TO "?", ".",
     EXAMPLE {
	  "sort {c,e,a,f,b,f}",
	  "sort {4,2,6,3,8,2}"
	  },
     SEEALSO { "rsort", "<=", "?" }
     }

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


document { quote rsort,
     TT "rsort v", " -- produces a reverse sorted version of the list v.",
     PARA,
     "The rsort function uses ", TO "<=", " to compare elements of the
     list, which in turn calls upon ", TO "?", ".",
     EXAMPLE {
	  "rsort {g,d,w,s,c,a,r}",
	  "rsort {4,2,3,1}",
	  },
     SEEALSO { "sort", "<=", "?" }
     }

lexcompare := (v,w,i) -> (
     if i === # v
     then (
	  if i === # w
     	  then quote ==
     	  else quote <)
     else (
	  if i === # w
	  then quote >
	  else (
     	       s := v#i ? w#i;
     	       if s === quote < then quote <
     	       else if s === quote > then quote >
     	       else lexcompare(v,w,i+1))))


BasicList ? BasicList := (v,w) -> lexcompare(v,w,0)
Sequence ? Sequence := (v,w) -> lexcompare(v,w,0)
Type ? Type := (x,y) -> hash x ? hash y
Thing ? Thing := (x,y) -> (
     if x === y then quote ==
     else (
	  s := class x ? class y;
	  if s =!= quote == then s
	  else (
	       t := parent x ? parent y;
	       if t =!= quote == then t
	       else hash x ? hash y
	       )))

