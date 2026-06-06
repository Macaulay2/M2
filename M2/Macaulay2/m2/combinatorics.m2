--		Copyright 1993-1999 by Daniel R. Grayson

needs "remember.m2" -- for memoize

-----------------------------------------------------------------------------

subsets = method(TypicalValue => List)
subsets(ZZ, ZZ) := (n, j) -> (
     if n < 0 then error "expected a nonnegative number";
     if j < 0 then return {};
     if j == 0 then return {{}};
     if j == 1 then return toList apply(0 .. n-1, i -> {i});
     if 2*j > n then return (
	  y := toList (0 .. n-1);
	  apply(reverse subsets(n,n-j), s -> y - set s));
     x := join apply(1 .. n-1, j -> apply(0 .. j-1, i -> (i,j)));
     scan(j-2, i -> x = join apply(x, s -> apply(0 .. s#0 - 1, i -> prepend(i,s))));
     toList apply(x,toList))

subsets(Set,      ZZ) := (s, j) -> apply(subsets(toList s, j), set)
subsets(List,     ZZ) := (s, j) -> apply(subsets(#s, j), v -> s_v)
subsets(Sequence, ZZ) := (s, j) -> subsets(toList s, j)

subsets ZZ   := n -> subsets toList(0 ..< n)
subsets Set  := x -> apply(subsets toList x, set)
subsets List := x -> (
     if #x === 0 then {x}
     else (
	  a := x#-1;
	  x = drop(x,-1);
	  s := subsets x;
	  join(s,apply(s,y->append(y,a)))))

-----------------------------------------------------------------------------

Partition = new Type of BasicList
Partition _ ZZ := (p,i) -> p#i

partitions = method(TypicalValue => List)
partitions(ZZ, ZZ) := memoize(
     (n,k) -> (
     	  if k > n then k=n;
	  if n < 0 then {}
	  else if n === 0 then {new Partition from {}} 
     	  else if k === 0 then {} 
     	  else if k === 1 then {new Partition from apply(n,i->1)} 
     	  else join( 
	       apply(partitions(n-k,k),i -> prepend(k,i)), 
	       partitions(n,k-1))))
partitions ZZ := n -> partitions(n, n)

compositions = method(TypicalValue => List)
compositions(ZZ, ZZ) := memoize(
    (n, k) -> (
	if k  < 0
	or n == 0 then {} else
	if k == 0 then { toList(n:0) }
	else join(
	    nz1 := toList(n-1:0) | {1};
	    apply(compositions(n - 1, k), s -> s | {0}),
	    apply(compositions(n, k - 1), s -> s + nz1))
    ))
compositions ZZ := n -> compositions(n, n)

conjugate Partition := Partition => (lambda) -> (
     if #lambda === 0 then new Partition from {} else (
     	  slot := #lambda-1;
     	  new Partition from
	  for i from 1 to lambda#0 list (
	       while lambda#slot < i do slot=slot-1;
	       slot+1)
     ))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
