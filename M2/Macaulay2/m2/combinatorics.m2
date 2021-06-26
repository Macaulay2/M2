--		Copyright 1993-1999 by Daniel R. Grayson

needs "remember.m2" -- for memoize

subsets(ZZ,ZZ) := List => (n,j) -> (
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

subsets(List,ZZ) := List => (s,j) -> apply(subsets(#s,j),v->apply(v,i->s#i))
subsets(Sequence,ZZ) := List => (s,j) -> subsets(toList s,j)
subsets(Set,ZZ) := List => (s,j) -> apply(subsets(toList s, j), set)
subsets Set := List => x -> apply(subsets toList x, set)
subsets List := List => x -> (
     if #x === 0 then {x}
     else (
	  a := x#-1;
	  x = drop(x,-1);
	  s := subsets x;
	  join(s,apply(s,y->append(y,a)))))
subsets ZZ := List => n -> (
    if n < 0 then error "expected a nonnegative number";
    subsets toList (0 .. n-1))

Partition = new Type of BasicList
Partition _ ZZ := (p,i) -> p#i
partitions(ZZ,ZZ) := List => memoize (
     (n,k) -> (
     	  if k > n then k=n;
	  if n < 0 then {}
	  else if n === 0 then {new Partition from {}} 
     	  else if k === 0 then {} 
     	  else if k === 1 then {new Partition from apply(n,i->1)} 
     	  else join( 
	       apply(partitions(n-k,k),i -> prepend(k,i)), 
	       partitions(n,k-1))))
partitions ZZ := List => (n) -> partitions(n,n)

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
