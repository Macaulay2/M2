--		Copyright 1993-1999 by Daniel R. Grayson

subsets(ZZ,ZZ) := List => (n,j) -> (
     subsetn := (n,j) -> (
	  if n<j then {}
	  else if n===j then {toList (0 .. n-1)}
	  else if j===0 then {{}}
	  else join(subsetn(n-1,j), apply(subsetn(n-1,j-1),s->append(s,n-1))));
     subsetn = memoize subsetn;
     result := subsetn(n,j);
     subsetn = null;
     result)

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
     if #lambda === 0 then {} else (
     	  slot := #lambda-1;
     	  new Partition from
	  for i from 1 to lambda#0 list (
	       while lambda#slot < i do slot=slot-1;
	       slot+1)
     ))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
