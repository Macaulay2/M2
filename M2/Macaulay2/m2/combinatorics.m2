--		Copyright 1994 by Daniel R. Grayson

subsets(ZZ,ZZ) := (n,j) -> (
     subsetn := (n,j) -> (
	  if n<j then {}
	  else if n===j then {toList (0 .. n-1)}
	  else if j===0 then {{}}
	  else join(subsetn(n-1,j), apply(subsetn(n-1,j-1),s->append(s,n-1))));
     subsetn = memoize subsetn;
     result := subsetn(n,j);
     subsetn = null;
     result)

subsets(List,ZZ) := (s,j) -> apply(subsets(#s,j),v->apply(v,i->s#i))
subsets(Sequence,ZZ) := (s,j) -> subsets(toList s,j)
subsets(Set,ZZ) := (s,j) -> apply(subsets(toList s, j), set)

subsets Set := x -> set subsets toList x
subsets List := x -> (
     if #x === 0 then {x}
     else (
	  a := x#-1;
	  x = drop(x,-1);
	  s := subsets x;
	  join(s,apply(s,y->append(y,a)))))

partitions(ZZ,ZZ) := memoize (
     (n,k) -> (
     	  if k > n then k=n;
     	  if n <= 0 then {{}} 
     	  else if k === 0 then {} 
     	  else if k === 1 then {apply(n,i->1)} 
     	  else join( 
	       apply(partitions(n-k,k),i -> prepend(k,i)), 
	       partitions(n,k-1))))

partitions ZZ := (n) -> partitions(n,n)

document { quote subsets,
     TT "subsets(n,j)", " -- for an integer n, yields a list of those
     subsets of {0, ..., n-1} which have j elements.",
     BR, NOINDENT,
     TT "subsets(s,j)", " -- yields a list of those subsets of the list or 
     set s which have j elements.",
     BR, NOINDENT,
     TT "subsets s", " -- yields a list of the subsets of the list or set s.",
     PARA,
     "In the case where s is a list, the subsets are returned as lists whose
     elements are in the same order.",
     PARA,
     EXAMPLE "subsets(set {a,b,c},2)",
     EXAMPLE "subsets(3,2)",
     EXAMPLE "subsets {1,2,3}"
     }

TEST "
assert( subsets(4,2) === {{0,1},{0,2},{1,2},{0,3},{1,3},{2,3}} )
assert( subsets({a,b,c,d},2) === {{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}} )
assert( 
     set subsets(set {a,b,c,d},2) === 
     set apply({{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}},set) )
assert( partitions 4 === {{4},{3,1},{2,2},{2,1,1},{1,1,1,1}} )
assert( partitions(5,3) === {{3,2},{3,1,1},{2,2,1},{2,1,1,1},{1,1,1,1,1}} )
"


document { quote partitions,
     TT "partitions n", " -- returns a list of the partitions of the integer n.",
     BR, NOINDENT,
     TT "partitions(n,k)", " -- returns a list of the partitions of the integer n
     into terms each of which does not exceed k.",
     PARA,
     EXAMPLE "partitions 4",
     EXAMPLE "partitions(4,2)",
     }
