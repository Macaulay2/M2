--		Copyright 1994 by Daniel R. Grayson

List _ ZZ     := (x,i) -> x#i
ggPush List  := v -> (ggINTARRAY, gg v)
List | List  := join

List + List  := {
     List,
     (v,w) -> apply(v,w,plus),
     TT "v + w", " -- the sum of two vectors represented as lists."
     }

     - List  := v -> apply(v,minus)
List - List  := (v,w) -> apply(v,w,difference)
Thing * List := (a,v) -> apply(v,x->a * x)
List / Thing := (v,b) -> apply(v,x->x / b)
Sequence _ List := List _ List := (x,y) -> apply(splice y, i -> x#i)

maxPosition = x -> (
     if # x === 0 then error "expected a nonempty list" 
     else (
     	  m := x#0; 
	  pos := 0;
	  scan(1 .. # x-1, i -> if x#i>m then (m=x#i;pos=i));
	  pos))


minPosition = x -> (
     if # x === 0 then error "expected a nonempty list" 
     else (
     	  m := x#0; 
	  pos := 0;
	  scan(1 .. # x-1, i -> if x#i>m then (m=x#i;pos=i));
	  pos))


number = x -> # select x

document { quote number,
     TT "number(x,f)", " -- the number of elements e of the list x for which f(e) is true.",
     PARA,
     "See also ", TO "positions", " and ", TO "select", "."
     }

all = (x,p) -> not any(x, i -> not p i)

document { quote all,
     TT "all(v,f)", " -- whether each element x of a list or hash table
     v has f(x) true.",
     PARA,
     "Returns the value true if all elements v#i of the list v yield 
     the value true when the function f is applied, otherwise returns 
     false.  For hash tables, the function is applied to all its key/value
     pairs (k,v), just as with ", TO "any", ".",
     PARA,
     SEEALSO ( "scan", "apply", "select", "any", "member" )
     }

same = v -> (
     -- this could be compiled for speed
     if # v === 0
     then true
     else (
	  w := v#0;
	  all(1 .. # v - 1, i -> w === v#i)
	  )
     )

document { quote same,
     TT "same v", " -- whether every element of the list v is the same.
     The comparison is done with ", TO "==", "."
     }

member(Thing,Sequence) := member(Thing,List) := (c,x) -> any(x, i -> c===i)

document { quote member,
     TT "member(e,x)", " -- whether e is an element of the list x.",
     PARA,
     "See also ", TO "positions", "."
     }

sum(List) := x -> plus unlist x

sum(List,Function) := 
sum(Sequence,Function) := 
sum(ZZ,Function) := (n,f) -> (
     s := 0;
     g := x -> (
	  s = f x; 
	  g = x -> s = s + f x);
     scan(n,x -> g x);
     s);

sum(Sequence, List, Function) :=
sum(List, Sequence, Function) :=
sum(Sequence, Sequence, Function) :=
sum(List, List, Function) := (v,w,f) -> sum(apply(v,w,identity),f)

document { quote sum,
     TT "sum(v,f)", " -- yields the sum of the expressions obtained by
     applying f to each of the elements of the list v.",
     PARA,
     EXAMPLE "sum(1 .. 10, i -> i^2)",
     PARA,
     NOINDENT,
     TT "sum(v,w,f)", " -- yields the sum of the expressions obtained by
     applying f to each of the pairs (i,j) of elements from the lists or
     sequences v and w, which should be of the same length.",
     PARA,
     NOINDENT,
     TT "sum(n,f)", " -- is equivalent to sum(0 .. n-1, f) when n is an integer",
     PARA,
     "sum v    -- yields the sum of the elements in the list v.",
     PARA,
     "See also ", TO "product", "."
     }

product List := x -> times unlist x

product(List,Function) := 
product(Sequence,Function) := 
product(ZZ,Function) := (n,f) -> (
     s := 1;
     g := x -> (
	  s = f x; 
	  g = x -> s = s * f x);
     scan(n,x -> g x);
     s);

product(Sequence, List, Function) :=
product(List, Sequence, Function) :=
product(Sequence, Sequence, Function) :=
product(List, List, Function) := (v,w,f) -> product(apply(v,w,identity),f)

document { quote product,
     TT "product(v,f)", " -- yields the product of the expressions obatained by applying 
     the function f to each of the elements of the list v.",
     PARA,
     EXAMPLE "product(1 .. 10,i->i)",
     NOINDENT,
     TT "product(v,w,f)", " -- yields the product of the expressions obtained by
     applying f to each of the pairs (i,j) of elements from the lists or
     sequences v and w, which should be of the same length.",
     PARA,
     NOINDENT,
     TT "product(n,f)", " -- is equivalent to product(0 .. n-1,f) when n is an integer",
     NOINDENT,
     TT "product v", " -- yields the product of the elements of the list v.",
     SEEALSO "sum"
     }
