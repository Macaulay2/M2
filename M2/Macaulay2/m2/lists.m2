--		Copyright 1994 by Daniel R. Grayson

singleton = x -> toSequence {x}

List _ ZZ     := (x,i) -> x#i
ggPush List  := v -> (ggINTARRAY, gg v)
List | List  := join

List + List  := (v,w) -> apply(v,w,plus)
document { (quote +, List, List),
     TT "v + w", " -- the sum of two vectors represented as lists."
     }

     - List  := v -> apply(v,minus)
List - List  := (v,w) -> apply(v,w,difference)
Thing * List := (a,v) -> apply(v,x->a * x)
List / Thing := (v,b) -> apply(v,x->x / b)
Sequence _ List := List _ List := (x,y) -> apply(splice y, i -> x#i)

document { (quote _, List, List),
     TT "w_{i,j,...}", " -- selects entries from a list or sequence ", TT "w", ".",
     PARA,
     EXAMPLE {
	  "w = {a,b,c,d,e,f,g,h};",
      	  "w_{1,3,4}",
	  },
     "We can use this operation to compute composition of permutations
     represented as lists.",
     EXAMPLE "{4,2,3,1,0} _ {2,1,3,4,0}"
     }

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
     SEEALSO { "scan", "apply", "select", "any", "member" }
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
     TT "member(e,x)", " -- whether ", TT "e", " is an element of the list, set, or 
     sequence ", TT "x", ".",
     PARA,
     EXAMPLE {
	  "x = {a,b,c,d,e};",
      	  "member(c,x)",
      	  "member(f,x)",
      	  {"positions", "Set"}
	  },
     }

sum(List) := x -> plus toSequence x

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
     TT "sum", " -- provides the sum of the members of a list, set, 
     or chain complex, optionally with a function applied to each one.",
     PARA,
     MENU {
	  TO {(sum, List), " -- sum the elements of a list or sequence"},
	  TO {(sum, List, List, Function), " -- sum results of applying a function"},
	  TO {(sum, List, Function), " -- sum results of applying a function"},
	  TO {(sum, ZZ, Function), " -- sum consecutive values of a function"},
	  TO {(sum, Tally), " -- sum elements of a tally"},
	  TO {(sum, Set), " -- sum elements of a tally"},
	  TO {(sum, ChainComplex), " -- sum modules in a chain complex"},
	  TO {(sum, ChainComplexMap), " -- sum components in a map of chain complexes"}
	  },
     SEEALSO "product"
     }
document { (sum, List),
     TT "sum v", " -- yields the sum of the elements in the list v.",
     PARA,
     EXAMPLE "sum {1,2,3,4,5}",
     SEEALSO "sum"
     }
document { (sum, List, List, Function),
     TT "sum(v,w,f)", " -- yields the sum of the results obtained by
     applying ", TT "f", " to each of the pairs ", TT "(i,j)", " of elements from 
     the lists or sequences ", TT "v", " and ", TT "w", ", which should be of 
     the same length.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y,z];",
      	  "sum({2,3,4},{x,y,z},(i,j)->j^i)",
	  },
     SEEALSO "sum"
     }
document { (sum, List, Function),
     TT "sum(v,f)", " -- yields the sum of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA,
     EXAMPLE "sum(1 .. 10, i -> i^2)",
     SEEALSO "sum"
     }
document { (sum, ZZ, Function),
     TT "sum(n,f)", " -- compute the sum ", TT "f(0) + f(1) + ... + f(n-1)", ".",
     PARA,
     EXAMPLE "sum(10, i -> i^2)",
     SEEALSO "sum"
     }
document { (sum, Tally),
     TT "sum v", " -- yields the sum of the elements in the tally ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = tally{1,1,1,1,1,10,10,10,100,100}",
      	  "sum a",
	  },
     SEEALSO "sum"
     }
document { (sum, Set),
     TT "sum v", " -- yields the sum of the elements in the set ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = set{1,100,10000}",
      	  "sum a",
	  },
     SEEALSO "sum"
     }

product List := x -> times toSequence x

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
     TT "product", " -- provides the product of the members of a list or set,
     optionally with a function applied to each one.",
     PARA,
     MENU {
	  TO {(product, List), " -- product the elements of a list or sequence"},
	  TO {(product, List, List, Function), " -- product results of applying a function"},
	  TO {(product, List, Function), " -- product results of applying a function"},
	  TO {(product, ZZ, Function), " -- product consecutive values of a function"},
	  TO {(product, Tally), " -- product elements of a tally"},
	  TO {(product, Set), " -- product elements of a tally"}
	  }
     }
document { (product, List),
     TT "product v", " -- yields the product of the elements in the list v.",
     PARA,
     EXAMPLE "product {1,2,3,4,5}",
     SEEALSO "product"
     }
document { (product, List, List, Function),
     TT "product(v,w,f)", " -- yields the product of the results obtained by
     applying ", TT "f", " to each of the pairs ", TT "(i,j)", " of elements from 
     the lists or sequences ", TT "v", " and ", TT "w", ", which should be of 
     the same length.",
     PARA,
     EXAMPLE {
	  "M = monoid [x,y,z];",
      	  "product({2,3,4},{x,y,z},(i,j)->j^i)",
	  },
     SEEALSO "product"
     }
document { (product, List, Function),
     TT "product(v,f)", " -- yields the product of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA,
     EXAMPLE "product(1 .. 5, i -> i^2)",
     SEEALSO "product"
     }
document { (product, ZZ, Function),
     TT "product(n,f)", " -- compute the product ", TT "f(0) * f(1) * ... * f(n-1)", ".",
     PARA,
     EXAMPLE "product(5, i -> 2*i+1)",
     SEEALSO "product"
     }
document { (product, Tally),
     TT "product v", " -- yields the product of the elements in the tally ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = tally{2,2,2,2,2,3,3,3,5,5}",
      	  "product a",
	  },
     SEEALSO "product"
     }
document { (product, Set),
     TT "product v", " -- yields the product of the elements in the set ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = set select(1..50, isPrime)",
      	  "product a",
	  },
     SEEALSO "product"
     }
