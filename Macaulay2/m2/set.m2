-- Copyright 1994 by Daniel R. Grayson

Tally.name = quote Tally
name Tally := x -> (
     "tally {"
     | demark(", ", apply(pairs x, (v,i) -> string i | " : " | name v))
     | "}"
     )

net Tally := x -> horizontalJoin flatten(
     "tally splice {",
     between(", ", apply(pairs x, (v,i) -> string i | " : " | net v)),
     "}"
     )

Tally _ Thing := {
     ZZ,
     (a,b) -> if a#?b then a#b else 0,
     TT "t_x", " -- returns the number of times ", TT "x", " is counted
     by ", TT "x", ".",
     SEEALSO "Tally"
     }

document { quote Tally,
     TT "Tally", " -- a class designed to hold tally results, i.e., multisets.",
     PARA,
     "Operations:",
     MENU {
	  (TO "tally", "               -- tally the elements of a list"),
	  (TO "toList", "            -- a list of the elements"),
	  (TO "apply(Tally,Function)", " -- apply a function to the elements."),
	  (TO "sum(Tally)", "          -- add the elements"),
	  (TO "product(Tally)", "      -- multiply the elements")
	  }
     }

document { "sum(Tally)",
     TT "sum x", " -- adds the elements of a tally.",
     SEEALSO "Tally"
     }

document { "product(Tally)",
     TT "product x", " -- adds the elements of a tally.",
     SEEALSO "Tally"
     }

Tally ** Tally := { Tally, 
     (x,y) -> combine(x,y,identity,times,),
     TT "x ** y", " -- produces the Cartesian product of two tallies.",
     PARA,
     "One of the arguments may be a ", TO "Set", ".",
     PARA,
     EXAMPLE "x = tally {a,a,b}",
     EXAMPLE "y = tally {1,2,2,2}",
     EXAMPLE "x ** y",
     SEEALSO ("Tally", "tally")
     }

Tally ? Tally := { Thing,
     (x,y) -> (
	  w := values (x-y);
	  if #w === 0 then quote ==
	  else if all(w,i -> i>0) then quote >
	  else if all(w,i -> i<0) then quote <
	  else incomparable),
     TT "x ? y", " -- compares two tallies, returning ", TT "quote <", ", ",
     TT "quote >", ", ", TT "quote ==", ", or ", TO "incomparable", ".",
     SEEALSO "Tally"
     }

Tally + Tally := { Tally, 
     (x,y) -> merge(x,y,plus),
     TT "x + y", " -- produces the union of two tallies.",
     PARA,
     "One of the arguments may be a ", TO "Set", ".",
     PARA,
     EXAMPLE "x = tally {a,a,a,b,b,c}",
     EXAMPLE "y = tally {b,c,c,d,d,d}",
     EXAMPLE "x + y",
     SEEALSO ("Tally", "tally")
     }

--document { "apply(Tally,Function)",
--     TT "apply(x,f)", " -- applies the function ", TT "f", " to each element of the
--     tally ", TT "x", ", accumulating the results in a tally.",
--     PARA,
--     EXAMPLE "x = tally {-1,-1,-2,1}",
--     EXAMPLE "apply(x,abs)",
--     SEEALSO ("Tally", "tally")
--     }

singleton := tally {0}

Tally - Tally := {Tally,
     (x,y) -> select(merge(x,applyPairs(y,(k,v)->(k,-v)),plus),i -> i =!= 0),
     TT "x - y", " -- produces the difference of two tallies.",
     PARA,
     EXAMPLE "tally {a,a,b,c} - tally {c,d,d}",
     SEEALSO "Tally"
     }
     
sum(Tally) := (w) -> sum(pairs w, (k,v) -> v * k)
product(Tally) := (w) -> product(pairs w, (k,v) -> k^v)

document { quote tally,
     TT "tally x", " -- tallies the frequencies of items in a list x.",
     PARA,
     "It produces an hash table (multiset) y which tallies the
     frequencies of occurrences of items in the list x, i.e.,
     y_i is the number of times i appears in x, or is 0 if
     i doesn't appear in the list.",
     PARA,
     EXAMPLE "y = tally {1,2,3,a,b,1,2,a,1,2,{a,b},{a,b},a}",
     EXAMPLE "y_{a,b}",
     PARA,
     SEEALSO "Tally"
     }

TEST "
assert( name tally {1,1,1,2,1,3,2} === \"tally {4 : 1, 2 : 2, 1 : 3}\" )
assert( tally {1,1,1,2,1,3,2} === new Tally from {(1,4),(2,2),(3,1)} )
"

Set.name = quote Set
document { quote Set,
     TT "Set", " -- the class of all sets.",
     PARA,
     "Function for creating sets:",
     MENU {
	  TO "set"
	  },
     "Operations on sets:",
     MENU {
	  (TO "+", "          -- union"),
	  (TO "Set ++ Set", " -- disjoint union"),
	  (TO "Set - Set", "  -- difference"),
	  (TO "*", "          -- intersection"),
	  (TO "**", "         -- Cartesian product"),
	  (TO "#", "          -- the number of elements"),
	  (TO "apply(Set,Function)", "  -- applying a function to elements"),
	  (TO "toList", "   -- a list of the elements"),
	  (TO "member", "     -- whether something is a member"),
	  (TO "product", "    -- multiply the elements"),
	  (TO "isSubset", "   -- whether a set is a subset of another"),
	  (TO "subsets", "    -- a list of the subsets"),
	  (TO "sum", "        -- sum the elements")
	  }
     }

document { "Set - Set",
     TT "x - y", " -- the difference of two sets.",
     SEEALSO ("Set", "-")
     }

document { "Set ++ Set",
     TT "x ++ y", " -- the disjoint union of two sets.",
     SEEALSO ("Set", "++")
     }

set Set := x -> x
net Set := x -> "set " | name keys x
name Set := x -> "set " | name keys x
Set + Set := {
     Set,
     (x,y) -> merge(x,y,(i,j)->i),
     TT "x + y", " -- the union of two sets."
     }
Set ++ Set := (x,y) -> apply(x,i->(0,i)) + apply(y,j->(1,j))
Set ** Set := (x,y) -> combine(x,y,identity,(i,j)->i,)
special := quote special
Set * Set := (x,y) -> (
     if # x < # y 
     then set select(keys x, k -> y#?k)
     else set select(keys y, k -> x#?k)
     )
Set - Set := (x,y) -> applyKeys(x, i -> if not y#?i then i)
sum Set := s -> sum toList s
product Set := s -> product toList s
unique = (x) -> keys set x

member(Thing,Set) := (a,s) -> s#?a

isSubset(Set,Set) := { Boolean,
     (S,T) -> all(S, (k,v) -> T#?k),
     TT "isSubset(X,Y)", " -- tells whether X is a subset of Y."
     }

isSubset(Sequence,Set) := isSubset(List,Set) := { Boolean, (S,T) -> all(S, x -> T#?x) }
isSubset(Sequence,List) := isSubset(List,List) := 
isSubset(Sequence,Sequence) := isSubset(List,Sequence) := (S,T) -> isSubset(S,set T)
isSubset(Set,List) := isSubset(Set,Sequence) := (S,T) -> isSubset(S,set T)

TEST "
x = set {1,2,3}
y = set {3,4,5}
assert( member(2,x) )
assert( not member(x,x) )
assert( sum y === 12 )
assert( product y === 60 )
assert ( x * y === set {3} )
assert ( x ** y === set {
	  (3, 4), (2, 5), (3, 5), (1, 3), (2, 3), (1, 4), (3, 3), (1, 5), (2, 4)
	  } )
assert ( x - y === set {2, 1} )
assert ( x + y === set {1, 2, 3, 4, 5} )
assert ( name x === \"set {1, 2, 3}\" )
"

document { quote isSubset,
     TT "isSubset(x,y)", " -- whether ", TT "x", " is a subset of ", TT "y", "."
     }
