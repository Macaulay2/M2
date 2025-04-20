undocumented {
    (NewFromMethod, Set, List),
    (set, Set)
}

document {
     Key => {set, (set,VisibleList)},
     Headline => "make a set",
     Usage => "set v\nset(v1,v2,...)",
     Inputs => {"v" => List},
     Outputs => {Set => " the set whose elements are the members of the list v"},
     EXAMPLE {
	  "v = {1,2,3,2,1}",
	  "S = set v",
	  "T = set(a,b,c,a,b,d)"
	  },
     SeeAlso => { Set }
     }

document {
     Key => Set,
     Headline => "the class of all sets",
     "Elements of sets may be any immutable object, such as integers, ring elements
     and lists of such.  Ideals may also be elements of sets.",
     EXAMPLE {
	  "A = set {1,2};",
	  "R = QQ[a..d];",
	  "B = set{a^2-b*c,b*d}"
	  },
     "Set operations, such as ",
     TO2((isMember,Thing,Set),"membership"), ", ",
     TO2((symbol+,Set,Set),"union"), ", ",
     TO2((symbol*,Set,Set),"intersection"), ", ",
     TO2((symbol-,Set,Set),"difference"), ", ",
     TO2((symbol**,Set,Set),"Cartesian product"), ", ",
     TO2((symbol^**,VirtualTally,ZZ),"Cartesian power"), ", and ",
     TO2((isSubset,Set,Set),"subset"),
     " are available. For example,",
     EXAMPLE {
	  "toList B",
	  "member(1,A)",
	  "member(-b*c+a^2,B)",
     	  "A ** A",
	  "A^**2",
	  "set{1,3,2} - set{1}",
	  "set{4,5} + set{5,6}",
	  "set{4,5} * set{5,6}",
	  "set{1,3,2} === set{1,2,3}"
	  },
     PARA{},
     TO2(Ideal,"Ideals"), " in Macaulay2 come equipped with a specific sequence of generators, so the following two ideals are not considered strictly equal,
     and thus the set containing them will appear to have two elements.",
     EXAMPLE {
     	  "I = ideal(a,b); J = ideal(b,a);",
	  "I == J",
	  "I === J",
	  "C = set(ideal(a,b),ideal(b,a))"
	  },
     "However, if you ", TO trim, " the ideals, then the generating sets will be
     the same, and so the set containing them will have one element.",
     EXAMPLE {
	  "C1 = set(trim ideal(a,b),trim ideal(b,a))"
	  },
     PARA{},
     "A set is implemented as a ", TO HashTable, ", whose keys are the elements of the
     set, and whose values are all 1.  In particular, this means that two objects
     are considered the same exactly when they are strictly equal, according to ", TO symbol===, ".",
     Subnodes => {
	 TO set,
	 TO elements,
	 TO union,
	 TO intersect,
	 TO (symbol #?, Set, Thing),
	 TO (symbol -, Set, Set),
	 TO (union, Set, Set),
	 TO (intersect, Set, Set),
	 TO (symbol **, Set, Set),
	 TO (sum, Set),
	 TO (product, Set),
	 TO (select, Set, Function),
	 TO (isSubset, Set, Set),
         },
     }

document {
     Key => (symbol #?, Set, Thing),
     Headline => "test set membership",
     Usage => "x#?e",
     Inputs => {
	  "x",
	  "e"
	  },
     Outputs => {
	  Boolean => {"whether e is in the set x"}
	  },
     "This is identical to ", TT "member(e,x)", ".",
     EXAMPLE {
	  "x = set{1,2,3}",
	  "x#?2",
	  "member(2,x)"
	  },
     SeeAlso => {Set}
     }

document {
     Key => {(symbol -, Set, Set),
	  (symbol -, Set, List),
	  (symbol -, List, Set)},
     Headline => "set difference",
     Usage => "x - y",
     Inputs => {
	  "x" => {" or ", ofClass List},
	  "y" => {" or ", ofClass List}
	  },
     Outputs => {
	  Set => {"or ", ofClass List, ", consisting of those elements of x not in y"}
	  },
     "At least one of ", TT "x", ", ", TT "y", " must be a set, and the other
     may be a list.  If ", TT "x", " is a list, then
     so is the result.",
     EXAMPLE {
	  "set{a,b,c} - set{a}",
	  "set{a,b,c} - {a}",
	  "{a,b,c} - set{a}"
	  },
     SeeAlso => {Set}
     }

document {
     Key => {
	 (union, Set, Set),
	 (symbol +, Set, Set)},
     Headline => "set union",
     Usage => "x + y",
     Inputs => {
	  "x",
	  "y",
	  },
     Outputs => {
	  Set => {"the union of ", TT "x", " and ", TT "y"},
	  },
     EXAMPLE {
	  "set{a,b,c} + set{a,d,f}",
	  "union(set{a,b,c}, set{a,d,f}, set{a,c,e})",
	  },
     PARA {
	  "The function ", TT "sum", " can be used to form the union of a list of sets, but this can be slow for long lists."
	  },
     EXAMPLE lines ///
     	  x = apply(3, i -> set apply(3, j -> 10*i+j))
	  sum x
     ///,
     SeeAlso => {Set}
     }

document {
     Key => {
	 (intersect, Set, Set),
	 (symbol *, Set, Set)},
     Headline => "set intersection",
     Usage => "x * y",
     Inputs => {
	  "x",
	  "y"
	  },
     Outputs => {
	  {"the intersection of ", TT "x", " and ", TT "y"}
	  },
     EXAMPLE "set {1,2,3} * set {2,3,4}",
     EXAMPLE "intersect(set{1,2,3}, set{2,3,4}, set{2,4,6})",
     SeeAlso => {Set}
     }

document {
     Key => (symbol **, Set, Set),
     Headline => "Cartesian product",
     Usage =>  "x ** y",
     Inputs => {
	  "x",
	  "y"
	  },
     Outputs => {
	  Set => "whose elements are the sequences (a,b), where a is an element
     	  of x, and b is an element of y."
	  },
     EXAMPLE "set {1,2} ** set {a,b,c}",
     "Suppose we wish to form the set of
     all triples with entries either in the set A below.",
     EXAMPLE {
	  "A = set{1,2}",
	  "A ** A ** A"
	  },
     "To make this last a set of triples, ", TO splice, " each element together.
     Or, use ", TO (symbol^**,VirtualTally,ZZ), ".",
     EXAMPLE {
	  "(A ** A ** A)/splice",
	  "A^**3"
	  },
     SeeAlso => { Set }
     }

document {
     Key => (sum, Set),
     Headline => "sum of elements",
     TT "sum v", " yields the sum of the elements in the set ", TT "v", ".",
     PARA{},
     EXAMPLE {
	  "a = set{1,100,10000}",
      	  "sum a",
	  },
     SeeAlso => "sum"
     }

document {
     Key => (product, Set),
     Headline => "product of elements",
     TT "product v", " yields the product of the elements in the set ", TT "v", ".",
     EXAMPLE {
	  "a = set select(1..50, isPrime)",
      	  "product a",
	  },
     SeeAlso => "product"
     }
