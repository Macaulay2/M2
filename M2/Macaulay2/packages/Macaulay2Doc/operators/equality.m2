undocumented select(makeDocumentTag \ methods symbol ==,
    m -> package m === Macaulay2Doc and not isUndocumented m and isMissingDoc m)

document {
    Key => {
	 symbol ===,
	(symbol ===, Thing, Thing)
    },
     Headline => "strict equality",
     Usage => "x === y",
     Inputs => { "x", "y" },
     Outputs => { Boolean => {"whether the expressions ", TT "x", " and ", TT "y", " are strictly equal"} },
     PARA{
	  "Strictly equal expressions have the same type, so ", TT "0===0.", " and
	  ", TT "0===0/1", " are false; the three types involved here are ", 
	  TO "ZZ", ", ", TO "RR", ", and ", TO "QQ", "."
	  },
     PARA{
	  "If x and y are ", TO "mutable", " then they are strictly equal only
	  if they are identical (i.e., at the same address in memory).  For
	  details about why strict equality cannot depend on the contents of
	  mutable hash tables, see ", TO "hashing", ".  On the other hand, if x
	  and y are non-mutable, then they are strictly equal if and only if
	  all their contents are strictly equal."
	  },
     EXAMPLE { "{1,2,3} === {1,2,3}", "{1,2,3} === {2,1,3}" },
     PARA {
	  "For some types, such as ring elements and matrices, strict equality is the same as mathematical equality.
	  This tends to be the case for objects for which not much computation is not required to test equality.
	  For certain other types, such as ", TO "Ideal", " or ", TO "Module", ", where extensive computations may
	  be required, the operator ", TO "==", " implements the desired comparison."
	  },
     EXAMPLE {
	  "R = QQ[a..d];",
	  "a^2+b === b+a^2",
	  "ideal(a^2+b,c*d) === ideal(b+a^2,c*d+b+a^2)",
     	  "matrix{{a,b,c}} === matrix{{a,b,c}}",
       	  "matrix{{a,b,c}} === transpose matrix{{a},{b},{c}}"
	  },
     PARA {
	  "As it happens, polynomial rings are mutable objects, and new ones are easily created, which are distinct from each other.
	  For example, the rings ", TT "A", " and ", TT "B", " below are not strictly equal."
	  },
     EXAMPLE {
     	  "A = QQ[x]; B = QQ[x];",
     	  "A === B"
     	  },
     SeeAlso =>{ symbol==,  symbol=!=, "operators" }
     }

document {
    Key => {
	 symbol =!=,
	(symbol =!=, Thing, Thing)
    },
     Headline => "strict inequality",
     TT "x =!= y", " -- returns true or false depending on whether the expressions
     x and y are strictly unequal.",
     PARA{},
     "See ", TO "===", " for details."
     }

document {
    Key => symbol ==,
     Headline => "equality",
     Usage => "x == y",
     PARA {
	  "Returns true or false, depending on whether 
	  the objects x and y are (mathematically) equal.  The objects x and y are
	  typically numbers, elements of rings, matrices, modules, ideals, 
	  chain complexes, and so on."
	  },
     PARA {
	  "A test for mathematical equality will typically involve doing a computation
	  to see whether two representations of the same mathematical object are being
	  compared.  For example, an ideal in a ring is represented by giving its
	  generators, and checking whether two sets of generators produce the same
	  ideal involves a computation with Gröbner bases.  The ideals must be defined
	  in the same ring."
	  },
     HEADER3 "Ideals",
     EXAMPLE {
	  "R = QQ[a,b,c];",
	  "ideal(a^2-b,a^3) == ideal(b^2, a*b, a^2-b)"
	  },
     PARA {
     	  "Often mathematical objects can be tested to see if they are 0 or 1."
	  },
     EXAMPLE {
	  "L = ideal(a^2-a-1,a^3+a+3)",
	  "L == 1",
	  "L == 0"
	  },
     HEADER3 "Matrices",
     PARA {
	  "Two ", TO "matrices", " are equal if their entries are equal, the source and target are
	  the same (including degrees), and the degree of the matrices are the same.  In this example,
	  m and n have different source free modules."
	  },
     EXAMPLE {
	  "m = matrix{{a,b},{c,a}}",
     	  "n = map(R^2,R^2,m)",
	  "m == n",
	  "source m == source n"
	  },
     PARA {
     	  "If you only want to know if they have the same entries, test the difference against zero."
	  },
     EXAMPLE {
	  "m-n == 0"
	  },
     HEADER3 "Rings",
     HEADER3 "Modules",
     PARA {
     	  "Two ", TO "modules", " are equal if they are isomorphic as subquotients of the
     	  same ambient free module."
	  },
     EXAMPLE {
      	  "image matrix {{2,a},{1,5}} == R^2",
      	  "image matrix {{2,a},{0,5}} == R^2"
	  },
     HEADER3 "Intervals",
        PARA { "If either side of the equality is an ", TO "RRi", ", the equality is an equality of sets." },
    EXAMPLE {
        "interval(1,3) == interval(1,3)",
        "interval(1/2) == 1/2",
        "interval(1/3) == 1/3"
    },
     PARA{
	  "It may happen that for certain types of objects, there is no method installed (yet)
	  for testing mathematical equality, in which case an error message will be
	  printed.  A good alternative may be to test for strict equality with
	  the operator ", TO "===", "."
	  },
     PARA {
	 "Since various sorts of mathematical objects are implemented as types, i.e., as
	 instances of ", TO "Type", ", there is no generic method for checking equality of types, so that
	 new mathematical comparison code can be provided in the future without breaking code that works."
	 },
     Caveat => {
	  "Warning: whether this comparison operator returns true is not necessarily 
     	  related to whether the comparison operator ", TO symbol ?, " returns ", TT "symbol ==", "."
	  },
     SeeAlso =>{ symbol!=, symbol===, symbol=!=, "operators" }
     }

document {
     Key => symbol !=,
     Headline => "inequality",
     TT "x != y", " -- the negation of ", TT "x == y", ".",
     PARA{},
     SeeAlso =>{ "==", "operators" }
     }
