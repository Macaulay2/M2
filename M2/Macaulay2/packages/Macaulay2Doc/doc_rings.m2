document {
     Key => Monoid,
     Headline => "the class of all monoids",
     "A monoid is a set with a multiplicative operation on
     it and an identity element.  A typical monoid is the set
     of monomials in a polynomial ring, which we consider to be
     created before the polynomial ring is created."
     }

document {
     Key => Ring,
     Headline => "the class of all rings",
     SeeAlso => "rings",
     "Common ways to make a ring:",
     UL {
	  TO (symbol /, Ring, Ideal),
	  TO (symbol SPACE, Ring, Array),
	  TO "GF",
	  },
     "Common functions for accessing the variables or elements in a ring:",
     UL {
	  TO (use, Ring),
	  TO (generators, Ring),
	  TO (numgens, Ring),
	  TO (symbol _, Ring, ZZ)
	  },
     "Common ways to get information about a ring:",
     UL {
	  TO (char, Ring),
	  TO (coefficientRing, Ring),
	  TO (dim, Ring),
	  },
     "Common ways to use a ring:",
     UL {
	  TO (symbol ^, Ring, ZZ),
	  TO (symbol ^, Ring, List),
	  TO (vars, Ring),
	  },
     }

document {
     Key => {(symbol SPACE, Ring, Array),(symbol SPACE,InexactFieldFamily,Array)},
     Headline => "the standard way to make a polynomial ring",
     TT "R[...]", " -- produces the monoid ring from a ring ", TT "R", " and the
     ordered monoid specified by ", TT "[...]", ".",
     PARA{},
     "This is the customary way to make a polynomial ring.",
     PARA{},
     "Optional arguments (placed inside the array):",
     UL (TO \ keys core "monoidDefaults"),
     SeeAlso => "polynomial rings"}
document {
     Key => (symbol SPACE, Ring, List),
     Headline => "make a local polynomial ring",
     TT "R{...}", " -- produces the monoid ring from a ring ", TT "R", " and the
     ordered monoid specified by ", TT "...", ", together with the option ", TT "Local => true", ".",
     PARA{},
     "Optional arguments (placed inside the list):",
     UL (TO \ keys core "monoidDefaults"),
     SeeAlso => "polynomial rings"}
document {
     Key => (symbol SPACE,Ring, OrderedMonoid),
     Headline => "make a polynomial ring",
     TT "R M", " -- produces the monoid ring from a ring ", TT "R", " and an ordered monoid
     ", TT "M", ".",
     SeeAlso => "polynomial rings"}

document {
     Key => (symbol SPACE, RingElement, Array),
     Headline => "substitution of variables",
     Usage => "f[a,b,c]",
     Inputs => { "f", Nothing => { TT "[a,b,c]", ", an array of ring elements" } },
     Outputs => {
	  "r" => { "the result of replacing the variables in ", TT "f", " by the ring elements provided in brackets." } } ,
     EXAMPLE {
	  "R = QQ[x,y];",
	  "f = x^3 + 99*y;",
	  "f[1000,3]"
	  }
     }

document {
     Key => {size, (size, RingElement)},
     Headline => "the size of an object",
     TT "size x", " -- returns the size of ", TT "x", " which usually gives
     a rough indication of memory space required to store the object ", TT "x", ".",
     PARA{},
     "For a polynomial, the size is the number of terms.",
     PARA{},
     "This function should be replaced by something more generally useful."
     }

document {
    Key => {
	 baseName,
	(baseName, Thing),
	(baseName, Holder),
	(baseName, IndexedVariable),
	(baseName, IndexedVariableTable),
	(baseName, RingElement),
	(baseName, Subscript),
	(baseName, Symbol)},
    Headline => "the base name of a generator",
    TT "baseName x", " -- returns the variable or symbol upon which an indexed variable table
    or a generator of a monoid or polynomial ring is based.",
    EXAMPLE lines ///
	  R = QQ[x_1 .. x_4,y]
	  y
	  baseName y
	  x_1
	  baseName x_1
	  x
	  baseName x
    ///
    }

document {
     Key => {
	 (options, Ring),
	 (options, PolynomialRing),
	 (options, QuotientRing)},
     Headline => "get values used for optional arguments",
     TT "options R", " -- returns the options used when the polynomial ring ", TT "R", " was created."
     }
document {
     Key => {
	 (options, Monoid),
	 (options, GeneralOrderedMonoid)},
     Headline => "get values used for optional arguments",
     TT "options M", " -- returns the options used when the monoid ", TT "M", " was created."
     }

document {
     Key => {
	  heft,
	 (heft, Ring),
	 (heft, PolynomialRing),
	 (heft, QuotientRing),
	 (heft, Module)},
     Headline => "heft vector of ring, module, graded module, or resolution",
     Usage => "heft X",
     Inputs => { "X" => {ofClass{Ring,Module}} },
     Outputs => { List => {"the heft vector in use for ", TT "X", ", if ", TT "X", " is a
	       ring, or for the ring of ", TT "X", ", if ", TT "X", " is a module.
	       If there is no heft vector, then ", TO "null", " is returned."
	       }},
     EXAMPLE lines ///
     S = QQ[a..d,DegreeRank => 4];
     degrees S
     heft S
     ///,
     SeeAlso => {"heft vectors"}
     }

document {
     Key => "heft vectors",
     PARA {
	  "A ", EM "heft vector", " for a polynomial ring is a vector with integer entries, of the same length
	  as the degree vectors of the variables of the ring, whose dot product with each of them
	  is (strictly) positive.  Unless one is specified explicitly, then a good one will be
	  found automatically.  The heft vector is used in various internal algorithms, such as the one
	  in ", TO "basis", ", as a way of organizing the sequence of steps, proceeding incrementally to larger
	  values of the dot product of the degree of a monomial with the heft vector."
	  },
     EXAMPLE lines ///
     R = QQ[a..d];
     degrees R
     heft R
     S = QQ[a..d,DegreeRank => 4];
     degrees S
     heft S
     T = QQ[a,b,Degrees => {1,-1}]
     degrees T
     heft T
     U = QQ[a..d,Degrees => {{2,0},{1,-1},{0,-2},{-1,-3}}]
     degrees U
     heft U
     ///,
     PARA {
	  "The heft vector, multiplied by -1, is used as the weight vector in the monomial ordering of
	  the degrees ring, and the ", EM "order", " of the series expansions of the Hilbert series refers to
	  the weight formed with respect to that weight vector."
	  },
     EXAMPLE lines ///
     hilbertSeries U
     describe ring numerator oo
     hilbertSeries(U,Order => 8)
     ///,
     PARA {
	  "The heft vector is used in the computation of degrees of modules over a polynomial ring ", TT "R", ", because it
	  gives a homomorphism from the degrees ring of ", TT "R", " to the Laurent
	  polynomial ring in one variable ", TT "T", " that sends monomials corresponding to the degrees of
	  variables of ", TT "R", " to positive powers of ", TT "T", ".  See ", TO "degree(Module)", "."
	  },
     EXAMPLE lines ///
     R = QQ[x,y,Heft=>{3}];
     degree ideal(x)
     ///,
     SeeAlso => {heft, [monoid,Heft], degreesRing, multidegree}
     }

document {
     Key => {multidegree,(multidegree,Module), (multidegree,Ideal), (multidegree,Ring)},
     Headline => "multidegree",
     Usage => "multidegree M",
     Inputs => { "M" => {ofClass{Module,Ideal,Ring}} },
     Outputs => { {"the multidegree of ", TT "M", ".  If ", TT "M", " is an ideal, the corresponding quotient ring is used."} },
     PARA {
	  "The multidegree is defined on page 165 of ", EM "Combinatorial Commutative Algebra", ", by
	  Miller and Sturmfels.  It is an element of the degrees ring of ", TT "M", ".  Our
	  implementation agrees with their definition provided the heft vector of the ring has every entry equal to 1.
	  See also ", EM "Gröbner geometry of Schubert polynomials", ", by Allen Knutson and Ezra Miller."
	  },
     EXAMPLE lines ///
     S = QQ[a..d, Degrees => {{2,-1},{1,0},{0,1},{-1,2}}];
     heft S
     multidegree ideal (b^2,b*c,c^2)
     multidegree ideal a
     multidegree ideal (a^2,a*b,b^2)
     describe ring oo
     ///,
     Caveat => {"This implementation is provisional in the case where the heft vector does not have every entry equal to 1."},
     SeeAlso => {"heft vectors", degreesRing}
     }

document {
     Key => "division in polynomial rings with monomials less than 1",
     PARA {
	  "Starting with version 1.2, a new division algorithm has been implemented in
	  rings with inverses, where the monomials can involve negative exponents, and hence
	  do not form a well-ordered set.  The ring should have a monomial ordering whose
	  first test involves at least one weight vector, explicitly, or perhaps implicitly, as with
	  ", TO "GRevLex", ".  The algorithm will work when dividing by
	  a polynomial that is ", EM "monic", " in the sense that its lead monomial has coefficient 1,
	  and all other terms have smaller weight, where the weight is computed with
	  respect to just the first weight vector.  When we say the algorithm works, we
	  mean: (1) that it terminates; and (2) that the remainder is zero if and only if the denominator
	  divides the numerator."
	  },
     PARA {
	  "Define the length of a nonzero ring element to be the weight of the first term minus
	  the weight of the last term.  The length is greater than or equal to 0, because
	  the terms in a sorted polynomial are decreasing in weight."
	  },
     PARA {
	  "We refuse to start dividing unless the denominator is monic in the sense defined above.
	  When dividing, we keep subtracting monomial multiples of the denominator
	  from the numerator to eliminate the lead term of the numerator, which is always possible
	  because the ring contains the reciprocals of its variables.  We stop
	  when we get a remainder whose length is strictly less than the length of the denominator."
	  },
     PARA {
	  "This algorithm works because, in an integral domain, the length of a product is
	  the sum of the lengths of the factors.  Thus the remainder, if it is not zero, can
	  not be a multiple of the denominator."
	  },
     PARA {
	  "This will be good enough for applications to Hilbert series, because in our degrees rings, the denominator of a
	  Hilbert series will be a product of terms ", TT "1-T", ", where ", TT "T", " is a monomial of
	  strictly negative weight.  That's because the weight vector is minus the heft
	  vector of the original ring, and ", TT "T", " is the monomial constructed from the degree
	  vector of one of the variables in the original ring.  Note that any divisor of
	  such a product will also be 1 plus terms of negative weight."
	  },
     EXAMPLE lines ///
     R = QQ[x,y, Inverses => true, MonomialOrder => Lex, Weights => {1,2}]
     quotientRemainder(x^100 - x^89, x^5 - 1)
     quotientRemainder(x^100 - y^61, x^5 - 1)
     ///,
     SeeAlso => {"heft vectors", "polynomial rings", degreesRing}
     }
