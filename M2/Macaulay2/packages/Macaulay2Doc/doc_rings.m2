document {
     Key => Monoid,
     Headline => "the class of all monoids",
     "A monoid is a set with a multiplicative operation on
     it and an identity element.  A typical monoid is the set
     of monomials in a polynomial ring, which we consider to be
     created before the polynomial ring is created.",
     Subnodes => {
	 TO OrderedMonoid,
	 TO GeneralOrderedMonoid,
	 TO MonoidElement,
        TO (generators, Monoid),
        TO (numgens, Monoid),
        TO (tensor, Monoid, Monoid),
        TO (vars, Monoid),
         },
     }
document {
    Key => OrderedMonoid,
    Headline => "the class of all ordered monoids",
    "An ordered monoid is a multiplicative monoid together with an ordering of
    its elements.  The ordering is required to be compatible with the
    multiplication in the sense that if x < y then x z < y z.  The class
    of all ordered monoids is ", TO "OrderedMonoid", ".",
    PARA{},
    "The reason for making a separate class for ordered monoids is that monoid
    rings can be implemented more efficiently for them - an element of the
    monoid ring can be stored as a sorted list, each element of which is
    a pair consisting of an element of the monoid and a coefficient.
    See ", TO "PolynomialRing", ".",
    PARA{},
    "A free commutative ordered monoid can be created with ", TO "monoid", ".",
    SeeAlso =>  {"Monoid"}}
document {
    Key => GeneralOrderedMonoid,
    Headline => "the class of all ordered free commutative monoids",
    "This is the class of free monoids that can be handled by
    the ", TO "the engine of Macaulay2", ".  Elements of such monoids are implemented
    as instances of ", TO "MonoidElement", ".",
    PARA{},
    SeeAlso => { "monoid" }
    }
document {
    Key => FlatMonoid,
    Usage => "R.FlatMonoid",
    Inputs => {
	"R" => PolynomialRing
    },
    Outputs => {
	GeneralOrderedMonoid => { "the flattened monoid in terms of which the polynomials 
	    are expressed when the coefficient ring of R is itself a polynomial ring"
	}
    },
    EXAMPLE lines ///
    R = QQ[a,b][x]
    R.FlatMonoid
    ///,
    SeeAlso => { flattenRing }
}
document {
    Key => MonoidElement,
    Headline => "the class of all monoid elements",
    SeeAlso => "monoid"}

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
    -- TODO: merge these with the above
     Subnodes => {
	 TO EngineRing,
	 TO ring,
	 TO isRing,
        TO (ambient, Ring),
	TO (baseRing, Ring),
        TO (degree, Ring),
	TO (degreeLength, Ring),
	TO (degreeGroup, Ring),
	TO degrees,
        TO (degrees, Ring),
	TO (degreesRing, Ring),
        TO (numgens, Ring),
        TO (dim, Ring),
        TO (vars, Ring),
        TO (generators, Ring),
        TO (hilbertPolynomial, Ring),
        TO (jacobian, Ring),
        TO (minimalPresentation, Ring),
	TO selectVariables,
	TO flattenRing,
	TO generators,
        },
     }
document {
    Key => RingFamily,
    "This family is used to contain classes that correspond to a family of similar rings with a default member.",
    Subnodes => {
	TO InexactFieldFamily,
	TO default,
        },
    }
document {
    Key => Engine,
    Headline => "specify whether a ring is handled by the engine",
    TT "Engine", " -- a key for rings that yields the value ", TT "true", " if this
    ring is supported by the ", TO "the engine of Macaulay2", "."}
document {
    Key => EngineRing,
    Headline => "the class of rings handled by the engine",
    "Typically, ", TO "the engine of Macaulay2", " handles the rings in the system.",
    PARA{},
    "The command ", TT "new EngineRing from x", " is not meant for general
    users, and provides the developers with a way to create top-level
    rings corresponding to rings implemented in the engine.  Here ", TT "x", "
    may be:",
    UL {
	"commands for the engine, as a string, or a sequence or list
	of strings, which cause a ring to be placed on the top of the
	engine's stack.",
	"a ring, in which case another top-level ring is formed as
	an interface to the same underlying engine ring.",
	"the handle of on engine ring"
	},
    Subnodes => {
	TO Engine,
        TO InexactField,
	TO GaloisField,
	TO FractionField,
	TO PolynomialRing,
	TO QuotientRing,
        },
    }
document {
    Key => RingElement,
    Headline => "the class of all ring elements handled by the engine",
    SeeAlso => EngineRing,
    Subnodes => {
	TO isUnit,
	TO (leadTerm, RingElement),
        TO (leadTerm, ZZ, RingElement),
        TO (quotientRemainder, RingElement, RingElement),
        TO (degree, RingElement),
        TO (degree, RingElement, RingElement),
        TO (factor, RingElement),
        TO (indices, RingElement),
        TO (symbol ^, RingElement, ZZ),
        TO (symbol /, RingElement, RingElement),
        TO (symbol .., RingElement, RingElement),
        TO (symbol ..<, RingElement, RingElement),
        },
    }
document {
    Key => PolynomialRing,
    Headline => "the class of all ordered monoid rings",
    "Every element of a polynomial ring is also a ", TO "RingElement", ".",
    SeeAlso => "polynomial rings",
    Subnodes => {
	TO (hilbertSeries, PolynomialRing),
        },
    }
document {
    Key => QuotientRing,
    Headline => "the class of all quotient rings",
    Subnodes => {
        TO (codim, QuotientRing),
        TO (presentation, PolynomialRing, QuotientRing),
        },
    }
document {
    Key => FractionField,
    Headline => "the class of all fraction fields",
    "Macaulay2 provides for fraction fields of integral domains.",
    PARA{},
    "In some cases, normal forms of fractions makes sense, but in general
    for fraction fieldss of quotient rings, there is no notion of
    normal form for a fraction.
    In other words, fractions
    may be equal without displaying the same numerator and denominator.",
    PARA{},
    "Computations over fraction fields, or polynomial rings over fraction fields,
    especially Gröbner basis computations, are much slower than over prime fields.
    Still, an attempt is made to speed up these computations as much as possible, and
    more is planned in the future.",
    PARA{},
    "For an overview, see ", TO "fraction fields", " and  ", TO frac, ".",
    HEADER4 "Useful functions for use with fractions and fraction fields include:",
    UL {
	TO frac,
	TO numerator,
	TO denominator,
	TO liftable,
	TO lift
	}
    }
document {
     Key => GaloisField,
     Headline => "the class of all Galois fields",
     Subnodes => {
	 TO GF,
	 TO order,
         TO (ambient, GaloisField),
         },
     }

document {
     Key => {(symbol SPACE, Ring, Array), (symbol SPACE,InexactFieldFamily, Array)},
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
     Key => {(symbol SPACE, Ring, List), (symbol SPACE, InexactFieldFamily, List)},
     Headline => "make a local polynomial ring",
     TT "R{...}", " -- produces the monoid ring from a ring ", TT "R", " and the
     ordered monoid specified by ", TT "...", ", together with the option ", TT "Local => true", ".",
     PARA{},
     "Optional arguments (placed inside the list):",
     UL (TO \ keys core "monoidDefaults"),
     SeeAlso => "polynomial rings"}
document {
     Key => {(symbol SPACE, Ring, Monoid), (symbol SPACE, InexactFieldFamily, Monoid)},
     Headline => "make a polynomial ring",
     TT "R M", " -- produces the monoid ring from a ring ", TT "R", " and an ordered monoid
     ", TT "M", ".",
     SeeAlso => "polynomial rings"}

document {
    Key => IndexedVariable,
    Headline => "the class of all indexed variables",
    "Indexed variables provide the possibility of producing
    polynomial rings ", TT "R[x_0, x_1, ..., x_(n-1)]", " in n variables,
    where n is not known in advance.  If ", TT "x", " is an symbol,
    and i is an integer, then ", TT "x_i", " produces an indexed variable.
    After this has been done, an assignment ", TT "x_i=v", " will assign another
    value to it.  A new sequence of indexed variables of
    length n assigned to the symbol ", TT "x", " can be produced with ",
    TT "x_1 .. x_n", " and that sequence can be used in constructing
    a polynomial ring.",
    EXAMPLE {
	"ZZ/101[t_0 .. t_4]",
	"(t_0 -  2*t_1)^3",
	},
    "Warning: the values of the indexed variables ", TT "t_i", " are stored in a global location,
    behind the scenes, so may not get garbage collected, even if ", TT "t", " is a local variable.",
    Subnodes => {
	TO (value, IndexedVariable),
        TO (symbol .., IndexedVariable, IndexedVariable),
        TO (symbol ..<, IndexedVariable, IndexedVariable),
	TO baseName,
        },
    }

undocumented {(NewFromMethod,IndexedVariableTable,Symbol)}
document {
    Key => {IndexedVariableTable,((symbol _,symbol =),IndexedVariableTable,Thing),(symbol _,IndexedVariableTable,Thing)},
    "This class is used as part of the implementation of indexed variables.  Objects of this class contain
    the values of the indexed variables that share the same base.",
    EXAMPLE lines ///
	p_1 = a
	p_2 = b
	p
	peek p
    ///,
    SeeAlso => {IndexedVariable}
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
	(baseName, IndexedVariable),
	(baseName, IndexedVariableTable),
	(baseName, MonoidElement),
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
	 (options, Monoid),
	 (options, Ring),
	 (options, PolynomialRing),
	 (options, QuotientRing)},
     Headline => "get values used for optional arguments",
     TT "options A", " -- returns the options used when the polynomial ring or monoid ", TT "A", " was created."
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

document { Key => InexactField,
     Headline => "the class of inexact fields",
     PARA {
	  "An inexact field is one whose elements are real or complex numbers,
	  represented floating point approximations of varying accuracy or precision."
	  },
     EXAMPLE lines ///
     numeric_100 pi
     ring oo
     class oo
     parent oo
     ///,
     Subnodes => { TO RealField, TO ComplexField },
     }

document { Key => InexactFieldFamily,
     Headline => "the class of all families of inexact fields",
     PARA {
	  "All real numbers have the same class, ", TO "RR", ", but the rings they
	  belong to depends on the number of binary digits of precision used
	  to represent them.  Similarly for complex numbers, which all belong
	  to the class ", TO "CC", ".  Thus ", TO "RR", " and ", TO "CC", " are regarded not as inexact
	  fields, but as families of inexact fields."
	  },
     EXAMPLE lines ///
     x = 1/3.
     class x
     ring x
     x = 1/3.p200
     class x
     ring x
     ///,
     SeeAlso => { InexactField }
     }

document { Key => RealField,
     Headline => "the class of all real fields",
     PARA { "A real number ring is a ring whose elements are real numbers of variable precision." }
     }

undocumented {
     (NewOfFromMethod,ComplexField,Nothing,ZZ),
     (NewOfFromMethod,RealField,Nothing,ZZ)
     }

document { Key => ComplexField,
     Headline => "the class of all complex fields",
     PARA { "A complex number ring is a ring whose elements are complex numbers of variable precision." }
     }
