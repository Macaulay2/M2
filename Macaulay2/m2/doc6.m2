--		Copyright 1993-1999 by Daniel R. Grayson


document { odd,
     TT "odd x", " -- returns true or false, tells whether x is an odd integer.",
     PARA,
     "See also ", TO "even", "."
     }

document { even,
     TT "even x", " -- returns true or false, tells whether x is an even integer.",
     PARA,
     "See also ", TO "odd", "."
     }

document { Numeric,
     TT "Numeric x", " -- yields the expression obtained from x by converting the 
     integers and rational numbers within to double precision floating 
     point numbers.",
     PARA,
     EXAMPLE "Numeric {1,2,3}",
     PARA,
     "See also ", TO "RR", "."
     }

document { "pi",
     TT "pi", " -- the numerical value of the arithmetic quantity pi."
     }

document { Engine,
     TT "Engine", " -- a key for rings which yields the value ", TT "true", " if this
     ring is supported by the ", TO "engine", "."
     }

document { ring,
     TT "ring x", " -- yields the ring associated with ", TT "x", ".",
     BR,
     NOINDENT,
     TT "ring x", " -- yields the ambient ring of a ring element ", TT "x", ".",
     BR,
     NOINDENT,
     TT "ring M", " -- yields the base ring of a module ", TT "M", ".",
     BR,
     NOINDENT,
     TT "ring C", " -- yields the base ring of a chain complex ", TT "C", ".",
     BR,
     NOINDENT,
     TT "ring I", " -- yields the ambient ring of an ideal ", TT "I", ".",
     BR,
     NOINDENT,
     TT "ring p", " -- yields the base ring of a module homomorphism ", TT "p", ".",
     BR,
     NOINDENT,
     TT "ring f", " -- yields the base ring of a map of chain complexes ", TT "f", ".",
     BR,
     NOINDENT,
     TT "ring X", " -- yields the coordinate ring of an affine variety ", TT "X", ".",
     BR,
     NOINDENT,
     TT "ring Z", " -- yields the homogeneous coordinate ring of a projective variety ", TT "Z", ".",
     SEEALSO "Ring"
     }

document { coefficientRing,
     TT "coefficientRing R", " -- yields the coefficient ring of the ring ", TT "R", ".",
     PARA,
     "If ", TT "R", " is a polynomial ring, then the coefficient ring is
     the base ring from which the coefficients are drawn.  If ", TT "R", " is
     constructed from a polynomial ring as a quotient ring or a fraction ring
     or a sequence of such operatinos, then the original coefficient ring
     is returned.",
     EXAMPLE {
	  "coefficientRing(ZZ/101[a][b])",
      	  "ultimate(coefficientRing,ZZ/101[a][b])"
	  },
     }

document { isCommutative,
     TT "isCommutative R", " -- tells whether the ring R is commutative."
     }

document { isRing,
     TT "isRing x", " -- determines whether x is a ring."
     }

document { baseRings,
     TT "baseRings", " -- a symbol used as a key in a ring ", TT "R", " under which is
     stored a list of base rings for ", TT "R", ".",
     PARA,
     "A base ring ", TT "A", " of ", TT "R", " is one of the rings involved in the
     construction of ", TT "R", ".  The natural ring homomorphism from ", TT "A", "
     to ", TT "R", " is implemented with ", TO "promote", ".",
     PARA,
     "The base rings are presented in chronological order."
     }

document { lift,
     TT "lift(f,R)", " -- promotes a ring element ", TT "f", " to 
     the ring ", TT "R", ".",
     PARA,
     "The ring ", TT "R", " should be one of the base rings associated with the
     ring of ", TT "f", ".",
     SEEALSO "baseRings"
     }

document { liftable,
     TT "lift(f,R)", " -- tells whether a ring element ", TT "f", " can be
     lifted to the ring ", TT "R", ".",
     PARA,
     "The ring ", TT "R", " should be one of the base rings associated with the
     ring of ", TT "f", ".",
     SEEALSO "baseRings"
     }

document { promote,
     TT "promote(f,R)", " -- promotes a ring element ", TT "f", " to 
     the ring ", TT "R", ".",
     PARA,
     "The element ", TT "f", " should be an element of some base ring of ", TT "R", ".",
     PARA,
     "A special feature is that if ", TT "f", " is rational, and ", TT "R", " is not
     an algebra over ", TT "QQ", ", then an element of ", TT "R", " is provided
     by attempting the evident division.",
     SEEALSO "baseRings"
     }

document { RingElement,
     TT "RingElement", " -- the class of all ring elements handled by the 
     ", TO "engine", ".",
     PARA,
     SEEALSO "PolynomialRing"
     }


document { EngineRing,
     TT "EngineRing", " -- denotes the class of all special-purpose engine
     rings, such as finite fields.",
     PARA,
     "The command ", TT "new Engine from x", " is not meant for general 
     users, and provides the developers with a way to create top-level 
     rings corresponding to rings implemented in the engine.  Here ", TT "x", "
     may be:",
     MENU {
	  "commands for the engine, as a string, or a sequence or list
	  of strings, which cause a ring to be placed on the top of the
	  engine's stack.",
	  "a ring, in which case another top-level ring is formed as
	  an interface to the same underlying engine ring.",
	  "the handle of on engine ring"
	  },
     "Types of EngineRing:",
     MENU {
	  TO "FractionField",
	  TO "GaloisField",
	  TO "PolynomialRing",
	  TO "QuotientRing",
	  TO "SchurRing"
	  }
     }


document { fraction,
     TT "fraction(f,g)", " -- manufactures the fraction f/g in the fraction
     ring of the ring containing f and g without reducing it to lowest terms."
     }

TEST "
frac(QQ[a,b])
assert ( a == denominator(b/a) )
assert ( b == numerator(b/a) )
assert ( 1 == numerator(b/b) )
"

document { FractionField,
     TT "FractionField", " -- the class of all fraction fields.",
     PARA,
     "Functions:",
     MENU {
	  (TO "frac", "     -- constructing a fraction field"),
	  (TO "fraction", " -- constructing a fraction")
	  }
     }

document { frac,
     TT "frac R", " -- construct the fraction field of the ring ", TT "R", ".",
     PARA,
     "If ", TT "R", " has no name yet, then the names for its symbols will
     be usurped as names for the corresponding elements of ", TT "R", ".",
     PARA,
     EXAMPLE {
	  "F = frac (ZZ/101[x,y])",
      	  "1/x + 1/y + 1/2",
	  },
     SEEALSO "FractionField"
     }

document { ZZ,
     TT "ZZ", " -- denotes the class of all integers.",
     PARA,
     EXAMPLE {
	  "1234 + 4",
      	  "basictype 1234",
	  },
     "Operations on integers:",
     MENU {
	  TO "gcdCoefficients",
	  TO (quote <<, ZZ, ZZ),
	  TO (quote >>, ZZ, ZZ)
	  }
     }

TEST "
assert (not isPrime 1333333)
assert (not isPrime 3133333)
assert (not isPrime 3313333)
assert ( isPrime 3331333)
assert ( isPrime 3333133)
assert ( isPrime 3333313)
assert ( isPrime 3333331)
"

document { isPrime,
     TT "isPrime x", " -- tests for primality",
     PARA,
     NOINDENT,
     TT "isPrime n", " -- returns ", TT "true", " if the integer ", TT "n", "
     is probably a prime, and ", TT "false", " if ", TT "n", " is not a
     prime.",
     PARA,
     "At the moment, for numbers larger than ", TT "2^31-1", " it checks for
     divisibility by small primes, and then applies a strong pseudoprimality
     test (Rabin-Miller) to the base 2.",
     PARA,
     TT "isPrime f", " -- returns ", TT "true", " if the polynomial ", TT "f", "
     is irreducible, otherwise ", TT "false", "."
     }

document { numerator,
     TT "numerator x", " -- provides the numerator of a fraction.",
     PARA,
     EXAMPLE "numerator (4/6)"
     }

document { denominator,
     TT "denominator x", " -- provides the denominator of a fraction.",
     PARA,
     EXAMPLE "denominator (4/6)"
     }

document { QQ,
     TT "QQ", " -- denotes the class of all rational numbers.",
     PARA,
     EXAMPLE "1/2 + 3/5",
     PARA,
     "Functions:",
     MENU {
	  TO "denominator",
	  TO "numerator"
	  },
     PARA,
     SEEALSO{"numbers", "arithmetic functions"}
     }

TEST ///
     assert( net (2/1) === "2" )
     assert( net (1/1) === "1" )
///

document { RR,
     TT "RR", " -- the class of all real numbers.  It is a field.",
     PARA,
     "A real number is entered as a sequence of decimal digits with a point.",
     EXAMPLE "3.14159",
     PARA,
     SEEALSO {"basictype", "numbers"}
     }

document { CC,
     TT "CC", " -- the class of all complex numbers.",
     PARA,
     "The symbol ", TO "ii", " represents the square root of -1.",
     PARA, 
     EXAMPLE {
	  "z = 3-4*ii",
      	  "z^5",
      	  "1/z",
	  },
     PARA,
     "Here are some functions for use with complex numbers.",
     MENU {
	  TO "realPart",
	  TO "imaginaryPart",
	  TO "conjugate"
	  },
     PARA,
     SEEALSO "numbers"
     }

document { ii,
     TT "ii", " -- the square root of -1.",
     PARA,
     SEEALSO{ "CC"}
     }
document { realPart,
     TT "realPart z", " -- return the real part of a complex number z."
     }
document { imaginaryPart,
     TT "imaginaryPart z", " -- return the imaginary part of a complex number z."
     }

document { conjugate,
     TT "conjugate z", " -- the complex conjugate of the complex number z."
     }

document { gcdCoefficients,
     TT "gcdCoefficients(a,b)", " -- returns ", TT "{r,s}", " so that
     ", TT"a*r + b*s", " is the greatest common divisor of ", TT "a", "
     and ", TT "b", ".",
     PARA,
     "Works for integers or elements of polynomial rings.",
     SEEALSO "gcd"
     }

document { mod,
     TT "mod(i,n)", " -- reduce the integer i modulo n, producing an
     element of ZZ/n."
     }

document { ProductRing,
     TT "ProductRing", " -- the class of all product rings.",
     PARA,
     "If R and S are rings, then R * S denotes their product ring.
     If r and s are elements of R and S respectively, then an element
     of the product is provided by ", 
     PRE "          new R*S from {r,s}",
     "This has to be rethought!"     
     }

document { OrderedMonoid,
     TT "OrderedMonoid", " -- the class of all ordered monoids.",
     PARA,
     "An ordered monoid is a multiplicative monoid together with an ordering of 
     its elements.  The ordering is required to be compatible with the 
     multiplication in the sense that if x < y then x z < y z.  The class
     of all ordered monomials is ", TO "OrderedMonoid", ".",
     PARA,
     "The reason for making a separate class for ordered monoids is that monoid
     rings can be implemented more efficiently for them - an element of the 
     monoid ring can be stored as a sorted list, each element of which is
     a pair consisting of an element of the monoid and a coefficient.
     See ", TO "PolynomialRing", ".",
     PARA,
     "A free commutative ordered monoid can be created with ", TO "monoid", ".",
     MENU {
	  TO "<",
	  TO "<=",
	  TO ">",
	  TO ">=",
	  TO "?"
	  },
     SEEALSO  {"Monoid", "group"}
     }

document { binomial,
     TT "binomial(n,i)", " -- returns the binomial coefficient, the coefficient
     of x^i in (1+x)^i.",
     PARA,
     EXAMPLE "binomial(13,6)"
     }

document { isPolynomialRing,
     TT "isPolynomialRing R", " -- tells whether R is a polynomial ring."
     }

document { PolynomialRing,
     TT "PolynomialRing", " -- denotes the class of all ordered monoid rings.",
     PARA,
     "If R is a ring and M is an ordered monoid, then R M denotes
     the ordered monoid ring constructed from them.",
     PARA,
     "If r is an element of R and m is an element of M
     then r m denotes the corresponding element of R M,
     provided R M has already been constructed.",
     PARA,
     "Elements of these rings are displayed with the monoid
     elements appearing in decreasing order from left to right.",
     PARA,
     "Operations on rings:",
     MENU {
	  TO "modifyRing",
	  TO "numgens",
	  TO "vars"
	  },
     "Operations on ring elements:",
     MENU {
	  TO "+",
	  TO "-",
	  TO "*",
	  TO "coefficients",
	  TO "content",
	  TO "exponents",
	  TO "index",
	  TO "isPrime",
	  TO "isPrimitive",
	  TO "isUnit",
	  TO "leadComponent",
	  TO "leadCoefficient",
	  TO "leadMonomial",
	  TO "leadTerm",
	  TO "lift",
	  TO "liftable",
	  TO "listForm",
	  TO "promote",
	  TO "size",
	  TO "someTerms",
	  TO "standardForm",
	  TO "substitute",
	  TO "terms"
	  },
     PARA,
     "Producing ring elements:",
     MENU {
	  TO "random"
	  },
     PARA,
     "Keys used:",
     MENU {
  	  TO "ring",
	  TO "degreesRing"
	  },
     SEEALSO {"OrderedMonoid", "RingElement"},
     }

document { isUnit,
     TT "isUnit r", " -- determines whether a ring element is a unit.",
     PARA,
     EXAMPLE {
	  "S = QQ[x,y]/(1-(x-1)*(y-1));",
	  "isUnit (x^2 - 2*x + 1)"
	  }
     }

document { exponents,
     TT "exponents m", " -- for a monomial ", TT "m", " provides the list
     of exponents.",
     BR, NOINDENT,
     TT "exponents f", " -- for a polynomial ", TT "f", " provides a list
     whose elements are the lists of exponents of the terms of ", TT "f", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "exponents (f = x^2 - 7 + x*y*z^11 + y)",
	  "leadMonomial f",
	  "exponents leadMonomial f"
	  },
     }

document { degreesRing,
     TT "degreesRing n", " -- produce the ring in n variables whose monomials
     are to be used to represent degrees in another ring with multi-degrees
     of length n",
     BR,NOINDENT,
     TT "degreesRing R", " -- produce the ring in n variables whose
     monomials are the degrees of elements of R.",
     PARA,
     "Elements of this ring are used as Poincare polynomials for modules
     over R.",
     SEEALSO "poincare"
     }

document { standardForm,
     TT "standardForm f", " -- converts a polynomial or monomial to a
     form involving hash tables.",
     PARA,
     "A polynomial is represented by hash tables in which the keys are
     hash tables representing the monomials and the values are the 
     coefficients.",
     PARA,
     "The monomials themselves are represented by hash tables 
     in which the keys are the variables and the values are the 
     corresponding exponents.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "standardForm (x^2 - 7 + x*y*z^11 + y)"
	  },
     }

document { listForm,
     TT "listForm f", " -- converts a polynomial or monomial to a form
     represented by nested lists.",
     PARA,
     "A monomial is represented by the list of its exponents.",
     PARA,
     "A polynomial is represented by lists of pairs (m,c), one for each
     term, where m is a list of exponents for monomial, and c is the
     coefficient.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "listForm (x^2 - 7 + x*y*z^11 + y)"
	  },
     }

document { (quote " ",Ring, OrderedMonoid),
     TT "R M", " -- produces the monoid ring from a ring ", TT "R", " and an ordered monoid
     ", TT "M", ".",
     SEEALSO {"Ring", "OrderedMonoid"}
     }

document { WeylAlgebra,
     TT "WeylAlgebra", " -- an option used when creating a polynomial ring
     to specify that a Weyl algebra is to be produced.",
     PARA,
     "A Weyl algebra is an algebra in which some of the variables behave
     as derivatives with respect to the other variables.",
     PARA,
     EXAMPLE "R = ZZ/101[x,dx,y,dy,WeylAlgebra => {x=>dx, y=>dy}];",
     "The list ", TT "{x=>dx, y=>dy}", " indicates that the variable ", TT "dx", "
     is to play the role of the derivative with respect to ", TT "x", ", and
     that ", TT "y", " is to play the role of the derivative with respect
     to ", TT "y", ".",
     EXAMPLE {
	  "dx*x",
      	  "dx*x^10",
      	  "dx*y^10"
	  }
     }

document { (quote _, RingElement, RingElement),
     TT "f_m", " -- provide the coefficient of the monomial m in the polynomial f.",
     PARA,
     EXAMPLE {
	  "ZZ[y];",
      	  "((1+y)^5) _ (y^2)",
	  },
     SEEALSO {"_"}
     }

document { (quote _, Ring, String),
     TT "R_\"x\"", " -- produce the indeterminate of the polynomial ring R 
     whose name is x.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y,z];",
      	  ///R_"x"///,
	  },
     PARA,
     "Eventually we will implement this for monoids, too."
     }

document { (quote _, Ring, ZZ),
     TT "R_i", " -- produce the ", TT "i", "-th generator of a ring ", TT "R", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ[a..d]",
      	  "R_2"
	  }
     }

document { (quote _, Ring, List),
     TT "R_w", " -- produce the monomial of the ring ", TT "R", " by using the 
     integers in the list ", TT "w", " as exponents of the variables.",
     PARA,
     EXAMPLE {
	  "R = ZZ[a..d]",
      	  "R_{1,2,3,4}"
	  }
     }

TEST "
-- test name
R = ZZ/101[a..e]
f = symmetricPower(2,vars R)
assert( f == value toExternalString f )
assert( f == value toString f )
"

document { Schur,
     TT "Schur n", " -- creates a Schur ring of degree n.",
     PARA,
     "This is the representation ring for the general linear group of n by n
     matrices.",
     PARA,
     SEEALSO {"SchurRing"}
     }

document { SchurRing,
     TT "SchurRing", " -- the class of all Schur rings.",
     PARA,
     "A Schur ring is the representation ring for the general linear group of 
     n by n matrices, and one can be constructed with ", TO "Schur", ".",
     EXAMPLE "R = Schur 4",
     "The element corresponding to the Young diagram ", TT "{3,2,1}", " is
     obtained as follows.",
     EXAMPLE "R_{3,2,1}",
     "The dimension of the underlying virtual representation can be obtained
     with ", TO "dim", ".",
     EXAMPLE "dim R_{3,2,1}",
     "Multiplication in the ring comes from tensor product of representations.",
     EXAMPLE "R_{3,2,1} * R_{1,1}",
     SEEALSO {"_", SchurRing, List}
     }

document { (quote _, SchurRing, List),
     TT "S_v", " -- produce the element of the Schur ring ", TT "S", " corresponding
     to the Young diagram whose rows have lengths as in the list ", TT "v", ".",
     PARA,
     "The row lengths should be in decreasing order.",
     SEEALSO "SchurRing"
     }

document { IndexedVariableTable,
     TT "IndexedVariableTable", " -- the class of those hash tables which
     are used to hold the values of those indexed variables sharing a given
     base name.",
     PARA,
     EXAMPLE {
	  "t_0",
      	  "scan(3, i -> t#i = i^2)",
      	  "t",
      	  "peek t",
	  },
     SEEALSO "IndexedVariable"
     }

document { assign,
     TT "assign(x,v)", " -- assigns v as the value of x.",
     PARA,
     "If the value of x is a symbol or indexed variable, then it
     can be assigned the value v with ",
     PRE "          assign(x,v)",
     "When the value of x is an indexed variable y_i then what happens
     is that the i-th element of the list y is replaced by v.",
     PARA,
     "Differs from x=v in that here x is evaluated.",
     PARA,
     "Note: it would be better if we could arrange for ",
     PRE "          x <- v",
     "to work with indexed variables.  See ", TO "<-", "."
     }

document { IndexedVariable,
     TT "IndexedVariable", " -- the class of all indexed variables.",
     PARA,
     "Indexed variables provide the possibility of producing 
     polynomial rings ", TT "R[x_0, x_1, ..., x_(n-1)]", " in n variables,
     where n is not known in advance.  If ", TT "x", " is an symbol,
     and i is an integer, then ", TT "x_i", " produces an indexed variable.
     (What actually happens is a hash table been assigned to the
     as the value of the symbol ", TT "x", ".
     After this has been done, an assignment ", TT "x#i=v", " will assign a 
     value to it.  A new sequence of indexed variables of
     length n assigned to the symbol ", TT "x", " can be produced with ",
     TT "x_1 .. x_n", " and that sequence can be used in constructing
     a polynomial ring.",
     EXAMPLE {
	  "ZZ/101[t_0 .. t_4]",
      	  "(t_0 -  2*t_1)^3",
	  },
     SEEALSO "IndexedVariableTable"
     }

document { MonoidElement,
     TT "MonoidElement", " -- the class of all monoid elements.",
     PARA,
     SEEALSO "monoid"
     }

document { Degrees,
     TT "Degrees", " -- an option which specifies the degrees of the generators.",
     PARA,
     "Used as an option to ", TO "monoid", ", or when a polynomial ring
     is created.",
     PARA,
     "See ", TO "monoid", " for details."
     }
document { SkewCommutative,
     TT "SkewCommutative", " -- name for an optional argument for monoids
     that specifies that monoid rings created from them will be skewcommutative.",
     PARA,
     "The default value is false.",
     EXAMPLE {
	  "R = ZZ[x,y,SkewCommutative=>true]",
      	  "x*y",
      	  "y*x"
	  }
     }

document { MonomialSize,
     TT "MonomialSize => n", " -- an option which determines the maximum 
     exponent size.",
     PARA,
     "Used as an option to ", TO "monoid", ", or when a polynomial ring
     is created.  Setting 'MonomialSize=>n' specifies that monomial exponents 
     may be as large as 2^n - 1.  The default value is 8, allowing for exponents 
     up to 255.",
     PARA,
     "See ", TO "monoid", " for details."
     }

document { Inverses,
     TT "Inverses", " -- an option used in creating a monoid which tells
     whether negative exponents will be allowed, making the monoid into
     a group.",
     SEEALSO "monoid"
     }

document { GeneralOrderedMonoid,
     TT "GeneralOrderedMonoid", " -- the class of all ordered free 
     commutative monoids, as implemented by ", TO "monoid", ".",
     PARA,
     "This is the class of free monoids that can be handled by the ",
     TO "engine", ".",
     PARA,
     "Functions:",
     MENU {
	  TO "degree"
	  },
     PARA,
     "Keys:",
     MENU {
	  TO "degreesMonoid",
	  TO "index"
	  },
     PARA,
     SEEALSO { "monoid", "Degrees", "MonoidElement"}
     }     

document { (quote _, Monoid,ZZ),
     TT "M_i", " -- produces the i-th generator of a monoid ", TT "M", ".",
     PARA,
     SEEALSO { "Monoid", "_" }
     }

document { degreesMonoid,
     TT "degreesMonoid n", " -- returns the monoid whose elements correspond
     to the multi-degrees of monomials in another monoid.",
     PARA,
     "Also used as a key under which to store the result."
     }

document { RevLex,
     TT "RevLex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the reverse lexicographic order."
     }
document { GRevLex,
     TT "GRevLex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the graded reverse lexicographic order.",
     PARA,
     "Caveat: If the number of degree vectors is greater than one, this
     is currently only graded using the first degree vector.  This will 
     eventually change."  -- MES
     }
document { GLex,
     TT "GLex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the graded lexicographic order.",
     PARA,
     "Caveat: If the number of degree vectors is greater than one, this
     is currently only graded using the first degree vector.  This will 
     eventually change."  -- MES
     }
document { Lex,
     TT "Lex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the (non-graded) lexicographic order."
     }
document { Eliminate,
     TT "Eliminate", " n -- an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the elimination order eliminating the
     first n variables, refined by the graded reverse lexicographic order.",
     PARA,
     "Caveat: If the number of degree vectors is greater than one, this
     is currently only graded using the first degree vector.  This will 
     eventually change."  -- MES
     }
document { ProductOrder,
     TT "ProductOrder", "{n1, ..., nr} -- an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the product of r graded reverse lex
     orders, each with n1, n2, ..., nr variables.",
     PARA,
     "Caveat: If the number of degree vectors is greater than one, the
     grading in each block only uses the first degree vector. This will 
     eventually change."  -- MES
     }

document { VariableBaseName,
     TT "VariableBaseName => x", " -- an optional argument used when creating
     monoids or rings to specify that the variables should be ",
     TT "x_0, ..., x_n", "."
     }

document { MonomialOrder,
     TT "MonomialOrder", " -- a key used with monoids to indicate a
     monomial order other than the default (graded reverse lexicographic)",
     PARA,
     "Values:",
     MENU {
	  {TO "GRevLex", " -- graded reverse lexicographic order (the default)"},
	  {TO "GLex", " -- graded lexicographic order"},
	  {TO "Lex", " -- lexicographic order"},
	  {TO "RevLex", " -- reverse lexicographic order"},
	  {TO "Eliminate", " -- elimination order"},
	  {TO "ProductOrder", " -- product order"}
          },
     "Eventually, more general monomial orders will be allowed.", -- MES
     SEEALSO "Weights"
     }

document { Weights,
     TT "Weights => {...}", " -- a keyword for an option used in specifying
     monomial orderings.",
     PARA,
     "This feature is currently under development."
     }

document { Variables,
     TT "Variables", " -- a key used with monoids to indicate the list of 
     variable names, or the number of variables.",
     PARA,
     "This option is useful for those situations when one doesn't care about the
     names of the variables in a ring or monoid, or when one is creating a 
     tensor product ring, symmetric algebra, or other ring, and one wants control
     over the names of the ring variables. See also ", TO "tensor", "."
     }

document { VariableOrder,
     TT "VariableOrder", " -- an option used when creating a monoid.",
     PARA,
     "Not implemented yet.",
     SEEALSO "monoid"
     }

document { monoid,
     TT "monoid R      ", " -- yields the underlying monoid of polynomial ring, 
                        group ring, or monoid ring.",
     PARA,
     NOINDENT,
     TT "monoid [a,b,c]", " -- makes a free ordered commutative monoid on the variables listed.",
     PARA,
     "Options available:",
     MENU {
	  TO "Degrees",
	  TO "Inverses",
	  TO "MonomialOrder",
	  TO "MonomialSize",
	  TO "SkewCommutative",
	  TO "Variables",
	  TO "VariableBaseName",
	  TO "VariableOrder"
	  },
     PARA,
     NOINDENT,
     TT "monoid [a,b,c,Degrees=>{2,3,4}]", " -- makes a free ordered commutative monoid on the
	     variables listed, with degrees 2, 3, and 4, respectively.",
     PARA,
     NOINDENT,
     TT "monoid [a,b,c,Degrees=>{{1,2},{3,-3},{0,4}}]", " -- makes a free ordered
     commutative monoid on the variables listed, with multi-degrees as listed.",
     PARA,
     NOINDENT,
     TT "monoid [a,b,c,Degrees=>{{},{},{}}]", " -- makes a free ordered commutative monoid on the
	     variables listed, ungraded.",
     PARA,
     "The variables listed may be symbols or indexed variables.
     The values assigned to these variables (with ", TO "assign", ") are
     the corresponding monoid generators.  The function ", TO "baseName", "
     may be used to recover the original symbol or indexed variable.",
     PARA,
     "The class of all monoids created this way is ", TO "GeneralOrderedMonoid", ".",
     PARA,
     SEEALSO {"OrderedMonoid","IndexedVariable","Symbol"}
     }

document { GeneralOrderedGroup,
     TT "GeneralOrderedGroup", " -- the class of all ordered free 
     commutative groups, as implemented by ", TO "group", ".",
     PARA,
     "This is the class of free commutative groups that can be 
     handled by the ", TO "engine", ".",
     PARA, "Functions:", MENU { 
	  TO "degree"
	  },
     PARA, "Keys:", MENU {
	  TO "index"
	  },
     PARA,
     SEEALSO { "group", "Degrees" }
     }     

document { group,
     TT "group R      ", " -- yields the underlying group of a group ring.",
     PARA,
     "group [a,b,c] -- makes a free ordered commutative group on the
     variables listed.",
     PARA,
     "group [a,b,c,Degrees=>{2,3,4}] 
     -- makes a free ordered commutative group on the
        variables listed, with degrees 2, 3, and 4, respectively.",
     PARA,
     "group [a,b,c,Degrees=>{{1,2},{3,-3},{0,4}}] 
     -- makes a free ordered commutative group on the
        variables listed, with multi-degrees as listed.",
     PARA,
     "group [a,b,c,Degrees=>{{},{},{}}] 
          -- makes a free ordered commutative group on the
	     variables listed, ungraded.",
     PARA,
     "The class of all groups created this way is ",
     TO "GeneralOrderedGroup", ".",
     PARA,
     SEEALSO { "Degrees", "OrderedMonoid", "monoid" }
     }

document { (quote **, Monoid, Monoid),
     TT "M ** N", " -- tensor product of monoids.",
     PARA,
     "For complete documentation, see ", TO "tensor", "."
     }

document { tensor,
  TT "tensor(M,N)", " -- tensor product of rings or monoids.",
  PARA,
  "This method allows all of the options available for monoids, see
  ", TO "monoid", " for details.  This routine essentially combines the 
  variables of M and N into one monoid.",
  PARA,
  "For rings, the rings should be quotient rings of polynomial rings over the same
  base ring.",
  PARA,
  "Here is an example with monoids.",
  EXAMPLE {
       "M = monoid[a..d, MonomialOrder => Eliminate 1]",
       "N = monoid[e,f,g, Degrees => {1,2,3}]",
       "P = tensor(M,N,MonomialOrder => GRevLex)",
       "describe P",
       "tensor(M,M,Variables => {t_0 .. t_7}, MonomialOrder => ProductOrder{4,4})",
       "describe oo",
       },
  "Here is a similar example with rings.",
  EXAMPLE "tensor(ZZ/101[x,y], ZZ/101[r,s], MonomialOrder => Eliminate 2)",
  SEEALSO "**"
  }

document { table,
     TT "table(u,v,f)", " -- yields a table m in which m_i_j is f(u_i,v_j).",
     PARA,
     "A table is a list of lists, all of the same length.  The entry m_i_j is 
     computed as f(u_i,v_j).",
     PARA,
     "table(m,n,f) -- yields, when m and n are integers, a table of size m by n
     whose entries are obtained by evaluating f() repeatedly.",
     PARA,
     "See also ", TO "isTable", ", and ", TO "subtable", ".",
     }

document { applyTable,
     TT "applyTable(m,f)", " -- applies the function f to each element of the table m.",
     PARA,
     "It yields a table of the same shape as m containing the resulting values.",
     PARA,
     "See also ", TO "table", "."
     }

document { subtable,
     TT "subtable(u,v,m)", " -- yields the subtable of the table m obtained from the
     list u of row numbers and the list v of column numbers.",
     PARA,
     EXAMPLE {
	  "m = table(5,5,identity)",
      	  "subtable({1,3,4},toList(2..4), m)"
	  },
     }

document { transpose,
     TT "transpose m", " -- yields the transpose n of the table or homomorphism m."
     }

document { vector,
     TT "vector {a,b,c,...}", " -- produces an element of a free module from a list.",
     PARA,
     "The elements a,b,c,... must be elements of the same ring, or be
     convertible to elements of the same ring."
     }

document { Module,
     TT "Module", " -- the class of all modules which are handled
     by the ", TO "engine", ".",
     PARA,
     "The most general module M is represented as a submodule of a 
     quotient module of a free module F.  The matrix of relations used to
     produce the quotient module is stored as ", TT "M.relations", " and 
     the matrix of generators is stored as ", TT "M.generators", ".",
     PARA,
     "Functions which create modules:",
     MENU {
	  TO (quote ^, Ring, ZZ),
	  TO "cokernel",
	  TO "homology",
	  TO "ideal",
	  TO "image",
	  TO "kernel",
	  TO "submodule"
	  },
     PARA,
     "Tests:",
     MENU {
	  TO "isDirectSum",
	  TO "isFreeModule",
	  TO "isIdeal",
	  TO "isModule",
	  TO "isQuotientModule",
	  TO "isSubmodule",
	  },
     "Operations on modules:",
     MENU {
	  TO (quote ==, Module, Module),
	  (TO (quote _, Module, ZZ), " -- get a generator of a module"),
	  TO (quote +,Module,Module),
	  (TO (quote **,Module,Ring), " -- tensor product, base change."),
	  (TO (quote ++, Module, Module), " -- direct sum"),
	  (TO (quote **, Module, Module), " -- tensor product"),
	  (TO (quote :, Module, Module), " -- the submodule quotient ", TT "M : N", ""),
	  (TO (quote /, Module, Module), " -- the quotient module ", TT "(M+N)/N"),
	  (TO (quote /, Module, Ideal), " -- the quotient module ", TT "M/IM"),
	  (TO (quote /, Ideal, Ideal), " -- the quotient module ", TT "(I+J)/J"),
	  TO "ambient",
	  (TO "annihilator", " -- the annihilator of a module"),
	  (TO "codim", " -- codimension of the support of a module"),
	  TO "cover",
	  TO "degree",
	  TO "degrees",
	  TO "dim",
	  TO "dual",
	  TO "End",
	  TO "euler",
	  (TO {"fittingIdeal", "(i,m)"}, " -- the ", TT "i", "-th Fitting ideal of the module ", TT "M"),
	  (TO {"Ext", "^i(M,N)"}, " -- Ext of two modules"),
	  TO "gcdDegree",
	  TO "genera",
	  TO "generators",
	  (TO {"poincare", "(M,t)", }, " -- the numerator of the Hilbert series of ", TT "M", "."),
	  (TO {"hilbertFunction", "(d,M)"}, " -- the Hilbert function of a module."),
	  (TO {"hilbertPolynomial", "(M)"}, " -- the Hilbert polynomial of a module"),
	  (TO {"hilbertSeries", "(M)"}, " -- the Hilbert series of a module."),
	  (TO {"Hom", "(M,N)"}, " -- the module of homomorphisms"),
	  (TO {"intersect", "(I,J)"}, " -- intersection of modules or ideals"),
	  TO "lcmDegree",
	  TO "numgens",
	  TO "mingens",
	  TO "pdim",
	  (TO "presentation", " M -- a presentation matrix for M"),
	  (TO "prune", " M -- a minimal presentation for M"),
	  TO "quotient",
	  TO "rank",
	  TO "relations",
	  TO "removeLowestDimension",
	  (TO "resolution", " M -- a finite free resolution of M"),
	  TO "super",
	  (TO "tensorAssociativity", " -- associativity isomorphisms for tensor products"),
	  TO "top",
  	  (TO {"Tor", "_i(M,N)"}, " -- Tor of two modules"),
	  (TO "trim", " -- replace generators and relations by minimal sets"),
	  TO "truncate"
     	  },
     "Operations on elements of modules (vectors):",
     MENU {
	  TO "+",
	  TO "-",
	  TO "*",
	  (TO (quote _, Vector, ZZ), " -- get a component of a vector"),
	  TO "components",
	  TO "leadCoefficient",
	  TO "leadMonomial"
	  },
     PARA,
     SEEALSO{ "Vector"}
     }

document { isModule,
     TT "isModule M", " -- tells whether its argument is a module."
     }

document { isFreeModule,
     TT "isFreeModule M", " -- determine whether M is evidently a free module.  No
     computation is done.",
     PARA,
     "To determine whether M is isomorphic to a free module, one may prune
     M first.",
     EXAMPLE {
	  "R = ZZ/101[x,y]",
      	  "M = kernel vars R",
      	  "isFreeModule M",
      	  "isFreeModule prune M"
	  },
     }

document { isSubmodule,
     TT "isSubmodule M", " -- tells whether M is provided as a submodule
     of a free module."
     }

document { isQuotientModule,
     TT "isQuotientModule M", " -- tells whether M is provided as a
     quotient module of a free module."
     }

document { isIdeal,
     TT "isIdeal I", " -- tells whether a module is an ideal.",
     PARA,
     "An ideal is a submodule of a free module of rank 1."
     }

document { numgens,
     TT "numgens X", " -- yields the number of generators used to present
     a module or ring.",
     PARA,
     "For a polynomial ring or quotient of one, this is also the number
     of variables.  For a free module, this is the same as the rank.  For
     a general module presented as a subquotient, it is the number of columns
     in the matrix of generators."
     }

document { relations,
     TT "relations M", " -- produce the relations defining a module M.",
     PARA,
     "The relations are represented as a matrix, and if not stored
     in the module under M.relations, the matrix is understood to be
     empty.",
     PARA,
     SEEALSO {"generators","subquotient"}
     }

document { (quote ==, Module, Module),
     TT "M == N", " -- test whether two modules are equal.",
     PARA,
     "Two modules are equal if they are isomorphic as subquotients of the
     same ambient free module.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x]",
      	  "image matrix {{2,x},{1,5}} == R^2",
      	  "image matrix {{2,x},{0,5}} == R^2"
	  },
     }

TEST "
R = ZZ/101[a,b,c]
M = cokernel matrix {{a,b^2,c^3}}
N = image M_{0}
assert( M == N )
"
document { Vector,
     TT "Vector", " -- the class of all elements of free modules which
     are handled by the ", TO "engine", ".",
     PARA,
     "If ", TT "R", " is a ring handled by the engine, and ", TT "M", " is a free
     module over ", TT "R", ", then M is a subclass of Vector.",
     PARA,
     SEEALSO "Module"
     }

document { (quote _, Vector, ZZ),
     TT "v_i", " -- produce the i-th entry of a vector or module element v.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..f]",
      	  "v = vector {a,b,c}",
      	  "v_1",
	  },
     SEEALSO {"_"}
     }

document { degrees,
     TT "degrees M", " -- provides a list of multi-degrees for the basis
     elements of a free module M.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "E = R^5",
      	  "degrees E",
      	  "F = R^{1,2,3,4}",
      	  "degrees F"
	  },
     }

document { (quote ^, Ring, List),
     TT "R^{i,j, k, ...}", " -- produce a free module over R whose generators have
     degrees -i, -j, -k, ...",
     PARA,
     "The degrees i, j, ... may themselves be multi-degrees, represented
     as lists of integers.  The operator ", TO ":", " may be used to
     indicate repetitions.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "F = R^{1,4:2,3,3:4}",
      	  "degrees F",
	  },
     SEEALSO {"degrees", "^"}
     }

document { components,
     TT "components x", " -- produces a list of the components of an element of a 
     free module.",
     BR,NOINDENT,
     TT "components M", " -- the list of components for a module ", TT "M", " which was
     formed as a direct sum, or ", TT "{M}", " if ", TT "M", " was not formed as a 
     direct sum.  Works also for homomorphism, chain complexes, and graded modules.",
     PARA,
     MENU {
	  TO (components,ChainComplex)
	  },
     SEEALSO {"vector", "directSum", "++"}
     }

document { (quote ^, Ring, ZZ),
     TT "R^n", " -- produce a free module of rank ", TT "n", " over the ring ", TT "R", "",
     PARA,
     SEEALSO{"^", "isFreeModule", (quote ^, Ring, List)}
     }

document { euler,
     TT "euler M", " -- provide a list of the successive sectional Euler 
     characteristics of a module, ring, or ideal.",
     PARA,
     "The i-th one in the list is the Euler characteristic of the i-th
     hyperplane section of M."
     }

document { genera,
     TT "genera M", " -- provide a list of the successive sectional 
     arithmetic genera of a module, ring, or ideal.",
     PARA,
     "The i-th one in the list is the arithmetic genus of the i-th
     hyperplane section of M."
     }

TEST ///
R = ZZ/101[a,b,c]/c^4
assert ( genera R == {3,3} )
assert ( euler R == {-2,4} )
R = ZZ/101[a,b,c]/c^3
assert ( genera R == {1,2} )
assert ( euler R == {0,3} )
///

document { rank,
     TT "rank M", " -- computes the rank of the module M.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = vars R;",
      	  "rank kernel p",
      	  "rank cokernel p"
	  },
     }

document { cover,
     TT "cover M", " -- yields the free module whose basis elements correspond
     to the generators of M.",
     PARA,
     SEEALSO {"ambient", "super"}
     }

document { super,
     TT "super M", " -- yields the module which the module M is a submodule of.",
     BR, NOINDENT,
     TT "super f", " -- if f is a map whose target is a submodule of M, yields the
     composite of f with the inclusion into M.",
     PARA,
     SEEALSO { "cover", "ambient" }
     }

document { End,
     TT "End M", " -- constructs the module of endomorphisms of M."
     }
document { ModuleMap,
     TT "ModuleMap", " -- the class of all maps between modules.",
     PARA,
     "This class is experimental, designed to support graded modules.",
     SEEALSO {"Matrix"}
     }
document { Matrix,
     TT "Matrix", " -- the class of all matrices for which Groebner basis operations
     are available from the ", TO "engine", ".",
     PARA,
     "A matrix is a map from a graded module to a graded module, see ",
     TO "Module", ".  The degree of the map is not necessarily 0, and may
     be obtained with ", TO "degree", ".",
     PARA,
     "Multiplication of matrices corresponds to composition of maps, and when
     ", TT "f", " and ", TT "g", " are maps so that the target ", TT "Q", "
     of ", TT "g", " equals the source ", TT "P", " of ", TT "f", ", the
     product ", TT "f*g", " is defined, its source is the source of ", TT
     "g", ", and its target is the target of ", TT "f", ".  The degree of ",
     TT "f*g", " is the sum of the degrees of ", TT "f", " and of ", TT "g",
     ".  The product is also defined when ", TT "P", " != ", TT "Q", ",
     provided only that ", TT "P", " and ", TT "Q", " are free modules of the
     same rank.  If the degrees of ", TT "P", " differ from the corresponding
     degrees of ", TT "Q", " by the same degree ", TT "d", ", then the degree
     of ", TT "f*g", " is adjusted by ", TT "d", " so it will have a good
     chance to be homogeneous, and the target and source of ", TT "f*g", "
     are as before.", 
     PARA,
     "If ", TT "h", " is a matrix then ", TT "h_j", " is the ", TT "j", "-th
     column of the matrix, and ", TT "h_j_i", " is the entry in row ", TT
     "i", ", column ", TT "j", ".  The notation ", TT "h_(i,j)", " can be
     used as an abbreviation for ", TT "h_j_i", ", allowing row and column
     indices to be written in the customary order.",
     PARA,
     "If ", TT "m", " and ", TT "n", " are matrices, ", TT "a", " is a ring element, 
     and ", TT "i", " is an integer, then ", TT "m+n", ", ", TT "m-n", ", 
     ", TT "-m", ", ", TT "m n", ", ", TT "a*m", ", and ", TT "i*m", " denote the
     usual matrix arithmetic.  Use ", TT "m == n", ", and ", TT "m == 0", " to 
     check equality of matrices.",
     PARA,
     "Operations which produce matrices:", 
     MENU {
	  (TO "adjoint", "                -- matrix of adjointness"),
	  (TO "adjoint1", "               -- another matrix of adjointness"),
	  (TO "flip", "                   -- matrix of commutativity of tensor product"),
          (TO "genericMatrix", "          -- a generic matrix"),
          (TO "genericSkewMatrix", "      -- a generic skew-symmetric matrix"),
          (TO "genericSymmetricMatrix", " -- a generic symmetric matrix"),
	  (TO "id", "                     -- identity maps"),
	  (TO "matrix", "                 -- create a matrix"),
	  (TO "map", "                    -- create a map of modules"),
	  (TO "random", "                 -- a random homgeneous matrix")
	  },
     "Tests on matrices:",
     MENU {
	  (TO "==", "                    -- equality test"),
	  (TO "!=", "                    -- inequality test"),
	  (TO "inducesWellDefinedMap", " -- whether a matrix would induce a well defined map"),
          (TO "isHomogeneous", "         -- whether a matrix is homogeneous"),
	  (TO "isInjective", "           -- whether a map is injective"),
          (TO "isIsomorphism", "         -- whether a map is an isomorphism"),
	  (TO "isSurjective", "          -- whether a map is surjective"),
	  (TO "isWellDefined", "         -- whether a map is well-defined"),
	  },
     "Making new matrices from old ones:",
     MENU {
	  (TO "+", "                      -- sum"),
	  (TO "-", "                      -- difference"),
	  (TO "*", "                      -- product"),
	  (TO "^", "                      -- power"),
	  (TO (quote ^, Matrix, List), "  -- extracting or permuting rows"),
	  (TO (quote ^, Matrix, Array), " -- extracting or permuting blocks of rows"),
	  (TO (quote %,Matrix,Matrix), "  -- remainder"),
	  (TO (quote %,Matrix,RingElement), " -- remainder"),
	  (TO (quote //,Matrix,Matrix), " -- quotient"),
	  (TO (quote //,Matrix,RingElement), " -- quotient"),
	  (TO (quote _, Matrix, List), " -- extracting or permuting columns"),
	  (TO (quote _, Matrix, Array), " -- extracting or permuting blocks of columns"),
	  (TO (quote |, Matrix, Matrix), " -- horizontal concatenation"),
	  (TO (quote ||, Matrix, Matrix), " -- vertical concatenation"),
	  (TO (quote ++, Matrix, Matrix), " -- direct sum"),
	  (TO (quote **,Matrix, Matrix), " -- tensor product of matrices"),
	  (TO (quote **, Matrix, Module), " -- tensor product, e.g., degree shifting"),
	  (TO (quote **,Matrix,Ring), " -- tensor product, base change"),
          (TO "borel", "                -- Borel submodule generated by a matrix"),
          (TO "complement", "           -- generators of cokernel"),
          (TO "compress", "             -- removal of zero columns"),
          (TO "contract", "             -- contraction"),
          (TO "diff", "                 -- differentiation"),
          (TO "divideByVariable", "     -- divide columns by a variable repeatedly"),
          (TO "dual", "                 -- dual of a map (transpose)"),
          (TO "exteriorPower", "        -- exterior power of m"),
          (TO "flatten", "              -- collect entries of a matrix into one row"),
          (TO "homogenize", "           -- homogenize a matrix"),
          (TO "inducedMap", "           -- a map induced on subquotients"),
          (TO "jacobian", "             -- Jacobian matrix of a matrix"),
          (TO "koszul", "               -- i-th Koszul matrix of a matrix"),
          (TO "leadTerm", "             -- lead monomial matrix of the columns of a matrix"),
          (TO "minors", "               -- ideal of minors of a matrix"),
          (TO "modulo", "               -- generators for pre-image of an image"),
          (TO "pfaffians", "            -- Pfaffians of a skew symmetric matrix"),
          (TO "reshape", "              -- reshape a matrix"),
          (TO "selectInSubring", "      -- select certain columns"),
          (TO "sortColumns", "          -- sort the columns of a matrix"),
          (TO "submatrix", "            -- extract a submatrix"),
          (TO "substitute", "           -- replacing the variables in a matrix"),
          (TO "symmetricPower", "       -- symmetric power of a matrix"),
	  (TO "syz", "                  -- matrix of syzygies the columns"),
          (TO "topCoefficients", "      -- top coefficients of a matrix"),
          (TO "transpose", "            -- transpose of a matrix")
	  },
     "Operations on matrices:",
     MENU {
	  (TO (quote _, Matrix, Sequence), " -- getting an entry"),
	  (TO (quote _, Matrix, ZZ), "  -- getting a column"),
          (TO "content", "              -- content of a matrix"),
          (TO "degree", "               -- degree of a matrix, as specified"),
          (TO "det", "                  -- determinant of a matrix"),
          (TO "entries", "              -- the entries of m"),
	  (TO "gb", "                   -- Groebner basis of the columns"),
          (TO "ring", "                 -- the base ring of a matrix"),
          (TO "source", "               -- the source module of a map"),
          (TO "target", "               -- the target module of a map"),
          (TO "trace", "                -- trace of a matrix"),
	  },
     PARA,
     "Operations which produce modules and ideals from matrices:",
     MENU {
	  (TO "cokernel", "            -- the cokernel of a map"),
	  (TO "homology", "            -- homology of a pair of maps"),
	  (TO "ideal", "               -- ideal generated by the entries"),
	  (TO "image", "               -- image of a map"),
	  (TO "kernel", "              -- the kernel of a map"),
	  (TO "subquotient", "         -- subquotient module"),
	  },
     "Printing matrices:",
     MENU {
	  (TO "compactMatrixForm", "   -- global flag for compact printing"),
	  (TO "net", "                 -- convert to a net"),
	  (TO "toExternalString", "    -- convert to a string"),
	  (TO "toString", "            -- convert to a string"),
	  }
     }

document { gcdDegree,
     TT "gcdDegree F", " -- I don't know what this is supposed to do.",
     }

document { lcmDegree,
     TT "lcmDegree F", " -- I don't know what this is supposed to do.",
     }

document { getMatrix,
     TT "getMatrix R", " -- pops a matrix over ", TT "R", " from the top of 
     the engine's stack and returns it."
     }
document { (quote _, Matrix, Sequence),
     TT "f_(i,j)", " -- provide the element in row ", TT "i", " and
     column ", TT "j", " of the matrix ", TT "f", ".",
     SEEALSO {"_", "Matrix"}
     }
document { (quote _, Matrix, ZZ),
     TT "f_i", " -- provide the ", TT "i", "-th column of a matrix ", TT "f", " as a vector.",
     PARA,
     "Vectors are disparaged, so we may do away with this function in the future.",
     SEEALSO "_"
     }

document { isWellDefined,
     TT "isWellDefined m", " -- tells whether a map m of modules is 
     well-defined."
     }

document { isDirectSum,
     TT "isDirectSum M", " -- returns ", TT "true", " if ", TT "M", " was
     formed as a direct sum.",
     PARA,
     "Works for modules, graded modules, etc.  The components of the sum
     can be recovered with ", TO "components", "."
     }

TEST "
assert isDirectSum (QQ^1 ++ QQ^2)
assert isDirectSum (QQ^1 ++ QQ^2)
"

document { youngest,
     TT "youngest s", " -- return the youngest mutable hash table in the sequence
     ", TT "s", ", if any, else ", TT "null", "."
     }

document { (quote ++,Module,Module),
     TT "M++N", " -- computes the direct sum of two modules.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "image vars R ++ kernel vars R",
	  },
     "Projection and inclusion maps for direct sums:",
     MENU {
	  TO (quote ^,Module,Array),
	  TO (quote _,Module,Array)
	  },
     SEEALSO directSum
     }

document { (quote ++,Matrix,Matrix),
     TT "f++g", " -- computes the direct sum of two maps between modules.",
     PARA,
     "If an argument is a ring element or integer, it is promoted
     to a one by one matrix.",
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "vars R ++ transpose vars R",
      	  "oo^[1]",
      	  "a++b++c",
	  },
     "Selecting rows or columns of blocks:",
     MENU {
	  TO (quote ^,Matrix,Array),
	  TO (quote _,Matrix,Array)
	  },
     SEEALSO {directSum, (quote |, Matrix, Matrix), (quote ||, Matrix, Matrix)}
     }

document { directSum,
     TT "directSum(M,N,...)", " -- forms the direct sum of matrices or modules.",
     PARA,
     "The components can be recovered later with ", TO "components", ".",
     PARA,
     "Projection and inclusion maps for direct sums:",
     MENU {
	  TO (quote ^,Module,Array),
	  TO (quote _,Module,Array),
	  TO (quote ^,Matrix,Array),
	  TO (quote _,Matrix,Array)
	  },
     PARA,
     "It sometimes happens that the user has indices for the components of
     a direct sum preferable to the usual consecutive small integers.  In 
     this case the preferred indices can be specified with code
     like ", TT "directSum(a=>M,b=>N,...)", ", as in the following example.",
     EXAMPLE {
	  ///F = directSum(a=>ZZ^1, b=>ZZ^2, c=>ZZ^3)///,
	  ///F_[b]///,
	  ///F^[c]///,
	  },
     "Similar syntax works with ", TO "++", ".",
     EXAMPLE {
	  ///F = (a => ZZ^1) ++ (b => ZZ^2)///,
	  ///F_[b]///,
	  },
     SEEALSO {"++", "components", "indexComponents", "indices"}
     }

document { indexComponents,
     TT "indexComponents", " -- a symbol used as a key in a direct sum
     under which to store a hash table in which to register preferred keys used
     to index the components of the direct sum.",
     PARA,
     SEEALSO {"directSum", "components", "indices"}
     }

document { indices,
     TT "indices", " -- a symbol used as a key in a direct sum
     under which to store a list of the preferred keys used
     to index the components of the direct sum.",
     PARA,
     SEEALSO {"directSum", "components", "indexComponents"}
     }
