--		Copyright 1993-2002 by Daniel R. Grayson

document { odd,
     Headline => "tell whether an integer is odd",
     TT "odd x", " -- returns true or false, tells whether x is an odd integer.",
     PARA,
     "See also ", TO "even", "."
     }

document { even,
     Headline => "tell whether an integer is even",
     TT "even x", " -- returns true or false, tells whether x is an even integer.",
     PARA,
     "See also ", TO "odd", "."
     }

document { numeric,
     Headline => "convert to floating point",
     TT "numeric x", " -- yields the expression obtained from x by converting the 
     integers and rational numbers within to double precision floating 
     point numbers.",
     PARA,
     EXAMPLE "numeric {1,2,3}",
     PARA,
     "See also ", TO "RR", "."
     }

document { "pi",
     Headline => "the number 'pi'",
     TT "pi", " -- the numerical value of the arithmetic quantity pi."
     }

document { Engine,
     Headline => "specify whether a ring is handled by the engine",
     TT "Engine", " -- a key for rings which yields the value ", TT "true", " if this
     ring is supported by the ", TO "engine", "."
     }

document { ring,
     Headline => "get the associated ring"
     }

document { coefficientRing,
     Headline => "get the coefficient ring",
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
     Headline => "whether a ring is commutative",
     TT "isCommutative R", " -- tells whether the ring R is commutative."
     }

document { isRing,
     Headline => "whether something is a ring",
     TT "isRing x", " -- determines whether x is a ring."
     }

document { baseRings,
     Headline => "store the list of base rings of a ring",
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
     Headline => "lift to another ring",
     TT "lift(f,R)", " -- promotes a ring element ", TT "f", " to 
     the ring ", TT "R", ".",
     PARA,
     "The ring ", TT "R", " should be one of the base rings associated with the
     ring of ", TT "f", ".",
     SEEALSO "baseRings"
     }

document { liftable,
     Headline => "whether a ring element can be lifted to another ring",
     TT "liftable(f,R)", " -- tells whether a ring element ", TT "f", " can be
     lifted to the ring ", TT "R", ".",
     PARA,
     "The ring ", TT "R", " should be one of the base rings associated with the
     ring of ", TT "f", ".",
     EXAMPLE {
	  "R = ZZ[x]",
	  "liftable ((x-1)*(x+1)-x^2, ZZ)",
	  },
     SEEALSO {"lift"}
     }

document { promote,
     Headline => "promote to another ring",
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
     Headline => "the class of all ring elements handled by the engine",
     SEEALSO "engine"
     }

document { EngineRing,
     Headline => "the class of rings handled by the engine",
     "The ", TO "engine", " handles most of the types of rings in the
     system.",
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
	  }
     }

document { fraction,
     TT "fraction(f,g)", " -- manufactures the fraction ", TT "f/g", " in the fraction
     field of the ring containing ", TT "f", " and ", TT "g", " without reducing
     it to lowest terms."
     }

TEST ///
if getenv "USER" == "dan" then exit 0
frac(QQ[a,b])
assert ( a == denominator(b/a) )
assert ( b == numerator(b/a) )
assert ( 1 == numerator(b/b) )
///

document { FractionField,
     Headline => "the class of all fraction fields",
     "Note: there is no way to reduce an element of an arbitrary
     fraction field to a normal form.  In other words, fractions
     may be equal without displaying the same numerator and denominator."
     }

document { frac,
     Headline => "construct a fraction field",
     TT "frac R", " -- construct the fraction field of the ring ", TT "R", ".",
     PARA,
     "If ", TT "R", " has no name yet, then the names for its symbols will
     be usurped as names for the corresponding elements of ", TT "R", ".",
     PARA,
     EXAMPLE {
	  "F = frac (ZZ/101[x,y])",
      	  "1/x + 1/y + 1/2",
	  },
     "Results of division with ", TO "/", " will be in the fraction field, and
     the fraction field will be created if necessary.",
     EXAMPLE {
	  "R = ZZ[x,y]",
	  "x/y"
	  },
     PARA,
     "The symbol ", TT "frac", " is also used as a key under which is stored 
     the fraction field of a ring."
     }

document { ZZ,
     Headline => "the class of all integers" }

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
     Headline => "tell whether an integer is a prime",
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
     Headline => "numerator of a fraction",
     TT "numerator x", " -- provides the numerator of a fraction.",
     PARA,
     EXAMPLE "numerator (4/6)"
     }

document { denominator,
     Headline => "denominator of a fraction",
     TT "denominator x", " -- provides the denominator of a fraction.",
     PARA,
     EXAMPLE "denominator (4/6)"
     }

document { QQ,
     Headline => "the class of all rational numbers",
     EXAMPLE "1/2 + 3/5"
     }

TEST ///
     assert( net (2/1) === "2" )
     assert( net (1/1) === "1" )
///

document { RR,
     Headline => "the class of all real numbers",
     "A real number is entered as a sequence of decimal digits with a point.",
     EXAMPLE "3.14159",
     PARA,
     SEEALSO {"basictype"}
     }

document { CC,
     Headline => "the class of all complex numbers",
     "The symbol ", TO "ii", " represents the square root of -1.",
     PARA, 
     EXAMPLE {
	  "z = 3-4*ii",
      	  "z^5",
      	  "1/z",
	  },
     }

document { ii,
     Headline => "square root of -1"
     }

document { realPart,
     Headline => "real part",
     TT "realPart z", " -- return the real part of a complex number z."
     }
document { imaginaryPart,
     Headline => "imaginary part",
     TT "imaginaryPart z", " -- return the imaginary part of a complex number z."
     }

document { conjugate,
     Headline => "complex conjugate",
     TT "conjugate z", " -- the complex conjugate of the complex number z."
     }

document { gcdCoefficients,
     Headline => "gcd with coefficients",
     TT "gcdCoefficients(a,b)", " -- returns ", TT "{r,s}", " so that
     ", TT"a*r + b*s", " is the greatest common divisor of ", TT "a", "
     and ", TT "b", ".",
     PARA,
     "Works for integers or elements of polynomial rings.",
     SEEALSO "gcd"
     }

document { mod,
     Headline => "reduce modulo an integer",
     TT "mod(i,n)", " -- reduce the integer ", TT "i", " modulo ", TT "n", ".",
     PARA,
     "The result is an element of ", TT "ZZ/n", "."
     }

document { OrderedMonoid,
     Headline => "the class of all ordered monoids",
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
     SEEALSO  {"Monoid"}
     }

document { binomial,
     Headline => "binomial coefficient",
     TT "binomial(n,i)", " -- returns the binomial coefficient, the coefficient
     of x^i in (1+x)^i.",
     PARA,
     EXAMPLE "binomial(13,6)"
     }

document { isPolynomialRing,
     Headline => "whether something is a polynomial ring" }

document { PolynomialRing,
     Headline => "the class of all ordered monoid rings",
     "Every element of a polynomial ring is also a ", TO "RingElement", ".",
     SEEALSO "polynomial rings"
     }

document { isUnit,
     Headline => "whether a ring element is a unit",
     EXAMPLE {
	  "S = QQ[x,y]/(1-(x-1)*(y-1));",
	  "isUnit (x^2 - 2*x + 1)"
	  }
     }

document { exponents,
     Headline => "list the exponents in a polynomial",
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
     Headline => "the ring of degrees",
     TT "degreesRing n", " -- produce the ring in n variables whose monomials
     are to be used to represent degrees in another ring with multi-degrees
     of length n",
     BR,NOINDENT,
     TT "degreesRing R", " -- produce the ring in n variables whose
     monomials are the degrees of elements of R.",
     PARA,
     "Elements of this ring are used as Poincare polynomials for modules
     over R.",
     PARA,
     "Note: the monomial ordering used in the degrees ring is ", TT "RevLex", ",
     so the polynomials in it will be displayed with the smallest exponents first,
     because such polynomials are often used as Hilbert series.",
     SEEALSO { "poincare", "hilbertSeries" }
     }

document { standardForm,
     Headline => "convert to standard form",
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
     Headline => "convert to list form",
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

document { WeylAlgebra,
     Headline => "make a Weyl algebra",
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

document { (symbol _, RingElement, RingElement),
     Headline => "get a coefficient",
     TT "f_m", " -- provide the coefficient of the monomial m in the polynomial f.",
     PARA,
     EXAMPLE {
	  "ZZ[y];",
      	  "((1+y)^5) _ (y^2)",
	  },
     SEEALSO {"_"}
     }

document { (symbol _, Ring, String),
     Headline => "get a variable by name",
     TT "R_\"x\"", " -- produce the variable of the polynomial ring R 
     whose name is ", TT "x", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y,z];",
      	  ///R_"x"///,
	  },
     PARA,
     "Eventually we will implement this for monoids, too."
     }

document { (symbol _, Ring, ZZ),
     Headline => "get a variable by number",
     TT "R_i", " -- produce the ", TT "i", "-th generator of a ring ", TT "R", ".",
     PARA,
     "The indexing of generators is based on 0, so ", TT "R_0", " would be
     the first one, and so on.",
     PARA,
     EXAMPLE {
	  "R = ZZ[a..d]",
      	  "R_2"
	  }
     }

document { (symbol _, Ring, List),
     Headline => "make a monomial from a list of exponents",
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
     Headline => "make a Schur ring",
     TT "Schur n", " -- creates a Schur ring of degree n.",
     PARA,
     "This is the representation ring for the general linear group of n by n
     matrices.",
     PARA,
     SEEALSO {"SchurRing"}
     }

document { SchurRing,
     Headline => "the class of all Schur rings",
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

document { (symbol _, SchurRing, List),
     Headline => "make an element of a Schur ring",
     TT "S_v", " -- produce the element of the Schur ring ", TT "S", " corresponding
     to the Young diagram whose rows have lengths as in the list ", TT "v", ".",
     PARA,
     "The row lengths should be in decreasing order.",
     SEEALSO "SchurRing"
     }

document { IndexedVariableTable,
     Headline => "the class of indexed variable tables",
     "These hash tables are used to hold the values of those indexed variables 
     sharing a given base name.",
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
     Headline => "assign a value",
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
     Headline => "the class of all indexed variables",
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
     Headline => "the class of all monoid elements",
     SEEALSO "monoid"
     }

document { Degrees,
     Headline => "specify the degrees",
     TT "Degrees", " -- an option which specifies the degrees of the generators.",
     PARA,
     "Used as an option to ", TO "monoid", ", or when a polynomial ring
     is created.",
     PARA,
     "See ", TO "monoid", " for details."
     }

document { SkewCommutative,
     Headline => "make a skewcommutative (alternating) ring",
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
     Headline => "specify maximum exponent size",
     TT "MonomialSize => n", " -- an option which determines the maximum 
     exponent size.",
     PARA,
     "Used as an option to ", TO "monoid", ", or when a polynomial ring
     is created.  Setting 'MonomialSize=>n' specifies that monomial exponents 
     may be as large as 2^(n-1) - 1.  
     The default value is 8, allowing for exponents up to 127.  Currently
     the maximum value is 16, allowing for exponents up to 32767.",
     PARA,
     "See ", TO "monoid", " for details."
     }

document { Inverses,
     Headline => "specify whether generators are invertible",
     TT "Inverses", " -- an option used in creating a monoid which tells
     whether negative exponents will be allowed, making the monoid into
     a group.",
     SEEALSO "monoid"
     }

document { GeneralOrderedMonoid,
     Headline => "the class of all ordered free commutative monoids",
     "This is the class of free monoids that can be handled by 
     the ", TO "engine", ".  Elements of such monoids are implemented
     as instances of ", TO "MonoidElement", ".",
     PARA,
     SEEALSO { "monoid" }
     }     

document { (symbol _, Monoid, ZZ),
     Headline => "get a generator of a monoid",
     TT "M_i", " -- produces the i-th generator of a monoid ", TT "M", ".",
     PARA,
     SEEALSO { "Monoid", "_" }
     }

document { degreesMonoid,
     Headline => "get the monoid of degrees",
     TT "degreesMonoid n", " -- returns the monoid whose elements correspond
     to the multi-degrees of monomials in another monoid.",
     PARA,
     "Also used as a key under which to store the result."
     }

document { RevLex,
     Headline => "reverse lexicographic ordering",
     TT "RevLex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the reverse lexicographic order."
     }

document { GRevLex,
     Headline => "reverse lexicographic ordering",
     TT "GRevLex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the graded reverse lexicographic order.",
     PARA,
     CAVEAT "If the number of degree vectors is greater than one, this
     is currently only graded using the first degree vector.  This will 
     eventually change."  -- MES
     }
document { GLex,
     Headline => "graded lexicographic ordering",
     TT "GLex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the graded lexicographic order.",
     PARA,
     CAVEAT "If the number of degree vectors is greater than one, this
     is currently only graded using the first degree vector.  This will 
     eventually change."  -- MES
     }

document { Lex,
     Headline => "lexicographic ordering",
     TT "Lex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the (non-graded) lexicographic order."
     }

document { Eliminate,
     Headline => "elimination ordering",
     TT "Eliminate", " n -- an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the elimination order eliminating the
     first n variables, refined by the graded reverse lexicographic order.",
     PARA,
     CAVEAT "If the number of degree vectors is greater than one, this
     is currently only graded using the first degree vector.  This will 
     eventually change."  -- MES
     }

document { ProductOrder,
     Headline => "product ordering",
     TT "ProductOrder", "{n1, ..., nr} -- an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the product of r graded reverse lex
     orders, each with n1, n2, ..., nr variables.",
     PARA,
     CAVEAT "If the number of degree vectors is greater than one, the
     grading in each block only uses the first degree vector. This will 
     eventually change."  -- MES
     }

document { VariableBaseName,
     Headline => "base name for variables",
     TT "VariableBaseName => x", " -- an optional argument used when creating
     monoids or rings to specify that the variables should be ",
     TT "x_0, ..., x_n", "."
     }

document { MonomialOrder,
     Headline => "monomial ordering",
     TT "MonomialOrder", " -- an optional argument used with monoids to indicate a
     monomial ordering other than the default (graded reverse lexicographic)",
     PARA,
     "Permissible values:",
     MENU {
	  TO "GRevLex",
	  TO "GLex",
	  TO "Lex",
	  TO "RevLex",
	  TO "Eliminate",
	  TO "ProductOrder"
          },
     "Eventually, more general monomial orders will be allowed.", -- MES
     SEEALSO {"polynomial rings with other monomial orderings", "Weights"}
     }

document { Weights,
     Headline => "specify monomial ordering by weights",
     TT "Weights => {...}", " -- a keyword for an option used in specifying
     monomial orderings.",
     PARA,
     "This feature is currently under development."
     }

document { Variables,
     Headline => "specify the variable names",
     TT "Variables", " -- a key used with monoids to indicate the list of 
     variable names, or the number of variables.",
     PARA,
     "This option is useful for those situations when one doesn't care about the
     names of the variables in a ring or monoid, or when one is creating a 
     tensor product ring, symmetric algebra, or other ring, and one wants control
     over the names of the ring variables."
     }

document { VariableOrder,
     TT "VariableOrder", " -- an option used when creating a monoid.",
     PARA,
     "Not implemented yet.",
     SEEALSO "monoid"
     }

monoidOptions := first frame first frame lookup(monoid,Array)
assert( monoidOptions #? MonomialOrder )

document { (monoid, Array),
     Headline => "make a polynomial ring or monoid ring",
     TT "monoid [a,b,c,...]", " -- makes a free ordered commutative monoid on the variables listed.",
     PARA,
     "Optional arguments (placed between the brackets):",
     SHIELD MENU (TO \ keys monoidOptions),
     SEEALSO {(symbol " ", Ring, Array)}
     }

document { (symbol " ", Ring, Array),
     Headline => "the standard way to make a polynomial ring",
     TT "R[...]", " -- produces the monoid ring from a ring ", TT "R", " and the
     ordered monoid specified by ", TT "[...]", ".",
     PARA,
     "This is the customary way to make a polynomial ring.",
     PARA,
     "Optional arguments (placed inside the array):",
     MENU (TO \ keys monoidOptions),
     SEEALSO "polynomial rings"
     }

document { (symbol " ",Ring, OrderedMonoid),
     Headline => "make a polynomial ring",
     TT "R M", " -- produces the monoid ring from a ring ", TT "R", " and an ordered monoid
     ", TT "M", ".",
     SEEALSO "polynomial rings"
     }

document { (monoid, Ring),
     Headline => "get the monoid from a monoid ring",
     TT "monoid R", " -- yields the underlying monoid of polynomial ring
     or monoid ring.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "monoid R"
	  }
     }

document { monoid,
     Headline => "make a monoid",
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

document { (symbol **, Monoid, Monoid),
     Headline => "tensor product of monoids",
     TT "M ** N", " -- tensor product of monoids.",
     PARA,
     "For complete documentation, see ", TO "tensor", "."
     }

document { tensor,
     Headline => "tensor product",
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
     Headline => "make a table (nested list)",
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
     Headline => "apply a function to elements of a table",
     TT "applyTable(m,f)", " -- applies the function f to each element of the table m.",
     PARA,
     "It yields a table of the same shape as m containing the resulting values.",
     PARA,
     "See also ", TO "table", "."
     }

document { subtable,
     Headline => "extract a subtable from a table",
     TT "subtable(u,v,m)", " -- yields the subtable of the table m obtained from the
     list u of row numbers and the list v of column numbers.",
     PARA,
     EXAMPLE {
	  "m = table(5,5,identity)",
      	  "subtable({1,3,4},toList(2..4), m)"
	  },
     }

document { transpose,
     Headline => "transpose",
     TT "transpose m", " -- yields the transpose ", TT "n", " of the table or homomorphism ", TT "m", "."
     }

document { vector,
     Headline => "make a vector",
     TT "vector {a,b,c,...}", " -- produces an element of a free module from a list.",
     PARA,
     "The elements a,b,c,... must be elements of the same ring, or be
     convertible to elements of the same ring."
     }

document { Module,
     Headline => "the class of all modules",
     PARA,
     "Common ways to make a module:",
     SHIELD MENU {
	  TO (symbol ^, Ring, ZZ),
	  TO (symbol ^, Ring, List),
	  TO (cokernel, Matrix),
	  TO (image, Matrix),
	  TO (kernel, Matrix),
	  TO (homology, Matrix, Matrix)
	  },
     "Common ways to get information about modules:",
     MENU {
	  TO (ring, Module),
	  TO (numgens, Module),
	  TO (degrees, Module),
	  TO (generators, Module),
	  TO (relations, Module),
	  TO "isFreeModule",
	  TO (isHomogeneous, Module),
	  TO "rank",
	  TO (ambient, Module),
	  TO (cover, Module),
	  TO (super, Module),
	  },
     "Common operations on modules:",
     MENU {
	  TO (symbol +, Module, Module),
	  TO (symbol /, Module, Module),
	  TO (symbol ==, Module, Module),
	  TO (symbol ++, Module, Module),
	  TO (symbol **, Module, Module),
	  TO (symbol ^, Module, List),
	  TO (symbol _, Module, List),
	  },
     "Numerical information about a module:",
     MENU {
	  TO (codim, Module),
	  TO (degree, Module),
	  TO (dim, Module),
	  TO (genera, Module),
	  TO (hilbertSeries, Module),
	  TO (hilbertFunction, ZZ, Module),
	  TO (poincare, Module),
	  TO (pdim, Module),
	  TO (regularity, Module),
	  TO (rank, Module)
	  },
     "Common computations on modules:",
     MENU {
	  TO (symbol :, Module, Ideal),
	  TO (annihilator, Module),
	  TO (gb, Module),
	  TO (prune, Module),
	  TO (res, Module),
	  TO (saturate, Module, Ideal),
	  TO "Hom",
	  TO (homomorphism,Matrix),
	  TO (Ext,ZZ,Module,Module),
	  TO (Tor,ZZ,Module,Module)
	  },
     "Common ways to use a module:",
     MENU {
	  TO (fittingIdeal, ZZ, Module),
	  TO (isSubset, Module, Module),
	  TO (exteriorPower,ZZ,Module),
	  },
     }

document { isModule,
     Headline => "whether something is a module."
     }

document { isFreeModule,
     Headline => "whether something is a free module",
     Usage =>
       {TT "isFreeModule M", " -- is the module ", TT "M", " evidently a 
     free module."}
     }

document { (isFreeModule,Module),
     Synopsis => {
	  "b = isFreeModule(M)",
	  "M" => null,
	  "b" => {"whether ", TT "M", " is evidently a free module."}
	  },
     "No computation is done, so the module may be free but we don't
     detect it.  To try to determine whether ", TT "M", " is isomorphic to a free 
     module, one may prune ", TT "M", " first.",
     EXAMPLE {
	  "R = ZZ/101[x,y];",
      	  "M = kernel vars R",
      	  "isFreeModule M",
      	  "isFreeModule prune M"
	  },
     SEEALSO {(prune,Module)}
     }
     
document { isSubmodule,
     Headline => "whether a module is evidently a submodule of a free module"
     }
document { (isSubmodule, Module),
     Synopsis => {
	  "b = isSubmodule(M)",
	  "M" => null,
	  "b" => {"whether ", TT "M", " is evidently a submodule of a free module."}
	  },
     "No computation is done, so the module may be isomorphic to a submodule
     of a free module but we don't detect it.",
     EXAMPLE {
	  "R = ZZ/101[a,b,c];",
	  "M = R^3;",
	  "N = ideal(a,b) * M",
	  "isSubmodule N",
	  "N1 = ideal(a,b) * (R^1 / ideal(a^2,b^2,c^2))",
	  "isSubmodule N1"
	  }
     }

document { isQuotientModule,
     Headline => "whether a module is evidently a quotient of a free module"
     }

document { (isQuotientModule, Module),
     Synopsis => {
	  "b = isQuotientModule(M)",
	  "M" => null,
	  "b" => {"whether ", TT "M", " is evidently a quotient of a free module."}
	  },
     "No computation is done.  This routine simply detects whether the given description
     of ", TT "M", " is such a quotient.",
     EXAMPLE {
	  "R = ZZ/101[a,b,c];",
	  "M = R^1/(a^2,b^2,c^2)",
	  "isQuotientModule M",
	  "f = M_{0}",
	  "N = image f"
	  },
     "Recall (", TO (symbol_, Module, List), ") that ", TT "f", " is a map to the first generator of ",
     TT "M", " so that the module ", TT "N", " is the same as ", TT "M", " but its description is now as a
     submodule of ", TT "M", " so isQuotientModule returns false.  However, these two modules are equal:",
     EXAMPLE {
	  "isQuotientModule N",
	  "M == N"
	  }
     }

document { isIdeal,
     Headline => "whether something is an ideal"
     }

document { (isIdeal, Module),
     Synopsis => {
	  "b = isIdeal(M)",
	  "M" => null,
	  "b" => {"whether ", TT "M", " is evidently an ideal."}
	  },
     "No computation is done.  This routine checks the given description
     of ", TT "M", " to see if it is an ideal.",
     EXAMPLE {
	  "R = QQ[a..d]/(a*b*c*d);",
	  "I = ideal(a^2,b^2) * R^1",
	  "isIdeal I",
	  "J = a^2 * R^2 + a*b * R^2",
	  "isIdeal J"
	  }
     }

document { numgens,
     Headline => "the number of generators",
     TT "numgens X", " -- yields the number of generators used to present
     a module or ring.",
     PARA,
     "For a polynomial ring or quotient of one, this is also the number
     of variables.  For a free module, this is the same as the rank.  For
     a general module presented as a subquotient, it is the number of columns
     in the matrix of generators."
     }

document { relations,
     Headline => "the defining relations",
     TT "relations M", " -- produce the relations defining a module M.",
     PARA,
     "The relations are represented as a matrix, and if not stored
     in the module under M.relations, the matrix is understood to be
     empty.",
     PARA,
     SEEALSO {"generators","subquotient"}
     }

document { (symbol ==, Module, Module),
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

document { (symbol ==, Ideal, ZZ),
     Synopsis => {
	  "I == n",
	  "I" => null,
	  "n" => "either 0, or 1."
	  },
     PARA,
     "If n==1, then determines whether I is the unit ideal.
     If n==0, then determines whether I is the zero ideal.  Any other value
     for n is an error.",
     PARA,
     EXAMPLE {
	  "R = QQ[x];",
      	  "ideal(x^2,x+1) == 1",
      	  "ideal(0_R) == 0"
	  },
     }

TEST "
R = ZZ/101[a,b,c]
M = cokernel matrix {{a,b^2,c^3}}
N = image M_{0}
assert( M == N )
"
document { Vector, 
     Headline => "the class of all elements of free modules which are handled by the engine",
     "If ", TT "R", " is a ring handled by the engine, and ", TT "M", " is a free
     module over ", TT "R", ", then M is a subclass of Vector.",
     PARA,
     SEEALSO {"engine", "Module"}
     }

document { (symbol _, Vector, ZZ),
     Headline => "get a component",
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
     Headline => "degrees of generators"
     }

document { (degrees, Ideal),
     Synopsis => {
	  "s = degrees I",
	  "I" => null,
	  "s" => { "the list of multi-degrees for the generators of the
	       module ", TT "I", "."
	       }
	  }
     }

document { (degrees, Matrix),
     Synopsis => {
	  "s = degrees f",
	  "f" => null,
	  "s" => { "a list ", TT "{x,y}", " where ", TT "x", " is the list
	       of degrees of the target of ", TT "f", " and ", TT "y", " is the
	       list of degrees of the source of ", TT "f", "."
	       }
	  }
     }

document { (degrees, Module),
     Synopsis => {
	  "s = degrees M",
	  "M" => null,
	  "s" => { "the list of multi-degrees for the generators of the
	       module ", TT "M", "."
	       }
	  },
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "E = R^5",
      	  "degrees E",
      	  "F = R^{1,2,3,4}",
      	  "degrees F"
	  }
     }

document { (degrees, Ring),
     Synopsis => {
	  "s = degrees R",
	  "R" => null,
	  "s" => { "the list of multi-degrees for the generators (variables) of the
	       ring ", TT "R", "."
	       }
	  },
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "degrees R",
	  "S = ZZ/101[x,y,z,Degrees => {{2,3},{1,2},{2,0}}]",
      	  "degrees S"
	  }
     }

document { (symbol ^, Ring, List),
     Headline => "make a free module",
     Synopsis => {
	  "M = R^{i,j,k,...}",
	  "R" => null,
	  "{i,j,k, ...}" => {"a list of integers or lists of integers"},
	  "M" => {"a free module over ", TT "R", " whose generators have
     	       degrees ", TT "-i", ", ", TT "-j", ", ", TT "-k", ", ..."}
	  },
     PARA,
     "If ", TT "i", ", ", TT "j", ", ... are lists of integers, then
     they represent multi-degrees, as in ", TO "multi-graded polynomial rings", ".",
     SEEALSO {"degrees", "free modules"}
     }

document { components,
     Headline => "list the components of a direct sum",
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

document { (symbol ^,Module,ZZ),
     Headline => "make a direct sum of several copies of a module",
     Synopsis => {
	  "Y = M^n",
	  "M" => {"a module"},
	  "n" => null,
	  "Y" => {"the direct sum of ", TT "n", " copies of ", TT "M", ""}
	  }
     }

document { (symbol ^,Ring,ZZ),
     Headline => "make a free module",
     Synopsis => {
	  "F = R^n",
	  "R" => {"a ring"},
	  "n" => null,
	  "F" => {"a new free ", TT "R", "-module of rank ", TT "n", "." }
	  },
     "The new free module has basis elements of degree zero.  To specify the
     degrees explicitly, see ", TO (symbol ^,Ring,List), "."
     }

document { euler,
     Headline => "list the sectional Euler characteristics",
     TT "euler M", " -- provide a list of the successive sectional Euler 
     characteristics of a module, ring, or ideal.",
     PARA,
     "The i-th one in the list is the Euler characteristic of the i-th
     hyperplane section of M."
     }

document { genera,
     Headline => "list the sectional arithmetic genera",
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
     Headline => "compute the rank",
     TT "rank M", " -- computes the rank of the module M.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = vars R;",
      	  "rank kernel p",
      	  "rank cokernel p"
	  },
     }

document { coverMap,
     Headline => "get the map to the module given by the generators of a module",
     Synopsis => {
	  "g = coverMap M",
	  "M" => null,
	  "g" => { "the map from a free module to ", TT "M", " given by the 
	       generators of ", TT "M", "." 
	       }
	  },
     SEEALSO { "cover" }
     }

document { cover,
     Headline => "get the covering free module",
     TT "cover M", " -- yields the free module whose basis elements correspond
     to the generators of M.",
     PARA,
     SEEALSO {"ambient", "super"}
     }

document { (cover,Module),
     Synopsis => {
	  "F = cover M",
	  "M" => null,
	  "F" => {"the free module whose basis elements correspond
	          to the generators of ", TT "M", "."}
	  },
     PARA,
     "The free module ", TT "F", " is the source of the generator matrix 
     of ", TT "M", ".",
     EXAMPLE "R = QQ[a..f];",
     EXAMPLE "g = matrix{{a,b},{c,d},{e,f}}",
     EXAMPLE "M = subquotient(g,matrix{{b},{c},{d}})",
     EXAMPLE "cover M",
     EXAMPLE "cover M == source generators M",
     SEEALSO {(ambient,Module), (super,Module)}
     }

document { super,
     Headline => "get the ambient module",
     TT "super M", " -- yields the module which the module ", TT "M", " is a submodule of.",
     BR, NOINDENT,
     TT "super f", " -- if ", TT "f", " is a map whose target is a submodule 
     of ", TT "M", ", yields the composite of ", TT "f", " with the inclusion into ", TT "M", ".",
     PARA,
     SEEALSO { "cover", "ambient" }
     }

document { End,
     Headline => "module of endomorphisms",
     TT "End M", " -- constructs the module of endomorphisms of ", TT "M", "."
     }

document { ModuleMap,
     Headline => "the class of all maps between modules",
     "This class is experimental, designed to support graded modules.",
     SEEALSO {"Matrix"}
     }


document { (symbol *, Matrix, Matrix),
     Headline => "matrix multiplication",
     "Multiplication of matrices corresponds to composition of maps, and when
     ", TT "f", " and ", TT "g", " are maps so that the target ", TT "Q", "
     of ", TT "g", " equals the source ", TT "P", " of ", TT "f", ", the
     product ", TT "f*g", " is defined, its source is the source of ", 
     TT "g", ", and its target is the target of ", TT "f", ".  The degree of ",
     TT "f*g", " is the sum of the degrees of ", TT "f", " and of ", TT "g",
     ".  The product is also defined when ", TT "P", " != ", TT "Q", ",
     provided only that ", TT "P", " and ", TT "Q", " are free modules of the
     same rank.  If the degrees of ", TT "P", " differ from the corresponding
     degrees of ", TT "Q", " by the same degree ", TT "d", ", then the degree
     of ", TT "f*g", " is adjusted by ", TT "d", " so it will have a good
     chance to be homogeneous, and the target and source of ", TT "f*g", "
     are as before."
     }
     
document { Matrix,
     Headline => "the class of all matrices",
     "A matrix is a homomorphism between two modules, together with
     an integer (or vector of integers) called its degree, which is
     used when determining whether the map is homogeneous.  The matrix
     is stored in the usual way as a rectangular array of ring elements.
     When the source or target modules are not free, the matrix is
     interpreted as a linear transformation in terms of the generators
     of the modules.",
     SEEALSO "matrices",
     PARA,
     "A matrix ", TT "f", " is an immutable object, so if you want to 
     cache information about it, put it in the hash table ", TT "f.cache", ".",
     PARA,
     "Common ways to make a matrix:",
     MENU {
	  TO "map",
	  TO "matrix",
	  },
     "Common ways to get information about matrices:",
     MENU {
	  TO (degree, Matrix),
	  TO (isHomogeneous, Matrix),
	  TO (matrix, Matrix),
	  },
     "Common operations on matrices:",
     MENU {
	  TO (symbol +, Matrix, Matrix),
	  TO (symbol -, Matrix, Matrix),
	  TO (symbol *, RingElement, Matrix),
	  TO (symbol *, Matrix, Matrix),
	  TO (symbol ==, Matrix, Matrix),
	  TO (symbol ++, Matrix, Matrix),
	  TO (symbol **, Matrix, Matrix),
	  TO (symbol %, Matrix, Matrix),
	  TO (symbol //, Matrix, Matrix),
	  TO (symbol |, Matrix, Matrix),
	  TO (symbol ||, Matrix, Matrix),
	  TO (symbol ^, Matrix, List),
	  TO (symbol _, Matrix, List),
	  },
     "Common ways to use a matrix:",
     MENU {
	  TO (cokernel, Matrix),
	  TO (image, Matrix),
	  TO (kernel, Matrix),
	  TO (homology, Matrix, Matrix),
	  },
     }

document { getMatrix,
     Headline => "get a matrix from the engine's stack",
     TT "getMatrix R", " -- pops a matrix over ", TT "R", " from the top of 
     the engine's stack and returns it.",
     PARA,
     "Intended for internal use only."
     }

document { (symbol _, Matrix, Sequence),
     Headline => "get an entry",
     TT "f_(i,j)", " -- provide the element in row ", TT "i", " and
     column ", TT "j", " of the matrix ", TT "f", ".",
     SEEALSO {"_", "Matrix"}
     }

document { (symbol _, Matrix, ZZ),
     Headline => "get a column from a matrix",
     TT "f_i", " -- provide the ", TT "i", "-th column of a matrix ", TT "f", " as a vector.",
     PARA,
     "Vectors are disparaged, so we may do away with this function in the future.",
     SEEALSO "_"
     }

document { isWellDefined,
     Headline => "whether a map is well defined" }

document { isDirectSum,
     Headline => "whether something is a direct sum",
     "Works for modules, graded modules, etc.  The components of the sum
     can be recovered with ", TO "components", "."
     }

TEST "
assert isDirectSum (QQ^1 ++ QQ^2)
assert isDirectSum (QQ^1 ++ QQ^2)
"

document { youngest,
     Headline => "the youngest member of a sequence",
     TT "youngest s", " -- return the youngest mutable hash table in the sequence
     ", TT "s", ", if any, else ", TO "null", "."
     }

document { (symbol ++,Module,Module),
     Headline => "direct sum of modules",
     TT "M++N", " -- computes the direct sum of two modules.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "image vars R ++ kernel vars R",
	  },
     "Projection and inclusion maps for direct sums:",
     SHIELD MENU {
	  TO (symbol ^,Module,Array),
	  TO (symbol _,Module,Array)
	  },
     SEEALSO directSum
     }

document { (symbol ++,Matrix,Matrix),
     Headline => "direct sum of maps",
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
     SHIELD MENU {
	  TO (symbol ^,Matrix,Array),
	  TO (symbol _,Matrix,Array)
	  },
     SEEALSO {directSum, (symbol |, Matrix, Matrix), (symbol ||, Matrix, Matrix)}
     }

document { directSum,
     Headline => "direct sum of modules or maps",
     TT "directSum(M,N,...)", " -- forms the direct sum of matrices or modules.",
     PARA,
     "The components can be recovered later with ", TO "components", ".",
     PARA,
     "Projection and inclusion maps for direct sums:",
     SHIELD MENU {
	  TO (symbol ^,Module,Array),
	  TO (symbol _,Module,Array),
	  TO (symbol ^,Matrix,List),
	  TO (symbol _,Matrix,List)
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
     Headline => "specify keys for components of a direct sum",
     TT "indexComponents", " -- a symbol used as a key in a direct sum
     under which to store a hash table in which to register preferred keys used
     to index the components of the direct sum.",
     PARA,
     SEEALSO {"directSum", "components", "indices"}
     }

document { indices,
     Headline => "specify keys for components of a direct sum",
     TT "indices", " -- a symbol used as a key in a direct sum
     under which to store a list of the preferred keys used
     to index the components of the direct sum.",
     PARA,
     SEEALSO {"directSum", "components", "indexComponents"}
     }
