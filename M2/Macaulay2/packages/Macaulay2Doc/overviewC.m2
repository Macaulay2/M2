-- -*- coding: utf-8 -*-
----------- File Mike is Working on! -------------------------

document {
     Key => "basic rings of numbers",
     "The following rings are initially present in every session with
     Macaulay2.",
     UL { 
	  TO "ZZ", 
	  TO "QQ", 
	  TO "RR",
	  TO "CC",
	  },
     "The names of some of these rings are double letters so the corresponding symbols
     with single letters are preserved for use as variables.",
     PARA{},
     "Numbers in these rings are constructed as follows.",
     EXAMPLE {
	  "1234",
      	  "123/4",
      	  "123.4",
      	  "1.234e-20",
      	  "123+4*ii",
	  },
     "The usual arithmetic operations are available.",
     EXAMPLE {
	  "4/5 + 2/3",
      	  "10^20",
      	  "3*5*7",
      	  "3.1^2.1",
	  "sqrt 3.",
	  },
     "An additional pair of division operations that produce integer quotients and remainders is available.",
     EXAMPLE {
	  "1234//100",
      	  "1234%100"
	  },
     "Numbers can be promoted to larger rings as follows, see ", TO (symbol _, RingElement, Ring), ".",
     EXAMPLE lines ///
     1_QQ
     (2/3)_CC
     ///,
     "One way to enter real and complex numbers with more precision is to insert the desired number of bits of precision
     after the letter p at the end of the number, but before the possible e that indicates the exponent of 10.",
     EXAMPLE lines ///
     1p300
     1p300e-30
     ///,
     "Numbers can be lifted to smaller rings as follows, see ", TO "lift", ".",
     EXAMPLE lines ///
     x = 2/3*ii/ii
     lift(x,RR)
     lift(x,QQ)
     ///
     }


document {
	Key => "integers modulo a prime",
	"Create the ring of integers modulo a prime number ", TT "p", " as follows.",
     	EXAMPLE {
	     "R = ZZ/101"
	     },
     	"We can create elements of the ring as follows.",
     	EXAMPLE {
	     "9_R",
	     "103_R"
	     },
     	"The usual arithmetic operations are available.",
     	EXAMPLE {
	     "9_R * 11_R",
	     "9_R ^ 11",
	     "9_R * 11_R == -2_R"
	     },
	"Find the inverse of an integer modulo a prime as follows.",
	EXAMPLE {
	     "17_R^-1"
	     },
	"To view this element as an element of ", TT "ZZ", " use the ", TO "lift", " command.",
	EXAMPLE {
	     "lift (17_R^-1, ZZ)"
	     } 
	}

document {
     Key => "finite fields",
     -- Also include: getting the variable, its equation.
     -- Current restrictions on p, p^n.
     -- example should include: making these, simple arithmetic
     -- Pointer to finite fields II.
	"Two basic finite fields are:",
     UL {
	  TO2 {"integers modulo a prime", "ZZ/p"},
	  TT "GF(p^n)"
	  },
     "Create a finite field with $q = p^n$ elements using",
     EXAMPLE "F = GF(81,Variable=>a)",
     "This creates the ring of characteristic 3, having 3^4 = 81 elements.  The elements
     of this ring are 0, a, a^2, a^3, ..., a^80.",
     EXAMPLE {
	  "a^80",
	  "a^40"
	  },
     "Use ", TO "ambient", " to see the quotient ring the field is made from.",
     EXAMPLE "ambient F",
     "Now check that ", TT "a", " satisfies this equation.",
     EXAMPLE "a^4 + a - 1",
     "It is often preferable to view elements of ", TT "F", " as polynomials
     in ", TT "a", " rather than as powers of ", TT "a", ".  This can be accomplished
     by lifting the elements back to this ambient ring.",
     EXAMPLE {
	  "lift(a^20, ambient F)",
	  "apply({20,40,80}, i -> lift(a^i, ambient F))",
	  },
     "(for more details on lift, see ", TO "working with multiple rings", ").",
     PARA{},
     "Finite fields can be used as base rings for polynomial rings.",
     EXAMPLE {
	  "R = F[x,y,z]",
      	  "f = random(2,R)",
	  "f = (leadCoefficient f)^(-1) * f"
	  },
     "Gröbner bases, and all related computations work in these rings.",
     PARA{},
	"The prime finite fields can be made easily as quotient rings of ", TO "ZZ", ".",
	EXAMPLE "ZZ/101",
     "In general, to make a finite field with ", TT "q", " elements, we use
     ", TO "GF", ".",
     EXAMPLE "k = GF 81",
     "The generator of the field can be obtained as usual.",
     EXAMPLE "k_0",
     "You may use ", TO "ambient", " to see the quotient ring the field is made from.",
     EXAMPLE "ambient k",
     "Use ", TO "ideal", " to see the ideal that defined that quotient ring.",
     EXAMPLE "ideal oo",
     "Finally, you may use ", TO "_", " to recover the generator of the ideal.",
     EXAMPLE "oo_0",
     "To specify a different name for the generator when the field is created, 
     use the ", TO "Variable", " option.",
     EXAMPLE {
	  "F = GF(16, Variable => b)",
      	  "b^20 + 1",
      	  "random F",
	  },
     "Finite fields can be used as base rings for polynomial rings.",
     EXAMPLE {
	  "R = F[x,y,z]",
      	  "random(2,R)",
	  },
     "If you have a quotient ring that you know is a finite field, then you can
     convert it to ring that is known by the system to be a finite field.",
     EXAMPLE "GF (ZZ/2[T]/(T^9+T+1), Variable => T) -* no-capture-flag *-",
     "You may also provide your own choice of primitive element.  Internally,
     elements of the finite field are stored as powers of the primitive element.
     First we assign our quotient ring to a global variable to ensure that
     ", TT "T", " gets set to a value in the quotient ring, and then we
     call ", TT "GF", ".",
     EXAMPLE {
	  "A = ZZ/2[T]/(T^9+T+1)",
      	  "k = GF (A, PrimitiveElement => T^3+1)",
	  },
     "Notice that ", TT "T", " is now recorded as an element of this finite field.",
     EXAMPLE "T",
     "The generator of A can be obtained this way:",
     EXAMPLE "A_0",
     "Use ", TO "substitute", " to map it to an element of the finite field.",
     EXAMPLE "substitute(A_0,k)",
     "Conversely, a given element of the finite field can be transferred back
     to the quotient ring with ", TO "lift", ".",
     EXAMPLE "lift(k_0, ring T)",
     "We can even lift it back to the polynomial ring.",
     EXAMPLE "lift(k_0, ambient ring T)",
     "For more information see ", TO "GaloisField", "."
     }

document {
     Key => "quotient rings",
     -- R/I.  GB of I is needed for arithmetic.
     -- The variables get set?  Doing a quotient ring twice: ie. R/I, then R/I
     -- gives DIFFERENT rings.  Pointer to working with multiple rings.
     "The usual notation is used to form quotient rings.  For quotients of
     polynomial rings, a Gröbner basis is computed
     and used to reduce ring elements to normal form after arithmetic operations.",
     EXAMPLE {
	  "R = ZZ/11",
      	  "6_R + 7_R",
	  },
     EXAMPLE {
	  "S = QQ[x,y,z]/(x^2-y, y^3-z)",
      	  "{1,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8}",
	  },
     "In the example above you might have wondered whether typing ", TT "x", "
     would give an element of ", TT "S", " or an element of ", TT "QQ[x,y,z]", ".  Our
     convention is that typing ", TT "x", " gives an element of the 
     last ring that has been assigned to a global variable.  Here is another
     example.",
     EXAMPLE {
	  "T = ZZ/101[r,s,t]",
      	  "T/(r^3+s^3+t^3)",
      	  "r^3+s^3+t^3",
	  },
     "Notice that this time, the variables end up in the ring ", TT "T", ", because
     we didn't assign the quotient ring to a global variable.  The
     command ", TO "use", " would install the variables for us, or we could
     assign the ring to a global variable.",
     EXAMPLE {
	  "U = ooo",
      	  "r^3+s^3+t^3",
	  },
     "The functions ", TO "lift", " and ", TO "substitute", " can be used to transfer
     elements between the polynomial ring and its quotient ring.",
     EXAMPLE {
	  ///lift(U_"r",T)///,
      	  ///substitute(T_"r",U)///,
	  },
     "A random element of degree ", TT "n", " can be obtained with ", TO "random", ".",
     EXAMPLE "random(2,S)",
     "In a program we can tell whether a ring is a quotient ring.",
     EXAMPLE {
	  "isQuotientRing ZZ",
      	  "isQuotientRing S",
	  },
     "We can recover the ring of which a given ring is a quotient.",
     EXAMPLE "ambient S",
     "We can also recover the coefficient ring, as we could for the original 
     polynomial ring.",
     EXAMPLE "coefficientRing S",
     "Here's how we can tell whether the defining relations of a quotient
     ring were homogeneous.",
     EXAMPLE {
	  "isHomogeneous S",
      	  "isHomogeneous U",
	  },
     "We can obtain the characteristic of a ring with ", TO "char", ".",
     EXAMPLE {
	  "char (ZZ/11)",
      	  "char S",
      	  "char U",
	  },
     "The presentation of the quotient ring can be obtained as a matrix
     with ", TO "presentation", ".",
     EXAMPLE "presentation S",
     "If a quotient ring has redundant defining relations, a new ring can
     be made in which these are eliminated with ", TO "trim", ".",
     EXAMPLE {
	  "R = ZZ/101[x,y,z]/(x-y,y-z,z-x)",
      	  "trim R"
	  },
     "For more information see ", TO "QuotientRing", "."
     }

document {
     Key => "manipulating polynomials",
     UL {
	  TO "+",
	  TO "-",
	  TO "*",
	  TO "^",
	  TO "//",
	  TO "%",
	  TO "terms",
	  TO "diff",
	  -- TO "f _ ZZ",
	  -- TO "f _ monomial",
	  TO "listForm",
	  TO "degree",
	  TO "homogenize",
	  TO "exponents",
	  TO "leadCoefficient",
	  TO "leadTerm",
	  TO "size",
	  },
     "Let's set up some polynomials.",
     EXAMPLE {
	  "R = ZZ/10007[a,b];",
      	  "f = (2*a+3)^4 + 5",
      	  "g = (2*a+b+1)^3",
	  },
     "The number of terms in a polynomial is obtained with ", TO "size", ".",
     EXAMPLE "size f, size g", 
     "The degree of a polynomial is obtained with ", TO "degree", ".",
     EXAMPLE {
	  "degree f", 
      	  "degree g",
	  },
     "(Notice that the degree is a list containing one integer, rather than
     an integer.  The degree is actually a vector of integers, represented as
     a list, with one component by default.)",
     PARA{},
     "The list of terms of a polynomial is obtained with ", TO "terms", ".",
     EXAMPLE "terms g",
     "We may combine that with ", TO "select", " to select terms satisfying 
     certain conditions.  Here we select the terms of degree 2, subsequently 
     summing them, keeping in mind that the degree of a polynomial is always a 
     list of integers.",
     EXAMPLE {
	  "select(terms g, i -> degree i == {2})",
      	  "sum oo",
	  },
     "Of course, if the list of selected terms is empty, the sum would turn out
     to be the zero integer, rather than the zero element of the ring ", TT "R", ".
     Fortunately, we have another way to select the elements of given degree
     or multi-degree (see ", TO part, ").",
     EXAMPLE {
	  "part(0,g)",
	  "part(1,g)",
	  "part(2,g)",
	  "part(3,g)"
	  },
     "A string representing the polynomial, suitable for entry into other programs,
     can be obtained with ", TO "toString", ".",
     EXAMPLE {
	  "toString f",
      	  "toString g",
	  },
     PARA{},
     "The usual algebraic operations on polynomials are available, but there
     are some special remarks to make about division.  The result of division
     depends on the ordering of monomials chosen when the ring is created, for
     division of ", TT "f", " by ", TT "g", " proceeds by locating monomials in
     ", TT "f", " divisible by the leading monomial of ", TT "g", ", and
     substituting for it the negation of the rest of ", TT "g", ".  The quotient 
     is provided by the expression ", TT "f//g", ", and the remainder is obtained 
     with ", TT "f%g", ".",
     EXAMPLE {
	  "quot = f//g",
      	  "rem = f%g",
      	  "f == quot * g + rem",
	  },
     "Notice that as in the example above, comparison of polynomials is done
     with the operator ", TO "==", ".",
     PARA{},
     "Polynomials can be homogenized with respect to one of the variables in the
     ring with ", TO "homogenize", ".",
     EXAMPLE "homogenize(f,b)",
     PARA{},
     "The ring containing a ring element can be obtained with ", TO "ring", ".",
     EXAMPLE "ring f",
     "You can use this in a program to check whether two ring elements 
     come from the same ring.",
     EXAMPLE "ring f === ring g",
     "Notice that in the comparison above, the strict equality operator ", TO "===", "
     is used.",     
     PARA{},
     "The coefficient of a monomial in a polynomial can be obtained with ", TO "_", ".",
     EXAMPLE {
	  "part(1,f)",
      	  "f_a",
      	  "g_(a*b)",
	  },
     "(Notice that the coefficients are elements of the coefficient ring.)",
     PARA{},
     "We may get parts of the leading term of a polynomial as follows.",
     EXAMPLE {
	  "leadTerm g",
      	  "leadCoefficient g",
      	  "leadMonomial g",
	  },
     "The exponents of a monomial or term can be extracted with ", TO "exponents", ".",
     EXAMPLE {
	 "exponents leadMonomial g",
	 "exponents leadTerm g",
	 },
     "We can get all of the coefficients at once, assembled in a one-rowed matrix,
     along with a matrix containing the corresponding monomials.",
     EXAMPLE {
	  "coefficients f",
      	  "coefficients g",
	  },
     "A list of lists of exponents appearing in a polynomial can be obtained with
     ", TO "exponents", ".",
     EXAMPLE {
	  "exponents f",
      	  "exponents g",
	  },
     "The entire structure of a polynomial can be provided in an accessible form
     based on lists with ", TO "listForm", ".",
     EXAMPLE {
	  "listForm f",
      	  "S = listForm g",
	  },
     "The lists above are lists of pairs, where the first member of each pair is
     a list of exponents in a monomial, and the second member is the corresponding
     coefficient.  Standard list operations can be used to manipulate the result.",
     EXAMPLE "S / print;",
     "The structure of a polynomial can also be provided in a form
     based on hash tables with ", TO "standardForm", ".",
     EXAMPLE {
	  "S = standardForm f",
      	  "standardForm g",
	  },
     "The hash tables above present the same information, except that only nonzero
     exponents need to be provided.  The information can be extracted with ", TO "#", ".",
     EXAMPLE "S#(new HashTable from {0 => 2})",
     PARA{},
--      "Monomials (monoid elements) have an accessible form that is implicitly used
--      above.",
--      EXAMPLE {
-- 	  "listForm leadMonomial g",
--       	  "standardForm leadMonomial g",
-- 	  },
     "Comparison of polynomials is possible, and proceeds by simply examining the
     lead monomials and comparing them.",
     EXAMPLE {
	  "f < g",
      	  "sort {b^2-1,a*b,a+1,a,b}"
	  },
     "The comparison operator ", TO "?", " returns a symbol indicating how two
     polynomials, or rather, their lead monomials, stand with respect to each 
     other in the monomial ordering.",
     EXAMPLE "f ? g",
     }

document {
     Key => "factoring polynomials",
     "Polynomials can be factored with ", TO "factor", ".  Factorization
     works in polynomial rings over prime finite fields, ZZ, or QQ.",
     EXAMPLE {
	  "R = ZZ/10007[a,b];",
	  "f = (2*a+3)^4 + 5",
	  "g = (2*a+b+1)^3",
	  "S = factor f",
      	  "T = factor g",
	  },
     PARA{},
     "The results have been packaged for easy viewing.  The number of
     factors is obtained using",
     EXAMPLE "#T",
     "Each factor is represented as a power (exponents equal
     to 1 don't appear in the display.)  The parts can be 
     extracted with ", TO "#", ".",
     EXAMPLE {
	  "T#0",
      	  "T#0#0",
      	  "T#0#1",
	  }
     }

document {
     Key => "fraction fields",
     "The fraction field of a ring (which must be an integral domain) is obtained
     with the function ", TO "frac", ".",
     EXAMPLE {
	  "frac ZZ",
      	  "R = ZZ/101[x,y]/(x^3 + 1 + y^3)",
      	  "frac R",
	  },
     "After defining a ring such as ", TT "R", ", fractions in its fraction field
     can be obtained by writing them explicitly.",
     EXAMPLE {
	  "x",
      	  "1/x",
      	  "x/1",
	  },
     "Alternatively, after applying the function ", TO "use", ", or assigning the
     fraction ring to a global variable, the symbols you used
     become associated with the corresponding elements of the fraction field.",
     EXAMPLE {
	  "use frac R",
      	  "x",
	  },
     "Fractions are reduced to the extent possible.  This is done by computing the
     syzygies between the numerator and denominator, and picking one of low degree.",
     EXAMPLE {
	  "f = (x-y)/(x^6-y^6)",
      	  "(x^3 - y^3) * f",
	  },
     "The parts of a fraction may be extracted.",
     EXAMPLE {
	  "numerator f",
      	  "denominator f",
	  },
     "Alternatively, the functions ", TO "lift", " and ", TO "liftable", " can
     be used.",
     EXAMPLE {
	  "liftable(1/f,R)",
      	  "liftable(f,R)",
      	  "lift(1/f,R)"
	  },
     "Note that computations, such as Gröbner bases, over fraction fields can be quite slow.",
     SeeAlso => {
	  frac,
	  numerator,
	  denominator,
	  liftable,
	  lift,
	  (kernel,RingMap)
	  }
     }

document {
     Key => "finite field extensions",
     UL {
	  TO "toField",
	  -- writeup under "toField" is a good start,
	  -- needs an example
	  }
     }

document {
     Key => "exterior algebras",
     -- making one, making quotients,
     -- using it.
     -- modules are right-modules, example of multiplication.
     "An exterior algebra is a polynomial ring where multiplication is
     mildly non-commutative, in that, for every x and y in the ring,
     y*x = (-1)^(deg(x) deg(y)) x*y, and that for every x of odd degree,
     x*x = 0.",
     "In Macaulay2, deg(x) is the degree of x, or the first degree of x, in case 
     a multi-graded ring is being used.  The default degree for each variable is 1, so
     in this case, y*x = -x*y, if x and y are variables in the ring.",
     PARA{},
     "Create an exterior algebra with explicit generators by creating a polynomial
     ring with the option ", TO "SkewCommutative", ".",
     EXAMPLE {
	  "R = QQ[x,y,z, SkewCommutative => true]",
      	  "y*x",
      	  "(x+y+z)^2",
      	  "basis R",
      	  "basis(2,R)",
	  },
     EXAMPLE {
	  "S = QQ[a,b,r,s,t, SkewCommutative=>true, Degrees=>{2,2,1,1,1}];",
	  "r*a == a*r",
	  "a*a",
	  "f = a*r+b*s; f^2",
	  "basis(2,S)",
	  },
     "All modules over exterior algebras are right modules.  This means that matrices 
     multiply from the opposite side:",
     EXAMPLE {
	  "x*y",
	  "matrix{{x}} * matrix{{y}}"
	  },
     "You may compute Gröbner bases, syzygies, and form quotient rings of these skew
     commutative rings."
     }

document {
     Key => "symmetric algebras",
     "Polynomial rings are symmetric algebras with explicit generators, and we have
     already seen how to construct them.  But if you have a module, then its symmetric
     algebra can be constructed with ", TO "symmetricAlgebra", ".",
     EXAMPLE {
	  "R = QQ[a..d];",
      	  "symmetricAlgebra R^3"
	  },
     "Maps between symmetric algebras can be constructed functorially.",
     EXAMPLE lines ///
     vars R
     symmetricAlgebra vars R
     symmetricAlgebra transpose vars R
     ///,
     "Until the ring is used with ", TO "use", " or assigned to a global variable, its
     generators are not assigned to global variables.",
     EXAMPLE {
	  "a",
	  "p_0",
	  "S = o2;",
	  "a",
	  "p_0"
	  },
     "To specify the names of the variables when creating the ring, use the 
     ", TO "Variables", " option or the ", TO "VariableBaseName", " option.",
     EXAMPLE {
	  "symmetricAlgebra(R^3, Variables => {t,u,v})",
	  "symmetricAlgebra(R^3, VariableBaseName => t)"
	  },
     "We can construct the symmetric algebra of a module that isn't
     necessarily free.",
     EXAMPLE {
	  "use R",
	  "symmetricAlgebra(R^1/(a,b^3))"
	  }
     }

document {
     Key => "tensor products of rings",
     -- **, tensor.  Options for changing monomial orders.
     -- What is the default monomial order
     -- What if the names of the variables clash.
     -- The tensor product of two quotients of poly rings is
     -- a quotient of another polynomial ring.
     "The operator ", TO "**", " or the function ", TO "tensor", " can be
     used to construct tensor products of rings.",
     EXAMPLE "ZZ/101[x,y]/(x^2-y^2) ** ZZ/101[a,b]/(a^3+b^3)",
     "Other monomial orderings can be specified.",
     EXAMPLE "T = tensor(ZZ/101[x,y], ZZ/101[a,b], MonomialOrder => Eliminate 2)",
     "The options to ", TT "tensor", " can be discovered with ", TO "options", ".",
     EXAMPLE "options tensor",
     "Given two (quotients of) polynomial rings, say, R = A[x1, ..., xn]/I, S = A[y1,...,yn]/J,
     then R ** S = A[x1,...,xn,y1, ..., yn]/(I + J).  The variables in the two rings are
     always considered as different.  If they have name conflicts, you may still use
     the variables with indexing, but the display will be confusing:",
     EXAMPLE {
	  "R = QQ[x,y]/(x^3-y^2);",
	  "T = R ** R",
	  "generators T",
	  "{T_0 + T_1, T_0 + T_2}"
	  },
     "We can change the variable names with the ", TO "Variables", " option.",
     EXAMPLE {
	  "U = tensor(R,R,Variables => {x,y,x',y'})",
	  "x + y + x' + y'"
	  }
     }

document {
     Key => "Weyl algebras",
     "A Weyl algebra is the non-commutative algebra of algebraic differential 
     operators on a polynomial ring.  To each variable ", TT "x", " corresponds 
     the operator ", TT "dx", " that differentiates with respect to that 
     variable.  The evident commutation relation takes the form 
     ", TT "dx*x == x*dx + 1", ".",
     PARA{},
     "We can give any names we like to the variables in a Weyl algebra, provided
     we specify the correspondence between the variables and the derivatives,
     with the ", TO "WeylAlgebra", " option, as follows.",
     PARA{},
     EXAMPLE {
	  "R = QQ[x,y,dx,dy,t,WeylAlgebra => {x=>dx, y=>dy}]",
	  "dx*dy*x*y",
	  "dx*x^5"
	  },
     "All modules over Weyl algebras are, in Macaulay2, right modules.  This means that 
     multiplication of matrices is from the opposite side:",
     EXAMPLE {
	  "dx*x",
	  "matrix{{dx}} * matrix{{x}}"
	  },
     "All Gröbner basis and related computations work over this ring.  For an extensive
     collection of D-module routines (A D-module is a module over a Weyl algebra), see ",
     TO "Dmodules::Dmodules", "."
     }

document {
     Key => "associative algebras",
     "Eventually we will implement associative algebras, not necessarily
     commutative."
     }

///
     "An element of the coefficient ring can be promoted to the polynomial ring.",
     EXAMPLE "promote(11/2,R)",
     "Conversely, an element of the polynomial ring that is known to be a scalar
     can be lifted back to the coefficient ring.",
     EXAMPLE {
	  "sc = (a-2)^2-a^2+4*a",
      	  "lift(sc,QQ)",
	  },
     "In programs, the function ", TO "liftable", " can be used to see whether
     this is possible.",
     EXAMPLE {
	  "liftable(sc,QQ)",
      	  "liftable(c^3,QQ)",
	  },

     "The Hilbert series of a polynomial ring can be obtained.  Its power
     series expansion is the generating function for the dimensions of the
     degree ", TT "n", " parts.",
     EXAMPLE "hilbertSeries R",
     "We may use the option ", TO "Degrees", " to produce rings where the
     generators have degrees other than 1.",
     EXAMPLE {
	  "S = ZZ/101[a,b,c,d,Degrees=>{1,2,3,4}]",
      	  "random(5,S)",
      	  "hilbertSeries S"
	  },
     SeeAlso => { "monomial orderings",  "PolynomialRing"}
///



-------------------
-- GB nodes -------
-------------------

-*
-- Mike wanted this: 
document {
     Key => "Hilbert functions",
     }

document {
     Key => "syzygies",
     }

document {
     Key => "saturation",
     }

document {
     Key => "fibers of a map between varieties",
     }

document {
     Key => "solving systems of polynomial equations",
     }
*-

/// 
Plan for the next node:
-- groebner basis object
-- getting it, 'snapshot'
-- information about a GB computation:
--   verbose
--   summary
-- using the Hilbert function
-- computing only partial gbs
-- tricks which might help
--   change monomial order
--   homogenize
--   compact monomials
--   remove linear equations, and the corresponding variable.
--   computing up to a given degree
///

///
-- document these routines DO THIS
-- schreyerMatrix F -- DO THIS

-*
leadTerm(ZZ,RingElement) := (n,f) -> (leadTerm(n,matrix{{f}}))_(0,0)
  -- leadTerm should call a ggleadterm routine?  DO THIS
*-
     
-*
installHilbertFunction = method()
installHilbertFunction(Module,RingElement) := (M,hf) -> (
     -- we need to place hf into the degree ring of M.
     hf = substitute(hf,degreesRing M);
     M.cache.poincare = hf;
     )
*-

-*
installGroebner = method()
-- DO THIS
*-

-*
gbRemove = method()
gbRemove Module := (M) -> remove((generators M).cache, {false,0})
gbRemove Ideal := (I) -> remove((generators I).cache, {false,0})
*-
  -- PROBLEM: what about the other GB
  
R = QQ[a..d,Weights=>{-1,0,0,0},Global=>false]
f = a+b^2+c^3-2*d^4+1+a*b*c*d
leadTerm f
leadCoefficient f
leadTerm(1,f)

M = image vars R
gbSnapshot(M)
gb(M,PairLimit=>2)
m1 = gbSnapshot(M)
m1s = toString m1
gb(M,PairLimit=>4)
m1  -- This has changed!  We probably don't want that
assert( toString m1 == m1s )				    -- it seems okay, now [dan]

///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
