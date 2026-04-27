-- -*- coding: utf-8 -*-
----------- File Mike is Working on! -------------------------
document {
     Key => "rings",

     "Macaulay2 differs from other computer algebra systems such as 
     Maple and Mathematica, in that before making a polynomial, 
     you must create a ring to contain it, deciding first
     the complete list of indeterminates and the type of coefficients 
     permitted.  Recall that a ring is a set with addition and multiplication operations 
     satisfying familiar axioms, such as the distributive rule.  
     Examples include the ring of integers (", TO "ZZ", "), the
     ring of rational numbers (", TO "QQ", "), and the most 
     important rings in Macaulay2, polynomial rings.",

     PARA{},
     "The sections below describe the types of rings available and how to use them.",
     Subnodes => {
	  "Rings",
	  TO "basic rings of numbers",
	  TO "integers modulo a prime",
	  TO "finite fields",
	  TO "polynomial rings",
	  TO "quotient rings",
	  TO "manipulating polynomials",
	  TO "factoring polynomials",
	  TO "substitution and maps between rings",
	  "Fields",
	  TO "fraction fields",
	  TO "finite field extensions",
	  "Other algebras",
	  TO "exterior algebras",
	  TO "symmetric algebras",
	  TO "tensor products of rings",
	  TO "Weyl algebras",
	  TO "local rings",
	  TO "Schur rings",
	  TO "associative algebras",
       	  },
     PARA{},
     "For additional common operations and a comprehensive list of all routines
     in Macaulay2 which return or use rings, see ", TO "Ring", "."
     }

document {
     Key => "basic rings of numbers",
     "The following rings are initially present in every session with
     Macaulay2.",
     UL { 
	  TO "ZZ", 
	  TO "QQ", 
	  TO "RR",
	  TO "RRi",
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
     "Integers may be entered in bases 2, 8, or 16 using particular prefixes.",
     EXAMPLE {
	  "0b10011010010 -- binary",
	  "0o2322 -- octal",
	  "0x4d2 -- hexadecimal",
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
     ///,
     Subnodes => { 
	  TO "ZZ", 
	  TO "QQ", 
	  TO "RR",
	  TO "RRi",
	  TO "CC",
     }
}

document {
    Key => Number,
    Headline => "the class of all numbers",
    Subnodes => {
	TO isANumber,
	TO InfiniteNumber,
	TO IndeterminateNumber,
	TO InexactNumber,
	TO InexactNumber'
    }}

document {
    Key => InfiniteNumber,
    Headline => "the class of all infinite numbers",
    Subnodes => { TO isFinite, TO isInfinite, TO infinity } }

document {
    Key => infinity,
    Headline => "infinity" }

document {
    Key => IndeterminateNumber,
    Headline => "the class of all indeterminate numbers",
    "Indeterminate numbers result, for example, from multiplying 0 by infinity.
    There is only one instance of this class.",
    Subnodes => { TO indeterminate } }

document {
    Key => indeterminate,
    Headline => "an indeterminate number",
    TT "indeterminate", " -- a representation of an indeterminate number, ",
    "such as might result from multiplying 0 by infinity." }

document {
    Key => InexactNumber,
    "This type of number is intended to serve as a parent class for those types of numbers ",
    "that are inexactly represented in the computer.",
    Subnodes => {
	TO precision,
	TO numeric,
	TO round,
	TO size2,
	TO clean,
	TO norm,
	TO "minExponent",
	TO "maxExponent",
        },
    }

document {
    Key => InexactNumber',
    "This class is the common parent of the classes of complex fields and real fields.",
    Subnodes => {
	TO "RR_*",
	TO "CC_*",
	TO (symbol _*, RingFamily),
        },
    }

document {
     Key => ZZ,
     Headline => "the class of all integers",
    Subnodes => {
	-- TO (factor, ZZ),
	TO zero,
	TO even,
	TO odd,
	TO (isPrime, ZZ),
	TO (isPseudoprime, ZZ),
	TO nextPrime,
	TO changeBase,
        },
    }

document {
     Key => QQ,
     Headline => "the class of all rational numbers",
     EXAMPLE "1/2 + 3/5"}

document {
     Key => RR,
     Headline => "the class of all real numbers",
     "A real number is entered as a sequence of decimal digits with a point.  It is stored internally
     as an arbitrary precision floating point number, using the ", TO "MPFR", " library.",
     EXAMPLE "3.14159",
     "The precision is measured in bits, is visible in the ring displayed on
     the second of each pair of output lines, and can be recovered using ", TO "precision", ".",
     EXAMPLE "precision 3.14159",
     "For real numbers, the functions ", TO "class", " and ", TO "ring", " yield different
     results.  That allows numbers of various precisions
     to be used without creating a new ring for each precision.",
     EXAMPLE {"class 3.1", "ring 3.1"},
     "The precision can be specified on input by appending the letter ", TT "p", " and a positive number.",
     EXAMPLE "3p300",
     "An optional exponent (for the power of ten to multiply by) can be specified on input
     by appending the letter ", TT "e", " and a number.",
     EXAMPLE {"3e3", "-3e-3", "-3p111e-3"},
     "Numbers that appear alone on an output line are displayed with all their meaningful digits.
     (Specifying 100 bits of precision yields about 30 decimal digits of precision.)",
     EXAMPLE {"1/3.","1/3p100", "100 * log(10,2)"},
     "Numbers displayed inside more complicated objects are printed with the number of digits
     specified by ", TO "printingPrecision", ".",
     EXAMPLE {"printingPrecision","{1/3.,1/3p100}"},
     "The notion of equality tested by ", TO "==", " amounts to equality of the internal binary digits.",
     EXAMPLE {".5p100 == .5p30", ".2p100 == .2p30"},
     "The notion of (strict) equality tested by ", TO "===", " also takes the precision into account.",
     EXAMPLE {".5p100 === .5p30", ".2p100 === .2p30"},
     "Perhaps surprisingly, the IEEE floating point standard also specifies that every number, including 0,
     has a sign bit, and strict equality testing takes it into account, as it must do, because some arithmetic
     and transcendental functions take it into account.",
     EXAMPLE lines ///
     0.
     -0.
     1/0.
     1/-0.
     log 0
     csc (0.)
     csc (-0.)
     ///,
     "Use ", TO "toExternalString", " to produce something that, when encountered as input, will reproduce
     exactly what you had before.",
     EXAMPLE lines ///
	  x = {1/3.,1/3p100}
	  x == {.333333, .333333}
	  y = toExternalString x
	  x === value y
     ///,
     "Transcendental constants and functions are available to high precision, with ", TO "numeric", ".",
     EXAMPLE lines ///
	  numeric pi
	  numeric_200 pi
	  Gamma oo
	  ///,
    SeeAlso => {numeric, precision, format, "printingPrecision", "printingAccuracy",
	  "printingLeadLimit", "printingTrailLimit", "printingSeparator",
	  "maxExponent", "minExponent"
	  },
    Subnodes => {
	TO toRR,
	TO isReal,
        },
     }

document {
    Key => toRR,
    Headline => "convert to high-precision real number",
    Usage => "toRR(prec,x)",
    Inputs => {
	"prec" => ZZ => {"the number of bits of precision desired"},
	"x" => {ofClass{RR,ZZ,QQ}}
    },
    Outputs => {RR => {"the result of converting ", TT "x", " to a high-precision real number"}},
    EXAMPLE lines ///
        toRR(200,1/7)
	precision oo
    ///
}

document {
     Key => "RR_*",
     Headline => "the parent class of all rings of real numbers",
     PARA {
	  "Floating point real numbers are treated in a special way.  Recall first to create a polynomial, one must
	  first create a polynomial ring to contain it.  And then, the polynomial ring is the class of the polynomial."
	  },
     EXAMPLE lines ///
     R = QQ[x,y,z]
     x^2
     class x^2
     ///,
     PARA {
	  "Floating point real numbers, however, can be created directly, as follows, without creating a ring."
	  },
     EXAMPLE lines ///
     r = 4.5
     s = 4.3p300
     ///,
     PARA {
	  "The floating point numbers created above have different precisions, and thus are regarded as being elements of 
	  different rings, whose elements all have the same precision."
	  },
     EXAMPLE lines ///
     precision r
     precision s
     ring r
     ring s
     ///,
     PARA {
	  "In order to make it convenient to define methods that apply to all such rings, those rings have a common
	  parent, namely ", TT "RR'", ".  Notice that ", TT "RR'", " is printed in a special way."
	  },
     EXAMPLE lines ///
     RR'
     parent ring r     
     parent ring s
     parent ring s === RR'
     ///
     }

document {
     Key => CC,
     Headline => "the class of all complex numbers",
     "In Macaulay2, complex numbers are represented as floating point numbers, and so are
     only approximate.  The symbol ", TO "ii", " represents the square root of -1 in many numeric
     contexts.  A complex number is obtained by using the symbolic constant ", TO "ii", " or the conversion
     functions ", TO "toCC", " and ", TO "numeric", ", in combination with real numbers (see ", TO "RR", ").
     It is stored internally as a pair of arbitrary precision floating point real numbers, using
     the ", TO "MPFR", " library.",
     EXAMPLE {
	  "z = 3-4*ii",
	  "z^5",
	  "1/z",
	  "+ii",
	  "numeric_200 ii",
	  },
     "Complex numbers are ordered lexicographically, mingled with real numbers.",
     EXAMPLE {
	  "sort {1+ii,2+ii,1-ii,2-ii,1/2,2.1,7/5}"
	  },
     "The precision is measured in bits, is visible in the ring displayed on
     the second of each pair of output lines, and can be recovered using ", TO "precision", ".",
     EXAMPLE "precision z",
     "For complex numbers, the functions ", TO "class", " and ", TO "ring", " yield different
     results.  That allows numbers of various precisions
     to be used without creating a new ring for each precision.",
     EXAMPLE {"class z", "ring z"},
     "A computation involving numbers of different precisions has a result with the minimal precision occurring.
     Numbers that appear alone on an output line are displayed with all their meaningful digits.
     (Specifying 100 bits of precision yields about 30 decimal digits of precision.)",
     EXAMPLE "3p100+2p90e3*ii",
     "Numbers displayed inside more complicated objects are printed with the number of digits
     specified by ", TO "printingPrecision", ".",
     EXAMPLE {"printingPrecision","x = {1/3.*ii,1/3p100*ii}"},
     "Use ", TO "toExternalString", " to produce something that, when encountered as input, will reproduce
     exactly what you had before.",
     EXAMPLE lines ///
	  y = toExternalString x
	  value y === x
     ///,
     Caveat => { "Currently, most transcendental functions are not implemented for complex arguments." },
    SeeAlso => {"ii", toRR, numeric, precision, format, "printingPrecision", "printingAccuracy", "printingLeadLimit", "printingTrailLimit", "printingSeparator"},
    Subnodes => {
	TO toCC,
	TO conjugate,
	TO realPart,
	TO imaginaryPart,
        },
     }

document {
    Key => {toCC,
	(toCC, ZZ, ZZ), (toCC, ZZ, QQ), (toCC, ZZ, RR), (toCC, ZZ, CC),
	(toCC, RR, RR), (toCC, ZZ, ZZ, ZZ), (toCC, ZZ, ZZ, QQ),
	(toCC, ZZ, QQ, ZZ), (toCC, ZZ), (toCC, ZZ, QQ, QQ), (toCC, QQ),
	(toCC, ZZ, RR, ZZ), (toCC, ZZ, ZZ, RR), (toCC, ZZ, RR, QQ),
	(toCC, ZZ, QQ, RR), (toCC, RR), (toCC, CC), (toCC, ZZ, RR, RR)
    },
    Headline => "convert to high-precision complex number",
    SYNOPSIS (
	Usage => "toCC(prec,x,y)\ntoCC(prec,x)",
	Inputs => {
	    "prec" => ZZ => {"the number of bits of precision desired"},
	    "x" => {ofClass{ZZ,QQ,RR}},
	    "y" => {ofClass{ZZ,QQ,RR}}
	},
	Outputs => {CC => {"the complex number with real part ", TT "x", " and complex part ", TT "y", ".  If
		", TT "y", " is omitted, the imaginary part is zero."}},
        EXAMPLE lines ///
	    toCC(200,7)
	    toCC(100,7,3.)
	///
    ),
    SYNOPSIS (
	Usage => "toCC(x,y)\ntoCC x",
	Inputs => { "x" => RR, "y" => RR },
	Outputs => {CC => {"the complex number with real part ", TT "x", " and complex part ", TT "y", ".  If
		", TT "y", " is omitted, the imaginary part is zero.  The precision of the result is
		the minimum precision of the arguments."}},
	EXAMPLE lines ///
	    toCC(3.,4.)
	    toCC(3.p100,4.p200)
	///
    )
}

document {
     Key => "CC_*",
     Headline => "the parent class of all rings of complex numbers",
     PARA {
	  "Floating point complex numbers are treated in a special way.  Recall first to create a polynomial, one must
	  first create a polynomial ring to contain it.  And then, the polynomial ring is the class of the polynomial."
	  },
     EXAMPLE lines ///
     R = QQ[x,y,z]
     x^2
     class x^2
     ///,
     PARA {
	  "Floating point complex numbers, however, can be created directly, as follows, without creating a ring."
	  },
     EXAMPLE lines ///
     r = 4.5 * ii
     s = 4.3p300 * ii
     ///,
     PARA {
	  "The floating point numbers created above have different precisions, and thus are regarded as being elements of 
	  different rings, whose elements all have the same precision."
	  },
     EXAMPLE lines ///
     precision r
     precision s
     ring r
     ring s
     ///,
     PARA {
	  "In order to make it convenient to define methods that apply to all such rings, those rings have a common
	  parent, namely ", TT "CC'", ".  Notice that ", TT "CC'", " is printed in a special way."
	  },
     EXAMPLE lines ///
     CC'
     parent ring r     
     parent ring s
     parent ring s === CC'
     ///
     }

undocumented {"RRi_*"}

document {
     Key => RRi,
     Headline => "the class of all real intervals",
     "A real interval is entered as a pair of real numbers to the interval function.  It is stored internally as an arbitrary precision interval using the ", TO "MPFI", " library.",
     EXAMPLE "interval(3.1415,3.1416)",
     "The precision is measured in bits, is visible in the ring displayed on
     the second of each pair of output lines, and can be recovered using ", TO "precision", ".",
     EXAMPLE "precision interval(3.1415,3.1416)",
     "For real intervals, the functions ", TO "class", " and ", TO "ring", " yield different
     results.  That allows numbers of various precisions
     to be used without creating a new ring for each precision.",
     EXAMPLE {"class interval(3.1,3.5)", "ring interval(3.1,3.5)"},
     "The precision can be specified on input by specifying the precision of both input ", TO "RR", " numbers.",
     "Alternatively, the precision can be specified by including the option ", TT "Precision", ".",
     EXAMPLE {"interval(2.5p100,3.2p1000)","interval(2.5,3.2,Precision=>200)"},
     "Intervals can also be created using ", TO (span,Sequence), " to create the smallest interval containing the inputs.",
     EXAMPLE {"span(2,Precision=>100)","span(2,3,interval(-1.5,-0.5),73)"},
     "Operations using intervals are computed as sets so that the resulting intervals contain all possible outputs from pairs of points in input intervals.",
     EXAMPLE {"interval(1,3)+interval(2,4)","interval(-1,1)*interval(2,3)","interval(0,1)-interval(0,1)","interval(1,2)/interval(1,2)"},
     "The notion of equality tested by ", TO "==", " amounts to checking the equality of the endpoints of intervals.",
     "The notion of equality tested by ", TO "===", " takes into account the precision of the inputs as well.",
     EXAMPLE {"interval(1,3) == interval(1,3,Precision=>100)","interval(1,3) === interval(1,3,Precision=>100)","interval(1/3,1,Precision=>100)==interval(1/3,1,Precision=>1000)"},
     "The notion of inequalities for intervals amounts to testing the inequality for all points in the intervals.  In particular, ",TO "<=", " is not the same as ",TO "<"," or ",TO "==",".",
    EXAMPLE {"interval(1,2)<=interval(2,3)","interval(1,2)<=interval(1,2)", "interval(1,2)<interval(2,3)","interval(1,2)<interval(3,4)"},
     "Transcendental functions on intervals produce intervals containing the image of the function on the interval.",
     EXAMPLE {"exp(interval(2,4))","cos(interval(1,1.3))","sqrt(interval(2))"},
     "Transcendental functions are available to high precision, with ", TO "numericInterval", ".",
    EXAMPLE {"numericInterval(100,pi)","numericInterval_200 EulerConstant"},
    SeeAlso => {toRRi, numericInterval, precision, interval, (span,Sequence), (span,List)},
    Subnodes => {
	TO toRRi,
	TO interval,
	TO diameter,
	TO left,
	TO right,
	TO midpoint,
	TO numericInterval,
        TO (intersect, RRi, RRi),
        TO (isMember, QQ, RRi),
        TO (isEmpty, RRi),
        TO (isSubset, RRi, RRi),
	TO span, -- TODO: perhaps this should be shared
	TO (span, List),
	TO (span, Sequence),
        },
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
	     },
    Subnodes => {
	TO char,
	TO getPrimeWithRootOfUnity,
        },
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
	  TO2 {"GF", "GF(p^n)"}
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
     "The generator of the field is available as the variable ", VAR "a",
     " or it can be obtained as usual.",
     EXAMPLE {
	 "a",
	 "k_0"},
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
     "For more information see ", TO "GaloisField", ".",
     Subnodes => TO isFinitePrimeField,
     }

document {
     Key => "polynomial rings",
     "A polynomial ring can be created with the usual mathematical notation.",
     EXAMPLE "ZZ[x,y,z]",
     "If you try to construct this ring again, you will get a different
     answer.  We use the strict comparison operator ", TO "===", " to
     demonstrate this.",
     EXAMPLE "ZZ[x,y,z]===ZZ[x,y,z]",
     "Thus it is a good idea to assign a new ring to a variable for
     future reference.",
     EXAMPLE "R = QQ[a,b,c,d,e,f]",
     "Notice that after assignment to a global variable, Macaulay2
     knows the ring's name, and this name is used when printing the ring.",
     EXAMPLE "R",
     "The original description of the ring can be recovered
     with ", TO "describe", ".",
     EXAMPLE "describe R",
     "Use the following subscript notation to obtain 0,1, or any multiple of 1,
     as elements in the ring.",
     EXAMPLE {
	  "0_R",
      	  "1_R",
      	  "11_R",
	  },
     "Obtain the variables (generators) of the ring by subscripting the name of 
     the ring.  As always in Macaulay2, indexing starts at 0.",
     EXAMPLE "R_0^10+R_1^3+R_2",
     "It is also possible to obtain the variables in a ring from strings
     containing their names.",
     EXAMPLE ///"a"_R^10+"b"_R^3+"c"_R///,
     "The number of variables is provided by ", TO "numgens", ".",
     EXAMPLE {
	  "numgens R",
      	  "apply(numgens R, i -> R_i^i)",
      	  "sum(numgens R, i -> R_i^i)"
	  },
     "(See ", TO "apply", " and ", TO "sum", ".)  ",
     "Use ", TO "generators", " to obtain a list of the variables of the ring.",
     EXAMPLE "gens R",
     "A matrix (with one row) containing the variables of the ring can be obtained
     using ", TO (vars,Ring), ".",
     EXAMPLE "vars R",
     "The ", TO "index", " of a variable:",
     EXAMPLE {
	  "index x, index y, index z",
	  },
     "The coefficient ring can be recovered with ", TO "coefficientRing", ".",
     EXAMPLE "coefficientRing R",

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
     "A random homogeneous element can be obtained with ", TO "random", ".",
     EXAMPLE "random(2,R)",

     "A basis of the subspace of ring elements of a given degree can be obtained
     in matrix form with ", TO "basis", ".",
     EXAMPLE "basis(2,R)",

     "We may construct polynomial rings over polynomial rings.",
     EXAMPLE "R = ZZ[a,b,c][d,e,f];",
     "When displaying an element of an iterated polynomial ring,
     parentheses are used to organize the coefficients recursively, which
     may themselves be polynomials.",
     EXAMPLE "(a+d+1)^2",
     "Internally, the polynomials in such towers are expressed in terms of a flattened monoid
     containing all the variables, obtainable with the key ", TO "FlatMonoid", ".",

     EXAMPLE "R.FlatMonoid",
     "Variable names may be words.",
     EXAMPLE {
	  "QQ[rho,sigma,tau];",
      	  "(rho - sigma)^2",
	  },
     "There are various other ways to specify the variables in a polynomial
     ring.  A sequence of variables can be obtained as follows.",
     EXAMPLE "ZZ[b..k];",
     "In this example, if you had previously assigned either b or k a value that
     was not a ring generator, then Macaulay2 would complain about this: it would
     no longer understand what variables you wanted.  To get around this, we could
     either do",
     EXAMPLE "ZZ[symbol b .. symbol k];",
     "or we may obtain the single-letter variables with ", TO "vars", ".",
     EXAMPLE {
	  "vars (0..4)",
      	  "ZZ[vars (0..4),vars(26..30),vars 51]",
	  },
     "Subscripted variables can be used, provided the base for the subscripted
     variable has not been used for something else.",
     EXAMPLE "ZZ[t,p_0,p_1,q_0,q_1];",
     "Sequences of subscripted variables can also be used.",
     EXAMPLE {
      	  "ZZ[p_(0,0) .. p_(2,1),q_0..q_5]",
	  "(p_(0,0)+q_2-1)^2",
	  },
     "The subscripts can be much more general, but care is required when using
     symbols as subscripts, for the symbols may acquire values later that would
     interfere with your original use of them as symbols.  Thus you should
     protect symbols that will be used in this way.",
     EXAMPLE {
	  "protect xx; protect yy; protect zz;",
      	  "ZZ[ee_[xx],ee_[yy],ee_[zz]]",
	  },
     "A basis of the subspace of ring elements of a given degree can be obtained
     in matrix form with ", TO "basis", ".",
     EXAMPLE {
	 "R = QQ[a,b,c,d,e,f];",
	 "basis(2,R)"
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
     "Some things to watch out for when using polynomial rings:",
     UL {
	  LI ("Defining a ring twice gives different rings, as far as Macaulay2 is concerned:
     	       We use the strict comparison operator ", TO "===", " to demonstrate this.",     
     	       EXAMPLE "ZZ[a,b,c] === ZZ[a,b,c]",
     	       "Thus it is a good idea to assign a new ring to a variable for future reference."
	       )
       	  },
     SeeAlso => {"heft vectors", "division in polynomial rings with monomials less than 1"},
     Subnodes => {
	 TO isPolynomialRing,
	 TO (symbol SPACE, Ring, Array),
	 TO (symbol SPACE, Ring, Monoid),
	 TO coefficientRing,
	 TO vars,
	 TO(vars, List),
	 TO "get a ring variable by index",
	 TO "get a ring variable by name",
	 TO "get a monomial by exponent vector",
	 TO "graded and multigraded polynomial rings",
	 TO "monomial orderings",
         }
     }

document {
     Key => "graded and multigraded polynomial rings",
     "It is possible to set up a polynomial ring so that the degree of an
     element is a vector of integers.  For this, the option
     ", TO "Degrees", " is used, together with a list of degrees for the
     variables in the ring.  Each degree is itself a list of integers.  The
     degrees given must all be of the same length, and length zero is
     allowed, to get an ungraded ring.",
     EXAMPLE {
	  "R = ZZ/101[a,b,c,Degrees=>{{1,2},{2,1},{1,0}}]",
      	  "describe R",
	  },
     EXAMPLE {
	  "degree a",
      	  "degree b^2",
      	  "degree 0_R",
      	  "degree 1_R",
	  },
     "A random element of bi-degree ", TT "{m,n}", " can be obtained with
     ", TO "random", ".",
     EXAMPLE "random({15,15},R)",
     "The function ", TO "degree", " applied to a polynomial will
     return the least upper bound of the degrees of its monomials.",
     EXAMPLE "degree (a+b)",
     "We may recover the number of integers in each degree list for our ring
     as follows.",
     EXAMPLE {
	  "degreeLength R",
      	  "degreeLength ZZ"
	  },
     "One restriction on degrees of variables is that the entries be small integer values, possibly
     zero or negative.  The notion of small depends on the size of exponents one wants: the degree
     of each monomial occurring should fit in a 32 bit integer (or 64 bit integer, on 64 bit machines).",
     PARA{
	 "Another restriction on degrees, at least if all the computational facilities of Macaulay2 are
	 needed, is that a heft vector exists for them.  A heft vector is a list of integers whose length is
	 the same as the length of the degrees (see ", TO degreeLength, "), such that its dot product with
	 the degree of each variable is positive.  Heft vectors are computed automatically for you,
	 as in the following example, or they may be provided by the user (see ", TO "Heft", ")."
	 },
    EXAMPLE lines ///
	 R = QQ[a,b,c,Degrees=>{{1,0},{-2,1},{-3,1}}];
	 random({1,1},R)
	 basis({1,1},R)
	 ///,
     PARA {
	  "The heft vector computed behind the scenes is available to the user."
	  },
     EXAMPLE lines ///
     (options R).Heft
     ///,
     PARA {
     	  "If the heft vector is not provided, many computations will work (e.g., Gröbner bases and computation of resolutions),
	  but certain other operations (such as ", TT "basis", " and ", TT "random", ") will raise errors."
	  },
     Subnodes => {TO "heft vectors"}
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
	  ///lift("r"_U,T)///,
	  ///substitute("r"_T,U)///,
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
     "For more information see ", TO "QuotientRing", ".",
     Subnodes => {
	 TO (symbol /, Ring, Ideal),
	 TO isQuotientRing,
	 TO isQuotientOf,
         },
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
	  TO "someTerms",
	  TO "part",
	  TO "parts",
	  TO "diff",
	  TO "contract",
	  TO "content",
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
     Subnodes => {
	 TO degree,
	 TO homogenize,
	 TO weightRange,
	 TO monomials,
	 TO coefficient,
	 TO coefficients,
	 TO content,
	 TO exponents,
	 TO index,
	 TO indices,
	 TO part,
	 TO parts,
	 TO someTerms,
	 TO terms,
	 TO topCoefficients,
	 TO leadCoefficient,
	 TO leadMonomial,
	 TO leadTerm,
	 TO size,
	 TO standardForm,
	 TO listForm,
         },
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
	  },
     Subnodes => {
	 TO (roots, RingElement),
         },
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
	  liftable,
	  lift,
	  (kernel,RingMap)
	  },
    Subnodes => {
	TO frac,
	TO fraction,
	TO numerator,
	TO denominator,
        },
     }

document {
     Key => "finite field extensions",
    Subnodes => {
	TO isField,
	TO toField,
	TO getNonUnit,
	TO isPrimitive,
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
     commutative rings.",
     Subnodes => {
	 TO isSkewCommutative,
	 TO antipode,
         },
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
     Key => {
	 "Weyl algebras",
	 isWeylAlgebra,
	(isWeylAlgebra, PolynomialRing),
	(isWeylAlgebra, QuotientRing),
	(isWeylAlgebra, Ring)},
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
     PARA {
	  "All Gröbner basis and related computations work over this ring.  For an extensive
	  collection of D-module routines (A D-module is a module over a Weyl algebra), see ",
	  TO "Dmodules::Dmodules", "."
	  },
     PARA {
	  "The function ", TT "isWeylAlgebra", " can be used to determine whether a polynomial ring has been
	  constructed as a Weyl algebra."
	  },
     EXAMPLE lines ///
     isWeylAlgebra R
     S = QQ[x,y]
     isWeylAlgebra S
     ///
     }

document {
    Key => "associative algebras",
    "Associative (i.e., not necessarily commutative) algebras are implemented in
    the ", TO "AssociativeAlgebras::AssociativeAlgebras", " package."
    }

document {
    Key => "local rings",
    "Localizations of polynomial rings with respect to prime ideals are implemented in
    the ", TO "LocalRings::LocalRings", " package.",
    Subnodes => {
	"alternative ways to construct a local ring",
	TO (symbol SPACE, Ring, List),
        },
    }

document {
    Key => "Schur rings",
    "Representation rings of general linear groups and of symmetric groups are implemented in
    the ", TO "SchurRings::SchurRings", " package."
    }

-*
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
*-



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
