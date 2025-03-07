-- -*- coding: utf-8 -*-
--		Copyright 1993-1999 by Daniel R. Grayson

document {
     Key => identity,
     Headline => "the identity function",
     TT "identity x", " -- returns x.",
     PARA{},
     "This is the identity function."
     }

document {
     Key => symbol &,
     Headline => "a binary operator",
     }

document {
     Key => (symbol &, ZZ, ZZ),
     Headline => "logical and",
     Usage => "m & n",
     Inputs => {"m", "n"},
     Outputs => {
	  ZZ => {"obtained from the bits of the 
     	       integers ", TT "m", " and ", TT "n", " by logical 'and'."}
	  },
     EXAMPLE "(2^15 + 2^13 + 2^42) & (2^15 + 2^23 + 2^42) == 2^15 + 2^42",
     SeeAlso => {(symbol |,ZZ,ZZ),(symbol ^^,ZZ,ZZ), (symbol ~, ZZ)}
     }

document {
     Key => {symbol %,
	  (symbol %, CC, CC),
	  (symbol %, CC, QQ),
	  (symbol %, CC, RR),
	  (symbol %, CC, ZZ),
	  (symbol %, Number, RingElement),
	  (symbol %, QQ, QQ),
	  (symbol %, QQ, ZZ),
	  (symbol %, RingElement, Number),
	  (symbol %, RR, QQ),
	  (symbol %, RR, RR),
	  (symbol %, RR, ZZ),
	  (symbol %, Number, GroebnerBasis),
	  (symbol %, Number, Ideal),
	  (symbol %, ZZ, MonomialIdeal),
	  (symbol %, ZZ, ZZ)
	  },	  
     Headline => "a binary operator, usually used for remainder and reduction",
     Usage => "x % y",
     "The usual meaning for this operator is remainder, or normal form with respect
     to a Gröbner basis.",
     PARA{},
     "For integers, the remainder is non-negative.",
     EXAMPLE lines ///
       1232132141242345 % 1000000
       (-4)%5
       ///,
     PARA{},
     "In polynomial rings, the division algorithm is used.",
     EXAMPLE lines ///
       A = ZZ[a,b]
       (3*a^3-a*b-4) % (5*a-b)
       pseudoRemainder(3*a^3-a*b-4, 5*a-b)
       B = QQ[a,b]
       (3*a^3-a*b-4) % (5*a-b)
     ///,
     "In more complicated situations, Gröbner bases are usually needed.  See ",
     TO "methods for normal forms and remainder", ".",
     SeeAlso => { remainder, remainder', pseudoRemainder, "//"}
     }

document {
     Key => {(symbol |, Matrix, Matrix),
	  (symbol |, RingElement, Matrix),
	  (symbol |, Matrix, RingElement),
	  (symbol |, RingElement, RingElement),
	  (symbol |, Number, Matrix),
	  (symbol |, Matrix, Number)
	  },
     Headline => "join matrices horizontally",
	Usage => "f = g | h",
	Inputs => {
		"g" => {"a ", TT "m", " by ", TT "n", " matrix"},
		"h" => {"a ", TT "m", " by ", TT "r", " matrix"}
		},
	Outputs => {
		"f" => {"the ", TT "m", " by ", TT "n+r", " matrix,
		     obtained from matrices ", TT "g", " and ", TT "h", " by
     		     concatenating the rows"}
	        },
     EXAMPLE lines ///
	  R = ZZ[i..p];
      	  g = matrix {{i,j},{k,l}}
      	  h = matrix {{m,n},{o,p}}
      	  f= g | h
	  ///,
     "If one of the arguments is a ring element or a number, then it
     will be multiplied by a suitable identity matrix.",
	EXAMPLE "f | (m-n)",
	Caveat => {"It is assumed that the matrices ", TT "g", " and ", TT "h", " have the same ", TO Ring, "."},
     SeeAlso =>{(symbol ||, Matrix, Matrix), (ring, Matrix)}
     }

document {
     Key => (symbol ||, Net, Net),
     Headline => "join nets or strings vertically",
     TT "m||n", " -- joins nets or strings by concatenating
     them vertically.  The baseline of the result is the baseline of the
     first one.",
     PARA{},
     "In this example, we build a large net with arrows to indicate
     the location of the baseline.",
     EXAMPLE {
	  ///x = "x" | "3"^1///,
      	  ///"<--- " | ( x || "" || x ) | " --->"///,
	  },
     SeeAlso => {"stack"}
     }

document {
     Key => {(symbol ||, Matrix, Matrix),
	  (symbol ||, RingElement, Matrix),
	  (symbol ||, Matrix, RingElement),
	  (symbol ||, RingElement, RingElement),
	  (symbol ||, Matrix, Number),
	  (symbol ||, Number, Matrix)
	  },
     Headline => "join matrices vertically",
	Usage => "f = g || h",
	Inputs => {
		"g" => {"a ", TT "m", " by ", TT "n", " matrix."},
		"h" => {"a ", TT "r", " by ", TT "n", " matrix."}
		},
	Outputs => {
		"f" => {"the ", TT "m+r", " by ", TT "n", " matrix,
		     obtained from matrices ", TT "g", " and ", TT "h", " by
     		     concatenating the columns."}
		},
     EXAMPLE lines ///
	  R = ZZ[i..p];
      	  g = matrix {{i,j},{k,l}}
      	  h = matrix {{m,n},{o,p}}
      	  f= g || h
	  ///,
     "If one of the arguments is a ring element or a number, then it
     will be multiplied by a suitable identity matrix.",
	EXAMPLE "f || 33",
	Caveat => {"It is assumed that the matrices ", TT "g", " and ", TT "h", " have the same ", TO Ring, "."},
     SeeAlso =>{(symbol |, Matrix, Matrix), (ring, Matrix)}
     }


document {
     Key => (symbol ||, Vector, Vector),
     Headline => "join Vectors ",
     Usage => "v || w",
	Inputs => {"v", "w"},
	Outputs => {
		Vector => {"obtained from vectors v and w by concatenating the columns."}
                   },
     EXAMPLE lines ///
	      R = (ZZ[x,y,z])^3;
      	  v = vector {1,x,x*y,x*z,x*y*z}
      	  w = vector {z*x,z^2,3}
      	  v || w
	  ///,
     PARA{},
     SeeAlso => {(symbol ||, Matrix, Matrix)}
     }

undocumented {
    (symbol**, QuotientRing, PolynomialRing),
    (symbol**, QuotientRing, QuotientRing),
    (symbol**, Number, Matrix),
    (symbol**, Matrix, Number),
    (symbol **,Number,RingElement),
    (symbol **,RingElement,Matrix),
    (symbol **,RingElement,Number),
    (symbol **,RingElement,RingElement),
    (symbol **,Thing,InexactFieldFamily),
    (symbol**, PolynomialRing, PolynomialRing),
    (symbol**, PolynomialRing, QuotientRing),
     }

document {
     Key => {symbol** },
     Headline => "a binary operator, usually used for tensor product or Cartesian product",
     SeeAlso => {symbol ^**}
     }

document {
     Key => symbol ^**,
     Headline => "a binary operator, usually used for tensor or Cartesian power",
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
