undocumented {
    (symbol /, Constant, Constant),
    (symbol /, Constant, InexactNumber),
    (symbol /, Constant, Number),
    (symbol /, Constant, RingElement),
    (symbol /, InexactNumber, Constant),
    (symbol /, InexactNumber, RingElement),
    (symbol /, InfiniteNumber, CC),
    (symbol /, InfiniteNumber, InfiniteNumber),
    (symbol /, InfiniteNumber, Number),
    (symbol /, InfiniteNumber, RR),
    (symbol /, MonoidElement, MonoidElement),
    (symbol /, Number, Constant),
    (symbol /, Number, InfiniteNumber),
    (symbol /, Number, RingElement),
    (symbol /, RingElement, Constant),
    (symbol /, RingElement, InexactNumber),
    (symbol /, RingElement, Number),
    --
    (symbol //, InexactNumber, RingElement),
    (symbol //, RingElement, InexactNumber),
    (symbol //, InfiniteNumber, RR),
    (symbol //, InfiniteNumber, CC),
    (symbol //, InfiniteNumber, Number),
    (symbol //, InfiniteNumber, InfiniteNumber),
}

document {
    Key => {
	 symbol /,
	(symbol /, CC, CC),
	(symbol /, CC, QQ),
	(symbol /, CC, RR),
	(symbol /, CC, ZZ),
	(symbol /, QQ, CC),
	(symbol /, QQ, QQ),
	(symbol /, QQ, RR),
	(symbol /, QQ, ZZ),
	(symbol /, RR, CC),
	(symbol /, RR, QQ),
	(symbol /, RR, RR),
	(symbol /, RR, ZZ),
	(symbol /, ZZ, CC),
	(symbol /, ZZ, QQ),
	(symbol /, ZZ, RR),
	(symbol /, ZZ, ZZ),
	(symbol /, QQ, RRi),
	(symbol /, RR, RRi),
	(symbol /, RRi, QQ),
	(symbol /, RRi, RR),
	(symbol /, RRi, RRi),
	(symbol /, RRi, ZZ),
	(symbol /, ZZ, RRi)
    },
     Headline => "a binary operator, usually used for division",
     Usage => "x / y",
     "This operator is currently used in several ways in Macaulay2.",
     UL {
	  "division in a ring, yielding a fraction",
     	  "division in unevaluated expressions",
	  "quotient rings, modules and sheaves",
	  "applying a function or ring map to every element of a list or set"
	  },
     EXAMPLE lines ///
     2/3
     2./3
     ///,
     HEADER3 "Intervals",
     PARA { "If one of the inputs is an ", TO "RRi", ", the output is an interval containing all quotients of pairs in the inputs." },
     EXAMPLE {
         "2/interval(1,3)",
         "interval(-1,2)/interval(1,3)",
         "interval(1,2)/interval(1,2)"
     },
     SeeAlso => { "//"}
     }

document {
    Key => {
	 symbol //,
	(symbol //, ZZ, ZZ),
	(symbol //, CC, CC),
	(symbol //, CC, QQ),
	(symbol //, CC, RR),
	(symbol //, CC, ZZ),
	(symbol //, QQ, QQ),
	(symbol //, QQ, ZZ),
	(symbol //, RR, QQ),
	(symbol //, RR, RR),
	(symbol //, RR, ZZ),
	(symbol //, Number, InfiniteNumber),
	(symbol //, Number, RingElement),
	(symbol //, RingElement, Number),
	(symbol //, RingElement, RingElement),
	(symbol //, List, Number),
	(symbol //, List, RingElement),
	(symbol //, ZZ, MonomialIdeal)
    },
    Headline => "a binary operator, usually used for quotient",
    Usage => "x // y",
    "For ring elements in the integers, polynomial rings, and other rings,
    there are two types of division:  exact division, whose result is often in a larger
    field, such as the rationals or a function field, and division with remainder,
    whose result is in the same ring.  In Macaulay2, '/' denotes the first kind of division, while
    '//' denotes the latter kind.
    The following example shows
    the difference between ", TO "//", " and ", TO "/", ".",
    EXAMPLE lines ///
     	  4/2
	  4//2
    ///,
    EXAMPLE lines ///
     	  R = QQ[x];
	  (x^2-3)//(x-1)
	  (x^2-3)%(x-1)
	  (x^2-3)/(x-1)
    ///,
    SeeAlso => { "/", "%" }
    }

doc ///
  Key
    (symbol /, Matrix, Number)
    (symbol /, Matrix, RingElement)
    (symbol /, Vector, Number)
    (symbol /, Vector, RingElement)
    (symbol /, List, RingElement)
    (symbol /, List, Number)
  Headline
    scalar division
  Usage
    v / c
  Inputs
    v:{Matrix, Vector, List}
    c:{Number, RingElement}
  Outputs
    :{Matrix, Vector, List}
  Description
    Text
      This operation is equivalent to right scalar multiplication by the
      multiplicative inverse of @CODE "c"@, i.e., @CODE "v * (1/c)"@.
    Example
      R = QQ[a,b,c,d]
      matrix {{d, -b}, {-c, a}} / (a * d - b * c)
      vector gens R / 3
      {1,2,3,4} / 3
  Caveat
    The base ring of the output will be a field containing the base ring of @CODE "v"@.
  SeeAlso
    symbol //
///

document {
     Key => (symbol /, RingElement, RingElement),
     Headline => "fraction",
     Usage => "f/g",
     Inputs => { "f", "g" },
     Outputs => { RingElement => "the fraction f/g" },
     "If either f or g is in a base ring of the other, then that one is promoted
     so that both are elements in the same ring R.",
     PARA{},
     "The fraction will be an element of the fraction field, frac R, of R.
     If R is already a field, then this means that the fraction will be an element
     of R.",
     EXAMPLE lines ///
      	   4/2
	   ///,
     EXAMPLE lines ///
	   R = GF(9,Variable=>a);
	   (a/a^3) * a^2 == 1
	   ///,
     EXAMPLE lines ///
     	   S = ZZ[a,b]
	   (a^6-b^6)/(a^9-b^9)
	   ///,
     "If the ring contains zero divisors, the fraction field is not defined.
     Macaulay2 will not inform you of this right away.  However, if computation
     finds a zero-divisor, an error message is generated.",
     EXAMPLE lines ///
     	  A = ZZ/101[a,b]/(a*b)
	  (a+b)/(a-b)
	  ///,
     "At this point, if one types ", TT "a/b", ", then Macaulay2 would give an error
     saying that a zero divisor was found in the denominator.",
     SeeAlso => {symbol //}
     }
