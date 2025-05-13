undocumented {
    (symbol -, ZZ, ProjectiveHilbertPolynomial),
    (symbol -, ProjectiveHilbertPolynomial),
    (symbol -, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial),
    (symbol -, ProjectiveHilbertPolynomial, ZZ),
}

document {
    Key => minus,
    Headline => "additive inverse",
    Usage => "minus(x)",
    TT "minus(x)", " yields ", TT "-x", ".",
    PARA{},
    "See also ", TO "difference", "."
}

document {
    Key => difference,
    Headline => "difference",
    Usage => "difference(x,y)",
    TT "difference(x,y)", " returns ", TT "x-y", "."
}

document {
    Key => {
	 symbol -,
	(symbol -, ZZ),
	(symbol -, ZZ, CC),
	(symbol -, ZZ, QQ),
	(symbol -, ZZ, RR),
	(symbol -, ZZ, RRi),
	(symbol -, ZZ, ZZ),
	(symbol -, QQ),
	(symbol -, QQ, CC),
	(symbol -, QQ, QQ),
	(symbol -, QQ, RR),
	(symbol -, QQ, RRi),
	(symbol -, QQ, ZZ),
	(symbol -, RR),
	(symbol -, RR, CC),
	(symbol -, RR, QQ),
	(symbol -, RR, RR),
	(symbol -, RR, RRi),
	(symbol -, RR, ZZ),
	(symbol -, RRi),
	(symbol -, RRi, QQ),
	(symbol -, RRi, RR),
	(symbol -, RRi, RRi),
	(symbol -, RRi, ZZ),
	(symbol -, CC),
	(symbol -, CC, CC),
	(symbol -, CC, InfiniteNumber),
	(symbol -, CC, QQ),
	(symbol -, CC, RR),
	(symbol -, CC, ZZ),
	(symbol -, Constant),
	(symbol -, Constant, Constant),
	(symbol -, Constant, InexactNumber),
	(symbol -, Constant, Number),
	(symbol -, Constant, RingElement),
	(symbol -, InexactNumber, Constant),
	(symbol -, InexactNumber, RingElement),
	(symbol -, InfiniteNumber),
	(symbol -, InfiniteNumber, CC),
	(symbol -, InfiniteNumber, InfiniteNumber),
	(symbol -, InfiniteNumber, Number),
	(symbol -, Matrix),
	(symbol -, Matrix, Matrix),
	(symbol -, Matrix, Number),
	(symbol -, Matrix, RingElement),
	(symbol -, MutableMatrix),
	(symbol -, MutableMatrix, MutableMatrix),
	(symbol -, Number, Constant),
	(symbol -, Number, InfiniteNumber),
	(symbol -, Number, Matrix),
	(symbol -, Number, RingElement),
	(symbol -, Number, Vector),
	(symbol -, RingElement),
	(symbol -, RingElement, Constant),
	(symbol -, RingElement, InexactNumber),
	(symbol -, RingElement, Matrix),
	(symbol -, RingElement, Number),
	(symbol -, RingElement, RingElement),
	(symbol -, RingElement, Vector),
	(symbol -, Tally),
	(symbol -, List),
	(symbol -, Vector),
	(symbol -, Vector, Number),
	(symbol -, Vector, RingElement),
	(symbol -, Vector, Vector),
    },
     Headline => "a unary or binary operator, usually used for negation or subtraction",
     Usage => "-y \n x-y",
     "In most cases, this operator refers to standard negation or subtraction.",
     PARA{},
     "In many cases, the integer 1 can be used as the identity, and scalars function as multiples
     of the identity.
     For example, the 1 below refers to the identity matrix
     and the 2 to twice the identity matrix.",
     EXAMPLE lines ///
     	  M = matrix{{1,2,3},{2,3,4},{3,4,6}}
	  M-1, M-2
     ///,
     HEADER3 "Intervals",
     PARA { "If one of the inputs is an ", TO "RRi", ", the output is an interval containing all differences of pairs in the inputs." },
     EXAMPLE {
          "2-interval(1,3)",
          "interval(1,3)-interval(-1,2)",
          "interval(-1,1)-interval(-1,1)"
     },
     SeeAlso =>{ "difference", "minus"}
     }

document {
     Key => (symbol -, List, List),
     Headline => "difference of two vectors",
     Usage => "v-w",
     Inputs => { "v" => "a list interpreted as a vector", "w" => "a list interpreted as a vector" },
     Outputs => {"the difference of the two vectors"},
     EXAMPLE "{1,2,3} - {1,5,6}"
     }
