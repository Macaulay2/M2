undocumented {
    (symbol *, Constant, RingElement),
    (symbol *, Ideal, RingElement),
    (symbol *, Ideal, ZZ),
    (symbol *, InexactNumber, RingElement),
    (symbol *, InfiniteNumber, CC),
    (symbol *, InfiniteNumber, InfiniteNumber),
    (symbol *, InfiniteNumber, Matrix),
    (symbol *, InfiniteNumber, QQ),
    (symbol *, InfiniteNumber, RR),
    (symbol *, InfiniteNumber, ZZ),
    (symbol *, Matrix, InfiniteNumber),
    (symbol *, Module, RingElement),
    (symbol *, Module, ZZ),
    (symbol *, MonoidElement, MonoidElement),
    (symbol *, MonoidElement, ZZ),
    (symbol *, MutableMatrix, RR),
    (symbol *, MutableMatrix, RingElement),
    (symbol *, MutableMatrix, ZZ),
    (symbol *, Number, InfiniteNumber),
    (symbol *, Number, RingElement),
    (symbol *, Number, Tally),
    (symbol *, Number, VirtualTally),
    (symbol *, RR, MutableMatrix),
    (symbol *, RingElement, Constant),
    (symbol *, RingElement, InexactNumber),
    (symbol *, RingElement, Number),
    (symbol *, String),
    (symbol *, ZZ, Ideal),
    (symbol *, ZZ, Module),
    (symbol *, ZZ, MonoidElement),
    (symbol *, ZZ, MonomialIdeal),
    (symbol *, ZZ, MutableMatrix),
}

document {
    Key => times,
    Headline => "multiplication",
    Usage => "times(x,y, ...)",
    TT "times(x,y, ...)", " yields the product of its arguments.
    If there are no arguments, the value is the integer 1."
}

document {
     Key => {
	  symbol *,
	 (symbol *, Ring, Ideal),
	 (symbol *, MutableMatrix, MutableMatrix),
	 (symbol *, Ideal, Module),
	 (symbol *, Ring, RingElement),
	 (symbol *, Constant,Constant),
	 (symbol *, Constant,InexactNumber),
	 (symbol *, Constant,Number),
	 (symbol *, InexactNumber,Constant),
	 (symbol *, Matrix,Number),
	 (symbol *, Number,Constant),
	 (symbol *, Number,Matrix),
	 (symbol *, QQ,CC),
	 (symbol *, QQ,QQ),
	 (symbol *, QQ,RR),
	 (symbol *, QQ,ZZ),
	 (symbol *, RR,CC),
	 (symbol *, RR,QQ),
	 (symbol *, RR,RR),
	 (symbol *, RR,ZZ),
	 (symbol *, ZZ,CC),
	 (symbol *, ZZ,QQ),
	 (symbol *, ZZ,RR),
	 (symbol *, ZZ,ZZ),
	 (symbol *, CC,CC),
	 (symbol *, CC,QQ),
	 (symbol *, CC,RR),
	 (symbol *, CC,ZZ),
	 (symbol *, QQ, RRi),
	 (symbol *, RR, RRi),
	 (symbol *, RRi, QQ),
	 (symbol *, RRi, RR),
	 (symbol *, RRi, RRi),
	 (symbol *, RRi, ZZ),
	 (symbol *, ZZ, RRi),
	 (symbol *, RingElement, Matrix),
	 (symbol *, RingMap, RingMap),
	 (symbol *, RingElement, MutableMatrix),
	 (symbol *, Ring, MonomialIdeal),
	 (symbol *, MonomialIdeal, MonomialIdeal),
	 (symbol *, RingElement, Ideal),
	 (symbol *, Matrix, Vector),
	 (symbol *, MonomialIdeal, Ring),
	 (symbol *, Ring, Vector),
	 (symbol *, Ideal, Vector),
	 (symbol *, RingElement, MonomialIdeal),
	 (symbol *, Matrix, RingElement),
	 (symbol *, RingElement, Module),
	 (symbol *, Ideal, Ring),
	 (symbol *, RingElement, RingElement),
	 (symbol *, Thing, List),
	 (symbol *, List, Thing),
	 (symbol *, ZZ, ProjectiveHilbertPolynomial),
	 (symbol *, ProjectiveHilbertPolynomial, ZZ),
	 (symbol *, RingElement, Vector),
	 (symbol *, Number, Vector),
	 (symbol *, Vector, RingElement),
	 (symbol *, Vector, Number)
     },
     Headline => "a binary operator, usually used for multiplication",
     Usage => "x * y",
     "The return type depends on the types of x and y.  If they have the
     same type, then usually the return type is the common type of x and y.",
     PARA{},
     "Multiplication involving ring elements (including integers, rational numbers,
     real and complex numbers), ideals, vectors, matrices, modules is 
     generally the usual multiplication, or composition of functions.",
     PARA{},
     "The intersection of sets is given by multiplication.  See ", TO (symbol *, Set,Set), ".",
     EXAMPLE {
	  "set{hi,you,there} * set{hi,us,here,you}"
	  },
     PARA{},
     "Multiplication involving a list attempts to multiply each element of the list.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "a * {b,c,d}"
	  },
     PARA{},
     "Multiplication of matrices (", TO (symbol *, Matrix, Matrix),") or ring maps is the same as composition.",
     EXAMPLE {
	  "f = map(R,R,{b,c,a,d})",
	  "g = map(R,R,{(a+b)^2,b^2,c^2,d^2})",
	  "f*g",
	  "(f*g)(a) == f(g(a))"
	  },
     PARA{},
     "Submodules of modules may be produced using multiplication and addition.",
     EXAMPLE {
	  "M = R^2; I = ideal(a+b,c);",
	  "N = I*M + a*R^2",
	  "isHomogeneous N"
	  },
     HEADER3 "Intervals",
     PARA { "If one of the factors is an ", TO "RRi", ", the output is an interval containing all products of pairs in the factors." },
    EXAMPLE {
    "2*interval(1,3)",
    "interval(1,3)*interval(-1,2)",
    "interval(-1,1)*interval(-1,1)"
    },
     SeeAlso =>{ "times", "product"}
     }

document {
     Key => (symbol *, Matrix, Matrix),
     Headline => "matrix multiplication",
     Usage => "f * g",
     Inputs => {"f", "g"},
     Outputs => { Matrix },
     "Multiplication of matrices corresponds to composition of maps, and when
     the target ", TT "Q", "
     of ", TT "g", " equals the source ", TT "P", " of ", TT "f", ", the
     product ", TT "f*g", " is defined, its source is the source of ", 
     TT "g", ", and its target is the target of ", TT "f", ".  ",
     EXAMPLE {
	  "R = QQ[a,b,c,x,y,z];",
	  "f = matrix{{x},{y},{z}}",
	  "g = matrix{{a,b,c}}",
	  "f*g"
	  },
     PARA{},
     "The degree of ",
     TT "f*g", " is the sum of the degrees of ", TT "f", " and of ", TT "g",
     ".",
     PARA{},
     "The product is also defined when ", TT "P", " != ", TT "Q", ",
     provided only that ", TT "P", " and ", TT "Q", " are free modules of the
     same rank.  If the degrees of ", TT "P", " differ from the corresponding
     degrees of ", TT "Q", " by the same degree ", TT "d", ", then the degree
     of ", TT "f*g", " is adjusted by ", TT "d", " so it will have a good
     chance to be homogeneous, and the target and source of ", TT "f*g", "
     are as before.",
     EXAMPLE {
	  "target (f*g) == target f",
	  "source (f*g) == source g",
	  "isHomogeneous (f*g)",
	  "degree(f*g)",
	  },
     "Sometimes, it is useful to
     make this a map of degree zero.  Use ", TO (map,Matrix), " for this purpose.",
     EXAMPLE {
	  "h = map(f*g,Degree=>0)",
	  "degree h",
	  "degrees source h"
	  },
     SeeAlso => {(degree,Matrix),degrees}
     }
