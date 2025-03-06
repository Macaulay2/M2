-- -*- coding: utf-8 -*-
--		Copyright 1993-1999 by Daniel R. Grayson

document {
     Key => identity,
     Headline => "the identity function",
     TT "identity x", " -- returns x.",
     PARA{},
     "This is the identity function."
     }

undocumented {
     (symbol *, Number, RingElement),
     (symbol *, RingElement, Number),
    (symbol*,  Number, InfiniteNumber),
    (symbol*,  InfiniteNumber, ZZ),
    (symbol*,  InfiniteNumber, QQ),
    (symbol*,  InfiniteNumber, RR),
    (symbol*,  ZZ, Ideal),
    (symbol*,  ZZ, GradedModuleMap),
    (symbol*,  InfiniteNumber, InfiniteNumber),
    (symbol*,  ZZ, Module),
    (symbol*,  ZZ, MonomialIdeal),
    (symbol*,  String),
    (symbol*,  ZZ, ChainComplexMap),
    }

     
document {
     Key => {symbol*,
    (symbol*,  Ring, Ideal),
    (symbol*,  MutableMatrix, MutableMatrix),
    (symbol*,  Ideal, Module),
    (symbol*,  Ring, RingElement),
    (symbol *,Constant,Constant),
    (symbol *,Constant,InexactNumber),
    (symbol *,Constant,Number),
    (symbol *,InexactNumber,Constant),
    (symbol *,Matrix,Number),
    (symbol *,Number,Constant),
    (symbol *,Number,Matrix),
    (symbol *,QQ,CC),
    (symbol *,QQ,QQ),
    (symbol *,QQ,RR),
    (symbol *,QQ,ZZ),
    (symbol *,RR,CC),
    (symbol *,RR,QQ),
    (symbol *,RR,RR),
    (symbol *,RR,ZZ),
    (symbol *,ZZ,CC),
    (symbol *,ZZ,QQ),
    (symbol *,ZZ,RR),
    (symbol *,ZZ,ZZ),
    (symbol *,CC,CC),
    (symbol *,CC,QQ),
    (symbol *,CC,RR),
    (symbol *,CC,ZZ),
    (symbol *, QQ, RRi),
    (symbol *, RR, RRi),
    (symbol *, RRi, QQ),
    (symbol *, RRi, RR),
    (symbol *, RRi, RRi),
    (symbol *, RRi, ZZ),
    (symbol *, ZZ, RRi),
    (symbol*,  GradedModuleMap, GradedModuleMap),
    (symbol*,  RingElement, Matrix),
    (symbol*,  RingMap, RingMap),
    (symbol*,  RingElement, MutableMatrix),
    (symbol*,  Ring, MonomialIdeal),
    (symbol*,  MonomialIdeal, MonomialIdeal),
    (symbol*,  RingElement, Ideal),
    (symbol*,  Matrix, Vector),
    (symbol*,  MonomialIdeal, Ring),
    (symbol*,  Ring, Vector),
    (symbol*,  RingElement, ChainComplexMap),
    (symbol*,  Ideal, Vector),
    (symbol*,  RingElement, MonomialIdeal),
    (symbol*,  Matrix, RingElement),
    (symbol*,  RingElement, Module),
    (symbol*,  RingElement, GradedModuleMap),
    (symbol*,  Ideal, Ring),
    (symbol*,  ChainComplexMap, ChainComplexMap),
    (symbol*,  RingElement, RingElement),
    (symbol*,  Thing, List),
    (symbol*,  List, Thing),
    (symbol*,  ZZ, ProjectiveHilbertPolynomial),
    (symbol*,  ProjectiveHilbertPolynomial, ZZ),
    (symbol*,  RingElement, Vector),
    (symbol*,  Number, Vector),
    (symbol*,  Vector, RingElement),
    (symbol*,  Vector, Number)
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
     "The intersection of sets is given by multiplication.  See ", TO (symbol*,Set,Set), ".",
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
     "Multiplication of matrices (", TO (symbol*,Matrix,Matrix),") or ring maps is the same as composition.",
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

undocumented {
	  (symbol +, Number, RingElement),
	  (symbol +, RingElement, Number)
	  }
document {
     Key => {symbol +,
	  (symbol +, ChainComplexMap, ChainComplexMap),
	  (symbol +, Number, InfiniteNumber),
	  (symbol +, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial),
	  (symbol +, ZZ, ProjectiveHilbertPolynomial),
	  (symbol +, ProjectiveHilbertPolynomial, ZZ),
	  (symbol +, MonomialIdeal, MonomialIdeal),
	  (symbol +,CC),
	  (symbol +,CC,CC),
	  (symbol +,CC,QQ),
	  (symbol +,CC,RR),
	  (symbol +,CC,ZZ),
	  (symbol +,Constant),
	  (symbol +,Constant,Constant),
	  (symbol +,Constant,InexactNumber),
	  (symbol +,Constant,Number),
	  (symbol +,InexactNumber,Constant),
	  (symbol +,Number,Constant),
	  (symbol +,QQ),
	  (symbol +,QQ,CC),
	  (symbol +,QQ,QQ),
	  (symbol +,QQ,RR),
	  (symbol +,QQ,ZZ),
	  (symbol +,RR),
	  (symbol +,RR,CC),
	  (symbol +,RR,QQ),
	  (symbol +,RR,RR),
	  (symbol +,RR,ZZ),
	  (symbol +,ZZ),
	  (symbol +,ZZ,CC),
	  (symbol +,ZZ,QQ),
	  (symbol +,ZZ,RR),
	  (symbol +,ZZ,ZZ),
      (symbol +, QQ, RRi),
      (symbol +, RR, RRi),
      (symbol +, RRi, QQ),
      (symbol +, RRi, RR),
      (symbol +, RRi),
      (symbol +, RRi, RRi),
      (symbol +, RRi, ZZ),
      (symbol +, ZZ, RRi),
      (symbol +, RingElement, GradedModuleMap),
	  (symbol +, Matrix, Number),
	  (symbol +, ZZ, ChainComplexMap),
	  (symbol +, ChainComplexMap, ZZ),
	  (symbol +, RingElement, RingElement),
	  (symbol +, InfiniteNumber, InfiniteNumber),
	  (symbol +, Ideal, RingElement),
	  (symbol +, RingElement, ChainComplexMap),
	  (symbol +, MutableMatrix, MutableMatrix),
	  (symbol +, Matrix, Matrix),
	  (symbol +, GradedModuleMap, GradedModuleMap),
	  (symbol +, InfiniteNumber, Number),
	  (symbol +, GradedModuleMap, RingElement),
	  (symbol +, Number, Matrix),
	  (symbol +, Vector),
	  (symbol +, Vector, Vector),
	  (symbol +, Matrix, RingElement),
	  (symbol +, RingElement, Matrix),
	  (symbol +, ChainComplexMap, RingElement)
     	  },
     Headline => "a unary or binary operator, usually used for addition",
     Usage => "+y \n x+y",
     "In most cases, this operator refers to standard addition.",
     PARA{},
     "In many cases, the integer 1 can be used as the identity, and scalars function as multiples
     of the identity.  
     For example, the 1 below refers to the identity matrix
     and the 2 to twice the identity matrix.",
     EXAMPLE lines ///
     	  M = matrix{{1,2,3},{2,3,4},{3,4,6}}
	  M+1, M+2
     ///,
    HEADER3 "Intervals",
    PARA { "If one of the addends is an ", TO "RRi", ", the output is an interval containing all sums of pairs in the addends." },
    EXAMPLE {
        "2+interval(1,3)",
        "interval(1,3)+interval(-1,2)",
        "interval(-1,1)+interval(-1,1)"
    },
     SeeAlso =>{ "plus", "sum"}
     }

undocumented {
     (symbol -, Number, RingElement),
     (symbol -, RingElement, Number),
     }

document {
     Key => {symbol -,
	  (symbol -, ChainComplexMap, ChainComplexMap),
	  (symbol -, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial),
	  (symbol -, ProjectiveHilbertPolynomial, ZZ),
	  (symbol -, ZZ, ProjectiveHilbertPolynomial),
	  (symbol -, Number, InfiniteNumber),
	  (symbol -, RingElement, GradedModuleMap),
	  (symbol -, GradedModuleMap),
	  (symbol -, Matrix, Number),
	  (symbol -, ProjectiveHilbertPolynomial),
	  (symbol -, RingElement),
	  (symbol -, RingElement, RingElement),
	  (symbol -, InfiniteNumber),
	  (symbol -, InfiniteNumber, InfiniteNumber),
	  (symbol -, RingElement, ChainComplexMap),
	  (symbol -, ChainComplexMap),
	  (symbol -, MutableMatrix, MutableMatrix),
	  (symbol -, Matrix, Matrix),
	  (symbol -, GradedModuleMap, GradedModuleMap),
	  (symbol -, InfiniteNumber, Number),
	  (symbol -, GradedModuleMap, RingElement),
	  (symbol -, Number, Matrix),
	  (symbol -, MutableMatrix),
	  (symbol -,CC),
	  (symbol -,CC,CC),
	  (symbol -,CC,QQ),
	  (symbol -,CC,RR),
	  (symbol -,CC,ZZ),
	  (symbol -,Constant),
	  (symbol -,Constant,Constant),
	  (symbol -,Constant,InexactNumber),
	  (symbol -,Constant,Number),
	  (symbol -,InexactNumber,Constant),
	  (symbol -,Number,Constant),
	  (symbol -,QQ),
	  (symbol -,QQ,CC),
	  (symbol -,QQ,QQ),
	  (symbol -,QQ,RR),
	  (symbol -,QQ,ZZ),
	  (symbol -,RR),
	  (symbol -,RR,CC),
	  (symbol -,RR,QQ),
	  (symbol -,RR,RR),
	  (symbol -,RR,ZZ),
	  (symbol -,ZZ),
	  (symbol -,ZZ,CC),
	  (symbol -,ZZ,QQ),
	  (symbol -,ZZ,RR),
	  (symbol -,ZZ,ZZ),
      (symbol -, QQ, RRi),
      (symbol -, RR, RRi),
      (symbol -, RRi),
      (symbol -, RRi, QQ),
      (symbol -, RRi, RR),
      (symbol -, RRi, RRi),
      (symbol -, RRi, ZZ),
      (symbol -, ZZ, RRi),
	  (symbol -, Matrix, RingElement),
	  (symbol -, Matrix),
	  (symbol -, RingElement, Matrix),
	  (symbol -, ChainComplexMap, RingElement),
	  (symbol -, Vector, Vector)
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
    Key => (symbol -, Vector),
    Headline => "negation of a Vector",
    TT "-v", " -- the negation of ", TT "v",
    PARA{},
    EXAMPLE lines ///
        v = vector {2,3,5,7}
	- v
    ///
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
     Key => symbol \,
     Headline => "a binary operator",
     }

document {
     Key => symbol \\,
     Headline => "a binary operator"
     }

document {
     Key => {symbol !, (symbol !, ZZ), (symbol !, QQ), (symbol !, RR),(symbol !,Constant)},
     Headline => "factorial",
     Usage => "n!",
     Inputs => {"n"=>ZZ},
     Outputs => { ZZ => "n factorial, 1*2*3*...*n."},
     EXAMPLE lines ///
     	  30!
     	  30.!
	  30.01!
     ///
     }


doc ///
  Key
    (symbol ~, ZZ)
  Headline
    logical not
  Usage
    n~
  Inputs
    n:ZZ
  Outputs
    :ZZ -- the bitwise complement of @TT "n"@
  Description
    Example
      7~
    Text
      Note that @TT "~"@ has @TO2 {"precedence of operators",
      "higher precedence"}@ than @TT "-"@, so enclose negative integers in
      parentheses.
    Example
      (-12)~
  SeeAlso
    (symbol &, ZZ, ZZ)
    (symbol |, ZZ, ZZ)
    (symbol ^^, ZZ, ZZ)
///

document {
     Key => symbol |, 
     Headline => "a binary operator, often used for horizontal concatenation",
     SeeAlso => {"||"}
     }

document {
     Key => (symbol |, ZZ, ZZ),
     Headline => "logical or",
     Usage => "m | n",
     Inputs => {"m", "n"},
     Outputs => {
	  ZZ => {"obtained from the bits of the 
     	       integers ", TT "m", " and ", TT "n", " by logical 'or'."}
	  },
     EXAMPLE "2^42 | 2^15 == 2^42 + 2^15",
     SeeAlso => {(symbol &,ZZ,ZZ),(symbol ^^,ZZ,ZZ), (symbol ~, ZZ)}
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
     Key => symbol ||,
     Headline => "a binary operator, often used for vertical concatenation"
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

document {
     Key => (symbol **, Set, Set),
     Headline => "Cartesian product",
     Usage =>  "x ** y", 
     Inputs => {
	  "x",
	  "y"
	  },
     Outputs => {
	  Set => "whose elements are the sequences (a,b), where a is an element
     	  of x, and b is an element of y."
	  },
     EXAMPLE "set {1,2} ** set {a,b,c}",
     "Suppose we wish to form the set of
     all triples with entries either in the set A below.",
     EXAMPLE {
	  "A = set{1,2}",
	  "A ** A ** A"
	  },
     "To make this last a set of triples, ", TO splice, " each element together.  
     Or, use ", TO (symbol^**,VirtualTally,ZZ), ".",
     EXAMPLE {
	  "(A ** A ** A)/splice",
	  "A^**3"
	  },
     SeeAlso => { Set }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
