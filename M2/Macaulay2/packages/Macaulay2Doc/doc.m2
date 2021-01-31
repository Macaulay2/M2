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
     Key => {describe,
	  (describe, PolynomialRing),
	  (describe, QuotientRing),
	  (describe, FractionField),
	  (describe, Thing),
	  (describe, AffineVariety),
	  (describe, CoherentSheaf),
	  (describe, GaloisField),
	  (describe, GeneralOrderedMonoid),
	  (describe, Matrix),
	  (describe, Module),
	  (describe, ProjectiveVariety),
	  (describe, RingMap)
	  },
     Headline => "real description",
     TT "describe x", " -- returns ", ofClass Expression, " containing the 
     real description of ", TT "x", ", bypassing the feature that causes
     certain types of things to acquire, for brevity, the names of global variables to which
     they are assigned.  For polynomial rings, it also displays the options used at creation.",
     PARA{},
     EXAMPLE lines ///
	  R = ZZ/101[a,b,c_1,c_2];
      	  R
      	  describe R
	  toString describe R
	  toExternalString R
	  QQ[x,d,WeylAlgebra=>{x=>d}]
	  describe oo
	  ///,
     SeeAlso => {"toString", "toExternalString"}
     }

document {
     Key => symbol SPACE, 
     Headline => "blank operator; often used for function application, making polynomial rings",
     SeeAlso =>(symbol SPACE, Function, Thing)		    -- not really a method
     }

document {
     Key => (symbol SPACE, Function, Thing),
     Headline => "function application",
     TT "f x", " -- yields the result of applying the function ", TT "f", " to ", TT "x", ".",
     }

undocumented {
     (symbol *, Number, RingElement),
     (symbol *, RingElement, Number),
    (symbol*,  Expression, Product),
    (symbol*,  Product, Expression),
    (symbol*,  Minus, Expression),
    (symbol*,  Product, Holder),
    (symbol*,  ZeroExpression, Expression),
    (symbol*,  Minus, Minus),
    (symbol*,  ZZ, InfiniteNumber),
    (symbol*,  QQ, InfiniteNumber),
    (symbol*,  RR, InfiniteNumber),
    (symbol*,  InfiniteNumber, ZZ),
    (symbol*,  InfiniteNumber, QQ),
    (symbol*,  InfiniteNumber, RR),
    (symbol*,  Product, ZeroExpression),
    (symbol*,  ZZ, Ideal),
    (symbol*,  Product, OneExpression),
    (symbol*,  ZeroExpression, Holder),
    (symbol*,  Holder, ZeroExpression),
    (symbol*,  Holder, OneExpression),
    (symbol*,  ZZ, GradedModuleMap),
    (symbol*,  InfiniteNumber, InfiniteNumber),
    (symbol*,  Expression, Minus),
    (symbol*,  Product, Product),
    (symbol*,  Holder, Product),
    (symbol*,  ZZ, Module),
    (symbol*,  ZZ, MonomialIdeal),
    (symbol*,  String),
    (symbol*,  ZZ, ChainComplexMap),
    (symbol*,  Expression, ZeroExpression),
    (symbol*,  Expression, OneExpression),
    (symbol*,  OneExpression, Expression),
    (symbol*,  Number, Vector)
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
    (symbol *,Matrix,ZZ),
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
    (symbol*,  GradedModuleMap, GradedModuleMap),
    (symbol*,  RingElement, Matrix),
    (symbol*,  Ideal, CoherentSheaf),
    (symbol*,  RingMap, RingMap),
    (symbol*,  RingElement, MutableMatrix),
    (symbol*,  Ring, MonomialIdeal),
    (symbol*,  MonomialIdeal, Module),
    (symbol*,  AffineVariety, AffineVariety),
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
    (symbol*,  ZZ, ProjectiveHilbertPolynomial),
    (symbol*,  RingElement, Vector)
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
     SeeAlso =>{ "times", "product"}
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
     SeeAlso => {(symbol |,ZZ,ZZ),xor}
     }

document {
     Key => symbol ^^,
     Headline => "a binary operator"
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
     SeeAlso =>{ "plus", "sum"}
     }

undocumented {
     (symbol -, Number, RingElement),
     (symbol -, RingElement, Number),
     }

document {
     Key => {symbol -,
	  (symbol -, ChainComplexMap, ChainComplexMap),
	  (symbol -, Minus),
	  (symbol -, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial),
	  (symbol -, Number, InfiniteNumber),
	  (symbol -, RingElement, GradedModuleMap),
	  (symbol -, GradedModuleMap),
	  (symbol -, Matrix, Number),
	  (symbol -, ProjectiveHilbertPolynomial),
	  (symbol -, RingElement),
	  (symbol -, RingElement, RingElement),
	  (symbol -, InfiniteNumber),
	  (symbol -, InfiniteNumber, InfiniteNumber),
	  (symbol -, ZeroExpression),
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
	  (symbol -, Matrix, RingElement),
	  (symbol -, Matrix),
	  (symbol -, RingElement, Matrix),
	  (symbol -, ChainComplexMap, RingElement),
	  (symbol -, Holder),
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
     SeeAlso =>{ "difference", "minus"}
     }

undocumented {
     (symbol /, InfiniteNumber, InfiniteNumber),
     (symbol /, RingElement, Number),
     (symbol /, Number, RingElement),
     (symbol /, Number, InfiniteNumber),
     (symbol /, InfiniteNumber, ZZ),
     (symbol /, InfiniteNumber, QQ),
     (symbol /, InfiniteNumber, RR),
     (symbol /, EngineRing, Ideal),
     (symbol /, Expression, OneExpression),
     (symbol /, List, SelfInitializingType)
     }

document {
     Key => {symbol /,
	  (symbol /,CC,CC),
	  (symbol /,CC,QQ),
	  (symbol /,CC,RR),
	  (symbol /,CC,ZZ),
	  (symbol /,QQ,CC),
	  (symbol /,QQ,QQ),
	  (symbol /,QQ,RR),
	  (symbol /,QQ,ZZ),
	  (symbol /,RR,CC),
	  (symbol /,RR,QQ),
	  (symbol /,RR,RR),
	  (symbol /,RR,ZZ),
	  (symbol /,ZZ,CC),
	  (symbol /,ZZ,QQ),
	  (symbol /,ZZ,RR),
	  (symbol /,ZZ,ZZ)
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
     SeeAlso => { "//"}
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
     Key => {symbol //,(symbol //,ZZ,ZZ),
	  (symbol //, CC, CC),
	  (symbol //, CC, QQ),
	  (symbol //, CC, RR),
	  (symbol //, CC, ZZ),
	  (symbol //, InfiniteNumber, ZZ),
	  (symbol //, InfiniteNumber, QQ),
	  (symbol //, InfiniteNumber, RR),
	  (symbol //, Matrix, Number),
	  (symbol //, QQ, QQ),
	  (symbol //, Number, RingElement),
	  (symbol //, QQ, ZZ),
	  (symbol //, RingElement, Number),
	  (symbol //, RR, QQ),
	  (symbol //, RR, RR),
	  (symbol //, RR, ZZ),
	  (symbol //, Number, InfiniteNumber),
	  (symbol //, Number, Matrix),
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

document {
     Key => symbol \,
     Headline => "a binary operator",
     }

document {
     Key => symbol \\,
     Headline => "a binary operator"
     }

undocumented {
     (symbol ^, ZeroExpression, Holder),
     (symbol ^, Holder, ZeroExpression),
     (symbol ^, Holder, OneExpression),
     (symbol ^, Expression, ZeroExpression),
     (symbol ^, ZeroExpression, Expression),
     (symbol ^, Expression, OneExpression),
     (symbol ^, ZeroExpression, ZeroExpression),
     (symbol ^, InfiniteNumber, ZZ),
     (symbol ^, InfiniteNumber, QQ),
     (symbol ^, InfiniteNumber, RR),
     (symbol ^, ZZ, InfiniteNumber),
     (symbol ^, QQ, InfiniteNumber),
     (symbol ^, RR, InfiniteNumber),
     }

document {
     Key => {symbol ^,
	  (symbol ^,CC,ZZ),
	  (symbol ^,InexactFieldFamily,ZZ)
	  },
     Headline => "a binary operator, usually used for powers",
     Usage => "x ^ y",
     PARA{},
     "This operator is used for exponentiation, making free modules and sheaves, 
     for shifting complexes left or right, for projection maps involving direct sums, 
     and for making nets.",
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

document {
     Key => "not",
     Headline => "negation",
     TT "not x", " -- yields the negation of x, which must be true or false.",
     SeeAlso =>{ "and", "or" }
     }

document {
     Key => symbol |, 
     Headline => "a binary operator, often used for horizontal concatenation",
     SeeAlso => {"||"}
     }

document {
     Key => {(symbol |, List, List),
	  (symbol |, Array, Array),
	  (symbol |, Sequence, Sequence)},
     Headline => "join lists, sequences or arrays",
     Usage => "v|w",
     Inputs => {"v" => Nothing =>  {ofClass List, ", ",
	       ofClass Array, ", or ",
	       ofClass Sequence},
	  "w" => Nothing => {"of the same type as ", TT "v"}},
     Outputs => {
	  Nothing => "The join of the lists, sequences or arrays."},
     EXAMPLE "{1,2,3}|{4,5,6}",
     EXAMPLE "(a,b,c)|(1,2)",
     SeeAlso => {join}
     }

document {
     Key => {(symbol |, Net, Net),
	  (symbol |, String, String),
	  (symbol |, String, ZZ),
	  (symbol |, ZZ, String)},
     Headline => "join strings or nets",
     TT "s|t", " -- concatenates strings or nets horizontally.", 
     PARA{},
     "The result is a string if the arguments are all strings, otherwise it
     is a net.  The baselines of the nets are aligned.",
     EXAMPLE {
	  ///"abc" | "def"///,
      	  ///x = "abc" || "ABC"///,
      	  ///x|"x"|x///,
	  },
     "If one of the two arguments is an integer, it is converted to a string first.",
     EXAMPLE ///"t = " | 333///
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
     SeeAlso => {(symbol &,ZZ,ZZ),xor}
     }

document {
     Key => {(symbol |, Matrix, Matrix),
	  (symbol |, RingElement, Matrix),
	  (symbol |, Matrix, RingElement),
	  (symbol |, RingElement, RingElement),
	  (symbol |, ZZ, Matrix),
	  (symbol |, Matrix, ZZ)
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
     "If one of the arguments is a ring element or an integer, then it
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
	  (symbol ||, Matrix, ZZ),
	  (symbol ||, ZZ, Matrix)
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
     "If one of the arguments is a ring element or an integer, then it
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

document {
     Key => {(symbol===,Thing,Thing), symbol ===},
     Headline => "strict equality",
     Usage => "x === y",
     Inputs => { "x", "y" },
     Outputs => { Boolean => {"whether the expressions ", TT "x", " and ", TT "y", " are strictly equal"} },
     PARA{
	  "Strictly equal expressions have the same type, so ", TT "0===0.", " and
	  ", TT "0===0/1", " are false; the three types involved here are ", 
	  TO "ZZ", ", ", TO "RR", ", and ", TO "QQ", "."
	  },
     PARA{
	  "If x and y are ", TO "mutable", " then they are strictly equal only
	  if they are identical (i.e., at the same address in memory).  For
	  details about why strict equality cannot depend on the contents of
	  mutable hash tables, see ", TO "hashing", ".  On the other hand, if x
	  and y are non-mutable, then they are strictly equal if and only if
	  all their contents are strictly equal."
	  },
     EXAMPLE { "{1,2,3} === {1,2,3}", "{1,2,3} === {2,1,3}" },
     PARA {
	  "For some types, such as ring elements and matrices, strict equality is the same as mathematical equality.
	  This tends to be the case for objects for which not much computation is not required to test equality.
	  For certain other types, such as ", TO "Ideal", " or ", TO "Module", ", where extensive computations may
	  be required, the operator ", TO "==", " implements the desired comparison."
	  },
     EXAMPLE {
	  "R = QQ[a..d];",
	  "a^2+b === b+a^2",
	  "ideal(a^2+b,c*d) === ideal(b+a^2,c*d+b+a^2)",
     	  "matrix{{a,b,c}} === matrix{{a,b,c}}",
       	  "matrix{{a,b,c}} === transpose matrix{{a},{b},{c}}"
	  },
     PARA {
	  "As it happens, polynomial rings are mutable objects, and new ones are easily created, which are distinct from each other.
	  For example, the rings ", TT "A", " and ", TT "B", " below are not strictly equal."
	  },
     EXAMPLE {
     	  "A = QQ[x]; B = QQ[x];",
     	  "A === B"
     	  },
     SeeAlso =>{ symbol==,  symbol=!=, "operators" }
     }

document {
     Key => {symbol =!=, (symbol=!=,Thing,Thing)},
     Headline => "strict inequality",
     TT "x =!= y", " -- returns true or false depending on whether the expressions
     x and y are strictly unequal.",
     PARA{},
     "See ", TO "===", " for details."
     }

document {
     Key => { symbol ==,
	  (symbol==, Matrix, Matrix), (symbol==, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial),
	  (symbol==, ChainComplex, ChainComplex), (symbol==, RingElement, RingElement), (symbol==, GradedModuleMap, GradedModuleMap),
	  (symbol==, Ideal, Ideal), (symbol==, MutableMatrix, MutableMatrix), (symbol ==,Boolean,Boolean),
	  (symbol ==,CC,CC), (symbol ==,CC,QQ), (symbol ==,CC,RR), (symbol ==,CC,ZZ), (symbol ==,Matrix,Number),
	  (symbol ==,Number,Matrix), (symbol ==,QQ,CC), (symbol ==,QQ,QQ), (symbol ==,QQ,RR), (symbol ==,RR,CC),
	  (symbol ==,RR,QQ), (symbol ==,RR,RR), (symbol ==,RR,ZZ), (symbol ==,RingElement,ZZ), (symbol ==,Sequence,Sequence),
	  (symbol ==,String,String), (symbol ==,Symbol,Symbol), (symbol ==,ZZ,CC), (symbol ==,ZZ,RR),
	  (symbol ==,ZZ,RingElement), (symbol ==,ZZ,ZZ), (symbol==, Module, Module), (symbol==, Vector, Vector),
	  (symbol==, BettiTally, BettiTally), (symbol==, VisibleList, VisibleList),
	  (symbol==, RingElement, Number),
	  (symbol==, Number, RingElement),
	  (symbol==, RingElement, Matrix),
	  (symbol==, Ideal, MonomialIdeal),
	  (symbol==, GradedModuleMap, ZZ),
	  (symbol==, InfiniteNumber, InfiniteNumber),
	  (symbol==, Equation, Expression),
	  (symbol==, ZZ, Ring),
	  (symbol==, ZZ, QQ),
	  (symbol==, Matrix, ZZ),
	  (symbol==, ZZ, ChainComplex),
	  (symbol==, ChainComplex, ZZ),
	  (symbol==, MonomialIdeal, Ring),
	  (symbol==, Ring, MonomialIdeal),
	  (symbol==, MonomialIdeal, ZZ),
	  (symbol==, Equation, Holder),
	  (symbol==, Holder, Equation),
	  (symbol==, ChainComplexMap, ZZ),
	  (symbol==, ZZ, MutableMatrix),
	  (symbol==, Number, InfiniteNumber),
	  (symbol==, Nothing, Nothing),
	  (symbol==, Equation, Equation),
	  (symbol==, GradedModuleMap, RingElement),
	  (symbol==, RingElement, GradedModuleMap),
	  (symbol==, String, Net),
	  (symbol==, Ideal, Ring),
	  (symbol==, ZZ, Ideal),
	  (symbol==, Ideal, ZZ),
	  (symbol==, Matrix, RingElement),
	  (symbol==, MonomialIdeal, Ideal),
	  (symbol==, ZZ, GradedModuleMap),
	  (symbol==, GradedModule, GradedModule),
	  (symbol==, Expression, Equation),
	  (symbol==, Module, ZZ),
	  (symbol==, ZZ, Module),
	  (symbol==, ChainComplexMap, RingElement),
	  (symbol==, RingElement, ChainComplexMap),
	  (symbol==, Ring, ZZ),
	  (symbol==, QQ, ZZ),
	  (symbol==, ZZ, MonomialIdeal),
	  (symbol==, ZZ, ChainComplexMap),
	  (symbol==, MutableMatrix, ZZ),
	  (symbol==, MonoidElement, MonoidElement),
	  (symbol==, MonomialIdeal, MonomialIdeal),
	  (symbol==, InfiniteNumber, Number),
	  (symbol==, ChainComplexMap, ChainComplexMap),
	  (symbol==, Net, Net),
	  (symbol==, Net, String),
	  (symbol==, Module, Ideal),
	  (symbol==, Ideal, Module),
	  (symbol==, Ring, Ideal),
	  (symbol==, RingMap, ZZ),
	  (symbol==, ZZ, RingMap)
	  },
     Headline => "equality",
     Usage => "x == y",
     PARA {
	  "Returns true or false, depending on whether 
	  the objects x and y are (mathematically) equal.  The objects x and y are
	  typically numbers, elements of rings, matrices, modules, ideals, 
	  chain complexes, and so on."
	  },
     PARA {
	  "A test for mathematical equality will typically involve doing a computation
	  to see whether two representations of the same mathematical object are being
	  compared.  For example, an ideal in a ring is represented by giving its
	  generators, and checking whether two sets of generators produce the same
	  ideal involves a computation with Gröbner bases.  The ideals must be defined
	  in the same ring."
	  },
     HEADER3 "Ideals",
     EXAMPLE {
	  "R = QQ[a,b,c];",
	  "ideal(a^2-b,a^3) == ideal(b^2, a*b, a^2-b)"
	  },
     PARA {
     	  "Often mathematical objects can be tested to see if they are 0 or 1."
	  },
     EXAMPLE {
	  "L = ideal(a^2-a-1,a^3+a+3)",
	  "L == 1",
	  "L == 0"
	  },
     HEADER3 "Matrices",
     PARA {
	  "Two ", TO "matrices", " are equal if their entries are equal, the source and target are
	  the same (including degrees), and the degree of the matrices are the same.  In this example,
	  m and n have different source free modules."
	  },
     EXAMPLE {
	  "m = matrix{{a,b},{c,a}}",
     	  "n = map(R^2,R^2,m)",
	  "m == n",
	  "source m == source n"
	  },
     PARA {
     	  "If you only want to know if they have the same entries, test the difference against zero."
	  },
     EXAMPLE {
	  "m-n == 0"
	  },
     HEADER3 "Rings",
     HEADER3 "Modules",
     PARA {
     	  "Two ", TO "modules", " are equal if they are isomorphic as subquotients of the
     	  same ambient free module."
	  },
     EXAMPLE {
      	  "image matrix {{2,a},{1,5}} == R^2",
      	  "image matrix {{2,a},{0,5}} == R^2"
	  },
     PARA{
	  "It may happen that for certain types of objects, there is no method installed (yet)
	  for testing mathematical equality, in which case an error message will be
	  printed.  A good alternative may be to test for strict equality with
	  the operator ", TO "===", "."
	  },
     PARA {
	 "Since various sorts of mathematical objects are implemented as types, i.e., as
	 instances of ", TO "Type", ", there is no generic method for checking equality of types, so that
	 new mathematical comparison code can be provided in the future without breaking code that works."
	 },
     Caveat => {
	  "Warning: whether this comparison operator returns true is not necessarily 
     	  related to whether the comparison operator ", TO symbol ?, " returns ", TT "symbol ==", "."
	  },
     SeeAlso =>{ symbol!=, symbol===, symbol=!=, "operators" }
     }

document {
     Key => symbol !=,
     Headline => "inequality",
     TT "x != y", " -- the negation of ", TT "x == y", ".",
     PARA{},
     SeeAlso =>{ "==", "operators" }
     }

undocumented {
    (symbol**, OneExpression, Holder),
    (symbol**, QuotientRing, PolynomialRing),
    (symbol**, Expression, NonAssociativeProduct),
    (symbol**, QuotientRing, QuotientRing),
    (symbol**, Number, Matrix),
    (symbol**, Matrix, Number),
    (symbol **,Number,RingElement),
    (symbol **,RingElement,Matrix),
    (symbol **,RingElement,Number),
    (symbol **,RingElement,RingElement),
    (symbol **,Thing,InexactFieldFamily),
    (symbol**, NonAssociativeProduct, NonAssociativeProduct),
    (symbol**, Holder, OneExpression),
    (symbol**, PolynomialRing, PolynomialRing),
    (symbol**, PolynomialRing, QuotientRing),
    (symbol**, NonAssociativeProduct, Expression),
    (symbol**, NonAssociativeProduct, Holder),
    (symbol**, Holder, NonAssociativeProduct),
    (symbol**, Expression, OneExpression),
    (symbol**, OneExpression, Expression)
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
