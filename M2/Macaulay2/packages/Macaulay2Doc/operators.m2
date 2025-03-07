load "./operators/dotdot.m2"
load "./operators/caret.m2"
load "./operators/shift.m2"
load "./operators/equality.m2"
load "./operators/quotient.m2"
load "./operators/division.m2"
load "./operators/factoring.m2"
load "./operators/comparison.m2"
load "./operators/assignment.m2"
load "./operators/augmented_assignment.m2"

document {
     Key => Number,
     Headline => "the class of all numbers"
     }

document {
     Key => plus,
     Headline => "addition",
     TT "plus(x,y,...)", " -- yields the sum of its arguments.",
     PARA{},
     "If there are no arguments, the answer is the integer 0."
     }

document {
     Key => times,
     Headline => "multiplication",
	Usage => "times(x,y, ...)",
     TT "times(x,y, ...)", " yields the product of its arguments.
	If there are no arguments, the value is the integer 1."
     }

document {
     Key => power,
     Headline => "power",
	Usage => "(x,n)",
     TT "power(x,n)", " yields the ", TT "n", "-th power of ", TT "x", ".",
     PARA{},
     SeeAlso => "^"
     }

document {
     Key => {powermod,(powermod,ZZ,ZZ,ZZ)},
     Headline => "powers of integers mod N",
     Usage => "powermod(x,i,N)",
     Inputs => {
	  "x" => ZZ,
	  "i" => ZZ,
	  "N" => ZZ
	  },
     Outputs => {
	  ZZ => {"the ", TT "i", "-th power of ", TT "x", " modulo ", TT "N"}
	  },
     EXAMPLE lines ///
	  powermod(2,3,10)
	  powermod(2,4,10)
	  powermod(2,30000,100000)
	  powermod(2,30000,100000000000000000000)
	  powermod(2,3331333,3331333)
     ///
     }

document {
     Key => difference,
     Headline => "difference",
	Usage => "difference(x,y)",
     TT "difference(x,y)", " returns ", TT "x-y", "."
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
     Key => {abs,(abs, RR),(abs, CC),(abs, ZZ),(abs, QQ),(abs, RRi)},
     Headline => "absolute value function",
	Usage => "abs x\nabs I",
	Inputs => {
		"x" => "a number",
        "I" => RRi
		},
	Outputs => {
		{"the absolute value of ", TT "x"},
    RRi => {"an interval containing the absolute values of all the point of ", TT "I"}
		},
     TT "abs x", " computes the absolute value of ", TT "x", ".",
	EXAMPLE {
		"abs(-pi)",
		"abs(1+ii)"
		},
     }

document {
     Key => (exp,RingElement),
     Usage => "exp x",
     Inputs => { "x" },
     Outputs => { { "the exponential of ", TT "x", ", provided ", TT "x", " is nilpotent,
	       and the denominators required have reciprocals in the ring of ", TT "x", "." } } ,
     EXAMPLE lines ///
     R = ZZ/11[x]/x^9
     exp x
     ///
     }

document {
     Key => {exp,(exp,RR),(exp,CC),(exp,RRi)},
     Headline => "exponential function",
     Usage => "exp x\nexp I",
     Inputs => { "x" => RR ,"I"=>RRi},
    Outputs => { { "the exponential of ", TT "x" },
        RRi=>{"an interval containing the exponentials of points of ", TT "I"} } ,
     EXAMPLE lines ///
     exp 1p300
     exp(pi*ii)
     ///
     }

document {
     Key => {log,(log, RR),(log,CC),(log, RRi),(log, RR, RR),(log, RRi, RRi),
	 (log,RR,CC),(log,RR,RRi),(log,RRi,RR)},
     Headline => "logarithm function",
     Usage => "log x\nlog(b,x)\nlog_b x\nlog I\nlog(b,I)\nlog_b I\nlog(J,x)\nlog_J x\nlog(J,I)\nlog_J I",
Inputs => { "x" => RR, "b" => RR => {"the base for the logarithm"}, "I" => RRi, "J" => RRi => {"an interval of bases for the logarithm"} },
Outputs => { { "the logarithm of ", TT "x"}, RRi => {"an interval containing the logarithms of points of ", TT "I"}, RRi => {"an interval containing the logarithms for bases in ", TT "J"} },
     EXAMPLE lines ///
	  log 10
	  log_2 10
	  log_10 2p100
     ///
     }
document {
     Key => {sqrt,(sqrt, CC),(sqrt, RR), (sqrt, RRi)},
     Headline => "square root function",
Usage => "sqrt x\nsqrt I",
     Inputs => { "x" => RR, "I" => RRi },
     Outputs => { { "the square root of ", TT "x"},
RRi => { "an interval containing the square roots of the points of ", TT "I" }
},
     EXAMPLE lines ///
     sqrt 2p200
     sqrt (+ii)
     ///
     }

document {
     Key => {gcd,
	  (gcd, List),
	  (gcd, Sequence),
	  (gcd, QQ, QQ),
	  (gcd, RingElement, RingElement),
	  (gcd, ZZ, QQ),
	  (gcd, QQ, ZZ),
	  (gcd, ZZ, ZZ),
	  (gcd,RingElement,ZZ),
	  (gcd,ZZ,RingElement),
	  (gcd, ZZ),
	  (gcd, QQ),
	  (gcd, RingElement)
	  },
     Headline => "greatest common divisor",
     Usage => "gcd(x,y,...)",
     Inputs => { "x" => ZZ, ", or ", ofClass QQ, ", or ",ofClass RingElement },
     Outputs => { ZZ => { ", or ", ofClass QQ, ", or ",ofClass RingElement, ",
	       the greatest common divisor of the arguments" } },
     EXAMPLE lines ///
     gcd(12,8,48)
     R = QQ[x,y,z];
     gcd(x^2*y,x*y^3^6)
     gcd(x^36-1,x^24-1)
     ///,
     SeeAlso => {gcdCoefficients, lcm}
     }

doc ///
  Key
    lcm
    (lcm, List)
    (lcm, Sequence)
    (lcm, QQ, QQ)
    (lcm, RingElement, RingElement)
    (lcm, ZZ, QQ)
    (lcm, QQ, ZZ)
    (lcm, ZZ, ZZ)
    (lcm,RingElement,ZZ)
    (lcm,ZZ,RingElement)
    (lcm, ZZ)
    (lcm, QQ)
    (lcm, RingElement)
  Headline
    least common multiple
  Usage
    lcm(x,y,...)
  Inputs
    x:RingElement
      all the elements should be integers, rational numbers or ring elements
  Outputs
    m:RingElement
      the least common multiple of the elements x,y,...
  Description
   Example
     lcm(-6,15,14)
     lcm(-6/7,15,14)
     R = QQ[a..d];
     lcm(a^2-d^2,(a-d)*(b+c))
     factor oo
  SeeAlso
    gcd
///

document {
     Key => {symbol ^^, (symbol ^^,ZZ,ZZ)},
     Headline => "logical exclusive-or",
     Usage => "m ^^ n",
     Inputs => { "m", "n"},
     Outputs => {
	  ZZ => {"the bitwise logical exclusive-or of
	       the integers ", TT "m", " and ", TT "n"}
	  },
     EXAMPLE "10 ^^ 12",
     SeeAlso => { (symbol|,ZZ,ZZ), (symbol&,ZZ,ZZ), (symbol ~, ZZ) }
     }

document {
     Key => Boolean,
     Headline => "the class of Boolean values",
     "Predicate functions return these as values, and the logical connectives
     expect to receive them as arguments.",
     PARA{},
     "Special operators dealing with truth values.",
     UL {
	  TO "not",
	  TO "and",
	  TO "or",
	  TO "xor",
	  TO "if"
	  }
     }

document {
     Key => true,
     TT "true", " is a value indicating truth."
     }

document {
     Key => false,
     TT "false", " is a value indicating falsity."
     }

-- TODO: implement or/and for ZZ
document {
     Key => "or",
     Headline => "disjunction",
     TT "t or u", " -- returns true if ", TT "t", " is true or ", TT "u", "
     is true.",
     PARA{},
     "If ", TT "t", " is true, then the code in ", TT "u", " is not evaluated.",
     SeeAlso =>{ "and", "not", "xor" }
     }

document {
     Key => "and",
     Headline => "conjunction",
     TT "t and u", " -- returns true if ", TT "t", " is true and ", TT "u", "
     is true.",
     PARA{},
     "If ", TT "t", " is false, then the code in ", TT "u", " is not evaluated.",
     SeeAlso =>{ "or", "not", "xor" }
     }

document {
     Key => "not",
     Headline => "negation",
     TT "not x", " -- yields the negation of x, which must be true or false.",
     SeeAlso =>{ "and", "or" }
     }

doc ///
  Key
    symbol xor
    (symbol xor, Boolean, Boolean)
  Headline
    exclusive disjunction
  Usage
    t xor u
  Inputs
    t:Boolean
    u:Boolean
  Outputs
    :Boolean
      equivalent to @TT "t and not u or not t and u"@
  SeeAlso
    symbol and
    symbol or
///

document {
     Key => symbol :,
     Headline => "a binary operator, uses include repetition; ideal quotients",
     }

document {
     Key => symbol ~,
     Headline => "a unary postfix operator",
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

document {
     Key => symbol ++,
     Headline => "a binary operator, usually used for direct sum"
     }

document {
     Key => symbol (*),
     Headline => "a unary postfix operator, used for indicating a graded object"
     }

document {
     Key => symbol ^*,
     Headline => "a unary postfix operator, used for indicating pullback maps"
     }

document {
     Key => symbol _*,
     Headline => "a unary postfix operator, used for indicating pushforward maps"
     }

document {
     Key => symbol ^!,
     Headline => "a unary postfix operator, used for the upper shriek functor"
     }

document {
     Key => symbol _!,
     Headline => "a unary postfix operator, used for the lower shriek functor"
     }

document {
     Key => symbol |_,
     Headline => "a binary operator, used for restriction to a subset"
     }

document {
     Key => symbol ^~,
     Headline => "a unary postfix operator, used for sheafification"
     }

document {
     Key => symbol _~,
     Headline => "a unary postfix operator"
     }

apply({symbol ^>, symbol ^>=, symbol ^<, symbol ^<=, symbol _>, symbol _>=, symbol _<, symbol _<=},
    symb -> document { Key => symb, Headline => "a binary operator, used for truncation" })

document {
     Key => symbol <==>,
     Headline => "a binary operator"
     }

document {
     Key => symbol ,,
     Headline => "the comma, used for separating entries in a list or sequence"
     }

document {
     Key => symbol ==>,
     Headline => "a binary operator"
     }

document {
     Key => symbol |-,
     Headline => "a binary operator"
     }

document {
     Key => symbol ===>,
     Headline => "a binary operator"
     }

document {
     Key => symbol <===,
     Headline => "a unary and binary operator"
     }

document {
     Key => symbol <==,
     Headline => "a unary and binary operator"
     }

document {
     Key => symbol @@,
     Headline => "a binary operator"
     }

document {
     Key => (symbol @@, Function, Function),
     Headline => "composition of functions",
     Usage => "f @@ g",
     Inputs => { "f", "g" },
     Outputs => {{ "the composite function of ", TT "f", " and ", TT "g", "." }},
     EXAMPLE {
	  "f = i -> i+1",
	  "g = i -> i^2",
	  "apply(0 .. 10, f @@ g)",
	  "apply(0 .. 10, g @@ f)"
	  }
     }

document {
     Key => symbol @,
     Headline => "a binary operator",
     "This operator is right associative."
     }

document {
     Key => { (symbol /,VisibleList,Function),
	  (symbol /,List,Function),
	  (symbol \,Function,VisibleList),
	  (symbol \,Function,VirtualTally),
	  (symbol \,SelfInitializingType,VisibleList),
	  (symbol \,Command,VisibleList),
	  (symbol \,RingMap,VisibleList),
	  (symbol \,Command,VirtualTally),
	  (symbol /, List, SelfInitializingType),
	  (symbol /,VisibleList,SelfInitializingType),
	  (symbol /,List,Command),
	  (symbol /, Set, Command),
	  (symbol /, Set, Function),
	  (symbol \, Command, Set),
	  (symbol \, Function, Set),
	  (symbol /,VirtualTally,Command),
	  (symbol /,VirtualTally,Function),
	  (symbol /,VisibleList,RingMap),
	  (symbol /,VisibleList,Command),
	  (symbol /,String,Command),
	  (symbol /,String,Function),
	  (symbol \,Command,String),
	  (symbol \,Function,String)
	  },
     Headline => "apply a function to elements of a list",
     Usage => "x/f\nf\\x",
     Inputs => { "x" => Nothing => {ofClass{VisibleList,List,Sequence,Array,Tally,Set,String}}, "f" => Nothing => {ofClass{Function,Command,SelfInitializingType,RingMap}} },
     Outputs => {{ "the list, tally, or set obtained by applying ", TT "f", " to each element of ", TT "x", "; it has the same type as ", TT "x", " has" }},
     PARA {
	  "The function ", TO "apply", " does the same thing."
	  },
     PARA {
     	  "The operator ", TO "/", " is left associative, which means that ", TT "w / f / g", " is interpreted as ", TT "(w / f) / g", ".
     	  The operator ", TO "\\", " is right associative, so ", TT ///g \ f \ w///, " is interpreted as ", TT ///g \ (f \ w)///, ".
	  Both operators have parsing precedence lower than that of ", TO "@@", ", which means that the previous two expressions are equivalent to ", TT "w / g @@ f", "
	  and ", TT "g @@ f \\ w", ", respectively. See ", TO "precedence of operators", "."
	  },
     EXAMPLE lines ///
     	  f = x -> x+1
	  g = x -> 2*x
     	  g \ (1 .. 10)
     	  (1 .. 10) / g
     	  f \ g \ (1 .. 10)
     	  f @@ g \ (1 .. 10)
	  set (1 .. 10)
	  g \ oo
	  R = QQ[x];
	  f = map(R,R,{x^2})
	  f \ {x,x^2,x^3,x^4}
     ///,
     SourceCode => {(symbol /,VisibleList,Function)},
     }

document {
     Key => { (symbol /,Ideal,Function),
	  (symbol \,Function,Ideal)},
     Headline => "apply a function to generators of an ideal",
     Usage => "I/f\nf\\I",
     Inputs => { "I","f"},
     Outputs => {List => { "obtained by applying the function ", TT "f", " to each generator of ", TT "I"}},
     PARA {
     	  "The operator ", TO "/", " is left associative, which means that ", TT "w / f / g", " is interpreted as ", TT "(w / f) / g", ".
     	  The operator ", TO "\\", " is right associative, so ", TT ///g \ f \ w///, " is interpreted as ", TT ///g \ (f \ w)///, ".
	  Both operators have parsing precedence lower than that of ", TO "@@", ", which means that the previous two expressions are
	  equivalent to ", TT "w / g @@ f", "
	  and ", TT "g @@ f \\ w", ", respectively. See ", TO "precedence of operators", "."
	  },
     EXAMPLE lines ///
     	  R = ZZ[a..d];
	  I = ideal"abc-d3,ab-d-1,a2+b2+c3-14d-3"
     	  I/size
	  (f->f+a*b-1)\I
	  I/leadTerm/support/set//sum
     ///,
     }

document {
    Key => {
	(symbol //, Thing, Function),
	(symbol //, Thing, Command),
	(symbol //, Thing, SelfInitializingType),
	(symbol \\, Function, Thing),
	(symbol \\, Command, Thing),
	(symbol \\, SelfInitializingType, Thing)
    },
    Headline => "apply a function",
    Usage => "x // f\nf \\\\ x",
    Inputs => { "x", "f" => Nothing => {ofClass{Function,Command,SelfInitializingType}}},
    Outputs => {{ "the result of applying ", TT "f", " to ", TT "x", ", i.e., ", TT "f x" }},
    SeeAlso => {(symbol /,VisibleList,Function)},
    PARA {
	"The parsing precedence of the operators ", TT "//", " and ", TT "\\\\", " is rather low, which makes
	them useful for avoiding parentheses.  See ", TO "precedence of operators", "."
    },
    EXAMPLE lines ///
     	  toList \\ sin \ ( 1 .. 5 )
     	  ( 1 .. 5 ) / sin // toList
	  (x -> (x,x)) \ (a,b,c,d)
	  splice \\ (x -> (x,x)) \ (a,b,c,d)
    ///
    }
