load "./operators/dotdot.m2"

document {
     Key => Number,
     Headline => "the class of all numbers"
     }

document {
     Key => plus,
     Headline => "addition",
     TT "plus(x,y,...)", " -- yields the sum of its arguments.",
     PARA{},
     "If the arguments are strings, they are concatenated.  If there
     are no arguments, the answer is the integer 0."
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
     Key => {exp,(exp,RR),(exp,CC),(exp,ZZ),(exp,QQ),(exp,Constant),(exp,RRi)},
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
     Key => {log,(log, RR),(log, QQ),(log, ZZ),(log,CC),(log, RRi),(log,QQ,CC),(log,RR,CC),(log,ZZ,CC),
	  (log, ZZ, ZZ),(log, QQ, ZZ),(log, ZZ, QQ),(log, QQ, QQ),(log, RR, ZZ),
	  (log, ZZ, RR),(log, QQ, RR),(log, RR, QQ),(log, RR, RR),
      (log, QQ, RRi), (log, RR, RRi), (log, RRi, QQ), (log, RRi, RR), (log, RRi, RRi), (log, RRi, ZZ),(log, ZZ, RRi)},
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
     Key => {sqrt,(sqrt, CC),(sqrt, QQ),(sqrt, ZZ),(sqrt, RR), (sqrt, RRi)},
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
	  (gcd,ZZ,RingElement)},
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
     SeeAlso => { (symbol|,ZZ,ZZ), (symbol&,ZZ,ZZ) }
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
