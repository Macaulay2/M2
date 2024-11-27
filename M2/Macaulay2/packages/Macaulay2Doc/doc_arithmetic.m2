document {
     Key => {numeric,(numeric,Matrix),(numeric,ZZ,Matrix),
	  (numeric, Vector),(numeric, ZZ, Vector),
	  (numeric, ZZ, CC), (numeric, RR), (numeric, RRi),
      (numeric, ZZ, RRi),
        (numeric, CC), (numeric, ZZ, VisibleList),
	  (numeric, VisibleList), (numeric, ZZ, Constant), (numeric, Constant),
	  (numeric, InfiniteNumber, Constant),
     	  (numeric, ZZ, Number), (numeric, Number),
	  (numeric, ZZ, InfiniteNumber),(numeric, InfiniteNumber),
	  (numeric, ZZ, IndeterminateNumber), (numeric, IndeterminateNumber)},
     Headline => "convert to floating point",
     Usage => "numeric x\nnumeric(prec,x)\nnumeric_prec x",
     Inputs => { 
	  "x",
	  "prec" => ZZ => {"the number of bits of precision to use in the computation"}
	  },
     Outputs => {{ "the expression obtained from ", TT "x", " by converting the 
     	       integers, rational numbers, and symbolic constants within it
	       to double floating point numbers."}},
     EXAMPLE {"x = {1,2/3,pi}","numeric oo","numeric_200 pi","numeric_100 oo"},
     SeeAlso => { RR, CC, Constant }}

document {
     Key => odd,
     Headline => "tell whether an integer is odd",
     TT "odd x", " -- returns true or false, tells whether x is an odd integer.",
     PARA{},
     "See also ", TO "even", "."}
document {
     Key => even,
     Headline => "tell whether an integer is even",
     TT "even x", " -- returns true or false, tells whether x is an even integer.",
     PARA{},
     "See also ", TO "odd", "."}

document {
     Key => {realPart, (realPart,Number), (realPart,QQ), (realPart,ZZ),
	 (realPart,InexactNumber)},
     Headline => "real part",
     Usage => "realPart z",
     Inputs => {"z" => "an integer, rational, real or complex number"},
     Outputs => {"the real part of the complex number z."},
     EXAMPLE {
	  "realPart(3/4)",
	  "realPart(1.5+2*ii)"
	  },
     SeeAlso => {CC, imaginaryPart}
     }
document {
     Key => {imaginaryPart,(imaginaryPart,Number), (imaginaryPart,QQ),
	 (imaginaryPart,ZZ), (imaginaryPart,InexactNumber)},
     Headline => "imaginary part",
     Usage => "imaginaryPart z",
     Inputs => {"z" => "an integer, rational, real or complex number"},
     Outputs => {"the imaginary part of the complex number z."},
     EXAMPLE {
	  "imaginaryPart(3/4)",
	  "imaginaryPart(1.5+2*ii)"
	  },
     SeeAlso => {CC, realPart}
     }

document {
     Key => {conjugate,(conjugate,CC),(conjugate,Number),(conjugate,Constant)},
     Headline => "complex conjugate",
     Usage => "conjugate z",
     Inputs => {"z"},
     Outputs => {CC => {"the complex conjugate of ", TT "z"}},
     EXAMPLE {
	  "conjugate(1+2.5*ii)",
	  "conjugate 3"
	  }
     }

doc ///
  Key
    gcdCoefficients
    (gcdCoefficients, RingElement, RingElement)
    (gcdCoefficients, RingElement, ZZ)
    (gcdCoefficients, ZZ, RingElement)
    (gcdCoefficients, ZZ, ZZ)
  Headline
    greatest common divisor with coefficients
  Usage
    gcdCoefficients(a, b)
  Inputs
    a:{RingElement, ZZ}
    b:{RingElement, ZZ}
  Description
    Text
      This returns a list of the form @CODE "{d, r, s}"@, where $d=\gcd(a, b)$
      and $r$ and $s$ are the minimal Bézout coefficients satisfying the
      equation $d = ar + bs$.

      It works for integers or elements of polynomial rings in one variable.
    Example
      gcdCoefficients(46, 240)
      gcd(46, 240)
      46 * 47 + 240 * (-9)
      R = ZZ/2[x]
      f = x^8 + x^4 + x^3 + x + 1
      g = x^6 + x^4 + x + 1
      gcdCoefficients(f, g)
      gcd(f, g)
      f * (x^5 + x^4 + x^3 + x^2 + 1) + g * (x^7 + x^6 + x^3 + x)
  SeeAlso
    gcd
///

document {
     Key => mod,
     Headline => "reduce modulo an integer",
     Usage => "mod(i,n)",
     Inputs => {
	  "i" => ZZ,
	  "n" => ZZ },
     Outputs => {
	  ZZ => { "the integer ", TT "i", " modulo ", TT "n", ", as an element of ", TT "ZZ/n", "." }
	  },
     SeeAlso => {(symbol %, ZZ, ZZ)}
     }

