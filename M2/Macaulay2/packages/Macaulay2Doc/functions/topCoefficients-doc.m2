--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => {topCoefficients,
	  (topCoefficients, RingElement),
	  (topCoefficients, Matrix)},
     Headline => "first variable and its coefficient of a polynomial or matrix",     
     Usage => "(lf, cf) = topCoefficients f",
     Inputs => {
	  "f" => Nothing => {ofClass RingElement, " or ", ofClass Matrix}
	  },
     Outputs => {
	  "lf" => Nothing => {ofClass RingElement, ", or ", ofClass Matrix, 
	       ", the power of the lowest index variable occurring in f, if f is a ring element,
	     or the one row matrix of these powers for each column, if f is a matrix."},
	  "cf" => Nothing => {ofClass RingElement, ", or ", ofClass Matrix, 
	       ", the cofficient of lf in f, if f is a ring element,
	       or the matrix of these coefficients for each column of f"}
	  },
     EXAMPLE lines ///
     	  A = ZZ[x]
	  (lf,cf) = topCoefficients (7*x^4-13*x^3+x+1)
	  v = first support lf
	  e = first degree lf
	  ///,
     "The polynomial ring may have more variables.",
     EXAMPLE lines ///
     	  B = ZZ[x,y,z]
	  f = y^4*(3*z^3-z^2-1) - y^3*z^7 + y + z^12
	  (lf,cf) = topCoefficients f
          ///,
     Caveat => {"If the polynomial ring B of f has a polynomial coefficient ring A, 
         and no variables of B occur in f, then this 'drills down' into A and finds the
         top variable and coefficient there, but as elements of B"
         },
     SeeAlso => {pseudoRemainder}
     }

TEST ///
  R = ZZ[x,y,z]
  f = y^4*(3*z^3-z^2-1) - y^3*z^7 + y + z^12
  assert(topCoefficients f == (y^4, 3*z^3-z^2-1))
  assert(topCoefficients matrix{{f}} == (matrix{{y^4}}, matrix{{3*z^3-z^2-1}}))
  assert(topCoefficients matrix{{f, x*y-1}} == (matrix{{y^4, x}}, matrix{{3*z^3-z^2-1, y}}))
  assert(topCoefficients matrix{{x*y^4}, {x*z}} == (matrix{{x}}, matrix{{y^4},{z}}))
  assert(topCoefficients 3_R == (1_R, 3_R))
  assert(topCoefficients 0_R == (1_R, 0_R))
  assert(topCoefficients matrix{{1_R}} == (matrix{{1_R}}, matrix{{1_R}}))
  assert(topCoefficients(x*y-1) == (x,y))
  assert(topCoefficients(x+3*x^2+5*x^3+7*x^4+19) == (x^4, 7_R))
///

TEST ///
  A = ZZ[a,b]
  B = A[c,d,e]
  f = a*c^2-b*c
  assert(topCoefficients f == (c^2, a))
  g = sub(a^2*b+a*b, B)
  assert(topCoefficients g == (a^2, b)) -- this is perhaps not completely expected behavior
///

TEST ///
  A = ZZ/32003[a,b,c,d]/(a^2-b-1)
  f = a*(b+c+d^2) - 3
  topCoefficients f == (0,0) -- no, it should really treat it as a polynomial...
///