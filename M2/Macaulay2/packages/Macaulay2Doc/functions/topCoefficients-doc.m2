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
