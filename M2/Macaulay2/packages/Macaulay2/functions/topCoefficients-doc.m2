--- status: DRAFT
--- author(s): 
--- notes: 

document {
     Key => topCoefficients,
     Headline => "first variable and its coefficient of a polynomial or matrix",
     TT "topCoefficients m", " -- for a matrix ", TT "m", ", for each column, returns
     the coefficients of the highest power of the variable with the lowest
     index.",
     PARA{},
     "Beware: the greatest variable is usually the first variable.",
     PARA{},
     "The value returned is a list ", TT "{monoms, coeff}", ".
     Let x_i be the smallest index variable that occurs in the
     j-th column of ", TT "m", ". Then the j-th column of ", TT "coeff", "
     contains the (vector) coefficient of the highest power of this
     variable, and the j-th element of ", TT "monoms", " is the highest power
     x_i^n."
     }

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
	  "lf" => Nothing => {"the power of the lead index variable occuring in f, if f is a ring element,
	     or the one row matrix of these variables for each column, if f is a matrix."},
	  "cf" => Nothing => {"the cofficient of lf in f, if f is a ring element,
	       or the matrix of these coefficients for each column of f."}
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
     SeeAlso => {pseudoRemainder}
     }
