--- status: TODO
--- author(s): MES
--- notes: 

-- document {
--      Key => coefficients,
--      Headline => "the coefficients",
--      TT "coefficients({i,j,...},p)", " -- yields the coefficients and
--      monomials of the polynomial or matrix p with respect to variables 
--      numbered i, j, ... .",
--      BR{},
--      TT "coefficients(p)", " -- yields the coefficients and monomials of
--      the polynomial or matrix p with respect to all of the variables."
--      }

document { 
     Key => {coefficients,(coefficients,Matrix),(coefficients,RingElement)},
     Headline => "monomials and their coefficients",
     Usage => "(monoms,coeffs) = coefficients(f,Variables=>v,Monomials=>m)",
     Inputs => {
	  "f" => {"either a one row ", TO "Matrix", " or a ", TO "RingElement"},
	  "v" => List => "of variables",
	  "m" => {"either a list of monomials, or a one row matrix of monomials"}
	  },
     Outputs => {
	  "monoms" => Matrix => {"a one row matrix of the 
	                         monomials appearing in ", TT "f"},
	  "coeffs" => Matrix => {"a matrix with the coefficients of ", TT "monoms", "
	                         appearing in ", TT "f"}
	  },
     "If the optional argument ", TT "Variables=>v", " is given, then the monomials will only
     involve these variables, and the coefficients will involve only the other variables.",
     PARA{},
     "If the optional argument ", TT "Monomials=>m", " is not given, then the set of monomials 
     appearing in ", TT "f", " is calculated using ", TO monomials, ".",
     EXAMPLE {
	  "R = QQ[x,y,a,b,c,d,e,f];",
	  "F = a*x^2+b*x*y+c*y^2",
	  "(M,C) = coefficients(F, Variables=>{x,y})"
	  },
     "The resulting matrices have the following property.",
     EXAMPLE {
	  "M*C == matrix{{F}}"
	  },
     PARA{},
     "The Sylvester matrix of two generic quadratic forms.",
     EXAMPLE {
	  "G = d*x^2+e*x*y+f*y^2",
	  "P = matrix{{x*F,y*F,x*G,y*G}}",
	  "(M,C) = coefficients(P, Variables=>{x,y})",
	  "M*C == P"
	  },
     "We may give the monomials directly.  This is useful if we are taking coefficients
     of several elements or matrices, and need a consistent choice of monomials.",
     EXAMPLE {
	  "(M,C) = coefficients(P, Variables=>{x,y}, Monomials=>{x^3,y^3,x^2*y,x*y^2})",
	  },
     "If not all of the monomials are used, then ", TT "M*C == P", " no longer holds.",
     EXAMPLE {
	  "(M,C) = coefficients(P, Variables=>{x,y}, Monomials=>{x^3,y^3})",
	  "M*C == P"
	  },
     Caveat => {"Currently, the matrix ", TT "f", " must have only one row.  This restriction
	  will hopefully be lifted in the future."},
     SeeAlso => {monomials}
     }
document { 
     Key => [coefficients,Monomials],
     Headline => "specify monomials",
     }
--     Usage => "(monoms,coeffs) = coefficients(M, Monomials=>mon)",
--     Inputs => {
--	  },
--     Consequences => {
--	  },     
--     "description",
--     EXAMPLE {
--	  },
--     Caveat => {},
--     SeeAlso => {}
--     }
document { 
     Key => [coefficients,Variables],
     Headline => "take coefficients using these variables",
     }
--     Usage => "",
--     Inputs => {
--	  },
--     Outputs => {
--	  },
--     Consequences => {
--	  },     
--     "description",
--     EXAMPLE {
--	  },
--     Caveat => {},
--     SeeAlso => {}
--     }

///
R = QQ[x,y,a,b,c,d,e,f]
F = a*x^2+b*x*y+c*y^2
(M,C) = coefficients(F, Variables=>{x,y})
M*C == matrix{{f}}
G = d*x^2+e*x*y+f*y^2
P = matrix{{x*F,y*F,x*G,y*G}}
(M,C) = coefficients(P, Variables=>{x,y})
M*C == P

(M,C) = coefficients(P, Variables=>{x,y}, Monomials=>{x^3,y^3,x^2*y,x*y^2})
(M,C) = coefficients(P, Variables=>{x,y}, Monomials=>matrix{{x^3,x^2*y,x*y^2}})
M*C == P

///

///
W = QQ [x, y, Dx, Dy, a, Da, WeylAlgebra=>{x=>Dx, y=>Dy, a=>Da}]
M = matrix {{3*x*Dx+2*y*Dy+15}, {3*x*y^2*Dx+2*x^2*Dy+3*y^2}, {y^3*Dy-x^2*Dy+6*y^2}}
coefficients(transpose M,Variables=>{x,y,Dx,Dy})
coefficients(transpose M,Variables=>{0..3})

coefficients({0,1,2,3},M)
///
