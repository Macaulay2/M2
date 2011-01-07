document { 
     Key => {coefficients,
	  (coefficients,Matrix),(coefficients,RingElement),
	  [coefficients,Monomials],[coefficients,Variables]},
     Headline => "monomials and their coefficients",
     Usage => "(M,C) = coefficients f",
     Inputs => {
	  "f" => {"a one-row ", TO "Matrix", " with ", TT "n", " columns, say, or a ", TO "RingElement", ", to
	       be interpreted as a one-by-one matrix.  (A future implementation will handle matrices with more than one row.)" },
	  Variables => List => {
	       "a list ", TT "v", " of variables.
	       If a value for this option is not specified, all of the (top-level) variables are used."
	       },
	  Monomials => {"a list or one-row matrix of monomials, each of which is formed using just variables in ", TT "v", "."}
	  },
     Outputs => {
	  "M" => Matrix => {
	       "either the value of the ", TT "Monomials", " option, if specified (converted to a one-row matrix, if necessary), or
	       a one-row matrix of those monomials appearing in ", TT "f", " that involve just variables 
	       in ", TT "v", ", in descending order.  Let ", TT "m", " denote the number of columns it has."
	       },
	  "C" => Matrix => {
	       "the ", TT "m", " by ", TT "n", " matrix ", TT "C", " such that ", TT "C_(i,j)", " is
	       the coefficient in ", TT "f_(0,j)", " of the monomial ", TT "M_(0,i)", ".
	       In other words, ", TT "C", " is the unique matrix not involving the (specified) variables
	       such that ", TT "M*C == f", ", unless a value was specified for the ", TT "Monomials", " option that did not include
	       all the monomials in the variables ", TT "v", " used by ", TT "f"
	       }
	  },
     EXAMPLE lines ///
     R = QQ[a,b,c,d,e,f][x,y];
     F = a*x^2+b*x*y+c*y^2
     (M,C) = coefficients F
     ///,
     "The resulting matrices have the following property.",
     EXAMPLE lines ///
     M*C === matrix F
     ///,
     "The Sylvester matrix of two generic quadratic forms:",
     EXAMPLE lines ///
     G = d*x^2+e*x*y+f*y^2
     P = matrix{{x*F,y*F,x*G,y*G}}
     (M,C) = coefficients P
     M*C === P
     ///,
     "We may give the monomials directly.  This is useful if we are taking coefficients
     of several elements or matrices, and need a consistent choice of monomials.",
     EXAMPLE lines ///
     (M,C) = coefficients(P, Monomials=>{x^3,y^3,x^2*y,x*y^2})
     ///,
     "If not all of the monomials are used, no error is signaled, but ", TT "M*C == P", " no longer holds.",
     EXAMPLE lines ///
     (M,C) = coefficients(P, Monomials=>{x^3,y^3})
     M*C == P
     ///,
     SeeAlso => {monomials}
     }

TEST ///
     R = QQ[a,b,c,d,e,f][x,y];
     F = a*x^2+b*x*y+c*y^2
     (M,C) = coefficients F
     assert(
	  last coefficients(F, Monomials=>M)
	  == C)
     coefficients(F, Variables=>{x,y,R_2})
///

TEST ///
     R = QQ[a,b,c,d,e,f][x,y];
     F = a*x^2+b*x*y+c*y^2    
     G = d*x^2+e*x*y+f*y^2
     FG = matrix"F;G"
     mons1 = monomials FG
     (M,C) = coefficients FG -- error at the moment BUG
     mons = matrix"0,0,0,x2,xy,y2;x2,xy,y2,0,0,0"
     assert(M == mons)
     assert(
	  last coefficients(FG, Monomials=>mons1)
	  == C)
     coefficients(F, Variables=>{x,y,R_2})
///

TEST ///
  -- Using this for basis(d, f), where f is a map of modules
  R = ZZ/101[a..d]
  M = matrix"a,b;c,d"
  -- let's compute basis(1,M):
  G = source M
  F = target M
  monsF = basis(1,F)
  monsG = basis(1,G)
  last coefficients(M * monsG, Monomials=>monsF) -- this is it!
  
  -- Another example
  R = ZZ/101[a..e]
  I = ideal"ab,bc,cd,de,ea"
  S = reesAlgebra I
  T = ambient S
  L = ideal S
  describe S
  C = res L
  f = C.dd_2
  f1 = basis(1,target f)
  f2 = basis(1,source f)
  last coefficients(f * f2, Monomials=>f1)
  f * f2
  f
  f2

  f = C.dd_1
  f1 = basis(2,target f)
  f2 = basis(2,source f)
  last coefficients(f * f2, Monomials=>f1)
  f * f2

///
