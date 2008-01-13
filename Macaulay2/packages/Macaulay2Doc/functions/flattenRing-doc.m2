--- status: TODO
--- author(s): 
--- notes: 

undocumented {
     (flattenRing, GaloisField),
     (flattenRing, QuotientRing),
     (flattenRing, PolynomialRing)
     }

document { 
     Key => {(flattenRing, Ring),
	  flattenRing,
	  [flattenRing,CoefficientRing]
	  },
     Headline => "write a ring as a (quotient) of a polynomial ring over ZZ or a prime field",
     Usage => "(S,F) = flattenRing R",
     Inputs => {
	  "R",
	  CoefficientRing => Ring => "the desired coefficient ring for the result"
	  },
     Outputs => {
	  "S" => Ring => "a ring isomorphic to the original ring",
	  "F" => RingMap => {"the isomorphism from ", TT "R", " to ", TT "S"}
	  },
     PARA{
     	  "If the optional argument is not given, then the coefficient ring of the result is 
     	  either ZZ or the base field."},
     PARA{"The inverse of the isomorphism ", TT "F", " is obtainable with ", TT "F^-1", "."},
     EXAMPLE lines ///
          A = ZZ[a]/(a^2-3)
	  B = A[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
	  (D,F) = flattenRing B
	  F
	  F^-1
     	  describe D	  
	  ///,
     PARA{"In the following example,
     the coefficient ring of the result is the fraction field ", TT "K", "."},	  
     EXAMPLE lines ///
          K = frac(ZZ[a])
	  B = K[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
	  (D,F) = flattenRing B
     	  describe D	  
	  ///,
     PARA{"Once a ring has been declared to be a field with ", TO toField, ", then it will be used as the
     coefficient ring."},
     EXAMPLE lines ///
          L = toField A
	  B = L[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
	  (D,F) = flattenRing(B[s,t])
     	  describe D	  
	  ///,
     PARA{"If a larger coefficient ring is desired, use the optional CoefficientRing parameter."},
     EXAMPLE lines ///
          use L
          C1 = L[s,t];
	  C2 = C1/(a*s-t^2);
	  C3 = C2[p_0..p_4]/(a*s*p_0)[q]/(q^2-a*p_1);
	  (D,F) = flattenRing(C3, CoefficientRing=>C2)
	  describe D
	  (D,F) = flattenRing(C3, CoefficientRing=>ZZ)
	  describe D
          ///,
     SeeAlso => {presentation, coefficientRing, describe}
     }
