--- status: TODO
--- author(s): 
--- notes: 

undocumented {
     (flattenRing, GaloisField),
     (flattenRing, QuotientRing),
     (flattenRing, PolynomialRing)
     }

document { 
     Key => {(flattenRing, Ring),flattenRing},
     Headline => "write a ring as a (quotient) of a polynomial ring over the ZZ or a prime field",
     Usage => "(S,F,G) = flattenRing R",
     Inputs => {
	  "R"
	  },
     Outputs => {
	  "S" => Ring => "a ring isomorphic to the original ring",
	  "F" => RingMap => {"the isomorphism from ", TT "R", " to ", TT "S"},
	  "G" => RingMap => {"the isomorphism from ", TT "S", " to ", TT "R"}
	  },
     EXAMPLE lines ///
          A = ZZ[a]/(a^2-3)
	  B = A[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
	  (D,F,G) = flattenRing B
     	  describe D	  
	  toExternalString D
	  ///,
     "The coefficient ring of the result is 
     either ZZ or the base field.  In the following example,
     the coefficient ring of the result is the fraction field ", TT "K", ".",
     EXAMPLE lines ///
          K = frac(ZZ[a])
	  B = K[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
	  (D,F,G) = flattenRing B
     	  describe D	  
	  toExternalString D
	  ///,
     "Once a ring has been declared to be a field with ", TO toField, ", then it will be used as the
     coefficient ring.",
     EXAMPLE lines ///
          L = toField A
	  B = L[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
	  (D,F,G) = flattenRing (B[s,t])
     	  describe D	  
	  toExternalString D
	  ///,
     "If a larger coefficient ring is desired, use ",
     EXAMPLE lines ///
          use L
          C1 = L[s,t];
	  C2 = C1/(a*s-t^2);
	  (D,F,G) = flattenRing(C2[p_0..p_4], CoefficientRing=>C1)
	  (D,F,G) = flattenRing(C2[p_0..p_4])	  
	  describe D
          ///,
     SeeAlso => {presentation, coefficientRing}
     }
