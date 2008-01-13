--- status: Draft
--- author(s): DRG, MES
--- notes: 

undocumented {
     (degreeLength, InexactField),
     (degreeLength,PolynomialRing), 
     (degreeLength,QuotientRing),
     (degreeLength,FractionField)
     }

document {
     Key => {degreeLength,
	  (degreeLength,Module), 
	  (degreeLength,Ideal),
	  (degreeLength,OrderedMonoid), 
     	  (degreeLength,GeneralOrderedMonoid), 
	  (degreeLength,Ring)},
     Headline => "the number of degrees",
     Usage => "degreeLength X",
     Inputs => {"X" => {ofClass Ring, ", ",ofClass Ideal, ", ",ofClass Module, ", or ",ofClass Monoid}
	       },
     Outputs => { ZZ => {"the length of a multidegree vector used as a degree of an element of ", TT "X"}},
     EXAMPLE lines ///
     	  degreeLength ZZ
     	  degreeLength (ZZ[x])
     	  degreeLength (ZZ[x, Degrees => {{1,2,3}}])
     	  degreeLength (GF 9)
     ///,
     SeeAlso => {"degree",degreesRing}
     }
