--- status: Draft
--- author(s): MES
--- notes: 

document { 
     Key => {symmetricAlgebra,(symmetricAlgebra,Module),
	  (symmetricAlgebra,Matrix),
	  (symmetricAlgebra,Nothing,Nothing,Matrix),
	  (symmetricAlgebra,Nothing,Ring,Matrix),
	  (symmetricAlgebra,Ring,Nothing,Matrix),
	  (symmetricAlgebra,Ring,Ring,Matrix),
	  [symmetricAlgebra, WeylAlgebra],[symmetricAlgebra, VariableBaseName],[symmetricAlgebra,DegreeLift],
	  [symmetricAlgebra, SkewCommutative],[symmetricAlgebra, MonomialSize],[symmetricAlgebra, Weights],
	  [symmetricAlgebra, Local],
	  [symmetricAlgebra, Inverses],
	  [symmetricAlgebra, Constants],
	  [symmetricAlgebra, Heft],
	  [symmetricAlgebra, Global],
	  [symmetricAlgebra, Degrees],
	  [symmetricAlgebra, DegreeMap],
	  [symmetricAlgebra, DegreeRank],
	  [symmetricAlgebra, DegreeGroup],
	  [symmetricAlgebra, MonomialOrder], [symmetricAlgebra, Variables],[symmetricAlgebra,Join]
	  },
     Headline => "the symmetric algebra of a module",
     SYNOPSIS (
	  Usage => "symmetricAlgebra M",
	  Inputs => { "M" => Module },
	  Outputs => { Ring => {"the symmetric algebra of ", TT "M"} },
	  EXAMPLE lines ///
	  R = QQ[a..d];
	  M = image matrix{{a,b,c}}
	  symmetricAlgebra M
	  symmetricAlgebra(R^{1,2,3})
	  ///,
	  PARA{
	       "Most of the optional arguments for monoids (see ", TO (symbol SPACE, Ring,Array), " or ", TO "monoid", ")
	       are available here as well, as in the following example.  They apply to the variables that correspond to the
	       generators of the module."
	       },
	  EXAMPLE lines ///
	  A = symmetricAlgebra(M, Variables=>{x,y,z})
	  describe A
	  B = symmetricAlgebra(M, VariableBaseName=>G, MonomialSize=>16)
	  describe B
	  symmetricAlgebra(M, Degrees=> {3:1})
	  ///
	  ),
     SYNOPSIS (
	  Usage => "symmetricAlgebra f",
	  Inputs => { "f" => Matrix },
	  Outputs => { RingMap => {"the map between symmetric algebras induced by ", TT "f"} },
	  EXAMPLE lines ///
	  symmetricAlgebra vars R
	  ///
	  ),
     SYNOPSIS (
	  Usage => "symmetricAlgebra(A,B,f)",
	  Inputs => { 
	       "A" => Ring => {"the symmetric algebra of the target of ", TT "f", ", previously computed.  Optional."},
	       "B" => Ring => {"the symmetric algebra of the source of ", TT "f", ", previously computed.  Optional."},
	       "f" => Matrix 
	       },
	  Outputs => { RingMap => {"the map between the specified symmetric algebras induced by ", TT "f"} },
	  PARA {
	       "This form of functoriality is needed, because various options are available when computing
	       symmetric algebras."
	       },
	  PARA {
	       "For a linear map that is an isomorphism, and is known to be so, e.g., by having had
	       its inverse computed, the inverse of the corresponding map
	       between symmetric algebras is precomputed and made available."
	       },
	  EXAMPLE lines ///
	  symmetricAlgebra vars R
	  p = symmetricAlgebra(A,B,id_M)
	  p^-1
	  p * p^-1 === id_A
	  p^-1 * p === id_B
	  ///
	  )
     }
