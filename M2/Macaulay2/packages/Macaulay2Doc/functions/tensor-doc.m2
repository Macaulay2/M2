--- status: TODO
--- author(s): 
--- notes: 

document {
     Key => (symbol **, Monoid, Monoid),
     Headline => "tensor product of monoids",
     TT "M ** N", " -- tensor product of monoids.",
     PARA{},
     "For complete documentation, see ", TO "tensor", "."}

undocumented {
     (tensor, QuotientRing, QuotientRing),(tensor, PolynomialRing, PolynomialRing),
     (tensor, QuotientRing, PolynomialRing),(tensor, PolynomialRing, QuotientRing)     
     }

document { 
     Key => {tensor,(tensor,Sequence)},
     Headline => "tensor product",
     PARA{
	  "For two arguments, this is the same as ", TT "A ** B", " except that options are allowed.
	  These optional arguments are valid only in the cases ", TO (tensor,Ring,Ring), " or ",
	  TO (tensor,Monoid,Monoid), ".  For multiple arguments, the tensor product is constructed
	  iteratively from the binary tensor products, working from left to right."
          },
     EXAMPLE ///tensor (ZZ^2, ZZ^3, ZZ^4)///
     }

document {
     Key => {(tensor, Ring, Ring),
	  (tensor, Monoid, Monoid),
	  [tensor,DegreeRank],
	  [tensor,Degrees],[tensor,DegreeLift],[tensor,DegreeMap],[tensor,Join],
	  [tensor,Inverses],[tensor,Local],
	  [tensor,Global],
	  [tensor,MonomialOrder],
	  [tensor,MonomialSize],
	  [tensor,SkewCommutative],
	  [tensor,Variables],
	  [tensor,VariableBaseName],
	  [tensor,Weights],
	  [tensor,WeylAlgebra],
	  [tensor,Heft]
	  },
     Usage => "tensor(A,B)",
     Inputs => {
	  "A",
	  "B",
	  DegreeRank => ZZ => {"see ", TO [monoid,DegreeRank]},
	  Degrees => List => {"see ", TO [monoid,Degrees]},
	  Inverses => Boolean => {"see ", TO [monoid,Inverses]},
	  Global => Boolean => {"see ", TO [monoid,Global]},
	  Local => Boolean => {"see ", TO [monoid,Local]},
	  MonomialOrder => List => {"see ", TO [monoid,MonomialOrder]},
	  MonomialSize => ZZ => {"see ", TO [monoid,MonomialSize]},
	  SkewCommutative => "this option is ignored",
	  Variables => {"see ", TO [monoid,Variables]},
	  VariableBaseName => Symbol => {"see ", TO [monoid,VariableBaseName]},
	  Weights => List => "ignored by this routine",
	  WeylAlgebra => List => "ignored by this routine",
	  Heft => List => {"see ", TO [monoid,Heft]},
	  Join => Boolean => {"overrides the corresponding option in ", TT "A", "; see ", TO [monoid,Join]},
	  DegreeMap => Boolean => {"overrides the corresponding option in ", TT "A", "; see ", TO [monoid,DegreeMap]},
	  DegreeLift => Boolean => {"overrides the corresponding option in ", TT "A", "; see ", TO [monoid,DegreeLift]}
	  },
     Outputs => {
	  {"the tensor product of A with B"}
	  },
     PARA {
	  "This is the same as ", TT "A ** B", " except that options are allowed,
	  see ", TO (symbol **, Monoid, Monoid), " and ", TO (symbol **, Ring, Ring), ".
	  This method allows many of the options available for monoids, see
	  ", TO "monoid", " for details.  This method essentially combines the 
	  variables of ", TT "A", " and ", TT "B", " into one monoid or ring."},
     EXAMPLE lines ///
     	  kk = ZZ/101
     	  A = kk[a,b]
	  B = kk[c,d,e]
	  ///,
     PARA{
	  "The simplest version is to simply use ", TO "**", ":",
	  },
     EXAMPLE lines ///
	  describe(A**B)
	  ///,
     PARA{},
     "If you wish to change the variable names:",
     EXAMPLE lines ///
	  describe tensor(A,B,VariableBaseName=>p)
	  describe tensor(A,B,Variables=>{a1,a2,b1,b2,b3})
	  ///,
     PARA{},
     "The tensor product of two singly graded rings is bigraded.
     Sometimes you want a singly graded ring.  Here is one way to get it:",
     EXAMPLE lines ///
	  describe (C = tensor(A,B,DegreeRank=>1,Degrees=>{5:1}))
	  degreeLength C
	  degreesRing C
	  ///,
     PARA{},
     "Packing monomials into smaller space is more efficient, but less flexible.
     The default is 32 bits, so if you want to pack them into 8 bit exponents, use:",
     EXAMPLE lines ///
	  describe tensor(A,B,MonomialSize=>8)
	  ///,
     PARA{},
     "The default monomial order for tensor products is a product order.
     Sometimes other orders are more desirable, e.g. GRevLex, or an elimination order:",
     EXAMPLE lines ///
	  describe (C = tensor(A,B,MonomialOrder=>Eliminate numgens A))
	  describe (C = tensor(A,B,MonomialOrder=>GRevLex)) 
	  ///,
     PARA {
	  "If you tensor two skew-commutative rings, (or one skew commutative ring with
	  a commutative polynomial ring), then all of the skew-commuting variables
          skew commute with each other:",
	  },
     EXAMPLE lines ///
     	  As = kk[a,b,SkewCommutative=>true]
          D = kk[c,d,e,SkewCommutative=>true]
	  E = tensor(As,D)
	  describe E
	  c*a
	  ///,
     PARA {
	  "Similarly, tensoring two Weyl algebras (or one and a polynomial ring) produces
	  a Weyl algebra with both sets of non-commuting pairs.",
	  },
     EXAMPLE lines ///
	  E = kk[x,Dx,WeylAlgebra=>{x=>Dx}]
	  tensor(E,E,Variables=>{x,Dx,y,Dy})
	  describe oo
	  ///,
     PARA {
	  "Two polynomial rings must have the same coefficient ring, otherwise an error
	  is issued.  Currently, there is no way to specify other rings over which to define the
	  tensor product."
	  },
     EXAMPLE lines ///
	  A = ZZ/101[a,b]
	  B = A[x,y]
	  C = tensor(B,B,Variables=>{x1,y1,x2,y2})
	  describe C
          ///,
     "The flat monoid with the all variables visible, including those from the base ring, can
     be obtained as follows.",
     EXAMPLE "C.FlatMonoid",
     Caveat => {"Not all of the options for monoid are useful here.  Some are silently ignored."},
     SeeAlso => {describe, degreesRing, degreeLength, symbol**, FlatMonoid}
     }

document {
     Key => {
	  (tensor, Ring, RingMap, Matrix),
	  (tensor, Ring, RingMap, Module),
	  (tensor, RingMap, Matrix),
	  (tensor, RingMap, Module)
	  },
     Headline => "tensor product via a ring map",
     Usage => "tensor(R,f,M)\ntensor(f,M)",
     Inputs => {
	  "S",
	  "f" => " from R --> S",
	  "M" => {"or ", ofClass Module, " over the source ring ", TT "R", " of ", TT "f"},
	  },
     Outputs => {
	  {ofClass{Matrix,Module}, " the same type as ", TT "M"}
	  },
     PARA{},
     "None of the options are relevant for these uses of tensor.",
     PARA{},
     EXAMPLE lines ///
     	  R = QQ[a..d]
	  S = QQ[s,t]
	  F = map(S,R,{s^4,s^3*t,s*t^3,t^4})
	  f = matrix{{a,b,c,d}}
	  tensor(F,f)
	  tensor(F,image f)
	  ///,
     PARA{
	  "If the ring S is given as an argument, then it must match the target of F,
	  and the result is identical to the version without S given.  The reason it is here is
	  to mimic natural mathematical notation: S **_R M.",
	  },
     EXAMPLE lines ///
	  tensor(S,F,f)
	  tensor(S,F,image f)
	  ///
     }
