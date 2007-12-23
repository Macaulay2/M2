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
     Key => tensor,
     Headline => "tensor product",
     PARA{
	  "This is the same as ", TT "A ** B", " except that options are allowed.
           These optional arguments are only valid in the ", TO (tensor,Ring,Ring), " or ",
           TO (tensor,Monoid,Monoid), " cases."
          }
     }

document {
     Key => {(tensor, Ring, Ring),
	  (tensor, Monoid, Monoid),
	  [tensor,DegreeRank],
	  [tensor,Degrees],
	  [tensor,Inverses],
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
	  DegreeRank => ZZ => {"see the corresponding option in ", TO monoid},
	  Degrees => List => {"see the corresponding option in ", TO monoid},
	  Inverses => Boolean => "can be used, but is not advised",
	  Global => Boolean => "can be used, but is not advised",
	  MonomialOrder => List => {"see the corresponding option in ", TO monoid},
	  MonomialSize => ZZ => {"see the corresponding option in ", TO monoid},
	  SkewCommutative => "ignored by this routine",
	  Variables => {"see the corresponding option in ", TO monoid},
	  VariableBaseName => Symbol => {"see the corresponding option in ", TO monoid},
	  Weights => List => "ignored by this routine",
	  WeylAlgebra => List => "ignored by this routine",
	  Heft => List => {"see the corresponding option in ", TO monoid},
	  },
     Outputs => {
	  Ring => {"the tensor product ", TT "A**B"}
	  },
     PARA{
	  "A and B may be monoids too, in which case the result is too.",
	  },
     PARA {
	  "This is the same as ", TT "A ** B", " except that options are allowed.
	  This method allows many of 
	  the options available for monoids.  See
	  ", TO "monoid", " for details.  This method essentially combines the 
	  variables of A and B into one monoid or ring."},
     EXAMPLE lines ///
     	  kk = ZZ/101
     	  A = kk[a,b]
	  B = kk[c,d,e]
	  ///,
     PARA{
	  "The simplest version is to simply use ", TT "tensor", " or ", TT "**", ":",
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
	  tensor(B,B,Variables=>{x1,y1,x2,y2})
	  describe oo
          ///,
     Caveat => {"Not all of the options for monoid are useful here.  Some are silently ignored."},
     SeeAlso => {describe, degreesRing, degreeLength, symbol**}
     }

TEST /// --Errors in the above code
kk = ZZ/101
A = kk[a,b]
B = kk[c,d,e]

describe (C = tensor(A,B,MonomialOrder=>Eliminate numgens A))
basis(2,C) -- bug
describe (C = tensor(A,B,MonomialOrder=>GRevLex)) 
basis(2,C) -- bug

describe tensor(A,B,Degrees=>{5:1}) -- BUG
describe tensor(A,B,WeylAlgebra=>{a=>c}) -- ignores it	  	  
describe tensor(A,B,DegreeRank=>3) -- weird behavior BUG
describe tensor(A,B,Heft=>{1,1,1}) -- incorrect BUG: should be forced to be the length DegreeRank
describe(C = tensor(A,B,Inverses=>true,MonomialOrder=>RevLex)) -- allowed, but not appropriate here
     
describe tensor(A,B,Weights=>{1,2,3,4,5}) -- ignored?
describe tensor(A,B,Global=>false) -- ??
describe(C = tensor(A,B,SkewCommutative=>true)) -- ignored
///

document {
     Key => {(tensor, Module, Module),
	  (tensor, CoherentSheaf, CoherentSheaf)},
     Usage => "tensor(M,N)",
     Inputs => {
	  "M" => {" or ", ofClass CoherentSheaf},
	  "N" => {" or ", ofClass CoherentSheaf, ", the same class as ", TT "M"}
	  },
     Outputs => {
	  {ofClass{Module,CoherentSheaf}, ", the same class as both ", TT "M", " and ", TT "N"}
	  },
     PARA {
	  "This is a synonym of ", TT "M ** N", ".  None of the options are used 
	  in this case."
	  },
     PARA {
	  "For examples, see ", TO (symbol**,Module,Module), ", and ", 
	  TO(symbol**,CoherentSheaf,CoherentSheaf), "."
	  }
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
