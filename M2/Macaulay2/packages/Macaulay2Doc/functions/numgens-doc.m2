--- status: Draft
--- author(s): MES
--- notes: 

undocumented{(numgens, InexactField)}

document { 
     Key => numgens,
     Headline => "the number of generators",
     SeeAlso => {}
     }
document { 
     Key => (numgens,Module),
     Headline => "number of generators of a module",
     Usage => "numgens M",
     Inputs => {
	  "M",
	  },
     Outputs => {
	  ZZ => "number of generators of M"
	  },
     "In Macaulay2, each module comes equipped with a matrix of 
     generators.  It is the number of columns of this matrix 
     which is returned.  If the module is graded, this may or 
     may not be the number of minimal generators.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "M = ker vars R",
     	  "generators M",
	  "numgens M"
	  },
     "The number of generators of a free module is its rank.",
     EXAMPLE {
	  "numgens R^10"
	  },
     SeeAlso => {generators, trim, prune, mingens, ker,vars}
     }
document { 
     Key => (numgens,GeneralOrderedMonoid),
     Headline => "number of generators of a monoid",
     Usage => "numgens M",
     Inputs => {
	  "M"
	  },
     Outputs => {
	  ZZ => "number of generators of M"
	  },
     
     EXAMPLE {
	  "M = monoid[x_1..x_10];",
	  "numgens M"
	  },
     SeeAlso => {monoid}
     }
document { 
     Key => (numgens,Ideal),
     Headline => "number of generators of an ideal",
     Usage => "numgens I",
     Inputs => {
	  "I"
	  },
     Outputs => {
	  ZZ => "number of generators of I"
	  },
     "In Macaulay2, each ideal comes equipped with a matrix of generators.  It is the
     number of columns of this matrix that is returned.  If the ideal is homogeneous,
     this may or may not be the number of minimal generators.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "I = ideal(a^2-b*d, a^2-b*d, c^2, d^2);",
	  "numgens I"
	  },
     PARA{},
     "In order to find a more efficient set of of generators, use ",
     TO mingens, " or ", TO trim, ".",
     EXAMPLE {
	  "mingens I",
	  "numgens trim I"
	  },
     SeeAlso => {mingens, trim, generators}
     }
document { 
     Key => (numgens,CoherentSheaf),
     Headline => "the number of generators of the underlying module",
     Usage => "numgens F",
     Inputs => {
	  "F"
	  },
     Outputs => {
	  ZZ => {"number of generators of the underlying module ", TT "M", " of ", TT "F"}
	  },
     "In Macaulay2, each coherent sheaf comes equipped with a module over
     the coordinate ring.  In the homogeneous case, this is not 
     necessarily the number of generators of the sum of twists ", 
     TT "H^0(F(d))", ", summed over all d, which in fact could be infinitely
     generated.",
     EXAMPLE {
	  "R = QQ[a..d]/(a^3+b^3+c^3+d^3)",
	  "X = Proj R;",
	  "T' = cotangentSheaf X",
     	  "numgens T'",
	  "module T'"
	  },
     SeeAlso => {(module,CoherentSheaf),tangentSheaf}
     }
document { 
     Key => {(numgens,Ring),(numgens, EngineRing),(numgens, FractionField),(numgens, MonomialIdeal),(numgens, PolynomialRing),(numgens, QuotientRing)},
     Headline => "number of generators of a polynomial ring",
     Usage => "numgens R",
     Inputs => {
	  "R"
	  },
     Outputs => {
	  ZZ => "number of generators of R over the coefficient ring"
	  },
     "If the ring ", TT "R", " is a fraction ring or a (quotient of a) polynomial ring, the number
     returned is the number of generators of ", TT "R", " over the coefficient ring.
     In all other cases, the number of generators is zero.",
     EXAMPLE {
	  "numgens ZZ",
	  "A = ZZ[a,b,c];",
	  "numgens A",
	  "KA = frac A",
	  "numgens KA"
	  },
     "If the ring is polynomial ring over another polynomial ring, then only the 
     outermost variables are counted.",
     EXAMPLE {
	  "B = A[x,y];",
	  "numgens B",
	  "C = KA[x,y];",
	  "numgens C",
	  },
     "In this case, use the ", TO "CoefficientRing", " option to ", TO generators, " to obtain the complete set of generators.",
     EXAMPLE {
     	  "g = generators(B, CoefficientRing=>ZZ)",
	  "#g"
	  },
     "Galois fields created using ", TO GF, " have zero generators, but their underlying
     polynomial ring has one generators.",
     EXAMPLE {
	  "K = GF(9,Variable=>a)",
	  "numgens K",
	  "R = ambient K",
	  "numgens R"
	  },
     SeeAlso => {generators, minPres, GF, ambient}
     }
