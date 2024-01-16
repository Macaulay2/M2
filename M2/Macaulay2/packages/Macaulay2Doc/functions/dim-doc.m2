--- status: draft
--- author(s): Decker, Popescu, Smith
--- notes: 

document { 
     Key => dim,
     Headline => "compute the Krull dimension",
     Caveat => {"To compute the dimension of a vector space, 
	one should use ", TO rank, ".",
	PARA{},
	"Over the integers, the computation effectively 
	 tensors first with the rational numbers, yielding the wrong 
	 answer in some cases."},
     SeeAlso => {codim}
     }

document { 
     Key => {(dim,Ring),(dim,FractionField),(dim,GaloisField),(dim,PolynomialRing),(dim,QuotientRing),(dim, InexactField)},
     Usage => "dim R",
     Inputs => {"R"},
     Outputs => {ZZ},
     "Computes the Krull dimension of the given ring.",
     PARA{},
     "The singular locus of a cuspidal plane curve", 
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I =ideal(y^2*z-x^3)",
	  "sing = singularLocus(R/I)",
	  "dim sing"
	  },
     "The exterior algebra is artinian:",
     EXAMPLE {
	  "R = ZZ/101[a,b,SkewCommutative => true]",
	  "dim R"
	  },
     "The Weyl algebra in 2 variables:",
     EXAMPLE {
          "R = ZZ/101[x,dx,y,dy,WeylAlgebra => {x=>dx, y=>dy}];",
	  "dim R"
	  },
     SeeAlso => {codim, "Varieties::dim(AffineVariety)"}
     }
document { 
     Key => (dim,Module),
     Usage => "dim M",
     Inputs => {"M"
	  },
     Outputs => {ZZ
	  },
     "Computes the Krull dimension of the module ", TT "M",
     EXAMPLE {
	  "R = ZZ/31991[a,b,c,d]",
	  "I = monomialCurveIdeal(R,{1,2,3})",
	  "M = Ext^1(I,R)",
	  "dim M",
	  "N = Ext^0(I,R)",
	  "dim N"
	  },
     "Note that the dimension of the zero module is ", TT "-1", ".",
     SeeAlso => {(dim,Ring),(dim,Ideal)}
     }
document { 
     Key => (dim,ProjectiveHilbertPolynomial),
     Headline => "the degree of the Hilbert polynomial",
     Usage => "dim P",
     Inputs => {"P"},
     Outputs => {ZZ},
     "The command ", TO dim,  "is designed so that the result 
     is the dimension of the projective scheme that 
     may have been used to produce the given Hilbert polynomial.",
     EXAMPLE {
	  "V = Proj(QQ[x_0..x_5]/(x_0^3+x_5^3))",
	  "P = hilbertPolynomial V",
	  "dim P"
	  },
      SeeAlso => {hilbertPolynomial, (degree,ProjectiveHilbertPolynomial), (euler,ProjectiveHilbertPolynomial)}
     }
document { 
     Key => {(dim,Ideal),(dim,MonomialIdeal)},
     Usage => "dim I",
     Inputs => {"I"},
     Outputs => {ZZ},
     "Computes the Krull dimension of the base ring of ", TT "I", " mod ", TT "I", ".",
     PARA{},
     "The ideal of 3x3 commuting matrices:",
     EXAMPLE {
	  "R = ZZ/101[x_(0,0)..x_(2,2),y_(0,0)..y_(2,2)]",
	  "M = genericMatrix(R,x_(0,0),3,3)",
	  "N = genericMatrix(R,y_(0,0),3,3)",
	  "I = ideal flatten(M*N-N*M);",
	  "dim I" 
	  },
     "The dimension of a Stanley-Reisner monomial ideal associated to a simplicial complex.", 
     PARA{},
     "A hollow tetrahedron:",
     EXAMPLE {
	  "needsPackage \"SimplicialComplexes\"", 
	  "R = QQ[a..d]",
	  "D = simplicialComplex {a*b*c,a*b*d,a*c*d,b*c*d}",
	  "I = monomialIdeal D",
          "facets D",
          "dim D",
	  "dim I"
	  },
     "Note that the dimension of the zero ideal is ", TT "-1", ".",
     SeeAlso => {ideal, monomialIdeal, "SimplicialComplexes::SimplicialComplexes"}
     }
