--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => {promote,
	  (promote, RingElement, Ring)},
     Undocumented => {
	  (promote, ZZ, RingElement),
	  (promote, QQ, RingElement),
	  (promote, ZZ, EngineRing),
	  (promote, RR, EngineRing),
	  (promote, ZZ, Ring),
	  (promote, QQ, Ring),
	  (promote, ZZ, ZZ),
	  (promote, ZZ, QQ),
	  (promote, QQ, QQ),
	  (promote, RR, Ring),
	  (promote, CC, Ring),
	  (promote, ZZ, RR),
	  (promote, ZZ, CC),
	  (promote, QQ, RR),
	  (promote, QQ, CC),
	  (promote, Matrix, ZZ),
	  (promote, Matrix, QQ),
	  (promote, RR, RR),
	  (promote, RR, CC),
	  (promote, CC, CC)},
     Headline => "promote to another ring",
     Usage => "promote(f,R)",
     Inputs => {
	  "f" => RingElement => {"in some base ring of R"},
	  "R" => Ring => ""
	  },
     Outputs => {
	  RingElement => "an element of R",
	  },
     "Promote the given element ", TT "f",
     " to an element of ", TT "R", ", via the natural map to ", TT "R", ".",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "f = a^2",
	  "S = R/(a^2-b-1);",
	  "promote(2/3,S)",
	  "promote(f,S)"
	  },
     PARA,
     "If you wish to promote a matrix, or ideal, or module to another ring, use tensor products of matrices
     or modules.",
     EXAMPLE {
	  "gens ideal(f) ** S"
	  },
     "A special feature is that if ", TT "f", " is rational, and ", TT "R", " is not
     an algebra over ", TO "QQ", ", then an element of ", TT "R", " is provided
     by attempting the evident division.",
     PARA,
     "This function is used internally to make the use of several related rings as smooth as possible.",
     SeeAlso => {baseRings,
	  lift,
	  liftable,
	  (symbol**,RingElement,Ring)
	  }
     }

--(promote, MonoidElement, Ring)
--(promote, MonoidElement, RingElement)
--(promote, RingElement, RingElement)

TEST ///
R = QQ[a..d]
S = R/(a^2-b^2)
T = S[x,y,z]
promote(1/2,S)
1/2 * 1_S
I = ideal(a^3,c^3)
(I_0) ** T
(gens I) ** T
///
