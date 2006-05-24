--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented {
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
	  (promote, CC, CC),
	  (promote, MonoidElement, Ring),
	  (promote, MonoidElement, RingElement),
	  (promote, RingElement, RingElement)}

document { 
     Key => {promote,
	  (promote, RingElement, Ring)},
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
     " to an element of ", TT "R", ", via the natural map to ", TT "R", ".
     This is semantically equivalent to creating the natural ring map from
     ", TT "ring f --> R", " and mapping f via this map.",
     EXAMPLE {
	  "R = QQ[a..d]; f = a^2;",
	  "S = R/(a^2-b-1);",
	  "promote(2/3,S)",
	  "F = map(R,QQ);  F(2/3)",
	  "promote(f,S)",
	  "G = map(S,R); G(f)"
	  },
     PARA{},
     "If you wish to promote a matrix, or ideal, or module to another ring, either
     use the natural ring map, or use tensor product of matrices or modules.",
     EXAMPLE {
	  "use R;",
	  "m = gens ideal(a^2,a^3,a^4)",
	  "G m",
	  "m ** S"
	  },
     "A special feature is that if ", TT "f", " is rational, and ", TT "R", " is not
     an algebra over ", TO "QQ", ", then an element of ", TT "R", " is provided
     by attempting the evident division.",
     SeeAlso => {baseRings,
	  lift,
	  liftable,
	  "substitution and maps between rings",
	  (symbol**,Matrix,Ring)
	  }
     }


TEST ///
R = QQ[a..d]
S = R/(a^2-b^2)
T = S[x,y,z]
promote(1/2,S)
1/2 * 1_S
I = ideal(a^3,c^3)
-- (I_0) ** T -- doesn't make sense [dan]
(gens I) ** T


R = QQ[a..d]
f = a^2
S = R/(a^2-b-1)
F = map(S,R)
F (2/3)
G = map(R,S)
G (a^2)
lift(a^2,R)
promote(2/3,S)
promote(f,S)

A = QQ[a,b,c]
B = ZZ
F = map(A,ZZ)
F 3
///
