--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented {
     (promote,CC,CC),
     (promote, Ideal, Number),
     (promote, Ideal, RingElement),
     (promote, List, CC, CC),
     (promote, List, QQ, CC),
     (promote, List, QQ, QQ),
     (promote, List, QQ, RR),
     (promote, List, RR, CC),
     (promote, List, RR, RR),
     (promote, List, ZZ, CC),
     (promote, List, ZZ, QQ),
     (promote, List, ZZ, RR),
     (promote, List, ZZ, ZZ),
     (promote,Matrix,CC,CC),
     (promote,Matrix,Number),
     (promote,Matrix,QQ,CC),
     (promote,Matrix,QQ,QQ),
     (promote,Matrix,QQ,RR),
     (promote,Matrix,RingElement),
     (promote,Matrix,RR,CC),
     (promote,Matrix,RR,RR),
     (promote,Matrix,ZZ,CC),
     (promote,Matrix,ZZ,QQ),
     (promote,Matrix,ZZ,RR),
     (promote,Matrix,ZZ,ZZ),
     (promote,MonoidElement,RingElement),
     (promote,QQ,CC),
     (promote,QQ,QQ),
     (promote, QQ, RingElement),
     (promote,QQ,RR),
     (promote,RR,CC),
     (promote,RR,RR),
     (promote,ZZ,CC),
     (promote,ZZ,QQ),
     (promote,ZZ,RingElement),
     (promote,ZZ,RR),
     (promote,ZZ,ZZ)
     }

document { 
     Key => {promote},
     Headline => "promote to another ring",
     Usage => "promote(f,R)",
     Inputs => {
	  "f" => {ofClass{RingElement, Ideal, Matrix}, " over some base ring of R"},
	  "R" => Ring
	  },
     Outputs => {
	  RingElement => {"or ", ofClass Matrix, ", over R"},
	  },
     "Promote the given element or matrix ", TT "f",
     " to an element or matrix of ", TT "R", ", via the natural map to ", TT "R", ".
     This is semantically equivalent to creating the natural ring map from
     ", TT "ring f --> R", " and mapping f via this map.",
     EXAMPLE lines ///
	  R = QQ[a..d]; f = a^2;
	  S = R/(a^2-b-1);
	  promote(2/3,S)
	  F = map(R,QQ);  F(2/3)
	  promote(f,S)
	  G = map(S,R); G(f)
	  ///,
     PARA{},
     "If you wish to promote a module to another ring, either
     promote the corresponding matrices,
     use the natural ring map, or use tensor product of matrices or modules.",
     EXAMPLE lines ///
	  use R;
	  I = ideal(a^2,a^3,a^4)
     	  promote(I,S)
	  m = image matrix{{a^2,a^3,a^4}}
	  promote(gens m,S)
	  G m
	  m ** S
	  ///,
     "A special feature is that if ", TT "f", " is rational, and ", TT "R", " is not
     an algebra over ", TO "QQ", ", then an element of ", TT "R", " is provided
     by attempting the evident division.",
     SeeAlso => {baseRings,
	  lift,
	  liftable,
	  "substitution and maps between rings",
	  substitute,
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

-- should we get this to work? (MES, 8/23/06):
          kk = ZZ/32003;
	  substitute(matrix{{12/235}},kk)
	  promote(12/235,kk)
	  12_kk/235_kk
	  lift(oo,QQ)

	  
///
