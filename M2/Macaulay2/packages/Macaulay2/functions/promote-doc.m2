--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented {(promote,Matrix,ZZ,RR), (promote,Matrix,RRR,RRR), (promote,Matrix,QQ,CCC), 
     (promote,Matrix,ZZ,CC), (promote,Matrix,RRR,CCC), (promote,Matrix,QQ,RR), 
     (promote,Matrix,RR,RRR),
     (promote,Matrix,QQ,CC), (promote,Matrix,CCC,CCC), (promote,Matrix,RR,CCC), 
     (promote,Matrix,RR,RR), (promote,Matrix,CC,CCC), (promote,Matrix,RR,CC), (promote,Matrix,CC,CC),
     (promote,MonoidElement,RingElement), (promote,ZZ,ZZ), (promote,ZZ,QQ), 
     (promote,ZZ,RRR), (promote,QQ,QQ), (promote,QQ,RRR), (promote,ZZ,CCC), 
     (promote,Matrix,Number),
     (promote,RRR,RRR), (promote,QQ,CCC), (promote,ZZ,RR), (promote,ZZ,CC), 
     (promote,RRR,CCC), (promote,QQ,RR), (promote,QQ,CC), (promote,CCC,CCC), 
     (promote,RR,RRR), (promote,RR,CCC),
     (promote,CC,CCC), (promote,RR,RR), (promote,RR,CC), (promote,CC,CC), 
     (promote,ZZ,RingElement), (promote,Matrix,RingElement), (promote,Matrix,ZZ,ZZ), (promote,Matrix,ZZ,QQ),
     (promote,Matrix,QQ,QQ), (promote,Matrix,ZZ,RRR), (promote,Matrix,QQ,RRR), 
     (promote,Matrix,ZZ,CCC),
     (promote, List, QQ, QQ),
     (promote, List, QQ, RRR),
     (promote, List, QQ, CCC),
     (promote, List, QQ, RR),
     (promote, List, QQ, CC),
     (promote, List, CCC, CCC),
     (promote, List, CC, CCC),
     (promote, List, CC, CC),
     (promote, List, ZZ, ZZ),
     (promote, List, ZZ, QQ),
     (promote, List, ZZ, RRR),
     (promote, List, ZZ, CCC),
     (promote, List, RRR, RRR),
     (promote, List, ZZ, RR),
     (promote, List, ZZ, CC),
     (promote, List, RRR, CCC),
     (promote, List, RR, RRR),
     (promote, List, RR, CCC),
     (promote, List, RR, RR),
     (promote, List, RR, CC),     
     (promote, QQ, RingElement)
     }

document { 
     Key => {promote},
     Headline => "promote to another ring",
     Usage => "promote(f,R)",
     Inputs => {
	  "f" => RingElement => {"or ", ofClass Matrix, " over some base ring of R"},
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
     "If you wish to promote an ideal or module to another ring, either
     promote the corresponding matrices,
     use the natural ring map, or use tensor product of matrices or modules.",
     EXAMPLE lines ///
	  use R;
	  m = gens ideal(a^2,a^3,a^4)
	  G m
	  m ** S
	  ideal promote(m, S)
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
