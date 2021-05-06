--- status: DRAFT
--- author(s): MES
--- notes: 

-*
-- TODO
promote(Module,type of InexactNumber')
promote(Module,type of InexactNumber)
promote(Module,type of Number)
promote(Module,type of RingElement)
promote(MutableMatrix,type of InexactNumber')
promote(MutableMatrix,type of InexactNumber)
promote(MutableMatrix,type of Number)
promote(MutableMatrix,type of RingElement)
promote(Vector,type of InexactNumber')
promote(Vector,type of InexactNumber)
promote(Vector,type of Number)
promote(Vector,type of RingElement)
*-

undocumented {(promote,CC,CC_*),
     (promote, Matrix, InexactNumber),(promote, Number, InexactNumber),
     (promote, Ideal, Number),
     (promote, Ideal, RingElement),
     (promote, List, QQ, CC_*),
     (promote, List, QQ, QQ),
     (promote, List, QQ, RR_*),
     (promote, List, RR_*, CC_*),
     (promote, List, RR_*, RR_*),
     (promote, List, CC_*, CC_*),
     (promote, List, ZZ, CC_*),
     (promote, List, ZZ, QQ),
     (promote, List, ZZ, RR_*),
     (promote, List, ZZ, ZZ),
     (promote,Matrix,CC_*,CC_*),
     (promote,Matrix,Number),
     (promote,Matrix,QQ,CC_*),
     (promote,Matrix,QQ,QQ),
     (promote,Matrix,QQ,RR_*),
     (promote,Matrix,RingElement),
     (promote,Matrix,RR_*,CC_*),
     (promote,Matrix,RR_*,RR_*),
     (promote,Matrix,ZZ,CC_*),
     (promote,Matrix,ZZ,QQ),
     (promote,Matrix,ZZ,RR_*),
     (promote,Matrix,ZZ,ZZ),
     (promote,MonoidElement,RingElement),
     (promote,QQ,CC_*),
     (promote,QQ,QQ),
     (promote, QQ, RingElement),
     (promote,QQ,RR_*),
     (promote,RR,CC_*),
     (promote,RR,RR_*),
     (promote, RRi, RRi_*),
     (promote,ZZ,CC_*),
     (promote,ZZ,QQ),
     (promote,ZZ,RingElement),
     (promote,ZZ,RR_*),
     (promote,Matrix,InexactNumber'),
     (promote,ZZ,ZZ),
     (symbol _,CC,ComplexField),
     (symbol _,Constant,InexactFieldFamily),
     (symbol _,Constant,Ring),
     (symbol _,InexactFieldFamily,ZZ),
     (symbol _,Number,InexactFieldFamily),
     (symbol _,QQ,ComplexField),
     (symbol _,QQ,RealField),
     (symbol _,RR,ComplexField),
     (symbol _,RR,RealField),
     (symbol _,ZZ,ComplexField),
     (symbol _,ZZ,RealField),
     (promote,List,QQ,RRi'),
     (promote,List,RR',RRi'),
     (promote,List,ZZ,RRi'),
     (promote,Matrix,QQ,RRi'),
     (promote,Matrix,RR',RRi'),
     (promote,Matrix,ZZ,RRi'),
     (promote,Module,InexactNumber'),
     (promote,Module,InexactNumber),
     (promote,Module,Number),
     (promote,Module,RingElement),
     (promote,MutableMatrix,InexactNumber'),
     (promote,MutableMatrix,InexactNumber),
     (promote,MutableMatrix,Number),
     (promote,MutableMatrix,RingElement),
     (promote,QQ,RRi'),
     (promote,RR,RRi'),
     (promote,Vector,InexactNumber'),
     (promote,Vector,InexactNumber),
     (promote,Vector,Number),
     (promote,Vector,RingElement),
     (promote,ZZ,RRi')
     }

document { 
     Key => {promote,
	  (symbol _, RingElement, Ring),(symbol _,Number,Ring),
	  (promote,RR,QQ)},
     Headline => "promote to another ring",
     Usage => "promote(f,R)",
     Inputs => {
	  "f" => {ofClass{RingElement, Ideal, Matrix}, " over some base ring of R"},
	  "R" => Ring
	  },
     Outputs => {
	  RingElement => {"or ", ofClass Matrix, ", over R"},
	  },
     PARA {
     	  "Promote the given ring element or matrix ", TT "f", " to an element or matrix of ", TT "R", ", via the natural map to ", TT "R", ".
     	  This is semantically equivalent to creating the natural ring map from ", TT "ring f --> R", " and mapping f via this map."
	  },
     EXAMPLE lines ///
     R = QQ[a..d]; f = a^2;
     S = R/(a^2-b-1);
     promote(2/3,S)
     F = map(R,QQ);  F(2/3)
     promote(f,S)
     G = map(S,R); G(f)
     ///,
     PARA {
	  "Promotion of real numbers to rational numbers is accomplished by using all of the bits of
	  the internal representation."
	  },
     EXAMPLE lines ///
     promote(101.,QQ)
     promote(.101,QQ)
     factor denominator oo
     ooo + 0.
     oo === .101
     ///,
     PARA {
	  "For promotion of ring elements, there is the following shorter notation."
	  },
     EXAMPLE ///13_R///,
     PARA{
	  "If you wish to promote a module to another ring, either promote the corresponding matrices,
	  use the natural ring map, or use tensor product of matrices or modules.",
	  },
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
     SeeAlso => {baseRings, lift, liftable, "substitution and maps between rings",
	  substitute, (symbol**,Matrix,Ring) }
     }
