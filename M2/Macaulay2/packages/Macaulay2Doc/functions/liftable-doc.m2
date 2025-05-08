--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented {(liftable, Number, Number), (liftable, Number, RingElement), 
     (liftable, RingElement, Number), (liftable, RingElement, RingElement),
     (liftable, QQ, ZZ),
     (liftable, CC, RR_*),
     (liftable, Number, InexactNumber),
     (liftable, RRi, QQ),(liftable, RRi, RR),(liftable, RRi, ZZ)}

document { 
     Key => {liftable},
     Headline => "whether lifting to another ring is possible",
     Usage => "liftable(f,R)",
     Inputs => {
	  "f" => { ofClass{RingElement,Matrix} },
	  "R" => Ring
	  },
     Outputs => {
	  Boolean => {"whether ", TT "f", " can be lifted to the ring ", TT "R", ""}
	  },
     "The ring ", TT "R", " should be one of the base rings associated with the
     ring of ", TT "f", ".",
     EXAMPLE lines ///
	  R = ZZ[x]
	  liftable ((x-1)*(x+1)-x^2, ZZ)
	  ///,
     EXAMPLE lines ///
     	  liftable(3/4,ZZ)
	  liftable((3/4)*4,ZZ)
          ///,
     SeeAlso => {lift, baseRings}
     }

