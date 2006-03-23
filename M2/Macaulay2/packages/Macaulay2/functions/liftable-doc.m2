--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => {liftable,
	  (liftable,RingElement,Ring)},
     Undocumented => {
     	  (liftable,ZZ,Ring),
	  (liftable,QQ,Ring)},
     Headline => "whether a ring element can be lifted to another ring",
     Usage => "liftable(f,R)",
     Inputs => {
	  "f" => RingElement => "",
	  "R" => Ring => ""
	  },
     Outputs => {
	  Boolean => "whether f can be lifted to the ring R"
	  },
     "The ring ", TT "R", " should be one of the base rings associated with the
     ring of ", TT "f", ".",
     EXAMPLE {
	  "R = ZZ[x]",
	  "liftable ((x-1)*(x+1)-x^2, ZZ)",
	  },
     SeeAlso => {lift, baseRings}
     }

