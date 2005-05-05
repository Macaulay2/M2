--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isConstant, (isConstant,RingElement), (isConstant, ZZ), (isConstant, QQ), (isConstant, RRR), 
	  (isConstant, RR), (isConstant, CC)
     Headline => "whether a ring element is constant",
     Usage => "isConstant r",
     Inputs => {
	  "r" => RingElement => ""
	  },
     Outputs => {
	  TO "true", " if ", TT "r", is a basic ring 
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
 -- doc1.m2:536:     Key => isConstant,
 -- doc1.m2:540:     Key => (isConstant, RingElement),
