--- status: DRAFT
--- author(s): 
--- notes: 

document { 
     Key => {ring, (ring,Vector), (ring,RingElement),
	  (ring, GroebnerBasis),(ring, Number),
	  (ring,Module),(ring,Matrix),
	  (ring,MutableMatrix),(ring,Ideal), (ring,CC),(ring,RR),(ring,RRi)
	  },
     Headline => "get the associated ring of an object",
     Usage => "ring M",
     Inputs => {"M" => "an object with a ring associated to it"},
     Outputs => {
	  Ring => "associated to the input object"
	  },
     "For example, ring elements, matrices, ideals, modules, chain complexes,
     varieties, coherent sheaves, etc., all have a base ring naturally associated
     to them.  ",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
	  "ring x",
	  "M = matrix {{2*x, x+y},{y^3, z*y}};",
	  "ring M",
	  "S = QQ[x,y,z];",
	  "ring x",
	  "I = ideal (x*y, y*z);",
	  "ring I"
	  },
    -- Caveat => {},
     SeeAlso => {baseRings, coefficientRing}
     }
