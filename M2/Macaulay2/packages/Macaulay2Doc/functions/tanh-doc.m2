--- status: DRAFT
--- author(s): L. Gold
--- notes: include example?

document { 
     Key => {tanh, (tanh,InexactNumber)},
     Headline => "compute the hyperbolic tangent",
     Usage => "tanh x\ntanh I",
     Inputs => { 
	  "x" => RR,
      "I" => RRi
	  },
     Outputs => { 
	  RR => { "the hyperbolic tangent of ", TT "x" },
      RRi => { "an interval containing the hyerbolic tangents of the points of ", TT "I" }
	  }
--     EXAMPLE {
--	  }
     }
