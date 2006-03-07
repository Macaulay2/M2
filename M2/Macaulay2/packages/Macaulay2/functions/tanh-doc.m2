--- status: DRAFT
--- author(s): L. Gold
--- notes: include example?

document { 
     Key => {tanh, (tanh,ZZ),(tanh,RR)},
     Headline => "compute the hyperbolic tangent",
     Usage => "tanh x",
     Inputs => { 
	  "x" => RR => null 
	  },
     Outputs => { 
	  RR => { "the hyperbolic tangent of ", TT "x" } 
	  }
--     EXAMPLE {
--	  }
     }
