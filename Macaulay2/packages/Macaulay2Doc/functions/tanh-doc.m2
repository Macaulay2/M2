--- status: DRAFT
--- author(s): L. Gold
--- notes: include example?

document { 
     Key => {tanh, (tanh,ZZ),(tanh,RR),(tanh, QQ)},
     Headline => "compute the hyperbolic tangent",
     Usage => "tanh x",
     Inputs => { 
	  "x" => RR 
	  },
     Outputs => { 
	  RR => { "the hyperbolic tangent of ", TT "x" } 
	  }
--     EXAMPLE {
--	  }
     }
