--- status: DRAFT
--- author(s): L. Gold
--- notes: include example?

document { 
     Key => {cosh, (cosh,ZZ),(cosh,RR)},
     Headline => "compute the hyperbolic cosine",
     Usage => "cosh x",
     Inputs => { 
	  "x" => RR => null 
	  },
     Outputs => { 
	  RR => { "the hyperbolic cosine of ", TT "x" } 
	  }
--    EXAMPLE {
--	  }
     }
