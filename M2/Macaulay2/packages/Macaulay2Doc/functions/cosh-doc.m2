--- status: DRAFT
--- author(s): L. Gold
--- notes: include example?

document { 
     Key => {cosh, (cosh,ZZ),(cosh,RR),(cosh, QQ)},
     Headline => "compute the hyperbolic cosine",
     Usage => "cosh x",
     Inputs => { "x" => RR },
     Outputs => { 
	  RR => { "the hyperbolic cosine of ", TT "x" } 
	  }
--    EXAMPLE {
--	  }
     }
