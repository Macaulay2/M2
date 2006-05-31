--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => {tan,(tan,RR), (tan,ZZ)},
     Headline => "compute the tangent",
     Usage => "tan x",
     Inputs => { 
	  "x" => RR 
	  },
     Outputs => { 
	  RR => { "the tangent of ", TT "x" } 
	  },
     EXAMPLE {
	  "tan 1"
	  }
     }
