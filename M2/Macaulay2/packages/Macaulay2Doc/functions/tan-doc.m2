--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => {tan,(tan,CC),(tan,RR), (tan,ZZ),(tan, QQ)},
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
