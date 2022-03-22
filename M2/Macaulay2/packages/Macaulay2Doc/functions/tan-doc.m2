--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => {tan,(tan,CC),(tan,RR), (tan,RRi)},
     Headline => "compute the tangent",
Usage => "tan x\ntan I",
     Inputs => { 
	  "x" => RR,
      "I" => RRi
	  },
     Outputs => { 
	  RR => { "the tangent of ", TT "x" },
      RRi => { "an interval containing the tangents of the points of ", TT "I" }
	  },
     EXAMPLE {
	  "tan 1"
	  }
     }
