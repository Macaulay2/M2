--- status: DRAFT
--- author(s): L. Gold
--- notes: Is the comment about being used as an example still relevant?

document { 
     Key => {sin,(sin,CC),(sin,RR),(sin, RRi)},
     -- this node is used as an example in the documentation node Inputs and Outputs.
     Headline => "compute the sine",
     Usage => "sin x\nsin I",
     Inputs => { 
	  "x" => RR,
      "I" => RRi,
	  },
     Outputs => { 
	  RR =>  {"the sine of ", TT "x", "" },
      RRi => { "an interval containing the sines of the points of ", TT "I" }
	  },
     EXAMPLE {
		"sin (pi/2)"
		}
	 }
