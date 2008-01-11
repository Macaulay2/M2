--- status: DRAFT
--- author(s): L. Gold
--- notes: Is the comment about being used as an example still relevant?

document { 
     Key => {sin,(sin,ZZ),(sin,RR),(sin, QQ)},
     -- this node is used as an example in the documentation node Inputs and Outputs.
     Headline => "compute the sine",
     Usage => "sin x",
     Inputs => { 
	  "x" => RR
	  },
     Outputs => { 
	  RR =>  {"the sine of ", TT "x", "" }
	  },
     EXAMPLE {
		"sin (pi/2)"
		}
	 }
