--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => {cos, (cos,ZZ), (cos,RR),(cos, QQ)},
     Headline => "compute the cosine",
     Usage => "cos x",
     Inputs => { "x" => RR },
     Outputs => { 
	  RR => { "the cosine of ", TT "x", "" } 
	  },
     EXAMPLE {
	  "cos 2"
	  }
     }
