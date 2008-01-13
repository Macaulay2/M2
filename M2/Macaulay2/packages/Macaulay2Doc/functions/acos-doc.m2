--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => {acos,(acos,ZZ), (acos,RR),(acos, QQ)},
     Headline => "compute the arccosine", 
     Usage => "acos x",
     Inputs => { 
	  "x" => RR
	  },
     Outputs => { 
	  RR => { "the arccosine (in radians) of ", TT "x"} 
	  },
     EXAMPLE {
	  "acos 0.5"
	  }
     }     
