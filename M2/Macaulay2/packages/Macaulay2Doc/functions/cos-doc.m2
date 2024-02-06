--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => {cos, (cos,CC),(cos,RR),(cos, RRi)},
     Headline => "compute the cosine",
     Usage => "cos x\ncos I",
     Inputs => { "x" => RR,"I"=>RRi},
     Outputs => { 
	  RR => { "the cosine of ", TT "x"},
      RRi => { "an interval containing the cosines of points of ", TT "I"}
	  },
     EXAMPLE {
	  "cos 2"
	  }
     }
