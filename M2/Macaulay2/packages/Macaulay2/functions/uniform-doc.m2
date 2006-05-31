--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => uniform,
     Headline => "test whether elements of a list are of the same class",
     Usage => "uniform x",
     Inputs => {
	  "x" => List
	  },
     Outputs => {
	  Boolean => {"whether all elements of ", TT "x", " are of the same ", TO "class"}
	  },
     EXAMPLE {
	  "uniform {2,3,5}",
	  "uniform {2,3,x}"
	  }
     }
