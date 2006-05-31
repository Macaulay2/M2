--- status: 
--- author(s): M2fest 2005
--- notes: 

document { 
     Key => {compress, (compress,Matrix)},
     Headline => "extract nonzero columns from a matrix",
     Usage => "compress m",
     Inputs => {
	  "m" => Matrix
	  },
     Outputs => {
	  Matrix => {"the matrix consisting of nonzero columns of ", TT "m"}
	  },
     EXAMPLE {
	  "m = matrix {{1,2,0,0},{0,0,1,0}}",
	  "compress m"
	  },
     SeeAlso => {"matrices"}
     }

