--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => {(pivots,Matrix),pivots},
     Headline => "list of pivot locations of a matrix",
     Usage => "pivots f",
     Inputs => {
	  "f"
	  },
     Outputs => {
	  List => {"of positions (r,c) which are the positions of the non-zero lead terms
	       (in each column)"}
	  },
     EXAMPLE lines ///
     	  f = matrix{{1,3,0,0,3,5,2,0,0},{0,0,0,1,3,6,7,8,0}}
	  pivots f
	  ///,
     PARA{"This function is used in the current implementation of the Smith normal form."},
     Caveat => "Should be implemented in the engine.",
     SeeAlso => {smithNormalForm}
     }

