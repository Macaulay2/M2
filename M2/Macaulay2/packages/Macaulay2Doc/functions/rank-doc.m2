--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {
	  rank,
	 (rank, CoherentSheaf),
	 (rank, GradedModule),
	 (rank, Matrix),
	 (rank, Module),
	 (rank, MutableMatrix)},
     Headline => "compute the rank",
     Usage => "rank M",
     Inputs => {
	  "M" => { 
	       ofClass Module, ", ", 
	       ofClass CoherentSheaf, ", ", 
	       ofClass GradedModule, ", ", 
	       ofClass ChainComplex, ", or ", 
	       ofClass Matrix
	       }
	  },
     Outputs => { { "the rank of ", TT "M" } },
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = vars R;",
      	  "rank kernel p",
      	  "rank cokernel p",
	  "C = res cokernel p",
	  "rank C"
	  }
     }
