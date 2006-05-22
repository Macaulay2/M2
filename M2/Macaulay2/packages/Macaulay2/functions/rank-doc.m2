--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {rank,(rank,CoherentSheaf),(rank,Matrix),(rank,Module),(rank,GradedModule)},
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
 -- doc10.m2:905:     Key => (rank, CoherentSheaf),
 -- doc6.m2:1156:     Key => rank,
 -- overviewB.m2:1038:     Key => "rank of a matrix",
