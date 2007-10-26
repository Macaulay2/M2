--- status: Draft
--- author(s):Giulio
--- notes: 

document { 
     Key => {intersect,(intersect, List),(intersect,Sequence)},
     Headline => "compute an intersection",
     Usage => "intersect (M,N,...,P) ",
     Inputs => { Nothing => { TT "(M,N,...,P)", ", ", ofClass {List, Sequence}, " of modules or ideals that are submodules of the same module
	       or ideals in the same ring" } },
     Outputs => { {ofClass Module, " or ", ofClass Ideal," that is the intersection of the elements in the list or in the sequence."} },
     "This function calculates the intersection of submodules of the same free module, or of ideals in the same ring.",
     PARA "The following example computes the intersection of a sequence of ideals.",
     EXAMPLE {
	  "R=ZZ/101[a..d];",
	  "I=intersect(ideal(a,b),ideal(b,c),ideal(c,d),ideal(d,a))"
	  },
     PARA "The following example computes the intersection of a list of modules.",
     EXAMPLE {
	  "R=ZZ[x,y,z];",
	  "M=image matrix{{3*x},{3*x}};", 
	  "N=image matrix{{5*y},{5*y}};",
	  "P=image matrix{{7*z},{7*z}};",
	  "intersect{M,N,P}"
	  },
     PARA {"The command ", TO "intersect", " will only work with proper
     ideals. To intersect an ideal with a ring, use ",
     TO "selectInSubring", "."}
     }
