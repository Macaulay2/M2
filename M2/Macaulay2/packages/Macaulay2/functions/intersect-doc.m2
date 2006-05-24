--- status: Draft
--- author(s):Giulio
--- notes: 

document { 
     Key => {intersect,(intersect, List),(intersect,Sequence)},
     Headline => "compute an intersection",
     Usage => "intersect (M,N,...,P) ",
     Inputs => {"M,N,...,P" => {" a ", TO List, " or a ", TO Sequence, " of modules or ideals"} 
	  },
     Outputs => {
	  "M" => {" a ", TO Module, ", respectively an ", TO Ideal," which is the intersection of the 
	       elements in the list or in the sequence."} 
	  },
     "This function calculates the intersection of ",  
     "submodules of the same free module, or of ideals in the same ring.",PARA{},
     "The following example computes the intersection of a sequence of ideals.",
     EXAMPLE {
	  "R=ZZ/101[a..d];",
	  "I=intersect(ideal(a,b),ideal(b,c),ideal(c,d),ideal(d,a))"
	  },
     
     "The following example computes the intersection of a list of modules.",
     EXAMPLE {
	  "R=ZZ[x,y,z];",
	  "M=image matrix{{3*x},{3*x}};", 
	  "N=image matrix{{5*y},{5*y}};",
	  "P=image matrix{{7*z},{7*z}};",
	  "intersect{M,N,P}"
	  },
     
     
--     Caveat => {},
     SeeAlso => {}
     }
--document { 
--     Key => (intersect,List),
--     Headline => "",
--     Usage => "",
--     Inputs => {
--	  },
--     Outputs => {
--	  },
--     Consequences => {
--	  },     
--     "description",
--     EXAMPLE {
--	  },
--     Caveat => {},
--     SeeAlso => {}
--     }
-- doc9.m2:462:     Key => intersect,
-- overviewB.m2:285:     Key => "intersection of ideals",


--document {
--     Key => intersect,
--     Headline => "compute an intersection",
--     TT "intersect(M,N,...)", " -- calculate the intersection of 
--     submodules of the same free module, or of monomial ideals in the same ring."
--     }