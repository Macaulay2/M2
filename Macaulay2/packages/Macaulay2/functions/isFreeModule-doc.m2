--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isFreeModule, (isFreeModule, Thing), (isFreeModule, Module)},
     Headline => "whether something is a free module",
     Usage => "isFreeModule M",
     Inputs => {
	  "M" => Thing => ""
	  },
     Outputs => {
	  {TO "true", " if ", TT "M", " is a evidently a free module and ",
	       TO "false", " otherwise"}
	  },
     "No computation is done --- the ", TO2("Module", "module"), " ", TT "M", 
     " may be free although this function will not detect it.  To try to determine whether ",
     TT "M", " is isomorphic to a free module, one may ", TO "prune", " ", TT "M", " first.",
     EXAMPLE {
	  "R = ZZ/7[x,y];",
      	  "M = kernel vars R",
      	  "isFreeModule M",
      	  "isFreeModule prune M"	  
	  },
     SeeAlso => {(prune,Module)}
     }
