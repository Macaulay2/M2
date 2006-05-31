--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isSubmodule, (isSubmodule,Thing), (isSubmodule,Module)},
     Headline =>  "whether a module is evidently a submodule of a free module",
     Usage => "isSubmodule M",
     Inputs => {
	  "M" => Thing
	  },
     Outputs => {
	  {TO "true", " if ", TT "M", " is evidently a submodule of a free module and ",
	       TO "false", " otherwise"}
	  },
     "No computation is done -- ", TT "M", " may be a isomorphic to a submodule of a free", 
     " module although this function will not detect it.",
     EXAMPLE {
	  "R = ZZ/5[a,b,c];",
	  "M = R^3;",
	  "isSubmodule M",
	  "N = ideal(a,b) * M",
	  "isSubmodule N",
	  "N' = ideal(a,b) * (R^1 / ideal(a^2,b^2,c^2))", 
	  "isSubmodule N'"	  
	  }
     }
