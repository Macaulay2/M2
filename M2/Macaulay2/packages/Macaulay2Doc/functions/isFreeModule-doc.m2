--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isFreeModule, (isFreeModule, Thing), (isFreeModule, Module)},
     Headline => "whether something is a free module",
     Usage => "isFreeModule M",
     Inputs => {
	  "M" => Thing
	  },
     Outputs => {
	  Boolean => {TO "true", " if the given representation of ", TT "M", 
	       " is a free module and ", TO "false", " otherwise"}
	  },
     "This function checks if the module ", TT "M", " is equal to its ", TO "ambient", 
     " free module by examining its matrix of ", TO2((generators, Module),"generators"), 
     " and its matrix of ", TO2((relations, Module),"relations"), ".",
     PARA{}, 
     "To determine whether ", TT "M", " is isomorphic to a free module, use ", 
     TO "prune", " ", TT "M", ".",
     EXAMPLE {
	  "R = ZZ/7[x,y];",
      	  "M = kernel vars R",
      	  "isFreeModule M",
      	  "isFreeModule prune M"	  
	  },
     SeeAlso => {ambient,(prune,Module)}
     }
