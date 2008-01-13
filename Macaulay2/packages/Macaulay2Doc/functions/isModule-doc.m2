--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isModule, (isModule, Thing), (isModule, Module)},
     Headline => "whether something is a module",
     Usage => "isModule M",
     Inputs => {
	  "M" => Thing
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "M", " is a ", TO2("Module", "module"), " and ", 
	       TO "false", " otherwise"}
	  },
     EXAMPLE {
	   "R = QQ[a..d]/(a*b*c*d);",
	   "isModule R",
	   "M = a^2 * R^2 + a*b * R^2",
	   "isModule M"
	  },
     }

