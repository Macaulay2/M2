--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isQuotientModule, (isQuotientModule,Thing), (isQuotientModule,Module)},
     Headline => "whether something is evidently a quotient of a free module",
     Usage => "isQuotientModule M",
     Inputs => {
	  "M" => Thing
	  },
     Outputs => {
	  Boolean => {TO "true", " if the given representation of ", TT "M", " a quotient of a free module."}
	  },
     "This function checks if the module ", TT "M", " is a quotient of its ", TO "ambient", 
     " free module by examining its matrix of ", TO2((generators, Module),"generators"), ".",     
     EXAMPLE {
	  "R = ZZ/101[a,b,c];",
	  "M = R^1/(a^2,b^2,c^2)",
	  "isQuotientModule M"
	  },
     "The image of a map from a free module to the first generator of ", TT "M", " yields an equivalent 
     module that is ", EM "not", " presented as a quotient.",
     EXAMPLE {
	  "f = M_{0}",
	  "N = image f",
	  "M == N",	  
	  "isQuotientModule N",
	  },     
     SeeAlso => {(symbol_, Module, List), ambient, isFreeModule}
     }

