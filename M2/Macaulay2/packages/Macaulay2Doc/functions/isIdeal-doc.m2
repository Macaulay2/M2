--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isIdeal, (isIdeal,Thing), (isIdeal,Module),(isIdeal, Ideal)},
     Headline => "whether something is an ideal",
     Usage => "isIdeal I",
     Inputs => {
	  "I" => Thing
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "I", " is either an ", TO2("Ideal", "ideal"), 
	       ", a ", TO2("MonomialIdeal", "monomial ideal"), " or a ", TO2("Module", "module"), 
	       " which is a submodule of a free module of rank 1 with generators in degree 0 and ", 
	       TO "false", " otherwise"}
	  },
     EXAMPLE {
	  "S = QQ[x,y,z];",
	  "I = ideal(x^2, y^2)",
	  "isIdeal I",
	  "J = monomialIdeal I",
	  "isIdeal J",	  	  
	  "R = QQ[a..d]/(a*b*c*d);",
	  "I = ideal(a^2,b^2) * R^1",
	  "isIdeal I",
	  "J = a^2 * R^2 + a*b * R^2",
	  "isIdeal J"
	  },
     SeeAlso => {(ideal, Module)}
     }
