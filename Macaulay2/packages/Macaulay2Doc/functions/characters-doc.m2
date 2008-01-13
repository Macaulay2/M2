--- status: DRAFT
--- author(s): from doc2, MES
--- notes: 

document { 
     Key => {characters,(characters,String)},
     Headline => "get characters from a string",
     Usage => "characters s",
     Inputs => {
	  "s" => String
	  },
     Outputs => {
	  List => {"of the characters in the string ", TT "s"}
	  },
     "The characters are represented by strings of length 1.",
     PARA{},
     EXAMPLE "characters \"asdf\"",
     PARA{},
     SeeAlso => {String}
     }

