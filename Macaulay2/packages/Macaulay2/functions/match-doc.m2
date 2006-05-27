--- status: 
--- author(s): dan
--- notes: 

document {
     Key => match, 
     Headline => "regular expression matching",
     Subnodes => {TO (match,String,String)}
     }

document { 
     Key => (match,String,String),
     Usage => "match(p,s)",
     Inputs => {
	  "p" => "a regular expression describing a pattern",
	  "s" => "a subject string to be searched"
	  },
     Outputs => {
	  {"whether the pattern ", TT "p", " matches a substring of the string ", TT "s", "."}
	  },
     PARA {
     	  "Warning: in version 0.9.2 and earlier ", TO "match", " behaved differently, and 
     	  the arguments were in the other order."},
     EXAMPLE {
	  ///match ("asd", "--asd--")///,
	  ///match ("asd", "--as--")///
	  },
     SeeAlso => {"regular expressions", "regex", "replace"}
     }
