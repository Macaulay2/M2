--- status: 
--- author(s): dan
--- notes: 

document {
     Key => replace, 
     Headline => "regular expression replacement",
     Subnodes => {TO (replace,String,String,String)}
     }

document { 
     Key => (replace,String,String,String),
     Usage => "replace(p,r,s)",
     Inputs => {
	  "p" => "a regular expression describing a pattern",
	  "r" => "a replacement string",
	  "s" => "a subject string to be searched"
	  },
     Outputs => {
	  {"the string obtained from ", TT "s", " by replacing its substrings matching ", TT "p", " by
	       copies of ", TT "r", "."}
	  },
     "If a backslash followed by a digit occurs in ", TT "r", ", then in the result they are replaced
     by the string matching the corresponding parenthesized subexpression of ", TT "p", ".",
     EXAMPLE {
	  ///replace ("[a-z]+", "x", "Dog cat cat.")///,
	  ///replace ("([a-z]+)", "(\\1)", "Dog cat cat.")///,
	  },
     SeeAlso => {"regular expressions", "regex", "replace"}
     }
