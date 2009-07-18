--- status: 
--- author(s): dan
--- notes: 

document {
     Key => replace, 
     Headline => "replacement in strings and lists"
     }

document { 
     Key => (replace,String,String,String),
     Headline => "regular expression replacement of substrings",
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

document {
     Key => {(replace,ZZ,Thing,VisibleList)},
     Headline => "copy a list, replacing an element",
     Usage => "replace(i,t,x)",
     Inputs => {"i","t","x"},
     Outputs => {{"a copy of the list ", TT "x", " in which ", TT "t", " has replaced the element at position ", TT "i", ".
	       A negative value of ", TT "i", " is taken relative to the end of the list."
	       }},
     EXAMPLE lines ///
     replace(4,t,0..10)
     replace(0,t,0..10)
     replace(10,t,0..10)
     replace(-1,t,0..10)
     replace(-11,t,0..10)
     ///
     }
