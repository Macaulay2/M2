--- status: 
--- author(s): dan
--- notes: 

document { 
     Headline => "regular expression matching",
     Key => {(match,String,String),match,lastMatch},
     Usage => "match(p,s)",
     Inputs => {
	  "p" => "a regular expression",
	  "s" => "a subject string to be searched"
	  },
     Outputs => {
	  {"whether the regular expression ", TT "p", " matches the string ", TT "s"}
	  },
     Consequences => {
	  {"the variable ", TO "lastMatch", " is set to the value returned by ", TO "regex", ", which
	       is called by ", TO "match"}
	  },
     EXAMPLE lines ///
     match ("asd", "--asd--")
     lastMatch
     match ("asd", "--as--")
     lastMatch
     ///,
     SeeAlso => {"regular expressions", "regex", "replace"}
     }
