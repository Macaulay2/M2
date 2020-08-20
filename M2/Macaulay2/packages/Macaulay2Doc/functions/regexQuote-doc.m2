document {
     Headline => "escape special characters in regular expressions",
     Key => {regexQuote, (regexQuote, String)},
     Usage => "regexQuote s",
     Inputs => {
	  "s" => "a string"
	  },
     Outputs => {
	  {"obtained from ", TT "s", " by escaping all of the ",
	   "characters that have special meanings in regular expressions ",
	   "(\\, ^, $, ., |, ?, *, +, (, ), [, ], {, and }) with a \\."}
	  },
     EXAMPLE lines ///
     match("2+2", "2+2")
     regexQuote "2+2"
     match(oo, "2+2")
     ///,
     SeeAlso => {"regular expressions", "regex", "match"}
     }
