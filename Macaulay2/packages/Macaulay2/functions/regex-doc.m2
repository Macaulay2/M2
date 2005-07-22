--- status: done
--- author(s): dan
--- notes: 

document { 
     Key => regex,
     Headline => "regular expression matching",
     }
document { 
     Key => (regex,String,String),
     Headline => "regular expression matching",
     Usage => "z = regex(p,s)",
     Inputs => {
	  "p" => "a regular expression describing a pattern",
	  "s" => "a subject string to be searched"
	  },
     Outputs => {
	  "z" => {"a list of pairs of integers describing the first substring of ", TT "s", " found that matches the pattern"}
	  },
     "The value returned is a list of pairs of integers corresponding to the
     subexpressions successfully matched.  The list has length 0 if no matching substring
     was found.  The first member of each pair is the offset within
     ", TT "s", " of the substring matched, and the second is the length.",
     EXAMPLE {
	  ///regex ("asdf*", "--asdffff--")///,
	  ///regex ("asd(f*)", "--asdffff--")///,
	  ///regex ("asd((f)*)", "--asdffff--")///,
	  ///regex ("asd((f)*)", "--asffff--")///
	  },
     "For complete documentation on regular expressions see the entry for ", TT "regex", " in
     section 7 of the unix man pages, or read the the GNU ", TT "regex", " manual.",
     SeeAlso => {"match", "replace"}
     }

-- doc4.m2:137:     Key => regex, 
