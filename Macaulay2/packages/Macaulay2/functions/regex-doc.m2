--- status: done
--- author(s): dan
--- notes: 

document { 
     Key => regex,
     Headline => "regular expression matching",
     Subnodes => {
	  TO (regex,String,String),
	  TO (regex,String,ZZ,String)
	  }
     }

document { 
     Key => (regex,String,String),
     Usage => "z = regex(p,s)",
     Inputs => {
	  "p" => "a regular expression describing a pattern",
	  "s" => "a subject string to be searched"
	  },
     Outputs => {
	  "z" => {"a list of pairs of integers describing the first substring of ", TT "s", " found that
	       matches the pattern, or null if nothing matched"}
	  },
     "The value returned is a list of pairs of integers corresponding to the
     parenthesized subexpressions successfully matched.  
     If no matching substring was found, then ", TO "null", " is returned.
     The first member of each pair is the offset within
     ", TT "s", " of the substring matched, and the second is the length.",
     SeeAlso => {(regex,String,ZZ,String), "regular expressions", "match", "replace"}
     }

document { 
     Key => (regex,String,ZZ,String),
     Usage => "z = regex(p,n,s)",
     Inputs => {
	  "p" => "a regular expression describing a pattern",
	  "n" => {"the offset in ", TT "s", " at which to begin the search"},
	  "s" => "a subject string to be searched"
	  },
     Outputs => {
	  "z" => {"a list of pairs of integers describing the first substring of ", TT "s", " found that 
	       matches the pattern, or null if nothing matched"}
	  },
     "The value returned is a list of pairs of integers corresponding to the
     parenthesized subexpressions successfully matched.  
     If no matching substring was found, then ", TO "null", " is returned.
     The first member of each pair is the offset within
     ", TT "s", " of the substring matched, and the second is the length.",
     EXAMPLE {
	  ///regex("a",0,"a b c a")///,
	  ///regex("a",1,"a b c a")///,
	  },
     SeeAlso => {(regex,String,String), "regular expressions", "match", "replace"}
     }

document {
     Key => "regular expressions",
     PARA {
	  "A regular expression is a string that specifies a pattern that describes a
	  set of matching subject strings.  Regular expressions are constructed
	  inductively as follows.  Ordinary (non-special) characters match themselves.
	  A concatenation of regular expressions matches the concatenation of
	  corresponding matching subject strings.  Regular expressions separated by the
	  character ", TT "|", " match strings matched by any.  Parentheses can be used
	  for grouping, and results about which substrings of the target string matched
	  which parenthesized subexpression of the regular expression can be returned."},
     PARA {
	  "The special characters are those appearing in the following constructions.  The special character ", TT "\\", "
	  may be confusing, as inside a string delimited by quotation marks (", TT "\"...\"", "), you type two of 
	  them to get one, whereas inside a string delimited by triple slashes (", TT "///...///", "),
	  you type one to get one.  Thus regular expressions delimited by triple slashes are more
	  readable."},
     UL {
	  {TT ".", " -- match any character except newline"},
	  {TT "^", " -- match the beginning of the string or the beginning of a line"},
	  {TT "$", " -- match the end of the string or the end of a line"},
	  {TT "*", " -- match previous expression 0 or more times"},
	  {TT "+", " -- match previous expression 1 or more times"},
	  {TT "?", " -- match previous expression 1 or 0 times"},
	  {TT "(...)", " -- subpattern grouping"},
	  {TT "|", " -- match expression to left or expression to right"},
	  {TT "{m,n}", " -- match previous expression at least m and at most n times"},
	  {TT "{,n}", " -- match previous expression at most n times"},
	  {TT "{m,}", " -- match previous expression at least m times"},
	  {TT "\\i", " -- match the same string that the i-th parenthesized subpattern matched"},
	  {TT "[...]", " -- match listed characters, ranges, or classes"},
	  {TT "[^...]", " -- match non-listed characters, ranges, or classes"},
	  {TT "\\b", " -- match word boundary"},
	  {TT "\\B", " -- match within word"},
	  {TT "\\<", " -- match beginning of word"},
	  {TT "\\>", " -- match end of word"},
	  {TT "\\w", " -- match word-constituent character"},
	  {TT "\\W", " -- match non-word-constituent character"},
	  {TT "\\`", " -- match beginning of string"},
	  {TT "\\'", " -- match end of string"}
	  },
     "There are the following character classes.",
     UL {
	  {TT "[:alnum:]", " -- letters and digits"},
	  {TT "[:alpha:]", " -- letters"},
	  {TT "[:blank:]", " -- a space or tab"},
	  {TT "[:cntrl:]", " -- control characters"},
	  {TT "[:digit:]", " -- digits"},
	  {TT "[:graph:]", " -- same as [:print:] except omits space"},
	  {TT "[:lower:]", " -- lowercase letters"},
	  {TT "[:print:]", " -- printable characters"},
	  {TT "[:punct:]", " -- neither control nor alphanumeric characters"},
	  {TT "[:space:]", " -- space, tab, carriage return, newline, vertical tab, and form feed"},
	  {TT "[:upper:]", " -- uppercase letters"},
	  {TT "[:xdigit:]", " -- hexadecimal digits"},
	  },
     "In order to match one of the special characters itself, precede it with a backslash.",
     PARA {"We illustrate the use of regular expressions with ", TO (regex,String,String), "."},
     EXAMPLE {
     	  ///regex("d", "1abcddddeF2")///,
     	  ///regex("d*", "1abcddddeF2")///,
     	  ///regex("d+", "1abcddddeF2")///,
     	  ///regex("d+", "1abceF2")///,
     	  ///regex("cdd+e", "1abcddddeF2")///,
     	  ///regex("cd(d+)e", "1abcddddeF2")///,
     	  ///regex("[a-z]+", "1abcddddeF2")///,
     	  ///regex("[[:alpha:]]+", "Dog cat cat.")///,
	  ///regex("([[:alpha:]]+) *\\1","Dog cat cat.")///,
	  },
     "For complete documentation on regular expressions see the entry for ", TT "regex", " in
     section 7 of the unix man pages, or read the the GNU ", TT "regex", " manual.",
     Subnodes => {
	  "functions that accept regular expressions",
	  TO "match",
	  TO "regex",
	  TO "replace",
	  TO (select, String, String),
	  TO (select, String, String, String)
	  },
     Caveat => {
	  "Regular expression matching is done by calls to a C library, where the convention is that 
	  the end of a string is signalled by a byte containing zero, whereas in Macaulbay 2, strings may contain
	  bytes containing zero.  Hence regular expression parsing and matching ignore any bytes containing zero,
	  as well as any subsequent bytes, potentially yielding surprising results."
	  }
     }

-- doc4.m2:137:     Key => regex, 
