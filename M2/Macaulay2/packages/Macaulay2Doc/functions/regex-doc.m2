--- status: done
--- author(s): dan
--- notes: 

document { 
     Key => {regex,(regex,String,ZZ,String),(regex,String,ZZ,ZZ,String),(regex,String,String)},
     Headline => "define and evaluate a regular expression",
     Usage => "z = regex(p,n,r,s)\nz = regex(p,n,s)",
     Inputs => {
	  "p" => "a regular expression describing a pattern",
	  "n" => {"the starting position in ", TT "s", " at which to begin the search.  If it is
	       omitted, the search starts at the beginning of the string."},
	  "r" => {"the extent of the search.  If it is omitted, the search extends to the end of the string.
	       If it is 0, then the regular expression matches only at the starting position.
	       If it is negative, then positions to the left of the starting position are examined for matches."
	       },	       
	  "s" => "the subject string to be searched"
	  },
     Outputs => {
	  "z" => {"a list of pairs of integers describing the first substring of ", TT "s", " found that 
	       matches the pattern, or ", TO "null", " if nothing matched.  The substring begins at
	       a position between ", TT "n", " and ", TT "n+r", "."}
	  },
     "The value returned is a list of pairs of integers corresponding to the
     parenthesized subexpressions successfully matched, suitable for use as the first
     argument of ", TO "substring", ".  The first member of each pair is the offset within
     ", TT "s", " of the substring matched, and the second is the length.",
     EXAMPLE lines ///
     s = "The cat is black.";
     word = ////\b([a-z]+)\b////;
     m = regex(word|" "|word,s)
     substring(m#0,s)
     substring(m#1,s)
     substring(m#2,s)
     s = "aa     aaaa";
     m = regex("a+",0,s)
     substring(m#0,s)
     m = regex("a+",2,s)
     substring(m#0,s)
     m = regex("a+",2,3,s)
     s = "line 1\nline 2\nline 3";
     m = regex("^.*$",8,-8,s)
     substring(m#0,s)
     m = regex("^",10,-10,s)
     substring(0,m#0#0,s)
     substring(m#0#0,s)
     m = regex("^.*$",4,-10,s)
     substring(m#0,s)
     m = regex("a.*$",4,-10,s)
     ///,
     SeeAlso => {"regular expressions", 
	 "match", 
	 "replace", 
	 "substring",
	 (select, String, String),
	 (select, String, String, String),
	 "separateRegexp",
 	 "about",
	 "backupFileRegexp",
	 "findFiles",
	 "pretty"
	  }
     }

document {
     Key => "regular expressions",
     Headline => "syntax for regular expressions inside Macaulay2",
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
	  {TT "{m}", " -- match previous expression exactly m times"},
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
     EXAMPLE lines ///
     s = "1abcddddeF2";
     regex("d", s)
     substring(oo#0,s)
     regex("d*", s)
     substring(oo#0,s)
     regex("d+", s)
     substring(oo#0,s)
     regex("d+", "1abceF2")
     regex("cdd+e", s)
     substring(oo#0,s)
     regex("cd(d+)e", s)
     substring(oo#0,s),substring(oo#1,s)
     regex("[a-z]+", s)
     substring(oo#0,s)
     t = "Dog cat cat.";
     regex("[[:alpha:]]+", t)
     regex("([[:alpha:]]+) *\\1",t)
     substring(oo#0,t),substring(oo#1,t)
     ///,
     "For complete documentation on regular expressions see the entry for ", TT "regex", " in
     section 7 of the unix man pages, or read the the GNU ", TT "regex", " manual.",
     Subnodes => {
	  "functions that accept regular expressions",
	  TO "match",
	  TO "regex",
	  TO "replace",
	  TO (select, String, String),
	  TO (select, String, String, String),
	  TO "separateRegexp"
	  },
      PARA{
	  "In addition to the functions mentioned below, regular expressions appear in ",
	  TO "about", ", ",
	  TO "backupFileRegexp", ", ",
	  TO "findFiles", ", and ",
	  TO "pretty", ".",
	  },
     Caveat => {
	  "Regular expression matching is done by calls to a C library, where the convention is that 
	  the end of a string is signalled by a byte containing zero, whereas in Macaulay2, strings may contain
	  bytes containing zero.  Hence regular expression parsing and matching ignore any bytes containing zero,
	  as well as any subsequent bytes, potentially yielding surprising results."
	  }
     }

-- doc4.m2:137:     Key => regex, 
