--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => select,
     Headline => "select from a list, hash table, or string",
     SeeAlso => {partition}
     }

document {
     Key => (select,String,String),
     Headline => "select substrings matching a regular expression from a string",
     Usage => "select(p,s)",
     Inputs => {
	  "p" => "a regular expression describing a pattern",
	  "s" => "a subject string to be searched"
	  },
     Outputs => {
	  {"a list of nonoverlapping substrings of ", TT "s", " matching the pattern ", TT "p"}
	  },
     EXAMPLE {
	  ///sort select("[[:alpha:]]+","Dog, cat, and deer.")///,
	  ///select("^.*$","asdf\nqwer\nqewr")///
	  },
     SeeAlso => {(select,String,String,String), "regular expressions"}
     }

document {
     Key => (select,String,String,String),
     Headline => "select substrings matching a regular expression from a string",
     Usage => "select(p,r,s)",
     Inputs => {
	  "p" => "a regular expression describing a pattern",
	  "r" => "a replacement string",
	  "s" => "a subject string to be searched"
	  },
     Outputs => {
	  {"the list with one entry for each substring of ", TT "s", " matching ", TT "p", ", obtained
	       	from ", TT "r", " by replacing each occurrence of a backslash and
		a digit by the string matching the corresponding parenthesized
		subexpression of ", TT "p"}
	  },
     EXAMPLE {
	  ///select("([a-z]+);","\\1","dog; cat, deer;")///
	  },
     SeeAlso => {(select,String,String), "regular expressions"}
     }

document { 
     Key => (select,ZZ,BasicList,Function),
     Headline => "select a limited number of elements from a list",
     Usage => "select(n,v,f)",
     Inputs => { "n", "v", "f" => {"returning either ", TO "true", " or ", TO "false"}},
     Outputs => {
	  {"a list containing at most ", TT "n", " elements of the list ", TT "v", " 
	       that yield ", TT "true", " when the function ", TT "f", " is applied."}
	  },
     "The order of the elements in the result will be the same as
     in the original list ", TT "v", ".",
     EXAMPLE {
	  ///select(4,0..10,even)///
	  },
     SeeAlso => {(select,BasicList,Function),partition}
     }

document { 
     Key => (select,HashTable,Function),
     Headline => "select part of a hash table",
     Usage => "select(v,f)",
     Inputs => { "v", "f" => {"returning either ", TO "true", " or ", TO "false"} },
     Outputs => {
	  {"whose pairs are those key-value pairs ", TT "(k,w)", " of the hash table ", TT "v", " that
	       yield ", TT "true", " when the function ", TT "f", " is applied to the value ", TT "w", "."}
	  },
     "The hash table ", TT "v", " should be immutable: to scan the values in a mutable hash
     table, use ", TT "scan(values x, f)", ".",
     EXAMPLE {
	  "x = new HashTable from { x => 1, y => 2, z => 3 }",
	  "select(x,odd)"
	  },
     SeeAlso => {partition}
     }

document { 
     Key => (select,ZZ,HashTable,Function),
     Headline => "select a limited number of pairs from a hash table",
     Usage => "select(n,v,f)",
     Inputs => { "n", "v", "f" => {"returning either ", TO "true", " or ", TO "false"} },
     Outputs => {
	  {"whose pairs are those key-value pairs of the hash table ", TT "v", " that
	       yield ", TT "true", " when the function ", TT "f", " is applied to the value,
	       except that at most ", TT "n", " pairs will be selected"}
	  },
     "The hash table ", TT "v", " should be immutable: to scan the values in a mutable hash
     table, use ", TT "scan(values x, f)", ".",
     EXAMPLE {
	  "x = new HashTable from { x => 1, y => 2, z => 3 }",
	  "select(1,x,odd)"
	  },
     SeeAlso => {(select,HashTable,Function), partition}
     }

document { 
     Key => (select,BasicList,Function),
     Headline => "select elements from a list",
     Usage => "select(v,f)",
     Inputs => { "v", "f" => {"returning either ", TO "true", " or ", TO "false"} },
     Outputs => {
	  {"a list of those elements of the list ", TT "v", " that yield ", TT "true", " when the function ", TT "f", " is applied"}
	  },
     "The order of the elements in the result will be the same as
     in the original list ", TT "v", ", and the class of the result 
     will be the same as the class of ", TT "v", ".",
     EXAMPLE {
	  "select({1,2,3,4,5}, odd)",
	  "select([1,2,3,4,5], odd)",
	  },
     SeeAlso => {(select,ZZ,BasicList,Function), partition}
     }
