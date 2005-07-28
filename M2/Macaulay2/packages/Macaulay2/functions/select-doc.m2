--- status: TODO
--- author(s): 
--- notes: 


document { 
     Key => select,
     Headline => "select from a list, hash table, or string"
     }

document {
     Key => (select,String,String),
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
     Inputs => {
	  "n",
	  "v",
	  "f"
	  },
     Outputs => {
	  {"a list containing at most ", TT "n", " elements of the list ", TT "v", " 
	       that yield ", TT "true", " when the function ", TT "f", " is applied."}
	  },
     "The order of the elements in the result will be the same as
     in the original list ", TT "v", ".",
     EXAMPLE {
	  ///select(4,0..10,even)///
	  },
     SeeAlso => {(select,ZZ,BasicList,Function)}
     }

document {
     Key => (select,HashTable,Function),
     Headline => "select pairs from a hash table",
     TT "select(v,f)", " -- select pairs of the hash table ", TT "v", "
     that yield ", TT "true", " when the function ", TT "f", " is applied to
     the value.",
     PARA,
     "The hash table should be immutable: to scan the values in a mutable hash
     table, use ", TT "scan(values x, f)", "."
     }

document { 
     Key => (select,HashTable,Function),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document {
     Key => (select,ZZ,HashTable,Function),
     Headline => "select a limited number of pairs from a hash table",
     TT "select(n,v,f)", " -- select at most ", TT "n", " pairs of the hash 
     table ", TT "v", " that yield ", TT "true", " when the function ", TT "f", " 
     is applied to the value.",
     PARA,
     "The hash table should be immutable: to scan the values in a mutable hash
     table, use ", TT "scan(values x, f)", "."
     }
document { 
     Key => (select,ZZ,HashTable,Function),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document {
     Key => (select,BasicList,Function),
     Headline => "select elements from a list",
     TT "select(v,f)", " -- select elements of the list
     ", TT "v", " that yield ", TT "true", " when the function 
     ", TT "f", " is applied.",
     PARA,
     "The order of the elements in the result will be the same as
     in the original list ", TT "v", ", and the class will be the same,
     too.",
     EXAMPLE {
	  "select({1,2,3,4,5}, odd)",
	  "select([1,2,3,4,5], odd)",
	  }
     }
document { 
     Key => (select,BasicList,Function),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
 -- doc.m2:537:     Key => select,
 -- doc.m2:542:     Key => (select,BasicList,Function),
 -- doc.m2:557:     Key => (select,HashTable,Function),
 -- doc.m2:567:     Key => (select,ZZ,BasicList,Function),
 -- doc.m2:578:     Key => (select,ZZ,HashTable,Function),
 -- doc8.m2:1199:     Key => selectInSubring,
