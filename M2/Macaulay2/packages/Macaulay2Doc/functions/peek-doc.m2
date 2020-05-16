--- status: TODO
--- author(s): 
--- notes: 

document {
     Key => peek,
     Headline => "examine contents of an object",
     Usage => "peek s",
     Inputs => { "s" },
     Outputs => { { "a net or string that displays the contents of ", TT "s", " to depth 1, bypassing installed methods for displaying the object" } },
     "This function is used during debugging Macaulay2 programs to examine the internal structure of objects.",
     EXAMPLE {
	  "set {1,2,3}",
      	  "peek oo",
      	  "new MutableHashTable from {a=>3, b=>44}",
      	  "peek oo"
	  },
     SeeAlso => "peek'"
     }

undocumented {(peek',ZZ,List), (peek',ZZ,Sequence), (peek',ZZ,HashTable), 
     (peek',ZZ,ZZ), (peek',ZZ,BasicList), (peek',ZZ,Symbol), (peek',ZZ,Net), (peek',ZZ,String),
     (peek',ZZ,Nothing), (peek',ZZ,Hypertext), (peek',ZZ,Dictionary), (peek',ZZ,HypertextParagraph)}

document {
     Key => {(peek',ZZ,Thing),peek'},
     Headline => "examine contents of an object",
     Usage => "peek'(n,s)",
     Inputs => { "n", "s" },
     Outputs => {
	  { "a net that displays the contents of ", TT "s", ", bypassing installed formatting and printing methods to depth ", TT "n" }
	  },
     EXAMPLE {
	  "s = factor 112",
      	  "peek s",
      	  "peek'_2 s"
	  },
     SeeAlso => "peek"
     }

