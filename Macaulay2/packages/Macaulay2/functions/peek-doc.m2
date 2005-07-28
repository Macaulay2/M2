--- status: TODO
--- author(s): 
--- notes: 

document {
     Key => peek,
     Headline => "examine contents of an object",
     TT "peek s", " -- displays contents of ", TT "s", " to depth 1, bypassing
     installed methods.",
     PARA,
     EXAMPLE {
	  "t = set {1,2,3}",
      	  "peek t",
      	  "new MutableHashTable from {a=>3, b=>44}",
      	  "peek oo"
	  },
     SeeAlso => "peek'"
     }

document { 
     Key => peek,
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
     Key => (peek',ZZ,Thing),
     Headline => "examine contents of an object",
     Usage => {TT "peek'(n,s)", " or ", TT "peek'_n s"},
     Inputs => {
	  "n" => "",
	  "s" => ""
	  },
     Outputs => {
	  { "a net that displays the contents of ", TT "s", ", bypassing installed formatting and printing methods to depth ", TT "n" }
	  },
     EXAMPLE {
	  "s = factor 112",
      	  "peek s",
      	  "peek'_2 s"
	  },
     PARA {
	  "Some types of things have the notion of depth modified slightly to make the entire structure visible at depth 1, as in the following example, which
	  also shows how to use ", TT "wrap", " with the output from ", TT "peek", "."
	  },
     EXAMPLE "wrap_80 peek documentation resolution",
     SeeAlso => "peek"
     }

 -- doc12.m2:914:     Key => peek,
 -- doc12.m2:929:     Key => (peek',ZZ,Thing),
