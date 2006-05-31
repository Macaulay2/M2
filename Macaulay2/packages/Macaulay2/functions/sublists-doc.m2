document {
     Key => sublists,
     Headline => "process interspersed sublists of a list",
     Usage => "sublist(x,f,g,h)",
     Inputs => {
	  "x" => List,
	  "f" => Function,
	  "g" => Function,
	  "h" => Function
	  },
     Outputs => {
	  { "a list of results obtained by applying ", TT "g", " to each of the elements ", TT "i", " of ", TT "x", " for which ", TT "f i", " is true,
	       and by applying ", TT "h", " to each of the lists, possibly empty, of consecutive elements ", TT "i", " of ", TT "x", " for which ", TT "f i", " is false"}
	  },
     EXAMPLE {
	  "sublists({0,0,1,2,0,3,0},i -> i == 0, identity, identity)",
	  "sublists({0,0,1,2,0,3,0},i -> i != 0, identity, identity)"
	  }
     }
