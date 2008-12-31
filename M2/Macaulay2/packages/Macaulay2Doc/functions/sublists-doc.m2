document {
     Key => { sublists,
	  (sublists, VisibleList, Function, Nothing),
	  (sublists, VisibleList, Function, Function, Function),
	  (sublists, VisibleList, Function),
	  (sublists, VisibleList, Function, Function),
	  (sublists, VisibleList, Function, Function, Nothing),
	  (sublists, VisibleList, Function, Nothing, Function),
	  (sublists, VisibleList, Function, Nothing, Nothing)
	  },
     Headline => "process interspersed sublists of a list",
     Usage => "sublist(x,f,g,h)",
     Inputs => { "x", "f", "g", "h" },
     Outputs => {
	  { "a list of results obtained by applying ", TT "g", " to each of the maximal nonempty sequences
	       of consecutive elements ", TT "i", " of ", TT "x", " for which ", TT "f i", " is true, and by 
	       applying ", TT "h", " to each of the elements ", TT "i", " of ", TT "x", " for which ", TT "f i", " is false."}
	  },
     PARA {
	  "If ", TT "g", " or ", TT "h", " is omitted, it is assumed to be the identity."
	  },
     EXAMPLE {
	  "sublists( (1,2,3,5,7,8,10,12,13,17,18,20,21), odd, toList, minus)"
	  }
     }
