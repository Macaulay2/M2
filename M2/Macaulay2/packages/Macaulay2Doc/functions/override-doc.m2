document {
     Key => override,
     Headline => "override default values for optional arguments",
     TT "override(defaults,args)", " overrides default values for
     optional arguments present in the argument sequence ", TT "args", ".",
     PARA{
	  "One possibility is for the argument ", TT "defaults", " to be an immutable hash table
	  (of type ", TO "OptionTable", "), and ", TT "args", " should be
     	  a sequence of arguments, some of which are optional arguments of
     	  the form ", TT "x => v", ".  Each such optional argument
     	  is removed from ", TT "args", ", and the value in ", TT "defaults", "
     	  corresponding to the key ", TT "x", " is replaced by ", TT "v", ".
     	  The value returned is the modified pair ", TT "(defaults, args)", ".
	  An error is signalled if the key ", TT "x", " does not occur in ", TT "defaults", "."
	  },
     PARA {
	  "A second possibility is for the argument ", TT "defaults", " to be ", TO "null", ",
	  in which case the keys x are not checked for validity, and no default values
	  are provided.  The main use of this is to separate the optional arguments from
	  the other arguments, which can then be used for dispatching to the correct method."
	  },
     PARA{
	  "This function is intended for internal use only, and is used in the processing
     	  of optional arguments for method functions that accept them."
	  },
     EXAMPLE {
	  "defs = new OptionTable from { a => 1, b => 2 };",
	  "override(defs, (4,b=>6,5))"
	  }
     }
