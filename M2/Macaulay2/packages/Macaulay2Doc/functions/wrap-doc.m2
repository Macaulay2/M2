document {
     Key => wrap,
     Usage => "wrap(wid,sep,s)",
     Inputs => {
	  "wid" => ZZ,
	  "sep" => String,
	  "s" => String
	  },
     Outputs => {
	  { "a string obtained by wrapping the string ", TT "s", ", in case it is wider than the number ", TT "wid", ", so that it occupies multiple lines,
	       separated by lines filled with the single character in the string ", TT "sep", ", if provided"}
	  },
     "The inputs ", TT "wid", " and ", TT "sep", " are optional, and can be given in either order.  The default for ", TT "wid", " is ", TT "printWidth", ",
     and the default for ", TT "sep", " is null.",
     EXAMPLE {
	  ///wrap(10,"abcdefghijklmnopqrstuvwxyz")///,
	  ///wrap(10,"-","abcdefghijklmnopqrstuvwxyz")///
	  }
     }
