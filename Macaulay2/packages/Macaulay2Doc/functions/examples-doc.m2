--- status: DRAFT
--- author(s): M. Stillman
--- notes: 

document { 
     Key => examples,
     Headline => "list the examples in documentation",
     Usage => "examples s",
     Inputs => {
	  "s" => "a descriptor for a documentation node (see below for examples)"
	  },
     Outputs => {
	  Net => {"containing examples of code provided in the documentation of ", TT "s"}
	  },
     "The output is returned as a ", TO Net, ".  Use ", TO print, " to place these on their own lines.",
     EXAMPLE {
	  "examples partitions",
	  "print examples (ideal,Matrix)"
	  },
     SeeAlso => {"reading the documentation", help}
     }

TEST ///
     assert( class examples MutableList === Net )
///

