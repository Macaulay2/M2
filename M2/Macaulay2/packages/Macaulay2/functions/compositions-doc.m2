--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => {compositions, (compositions,ZZ,ZZ)},
     Headline => "compositions of an integer",
     Usage => "compositions(k,n)",
     Inputs => {
	  "k" => ZZ => "a nonnegative integer",
	  "n" => ZZ => "also nonnegative"
	  },
     Outputs => {
	  List => {"of all of the ways of writing ", TT "n", " as a sum 
	       of exactly ", TT "k", " nonnegative integers"}
	  },
     EXAMPLE {
	  "compositions(2,5)"
	  },
     SeeAlso => {partitions, "combinatorial functions"}
     }

