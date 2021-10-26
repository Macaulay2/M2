--- status: DRAFT
--- author(s): L. Gold
--- notes: should "mapping over lists" be in the see also spot?
--- The apply(HashTable,Function) in the "ways to use" section doesn't seem
--- to have disappeared in my documentation, even though it is obsolete.

document { 
     Key => apply,
     Headline => "apply a function to each element",
     Usage => "apply(L,f) or apply(L1,L2,f)",
     SeeAlso => {"applyKeys", "applyPairs", "applyValues", "applyTable", "lists and sequences"}
     }
document { 
     Key => {(apply,BasicList,Function), (apply,String,Function)},
     Headline => "apply a function to each element of a list",
     Usage => "apply(L,f)",
     Inputs => {
	  "L" => Nothing => ofClass{BasicList,String},
	  "f" => Function => "with one argument",
	  },
     Outputs => {
	  BasicList => 
	       {"obtained by applying ", TT "f", " to each element of ", TT "L"}
	  },
     "The result will have the same class as ", TT "L", ".",
     EXAMPLE {
	  "apply({1,3,5,7}, i -> i^2)",
	  "apply([1,3,5,7], i -> i^2)",
	  "apply((1,3,5,7), i -> i^2)"
     	  },
     "The exception is that for strings, the result will be a sequence.",
     EXAMPLE {///apply("foo", identity)///},
     SeeAlso => {(symbol /, VisibleList, Function), (symbol \,Function,VisibleList)}
     }
document { 
     Key => {(apply,BasicList,BasicList,Function),
	 (apply,BasicList,String,Function),
	 (apply,String,BasicList,Function),
	 (apply,String,String,Function)},
     Headline => "apply a function to pairs of elements, one from each list",
     Usage => "apply(L1,L2,f)",
     Inputs => {
	  "L1" => Nothing => ofClass{BasicList,String},
	  "L2" => Nothing => {ofClass{BasicList,String}, " of the same length as ", TT "L1"},
	  "f" => Function => "with two arguments",
	  },
     Outputs => {
	  BasicList => {
	       "with the ith element obtained by evaluating ", TT "f(L1_i,L2_i)"}
	  },
     "The result will have the same class as the class of ", TT "L1", " and ", TT "L2", ".",
     EXAMPLE {
     	  "apply({1,2,3}, {100,200,300}, (i,j) -> i+j)",
     	  "apply([1,2,3], [100,200,300], (i,j) -> i+j)",
	  "apply((1,2,3), (100,200,300), (i,j) -> i+j)"	  
     	  },
     "The exception is that for strings, the result will be a sequence.",
     EXAMPLE {///apply("foo", "bar", concatenate)///},
     }
document { 
     Key => (apply,ZZ,Function),
     Headline => "apply a function to {0,..., n-1}",
     Usage => "apply(n,f)",
     Inputs => {
	  "n" => ZZ,
	  "f" => Function => "with one argument",
	  },
     Outputs => {
	  List => {"obtained by applying ", TT "f", " to the list of integers {0,..., n-1}"}
	  },
     "The command ", TT " apply(n,f)", " is equivalent to ", TT "apply(toList(0 .. n-1),f)", ".",
     EXAMPLE {
	  "apply(10, i -> i^2)"
     	  },
     }

