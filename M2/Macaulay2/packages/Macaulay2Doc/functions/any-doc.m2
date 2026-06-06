--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => any,
     Headline => "whether any elements satisfy a specified condition",
     SeeAlso =>{ "scan", "apply", "select", "all", "member"},
     Subnodes => {
	 TO (any, BasicList, Function),
	 TO (any, BasicList, BasicList, Function),
         },
     }
document { 
     Key => {(any,BasicList,Function),(any,ZZ,Function)},
     Headline => "whether any elements of a list satisfy a specified condition",
     Usage => "any(L,f)",
     Inputs => {
	  "L" => {"or ", ofClass {ZZ}, ".  If an integer is given, then the sequence ", TT "0..L-1", " is used."},
	  "f" => Function => "which returns true or false"
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "f", " returns true when applied to any element of ", TT "L", 
	       " and ", TO "false", " otherwise"}
	  },
     EXAMPLE lines ///
     any({1,2,3,4}, even)
     any({1,3,5,7}, even)
     any(20,n -> n == 15)
     ///,
     PARA {
	  "We can test whether a permutation has a fixed point as follows."
	  },
     EXAMPLE lines ///
     fp = x -> any(#x, i -> x#i == i);
     fp {2,3,4,0,1}
     fp {2,4,0,3,1}
     ///,
     SeeAlso => { "scan", "apply", "select", "any", "member" }
     }
document { 
     Key => (any,BasicList,BasicList,Function),
     Headline => "whether any corresponding elements of a pair of lists satisfy a condition",
     Usage => "any(v,w,f)",
     Inputs => {
	  "v" => BasicList,
	  "w" => BasicList,
	  "f" => Function => "a function of two variables that returns true or false"
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "f", " returns true when applied to some pair ", TT "(x,y)", " of corresponding elements of ", TT "v", " and ", TT "w", ",
	       and ", TO "false", " otherwise"}},
     EXAMPLE lines ///
	  any((1,2,3,4),(2,3,4,5), (i,j) -> i>j)
	  any((1,2,5,4),(2,3,4,5), (i,j) -> i>j)
	  any((1,2,5,4),(2,3,4,5), x -> (print x; false))
	  any((1,2,5,4),(2,3,4,5), x -> (print x; true))
	  ///,
     SeeAlso => { "scan", "apply", "select", "any", "member" }
     }
document { 
     Key => (any,HashTable,Function),
     Headline => "whether all key/value pairs in a hash table satisfy a specified condition",
     Usage => "any(H,f)",
     Inputs => {
	  "L" => HashTable,
	  "f" => Function => "which returns true or false"
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "f", "returns true when applied to all key/value pairs of ", TT "H",
	       " and ", TO "false", " otherwise"}
	  },
     EXAMPLE {
	  "any(hashTable{1=>5, 2=>4, 3=>3, 4=>2, 5=>1}, (a,b) -> a == b)",
	  "any(hashTable{1=>4, 2=>3, 3=>2, 4=>1}, (a,b) -> a == b)"
	  },
     Caveat => {},
     SeeAlso =>{ "scan", "apply", "select", "all", "member"}
     }
