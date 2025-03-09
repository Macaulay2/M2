--- status: DRAFT
--- author(s): L.Gold
--- notes: 

document { 
     Key => all,
     Headline => "whether all elements satisfy a specified condition",
     SeeAlso => { "scan", "apply", "select", "any", "member" },
     Subnodes => {
	 TO (all, BasicList, Function),
	 TO (all, BasicList, BasicList, Function),
         },
     }
document { 
     Key => {(all,BasicList,Function),(all,ZZ,Function),(all,BasicList)},
     Headline => "whether all elements of a list satisfy a specified condition",
     Usage => "all(L,f)",
     Inputs => {
	  "L" => {"or ", ofClass {ZZ}, ".  If an integer is given, then the sequence ", TT "0..L-1", " is used."},
	  "f" => {"which returns true or false. If omitted, the ", TO identity, " function is used"}
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "f", " returns true when applied to every element of ", TT "L", 
	       " and ", TO "false", " otherwise"}
	  },
     EXAMPLE {
	  "all({1,2,3,4}, even)",
	  "all({2,4,6,8}, even)",
	  "all{true,true,true}",
	  "all{true,true,false}",
	  "all{}",
	  "all(7, x -> x < 10)"
	  },
     SeeAlso => { "scan", "apply", "select", "any", "member" }
     }
document { 
     Key => (all,BasicList,BasicList,Function),
     Headline => "whether all corresponding elements of a pair of lists satisfy a condition",
     Usage => "all(v,w,f)",
     Inputs => {
	  "v" => BasicList,
	  "w" => BasicList,
	  "f" => Function => "a function of two variables that returns true or false"
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "f", " returns true when applied to every pair ", TT "(x,y)", " of corresponding elements of ", TT "v", " and ", TT "w", ",
	       and ", TO "false", " otherwise"}},
     EXAMPLE lines ///
	  all((1,2,3,4),(2,3,4,5), (i,j) -> i<=j)
	  all((1,2,5,4),(2,3,4,5), (i,j) -> i<=j)
	  all((1,2,5,4),(2,3,4,5), x -> (print x; false))
	  all((1,2,5,4),(2,3,4,5), x -> (print x; true))
	  ///,
     SeeAlso => { "scan", "apply", "select", "any", "member" }
     }
document { 
     Key => (all,HashTable,Function),
     Headline => "whether all key/value pairs in a hash table satisfy a specified condition",
     Usage => "all(H,f)",
     Inputs => {
	  "H" => HashTable,
	  "f" => Function => ", which returns true or false"
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "f", "returns true when applied to all key/value pairs of ", TT "H",
	       " and ", TO "false", " otherwise"}
	  },
     EXAMPLE {
	  "all(hashTable{1=>3, 2=>2, 3=>1}, (a,b) -> a == b)",
	  "all(hashTable{1=>1, 2=>2, 3=>3}, (a,b) -> a == b)"
	  },
     SeeAlso => { "scan", "apply", "select", "any", "member" }
     }
