--- status: DRAFT
--- author(s): L.Gold
--- notes: 

document { 
     Key => all,
     Headline => "whether all elements satisfy a specified condition",
     Usage => "all(V,f)",
     SeeAlso => { "scan", "apply", "select", "any", "member" }
     }
document { 
     Key => (all,BasicList,Function),
     Headline => "whether all elements of a list satisfy a specified condition",
     Usage => "all((L,f)",
     Inputs => {
	  "L" => BasicList => "",
	  "f" => Function => "which returns true or false"
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "f", " returns true when applied to every element of ", TT "L", 
	       " and ", TO "false", " otherwise"}
	  },
     EXAMPLE {
	  "all({1,2,3,4}, even)",
	  "all({2,4,6,8}, even)"
	  },
     SeeAlso => { "scan", "apply", "select", "any", "member" }
     }
document { 
     Key => (all,HashTable,Function),
     Headline => "whether all key/value pairs in a hash table satisfy a specified condition",
     Usage => "all(H,f)",
     Inputs => {
	  "H" => HashTable => "",
	  "f" => Function => ", which returns true or false"
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "f", "returns true when applied to all key/value pairs of ", TT "H",
	       " and ", TO "false", " otherwise"}
	  },
     EXAMPLE {
	  "all(hashTable{1=>3, 2=>2, 3=>1}, (a,b) -> (a==b))",
	  "all(hashTable{1=>1, 2=>2, 3=>3}, (a,b) -> (a==b))"
	  },
     SeeAlso => { "scan", "apply", "select", "any", "member" }
     }


