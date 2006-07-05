--- status: DRAFT
--- author(s): L.Gold
--- notes: 

document { 
     Key => {applyPairs,(applyPairs,HashTable,Function)},
     Headline => "apply a function to each pair in a hash table",
     Usage => "applyPairs(H,f)",
     Inputs => {
  	  "H" => HashTable,
  	  "f" => Function => {"with two arguments, returning a pair or ", TO "null"},
	  },
     Outputs => {
  	  HashTable
	  },
     Caveat => {
	  "It is an error for the function ", TT "f", " to return two pairs with the same key."
	  },
     "The result of ", TT "applyPairs(H,f)",
     " is a new hash table obtained by applying the function ", TT "f",
     " to each key/value pair ", TT "(k,v)", " in ", TT "H", 
     " and storing the resulting key/value pair in the new hash table. ",
     " If the function returns ", TO "null",  " then no action is performed.",
     PARA{},
     "In this example, we show how to produce the hash table corresponding
     to the inverse of a function.",
     EXAMPLE {
	  "x = new HashTable from {1 => a, 2 => b, 3 => c}",
	  "y = applyPairs(x, (k,v) -> (v,k))",
	  "x#2",
	  "y#b",
	  },
     SeeAlso => {"applyKeys", "applyValues", "scanPairs"}
     }
