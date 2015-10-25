--- status: DRAFT
--- author(s): L.Gold
--- notes:

document { 
     Key => applyKeys,
     Headline => "apply a function to each key in a hash table"
     }
 
document { 
     Key => (applyKeys,HashTable,Function),
     Headline => "apply a function to each key in a hash table",
     Usage => "applyKeys(H,f)",
     Inputs => {
  	  "H" => HashTable,
  	  "f" => Function => "with one argument",
  	  },
     Outputs => {
  	  HashTable => {"obtained by applying ", TT "f", " to each key ", 
	  TT "k", " in ", TT "H"}
	  },
     Caveat => {"It is an error for the function ", TT "f", " to return two pairs with the same key. If such a case may occur, use ",TO "applyKeys(HashTable,Function,Function)"," instead."},
     EXAMPLE {
	  "H = new HashTable from {1 => a, 2 => b, 3 => c}",
	  "applyKeys(H, k -> k + 100)",
	  },
     SeeAlso => {"applyPairs", "applyValues", "scanKeys"}
     }


document { 
     Key => (applyKeys,HashTable,Function,Function),
     Headline => "apply a function to each key in a hash table with collision handling",
     Usage => "applyKeys(H,f,g)",
     Inputs => {
  	  "H" => HashTable,
  	  "f" => Function => "with one argument",
	  "g" => Function => {"with two arguments to be used to combine values when keys coincide after applying", TT "f"}
  	  },
     Outputs => {
  	  HashTable => {"obtained by applying ", TT "f", " to each key ", 
	  TT "k", " in ", TT "H"}
	  },
     EXAMPLE {
	  "H = new HashTable from {1 => 10, 2 => 15, 3 => 20}",
	  "applyKeys(H, k -> k//2, max)",
	  },
     SeeAlso => {"applyPairs", "applyValues", "scanKeys"}
     }


