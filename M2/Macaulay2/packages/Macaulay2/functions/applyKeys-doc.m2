--- status: DRAFT
--- author(s): L.Gold
--- notes:

document { 
     Key => {applyKeys,(applyKeys,HashTable,Function)},
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
     Caveat => {"It is an error for the function ", TT "f", " to return two pairs with the same key."},
     EXAMPLE {
	  "H = new HashTable from {1 => a, 2 => b, 3 => c}",
	  "applyKeys(H, k -> k + 100)",
	  },
     SeeAlso => {"applyPairs", "applyValues", "scanKeys"}
     }


