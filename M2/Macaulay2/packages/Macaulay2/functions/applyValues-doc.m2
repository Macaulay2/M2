--- status: DRAFT
--- author(s): L.Gold
--- notes: 

document { 
     Key => {applyValues,(applyValues,HashTable,Function)},
     Headline => "apply a function to each value",
     Usage => "applyValues(H,f)",
     Inputs => {
	  "H" => HashTable,
	  "f" => Function => "with one argument"
	  },
     Outputs => {
	  HashTable => {"obtained by applying ", TT "f", " to each value ", TT "v", 
	       " in ", TT "H"}
	  },
     EXAMPLE {
	  "H = new HashTable from {a => 1, b => 2, c => 3}",
	  "applyValues(H, v -> v + 100)",
	  },
     SeeAlso => {"applyKeys", "applyPairs", "scanValues"
	  }
     }
