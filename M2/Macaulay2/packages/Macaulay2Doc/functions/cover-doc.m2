--- status: moved December 2020

document {
     Key => cover,
     Headline => "get the covering free module",
     TT "cover M", " -- yields the free module whose basis elements correspond
     to the generators of M.",
     SeeAlso => {"ambient", "super"}}

document {
     Key => {(cover,Matrix)},
     Headline => "get the map between the covering free modules",
     Usage => "cover f",
     Inputs => {"f"},
     Outputs => {{"the corresponding map of free modules between the covers of the source and target of ", TT "f" }},
     SeeAlso => {(cover,Module)}
     }

document {
     Key => (cover, Module),
     Headline => "get the covering free module",
     Usage => "F = cover M",
     Inputs => {"M"},
     Outputs => {"F" => {"the free module whose basis elements correspond to the generators of ", TT "M", "."}},
     "The free module ", TT "F", " is the source of the generator matrix 
     of ", TT "M", ".",
     EXAMPLE {
	  "R = QQ[a..f];",
	  "g = matrix{{a,b},{c,d},{e,f}}",
	  "M = subquotient(g,matrix{{b},{c},{d}})",
	  "cover M",
	  "cover M == source generators M"},
     SeeAlso => {(ambient,Module), (super,Module)}}

document {
     Key => {(coverMap,Module),coverMap},
     Headline => "the surjective map from a free module to a module corresponding to the generators",
     Usage => "coverMap M",
     Inputs => {"M"},
     Outputs => {{"the surjective map from a free module to ", TT "M", " corresponding to the generators"}},
     EXAMPLE lines ///
     	  M = image matrix {{2},{0}}
	  f = coverMap M
	  isSurjective f
     ///
     }
