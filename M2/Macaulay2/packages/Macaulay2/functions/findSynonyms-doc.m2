--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {(findSynonyms,Symbol),findSynonyms},
     Headline => "find synonyms of symbols",
     Usage => "findSynonyms x",
     Inputs => {
	  "x"
	  },
     Outputs => {
	  {"a list of all the names for the symbol appearing in global dictionaries in use" }
	  },
     SourceCode => (findSynonyms,Symbol),
     EXAMPLE lines ///
         findSynonyms symbol res
	 symbol res === symbol resolution
	 res === resolution
	 res
     ///,
     SeeAlso => {"dictionaryPath"}
     }
