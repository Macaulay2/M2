--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => isGlobalSymbol,
     Headline => "whether a global symbol with a given name exists",
     Usage => "isGlobalSymbol s",
     Inputs => { "s" => String },
     Outputs => { Boolean => "whether a global symbol with the name contained in the string ", TT "s", " has been defined" },
     "The search for the symbol runs through the dictionaries in the list ", TO "dictionaryPath", ".",
     EXAMPLE lines ///
	  isGlobalSymbol "res"
	  isGlobalSymbol "resres"
     ///,
     SeeAlso => { "dictionaryPath", getGlobalSymbol }
     }
