--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {dismiss,(dismiss,String),(dismiss,Package)},
     Headline => "dismiss a package",
     Usage => "dismiss P",
     Inputs => {
	  "P" => Package 
	  },
     Consequences => {
	  {"the dictionary of the package ", TT "P", " is removed from ", TO "dictionaryPath", ", so the symbols
	       exported by ", TT "P", " are no longer available for use"  }
	  },
     PARA {
	  "The package itself is still accessible under its name in the ", TO "PackageDictionary", ", and its exported
	  symbols can be made available again with ", TO "use", "."
	  },
     EXAMPLE lines ///
     	  newPackage "P"
	  export x1
	  x1=3
	  endPackage "P"
	  dictionaryPath
	  x1
	  dismiss P
	  dictionaryPath
	  x1
	  values PackageDictionary
     	  use P
	  x1
     ///,
     SeeAlso => { "dictionaryPath", "PackageDictionary", "use" }
     }

