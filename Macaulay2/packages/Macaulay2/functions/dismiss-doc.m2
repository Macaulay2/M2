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
	  {"the dictionary of the package ", TT "P", " is removed from ", TO "globalDictionaries", ", so the symbols
	       exported by ", TT "P", " are no longer available for use"  }
	  },
     PARA {
	  "The package itself is still accessible under its name in the ", TO "PackageDictionary", ", and its exported
	  symbols can be made available again with ", TO "use", "."
	  },
     EXAMPLE lines ///
     	  newPackage "P"
	  export x
	  x=3
	  closePackage "P"
	  globalDictionaries
	  x
	  dismiss P
	  globalDictionaries
	  x
	  values PackageDictionary
     	  use P
	  x
     ///,
     SeeAlso => { "globalDictionaries", "PackageDictionary", "use" }
     }

