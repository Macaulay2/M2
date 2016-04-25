--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {dismiss,(dismiss,String),(dismiss,Package)},
     Headline => "dismiss a package",
     Usage => "dismiss PKG",
     Inputs => {
	  "PKG" => Package 
	  },
     Consequences => {
	  {"the dictionary of the package ", TT "PKG", " is removed from ", TO "dictionaryPath", ", so the symbols
	       exported by ", TT "PKG", " are no longer available for use"  }
	  },
     PARA {
	  "The package itself is still accessible under its name in the ", TO "PackageDictionary", ", and its exported
	  symbols can be made available again with ", TO "use", "."
	  },
     EXAMPLE lines ///
     	  newPackage "PKG"
	  export "x1"
	  x1=3
	  endPackage "PKG"
	  dictionaryPath
	  x1
	  dismiss PKG
	  dictionaryPath
	  x1
	  values PackageDictionary
     	  use PKG
	  x1
     ///,
     SeeAlso => { "dictionaryPath", "PackageDictionary", "use" }
     }

