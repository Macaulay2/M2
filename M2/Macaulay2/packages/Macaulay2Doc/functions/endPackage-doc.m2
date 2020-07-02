document { 
     Key => {(endPackage,String),endPackage},
     Headline => "end a new package",
     Usage => ///endPackage pkgname///,
     Inputs => { "pkgname" => { "the name of the package" } },
     Outputs => { Package => { "the new package" } },
     Consequences => {{ "The package named should have been opened with ", TO "newPackage", ".  The package is closed, and the former value of ", TO "dictionaryPath", "
	       is restored, with the global dictionary (containing the exported symbols) of the package prepended."
	  }},     
     "A package writer need not call this function explicitly, since the end of file hook for the file containing the ", TO "newPackage", " command
     that opened the package will call ", TO "endPackage", ".",
     EXAMPLE lines ///
          newPackage "Foo"
	  export "abc"
	  abc = 3
	  dictionaryPath
	  endPackage "Foo"
	  peek oo
	  dictionaryPath
	  abc
     ///
     }
