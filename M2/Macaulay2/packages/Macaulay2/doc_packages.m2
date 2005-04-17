document {
     Key => Package,
     Headline => "the class of all packages",
     "See ", TO "packages", " for an overview about using and writing
     packages.",
     PARA {
     	  "The directory containing the packages is ", HREF { LAYOUT#"packages",LAYOUT#"packages" }, "."
	  }
     }

document {
     Key => (loadPackage,String),
     Headline => "load a package",     
     Usage => "loadPackage PACKAGENAME",
     Inputs => {
	  "PACKAGENAME" => String => "the name of the package",
	  DebuggingMode => Boolean => "the debugging mode to use during the loading of the package"
	  },
     Outputs => {
	  Package => "the package just loaded.  Typically not used."
	  },
     Consequences => {
	  {"Loads the package 'PACKAGENAME' which is in the file 'PACKAGENAME.m2'"}
	  },
    "The file ", TT "PACKAGENAME.m2", " should be on the load ", TO "path", 
    " and should contain a package named ", TT "PACKAGENAME", ".",
    PARA,
    "For example, to load the sample package ", TT "FirstPackage", ",",
     EXAMPLE {
	  ///loadPackage "FirstPackage"///
	  },
     SeeAlso => {"packages", "an example of a package", (needsPackage,String),
	  load}
     }

document {
     Key => (needsPackage,String),
     Headline => "load a package if not already loaded",
     Usage => "needsPackage PACKAGENAME",
     Inputs => {
	  "PACKAGENAME" => "the name of the package"
	  },
     Outputs => {
	  {
	  "either the package just loaded, or ", TO null, " if the package 
	  did not need to be loaded."}
	  },
     Consequences => {
	  {"Loads the package ", TT "PACKAGENAME", " in the file ", TT "PACKAGENAME.m2"}
         },
    "The file ", TT "PACKAGENAME.m2", " should be on the load ", TO "path", 
    " and should contain a package named ", TT "PACKAGENAME", ".",
    PARA,
    "For example, to load the sample package ", TT "FirstPackage", ",",
     EXAMPLE {
	  ///needsPackage "FirstPackage"///,
	  ///needsPackage "FirstPackage"///	  
	  },
     SeeAlso => {"packages", "an example of a package", (loadPackage,String)}
     }

document {
     Key => makePackageIndex,
     Headline => "",
     Usage => "makePackageIndex()",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {"Creates a file 'index.html' in your Macaulay2 directory,
     containing links to the documentation for Macaulay2 and
     all installed packages."},
     "This command needs to be run after installing a package via ",
     TO (installPackage,String), ".",
     PARA,
     "This command is run each time the program is started, therefore 
     overwriting this file.  Thus, one can simply restart Macaulay2 to
     obtain the same consequence",
     EXAMPLE {
	  "makePackageIndex()"
	  },
     SeeAlso => {"packages"}
     }

document {
     Key => (newPackage,String),
     Headline => "package item: preamble",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {""},
     "description",
     EXAMPLE {
	  },
     Caveat => "",
     SeeAlso => {}
     }

document {
     Key => export,
     Headline => "package item: export functions",
     Usage => "export(symbol1,...,symbolN)",
     Inputs => {
	  "(symbol1,...,symbolN)" => Sequence => " of symbols to be made 
	  available to those using this package."
	  },
     Consequences => {"the functions in the sequence are made available 
	  to the user of the package"},
     "A package can contain the code for many functions, only some 
     of which should be made visible to the user. ", TT "export",
     " allows one to decided which variables are global variables. 
     For an example see ", TO "an example of a package", ".",
     PARA,
     "Use ", 
     TO exportMutable,
     " to export variables which the user can modify.", 
     SeeAlso => {"packages", "creating packages", (debug,Package)}
     }

document {
     Key => exportMutable,
     Headline => "package item: export writable variables",
     Usage => "exportMutable(symbol1,...,symbolN)",
     Inputs => {
	  "(symbol1,...,symbolN)" => Sequence => " of mutable
	  symbols to be made 
	  available to those using this package."
	  },
     Consequences => {"the functions in the sequence are made available 
	  to the user of the package"},
     "This package item is needed much less frequently that ", TO export, 
     ".  For an example, see ", TO "an example of a package",
     SeeAlso => {"packages", "creating packages", (debug,Package)}
     }

document {
     Key => beginDocumentation,
     Headline => "package item: start documentation section",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },
     "description",
     EXAMPLE {
	  },
     Caveat => "",
     SeeAlso => {}
     }

document {
     Key => (TEST,String),
     Headline => "package item: register a test of the package",
     Usage => "TEST s",
     Inputs => {
	  "s" => String => ", containing Macaulay2 code"
	  },
     Outputs => {
	  },
     Consequences => {"Registers the string ", TT "s", " as a test of the
     current package"},
     "This function should only occur in the documentation section 
     of a package.  Use ", TO (check,Package), " to run all of the tests
     associated to a package.",
     PARA,
     "For an example, see ", TO "an example of a package",
     Caveat => "When creating tests, try to insure that they ",
     SeeAlso => {Package}
     }

document {
     Key => (installPackage,String),
     Headline => "load and install a package and its documentation",
     Usage => "installPackage PACKAGENAME",
     Inputs => {
	  "PACKAGENAME" => "the name of the package"
	  },
     Outputs => {
	  },
     Consequences => {
	  {"Install the package in a local directory, so that in the future, 
	  one may simply use load or loadPackage.  Documentation for the
	  package is also produced, running any Macaulay2 examples that
	  are requested in the package documentation."}
	  },
     HEADER3 {"Actual activity of ", TT "installPackage"},
     "This function first loads the package if it has not been loaded yet.  It then loads
     the documentation for the package, and runs the examples, and then creates the 
     html pages and/or info pages which form the documentation.  It places all of this
     in the application directory, and finally creates symbolic links to this package.",
     PARA,
     "The reason for the complication is to keep each package separate, and to allow the 
     distribution on the internet of both the PACKAGENAME.m2 form of the package and the
     processed form of the package (a whole directory tree).",
     PARA,
     "The many options control how much of this activity is performed or bypassed.",
     SeeAlso => {"packages"}
     }

document {
     Key => [installPackage,MakeInfo],
     Headline => "compute the info pages",
     TT "MakeInfo => true", " -- " 
     }
document {
	Key => newPackage, 
     Headline => "starts the definition of a package",
	Usage => "newPackage ( \"package name\", ... )",
 	Inputs => {},
	Consequences => {"a package is created"},
	"Here is a template for a typical ", TT "newPackage", " entry in a package.",
 	PRE ///
	newPackage(
		"package name",
		Headline => "one line description",
		Version => 1.0,
		Date => "month  XX, 20XX",
		Author => "author <email>",
		HomePage => "url",
		DebuggingMode => true,
		InfoDirSection => ...  -- this option is not normally used
		)
	///,
	SeeAlso => {"writing packages"}
  }

end
---------------------------------
document {
     Key => ,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },
     "description",
     EXAMPLE {
	  },
     Caveat => "",
     SeeAlso => {}
     }

-- markup commands that are useful
-- menus
-- 
     
     
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
