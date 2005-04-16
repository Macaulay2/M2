document {
     Key => Package,
     Headline => "the class of all packages",
     "A package is a body of Macaulay2 source code devoted to a 
     particular topic.  Many packages are distributed with Macaulay 2,
     and others are available from various users on the internet.",
     SeeAlso => "writing documentation",
     Subnodes => {
     "Using and loading packages",
	  TO needsPackage,
	  TO loadPackage,
	  TO installPackage,
     "Creating new packages",
     	  TO "package format",
	  TO newPackage,
	  TO export,
	  TO beginDocumentation,
	  TO (TEST,String),
     "Writing documentation for packages",
	  TO document,
     "Debugging your package",
	  TO "using the debugger",
	  TO (debug,Package),
	  TO (check,Package)
	  },
     PARA {
     	  "The directory containing the packages is ", HREF { LAYOUT#"packages",LAYOUT#"packages" }, "."
	  }
     }

document {
     Key => (loadPackage,String),
     Headline => "",     
     Usage => "loadPackage FILENAME",
     Inputs => {
	  "FILENAME" => String => "the name of the file containing the package",
	  DebuggingMode => Boolean => "the debugging mode to use during the loading of the package"
	  },
     Consequences => {
	  "The file, which will typically start with a 'newPackage' command, is loaded."
	  },
     EXAMPLE {
	  ///loadPackage "First"///
	  },
     SeeAlso => { "newPackage", "Package", "load" }
     }

document {
     Key => (needsPackage,String),
     Headline => "load a package if not already loaded",
     Usage => "needsPackage packagename",
     Inputs => {
	  "packagename" => "package name"
	  },
     Consequences => {"Loads the file 'packagename.m2' which should be on the
     load path ", TO "path", ", and should also contain a package named
     'packagename'"},
     EXAMPLE {
	  ///needsPackage "FirstPackage"///
	  },
     Caveat => "",
     SeeAlso => {(installPackage,String)}
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
     "This command should be run after installing a package via ",
     TO (installPackage,String), ".",
     PARA,
     "This command is run each time the program is started, therefore 
     overwriting this file.  Thus, one can simply restart Macaulay2 to
     obtain the new ",
     EXAMPLE {
	  "makePackageIndex()"
	  },
     SeeAlso => {}
     }

document {
     Key => (newPackage,String),
     Headline => "",
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

document {
     Key => exportMutable,
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

document {
     Key => beginDocumentation,
     Headline => "start the documentation section of a package",
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
     Headline => "",
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

document { 
	Key => export,
	Headline => "make global variables visible to the user",
	Usage => "export(function1, function2, ... )",
	Inputs => { Sequence => {} },
	Consequences => {"the functions in the sequence are made available to the user"},
	"A package can contain the code for many functions, only some of which should be made visible to the user. ", TT "export",
	" allows one to decided which variables are global variables. For an example see ", TO "an example of a package", "."
	}
document { Key => exportMutable }

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
     Consequences => "",
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
