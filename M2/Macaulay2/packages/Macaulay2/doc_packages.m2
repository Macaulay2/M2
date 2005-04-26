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
	  "PACKAGENAME" => String => "the name of the package"
	  },
     Outputs => {
	  Package => "the package just loaded.  This output value is typically ignored."
	  },
     Consequences => {
	  {"Loads the package 'PACKAGENAME' which is in the file 'PACKAGENAME.m2'"}
	  },
    "The file ", TT "PACKAGENAME.m2", " should be on the load ", TO "path", 
    " and should contain a package named ", TT "PACKAGENAME", ".",
    PARA,
    "For example, to load the sample package ", TT "FirstPackage", ":",
     EXAMPLE {
	  ///loadPackage "FirstPackage"///
	  },
     SeeAlso => {"packages", 
	  "an example of a package", 
	  (needsPackage,String),
	  load
	  }
     }

document {
     Key => [loadPackage,DebuggingMode],
     Usage => "loadPackage(...,DebuggingMode=>b)",
     Inputs => {
	  "b" => Boolean => "default value is false"
	  },
     Consequences => {
	  {"if the value ", TT "b", " is true, then the debugger is 
	  entered if an error occurs"}
     },
     EXAMPLE {
	  ///loadPackage("Points", DebuggingMode=>true)///
	  },
     SeeAlso => {
	  debug,
	  "using the debugger"
	  }
     }
document {
     Key => DebuggingMode,
     Headline => "whether to enter the debugger upon error",
     Usage => "loadPackage(...,DebuggingMode=>b)",
     Inputs => {
	  "b" => Boolean => "default value is false"
	  },
     Consequences => {
	  {"if the value ", TT "b", " is true, then the debugger is 
	  entered if an error occurs while loading the package"}
     },
     EXAMPLE {
	  ///loadPackage("FirstPackage", DebuggingMode=>true)///,
	  ///needsPackage("FirstPackage", DebuggingMode=>true)///,
	  ///installPackage("FirstPackage", DebuggingMode=>true)///
	  },
     SeeAlso => {
	  debug,
	  "using the debugger"
	  },
     PARA,     
     TT "DebuggingMode", " -- keyword for an optional argument
     which specifies whether to enter the debugger upon an error
     while loading a file."
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
	  "either the ", TO Package, " just loaded, or ", TO null, " if the package 
	  has already been loaded."}
	  },
     Consequences => {
	  {"Loads the package ", TT "PACKAGENAME", " in the file ", TT "PACKAGENAME.m2"}
         },
    "The file ", TT "PACKAGENAME.m2", " should be on the load ", TO "path", 
    " and should contain a package named ", TT "PACKAGENAME", ".",
    PARA,
    "For example, to load the sample package ", TT "FirstPackage", ":",
     EXAMPLE {
	  ///needsPackage "FirstPackage"///,
	  ///needsPackage "FirstPackage"///	  
	  },
     "The second time the package was not reloaded",
     SeeAlso => {"packages", 
	  "an example of a package", 
	  (loadPackage,String)
	  }
     }
document {
     Key => [needsPackage,DebuggingMode],
     Usage => "needsPackage(...,DebuggingMode=>b)",
     Inputs => {
	  "b" => Boolean => "default value is false"
	  },
     Consequences => {
	  {"if the value ", TT "b", " is true, then the debugger is 
	  entered if an error occurs"}
     },
     EXAMPLE {
	  ///needsPackage("Points", DebuggingMode=>true)///
	  },
     SeeAlso => {
	  debug,
	  "using the debugger"
	  }
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
     Key => {export, (export,Sequence), (export,Symbol)},
     Headline => "package item: export functions",
     Usage => "export(symbol1,symbol2,...)",
     Inputs => {
	  "(symbol1,symbol2,...)" => Sequence => " of symbols."
	  },
     Consequences => {"the symbols in the sequence, which should refer
	  to functions or other symbols defined in the package,
	   are made available 
	  to the user of the package"},
     "A package can contain the code for many functions, only some 
     of which should be made visible to the user. ", TT "export",
     " allows one to specify which are not private symbols or functions.
     For an example see ", TO "an example of a package", ".",
     PARA,
     "Use ", 
     TO exportMutable,
     " to export variables which the user can modify.", 
     SeeAlso => {"packages", "creating packages", (debug,Package)}
     }

document {
     Key => {exportMutable, (exportMutable,Sequence), (exportMutable,Symbol)},
     Headline => "package item: export writable variables",
     Usage => "export(symbol1,symbol2,...)",
     Inputs => {
	  "(symbol1,symbol2,...)" => Sequence => " of symbols."
	  },
     Consequences => {"the symbols in the sequence, which should refer
	  to variables defined in the package,
	   are made available 
	  to the user of the package, in such a way that they may modify their 
	  values"},
     "This package item is needed much less frequently than ", TO export, 
     ".  For an example, see ", TO "an example of a package",
     SeeAlso => {"packages", "creating packages", (debug,Package)}
     }

document {
     Key => beginDocumentation,
     Headline => "package item: start documentation section",
     Usage => "beginDocumentation()",
     Consequences => {
	  "Initiates the documentation section of a package"
	  },
     "Documentation for a package, and tests for the package, are 
     placed after this point in a package file.  This way, documentation can
     be loaded separately, Macaulay 2 examples in the documentation can
     be run, and the whole documentation can be stord in a database.",
     PARA,
     "For an example, see ", TO "an example of a package",
     PARA,
     SeeAlso => {
	  (installPackage,String),
	  (check,Package),
	  "writing documentation"
          }
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
     Caveat => "When creating tests, try to insure that they run relatively quickly.",
     SeeAlso => {
	  beginDocumentation,
	  "packages"
	  }
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
