document {
     Key => Package,
     Headline => "the class of all packages",
     "See ", TO "packages", " for an overview about using and writing packages.",
     PARA { "The directory containing the packages is ", HREF { LAYOUT#"packages",LAYOUT#"packages" }, "." }
     }

document {
     Key => {loadPackage,(loadPackage,String)},
     Headline => "load a package",     
     Usage => "loadPackage PACKAGENAME",
     Inputs => { "PACKAGENAME" => String => "the name of the package" },
     Outputs => { Package => "the package just loaded." },
     Consequences => { {"Loads the package 'PACKAGENAME' which is in the file 'PACKAGENAME.m2'"} },
     PARA { "The file ", TT "PACKAGENAME.m2", " should be on the load ", TO "path", "
    	   and should contain a package named ", TT "PACKAGENAME", "." },
     EXAMPLE {
	  ///loadPackage "FirstPackage"///
	  },
     SeeAlso => {"packages", "an example of a package", needsPackage, load }
     }
-- document {
--      Headline => "whether to enter the debugger upon error",
--      Key => {DebuggingMode,[loadPackage,DebuggingMode],[needsPackage,DebuggingMode],[installPackage,DebuggingMode]},
--      Usage => "loadPackage(...,DebuggingMode=>b)",
--      Inputs => { "b" => Boolean },
--      Consequences => { {"if the value ", TT "b", " is ", TT "true", ", then, while loading the package, the debugger is entered if an error occurs"} },
--      PARA { "This option has the same effect when used with the functions ", TO "needsPackage", " and ", TO "installPackage", "." },
--      EXAMPLE {
-- 	  ///loadPackage("Points", DebuggingMode=>true)///
-- 	  },
--      SeeAlso => { debug, "the debugger" }
--      }
document {
     Key => {needsPackage,(needsPackage,String)},
     Headline => "load a package if not already loaded",
     Usage => "needsPackage PACKAGENAME",
     Inputs => { "PACKAGENAME" => "the name of the package" },
     Outputs => { { "either the ", TO Package, " just loaded, or ", TO null, " if the package has already been loaded."} },
     Consequences => { {"Loads the package ", TT "PACKAGENAME", " in the file ", TT "PACKAGENAME.m2"} },
     PARA { "The file ", TT "PACKAGENAME.m2", " should be on the load ", TO "path", " and should contain a package named ", TT "PACKAGENAME", "." },
     PARA { "For example, to load the sample package ", TT "FirstPackage", ":"},
     EXAMPLE {
	  ///needsPackage "FirstPackage"///,
	  ///needsPackage "FirstPackage"///	  
	  },
     "The second time the package was not reloaded.",
     SeeAlso => {"packages", "an example of a package", loadPackage }
     }
document {
     Key => makePackageIndex,
     Headline => "",
     Usage => "makePackageIndex()",
     Consequences => {"Creates a file 'index.html' in your Macaulay2 directory,
     containing links to the documentation for Macaulay2 and
     all installed packages."},
     "This command may need to be run after installing a package via ", TO installPackage, ".",
     PARA {
	 "This command is run each time the program is started, therefore 
	 overwriting this file.  Thus, one can simply restart Macaulay2 to
	 obtain the same consequence."},
     EXAMPLE {
	  "makePackageIndex()"
	  },
     SeeAlso => {"packages"}
     }

document {
     Key => {export, (export,Sequence), (export,Symbol)},
     Headline => "package item: export functions",
     Usage => "export(symbol1,symbol2,...)",
     Inputs => { { TT "(symbol1,symbol2,...)", ", a sequence of symbols" } },
     Consequences => {"The symbols in the sequence, which should refer
	  to functions or other symbols defined in the package, are made available 
	  to the user of the package, and are marked non-mutable."},
     "A package can contain the code for many functions, only some 
     of which should be made visible to the user.  The function ", TT "export", " 
     allows one to specify which symbols are to be made visible.
     For an example see ", TO "an example of a package", ".",
     PARA{ "Use ", TO exportMutable, " to export symbols whose values the user is permitted to modify." },
     SeeAlso => {debug}
     }

document {
     Key => {exportMutable, (exportMutable,Sequence), (exportMutable,Symbol)},
     Headline => "package item: export writable variables",
     Usage => "exportMutable(symbol1,symbol2,...)",
     Inputs => { Nothing => { TT "(symbol1,symbol2,...)", ", a sequence of symbols"  } },
     Consequences => {
	  {"the symbols in the sequence, which should refer to variables defined in the package,
	       are made available to the user of the package, in such a way that their values may be modified by the user"}
	 },
     "This function is needed much less frequently than ", TO export, ".  For an example, see ", TO "an example of a package",
     SeeAlso => {export, debug}
     }

document {
     Key => beginDocumentation,
     Headline => "package item: start documentation section",
     Usage => "beginDocumentation()",
     Consequences => { "Initiates the documentation section of a package" },
     PARA {
	  "Documentation for a package, and tests for the package, are 
	  placed after this point in a package file.  This way, documentation can
	  be loaded separately, Macaulay 2 examples in the documentation can
	  be run, and the whole documentation can be stored in a database."},
     PARA { "For an example, see ", TO "an example of a package" },
     SeeAlso => { installPackage, check, "writing documentation" }
     }
document {
     Key => {TEST, (TEST,String)},
     Headline => "package item: register a test of the package",
     Usage => "TEST s",
     Inputs => {
	  "s" => String => "containing Macaulay2 code"
	  },
     Outputs => {
	  },
     Consequences => {
	  {"Registers the string ", TT "s", " as a test of the
     	       current package"},
	},
     "This function should only occur in the documentation section 
     of a package.  Use ", TO check, " to run all of the tests
     associated to a package.",
     PARA{},
     "For an example, see ", TO "an example of a package",
     Caveat => "When creating tests, try to insure that they run relatively quickly.",
     SeeAlso => {
	  beginDocumentation,
	  assert
	  }
     }
document {
     Key => {newPackage, (newPackage,String)}, 
     Headline => "start a new package",
     Usage => "newPackage ( \"package name\", ... )",
     Inputs => {
	  },
     Consequences => {"a package is created"},
     PARA { "Here is a template for a typical ", TT "newPackage", " entry in a package."},
     PRE ///newPackage("package name",
    Headline => "one line description",
    Version => 1.0,
    Date => "month XX, 20XX",
    Authors => {{Name => "author", Email => "email", HomePage => "url"}},
    DebuggingMode => true
    )///,
	SeeAlso => {"packages"}
  }

--error: option has no documentation: Macaulay2 :: installPackage(..., PackagePrefix => ...), key: [installPackage, PackagePrefix]
--error: option has no documentation: Macaulay2 :: installPackage(..., Encapsulate => ...), key: [installPackage, Encapsulate]
--error: option has no documentation: Macaulay2 :: installPackage(..., InstallPrefix => ...), key: [installPackage, InstallPrefix]
--error: option has no documentation: Macaulay2 :: installPackage(..., IgnoreDocumentationErrors => ...), key: [installPackage, IgnoreDocumentationErrors]
--error: option has no documentation: Macaulay2 :: installPackage(..., AbsoluteLinks => ...), key: [installPackage, AbsoluteLinks]
--error: option has no documentation: Macaulay2 :: installPackage(..., MakeLinks => ...), key: [installPackage, MakeLinks]
--error: symbol has no documentation: Macaulay2 :: uninstallPackage
--error: method has no documentation: Macaulay2 :: uninstallPackage Package, key: (uninstallPackage, Package)
--error: method has no documentation: Macaulay2 :: uninstallPackage String, key: (uninstallPackage, String)
--error: option has no documentation: Macaulay2 :: uninstallPackage(..., PackagePrefix => ...), key: [uninstallPackage, PackagePrefix]
--error: option has no documentation: Macaulay2 :: uninstallPackage(..., Encapsulate => ...), key: [uninstallPackage, Encapsulate]
--error: option has no documentation: Macaulay2 :: uninstallPackage(..., InstallPrefix => ...), key: [uninstallPackage, InstallPrefix]
--error: option has no documentation: Macaulay2 :: uninstallPackage(..., MakeLinks => ...), key: [uninstallPackage, MakeLinks]
--error: option has no documentation: Macaulay2 :: newPackage(..., InfoDirSection => ...), key: [newPackage, InfoDirSection]
--error: option has no documentation: Macaulay2 :: newPackage(..., Headline => ...), key: [newPackage, Headline]
--error: option has no documentation: Macaulay2 :: newPackage(..., HomePage => ...), key: [newPackage, HomePage]
--error: option has no documentation: Macaulay2 :: newPackage(..., Date => ...), key: [newPackage, Date]
--error: option has no documentation: Macaulay2 :: newPackage(..., DebuggingMode => ...), key: [newPackage, DebuggingMode]
     
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
