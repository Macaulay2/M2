document {
     Key => Package,
     Headline => "the class of all packages",
     "A package is a body of Macaulay2 source code devoted to a 
     particular topic.  Many packages are distributed with Macaulay2, 
     and others are available from various users on the internet.",
     PARA{},
     "See ", TO "packages", " for an overview about using and writing packages.",
     PARA{},
     BOLD "Available packages",
     UL {
	  TO "packages provided with Macaulay2",
	  {"The directory containing the packages is ", HREF { currentLayout#"packages",currentLayout#"packages" }}
	  },
     PARA{},
     BOLD "Functions useful when ", TO "using packages", ".",
     UL {
	  TO needsPackage,
	  TO loadPackage,
	  TO installPackage,
	  TO check,
	  TO debug
	  },
     BOLD "Parts of a package.  See ", TO "creating a package", ".",
     UL {
	  TO newPackage,
	  TO export,
	  TO exportMutable,
	  TO beginDocumentation,
	  TO document,
	  TO TEST
	  },
     BOLD "Functions useful while writing and debugging a package", ".",
     UL {
	  TO "debugging",
	  TO debug,
	  TO check
	  },
     BOLD "Functions that are only rarely used.",
     UL {
	  TO dismiss,
	  TO uninstallPackage,
	  TO use,
	  TO makePackageIndex,
	  TO "loadedPackages"
	  },
     }

document {
     Key => {loadPackage,(loadPackage,String),
	  [loadPackage,FileName],[loadPackage,DebuggingMode],[loadPackage,Reload],[loadPackage,Configuration],[loadPackage,LoadDocumentation]},
     Headline => "load a package",     
     Usage => "loadPackage \"PACKAGENAME\"",
     Inputs => { 
	  "PACKAGENAME" => String => "the name of the package",
	  FileName => String => "the name of the file containing the source code of the package, from which it should be loaded",
	  LoadDocumentation => Boolean => {"whether to load the documentation of the package, too; see ", TO "beginDocumentation"},
	  DebuggingMode => Boolean => {
	       "the value of ", TO "debuggingMode", " during loading the package; specifying it here overrides the 
	       value specified as an option to ", TO "newPackage", " by the package itself; however, if ", TO "debuggingMode", " 
	       is already ", TT "false", ", it will remain so."
	       },
	  Configuration => List => {"a list of options ", TT "KEY => VALUE", ", overriding the defaults specified in 
	       the source code of the package and the (possibly updated) values in the file in the user's application
	       directory."},
	  Reload => Boolean => {"whether to reload the package, if it has been loaded before"}
	  },
     Outputs => { Package => "the package just loaded." },
     Consequences => { {"Loads the package PACKAGENAME that is in the file PACKAGENAME.m2"} },
     PARA { "The file ", TT "PACKAGENAME.m2", " should be on the load ", TO "path", "
    	   and should contain a package named ", TT "PACKAGENAME", "." },
     PARA {
	  "If the variable ", TO "notify", " is set to true, then an informational message is displayed after the file is loaded."
	  },
     EXAMPLE {
	  ///notify = true///,
	  ///loadPackage "FirstPackage"///
	  },
     SeeAlso => {"packages", "an example of a package", needsPackage, load }
     }
document {
     Key => {(loadPackage,Package)},
     Headline => "reload an already loaded package",
     Usage => "loadPackage PACKAGE",
     Inputs => {
       "PACKAGE" => "the already loaded package"
     },
     Outputs => { Package => "the package just reloaded" },
     Consequences => {{"Reloads the package PACKAGE"}},
     PARA { "This reloads a package. ",
       "The package ", TT "PACKAGE", " should already have been loaded at an earlier step, ",
       "using a command such as ", TT "loadPackage(\"PACKAGE\")", " or ",
       TT "needsPackage(\"PACKAGE\")", ", i.e., using the name of the package ",
       "as a string. If that has been done, then this command will reload the package."},
     PARA { "In fact this version of the command is simply a convenient short form ",
     "of ", TT "loadPackage", " with the option ", TT "Reload", " set to ",
     TT "true", ". All options of ", TT "loadPackage", " (other than ", TT "Reload",
     ") are available." },
     EXAMPLE lines ///
       loadPackage "FirstPackage"
       loadPackage FirstPackage
     ///,
     SeeAlso => {loadPackage, needsPackage}
     }
       
document {
     Key => {(needsPackage,String),needsPackage,
	  [needsPackage, LoadDocumentation],[needsPackage,Configuration],[needsPackage,DebuggingMode],[needsPackage,FileName]},
     Headline => "load a package if not already loaded",
     Usage => "needsPackage \"PACKAGENAME\"",
     Inputs => { 
	  "PACKAGENAME" => "the name of the package",
	  FileName => String => "the name of the file containing the source code of the package, from which it should be loaded",
	  LoadDocumentation => Boolean => {"whether to load the documentation of the package, too; see ", TO "beginDocumentation"},
	  DebuggingMode => Boolean => {
	       "the value of the variable ", TO "debuggingMode", " during loading the package; specifying it here overrides the 
	       value specified as an option to ", TO "newPackage", " by the package itself"
	       },
	  Configuration => List => {"a list of options ", TT "KEY => VALUE", ", overriding the defaults specified in 
	       the source code of the package and the (possibly updated) values in the file in the user's application
	       directory."}
	  },
     Outputs => { 
	  { "the package requested"}
	  },
     Consequences => { 
	  {
	       "loads the package ", TT "PACKAGENAME", " by loading the file ", TT "PACKAGENAME.m2", ", which should appear
	       in one of the directories occuring in the list ", TO "path", ", unless it has
	       already been loaded, in which case it ensures that the package's dictionary of exported symbols 
	       occurs in ", TO "dictionaryPath", ", and are thus available to the user.  In addition, the function ", TO "needsPackage", "
	       is applied to each of the packages whose names are specified by the ", TO "PackageExports", " option of ", TO "newPackage", " 
	       for the requested package."
	       },
	  {
	       "if the variable ", TO "notify", " is set to true, then an informational message is displayed after the file is loaded"
	       }
	  },
     PARA { "For example, to load the sample package ", TT "FirstPackage", ":"},
     EXAMPLE {
	  ///notify = true///,
	  ///needsPackage "FirstPackage"///,
	  ///needsPackage "FirstPackage"///	  
	  },
     "The second time the package was not reloaded.",
     SeeAlso => {"packages", "an example of a package", loadPackage, needsPackage }
     }
document {
     Key => {makePackageIndex,(makePackageIndex, List), (makePackageIndex, Sequence)},
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
     SeeAlso => {"packages"}
     }

undocumented (export,Symbol)

document {
     Key => {export, (export,List), (export,String)},
     Headline => "package item: export functions",
     Usage => "export {symbol1,symbol2,...}",
     Inputs => { { TT "{symbol1,symbol2,...}", ", a list of strings or options" } },
     Outputs => {List => "the list of exported symbols"},
     Consequences => {{"The symbols whose names are in the list as strings, which should refer
	  to functions or other symbols defined in the package, are made available 
	  to the user of the package, and are marked non-mutable.  The strings are converted to symbols
	  with those names in the dictionary of the package.  An option of the form ", TT "\"nam\" => \"sym\"", "
	  creates a symbol with the name ", TT "nam", " that is a synonym of the symbol", TT "sym", "."
	  }},
     PARA {
	  "A package can contain the code for many functions, only some 
	  of which should be made visible to the user.  The function ", TT "export", " 
	  allows one to specify which symbols are to be made visible.
	  For an example see ", TO "an example of a package", "."
	  },
     PARA {
	  "No single-letter symbol should be exported, as such symbols are reserved as variables for the user."
	  },
     PARA{ "Use ", TO exportMutable, " to export symbols whose values the user is permitted to modify." },
     SeeAlso => {debug}
     }

undocumented (exportMutable,Symbol)

document {
     Key => {exportMutable, (exportMutable,List), (exportMutable,String)},
     Headline => "package item: export writable variables",
     Usage => "exportMutable(symbol1,symbol2,...)",
     Inputs => { Nothing => { TT "(symbol1,symbol2,...)", ", a sequence of strings interpreted as names of symbols"  } },
     Outputs => {List => "the list of exported symbols"},
     Consequences => {
	  {"the names of symbols in the sequence, which should refer to variables defined in the package,
	       are made available to the user of the package, in such a way that their values may be modified by the user"}
	 },
     PARA {
     	  "This function is needed much less frequently than ", TO export, ".  For an example, see ", TO "an example of a package"
	  },
     PARA {
	  "No single-letter symbol should be exported, as such symbols are reserved as variables for the user."
	  },
     SeeAlso => {export, debug}
     }

document {
     Key => beginDocumentation,
     Headline => "package item: start documentation section",
     Usage => "beginDocumentation()",
     Consequences => {
	  { 
	       "Initiates the documentation section of a package:
	       If the documentation has previously been processed and stored, then the rest of
	       the file after the invocation of ", TO "beginDocumentation", " will be skipped.
	       Otherwise the packages ", TO "SimpleDoc::SimpleDoc", " and ", TO "Text::Text", "
	       will be loaded and the rest of the file will be loaded." }
	  },
     PARA {
	  "Documentation for a package, and tests for the package, are 
	  placed after this point in a package file.  This way, documentation can
	  be loaded separately, Macaulay2 examples in the documentation can
	  be run, and the whole documentation can be stored in a database."},
     PARA { "For an example, see ", TO "an example of a package" },
     PARA {
	  "To write documentation without using the function ", TO "beginDocumentation", ", which is just
	  an optimization, use ", TO "needsPackage", " to load the packages ", TT "SimpleDoc", " and ", TT "Text", "."
	  },
     SeeAlso => { installPackage, check, "writing documentation" }
     }
document {
     Key => {TEST, (TEST,String), (TEST,List)},
     Headline => "package item: register a test of the package",
     Usage => "TEST s",
     Inputs => { "s" => {"a string or list of strings containing Macaulay2 code"} },
     Consequences => { {"Registers the string ", TT "s", " as a test of the current package"}},
     "This function should only occur in the documentation section of a package.  Use ", TO check, " to run all of the tests
     associated to a package.",
     PARA{},
     "For an example, see ", TO "an example of a package",
     Caveat => "When creating tests, try to insure that they run relatively quickly.",
     SeeAlso => { beginDocumentation, assert }
     }

document {
     Key => {(newPackage,String), newPackage, Date, [newPackage,Date], Authors, [newPackage,Authors], Version, [newPackage, Version],
	  [newPackage,Headline],HomePage, [newPackage,HomePage],[newPackage,DebuggingMode],Email,Name,Configuration,[newPackage,Configuration],
	  InfoDirSection, [newPackage,InfoDirSection],AuxiliaryFiles,[newPackage,AuxiliaryFiles],[newPackage,CacheExampleOutput],
	  [newPackage,PackageExports], PackageExports, [newPackage,PackageImports], PackageImports,
	  [newPackage,Certification], [newPackage,Reload]
	  }, 
     Headline => "package item: start a new package",
     Usage => "newPackage ( title )",
     Inputs => {
	  "title" => "the name of the new package",
	  Version => String => {"the version number of the package.  A version number less than 1.0 indicates that the package is under
	       development, and the user interface may change."},
	  Date => String => "the date of this version of the package",
	  InfoDirSection => String => {"the title of the section in the info page directory where the menu entry for this package should be made"},
	  Headline => String => {"a brief (5-10 words) description of the package"},
	  Authors => List => {"a list of lists of options, one for each author.  The suboptions are of the form
	       ", TT "Name => x", ", ", TT "Email => x", ", or ", TT "HomePage => x", ", where ", TT "x", " is a string."
	       },
	  HomePage => String => "the URI pointing to the home page of the package, if any",
	  DebuggingMode => Boolean => {"whether ", TO "debuggingMode", " should be true during package loading.  However, if ", TO "debuggingMode", " 
	       is already ", TT "false", ", it will remain so."},
          AuxiliaryFiles => Boolean => {"whether the package source to be distributed includes a directory for
	       auxiliary files, with the same name as the package"},
	  PackageExports => List => {"a list of names of other packages to load, both for the user and for the code of the new package"},
	  PackageImports => List => {"a list of names of other packages to load, just for the code of the new package"},
	  CacheExampleOutput => Boolean => {
	       "whether ", TO "installPackage", " should cache (newer) example output in a subdirectory of the ", TO2{[newPackage,AuxiliaryFiles],"auxiliary file directory"}, "
	       named ", TT "examples", ", for use in a future installation.  This value can be overridden by a value explicitly specified
	       when ", TO "installPackage", " is called.  After the directory is created, it will necessary for the user also to specify
	       ", TT "AuxiliaryFiles=>true", "."
	       },
	  Certification => List => {
	       "the certification block inserted by the maintainers of ", EM "Macaulay2", " after the package has been accepted for publication by a 
	       journal, such as The Journal of Software for Algebra and Geometry: ", EM "Macaulay2", ".  Authors should
	       not undertake to create such a certification block themselves."
	       },
	  Configuration => List => {"a list of configuration options for the package.  The keys and values should be constant expressions,
	       such as strings and integers, not incorporating symbols to be exported by the package (and not yet defined).
	       The first time the package is loaded by the user, unless the ", TT "-q", " option is specified on the ", TT "M2", " command
	       line, these options will be stored in a file in the user's application
	       directory (see ", TO "applicationDirectory", ").  The user can change the configuration by editing the file.  
	       The user can override the configuration settings when loading the package; see ", TO [loadPackage,Configuration], " and 
	       ", TO [needsPackage,Configuration], ".  The file will be overwritten when a newer version of the package 
	       with different configuration options is loaded, but a backup will be made and the user's settings for the surviving options will be retained.
	       "
	       },
	  Reload => Boolean => {"whether to reload the package, if it has been loaded before"}
	  },
     Consequences => {"a package is created"},
     PARA {
	  "The dictionaries for the symbols in the packages loaded by the user are moved out of the way to avoid conflicts, so
	  just the standard pre-loaded packages are visible to the source code of the package.  In addition, the package
	  ", TO2( "SimpleDoc::SimpleDoc", "SimpleDoc"), " is made available.  If functions from additional packages are needed
	  by the code in the new package, then ", TO "needsPackage", " can be used (after the use of ", TO "newPackage", ") to provide them.
	  If functions from additional packages are needed by the user who will load the new package, then 
	  ", TO "needsPackage", " can be used (before the use of ", TO "newPackage", ") to provide them."
	  },
     EXAMPLE { 
///newPackage("Foo", 
     Version => "1.1", 
     Headline => "making Foo",
     Configuration => { "foo" => 42, "bar" => "x" }
     )///,
          ///endPackage "Foo"///
     	  },
     "The options can be recovered with ", TO "options", " as follows.",
     EXAMPLE lines ///
     	  options Foo
	  oo.Headline
     	  (options Core).Version
     ///,
     PARA { "Here is a template for a typical ", TT "newPackage", " entry in a package."},
     PRE ///newPackage("PACKAGENAME",
    Headline => "one line description",
    Version => 0.1,
    Date => "month XX, 20XX",
    Authors => {
	 {Name => "author1", Email => "email1", HomePage => "url1"}
	 {Name => "author2", Email => "email2", HomePage => "url2"},
	 },
    DebuggingMode => false,
    HomePage => "http://univ.edu/~user/PACKAGENAME/",
    Configuration => {}
    )///,
	SeeAlso => {"packages"}
  }
     
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
