document { Key => "epkg",
     "The program ", TT "epkg", " is used to ", EM "encapsulate", " the files of a package
     in a subdirectory for easy later removal.  Symbolic links make it look like they have
     been installed in the right place.  See ", HREF "http://www.encap.org/epkg/", "."
     }

document {
     Key => {installPackage,
	  (installPackage,String),
	  (installPackage,Package),
	  [installPackage,AbsoluteLinks],
	  [installPackage,CacheExampleOutput],
	  [installPackage,CheckDocumentation],
	  [installPackage,DebuggingMode],
	  [installPackage,Encapsulate],
	  [installPackage,EncapsulateDirectory],
	  [installPackage,FileName],
	  [installPackage,IgnoreExampleErrors],
	  [installPackage,InstallPrefix],
	  [installPackage,MakeDocumentation],
	  [installPackage,MakeInfo],
	  [installPackage,MakeLinks],
	  [installPackage,PackagePrefix],
	  [installPackage,RemakeAllDocumentation],
	  [installPackage,RerunExamples],
	  [installPackage,RunExamples],
	  [installPackage,UserMode]
	  },
     Headline => "load and install a package and its documentation ",
     Usage => "installPackage PACKAGENAME",
     Inputs => {
	  "PACKAGENAME" => {"a ", TO String, " or ", TO Package},
	  FileName => String => "the name of the file containing the source code of the package, from which it should be loaded",
	  UserMode => Boolean => { "whether the installation will ignore packages installed in the user's 
	       ", TO2{"applicationDirectory", "application directory"}, " and will ignore the user's ", TO "initialization file", " when running
	       examples.  Not setting this to ", TO "false", " is necessary when installing an optional package that depends on another optional package,
	       previously installed by the user." 
	       },
	  DebuggingMode => Boolean => { "whether to enter ", TO "the debugger", " if an error occurs during installation" },
	  RerunExamples => Boolean => "whether to rerun all the examples during installation",
	  RunExamples => Boolean => "whether to run the examples during installation",
	  IgnoreExampleErrors => Boolean => "whether to ignore errors that occur during the running of examples during installation",
     	  CheckDocumentation => Boolean => "whether to check the package's documentation for consistency and completeness",
	  MakeDocumentation => Boolean => "whether to make the documentation for the package during installation",
	  MakeInfo => Boolean => { "whether to make the info pages.  This is a form of the documentation that can be viewed using the
	       Unix command ", TT "info", " or using ", TT "emacs", "." 
	       },
	  InstallPrefix => { "the path to the directory where the files should be installed, in case encapsulation is not
	       enabled, or where the links should be created, in case encapsulation is enabled.  The value of this option can
	       be a string or a function of no arguments returning a string.  The default value is the subdirectory named ", TT "local", " of
	       the user's ", TO "application directory", "." },
	  PackagePrefix => { "the path to the directory where the files of the package should be installed in case encapsulation is
	       enabled.  The default value is the subdirectory named ", TT "encap", " of the user's ", TO "application directory", ".
	       (Note: in version 1.1 and before, the files were installed here also when encapsulation was not enabled.)" },
	  Encapsulate => Boolean => { "whether to encapsulate all the installed files in a subdirectory of
	       the directory specified by the ", TT "PackagePrefix", " option, 
	       whose name is specified by the ", TT "EncapsulateDirectory", " option.
	       Encapsulation makes it easy to delete all the files associated with a package
	       (see ", TO "epkg", ").  On the other hand, encapsulation involves the use of symbolic links, which are of limited
	       utility in a Cygwin version of Macaulay 2, because non-Cygwin programs don't understand them." 
	       },
	  EncapsulateDirectory => { "a string that gives the name of the encapsulation subdirectory, terminated with a ", TT "/", ", in the case where
	        the value of the ", TT "Encapsulate", " option is ", TT "true", ", or a function that accepts a package and returns
		such a string.  The default function returns a string that has the form
	       ", TT "PACKAGENAME-VERSION", ", where ", TT "VERSION", " is the version number specified 
	       by the package as value of the ", TO "Version", " option
	       provided to ", TO "newPackage", "."
	       },
	  MakeLinks => Boolean => { "whether to make links to the files after installing them, in case encapsulation is enabled" },
	  AbsoluteLinks => { "whether the links made should contain absolute paths, rather than relative paths" },
	  RemakeAllDocumentation => { "whether to regenerate all of the help pages for this package.  The default action
     	       is to rebuild only the html pages of the documentation entries that have been changed since the last time
     	       the package was installed.  However, some changes to an entry, such as to its headline, will change the html of other pages
	       that cross-reference it."},
	  CacheExampleOutput => Boolean => {
	       "whether to cache (newer) example output in a subdirectory of the ", TO2{[newPackage,AuxiliaryFiles],"auxiliary file directory"}, "
	       named ", TT "examples", ", for use in a future installation.  This value will override any value explicitly specified
	       when ", TO "newPackage", " is called.  After the directory is created, it will necessary for the user to specify
	       ", TT "AuxiliaryFiles=>true", " with the ", TO "newPackage", " command."
	       }
	  },
     Consequences => {
	  {"The package is installed in a local directory, so that in the future, one may simply use ", TO "loadPackage", ".  Documentation for the
	  package is also produced, running any Macaulay2 examples that are requested in the package documentation." }
	  },
     "The main action of this routine is to generate the documentation of the given package and install the Macaulay2 package and documentation. ",
     PARA{ "The actual file loaded is ", TT "PACKAGENAME.m2", ", which should be on the load ", TO "path", " and should contain a package named ", TT "PACKAGENAME", "."},
     PARA{ "In order to accomplish this, several steps are performed (or bypassed, depending on the values of the optional arguments)." },
     UL {
	  {"load the package, if not already loaded (see ", TO "loadPackage", ")"},
	  {"determine which help pages have changed since last install"},
	  {"run any new or previously failed examples, or all examples, as specified by the ", TO "RemakeAllExamples", " option"},
	  {"generate the html pages of modified help pages, or all html pages, as specificed by the ", TO "RemakeAllDocumentation", " option"},
	  {"generate the info pages, or not, as specified by the ", TO "MakeInfo", " option"},
	  {"install the documentation and package in the location specified by the ", TO "PackagePrefix", " option"},
	  {"create symbolic links in the location specified by the ", TO "InstallPrefix", " option"},
	  {"place a link to this html documentation in the 
	       file ", TT "index.html", " in the user's ", TO "application directory", "; see ", TO "makePackageIndex"}
	  },
     PARA {
	  "The current value of ", TO "prefixPath", " is used to determine how to direct documentation hyperlinks,
	  provided the value of the option ", TO "AbsoluteLinks", " is ", TO "true", ", as it is by default; the link
	  will be directed to the appropriate file if one is found by searching the trees referred to by ", TO "prefixPath", ".  
	  Otherwise, all documentation hyperlinks are relative to positions within a single tree of directories, as describe by ", TO "Layout", "."
	  },
     PARA {
	  "It might be necessary to run ", TO "installPackage", " twice if a package with the same name is already installed:
	  the second installation will redirect the hyperlinks to the freshly installed documentation, because the files will 
	  have been installed by the first installation.
	  This applies, for example, to those authors who are developing updates to packages already included with Macaulay 2."
	  },
     PARA {
	  "The files of the package are placed in subdirectories of the appropriate prefix directory as specified by ", TO "Layout", ", depending on
	  the value of the ", TO "SeparateExec", " option: when it is false the files are all in ", 
	  TT (Layout#1#"packages"|"PACKAGENAME.m2"), ", ",
	  TT (Layout#1#"info"|"PACKAGENAME.info"), ", ",
	  TT (Layout#1#"lib"|"PACKAGENAME.info"), ", ",
	  TT replace("PKG", "PACKAGENAME", Layout#1#"package"), ", and ",
	  TT replace("PKG", "PACKAGENAME", Layout#1#"packagedoc"), 
	  "; when it is true the paths to the files are modified to reflect the type of your machine, e.g., ",
	  TT (Layout#2#"packages"|"PACKAGENAME.m2"), ", ",
	  TT (Layout#2#"info"|"PACKAGENAME.info"), ", ",
	  TT (Layout#2#"lib"|"PACKAGENAME.info"), ", ",
	  TT replace("PKG", "PACKAGENAME", Layout#2#"package"), ", and ",
	  TT replace("PKG", "PACKAGENAME", Layout#2#"packagedoc"), "."
	  },
     PARA {
	  "In addition, if no errors occurred during running the examples, then an empty file whose name is 
	  ", TT (Layout#1#"packages"|"PACKAGENAME.m2"), " or
	  ", TT (Layout#2#"packages"|"PACKAGENAME.m2"), " is created, to signify that installation was completed."
	  },
     SeeAlso => {"packages", "epkg", "prefixPath", "Layout"}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
