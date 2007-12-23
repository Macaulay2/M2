document {
     Key => {installPackage,
	  (installPackage,String),
	  (installPackage,Package),
	  [installPackage,AbsoluteLinks],
	  [installPackage,CheckDocumentation],
	  [installPackage,DebuggingMode],
	  [installPackage,Encapsulate],
	  [installPackage,FileName],
	  [installPackage,IgnoreExampleErrors],
	  [installPackage,InstallPrefix],
	  [installPackage,MakeDocumentation],
	  [installPackage,MakeInfo],
	  [installPackage,MakeLinks],
	  [installPackage,PackagePrefix],
	  [installPackage,RemakeAllDocumentation],
	  [installPackage,RerunExamples],
	  [installPackage,UserMode]
	  },
     Headline => "load and install a package and its documentation ",
     Usage => "installPackage PACKAGENAME",
     Inputs => {
	  "PACKAGENAME" => {"a ", TO String, " or ", TO Package},
	  FileName => String => "the name of the file containing the source code of the package, from which it should be loaded",
	  UserMode => Boolean => { "whether the installation will ignore packages installed in the user's 
	       ", TO2{"applicationDirectory", "application directory"}, " and will ignore the user's ", TO "initialization file", " when running
	       examples" 
	       },
	  DebuggingMode => Boolean => { "whether to enter ", TO "the debugger", " if an error occurs during installation" },
	  RerunExamples => Boolean => "whether to rerun all the examples during installation",
	  IgnoreExampleErrors => Boolean => "whether to ignore errors that occur during the running of examples during installation",
     	  CheckDocumentation => Boolean => "whether to check the package's documentation for consistency and completeness",
	  MakeDocumentation => Boolean => "whether to make the documentation for the package during installation",
	  MakeInfo => Boolean => { "whether to make the info pages.  This is a form of the documentation that can be viewed using the
	       Unix command ", TT "info", " or using ", TT "emacs", "." 
	       },
	  Encapsulate => Boolean => { "whether to encapsulate all the installed files in a subdirectory whose name has the form
	       ", TT "PACKAGENAME-VERSION", ", where ", TT "VERSION", " is the version number specified by the package as value of the Version option
	       provided to ", TO "newPackage", ".  Encapsulation makes it easy to delete all the files associated with a package,
	       and symbolic links can make it look like the files have been installed in the right place." },
	  PackagePrefix => { "the path to the directory where the files of the package should be installed.  The default value is the subdirectory
	       named ", TT "encap", " of the user's ", TO "application directory", "." },
	  InstallPrefix => { "the path to the directory where the links should be created, in case encapsulation is enabled.  The value of this option can
	       be a string or a function of 0 arguments returning a string.  The default value is the subdirectory named ", TT "local", " of
	       the user's ", TO "application directory", "." },
	  MakeLinks => Boolean => { "whether to make links to the files after installing them, in case encapsulation is enabled" },
	  AbsoluteLinks => { "whether the links made should contain absolute paths, rather than relative paths" },
	  RemakeAllDocumentation => { "whether to regenerate all of the help pages for this package.  The default action
     	       is to rebuild only the html pages of the documentation entries that have been changed since the last time
     	       the package was installed.  However, some changes to an entry, such as to its headline, will change the html of other pages
	       that cross-reference it."}
	  },
     Consequences => {
	  {"The package is installed in a local directory, so that in the future, one may simply use ", TO "loadPackage", ".  Documentation for the
	  package is also produced, running any Macaulay2 examples that are requested in the package documentation." }
	  },
     "The main action of this routine is to generate the documentation of the given package and install the Macaulay2 package and documentation. ",
     PARA{ "The actual file loaded is ", TT "PACKAGENAME.m2", ", which should be on the load ", TO "path", " and should contain a package named ", TT "PACKAGENAME", "."},
     PARA{ "In order to accomplish this, several steps are performed, or bypassed, depending on the optional parameters." },
     UL {
	  "load the package, if not already loaded",
	  "determine which help pages have changed since last install",
	  {"run any new or previously failed examples, or all examples, as specified by the ", TT "RemakeAllExamples", " option"},
	  {"generate the html pages of modified help pages, or all html pages, as specificed by the ", TT "RemakeAllDocumentation", " option"},
	  {"generate the info pages, or not, as specified by the ", TT "MakeInfo", " option"},
	  {"install the documentation and package in the location specified by the ", TT "PackagePrefix", " option"},
	  {"create symbolic links in the location specified by the ", TT "InstallPrefix", " option"},
	  {"place a link to this html documentation in the file ", TT "index.html", " in the user's ", TT "application directory", "; see ", TO "makePackageIndex"}
	  },
     SeeAlso => {"packages"}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
