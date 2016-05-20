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
	  [installPackage,Verbose],
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
	  [installPackage,SeparateExec],
	  [installPackage,UserMode]
	  },
     Headline => "load and install a package and its documentation ",
     Usage => "installPackage PACKAGENAME",
     Inputs => {
	  "PACKAGENAME" => {"a ", TO String, " or ", TO Package},
	  FileName => String => "the name of the file containing the source code of the package, from which it should be loaded",
	  UserMode => { "if ", TO "true", ", then do not give the ", TT "-q", " option to ", TT "M2", " when running 
	       examples, thereby allowing it to load the user's ", TO "initialization file", ",
	       allowing it to load packages previously installed in the user's ", TO2{"applicationDirectory", "application directory"}, ",
	       and allowing packages it loads to read their configuration files from the 
	       the user's ", TO2{"applicationDirectory", "application directory"}, ".
	       If ", TO "false", ", then do give the option.
	       If ", TO "null", ", then propagate the option from the current ", TO "commandLine", ", if one occurs there." 
	       },
	  DebuggingMode => Boolean => { "whether to enter ", TO "the debugger", " if an error occurs during installation; 
	       however, if ", TO "debuggingMode", " is already ", TT "false", ", it will remain so." },
	  RerunExamples => Boolean => "whether to rerun all the examples during installation",
	  RunExamples => Boolean => "whether to run the examples during installation",
	  IgnoreExampleErrors => Boolean => "whether to ignore errors that occur during the running of examples during installation",
     	  CheckDocumentation => Boolean => "whether to check the package's documentation for consistency and completeness",
	  MakeDocumentation => Boolean => "whether to make the documentation for the package during installation",
	  MakeInfo => Boolean => { "whether to make the info pages.  This is a form of the documentation that can be viewed using the
	       Unix command ", TT "info", " or using ", TT "emacs", "." 
	       },
	  InstallPrefix => { "the installation prefix for installation of the files of the package, in case encapsulation is not
	       enabled, or for installation of the links to the files, in case encapsulation is enabled.  The value of 
	       this option can be a string or a function of no arguments returning a string.  The default value is the 
	       subdirectory named ", TT "local", " of the user's ", TO "application directory", "." },
	  PackagePrefix => { "the installation prefix for installation of the files of the package in case encapsulation is
	       enabled.  The value of this option can
	       be a string or a function of no arguments returning a string.  
	       The default value is the subdirectory named ", TT "encap", " of the user's ", TO "application directory", "." },
	  Encapsulate => Boolean => { "whether to encapsulate all the installed files in a subdirectory of
	       the directory specified by the ", TT "PackagePrefix", " option, 
	       whose name is specified by the ", TT "EncapsulateDirectory", " option.
	       Encapsulation makes it easy to delete all the files associated with a package
	       (see ", TO "epkg", ").  On the other hand, encapsulation involves the use of symbolic links, which are of limited
	       utility in a Cygwin version of Macaulay2, because non-Cygwin programs don't understand them." 
	       },
	  EncapsulateDirectory => { "a string that gives the name of the encapsulation subdirectory, terminated with a ", TT "/", ", in the case where
	        the value of the ", TT "Encapsulate", " option is ", TT "true", ", or a function that accepts a package and returns
		such a string.  The default function returns a string that has the form
	       ", TT "PACKAGENAME-VERSION", ", where ", TT "VERSION", " is the version number specified 
	       by the package as value of the ", TO "Version", " option
	       provided to ", TO "newPackage", "."
	       },
	  MakeLinks => Boolean => { "whether to make links to the files after installing them, in case encapsulation is enabled" },
	  AbsoluteLinks => Boolean => {
	       "whether the links made should contain real absolute paths, rather than relative paths.  If set to
	       ", TO "true", ", the default value, then the files linked to should already exist, either under the current installation prefix,
	       or in any of the directory trees listed in ", TO "prefixPath", ".  (The other files to be created as part of the installation of 
	       the current package will be made to exist (as empty files) in an earlier pass.)
	       If the option is set to ", TO "false", ", then no absolute links will be made, and all references 
	       to documentation nodes will point to locations in the same directory tree, even though the corresponding files may 
	       not be there (yet).  This behaviour is useful only when installing documentation in the main ", EM "Macaulay2", " 
	       documentation tree (given by ", TO "prefixDirectory", "), or for preparing documentation that will eventually be
	       installed there."
	       },
	  RemakeAllDocumentation => { "whether to regenerate all of the help pages for this package.  The default action
     	       is to rebuild only the html pages of the documentation entries that have been changed since the last time
     	       the package was installed.  However, some changes to an entry, such as to its headline, will change the html of other pages
	       that cross-reference it."},
	  CacheExampleOutput => Boolean => {
	       "whether to cache (newer) example output in a subdirectory of the ", TO2{[newPackage,AuxiliaryFiles],"auxiliary file directory"}, "
	       named ", TT "examples", ", for use in a future installation.  This value, if set to ", TO "true", " or ", TO "false", ", will override any value explicitly specified
	       when ", TO "newPackage", " is called.  After the directory is created, it will be necessary for the user to specify
	       ", TT "AuxiliaryFiles=>true", " with the ", TO "newPackage", " command."
	       },
	  SeparateExec => Boolean => {
	       "whether to install the files of the package in two separate directory trees, one for the architecture independent files,
	       and one for the architecture dependent files"
	       },
	  Verbose => Boolean => {
	       "whether to display some details of the installation procedure.  For even more information set ", TO "debugLevel", " to
	       a number greater than 0 or a number greater than 5."
	       }
	  },
     Consequences => {
	  {"The package is installed in a local directory, so that in the future, one may simply use ", TO "loadPackage", ".  Documentation for the
	  package is also produced, running any Macaulay2 examples that are requested in the package documentation, with
	  the random number seed initialized to 0." }
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
	  This applies, for example, to those authors who are developing updates to packages already included with Macaulay2."
	  },
     PARA {
	  "The files of the package are placed in subdirectories of the appropriate prefix directory as specified by ", TO "Layout", ", depending on
	  the value of the ", TO "SeparateExec", " option: when it is false the files are all in ", 
	  TT (Layout#1#"packages"|"PACKAGENAME.m2"), ", ",
	  TT (Layout#1#"info"|"PACKAGENAME.info"), ", ",
	  TT replace("PKG", "PACKAGENAME", Layout#1#"packagelib"), ", ",
	  TT replace("PKG", "PACKAGENAME", Layout#1#"package"), ", and ",
	  TT replace("PKG", "PACKAGENAME", Layout#1#"packagedoc"), 
	  "; when it is true the paths to the files are modified to reflect the type of your machine, e.g., ",
	  TT (Layout#2#"packages"|"PACKAGENAME.m2"), ", ",
	  TT (Layout#2#"info"|"PACKAGENAME.info"), ", ",
	  TT replace("PKG", "PACKAGENAME", Layout#2#"packagelib"), ", ",
	  TT replace("PKG", "PACKAGENAME", Layout#2#"package"), ", and ",
	  TT replace("PKG", "PACKAGENAME", Layout#2#"packagedoc"), "."
	  },
     PARA {
	  "In addition, if no errors occurred during running the examples, then an empty file whose name is 
	  ", TT (replace("PKG", "PACKAGENAME", Layout#1#"packagelib")|".installed"), " or
	  ", TT (replace("PKG", "PACKAGENAME", Layout#2#"packagelib")|".installed"), " is created, to signify that installation was completed."
	  },
     SeeAlso => {"packages", "epkg", "prefixPath", "Layout", installedPackages, uninstallAllPackages}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
