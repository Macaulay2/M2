document { Key => "packages",
     "A package is a body of Macaulay2 source code devoted to a 
     particular topic.  Many packages are distributed with Macaulay 2,
     and others are available from various users on the internet.",
     PARA, "For more detailed information about packages, see ", 
     TO Package, ".",
     "Some packages which are distributed with Macaulay2 include",
     UL {
	  TO "convex hulls and polar cones",
          TO "Dmodules",
	  TO "elimination theory",
	  TO "PrimaryDecomposition"
	  },
     Subnodes => {
     "using packages",
	  TO (needsPackage,String),
	  TO (loadPackage,String),
	  TO (installPackage,String),
     "Creating new packages",
     	  TO "creating a package",
	  TO "documenting a package",
	  TO "testing a package",
	  TO "informing others about your package"
	  }
     }

     
     
--     "Writing documentation for packages",
--	  TO document,
--     "Debugging your package",
--	  TO "using the debugger",
--	  TO (debug,Package),
--	  TO (check,Package)
--	  }
--     }

--     Subnodes => {
--	  TO "using packages",				    -- ?
--	  TO "writing packages",			    -- ?
--     	  -- these next ones might not be needed:
--     	  TO "loading a library",
--     	  TO "how to get documentation on a library",
--     	  "available libraries",
--	  TO "blow ups",
--	  TO "convex hulls and polar cones",
--	  TO "Dmodules",
--	  TO "elimination theory",
--	  TO "graphing curves and surfaces via 'surf'",
--	  TO "invariants of finite groups",
--	  TO "Lenstra-Lenstra-Lovasz (LLL) lattice basis reduction",
--	  TO "SAGBI bases",
--	  }

--document {
--     Key => "using packages",
--     Subnodes => {
--	  TO (needsPackage,String),
--	  TO (loadPackage,String),
--	  TO (installPackage,String)
--	  }
--     }

     
document { Key => "creating a package",
	"There are four parts to a Macaulay 2 package: a preamble which 
	is initiated by the ", TO "newPackage",
	" function, a section where one defines which variables will be 
	exported to global variables, a section containing 
	the actual coding that constitutes the package, and a 
	section containing documentation and tests for the new package.",
	PARA,
	"A basic template for new packages:",
	PRE///
	newPackage( ... )
	
	export( ... )
	exportMutable( ... )
	
	-- Macaulay 2 code goes here
	
	beginDocumentation()
	document { ... } 
	TEST " ... "
	///,
	"The name of the package must be the name of the file, without the
	'.m2' suffix.  Thus a package 'floop' will be in a file named
	'floop.m2'.  If the package were more complex, then by convention,
	there should be a subdirectory named 'floop', and the file
	'floop.m2' would load these files.",
     Subnodes => {
	  TO "an example of a package",
	  TO "newPackage",
	  TO "export",
	  TO "exportMutable",
	  TO beginDocumentation,
	  TO (TEST,String),
	  TO "writing documentation"
	  }
     }

document {
     Key => "documenting a package",
     "Documentation for a package is in the ", TO beginDocumentation, "
     section of a package.",
     PARA,
     "The easiest way to write a documentation node for yout package, 
     a function or variable in your package, or an overview node for your
     package is to start from one of the following templates.",
     UL {
	  "document template for a package",
	  "document template for function names",
	  "document template for function calls (includes argument types)",
	  "document template for an overview node",
	  "document template for optional argument to a function"
	  },
     "Many items use a formatting method closely related to html. See ", 
     TO "hypertext documentation",  " for the possible types and for examples.",
     PARA,
     "The steps in creating documentation, which one cycles through until happy
     with the documentation are:",
     UL {
	  {"write or edit documentation nodes starting from templates"},
	  {"in Macaulay2, use ", TO (installPackage,String), " to load the
	       documentation and run the Macaulay 2 examples in the documentation."},
	  {"view the documentation from your browser.  The documentation for your 
	       package should be available from the main Macaulay2 documentation page ", 
	       TT "applicationDirectory()|index.html", "."}
	       }
     }
     
document {
     Key => "testing a package",
     }

document {
     Key => "informing others about your package",
     }

document {
     Key => "an example of a package",
     "Here is a basic example of a complete package:",
PRE///
newPackage(
	"FirstPackage",
    	Version => "1.0", 
    	Date => "February 11, 2004",
    	Author => "Jane Doe <doe@math.uiuc.edu>",
    	HomePage => "http://www.math.uiuc.edu/~doe/",
    	Headline => "an example Macaulay 2 package",
    	DebuggingMode => true
    	)

export(firstFunction)

firstFunction = method(TypicalValue => String)
firstFunction ZZ := String => n -> (
	if n == 1
	then print "Hello World!"
	else print "D'oh!"	
	);

beginDocumentation()
document { 
	Key => FirstPackage,
	Headline => "an example Macaulay 2 package",
	EM "FirstPackage", " is a basic package to be used as an example."
	}
document {
	Key => firstFunction,
	Headline => "a silly first function",
	Usage => "firstFunction n",
	Inputs => {
		"n" => ZZ => {}
		},
	Outputs => {
		String => {}
		},
	EXAMPLE {
		"firstFunction 1",
		"firstFunction 0"
		}
	}
TEST "firstFunction 2"
///,
	SeeAlso => {newPackage,
	     export,
	     exportMutable,
	     "writing documentation"}
	}


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
