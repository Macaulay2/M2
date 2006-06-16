packagelist := (
     if sourceHomeDirectory =!= null
     then lines get (sourceHomeDirectory|"packages/packagelist")
     else if prefixDirectory =!= null
     then lines get (prefixDirectory|LAYOUT#"packages"|"packagelist")
     else error ("can't find file ", format "packagelist"))

packagelist = select(packagelist, pkg -> pkg =!= "Macaulay2")

document { Key => "packages provided with Macaulay 2",
     PARA{
     	  "The packages that are distributed with Macaulay2 are:"
	  },
     UL apply(sort packagelist, pkg -> LI TO (pkg|"::"|pkg))
     }

document { Key => "packages",
     "A package is a body of Macaulay2 source code devoted to a 
     particular topic.  Many packages are distributed with Macaulay 2, 
     and others are available from various users on the internet.",
     PARA{}, "For more detailed information about packages, see ", 
     TO Package, ".",
     SeeAlso => { "packages provided with Macaulay 2" },
     Subnodes => {
     "Using existing packages",
	  TO needsPackage,
	  TO loadPackage,
	  TO installPackage,
	  TO "loadedPackages",
     "Creating new packages",
     	  TO "creating a package"
	  }
     }

document { Key => "loadedPackages",
     "The value of the variable ", TT "loadedPackages", " is a list of the packages that have been loaded.",
     EXAMPLE "loadedPackages"
     }
     
     
--     "Writing documentation for packages",
--	  TO document,
--     "Debugging your package",
--	  TO "the debugger",
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
	PARA{},
	"A basic template for new packages:",
	PRE///newPackage( ... )
	
	export{ ... }
	exportMutable{ ... }
	
	-- Macaulay 2 code goes here
	
	beginDocumentation()
	document { ... }  -- several document's and TEST's, interspersed
	TEST " ... "///,
	"The name of the package must be the name of the file, without the
	'.m2' suffix.  Thus a package 'PACKAGENAME' will be in a file named
	'PACKAGENAME.m2'.  If the package were more complex, then by convention,
	there should be a directory named 'PACKAGENAME' on the load ", TO "path", 
	", and the file
	'PACKAGENAME.m2' in this directory would load the necessary files.",
     Subnodes => {
	  TO "an example of a package",
	  "Parts of a package",
	  TO "newPackage",
	  TO "export",
	  TO "exportMutable",
	  TO beginDocumentation,
	  "Documenting, testing, and distributing a package",
	  TO "writing documentation",
	  TO "testing a package",
	  TO "informing others about your package"
	  },
     SeeAlso => {
	  document,
	  TEST
	  }
     }

document {
     Key => "testing a package",
     "It is important to provide tests to insure that your package
     is functioning properly.  One provides tests using  ", TO TEST,
     " in the ", TO beginDocumentation, " section.",
     PARA{},
     "All of the tests provided for a package 'YourPackage' may be run by 
     using ", TO check,
     PRE///check YourPackage///
     }

document {
     Key => "informing others about your package",
     "We keep a list of user defined packages on the Macaulay 2 web site.
     Contact Macaulay2@math.uiuc.edu to have your package placed on the
     list."
     }

document {
     Key => "an example of a package",
     "Here is a basic example of a complete package:",
     TABLE { "class" => "examples",  TR TD PRE get first searchPath(path,"FirstPackage.m2")  },
     SeeAlso => {
	     "packages",
	     newPackage,
	     export,
	     exportMutable,
	     beginDocumentation,
	     document,
	     TEST
	     }
	}


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
