
star := IMG {
     "src" => replace("PKG","Style",currentLayout#"package") | "GoldStar.png",
     "alt" => "a gold star"
     }

document { Key => "packages provided with Macaulay2",
     PARA{
     	  "Here is a list of the packages that are distributed with Macaulay2.  The ones that have been
	  refereed are marked with a star."
	  },
     UL apply(sort select(separate_" " version#"packages", pkg -> pkg =!= "Macaulay2Doc"), 
	  pkg -> (
	       local p;
	       (dictionaryPath,loadedPackages,currentPackage,p) = (dictionaryPath,loadedPackages,currentPackage,
		    try 
		    needsPackage(pkg,DebuggingMode => false)
		    else (
			 stderr << "--warning: *** package " << pkg << " failed to load while generating list of packages provided with Macaulay2" << endl;
			 if PackageDictionary#?pkg and instance(value PackageDictionary#pkg,Package)
			 then value PackageDictionary#pkg
			 else first ( newPackage pkg, endPackage pkg ) -- just fake it
			 )
		    );
	       LI {
		    if (options p).Certification =!= null then (star," "),
		    TO (pkg|"::"|pkg),
		    if (options p).Headline =!= null then (" -- ", (options p).Headline)
		    }
	       ))
     }

document { Key => "packages",
     "A package is a body of Macaulay2 source code devoted to a 
     particular topic.  Many packages are distributed with Macaulay2, 
     and others are available from various users on the internet.",
     PARA{}, "For more detailed information about packages, see ", 
     TO Package, ".",
     SeeAlso => { "packages provided with Macaulay2" },
     Subnodes => {
     "Using existing packages",
	  TO needsPackage,
	  TO loadPackage,
	  TO installPackage,
	  TO "loadedPackages",
     "Creating new packages",
     	  TO "creating a package",
     "Downloading packages",
     	  TO "getPackage"
	  }
     }

document { Key => "using packages",
     "A package is a body of Macaulay2 source code devoted to a 
     particular topic.  Many packages are distributed with Macaulay2, 
     and others are available from various users on the internet.",
     PARA{}, "For more detailed information about packages, see ", 
     TO Package, ".",
     PARA{},
     "To load a package, say FirstPackage, use ", TO loadPackage, " or ", TO needsPackage, ", as in:",
     PRE///    loadPackage "FirstPackage"///,
     "or",
     PRE///    needsPackage "FirstPackage"///,
     "Macaulay2 searches for the file FirstPackage.m2 on your search ", TO "path", ".
     The packages provided with Macaulay2 are on your search path,
     as is your current working directory.",
     PARA{},
     "Documentation for the packages provided with Macaulay2 is already installed.  TO
     install documentation for another package, use ", TO installPackage, ".",
     PRE///    installPackage FirstPackage///,
     "You may see what packages have been loaded with the variable ", TO "loadedPackages", ".",
     PRE///    loadedPackages///,
     SeeAlso => {
     	  "packages provided with Macaulay2",
	  "creating a package"
	  }
     }

document { Key => "loadedPackages",
     Headline => "the list of loaded packages",
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
	"There are four parts to a Macaulay2 package: a preamble, which 
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
	
  -- Macaulay2 code goes here
	
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
	  TO document,
	  TO TEST,
	  "Documenting, testing, and distributing a package",
	  TO "writing documentation",
	  TO check,
	  TO "informing others about your package"
	  },
     }

document {
     Key => "informing others about your package",
     "We keep a list of user defined packages on the Macaulay2 web site.
     Contact Macaulay2@math.uiuc.edu to have your package placed on the
     list."
     }

document {
     Key => "an example of a package",
     "Here is a basic example of a complete package:",
     TABLE { "class" => "examples",  TR TD PRE get (first searchPath(path,"FirstPackage.m2") | "FirstPackage.m2")  },
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
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Macaulay2Doc RemakeAllDocumentation=true "
-- End:
