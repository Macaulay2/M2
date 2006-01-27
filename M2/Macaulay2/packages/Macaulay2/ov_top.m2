document {
     Key => "Macaulay2",
     PARA IMG (LAYOUT#"packagesrc" "Style" | "9planets.gif", "image of nine planets"),
     PARA {
	  "This documentation addresses version ", version#"VERSION", " of Macaulay 2. "
	  },
     Subnodes => {
	       TO "preface",
	       TO "getting started",
	  "Mathematical Objects",
	       TO "rings",
	       TO "ideals",
	       TO "matrices",
	       TO "substitution and maps between rings",
	       TO "modules",
	       TO "chain complexes",
	       TO "varieties",
	       TO "Groebner bases",
	  -- "Important General Concepts",
	  "Some Mathematical Computations",
	       TO "normal forms",
	       TO "Hilbert functions",
	       TO "elimination of variables",
	       TO "syzygies",
	       TO "saturation",
	       TO "fibers of a map between varieties",
	       TO "solving systems of polynomial equations",
     	       TO "integralClosure",
     	       TO "primaryDecomposition",
	       TO "Hom module",
	       TO "Tor and Ext",
	       TO "combinatorial functions",
     	  "The Macaulay 2 Language",			    -- part 2, one liners, lists, sets, but not hash tables
	       TO "variables and symbols",
	       TO "basic types",
	       TO "control structures",			    -- control structures: scan, apply, for, while, if, return, break, continue
	       TO "functions",
	       TO "input and output",			    -- but sockets, etc, move later
	       TO "operators",				    -- but with explanations
	  "Programming Overview",			    -- part 3, multiple line techniques, saved in a function in a file, writing packages
	       TO "classes and types",
	       TO "system",
	       TO "advanced input and output",
	       TO "debugging",
	       TO "executing other programs",
	       TO "packages",
		  TO "writing documentation",
		  "Developer's Corner",
		  TO "handling hypertext",
     	  "Appendix",
	       TO "mathematical examples",
	       TO "basic commutative algebra",
	       TO "frequently encountered problems",
	       TO "replacements for commands and scripts from Macaulay",
     	       TO "reference material",			    -- every symbol and methods
	  }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
