document {
     Key => "Macaulay2",
     PARA IMG (LAYOUT#"packagesrc" "Style" | "9planets.gif", "image of nine planets"),
     "Macaulay 2 is a software system devoted to supporting research in 
     algebraic geometry and commutative algebra, developed with funding
     from the National Science Foundation.  We are eager to help new users
     get started with it.",
     Subnodes => {
	  "Preface",
	       TO "Copyright and license",
	       TO "Acknowledgements",
	       TO "The authors",
	       TO "Other sources of information about Macaulay 2", -- node to be written, will point to our book, Hal's book, Mike's Arizona notes, Sage, etc
	  "For the New User",
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
	       TO "combinatorial functions",
          "The Macaulay 2 Language",
	       TO "The Macaulay2 language",
     	  "The Macaulay 2 Language (old version)",			    -- part 2, one liners, lists, sets, but not hash tables
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
     	  "Reference material",
	       TO "Type"
	  }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
