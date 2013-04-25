-- -*- coding: utf-8 -*-
document {
     Key => {Macaulay2Doc,"Macaulay2"},
     Headline => "Macaulay2 documentation", 
     PARA IMG ("src" => replace("PKG","Style",currentLayout#"package") | "9planets.gif", "alt" => "image of nine planets"),
     "Macaulay2 is a software system devoted to supporting research in 
     algebraic geometry and commutative algebra, developed with funding
     from the National Science Foundation.  We are eager to help new users
     get started with it.",
     Subnodes => {
	  "Preface",
	       TO "Copyright and license",
	       TO "Acknowledgements",
	       TO "The authors",
	       TO "Other sources of information about Macaulay2", -- node to be written, will point to our book, Hal's book, Mike's Arizona notes, Sage, etc
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
	       TO "Gröbner bases",
	  "Some Mathematical Computations",
	       TO "normal forms",
	       -- Mike wanted this: TO "Hilbert functions",
	       TO "elimination of variables",
	       -- Mike wanted this: TO "syzygies",
	       -- Mike wanted this: TO "saturation",
	       -- Mike wanted this: TO "fibers of a map between varieties",
	       -- Mike wanted this: TO "solving systems of polynomial equations",
     	       -- Mike wanted this, but it's in another package (!): TO "integralClosure",
     	       -- this is in another package now: TO "PrimaryDecomposition :: primaryDecomposition",
	       TO "combinatorial functions",
          "The Macaulay2 Language",
	       TO "The Macaulay2 language",
	  "Packages",
	       TO "packages provided with Macaulay2",
     	  "Appendix",
	       TO "changes to Macaulay2, by version",
	       TO "mathematical examples",
	       TO "basic commutative algebra",
	       -- Mike wanted this: TO "frequently encountered problems",
	       TO "replacements for commands and scripts from Macaulay",
	       TO "how Macaulay2 finds its files",
     	  "Reference material",
	       TO "Type",
	       TO "Function"
	  }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
