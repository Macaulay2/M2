----------------------------
-- The granddaddy node -----
----------------------------

document { "Mathematical Overview",
     "In this section we give a comprehensive overview of the main 
     mathematical types of Macaulay 2, their construction and most common
     operations.",
     PARA,
     MENU {
	  TO "rings",
	  TO "ideals",
	  TO "matrices",
	  TO "substitution and maps between rings",
	  ("modules",
	       MENU {
		    (TO "modules I", " -- getting started"),
		    (TO "modules II", " -- homological and multilinear algebra")
		    }
	       ),
	  TO "Groebner bases and related computations",
	  TO "chain complexes",
	  TO "varieties",
	  TO "external libraries",
	  ("specialized routines",
	       MENU {
		    TO "commutative algebra",
		    TO "algebraic geometry"
		    }
	       )
	  }
     }

----------------------------
-- Top level nodes ---------
----------------------------

document { "rings",
     Headline => "an overview",
     "Macaulay 2 differs from computer algebra systems such as maple and
     mathematica, in that rings are first class objects.  This means that
     before making polynomials or matrices, you must create a ring where
     you give the variables that you want, and the kinds of coefficients
     you want (e.g. rational numbers, or integers modulo a prime number).",
     "In this section we present an overview of rings.",
     MENU {
	  (TO "basic rings",
	       MENU {
		    TO "finite fields"
		    }
	       ),
	  (TO "polynomial rings",
	       MENU {
		    TO "monomial orderings",
		    TO "quasi- and multi-graded polynomial rings",
		    TO "quotient rings",
		    TO "manipulating polynomials",
		    TO "factoring polynomials"
		    }
	       ),
	  ("fields",
	       MENU {
		    TO "finite fields, part II",
		    TO "fraction fields",
		    TO "finite field extensions"
		    }
	       ),
	  ("other algebras",
	       MENU {
		    TO "exterior algebras",
		    TO "symmetric algebras",
		    TO "tensor products of rings",
		    TO "Weyl algebras",
		    (TO "Schur rings", " -- monomials represent irreducible representations of GL(n)"),
		    TO "associative algebras"
		    }
	       )
       },
      "For additional common operations and a comprehensive list of all routines
     in Macaulay 2 which return or use rings, see ", TO "Ring", "."
     }

document { "ideals",
     HEADLINE => "an overview",
     "In this section, blah blah blah.",
     MENU {
	  TO "creating an ideal",
	  ("conversions",
	       MENU {
		    TO "ideals to and from matrices",
		    TO "ideals to and from modules"
		    }
	       ),
	  ("basic operations on ideals",
	       MENU {
		    TO "sums, products, and powers of ideals",
		    TO "equality and containment",
		    TO "extracting generators of an ideal",
		    TO "dimension, codimension, and degree"
		    }
	       ),
	  ("components of ideals",
	       MENU {
		    TO "intersection of ideals",
		    TO "ideal quotients and saturation",
		    TO "radical of an ideal",
		    TO "minimal primes of an ideal",
		    TO "associated primes of an ideal",
		    TO "primary decomposition"
		    }
	       ),
	  (TO "Groebner bases and related computations"),
          },
     "For those operations where we consider an ideal as a module, such
     as computing Hilbert functions and polynomials, syzygies, free resolutions, see ",
     TO "modules", ".",
     PARA,
      "For additional common operations and a comprehensive list of all routines
     in Macaulay 2 which return or use ideals, see ", TO "Ideal", "."
     }


document { "matrices",
     HEADLINE => "an overview",
     MENU {
	  ("making matrices", 
	       MENU {
		    TO "input a matrix",
		    TO "random and generic matrices",
		    TO "concatenating matrices"
		    }
	       ),
	  ("operations involving matrices",
	       MENU {
		    TO "simple information about a matrix",
		    TO "basic arithmetic of matrices",
		    TO "kernel, cokernel and image of a matrix",
		    TO "differentiation"
		    }
	       ),
	  ("determinants and related computations",
	       MENU {
		    TO "rank of a matrix",
		    TO "determinants and minors",
		    TO "Pfaffians",
		    TO "exterior power of a matrix"
		    }
	       ),
	  ("display of matrices and saving matrices to a file",
	       MENU {
		    TO "format and display of matrices in Macaulay 2",
		    TO "importing and exporting matrices"
		    }
	       )
	  },
      "For additional common operations and a comprehensive list of all routines
     in Macaulay 2 which return or use matrices, see ", TO "Matrix", "."
     }

document { "substitution and maps between rings",
     HEADLINE => "an overview",
     MENU {
	  TO "substitute values for variables",
	  TO "working with multiple rings",
	  ("ring maps",
	       MENU {
		    TO "basic construction, source and target of a ring map",
	       	    TO "evaluation and composition of ring maps",
		    TO "kernel and image of a ring map"
		    }
	       ),
	  },
      "For additional common operations and a comprehensive list of all routines
     in Macaulay 2 which return or use ring maps, see ", TO "RingMap", "."
     }
     
document { "modules I",
     HEADLINE => "getting started",
     MENU {
	  ("basic construction of modules",
	       MENU {
		    TO "construction of free modules",
		    TO "matrices to and from modules",
		    TO "ideals to and from modules"
		    }
	       ),
	  ("basic operations of modules",
	       MENU {
		    TO "extracting elements",
		    TO "equality and containment of modules",
		    TO "minimal presentations and generators",
		    TO "annihilator of a module",
		    }
	       ),
	  ("homomorphisms (maps) between modules",
	       MENU {
		    TO "constructing maps between modules",
		    TO "information about a map of modules",
		    TO "kernel, cokernel and image of a map of modules"
		    }
	       ),
	  ("graded modules",
	       MENU {
		    TO "degree and multiplicity of a module",
		    TO "Hilbert functions and polynomials",
		    TO "homogenization",
		    TO "truncation and homogeneous components of a graded module"
		    }
	       ),
	  ("subquotient modules -- the way Macaulay 2 represents modules",
	       MENU {
		    TO "what is a subquotient module?",
		    TO "extracting parts of a subquotient module",
		    TO "quotients of modules",
		    TO "direct sums of modules"
		    }
	       )
	  },
     "See ", TO "modules II", " for more operations on modules."
     }

document { "modules II",
     MENU {
	  ("multilinear algebra",
	       MENU {
		    TO "exterior power of a module",
		    TO "Fitting ideals",
		    TO "adjoints of maps"
		    }
	       ),
	  ("homological algebra",
	       MENU {
		    TO "free resolutions",
		    TO "Hom module",
		    TO "tensor products of modules",
		    TO "Tor and Ext"
		    },
	       "For more operations in homological algebra, see ", TO "chaincomplexes", "."
	       )
	  },
      "For additional common operations and a comprehensive list of all routines
     in Macaulay 2 which return or use modules, see ", TO "Module", "."
     }

document { "Groebner bases and related computations",
     MENU {
	  TO "what is a Groebner basis?",
	  TO "finding a Groebner basis",
	  ("a few applications of Groebner bases",
	       MENU {
		    TO "elimination of variables",
		    TO "Hilbert functions",
		    TO "syzygies",
		    TO "saturation",
		    TO "fibers of maps",
		    TO "solving systems of polynomial equations"
		    }
	       ),
	  TO "fine control of a Groebner basis computation"
	  }
     }

document { "external libraries",
     MENU {
     	  TO "loading a library",
     	  TO "how to get documentation on a library",
     	  ("available libraries",
	       MENU {
	       	    (TO "blow ups", " -- goodluck.m2"),
	       	    (TO "convex hulls and polar cones", " -- polarCone.m2"),
	       	    (TO "D-modules", " -- D-modules.m2"),
	       	    (TO "elimination theory", " -- eliminate.m2"),
	       	    (TO "graphing curves and surfaces via 'surf'", " -- inyourdreams.m2"),
	       	    (TO "invariants of finite groups", " -- invariants.m2"),
	       	    (TO "Lenstra-Lenstra-Lovasz (LLL) lattice basis reduction", " -- LLL.m2"),
	       	    (TO "SAGBI bases", " -- sagbi.m2")
	       	    })
	  }
     }

----------------------------
-- Lead nodes --------------
----------------------------

document { "GB blah blah",
     "In Macaulay 2, Groebner bases can be computed for ideals and submodules over many
     different rings.",
     MENU {
	  TO "over fields",
          TO "over the ring of integers",
          TO "over polynomial rings over a field",
	  TO "over polynomial rings over the integers",
	  TO "over Weyl algebras",
	  TO "over local rings"
     	  }
    }

-------------------
-- Ring nodes -----
-------------------

-------------------
-- ideal nodes ----
-------------------


-------------------
-- Matrix nodes ---
-------------------

document { "input a matrix",
     MENU {
	  TO "matrix",
	  TO "map",
	  TO "id"
	  }
     }

document { "random and generic matrices",
     MENU {
	  TO "random",
	  TO "genericMatrix",
	  TO "genericSymmetricMatrix",
	  TO "genericSkewMatrix"
	  }
     }

document { "concatenating matrices",
     MENU {
	  TO "|",
	  TO "||",
	  TO "making block matrices",
	  TO "++"
	  }
     }

document { "simple information about a matrix",
     MENU { 
	  TO "ring",
	  TO "target",
	  TO "source",
	  TO "extracting an element from a matrix",
	  TO "submatrix",
	  TO "number of rows or columns",
	  TO "entries"
	  }
     }

document { "basic arithmetic of matrices",
     MENU {
	  TO "+",
	  TO "-",
	  TO "*",
	  TO "^",
	  TO "inverse of a matrix",
	  TO "==", -- m == n, m-n == 0 are different
	  TO "!=",
	  TO "**"
	  }
     }

document { "kernel, cokernel and image of a matrix",
     MENU {
	  (TO "kernel", " -- (synonym is 'ker')"),
	  TO "image",
	  TO "cokernel"
	  }
     }

document { "differentiation",
     MENU {
	  TO "diff",
	  TO "diff'",
	  TO "contract",
	  TO "contract'",
	  TO "jacobian"
	  }
     }

document { "rank of a matrix",
     MENU {
	  TO "rank",
	  "random rank of a matrix"
	  }
     }

document { "determinants and minors",
     MENU {
	  TO "det",
	  TO "minors"
	  }
     }

document { "Pfaffians",
     MENU {
	  TO "pfaffians"
	  }
     }

document { "exterior power of a matrix",
     MENU {
	  TO "exteriorPower"
	  }
     }

document { "format and display of matrices in Macaulay 2",
     MENU {
	  TO "compactMatrixForm",
	  }
     }

document { "importing and exporting matrices",
     MENU {
	  TO "toString",
	  TO "toExternalString"
	  }
     }

-------------------
-- ring map nodes -
-------------------

document { "substitute values for variables",
     MENU {
	  TO "substitute",
	  }
     }

-------------------
-- module nodes ---
-------------------

document { "construction of free modules",
     }

document { "matrices to and from modules",
     }

document { "extracting elements",
     }

document { "equality and containment of modules",
     }

document { "minimal presentations and generators",
     }

document { "annihilator of a module",
     }

document { "constructing maps between modules",
     }

document { "information about a map of modules",
     }

document { "kernel, cokernel and image of a map of modules",
     }

document { "degree and multiplicity of a module",
     }

document { "Hilbert functions and polynomials",
     }

document { "homogenization",
     }

document { "truncation and homogeneous components of a graded module",
     }

document { "what is a subquotient module?",
     }

document { "extracting parts of a subquotient module",
     }

document { "quotients of modules",
     }

document { "direct sums of modules",
     }

document { "exterior power of a module",
     }

document { "Fitting ideals",
     }

document { "adjoints of maps",
     }

document { "free resolutions",
     }

document { "Hom module",
     }

document { "tensor products of modules",
     }

document { "Tor and Ext",
     }

-------------------
-- GB nodes -------
-------------------

document { "what is a Groebner basis?",
     }

document { "finding a Groebner basis",
     }

document { "elimination of variables",
     }

document { "Hilbert functions",
     }

document { "syzygies",
     }

document { "saturation",
     }

document { "fibers of maps",
     }

document { "solving systems of polynomial equations",
     }

document { "fine control of a Groebner basis computation",
     }

-------------------
-- library nodes --
-------------------

document { "loading a library",
     }

document { "how to get documentation on a library",
     }

document { "blow ups",
     }

document { "convex hulls and polar cones",
     }

document { "D-modules",
     }

document { "elimination theory",
     }

document { "graphing curves and surfaces via 'surf'",
     }

document { "invariants of finite groups",
     }

document { "Lenstra-Lenstra-Lovasz (LLL) lattice basis reduction",
     }

document { "SAGBI bases",
     }

-------------------
-- specialized   --
-------------------

document { "commutative algebra",
     MENU {
	  "integralClosure",
	  "primaryDecomposition",
	  "symmetricAlgebra"
	  }
     }

document { "algebraic geometry",
     MENU {
	  TO "cotangentSheaf"
	  }
     }
