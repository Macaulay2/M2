
document {
     Key => "rings",
     "Macaulay 2 differs from other computer algebra systems such as Maple and Mathematica, in that before making a polynomial, you must create a ring to contain it, deciding first
     the complete list of indeterminates and the type of coefficients permitted.  Recall that a ring is a set with addition and multiplication operations 
     satisfying familiar axioms, such as the distributive rule.  Examples include the ring of integers (", TO "ZZ", "), the
     ring of rational numbers (", TO "QQ", "), and the most important rings in Macaulay 2, polynomial rings.",
     PARA,
     "The sections below describe the types of rings available and how to use them.",
     Subnodes => {
	  "Rings",
	  TO "basic rings of numbers",
	  TO "integers modulo a prime",
	  TO "finite fields",
	  TO "polynomial rings",
	  TO "monomial orderings",
	  TO "graded and multigraded polynomial rings",
	  TO "quotient rings",
	  TO "manipulating polynomials",
	  TO "factoring polynomials",
	  "Fields",
	  TO "fraction fields",
	  TO "finite field extensions",
	  "Other algebras",
	  TO "exterior algebras",
	  TO "symmetric algebras",
	  TO "tensor products of rings",
	  TO "Weyl algebras",
	  -- TO "Schur rings", 
	  TO "associative algebras",
       	  },
     PARA,
     "For additional common operations and a comprehensive list of all routines
     in Macaulay 2 which return or use rings, see ", TO "Ring", "."
     }


///


///


document {
     Key => "substitution and maps between rings",
     HEADER2 "An overview",
     Subnodes => {
     	  "Substitution",
	  TO "substituting values for variables",
	  TO "working with multiple rings",
	  "Ring maps",
	  TO "basic construction, source and target of a ring map",
	  TO "evaluation and composition of ring maps",
	  TO "kernel and image of a ring map",
	  TO "preimage of an ideal",
	  },
     PARA,
      "For additional common operations and a comprehensive list of all routines
     in Macaulay 2 which return or use ring maps, see ", TO "RingMap", "."
     }
     
document {
     Key => "modules",
     Subnodes => {
	  "construction of modules",
	  TO "free modules",
	  TO "matrices to and from modules",
	  -- TO "ideals to and from modules", -- already referred to in the section on ideals
	  TO "Hilbert functions and free resolutions",
	  TO "operations on modules",
	  TO "homomorphisms (maps) between modules",
	  TO "constructing maps between modules",
	  TO "information about a map of modules",
	  TO "kernel, cokernel and image of a map of modules",
	  "graded modules",
	  TO "degrees of elements and free modules",
	  TO "degree and multiplicity of a module",
	  TO "Hilbert functions and polynomials",
	  TO "homogenization",
	  TO "truncation and homogeneous components of a graded module",
	  TO "subquotient modules",
	  "Macaulay 2 has handed you a subquotient module.  What now?",
	  TO "what is a subquotient module?",
	  TO "extracting parts of a subquotient module",
	  TO "quotients of modules",
	  "multilinear algebra",
	  TO "exterior power of a module",
	  TO "Fitting ideals",
	  TO "adjoints of maps",
	  },
     PARA,
     "For more operations in homological algebra, see ", TO "chain complexes", ".  For additional common operations and a comprehensive list of all routines
     in Macaulay 2 which return or use modules, see ", TO "Module", "."
     }


document { -- this node is used as an example in the node: Key
     Key => "chain complexes",
     "For additional common operations and a comprehensive list of all routines
     in Macaulay 2 which return or use chain complexes or maps between chain complexes, see ", 
     TO "ChainComplex", " and ", TO "ChainComplexMap", ".",
     Subnodes => {
	  TO "free resolutions of modules",
	  TO "extracting information from chain complexes",
	  TO "making chain complexes by hand",
	  TO "manipulating chain complexes",
	  TO "maps between chain complexes",
	  },
     }

document {
     Key => "varieties",
     HEADER2 "An overview",
     Subnodes => {
	  TO "algebraic varieties",
	  TO "coherent sheaves",
	  TO "Variety"
	  },
     }

----------------------------
-- Lead nodes --------------
----------------------------

document {
     Key => "rings that are available for Groebner basis computations",
     "In Macaulay 2, Groebner bases can be computed for ideals and submodules over many
     different rings.",
     Subnodes => {
	  -- we don't need all these subnodes!
	  TO "Groebner bases over fields",
          TO "Groebner bases over the ring of integers",
          TO "Groebner bases over polynomial rings over a field",
	  TO "Groebner bases over polynomial rings over the integers",
	  TO "Groebner bases over Weyl algebras",
	  TO "Groebner bases over local rings"
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


-------------------
-- ring map nodes -
-------------------

-------------------
-- module nodes ---
-------------------


-------------------
-- GB nodes -------
-------------------


-------------------
-- library nodes --
-------------------

document {
     Key => "loading a library",
     }

document {
     Key => "how to get documentation on a library",
     }

document {
     Key => "blow ups",
     }

document {
     Key => "convex hulls and polar cones",
     Headline => "polarCone.m2"
     }

document {
     Key => "Dmodules",
     Headline => "Dmodules.m2",
     }

document {
     Key => "elimination theory",
     Headline => "eliminate.m2",
     }

document {
     Key => "graphing curves and surfaces via 'surf'",
     }

document {
     Key => "invariants of finite groups",
     Headline => "invariants.m2",
     }

document {
     Key => "Lenstra-Lenstra-Lovasz (LLL) lattice basis reduction",
     Headline => "LLL.m2",
     }

document {
     Key => "SAGBI bases",
     Headline => "sagbi.m2",
     }

-------------------
-- specialized   --
-------------------

document {
     Key => "commutative algebra",
     }

document {
     Key => "algebraic geometry",
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
