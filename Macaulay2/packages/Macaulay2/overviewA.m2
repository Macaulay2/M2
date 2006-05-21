
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
