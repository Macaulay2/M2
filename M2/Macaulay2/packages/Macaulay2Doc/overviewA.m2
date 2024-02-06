
document {
     Key => "rings",

     "Macaulay2 differs from other computer algebra systems such as 
     Maple and Mathematica, in that before making a polynomial, 
     you must create a ring to contain it, deciding first
     the complete list of indeterminates and the type of coefficients 
     permitted.  Recall that a ring is a set with addition and multiplication operations 
     satisfying familiar axioms, such as the distributive rule.  
     Examples include the ring of integers (", TO "ZZ", "), the
     ring of rational numbers (", TO "QQ", "), and the most 
     important rings in Macaulay2, polynomial rings.",

     PARA{},
     "The sections below describe the types of rings available and how to use them.",
     Subnodes => {
	  "Rings",
	  TO "basic rings of numbers",
	  TO "integers modulo a prime",
	  TO "finite fields",
	  TO "polynomial rings",
	  TO "monoid",
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
     PARA{},
     "For additional common operations and a comprehensive list of all routines
     in Macaulay2 which return or use rings, see ", TO "Ring", "."
     }


document { -- this node is used as an example in the node: Key
     Key => "chain complexes",
     "For additional common operations and a comprehensive list of all routines
     in Macaulay2 which return or use chain complexes or maps between chain complexes, see ", 
     TO "ChainComplex", " and ", TO "ChainComplexMap", ".",
     Subnodes => {
	  TO "free resolutions of modules",
	  TO "extracting information from chain complexes",
	  TO "making chain complexes by hand",
	  TO "manipulating chain complexes",
	  TO "maps between chain complexes",
	  },
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
