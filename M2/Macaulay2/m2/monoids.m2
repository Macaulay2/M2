--		Copyright 1994 by Daniel R. Grayson

Monoid = new Type of Type

ZZ _ Monoid := (i,M) -> (
     if i === 1 then M#1
     else error "expected integer to be 1"
     )

document { quote Monoid,
     TT "Monoid", " -- denotes the class of all monoids.",
     PARA,
     "A monoid is a set with a multiplicative operation on
     it and an identity element.",
     MENU {
	  TO "OrderedMonoid",
	  TO "GeneralOrderedMonoid",
	  TO "GeneralOrderedGroup"
	  },
     "Methods for creating monoids:",
     MENU {
	  TO "group",
	  TO "monoid"
	  },
     "Operations on monoids:",
     MENU {
	  (TO (quote **,Monoid,Monoid), " -- product of monoids."),
	  (TO "generators", "        -- get the generators of the monoid"),
	  (TO "1_G", "         -- get the unit element"),
	  (TO "G_i", "         -- get a generator from a monoid"),
	  (TO "Ring Monoid", " -- make a monoid ring")
	  },
     PARA,
     "Keys:",
     MENU {
	  TO "index",
	  TO "generatorSymbols"
	  }
     }

document { "Ring Monoid",
     TT "R M", " -- makes a monoid ring from a ring and a monoid.",
     SEEALSO ("Ring", "Monoid")
     }

document { "1_G",
     TT "1_G", " -- provides the unit element of a group or monoid"
     }

document { "G_i",
     TT "G_i", " -- yields the i-th generator, if present, of the monoid G",
     PARA,
     SEEALSO( "_", "Monoid")
     }

baseName Symbol := identity

OrderedMonoid = new Type of Monoid
degreeLength OrderedMonoid := M -> M.degreeLength

document { quote OrderedMonoid,
     TT "OrderedMonoid", " -- the class of all ordered monoids.",
     PARA,
     "An ordered monoid is a multiplicative monoid together with an ordering of 
     its elements.  The ordering is required to be compatible with the 
     multiplication in the sense that if x < y then x z < y z.  The class
     of all ordered monomials is ", TO "OrderedMonoid", ".",
     PARA,
     "The reason for making a separate class for ordered monoids is that monoid
     rings can be implemented more efficiently for them - an element of the 
     monoid ring can be stored as a sorted list, each element of which is
     a pair consisting of an element of the monoid and a coefficient.
     See ", TO "PolynomialRing", ".",
     PARA,
     "A free commutative ordered monoid can be created with ", TO "monoid", ".",
     MENU {
	  TO "<",
	  TO "<=",
	  TO ">",
	  TO ">=",
	  TO "?"
	  },
     SEEALSO  ("Monoid", "group")
     }     
