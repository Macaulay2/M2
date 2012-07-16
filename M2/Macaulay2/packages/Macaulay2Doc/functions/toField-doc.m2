-- -*- coding: utf-8 -*-
--- status: Draft
--- author(s): MES
--- notes: 

document {
     Key => {(toField,Ring),toField},
     Headline => "declare that a ring is a field",
     Usage => "L = toField R",
     Inputs => {
	  "R"
	  },
     Outputs => {
	  "L" => { "a new ring, isomorphic to ", TT "R", ", declared to be a field.
	       Polynomial rings over it will support Gröbner basis operations." }
	  },
     EXAMPLE lines ///
          A = QQ[i]/(i^2+1);
	  L = toField A
	  B = L[x,y,z]
	  I = ideal(i*x^2-y-i, i*y^2-z-i)
	  gens gb I
          ///,
     PARA{
	  "If the engine eventually discovers that some nonzero element of ", TT "L", "
	  is not a unit, an error will be signalled.  The user may then use
	  ", TO "getNonUnit", " to obtain a non-invertible element of ", TT "L", ".
	  If a ring probably is a field, it can be used as a field until a
	  contradiction is found, and this may be a good way of discovering
	  whether a ring is a field."
	  },
     EXAMPLE lines ///
     A = ZZ[a]/(a^2+3);
     L = toField A
     L[x,y,z]
     try gb ideal (a*x^2-y^2-z^2, y^3, z^3) else getNonUnit L
     ///,
     SeeAlso => { getNonUnit, "try" }
     }

