--- status: Draft
--- author(s): MES
--- notes: 

undocumented {
     (toField, FractionField)
     }

document {
     Key => {(toField,Ring),toField},
     Headline => "declare that a ring is a field",
     Usage => "toField R",
     Inputs => {
	  "R"
	  },
     Consequences => {
	  { "The ring ", TT "R", " is declared to be a field." }
	  },
     EXAMPLE lines ///
          A = QQ[i]/(i^2+1);
	  toField A
	  B = A[x,y,z]
	  I = ideal(i*x^2-y-i, i*y^2-z-i)
	  gens gb I
          ///,
     "The declaration is accomplished by setting ", TT "R.isField", " to be ", TT "true", ",
     and, in case the ring is a ring handled by the engine, informing the
     engine.  Polynomial rings over rings declared to be fields support
     Groebner basis operations.",
     PARA{},
     "If the engine eventually discovers that some nonzero element of ", TT "R", "
     is not a unit, an error will be signalled.  The user may then use
     ", TO "getNonUnit", " to obtain a non-invertible element of ", TT "R", ".
     If a ring is probably a field, it can be used as a field until a
     contradiction is found, and this may be a good way of discovering
     whether a ring is a field.",
     SeeAlso => getNonUnit
     }

