--- status: DRAFT
--- author(s): 
--- notes: 

document {
     Key => {annihilator,
	  (annihilator, CoherentSheaf),
	  (annihilator, Ideal),
	  (annihilator, RingElement),
	  (annihilator, Module),
	  [annihilator, Strategy]},
     Headline => "the annihilator ideal",
     Usage => "annihilator M",
     Inputs => {
	  "M" => "a module, an ideal, a ring element, or a coherent sheaf",
	  Strategy => { TO "Quotient", " or ", TO "Intersection" }
	  },
     Outputs => {
	  { "the annihilator ideal, ", TT "ann(M) = { f in R | fM = 0 }", " where ", 
	       TT "R", " is the ring of ", TT "M"}
	  },
     "You may use ", TT "ann", " as a synonym for ", TT "annihilator", ".",
     PARA{},
     "As an example, we compute the annihilator of the canonical module of the 
     rational quartic curve.",
     EXAMPLE lines ///
	  R = QQ[a..d];
	  J = monomialCurveIdeal(R,{1,3,4})
	  M = Ext^2(R^1/J, R)
	  annihilator M
	  ///,
     "For another example, we compute the annihilator of an element
     in a quotient ring",
     EXAMPLE lines ///
	  A = R/(a*b,a*c,a*d)
	  ann a
	  ///,
     "Macaulay 2 uses two algorithms to compute annihilators.  The default
     version is to compute the annihilator of each generator of the module ",
     TT "M", " and to intersect these two by two.  Each annihilator is
     done using a submodule quotient.  The other algorithm computes the annihilator in one large
     computation and is used if ", TT "Strategy => Quotient", " is specified.", 
     EXAMPLE lines ///
	  annihilator(M, Strategy=>Quotient)
	  ///,
     SeeAlso => {(quotient, Module, Module), Ext}
     }
