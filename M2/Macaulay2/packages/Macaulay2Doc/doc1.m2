-- -*- coding: utf-8 -*-

document {
     Key => {isIsomorphism,(isIsomorphism, Matrix)},
     Headline => "whether a map is an isomorphism",
     TT "isIsomorphism f", " -- whether the map f of modules is an isomorphism."
     }
document {
     Key => {complete,(complete, GradedModule),(complete, ChainComplexMap)},
     TT "complete C", " -- completely fills out the chain complex C by
     calling upon the engine to provide the maps and modules computed
     by ", TO "resolution", ".",
     PARA{},
     "This is mainly intended for developers of new routines for chain
     complexes that have to make use of their internal structure.
     Before running this routine, it is not possible to determine which
     spots in a chain complex are actually occupied by modules or maps."
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
