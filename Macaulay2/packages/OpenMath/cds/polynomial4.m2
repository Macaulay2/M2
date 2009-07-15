-- This CD is experimental; A recent version may possibly be found at
--     http://www.win.tue.nl/SCIEnce/cds/polynomial4.html
--
-- done:
-- todo/not done: definitely_irreducible, divide, factor, factorisations, factorisations_complete, 
--                factorisations_incomplete, factorise, factors, ground_ring_injected, multiplicity, 
--                possibly_reducible, quotient, quotient_remainder, remainder

--- To OpenMath ---
toOpenMathFactoredPoly := x -> (

)

--- From OpenMath ---
OMSEvaluators#"polynomial4" = new MutableHashTable;
OMSEvaluators#"polynomial4"#"factorise" = args -> (
	pol := fromOpenMath(args#0);
	print pol;
	true
)
