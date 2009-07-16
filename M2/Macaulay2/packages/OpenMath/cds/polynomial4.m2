-- This CD is experimental; A recent version may possibly be found at
--     http://www.win.tue.nl/SCIEnce/cds/polynomial4.html
--
-- done: factor, factorisations, factorisations_complete, factorisations_incomplete, factorise, factors, 
--       multiplicity
-- todo/not done: definitely_irreducible, possibly_reducible, ground_ring_injected, , 
--                divide, quotient, quotient_remainder, remainder

FactoredPolynomial = new Type of BasicList

--- To OpenMath ---
ring FactoredPolynomial := f -> f#0#0
toOpenMath FactoredPolynomial := x -> (	
	R := ring x;
	
	l := {  };
	l = append(l, toOpenMath(R));
	l = append(l, OMI(1));
	for i in x do
		l = append(l, 
			OMA("polynomial4", "factor", { 
			toOpenMath(i#0), 
			OMA("polynomial4", "multiplicity", {toOpenMath(i#1)})
		}));
	
	--Then, wrap l in some thing that people care about
	OMA("polynomial4", "factorisations", {
		OMS("polynomial4", "factorisations_complete"),
		OMA("polynomial4", "factors", l)
	})
)

--- From OpenMath ---
OMSEvaluators#"polynomial4" = new MutableHashTable;
OMSEvaluators#"polynomial4"#"factorise" = (args, expr) -> (
	pol := fromOpenMath(args#0);
	<< "pol = " << pol << endl;
	new FactoredPolynomial from factor(pol)
)
