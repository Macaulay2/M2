-- symbols in polyd1: ambient_ring, anonymous, DMP, DMPL, minus, plus, 
--                    poly_ring_d, poly_ring_d_named, power, rank, SDMP, 
--                    term, times, variables
--
-- Note that we replace "_", which GAP quite frequently uses in variables of polynomial rings,
-- by "$". And on the way out we do the inverse replacement. Just... because. 
-- Maybe we should do something else. Not sure.

--- To OpenMath ---
toOpenMath PolynomialRing := R -> (
	vars := apply(gens R, i->OMV(replace("\\$", "_", toString(i))));
	OMA("polyd1", "poly_ring_d_named", prepend(toOpenMath(coefficientRing(R)), vars))
)

toOpenMath RingElement := p -> (
	ring := toOpenMath(class(p));
	terms := OMA("polyd1", "SDMP", 
		apply(listForm(p), i->OMA("polyd1", "term", { i#0, i#1 }))
	);

	OMA("polyd1", "DMP", {ring, terms})
)


--- From OpenMath ---

OMSEvaluators#"polyd1" = new MutableHashTable;
OMSEvaluators#"polyd1"#"poly_ring_d_named" = args -> ( 
	a := apply(args, fromOpenMath); 

	--First argument is the coefficient ring
	CR := a#0;
	--Rest of the arguments is the variables
	vars := {};
	for v in take(a, {1,#a-1}) do (
		if v#tag =!= "OMV" then error("poly_ring_d_named should have variables.");
		
		vname := v#"name";
		vname = replace("_", "$", toString(vname));
		if regex("^[a-zA-Z][a-zA-Z0-9\\$]*$", vname) === null then 
			error(concatenate("Illegal variable name: '", v#"name", "'"));

		vars = append(vars, value(concatenate("symbol ", vname)));
	);

	--Done!
	CR(new Array from vars)
)
OMSEvaluators#"polyd1"#"poly_ring_d" = args -> ( 
	if #args =!= 2 then error("Wrong number of arguments to polyd1.poly_ring_d: Should be 2");
	if (args#1).tag =!= "OMI" then error("2nd argument to polyd1.poly_ring_d should be an OMI");
	numvars := fromOpenMath(args#1);
	vars := apply( new List from 1..3, i->OMV(concatenate("x", toString(i))));
	fromOpenMath(OMA("polyd1", "poly_ring_d_named", prepend(args#0, vars)))
)
