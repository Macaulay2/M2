-- done: poly_ring_d, poly_ring_d_named, DMP, SDMP, term, ambient_ring, variables, rank,
--       minus, plus, power, times
-- half done: DMPL (handling the polynomial ordering is todo)
-- not done:anonymous, 
--
-- Note that we replace "_", which GAP quite frequently uses in variables of polynomial rings,
-- by "$". And on the way out we do the inverse replacement. Just... because. 
-- Maybe we should do something else. Not sure.

--- To OpenMath ---
toOpenMath PolynomialRing := autoCreateIDCheck(R -> (
	vars := apply(gens R, i->OMV(replace("\\$", "_", toString(i))));
	OMA("polyd1", "poly_ring_d_named", prepend(toOpenMath(coefficientRing(R)), vars))
))
toOpenMathTerm = p -> prepend(toOpenMath(p#1), apply(p#0,i->toOpenMath(i)))
toOpenMathSDMP = p -> (
	OMA("polyd1", "SDMP", 
		apply(listForm(p), i->OMA("polyd1", "term", toOpenMathTerm(i)))
	)
)
toOpenMathPoly = p -> (
	ring := toOpenMath(class(p));
	terms := toOpenMathSDMP(p);

	OMA("polyd1", "DMP", {ring, terms})
)

toOpenMathDMPL = l -> (
	--Assumes l is a list of polynomials, over the same polynomial ring...
	R := toOpenMath(class(l#0));
	pols := apply(l, toOpenMathSDMP);
	OMA("polyd1", "DMPL", prepend(R, pols))
)



--- From OpenMath ---

OMSEvaluators#"polyd1" = new MutableHashTable;
OMSEvaluators#"polyd1"#"poly_ring_d_named" = (args, attrs) -> ( 
	a := apply(args, fromOpenMath); 

	--First argument is the coefficient ring
	CR := a#0;
	--Rest of the arguments is the variables
	vars := take(a, {1, #a-1});
	
	--Done!
	CR(new Array from vars)
)
OMSEvaluators#"polyd1"#"poly_ring_d" = (args, attrs) -> ( 
	if #args =!= 2 then (theOMerror = "Wrong number of arguments to polyd1.poly_ring_d: Should be 2"; error("whoops"));
	if (args#1).tag =!= "OMI" then (theOMerror = "2nd argument to polyd1.poly_ring_d should be an OMI"; error("whoops"));
	numvars := fromOpenMath(args#1);
	vars := apply( new List from 1..numvars, i->OMV(concatenate("x", toString(i))));
	fromOpenMath(OMA("polyd1", "poly_ring_d_named", prepend(args#0, vars)))
)

--Parses a term/an SDMP given the polynomial ring where the expression lives
evalterm = (obj, R) -> (
	args := take(obj.children, {1, #(obj.children)-1});

	gensR := gens(R);
	if #args =!= (1 + #gensR) then
		(theOMerror = "polyd1.term should have 1+#gensR arguments"; error("whoops"));

	for i in take(args, {1,#args-1}) do (
		if i.tag =!= "OMI" then 
			return (theOMerror = "polyd1.term should have integer powers of the generators of the poly ring"; error("whoops"));
	);
	
	fromOpenMath(args#0)*product(
		apply(new List from 0..(#gensR-1), i->(gensR#i)^(fromOpenMath(args#(i+1))))
	)
)
evalSDMP = (obj, R) -> (
	r := 0;
	trms := take(obj.children, {1, #(obj.children)-1});
	
	for t in trms do (
		if isOMAOf(t, "polyd1", "term") then
			r = r + evalterm(t, R)
		else
			return OME concatenate("Cannot handle argument to SDMP: ", toString(r));
	);

	r
)

OMSEvaluators#"polyd1"#"DMP" = (args, attrs) -> ( 
	--First argument is polynomial ring, 
	--second is list of expressions
	R := fromOpenMath(args#0);
	if class(R) =!= PolynomialRing then 
		return OME concatenate("1st argument of polyd1.DMP should be a polynomial ring (is a ",toString class R,").");

	if isOMAOf(args#1, "polyd1", "SDMP") then
		r := evalSDMP(args#1, R)
	else
		return OME concatenate("Cannot handle 2nd argument of polyd1.DMP: "+ toString(args#1));
		
	r
)

OMSEvaluators#"polyd1"#"DMPL" = (args, attrs) -> (
	--First argument is polynomial ring
	--Rest should be SDMP's
	R := fromOpenMath(args#0);
	if class(R) =!= PolynomialRing then
		return OME "1st argument of polyd1.DMPL should be a polynomial ring.";
	
	for v in take(args, {1, #args-1}) do
		if not isOMAOf(v, "polyd1", "SDMP") then
			return OME "polyd1.DMPL should have as 2nd .. end arguments only SDMPs";
	
	apply(take(args, {1, #args-1}), p -> evalSDMP(p, R))
)


OMSEvaluators#"polyd1"#"ambient_ring" = (args, attrs) -> class(fromOpenMath(args#0));
OMSEvaluators#"polyd1"#"variables" = (args, attrs) -> gens(fromOpenMath(args#0))
OMSEvaluators#"polyd1"#"rank" = (args, attrs) -> (
	a := args#0;
	if (isOMAOf(a, "polyd1", "poly_ring_d_named")) then
		#(gens(fromOpenMath(a)))
	else if (isOMAOf(a, "polyd1", "poly_ring_d")) then
		#(gens(fromOpenMath(a)))
	else if (isOMAOf(a, "polyd1", "DMP")) then (
		#(gens(fromOpenMath((a.children)#1)))
	) else
		return OME concatenate("Cannot handle polyd1.rank of: " + toString(a))
)


OMSEvaluators#"polyd1"#"plus" = (args, attrs) -> (
	a := args#0;
	if not isOMAOf(a, "polyd1", "DMPL") then 
		return OME "Argument to polyd1.plus should be a DMPL";
	
	sum(fromOpenMath(a))
)
OMSEvaluators#"polyd1"#"minus" = (args, attrs) -> (
	a := args#0;
	if not isOMAOf(a, "polyd1", "DMPL") then 
		return OME "Argument to polyd1.minus should be a DMPL";
	
	l := fromOpenMath(a);
	if #l =!= 2 then 
		return OME "Can only polyd1.minus on 2 arguments.";
		
	l#0 - l#1
)	
OMSEvaluators#"polyd1"#"times" = (args, attrs) -> (
	a := args#0;
	if not isOMAOf(a, "polyd1", "DMPL") then 
		return OME "Argument to polyd1.times should be a DMPL";
	
	product(fromOpenMath(a))
)
OMSEvaluators#"polyd1"#"power" = (args, attrs) -> (
	if not isOMAOf(args#0, "polyd1", "DMP") then 
		return OME "1st argument to polyd1.power should be a DMP";
	if not (args#1).tag === "OMI" then
		return OME "2nd argument to polyd1.power should be an OMI";
	
	(fromOpenMath(args#0))^(fromOpenMath(args#1))
)

