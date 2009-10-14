-- done: field_by_poly 
-- todo: fraction_field, free_field

--- From OpenMath ---
OMSEvaluators#"field3" = new MutableHashTable;
OMSEvaluators#"field3"#"field_by_poly" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	if #a =!= 2 then (
		theOMerror = "field_by_poly is supposed to have two elements.";
		error("whoops");
	);
		
	R := a#0;
	p := a#1;
	
	F := toField(class(p)/p);

	try F#OpenMathPrefEltRepr = FieldByPolyVector;

	F
)

--- To OpenMath ---
toOpenMathFieldByPoly = autoCreateIDCheck(F -> (
	CR := toOpenMath coefficientRing ambient F;
	R := toOpenMath (ideal ambient F)_0;
	OMA("field3", "field_by_poly", {CR, R})
))
