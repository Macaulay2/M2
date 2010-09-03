-- done: field_by_conway, primitive_element
-- not done: conway_polynomial, discrete_log, is_primitive, is_primitive_poly, minimal_polynomial

--- To OpenMath ---
toOpenMathFFElt_PowerOfPrimElt = x -> (
	if (x == 0) then (
		recentOMProblem = concatenate("toOpenMathFFElt_PowerOfPrimElt applied to zero element");
		print "WARNING -- "|recentOMProblem;
		return toOpenMath 0;
	);

	print "WARNING THIS IS HORRIBLY SLOW AND INEFFICIENT AND SHOULD BE FIXED";
		
	a := (generators class x)_0;
	q := (class x).order;
	r := a^0;
	i := 0;

	while (i < q) do (
		if (r == x) then break; 
		r = r*a;
		i = i+1;
	);
	
	if (i >= q) then (theOMerror = "Infinite loop in finfield1.primitive_element."; error("whoops"));

	OMA("arith1", "power", {OMA("finfield1", "primitive_element", {toOpenMath q}), toOpenMath i })
)

--- From OpenMath ---
OMSEvaluators#"finfield1" = new MutableHashTable;
OMSEvaluators#"finfield1"#"field_by_conway" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	getGF(a#0, a#1)
)
OMSEvaluators#"finfield1"#"primitive_element" = (args, attrs) -> (
	if (#args =!= 1) then
		(theOMerror = concatenate("finfield1.primitive_element only supported with one argument; ", toString #args, " given."); error("whoops"));
		
	a := fromOpenMath args#0;
	if (class a =!= ZZ) then
		(theOMerror = "finfield1.primitive_element only supported with one integer argument."; error("whoops"));
		
	F := getGF(a, 1);
	try F#OpenMathPrefEltRepr = PowerOfPrimitiveElement;
	
	F_0
)


