-- done: field_by_conway, primitive_element
-- not done: conway_polynomial, discrete_log, is_primitive, is_primitive_poly, minimal_polynomial

createdGFs = new MutableHashTable;
getGF = (p,n) -> (
	q := p^n;
	r := null;
	if (createdGFs#?q) then (
		createdGFs#q
	) else (
		if n === 1 then r = GF(p) else r = GF(p,n);
		createdGFs#q = r;
		r
	)
);

--- From OpenMath ---
OMSEvaluators#"finfield1" = new MutableHashTable;
OMSEvaluators#"finfield1"#"field_by_conway" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	getGF(a#0, a#1)
)
OMSEvaluators#"finfield1"#"primitive_element" = (args, attrs) -> (
	if (#args =!= 1) then
		(theOMerror = concatenate("finfield1.primitive_element only supported with one argument; ", toString #args, " given."); error("whoops"));
	a = fromOpenMath args#0;
	if (class a =!= ZZ) then
		(theOMerror = "finfield1.primitive_element only supported with one integer argument."; error("whoops"));
	
	(getGF(a,1))_0
)


