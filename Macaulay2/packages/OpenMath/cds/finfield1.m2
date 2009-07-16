-- done: field_by_conway
-- not done: conway_polynomial, discrete_log, is_primitive, is_primitive_poly, minimal_polynomial, primitive_element

--- From OpenMath ---
OMSEvaluators#"finfield1" = new MutableHashTable;
OMSEvaluators#"finfield1"#"field_by_conway" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	GF(a#0, a#1)
)



--- To OpenMath ---
--I don't really like this too much, but can't find anything better for now.
toOpenMath QuotientRing := R -> (
	if isField(R) and (ambient(R) === ZZ) then
		OMA("finfield1", "field_by_conway", { OMI(char(R)), OMI(1) })
	else
		(theOMerror = concatenate("Cannot convert quotient ring ", toString(R), " to OpenMath"); error("whoops"));
)
toOpenMath GaloisField := F -> (
	OMA("finfield1", "field_by_conway", { OMI(F.char), OMI(F.degree) })
	
)