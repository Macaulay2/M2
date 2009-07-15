-- done:
-- todo: field_by_poly, fraction_field, free_field

--- From OpenMath ---
OMSEvaluators#"field3" = new MutableHashTable;
OMSEvaluators#"field3"#"field_by_poly" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	if #a =!= 2 then
		OME("field_by_poly is supposed to have two elements.");
		
	F := a#0;
	p := a#1;
	toField(class(p)/p)
)
