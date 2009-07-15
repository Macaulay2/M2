-- done:field_by_poly_vector, 
-- todo: automorphism_group, field_by_poly_map, homomorphism_by_generators

--- From OpenMath ---
OMSEvaluators#"field4" = new MutableHashTable;
OMSEvaluators#"field4"#"field_by_poly_vector" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	if #a =!= 2 then
		OME("field4.field_by_poly_vector comes with 2 arguments.");
		
	--The field and its generator
	R := a#0;
	g := (generators(R))#0;
	
	--The element
	r := 0;
	for i in 0..((R.degree)-1) do
		r = r + ((a#1)#i)*g^i;
		
	--Done!
	r
)


--- To OpenMath ---
toOpenMathFFelt = p -> (
	R := class t;
	
	--Get the polynomial (coefficients) for the element
	lst := listForm(lift(t, ambient(R)));
	r := new MutableList from apply(1..(R.degree), i->0);
	for i in lst do r#(((i#0)#0)) = i#1;
	r = new List from r;
	r = toOpenMath r;
	
	--Get the field itself
	omR := toOpenMath R;
	
	--Done!
	OMA("field4", "field_by_poly_vector", {omR, r})
)
