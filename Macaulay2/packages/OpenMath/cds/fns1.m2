-- done: identity, lambda
-- not done / todo: domain, domainofapplication, image, inverse, left_compose, left_inverse, range, right_inverse

--- From OpenMath ---
OMSEvaluators#"fns1" = new MutableHashTable;
OMSEvaluators#"fns1"#"identity" = args -> ( a := apply(args, fromOpenMath); a#0 )
OMSEvaluators#"fns1"#"lambda" = args -> ( 
	--This is for use with OMBIND
	<< "args = " << args << endl;
	renderXML(args#0, 0);
	a := apply(args, fromOpenMath);
	a#0
)
