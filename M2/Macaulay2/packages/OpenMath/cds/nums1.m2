-- done: rational, e, pi, infinity, i, gamma
-- not done: based_integer, NaN,

--- From OpenMath ---
OMSEvaluators#"nums1" = new MutableHashTable;
OMSEvaluators#"nums1"#"rational" = (args, attrs) -> ( a := apply(args, fromOpenMath); a#0/a#1 )
OMSEvaluators#"nums1"#"pi" = pi
OMSEvaluators#"nums1"#"e" = exp(1)
OMSEvaluators#"nums1"#"infinity" = infinity
OMSEvaluators#"nums1"#"i" = ii
OMSEvaluators#"nums1"#"gamma" = Gamma;


--- To OpenMath ---
toOpenMath QQ := x -> OMA("nums1", "rational", { OMI(numerator(x)), OMI(denominator(x))})
toOpenMath Constant := x -> (
	if x === pi then 
		OMS("nums1", "pi")
	else if x === ii then
		OMS("nums1", "pi")
	else
		(theOMerror = concatenate("Cannot handle constant ", toString(x)); error("whoops"))
)
toOpenMath InfiniteNumber := x -> (
	if x === infinity then
		OMS("nums1", "infinity")
	else
		OMA("arith1", "unary_minus", { OMS("nums1", "infinity") })
)
