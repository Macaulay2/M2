-- done: matrix, matrixrow, vector

--- From OpenMath ---
OMSEvaluators#"linalg2" = new MutableHashTable;
OMSEvaluators#"linalg2"#"vector" = (args, attrs) -> vector apply(args, fromOpenMath)
OMSEvaluators#"linalg2"#"matrixrow" = (args, attrs) -> toList apply(args, fromOpenMath)
OMSEvaluators#"linalg2"#"matrix" = (args, attrs) -> matrix toList apply(args, fromOpenMath)

--- To OpenMath ---
toOpenMath Matrix := m -> (
	nr := numRows m; nc := numColumns m;
	OMA("linalg2", "matrix", toList apply( 0..(nr-1), i -> 
		OMA("linalg2", "matrixrow", toList apply( 0..(nc-1), j-> toOpenMath m_(i,j) ))
	))
)

toOpenMath Vector := v -> (
	OMA("linalg2", "vector", toList apply( 0..((numgens class v)-1), i -> toOpenMath v_i) )
)
