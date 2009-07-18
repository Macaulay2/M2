-- done: determinant,outerproduct,vectorproduct, scalarproduct, transpose,  matrix_selector, vector_selector, 

--- From OpenMath ---
OMSEvaluators#"linalg1" = new MutableHashTable;
OMSEvaluators#"linalg1"#"determinant" = (args, attrs) -> det fromOpenMath args#0
OMSEvaluators#"linalg1"#"transpose" = (args, attrs) -> (
	a := fromOpenMath args#0;
	if class a === List then a = vector a;
	if class class a === Module then a = matrix{a};
	transpose a
)
OMSEvaluators#"linalg1"#"vectorproduct" = (args, attrs) -> (
	--No one will understand this, but it works!
	v := fromOpenMath args#0; if class(v) === List then v = vector v;
	w := fromOpenMath args#1; if class(w) === List then w = vector w;
	(exteriorPower_2 matrix{v,w})^{2,1,0}_0
)
OMSEvaluators#"linalg1"#"outerproduct" = (args, attrs) -> (
	v := fromOpenMath args#0; if class(v) === List then v = vector v;
	w := fromOpenMath args#1; if class(w) === List then w = vector w;
 	((transpose matrix {v}) * (matrix {w}))_(0,0)
)
OMSEvaluators#"linalg1"#"vector_selector" = (args, attrs) -> (
	<< "args = '" << args << "'" << endl;
	i := fromOpenMath args#0;
	if class(i) =!= ZZ then (theOMerror = "linalg1.vector_selector: Expects 1st argument to be integer"; error("whoops"));
	a := fromOpenMath args#1;
	if class a === List then a#(i-1)
	else if class class a === Module then a_(i-1)
	else if class a === Matrix and numRows a === 1 then a_(0,i-1)
	else if class a === Matrix and numColumns a === 1 then a_(i-1,0)
	else (
		theOMerror = concatenate("linalg1.vector_selector: Invalid argument of class ", toString class a);
		error("whoops")
	)
)
OMSEvaluators#"linalg1"#"matrix_selector" = (args, attrs) -> (
	i := fromOpenMath args#0;
	j := fromOpenMath args#1;
	if class(i) =!= ZZ then (theOMerror = "linalg1.matrix_selector: Expects 1st argument to be integer"; error("whoops"));
	if class(j) =!= ZZ then (theOMerror = "linalg1.matrix_selector: Expects 2nd argument to be integer"; error("whoops"));
	a := fromOpenMath args#2;
	a_(i-1, j-1)
)


