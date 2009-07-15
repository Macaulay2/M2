-- done:  elimination, graded_lexicographic, graded_reverse_lexicographic, lexicographic, 
--        reverse_lexicographic, weighted, weighted_degree
-- not done: matrix_ordering, 
-- don't know if I should right here: ordering

OMSEvaluators#"polyd2" = new MutableHashTable;
OMSEvaluators#"polyd2"#"weighted_degree" = (args, attrs) -> (
	r := degree(fromOpenMath(args));
	if #r =!= 1 then 
		return OME "polyd2.weighted_degree: result of degree has length different from 1";
		
	r#0
)
OMSEvaluators#"polyd2"#"graded_lexicographic" = GLex;
OMSEvaluators#"polyd2"#"graded_reverse_lexicographic" = GRevLex;
OMSEvaluators#"polyd2"#"lexicographic" = Lex;
OMSEvaluators#"polyd2"#"reverse_lexicographic" = RevLex;

OMSEvaluators#"polyd2"#"weighted" = (args, attrs) -> (
	--"The first argument is a list of integers to act as variable weights,and the second is an ordering. "
	--
	--Must remember! e.g. weighted([-1,2,3,4], graded_lexicographic) should imply:
	--  R = QQ[a..d,Degrees=>{-1,2,3,4},MonomialOrder=>{GLex}]
	--So should do something special here.
	
	if not isOMAOf(args#0, "list1", "list") then
		return OME "1st argument of polyd2.weighted should be a list of integers";
	
	wts := fromOpenMath(args#0);
	ord := fromOpenMath(args#1);
	{ wts, ord }
)


OMSEvaluators#"polyd2"#"elimination" = (args, attrs) -> (
	--"This is an ordering, which is partially in terms of one
	--        ordering, and partially in terms of another.
	--        First argument is a number of variables.
	--        Second is ordering to apply on the first so many variables.
	--        Third is an ordering on the rest, to be used to break ties."
	--R = QQ[a..i, MonomialOrder => {Eliminate 3, GLex}];
	
	n := fromOpenMath(args#0);
	if class(n) =!= ZZ then
		return OME "1st argument of polyd2.elimination should be an integer";
	
	o1 := fromOpenMath(args#1);
	if o1 =!= GRevLex then
		return OME "Macaulay2 assumes GRevLex within the variables to be eliminated";
	
	o2 := fromOpenMath(args#2);
	
	{ Weights => {n:1} , o2 }
)

OMSEvaluators#"polyd2"#"matrix_ordering" = (args, attrs) -> (
    -- "The argument is a matrix with as many columns as indeterminates
    -- (= rank). Each row in turm is multiplied by the column vector of
    -- exponents to produce a weighting for comparison purposes."

)



