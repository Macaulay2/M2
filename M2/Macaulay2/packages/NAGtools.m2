-- A collection of algorithms that use NumericalAlgebraicGeometry and related packages. 


-- Monodromy-based algorithm
-- in: 
--     PH, a homotopy from f_A to f_B, where f is a family of (polynomial or other) systems; depends on 2m parameters, m=|A|=|B| 
--     p0, column vector, values of parameters (assumed generic)
--     s0, column vector, solution of PH    
--     nextP(seed), a functions that returns a random column vector of parameters p1 suitable for the family f  
degree (ParameterHomotopySystem, Matrix, Matrix, FunctionClosure) := (PH,p0,s0,f) -> (
    sols0 := {point s0};
    nSols := #sols0; 
    same := 0;
    while same<10 do try (
    	p1 := nextP(random 100000);
    	p2 := nextP(random 100000);
	elapsedTime sols1 = trackHomotopy(specialize(PH,p0||p1),sols0);
	sols1 = select(sols1, s->status s === Regular);
	<< "  H01: " << #sols1 << endl;
	elapsedTime sols2 = trackHomotopy(specialize(PH,p1||p2),sols1);
	sols2 = select(sols2, s->status s === Regular);
	<< "  H12: " << #sols2 << endl;
    	elapsedTime sols0' = trackHomotopy(specialize(PH,p2||p0),sols2);
	sols0' = select(sols0', s->status s === Regular);
	<< "  H20: " << #sols0' << endl;
	sols0 = solutionsWithMultiplicity(sols0 | sols0'); -- take the union	
	if #sols0 == nSols then same = same + 1 else (nSols = #sols0; same = 0);  
    	<< "found " << #sols0 << " points in the fiber so far" << endl;
    	) else print "something went wrong";
    nSols
    )
