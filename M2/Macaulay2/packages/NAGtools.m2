-- A collection of algorithms that use NumericalAlgebraicGeometry and related packages. 


-- Monodromy-based algorithm
-- in: 
--     PH, a homotopy from f_A to f_B, where f is a family of (polynomial or other) systems; depends on 2m parameters, m=|A|=|B| 
--     p0, column vector, values of parameters (assumed generic)
--     s0, list of points, solutions of PH_p0 (at least one)    
--     nextP(seed), a functions that returns a random column vector of parameters p1 suitable for the family f  
degree (ParameterHomotopySystem, Matrix, List, FunctionClosure) := (PH,p0,s0,f) -> (
    if #s0 < 1 then error "at least one solution expected";   
    sols0 := s0;
    nSols := #sols0; 
    same := 0;
    dir := temporaryFileName(); -- build a directory to store temporary data 
    makeDirectory dir;
    << "--backup directory created: "<< toString dir << endl;  
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
	if #sols0 == nSols then same = same + 1 else (
	    nSols = #sols0; 
	    same = 0;
	    ff := openOut (dir|"/backup-"|toString nSols|"-solutions"); 
	    ff << toExternalString sols0;
	    close ff; 
	    );  
    	<< "found " << #sols0 << " points in the fiber so far" << endl;
    	) else print "something went wrong";
    nSols
    )

-- Parameter homotopy for getting a point on the fiber of an onto map 
-- in: 
--     F, a map (column vector)
--     V, variables (list)
-- out: 
--     HomotopySystem that has A_v and B_v as parameters, 
--     	       	      where v in V are coordinates of the target space 
gateHomotopy4preimage = method()
gateHomotopy4preimage(GateMatrix,List) := (F,V) -> (
    assert(#V == numrows F); 
    A := matrix{apply(V, v->inputGate symbol A_v)};
    B := matrix{apply(V, v->inputGate symbol B_v)};
    t := inputGate symbol t;
    H := F-((1-t)*transpose A+t*transpose B);
    gateHomotopySystem(H,matrix{V},t,Parameters=>A|B,Software=>M2engine)
    )
