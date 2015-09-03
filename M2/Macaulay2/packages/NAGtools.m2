-- A collection of algorithms that use NumericalAlgebraicGeometry and related packages. 

needsPackage "SLPexpressions"

-- Monodromy-based algorithm
-- in: 
--     PH, a homotopy from f_A to f_B, where f is a family of (polynomial or other) systems; depends on 2m parameters, m=|A|=|B| 
--     p0, column vector, values of m parameters (assumed generic)
--     s0, a nonempty list of points, solutions of PH_(p0,*)(0)
--     NextPoint, a function that returns a random column vector of m parameters p1 suitable for PH  
degreeViaMonodromy = method(Options=>{RandomPointFunction=>null,StoppingCriterion=>((n,L)->n>9)})
degreeViaMonodromy (ParameterHomotopySystem, Matrix, List) := o -> (PH,p0,s0) -> (
    if #s0 < 1 then error "at least one solution expected";   
    sols0 := s0;
    nSols := #sols0; 
    same := 0;
    dir := temporaryFileName(); -- build a directory to store temporary data 
    makeDirectory dir;
    << "--backup directory created: "<< toString dir << endl;  
    while stop() do try (
    	p1 := nextP();
    	p2 := nextP();
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

-- Parameter homotopy for getting a point on the fiber of a ***generically finite-to-one onto*** map 
-- in: 
--     F, a map (column vector)
--     V, variables (list of InputGates)
--     W (optional; W=V if omitted), variables names (list of anything) for coordinates in the target space 
-- out: 
--     HomotopySystem that has A_v and B_v as parameters, 
--     	       	      where v in V are coordinates of the target space 
gateHomotopy4preimage = method()
gateHomotopy4preimage(GateMatrix,List) := (F,V) -> gateHomotopy4preimage(F,V,V)
gateHomotopy4preimage(GateMatrix,List,List) := (F,V,W) -> (
    assert(#W == numrows F); 
    assert(#V == #W); 
    A := matrix{apply(W, v->inputGate symbol A_v)};
    B := matrix{apply(W, v->inputGate symbol B_v)};
    t := inputGate symbol t;
    H := F-((1-t)*transpose A+t*transpose B);
    gateHomotopySystem(H,matrix{V},t,Parameters=>A|B,Software=>M2engine)
    )
