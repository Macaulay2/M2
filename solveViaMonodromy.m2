debug needsPackage "NumericalAlgebraicGeometry"
debug NAGtypes
-- in: PF, a system of polynomials in a ring of the form CC[parameters][variables]
--     point0, (as above)
--     s0, (as above)
solveViaMonodromy = method(Options=>{RandomPointFunction=>null,StoppingCriterion=>((n,L)->n>3)})
solveViaMonodromy (Matrix, Point, List) := o -> (PF,point0,s0) -> (
    if #s0 < 1 then error "at least one solution expected";  
    p0 := matrix point0; -- points are row matrices
    nParameters := numgens coefficientRing ring PF;
    assert(nParameters == numcols p0);
    (PR,toPR) := flattenRing ring PF; -- ring PF = C[a][x]
    	 -- toPR: ring PF -> PR
    X := drop(gens PR, -nParameters); 
    PF = toPR PF;
    C := coefficientRing PR;
    R := C[X];
    X = vars R;
    nextP := if o.RandomPointFunction =!= null then o.RandomPointFunction else (
	K := ring p0;
	()->point {apply(numcols p0, i->exp(2*pi*ii*random RR))}
	); 
    sols0 := s0;
    nSols := #sols0; 
    same := 0;
    dir := temporaryFileName(); -- build a directory to store temporary data 
    makeDirectory dir;
    nPathsTracked := 0;
    << "--backup directory created: "<< toString dir << endl;
    while not o.StoppingCriterion(same,sols0) do --try 
    (
    	p1 := matrix nextP(); -- row matrix
	F0 := flatten entries (map(R,PR,X|p0)) PF;
	F1 := flatten entries (map(R,PR,X|p1)) PF;
	elapsedTime sols1 := track(F0,F1,sols0);
        nPathsTracked = nPathsTracked + #sols0;
	sols1 = select(sols1, s->status s === Regular);
	<< "  H01: " << #sols1 << endl;
    	elapsedTime sols0' := track(F1,F0,gamma=>exp(2*pi*ii*random RR),sols1);
        nPathsTracked = nPathsTracked + #sols1;
	sols0' = select(sols0', s->status s === Regular);
	<< "  H10: " << #sols0' << endl;
	elapsedTime sols0 = clusterSolutions(sols0 | sols0'); -- take the union	
    << "number of paths tracked: " << nPathsTracked << endl;
	if #sols0 == nSols then same = same + 1 else (
	    nSols = #sols0; 
	    same = 0;
	    ff := openOut (dir|"/backup-"|toString nSols|"-solutions"); 
	    ff << toExternalString sols0;
	    close ff; 
	    );  
    	<< "found " << #sols0 << " points in the fiber so far" << endl;
    	) -- else print "something went wrong"
    ;
    sols0
    )    
    
{* 
The idea of this version is experimenting with making one graph that has 
some a number of nodes n = 1 + ExtraNodeCount. This function loops around
these n loops until the number of solutions stabilizes, as in the original
version. When ExtraNodeCount = 1, this is the minimal graph version.
*}
solveViaMonodromyOneLoop = method(Options=>{RandomPointFunction=>null,StoppingCriterion=>((n,L)->n>3),ExtraNodeCount=>1})
solveViaMonodromyOneLoop (Matrix, Point, List) := o -> (PF,point0,s0) -> (
    if #s0 < 1 then error "at least one solution expected";  
    p0 := matrix point0; -- points are row matrices
    nParameters := numgens coefficientRing ring PF;
    assert(nParameters == numcols p0);
    (PR,toPR) := flattenRing ring PF; -- ring PF = C[a][x]
    	 -- toPR: ring PF -> PR
    X := drop(gens PR, -nParameters);
    PF = toPR PF;
    C := coefficientRing PR;
    R := C[X];
    X = vars R;
    nextP := if o.RandomPointFunction =!= null then o.RandomPointFunction else (
	K := ring p0;
	()->point {apply(numcols p0, i->exp(2*pi*ii*random RR))}
	); 
    sols0 := s0;
    nSols := #sols0;
    F0 := flatten entries (map(R,PR,X|p0)) PF;
    FList := for i in 1..o.ExtraNodeCount list(p1 := matrix nextP(); flatten entries (map(R,PR,X|p1)) PF);
    FList = {F0} | FList;
    solsList := for i in 1..o.ExtraNodeCount list({});
    solsList = {sols0} | solsList;
    solsList = new MutableList from solsList;
    same := 0;
    nPathsTracked := 0;
    dir := temporaryFileName(); -- build a directory to store temporary data 
    makeDirectory dir;
    << "--backup directory created: "<< toString dir << endl;
    while not o.StoppingCriterion(same,sols0) do --try 
    (
        elapsedTime for i in 0..#FList - 1 do (
          F0 := FList#i;
          F1 := FList#((i+1)%(#FList));
          NewSols1 := track(F0,F1,gamma=>exp(2*pi*ii*random RR),solsList#i);
          nPathsTracked = nPathsTracked + #(solsList#i);
          NewSols1 = select(NewSols1, s->status s === Regular);
	  sols1 := clusterSolutions((solsList#((i+1)%(#FList))) | NewSols1); -- take the union
          solsList#((i+1)%(#FList)) = sols1;
          << "i:" << i << ". " << #NewSols1 << " , " << #sols1 << endl;
        );
        << "number of paths tracked: " << nPathsTracked << endl;
	if #solsList#0 == nSols then same = same + 1 else (
	    nSols = #solsList#0;
	    same = 0;
	    ff := openOut (dir|"/backup-"|toString nSols|"-solutions"); 
	    ff << toExternalString sols0;
	    close ff; 
	    );  
    	<< "found " << #solsList#0 << " points in the fiber so far" << endl;
    	) -- else print "something went wrong"
    ;
    solsList#0
    )
