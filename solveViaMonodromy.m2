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
    
-- ideally, this function wouldn't be necessary. Currently there's a bug in PHCpack.m2 that
-- causes it to break when there's a variable named "e". I spent a while trying to fix it,
-- but it was difficult to fix cleanly. Instead, I chose to write a function that maps 
-- to a new ring with variables that phc will tolerate, and then computes the mixed volume.
needsPackage "PHCpack";
computeMixedVolume = method()
computeMixedVolume (List) := polys -> (
  R1 := ring polys#0;
  R1Gens := gens R1;
  numDigits := length (toString (#R1Gens));
  R2 := (coefficientRing R1) (for i in 1..#R1Gens list (
    value ("x" | demark ("",for i from 1 to numDigits-(length toString i) list "0") | toString i)
  ) );
  R2Gens := gens R2;
  generatorMapping := for i in 0..#(gens R1) - 1 list (R1Gens#i =>R2Gens#i);
  ringMap := map(R2, R1, generatorMapping);
  mixedVolume (polys/ringMap)
)    

diffSolutions = method(TypicalValue=>Sequence, Options=>{Tolerance=>1e-3})
-- in:  A, B (presumably sorted)
-- out: (a,b), where a and b are lists of indices where A and B differ
diffSolutions (List,List) := o -> (A,B) -> (
     i := 0; j := 0;
     a := {}; b := {};
     while i<#A and j<#B do 
     if areEqual(A#i,B#j) then (i = i+1; j = j+1)
     else if isGEQ(A#i,B#j) then (b = append(b,j); j = j+1)
     else (a = append(a,i); i = i+1);	  
     (a|toList(i..#A-1),b|toList(j..#B-1))	      	    
     )

{* 
The idea of this version is experimenting with making one graph that has 
some a number of nodes n = 1 + ExtraNodeCount. This function loops around
these n loops until the number of solutions stabilizes, as in the original
version. When ExtraNodeCount = 1, this is the minimal graph version.
*}
solveViaMonodromyOneLoop = method(Options=>{RandomPointFunction=>null,ExtraNodeCount=>1})
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
    allowableThreads := 4;
    mvFunctionClosure := polys -> () -> computeMixedVolume polys;
    domvComputation := mvFunctionClosure F0;
    mvTask := schedule domvComputation;
    mvComputationCompleted := false;
    FList := for i in 1..o.ExtraNodeCount list(p1 := matrix nextP(); flatten entries (map(R,PR,X|p1)) PF);
    FList = {F0} | FList;
    solsList := for i in 1..o.ExtraNodeCount list({});
    solsList = {sols0} | solsList;
    solsList = new MutableList from solsList;
    previousLoopSolsList := new MutableList from for i in 0..o.ExtraNodeCount list({});
    gammaList := new MutableList;
    same := 0;
    nPathsTracked := 0;
    mv := -1;
    while true do (
        elapsedTime for i in 0..#FList - 1 do (
          ind1 := i;
          ind2 := ((i+1)%(#FList));
          F0 := FList#ind1;
          F1 := FList#ind2;
          gammaList#ind1 = exp(2*pi*ii*random RR);
          NewSols1 := track(F0,F1, gamma=>gammaList#ind1, trackbackwards=>false, solsList#ind1);
          nPathsTracked = nPathsTracked + #(solsList#ind1);
          NewSols1 = select(NewSols1, s->status s === Regular);
          previousLoopSolsList#ind2 = sortSolutions(NewSols1);

	  sols1 := clusterSolutions(solsList#ind2 | NewSols1); -- take the union
          solsList#ind2 = sortSolutions(sols1);
          << "i:" << i << ". " << #NewSols1 << " , " << #sols1 << ", " << endl;
          << "number of paths tracked: " << nPathsTracked << endl;

          if isReady mvTask then (
            mv = taskResult mvTask;
            << "Mixed volume computation completed! MV = ", << mv << endl;
            mvComputationCompleted = true;
          );

          if mvComputationCompleted then (
            if #(solsList#ind2) == mv then (
              for j in 0..ind2 do (
                startIndex := ind2 - j;
                endIndex := startIndex - 1;
                if startIndex == 0 then return nPathsTracked;

                diffs := diffSolutions(previousLoopSolsList#startIndex, solsList#startIndex);
                ptsToTrack := for diff in diffs#1 list (solsList#startIndex#diff);
                newSols := track(FList#startIndex,gammaList#endIndex*(FList#endIndex), gamma=>1,ptsToTrack);
                nPathsTracked = nPathsTracked + #ptsToTrack;
                newSols = select(newSols,s->status s === Regular);
                solsList#endIndex = sortSolutions(clusterSolutions(solsList#endIndex | newSols));
                << "number of paths tracked: " << nPathsTracked << endl;
              )
            );
          );
        );

        if mvComputationCompleted then (
          if #(solsList#0) == mv then break;
        );
    ) -- else print "something went wrong"
    ;
    solsList#0
    )