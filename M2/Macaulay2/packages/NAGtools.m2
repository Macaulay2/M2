-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version

-- A collection of algorithms that use NumericalAlgebraicGeometry and related packages. 
newPackage select((
     "NAGtools",
     Version => "1.9",
     Date => "Apr 2016",
     Headline => "tools of NumericalAlgebraicGeometry",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     AuxiliaryFiles => false,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     PackageExports => {"NumericalAlgebraicGeometry", "SLPexpressions"},
     PackageImports => {},
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     --DebuggingMode => true
     DebuggingMode => false
     ), x -> x =!= null)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
    "preimageViaMonodromy",
    "solveViaMonodromy",
    "gateHomotopy4preimage",
    "RandomPointFunction",
    "StoppingCriterion"    
    }
exportMutable {
    }

debug NAGtypes
debug NumericalAlgebraicGeometry

-- Monodromy-based algorithm
-- in: 
--     PH, a homotopy from f_A to f_B, where f is a family of (polynomial or other) systems; depends on 2m parameters, m=|A|=|B| 
--     p0, Point, values of m parameters (assumed generic)
--     s0, a nonempty list of points, solutions of PH_(p0,*)(0)
--     RandomPointFunction, a function that returns a random point p1 suitable for PH  
preimageViaMonodromy = method(Options=>{RandomPointFunction=>null,StoppingCriterion=>((n,L)->n>3),Precision=>DoublePrecision})
preimageViaMonodromy (ParameterHomotopy, Point, List) := o -> (PH,point0,s0) -> (
    if #s0 < 1 then error "at least one solution expected";  
    p0 := transpose matrix point0; 
    nextP := if o.RandomPointFunction =!= null then o.RandomPointFunction else (
	K := ring p0;
	()->point {apply(numrows p0, i->exp(2*pi*ii*random RR))}
	); 
    sols0 := s0;
    nSols := #sols0; 
    same := 0;
    dir := temporaryFileName(); -- build a directory to store temporary data 
    makeDirectory dir;
    << "--backup directory created: "<< toString dir << endl;
    opts := new OptionTable from {Precision=>o.Precision}; 
    while not o.StoppingCriterion(same,sols0) do --try 
    (
    	p1 := transpose matrix nextP();
    	p2 := transpose matrix nextP();
	elapsedTime sols1 := trackHomotopy(specialize(PH,p0||p1),sols0,opts);
	sols1 = select(sols1, s->status s === Regular);
	<< "  H01: " << #sols1 << endl;
	elapsedTime sols2 := trackHomotopy(specialize(PH,p1||p2),sols1,opts);
	sols2 = select(sols2, s->status s === Regular);
	<< "  H12: " << #sols2 << endl;
    	elapsedTime sols0' := trackHomotopy(specialize(PH,p2||p0),sols2,opts);
	sols0' = select(sols0', s->status s === Regular);
	<< "  H20: " << #sols0' << endl;
	elapsedTime sols0 = clusterSolutions(sols0 | sols0'); -- take the union	
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
    if DBG>9 then (
	dir := temporaryFileName(); -- build a directory to store temporary data 
    	makeDirectory dir;
    	<< "--backup directory created: "<< toString dir << endl;
	);
    totalNumberOfPaths := 0;
    while not o.StoppingCriterion(same,sols0) do --try 
    (
    	p1 := matrix nextP(); -- row matrix
	F0 := flatten entries (map(R,PR,X|p0)) PF;
	F1 := flatten entries (map(R,PR,X|p1)) PF;
	totalNumberOfPaths = totalNumberOfPaths + #sols0;
	elapsedTime sols1 := track(F0,F1,sols0);
	sols1 = select(sols1, s->status s === Regular);
	<< "  H01: " << #sols1 << endl;
	totalNumberOfPaths = totalNumberOfPaths + #sols1;
    	elapsedTime sols0' := track(F1,F0,gamma=>exp(2*pi*ii*random RR),sols1);
	sols0' = select(sols0', s->status s === Regular);
	<< "  H10: " << #sols0' << endl;
	elapsedTime sols0 = clusterSolutions(sols0 | sols0'); -- take the union	
	if #sols0 == nSols then same = same + 1 else (
	    nSols = #sols0; 
	    same = 0;
	    if DBG>9 then (
	    	ff := openOut (dir|"/backup-"|toString nSols|"-solutions"); 
	    	ff << toExternalString sols0;
	    	close ff;
		); 
	    );  
    	<< "found " << #sols0 << " points in the fiber so far" << endl;
    	);
    << "totalNumberOfPaths = " << totalNumberOfPaths << endl;
    sols0
    )

-- Parameter homotopy for tracking a point on the fiber of a covering (generically finite-to-one onto) map 
-- in: 
--     F, a map (column vector)
--     V, variables (list of InputGates)
--     W (optional; W=V if omitted), variables names (list of anything) for coordinates in the target space 
-- out: 
--     Homotopy that has A_v and B_w as parameters, 
--     	       	      where v in V are coordinates of the source space 
gateHomotopy4preimage = method()
gateHomotopy4preimage(GateMatrix,List) := (F,V) -> gateHomotopy4preimage(F,V,V)
gateHomotopy4preimage(GateMatrix,List,List) := (F,V,W) -> (
    assert(#W == numrows F); 
    assert(#V == #W); 
    A := matrix{apply(W, v->inputGate symbol A_v)};
    B := matrix{apply(W, v->inputGate symbol B_v)};
    t := inputGate symbol t;
    H := F-((1-t)*transpose A+t*transpose B);
    gateHomotopy(H,matrix{V},t,Parameters=>A|B)
    )
-- in: S, polynomials desribing a subvariety of CC^V
gateHomotopy4preimage(GateMatrix,GateMatrix,List,List) := (F,S,V,W) -> (
    assert(#W == numrows F); 
    A := matrix{apply(W, w->inputGate symbol A_w)};
    B := matrix{apply(W, w->inputGate symbol B_w)};
    t := inputGate symbol t;
    H := (F-((1-t)*transpose A+t*transpose B)) || S;
    gateHomotopy(H,matrix{V},t,Parameters=>A|B)
    )

TEST ///
setRandomSeed 1
X = inputGate x
F = matrix{{X^2}} 
PH = gateHomotopy4preimage(F,{X})
K = CC_53
setDefault(Software=>M2)
SPH = specialize(PH,matrix{{1_K},{2}})
p =matrix{{1_K}}
assert areEqual(norm evaluateH(SPH,p,0), 0)
assert areEqual(norm evaluateHt(SPH,p,0), 1)
assert areEqual(norm evaluateHx(SPH,p,0), 2)
peek PH.GateHomotopy    
assert (#preimageViaMonodromy(PH,point p,{point p}) == 2)
///

beginDocumentation()
undocumented{
preimageViaMonodromy,
(preimageViaMonodromy,ParameterHomotopy,Point,List),
gateHomotopy4preimage,
(gateHomotopy4preimage,GateMatrix,GateMatrix,List,List),
(gateHomotopy4preimage,GateMatrix,List),
(gateHomotopy4preimage,GateMatrix,List,List),
StoppingCriterion,
RandomPointFunction
    }
endPackage "NAGtools" 
