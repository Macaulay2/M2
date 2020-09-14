-------------------------------------------------------------
-- These are functions setting up monodromy-based computation
-- (not used by anything else at the moment; 
--  something to look at in the future)
----------------------------------------


solveSchubertProblemViaMonodromy = method(Options=>{Verbose=>false})
solveSchubertProblemViaMonodromy (List, ZZ, ZZ) := o -> (problem, k, n) -> (
    G := solveRandomSchubertProblemViaMonodromy(problem/first, k, n);
    -- create a point P in the parameter space corresponding to problem
    -- augment G with a node P
    -- complete P
    -- parse the solutions at P (as Schubert problem solutions)
    G
    )

-- subroutine that returns a HomotopyGraph
solveRandomSchubertProblemViaMonodromy = method(Options=>{Verbose=>false})
solveRandomSchubertProblemViaMonodromy (List, ZZ, ZZ) := o -> (conds, k, n) -> (
    (X,P,PS) := parametricSchubertProblem(conds,k,n);
    GS := gateSystem(P,X,PS);
    -*
    R := FFF[P/(p->p.Name)][X/(x->x.Name)];
    PR := P/(p->R_(p.Name));
    XR := X/(x->R_(x.Name));
    PSR := value(PS,valueHashTable(P|X,PR|XR));
    *-
    -- get seed solution
    (s0,XX,inverse'flags) := oneSolutionForOneInstance(conds,k,n);
    p0 := point{inverse'flags/entries//flatten//flatten};
    (V,npaths) := monodromySolve(
	--polySystem PSR, 
	GS,
	p0, {s0}, 
	NumberOfNodes=>4, NumberOfEdges=>1, 
	--"new tracking routine"=>false, 
	Verbose=>o.Verbose);
    (V, npaths, getTrackTime(V.Graph)) 
    )

parametricSchubertProblem = method()
p := symbol p;
x := symbol x; 
parametricSchubertProblem (List,ZZ,ZZ) := (conds,k,n) -> (
    twoconds := take(conds,2);
    c1 := first twoconds;
    c2 := last twoconds;
    all'but2 := drop(conds,2);
    P := {};
    remaining'conditions'flags := apply(#all'but2, nc->( 
	    c := all'but2#nc;
	    (c, matrix table(n,n,(i,j)->(
		    pInput := inputGate (symbol p)_("f"|toString nc,i,j);
		    P = P | {pInput};
		    pInput
		    )))
	));   
    (PX,X) := skewSchubertVariety((k,n),c1,c2,Inputs=>symbol x);
    (X,P,plueckerSystem(transpose PX,remaining'conditions'flags))
    )

oneSolutionForOneInstance = method()
oneSolutionForOneInstance (List,ZZ,ZZ) := (conds,k,n) -> (
    twoconds := take(conds,2);
    c1 := first twoconds;
    c2 := last twoconds;
    all'but2 := drop(conds,2);
    X := skewSchubertVariety((k,n),c1,c2);
    R := ring X; 
    C := coefficientRing R;
    p0 := point {apply(numgens R,i->exp(2*pi*ii*random RR))};
    X0 := transpose sub(X,matrix p0);
    remaining'inverse'flags := apply(all'but2, c->(
	    b := set partition2bracket (c,k,n); 
	    F := map(C^n,C^0,0);
	    for i from 1 to n do F = F | (
		if member(i,b) then X0 * random(C^k,C^1)  
		else random(C^n,C^1)
		);
	    solve(F,id_(C^n))
	    --F
	));
    (p0,X0,remaining'inverse'flags)
    )

randomSchubertProblemSolution = method()
randomSchubertProblemSolution (List,ZZ,ZZ) := (problem,k,n) -> (
    (p,X,remaining'inverse'flags) := oneSolutionForOneInstance(problem/first,k,n);
    ID := id_(FFF^n);
    flags1 := {ID,rsort ID} | remaining'inverse'flags/(F->solve(F,id_(FFF^n)));
    flags2 := problem/last;
    changeFlags({X},(problem/first,flags1,flags2))
    )
