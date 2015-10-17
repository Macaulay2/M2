export {"randomSchubertProblemSolution"}
needsPackage "NumericalAlgebraicGeometry"
if version#"VERSION" == "1.8.2.1" then needsPackage "SLPexpressions"
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
    (X,P,makeSquareSystem(transpose PX,remaining'conditions'flags))
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
