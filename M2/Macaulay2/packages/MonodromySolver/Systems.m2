export {"specializeSystem"}

-*
Doesn't belong anywhere else.
Note: implementation for PolySystem and GateSystem are quite different.
*-
specializeSystem = method()
specializeSystemInternal := (p, M, R'PR'toPR'X) -> (
    (R, PR, toPR, X) := R'PR'toPR'X; -- see below for ingredients
    flatten entries (map(R,PR,X|matrix p)) toPR transpose M
    )   
specializeSystem (Point, PolySystem) := (p, F) -> (
    if not F#?"specialization ingredients" then (
    	nParameters := numgens coefficientRing ring F;
    	assert(nParameters == #coordinates p);
    	(PR,toPR) := flattenRing ring F;
    	X := drop(gens PR, -nParameters);
	R := (coefficientRing PR)[X]; 
    	X = vars R;
    	F#"specialization ingredients" = (R,PR,toPR,X);
	);
    specializeSystemInternal(p, F#PolyMap, F#"specialization ingredients")
    )
specializeSystem (Point, Matrix) := (p, M) -> (
    nParameters := numgens coefficientRing ring M;
    assert(nParameters == #coordinates p);
    (PR,toPR) := flattenRing ring M;
    X := drop(gens PR, -nParameters); 
    R := (coefficientRing PR)[X];
    X = vars R;
    specializeSystemInternal(p,M,(R,PR,toPR,X))
    )
specializeSystem (Point, GateSystem) := (p, GS) -> (
    pMat := matrix p;
    assert(numrows pMat == 1);
    pGateMatrix := sub(flatten GS, vars GS | pMat);
    gateSystem(vars GS, pGateMatrix)
    )

-*
Overloads for creating and manipulating GateSystems
Some may find a better home in other packages (eg. (extra)NAGtypes, SLPexpressions, ...)
Try not to define any new methods here
*-

-- make all parameters new variables
flatten GateSystem := GS -> gateSystem(vars GS | parameters GS, gateMatrix GS)

-- syntactic sugar for creating instances of GateSystem
gateSystem (BasicList, BasicList, GateMatrix) := (P, X, F) -> (
    assert numcols F == 1;
    gateSystem(gateMatrix{toList P}, gateMatrix{toList X}, F)
    )
--gateSystem (BasicList, BasicList, BasicList) := (P, X, Fs) -> foldVert apply(toList Fs, f -> gateSystem(P, X, gateMatrix f))
gateSystem (Thing, Thing, Gate) := (X, P, g) -> gateSystem(X, P, gateMatrix{{g}})
gateSystem (List, Thing) := (X, F) -> gateSystem({}, X, F)

-- syntactic sugar for declareVariable \ {symbols}
vars IndexedVariable := x -> declareVariable x
vars Symbol := x -> declareVariable x
vars InputGate := x -> x
InputGate .. InputGate := (A, B) -> value \ (A.Name .. B.Name)
