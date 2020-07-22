export {
    "specializeSystem",
    "isAffineLinearFunction"
    }

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
    pGateMatrix := sub(flatten GS, pMat | vars GS);
    gateSystem(vars GS, pGateMatrix)
    )

-- needed to override "not implemented error" for (evaluate, PolySystem, Point, Point)
evaluate (PolySystem, Point, Point) := (PS, p0, x0) -> evaluate(polySystem specializeSystem(p0, PS), x0)

-- used for automated seeding
-- will also be useful for trace test
isAffineLinearFunction = method(Options=>{Tolerance=>1e-6})
isAffineLinearFunction GateSystem := o -> GS -> (
    if numParameters GS > 0 then error "expected parameter-less system";
    (x0, x1, x2) := toSequence apply(3, i -> random(CC^1, CC^(numVariables GS)));
    (y0, y1, y2, yAll) := (evaluate(GS, x0), evaluate(GS, x1), evaluate(GS, x2), evaluate(GS, x1 + x2 - x0));
    areEqual(y1 + y2, y0 + yAll, Tolerance => o.Tolerance)
    )

-*
Overloads for creating and manipulating GateSystems
Some may find a better home in other packages (eg. (extra)NAGtypes, SLPexpressions, ...)
*-

-- make all parameters new variables
flatten GateSystem := GS -> gateSystem(parameters GS | vars GS, gateMatrix GS)

-- syntactic sugar for creating instances of GateSystem
gateSystem (BasicList, BasicList, GateMatrix) := (P, X, F) -> (
    GM := if numcols F == 1 then F else transpose F;
    gateSystem(gateMatrix{toList P}, gateMatrix{toList X}, GM)
    )
gateSystem (Thing, Thing, Gate) := (X, P, g) -> gateSystem(X, P, gateMatrix{{g}})
gateSystem (List, Thing) := (X, F) -> gateSystem({}, X, F)

-- syntactic sugar for declareVariable \ {symbols}
vars IndexedVariable := x -> declareVariable x
vars Symbol := x -> declareVariable x
vars InputGate := x -> x
InputGate .. InputGate := (A, B) -> value \ (A.Name .. B.Name)
