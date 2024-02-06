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
specializeSystem (AbstractPoint, PolySystem) := (p, F) -> (
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
specializeSystem (AbstractPoint, Matrix) := (p, M) -> (
    nParameters := numgens coefficientRing ring M;
    assert(nParameters == #coordinates p);
    (PR,toPR) := flattenRing ring M;
    X := drop(gens PR, -nParameters); 
    R := (coefficientRing PR)[X];
    X = vars R;
    specializeSystemInternal(p,M,(R,PR,toPR,X))
    )
specializeSystem (AbstractPoint, GateSystem) := (p, GS) -> (
    pMat := matrix p;
    assert(numrows pMat == 1);
    pGateMatrix := sub(flatten GS, pMat | vars GS);
    gateSystem(vars GS, pGateMatrix)
    )

--  IN: System F(x; p) w/ variables x, parameters p
-- OUT: list of polynomials in the ring FF[p][x]
expand = method()
expand (Thing, System) := (FF, sys) -> (
    (n, m) := (numVariables sys, numParameters sys);
    flatGS := flatten sys;
    R := FF(monoid[vars(1..n+m)]);
    polys := flatten entries evaluate(flatGS, vars R);
    S := FF[take(gens R, {n, n+m-1})][take(gens R, {0, n-1})];
    polys/(p -> sub(p, S))
    )

-- needed to override "not implemented error" for (evaluate, PolySystem, AbstractPoint, AbstractPoint)
evaluate (PolySystem, AbstractPoint, AbstractPoint) := (PS, p0, x0) -> evaluate(polySystem specializeSystem(p0, PS), x0)

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
flatten PolySystem := PS -> sub(PS, first flattenRing ring PS)

-- syntactic sugar for creating instances of GateSystem
gateSystem (Thing, Thing, Gate) := (X, P, g) -> gateSystem(X, P, gateMatrix{{g}})
gateSystem (List, Thing) := (X, F) -> gateSystem({}, X, F)

-- routines for mixed volume computation (via gfan)
computeMixedVolume = method()
computeMixedVolume List := polys -> value gfanMixedVolume(
    polynomialsWithSameSupportsAndRationalCoefficients polys
    )
computeMixedVolume System := sys -> (
    m := numParameters sys;
    specGS := specializeSystem(
	point{for i from 1 to m list randomRationalNumber()},
    	expand(CC_53, sys) -- unclear what the ground field of a GateSystem should be
	);	    
    computeMixedVolume specGS
    )

newtonHomotopy = method(Options=>{Iterations=>10})
newtonHomotopy GateSystem := o -> G -> (
    NH := symbol NH;
    newtonParams := gateMatrix{toList(vars(NH_1..NH_(numFunctions G)))};
    Gnewton := gateSystem(
    	parameters G | newtonParams,
    	vars G,
    	gateMatrix G + transpose newtonParams
    	);
    done := false;
    numIters := 0;
    H := parametricSegmentHomotopy Gnewton;
    p0 := random(CC^1,CC^(numParameters G));
    targetParams := p0 | matrix{toList((numFunctions G):0)};
    while not done and numIters < o.Iterations  do (
    	-- track Newton homotopy from t=1 to t=0
    	x1 := random(CC^1, CC^(numVariables G));
    	eval1 := evaluate(G, p0, x1);
    	startParams := p0 | -eval1;
	-- next line can be time-consuming: better to do partial monodromy on Gnewton...
    	H10 := specialize(H, transpose(startParams|targetParams));
    	x0 := first trackHomotopy(H10, {x1});
    	rk := numericalRank evaluateJacobian(G, point p0, point x0);
    	if DBG > 0 then << "residual " << norm evaluate(G, point p0, point x0) << endl;
    	if DBG > 0 then << "Jacobian rank " << rk << endl;
    	if rk == numFunctions G then done = true;	
    	numIters = numIters + 1;
    	);
    if done then (point p0, x0) else "Newton homotopy seeding failed"
    )
