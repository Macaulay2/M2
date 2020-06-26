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
    pGateMatrix := sub(flatten GS, vars GS | pMat);
    gateSystem(vars GS, pGateMatrix)
    )

-- needed to override "not implemented error" for (evaluate, PolySystem, Point, Point)
evaluate (PolySystem, Point, Point) := (PS, p0, x0) -> evaluate(polySystem specializeSystem(p0, PS), x0)

-- used for automated seeding
-- WILL be useful for trace test
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
Try not to define any new methods here
*-

-- make all parameters new variables
flatten GateSystem := GS -> gateSystem(vars GS | parameters GS, gateMatrix GS)

-- syntactic sugar for creating instances of GateSystem
gateSystem (BasicList, BasicList, GateMatrix) := (P, X, F) -> (
    GM := if numcols F == 1 then F else transpose F;
    gateSystem(gateMatrix{toList P}, gateMatrix{toList X}, GM)
    )
gateSystem (BasicList, BasicList, BasicList) := (P, X, Fs) -> matrix apply(toList Fs, f -> gateSystem(P, X, gateMatrix f))
gateSystem (Thing, Thing, Gate) := (X, P, g) -> gateSystem(X, P, gateMatrix{{g}})
gateSystem (List, Thing) := (X, F) -> gateSystem({}, X, F)

-- syntactic sugar for declareVariable \ {symbols}
vars IndexedVariable := x -> declareVariable x
vars Symbol := x -> declareVariable x
vars InputGate := x -> x
InputGate .. InputGate := (A, B) -> value \ (A.Name .. B.Name)


-- overrides PolySystem methods in NumericalAlgebraicGeometry/refine.m2
newton (System, Point) := (F,P) -> (
    X := transpose matrix P;
    X' := newton(F,X); 
    P' := point X';
    P'.ErrorBoundEstimate = norm(X'-X);
    P'
    )
newton (System, Matrix) := (F,X) -> (
    J := jacobian F;
    dX := if isSquare F then solve(evaluate(J,X), evaluate(F,X))
    else if F.NumberOfVariables < F.NumberOfPolys then solve(evaluate(J,X), evaluate(F,X), ClosestFit=>true)
    else ( -- underdetermined
	M := evaluate(J,X);
	(S,U,Vt) := SVD M;
	invJ := conjugate transpose Vt *
	        diagonalMatrix(numColumns M, numRows M, apply(S, s->if s==0 then 0 else 1/s)) * 
		conjugate transpose U; -- pseudo-inverse
	invJ*evaluate(F,X) 
	); 
    X-dX
    )

TEST ///
-- square 
CC[x,y]
F = polySystem {x^2+y^2-2,x+y-2}
P = point {{1.2,1.2+0.2*ii}} 
for i from 1 to 10 do (
    P = newton(F,P);
    print coordinates P;
    )
assert( norm evaluate(F,P) < 0.000001 ) 

-- overdetermined
CC[x,y]
F = polySystem {x^3+y^3-2,x^2+y^2-2,x+y-2}
P = point {{1.2,1.2+0.2*ii}} 
for i from 1 to 10 do (
    P = newton(F,P);
    print coordinates P;
    )
assert( norm evaluate(F,P) < 0.000001 ) 

-- underdetermined 
CC[x,y]
F = polySystem {x^2+y^2-2}
P = point {{1.2,1.2+0.2*ii}} 
for i from 1 to 10 do (
    P = newton(F,P);
    print coordinates P;
    )
assert( norm evaluate(F,P) < 0.000001 ) 
CC[x,y,z]
F = polySystem {x^2-y,x^3-z}
P = point {{1.2,1.2+0.2*ii,0.9+ii}} 
for i from 1 to 10 do (
    P = newton(F,P);
    print coordinates P;
    )
assert( norm evaluate(F,P) < 0.000001 ) 
///



-- this overrides the main function for M2 implemented in NumericalAlgebraicGeometry
refine (PolySystem,Point) := Point => o -> (F',s) -> 
if member(o.Software,{PHCPACK}) then first refine(F',{s},o) else 
if member(o.Software,{BERTINI}) then refineBertini(F',s,o) else 
(
    o = fillInDefaultOptions o;
    errorTolerance := o.ErrorTolerance;
    x := transpose matrix s; -- convert to vector 
    if o.Bits =!= infinity then errorTolerance = min(errorTolerance, 
	2.^(-o.Bits)*conditionNumber evaluate(jacobian F', x));
    bits := max(53,
	( if o.Bits =!= infinity then o.Bits
	    else -round(log_2 errorTolerance) )
	+ 2); -- two safety bits
    K := CC_bits; -- new field
    R := K(monoid[gens ring F']); -- new ring
    F := polySystem (map(R, ring F', gens R)) F'.PolyMap;
    n'iterations := o.Iterations;
    -- if isProjective then x = normalize x;
    x1 := promote(x,K); -- refined x
    error'bound := infinity;
    norm'dx := infinity;
    norm'Fx := infinity;
    refinement'success := true;
    nCorrSteps := 0;
    while (norm'Fx > o.ResidualTolerance 
	or norm'dx > errorTolerance * norm x1) 
    and nCorrSteps < n'iterations 
    and refinement'success
    --and cond < o.SingularConditionNumber 
    do ( 
	Fx := evaluate(F,x1);
	norm'Fx = norm Fx;
	J := evaluate(jacobian F, x1);
	--cond = conditionNumber J;
	try (
	    dx := solve(J, -Fx);
	    norm'dx = norm dx;
	    if DBG > 3 then << "x=" << toString x1 << " res=" <<  Fx << " dx=" << dx << endl;
	    if norm'dx < error'bound then (
	    	x1 = x1 + dx;
	    	--if isProjective then x1 = normalize x1;
	    	nCorrSteps = nCorrSteps + 1;
	    	)
	    else (
	    	if DBG>2 then print "warning: Newton's method correction exceeded the error bound obtained in the previous step"; 
	    	refinement'success = false;
	    	);
	    error'bound = norm'dx; 
	    ) else (
	    refinement'success = false;
	    )
	);
    if refinement'success then (
    	if norm'Fx > o.ResidualTolerance then (
	    if DBG>2 then print "warning: Newton's method did not converge within given residual bound in the given number of steps";
	    refinement'success = false;
	    );
    	if norm'dx > errorTolerance * norm x1 then (
	    if DBG>2 then print "warning: Newton's method did not converge within given error bound in the given number of steps";
	    refinement'success = false;
	    );
	);  
    s' := point { flatten entries x1, 
	SolutionSystem => F, 
	ConditionNumber => conditionNumber evaluate(jacobian F, x1)
	};
    s'.ErrorBoundEstimate = error'bound;
    s'.SolutionStatus = if refinement'success then (
	if s'.ConditionNumber > o.SingularConditionNumber 
	then Singular 
	else Regular
	)
    else (
	if DBG>2 then print "warning: refinement failed";
	RefinementFailure
	);
    s'
    )
