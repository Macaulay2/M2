------------------------------------------------------
-- refining/sharpening routines, Newton's method
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------
export {"refine", "newton", "endGameCauchy"}
    
newton = method()
-- assumes F has coefficients in field RR_prec or CC_prec 
--         P has coordinates in that can be promoted to the above field 
newton (PolySystem, Point) := (F,P) -> (
    X := transpose matrix P;
    X' := newton(F,X); 
    P' := point X';
    P'.ErrorBoundEstimate = norm(X'-X);
    P'
    )
newton (PolySystem, Matrix) := (F,X) -> (
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

refine = method(TypicalValue => List, Options =>{
	  Software=>null, 
	  ErrorTolerance =>null,
	  ResidualTolerance =>null,
	  Iterations => null,
	  Bits => null,
	  SingularConditionNumber=>null
	  })
refine (List,List) := List => o -> (T,solsT) -> refine(polySystem T, solsT, o)

refine Point := o -> P -> if P.?SolutionSystem then (
    ret := new Point from P;
    if P.?LiftedSystem then (
	P' := refine(P.LiftedSystem,P.LiftedPoint,o);
    	ret.Coordinates = take(P'.Coordinates, P.SolutionSystem.NumberOfVariables);
    	ret.SolutionStatus = P'.SolutionStatus;
	if ret.SolutionStatus === Regular then ret.SolutionStatus = Singular
	) 
    else (
	P' = refine(P.SolutionSystem,P,o);
    	ret.Coordinates = P'.Coordinates;
    	ret.SolutionStatus = P'.SolutionStatus;
	);
    ret.ErrorBoundEstimate = P'.ErrorBoundEstimate;
    ret.ConditionNumber = P'.ConditionNumber;
    ret
    ) else error "there is no polynomial system associated with the point"

-- this is the main function for M2
refine (PolySystem,Point) := Point => o -> (F',s) -> 
if member(o.Software,{BERTINI,PHCPACK}) then first refine(F',{s},o) else (
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

-- this is the main function for Bertini and PHCpack
refine (PolySystem,List) := List => o -> (F,solsT) -> (
-- refines solutions to solsT of the system F
     o = fillInDefaultOptions o;
     n := F.NumberOfVariables; 
     if n > 0 then (
	 R := ring F;
	 C := coefficientRing R; 
	 )
     else error "expected nonempty target system";
     isProjective := false;
     if n != numgens R then (
	  if numgens R == n+1 and isHomogeneous F 
	  then ( 
	       isProjective = true; 
	       n = n+1;
	       )  
	  else error "expected a square system";
     	  );
     if #solsT == 0 then return solsT;
     
     if isProjective then (
     	  if o.Software === M2engine then ( -- engine refiner is primitive
--      	       PT := if class first solsT === Point and (first solsT).?Tracker then (first solsT).Tracker else null;
--                if PT=!=null then (
--  		    ref'sols = apply(entries map(CC_53, 
--  		    	      rawRefinePT(PT, raw matrix solsT, o.ErrorTolerance, o.Iterations)
--  		    	      ), s->{s}); -- old format
-- 		    );
	       error "refine is not implemented in the engine yet";
	       ) 
	   else error "refining projective solutions is not implemented yet";
    	  );  
    if o.Software === PHCPACK then  return refinePHCpack(equations F,solsT,o)/point;
    if o.Software === BERTINI then (
	-- bits to decimals 
	decimals := if o.Bits =!= infinity then ceiling(o.Bits * log 2 / log 10) else log_10 o.ErrorTolerance;
	return bertiniRefineSols(decimals,equations F,solsT)
	);

     -- Software=>M2 (and Software=>M2engine for now)
     apply(solsT, s->refine(F,    		  
	     if class s === Point then s else point {s/toCC},
	     o 
	     ))
     )         
TEST /// -- refine 
sqrt2 = point {{sqrt(2p1000)}}
R = CC[x]
-- sqrt2' = refine(polySystem{x^2-2}, point {{1.5}}, Bits=>500) -- fails with SIGSEGV
sqrt2' = refine(polySystem{x^2-2}, point {{1.5_CC}}, Bits=>500)
areEqual(sqrt2',sqrt2, Tolerance=>2^-498)

R = CC[x,y];
T = {x^2+y^2-1, x*y};
sols = { {1.1_CC,0.1}, { -0.1,1.2} };
rsols = refine(T, sols, Software=>M2, ErrorTolerance=>.001, Iterations=>10)
assert areEqual(rsols, {{1,0},{0,1}})
r1000sols = refine(T, rsols, Software=>M2, Bits=>1000)
assert areEqual(r1000sols, {{1,0},{0,1}}, Tolerance=>2^-997)


T = polySystem {x^2+y^2-1, (x-y)^2};
e = 1e-9; 
P = point {{sqrt 2/2 + e*ii,sqrt 2/2 - e*ii}};
P' = refine(T,P)
assert(P'.ErrorBoundEstimate < 1e-6 and P'.ConditionNumber > 1e6 and status P' === Singular)
deflateInPlace(P,T)
P'' = refine P
assert(P''.ErrorBoundEstimate < 1e-6 and P''.ConditionNumber < 100 and status P'' === Singular)
///

------------------------------- ENGAMES -------------------------------------------------------------------
-- H: a homotopy
-- t'end: the end value of the continuation parameter t
-- p0: a Point = a solution to H_t(x)=0, with t0=p0.LastT close to t'end
-- "number of vertices" (optional): ... of the regular polygon approximating the circle |t-t'end|=|t0-t'end|
-- OUTPUT: a Point
endGameCauchy = method(Options=>{"number of vertices"=>16,"backtrack factor"=>1.,
	tStep => null, -- initial
	tStepMin => null,
	stepIncreaseFactor => null,
	numberSuccessesBeforeIncrease => null,
	maxCorrSteps => null,
	CorrectorTolerance => null, -- tracking tolerance
	-- end of path
	EndZoneFactor => null, --!!! EndZoneCorrectorTolerance = CorrectorTolerance*EndZoneFactor when 1-t<EndZoneFactor 
	InfinityThreshold => null -- used to tell if the path is diverging
	})

endGameCauchy (GateHomotopy, Number, Point):= o -> (H, t'end, p0) -> (
    x0 := mutableMatrix transpose {coordinates p0 | {p0.LastT}} ; 
    w := endGameCauchy(H,t'end,x0,o);
    -- if w == 0 then error "endGameCauchy: something went wrong";
    p := point {drop(first entries transpose x0,-1)};
    if w>0 then ( 
	p.Multiplicity = w; 
	p.SolutionStatus = (if w == 1 then Regular else Singular);
	p.LastT = t'end;
	) 
    else (
	p.SolutionStatus = RefinementFailure; 
	);
    p
    )

-- x0: a column vector with n+1 coordinates, the last one is t0
-- OUTPUT: changes x0 in place, returns the winding number or 0 if failed 
endGameCauchy (GateHomotopy, Number, MutableMatrix):= o -> (H, t'end, x0in) -> (
    assert(numcols x0in === 1);
    checkPrecision := true;  -- !!! should be adaptive eventually
    if not canHaveRawHomotopy H then error "expected a Homotopy with RawHomotopy";  
    o = fillInDefaultOptions o;
    m := o#"number of vertices";
    if DBG>0 then << "(C"<<m<<")";
    n := numrows x0in - 1;
    statusOut := mutableMatrix(ZZ,2,1); -- 2 rows (status, number of steps), #solutions 
    nPlus1rows := toList(0..n);
    nPlus1by1submatrix := M -> submatrix(M,nPlus1rows,{0});
    
    -- bactrack from x0in to x0
    x0out := mutableMatrix(ring x0in,n+2,1);     
    dt0 := x0in_(n,0)-t'end;
    x0out_(n,0) = t'end + o#"backtrack factor"*dt0;  
    trackHomotopyM2engine(H, x0in, 
	x0out, statusOut, -- output goes here
	o.tStep, o.tStepMin, 
	o.CorrectorTolerance, o.maxCorrSteps, 
	toRR o.InfinityThreshold,
	checkPrecision
	);
    s'status := solutionStatusLIST#(statusOut_(0,0));
    if s'status =!= Regular then return 0; -- error
    
    x0 := nPlus1by1submatrix(x0out);
    inp := mutableMatrix x0;
    out := mutableMatrix(ring x0,n+2,1);
    
    x0' := mutableMatrix x0; -- x0' stores the sum approximating the integral
    dt0 = x0_(n,0)-t'end;
    out_(n,0) = t'end + dt0*exp(2*pi*ii/m);
    loop'incomplete := true;
    s'status = Regular; 
    w := 0;
    while loop'incomplete and w<=10 --!!!
    do (
    	for i to m-1 do (
	    ti'out := timing trackHomotopyM2engine(H, inp, 
		out, statusOut, -- output goes here
		o.tStep, o.tStepMin, 
		o.CorrectorTolerance, o.maxCorrSteps, 
		toRR o.InfinityThreshold,
		checkPrecision);
    	    if DBG>2 then << "-- endGameCauchy: trackHomotopyM2engine time = " << first ti'out << " sec." << endl;
    	    s'status = solutionStatusLIST#(statusOut_(0,0));
	    if s'status =!= Regular then return 0; -- error
    	    if DBG>2 then << "-- endGameCauchy: number of steps = " << statusOut_(1,0) << endl;
	    x0'' := nPlus1by1submatrix(out);
    	    x0' = x0'+x0'';
	    inp = x0''; -- reuse matrix for the next step;
    	    out_(n,0) = t'end + dt0*exp(2*pi*(i+2)*ii/m); 
	    );
	w = w + 1;
	loop'incomplete = not areEqual(inp,x0);
	-- print (inp-x0);
	if loop'incomplete 
	then out_(n,0) = t'end + dt0*exp(2*pi*ii/m) -- reset (roundoff error may accumulate)
	else x0' = x0' - inp; -- x0 is "double counted"
	);
    for i to n do x0in_(i,0) = x0'_(i,0)/(w*m);    
    x0in_(n,0) = t'end;
    w
    )

TEST ///
restart
debug needsPackage "NumericalAlgebraicGeometry"
CC[x,y]
d = 4;
-- d = 5; -- fails 
T = {(x-2)^d,y-x+x^2-x^3}
sols = solveSystem(T,PostProcess=>false)
p0 = first sols
peek p0
t'end = 1
NAGtrace 1
p = endGameCauchy(p0#"H",t'end,p0)
p = endGameCauchy(p0#"H",t'end,p0,"backtrack factor"=>0.5)
assert (d == p.Multiplicity)
p = endGameCauchy(p0#"H",t'end,p0,"number of vertices"=>20)
assert (d == p.Multiplicity)

-- 
restart
needsPackage "NumericalAlgebraicGeometry"
n = 3;
R = CC[x_1..x_n]
d = 3;
rMap = map(R,R,apply(n,i->random(1,R)+random(2,R)))
T = polySystem(gens R / (f->rMap (f-1)^d))
sols = solveSystem(T,PostProcess=>false)
p0 = first select(sols, s->status s =!= Regular)
peek p0
t'end = 1
p = endGameCauchy(p0#"H",t'end,p0)
norm evaluate(T,p)
assert (d == p.Multiplicity)
p = endGameCauchy(p0#"H",t'end,p0,"backtrack factor"=>2)
norm evaluate(T,p)
assert (d == p.Multiplicity)
p = endGameCauchy(p0#"H",t'end,p0,"number of vertices"=>16)
norm evaluate(T,p)
assert (d == p.Multiplicity)
p = endGameCauchy(p0#"H",t'end,p0,"backtrack factor"=>100,"number of vertices"=>100)
norm evaluate(T,p)
assert (d == p.Multiplicity)
--
restart
R=QQ[x,y]
f = x^2 + y^2 - 1 
g = x^3 + y^3 - 1 
needsPackage "NumericalAlgebraicGeometry"
solveSystem  ({f,g},PostProcess=>false)
solveSystem  {f,g}

///

{* -- (Deprecated) 
endGame'Cauchy'polygon = method(Dispatch=>Thing)
endGame'Cauchy'polygon Sequence := parameters -> (
    -- H: PolySystem, a homotopy
    -- t'end: the end value of the continuation parameter t
    -- t0: a value of t close to t'end
    -- x0: Point, a solution to H_t(x)=0 
    -- m: the number of vertices of the regular polygon approximating the circle |t-t'end|=|t0-t'end|
    (H,t'end,t0,x0,m) := parameters;
    -- output: (x'end, w) = (a solution at t=t'end, the winding number)  
    H' := substituteContinuationParameter(H,
	(t0-t'end)*H.ContinuationParameter + t'end);  -- t'end = 0 after this and t0 = 1
    loop'incomplete := true;
    w := 0;
    x := x0;
    x'values := {}; -- the list of values of x along the loop
    while loop'incomplete do (
    	for i to m-1 do (
	    x = first trackSegment(H', exp(2*pi*i*ii/m), exp(2*pi*(i+1)*ii/m), {x}); 
	    x'values = append(x'values,x);
	);
    	w = w + 1;
	loop'incomplete = not areEqual(x,x0);
	);
    print VerticalList x'values;
    (sum (coordinates\x'values) /(w*m), w)
    )  

TEST ///
CC[x,y]
d = 5;
T = {(x-2)^d,y-x+x^2-x^3}
(S,solsS) = totalDegreeStartSystem T
H = segmentHomotopy(polySystem S, polySystem ((1+ii)*T))
t0 = 0.999
solsT = trackSegment(H,0,t0,solsS)
debug NumericalAlgebraicGeometry
x0 := first select(solsT, p->norm matrix p < 15)
endGame'Cauchy'polygon(H,1,t0,x0,3)
endGame'Cauchy'polygon(H,1,t0,x0,8)
endGame'Cauchy'polygon(H,1,t0,x0,16)
x32 = endGame'Cauchy'polygon(H,1,t0,x0,32)
-- x64 = endGame'Cauchy'polygon(H,1,t0,x0,64)
assert areEqual(first x32, {2,6}, Tolerance=>0.001)
///
*}

/// -- OLD CODE
refineViaDeflation = method(Options=>{Order=>1, Tolerance=>0.0001})
refineViaDeflation(List, List) := o->(T,solsT) -> refineViaDeflation(transpose matrix{T},solsT,o)
refineViaDeflation(Matrix, List) := o->(sysT,solsT) -> (
     n := numgens ring sysT;
     ref'solsT := refine(flatten entries sysT,solsT);     
     for s in ref'solsT do (
	  if s.SolutionStatus =!= Regular then (
	       T := sysT;
	       s.DeflationRandomMatrix = {};
	       s.DeflationSequence = {};
	       local dT;
	       local SM; 
	       ls := coordinates s; -- current lifted solution
	       dOrder := o.Order;
	       minRank := infinity;
	       done := false;
	       while not done do (
		    if DBG > 1 then << "-- performing deflation of order " << dOrder << " at the point " << ls << endl;  
		    M := dMatrix(ideal T, dOrder);
		    r := numericalRank sub(M, matrix{ls});
 		    attempt := 0;
		    lucky := false;
		    local new'ls;
		    while not lucky and attempt < DEFAULT.Attempts do (
			 (dT,SM) = deflatedSystem(ideal T,M,r,attempt);
			 lucky = ((new'ls = liftSolution(ls,dT))=!=null);
			 attempt = attempt + 1;
			 );
		    ls = new'ls;
		    s.DeflationSequence = s.DeflationSequence|{dOrder}; 
		    s.DeflationRandomMatrix = s.DeflationRandomMatrix | {SM};
		    if not lucky
		    then (
			 s.SolutionStatus = NumericalRankFailure;
			 done = true;
			 )
		    else (
			 rs := first refine(dT,{ls});
			 --print(rs,coordinates s,s.ErrorBoundEstimate,norm matrix s);
			 solsAreClose := (
			      norm matrix{take(coordinates rs, n) - coordinates s} 
			      < o.Tolerance * norm matrix s); -- new approximation is not too far
			 if solsAreClose then (
			      s.Coordinates = take(coordinates rs, n); -- refine the approximation
			      );
			 if rs.SolutionStatus===Regular
			 then done = true
			 else if rs.SolutionStatus===Singular then (
			      done = numgens ring first dT > DEFAULT.maxNumberOfVariables;
			      if done then s.DeflationSequence = s.DeflationSequence|{infinity}; -- indicates that deflation failed  
			      T = dT;
			      )
			 else (
			      s.SolutionStatus = NumericalRankFailure;
			      )
			 ))
	       )
	  ); 
     solutionsWithMultiplicity ref'solsT
     )
///
