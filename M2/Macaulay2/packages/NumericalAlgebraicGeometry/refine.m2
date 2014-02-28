------------------------------------------------------
-- refining/sharpening routines, Newton step 
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------
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
     
     ref'sols := null;
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
	decimals := ceiling(o.Bits * log 2 / log 10);
	return bertiniRefineSols(equations F,solsT,decimals)
	);

     -- Software=>M2 (and Software=>M2engine for now)
     if ref'sols === null then (
     	  n'iterations := o.Iterations;
     	  J := jacobian F; 
     	  evalF := x0 -> (
	       ret := evaluate(F,x0); 
	       if isProjective then ret || matrix{{0_C}} else ret
	       );
	  evalJ := x0 -> (
	       ret := evaluate(J,x0);
	       if isProjective then ret || matrix{ flatten entries x0 / conjugate} else ret
	       );
	  ref'sols = apply(solsT, s->(
	       if class s =!= Point then s = point {s} 
	       else if s.SolutionStatus === Infinity 
	       -- or s.SolutionStatus === Singular 
	       then return s;
	       x := transpose matrix s; -- convert to vector 
	       if isProjective then x = normalize x;
	       x1 := x; -- refined x
	       error'bound := if not s.?ErrorBoundEstimate or s.SolutionSystem =!= F 
	                      then infinity
			      else s.ErrorBoundEstimate;
	       norm'dx := infinity; -- dx = + infinity
	       norm'Fx := infinity;
	       refinement'success := true;
	       nCorrSteps := 0;
	       while (norm'Fx > o.ResidualTolerance 
	       	    or norm'dx > o.ErrorTolerance * norm x1) 
	       and nCorrSteps < n'iterations 
	       and refinement'success
	       --and cond < o.SingularConditionNumber 
	       do ( 
		   Fx := evalF(x1);
		   norm'Fx = norm Fx;
		   J := evalJ(x1);
		   --cond = conditionNumber J;
		   dx := solve(J, -Fx);
		   norm'dx = norm dx;
		   if DBG > 3 then << "x=" << toString x1 << " res=" <<  Fx << " dx=" << dx << endl;
		   if norm'dx < error'bound then (
		       x1 = x1 + dx;
		       if isProjective then x1 = normalize x1;
		       nCorrSteps = nCorrSteps + 1;
		       )
		   else (
		       if DBG>2 then print "warning: Newton's method correction exceeded the error bound obtained in the previous step"; 
		       refinement'success = false;
		       );
		   error'bound = norm'dx; 
		   );
	       if norm'Fx > o.ResidualTolerance then (
		   if DBG>2 then print "warning: Newton's method did not converge within given residual bound in the given number of steps";
		   refinement'success = false;
		   );
	       if norm'dx > o.ErrorTolerance * norm x1 then (
		   if DBG>2 then print "warning: Newton's method did not converge within given error bound in the given number of steps";
		   refinement'success = false;
		   );  
	       s' := point { flatten entries x1, 
		   SolutionSystem => F, 
		   ConditionNumber => conditionNumber evalJ(x1)
		   };
    	       if error'bound =!= infinity then s'.ErrorBoundEstimate = error'bound;
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
	       ));
     	   );
      	   ref'sols
      )     

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
x0 := first solsT
endGame'Cauchy'polygon(H,1,t0,x0,3)
endGame'Cauchy'polygon(H,1,t0,x0,8)
endGame'Cauchy'polygon(H,1,t0,x0,16)
x32 = endGame'Cauchy'polygon(H,1,t0,x0,32)
-- x64 = endGame'Cauchy'polygon(H,1,t0,x0,64)
assert areEqual(first x32, {2,6}, Tolerance=>0.001)
///


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
