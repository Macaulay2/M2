------------------------------------------------------
-- refining/sharpening routines 
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------
refine = method(TypicalValue => List, Options =>{
	  Software=>null, 
	  ErrorTolerance =>null,
	  ResidualTolerance =>null,
	  Iterations => null,
	  Bits => null,
	  SingularConditionNumber=>null
	  })
refine (List,List) := List => o -> (T,solsT) -> (
-- tracks solutions from start system to target system
-- IN:  T = list of polynomials in target system
--      solsT = list of solutions to T
-- OUT: solsR = list of refined solutions 
     o = fillInDefaultOptions o;
     n := #T; 
     if n > 0 then R := ring ideal T else error "expected nonempty target system";
     isProjective := false;
     if n != numgens R then (
	  if numgens R == n+1 and all(T, isHomogeneous) 
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
    if o.Software === PHCPACK then  return refinePHCpack(T,solsT,o)/point;
    if o.Software === BERTINI then (
	-- bits to decimals 
	decimals := ceiling(o.Bits * log 2 / log 10);
	return bertiniRefineSols(T,solsT,decimals)
	);

     -- Software=>M2 (and Software=>M2engine for now)
     if ref'sols === null then (
     	  n'iterations := o.Iterations; 
     	  T = matrix {T};
     	  J := transpose jacobian T; 
     	  evalT := x0 -> (
	       ret := lift(sub(transpose T, transpose x0), CC); 
	       if isProjective then ret || matrix{{0_CC}} else ret
	       );
	  evalJ := x0 -> (
	       ret := lift(sub(J, transpose x0), CC);
	       if isProjective then ret || matrix{ flatten entries x0 / conjugate} else ret
	       );
	  ref'sols = apply(solsT, s->(
	       if class s =!= Point then s = point {s} 
	       else if s.SolutionStatus === Infinity or s.SolutionStatus === Singular then return s;
	       x := sub(transpose matrix s, CC); -- convert to vector 
	       if isProjective then x = normalize x;
	       x1 := x; -- refined x
	       error'bound := infinity;
	       norm'dx := infinity; -- dx = + infinity
	       norm'residual := infinity;
	       newton'converges := true;
	       nCorrSteps := 0;
	       while (norm'residual > o.ResidualTolerance 
	       	    or norm'dx > o.ErrorTolerance * norm x1) 
	       and nCorrSteps < n'iterations 
	       and newton'converges
	       --and cond < o.SingularConditionNumber 
	       do ( 
		    residual := evalT(x1);
		    norm'residual = norm residual;
		    J := evalJ(x1);
		    --cond = conditionNumber J;
		    dx := solve(J, -residual);
		    norm'dx = norm dx;
		    if DBG > 3 then << "x=" << toString x1 << " res=" <<  residual << " dx=" << dx << endl;
		    if norm'dx < error'bound then (
		    	 x1 = x1 + dx;
		    	 if isProjective then x1 = normalize x1;
		    	 nCorrSteps = nCorrSteps + 1;
			 )
		    else (
			 error'bound = norm'dx;
			 if DBG>2 then print "warning: Newton's method correction exceeded the error bound obtained in the previous step"; 
			 newton'converges = false;
			 );
		    error'bound = norm'dx; 
		    );
	       if DBG>2 then (
		    if norm'residual > o.ResidualTolerance
		    then print "warning: Newton's method did not converge within given residual bound in the given number of steps";
		    if norm'dx > o.ErrorTolerance * norm x1 
		    then print "warning: Newton's method did not converge within given error bound in the given number of steps";
		    --if cond > o.SingularConditionNumber  
		    --then print "warning: condition number larger then SingularConditionNumber";
		    );
	       cond := conditionNumber evalJ(x1);
	       st := if cond > o.SingularConditionNumber then Singular else Regular;
     	       if s.?ErrorBoundEstimate and norm(x-x1) > s.ErrorBoundEstimate then (
		    if DBG>2 then print "warning: refinement failed";
		    s.ErrorBoundEstimate = infinity;
		    s
		    )
	       else (
		    point ({flatten entries x1, 
		    SolutionStatus=>st, 
		    ConditionNumber=>if cond===null then conditionNumber evalJ(x1) else cond, 
		    LastT=>1.} | (if norm'dx===infinity then {} else {ErrorBoundEstimate=>error'bound}))
	       )     
	       ));
     	   );
      	   ref'sols
      )     

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


