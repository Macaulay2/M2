------------------------------------------------------
-- core tracking routines 
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------
export { "track", 
    -- "trackSegment", 
    "trackHomotopy" }

track'option'list = {
	  Software=>null, NoOutput=>null, 
	  NumericalAlgebraicGeometry$gamma=>null, 
	  NumericalAlgebraicGeometry$tDegree=>null,
     	  -- step control
	  tStep => null, -- initial
          tStepMin => null,
	  stepIncreaseFactor => null,
	  numberSuccessesBeforeIncrease => null,
	  -- predictor 
	  Predictor=>null, 
	  SLPpredictor=>null, --temp!!!
	  MultistepDegree => null, -- used only for Predictor=>Multistep
	  -- corrector 
	  SLPcorrector=>null, --temp!!!
	  maxCorrSteps => null,
     	  CorrectorTolerance => null, -- tracking tolerance
	  -- end of path
	  EndZoneFactor => null, -- EndZoneCorrectorTolerance = CorrectorTolerance*EndZoneFactor when 1-t<EndZoneFactor 
	  InfinityThreshold => null, -- used to tell if the path is diverging
	  SingularConditionNumber=>null, -- threshold for the condition number of the jacobian
	  -- projectivize and normalize
	  Normalize => null, -- normalize in the Bombieri-Weyl norm
	  Projectivize => null, 
	  AffinePatches => null,
	  -- slp's 
	  SLP => null -- possible values: false, HornerForm, CompiledHornerForm 	  
	  }
track = method(TypicalValue => List, Options => track'option'list)
-- tracks solutions from start system to target system
-- IN:  S = list of polynomials in start system
--      T = list of polynomials in target system
--      solsS = list of solutions to S
-- OUT: solsT = list of target solutions corresponding to solsS
track (List,List,List) := List => o -> (S,T,solsS) -> (
    checkCCpolynomials(S,T);
    track(polySystem S, polySystem T,solsS,o)    
    )
-- tracks solutions from start system to target system
-- IN:  S = start system
--      T = target system
--      solsS = list of solutions to S
-- OUT: solsT = list of target solutions corresponding to solsS
track (PolySystem,PolySystem,List) := List => o -> (S,T,solsS) -> (
     if #solsS == 0 then return solsS;    
     o = fillInDefaultOptions o;
     HISTORY := DBG>1 or member(o.Predictor, {Multistep,Secant});
     n := T.NumberOfPolys; 
     K := CC_53; -- THE coefficient ring
     
     R := ring S; 
     
     if o.tStep <= 0 then error "expected positive tStep";  
     if (o.Projectivize or o.SLP===false) and (o.SLPpredictor or o.SLPcorrector) 
     then error "SLPpredictor amd SLPcorrector can be used only with Projectivize=false and SLP=!=false"; 
     if o.Software===M2enginePrecookedSLPs and (o.Projectivize or o.SLP===null) 
     then error "M2enginePrecookedSLPs is implemented for Projectivize=>false and SLP != false";
--     if o.Predictor===Certified and (o.Software=!=M2 or o.SLP=!=false)
--     then error "Certified (experimental) requires Software=>M2 and o.SLP=>false";
     
     -- PHCpack -------------------------------------------------------
     if o.Software === PHCPACK then return trackPHCpack(S,T,solsS,o)
     else if o.Software === BERTINI then return trackBertini(S,T,solsS,o)
     else if not member(o.Software,{M2,M2engine,M2enginePrecookedSLPs}) 
     then error "wrong Software option or implementation is not available";
     
     -- M2 (main code)  --------------------------------------------------------     
     setupStartTime := currentTime();
     -- thresholds and other tuning parameters (should include most of them as options)
     stepDecreaseFactor := 1/o.stepIncreaseFactor;
     theSmallestNumber := 1e-12;
     
     -- determine whether the problem is projective
     isProjective := false;
     if n != numgens R then (
	  if numgens R == n+1 and isHomogeneous S and isHomogeneous T 
	  then ( 
	       isProjective = true; 
	       n = n+1;
	       )  
	  else error "expected a square system";
     	  );
    
     solsS = solsS / (s->sub(transpose matrix (
		 if class s === Point then s
	     	 else {toList s}
	     	 ), CC)); -- convert to vectors
     if o.Projectivize then (
	  if isProjective then error "the problem is already projective";
	  h := symbol h;
	  R = K(monoid[gens R | {h}]); 
	  h = last gens R;
	  n = numgens R;
	  T = homogenize(T,R,h); 
	  S = homogenize(S,R,h);
     	  solsS = solsS / (s->s||matrix{{1_K}});
	  isProjective = true;
	  );
     
     if o.Predictor===Certified and not isProjective 
     then "projective expected: either homogeneous system or Projectivize=>true";
     
     if isProjective then (
     	  if o.Software === M2enginePrecookedSLPs 
	  then error "Software=>M2enginePrecookedSLPs not implemented for projective case";	       
     	  -- affine patch functions 
     	  pointToPatch := (x0,p)-> (1/(p*x0)_(0,0))*x0; -- representative for point x0 in patch p
	  patchEquation := p -> p * transpose vars R - 1;

 	  if o.Predictor === Certified or o.AffinePatches === DynamicPatch then (
	       solsS = solsS/normalize;
	       dPatch := true; -- not null        
	       )
	  else (   
	       dPatch = null;
	       patches := { 	    
	       	    promote(matrix{{append((n-1):0, 1)}},K), -- dehomogenizing patch
	       	    if #o.AffinePatches > 0 then first o.AffinePatches -- either provided patch...
	       	    else ( 
		    	 matrix{apply(n, i->exp(random(0.,2*pi)*ii))} ) -- ... or random patch
	       	    };
	       patches = patches | { o#(NumericalAlgebraicGeometry$gamma)*patches#1 };
     	       if DBG>1 then << "affine patch: " << toString patches#1 <<endl;
	       T = polySystem(XXXtoList T | {patchEquation patches#1});
	       S = polySystem(S | {patchEquation patches#2});
	       solsS = solsS / (s->pointToPatch(s, patches#2));
	       );
	  ); 
     
     --!!! currently all projective trackers use the unit sphere !!!
     shouldNormalize := isProjective or o.Normalize;
    
     -- create homotopy
     t := symbol t;
     Rt := K(monoid[gens R, t]); 
     t = last gens Rt; 
     (nS,nT) := if shouldNormalize -- make Bomboeri-Weyl norm of the systems equal 1
     then (XXXapply(S, f->f/sqrt(S.NumberOfPolys * BombieriWeylNormSquared f)), 
	 XXXapply(T, f->f/sqrt(T.NumberOfPolys * BombieriWeylNormSquared f)))
     else (S,T);
     
     if o.Predictor===Certified or (isProjective and o.Software===M2engine)
     -- in both cases a linear homotopy on the unit sphere is performed
     then (
     	  (nS,nT) = (XXXtoList nS, XXXtoList nT); -- rolling back to the old (List) representation of PolySystem
	  nT = (o#(NumericalAlgebraicGeometry$gamma)/abs(o#(NumericalAlgebraicGeometry$gamma)))*nT;
	  H := {matrix{nS}, matrix{nT}}; -- a "linear" homotopy is cooked up at evaluation using nS and nT
	  DMforPN := diagonalMatrix append(nT/(f->1/sqrt sum degree f),1);
	  maxDegreeTo3halves := power(max(nT/first@@degree),3/2);
	  reBW'ST := realPart sum(S.NumberOfPolys, i->BombieriWeylScalarProduct(nS#i,nT#i));-- real Bombieri-Weyl scalar product
	  sqrt'one'minus'reBW'ST'2 :=  sqrt(1-reBW'ST^2);
	  bigT := asin sqrt'one'minus'reBW'ST'2; -- the linear homotopy interval is [0,bigT]
	  if reBW'ST < 0 then bigT = pi-bigT; -- want: sgn(cos)=sgn(reBW'ST) 
	  Hx := H/transpose@@jacobian; -- store jacobians (for evalHx)
	  if DBG>4 then << "Re<S,T> = " << reBW'ST << ", bigT = " << bigT << endl; 
     	  )	  
     else (
     	  H = transpose (
	      o#(NumericalAlgebraicGeometry$gamma)*(1-t)^(o#(NumericalAlgebraicGeometry$tDegree))*sub(nS.PolyMap,Rt)
	      + t^(o#(NumericalAlgebraicGeometry$tDegree))*sub(nT.PolyMap,Rt)
	      ); -- row matrix
     	  JH := transpose jacobian H; 
     	  Hx = JH_(toList(0..n-1));
     	  Ht := JH_{n};
     	  (nS,nT) = (XXXtoList nS, XXXtoList nT); -- rolling back to the old (List) representation of PolySystem
	  );

     -- evaluation times
     etH := 0;
     etHx := 0; 
     etHt := 0;
     -- evaluation functions using SLP
     if o.SLP =!= false and o.Software =!= M2engine then (
	  toSLP := pre -> (
	       (constMAT, prog) := (if o.SLP === HornerForm 
		    then preSLPinterpretedSLP 
		    else preSLPcompiledSLP) (n+1, pre);
	       rawSLP(raw constMAT, prog)
	       );  
	  preH := prunePreSLP stackPreSLPs apply(entries H, row -> concatPreSLPs apply(row, poly2preSLP));
	  if o.Software==M2 then (
	       slpHno := SLPcounter; slpH := toSLP preH; 
	       slpHxno := SLPcounter; slpHx := toSLP prunePreSLP transposePreSLP jacobianPreSLP(preH,toList (0..n-1));
	       slpHtno := SLPcounter; slpHt := toSLP prunePreSLP transposePreSLP jacobianPreSLP(preH,{n}); 
	       ) 
	  else if o.Software==M2enginePrecookedSLPs then (
	       -- take jacobian dH/(dx,dt) and DO NOT transpose
	       preHxt := jacobianPreSLP(preH,toList (0..n));
	       slpHxtno := SLPcounter;  
	       if o.Predictor =!= Euler 
	       then slpHxt := toSLP prunePreSLP preHxt
	       else slpHxt = toSLP prunePreSLP (preHxt#0,preHxt#1,preHxt#2||preH#2); -- positions of the entries of H in the jacobian preslp do not change 
	       -- take jacobian dH/(dx,dt), DO NOT transpose, augment with H
	       preHx := jacobianPreSLP(preH,toList (0..n-1));
	       slpHxHno := SLPcounter; slpHxH := toSLP prunePreSLP (preHx#0,preHx#1,
		    preHx#2||preH#2); -- positions of the entries of H in the jacobian preslp do not change  
	       );
	  fromSlpMatrix := (S,params) -> (
	       result := rawEvaluateSLP(S, raw params);
	       lift(map(K, result),K)
	       );
	  );
     if o.Software==M2 then ( --------------- M2 section -------------------------------------------------------

     -- evaluation functions	
     evalH := (x0,t0)-> (
	  tr := timing (
	       r := if o.Predictor === Certified 
	       then (
		    sine := sin(t0*bigT); cosine := cos(t0*bigT);
		    transpose( lift(sub(H#0,transpose x0),K)*(cosine-(reBW'ST/sqrt'one'minus'reBW'ST'2)*sine) 
	       	    	 + lift(sub(H#1,transpose x0),K)*(sine/sqrt'one'minus'reBW'ST'2) )
		    )
	       else if o.SLP =!= false then transpose fromSlpMatrix(slpH, transpose x0 | matrix {{t0}})
     	       else lift(sub(transpose H, transpose x0 | matrix {{t0}}), K);
	       --(old) if o.Predictor === Certified then ((normalizer t0)*r) || matrix{{0_K}} else 
	       if dPatch === null then r
	       else r || matrix{{(dPatch*x0)_(0,0)-1}} -- patch equation evaluated  
	       );
	  etH = etH + tr#0;
	  tr#1
	  );
     evalHNoPatchNoNormalizer := (x0,t0)-> (
	  tr := timing (
	       if o.SLP =!= false then r := transpose fromSlpMatrix(slpH, transpose x0 | matrix {{t0}})
     	       else r = lift(sub(transpose H, transpose x0 | matrix {{t0}}), K);
	       r
	       );
	  etH = etH + tr#0;
	  tr#1
	  );
     evalHxNoPatch := (x0,t0)-> (
	  r := if o.Predictor === Certified then (
	       sine := sin(t0*bigT); cosine := cos(t0*bigT);
	       lift(sub(Hx#0,transpose x0),K)*(cosine-(reBW'ST/sqrt'one'minus'reBW'ST'2)*sine) 
	       + lift(sub(Hx#1,transpose x0),K)*(sine/sqrt'one'minus'reBW'ST'2)   
	       )
	  else if o.SLP =!= false then fromSlpMatrix(slpHx, transpose x0 | matrix {{t0}})
     	  else lift(sub(Hx, transpose x0 | matrix {{t0}}), K);
	  --(old) if o.Predictor === Certified then r = r * normalizer t0;
	  r
	  );  
     evalHx := (x0,t0)->( 
	  tr := timing (
     	       r := evalHxNoPatch(x0,t0);
	       if dPatch === null then r
	       else r || matrix { flatten entries dPatch }
	       );
	  etHx = etHx + tr#0;
	  tr#1
	  );  
     evalHt := (x0,t0)->(
	  tr := timing (
	       r := if o.Predictor === Certified then (
		    sine := sin(t0*bigT); cosine := cos(t0*bigT);
		    transpose( lift(sub(H#0,transpose x0),K)*(-sine-(reBW'ST/sqrt'one'minus'reBW'ST'2)*cosine)
		    + lift(sub(H#1,transpose x0),K)*(cosine/sqrt'one'minus'reBW'ST'2) )
		    )
	       else if o.SLP =!= false then fromSlpMatrix(slpHt, transpose x0 | matrix {{t0}})
     	       else lift(sub(Ht, transpose x0 | matrix {{t0}}), K);
	       --(old) if o.Predictor === Certified then r = r * (normalizer t0) + evalHNoPatchNoNormalizer(x0,t0) * (normalizer' t0);
	       if dPatch === null then r
	       else r || matrix {{0_K}}
	       );
	  etHt = etHt + tr#0;
	  tr#1
	  );
     evalMinusInverseHxHt := (x0,t0)-> -(inverseMatrix evalHx(x0,t0))*evalHt(x0,t0);
     solveHxTimesDXequalsMinusHt := (x0,t0) -> solve(evalHx(x0,t0),-evalHt(x0,t0)); 
     
     if o.SLPpredictor then (
	  slpPred := rawSLP(raw matrix(K,{{}}), {
		    0, --#constants 
		    n+2, --#inputs: x,t,dt  
		    n, --#rows in output: dx
		    1, --#cols in output
		    slpPREDICTOR,
		    ( if o.Predictor === Tangent then predTANGENT
		    	 else if o.Predictor === RungeKutta4 then predRUNGEKUTTA
		    	 else error "no slp for the predictor"),
		    slpHxno,
		    slpHtno,
		    slpHno 
		    });
	  SLPcounter = SLPcounter + 1;
	  );
     if o.SLPcorrector then (
	  slpCorr := rawSLP(raw matrix(K,{{}}), {
		    0, --#constants 
		    n+2, --#inputs: x,t,dt  
		    2, --#rows in output
		    n, --#cols in output: x1,dx are ROWS
		    slpCORRECTOR,
		    slpHxno,
		    slpHno, 
		    o.maxCorrSteps,
		    o.EndZoneFactor
		    });
	  SLPcounter = SLPcounter + 1;
	  );
     ); ----------------- end ----------- M2 section -------------------------------------          

     compStartTime := currentTime();      

     PT := null;     
     rawSols := if member(o.Software,{M2enginePrecookedSLPs, M2engine}) then (
	  PT = if o.Software===M2engine then (
	       if isProjective then rawPathTrackerProjective( raw matrix {toList nS}, raw matrix {toList nT}, 
		    reBW'ST ) -- pass normalized start/target and Re(B-W product)
	       else rawPathTracker(raw H) 
	       ) else rawPathTrackerPrecookedSLPs(slpHxt, slpHxH);
	  
	  rawSetParametersPT(PT, 
	       isProjective,
	       o.tStep, o.tStepMin, 
	       toRR o.stepIncreaseFactor, toRR stepDecreaseFactor, o.numberSuccessesBeforeIncrease,
	       o.CorrectorTolerance, o.maxCorrSteps, o.EndZoneFactor, toRR o.InfinityThreshold,
	       ( -- pred_type:
		    if o.Predictor === Tangent then predTANGENT
		    else if o.Predictor === RungeKutta4 then predRUNGEKUTTA
		    else if o.Predictor === Euler then predEULER
		    else if o.Predictor === Certified then predPROJECTIVENEWTON
		    else error "engine: unknown predictor")
	       );
	  solsM := matrix apply(solsS,s->first entries transpose s);
	  rawLaunchPT(PT, raw solsM);
	  if o.NoOutput then null else 
	  apply(#solsS,i->apply({Coordinates, SolutionStatus, LastT, ConditionNumber, NumberOfSteps}, 
		    toList getSolution(PT,i), 
		    (attr,val)->if attr===Coordinates then val else attr=>val))
	  )
     else if o.Software===M2 then (
     	  apply(#solsS, sN->(
	       s := solsS#sN;
	       s'status := Processing;
	       endZone := false;
	       CorrectorTolerance := ()->(if endZone then o.EndZoneFactor else 1)*o.CorrectorTolerance;
	       if DBG > 2 then << "tracking solution " << toString s << endl;
     	       tStep := o.tStep;
	       predictorSuccesses := 0;
	       x0 := s; 
	       t0 := 0_K; 
	       count := 1; -- number of computed points
	       stepAdj := 0; -- step adjustment number (wrt previous step): 
	                     -- newstep = oldstep * stepIncreaseFactor^stepAdj  
	       if HISTORY then history := new MutableHashTable from{ count => new MutableHashTable from {
			 "t"=>t0,"x"=>x0
			 } };
	       while s'status === Processing and 1-t0 > theSmallestNumber do (
		    if 1-t0<=o.EndZoneFactor+theSmallestNumber and not endZone then (
			 endZone = true;
			 -- to do: see if this path coincides with any other path
			 );
		    if DBG > 4 then << "--- current t = " << t0 << endl;
                    -- monitor numerical stability: perhaps change patches if not stable ???
		    -- Hx0 := evalHx(x0,t0);
		    -- svd := sort first SVD Hx0;
		    -- if o.Projectivize and first svd / last svd > condNumberThresh then ( 
     	       	    --	 << "CONDITION NUMBER = " <<  first svd / last svd << endl;			 
		    --	 );

		    -- predictor step
		    if DBG>9 then << ">>> predictor" << endl;
		    local dx; local dt;
		    -- default dt; Certified and Multistep modify dt
		    dt = if endZone then min(realPart tStep, realPart(1-t0)) else min(realPart tStep, realPart(1-o.EndZoneFactor-t0));

     	       	    -- projective stuff
		    if o.Predictor == Certified then (
			 dPatch = matrix{ flatten entries x0 / conjugate};
			 )
		    else if dPatch =!= null then ( -- generate dynamic patch
			 if DBG>9 then << "dynamic patch... ";
			 HxNoPatch := evalHxNoPatch(x0,t0);
			 -- patch = conjugate of the normalized kernel vector 
			 --dPatch = matrix{ flatten entries normalize transpose gens ker HxNoPatch / conjugate };
			 dPatch = matrix{ flatten entries x0 / conjugate};
			 if DBG>9 then << "generated" << endl;
			 );   
		    
		    if o.Predictor == Tangent then (
			 if o.SLP =!= false and o.SLPpredictor then (
			      dxFromSLP := rawEvaluateSLP(slpPred, raw (transpose x0 | matrix {{t0,dt}}));
			      dx = lift(map(K,dxFromSLP),K);
			      )
			 else (
		    	      Hx0 := evalHx(x0,t0);
			      Ht0 := evalHt(x0,t0);
			      dx = solve(Hx0,-dt*Ht0);
			      );
			 ) 
		    else if o.Predictor == Euler then (
			 H0 := evalH(x0,t0);
			 Hx0 = evalHx(x0,t0);
			 Ht0 = evalHt(x0,t0);
			 dx = solve(Hx0, -H0-Ht0*dt);
			 )
		    else if o.Predictor == Secant then (
			 if count > 1 then ( -- if there is a preceding point
			      u := x0 - history#(count-1)#"x";			      
			      dx = dt*normalize u;
     			      )			      
			 else ( -- use tangential predictor
			      Hx0 = evalHx(x0,t0);
			      Ht0 = evalHt(x0,t0);
			      dx = solve(Hx0,-dt*Ht0);
			      );
			 )
		    else if o.Predictor == RungeKutta4 then (
			 --k1 := evalMinusInverseHxHt(x0,t0);
			 --k2 := evalMinusInverseHxHt(x0+.5*k1,t0+.5*dt);
			 --k3 := evalMinusInverseHxHt(x0+.5*k2,t0+.5*dt);
			 --k4 := evalMinusInverseHxHt(x0+k3,t0+dt);
			 --dx = (1/6)*(k1+2*k2+2*k3+k4)*dt;     
			 if o.SLP =!= false and o.SLPpredictor then (
			      dxFromSLP = rawEvaluateSLP(slpPred, raw (transpose x0 | matrix {{t0,dt}}));
			      dx = lift(map(K,dxFromSLP),K);
			      )
			 else (
			      dx1 := solveHxTimesDXequalsMinusHt(x0,t0);
			      dx2 := solveHxTimesDXequalsMinusHt(x0+.5*dx1*dt,t0+.5*dt);
			      dx3 := solveHxTimesDXequalsMinusHt(x0+.5*dx2*dt,t0+.5*dt);
			      dx4 := solveHxTimesDXequalsMinusHt(x0+dx3*dt,t0+dt);
			      dx = (1/6)*dt*(dx1+2*dx2+2*dx3+dx4);     
			      )
			 )
		    else if o.Predictor == Multistep then (
			 if DBG>3 then << ">>> entering Multistep predictor: x0=" << x0 << ", t0=" << t0 << endl;
     	       	    	 history#count#"rhsODE" = evalMinusInverseHxHt(x0,t0);
			 nPoints := min(count, o.MultistepDegree);
			 -- think: t_0, ..., t_{nPoints-1}
			 
			 --if stepAdj < -1 then ( -- the step has been decreased twice
			 --     nPoints = 1;
			 --     );
			 
			 if tStep > 1-t0 then (
			      if DBG > 2 then << "tStep > 1-t" << endl;
			      while stepDecreaseFactor*tStep >= 1-t0 do (
				   -- reduce the step as much as possible 
				   -- (so that if predictor fails, then t0+stepDecreaseFactor*tStep < 1)
			      	   stepAdj = stepAdj - 1;
	 	 	 	   tStep = stepDecreaseFactor*tStep;
			      	   );
			      dt = 1-t0;
			      )
			 else dt = tStep;
			 
			 stepAdjSequence := 
			 if nPoints == 1 then ( 
			      delta := dt; -- delta = t_1 - t_0
			      {} -- produces tangent predictor
			      )
			 else ( -- at least two previous points are used
			      delta = ( history#(count-nPoints+2)#"t" - history#(count-nPoints+1)#"t" );
			      apply(nPoints-2, i->history#(count-nPoints+3+i)#"stepAdj") | {stepAdj}
			      );
			 if DBG>3 then << "stepAdjSequence = " << stepAdjSequence << endl;
			 			 
			 MScoeffs := 
			 if tStep>1-t0 and nPoints > 1 then (
			      aMScoeffs := multistepPredictorLooseEnd(o.stepIncreaseFactor,drop(stepAdjSequence,-1));
			      aRing := ring first aMScoeffs;
			      aMap := map(K,aRing, matrix{{dt/(history#count#"t"-history#(count-1)#"t")}});
			      aMScoeffs/aMap  
			      )
			 else multistepPredictor(o.stepIncreaseFactor,stepAdjSequence); 
     	       	    	 
                	 -- dx = dt*sum_{i=0..nPoints-1} MScoeff_i*rhsODE(t_i)
			 dx = delta*sum(nPoints, i->MScoeffs#i*history#(count-nPoints+1+i)#"rhsODE");
			 if DBG > 3 then << "delta = " << delta << "   MScoeffs = " << MScoeffs << endl;
			 )
		    else if o.Predictor == Certified then (
			 Hx0 = evalHx(x0,t0);
			 Ht0 = evalHt(x0,t0);
			 chi2 := sqrt((norm2 Ht0)^2 + (norm2 solve(Hx0, Ht0))^2);
			 chi1 := 1 / min first SVD(DMforPN*Hx0);
			 if DBG > 2 then (
			      << "chi1 = " << chi1 << endl;
			      if count<=5 then print(DMforPN*Hx0);
			      );
			 dt = 0.04804448/(maxDegreeTo3halves*chi1*chi2*bigT); -- divide by bigT since t is in [0,1]
			 if dt<o.tStepMin then (
			      if DBG > 2 then (
				   << "chi1 = " << chi1 << endl;
			      	   << "chi2 = " << chi2 << endl;
				   );
			      s'status = MinStepFailure; 
			      --error "too small step";
			      );
			 if dt > 1-t0 then dt = 1-t0;
			 dx = 0;
			 )
		    else error "unknown Predictor";


		    if DBG > 3 then << " x0 = " << x0 << ", t0 = " << t0 << ", res=" <<  toString evalH(x0,t0) << ",  dt = " << dt << ",  dx = " << toString dx << endl;
		    if HISTORY then history#count#"dx" = dx;

    	 	    t1 := t0 + dt;
		    x1 := x0 + dx;
		    
		    -- corrector step
		    if DBG>9 then << ">>> corrector" << endl;
		    nCorrSteps := 0;
		    if o.Predictor === Certified then (
			 nCorrSteps = 1;
			 dx = solve(evalHx(x1,t1), -evalH(x1,t1));
			 x1 = x1 + dx;
			 )
		    else if o.SLPcorrector then (
			 corr'error := rawEvaluateSLP(slpCorr, raw (transpose x1 | matrix {{t1,dt}}));
			 corr'error = transpose lift(map(K,corr'error),K);
			 x1 = corr'error_{0};
			 dx = corr'error_{1};
			 if DBG > 4 then << "x=" << toString x1 << " res=" <<  toString evalH(x1,t1) << endl << " dx=" << dx << endl;
			 )
		    else (
		    	 dx = infinity;
		    	 while dx === infinity or norm dx > CorrectorTolerance()*norm x1+theSmallestNumber 
			 and nCorrSteps < o.maxCorrSteps
		    	 do( 
			      dx = solve(evalHx(x1,t1), -evalH(x1,t1));
			      x1 = x1 + dx;
			      nCorrSteps = nCorrSteps + 1;
			      if DBG > 4 then <<"corrector step " << nCorrSteps << endl <<"x=" << toString x1 << " res=" <<  toString evalH(x1,t1) 
			      << endl << " dx=" << dx << endl;
			      if (not isProjective and norm x1 > o.InfinityThreshold) 
			      or (o.Projectivize and x1_(n-1,0) < 1/o.InfinityThreshold)
			      then ( s'status = Infinity; dx = 0 );
			      );
			 );
		    if DBG>9 then << ">>> step adjusting" << endl;
		    if o.Predictor =!= Certified and dt > o.tStepMin 
		    and norm dx > CorrectorTolerance() * norm x1 then ( -- predictor failure 
			 predictorSuccesses = 0;
			 stepAdj = stepAdj - 1;
	 	 	 tStep = stepDecreaseFactor*tStep;
			 if DBG > 2 then << "decreased tStep to "<< tStep << endl;	 
			 if tStep < o.tStepMin then s'status = MinStepFailure;
			 ) 
		    else ( -- predictor success
			 predictorSuccesses = predictorSuccesses + 1;
			 x0 = x1;
			 if dPatch =!= null then x0 = normalize x0;
		         t0 = t1;
			 count = count + 1;
		         if HISTORY then history#count = new MutableHashTable from {"t"=>t0,"x"=>x0,"stepAdj"=>stepAdj};
			 if nCorrSteps <= o.maxCorrSteps - 1 -- over 2 / minus 2 ???
              		    and predictorSuccesses >= o.numberSuccessesBeforeIncrease 
			 then (			      
			      predictorSuccesses = 0;
			      if o.Predictor =!= Certified then (
			      	   stepAdj = 1;
			      	   tStep = o.stepIncreaseFactor*tStep;	
			      	   if DBG > 2 then << "increased tStep to "<< tStep << endl;
				   )
			      )
			 else stepAdj = 0; -- keep the same step size
			 );
		    );        	    
	       if s'status===Processing then s'status = Regular;
	       if DBG > 0 then << (if s'status == Regular then "."
		    else if s'status == Singular then "S"
		    else if s'status == MinStepFailure then "M"
		    else if s'status == Infinity then "I"
		    else error "unknown solution status"
		    ) << if (sN+1)%100 == 0 then endl else flush;
	       -- create a solution record 
	       (x0,
		    NumberOfSteps => count-1, -- number of points - 1 
		    SolutionStatus => s'status, 
		    LastT => t0, 
		    ConditionNumber => conditionNumber evalHx(x0,t0)
		    ) | ( if HISTORY
		    then sequence new HashTable from history 
		    else sequence ())
	       ))
     );
     if DBG>3 then print rawSols;
     ret := if o.NoOutput then null 
          else if o.Software===M2engine or o.Software===M2enginePrecookedSLPs then (
	       if o.Projectivize then apply(rawSols, s->(
			 ss := first s;
			 if norm(last ss) < 1/o.InfinityThreshold then print "Warning: solution at infinity encountered";
			 {apply(drop(ss,-1),u->(1/last ss)*u)}|drop(s,1)
			 ))
	       else rawSols 
          ) else (
     	       if o.Projectivize then (
	  	    rawSols = apply(rawSols, s->(
		    	      s' := flatten entries first s;
		    	      s'status := s#2#1;
		    	      if norm(last s') < 1/o.InfinityThreshold then s'status = Infinity;
		    	      {matrix {apply(drop(s',-1),u->(1/last s')*u)}} | {s#1} | {SolutionStatus => s'status} | drop(toList s, 3) 
	       	    	      ))
	  	    );
	       rawSols/(s->{flatten entries first s} | drop(toList s,1))
	       );
     if DBG>0 then (
	  if o.Software==M2 then (
	       << "Number of solutions = " << #ret << endl << "Average number of steps per path = " << toRR sum(ret,s->s#1#1)/#ret << endl;
     	       if DBG>1 then (
	       	    if o.SLP =!= false 
	       	    then << "Hx SLP: " << slpHx << endl << "Ht SLP: " << slpHt << endl << "H SLP: " << slpH << endl
	       	    else << "Evaluation time (M2 measured): Hx = " << etHx << " , Ht = " << etHt << " , H = " << etH << endl;
		    )
	       )
	  else if o.Software==M2enginePrecookedSLPs and DBG>1 then (
	       << "Hxt SLP: " << slpHxt << endl << "HxH SLP: " << slpHxH << endl;  
	       );
	  << "Setup time: " << compStartTime - setupStartTime << endl;
	  << "Computing time:" << currentTime() - compStartTime << endl; 
	  );
     apply(ret, s->(
	     p := point (
		 if HISTORY then drop(toList s, -1)
		 else toList s
		 );
	     p.SolutionSystem = T;
	     --if p.ConditionNumber > o.SingularConditionNumber and p.SolutionStatus===Regular 
	     --then p.SolutionStatus = Singular;
	     if PT=!=null then p.Tracker=PT;
	     p
	     ))
     )

debug Core -------------------------------------------------------------------------

-------------------------------------------------------------------------------------
-- trackHomotopyM2engine SUBROUTINES
trackHomotopyM2engine = (H, inp, 
	out, statusOut,
	tStep, tStepMin, 
	CorrectorTolerance, maxCorrSteps, 
	InfinityThreshold,
	checkPrecision
	) -> (
    if numVariables H != numrows inp - 1 -- one input is t-value 
    then error "the number of variables does not match the number of inputs"; 
    K := ring inp;
    if K =!= ring out then error "inp and out have to have the same ring";
    rawHomotopyTrack(getRawHomotopy(H,K), raw inp, 
	raw out, raw statusOut,
	tStep, tStepMin, 
	CorrectorTolerance, maxCorrSteps, 
	InfinityThreshold,
	checkPrecision)
    )
extractM2engineOutput = method()
extractM2engineOutput (MutableMatrix,MutableMatrix) := (out,statusOut) -> (
    nSols := numColumns out; 
    n := numRows out - 2;
    assert(nSols == numColumns statusOut);
    apply(nSols, sN->(
	    s'status := solutionStatusLIST#(statusOut_(0,sN));
	    count := statusOut_(1,sN);
	    if DBG > 0 then << (if s'status == Regular then "."
		else if s'status == Singular then "S"
		else if s'status == MinStepFailure then "M"
		else if s'status == Infinity then "I"
		else if s'status == Origin then "O"
		else if s'status == IncreasePrecision then "P"
		else if s'status == DecreasePrecision then "p"
		else error "unknown solution status"
		) << if (sN+1)%100 == 0 or sN==nSols-1 then endl else flush;
	    -- create a solution record 
	    x0 := submatrix(out,toList(0..n-1),{sN});
	    point {flatten entries x0,
		SolutionStatus => s'status, 
		NumberOfSteps => count,
		LastT => out_(n,sN), 
		LastIncrement => out_(n+1,sN)
		}
	    ))
    )    

minimalStepSize = method() 
minimalStepSize ZZ := prec -> 2.^(15-prec) 

higherPrecision = new HashTable from {
    53 => 100,
    100 => 200,
    200 => 400,
    400 => 800,
    800 => 1600,
    1600 => null
    }
lowerPrecision = new HashTable from {
    1600 => 800,
    800 => 400,
    400 => 200,
    200 => 100,
    100 => 53,
    53 => null
    }

trackHomotopy = method(TypicalValue => List, Options =>{
	  Field => null,
	  Software => null, 
	  NoOutput => null, 
     	  -- step control
	  tStep => null, -- initial
          tStepMin => null,
	  stepIncreaseFactor => null,
	  numberSuccessesBeforeIncrease => null,
	  -- predictor 
	  Predictor=>null, 
	  -- corrector 
	  maxCorrSteps => null,
	  Precision => null, 
     	  CorrectorTolerance => null, -- tracking tolerance
	  -- end of path
	  EndZoneFactor => null, -- EndZoneCorrectorTolerance = CorrectorTolerance*EndZoneFactor when 1-t<EndZoneFactor 
	  InfinityThreshold => null -- used to tell if the path is diverging
	  } )
  
  
trackHomotopy(Matrix,List) := List => o -> (H,solsS) -> (
    F := gateMatrix polySystem H;
    XT := getVarGates ring H;
    X := drop(XT,-1);
    T := last XT;
    trackHomotopy(gateHomotopy(F, gateMatrix{X}, T), solsS, o)
    )
 
trackHomotopy(Sequence,List) :=
trackHomotopy(Homotopy,List) := List => o -> (H,solsS) -> (
-- tracks homotopy H starting with solutions solsS 
-- IN:  H = either a column vector of polynomials in CC[x1,...,xn,t]  -- the last variable is assumed to be the _continuation parameter_
--          or an SLP representing one -- !!! at this point it is preSLP
--      solsS = list of one-column matrices over CC
-- OUT: solsT = list of target solutions corresponding to solsS
     if #solsS === 0 then return {};
     o = fillInDefaultOptions o;
     stepDecreaseFactor := 1/o.stepIncreaseFactor;
     theSmallestNumber := 1e-16;

     -- evaluation times
     etH := 0;
     etHx := 0; 
     etHt := 0;    
    
     if instance(H,Sequence) -* preSLP,  if o.Software===M2enginePrecookedSLPs 
	                        then compile preSLPs in the engine *-
     then ( -- preSLPs (developer only) -------------------------------------------------------
     	 (R,slpH) := H; 
	 n := numgens R - 1;
     	 K := coefficientRing R;
      	 --
	 fromSlpMatrix := if o.Software===M2enginePrecookedSLPs then
     	 (S,params) -> (
	     result := rawEvaluateSLP(S, raw params);
	     lift(map(K, result),K)
	     )
	 else (S,inputMatrix) -> evaluatePreSLP(S, flatten entries inputMatrix);
     	 slpHx := transposePreSLP jacobianPreSLP(slpH,toList(0..n-1));
     	 slpHt := transposePreSLP jacobianPreSLP(slpH,{n}); 
     	 toSLP := pre -> (
       	     (constMAT, prog) := preSLPinterpretedSLP (n+1, pre);
       	     rawSLP(raw constMAT, prog)
       	     );  
     	 if o.Software===M2enginePrecookedSLPs then ( -- !!! used temporarily
	     slpH = toSLP slpH; 
	     slpHx = toSLP slpHx;
	     slpHt = toSLP slpHt; 	 
	     );    
       	 evalH := (x0,t0)-> (
	     tr := timing (
	       	 transpose fromSlpMatrix(slpH, transpose x0 | matrix {{t0}})
	       	 );
	     etH = etH + tr#0;
	     tr#1
	     );
       	 evalHx := (x0,t0)-> (
	     tr := timing (
	       	 fromSlpMatrix(slpHx, transpose x0 | matrix {{t0}})
	       	 );
	     etHx = etHx + tr#0;
	     tr#1
	     );  
       	 evalHt := (x0,t0)->(
	     tr := timing (
	       	 fromSlpMatrix(slpHt, transpose x0 | matrix {{t0}})
	       	 );
	     etHt = etHt + tr#0;
	     tr#1
	     );
    	 )
     else if instance(H,Homotopy) then ( -- main case
     	 if o.Software =!= M2engine then (--... but M2engine does not need evaluation functions 
	     K = CC_53; --!!!
      	     evalH = (x0,t0)-> (
	     	 tr := timing evaluateH(H,x0,t0);
	     	 etH = etH + tr#0;
	     	 tr#1
	     	 );
       	     evalHx = (x0,t0)-> (
	     	 tr := timing evaluateHx(H,x0,t0);
	     	 etHx = etHx + tr#0;
	     	 tr#1
	     	 );  
       	     evalHt = (x0,t0)->(
	     	 tr := timing evaluateHt(H,x0,t0);
	     	 etHt = etHt + tr#0;
	     	 tr#1
	     	 );
	     )
    	 )     
     else error "unexpected type of homotopy (first parameter)";
     
     -- Sortware=>M2 only ---------------------------------------------------------------------------------
     solveLinear := (A,b) -> solve(A,b,ClosestFit=>true,MaximalRank=>true); -- this takes care of non-square case!!!
     solveHxTimesDXequalsMinusHt := (x0,t0) -> solveLinear(evalHx(x0,t0),-evalHt(x0,t0));
     
     compStartTime := currentTime();      

--------------------- M2engine --------------------------------------------------------------------------
     rawSols := if o.Software===M2engine then ( 
	 prec := o.Precision; -- current precision
	 checkPrecision := o.Precision===infinity; 
	 if prec === infinity then prec = 53; -- for adaptive precision, start with double precision
	 mainRing := (o.Field)_prec; -- homotopyRing, i.e. RR or CC with current precision !!!
	 -- if class H === SpecializedParameterHomotopy then 1/0;
	 if not canHaveRawHomotopy H then error "expected a Homotopy with RawHomotopy";  
	 nSols := #solsS;
	 statusOut := mutableMatrix(ZZ,2,nSols); -- 2 rows (status, number of steps), #solutions columns 
	 inp := mutableMatrix promote(matrix(
	     	 transpose apply(solsS, s->(if instance(s,Point) 
		     	 then coordinates s 	 
		     	 else flatten entries s) | {0} -- track from t=0 
		     )),
	     mainRing); 
	 n = numrows inp - 1;
	 out := mutableMatrix(mainRing,n+2,nSols); -- one more row than inp (to store last increment)
	 scan(nSols, i->out_(n,i) = 1); -- track to t=1 
	 ti'out := timing trackHomotopyM2engine(H, inp, 
	     out, statusOut,
	     o.tStep, o.tStepMin, 
	     o.CorrectorTolerance, o.maxCorrSteps, 
	     toRR o.InfinityThreshold,
	     checkPrecision);
	 if DBG>2 then 
	 << "-- trackHomotopyM2engine time: " << first ti'out << " sec." << endl;
    	 sols := new MutableList from extractM2engineOutput(out,statusOut);
	 if o.Precision === infinity then (
	     tempInpMatrix := memoize (F->mutableMatrix(F,n+1,1));
	     tempOutMatrix := memoize (F->mutableMatrix(F,n+2,1));
	     statusOut = mutableMatrix(ZZ,2,1);
	     for nS to #sols-1 do (
		 s := sols#nS;
		 currentPrec := prec;
	     	 while not ( 
		     member(status s, {Regular,Infinity,Origin}) 
		     or realPart(1-s.LastT) < o.EndZoneFactor
		     or currentPrec === null 
		     ) do ( 
		     if status s === DecreasePrecision then (
			 -- decrease precision increase minimal step size
			 currentPrec = lowerPrecision#currentPrec;
			 if currentPrec === null then error "precision can not be lowered (this is a bug)";
			 F := o.Field_currentPrec;
			 inp = tempInpMatrix F;
			 scan(n, i->inp_(i,0) = (coordinates s)#i);
			 inp_(n,0) = s.LastT;
			 out = tempOutMatrix F; 
			 out_(n,0) = 1;
			 ti'out := timing trackHomotopyM2engine(H, inp, 
			     out, statusOut,
			     abs s.LastIncrement, minimalStepSize currentPrec, 
			     o.CorrectorTolerance, o.maxCorrSteps, 
			     toRR o.InfinityThreshold,
			     checkPrecision);
			 if DBG>3 then 
			 << "-- trackHomotopyM2engine (at decreased prec="<< currentPrec << ") time: " << first ti'out << " sec." << endl;
			 sols#nS = first extractM2engineOutput(out,statusOut);
			 (sols#nS).NumberOfSteps = (sols#nS).NumberOfSteps+s.NumberOfSteps;
			 s = sols#nS;
			 )
		     else ( -- status s === IncreasePrecision... or other reason
		     	 -- increase precision decrease minimal step size
			 currentPrec = higherPrecision#currentPrec;
			 if currentPrec =!= null then (
			     F = o.Field_currentPrec;
			     inp = tempInpMatrix F;
			     scan(n, i->inp_(i,0) = (coordinates s)#i);
			     inp_(n,0) = s.LastT;
			     out = tempOutMatrix F; 
			     out_(n,0) = 1;
			     ti'out = timing trackHomotopyM2engine(H, inp, 
	     			 out, statusOut,
	     			 abs s.LastIncrement, minimalStepSize currentPrec, 
	     			 o.CorrectorTolerance, o.maxCorrSteps, 
	     			 toRR o.InfinityThreshold,
				 checkPrecision);
	 		     if DBG>3 then 
	 		     << status s << "-- trackHomotopyM2engine (at increased prec="<< currentPrec << ") time: " << first ti'out << " sec." << endl;
			     sols#nS = first extractM2engineOutput(out,statusOut);
			     (sols#nS).NumberOfSteps = (sols#nS).NumberOfSteps+s.NumberOfSteps;
			     s = sols#nS;
    	 		     )			 
		  	 )
		     ); -- while ... 
		     if status s =!= Regular then -- change status to Regular if at t=1 
		     if abs(realPart s.LastT) < theSmallestNumber then s.SolutionStatus = Regular;   
		 ) -- for nS ... 
	     );	      
	 toList sols 
	 )
     ---------------- M2 (top-level) -------------------------------------------------------------------------------------
     else if o.Software===M2 
     or o.Software===M2enginePrecookedSLPs -- !!! used temporarily
     then 
	 apply(#solsS, sN->(
	       s := solsS#sN;
	       s'status := Processing;
	       endZone := false;
	       CorrectorTolerance := ()->(if endZone then o.EndZoneFactor else 1)*o.CorrectorTolerance;
	       if DBG > 2 then << "tracking solution " << toString s << endl;
     	       tStep := sub(o.tStep,K);
	       predictorSuccesses := 0;
	       x0 := if instance(s,Point) then transpose matrix s else s; 
	       t0 := 0_K; 
	       count := 1; -- number of computed points
	       stepAdj := 0; -- step adjustment number (wrt previous step): 
	                     -- newstep = oldstep * stepIncreaseFactor^stepAdj  
	       while s'status === Processing and 1-t0 > theSmallestNumber do (
		    if 1-t0<=o.EndZoneFactor+theSmallestNumber and not endZone then (
			 endZone = true;
			 -- to do: see if this path coincides with any other path
			 );
		    if DBG > 4 then << "--- current t = " << t0 << "; precision = " << precision ring x0 << endl;
             
		    -- predictor step
		    if DBG>9 then << ">>> predictor" << endl;
		    local dx; local dt;
		    dt = if endZone then min(realPart tStep, realPart(1-t0)) else min(realPart tStep, realPart(1-o.EndZoneFactor-t0));

		    dx = if o.Predictor === zero then 0*x0
		    else if o.Predictor === Tangent then dt*solveHxTimesDXequalsMinusHt(x0,t0)
		    else if o.Predictor === Euler then (
			H0 := evalH(x0,t0);
			Hx0 := evalHx(x0,t0);
			Ht0 := evalHt(x0,t0);
			solveLinear(Hx0, -H0-Ht0*dt)
			)
		    else if o.Predictor === RungeKutta4 then (
			dx1 := solveHxTimesDXequalsMinusHt(x0,t0);
			dx2 := solveHxTimesDXequalsMinusHt(x0+(1/2)*dx1*dt,t0+(1/2)*dt);
			dx3 := solveHxTimesDXequalsMinusHt(x0+(1/2)*dx2*dt,t0+(1/2)*dt);
			dx4 := solveHxTimesDXequalsMinusHt(x0+dx3*dt,t0+dt);
			(1/6)*dt*(dx1+2*dx2+2*dx3+dx4)
			)
		    else error "unknown Predictor";

		    if DBG > 3 then << " x0 = " << x0 << ", t0 = " << t0 << ", res=" <<  toString evalH(x0,t0) << ",  dt = " << dt << ",  dx = " << toString dx << endl;

    	 	    t1 := t0 + dt;
		    x1 := x0 + dx;
		    
		    -- corrector step
		    if DBG>9 then << ">>> corrector" << endl;
		    nCorrSteps := 0;
		    dx = infinity;
		    while dx === infinity or norm dx > CorrectorTolerance()*norm x1+theSmallestNumber 
		    and nCorrSteps < o.maxCorrSteps
		    do( 
			dx = solveLinear(evalHx(x1,t1), -evalH(x1,t1));
			x1 = x1 + dx;
			nCorrSteps = nCorrSteps + 1;
			if DBG > 4 then <<"corrector step " << nCorrSteps << endl <<"x=" << toString x1 << " res=" <<  toString evalH(x1,t1) 
			<< endl << " dx=" << dx << endl;
			if norm x1 > o.InfinityThreshold
			then ( s'status = Infinity; dx = 0 );
			);
		    if DBG>9 then << ">>> step adjusting" << endl;
		    if dt > o.tStepMin 
		    and norm dx > CorrectorTolerance() * norm x1 then ( -- predictor failure 
			predictorSuccesses = 0;
			stepAdj = stepAdj - 1;
			tStep = stepDecreaseFactor*tStep;
			if DBG > 2 then << "decreased tStep to "<< tStep << endl;	 
			if tStep < o.tStepMin then s'status = MinStepFailure;
			) 
		    else ( -- predictor success
			predictorSuccesses = predictorSuccesses + 1;
			x0 = x1;
			t0 = t1;
			count = count + 1;
			if nCorrSteps <= o.maxCorrSteps - 1 -- over 2 / minus 2 ???
			   and predictorSuccesses >= o.numberSuccessesBeforeIncrease 
			then (			      
			    predictorSuccesses = 0;
			    if o.Predictor =!= Certified then (
				stepAdj = 1;
				tStep = o.stepIncreaseFactor*tStep;	
				if DBG > 2 then << "increased tStep to "<< tStep << endl;
				)
			    )
			 else stepAdj = 0; -- keep the same step size
			 );
		    );        	    
	       if s'status===Processing then s'status = Regular;
	       if DBG > 0 then << (if s'status == Regular then "."
		    else if s'status == Singular then "S"
		    else if s'status == MinStepFailure then "M"
		    else if s'status == Infinity then "I"
		    else error "unknown solution status"
		    ) << if (sN+1)%100 == 0 then endl else flush;
	       -- create a solution record 
	       (x0,
		   NumberOfSteps => count-1, -- number of points - 1 
		   SolutionStatus => s'status, 
		   LastT => t0, 
		   ConditionNumber => conditionNumber evalHx(x0,t0)
		   ) 
    	    ))
    else error "wrong Software option";
    
    if DBG>3 then print rawSols;
    ret := if instance(first rawSols,Point) then rawSols else
         apply(rawSols,s->point({flatten entries first s} | drop(toList s,1))); 	 
    if DBG>1 then (
	if member(o.Software,{M2,M2engine}) then (
	    << "Number of solutions = " << #ret << endl 
	    << "Evaluation time (M2 measured): Hx = " << etHx << " , Ht = " << etHt << " , H = " << etH << endl;
	    )
	);
     scan(ret,s->s#"H"=H);
     ret
     ) -- trackHomotopy

getSolution = method(Options =>{SolutionAttributes=>(Coordinates, SolutionStatus, LastT, ConditionNumber, NumberOfSteps)})
getSolution(Thing, ZZ) := Thing => o -> (PT,i) -> (
-- gets specified solution from the engine (not exported anymore)
-- IN:  (rawPathTracker, solution's number)
--      SolutionAttributes=> ... specifies various data attached to a solution ...
-- OUT: whatever is requested by SolutionAttributes (either as a sequence of as a single element)
     p := o.SolutionAttributes; 
     possible := set{Coordinates, SolutionStatus, LastT, ConditionNumber, NumberOfSteps};
     if not isSubset(set{p}, possible) and 
     not (class p === Sequence and isSubset(set p, possible))
     then error "wrong SolutionAttributes option";
     pp := if class p === Sequence then p else {p};
     ret := apply(pp, r->
	  if r===Coordinates then flatten entries map(CC_53, rawGetSolutionPT(PT, i))
	  else if r===SolutionStatus then solutionStatusLIST#(rawGetSolutionStatusPT(PT, i))
	  else if r===LastT then rawGetSolutionLastTvaluePT(PT, i)
	  else if r===ConditionNumber then rawGetSolutionRcondPT(PT, i)
	  else if r===NumberOfSteps then rawGetSolutionStepsPT(PT, i)
	  );
     if class p === Sequence then ret else first ret
     )

-*
trackSegment = method(Options=>track'option'list) -- a better implementation needed!!!
trackSegment (PolySystem,Number,Number,List) := o -> (H,a,b,pts) -> (
    S := specializeContinuationParameter(H,a);
    T := specializeContinuationParameter(H,b);
    track(S,T,pts,o) 
    )
*-

-- The following code added by MES, for debugging purposes
-- for adaptive homotopy tracking.  This code is not exported or used elsewhere.
mesTracker = method(Options => {
        tStep => .05,
        tStepMin => .000001,
        CorrectorTolerance => .000001,
        maxCorrSteps => 3,
        InfinityThreshold => 100000.,
	LastT => 1.
        })
mesTracker(Homotopy, MutableMatrix) := o -> (H, inp) -> (
    -- Returns a pair (out, outStatus)
    -- out: d x npoints MutableMatrix of points (last coordinate is homotopy parameter)
    -- outStatus: a list of length npoints, where the i-th element is 
    --  (status, nsteps)
    -- status can be:
    --  Undetermined, Processing, Regular, Singular, Infinity, MinStepFailure, RefinementFailure
    -- ANTON: which of these can actually occur here?  All of them?
    -- nsteps: number of tracker steps?
    n := numrows inp;
    out := mutableMatrix(ring inp, n+2, numcols inp);
    for i from 0 to numColumns inp-1 do out_(n, i) = o.LastT;
    statusOut := mutableMatrix(ZZ, 2, numColumns inp);
    trackHomotopyM2engine(
        H, 
        inp, 
        out, 
        statusOut, 
        o.tStep,
        o.tStepMin,
        o.CorrectorTolerance,
        o.maxCorrSteps,
        o.InfinityThreshold,
	false
        );
    -- now we need to grab results
    --(out, for i from 0 to numColumns statusOut - 1 list (solutionStatusLIST#(statusOut_(0,i)), statusOut_(1,i)))
    (out,statusOut)
    )

TEST ///
setRandomSeed 0
debug needsPackage "NumericalAlgebraicGeometry"
NAGtrace 1
n = 2; d = 2;
R=QQ[x_0..x_(n-1)]
eps = 1/10^14
T = apply(n, i->if i==0 then x_i^d-eps^d else (x_i-i)^d-eps^(d-1)*x_i)
(S,solsS) = totalDegreeStartSystem T
H = segmentHomotopy(S,T,gamma=>1+pi*ii)
sols = trackHomotopy(H,solsS,tStepMin=>minimalStepSize 53,CorrectorTolerance=>1e-15,Precision=>infinity,EndZoneFactor=>0)
peek sols 
assert all(sols, s->status s === Regular and s.NumberOfSteps > 100)

sols = trackHomotopy(H,solsS,tStepMin=>minimalStepSize 53,CorrectorTolerance=>1e-15,Precision=>53,EndZoneFactor=>0)
peek sols 
assert all(sols, s->status s === MinStepFailure)

sols = trackHomotopy(H,solsS,tStepMin=>minimalStepSize 100,CorrectorTolerance=>1e-15,Precision=>100,EndZoneFactor=>0)
peek sols 
assert all(sols, s->status s === Regular)
///
