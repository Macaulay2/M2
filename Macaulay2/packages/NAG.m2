newPackage(
	"NAG",
    	Version => "1.0", 
    	Date => "04/01/2008",
    	Authors => {
	     {Name => "Anton Leykin", Email => "leykin@ima.umn.edu"}
	     },
    	HomePage => "http://www.ima.umn.edu/~leykin",
    	Headline => "Numerical Algebraic Geometry",
	Configuration => { "PHCpack" => "phc",  "Bertini" => "bertini", "HOM4PS2" => "hom4ps2" },	
	-- DebuggingMode should be true while developing a package, 
	--   but false after it is done
    	DebuggingMode => true 
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
     solveSystem, track, refine, totalDegreeStartSystem,
     areEqual, sortSolutions, multistepPredictor, multistepPredictorLooseEnd,
     --options
     Software,PHCpack,Bertini,HOM4PS2,
     gamma,tDegree,tStep,tStepMin,stepIncreaseFactor,numberSuccessesBeforeIncrease,
     Predictor,RungeKutta4,Multistep,Tangent,Euler,Secant,MultistepDegree,
     finalMaxCorSteps, maxCorSteps, 
     Projectivize,
     AffinePatches, DynamicPatch,
     RandomSeed     
     }
exportMutable {
     }

-- GLOBAL VARIABLES ----------------------------------
PHCexe = NAG#Options#Configuration#"PHCpack";
BERTINIexe = NAG#Options#Configuration#"Bertini";
HOM4PS2exe = NAG#Options#Configuration#"HOM4PS2";

DBG = 0; -- debug level (10=keep temp files)

-- CONVENTIONS ---------------------------------------

-- Polynomial systems are represented as lists of polynomials.

-- Solutions are pairs (s, h) where s is list of coordinates (in CC)
-- and h is a hashtable collecting all extra information.
 
-- M2 tracker ----------------------------------------
integratePoly = method(TypicalValue => RingElement)
integratePoly (RingElement,Thing,Thing) := RingElement => (f,a,b) -> (
     -- integral of a polynomial f from a to b
     R := ring f; 
     if numgens R != 1 then error "expected a univariate polynomial";
     CM := coefficients f;
     M := apply(flatten entries first CM, m->(
	       e := first flatten exponents m;
	       (1/(e+1))*R_0^(e+1)     
	       ));
     g := (matrix{M}*CM#1)_(0,0); -- antiderivative
     T := first gens R; 
     sub(g,T=>b)-sub(g,T=>a)
     )
		       

multistepHash = new MutableHashTable;
multistepPredictor = method(TypicalValue => List)
multistepPredictor (QQ,List) := List => (c,s) -> (
-- coefficients for a multistep predictor
-- IN:  c = step adjustment coefficient (in QQ)
--      s = list of step adjustments (from the stepsize h = t_1-t_0)
--          s#i =  1 => t_(i+2)-t_(i+1) = c^(s#j)*(t_(i+1)-t_i)
-- OUT: b = coefficients in Adams-Bashforth-like method (list of rational numbers)
     if multistepHash#?(c,s) then return multistepHash#(c,s);
     t := symbol t;
     n := #s + 1; -- t_n is the one for which prediction is being made
     R := QQ; -- frac(QQ[h]);        
     step := 1_R; -- h; WLOG, assume h=1 
     t_0 = 0_R;
     scan(n, j->(
	       t_(j+1) = t_j+step;
	       if DBG>3 then << "t_("<< j+1 <<") = " << t_(j+1) << endl;
	       if j<n-1 then step = c^(s#j)*step;
	       ));
     multistepHash#(c,s) = apply(n, i->(
	       -- lagrange poly
	       T := symbol T;
	       RT := R[T];
	       allBUTi := toList(0..i-1) | toList(i+1..n-1);
	       L := sub((1/product(allBUTi, j->t_i-t_j))*product(allBUTi, j->T-t_j),RT);
	       -- << "i = " << i << "  L = " << L << endl;
	       integratePoly(L,t_(n-1),t_n)
	       ))
     )

multistepHashLooseEnd = new MutableHashTable;
multistepPredictorLooseEnd = method(TypicalValue => List)
multistepPredictorLooseEnd (QQ,List) := List => (c,s) -> (
-- coefficients for a multistep predictor with intederminate last step
-- IN:  c = step adjustment coefficient (in QQ)
--      s = list of step adjustments (from the initial stepsize h = t_1-t_0)
-- OUT: b = list of polinomials in QQ[a], where a=(last step size)/(next to last stepsize)   
     if multistepHashLooseEnd#?(c,s) then return multistepHashLooseEnd#(c,s);
     t := symbol t;
     n := #s + 2; -- t_n is the one for which prediction is being made
     a := symbol a;
     R := QQ[a];         
     step := 1_R; -- h; WLOG, assume h=1 
     t_0 = 0_R;
     scan(n, j->(
	       t_(j+1) = t_j+step;
	       if DBG>3 then << "t_("<< j+1 <<") = " << t_(j+1) << endl;
	       step = if j<n-2 then c^(s#j)*step 
	       else if j==n-2 then a*step;
	       ));
     multistepHashLooseEnd#(c,s) = apply(n, i->(
	       -- lagrange poly
	       T := symbol T;
	       RT := R[T];
	       allBUTi := toList(0..i-1) | toList(i+1..n-1);
	       L := sub((1/product(allBUTi, j->lift(t_i-t_j,QQ)))*product(allBUTi, j->T-t_j),RT);
	       -- << "i = " << i << "  L = " << L << endl;
	       integratePoly(L,t_(n-1),t_n)
	       ))
     )
------------------------------------------------------
normalize = method(TypicalValue => Matrix)
normalize Matrix := v->(
-- normalizes a column vector with CC entries
     (1/sqrt(sum(flatten entries v, x->x*conjugate x)))*v
     )
------------------------------------------------------
track = method(TypicalValue => List, Options =>{
	  Software=>M2,
	  gamma=>1, 
	  tDegree=>1,
     	  -- step control
	  tStep => 0.05, -- initial
          tStepMin => 1e-2,
	  stepIncreaseFactor => 2_QQ,
	  numberSuccessesBeforeIncrease => 4,
	  -- predictor 
	  Predictor=>RungeKutta4, 
	  MultistepDegree => 3, -- used only for Predictor=>Multistep
	  -- corrector 
	  finalMaxCorSteps => 11,
	  maxCorSteps => 4,
	  -- projectivization
	  Projectivize => true, 
	  AffinePatches => DynamicPatch,
	  RandomSeed => 0 } )
track (List,List,List) := List => o -> (S,T,solsS) -> (
-- tracks solutions from start system to target system
-- IN:  S = list of polynomials in start system
--      T = list of polynomials in target system
--      solsS = list of solutions to S
-- 	gamma => nonzero complex number
-- OUT: solsT = list of target solutions corresponding to solsS
     n := #T; 
     if n > 0 then R := ring first T else error "expected nonempty target system";
     if #S != n then 
     error "expected same number of polynomials in start and target systems";
     if any(S, f->ring f != R) or any(T, f->ring f != R)
     then error "expected all polynomials in the same ring";
     if o.tStep <= 0 then "expected positive tStep";  

     -- PHCpack -------------------------------------------------------
     if o.Software == PHCpack then ( 
	  targetfile := -- temporaryFileName() | 
	             "PHCtarget";
	  startfile := -- temporaryFileName() | 
	             "PHCstart";
	  outfile := -- temporaryFileName() | 
	             "PHCoutput";
	  solsSfile := -- temporaryFileName() | 
	             "PHCstartsols";
	  solsTfile := -- temporaryFileName() | 
	             "PHCtargetsols";
	  batchfile := -- temporaryFileName() | 
	             "PHCbat";
	  -- writing data to the corresponding files                                                                                                                                                                           
     	  if n < numgens R then error "the system is underdetermined";
	  if n > numgens R then (
	       nSlacks := n - numgens R;
	       slackVars := apply(nSlacks, i->getSymbol("S"|toString i));
	       newR := CC[gens R, slackVars];
	       rM := random(CC^n,CC^nSlacks);
	       S = apply(#S, i->sub(S#i,newR)+(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0));
	       rM = random(CC^n,CC^nSlacks);
	       T = apply(#T, i->sub(T#i,newR)+(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0));
	       solsS = apply(solsS, s->s|toList(nSlacks:0_CC)); 
	       ) else newR = R;
	  systemToFile(T,targetfile);
	  systemToFile(S,startfile);
     	  solutionsToFile(solsS,newR,solsSfile);	  
	  -- making batch file
	  bat := openOut batchfile;
	    bat << targetfile << endl << outfile << endl <<"n"<< endl 
	    << startfile << endl << solsSfile << endl;
	    -- first menu
	    bat << "k" << endl << o.tDegree << endl; 
	    bat << "a" << endl << realPart o.gamma << endl << imaginaryPart o.gamma << endl;
	    bat << "0" << endl;
	    -- second menu 
	    bat << "0" << endl; -- exit for now
   	    -- third menu
	    bat << "0" << endl; -- exit for now
   	    -- fourth menu
	    bat << "0" << endl; -- exit for now
	  close bat;
	  run(PHCexe|" -p <"|batchfile|" >null");
  	  run(PHCexe|" -z "|outfile|" "|solsTfile);
	  -- parse and output the solutions                                                                                                                                                                                    
	  result := parseSolutions(solsTfile, newR);
	  if n > numgens R then (
	       result = apply(result, s->(
			 if any(drop(first s, numgens R), x->abs x > 0.01) 
			 then error "slack value is nonzero";
			 {take(first s, numgens R)}|drop(s,1)
			 ));
     	       totalN := #result;
	       scan(result, s->(
			 if s#1#"mult">1 then error "mutiple root encountered";
			 if s#1#"mult"<0 then error "negative mutiplicity";
     	       		 ));			 
	       result = select(result, 
		    s->--s#1#"mult">0 and 
		    max(s#0/abs)<10000 -- path failed and/or diverged
		    );
	       if DBG>0 and #result < totalN 
	       then  -- error "discarded!" 
	          << "track[PHCpack]: discarded "<< 
	          totalN-#result << " out of " << totalN << " solutions" << endl;
	       );
	  -- clean up                                                                                                                                                                                                          
	  if DBG<10 then {targetfile, startfile, outfile,
	       solsSfile, solsTfile, batchfile } / removeFile ;
	  return result;
	  ) 
     else if o.Software == Bertini then (
	  -- tempdir := temporaryFileName() | "NAG-bertini";
	  -- mkdir tempdir; 	  
  	  makeBertiniInput(gens R, T, StartSystem=>S, StartSolutions=>solsS, gamma=>o.gamma);
  	  run(BERTINIexe);
	  result = readSolutionsBertini("raw_solutions");
	  -- remove Bertini input/output files
    	  for f in {"failed_paths", "nonsingular_solutions",
               "raw_data", "start", "input", "output", "raw_solutions",
               "main_data", "real_finite_solutions", "finite_solutions",
               "midpath_data", "singular_solutions", "real_solutions",
               "singular_solutions", "midpath_data"} do
          if fileExists f then removeFile f;
	  return result;
	  );
     
     -- M2 (main code)  --------------------------------------------------------
     if n != numgens R then error "expected a square system";
           
     solsS = solsS / (s->sub(transpose matrix {toList s}, CC)); -- convert to vectors
     
     if o.Projectivize then (
	  h := symbol h;
	  R = CC[gens R | {h}]; 
	  n = numgens R;
	  T = apply(T, f->homogenize(sub(f,R), h)); 
	  S = apply(S, f->homogenize(sub(f,R), h));
     	  solsS = solsS / (s->s||matrix{{toCC 1}});

     	  -- affine patch functions 
     	  pointToPatch := (x0,p)-> (1/(p*x0)_(0,0))*x0; -- representative for point x0 in patch p
	  patchEquation := p -> p * transpose vars R - 1;

 	  if o.AffinePatches === DynamicPatch then (
	       solsS = solsS/normalize;
	       dPatch := true; -- not null        
	       )
	  else (   
	       dPatch = null;
	       patches := { 	    
	       	    promote(matrix{{append((n-1):0, 1)}},CC), -- dehomogenizing patch
	       	    if #o.AffinePatches > 0 then first o.AffinePatches -- either provided patch...
	       	    else ( 
		    	 setRandomSeed o.RandomSeed; 
		    	 matrix{apply(n, i->exp(random(0.,2*pi)*ii))} ) -- ... or random patch
	       	    };
	       patches = patches | { o.gamma*patches#1 };
     	       if DBG>1 then << "affine patch: " << toString patches#1 <<endl;
	       T = T | {patchEquation patches#1};
	       S = S | {patchEquation patches#2};
	       solsS = solsS / (s->pointToPatch(s, patches#2));
	       );
	  ); 
     
     -- create homotopy
     t := symbol t;
     Rt := CC[gens R, t]; -- how do you cleanly extend the generators list: e.g., what if "n" is a var name?
     H := matrix {apply(#S, i->(1-t)^(o.tDegree)*sub(S#i,Rt)+o.gamma*t^(o.tDegree)*sub(T#i,Rt))};
     JH := transpose jacobian H; 
     Hx := JH_(toList(0..n-1));
     Ht := JH_{n};
     
     -- evaluation functions
     evalH := (x0,t0)-> ( r := lift(sub(transpose H, transpose x0 | matrix {{t0}}), CC);
	  if dPatch === null then r
	  else r || matrix{{(dPatch*x0)_(0,0)-1}} -- patch equation evaluated  
	  );
     evalHx := (x0,t0)-> ( r := lift(sub(Hx, transpose x0 | matrix {{t0}}), CC);
	  if dPatch === null then r
	  else r || matrix { flatten entries dPatch }
	  );  
     evalHt := (x0,t0)-> ( r:= lift(sub(Ht, transpose x0 | matrix {{t0}}), CC);
	  if dPatch === null then r
	  else r || matrix {{0_CC}}
	  );
     evalMinusInverseHxHt := (x0,t0)-> -(inverse evalHx(x0,t0))*evalHt(x0,t0);
          
     
     -- threshholds and other tuning parameters (should include most of them as options)
     epsilon := 1e-5; -- tracking tolerance (universal)
     divThresh := 1e4; 
     condNumberThresh := 1e3;
     stepDecreaseFactor := 1/o.stepIncreaseFactor;
     theSmallestNumber := 1e-12;

      
     rawSols := apply(solsS, s->(
	       if DBG > 1 then << "tracking solution " << toString s << endl;
     	       tStep := o.tStep;
	       predictorSuccesses := 0;
	       x0 := s; 
	       t0 := toCC 0.; 
	       count := 1; -- number of computed points
	       stepAdj := 0; -- step adjustment number (wrt previous step): 
	                     -- newstep = oldstep * stepIncreaseFactor^stepAdj  
	       history := new MutableHashTable from{ count => new MutableHashTable from {
			 "t"=>t0,"x"=>x0
			 } };
	       while x0 =!= infinity and 1-t0 > theSmallestNumber do (
		    if DBG > 4 then << "current t = " << t0 << endl;
                    -- monitor numerical stability: perhaps change patches if not stable ???
		    -- Hx0 := evalHx(x0,t0);
		    -- svd := sort first SVD Hx0;
		    -- if o.Projectivize and first svd / last svd > condNumberThresh then ( 
     	       	    --	 << "CONDITION NUMBER = " <<  first svd / last svd << endl;			 
		    --	 );

		    -- predictor step
		    local dx; local dt;
     	       	    if dPatch =!= null 
		    then dPatch = -- conjugate of the normalized kernel vector 
		    matrix{ flatten entries normalize transpose gens ker lift(sub(Hx, transpose x0 | matrix {{t0}}), CC) / conjugate };   
		    --matrix { flatten entries transpose x0 / conjugate };
		    	 
		    if o.Predictor == Tangent then (
		    	 Hx0 := evalHx(x0,t0);
			 Ht0 := evalHt(x0,t0);
	     	    	 invHx0 := inverse Hx0;
		    	 --invHx0timesHt0 := invHx0*Ht0;
		    	 -- if norm invHx0timesHt0 > divThresh then (x0 = infinity; break);
		    	 tStepSafe := 1; -- 1/(norm(Hx0)*norm(invHx0)); -- what is a good heuristic?
		    	 dt = min(min(tStep,tStepSafe), 1-t0);
		         --dx = -dt*invHx0timesHt0;
			 dx = solve(Hx0,-dt*Ht0);
			 ) 
		    else if o.Predictor == Euler then (
			 H0 := evalH(x0,t0);
			 Hx0 = evalHx(x0,t0);
			 Ht0 = evalHt(x0,t0);
	     	    	 invHx0 = inverse Hx0;
			 dt = min(tStep, 1-t0);
			 dx = -invHx0*(H0+Ht0*dt);
			 )
		    else if o.Predictor == Secant then (
			 dt = min(tStep, 1-t0);
			 if count > 1 then ( -- if there is a preceding point
			      u := x0 - history#(count-1)#"x";			      
			      dx = dt*normalize u;
     			      )			      
			 else ( -- use tangential predictor
			      dx = dt*evalMinusInverseHxHt(x0,t0);
			      );
			 )
		    else if o.Predictor == RungeKutta4 then (
			 dt = min(tStep, 1-t0);
			 k1 := dt*evalMinusInverseHxHt(x0,t0);
			 k2 := dt*evalMinusInverseHxHt(x0+.5*k1,t0+.5*dt);
			 k3 := dt*evalMinusInverseHxHt(x0+.5*k2,t0+.5*dt);
			 k4 := dt*evalMinusInverseHxHt(x0+k3,t0+dt);
			 dx = (1/6)*(k1+2*k2+2*k3+k4);
			 )
		    else if o.Predictor == Multistep then (
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
			 
			 MScoeffs := 
			 if tStep>1-t0 and nPoints > 1 then (
			      aMScoeffs := multistepPredictorLooseEnd(o.stepIncreaseFactor,drop(stepAdjSequence,-1));
			      aRing := ring first aMScoeffs;
			      aMap := map(CC,aRing, matrix{{dt/(history#count#"t"-history#(count-1)#"t")}});
			      aMScoeffs/aMap  
			      )
			 else multistepPredictor(o.stepIncreaseFactor,stepAdjSequence); 
     	       	    	 
                	 -- dx = dt*sum_{i=0..nPoints-1} MScoeff_i*rhsODE(t_i)
			 dx = delta*sum(nPoints, i->toCC(MScoeffs)#i*history#(count-nPoints+1+i)#"rhsODE");
			 if DBG > 3 then << "delta = " << delta << "   MScoeffs = " << MScoeffs << endl;
			 )
		    else error "unknown Predictor";
		    
		    if DBG > 3 then << "  dt = " << dt << "  dx = " << toString dx << endl;
		    		    
    	 	    t1 := t0 + dt;
		    x1 := x0 + dx;
		    
		    -- corrector step
		    dx = 1; -- dx = + infinity
		    nCorSteps := 0;
		    while norm dx > epsilon 
		          and nCorSteps < (if 1-t1 < theSmallestNumber and dt < o.tStepMin 
			                   then o.finalMaxCorSteps -- infinity
			                   else o.maxCorSteps) 
		    do ( 
			 if norm x1 > divThresh then (x1 = infinity; break);
			 if DBG > 4 then << "x=" << toString x1 << " res=" <<  toString evalH(x1,t1) << " dx=" << dx << endl;
			 dx = - (inverse evalHx(x1,t1))*evalH(x1,t1);
			 x1 = x1 + dx;
			 nCorSteps = nCorSteps + 1;
			 );
		    if dt > o.tStepMin and nCorSteps == o.maxCorSteps then ( -- predictor failure 
			 predictorSuccesses = 0;
			 stepAdj = stepAdj - 1;
	 	 	 tStep = stepDecreaseFactor*tStep;
			 if DBG > 2 then << "decreased tStep to "<< tStep << endl;	 
			 ) 
		    else ( -- predictor success
			 predictorSuccesses = predictorSuccesses + 1;
		         x0 = x1;
			 if dPatch =!= null then x0 = normalize x0;
		         t0 = t1;
			 count = count + 1;
		         history#count = new MutableHashTable from {"t"=>t0,"x"=>x0,"stepAdj"=>stepAdj};
			 if nCorSteps <= o.maxCorSteps - 1 -- over 2 / minus 2 ???
              		    and predictorSuccesses >= o.numberSuccessesBeforeIncrease 
			 then (			      
			      predictorSuccesses = 0;
			      stepAdj = 1;
			      tStep = o.stepIncreaseFactor*tStep;	
			      if DBG > 2 then << "increased tStep to "<< tStep << endl;
			      )
			 else stepAdj = 0; -- keep the same step size
			 );
		    );        	    
	       (x0,symbol nSteps=>count)
	       ));
     
     if o.Projectivize then (
	  if DBG>3 then print rawSols;
	  return select(
	       apply(select(rawSols,s -> first s =!= infinity), s->(
		    	 s' = flatten entries first s;
		    	 --if norm(last s) < epsilon then infinity 
		    	 --else 
			 (apply(drop(s',-1),u->(1/last s')*u),s#1)
			 )
	       	    ),
	       s->s=!=infinity);
	  )
     else (
	  return select(rawSols,s -> first s =!= infinity)/(s->(flatten entries first s, s#1));
	  );
     )

refine = method(TypicalValue => List, Options =>{epsilon=>1e-8, maxCorSteps=>30})
refine (List,List) := List => o -> (T,solsT) -> (
-- tracks solutions from start system to target system
-- IN:  T = list of polynomials in target system
--      solsT = list of solutions to T
-- OUT: solsR = list of refined solutions 
     n := #T; 
     if n > 0 then R := ring first T else error "expected nonempty target system";
     if n != numgens R then error "expected a square system";
          
     T = matrix {T};
     J = transpose jacobian T; 
     evalT := x0 -> lift(sub(transpose T, transpose x0), CC);
     evalJ := x0 -> lift(sub(J, transpose x0), CC);
     
     solsR := apply(solsT, s->(
	       x1 := sub(transpose matrix {first s}, CC); -- convert to vector 
	       -- corrector step
	       dx = 1; -- dx = + infinity
	       nCorSteps := 0;
	       while norm dx > o.epsilon and nCorSteps < o.maxCorSteps do ( 
		    if DBG > 3 then << "x=" << toString x1 << " res=" <<  toString evalT(x1) << " dx=" << dx << endl;
		    dx = - (inverse evalJ(x1))*evalT(x1);
		    x1 = x1 + dx;
		    nCorSteps = nCorSteps + 1;
		    );
	       x1
	       ));
     apply(#solsT, i-> {flatten entries solsR#i, last solsT#i})      
     )     

totalDegreeStartSystem = method(TypicalValue => Sequence)
totalDegreeStartSystem List := Sequence => T -> (
-- contructs a total degree start system and its solutions 
-- for the given target system T
-- IN:  T = list of polynomials 
-- OUT: (S,solsS}, where 
--      S     = list of polynomials, 
--      solsS = list of sequences
     R := ring first T;
     S := apply(numgens R, i->R_i^(first degree T_i)-1);
     s := apply(numgens R, i->( 
	  d = first degree T_i; 
	  set apply(d, j->exp(ii*2*pi*j/d))
	  ));
     solsS := first s;
     scan(drop(s,1), t->solsS=solsS**t);
     solsS = toList solsS/deepSplice; 
     (S, solsS)
     )     

-- INTERFACE part ------------------------------------
solveSystem = method(TypicalValue => List, Options =>{Software=>M2})
solveSystem List := List => o -> F -> (
-- solves a system of polynomial equations
-- IN:  F = list of polynomials
--      Software => {PHCpack, Bertini, hom4ps2}
-- OUT: {s,m}, where 
--             s = list of solutions 
--     	       m = list of corresponding multiplicities	 
     local result;
     R := ring F#0;
     v := flatten entries vars R;
     if o.Software == M2 then ( 
	  result = 
	  if all(F, f -> first degree f <= 1)
     	  then ( 
	       A := matrix apply(F, f->apply(v, x->coefficient(x,f)));
	       b := matrix apply(F, f->{coefficient(1_R,f)});
	       {{flatten entries solve(A,b)}}
	       )
	  else (
	       (S,solsS) := totalDegreeStartSystem F;
	       track(S,F,solsS,gamma=>exp(random(0.,2*pi)*ii))
	       )
	  )
     else if o.Software == PHCpack then ( 
	  -- assume the ideal is 0-dim. 
	  -- !!! problem with temporaryFileName: cygwin's /tmp is different from Windows' /tmp 
	  tarsolfile := -- temporaryFileName() | 
	                "PHCtasols";
	  targetfile := -- temporaryFileName() | 
	             "PHCtarget";
	  outfile := -- temporaryFileName() | 
	             "PHCoutput";
	  -- writing data to the corresponding files                                                                                                                                                                           
	  systemToFile(F,targetfile);
	  -- launching blackbox solver; converting the solutions to the Maple format                                                                                                                                           
	  run(PHCexe|" -b "|targetfile|" "|outfile);
	  run(PHCexe|" -z "|targetfile|" "|tarsolfile);
	  -- parse and output the solutions                                                                                                                                                                                    
	  result = parseSolutions(tarsolfile, R);
	  -- clean up                                                                                                                                                                                                          
	  if DBG<10 then (
	       removeFile targetfile;
	       removeFile tarsolfile; 
	       removeFile outfile;
	       )
	  )
     else if o.Software == Bertini then (
	  -- tempdir := temporaryFileName() | "NAG-bertini";
	  -- mkdir tempdir; 	  
  	  makeBertiniInput(gens R, F);
  	  run(BERTINIexe);
	  sols := readSolutionsBertini("finite_solutions");
	  result = sols;
	  -- remove Bertini input/output files
    	  for f in {"failed_paths", "nonsingular_solutions",
               "raw_data", "start", "input", "output", "raw_solutions",
               "main_data", "real_finite_solutions", "finite_solutions",
               "midpath_data", "singular_solutions", "real_solutions",
               "singular_solutions", "midpath_data"} do
          if fileExists f then removeFile f;
	  )
     else if o.Software == hom4ps2 then (
	  -- newR := coefficientRing R[xx_1..xx_(numgens R)];
	  (name, p) := makeHom4psInput(R, F);
	  targetfile = name; --(map(newR, R, gens newR)\F);
	  tarsolfile = temporaryFileName() | 
	                "tasols";
 	  run(HOM4PS2exe|" "|targetfile|" "|tarsolfile);
	  sols = readSolutionsHom4ps(tarsolfile, p);
	  result = sols;
	  if DBG<10 then (
	       removeFile targetfile;
	       removeFile tarsolfile; 
	       )
	  )
     else error "invalid Software option";  		
     result
     )

-- PHC part ------------------------------------------
systemToFile = method(TypicalValue => Nothing)
systemToFile (List,String) := (F,name) -> (
     file := openOut name;
     file << #F << endl;
     scan(F, f->( 
     	       L := toExternalString f;
     	       L = replace("ii", "I", L);
     	       L = replace("e", "E", L);
	       L = replace("p53","",L);
	       file << L << ";" << endl;
	       ));
     close file;
     ) 

solutionsToFile = method(TypicalValue => Nothing)
solutionsToFile (List,Ring,String) := (S,R,name) -> (
     file := openOut name;
     file << #S << " " << numgens R << endl << 
     "===========================================================" << endl;
     scan(#S, i->( 
     	       file << "solution " << i << " :" << endl <<
	       "t :  0.00000000000000E+00   0.00000000000000E+00" << endl <<
	       "m :  1" << endl <<
	       "the solution for t :" << endl;
	       scan(numgens R, v->(
      	       		 L := " "|toString R_v|" :  "|
			 format(0,-1,9,9,realPart toCC S#i#v)|"  "|
			 format(0,-1,9,9,imaginaryPart toCC S#i#v);
			 file << L << endl; 
			 ));
	       file <<  "== err :  0 = rco :  1 = res :  0 ==" << endl;
	       ));
     close file;
     ) 
///
restart
loadPackage "NAG"; debug NAG;
R = CC[x,y,z]
solutionsToFile( {(0,0,1),(0,0,-1)}, R, "PHCsols" )
///

parseSolutions = method(TypicalValue => Sequence)
parseSolutions (String,Ring) := (s,R) -> (
-- parses solutions in PHCpack format 
-- IN:  s = string of solutions in PHCmaple format 
--      V = list of variable names
-- OUT: {list of solutions, list of multiplicities}
     L := get s;
     L = replace("=", "=>", L);
     L = replace("I", "ii", L);
     L = replace("E", "e", L);
     L = replace("e\\+","e",L);
     L = replace("time", "\"time\"", L);
     L = replace("multiplicity", "\"mult\"", L);
     
     use R; 	  
     sols := toList apply(value L, x->new HashTable from toList x);
     sols/(x->{apply(gens R, v->x#v), x})
     )

-- HOM4PS2 part -----------------------------------------------------------

makeHom4psInput = method(TypicalValue=>Sequence)
makeHom4psInput (Ring, List) := (R, T) -> (
-- IN:  R = ring
--      T = polynomials of target system (in R)
-- OUT: (name, perm), where
--      name = input filename   
--      perm = hashtable of order of appearences of variables in the input
  filename := temporaryFileName() | "input"; 
  s := "{\n";
  scan(T, p -> s = s | toString p | ";\n");
  s = s | "}\n";
  f := openOut filename; 
  f << s;
  close f;
  -- assume names of vars are not substrings of each other
  p := sort apply(numgens R, i->(first first regex(toString R_i, s), i));
  ( filename, new HashTable from apply(#p, i->p#i#1=>i) )
  )

readSolutionsHom4ps = method(TypicalValue=>List)
readSolutionsHom4ps (String, HashTable) := (f,p) -> (
-- IN:  f = output filename
--      p = permutation of coordinates to be applied (hashtable)
-- OUT: list of solutions
  s := {};
  l := lines get f;
  i := 0; -- line counter
  while #l#i > 2 do ( -- current line is non-empty	   
       coords := {};
       while #l#i > 2 do ( -- until an empty line
	    a := select(separate(" ",  cleanupOutput(l#i)), t->#t>0);
	    coords = coords | {(value a#0)+ii*(value a#1)};
	    i = i + 1;
       	    );
       if DBG>=10 then << coords << endl;
       s = s | { apply(#coords, i->coords#(p#i)) };
       i = i + 4; -- skip to the next solution
       );
  s
  )
  
makeBertiniInput = method(TypicalValue=>Nothing, Options=>{StartSystem=>{},StartSolutions=>{},gamma=>1.0+ii})
makeBertiniInput (List, List) := o -> (v,T) -> (
-- IN:
--	v = variables
--	T = polynomials of target system
--      o.StartSystem = start system
  f := openOut "input"; -- THE name for Bertini's input file 
  f << "CONFIG" << endl;
  f << "MPTYPE: 2;" << endl; -- multiprecision
  if #o.StartSystem > 0 then
    f << "USERHOMOTOPY: 1;" << endl;
  f << endl << "END;" << endl << endl;
  f << "INPUT" << endl << endl;
  if #o.StartSystem > 0 then
    f << "variable "
  else f << "variable_group "; -- variable section
  scan(#v, i->
       if i<#v-1 
       then f << toString v#i << ", "
       else f << toString v#i << ";" << endl
       );
  f << "function "; -- "function" section
  scan(#T, i->
       if i<#T-1
       then f << "f" << i << ", "
       else f << "f" << i << ";" << endl << endl
      );
  bertiniNumbers := p->( L := toString p; 
       L = replace("ii", "I", L); 
       L = replace("e", "E", L);
       L
       );
  if #o.StartSystem == 0 
  then scan(#T, i -> f << "f" << i << " = " << bertiniNumbers T#i << ";" << endl)
  else (
       if #o.StartSystem != #T then error "expected equal number of equations in start and target systems";
       f << "pathvariable t;" << endl 
         << "parameter s;" << endl
         << "s = t;" << endl;
       scan(#T, i -> f << "f" << i 
	    << " = (" << bertiniNumbers T#i << ")*(1-s)+s*("<< bertiniNumbers o.gamma << ")*(" << bertiniNumbers o.StartSystem#i << ");" << endl 
	   );
       );
  f << endl << "END" << endl << endl;
  close f;
  
  if #o.StartSolutions > 0 then (
       f = openOut "start"; -- THE name for Bertini's start solutions file 
       f << #o.StartSolutions << endl << endl;
       scan(o.StartSolutions, s->(
		 scan(s, c-> f << realPart c << " " << imaginaryPart c << ";" << endl );
		 f << endl;
		 ));
       close f;
       );
  )

cleanupOutput = method(TypicalValue=>String)
cleanupOutput String := s -> (
-- cleanup output (Bertini and hom4ps2)
  t := replace("E", "e", s);
  t = replace("[(,)]","", t);
  t = replace("e\\+","e",t)
  )

readSolutionsBertini = method(TypicalValue=>List)
readSolutionsBertini String := f -> (
  s := {};
  if f == "finite_solutions" then (
       print "implementation unstable: Bertini output format uncertain";
       l := lines get f;
       nsols := value first separate(" ", l#0);
       l = drop(l,2);
       while #s < nsols do (	 
	    coords := {};
	    while #(first l) > 2 do ( 
	      	 coords = coords | {(
		   	   a := separate(" ",  cleanupOutput(first l));	 
		   	   (value a#0)+ii*(value a#1)
	      	   	   )};
    	      	 l = drop(l,1);
	      	 );	
	    l = drop(l,1);
            if DBG>=10 then << coords << endl;
	    s = s | {{coords}};
	    );	
       ) 
  else if f == "raw_solutions" then (
       l = lines get f;
       while #l>0 and #separate(" ", l#0) < 2 do l = drop(l,1);
       while #l>0 do (
	    if DBG>=10 then << "------------------------------" << end;
	    coords = {};
	    while #l>0 and #separate(" ", l#0) >= 2 do ( 
	      	 coords = coords | {(
		   	   a = separate(" ",  cleanupOutput(first l));	 
		   	   (value a#0)+ii*(value a#1)
	      	   	   )};
    	      	 l = drop(l,1);
	      	 );
	    while #l>0 and #separate(" ", l#0) < 2 do l = drop(l,1);
            if DBG>=10 then << coords << endl;
	    s = s | {{coords}};
	    );     
    ) else error "unknow output file";
  s
  )

-----------------------------------------------------------------------
-- WITNESS SET
-- {equations, slice, points}
-- caveat: we assume that #equations = dim(slice)   

sliceEquations = method(TypicalValue=>List) 
sliceEquations (Matrix, Ring) := (S,R) -> (
-- make slicing plane equations 
     apply(numgens target S, i->(sub(S^{i},R) * transpose(vars R | matrix{{1_R}}))_(0,0)) 
     )

moveSlice = method(TypicalValue=>HashTable, Options =>{Software=>M2})
moveSlice (HashTable, Matrix) := HashTable => o -> (W,S) -> (
-- IN:  W = witness set
--      S = matrix defining a new slicing plane (same dimensions as W#slice)
-- OUT: new witness set that uses S
     if numgens target S != numgens target W#slice 
     or numgens source S != numgens source W#slice 
     then error "wrong dimension of new slicing plane";
     R := ring first W#equations;
     st := W#equations | sliceEquations(W#slice,R);
     ta := W#equations | sliceEquations(S,R);
     newpoints := track(st,ta,W#points,Software=>o.Software,gamma=>.6+.8*ii) / first;
     if #newpoints != #W#points then error "number of points in the witness set changed";
     new HashTable from {
	  equations => W#equations, 
	  slice => S,
	  points => newpoints
	  }              	  
     )
///
restart
loadPackage ("NAG", Configuration => { "PHCpack" => "./phc" }); debug NAG;
R = CC[x,y,z]
W1 = new HashTable from {
     equations=>{x^2+y^2+z^2-1},
     slice=>sub(matrix "1,0,0,0;0,1,0,0",R),
     points=>{(0,0,1),(0,0,-1)}
     } 
sliceEquations (W1#slice,R)
W2 = moveSlice(W1, sub(matrix "0,1,0,0;0,0,1,0",R)
     --, Software=>PHCpack
     )
///

splitWitness = method(TypicalValue=>Sequence, Options =>{Software=>M2, epsilon=>1e-6})
splitWitness (HashTable,RingElement) := Sequence => o -> (w,f) -> (
-- splits the witness set into two parts: one contained in {f=0}, the other not
-- IN:  comp = a witness set
--      f = a polynomial
-- OUT: (w1,w2) = two witness sets   
     w1 := {}; w2 := {};
     for x in w#points do 
	 if norm evalPoly(f,x) < o.epsilon 
	 then w1 = w1 | {x}
	 else w2 = w2 | {x};   
     ( if #w1===0 then null 
	  else new HashTable from {equations=>w#equations, slice=>w#slice, points=>w1}, 
       if #w2===0 then null 
          else new HashTable from {equations=>w#equations, slice=>w#slice, points=>w2} )
     )

regeneration = method(TypicalValue=>List, Options =>{Software=>M2})
regeneration List := HashTable => o -> F -> (
-- solves a system of polynomial equations by generation     
-- IN:  F = list of polynomials
--      Software => {PHCpack, Bertini, hom4ps2}
-- OUT: {s,m}, where 
--             s = list of solutions 
--     	       m = list of corresponding multiplicities	 
     R := ring F#0;
     c1 := {}; -- current solution components
     for f in F do (
	  d := first degree f;
	  c2 := {}; -- new components
	  for comp in c1 do (
	       print (#comp#points);
	       (cIn,cOut) := splitWitness(comp,f); 
	       if cIn =!= null 
	       then c2 = c2 | { new HashTable from {
			 equations => cIn#equations | {f}, 
			 slice => cIn#slice,
			 points => cIn#points  
			 } }; 
     	       if cOut =!= null then (
		    dWS = apply(d, i->(
			      s := cOut#slice;
			      newSlice := random(CC^1, CC^(numgens source s)) || submatrix'(s,{0},{}); 
			      moveSlice(cOut,newSlice,Software=>o.Software)
			      ));
	       	    S := comp#equations 
	       	    | { product flatten apply( dWS, w->sliceEquations(w#slice^{0},R) ) } -- product of linear factors
	       	    | sliceEquations( submatrix'(comp#slice,{0},{}), R );
	       	    T := comp#equations
	       	    | {f}
	       	    | sliceEquations( submatrix'(comp#slice,{0},{}), R );
	       	    targetPoints := track(S,T,flatten apply(dWS,w->w#points), 
			 gamma=>exp(random(0.,2*pi)*ii), Software=>o.Software);
		    --if o.Software == M2 then targetPoints = refine(T, targetPoints, epsilon=>1e-10);
		    if #targetPoints>0 
		    then c2 = c2 | { new HashTable from {
			      equations => cOut#equations | {f}, 
			      slice => submatrix'(comp#slice,{0},{}),
			      points => targetPoints/first  
			      } };
		    ); 
	       );
	  c1 = c2;
	  if #c1 === 0 then ( -- if the first equation is being processed 
	       rM := random(CC^(numgens R-1), CC^(numgens R + 1));
     	       c1 = { new HashTable from { 
	       		 equations => {f},
	       		 slice => rM,
	       		 points => solveSystem( {f} | sliceEquations(rM,R), Software=>o.Software ) / first 
	       		 } }; 
	       );
	  );
     c1	  
     )
///
restart
loadPackage ("NAG", Configuration => { "PHCpack" => "./phc", "Bertini" => "./bertini" }); debug NAG;
R = CC[x,y,z]
regeneration({x^2+y^2+z^2-1,x,y*z} 
     ,Software=>PHCpack
     --,Software=>Bertini
     )
regeneration({x^2+y^2+z^2-1,x*y,y*z} 
     --,Software=>PHCpack
     --,Software=>Bertini
     )
///
-----------------------------------------------------------------------
-- AUXILIARY FUNCTIONS

areEqual = method(TypicalValue=>Boolean, Options=>{epsilon=>1e-6})
areEqual (List,List) := o -> (a,b) -> (
     #a == #b and norm matrix {a-b} < o.epsilon
     ) 
areEqual (CC,CC) := o -> (a,b) -> (
     abs(a-b) < o.epsilon
     ) 
sortSolutions = method(TypicalValue=>List, Options=>{epsilon=>1e-6})
sortSolutions List := o -> sols -> (
-- sorts numerical solutions     
     if #sols == 0 then sols
     else (
     	  n := # first first sols; 
     	  sorted := {};
	  scan(sols, s->(
		    -- find the first element that is "larger";
		    -- "larger" means the first coord that is not (approx.) equal 
		    -- has (significantly) larger realPart, if tie then larger imaginaryPart
		    l := position(sorted, t->(
			      for i from 0 to n-1 do ( 
				   if not areEqual((first t)#i,(first s)#i, epsilon=>o.epsilon) 
				   then 
				   if abs(realPart (first t)#i - realPart (first s)#i) < o.epsilon then 
			      	   return imaginaryPart (first t)#i > imaginaryPart (first s)#i
				   else return realPart (first t)#i > realPart (first s)#i
				   ); 
			      true -- if approx. equal 
			      ));
		    
		    if l === null then sorted = sorted | {s}
		    else if not areEqual(first sorted#l, first s, epsilon=>o.epsilon) then sorted = take(sorted,l) | {s} | drop(sorted,l);
		    ));      
	  );
     	  sorted
     )

evalPoly = method(TypicalValue=>CC)
evalPoly (RingElement, List) := (f,x) -> (
     sub(sub(f, sub(matrix{x},ring f)), coefficientRing ring f)
     )

------------ SLPs ------------------------------------------------------------------------
-- SLP = (constants, list of integers)
-- constants = list of elements in CC
-- list of integers = number of inputs, number of outputs, node, node, ...
-- node = binary_operation, a, b
--     or multi_operation, n, a1, ... , an        
--     or copy, a                                 -- copies node n    
-- binary_operation = {sum, product}
-- multi_operation = {msum, mproduct} 
--
-- evaluation of SLP assumes 
--       first #constants nodes to be constants 
--       the next (number of inputs) to be inputs
--       the last (number of output) to be outputs

slpCOPY = 1; -- "COPY"; -- node positions for slpCOPY commands are absolute
slpMULTIsum = 2; -- "MULTIsum";
slpPRODUCT = 3; -- "PRODUCT";
shiftConstsSLP = method(TypicalValue=>List);
shiftConstsSLP (List,ZZ) := (slp,shift) -> apply(slp, 
     n->apply(n, b->
	  if class b === Option and b#0 === "const" 
     	  then "const"=>shift+b#1 
     	  else b
	  )
     );

poly2slp = method(TypicalValue=>Sequence)
poly2slp RingElement :=  g -> (
     prog := {}; -- our SLP
     R := ring g;
     const := coefficient(1_R,g);
     finalMULTIsum := {}; -- list of nodes for final multisum
     constants := if const == 0 then {} else ( finalMULTIsum = finalMULTIsum | {"const"=>0}; {const} );
     f := g - const;
     scan(numgens R, i->(
	       fnox := f%R_i;
	       fx := f - fnox;
	       if fx != 0 then (
	       	    (constfx, progfx) := poly2slp (fx//R_i);
	       	    -- shift constant nodes positions
	       	    prog = prog | shiftConstsSLP(progfx, #constants); 
	       	    constants = constants | constfx;
	       	    -- multiply by x=R_i
	       	    prog = prog | {{slpPRODUCT, "in"=>i, -1}};
	       	    finalMULTIsum = finalMULTIsum | {#prog-1};
		    );	       
	       f = fnox;
	       )); 
     curPos := #prog;
     if #finalMULTIsum === 1 then (
       	  if finalMULTIsum#0 === curPos-1  -- if trivial 
       	  then null -- do nothing
       	  else if class finalMULTIsum#0 === Option and finalMULTIsum#0#0 == "const" then 
	  prog = prog | {{slpCOPY, finalMULTIsum#0}}
	  else error "unknown trivial MULTIsum"; 
	  )   
     else prog = prog | {{slpMULTIsum, #finalMULTIsum} | apply(finalMULTIsum, 
	       p->if class p === Option then p 
	       else p - curPos -- add a relative position
	       )};    
     << g << " => " << (constants, prog) << endl;
     (constants, prog)
     )

M2slp = method()
M2slp (List,ZZ,List) := (consts,nIns,slp) -> (
-- makes rawM2slp from pre-slp   
     curNode = #consts+nIns;
     p = {};
     scan(slp, n->(
	   k := first n;
	   if k === slpCOPY then (
	   	if class n#1 === Option and n#1#0 == "const" then p = p | {slpCOPY} | {n#1#1} 
		else error "unknown node type" 
		)  
	   else if k === slpMULTIsum then (
		p = p | {slpMULTIsum, n#1} | toList apply(2..1+n#1, 
		     j->if class n#j === Option and n#j#0 == "const" then n#j#1
		     else if class n#j === ZZ then curNode+n#j
		     else error "unknown node type" 
		     )
	   	)
	   else if k === slpPRODUCT then (
		p = p | {slpPRODUCT} | toList apply(1..2, j->(
          		       if class n#j === Option and n#j#0 == "in" then #consts + n#j#1
			       else if class n#j === ZZ then curNode+n#j
			       else error "unknown node type" 
			       ))
		)
	   else error "unknown SLP node key";   
	   curNode = curNode + 1;
	   ));
     p = {#consts,nIns,1,curNode-1} | p;
     (map(CC^1,CC^(#consts), {consts}), p)
     )
     
evaluateSLP = method()
evaluateSLP (List,List,List) := (constants, v, slp) -> (
     val := {};
     scan(#slp, i->(
	   n := slp#i;
	   k := first n;
	   if k === slpCOPY then (
	   	if class n#1 === Option and n#1#0 == "const" then val = val | {constants#(n#1#1)}
		else error "unknown node type"; 
		)  
	   else if k === slpMULTIsum then (
		val = val | { sum(2..1+n#1, 
			  j->if class n#j === Option and n#j#0 == "const" then constants#(n#j#1)
			  else if class n#j === ZZ then val#(i+n#j)
			  else error "unknown node type" 
			  )
		     }
	   	)
	   else if k === slpPRODUCT then (
		val = val | { 
		     product(1..2, j->(
          		       if class n#j === Option and n#j#0 == "in" then v#(n#j#1)
			       else if class n#j === ZZ then val#(i+n#j)
			       else error "unknown node type" 
			       ))
		     }
		)
	   else error "unknown SLP node key";   
	   ));
 last val
 )
///
restart
loadPackage "NAG"; debug NAG;
R = CC[x,y,z]
g = 3*y^2+2*x
g = 1 + 2*x^2 + 3*x*y^2 + 4*z^2
g = random(3,R)
(constants, slp) = poly2slp g
eg = evaluateSLP(constants,gens R,slp)
eg == g
///

beginDocumentation()

document { 
	Key => NAG,
	Headline => "Numerical Algebraic Geometry",
	EM "NAG (Numerical Algebraic Geometry)", " implements methods of polynomial homotopy continuation
	to solve systems of polynomial equations and deal with complex algebraic varieties."
	}

TEST ///
     assert(multistepPredictor(2_QQ,{0,0,0}) === {-3/8, 37/24, -59/24, 55/24}) -- Wikipedia: Adams-Bashforth
     assert(multistepPredictor(2_QQ,{-1}) === {-1/8, 5/8}) -- computed by hand
     assert(flatten entries (coefficients first multistepPredictorLooseEnd(2_QQ,{0,0,0}))#1=={1/120, 1/16, 11/72, 1/8})
///
  
       
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
loadPackage "NAG"
installPackage "NAG"
installPackage("PackageTemplate", RemakeAllDocumentation=>true)
check "NAG"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages NAMEOFPACKAGE=PackageTemplate install-one"
-- End:
