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
     areEqual, sortSolutions, multistepPredictor, multistepPredictorLooseEnd
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
	  AffinePatches => {},
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
     if n != numgens R then error "expected a square system";
     if o.tStep <= 0 then "expected positive tStep";  

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
		    	 invHx0timesHt0 := invHx0*Ht0;
		    	 -- if norm invHx0timesHt0 > divThresh then (x0 = infinity; break);
		    	 tStepSafe := 1; -- 1/(norm(Hx0)*norm(invHx0)); -- what is a good heuristic?
		    	 dt = min(min(tStep,tStepSafe), 1-t0);
		         dx = -dt*invHx0timesHt0;
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
	       solve(A,b)
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
	  result = parseSolutions(tarsolfile, gens R);
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
	  sols := readSolutionsBertini({"finite_solutions"});
	  result = {sols, toList(#sols:1)};
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
	  result = {sols, toList(#sols:1)};
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
     	       L := toString f;
     	       L = replace("ii", "I", L);
     	       L = replace("e", "E", L);
	       file << L << ";" << endl;
	       ));
     close file;
     ) 

parseSolutions = method(TypicalValue => Sequence)
parseSolutions (String,List) := (s,V) -> (
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
	  
     sols := toList apply(value L, x->new HashTable from toList x);
     sols/(x->{apply(V, v->x#v), x})
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
  print p;
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
  
makeBertiniInput = method(TypicalValue=>Nothing, Options=>{S=>{}})
makeBertiniInput (List, List) := o -> (v,T) -> (
-- IN:
--	v = variables
--	T = polynomials of target system
--      o.S = start system
  f := openOut "input"; -- THE name for Bertini's input file 
  f << "CONFIG" << endl;
  f << "MPTYPE: 2;" << endl; -- multiprecision
  if #o.S > 0 then
    f << "USERHOMOTOPY: 1;" << endl;
  f << endl << "END;" << endl << endl;
  f << "INPUT" << endl << endl;
  if #o.S > 0 then
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
  bertiniPolyString := p->( L := toString p; 
       L = replace("ii", "I", L); 
       L = replace("e", "E", L);
       L
       );
  if #o.S == 0 
  then scan(#T, i -> f << "f" << i << " = " << bertiniPolyString T#i << ";" << endl)
  else (
       if #o.S != #T then error "expected equal number of equations in start and target systems";
       f << "pathvariable t;" << endl 
         << "parameter s;" << endl
         << "s = t;" << endl;
       scan(#T, i -> f << "f" << i 
	    << " = (" << bertiniPolyString T#i << ")*(1-s)+s*(" << bertiniPolyString o.S#i << ");" << endl 
	   );
       );
  f << endl << "END" << endl << endl;
  close f;
  )

cleanupOutput = method(TypicalValue=>String)
cleanupOutput String := s -> (
-- cleanup output (Bertini and hom4ps2)
  t := replace("E", "e", s);
  t = replace("[(,)]","", t);
  t = replace("e\\+","e",t)
  )

readSolutionsBertini = method(TypicalValue=>List)
readSolutionsBertini List := ofiles -> (
  s := {};
  for f in ofiles do (
    l := lines get f;
    nsols := value first separate(" ", l#0);
    l = drop(l,2);
    while #s < nsols do (
	 if DBG>=10 then << "sol[<<" << temp << "] --------------------" << endl;
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
	 s = s | {coords};
	 );     
    );
  s
  )

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
