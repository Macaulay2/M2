-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version
newPackage(
     "NumericalAlgebraicGeometry",
     Version => "1.3.0.1",
     Date => "Oct 30, 2009",
     Headline => "Numerical Algebraic Geometry",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     Configuration => { "PHCpack" => "phc",  "Bertini" => "bertini", "HOM4PS2" => "hom4ps2" },	
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     DebuggingMode => true 
     )

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
     "solveSystem", "track", "refine", "totalDegreeStartSystem",
     "areEqual", "sortSolutions", -- "multistepPredictor", "multistepPredictorLooseEnd",
     "Software","PHCpack","Bertini","HOM4PS2","M2","M2engine","M2enginePrecookedSLPs",
     "gamma","tDegree","tStep","tStepMin","stepIncreaseFactor","numberSuccessesBeforeIncrease",
     "Predictor","RungeKutta4","Multistep","Tangent","Euler","Secant","MultistepDegree","ProjectiveNewton",
     "EndZoneFactor", "maxCorrSteps", "InfinityThreshold",
     "Normalize", "Projectivize",
     "AffinePatches", "DynamicPatch",
     "SLP", "HornerForm", "CompiledHornerForm", "CorrectorTolerance", "SLPcorrector", "SLPpredictor",
     "NoOutput",
     "Tolerance",
     "getSolution", "SolutionAttributes", "Coordinates", "SolutionStatus", "LastT", "RCondition", "NumberOfSteps",
     "randomSd", "goodInitialPair", "randomInitialPair", "GeneralPosition",
     "Bits", "Iterations", "ResidualTolerance", "ErrorTolerance",
     --"points", 
     "NAGtrace"
     }
exportMutable {
     }

-- DEBUG CORE ----------------------------------------
debug Core; -- to enable engine routines

-- GLOBAL VARIABLES ----------------------------------
PHCexe = NumericalAlgebraicGeometry#Options#Configuration#"PHCpack";
BERTINIexe = NumericalAlgebraicGeometry#Options#Configuration#"Bertini";
HOM4PS2exe = NumericalAlgebraicGeometry#Options#Configuration#"HOM4PS2";

DBG = 0; -- debug level (10=keep temp files)
SLPcounter = 0; -- the number of compiled SLPs (used in naming dynamic libraries)
lastPathTracker = null; -- path tracker object used last

WitnessSet = new Type of MutableHashTable 

-- ./NumericalAlgebraicGeometry/ FILES -------------------------------------
needs "./NumericalAlgebraicGeometry/PHCpack/PHCpack.interface.m2" 
needs "./NumericalAlgebraicGeometry/Bertini/Bertini.interface.m2" 

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
     stp := 1_R; -- h; WLOG, assume h=1 
     t_0 = 0_R;
     scan(n, j->(
	       t_(j+1) = t_j+stp;
	       if DBG>3 then << "t_("<< j+1 <<") = " << t_(j+1) << endl;
	       if j<n-1 then stp = c^(s#j)*stp;
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
     stp := 1_R; -- h; WLOG, assume h=1 
     t_0 = 0_R;
     scan(n, j->(
	       t_(j+1) = t_j+stp;
	       if DBG>3 then << "t_("<< j+1 <<") = " << t_(j+1) << endl;
	       stp = if j<n-2 then c^(s#j)*stp 
	       else if j==n-2 then a*stp;
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
-- 2-norm of a vector with CC entries
norm2 = method(TypicalValue=>RR)
norm2 List := v -> sqrt(sum(v, x->x*conjugate x));
norm2 Matrix := v -> norm2 flatten entries v;
-- Frobenius norm of a matrix
normF = method(TypicalValue=>RR)
normF Matrix := M -> max first SVD M;
     

normalize = method(TypicalValue => Matrix)
-- normalizes a column vector with CC entries
normalize Matrix := v -> (1/norm2 v)*v;

BombieriWeylScalarProduct = method(TypicalValue=>CC)
BombieriWeylScalarProduct (RingElement,RingElement) := CC => (f,g) -> sum(listForm f, a->(
	  imc := product(a#0, d->d!) / (sum(a#0))!; -- inverse of multinomial coeff
	  bc := coefficient((ring f)_(first a),g); -- coeff of corresponding monomial in g
	  imc*a#1*conjugate bc -- ring=CC[...]
	  ))
BombieriWeylNormSquared = method(TypicalValue=>RR)
BombieriWeylNormSquared RingElement := RR => f -> realPart sum(listForm f, a->(
	  imc := product(a#0, d->d!) / (sum(a#0))!; -- inverse of multinomial coeff
	  imc*a#1*conjugate a#1 -- ring=CC[...]
	  ))

------------------------------------------------------
track = method(TypicalValue => List, Options =>{
	  Software=>M2engine, NoOutput=>false, 
	  gamma=>1, 
	  tDegree=>1,
     	  -- step control
	  tStep => 0.05, -- initial
          tStepMin => 1e-6,
	  stepIncreaseFactor => 2_QQ,
	  numberSuccessesBeforeIncrease => 4,
	  -- predictor 
	  Predictor=>RungeKutta4, 
	  SLPpredictor=>false, --temp!!!
	  MultistepDegree => 3, -- used only for Predictor=>Multistep
	  -- corrector 
	  SLPcorrector=>false, --temp!!!
	  maxCorrSteps => 3,
     	  CorrectorTolerance => 1e-6, -- tracking tolerance
	  -- end of path
	  EndZoneFactor => 0.05, -- EndZoneCorrectorTolerance = CorrectorTolerance*EndZoneFactor when 1-t<EndZoneFactor 
	  InfinityThreshold => 100000, -- used to tell if the path is diverging
	  -- projectivize and normalize
	  Normalize => false, -- normalize in the Bombieri-Weyl norm
	  Projectivize => false, 
	  AffinePatches => DynamicPatch,
	  -- slp's 
	  SLP => null -- possible values: null, HornerForm, CompiledHornerForm 	  
	  } )
track (List,List,List) := List => o -> (S,T,solsS) -> (
-- tracks solutions from start system to target system
-- IN:  S = list of polynomials in start system
--      T = list of polynomials in target system
--      solsS = list of solutions to S
-- 	gamma => nonzero complex number
-- OUT: solsT = list of target solutions corresponding to solsS
     HISTORY := DBG>1 or member(o.Predictor, {Multistep,Secant});
     n := #T; 
     if n > 0 then R := ring first T else error "expected nonempty target system";
     if #S != n then 
     error "expected same number of polynomials in start and target systems";
     if any(S, f->ring f =!= R) or any(T, f->ring f =!= R)
     then error "expected all polynomials in the same ring";
     if o.tStep <= 0 then error "expected positive tStep";  
     if (o.Projectivize or o.SLP===null) and (o.SLPpredictor or o.SLPcorrector) 
     then error "SLPpredictor amd SLPcorrector can be used only with Projectivize=false and SLP=!=null"; 
     if o.Software===M2enginePrecookedSLPs and (o.Projectivize or o.SLP===null) 
     then error "M2enginePrecookedSLPs is implemented for Projectivize=>false and SLP != null";
--     if o.Predictor===ProjectiveNewton and (o.Software=!=M2 or o.SLP=!=null)
--     then error "ProjectiveNewton (experimental) requires Software=>M2 and o.SLP=>null";
     
     -- PHCpack -------------------------------------------------------
     if o.Software == PHCpack then return trackPHCpack(S,T,solsS,o)
     else if o.Software == Bertini then return trackBertini(S,T,solsS,o)
     else if not member(o.Software,{M2,M2engine,M2enginePrecookedSLPs}) 
     then error "wrong Software option or implementation is not available";
     
     -- M2 (main code)  --------------------------------------------------------     
     setupStartTime := currentTime();
     -- threshholds and other tuning parameters (should include most of them as options)
     stepDecreaseFactor := 1/o.stepIncreaseFactor;
     theSmallestNumber := 1e-12;
     
     -- determine whether the problem is projective
     isProjective := false;
     if n != numgens R then (
	  if numgens R == n+1 and all(S, isHomogeneous) and all(T, isHomogeneous) 
	  then ( 
	       isProjective = true; 
	       n = n+1;
	       )  
	  else error "expected a square system";
     	  );
    
     K := CC_53; -- THE coefficient ring
     solsS = solsS / (s->sub(transpose matrix {toList s}, CC)); -- convert to vectors
     if o.Projectivize then (
	  if isProjective then error "the problem is already projective";
	  h := symbol h;
	  R = K[gens R | {h}]; 
	  n = numgens R;
	  T = apply(T, f->homogenize(sub(f,R), h)); 
	  S = apply(S, f->homogenize(sub(f,R), h));
     	  solsS = solsS / (s->s||matrix{{1_K}});
	  isProjective = true;
	  );
     
     if o.Predictor===ProjectiveNewton and not isProjective 
	  then "projective expected: either homogeneous system or Projectivize=>true";
     if isProjective then (
	  if member(o.Software,{M2engine,M2enginePrecookedSLPs}) and not o.Normalize
	  then error "Normalize=>true expected for this choice of Software";
     	  if o.Software === M2enginePrecookedSLPs 
	  then error "Software=>M2enginePrecookedSLPs not implemented for projective case";	       
     	  -- affine patch functions 
     	  pointToPatch := (x0,p)-> (1/(p*x0)_(0,0))*x0; -- representative for point x0 in patch p
	  patchEquation := p -> p * transpose vars R - 1;

 	  if o.Predictor === ProjectiveNewton or o.AffinePatches === DynamicPatch then (
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
	       patches = patches | { o.gamma*patches#1 };
     	       if DBG>1 then << "affine patch: " << toString patches#1 <<endl;
	       T = T | {patchEquation patches#1};
	       S = S | {patchEquation patches#2};
	       solsS = solsS / (s->pointToPatch(s, patches#2));
	       );
	  ); 
     
     -- create homotopy
     t := symbol t;
     Rt := K(monoid[gens R, t]); -- how do you cleanly extend the generators list: e.g., what if "t" is a var name?
     t = last gens Rt; 
     Kt := K(monoid[t]);
     (nS,nT) := if o.Normalize -- make Bomboeri-Weyl norm of the systems equal 1
     then (apply(S, s->s/sqrt(#S * BombieriWeylNormSquared s)), apply(T, s->s/sqrt(#T * BombieriWeylNormSquared s)))
     else (S,T);
     
     if o.Predictor===ProjectiveNewton or (isProjective and o.Software===M2engine)
     -- in both cases a linear homotopy on the unit sphere is performed
     then (
	  nS = (o.gamma/abs(o.gamma))*nS;
	  H := {matrix{nS},matrix{nT}}; -- a "linear" homotopy is cooked up at evaluation using nS and nT
	  DMforPN := diagonalMatrix append(T/(f->1/sqrt first degree f),1);
	  maxDegreeTo3halves := power(max(T/first@@degree),3/2);
	  reBW'ST := realPart sum(#S, i->BombieriWeylScalarProduct(nS#i,nT#i));-- real Bombieri-Weyl scalar product
	  sqrt'one'minus'reBW'ST'2 :=  sqrt(1-reBW'ST^2);
	  bigT := asin sqrt'one'minus'reBW'ST'2; -- the linear homotopy interval is [0,bigT]
	  Hx := H/transpose@@jacobian; -- store jacobians (for evalHx)
     	  )	  
     else (
     	  H = matrix {apply(#S, i->o.gamma*(1-t)^(o.tDegree)*sub(nS#i,Rt)+t^(o.tDegree)*sub(nT#i,Rt))};
     	  JH := transpose jacobian H; 
     	  Hx = JH_(toList(0..n-1));
     	  Ht := JH_{n};
	  );

     -- evaluation times
     etH := 0;
     etHx := 0; 
     etHt := 0;
     -- evaluation functions using SLP
     if o.SLP =!= null and o.Software =!= M2engine then (
	  toSLP := pre -> (
	       (constMAT, prog) = (if o.SLP === HornerForm 
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
	       r := if o.Predictor === ProjectiveNewton 
	       then (
		    sine := sin(t0*bigT); cosine := cos(t0*bigT);
		    transpose( lift(sub(H#0,transpose x0),K)*(cosine-(reBW'ST/sqrt'one'minus'reBW'ST'2)*sine) 
	       	    	 + lift(sub(H#1,transpose x0),K)*(sine/sqrt'one'minus'reBW'ST'2) )
		    )
	       else if o.SLP =!= null then transpose fromSlpMatrix(slpH, transpose x0 | matrix {{t0}})
     	       else lift(sub(transpose H, transpose x0 | matrix {{t0}}), K);
	       --(old) if o.Predictor === ProjectiveNewton then ((normalizer t0)*r) || matrix{{0_K}} else 
	       if dPatch === null then r
	       else r || matrix{{(dPatch*x0)_(0,0)-1}} -- patch equation evaluated  
	       );
	  etH = etH + tr#0;
	  tr#1
	  );
     evalHNoPatchNoNormalizer := (x0,t0)-> (
	  tr := timing (
	       if o.SLP =!= null then r := transpose fromSlpMatrix(slpH, transpose x0 | matrix {{t0}})
     	       else r = lift(sub(transpose H, transpose x0 | matrix {{t0}}), K);
	       r
	       );
	  etH = etH + tr#0;
	  tr#1
	  );
     evalHxNoPatch := (x0,t0)-> (
	  r := if o.Predictor === ProjectiveNewton then (
	       sine := sin(t0*bigT); cosine := cos(t0*bigT);
	       lift(sub(Hx#0,transpose x0),K)*(cosine-(reBW'ST/sqrt'one'minus'reBW'ST'2)*sine) 
	       + lift(sub(Hx#1,transpose x0),K)*(sine/sqrt'one'minus'reBW'ST'2)   
	       )
	  else if o.SLP =!= null then fromSlpMatrix(slpHx, transpose x0 | matrix {{t0}})
     	  else lift(sub(Hx, transpose x0 | matrix {{t0}}), K);
	  --(old) if o.Predictor === ProjectiveNewton then r = r * normalizer t0;
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
	       r := if o.Predictor === ProjectiveNewton then (
		    sine := sin(t0*bigT); cosine := cos(t0*bigT);
		    transpose( lift(sub(H#0,transpose x0),K)*(-sine-(reBW'ST/sqrt'one'minus'reBW'ST'2)*cosine)
		    + lift(sub(H#1,transpose x0),K)*(cosine/sqrt'one'minus'reBW'ST'2) )
		    )
	       else if o.SLP =!= null then fromSlpMatrix(slpHt, transpose x0 | matrix {{t0}})
     	       else lift(sub(Ht, transpose x0 | matrix {{t0}}), K);
	       --(old) if o.Predictor === ProjectiveNewton then r = r * (normalizer t0) + evalHNoPatchNoNormalizer(x0,t0) * (normalizer' t0);
	       if dPatch === null then r
	       else r || matrix {{0_K}}
	       );
	  etHt = etHt + tr#0;
	  tr#1
	  );
     evalMinusInverseHxHt := (x0,t0)-> -(inverse evalHx(x0,t0))*evalHt(x0,t0);
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

     compStartTime = currentTime();      
     
     rawSols := if member(o.Software,{M2enginePrecookedSLPs, M2engine}) then (
	  PT := if o.Software===M2engine then (
	       if isProjective then rawPathTrackerProjective( raw matrix {nS}, raw matrix {nT}, 
		    reBW'ST ) -- pass normalized start/target and Re(B-W product)
	       else rawPathTracker(raw H) 
	       ) else rawPathTrackerPrecookedSLPs(slpHxt, slpHxH);
	  lastPathTracker = PT;
	  rawSetParametersPT(PT, 
	       isProjective,
	       o.tStep, o.tStepMin, 
	       toRR o.stepIncreaseFactor, toRR stepDecreaseFactor, o.numberSuccessesBeforeIncrease,
	       o.CorrectorTolerance, o.maxCorrSteps, o.EndZoneFactor, toRR o.InfinityThreshold,
	       ( -- pred_type:
		    if o.Predictor === Tangent then predTANGENT
		    else if o.Predictor === RungeKutta4 then predRUNGEKUTTA
		    else if o.Predictor === Euler then predEULER
		    else if o.Predictor === ProjectiveNewton then predPROJECTIVENEWTON
		    else error "engine: unknown predictor")
	       );
	  solsM := matrix apply(solsS,s->first entries transpose s);
	  --print solsM;
	  rawLaunchPT(PT, raw solsM);
	  if o.NoOutput then null else entries map(K,rawGetAllSolutionsPT(PT))
     	  )
     else if o.Software===M2 then (
     	  apply(#solsS, sN->(
	       s := solsS#sN;
	       s'status := "PROCESSING";
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
	       while s'status === "PROCESSING" and 1-t0 > theSmallestNumber do (
		    if 1-t0<=o.EndZoneFactor+theSmallestNumber and not endZone then (
			 endZone = true;
			 -- to do: see if this path coinsides with any other path
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
		    -- default dt; ProjectiveNewton and Multistep modify dt
		    dt = if endZone then min(tStep, 1-t0) else min(tStep, 1-o.EndZoneFactor-t0);

     	       	    -- projective stuff
		    if o.Predictor == ProjectiveNewton then (
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
			 if o.SLP =!= null and o.SLPpredictor then (
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
			 if o.SLP =!= null and o.SLPpredictor then (
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
			      aMap := map(K,aRing, matrix{{dt/(history#count#"t"-history#(count-1)#"t")}});
			      aMScoeffs/aMap  
			      )
			 else multistepPredictor(o.stepIncreaseFactor,stepAdjSequence); 
     	       	    	 
                	 -- dx = dt*sum_{i=0..nPoints-1} MScoeff_i*rhsODE(t_i)
			 dx = delta*sum(nPoints, i->MScoeffs#i*history#(count-nPoints+1+i)#"rhsODE");
			 if DBG > 3 then << "delta = " << delta << "   MScoeffs = " << MScoeffs << endl;
			 )
		    else if o.Predictor == ProjectiveNewton then (
			 Hx0 = evalHx(x0,t0);
			 Ht0 = evalHt(x0,t0);
			 chi2 := sqrt((norm2 Ht0)^2 + (norm2 solve(Hx0, Ht0))^2);
			 chi1 := 1 / min first SVD(DMforPN*Hx0);
			 << "chi1 = " << chi1 << endl; --!!!
			 if count<=5 then print(DMforPN*Hx0); --!!!
			 dt = 0.04804448/(maxDegreeTo3halves*chi1*chi2*bigT); -- divide by bigT since t is in [0,1]
			 if dt<o.tStepMin then (
			      if DBG > 2 then (
				   << "chi1 = " << chi1 << endl;
			      	   << "chi2 = " << chi2 << endl;
				   );
			      s'status = "MIN STEP (FAILURE)"; 
			      --error "too small step";
			      );
			 if dt > 1-t0 then dt = 1-t0;
			 dx = 0;
			 )
		    else error "unknown Predictor";


		    if DBG > 3 then << "  dt = " << dt << "  dx = " << toString dx << endl;
		    if HISTORY then history#count#"dx" = dx;

    	 	    t1 := t0 + dt;
		    x1 := x0 + dx;
		    
		    -- corrector step
		    if DBG>9 then << ">>> corrector" << endl;
		    nCorrSteps := 0;
		    if o.Predictor === ProjectiveNewton then (
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
			      then ( s'status = "INFINITY (FAILURE)"; dx = 0 );
			      );
			 );
		    if DBG>9 then << ">>> step adjusting" << endl;
		    if o.Predictor =!= ProjectiveNewton and dt > o.tStepMin 
		    and norm dx > CorrectorTolerance() * norm x1 then ( -- predictor failure 
			 predictorSuccesses = 0;
			 stepAdj = stepAdj - 1;
	 	 	 tStep = stepDecreaseFactor*tStep;
			 if DBG > 2 then << "decreased tStep to "<< tStep << endl;	 
			 if tStep < o.tStepMin then s'status = "MIN STEP (FAILURE)";
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
			      stepAdj = 1;
			      tStep = o.stepIncreaseFactor*tStep;	
			      if DBG > 2 then << "increased tStep to "<< tStep << endl;
			      )
			 else stepAdj = 0; -- keep the same step size
			 );
		    );        	    
	       if s'status==="PROCESSING" then s'status = "REGULAR";
	       if DBG > 0 then << (if s'status == "REGULAR" then "."
		    else if s'status == "SINGULAR" then "S"
		    else if s'status == "MIN STEP (FAILURE)" then "M"
		    else if s'status == "INFINITY (FAILURE)" then "I"
		    else error "unknown solution status"
		    ) << if (sN+1)%100 == 0 then endl else flush;
	       -- create a solution record 
	       (x0,
		    "#steps"=>count-1, -- number of points - 1 
		    "status "=>s'status, 
		    "last t" => t0, 
		    "cond#^{-1}" => (svd := sort first SVD evalHx(x0,t0); first svd / last svd )
		    ) | ( if HISTORY
		    then sequence new HashTable from history 
		    else sequence ())
	       ))
     );
     if DBG>3 then print rawSols;
     ret := if o.NoOutput then null 
          else if o.Software===M2engine or o.Software===M2enginePrecookedSLPs then (
	       if o.Projectivize then apply(rawSols, s->(
			 if norm(last s) < 1/o.InfinityThreshold then print "Warning: solution at infinity encountered";
			 {apply(drop(s,-1),u->(1/last s)*u)}
			 ))
	       else rawSols/(s->{s}) 
          ) else (
     	       if o.Projectivize then (
	  	    rawSols = apply(rawSols, s->(
		    	      s' = flatten entries first s;
		    	      s'status = s#2#1;
		    	      if norm(last s') < 1/o.InfinityThreshold then s'status = "INFINITY (FAILURE)";
		    	      {matrix {apply(drop(s',-1),u->(1/last s')*u)}} | {s#1} | {STATUS => s'status} | drop(toList s, 3) 
	       	    	      ))
	  	    );
	       rawSols --, s->s#2#1==="REGULAR" or s#2==="SINGULAR"} )
	       /(s->{flatten entries first s} | drop(toList s,1))
	       );
     if DBG>0 then (
	  if o.Software==M2 then (
	       << "Number of solutions = " << #ret << endl << "Average number of steps per path = " << toRR sum(ret,s->s#1#1)/#ret << endl;
     	       if DBG>1 then (
	       	    if o.SLP =!= null 
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
     ret
     )

refine = method(TypicalValue => List, Options =>{
	  Software=>M2engine, 
      	  ResidualTolerance => 0, 
	  ErrorTolerance => 1e-10,
	  Iterations => null,
	  Bits => 300
	  })
refine (List,List) := List => o -> (T,solsT) -> (
-- tracks solutions from start system to target system
-- IN:  T = list of polynomials in target system
--      solsT = list of solutions to T
-- OUT: solsR = list of refined solutions 
     n := #T; 
     if n > 0 then R := ring first T else error "expected nonempty target system";
     if any(T, f->ring f =!= R)
     then error "expected all polynomials in the same ring";
     isProjective := false;
     if n != numgens R then (
	  if numgens R == n+1 and all(T, isHomogeneous) 
	  then ( 
	       isProjective = true; 
	       n = n+1;
	       )  
	  else error "expected a square system";
     	  );

     if not isProjective then (
     	  if o.Software === M2engine then (
	       if lastPathTracker === null 
	       then error "path tracker is not set up"
	       else return entries map(CC_53, rawRefinePT(lastPathTracker, raw matrix solsT, o.ErrorTolerance, 
		    	 if o.Iteration===null then 30 else o.Iteration));
     	       ) else if o.Software === PHCpack then (
	       return refinePHCpack(T,solsT,Iterations=>o.Iterations,Bits=>o.Bits,ErrorTolerance=>o.ErrorTolerance)
	       );
	  );
     -- M2 part 
     n'iterations := if o.Iterations =!= null then o.Iterations else 100; -- infinity
     T = matrix {T};
     J = transpose jacobian T; 
     evalT := x0 -> (
	  ret := lift(sub(transpose T, transpose x0), CC); 
	  if isProjective then ret || matrix{{0_CC}} else ret
	  );
     evalJ := x0 -> (
	  ret := lift(sub(J, transpose x0), CC);
	  if isProjective then ret || matrix{ flatten entries x0 / conjugate} else ret
	  );
     
     solsR := apply(solsT, s->(
	       x1 := sub(transpose matrix {s}, CC); -- convert to vector 
	       if isProjective then x1 = normalize x1;
	       -- corrector step
	       dx := 1; -- dx = + infinity
	       nCorrSteps := 0;
	       while norm dx > o.ErrorTolerance * norm x1 and nCorrSteps < n'iterations do ( 
		    if DBG > 3 then << "x=" << toString x1 << " res=" <<  toString evalT(x1) << " dx=" << dx << endl;
		    dx = solve(evalJ(x1), -evalT(x1));
		    x1 = x1 + dx;
		    if isProjective then x1 = normalize x1;
		    nCorrSteps = nCorrSteps + 1;
		    );
	       x1
	       ));
     apply(#solsT, i-> flatten entries solsR#i)      
     )     

-- possible solution statuses returned by engine
solutionStatusLIST := {"UNDETERMINED", "PROCESSING", "REGULAR", "SINGULAR", "INFINITY (FAILURE)", "MIN STEP (FAILURE)"}

getSolution = method(Options =>{SolutionAttributes=>(Coordinates, SolutionStatus, LastT, RCondition, NumberOfSteps)})
getSolution ZZ := Thing => o -> i -> (
-- gets specified solution from the engine
-- IN:  the number of solution
--      SolutionAttributes=> ... specifies various data attached to a solution ...
-- OUT: whatever is requested by SolutionAttributes (either as a sequence of as a single element)
     if lastPathTracker === null 
     then error "path tracker is not set up";
     p := o.SolutionAttributes; 
     possible := set{Coordinates, SolutionStatus, LastT, RCondition, NumberOfSteps};
     if not isSubset(set{p}, possible) and 
     not (class p === Sequence and isSubset(set p, possible))
     then error "wrong SolutionAttributes option";
     pp := if class p === Sequence then p else {p};
     ret := apply(pp, r->
	  if r===Coordinates then flatten entries map(CC_53, rawGetSolutionPT(lastPathTracker, i))
	  else if r===SolutionStatus then solutionStatusLIST#(rawGetSolutionStatusPT(lastPathTracker, i))
	  else if r===LastT then rawGetSolutionLastTvaluePT(lastPathTracker, i)
	  else if r===RCondition then rawGetSolutionRcondPT(lastPathTracker, i)
	  else if r===NumberOfSteps then rawGetSolutionStepsPT(lastPathTracker, i)
	  );
     if class p === Sequence then ret else first ret
     )

homogenizeSystem = method(TypicalValue => List)
homogenizeSystem List := List => T -> (
     R := ring first T;
     h := symbol h;
     Rh := (coefficientRing R)[gens R | {h}]; 
     apply(T, f->homogenize(sub(f,Rh), h))
     )
dehomogenizeSystem = method(TypicalValue => List)
dehomogenizeSystem List := List => T -> (
     Rh := ring first T;
     R := (coefficientRing Rh)[drop(gens Rh,-1)]; 
     apply(T, f -> (map(R,Rh,vars R | matrix{{1_R}})) f)
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
     n := #T;
     if n != numgens R then (
	  if numgens R == n+1 and all(T, isHomogeneous) 
	  then isH = true
	  else error "wrong number of polynomials";
	  )
     else isH = false;
     S := apply(n, i->R_i^(first degree T_i) - (if isH then R_n^(first degree T_i) else 1) );
     s := apply(n, i->( 
	  d := first degree T_i; 
	  set apply(d, j->sequence exp(ii*2*pi*j/d))
	  ));
     solsS := first s;
     scan(drop(s,1), t->solsS=solsS**t);
     if numgens R === 1 
     then solsS = toList solsS/(a -> 1:a)
     else solsS = toList solsS/deepSplice; 
     if isH then solsS = solsS / (s->s|sequence 1);
     (S, solsS)
     ) 

randomGaussian = method()
randomGaussian := () -> sum(12, i->random 1.0) - 6;

randomInComplexUnitSphere = method()
randomInComplexUnitSphere ZZ := Matrix => n->(
     x := transpose matrix {apply(n, i->randomGaussian()+ii*randomGaussian())};
     (1/norm2 x)*x
     )

randomInComplexUnitBall = method()
randomInComplexUnitBall ZZ := Matrix => n->(
     x := randomInComplexUnitSphere n; 
     r := (random 1.)^(1/(2*n));
     r*x
     );  
     
--dimension of \cal H_{(d)}, where d is a degree vector
dimHd = method()
dimHd List := ZZ => d->sum(#d, i->binomial(#d+d#i,d#i));

randomDiagonalUnitaryMatrix = method()
randomDiagonalUnitaryMatrix ZZ := n -> diagonalMatrix apply(n, i->exp(ii*random(2*pi)))

--random unitary n-by-n matrix (w.r.t. Haar measure)
randomUnitaryMatrix = method()
randomUnitaryMatrix ZZ := n -> (
     Ml := flatten entries randomInComplexUnitBall(n^2);
     M := map(CC^n, n, (i,j)->Ml#(n*i+j)); -- n+1 by n+1 matrix                         
     randomDiagonalUnitaryMatrix n * (last SVD M) 
     )

--follows the algorithm of Zyczkowski, Kus "Random Unitary Matrices"
--(it works, but seems to produce a wrong distribution)  
randomUnitary = method()
randomUnitary ZZ := n -> (
     E := (i,j,phi'psi'xi) -> (
	  (phi,psi,xi) := phi'psi'xi;
	  map(CC^n,n,(a,b)->
	       if a==b then (
	       	    if a==i then cos(phi)*exp(ii*psi)
		    else if b==j then cos(phi)*exp(-ii*psi)
		    else 1
	       	    )
	       else (
		    if a==i and b==j then sin(phi)*exp(ii*xi)
		    else if b==i and a==j then -sin(phi)*exp(-ii*xi)
		    else 0
		    )
	       )
     	  );
     ijs := reverse apply(toList select((0,0)..(n-1,n-1), ij -> ij#0<ij#1), ij->(ij#1-ij#0-1,n-ij#0-1));
     alpha := random(2*pi); 
     exp(ii*alpha)*product(ijs, ij->(
	       (i,j) := ij;
	       phi := asin(random(1_RR)^(1/(2*(i+1))));
	       psi := random(2*pi);
	       xi := if i!=0 then 0 else random(2*pi);
	       E(i,j,(phi,psi,xi))
	       ))
     )

-- IN: R, polynomial ring in n+1 vars
--     d, list of equation degrees
-- OUT: conjectured (by Shub and Smale) good initial pair (f,z_0), f\in Hd, z\in P^n
--      f, List; z_0, List of one element
goodInitialPair = method(Options=>{GeneralPosition=>false})
goodInitialPair List := o -> T -> (
     R := ring first T;
     d := T/first@@degree;
     lastVar := last gens R;
     n := numgens R - 1;
     (S, solsS) := ( transpose matrix{apply(n, i->(sqrt d#i)*lastVar^(d#i - 1)*R_i)}, 
     	  transpose matrix{toList (n:0_CC) | {1}} );
      if o.GeneralPosition === true then (
	  coord'transform := randomUnitaryMatrix(n+1);
     	  inv'coord'transform := solve(coord'transform, map CC^(n+1));
     	  coord'change := map(R,R,(vars R) * transpose coord'transform);	       	    
     	  base'S := flatten entries coord'change ((map(R,ring S, vars R)) S);
     	  base'sol := entries transpose (inv'coord'transform * solsS);
	  (base'S, base'sol)
	  ) else (flatten entries S, entries transpose solsS)
     )

-- IN: ds, list of equation degrees                                                                                                         
-- OUT: a random system in unit ball Hd \cap {systems with no two highest terms in x_n}  
randomHd'NoHighXn = method()
randomHd'NoHighXn List := ds -> (
     n := #ds;
     x := symbol x;
     R := CC_53(monoid [vars(53..n+53)]); 
     sqrt'n := 1/sqrt n;
     u := randomInComplexUnitBall (dimHd ds - (n+1)*#ds);     
     i := -1; --counter
     ret := apply(ds,d->sum( 
	       ((ideal gens R)^d)_*, 
	       m->(
	       	    a := first first listForm m; -- exponent vector
	     	    if last a > d-2 then 0 else (
	       	    	 i = i + 1;
     	       	    	 u_(i,0) * sqrt((sum a)! / product(a, d->d!))*m -- ... * sqrt of multinomial coeff
	       	    	 )
		    )
	       ));
     assert(i+1 == dimHd ds - (n+1)*#ds);
     ret          
     );


-- IN: ds, list of equation degrees                                                                                                         
-- OUT: a random system in unit sphere S \subset Hd 
randomSd = method()
randomSd List := ds -> (
     n := #ds;
     x := symbol x;
     R := CC_53(monoid [vars(53..n+53)]); 
     sqrt'n := 1/sqrt n;
     u := randomInComplexUnitSphere dimHd ds;     
     i := -1; --counter
     ret := apply(ds,d->sum( 
	       ((ideal gens R)^d)_*, 
	       m->(
	       	    a := first first listForm m; -- exponent vector
	       	    i = i + 1;
     	       	    u_(i,0) * sqrt((sum a)! / product(a, d->d!))*m -- ... * sqrt of multinomial coeff
	       	    )
	       ));
     assert(i+1 == dimHd ds);
     ret          
     );

///
restart 
debug loadPackage "NumericalAlgebraicGeometry"
sum(randomSd {2,3,4,5} / BombieriWeylNormSquared) -- should be 1
///

randomInitialPair = method()
randomInitialPair List := T -> (
-- for a homogeneous system constructs a start system and one root 
-- IN:  T = list of polynomials 
-- OUT: (S,solsS}, where 
--      S     = list of polynomials, 
--      solsS = list of sequences
     R := ring first T;
     n := numgens R - 1; 
     Ml := flatten entries randomInComplexUnitBall dimHd(T/first@@degree);
     M := map(CC^n, n+1, (i,j)->Ml#((n+1)*i+j)); -- n by n+1 matrix formed from n^2+n (first) entries of Ml    
     coord'transform := last SVD M;
     --oMl := flatten entries randomInComplexUnitBall (dimHd(T/first@@degree)-n);
     --M := map(CC^n,CC^1,0) | map(CC^n, n, (i,j)->oMl#(n*i+j)); -- 0-column | n by n matrix formed from n^2 (first) entries of Ml    
     --O := flatten entries randomInComplexUnitSphere((n+1)^2);
     --coord'transform := last SVD map(CC^(n+1), n+1, (i,j)->O#((n+1)*i+j));
     inv'coord'transform := solve(coord'transform, map CC^(n+1));
     coord'change := map(R,R,(vars R) * transpose coord'transform);
     --(good'sys, good'sol) := goodInitialPair(R,T/first@@degree);
     good'sol := transpose matrix{toList (n:0_CC)|{1}};
     sol := inv'coord'transform * good'sol; -- root
     good'sys := transpose matrix{randomHd'NoHighXn(T/first@@degree)};  
     h := coord'change ((map(R,ring good'sys, vars R)) good'sys); 
     ret := sqrt(1-(normF M)^2)*h + diagonalMatrix apply(T,f->(
	       d := first degree f;
	       (sum(numgens R, i->R_i*conjugate sol_(i,0)))^(d-1) * sqrt d 
	       )) * M * transpose vars R;     
     assert (norm2 sub(ret,transpose sol)<0.0001); -- just a check
     (flatten entries ret, {flatten entries sol})
     )     

-- INTERFACE part ------------------------------------
solveSystem = method(TypicalValue => List, Options =>{Software=>M2engine})
solveSystem List := List => o -> F -> (
-- solves a system of polynomial equations
-- IN:  F = list of polynomials
--      Software => {PHCpack, Bertini, hom4ps2}
-- OUT: {s,m}, where 
--             s = list of solutions 
--     	       m = list of corresponding multiplicities	 
     local result;
     R := ring F#0;
     --if numgens R != #F then error "expected a square system";
     v := flatten entries vars R;
     if member(o.Software, {M2,M2engine,M2enginePrecookedSLPs}) then ( 
	  result = 
	  --if all(F, f -> first degree f <= 1)
     	  --then ( 
	  --     A := matrix apply(F, f->apply(v, x->coefficient(x,f)));
	  --     b := matrix apply(F, f->{coefficient(1_R,f)});
	  --     {{flatten entries solve(A,-b)}}
	  --     )
	  --else 
	       (
	       (S,solsS) := totalDegreeStartSystem F;
	       track(S,F,solsS,gamma=>exp(random(0.,2*pi)*ii),o)
	       )
	  )
     else if o.Software == PHCpack then result = solvePHCpack(F,o)
     else if o.Software == Bertini then result = solveBertini(F,o)
     else if o.Software == HOM4PS2 then (
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
  scan(T, p -> s = s | replace("ii", "I", toString p) | ";\n");
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
  while substring(2,9,l#i) != "The order" do ( -- current line is non-empty	   
       coords := {};
       while #l#i > 2 do ( -- until an empty line
	    a := select(separate(" ",  cleanupOutput(l#i)), t->#t>0);
	    coords = coords | {(value a#0)+ii*(value a#1)};
	    i = i + 1;
       	    );
       if DBG>=10 then << coords << endl;
       s = s | { {apply(#coords, i->coords#(p#i))} };
       i = i + 4; -- skip to the next solution
       );
  s
  )
  

-----------------------------------------------------------------------
-- WITNESS SET = {
--   Equations,            -- an ideal  
--   Slice,                -- a list of linear equations OR a matrix (of their coefficients)
--   Points	           -- a list of points (in the format of the output of solveSystem/track) 
--   }
-- caveat: we assume that #Equations = dim(Slice)   
WitnessSet.synonym = "witness set"
WitnessSet.Tolerance = 1e-6;
dim WitnessSet := W -> ( if class W.Slice === List then #W.Slice 
     else if class W.Slice === Matrix then numrows W.Slice 
     else error "ill-formed slice in WitnessSet" )
codim WitnessSet := W -> numgens ring W - dim W
ring WitnessSet := W -> ring W.Equations
degree WitnessSet := W -> #W.Points
ideal WitnessSet := W -> W.Equations
net WitnessSet := W -> "(dim=" | net dim W |",deg="| net degree W | ")" 
witnessSet = method()
witnessSet (Ideal,Ideal,List) := (I,S,P) -> new WitnessSet from { Equations => I, Slice => S_*, Points => VerticalList P}
witnessSet (Ideal,Matrix,List) := (I,S,P) -> new WitnessSet from { Equations => I, Slice => S, Points => VerticalList P}
witnessSet Ideal := I -> (
     n := numgens ring I;
     d := dim I;
     SM := (randomUnitaryMatrix n)^(toList(0..d-1));
     SM = promote(SM,ring I);
     S := ideal(SM * transpose vars ring I + random(CC^d,CC^1));
     RM := (randomUnitaryMatrix numgens I)^(toList(0..n-d-1));
     RM = promote(RM,ring I);
     P := solveSystem(flatten entries (RM * transpose gens I) | S_*);
     PP := select(P, p->norm sub(gens I, matrix p) < 1e-5);
     witnessSet(I,S,PP/first)
     )
points = method() -- strips all info except coordinates, returns a doubly-nested list
points WitnessSet := (W) -> apply(W.Points, first)
equations = method() -- returns list of equations
equations WitnessSet := (W) -> (W.Equations)_*
slice = method() -- returns linear equations for the slice (in both cases)   
slice WitnessSet := (W) -> ( if class W.Slice === List then W.Slice
     else if class W.Slice === Matrix then sliceEquations(W.Slice, ring W)
     else error "ill-formed slice in WitnessSet" )
see = method()
see WitnessSet := (W) -> new HashTable from W
isContained = method()
isContained (List,WitnessSet) := (point,W) -> any(points W, p->areEqual(point,p,Tolerance=>WitnessSet.Tolerance))
isContained (WitnessSet,WitnessSet) := (V,W) -> (
     dim V <= dim W 
     and all(points V, p->isContained(p,moveSlice(W,randomSlice(dim W, numgens ring W, p))))
     )

///
restart
debug loadPackage "NumericalAlgebraicGeometry"
CC[x,y,z]
I = ideal (x^2+y)
S = ideal (x+y+2*z-1)
P = {{ii_CC,1_CC},{ii_CC,1_CC}}
I = ideal {z-x*y, x^2-y, y^2-z*x}
W = witnessSet(I,S,P)
W = witnessSet I
W = witnessSet(I, sub(transpose last coefficients gens S,CC), P)
points W
equations W
slice W
///

sliceEquations = method(TypicalValue=>List) 
sliceEquations (Matrix,Ring) := (S,R) -> (
-- make slicing plane equations
     apply(numrows S, i->(sub(S^{i},R) * transpose(vars R | matrix{{1_R}}))_(0,0)) 
     )

randomSlice = method()
randomSlice (ZZ,ZZ) := (d,n) -> randomSlice(d,n,{})
randomSlice (ZZ,ZZ,List) := (d,n,point) -> (
     SM := (randomUnitaryMatrix n)^(toList(0..d-1));
     SM | (if #point==0
	  then random(CC^d,CC^1)    
	  else -SM * transpose matrix{point} -- slice goes thru point
	  )
     )

movePointsToSlice = method(TypicalValue=>List, Options =>{Software=>M2})
movePointsToSlice (WitnessSet, List) := WitnessSet => o -> (W,S) -> (
-- IN:  W = witness set
--      S = equations of the new slice
-- OUT: new witness points
     if #S < dim W
     then error "dimension of new slicing plane is too high";
     R := ring W;
     st := equations W | take(slice W,-#S); -- take last #S equations
     ta := equations W | S;
     newpoints := track(st,ta,points W,Software=>o.Software,gamma=>exp(random(0.,2*pi)*ii));
     if #newpoints != #W#Points then error "number of points in the witness set changed";
     newpoints
     )

moveSlice = method(TypicalValue=>WitnessSet, Options =>{Software=>M2})
moveSlice (WitnessSet, Matrix) := WitnessSet => o -> (W,S) -> (
-- IN:  W = witness set
--      S = matrix defining a new slicing plane (same dimensions as W#Slice)
-- OUT: new witness set that uses S
     if numgens target S != numgens target W#Slice 
     or numgens source S != numgens source W#Slice 
     then error "wrong dimension of new slicing plane";
     witnessSet(W#Equations,S,movePointsToSlice(W,sliceEquations(S,ring W)))             	  
     )
///
restart
debug loadPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z]
W1 = new WitnessSet from {
     Equations=>ideal {x^2+y^2+z^2-1},
     Slice=>sub(matrix "1,0,0,0;0,1,0,0",R),
     Points=>{(0,0,1),(0,0,-1)}
     } 
sliceEquations (W1#Slice,R)
W2 = moveSlice(W1, sub(matrix "0,1,0,0;0,0,1,0",R)
     --, Software=>PHCpack
     )
///

splitWitness = method(TypicalValue=>Sequence, Options =>{Software=>M2, Tolerance=>1e-6})
splitWitness (WitnessSet,RingElement) := Sequence => o -> (w,f) -> (
-- splits the witness set into two parts: one contained in {f=0}, the other not
-- IN:  comp = a witness set
--      f = a polynomial
-- OUT: (w1,w2) = two witness sets   
     w1 := {}; w2 := {};
     for x in w#Points do 
	 if norm evalPoly(f,first x) < o.Tolerance 
	 then w1 = w1 | {x}
	 else w2 = w2 | {x};   
     ( if #w1===0 then null 
	  else witnessSet(w#Equations, w#Slice, w1), 
       if #w2===0 then null 
          else witnessSet(w#Equations, w#Slice, w2) )
     )

regeneration = method(TypicalValue=>List, Options =>{Software=>M2})
regeneration List := List => o -> F -> (
-- solves a system of polynomial Equations via regeneration     
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
	       print (#comp#Points);
	       (cIn,cOut) := splitWitness(comp,f); 
	       if cIn =!= null 
	       then c2 = c2 | { witnessSet(cIn#Equations 
			 -- + ideal f -- should we store additional equations?
			 , cIn#Slice, cIn#Points) }; 
     	       if cOut =!= null then (
		    dWS = apply(d, i->(
			      s := cOut#Slice;
			      newSlice := random(CC^1, CC^(numcols s)) || submatrix'(s,{0},{}); -- replace the first row
			      moveSlice(cOut,newSlice,Software=>o.Software)
			      ));
	       	    S := ( equations comp
	       	    	 | { product flatten apply( dWS, w->sliceEquations(w.Slice^{0},R) ) } -- product of linear factors
	       	    	 | sliceEquations( submatrix'(comp#Slice,{0},{}), R ) );
	       	    T := ( equations comp
	       	    	 | {f}
	       	    	 | sliceEquations( submatrix'(comp#Slice,{0},{}), R ) );
	       	    targetPoints := track(S,T,flatten apply(dWS,points), 
			 gamma=>exp(random(0.,2*pi)*ii), Software=>o.Software);
		    --if o.Software == M2 then targetPoints = refine(T, targetPoints, Tolerance=>1e-10);
		    if #targetPoints>0 
		    then c2 = c2 | { 
			 witnessSet(cOut#Equations + ideal f, submatrix'(comp#Slice,{0},{}), targetPoints)  
			 };
		    ); 
	       );
	  c1 = c2;
	  if #c1 === 0 then ( -- if the first equation is being processed 
	       n := numgens R;
	       S := randomSlice(n-1,n);
     	       c1 = { witnessSet(ideal f, S, solveSystem( {f} | sliceEquations(S,R), Software=>o.Software )) }; 
	       );
	  );
     c1	  
     )

-----------------------------------------------------------------------
-- AUXILIARY FUNCTIONS
projectiveDistance = method()
projectiveDistance (List,List) := (a,b) -> acos((abs sum(a,b,(x,y)->x*conjugate y)) / ((norm2 a) * (norm2 b)));

areEqual = method(TypicalValue=>Boolean, Options=>{Tolerance=>1e-6, Projective=>false})
areEqual (List,List) := o -> (a,b) -> (
     if class first a === List then (
	  #a == #b and all(#a, i->areEqual(a#i,b#i,o))
	  ) else (
     	  #a == #b and ( if o.Projective 
	       then projectiveDistance(a,b) < o.Tolerance
	       else norm2 (a-b) < o.Tolerance * norm2 a
	       )
	  )
     ) 
areEqual (CC,CC) := o -> (a,b) -> (
     abs(a-b) < o.Tolerance
     ) 
areEqual (Matrix,Matrix) := o -> (a,b) -> (
     areEqual(flatten entries a, flatten entries b, o)
     ) 

isGEQ := method(TypicalValue=>Boolean, Options=>{Tolerance=>1e-6})
isGEQ(List,List) := o->(t,s)-> (
     n := #t;
     for i from 0 to n-1 do ( 
	  if not areEqual(t#i,s#i, Tolerance=>o.Tolerance) 
	  then 
	  if abs(realPart t#i - realPart s#i) < o.Tolerance then 
	  return imaginaryPart t#i > imaginaryPart s#i
	  else return realPart t#i > realPart s#i
	  ); 
     true -- if approx. equal 
     )

sortSolutions = method(TypicalValue=>List, Options=>{Tolerance=>1e-6})
sortSolutions List := o -> sols -> (
-- sorts numerical solutions     
     if #sols == 0 then sols
     else (
	  sorted := {0};
	  scan(#sols-1, s->(
		    -- find the first element that is "larger";
		    -- "larger" means the first coord that is not (approx.) equal 
		    -- has (significantly) larger realPart, if tie then larger imaginaryPart
		    --l := position(sorted, t->isGEQ(first t, first s));
     	       	    s = s + 1;
		    t := first sols#s;
		    l := 0; r := #sorted-1;
		    if isGEQ(t, first sols#(sorted#r)) then  sorted = sorted | {s}
		    else if isGEQ(first sols#(sorted#l),t) then  sorted = {s} | sorted 
		    else (
		    	 while r-l>0 do (
			      m := (l+r)//2;
			      if isGEQ(first sols#(sorted#m), t) then r=m
			      else l=m+1; 
			      );
		    	 sorted = take(sorted,r) | {s} | drop(sorted,r);
		    	 )
		    ));      
	  );
     apply(sorted, i->sols#i)
     )

evalPoly = method(TypicalValue=>CC)
evalPoly (RingElement, List) := (f,x) -> (
     sub(sub(f, sub(matrix{x},ring f)), coefficientRing ring f)
     )

diffSolutions = method(TypicalValue=>Sequence, Options=>{Tolerance=>1e-3})
-- in:  A, B (presumably sorted)
-- out: (a,b), where a and b are lists of indices where A and B differ
diffSolutions (List,List) := o -> (A,B) -> (
     i := 0; j := 0;
     a := {}; b = {};
     while i<#A and j<#B do 
     if areEqual(A#i,B#j) then (i = i+1; j = j+1)
     else if isGEQ(A#i,B#j) then (b = append(b,j); j = j+1)
     else (a = append(a,i); i = i+1);	  
     (a|toList(i..#A-1),b|toList(j..#B-1))	      	    
     )


------------ preSLPs ------------------------------------------------------------------------
-- preSLP = (constants, program, output_description)
--   constants = list of elements in CC
--   program = node, node, ...
--     node = {binary_operation, a, b}
--         or {multi_operation, n, a1, ... , an}        
--         or {copy, a}                                 -- copies node n    
--       a,b,ai are 
--          negative integers (relative position) 
--          or const => i (refers to i-th constant)
-- 	    or in => i (refers to i-th input) 
--       binary_operation = {sum, product}
--       multi_operation = {msum, mproduct} 
--   output_description = Matrix over ZZ (each entry refers to a node)

libPREFIX = "/tmp/slpFN.";
slpCOMPILED = 100;
slpPREDICTOR = 101;
slpCORRECTOR = 102;
slpEND = 0;
slpCOPY = 1; --"COPY"; -- node positions for slpCOPY commands are absolute
slpMULTIsum = 2; --"MULTIsum";
slpPRODUCT = 3; --"PRODUCT";

-- types of predictors
predRUNGEKUTTA = 1;
predTANGENT = 2;
predEULER = 3;
predPROJECTIVENEWTON = 4;

shiftConstsSLP = method(TypicalValue=>List);
shiftConstsSLP (List,ZZ) := (slp,shift) -> apply(slp, 
     n->apply(n, b->
	  if class b === Option and b#0 === "const" 
     	  then "const"=>shift+b#1 
     	  else b
	  )
     );

poly2preSLP = method(TypicalValue=>Sequence)
poly2preSLP RingElement :=  g -> (
     prog := {}; -- our SLP
     R := ring g;
     const := coefficient(1_R,g);
     finalMULTIsum := {}; -- list of nodes for final multisum
     constants := if const == 0 then {} else ( finalMULTIsum = finalMULTIsum | {"const"=>0}; {const} );
     f := g - const;
     scan(numgens R, i->(
	       fnox := sum(select(listForm f,ec->(first ec)#i==0), (e,c)->c*R_e); -- fnox := f%R_i;
	       if fnox == 0 then fnox = 0_R;
	       fx := f - fnox;
	       if fx != 0 then (
		    fxOverRi := --fx//R_i
		    sum(listForm fx, (e,c)->c*R_(take(e,i)|{e#i-1}|drop(e,i+1)));
		    if fxOverRi == 0 then fxOverRi = 0_R;      
	       	    (constfx, progfx, outfx) := poly2preSLP(
			 fxOverRi
			 );
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
      (constants, prog, matrix{{#prog-1}})
     )


concatPreSLPs = method() -- concatenate pre-slps 
-- ( if 2 slp's output matrices A and B, their concatenation returns A|B )
concatPreSLPs List := S -> (
     c := {};
     p := {}; 
     o := null;
     scan(S, s->(
	       if o === null then o = s#2
	       else -- shift output by the current length of the program 
	            o = o | (s#2 + map(ZZ^(numgens target s#2), ZZ^(numgens source s#2), (i,j)->#p));  
	       p = p | apply(s#1, a->
		    apply(a, b->
			 if class b === Option and b#0 == "const" 
			 then b#0=>b#1+#c -- shift the constants
			 else b
			 ) 
		    );
	       c = c | s#0;
	       ));     
     (c,p,o)
     );

stackPreSLPs = method() -- stacks pre-slps (A||B)
stackPreSLPs List := S -> (
     c := {};
     p := {}; 
     o := null;
     scan(S, s->(
	       if o === null then o = s#2
	       else -- shift output by the current length of the program 
	            o = o || (s#2 + map(ZZ^(numgens target s#2), ZZ^(numgens source s#2), (i,j)->#p));  
	       p = p | apply(s#1, a->
		    apply(a, b->
			 if class b === Option and b#0 == "const" 
			 then b#0=>b#1+#c -- shift the constants
			 else b
			 ) 
		    );
	       c = c | s#0;
	       ));     
     (c,p,o)
     );
     
evaluatePreSLP = method() -- evaluates preSLP S at v
evaluatePreSLP (Sequence,List) := (S,v)-> (
     val := {};
     constants := S#0;
     slp := S#1;
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
          		       if class n#j === Option and n#j#0 == "const" then constants#(n#j#1)
			       else if class n#j === Option and n#j#0 == "in" then v#(n#j#1)
			       else if class n#j === ZZ then val#(i+n#j)
			       else error "unknown node type" 
			       ))
		     }
		)
	   else error "unknown SLP node key";   
	   ));
 matrix apply(entries S#2, r->apply(r, e->val#e))
 )

transposePreSLP = method() 
transposePreSLP(List,List,Matrix) := (C,slp,M) -> (C, slp, transpose M)

jacobianPreSLP = method() -- finds jacobian of S with respect to inputs listed in L
jacobianPreSLP (Sequence, List) := (S,L) -> (  
     constants := S#0 | {1_CC};
     slp := S#1;
     outMatrix := S#2;
     if numgens target outMatrix != 1 then error "preSLP: row vector expected";
     diffNodeVar := (ni,vj)->( 
	  -- augments slp with nodes necessary to differentiate node #ni w.r.t. input #vj 
	  -- output: the (absolute) position of the result in slp (or -1 "zero")
	  n := slp#ni;
	  k := first n;
	  if k === slpCOPY then (
	       if class n#1 === Option and n#1#0 == "const" 
	       then return -1 -- "zero"
	       else error "unknown node type"; 
	       )  
	  else if k === slpMULTIsum then (
	       pos := toList apply(2..1+n#1, j->if class n#j === Option and n#j#0 == "const" then -1 -- "zero"
		    else if class n#j === ZZ then diffNodeVar(ni+n#j,vj)
		    else error "unknown node type" 
		    );
	       summands := select(pos, p->p!=-1);
	       if #summands == 0 then return -1 -- "zero"
	       else if #summands == 1 then return first summands
	       else (
		    slp = slp | {{slpMULTIsum,#summands} | apply(summands, i->i-#slp)};
		    return (#slp-1);
		    )
	       )
	   else if k === slpPRODUCT then (
	       pos = toList apply(1..2, j->(
			 if class n#j === Option and n#j#0 == "in" then (
			      if n#j#1 == vj then ( 
     	       	    	      	   slp = slp | {{slpPRODUCT}|
					toList apply(1..2, t->if t==j 
					     then "const"=> #constants-1 -- "one"
					     else (
						  if class n#t === ZZ then ni+n#t-#slp
					     	  else n#t
						  )
					     )};
				   (#slp-1)
				   )
			      else -1 -- "zero"
			      )
			 else if class n#j === ZZ then (
			      p:=diffNodeVar(ni+n#j,vj);
			      if p==-1 then -1 -- "zero"
			      else (
			      	   slp = slp | {
				   	{slpPRODUCT}|
				   	toList apply(1..2, t->
					     if t==j 
					     then p-#slp 
					     else (
					     	  if class n#t === ZZ then ni+n#t-#slp
					     	  else n#t
					     	  )
					     )};
			      	   (#slp-1)
				   )
			      )
			 else error "unknown node type" 
			 ));
	       summands = select(pos, p->p!=-1);
	       if #summands == 0 then return -1 -- "zero"
	       else if #summands == 1 then return first summands
	       else (
		    slp = slp | {{slpMULTIsum,#summands} | apply(summands, p->p-#slp)};
		    return (#slp-1);
		    )
	       )
	   else error "unknown SLP node key";   
	  ); 
     newOut := transpose matrix apply(first entries outMatrix, ni->apply(L, vj->diffNodeVar(ni,vj)));
     constants = constants | {0_CC};
     slp = slp | {{slpCOPY, "const"=>#constants-1}};
     ( constants, slp,  
	 matrix apply(entries newOut, row->apply(row, i->if i==-1 then (#slp-1) else i)) ) 
     )

prunePreSLP = method() -- finds jacobian of S with respect to inputs listed in L
prunePreSLP (List,List,Matrix) := (C,slp,outMatrix) -> (
     -- look for duplicate constants
     newC := {};
     remap := apply(#C, i->(
	       p := position(newC,c->C#i==c);
	       if p =!= null 
	       then p
	       else ( 
		    newC = newC | {C#i};
		    #newC - 1
		    )
	       ));
     newslp := apply(slp, n->(
	   k := first n;
	   if k === slpCOPY then (
	   	if class n#1 === Option and n#1#0 == "const" then {n#0,"const"=>remap#(n#1#1)}
		else error "unknown node type"
		)  
	   else if k === slpMULTIsum then (
		{n#0,n#1} | toList apply(2..1+n#1, 
		     j -> if class n#j === Option and n#j#0 == "const" 
		     then "const"=>remap#(n#j#1) 
		     else n#j
		     )
	   	)
	   else if k === slpPRODUCT then (
		{n#0} | toList apply(1..2, j->
		     if class n#j === Option and n#j#0 == "const" 
		     then "const" => remap#(n#j#1)
		     else n#j
		     )
		)
	   else error "unknown SLP node key"   
	   ));
     	  (newC,newslp,outMatrix)
     )

-- create a file <fn>.cpp with C++ code for the function named fn that evaluates a preSLP S
-- format:
--   void fn(const complex* a, complex* b)
-- here: input array a
--       output array b 
preSLPtoCPP = method(TypicalValue=>Nothing, Options=>{System=>MacOsX})
preSLPtoCPP (Sequence,String) := o-> (S,filename)-> (
     constants := S#0;
     slp := S#1;
     fn := "slpFN"; -- function name
     f := openOut(filename);
     f << ///
#include<stdio.h>
#include<math.h>

class complex
{
private:
  double real;  // Real Part
  double imag;      //  Imaginary Part                                                                                                       
public:
  complex();
  complex(double,double);
  complex(const complex&);
  //complex(M2_CCC);
  complex operator +(complex);
  complex operator -(complex);
  complex operator *(complex);
  complex operator /(complex);
  complex getconjugate();
  complex getreciprocal();
  double getreal();
  double getimaginary();
  bool operator ==(complex);
  void operator =(complex);
  void sprint(char*);
};

complex::complex() { }
complex::complex(double r, double im)
{
  real=r;
  imag=im;
}
 
//                                 COPY CONSTRUCTOR
complex::complex(const complex &c)
{
  this->real=c.real;
  this->imag=c.imag;
}
 
void complex::operator =(complex c)
{
  real=c.real;
  imag=c.imag;
}
 
 
complex complex::operator +(complex c)
{
  complex tmp;
  tmp.real=this->real+c.real;
  tmp.imag=this->imag+c.imag;
  return tmp;
}
 
complex complex::operator -(complex c)
{
  complex tmp;
  tmp.real=this->real - c.real;
  tmp.imag=this->imag - c.imag;
  return tmp;
}
 
complex complex::operator *(complex c)
{
  complex tmp;
  tmp.real=(real*c.real)-(imag*c.imag);
  tmp.imag=(real*c.imag)+(imag*c.real);
  return tmp;
}
 
complex complex::operator /(complex c)
{
  double div=(c.real*c.real) + (c.imag*c.imag);
  complex tmp;
  tmp.real=(real*c.real)+(imag*c.imag);
  tmp.real/=div;
  tmp.imag=(imag*c.real)-(real*c.imag);
  tmp.imag/=div;
  return tmp;
}
complex complex::getconjugate()
{
  complex tmp;
  tmp.real=this->real;
  tmp.imag=this->imag * -1;
  return tmp;
}
 
complex complex::getreciprocal()
{
  complex t;
  t.real=real;
  t.imag=imag * -1;
  double div;
  div=(real*real)+(imag*imag);
  t.real/=div;
  t.imag/=div;
  return t;
}
 
double complex::getreal()
{
  return real;
}
 
double complex::getimaginary()
{
  return imag;
}
 
bool complex::operator ==(complex c)
{
  return (real==c.real)&&(imag==c.imag) ? 1 : 0;
}

void complex::sprint(char* s)
{
  sprintf(s, "(%lf) + i*(%lf)", real, imag);
}
/// << endl;
     if o.System === MacOsX then f << ///#define EXPORT __attribute__((visibility("default")))/// <<endl;
     if o.System === MacOsX then f << /// extern "C" EXPORT ///;
     f << "void " << fn << "(complex* a, complex* b)" << endl  
     << "{" << endl 
     << "  complex ii(0,1);" << endl
     << "  complex c[" << #constants << "]; " << endl
     << "  complex node[" << #slp << "];" << endl
     << "  complex* n = node;" << endl; -- current node      
     -- hardcode the constants
     scan(#constants, i-> f << "c[" << i << "] = " << "complex(" <<
	  realPart constants#i << "," << imaginaryPart constants#i << ");" << endl);
     scan(#slp, i->(
	   n := slp#i;
	   k := first n;
	   f << "  *n = ";
	   if k === slpCOPY then (
	   	if class n#1 === Option and n#1#0 == "const" 
		then f << "c[" << n#1#1 << "];" 
		else error "unknown node type"; 
		)  
	   else if k === slpMULTIsum then (
		scan(2..1+n#1, j->(
			  if class n#j === Option and n#j#0 == "const" 
			  then f << "c[" << n#j#1 << "]"
			  else if class n#j === ZZ 
			  then f << "node[" << i+n#j << "]"
		     	  else error "unknown node type";
			  if j < 1+n#1 then f << " + ";
		     	  ));
		f << ";";
		)
	   else if k === slpPRODUCT then (
		scan(1..2, j->(
			  if class n#j === Option then (
			       if n#j#0 == "in" 
			       then f << "a[" << n#j#1 << "]"
			       else if n#j#0 == "const"
			       then f << "c[" << n#j#1 << "]"
			       else error "unknown node type"
			       ) 
			  else if class n#j === ZZ 
			  then f << "node[" << i+n#j << "]"
			  else error "unknown node type";
			  if j < 2 then f << " * "; 
			  ));
		f << ";";
	   	)
	   else error "unknown SLP node key";   
	   f << " n++;" << endl
	   ));
      f << "  // creating output" << endl << "  n = b;" << endl;
      scan(flatten entries S#2, e->(
	   	f << "  *(n++) = node[" << e << "];" << endl 		
		));
      f << "}" << endl << close; 	              
      )

-- create a file <fn>.c with C code for the function named fn that evaluates a preSLP S
-- format:
--   void fn(const complex* a, complex* b)
-- here: input array a
--       output array b 
preSLPtoC = method(TypicalValue=>Nothing, Options=>{System=>MacOsX})
preSLPtoC (Sequence,String) := o-> (S,filename)-> (
     constants := S#0;
     slp := S#1;
     fn := "slpFN"; -- function name
     f := openOut(filename);
     f << ///
#include<stdio.h>
#include<math.h>

typedef struct 
{
  double re;  
  double im;  
} complex;

inline void init_complex(complex* c, double r, double i) __attribute__((always_inline));
void init_complex(complex* c, double r, double i)
{ c->re = r; c->im = i; }

/* #define init_complex(c,r,i) { c->re = r; c->im = i; } */

/* register */ 
static double r_re, r_im; 

inline set_r(complex c) __attribute__((always_inline));
inline set_r(complex c) 
{ r_re = c.re; r_im = c.im; }

/* #define set_r(c) { r_re = c.re; r_im = c.im; } */

inline copy_r_to(complex* c) __attribute__((always_inline));
inline copy_r_to(complex* c) 
{ c->re = r_re; c->im = r_im; }

/* #define copy_r_to(c) { c->re = r_re; c->im = r_im; } */

inline add(complex c) __attribute__((always_inline));
inline add(complex c)
{ r_re += c.re; r_im += c.im; }

/* #define add(c) { r_re += c.re; r_im += c.im; } */

inline mul(complex c) __attribute__((always_inline));
inline mul(complex c)
{ 
  double t_re = r_re*c.re - r_im*c.im;
  r_im = r_re*c.im + r_im*c.re;
  r_re = t_re;
}

/*#define mul(c) { double t_re = r_re*c.re - r_im*c.im; r_im = r_re*c.im + r_im*c.re; r_re = t_re; } */

/// << endl;
     -- if o.System === MacOsX then f << ///#define EXPORT __attribute__((visibility("default")))/// <<endl;
     -- if o.System === MacOsX then f << /// extern "C" EXPORT ///;
     f << "void " << fn << "(complex* a, complex* b)" << endl  
     << "{" << endl 
     << "  complex c[" << #constants << "]; " << endl
     << "  complex node[" << #slp << "];" << endl
     << "  complex* cp = c;" << endl
     << "  complex* n = node;" << endl; -- current node      
     -- hardcode the constants
     scan(#constants, i-> f <<  "init_complex(cp," <<
	  realPart constants#i << "," << imaginaryPart constants#i << "); cp++;" << endl);
     scan(#slp, i->(
	   n := slp#i;
	   k := first n;
	   if k === slpCOPY then (
	   	if class n#1 === Option and n#1#0 == "const" 
		then f << "  *n = c[" << n#1#1 << "];" 
		else error "unknown node type"; 
		)  
	   else if k === slpMULTIsum then (
		scan(2..1+n#1, j->(
			  if class n#j === Option and n#j#0 == "const" 
			  then f << (if j>2 then "add" else "set_r") << "(c[" << n#j#1 << "]); "
			  else if class n#j === ZZ 
			  then f << (if j>2 then "add" else "set_r") << "(node[" << i+n#j << "]); "
		     	  else error "unknown node type";
		     	  ));
		f << "copy_r_to(n);";
		)
	   else if k === slpPRODUCT then (
		scan(1..2, j->(
			  if class n#j === Option then (
			       if n#j#0 == "in" 
			       then f << (if j>1 then "mul" else "set_r") << "(a[" << n#j#1 << "]); "
			       else if n#j#0 == "const"
			       then f << (if j>1 then "mul" else "set_r") << "(c[" << n#j#1 << "]); "
			       else error "unknown node type"
			       ) 
			  else if class n#j === ZZ 
			  then f << (if j>1 then "mul" else "set_r") << "(node[" << i+n#j << "]); "
			  else error "unknown node type";
			  ));
		f << "copy_r_to(n);";
	   	)
	   else error "unknown SLP node key";   
	   f << " n++;" << endl
	   ));
      f << "  // creating output" << endl << "  n = b;" << endl;
      scan(flatten entries S#2, e->(
	   	f << "  *(n++) = node[" << e << "];" << endl 		
		));
      f << "}" << endl << close; 	              
      )

///
restart
loadPackage "NumericalAlgebraicGeometry"; debug NumericalAlgebraicGeometry;
R = CC[x,y,z]
g = 3*y^2+(2.1+ii)*x
--g = 1 + 2*x^2 + 3*x*y^2 + 4*z^2
--g = random(3,R)
pre = poly2preSLP g
g3 = concatPreSLPs {pre,pre,pre}
g6 = stackPreSLPs {g3,g3}
eg = evaluatePreSLP(g6,gens R)
eg_(1,1) == g
--preSLPtoCPP(g6,"slpFN")
debug Core
(constMAT, prog) = fromPreSLP(3,g6)
rSLP = rawSLP(raw constMAT, prog)
K = CC_53
params = matrix{{ii_K,1_K,-1_K}}; 
result = rawEvaluateSLP(rSLP, raw params)
sub(g, params) - (map(K,result))_(0,0)

///
----------------- SLPs -----------------------------------------------------
-- SLP = (constants, array of ints)
-- constants = one-row matrix
-- array of ints = 
--0  #constants
--1  #inputs 
--2  #rows in output
--3  #columns in output
--4  type of program (slpCOMPILED,slpINTERPRETED,slpPREDICTOR) 
--   OR the beginning of slp operations list
--
--   if COMPILED then {
--5     integer -> used to create the dynamic library filename
--   }
--   else if PREDICTOR then {
--5     predictor type  
--6+    list of catalog numbers of SLPs for Hx,Ht,H
--   } else {
--     list of commands
--     output matrix entries (numbers of nodes) 
--   }
  
preSLPinterpretedSLP = method()
preSLPinterpretedSLP (ZZ,Sequence) := (nIns,S) -> (
-- makes input for rawSLP from pre-slp
     consts := S#0;
     slp := S#1;   
     o := S#2;
     SLPcounter = SLPcounter + 1;
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
          		       if class n#j === Option then (
				    if n#j#0 == "in" then #consts + n#j#1
				    else if n#j#0 == "const" then n#j#1
				    else error "unknown node type"
				    ) 
			       else if class n#j === ZZ then curNode+n#j
			       else error "unknown node type" 
			       ))
		)
	   else error "unknown SLP node key";   
	   curNode = curNode + 1;
	   ));
     p = {#consts,nIns,numgens target o, numgens source o} | p | {slpEND}
	 | apply(flatten entries o, e->e+#consts+nIns); 
     (map(CC^1,CC^(#consts), {consts}), p)
     )

preSLPcompiledSLP = method(TypicalValue=>Nothing, Options=>{System=>MacOsX, Language=>LanguageC})
preSLPcompiledSLP (ZZ,Sequence) := o -> (nIns,S) -> (
-- makes input for rawSLP from pre-slp
     consts := S#0;
     slp := S#1;   
     out := S#2;
     fname := SLPcounter; SLPcounter = SLPcounter + 1; -- this gives libraries distinct names 
                                                       -- the name of the function stays the same, should it change?
     curNode = #consts+nIns;
     p = {#consts,nIns,numgens target out, numgens source out} | {slpCOMPILED}
          | { fname }; -- "lib_name" 
     cppName := libPREFIX | toString fname | if o.Language === LanguageCPP then ".cpp" else ".c";
     libName := libPREFIX | toString fname | if o.System === Linux then ".so" else  ".dylib";
     (if o.Language === LanguageCPP then preSLPtoCPP else preSLPtoC) (S, cppName, System=>o.System);
     compileCommand := if o.System === Linux then "gcc -shared -Wl,-soname," | libName | " -o " | libName | " " | cppName | " -lc -fPIC"
     else if o.System === MacOsX and version#"pointer size" === 8 then "g++ -m64 -dynamiclib -O2 -o " | libName | " " | cppName
     else if o.System === MacOsX then (
     	  "gcc -dynamiclib -O1 -o " | libName | " " | cppName
	  )
     else error "unknown OS";
     print compileCommand;
     run compileCommand;      
     (map(CC^1,CC^(#consts), {consts}), p)
     )

NAGtrace = method()
NAGtrace ZZ := l -> (gbTrace=l; oldDBG=DBG; DBG=l; oldDBG);

-- normalized condition number of F at x
conditionNumber = method(Options=>{Variant=>OrthogonalProjection})
conditionNumber (List,List) := o -> (F,x) -> (
     nF := apply(F, f->f/sqrt(#F * BombieriWeylNormSquared f)); -- normalize F
     x0 := normalize transpose matrix{x}; -- column unit vector
     DMforPN := diagonalMatrix(nF/(f->1/sqrt first degree f) | if o.Variant===OrthogonalProjection then {1} else {});
     J := sub(transpose jacobian matrix{nF}, transpose sub(x0,CC)); -- Jacobian of F at x
     if o.Variant===OrthogonalProjection then J = J || matrix{ flatten entries x0 / conjugate};
     1 / min first SVD(DMforPN*J) --  norm( Moore-Penrose pseudoinverse(J) * diagonalMatrix(sqrts of degrees) )     
     )

beginDocumentation()
load "./NumericalAlgebraicGeometry/doc.m2"

TEST ///
     --assert(multistepPredictor(2_QQ,{0,0,0}) === {-3/8, 37/24, -59/24, 55/24}) -- Wikipedia: Adams-Bashforth
     --assert(multistepPredictor(2_QQ,{-1}) === {-1/8, 5/8}) -- computed by hand
     --assert(flatten entries (coefficients first multistepPredictorLooseEnd(2_QQ,{0,0,0}))#1=={1/120, 1/16, 11/72, 1/8})
     load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/SoftwareM2.tst.m2")
     load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/SoftwareM2engine.tst.m2")
     load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/SoftwareM2enginePrecookedSLPs.tst.m2")
///

end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
loadPackage "NumericalAlgebraicGeometry"
uninstallPackage "NumericalAlgebraicGeometry"
installPackage("NumericalAlgebraicGeometry", SeparateExec=>true, AbsoluteLinks=>false)
installPackage "NumericalAlgebraicGeometry"
check "NumericalAlgebraicGeometry"

R = CC[x,y,z]
f1 = (y-x^2)*(x^2+y^2+z^2-1)*(x-0.5);
f2 = (z-x^3)*(x^2+y^2+z^2-1)*(y-0.5);
f3 = (y-x^2)*(z-x^3)*(x^2+y^2+z^2-1)*(z-0.5);


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=PackageTemplate pre-install"
-- End:
