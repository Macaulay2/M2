-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version

BERTINI'M2'EXISTS := fileExists(currentFileDirectory | "Bertini.m2") -- remove when Bertini is distributed
if version#"VERSION" <= "1.4" then needsPackage "NAGtypes"
if version#"VERSION" <= "1.4" then needsPackage "PHCpack"
PHC'EXISTS := (version#"VERSION" > "1.4")
--if BERTINI'M2'EXISTS and version#"VERSION" <= "1.4" then needsPackage "Bertini"

newPackage select((
     "NumericalAlgebraicGeometry",
     Version => "1.4",
     Date => "March, 2011",
     Headline => "Numerical Algebraic Geometry",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     Configuration => { "PHCPACK" => "phc",  "BERTINI" => "bertini", "HOM4PS2" => "hom4ps2" },	
     if version#"VERSION" > "1.4" then PackageExports => {"NAGtypes"},
     if version#"VERSION" > "1.4" then PackageImports => {"PHCpack"} | if BERTINI'M2'EXISTS then {"Bertini"} else {},
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     DebuggingMode => true,
--     DebuggingMode => false,
     Certification => {
	  "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	  "journal URI" => "http://j-sag.org/",
	  "article title" => "Numerical Algebraic Geometry",
	  "acceptance date" => "2011-05-20",
	  "published article URI" => "http://j-sag.org/Volume3/jsag-2-2011.pdf",
	  "published code URI" => "http://j-sag.org/Volume3/NumericalAlgebraicGeometry.tar",
	  "repository code URI" => "svn://svn.macaulay2.com/Macaulay2/trunk/M2/Macaulay2/packages/NumericalAlgebraicGeometry.m2",
	  "release at publication" => 13254,	    -- as an integer
	  "version at publication" => "1.4",
	  "volume number" => "3",
	  "volume URI" => "http://j-sag.org/Volume3/"
	  }
     ), x -> x =!= null)

if version#"VERSION" <= "1.4" then needsPackage "NAGtypes"

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
     "setDefault", "getDefault",
     "solveSystem", "track", "refine", "totalDegreeStartSystem",
     -- "multistepPredictor", "multistepPredictorLooseEnd",
     "Software", "PostProcess", "PHCPACK", "BERTINI","HOM4PS2","M2","M2engine","M2enginePrecookedSLPs",
     "gamma","tDegree","tStep","tStepMin","stepIncreaseFactor","numberSuccessesBeforeIncrease",
     "Predictor","RungeKutta4","Multistep","Tangent","Euler","Secant","MultistepDegree","Certified",
     "EndZoneFactor", "maxCorrSteps", "InfinityThreshold",
     "Normalize", "Projectivize",
     "AffinePatches", "DynamicPatch",
     "SLP", "HornerForm", "CompiledHornerForm", "CorrectorTolerance", "SLPcorrector", "SLPpredictor",
     "NoOutput",
     "randomSd", "goodInitialPair", "randomInitialPair", "GeneralPosition",
     "Bits", "Iterations", "ErrorTolerance", "ResidualTolerance",
     "Attempts", "SingularConditionNumber", 
     "numericalRank", "toAffineChart",
     "regeneration", 
     "Output", -- may rename/remove later
     "NAGtrace"
     }
exportMutable {
     }

-- local functions/symbols:
protect Processing, protect Undetermined -- possible values of SolutionStatus
protect SolutionAttributes -- option of getSolution 
protect Tracker -- an internal key in Point 
-- experimental:
protect AllowSingular -- in movePoints, regeneration
protect LanguageCPP, protect MacOsX, protect System, 
protect LanguageC, protect Linux, protect Language
protect DeflationSequence, protect DeflationRandomMatrix
protect MaxNumberOfVariables

-- DEBUG CORE ----------------------------------------
debug Core; -- to enable engine routines

-- ./NumericalAlgebraicGeometry/ FILES -------------------------------------
--if PHC'EXISTS then 
load "./NumericalAlgebraicGeometry/PHCpack/PHCpack.interface.m2" 
--else phcSolve = trackPaths = refineSolutions = null
if BERTINI'M2'EXISTS then load "./NumericalAlgebraicGeometry/Bertini/Bertini.interface.m2" else trackBertini = solveBertini = cleanupOutput = bertiniPosDimSolve = null

-- GLOBAL VARIABLES ----------------------------------
--PHCexe = NumericalAlgebraicGeometry#Options#Configuration#"PHCPACK";
BERTINIexe = NumericalAlgebraicGeometry#Options#Configuration#"BERTINI";
HOM4PS2exe = NumericalAlgebraicGeometry#Options#Configuration#"HOM4PS2";

DBG = 0; -- debug level (10=keep temp files)
SLPcounter := 0; -- the number of compiled SLPs (used in naming dynamic libraries)
lastPathTracker := null; -- path tracker object used last

DEFAULT = new MutableHashTable from {
     Software=>M2engine, NoOutput=>false, 
     NumericalAlgebraicGeometry$gamma=>1, 
     NumericalAlgebraicGeometry$tDegree=>1,
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
     SLP => false, -- possible values: false, HornerForm, CompiledHornerForm 	  
     -- refine options 
     ErrorTolerance => 1e-8,
     ResidualTolerance => 1e-8,
     Iterations => 30, 
     Bits => 300,
     -- general
     Attempts => 5, -- max number of attempts (e.g., to find a regular path)
     Tolerance => 1e-6,
     SingularConditionNumber => 1e4,
     MaxNumberOfVariables => 50
     }

setDefault = method(Options => {
     Software=>null, 
     NoOutput=>null, 
     NumericalAlgebraicGeometry$gamma=>null, 
     NumericalAlgebraicGeometry$tDegree=>null,
     -- step control
     tStep=>null, -- initial
     tStepMin=>null,
     stepIncreaseFactor=>null,
     numberSuccessesBeforeIncrease=>null,
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
     -- projectivize and normalize
     Normalize => null, -- normalize in the Bombieri-Weyl norm
     Projectivize => null, 
     AffinePatches => null,
     -- slp's 
     SLP => null, -- possible values: null, HornerForm, CompiledHornerForm 	  
     -- refine options 
     ErrorTolerance => null,
     ResidualTolerance => null,
     Iterations => null,
     Bits => null,
     -- general
     Attempts => null, -- max number of attempts (e.g., to find a regular path)
     Tolerance => null,
     SingularConditionNumber => null
     })
installMethod(setDefault, o -> () -> scan(keys o, k->if o#k=!=null then DEFAULT#k=o#k))
getDefault = method()
getDefault Symbol := (s)->DEFAULT#s

-- CONVENTIONS ---------------------------------------

-- Polynomial systems are represented as lists of polynomials.

-- OLD FORMAT:
-- Solutions are lists {s, a, b, c, ...} where s is list of coordinates (in CC)
-- and a,b,c,... contain extra information, e.g, SolutionStatus=>Regular indicates the solution is regular.
-- NEW FORMAT:
-- Solutions are of type Point (defined in NAGtypes).
 
-- M2 tracker ----------------------------------------
integratePoly = method(TypicalValue => RingElement)
integratePoly (RingElement,Number,Number) := RingElement => (f,a,b) -> (
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
		       

multistepPredictor = method(TypicalValue => List)
multistepPredictor (QQ,List) := List => memoize((c,s) -> (
-- coefficients for a multistep predictor
-- IN:  c = step adjustment coefficient (in QQ)
--      s = list of step adjustments (from the stepsize h = t_1-t_0)
--          s#i =  1 => t_(i+2)-t_(i+1) = c^(s#j)*(t_(i+1)-t_i)
-- OUT: b = coefficients in Adams-Bashforth-like method (list of rational numbers)
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
     apply(n, i->(
	       -- lagrange poly
	       T := symbol T;
	       RT := R[T];
	       allBUTi := toList(0..i-1) | toList(i+1..n-1);
	       L := sub((1/product(allBUTi, j->t_i-t_j))*product(allBUTi, j->T-t_j),RT);
	       -- << "i = " << i << "  L = " << L << endl;
	       integratePoly(L,t_(n-1),t_n)
	       ))
     ))

multistepPredictorLooseEnd = method(TypicalValue => List)
multistepPredictorLooseEnd (QQ,List) := List => memoize((c,s) -> (
-- coefficients for a multistep predictor with intederminate last step
-- IN:  c = step adjustment coefficient (in QQ)
--      s = list of step adjustments (from the initial stepsize h = t_1-t_0)
-- OUT: b = list of polinomials in QQ[a], where a=(last step size)/(next to last stepsize)   
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
     apply(n, i->(
	       -- lagrange poly
	       T := symbol T;
	       RT := R[T];
	       allBUTi := toList(0..i-1) | toList(i+1..n-1);
	       L := sub((1/product(allBUTi, j->lift(t_i-t_j,QQ)))*product(allBUTi, j->T-t_j),RT);
	       -- << "i = " << i << "  L = " << L << endl;
	       integratePoly(L,t_(n-1),t_n)
	       ))
     ))

inverseMatrix = method() -- since inverse(Matrix) does not work for matrices with CC entries. 
inverseMatrix Matrix := M -> (
     n := numRows M;
     if n!=numColumns M then error "square matrix expected";
     solve(M, map CC^n) 
     )

norm2 = method(TypicalValue=>RR) -- 2-norm of a vector with CC entries
norm2 List := v -> sqrt(sum(v, x->x*conjugate x));
norm2 Matrix := v -> norm2 flatten entries v

normF = method(TypicalValue=>RR) -- Frobenius norm of a matrix
normF Matrix := M -> max first SVD M;
     

normalize = method(TypicalValue => Matrix) -- normalizes a column vector with CC entries
normalize Matrix := v -> (1/norm2 v)*v

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
	  -- projectivize and normalize
	  Normalize => null, -- normalize in the Bombieri-Weyl norm
	  Projectivize => null, 
	  AffinePatches => null,
	  -- slp's 
	  SLP => null -- possible values: false, HornerForm, CompiledHornerForm 	  
	  } )
track (List,List,List) := List => o -> (S,T,solsS) -> (
-- tracks solutions from start system to target system
-- IN:  S = list of polynomials in start system
--      T = list of polynomials in target system
--      solsS = list of solutions to S
-- 	gamma => nonzero complex number
-- OUT: solsT = list of target solutions corresponding to solsS
     o = new MutableHashTable from o;
     scan(keys o, k->if o#k===null then o#k=DEFAULT#k); 
     o = new OptionTable from o;
     HISTORY := DBG>1 or member(o.Predictor, {Multistep,Secant});
     n := #T; 
     K := CC_53; -- THE coefficient ring
     
     if n > 0 then R := ring first T else error "expected nonempty target system";
     if not instance(R, PolynomialRing) then error "expected input in a polynomial ring"; 
     coeffR := coefficientRing R; 
     if #S != n then error "expected same number of polynomials in start and target systems";
     if not(
	  instance(ring 1_coeffR, ComplexField) 
	  or instance(ring 1_coeffR, RealField)
	  or coeffR===QQ or coeffR ===ZZ
	  ) then error "expected coefficients that can be converted to complex numbers";  
     if any(S, f->ring f =!= R) or any(T, f->ring f =!= R)
     then error "expected all polynomials in the same ring";
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
	       patches = patches | { o.NumericalAlgebraicGeometry$gamma*patches#1 };
     	       if DBG>1 then << "affine patch: " << toString patches#1 <<endl;
	       T = T | {patchEquation patches#1};
	       S = S | {patchEquation patches#2};
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
     then (apply(S, s->s/sqrt(#S * BombieriWeylNormSquared s)), apply(T, s->s/sqrt(#T * BombieriWeylNormSquared s)))
     else (S,T);
     
     if o.Predictor===Certified or (isProjective and o.Software===M2engine)
     -- in both cases a linear homotopy on the unit sphere is performed
     then (
	  nT = (o.NumericalAlgebraicGeometry$gamma/abs(o.NumericalAlgebraicGeometry$gamma))*nT;
	  H := {matrix{nS},matrix{nT}}; -- a "linear" homotopy is cooked up at evaluation using nS and nT
	  DMforPN := diagonalMatrix append(T/(f->1/sqrt first degree f),1);
	  maxDegreeTo3halves := power(max(T/first@@degree),3/2);
	  reBW'ST := realPart sum(#S, i->BombieriWeylScalarProduct(nS#i,nT#i));-- real Bombieri-Weyl scalar product
	  sqrt'one'minus'reBW'ST'2 :=  sqrt(1-reBW'ST^2);
	  bigT := asin sqrt'one'minus'reBW'ST'2; -- the linear homotopy interval is [0,bigT]
	  if reBW'ST < 0 then bigT = pi-bigT; -- want: sgn(cos)=sgn(reBW'ST) 
	  Hx := H/transpose@@jacobian; -- store jacobians (for evalHx)
	  if DBG>4 then << "Re<S,T> = " << reBW'ST << ", bigT = " << bigT << endl; 
     	  )	  
     else (
     	  H = matrix {apply(#S, i->o.NumericalAlgebraicGeometry$gamma*(1-t)^(o.NumericalAlgebraicGeometry$tDegree)*sub(nS#i,Rt)+t^(o.NumericalAlgebraicGeometry$tDegree)*sub(nT#i,Rt))};
     	  JH := transpose jacobian H; 
     	  Hx = JH_(toList(0..n-1));
     	  Ht := JH_{n};
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
		    -- default dt; Certified and Multistep modify dt
		    dt = if endZone then min(tStep, 1-t0) else min(tStep, 1-o.EndZoneFactor-t0);

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
	       if PT=!=null then p.Tracker=PT;
	       p
	       ))
     )

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
     o = new MutableHashTable from o;
     scan(keys o, k->if o#k===null then o#k=DEFAULT#k); o = new OptionTable from o;
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
     if not isProjective then (
     	  if o.Software === M2engine then ( -- engine refiner is primitive
--      	       PT := if class first solsT === Point and (first solsT).?Tracker then (first solsT).Tracker else null;
--                if PT=!=null then (
--  		    ref'sols = apply(entries map(CC_53, 
--  		    	      rawRefinePT(PT, raw matrix solsT, o.ErrorTolerance, o.Iterations)
--  		    	      ), s->{s}); -- old format
-- 		    );
-- 	       error "refine is not implemented in the engine yet";
	       ) 
	  else if o.Software === PHCPACK then (
	       ref'sols = refinePHCpack(T,solsT,o)/point
	       );
	  );
     -- M2 part 
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
			      done = numgens ring first dT > DEFAULT.MaxNumberOfVariables;
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


-- possible solution statuses returned by engine
solutionStatusLIST := {Undetermined, Processing, Regular, Singular, Infinity, MinStepFailure}

getSolution = method(Options =>{SolutionAttributes=>(Coordinates, SolutionStatus, LastT, ConditionNumber, NumberOfSteps)})
getSolution(Thing, ZZ) := Thing => o -> (PT,i) -> (
-- gets specified solution from the engine (not exported anymore)
-- IN:  (rawPathTracker, solution's number)
--      SolutionAttributes=> ... specifies various data attached to a solution ...
-- OUT: whatever is requested by SolutionAttributes (either as a sequence of as a single element)
     --if lastPathTracker === null 
     --then error "path tracker is not set up";
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

isRegular = method()
-- isRegular ZZ := (s) -> getSolution(s,SolutionAttributes=>SolutionStatus) == Regular  
isRegular Point := (s) ->  s.SolutionStatus === Regular
isRegular (List, ZZ) := (sols, s) -> isRegular sols#s

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
	  then isH := true
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

randomOrthonormalRows = method() -- return a random n-by-r matrix with orthonormal columns
randomOrthonormalRows(ZZ,ZZ) := (n,r) -> 
if n<r or r<1 then error "wrong input" else (randomUnitaryMatrix n)^(toList(0..r-1))

randomOrthonormalCols = method() -- return a random r-by-n matrix with orthonormal rows
randomOrthonormalCols(ZZ,ZZ) := (n,r) -> 
if n<r or r<1 then error "wrong input" else (randomUnitaryMatrix n)_(toList(0..r-1))

squareUpSystem = method() -- squares up a polynomial system (presented as a one-column matrix)
squareUpSystem Matrix := M -> (
     if numcols M != 1 then error "one-column matrix expected";
     n := numgens ring M;
     m := numrows M;
     if m<=n then "overdetermined system expected";
     sub(randomOrthonormalRows(m,n),ring M)*M
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
solveSystem = method(TypicalValue => List, Options =>{Software=>null, PostProcess=>true})
solveSystem List := List => o -> F -> (
-- solves a system of polynomial equations
-- IN:  F = list of polynomials
--      Software => {PHCPACK, BERTINI, hom4ps2}
-- OUT: {s,m}, where 
--             s = list of solutions 
--     	       m = list of corresponding multiplicities	 
     o = new MutableHashTable from o;
     scan(keys o, k->if o#k===null then o#k=DEFAULT#k); o = new OptionTable from o;
     local result;
     R := ring F#0;
     v := flatten entries vars R;
     if numgens R > #F then error "expected a 0-dimensional system";
     if member(o.Software, {M2,M2engine,M2enginePrecookedSLPs}) then ( 
	  overdetermined := numgens R < #F; 
	  T := (if overdetermined 
	       then generalEquations(numgens R, F)
	       else F);  
  	  result = 
-- 	  if all(F, f -> first degree f <= 1)
--      	  then ( 
-- 	       A := matrix apply(F, f->apply(v, x->coefficient(x,f)));
-- 	       b := matrix apply(F, f->{coefficient(1_R,f)});
-- 	       {{flatten entries solve(A,-b)}}
-- 	       )
--	  else 
	       (
	       (S,solsS) := totalDegreeStartSystem T;
	       track(S,T,solsS,NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii),Software=>o.Software)
	       );
	  if o.PostProcess and not overdetermined 
	  then (
	       result = select(refine(F,result), s->residual(F,s)<DEFAULT.Tolerance);
	       result = solutionsWithMultiplicity result;
	       -- below is a hack!!!
	       scan(result, s->if status s =!= Regular and s.ErrorBoundEstimate < DEFAULT.ErrorTolerance then (
			 if DBG>1 then print "path jump occured";
			 s.Multiplicity = 1;
			 s.SolutionStatus = Regular;
			 ));
	       )
	  )
     else if o.Software == PHCPACK then result = solvePHCpack(F,o)
     else if o.Software == BERTINI then result = solveBertini(F,o)
     else if o.Software == HOM4PS2 then (
	  -- newR := coefficientRing R[xx_1..xx_(numgens R)];
	  (name, p) := makeHom4psInput(R, F);
	  targetfile := name; --(map(newR, R, gens newR)\F);
	  tarsolfile := temporaryFileName() | 
	                "tasols";
 	  run(HOM4PS2exe|" "|targetfile|" "|tarsolfile);
	  sols := readSolutionsHom4ps(tarsolfile, p);
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

------------------------------------------------------------------------------------------
-- NAG witness set extra functions  
WitnessSet.Tolerance = 1e-6;
check WitnessSet := o -> W -> for p in points W do if norm sub(matrix{equations W | slice W}, matrix {p})/norm p > 1000*DEFAULT.Tolerance then error "check failed" 
isContained = method()
isContained (List,WitnessSet) := (point,W) -> (
     pts := movePointsToSlice(W, sliceEquations(randomSlice(dim W, numgens ring W, point),ring W)) / coordinates;
     any(pts, p->areEqual(point,p,Tolerance=>WitnessSet.Tolerance))
     )
isContained (WitnessSet,WitnessSet) := (V,W) -> (
     coD := dim W - dim V;
     coD >= 0
     and all(points V, p->isContained(p,W))
     )
-- subtract = method()
-- subtract (WitnessSet, WitnessSet) 
WitnessSet - WitnessSet := (V,W) -> ( -- difference V/W, also used to remove junk points
     coD := dim W - dim V;
     if coD < 0 then V
     else witnessSet(V.Equations, V.Slice, select(V.Points, p->not isContained(coordinates p,W)))
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

randomSlice = method()
randomSlice (ZZ,ZZ) := (d,n) -> randomSlice(d,n,{})
randomSlice (ZZ,ZZ,List) := (d,n,point) -> (
     SM := (randomUnitaryMatrix n)^(toList(0..d-1));
     SM | (if #point==0
	  then random(CC^d,CC^1)    
	  else -SM * transpose matrix{point} -- slice goes thru point
	  )
     )

movePoints = method(Options=>{AllowSingular=>false})
movePoints (List, List, List, List) := List => o -> (E,S,S',w) -> (
-- IN:  E = equations, 
--      S = equations of the current slice,
--      S' = equations of the new slice,
--      w = points satisfying E and S (in the output format of track) 
--      AllowSingular => false: S' is generic, several attempts are made to get regular points
-- OUT: new witness points satisfying E and S'
     attempts := DEFAULT.Attempts;
     success := false;
     while (not success and attempts > 0) do (
	  attempts = attempts - 1;
	  w' := track(E|S, E|S', w,NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii)); 
	  success = o.AllowSingular or all(toList(0..#w'-1), p->isRegular(w',p));
	  );
     if attempts == 0 and not success then error "some path is singular generically";  
     w'
     )

movePointsToSlice = method(TypicalValue=>List)
movePointsToSlice (WitnessSet, List) := List => (W,S') -> (
-- IN:  W = witness set
--      S' = equations of the new slice
-- OUT: new witness points
     if #S' < dim W
     then error "dimension of new slicing plane is too high";
     R := ring W;
     S := take(slice W,-#S'); -- take last #S equations
     movePoints(equations W, S, S', points W, AllowSingular=>true)
     )

moveSlice = method(TypicalValue=>WitnessSet)
moveSlice (WitnessSet, Matrix) := WitnessSet => (W,S) -> (
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
     )
///

splitWitness = method(TypicalValue=>Sequence, Options =>{Tolerance=>null})
splitWitness (WitnessSet,RingElement) := Sequence => o -> (w,f) -> (
-- splits the witness set into two parts: one contained in {f=0}, the other not
-- IN:  comp = a witness set
--      f = a polynomial
-- OUT: (w1,w2) = two witness sets   
     o = new MutableHashTable from o;
     scan(keys o, k->if o#k===null then o#k=DEFAULT#k); o = new OptionTable from o;
     w1 := {}; w2 := {};
     for x in w#Points do 
	 if norm evalPoly(f,coordinates x) < o.Tolerance 
	 then w1 = w1 | {x}
	 else w2 = w2 | {x};   
     ( if #w1===0 then null 
	  else witnessSet(w#Equations, w#Slice, w1), 
       if #w2===0 then null 
          else witnessSet(w#Equations, w#Slice, w2) )
     )

insertComponent = method()
insertComponent(WitnessSet,MutableHashTable) := (W,H) -> (
     d := dim W;
     if H#?d then H#d#(#H) = W 
     else H#d = new MutableHashTable from {0=>W};
     )

regeneration = method(TypicalValue=>List, Options =>{Software=>null, Output=>Regular
	  --AllButInfinity
	  })
regeneration List := List => o -> F -> (
-- solves a system of polynomial Equations via regeneration     
-- IN:  F = list of polynomials
--      Software => {PHCPACK, BERTINI, hom4ps2}
-- OUT: {s,m}, where 
--             s = list of solutions 
--     	       m = list of corresponding multiplicities	 
     o = new MutableHashTable from o;
     scan(keys o, k->if o#k===null then o#k=DEFAULT#k); o = new OptionTable from o;
     saveDEFAULTsoftware := DEFAULT.Software;     
     DEFAULT.Software = o#Software;
     R := ring F#0;
     c1 := {}; -- current solution components
     for f in F do (
	  d := first degree f;
	  c2 := new MutableHashTable; -- new components
	  for comp in c1 do (
	       if DBG>2 then << "*** proccesing component " << peek comp << endl;
	       (cIn,cOut) := splitWitness(comp,f); 
	       if cIn =!= null 
	       then insertComponent(
		    witnessSet(cIn#Equations, cIn#Slice, cIn#Points), 
		    c2
		    ); 
     	       if cOut =!= null 
	       and dim cOut > 0 -- 0-dimensional components outside V(f) discarded
	       then (
		    s := cOut#Slice;
		    -- RM := (randomUnitaryMatrix numcols s)^(toList(0..d-2)); -- pick d-1 random orthogonal row-vectors (this is wrong!!! is there a good way to pick d-1 random hyperplanes???)
     	       	    RM := random(CC^(d-1),CC^(numcols s));
		    dWS := {cOut} | apply(d-1, i->(
			      newSlice := RM^{i} || submatrix'(s,{0},{}); -- replace the first row
			      moveSlice(cOut,newSlice)
			      ));
	       	    S := ( equations comp
	       	    	 | { product flatten apply( dWS, w->sliceEquations(w.Slice^{0},R) ) } -- product of linear factors
	       	    	 | sliceEquations( submatrix'(comp#Slice,{0},{}), R ) );
	       	    T := ( equations comp
	       	    	 | {f}
	       	    	 | sliceEquations( submatrix'(comp#Slice,{0},{}), R ) );
	       	    targetPoints := track(S,T,flatten apply(dWS,points), 
			 NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii));
		    --if o.Software == M2 then targetPoints = refine(T, targetPoints, Tolerance=>1e-10);
		    if o.Software == M2engine then (
			 sing := toList singularSolutions(T,targetPoints);
			 regPoints := select(targetPoints, p->p.SolutionStatus==Regular);
			 --print (sing,reg);
		    	 if o.Output == Regular then targetPoints = regPoints 
		    	 else targetPoints = regPoints | targetPoints_sing;
			 );
		    newW := witnessSet(cOut#Equations + ideal f, submatrix'(comp#Slice,{0},{}), 
			 selectUnique(targetPoints, Tolerance=>1e-2));
		    check newW;
		    if DBG>2 then << "   new component " << peek newW << endl;
		    if #targetPoints>0 
		    then insertComponent(newW,c2);
		    ); 
	       );
	  scan(rsort keys c2, d->scan(keys c2#d,i->(
			 W := c2#d#i;
			 scan(rsort keys c2,j->if j>d then (for k in keys c2#j do W = W - c2#j#k));
			 c2#d#i = W;
			 )));
	  c1 = flatten apply(keys c2, i->apply(keys c2#i, j->c2#i#j));
	  if f == first F then ( -- if the first equation is being processed 
	       n := numgens R;
	       S := randomSlice(n-1,n);
     	       c1 = {witnessSet(ideal f, S, solveSystem( {f} | sliceEquations(S,R)))}; 
	       );
	  );
     DEFAULT.Software = saveDEFAULTsoftware;
     c1
     )

-----------------------------------------------------------------------
-- DECOMPOSITION
decompose WitnessSet := (W) -> (
     R := ring W;
     n := numgens R;
     k := dim W;
     eq := equations W;
     which := new MutableHashTable from {}; 
     cs := new MutableList from apply(degree W, i->(which#i = i; {i})); -- current components
     i'cs := {}; -- certified irreducible components
     for i from 0 to #cs-1 do if linearTraceTest(W, cs#i) then (i'cs = i'cs | {cs#i}; cs#i = {}) ;
     --sorted'cs := MutableList toList(0..deg W - 1); -- list of numbers of components sorted by degree (small to large)
     -- -1 indicates no component 
     mergeComponents := (c,c') -> (
	  cs#c = cs#c | cs#c';
	  cs#c' = {};
	  );	     	
     findComponent := (pt) -> ( for i to #cs-1  do if any(cs#i, p->areEqual((points W)#p,pt)) then return i; return null );
     done := all(new List from cs, c->#c==0);
     n'misses := 0;
     while not done do (
	  while (c := random(#cs); #cs#c == 0) do (); -- vvv
	  p := cs#c#(random(#(cs#c))); -- pick a component/point (rewrite!!!)
	  S := eq | slice W;	  
	  while (T := sliceEquations(randomSlice(k,n),R); 
	       pt' := track(S,eq|T,{coordinates (W.Points)#p}); 
	       not isRegular(pt',0)) 
	  do (); 
	  pt := first movePoints(eq, T, slice W, pt'/coordinates);
	  if (c' :=  findComponent coordinates pt) === null then error "point outside of any current component";
	  if c' == c then n'misses = n'misses + 1
	  else ( 
	       mergeComponents(c,c');
	       if linearTraceTest(W, cs#c) then (i'cs = i'cs | {cs#c}; cs#c = {});  
	       n'misses = 0 );
	  done = all(new List from cs, c->#c==0) or n'misses > 30;
	  );
     incomplete := select(new List from cs, c->#c!=0);
     if #incomplete>0 then print "-- decompose: some witness points were not classified";
     irred := apply(i'cs, c->new WitnessSet from {Equations=>W.Equations, Slice=>W.Slice, Points=>(W.Points)_c});
     scan(irred, c->c.IsIrreducible = true);
     irred | if #incomplete == 0 then {} 
             else {new WitnessSet from {Equations=>W.Equations, Slice=>W.Slice, Points=>(W.Points)_(flatten(incomplete))}}
     ) 

linearTraceTest = method() -- check linearity of trace to see if component is irreducible
linearTraceTest (WitnessSet, List) := (W,c) -> (
-- IN: W = witness superset, 
--     c = list of integers (witness points subset)
-- OUT: do (points W)_c represent an irreducible component?
     if dim W == 0 then return true;
     w := (points W)_c;
     proj := random(CC^(numgens ring W), CC^1); 
     three'samples := apply(3, i->(
	       local r;
	       w' := (
		    if i == 0 then (
     	       	    	 r = W.Slice_(dim W - 1, numgens ring W);
			 w 
		    	 )
		    else (
	       	    	 M := new MutableMatrix from W.Slice;
		    	 M_(dim W - 1, numgens ring W) = r = random CC; -- replace last column
		    	 movePoints(equations W, slice W, sliceEquations(matrix M,ring W), w) / coordinates 
	       	    	 ) );
	       {1, r, sum flatten entries (matrix w' * proj)} 
               ));
     if DBG>2 then (
	  print matrix three'samples;
     	  print det matrix three'samples;
	  );
     abs det matrix three'samples < DEFAULT.Tolerance  -- points are (approximately) on a line
     )  

numericalVarietyM2 = I -> numericalVariety flatten (regeneration I_* / decompose)
numericalVarietyBertini = I -> bertiniPosDimSolve I_*
numericalVariety Ideal := I -> (
     if DEFAULT.Software === BERTINI 
     then numericalVarietyBertini 
     else numericalVarietyM2
     ) I

-----------------------------------------------------------------------
-- AUXILIARY FUNCTIONS
toAffineChart = method() -- coordinates of the point (x_0:...:x_n) in the k-th affine chart
toAffineChart (ZZ,List) := List => (k,x) -> (
     if k<0 or k>#x then error "chart number is out of range ";
     if x#k == 0 then return infinity;
     y := apply(x, c->c/x#k);
     take(y,k) | drop(y,k+1)
     ) 

projectiveDistance = method()
projectiveDistance (List,List) := (a,b) -> acos((abs sum(a,b,(x,y)->x*conjugate y)) / ((norm2 a) * (norm2 b)))
projectiveDistance (Point,Point) := (a,b) -> projectiveDistance(coordinates a, coordinates b)

selectUnique = method(TypicalValue=>Boolean, Options=>{Tolerance=>1e-6, Projective=>false})
selectUnique List := o -> sols ->(
     u := {};
     scan(sols, s->if all(u, t->not areEqual(s,t,o)) then u = u|{s});
     u
     )
 
solutionDuplicates = method(TypicalValue=>MutableHashTable)
solutionDuplicates List := sols -> ( 
-- find positions of duplicate solutions
-- IN: list of solutions
-- OUT: H = MutableHashTable with entries of the form i=>j (sols#i is a duplicate for sols#j);
--      connected components (which are cycles) in the graph stored in H correspond to clusters of "duplicates" 
--      i=>i indicates a nonduplicate
     H := new MutableHashTable;
     for j from 0 to #sols-1 do (
	  H#j = j;
	  i := j-1;
	  while i>=0 do
	  if areEqual(sols#i,sols#j) then (
	       H#j = H#i;
	       H#i = j;
	       i = -1
	       ) 
	  else i = i - 1;
	  );
     H
     )

groupClusters = method()
groupClusters MutableHashTable := H -> (
-- processes the output of solutionDuplicates to get a list of clusters of solutions
     cs := {};
     apply(keys H, a->if H#a=!=null then (
	       c := {a};
	       b := H#a; 
	       H#a = null;
	       while b != a do (
	       	    c = c | {b};
	       	    bb := H#b;
		    H#b = null;
		    b = bb;
	       	    );
	       cs = cs | {c};
	       ));
     cs
     )

solutionsWithMultiplicity = method()
solutionsWithMultiplicity List := sols -> ( 
     clusters := groupClusters solutionDuplicates sols;
     apply(clusters, c->(
	       s := new Point from sols#(first c);
	       if (s.Multiplicity = #c)>1 and s.SolutionStatus === Regular then s.SolutionStatus = Singular;
	       s
	       ))
     )

singularSolutions = method() -- decide on the tolerance!!!
singularSolutions(List,List) := (T,sols) -> (
-- find positions of singular solutions in sols
-- IN: number of solutions 
-- OUT: list of numbers of solutions considered to be singular 
--      (i.e., nearly satisfies target system, but Status!=REGULAR)    
     select(0..#sols-1, i->(
	       x := coordinates sols#i;
	       not isRegular(sols,i) and all(T, f->(rs := evalPoly(f,x); abs(rs)/norm matrix{x} < 1000*DEFAULT.Tolerance))
	       ))
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
     a := {}; b := {};
     while i<#A and j<#B do 
     if areEqual(A#i,B#j) then (i = i+1; j = j+1)
     else if isGEQ(A#i,B#j) then (b = append(b,j); j = j+1)
     else (a = append(a,i); i = i+1);	  
     (a|toList(i..#A-1),b|toList(j..#B-1))	      	    
     )


-------------------------------------------------------
-- DEFLATION ------------------------------------------
-------------------------------------------------------

numericalRank = method(Options=>{Threshold=>1e2}) -- looks for a gap between singular values 
numericalRank Matrix := o -> M -> (
     o = new MutableHashTable from o;
     scan(keys o, k->if o#k===null then o#k=DEFAULT#k); o = new OptionTable from o;
     if not member(class ring M, {RealField,ComplexField}) 
     then error "matrix with real or complex entries expected";
     S := first SVD M;
     r := 0; last's := 1;
     for i to #S+1 do (
	  if o.Threshold*S#i < last's 
	  then break
	  else (r = r + 1; last's = S#i)
	  );
     r 
     )  

dMatrix = method()
dMatrix (List,ZZ) := (F,d) -> dMatrix(ideal F, d)
dMatrix (Ideal,ZZ) := (I, d) -> (
-- deflation matrix of order d     
     R := ring I;
     v := flatten entries vars R;
     n := #v;
     ind := toList((n:0)..(n:d)) / toList;
     ind = select(ind, i->sum(i)<=d and sum(i)>0);
     A := transpose diff(matrix apply(ind, j->{R_j}), gens I);
     scan(select(ind, i->sum(i)<d and sum(i)>0), i->(
	       A = A || transpose diff(matrix apply(ind, j->{R_j}), R_i*gens I);
	       ));
     A
     )
dIdeal = method()
dIdeal (Ideal, ZZ) := (I, d) -> (
-- deflation ideal of order d     
     R := ring I;
     v := gens R;
     n := #v;
     ind := toList((n:0)..(n:d)) / toList;
     ind = select(ind, i->sum(i)<=d and sum(i)>0);
     A := dMatrix(I,d);
     newvars := apply(ind, i->getSymbol("x"|concatenate(i/toString)));
     S := (coefficientRing R)[newvars,v]; 
     sub(I,S) + ideal(sub(A,S) * transpose (vars S)_{0..#ind-1})
     )	   
deflatedSystem = method()
deflatedSystem(Ideal, Matrix, ZZ, ZZ) := memoize (
(I, M, r, attempt) -> (
-- In: gens I = the original (square) system   
--     M = deflation matrix
--     r = numerical rank of M (at some point)
-- Out: (square system of n+r equations, the random matrix SM)
     R := ring I;
     n := numgens R;
     SM := randomOrthonormalCols(numcols M, r+1);
     d := local d;
     S := (coefficientRing R)(monoid[gens R, d_0..d_(r-1)]);
     DF := sub(M,S)*sub(SM,S)*transpose ((vars S)_{n..n+r-1}|matrix{{1_S}}); -- new equations
     --print DF;     
     (
	  flatten entries squareUpSystem ( sub(transpose gens I,S) || DF ),
	  SM
	  )
     )
) -- END memoize

liftSolution = method(Options=>{Tolerance=>null}) -- lifts a solution s to a solution of a deflated system dT (returns null if unsuccessful)
liftSolution(List, List) := o->(s,dT)->liftSolution(s, transpose matrix{dT},o)
liftSolution(List, Matrix) := o->(c,dT)->(
     R := ring dT;
     n := #c;
     N := numgens R;
     if N<=n then error "the number of variables in the deflated system is expected to be larger"; 
     newVars := (vars R)_{n..N-1};
     specR := (coefficientRing R)(monoid[flatten entries newVars]);
     dT0 := (map(specR, R, matrix{c}|vars specR)) dT;
     ls := first solveSystem flatten entries squareUpSystem dT0; -- here a linear system is solved!!!     
     if status ls =!= Regular then return null;
     ret := c | coordinates ls;
     -- if norm sub(dT, matrix{ret}) < o.Tolerance * norm matrix{c} then ret else null
     ret
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
     curNode := #consts+nIns;
     p := {};
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
     curNode := #consts+nIns;
     p := {#consts,nIns,numgens target out, numgens source out} | {slpCOMPILED}
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
NAGtrace ZZ := l -> (gbTrace=l; oldDBG:=DBG; DBG=l; oldDBG);

-- normalized condition number of F at x
conditionNumber = method()
conditionNumber Matrix := M -> (s := first SVD M; max s / min s)
conditionNumber (List,List) := (F,x) -> (
     nF := apply(F, f->f/sqrt(#F * BombieriWeylNormSquared f)); -- normalize F
     x0 := normalize transpose matrix{x}; -- column unit vector
     DMforPN := diagonalMatrix(nF/(f->1/sqrt first degree f) | {1});
     J := sub(transpose jacobian matrix{nF}, transpose sub(x0,CC)); -- Jacobian of F at x
     J = J || matrix{ flatten entries x0 / conjugate};
     conditionNumber(DMforPN*J) --  norm( Moore-Penrose pseudoinverse(J) * diagonalMatrix(sqrts of degrees) )     
     )

-- a constructor for witnessSet that depends on NAG
witnessSet Ideal := I -> witnessSet(I,dim I) -- caveat: uses GB driven dim
witnessSet (Ideal,ZZ) := (I,d) -> (
     n := numgens ring I;
     R := ring I;
     SM := (randomUnitaryMatrix n)^(toList(0..d-1))|random(CC^d,CC^1);
     S := ideal(promote(SM,R) * ((transpose vars R)||matrix{{1_R}}));
     RM := (randomUnitaryMatrix numgens I)^(toList(0..n-d-1));
     RM = promote(RM,ring I);
     rand'I := flatten entries (RM * transpose gens I);
     P := solveSystem(rand'I | S_*);
     PP := select(P, p->norm sub(gens I, matrix p)  < 1e-3 * norm matrix p);
     witnessSet(ideal rand'I,SM,PP)
     )

beginDocumentation()

load "./NumericalAlgebraicGeometry/doc.m2";

TEST ///
--assert(multistepPredictor(2_QQ,{0,0,0}) === {-3/8, 37/24, -59/24, 55/24}) -- Wikipedia: Adams-Bashforth
--assert(multistepPredictor(2_QQ,{-1}) === {-1/8, 5/8}) -- computed by hand
--assert(flatten entries (coefficients first multistepPredictorLooseEnd(2_QQ,{0,0,0}))#1=={1/120, 1/16, 11/72, 1/8})

-- numerical rank
assert (numericalRank matrix {{2,1},{0,0.001}} == 1)

-- random and good initial pairs
setRandomSeed 0
T = randomSd {2,3};
(S,solsS) = goodInitialPair T
M = track(S,T,solsS,Normalize=>true)
RM = refine(T,M)
debug NumericalAlgebraicGeometry
assert areEqual(norm2 matrix first RM, 1_CC)
///
TEST ///
load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/SoftwareM2.tst.m2")
///
TEST ///
load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/SoftwareM2engine.tst.m2")
///
TEST ///
load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/SoftwareM2enginePrecookedSLPs.tst.m2")
///


end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
uninstallPackage "NumericalAlgebraicGeometry"
installPackage "NumericalAlgebraicGeometry"
installPackage ("NumericalAlgebraicGeometry",RerunExamples=>true, RemakeAllDocumentation=>true)
installPackage ("NumericalAlgebraicGeometry",RerunExamples=>false, RemakeAllDocumentation=>true)

-- (old way) installPackage("NumericalAlgebraicGeometry", SeparateExec=>true, AbsoluteLinks=>false)

-- install docs with no absolute links
uninstallPackage "Style"
installPackage("Style", AbsoluteLinks=>false)
installPackage("NumericalAlgebraicGeometry", AbsoluteLinks=>false)

installPackage ("NumericalAlgebraicGeometry", MakeDocumentation=>false)
check "NumericalAlgebraicGeometry"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=NumericalAlgebraicGeometry "
-- End:
