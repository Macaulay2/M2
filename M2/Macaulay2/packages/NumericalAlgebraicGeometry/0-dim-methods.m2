------------------------------------------------------
-- routines for 0-dim solution sets 
-- not included in other files
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------
satisfiesOverdeterminedSystem = method(Options=>{ResidualTolerance=>null})
satisfiesOverdeterminedSystem (Point, List) := o -> (s,F) -> (
    o = fillInDefaultOptions o; 
    norm evaluate(matrix{F},s) < o.ResidualTolerance    
    )

solveSystem = method(TypicalValue => List, Options =>{
	PostProcess=>true, 
	-- *** below are the relevant options of trackHomotopy ***
	Precision=>null,
	Software=>null, 
	NumericalAlgebraicGeometry$gamma=>null, 
	NumericalAlgebraicGeometry$tDegree=>null,
	-- step control
	tStep => null, -- initial
	tStepMin => null,
	stepIncreaseFactor => null,
	numberSuccessesBeforeIncrease => null,
	-- predictor 
	Predictor=>null, 
	-- corrector 
	maxCorrSteps => null,
	CorrectorTolerance => null, -- tracking tolerance
	-- end of path
	EndZoneFactor => null, -- EndZoneCorrectorTolerance = CorrectorTolerance*EndZoneFactor when 1-t<EndZoneFactor 
	InfinityThreshold => null, -- used to tell if the path is diverging
	SingularConditionNumber=> null, -- threshold for the condition number of the jacobian
	ResidualTolerance => null, -- threshold for the norm of the residual
	-- projectivize and normalize
	Normalize => null, -- normalize in the Bombieri-Weyl norm
	Projectivize => null
	})
solveSystem List := List => o -> F -> (
    checkCCpolynomials F;
    solveSystem(polySystem F, o)
    )
solveSystem PolySystem := List => o -> P -> (
-- solves a system of polynomial equations
-- IN:  F = list of polynomials
--      Software => {PHCPACK, BERTINI, hom4ps2}
-- OUT: {s,m}, where 
--             s = list of solutions 
--     	       m = list of corresponding multiplicities	 
     o = fillInDefaultOptions o;
     local result;
     F := equations P; 
     R := ring F#0;
     v := flatten entries vars R;
     if numgens R == 0 then error "expected at least one variable";
     if numgens R > #F then error "expected a 0-dimensional system";
     if member(o.Software, {M2,M2engine,M2enginePrecookedSLPs}) then ( 
	  if o.Normalize then F = apply(F,normalize);
	  overdetermined := numgens R < #F; 
	  T := (if overdetermined 
	       then generalEquations(numgens R, F)
	       else F);  
  	  result = (
	       (S,solsS) := totalDegreeStartSystem T;
	       polyS := polySystem S;
	       polyT := polySystem T;
	       unit := exp(random(0.,2*pi)*ii);
	       if DBG>2 then (
		   << "H = segmentHomotopy(" << toExternalString polyS << "," << toExternalString polyT << ",gamma=>" << toExternalString unit << ")" << endl;    
	       	   << "trackHomotopy(H," << toExternalString solsS << ")" << endl;
		   );
	       H := segmentHomotopy(polyS, polyT, NumericalAlgebraicGeometry$gamma=>unit);
	       trackHomotopy(H,solsS,
		   Precision => o.Precision,
		   CorrectorTolerance => o.CorrectorTolerance,
		   maxCorrSteps => o.maxCorrSteps,
		   numberSuccessesBeforeIncrease => o.numberSuccessesBeforeIncrease,
                   Predictor => o.Predictor,
                   Software => o.Software,
                   stepIncreaseFactor => o.stepIncreaseFactor,
                   tStep => o.tStep,
                   tStepMin => o.tStepMin)
	       );
	  if o.PostProcess 
	  then (
	      plausible := select(result, p-> status p =!= Regular and status p != Origin and status p =!= Infinity 
		  and p.LastT > 1-o.EndZoneFactor);
  	      result = select(result, p->status p === Regular or status p === Origin) | select( 
		  apply(#plausible,     
		      i -> (
			  p := plausible#i;
			  q := endGameCauchy(p#"H",1,p,"backtrack factor"=>2); -- endgame ...
    	    	    	  -- if DBG>0 then if (i+1)%10 == 0 or i+1==#plausible then << endl;
      	      	      	  q
			  )
		      ),
		  p -> norm evaluateH(H,transpose matrix p,1) < o.ResidualTolerance -- ... may result in non-solution
		  );
	      );
	  if overdetermined then result = select(result, s->satisfiesOverdeterminedSystem(s,F))  	      
	  )
     else if o.Software === PHCPACK then result = solvePHCpack(F,o)
     else if o.Software === BERTINI then result = solveBertini(F,o)
     else if o.Software === HOM4PS2 then (
	  (name, p) := makeHom4psInput(R, F);
	  targetfile := name;
	  tarsolfile := temporaryFileName() | "tasols";
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

totalDegreeStartSystem = method(TypicalValue => Sequence)
totalDegreeStartSystem List := Sequence => T -> (
-- constructs a total degree start system and its solutions 
-- for the given target system T
-- IN:  T = list of polynomials 
-- OUT: (S,solsS}, where 
--      S     = list of polynomials, 
--      solsS = list of sequences
     R := commonRing T;
     if any(gens R, x->sum degree x != 1) then error "expected degrees of ring generators to be 1";
     n := #T;
     if n != numgens R then (
	  if numgens R == n+1 and all(T, isHomogeneous) 
	  then isH := true
	  else error "wrong number of polynomials";
	  )
     else isH = false;
     S := apply(n, i->R_i^(sum degree T_i) - (if isH then R_n^(sum degree T_i) else 1) );
     s := apply(n, i->( 
	  d := sum degree T_i; 
	  set apply(d, j->sequence exp(ii*2*pi*j/d))
	  ));
     solsS := first s;
     scan(drop(s,1), t->solsS=solsS**t);
     if numgens R === 1 
     then solsS = toList solsS/(a -> 1:a)
     else solsS = toList solsS/deepSplice; 
     if isH then solsS = solsS / (s->s|sequence 1);
     (S, apply(solsS,s->point{toList s}))
     ) 

TEST ///
needsPackage "NumericalAlgebraicGeometry"
R = QQ[x,y]
F = polySystem {x*y-1,x^2+y^3-2}
solveSystem F
NAGtrace 3
assert(#solveSystem(F,Precision=>infinity) == 5)
///
