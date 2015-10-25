------------------------------------------------------
-- routines for 0-dim solution sets 
-- not included in other files
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------
solveSystem = method(TypicalValue => List, Options =>{
	PostProcess=>true, 
	-- *** below are the relevant options of track ***
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
	SingularConditionNumber=>null, -- threshold for the condition number of the jacobian
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
     -- F = toCCpolynomials(F,53);
     F := equations P; 
     R := ring F#0;
     v := flatten entries vars R;
     if numgens R > #F then error "expected a 0-dimensional system";
     if member(o.Software, {M2,M2engine,M2enginePrecookedSLPs}) then ( 
	  overdetermined := numgens R < #F; 
	  T := (if overdetermined 
	       then generalEquations(numgens R, F)
	       else F);  
  	  result = (
	       (S,solsS) := totalDegreeStartSystem T;
	       track(S,T,solsS,NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii),Software=>o.Software)
	       );
	  if o.PostProcess and not overdetermined 
	  then (
	       result = select(refine(F,result,Software=>o.Software), s->residual(F,s)<DEFAULT.Tolerance);
	       result = solutionsWithMultiplicity result;
	       -- below is a hack!!!
	       scan(result, s->if status s =!= Regular 
		   and s.?ErrorBoundEstimate 
		   and s.ErrorBoundEstimate < DEFAULT.ErrorTolerance then (
			 if DBG>1 then print "path jump occured";
			 s.Multiplicity = 1;
			 s.SolutionStatus = Regular;
			 ));
	       )
	  )
     else if o.Software === PHCPACK then result = solvePHCpack(F,o)
     else if o.Software === BERTINI then result = solveBertini(F,o)
     else if o.Software === HOM4PS2 then (
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
