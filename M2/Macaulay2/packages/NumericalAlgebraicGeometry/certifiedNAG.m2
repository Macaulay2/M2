DBG = 5;
trackProjectiveCertified = method()
trackProjectiveCertified (List,List,List) := List => (S,T,solsS) -> (
-- tracks solutions from start system to target system (robust certified)
-- IN:  S = list of polynomials in start system
--      T = list of polynomials in target system
--      solsS = list of solutions to S
-- OUT: solsT = list of target solutions corresponding to solsS
     HISTORY := DBG>1;
     if #T > 0 then R := ring first T else error "expected nonempty target system";
     if #S != #T then 
     error "expected same number of polynomials in start and target systems";
     if any(S, f->ring f =!= R) or any(T, f->ring f =!= R)
     then error "expected all polynomials in the same ring";
     n := numgens R; 
     if not(n == #T+1 
	  -- and all(T, isHomogeneous) and all(S, isHomogeneous) -- bug in isHomogeneous!!!
	  ) 
     then error "expected n equations in in n+1 variables";
     -- M2 (main code)  --------------------------------------------------------     
     setupStartTime := currentTime();
     -- threshholds and other tuning parameters (should include most of them as options)
     theSmallestNumber := 1e-12;
     
     I := symbol I;
     K := QQ[I]/ideal(I^2+1); -- THE coefficient ring
     R = K[gens R];
     solsS = solsS / (s->sub(transpose matrix {toList s}, K)); -- convert to vectors
     	  -- affine patch functions ???
     	  pointToPatch := (x0,p)-> (1/(p*x0)_(0,0))*x0; -- representative for point x0 in patch p
	  patchEquation := p -> p * transpose vars R - 1;
	  --dPatch := true; -- not null ???       
     	  dPatch := null; -- for now !!!
     -- create homotopy
     Rt := K(monoid[gens R, symbol t]); 
     t := last gens Rt; 
     H := matrix {apply(#S, i->(1-t)*sub(S#i,Rt)+t*sub(T#i,Rt))};
     JH := transpose jacobian H; 
     Hx := JH_(toList(0..n-1));
     Ht := JH_{n};
     -- in both cases a linear homotopy on the unit sphere is performed ???
     -- then (
-- 	  nS = (o.gamma/abs(o.gamma))*nS;
-- 	  H := {matrix{nS},matrix{nT}}; -- a "linear" homotopy is cooked up at evaluation using nS and nT
-- 	  DMforPN := diagonalMatrix append(T/(f->1/sqrt first degree f),1);
-- 	  maxDegreeTo3halves := power(max(T/first@@degree),3/2);
-- 	  reBW'ST := realPart sum(#S, i->BombieriWeylScalarProduct(nS#i,nT#i));-- real Bombieri-Weyl scalar product
-- 	  sqrt'one'minus'reBW'ST'2 :=  sqrt(1-reBW'ST^2);
-- 	  bigT := asin sqrt'one'minus'reBW'ST'2; -- the linear homotopy interval is [0,bigT]
-- 	  Hx := H/transpose@@jacobian; -- store jacobians (for evalHx)
--      	  )	  
     -- evaluation times
     etH := 0;
     etHx := 0; 
     etHt := 0;
     -- evaluation functions	
     evalH := (x0,t0)-> (
	  tr := timing (
     	       r := lift(sub(transpose H, transpose x0 | matrix {{t0}}), K);
	       --(old) if o.Predictor === Certified then ((normalizer t0)*r) || matrix{{0_K}} else 
	       if dPatch === null then r
	       else r || matrix{{(dPatch*x0)_(0,0)-1}} -- patch equation evaluated  
	       );
	  etH = etH + tr#0;
	  tr#1
	  );
--      evalHxNoPatch := (x0,t0)-> (
-- 	  r := if o.Predictor === Certified then (
-- 	       sine := sin(t0*bigT); cosine := cos(t0*bigT);
-- 	       lift(sub(Hx#0,transpose x0),K)*(cosine-(reBW'ST/sqrt'one'minus'reBW'ST'2)*sine) 
-- 	       + lift(sub(Hx#1,transpose x0),K)*(sine/sqrt'one'minus'reBW'ST'2)   
-- 	       )
-- 	  else if o.SLP =!= false then fromSlpMatrix(slpHx, transpose x0 | matrix {{t0}})
--      	  else lift(sub(Hx, transpose x0 | matrix {{t0}}), K);
-- 	  --(old) if o.Predictor === Certified then r = r * normalizer t0;
-- 	  r
-- 	  );  
     evalHx := (x0,t0)->( 
	  tr := timing (
     	       r := lift(sub(Hx, transpose x0 | matrix {{t0}}), K);
	       if dPatch === null then r
	       else r || matrix { flatten entries dPatch }
	       );
	  etHx = etHx + tr#0;
	  tr#1
	  );  
     evalHt := (x0,t0)->(
	  tr := timing (
	       r := lift(sub(Ht, transpose x0 | matrix {{t0}}), K);
	       if dPatch === null then r
	       else r || matrix {{0_K}}
	       );
	  etHt = etHt + tr#0;
	  tr#1
	  );
     --evalMinusInverseHxHt := (x0,t0)-> -(inverseMatrix evalHx(x0,t0))*evalHt(x0,t0);
     --solveHxTimesDXequalsMinusHt := (x0,t0) -> solve(evalHx(x0,t0),-evalHt(x0,t0)); 
     
     compStartTime := currentTime();      

     rawSols := apply(#solsS, sN->(
	       s := solsS#sN;
	       s'status := Processing;
	       if DBG > 2 then << "tracking solution " << toString s << endl;
	       x0 := s; 
	       t0 := 0_K; 
	       count := 1; -- number of computed points
	       if HISTORY then history := new MutableHashTable from{ count => new MutableHashTable from {
			 "t"=>t0,"x"=>x0
			 } };
	       while s'status === Processing do (
		    if DBG > 4 then << "--- current t = " << t0 << endl;
     		    
		    dPatch = matrix{ flatten entries x0 / conjugateQI};
	     				    
		    Hx0 = evalHx(x0,t0);
		    Ht0 = evalHt(x0,t0);
-- 		     	 chi2 := sqrt((norm2 Ht0)^2 + (norm2 solve(Hx0, Ht0))^2);
-- 			 chi1 := 1 / min first SVD(DMforPN*Hx0);
-- 			      if DBG > 2 then (
-- 			      << "chi1 = " << chi1 << endl;
-- 			      if count<=5 then print(DMforPN*Hx0);
-- 			      );
-- 			 dt = 0.04804448/(maxDegreeTo3halves*chi1*chi2*bigT); -- divide by bigT since t is in [0,1]
-- 			 if dt<o.tStepMin then (
-- 			      if DBG > 2 then (
-- 				   << "chi1 = " << chi1 << endl;
-- 			      	   << "chi2 = " << chi2 << endl;
-- 				   );
-- 			      s'status = MinStepFailure; 
-- 			      --error "too small step";
-- 			      );
-- 			 if dt > 1-t0 then dt = 1-t0;
                    dt = 1/10; -- test!!!
		    dx = 0; -- 0-th order predictor

		    if DBG > 3 then << "  dt = " << dt << "  dx = " << toString dx << endl;
		    if HISTORY then history#count#"dx" = dx;

    	 	    t1 := t0 + dt;
		    x1 := x0 + dx;
		    
		    nCorrSteps = 1;
		    dx = -inverse(evalHx(x1,t1))*evalH(x1,t1);
		    x1 = x1 + dx;
		    roundQI x1;
		    
		    x0 = normalizeQI x1; -- approximate normalization 
		    t0 = t1;
		    count = count + 1;
		    if HISTORY then history#count = new MutableHashTable from {"t"=>t0,"x"=>x0};
		    
		    if t0 == 1 then s'status = Regular;
		    );        	    
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
		    --ConditionNumber => conditionNumber evalHx(x0,t0),
		    LastT => t0 
		    ) | ( if HISTORY
		    then sequence new HashTable from history 
		    else sequence ())
	       ));
     if DBG>3 then print rawSols;
     ret := rawSols/(s->{flatten entries first s} | drop(toList s,1));
     if DBG>0 then (
	  << "Number of solutions = " << #rawSols << endl;
	  << "Average number of steps per path = " << toRR sum(ret,s->s#1#1)/#ret << endl;
	  if DBG>1 then 
	  << "Evaluation time (M2 measured): Hx = " << etHx << " , 
	  Ht = " << etHt << " , H = " << etH << endl;
	  << "Setup time: " << compStartTime - setupStartTime << endl;
	  << "Computing time:" << currentTime() - compStartTime << endl; 
	  );
     apply(ret, s->point(
	       if HISTORY then drop(toList s, -1)
	       else toList s
	       )
	  )
     )

-- Gaussian rationals: "QI" = QQ[I]/(I^2+1)
conjugateQI = method() 
conjugateQI RingElement := RingElement => x -> sub(sub(x, matrix{{ -I }}),ring x)

normSquareQI = method(TypicalValue=>RingElement) -- 2-norm of a vector                           
normSquareQI List := v -> sub(sum(v, x->x*conjugateQI x),QQ);
normSquareQI Matrix := v -> normSquareQI flatten entries v

normalizeQI = method(TypicalValue => Matrix) -- normalizes a column vector
normalizeQI Matrix := v -> (1/approxSqrt(normSquareQI v,1/100000))*v

roundQI = method(TypicalValue=>RingElement) 
roundQI (ZZ, RingElement) := (n,x) -> (
     cs := coefficients (10^n * x);
     apply(last cs, c->round c)
     )
roundQI (ZZ, Matrix) := (n,M) -> matrix apply(entries N, r->apply(r, e->roundQI(n,e)))


approxSqrt = method()
approxSqrt(QQ,QQ) := (t,e) -> (
     t0 := t;
     t' := (t0 + 1)/2;
     while t'^2>t*(1+e)^2 do (
	  if (floor t')^2 >= t then t' = floor t';
	  t0 = t';
	  t' = t0/2 + t/(2*t0);
	  );
     t'
     )
