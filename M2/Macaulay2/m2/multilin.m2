--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman

koszul(ZZ, Matrix) := Matrix => (i,m) -> map(ring m, rawKoszul(i, raw m))
symmetricPower(ZZ, Matrix) := Matrix => (i,m) -> map(ring m, rawSymmetricPower(i, raw m))

MinorsComputation = new SelfInitializingType of BasicList
MinorsComputation.synonym = "minors computation"

getMinorsStrategy := (R,options) -> (
     bareiss := 0;  -- WARNING: these must match the engine!!
     cofactor := 1;
     strat := if options.?Strategy then options.Strategy else null;
     if strat === symbol Bareiss then bareiss
     else if strat === symbol Cofactor then cofactor
     else if strat =!= null then (
	  error "'Strategy' keyword must be 'Cofactor' or 'Bareiss'";
	  )
     else (
	  -- Use the Bareiss algorithm unless R is a quotient of
	  -- a polynomial ring.  Note that if R is non-commutative
	  -- then either algorithm is incorrect.  What is the correct
	  -- thing to do in this case?
	  if isQuotientRing R and not isField R then
     	    cofactor
	  else
	    bareiss
     ))

minors = method(Options => { Limit => infinity, First => null, Strategy => null })
exteriorPower = method(Options => { Strategy => null })
det = method(Options => { Strategy => null })

exteriorPower(ZZ,Module) := Module => options -> (p,M) -> (
     R := ring M;
     if p < 0 then R^0
     else if p === 0 then R^1
     else if p === 1 then M
     else (
	  if isFreeModule M then newModule(R, rawExteriorPower(p,M.RawFreeModule))
	  else (
	       m := presentation M;
	       F := target m;
	       G := source m;
	       Fp1 := exteriorPower(p-1,F,options);
	       Fp := exteriorPower(p,F,options);
	       h1 := m ** id_Fp1;
	       h2 := wedgeProduct(1,p-1,F);
	       cokernel (h2*h1))))

exteriorPower(ZZ, Matrix) := Matrix => options -> (p,m) -> (
     R := ring m;
     if p < 0 then map(R^0,R^0,0)
     else if p === 0 then map(R^1,R^1,1)
     else if p === 1 then m
     else map(
	  exteriorPower(p, target m, options),
	  exteriorPower(p, source m, options),
	  rawExteriorPower(p,raw m,getMinorsStrategy(R,options))))

wedgeProduct = method()
wedgeProduct(ZZ,ZZ,Module) := Matrix => (p,q,M) -> (
     if isFreeModule M then map(ring M, rawWedgeProduct(p,q,raw M))
     else map(exteriorPower(p+q,M),exteriorPower(p,M)**exteriorPower(q,M),wedgeProduct(p,q,cover M)))

-- someMinors := (j,m,options) -> (
--      strat := getMinorsStrategy(ring m,options);
--      -- setup computation in the engine
--      sendgg(
-- 	  ggPush m,			  -- m
-- 	  ggPush j,
-- 	  ggPush strat,          -- 0: bareiss, 1: cofactor.
-- 	  ggdets);			  -- create computation
--      h := newHandle();
--      nsteps := if options.Limit === infinity then -1 else options.Limit;
--      if options.?First then (
--      	  if not(class options.First === List and #(options.First) === 2
--        	    and class options.First#0 === List and class options.First#1 === List
--        	    and all(options.First#0, i -> class i === ZZ)
--        	    and all(options.First#1, i -> class i === ZZ) 
-- 	    and #(options.First#0) === j
-- 	    and #(options.First#1) === j)
-- 	    then
-- 	    error "expected 'First' value to be a list of 2 lists of integers";
--          sendgg(
-- 	   ggPush h,
-- 	   ggPush nsteps,
-- 	   ggPush 0,  -- don't discard previous (there are none...)
-- 	   ggPush options.First#0,
-- 	   ggPush options.First#1,
-- 	   ggcalc))
--      else
--        sendgg(
-- 	  ggPush h,
-- 	  ggPush nsteps,
-- 	  ggcalc);
--      eePopInt();
--      sendgg(ggPush h,
-- 	  ggPush 0,
-- 	  ggindex);
--      ideal getMatrix ring m
--      )

minors(ZZ,Matrix) := Ideal => opts -> (j,m) -> (
     if opts.First =!= null then error "optional argument 'First' not re-implemented yet";
     if opts.Limit =!= infinity then error "optional argument 'Limit' not re-implemented yet";
     ideal map(ring m, rawMinors(j,raw m, getMinorsStrategy(ring m,opts))))

pfaffians = method(TypicalValue => Ideal)
pfaffians(ZZ,Matrix) := Ideal => (j,m) -> (
     ideal(map(ring m, rawPfaffians(j,raw m))))

-----------------------------------------------------------------------------
trace Matrix := RingElement => f -> (
     if rank source f != rank target f
     or not isFreeModule source f
     or not isFreeModule target f
     then error "expected a square matrix";
     sum(rank source f, i -> f_(i,i)))
-----------------------------------------------------------------------------
det Matrix := RingElement => options -> f -> (
     if rank source f != rank target f
     or not isFreeModule source f
     or not isFreeModule target f
     then error "expected a square matrix";
     (exteriorPower(numgens source f, f, options))_(0,0))

fittingIdeal = method(TypicalValue => Ideal)
fittingIdeal(ZZ,Module) := Ideal => (i,M) -> (
     p := presentation M;
     n := rank target p;
     if n <= i
     then ideal 1_(ring M)
     else trim minors(n-i,p))


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
