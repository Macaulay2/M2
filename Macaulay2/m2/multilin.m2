--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

koszul(ZZ, Matrix) := (i,m) -> (
     sendgg(ggPush m, ggINT, gg i, ggkoszul);
     getMatrix ring m)

symmetricPower(ZZ, Matrix) := (i,m) -> (
     sendgg(ggPush m, ggINT, gg i, ggsymm);
     getMatrix ring m)

MinorsComputation = new SelfInitializingType of BasicList

PfaffiansComputation = new SelfInitializingType of BasicList

exteriorPower(ZZ,Module) := (p,M) -> (
     R := ring M;
     if p < 0 then R^0
     else if p === 0 then R^1
     else if p === 1 then M
     else (
	  if isFreeModule M then (
	       sendgg(ggPush M, ggPush p, ggexterior);
	       new Module from R)
	  else (
	       m := presentation M;
	       F := target m;
	       G := source m;
	       Fp1 := exteriorPower(p-1,F);
	       Fp := exteriorPower(p,F);
	       h1 := m ** id_Fp1;
	       h2 := wedgeProduct(1,p-1,F);
	       coker (h2*h1))))

exteriorPower(ZZ, Matrix) := (p,m) -> (
     R := ring m;
     if p < 0 then map(R^0,R^0,0)
     else if p === 0 then map(R^1,R^1,1)
     else if p === 1 then m
     else (
	  -- h := prune m;
	  h := m;			  -- DRG: disabled 'prune' here.
	  h1 := matrix h;
	  sendgg(ggPush h1, ggPush p, ggexterior);
	  hp := getMatrix ring m;
	  map(exteriorPower(p, target h),
	      exteriorPower(p, source h),
	      hp))
    )

wedgeProduct = method()
wedgeProduct(ZZ,ZZ,Module) := (p,q,M) -> (
     if isFreeModule M then (
	  R := ring M;
	  sendgg(ggPush p, ggPush q, ggPush M, ggexteriorproduct);
	  getMatrix R)
     else map(exteriorPower(p+q,M),exteriorPower(p,M)**exteriorPower(q,M),wedgeProduct(p,q,cover M)))

minors = method(Options => { Limit => infinity })

minors(ZZ,Matrix) := options -> (j,m) -> (
     if j === 0 then ideal 1_(ring m)
     else if j < 0 then ideal 0_(ring m)
     else (
	  comp := MinorsComputation{j};
	  if not m#?comp then (
	      sendgg(
		   ggPush m,			  -- m
		   ggINT, gg j,		  -- m j
		   ggdets);			  -- create computation
	      m#comp = {1, newHandle()};       -- the '1' means: not done yet.
	     );
	  if m#comp#0 =!= 0 then (
	      nsteps := if options.Limit === infinity then -1 else options.Limit;
	      sendgg(
		   ggPush m#comp#1,
		   ggPush nsteps,
		   ggcalc);
	      m#comp = {eePopInt(), m#comp#1}   -- return code: 0 means done, != 0 means more left
	      );
	  sendgg(ggPush m#comp#1,
		 ggINT, gg 0,
		 ggindex);
	  ideal getMatrix ring m
	  ))

pfaffians = method(Options => { Limit => infinity })
pfaffians(ZZ,Matrix) := options -> (j,m) -> (
     if j === 0 then ideal 1_(ring m)
     else if j < 0 then ideal 0_(ring m)
     else (
	  comp := PfaffiansComputation{j};
	  if not m#?comp then (
	      sendgg(
		   ggPush m,			  -- m
		   ggINT, gg j,		  -- m j
		   ggpfaffs);			  -- create computation
	      m#comp = {1, newHandle()};       -- the '1' means: not done yet.
	     );
	  if m#comp#0 =!= 0 then (
	      nsteps := if options.Limit === infinity then -1 else options.Limit;

	      sendgg(
		   ggPush m#comp#1,
		   ggPush nsteps,
		   ggcalc);
	      m#comp = {eePopInt(), m#comp#1}   -- return code: 0 means done, != 0 means more left
	      );

	  sendgg(ggPush m#comp#1,
		 ggINT, gg 0,
		 ggindex);
	  ideal getMatrix ring m
	  ))

-----------------------------------------------------------------------------
trace = method()
trace Matrix := f -> (
     if rank source f != rank target f
     or not isFreeModule source f
     or not isFreeModule target f
     then error "expected a square matrix";
     sum(rank source f, i -> f_(i,i)))
-----------------------------------------------------------------------------
det Matrix := f -> (
     if rank source f != rank target f
     or not isFreeModule source f
     or not isFreeModule target f
     then error "expected a square matrix";
     (exteriorPower(numgens source f, f))_(0,0))

fittingIdeal = method()
fittingIdeal(ZZ,Module) := (i,M) -> (
     p := presentation M;
     n := rank target p;
     if n <= i
     then ideal 1_(ring M)
     else trim minors(n-i,p))

