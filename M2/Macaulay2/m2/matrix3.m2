-- Copyright 1996 Michael E. Stillman

needs "modules.m2"
needs "ringmap.m2"

PushforwardComputation = new SelfInitializingType of BasicList
PushforwardComputation.synonym = "push-forward computation"

-- valid values are (a) {J,cleanup code}   -- J is the aux matrix and the code to derive
-- 						the answer is the 2nd element.
--               or (b) {answer}           -- answer is a matrix

--subringOptions := mergeOptions(options gb, Strategy => , UseHilbertFunction => true

protect NonLinear					    -- no longer exported

pushOptions := new OptionTable from {
	  MonomialOrder => Eliminate,       -- default is an elimination order
	  UseHilbertFunction => true,  -- if possible
	  StopBeforeComputation => false,
	  DegreeLimit => {},
	  PairLimit => infinity
	  -- unused options:
	  -- Strategy => NonLinear,            -- use the best choice
	  -- BasisElementLimit => infinity,  -- number of generators of GB in the subring
	  -- StopWithMinimalGenerators => false            -- determine the minimal generators of the subring
	  }

pushNonLinear := opts -> (f,M) -> (				    -- this returns the presentation matrix of the pushforward module
    -- written by Mike and David
    R := target f;
    assert( ring M === R );
    comp := PushforwardComputation{M,NonLinear};
    if not f.cache#?comp then (	       -- a bad place to cache the computation, because M may not get garbage-collected
	S := source f;
	n1 := numgens R;				    -- what if R is a tower?
        order := if opts.MonomialOrder === Eliminate then Eliminate n1
                 else if opts.MonomialOrder === ProductOrder then ProductOrder{n1, numgens S}
		 else if opts.MonomialOrder === Lex then Lex
		 else error("MonomialOrder option: expected Eliminate, ProductOrder, or Lex");
	J := graphIdeal(f,MonomialOrder => order, VariableBaseName => local X);
	G := ring J;
	m := presentation M;
	xvars := map(G, R, submatrix(vars G, toList(0..n1-1)));
	m1 := presentation (cokernel xvars m  **  cokernel generators J);
	if opts.UseHilbertFunction and all((f,m),isHomogeneous) then (
	     p := poincare cokernel m;
     	     assert( degreesRing R === degreesRing G );
	     DG := degreesRing G;
	     hf := p * product(degrees source generators J, d -> 1 - DG_d);
	     assert( degreesRing ring m1 === ring hf );
	     (cokernel m1).cache.poincare = hf;
	     );
	deglen := degreeLength S;
	mapbackdeg := d -> take(d,-deglen);
	-- that choice of degree map was chosen to make the symmetricPower functor homogeneous, but it doesn't have much
	-- else to recommend it.
	-- we should really be *lifting* the result to S along the natural map S ---> G
	mapback := map(S, G, map(S^1, S^n1, 0) | vars S, DegreeMap => mapbackdeg );
	if isHomogeneous f then (
	     -- let's at least check it splits f's degree map:
	     for i from 0 to deglen-1 do (
		  e := for j from 0 to deglen-1 list if i === j then 1 else 0;
		  if mapbackdeg f.cache.DegreeMap e =!= e then error "not implemented yet: unexpected degree map of ring map";
		  ));
	cleanupcode := g -> mapback selectInSubring(if numgens target f > 0 then 1 else 0, generators g);
	f.cache#comp = (m1, cleanupcode);
	);
    if # f.cache#comp === 1
    then f.cache#comp#0
    else (
	 (m1, cleanupcode) = f.cache#comp;
	 g := gb(m1, new OptionTable from {
		   StopBeforeComputation => opts.StopBeforeComputation,
		   DegreeLimit => opts.DegreeLimit,
		   PairLimit => opts.PairLimit});
	 result := cleanupcode g;
	 --if isdone g then f.cache#comp = {result};  -- MES: There is NO way to check this yet!!
	 -- MES: check if the monomial order restricts to S.  If so, then do `` forceGB result ''
	 result
	 )
    )

-*
pushLinear := opts -> (f,M) -> (
    -- assumptions here:
    -- (a) f is homogeneous linear, and the linear forms are independent
    -- 
    -- First bring M over to a ring with an elimination order, which eliminates
    -- the variables 'not in' f.
    m := presentation M;    
    R := target f;
    S := source f;
    Rbase := ring m;
    fmat := lift(f.matrix,Rbase);
    n := numgens source f.matrix;
    n1 := numgens R - n;
    k := coefficientRing Rbase;
    X := local X;
    N := monoid [VariableBaseName => X, Variables => numgens R, MonomialOrder => Eliminate n1];
    R1 := k N;
    (Fto,Ffrom) := newCoordinateSystem(R1, fmat);
    m1 := Fto m;
    m1 = presentation (cokernel m1 ** cokernel Fto presentation R);
    if isHomogeneous f and isHomogeneous m then (
        hf := poincare cokernel m;
        T := (ring hf)_0;
        (cokernel m1).cache.poincare = hf;
        );
    gbopts := applyPairs(gbDefaults, (k,v) -> if opts#?k and k =!= Strategy then (k,opts#k) else (k,v));
    g := selectInSubring(1, generators gb(m1,gbopts));
    -- now map the answer back to S = source f.
    mapback := map(S, R1, map(S^1, S^n1, 0) | submatrix(vars S, {0..n-1}));
    mapback g
    )
*-

kernel Matrix := Module => opts -> (cacheValue symbol kernel) ((m) -> (
	  N := source m;
	  if m == 0 then return N;
	  P := target m;
	  if m.?RingMap then (
	       f := m.RingMap;
	       n := map(target m,f source m,raw m);
	       p := (pushNonLinear pushOptions)(f,coimage n);
	       image p)
	  else (
	       m = matrix m;
	       if P.?generators then m = P.generators * m;
	       h := modulo(m, if P.?relations then P.relations);
	       if N.?generators then h = N.generators * h;
	       subquotient( h, if N.?relations then N.relations))))
kernel RingElement := Module => options -> (m) -> kernel (matrix {{m}},options)

pushForward = method (Options => pushOptions)
pushForward(RingMap, Module) := Module => opts -> (f,M) -> (
     if isHomogeneous f and isHomogeneous M then (
	  -- given f:R-->S, and M an S-module, finite over R, find R-presentation for M
	  S := target f;
	  M = cokernel presentation M;
	  M1 := M / ideal f.matrix;
	  M2 := subquotient(matrix basis M1, relations M);
	  cokernel (pushNonLinear opts)(f,M2)
	  )
     else error "not implemented yet for inhomogeneous modules or maps"
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
