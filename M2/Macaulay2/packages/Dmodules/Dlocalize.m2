-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Takes a cyclic holonomic module D_n/I and returns 
-- localization D_n/I [1/f] as D_n module
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Dlocalization = method( Options => {Strategy => OTW})
Dlocalization(Ideal, RingElement) := options -> (I, f) -> (print("WARNING! Dlocalization is an obsolete name for Dlocalize"); Dlocalize(I,f,options))
Dlocalization(Module, RingElement) := options -> (M, f) -> (print("WARNING! Dlocalization is an obsolete name for Dlocalize"); Dlocalize(M,f,options))
     
Dlocalize = method( Options => {Strategy => OTW})
Dlocalize(Ideal, RingElement) := options -> (I, f) -> (
     Dlocalize((ring I)^1/I, f, options) )
Dlocalize(Module, RingElement) := options -> (M, f) -> (
     outputRequest := {LocModule};
     outputTable := computeLocalization(M, f, outputRequest, options);
     outputTable#LocModule )

DlocalizationMap = method( Options => {Strategy => OTW})
DlocalizationMap(Ideal, RingElement) := options -> (I, f) -> (print("WARNING! Dlocalization is an obsolete name for Dlocalize"); DlocalizeMap(I,f,options))
DlocalizationMap(Module, RingElement) := options -> (M, f) -> (print("WARNING! Dlocalization is an obsolete name for Dlocalize"); DlocalizeMap(M,f,options))
     
DlocalizeMap = method( Options => {Strategy => OTW})
DlocalizeMap(Ideal, RingElement) := options -> (I, f) -> (
     DlocalizeMap((ring I)^1/I, f, options) )
DlocalizeMap(Module, RingElement) := options -> (M, f) -> (
     outputRequest := {LocMap};
     outputTable := computeLocalization(M, f, outputRequest, options);
     outputTable#LocMap )

DlocalizationAll = method( Options => {Strategy => OTW})
DlocalizationAll(Ideal, RingElement) := options -> (I, f) -> 
(print("WARNING! Dlocalization is an obsolete name for Dlocalize"); DlocalizeAll(I,f,options))
DlocalizationAll(Module, RingElement) := options -> (M, f) -> 
(print("WARNING! Dlocalization is an obsolete name for Dlocalize"); DlocalizeAll(M,f,options))
     
DlocalizeAll = method( Options => {Strategy => OTW})
DlocalizeAll(Ideal, RingElement) := options -> (I, f) -> (
     DlocalizeAll((ring I)^1/I, f, options) )
DlocalizeAll(Module, RingElement) := options -> (M, f) -> (
     outputRequest := {LocModule, LocMap, Bfunction, 
	  IntegrateBfunction, Boperator, GeneratorPower, annFS};
     computeLocalization(M, f, outputRequest, options) )

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
--
-- computeLocalization (Module M, RingElement f, List output)
--
-- Computes the localization and returns a hash table of outputs according to the 
-- request given by "output"
--
--
-- Two different strategies are possible:
--
-- 1. "Oaku" -- appears in Walther's paper on local cohomology and 
--     based on Oaku's work
--
-- 2. "OTW" -- appears in paper of Oaku-Takayama-Walther on localization
--
--
-- Possible output formats: 
--
--     LocModule, LocMap, Bpolynomial, Boperator
--
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
computeLocalization = (M, f, output, options) -> (
   if f == 0 then error "can't localize at 0";
   W := ring M;
   r := numgens target gens M; 
   -- case 1: M is a proper submodule of (D_n)^r/N
   -- MES: the original version did not use zeroize.
   ensureQuotientModule(M, "expected input to be a cokernel");
   -- case 2: M is a cokernel
   
   outputList := {};
   pInfo(1, "localize: Computing localization with " | 
	toString options.Strategy | " strategy...");
   
   if (options.Strategy === Oaku) then (
	pInfo(1, "localize: Warning: Oaku's strategy requires 
	     module to be f-saturated");
	if r > 1 then error "non-cyclic modules not yet supported with this strategy";
	I := ideal relations M;
     	tInfo := toString first timing (AnnI := AnnIFs2 (I,f););
	pInfo(2, "\t\t\t time = " | tInfo | " seconds");
     	Ws := ring AnnI;
     	ns := numgens Ws;
	
	elimWs := (coefficientRing Ws)(monoid [(entries vars Ws)#0,
	     WeylAlgebra => Ws.monoid.Options.WeylAlgebra,
	     MonomialOrder => Eliminate (ns-1)]);
     	ff := substitute(f,elimWs);
     	elimAnnI := substitute(AnnI, elimWs);
     	H := gens elimAnnI | matrix{{ff}};

	if member(Boperator, output) then (
	     pInfo(1, "localize: computing Bpoly and Bop...");
     	     tInfo = toString first timing (
	     	  gbH := gb(H, ChangeMatrix => true);
     	     	  bpolys := selectInSubring(1, gens gbH);
     	     	  if bpolys == 0 then error "module not specializable";
     	     	  if rank source bpolys > 1 then error "ideal principal but not
     	     	  realized as such.  Need better implementation";
     	     	  bpoly := bpolys_(0,0);
	     	  ind := position((entries gens gbH)#0, i -> (i == bpoly));
     	     	  C := getChangeMatrix gbH;
     	     	  tempBop := C_(numgens source H - 1, ind);     
	     	  Bop := tempBop;
		  );
	     pInfo(2, "\t\t\t time = " | tInfo | " seconds");	     
	     )
	else (
	     pInfo(1, "localize: computing Bpoly...");
     	     tInfo = toString first timing (
	     	  bpoly = (mingens ideal selectInSubring(1, gens gb H))_(0,0);
		  );
	     pInfo(2, "\t\t\t time = " | tInfo | " seconds");	     
	     if bpoly == 0 then error "module not specializable";
	     );

     	bpoly = substitute(bpoly, (coefficientRing W)(monoid [Ws_(ns-1)]));
     	bestPower := min (getIntRoots (bpoly));
        if bestPower == infinity then bestPower = 0;
     	locIdeal := substitute(substitute(AnnI, {Ws_(ns-1) => bestPower}), W);
     	locModule := W^1/locIdeal;
     	local locMap;
	
     	if member (LocMap, output) then (
	     if locModule == 0 then locMap = map(W^0, M, map(W^0,W^1,0))
       	     else (
	     	  if bestPower > 0 then (
	     	       pInfo(1, "Warning: Still need to add b-operator.  Adjusting
		       generator to make localization map simple");
		       bestPower = 0;
		       locIdeal = substitute(substitute(AnnI, 
			    	 {Ws_(ns-1) => bestPower}), W);
     		       locModule = W^1/locIdeal;
		       locMap = map(locModule, M, matrix{{f^(-bestPower)}})
		       )
	     	  else locMap = map(locModule, M, matrix{{f^(-bestPower)}});
		  );
	     );
	)
   else if options.Strategy === OTWcyclic then (
       if r > 1 then error "non-cyclic modules not yet supported with this strategy";
       N := relations M;
       nW := numgens W;
       createDpairs W;
       n := #W.dpairVars#0;
       -- create the auxilary ring D_n<a,Da> 
       a := symbol a;
       Da := symbol Da;
       LW := (coefficientRing W)(monoid [(entries vars W)#0, a, Da,
	       WeylAlgebra => append(W.monoid.Options.WeylAlgebra, a=>Da)]);
       a = LW_a;
       Da = LW_Da;
       nLW := numgens LW;
       WtoLW := map(LW, W, (vars LW)_{0..nW-1});
       LWtoW := map(W, LW, (vars W) | matrix{{0_W,0_W}});
       -- weight vectors for integration to a = 0
       w := append( toList(n:0), -1);
       wt := join( toList(nW:0), {1,-1} );
       -- twist generators of I into generators of twistI;
       Lf := WtoLW f;
       twistList := apply( toList(0..nLW-1), 
	   i -> LW_i - (LW_i*Lf - Lf*LW_i) * a^2 * Da );
       twistMap := map(LW, LW, matrix{twistList});
       LN := WtoLW N;
       twistN := matrix{{1-Lf*a}} | twistMap LN;
       pInfo (1, "localize: computing Bpoly...");
       tInfo = toString first timing (
	   bpoly = bFunction(ideal twistN, w);
	   );
       pInfo(2, "\t\t\t time = " | tInfo | " seconds");
       if bpoly == 0 then (
	   error "Module not specializable. Localization cannot be computed.";
	   );
       bpoly = substitute(bpoly, {(ring bpoly)_0 => (ring bpoly)_0 + 1});
       intRoots := getIntRoots(bpoly);
       if #intRoots == 0 then maxRoot := -infinity
       else maxRoot = max intRoots;
       -- case 1: no non-negative integer roots
       if maxRoot < 0 then (
	   locModule = W^0;
	   locMap = map(W^0, M, transpose compress matrix{{0_W}});
	   maxRoot = 0;
	   bestPower = -2;
	   )
       -- case 2: localization generated by (1/f)^(maxroot+2)
       else (
	   bestPower = -maxRoot - 2;
	   pInfo(1, "localize: computing GB...");
	   tInfo = toString first timing (
	       G := gens gbW2 (ideal twistN, wt);
	       );
	   pInfo(2, "\t\t\t time = " | tInfo | " seconds");
	   i := 0;
	   relationsList := {};
	   while i < numgens source G do (
	       gi := G_(0,i);
	       weight := max apply(exponents gi, e -> sum(e, wt, (b,c)->b*c) );
	       j := 0;
	       while j <= maxRoot - weight do (
		   tmp := Fourier (a^j * gi);
		   relationsList = append(relationsList, 
		       FourierInverse substitute(tmp, {a => 0}) );
		   j = j+1;
		   );
	       i = i+1;
	       );
	   relationsMat := transpose matrix{relationsList};
	   --tempL := oldCoefficients({nLW-2}, relationsMat);
	   tempL := coefficients(transpose relationsMat, Variables=>{nLW-2});
	   presMat := tempL#1;
	   srcSize := numgens source presMat;
	   targSize := numgens target presMat;
	   genIndex := position((entries tempL#0)#0, e->(e==a^maxRoot));
	   permList := apply( targSize, i ->
	       if i == genIndex then targSize-1
	       else if i == targSize-1 then genIndex
	       else i );
	   presMat = map(LW^targSize, LW^srcSize, presMat^permList);
	   -- eliminate the first "maxRoot" components to get annihilating ideal
	   -- of "a^(maxRoot)"
	   homVar := symbol homVar;
	   HW := (coefficientRing W)(monoid [homVar, (entries vars W)#0,
	       	   WeylAlgebra => W.monoid.Options.WeylAlgebra,
	       	   MonomialOrder => Eliminate 1]);
	   HWtoW := map(W, HW, matrix{{1_W}} | (vars W) );
	   WtoHW := map(HW, W, (vars HW)_{1..numgens W});
	   I1 := LWtoW presMat;
	   I2 := WtoHW I1;
	   I3 := transpose ( HW_0 * (transpose I2)_{0..(targSize)-2} | 
	       (transpose I2)_{targSize - 1} );
	   pInfo(1, "localize: computing presentation...");
	   tInfo = toString first timing (
	       I4 := gens gb I3;
	       );
	   pInfo(2, "\t\t\t time = " | tInfo | " seconds");
	   I5 := map(HW^(numgens target I4), HW^(numgens source I4), I4); 
	   testMap := map(HW^targSize, HW^targSize,
	       matrix append( toList(targSize-1 : toList(targSize:0_HW)),
		   append(toList(targSize-1:0_HW), 1_HW) ) );
	   i = 0;
	   tempList := {};
	   while i < numgens source I5 do (
	       if testMap * I5_{i} == I5_{i} then (
		   tempList = append(tempList, I5_(targSize-1,i)) );
	       i = i+1;
	       );
	   if tempList === {} then (
	       locModule = W^0;
	       locMap = map(W^0, M, transpose compress matrix{{0_W}});
	       )
	   else (
	       locModule = cokernel HWtoW matrix{tempList};
	       locMap = map(locModule, M, matrix{{f^(-bestPower)}});
	       );	     
	   );
       )
   else if options.Strategy === OTW then (
       N = matrix presentation M;
       nW = numgens W;
       createDpairs W;
       n = #W.dpairVars#0;
       m :=numColumns(N);
       -- create the auxilary ring D_n<a,Da> 
       a = symbol a;
       Da = symbol Da;
       LW = (coefficientRing W)(monoid [(entries vars W)#0, a, Da,
	       WeylAlgebra => append(W.monoid.Options.WeylAlgebra, a=>Da)]);
       a = LW_a;
       Da = LW_Da;
       nLW = numgens LW;
       WtoLW = map(LW, W, (vars LW)_{0..nW-1});
       LWtoW = map(W, LW, (vars W) | matrix{{0_W,0_W}});
       -- weight vectors for integration to a = 0
       w = append( toList(n:0), -1);
       wt = join( toList(nW:0), {1,-1} );
       -- twist generators of I into generators of twistI;
       Lf = WtoLW f;
       twistList = apply( toList(0..nLW-1), 
	   i -> LW_i - (LW_i*Lf - Lf*LW_i) * a^2 * Da );
       twistMap = map(LW, LW, matrix{twistList});
       diag := apply( toList(0..r-1),
	   i -> twistList);
       LN=mutableMatrix(LW,r,m);
       for i from 0 to r-1 do  
       for j from 0 to m-1 do
       LN_(i,j)= twistMap(WtoLW N_(i,j));
       col := apply( toList(0..r-1),
	   i -> 1-Lf*a);
       twistN = diagonalMatrix(col) | matrix LN;
       locIntegrationModule := Dintegration(0,coker twistN,-w);
       --realize it over the original Weyl Algebra 
       locModule = map(W,ring locIntegrationModule, gens W)**locIntegrationModule;  
       bpoly = value locIntegrationModule.cache.BFunction;
       bestPower = -max(getIntRoots bpoly | {0}) - 2;
       locMap = if locModule == 0 then map(W^0, M, map(W^0,W^1,0))
       else map(locModule, M, f^(-bestPower)*map(W^r));
       )
   else error "Only recognizes strategies Oaku, OTWcyclic, and OTW (default).";

   if member(LocModule, output) then outputList = append(outputList, 
	LocModule => locModule);
   if member(LocMap, output) then outputList = append(outputList,
	LocMap => locMap);
   if member(GeneratorPower, output) then outputList = append(outputList, 
	GeneratorPower => bestPower);
   if options.Strategy === OTWcyclic then (
   	if member(IntegrateBfunction, output) then outputList = append(outputList,
	     IntegrateBfunction => factorBFunction bpoly);
	);
    if options.Strategy === OTW then (
   	if member(IntegrateBfunction, output) then outputList = append(outputList,
	     IntegrateBfunction => factorBFunction bpoly);
	);
   if options.Strategy === Oaku then (
   	if member(Bfunction, output) then outputList = append(outputList,
	     Bfunction => factorBFunction bpoly);
	if member(annFS, output) then outputList = append(outputList,
	     annFS => AnnI);  
   	if member(Boperator, output) then outputList = append(outputList,
	     Boperator => Bop);
   	);
   hashTable outputList
   )

AnnIFs2 = method()
AnnIFs2(Ideal, RingElement) := (I, f) -> (
     pInfo(1, "computing AnnIFs... ");
     W := ring I;
     n := numgens W;
     
     t := symbol t;
     dt := symbol dt;
     WAopts := W.monoid.Options.WeylAlgebra | {t => dt};
     WT := (coefficientRing W)(monoid [ t, dt, (entries vars W)#0, 
	  WeylAlgebra => WAopts,
	  MonomialOrder => Eliminate 2 ]);
     u := symbol u;
     v := symbol v;
     WTUV := (coefficientRing W)(monoid [ u, v, t, dt, (entries vars W)#0,
	  WeylAlgebra => WAopts,
	  MonomialOrder => Eliminate 2 ]);
     u = WTUV_symbol u;
     v = WTUV_symbol v;
     t = WTUV_symbol t;
     dt = WTUV_symbol dt;
     WtoWTUV := map(WTUV, W, (vars WTUV)_{4..n+3});
     -- twist generators of I into generators of KI
     f' := substitute(f,WTUV);
     twistList := join({u,v,t-f',dt}, apply( toList(4..n+3), 
	  i -> WTUV_i + (WTUV_i*f' - f'*WTUV_i)*dt));
     twistMap := map(WTUV, WTUV, matrix{twistList});
     tempKI := twistMap(ideal (t) + WtoWTUV I);
     wts := {1,-1,1,-1} | toList(n:0);
     KI := ideal homogenize(gens tempKI, u, wts);
     
     g := (entries gens KI)#0 | { u * v - 1 };
     preGens := flatten entries substitute(
	  selectInSubring(1, gens gb ideal g), WT);
     s := symbol s;
     WS := (coefficientRing W)(monoid [(entries vars W)#0, s,
	  WeylAlgebra => W.monoid.Options.WeylAlgebra]);
     s = WS_symbol s;
     t = WT_symbol t;
     dt = WT_symbol dt;
     WTtoWS := g -> (
	  e := first exponents leadMonomial g;
	  if e#0 > e#1 then g = dt^(e#0-e#1) * g
	  else g = t^(e#1-e#0) * g;
	  g' := 0_WS;
	  while (d := first exponents leadMonomial g; d#0 * d#1 != 0) do(
	       c := leadCoefficient g;
	       g' = g' + c * (-s-1)^(d#1) * WS_(drop(d, 2) | {0}); -- >%-0	
	       g = g - c * (t*dt)^(d#1) * WT_({0,0} | drop(d, 2));
	       ); 
	  g' + substitute(g, WS)
	  );
     ideal (preGens / WTtoWS) 
     )

----------------------------------------
-- TESTS

TEST ///
-- Test 1: Boundary cases 
x = symbol x; dx = symbol dx; 
W = QQ[x,dx,WeylAlgebra=>{x=>dx}];
M = cokernel matrix{{dx}};
assert(Dlocalize(M, 1_W) == M);
assert(Dlocalize(M, 1_W, Strategy => Oaku) == M);

-- simplest localization
assert(target DlocalizeMap(M, x) == Dlocalize(M,x))
assert(target DlocalizeMap(M, x, Strategy => Oaku) ==
     Dlocalize(M, x, Strategy => Oaku))

-- Pure torsion module
M = cokernel matrix{{x}};
assert(Dlocalize(M, x) == W^0);
assert(DlocalizeMap(M, x) == 0);
assert(Dlocalize(M, x, Strategy => Oaku) == M)
assert(DlocalizeMap(M, x, Strategy => Oaku) == map(M))

-- Already localized
M = cokernel matrix{{x*dx+3/2}};
assert(Dlocalize(M, x) == cokernel matrix{{x*dx+3/2+2}});
assert(Dlocalize(M, x, Strategy => Oaku) == M);
assert(DlocalizeMap(M, x, Strategy => Oaku) == map(M));
-- Test 2: from Coutinho "Primer..."
u = symbol u; Du = symbol Du;
n = 4;
W = QQ[u_1..u_n, Du_1..Du_n, WeylAlgebra => 
     apply(toList(1..n), i -> u_i => Du_i)];
M = W^1/ideal(Du_1..Du_n);
f = sum(toList(1..n), i -> u_i^2);
assert(Dlocalize(M, f) == Dlocalize(M, f, Strategy => Oaku));
assert(DlocalizeMap(M, f) == DlocalizeMap(M, f, Strategy => Oaku));
///
