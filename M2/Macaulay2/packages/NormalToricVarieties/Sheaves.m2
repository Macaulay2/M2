------------------------------------------------------------------------------
-- Total Coordinate Rings and Coherent Sheaves 
------------------------------------------------------------------------------
ring NormalToricVariety := PolynomialRing => (
    cacheValue symbol ring) (
    X -> (
    	if isDegenerate X then 
      	    error "-- not yet implemented for degenerate varieties";
    	if not isFreeModule classGroup X then 
      	    error "-- gradings by torsion groups not yet implemented";
    	-- constructing ring
    	K := X.cache.CoefficientRing;	  
    	x := X.cache.Variable;	  
    	n := #rays X;
    	deg := entries transpose matrix fromWDivToCl X;
    	S := K (monoid [x_0..x_(n-1), Degrees => deg]);
    	S.variety = X;
    	S 
	)
    );

normalToricVariety Ring := NormalToricVariety => opts -> S -> variety S

ideal NormalToricVariety := Ideal => (
    cacheValue symbol ideal) (
    X -> (
    	S := ring X;
    	ideal apply (max X, L -> product (numgens S, 
		i -> if member (i,L) then 1_S else S_i) 
	    )
	)
    );
monomialIdeal NormalToricVariety := MonomialIdeal => X ->
    monomialIdeal ideal X

------------------------------------------------------------------------------
-- sheaves
------------------------------------------------------------------------------
sheaf (NormalToricVariety, Module) := CoherentSheaf => (X,M) -> (
    if ring M =!= ring X then 
    	error "-- expected the module and the variety to have the same ring";
    if not isHomogeneous M then 
    	error "-- expected a homogeneous module";
    -- constructing coherent sheaf
    new CoherentSheaf from {
    	symbol module  => M,
    	symbol variety => X
	}
    );
sheaf (NormalToricVariety, Ring) := SheafOfRings => (X,R) -> (
    if ring X =!= R then 
	error "-- expected the ring of the variety";
    new SheafOfRings from { 
      	symbol variety => X, 
      	symbol ring    => R
	}
    );
sheaf NormalToricVariety := X -> sheaf_X ring X

CoherentSheaf#{Standard,AfterPrint} = F -> (
    X := variety F;
    M := module F;
    << endl;				  -- double space
    n := rank ambient F;
    << concatenate(interpreterDepth:"o") << lineNumber << " : coherent sheaf on " << X;
    if M.?generators then
    if M.?relations then << ", subquotient of " << ambient F
    else << ", subsheaf of " << ambient F
    else if M.?relations then << ", quotient of " << ambient F;
    << endl;)

installMethod(symbol _, OO, NormalToricVariety, (OO,X) -> sheaf(X, ring X))

CoherentSheaf Sequence := CoherentSheaf => (F,a) -> sheaf(variety F, 
    F.module ** (ring F)^{toList(a)})
   
sheafHom(CoherentSheaf,CoherentSheaf) := (F,G) -> (
    sheaf(variety F, Hom(module F, module G)) );
Hom(CoherentSheaf,CoherentSheaf) := Module => (F,G) -> HH^0(variety F, sheafHom(F,G))

CoherentSheaf.directSum = args -> (
    assert(#args > 0);
    X := variety args#0;
    if not all(args, F -> variety F === X) then (
    	error "-- expected all sheaves to be over the same ring");
    sheaf(X, directSum apply(args, F -> F.module)) );

SheafOfRings Sequence := CoherentSheaf => (O,a) -> O^1 a

super   CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, super   module F)
ambient CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, ambient module F)
cover   CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, cover   module F)

minimalPresentation CoherentSheaf := 
prune CoherentSheaf := opts -> F -> (
    X := variety F;
    if instance(X, NormalToricVariety) then (
    	M := module F;
    	S := ring M;
    	B := ideal X;
    	N := saturate(image map(M,S^0,0), B);
    	if N != 0 then M = M/N;
    	C := res M;
    	-- is there a better bound?
    	a := max(1, max flatten flatten apply(length C +1, i -> degrees C#i));
    	return sheaf(X, minimalPresentation Hom(B^[a], M)) );
    sheaf minimalPresentation HH^0 F(>=0) );

cotangentSheaf NormalToricVariety := CoherentSheaf => opts -> (
    (cacheValue (symbol cotangentSheaf => opts)) (
	X -> (
      	    if isDegenerate X then 
		error "-- expect a non-degenerate toric variety";
      	    S := ring X;
      	    d := dim X;
      	    n := numgens S;
      	    nu := map (S^n, S^d, (matrix rays X) ** S);
      	    eta := map (directSum apply(n, i -> S^1 / ideal(S_i)), S^n, id_(S^n));
      	    om := sheaf (X, kernel (eta * nu));
      	    if opts.Minimize then om = minimalPresentation om;
      	    om 
	    )
	)
    );

cotangentSheaf(ZZ,NormalToricVariety) := CoherentSheaf => opts -> (i,X) -> 
  exteriorPower (i, cotangentSheaf (X,opts)) 


-- THIS FUNCTION IS NOT EXPORTED.  Given a normal toric variety, this function
-- creates a HashTable describing the cohomology of all twists of the
-- structure sheaf.  For more information, see Propositon~3.2 in
-- Maclagan-Smith "Multigraded regularity"
setupHHOO = X -> (
    X.cache.emsBound = new MutableHashTable;
    -- create a fine graded version of the total coordinate ring
    S := ring X;
    n := numgens S;
    fineDeg := entries id_(ZZ^n);
    h := toList (n:1);
    R := QQ (monoid [gens S, Degrees => fineDeg, Heft => h]);
    RfromS := map (R, S, gens R);
    B := RfromS ideal X;
    -- use simplicial cohomology find the support sets 
    quasiCech := Hom (res (R^1/B), R^1);
    supSets := delete ({}, subsets (toList (0..n-1)));
    d := dim X;
    sigma := new MutableHashTable;
    sigma#0 = {{{},1}};
    for i from 1 to d do (
    	sHH := prune HH^(i+1)(quasiCech);
    	sigma#i = for s in supSets list (
      	    m := product (s, j ->  R_j);
      	    b := rank source basis (-degree m, sHH);
      	    if b > 0 then {s,b} else continue
	    )
	);
    -- create rings
    degS := degrees S; 
    X.cache.rawHHOO = new HashTable from apply(d+1, 
	i -> {i, apply(sigma#i, s -> (
	  	    v := - degree product(n, 
			j -> if member(j,s#0) then S_j else 1_S
			);
	  	    degT := apply(n, 
	    		j -> if member(j,s#0) then -degS#j else degS#j
			);
	  	    T := (ZZ/2)(monoid [gens S, Degrees => degT]);
	  	    {v,T,s#0,s#1}
		    )
		)
	    }
	)
    );

-- Defines the Frobenius power of an ideal
Ideal ^ Array := (I,p) -> ideal apply (I_*, i -> i^(p#0))

-- THIS FUNCTION IS NOT EXPORTED.  This function creates a HastTable which
-- stores the data for determining the appropriate Frobenius power needed to
-- compute the cohomology of a general coherent sheaf; see Proposition 4.1 in
-- Eisenbud-Mustata-Stillman.
emsbound = (i, X, deg) -> (
    if not X.cache.emsBound#?{i,deg} then (
    	if i < 0 or i > dim X then X.cache.emsBound#{i,deg} = 1
    	else X.cache.emsBound#{i,deg} = max ( {1} | 
	    apply (X.cache.rawHHOO#i, 
		t -> # t#2 + max apply(first entries basis(deg-t#0,t#1),
	  	    m -> max (first exponents m)_(t#2)
		    )
		)
	    )
	);
    X.cache.emsBound#{i,deg}
    );

cohomology (ZZ, NormalToricVariety, CoherentSheaf) := Module => opts -> (i,X,F) -> (
    if ring F =!= ring X then 
    	error "-- expected a coherent sheaf on the toric variety";
    S := ring X;
    kk := coefficientRing S;
    if not isField kk then error "-- expected a toric variety over a field";
    if i < 0 or i > dim X then kk^0
    else (
    	if not X.cache.?rawHHOO then setupHHOO X;
    	M := module F;
    	if isFreeModule M then kk^(
      	    sum apply (degrees M, deg -> sum apply(X.cache.rawHHOO#i,
	  	    t -> rank source basis (-deg-t#0,(t#1)^(t#3))
		    )
		)
	    )
    	else (
      	    B := ideal X;
      	    C := res M;
      	    deg := toList (degreeLength S : 0);
      	    bettiNum := flatten apply (1+length C, 
		j -> apply (unique degrees C_j, alpha -> {j,alpha}));
      	    b1 := max apply (bettiNum, 
		beta -> emsbound(i+beta#0-1,X,deg-beta#1)
		);
      	    b2 := max apply(bettiNum, 
		beta -> emsbound(i+beta#0,X,deg-beta#1)
		);	       
      	    b := max(b1,b2);
      	    if i > 0 then 
		kk^(rank source basis (deg, Ext^(i+1)(S^1/B^[b],M)))
      	    else (
		h1 := rank source basis (deg, Ext^(i+1)(S^1/B^[b],M));
		h0 := rank source basis (deg, Ext^i(S^1/B^[b1],M));
		kk^(rank source basis (deg,M) + h1 - h0)
		)
	    )
	)
    );

cohomology (ZZ, NormalToricVariety, SheafOfRings) := Module => opts -> 
    (i, X ,O) -> HH^i (X, O^1)

euler CoherentSheaf := F -> (
    X := variety F;
    if class variety F === NormalToricVariety then 
    	return sum (1 + dim X, i -> (-1)^i * (rank HH^i(X,F)) );
    if class variety F === ProjectiveVariety then return euler module F;
    error "-- expected a sheaf on a ProjectiveVariety or NormalToricVariety");
