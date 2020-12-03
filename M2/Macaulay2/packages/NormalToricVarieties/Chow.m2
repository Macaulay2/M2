------------------------------------------------------------------------------
-- Chow and Chern 
------------------------------------------------------------------------------
-- abstract toric varieties
------------------------------------------------------------------------------
intersectionRing (NormalToricVariety, AbstractVariety) := Ring => (X, B) -> (
    if not isComplete X then 
        error "-- not yet implemented for a non-complete normal toric variety";    
    if not isSimplicial X then 
        error "-- not yet implemented for a non-simplicial normal toric variety";
    if not X.cache#?(intersectionRing, B) then X.cache#(intersectionRing, B) = (
    	kk := intersectionRing B;
    	n := # rays X;
    	t := getSymbol "t";
    	R := kk(monoid [t_0 .. t_(n-1), Join => false]);
    	M := dual monomialIdeal apply (max X, 
	    L -> product (n, i -> if member (i,L) then 1_R else R_i)
	    );
    	L := ideal (vars R * (matrix rays X) ** R);
    	R / (M + L)
    	);
    X.cache#(intersectionRing, B)
    );
intersectionRing NormalToricVariety := Ring => X -> intersectionRing (X, point)

abstractVariety (NormalToricVariety, AbstractVariety) := AbstractVariety => 
    opts -> (X, B) -> (
    	if not isComplete X then 
  	    error "-- not yet implemented for a non-complete normal toric variety";        
    	if not isSimplicial X then 
    	    error "-- not yet implemented for a non-simplicial normal toric variety";
    	if not X.cache#?(abstractVariety, B) then X.cache#(abstractVariety, B) = (
	    A := intersectionRing (X, B);
	    aX := abstractVariety (dim X, A);
	    aX.TangentBundle = abstractSheaf (aX, 
	    	Rank       => dim X, 
	    	ChernClass => product(gens A, x -> 1+x)
	    	);
	    -- Now we determine the mapping 'integral':
	    raysX := transpose matrix rays X;
	    onecone := first max X;
	    pt := (abs det raysX_onecone) * product(onecone, i -> A_i);
	    if size pt != 1 then 
	        error "-- cannot define integral: some strange error has occurred";
	    mon := leadMonomial pt;
	    cf := leadCoefficient pt;
	    if not liftable(cf, QQ) then 
	        error "-- cannot create integral function";
	    a := 1 / lift(cf, QQ);
	    integral A := f -> a * coefficient(mon, f);
	    -- a check:
	    assert all (max X, f -> integral(product(f, i -> A_i)) == 1 / (abs det raysX_f));
	    aX
	    );
    	X.cache#(abstractVariety, B)
    	);
abstractVariety NormalToricVariety := opts -> X -> abstractVariety (X, point)

abstractSheaf (NormalToricVariety, AbstractVariety, ToricDivisor) := opts -> 
    (X, B, D) -> (
    	aX := abstractVariety (X, B);
    	A := intersectionRing aX;
    	OO_aX ( ((vars A) * vector D)_0 )
    	);
abstractSheaf (NormalToricVariety, ToricDivisor) := opts -> (X, D) -> 
    abstractSheaf (X, point, D)

-- THIS FUNCTION IS NOT EXPORTED.  For a given normal toric variety, it
-- constructs a one-sided inverse for the map from the group of Weil divisors
-- to the class group.  It is used, in the next method, to choose an
-- equivariant representative for a given line bundle.
liftClToWDiv := X -> (
    A := fromWDivToCl X;
    I := id_(ZZ^(numRows A));
    if I % A != 0 then 
	error "-- cannot a lift from the class group to group of weil divisors?";
    I // A
    )
abstractSheaf (NormalToricVariety, AbstractVariety, CoherentSheaf) := opts -> 
    (X, B, F) -> (
      	if variety F =!= X then 
	    error "-- expected a coherent sheaf over the given variety";
      	M := module F;
        aX := abstractVariety (X, B);
       	A := intersectionRing aX;
      	lift := liftClToWDiv X;
	f := poincare M;
	p := pairs standardForm f;
	m := numgens ring f;
	r := sum (p, (d, c) -> (
		deg := matrix {apply (m, j -> if d#?j then -d#j else 0)};
		L := OO_aX first first entries (vars A * lift * transpose deg);
		c * ch L
		)
    	    );
	abstractSheaf (aX, ChernCharacter => r)
	);
abstractSheaf (NormalToricVariety, CoherentSheaf) := opts -> (X, F) -> 
    abstractSheaf (X, point, F)    	       
    
    
chern CoherentSheaf := RingElement => F -> chern abstractSheaf (variety F, F)
chern (ZZ, CoherentSheaf) := RingElement => (p, F) -> (
    chern (p, abstractSheaf (variety F, F)))
chern (ZZ, ZZ, CoherentSheaf) := RingElement => (p, q, F) -> (
    chern (p, q, abstractSheaf (variety F, F)))

ctop CoherentSheaf := RingElement => F -> ctop abstractSheaf (variety F, F)

ch CoherentSheaf := RingElement => F -> ch abstractSheaf (variety F, F)
ch (ZZ, CoherentSheaf) := RingElement => (n, F) -> ch (n, abstractSheaf (variety F, F))

todd CoherentSheaf := RingElement => F -> todd abstractSheaf (variety F, F)
todd NormalToricVariety := RingElement => X -> todd abstractVariety X

chi CoherentSheaf := RingElement => F -> chi abstractSheaf (variety F, F)

segre CoherentSheaf := RingElement => F -> segre abstractSheaf (variety F, F)
segre (ZZ, CoherentSheaf) := RingElement => (p, F) -> segre (p, abstractSheaf (variety F, F))


hilbertPolynomial NormalToricVariety := RingElement => opts -> (
    cacheValue symbol hilbertPolynomial) (
    X -> (
	if not isSmooth X then 
	error "-- not yet implemented for singular normal toric varieties";
	if not isComplete X then 
	error "-- not yet implemented for non-complete normal toric varieties";
	r := rank picardGroup X;
	i := getSymbol "i";	   
	pt := base (i_0 .. i_(r-1));
	V := abstractVariety (X, pt);
	S := intersectionRing V;
	R := coefficientRing S;
	sigma := first max X;
	sigmaVee := select (# rays X, j -> not member (j, sigma));
	f := ((vars S)_sigmaVee * 
	    ((fromCDivToPic X)_sigmaVee)^(-1) * transpose vars R)_(0,0);
	chi OO_V (f)
	)
    );

hilbertPolynomial (NormalToricVariety, Module) := RingElement => opts -> 
    (X,M) -> (
  	if not isHomogeneous M then error "-- expected a homogeneous module";
  	if ring X =!= ring M then 
	    error "-- expected a module over the total coordinate ring";
  	h := hilbertPolynomial X;
  	R := ring h;
  	r := numgens R;
  	f := poincare M;
  	p := pairs standardForm f;
  	if #p === 0 then 0_R
  	else sum (p, (d,c) -> (
      		shift := apply (r, j -> if d#?j then R_j - d#j else R_j);
      		c * sub (h, matrix{shift})
		)
	    )
	);

hilbertPolynomial (NormalToricVariety, Ring) := RingElement => opts -> 
    (X, S) -> hilbertPolynomial (X, S^1, opts)
hilbertPolynomial (NormalToricVariety, SheafOfRings) := RingElement => 
    opts -> (X, S) -> hilbertPolynomial (X, ring S, opts)    
hilbertPolynomial (NormalToricVariety, Ideal) := RingElement => opts -> 
    (X, I) -> hilbertPolynomial (X, (ring I)^1/I, opts)
hilbertPolynomial (NormalToricVariety, CoherentSheaf) := RingElement => 
    opts -> (X, F) -> hilbertPolynomial (X, module F, opts)
