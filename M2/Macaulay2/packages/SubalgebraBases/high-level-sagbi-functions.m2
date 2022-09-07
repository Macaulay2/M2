-- internalVerifySagbi is a version of verifySabi just for SAGBIBasis objects
-- it returns a SAGBIBasis object with its SB#"data"#"sagbiDone" flag correctly set
-- it is used as an intermediate step for verifySagbi since matrices and subrings
--   are handled slightly differently
-- 

internalVerifySagbi = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
	Limit => 100,
	PrintLevel => 0, -- see print level for sagbi
    	Recompute => false,
	RenewOptions => false
	}
    );


internalVerifySagbi(SAGBIBasis) := opts -> SB -> (
    compTable := initializeCompTable(SB, opts);    
    -- Get the SPairs
    sagbiGB := gb(compTable#"ideals"#"reductionIdeal");
    k := rawMonoidNumberOfBlocks(raw monoid (compTable#"rings"#"tensorRing")) - 2;
    zeroGens := selectInSubring(k, gens sagbiGB);
    SPairs := compTable#"maps"#"fullSubstitution"(zeroGens) % compTable#"ideals"#"I";
    -- Reduce the SPairs
    reducedSPairs := compSubduction(compTable, SPairs);
    -- check the sagbi gens are high enough degree (i.e. higher than the subring generators)
    highEnoughDegree := max flatten (degrees compTable#"data"#"subalgebraGenerators")_1 <= max flatten (degrees compTable#"data"#"sagbiGenerators")_1;
    -- if all the reduced SPairs are zero then we have a sagbiBasis
    compTable#"data"#"sagbiDone" = zero(reducedSPairs) and highEnoughDegree;
    sagbiBasis compTable
    );

-- verifySagbi [no longer used in any of the methods - it is here for legacy purposes]
-- checks whether or not the generators of a subring S form a sagbi basis wrt the given term order
-- 
-- the method uses the code from inside the method that collects SPairs to find them
-- the method uses the subduction fuction so setting the option SubductionMethod will change the strategy
-- print level will be called inside of subduction so it can be used to perform subduction 
--
-- the resulting SAGBIBasis object SB will have SB#"data"#"sagbiDone" updated appropriately
-- the function then returns true or false depending on whether the generating set is a SAGBIBasis 
--

verifySagbi = method(
    TypicalValue => Subring,
    Options => {
	Compute => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
	Limit => 100,
	PrintLevel => 0, -- see print level for sagbi
    	Recompute => false,
	RenewOptions => false
	}
);

verifySagbi(Subring) := opts -> S -> (
    local SB;

    if (S#cache#?"SAGBIBasis") and (S#cache#"SAGBIBasis"#"data"#"sagbiGenerators" == gens S) then (
	-- S has a sagbi basis so use this object as a compTable
	SB = S#cache#"SAGBIBasis";
	) else (
	SB = initializeCompTable(sagbiBasis S, opts);
	-- add the generators to the sagbiGenerators
	SB#"data"#"sagbiGenerators" = gens S;
	updateComputation(SB);
	SB = sagbiBasis SB;
	);
    
    SB = internalVerifySagbi(opts, SB);
    if not S.cache#?"SAGBIBasis" then (
    	S.cache#"SAGBIBasis" = SB; -- add a new SAGBIBasis if there wasn't one already
	) else if (SB#"data"#"sagbiDone" and not S.cache#"SAGBIBasis"#"data"#"sagbiDone") then (
	S.cache#"SAGBIBasis" = SB; -- update the SAGBIBasis if we managed to newly verify the sagbi basis
	);
    SB#"data"#"sagbiDone"
    )

verifySagbi(Matrix) := opts -> M -> (
    local S;
    local SB;

    if (M#cache#?"Subring") and (M#cache#"Subring"#cache#"SAGBIBasis"#"data"#"sagbiGenerators" == M) then (
	-- S has a sagbi basis so use this object as a compTable
	S = M.cache#"Subring";
	SB = S.cache#"SAGBIBasis";
	) else (
	SB = initializeCompTable(sagbiBasis M, opts);
	-- add the generators to the sagbiGenerators
	SB#"data"#"sagbiGenerators" = M;
	updateComputation(SB);
	SB = sagbiBasis SB;
	S = SB#"data"#"subring";
	M.cache#"Subring" = S;
	);
    
    SB = internalVerifySagbi(opts, SB);
    if not S.cache#?"SAGBIBasis" then (
    	S.cache#"SAGBIBasis" = SB; -- add a new SAGBIBasis if there wasn't one already
	) else if (SB#"data"#"sagbiDone" and not S.cache#"SAGBIBasis"#"data"#"sagbiDone") then (
	S.cache#"SAGBIBasis" = SB; -- update the SAGBIBasis if we managed to newly verify the sagbi basis
	);
    SB#"data"#"sagbiDone"
    )

-- A list does not have a cache, so verifySagbi on a list 
--   is only checking whether it forms a sagbi basis
--   the SAGBIBasis object becomes inaccessible

verifySagbi(List) := opts -> L -> (
    verifySagbi(opts, subring L)
    )


-- internalIsSAGBI is a version of isSAGBI for a SAGBIBasis a object SB
-- it returns a modified SAGBIBasis object with the SB#"data"#"sagbiDone" flag correctly set
-- it is used as an intermediate step for isSAGBI when it is passed something
--   that does not have a cached SAGBIBasis object

internalIsSAGBI = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
	Strategy => "Master",
        SubductionMethod => "Top",
	Limit => 100,
	PrintLevel => 0,
    	Recompute => false,
	RenewOptions => false
	}
    );

internalIsSAGBI(SAGBIBasis) := opts -> SB -> (
    compTable := initializeCompTable(SB, opts);    
    -- Get the SPairs
    sagbiGB := gb(compTable#"ideals"#"reductionIdeal");
    k := rawMonoidNumberOfBlocks(raw monoid (compTable#"rings"#"tensorRing")) - 2;
    zeroGens := selectInSubring(k, gens sagbiGB);
    SPairs := compTable#"maps"#"fullSubstitution"(zeroGens) % compTable#"ideals"#"I";
    -- Reduce the SPairs
    reducedSPairs := compSubduction(compTable, SPairs);
    -- check the sagbi gens are high enough degree (i.e. higher than the subring generators)
    highEnoughDegree := max flatten (degrees compTable#"data"#"subalgebraGenerators")_1 <= max flatten (degrees compTable#"data"#"sagbiGenerators")_1;
    -- if all the reduced SPairs are zero then we have a sagbiBasis
    compTable#"data"#"sagbiDone" = zero(reducedSPairs) and highEnoughDegree;
    -- if the computation gives a sagbi basis then the don't recompute in the future
    if compTable#"data"#"sagbiDone" then (
	compTable#"options"#Recompute = false;
	) else (
	compTable#"options"#Recompute = true; 
	);
    sagbiBasis compTable
    );


-- isSAGBI determines / checks if the generators of an objects form a sagbi basis
-- isSAGBI SAGBIBasis checks the flag of the SAGBIBasis
-- isSAGBI Subring checks the generators of the subring
-- if an object has a cached SAGBIBasis, then it checks the flag of that cached object: 
--   e.g. Matrix that comes from gens SAGBIBasis
-- if an object that does not have a cached SAGBIBasis object is passed then 
--   if Compute == true then we perform 1-step of the sagbi algorithm to check
--      whether all the S-polys subduct to 0
--   otherwise we output null

isSAGBI = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
	Strategy => "Master",
	SubductionMethod => "Top",
	Limit => 0, 
	PrintLevel => 0,
	Recompute => false,
	RenewOptions => false
	}
    );

isSAGBI SAGBIBasis := opts -> SB -> (
    if SB#"data"#"sagbiDone" or (not opts.Compute) then (
        SB#"data"#"sagbiDone"
	) else (
	SB' := internalVerifySagbi(SB, opts);
	if SB'#"data"#"sagbiDone" then ( -- only update the Subring of the SAGBIBasis if we have a sagbi basis
	    S := SB#"data"#"subring";
	    S.cache#"SAGBIBasis" = SB';
	    );
	SB'#"data"#"sagbiDone"
	)
    )

isSAGBI Subring := opts -> S -> (
    local SB;
    local compTable;
    if S.cache#?"SAGBIBasis" then (
	SB = S.cache#"SAGBIBasis";
	if isSubset(first entries gens SB , first entries gens S) then (
	    isSAGBI(opts, SB)
	    ) else (
	    false
	    ) 
	) else (
	if opts.Compute then (
	    -- construct a SAGBIBasis for S and verify whether it is a sagbi basis
	    compTable = initializeCompTable(sagbiBasis S, opts);
	    -- add the generators to the sagbiGenerators
	    compTable#"data"#"sagbiGenerators" = gens S;
	    updateComputation(compTable);
	    SB = sagbiBasis compTable;
	    SB = internalIsSAGBI(opts, SB);
    	    S.cache#"SAGBIBasis" = SB;
    	    SB#"data"#"sagbiDone"
	    ) else ( 
	    null -- S has no SAGBIBasis cached and Compute is set to false
	    )
	)
    )    

isSAGBI Matrix := opts -> M -> (
    local S;
    local SB;
    if (M.cache#?"Subring") and (M.cache#"Subring".cache#?"SAGBIBasis") then (
	S = M.cache#"Subring";
	SB = S.cache#"SAGBIBasis";
	if isSubset(first entries gens SB, first entries M) then (
	    isSAGBI(opts, SB)
	    ) else (
	    false
	    ) 
	) else (
	S = subring M;
	M.cache#"Subring" = S;
	isSAGBI(opts, S)
	)
    )

isSAGBI List := opts -> L -> (
    local S;
    S = subring L;
    isSAGBI(opts, S)
    )


-- groebnerMembershipTest(f, S) = (f lies in S)
-- f an element of the ambient ring of S
-- S a subring of a polynomial ring (or quotient ring)
--
groebnerMembershipTest = method() 
groebnerMembershipTest(RingElement, Subring) := (f, S) -> (
    G := gens S;
    Q := ring G;
    R := ambient Q;
    J := ideal Q;
    fR := lift(f, R);
    GR := lift(G, R);
    tensorRingNumVars := (numgens R) + (numcols G);
    tensorRing := QQ[Variables => tensorRingNumVars, MonomialOrder => {Eliminate(numgens R)}];    
    liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    fInTensorRing := liftToTensorRing fR;
    GInTensorRing := liftToTensorRing GR;
    JInTensorRing := liftToTensorRing J;
    I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} - GInTensorRing);
    fNormalForm := fInTensorRing % (I + JInTensorRing);
    numcols selectInSubring(1, matrix {{fNormalForm}}) == 1
    )

-- groebnerSubductionQuotient(f, S) = h
-- Computes the subduction quotient of f with respect to the generators of S
-- setup: gens S = {g_1 .. g_s} in Q = K[x_1 .. x_n]/I and f in Q 
-- output: h in K[y1 .. ys] such that (f - f%S) = h(g_1 .. g_s)
groebnerSubductionQuotient = method() 
groebnerSubductionQuotient(RingElement, Subring) := (f, S) -> (
    local outputRing;
    G := gens S;
    Q := ring G;
    R := ambient Q;
    J := ideal Q;
    FF := coefficientRing R;
    fR := lift(f, R);
    GR := lift(G, R);
    tensorRingNumVars := (numgens R) + (numcols G);
    oldOrder := (options R).MonomialOrder;
    newOrder := prepend(Eliminate(numgens R), oldOrder);
    tensorRing := FF[Variables => tensorRingNumVars, MonomialOrder => oldOrder];
    liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    fInTensorRing := liftToTensorRing fR;
    GInTensorRing := liftToTensorRing GR;
    JInTensorRing := liftToTensorRing J;
    I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} - GInTensorRing);
    fNormalForm := fInTensorRing % (I + JInTensorRing);
    -- output fNormalForm in the subductionQuotientRing
    --  construct the ring if not present in the cache of S 
    --  also store the natural map from the subductionQuotientRing to Q 
    if not S.cache#?"subductionQuotientRing" then (
	outputRing = FF[Variables => numcols G];
	S.cache#"subductionQuotientRing" = outputRing;
	S.cache#"subductionQuotientRingMap" = map(Q, outputRing, G);
	);
    outputRing = S.cache#"subductionQuotientRing";
    outputMap := map(outputRing, tensorRing, matrix {toList((numgens R):0)} | vars outputRing);
    outputMap fNormalForm
    )


-- RingElement // Subring 
-- returns the subduction quotient
RingElement // Subring := (f, S) -> (
    groebnerSubductionQuotient(f, S)    
    )


-- Matrix or RingElement % Subring
-- M % S, f % S
-- Returns the smallest r in the ambient ring of S
-- such that M or f = r + s for some s in S
-- Two modes of operation: 
-- 1) If Subring has a sagbi basis (stored in its cache) then use subduction
-- 2) If there is not a complete sagbi basis then use the 'extrinsic method' - see groebnerMembershipTest above
-- Note: we construct the tensor ring with a monomial order lifted from the ambient ring

Matrix % SAGBIBasis := (M, SB) -> (
    assert(ambient SB === ring M);
    subduction(SB, M)
    );

RingElement % SAGBIBasis := (f, SB) -> (
    assert(ambient SB === ring f);
    first first entries subduction(SB, matrix{{f}})
    );

Matrix % Subring := (M, S) -> (
    local result;
    assert(ring M === ambient S); 
    if (S#cache#?"SAGBIBasis") and (S#cache#"SAGBIBasis"#"data"#"sagbiDone") then (
	-- S has a complete sagbi basis so use internal subduction
	SB := S#cache#"SAGBIBasis";
	result = subduction(SB, M);	
	) else (
	-- extrinsic subduction
	G := gens S;
	Q := ring G;
    	R := ambient Q;
    	J := ideal Q;
    	FF := coefficientRing R;
    	MR := lift(M, R);
    	GR := lift(G, R);
    	tensorRingNumVars := (numgens R) + (numcols G);
    	oldOrder := (options R).MonomialOrder;
    	newOrder := prepend(Eliminate(numgens R), oldOrder);
    	tensorRing := FF[Variables => tensorRingNumVars, MonomialOrder => oldOrder];
    	liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    	MInTensorRing := liftToTensorRing MR;
    	GInTensorRing := liftToTensorRing GR;
    	JInTensorRing := liftToTensorRing J;
    	I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} - GInTensorRing);
    	MNormalForm := MInTensorRing % (I + JInTensorRing);	
	projectToQ := map(Q, tensorRing, matrix {toList(numgens Q : 0_Q)} | G);
        result = M - (projectToQ MNormalForm);
	);    
    result
    );

RingElement % Subring := (f, S) -> (
    first first entries (matrix{{f}} % S)
    );

-- subringIntersection(Subring, Subring)
-- intersects the subrings using a method analogous to the GB method

subringIntersection = method(
    Options => {
	Strategy => "Master",
	SubductionMethod => "Top",
	"SAGBILimitType" => "Fixed", -- "Fixed" or "Function"
	Limit => 20,
	PrintLevel => 0
	}
    )

subringIntersection(Subring, Subring) := opts -> (S1, S2) -> (
    local limit;
    local t;
    Q1 := ambient S1;
    Q2 := ambient S2;
    assert(Q1 === Q2);
    Q := Q1;
    I := ideal(Q);
    R := ambient(Q);
    
    -- construct the large ring
    -- monomial order:
    -- 1) monomial order from R (on p_1 ... p_n+1)
    -- 2) eliminate p_0
    
    newMonomialOrder := prepend(GRevLex => 1, (monoid Q).Options.MonomialOrder); -- product order with p_0 in a different subring
    M := monoid [
	Variables => (numgens Q + 1), 
	MonomialOrder => newMonomialOrder
	];
    TAmb := (coefficientRing Q) M;
    t = (vars TAmb)_(0,0);
    RtoTAmb := map(TAmb, R, (vars TAmb)_{1 .. numgens Q});
    J := (RtoTAmb I) + ideal(t^2 - t); -- I + (t^2 - t)
    T := TAmb / J; -- tensor product of Q and K[t]/(t^2-t)
    QtoT := map(T, Q, (vars T)_{1 .. numgens Q});
    TtoQ := map(Q, T, matrix{{0_Q}} | vars Q);
    t = (vars T)_(0,0);
    ---------
    G1 := QtoT gens S1;
    G2 := QtoT gens S2;
    use T;
    G := t*G1 | (1-t)*G2;
    S := subring G;
    if opts#"SAGBILimitType" == "Fixed" then (
	limit = opts.Limit;
	) else if opts#"SAGBILimitType" == "Function" then (
	limit = (max (degrees gens S1)_1)*(max (degrees gens S2)_1);
	);
    SB := sagbi(S, 
	Strategy => opts.Strategy,
	SubductionMethod => opts.SubductionMethod,
	Limit => limit,
	PrintLevel => opts.PrintLevel
	);
    -- if SB is a sagbi basis the intersection computation is correct!
    --  in this case, the intersectionGens form a sagbi basis for the intersection 
    intersectionGens := selectInSubring(1, gens SB);
    subring TtoQ intersectionGens
    );

