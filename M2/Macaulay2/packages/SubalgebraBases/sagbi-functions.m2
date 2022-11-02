-- internalVerifySagbi is a version of verifySabi just for SAGBIBasis objects
-- it returns a SAGBIBasis object with its SB#"data"#"sagbiDone" flag correctly set
-- it is used as an intermediate step for verifySagbi since matrices and subrings
--   are handled slightly differently
-- 
-*
internalVerifySagbiProcess = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
	Limit => 100,
	PrintLevel => 0, -- see print level for sagbi
    	Recompute => false,
	RenewOptions => false,
	UseSubringGens => false
	}
    );


internalVerifySagbiProcess(SAGBIBasis) := opts -> SB -> (
    compTable := initializeCompTable(SB, opts);    
    if opts.UseSubringGens then ( -- replace the sagbiGens with the subring gens
	compTable#"data"#"sagbiGenerators" = lift(compTable#"data"#"subalgebraGenerators", compTable#"rings"#"liftedRing"); --put into the correct ring (lifted ring)
        updateComputation(compTable);
	);
    -- Get the SPairs
    sagbiGB := gb(compTable#"ideals"#"reductionIdeal");
    k := rawMonoidNumberOfBlocks(raw monoid (compTable#"rings".tensorRing)) - 2;
    zeroGens := selectInSubring(k, gens sagbiGB);
    SPairs := compTable#"maps"#"fullSubstitution"(zeroGens) % compTable#"ideals"#"I";
    -- Reduce the SPairs
    reducedSPairs := compSubduction(compTable, SPairs);
    -- check the sagbi gens are high enough degree (i.e. higher than the subring generators)
    highEnoughDegree := max flatten (degrees compTable#"data"#"subalgebraGenerators")_1 <= max flatten (degrees compTable#"data"#"sagbiGenerators")_1;
    -- if all the reduced SPairs are zero then we have a sagbiBasis
    compTable#"data"#"sagbiDone" = zero(reducedSPairs) and highEnoughDegree;
    SB' := sagbiBasis compTable; -- note that this operation caches SB' in the subring
    if opts.UseSubringGens then ( -- return the cache of the subring to SB
	SB#"data"#"subring".cache#"SAGBIBasis" = SB;
	);
    SB'
    );

memoizeInternalVerifySagbiProcess = memoize internalVerifySagbiProcess;

internalVerifySagbi = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
	Limit => 100,
	PrintLevel => 0, -- see print level for sagbi
    	Recompute => false,
	RenewOptions => false,
	UseSubringGens => false
	}
    );

internalVerifySagbi(SAGBIBasis) := opts -> SB -> (
    memoizeInternalVerifySagbiProcess(SB, opts)
    )


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
-- TODO: kill verifySagbi
--

verifySagbi = method(
    TypicalValue => Subring,
    Options => {
	Compute => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Engine", -- top or engine
	Limit => 100,
	PrintLevel => 0, -- see print level for sagbi
    	Recompute => false,
	RenewOptions => false,
	UseSubringGens => false
	}
);

verifySagbi(Subring) := opts -> S -> (
    local SB;
    
    if (S#cache#?"SAGBIBasis") and (gens(S#cache#"SAGBIBasis") == gens S) then (
	-- S has a sagbi basis so use this object as a compTable
	SB = S#cache#"SAGBIBasis";	
	SB = internalVerifySagbi(SB, opts);
	) else (
    	trimmedOptionKeys := delete(Compute, keys opts);
	trimmedOptionKeys = delete(UseSubringGens, trimmedOptionKeys);
    	trimmedOptionTable := new OptionTable from apply(trimmedOptionKeys, k -> k => opts#k);
	--SB = initializeCompTable(sagbiBasis(S, trimmedOptionTable), trimmedOptionTable);
	-- add the generators to the sagbiGenerators
	--SB#"data"#"sagbiGenerators" = lift(gens S, SB#"rings"#"liftedRing");
	--updateComputation(SB);
	SB = sagbiBasis(S, trimmedOptionTable);
	SB = internalVerifySagbi(SB, opts, UseSubringGens => true);
	);
    
    -- The following is done automatically by sagbiBasis
    --if not S.cache#?"SAGBIBasis" then (
    --	S.cache#"SAGBIBasis" = SB; -- add a new SAGBIBasis if there wasn't one already
	--) else if (SB#"data"#"sagbiDone" and not S.cache#"SAGBIBasis"#"data"#"sagbiDone") then (
	--S.cache#"SAGBIBasis" = SB; -- update the SAGBIBasis if we managed to newly verify the sagbi basis
	--);
    SB#"data"#"sagbiDone"
    )

verifySagbi(Matrix) := opts -> M -> (
    local S;
    local SB;

    if (M#cache#?"Subring") and (gens(M#cache#"Subring"#cache#"SAGBIBasis") == M) then (
	-- S has a sagbi basis so use this object as a compTable
	S = M.cache#"Subring";
	SB = S.cache#"SAGBIBasis";
	) else (
	SB = initializeCompTable(sagbiBasis subring M, opts);
	-- add the generators to the sagbiGenerators
	SB#"data"#"sagbiGenerators" = lift(M, SB#"rings"#"liftedRing");
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
*-

-- forceSB(Subring / SAGBIBasis) constructs a SAGBIBasis object, using any
-- existing one as a template, applied autosubduction to the (sagbi)generators,
-- and sets the sagbiDone flag to true - this function should only be applied to
-- subrings whose generators are known to be a sagbi basis.

forceSB = method(
    Options => {
	AutoSubduce => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Engine", -- top or engine
	Limit => 100,
	RenewOptions => false,
	PrintLevel => 0 -- see print level for sagbi
    	}
    )
forceSB SAGBIBasis := opts -> SB -> (
    compTable := initializeCompTable(SB, opts);
    if zero compTable#"data"#"sagbiGenerators" then (
	compTable#"data"#"sagbiGenerators" = lift(compTable#"data"#"subalgebraGenerators", compTable#"rings"#"liftedRing");
	);
    if opts#AutoSubduce then (
	compTable#"data"#"sagbiGenerators" = lift(autosubduceSagbi compTable, compTable#"rings"#"liftedRing");
	);
    updateComputation(compTable);
    compTable#"data"#"sagbiDone" = true;
    newSB := sagbiBasis compTable;
    S := newSB#"data"#"subring";
    S.cache#"SAGBIBasis" = newSB;
    )

forceSB Subring := opts -> S -> (
    local SB;
    SB = sagbiBasis(S, opts);
    forceSB(SB, opts); -- cache of S is updated    
    )

-- internalIsSAGBI is a version of isSAGBI for a SAGBIBasis a object SB
-- it returns a modified SAGBIBasis object with the SB#"data"#"sagbiDone" flag correctly set
-- it is used as an intermediate step for isSAGBI when it is passed something
--   that does not have a cached SAGBIBasis object

-- memoizeInternalIsSAGBI = memoize internalIsSAGBI
--   when calling isSAGBI on a subring without SAGBIBasis
--   it produces a very simple object: sagbiBasis(S) which has no sagbiGenerators etc.
--   (in particular it can be used for sagbi in the future)
--   and in internalIsSAGBI, we check the sagbi generators (and create the correct compTable if necessary)
--   remove the Recompute flag from the compTable
-- TODO:  
--   WARNING (in documentation): memoize is used so excessive use of isSAGBI will cause a lot of computation objects 
--   to be created / not be garbage collected since they may be stored

-- // something that is not done but can be kept for the future //(debatable - don't do it!)
-- add an option for remembering the output of internalIsSAGBI
-- to do this, one could: let mem = memoize memoize
-- then if the remembering option is true then run "mem internalIsSAGBI" otherwise
-- run "internalIsSAGBI"
--
-- idea: we could have memoize as an option and use a custom version of memoize for the package
-- this is just a fancy way of not having to create two almost-identical functions
-- but that would be fine too: memInternalIsSAGBI = memoize internalIsSAGBI


-- internalIsSAGBIProcess is the core method
--  internalIsSAGBI is a wrapper for the memoized version of this method

internalIsSAGBIProcess = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
	Strategy => "Master",
        SubductionMethod => "Top",
	Limit => 100,
	PrintLevel => 0,
    	Recompute => false,
	RenewOptions => false,
	UseSubringGens => false
	}
    );

internalIsSAGBIProcess(SAGBIBasis) := opts -> SB -> (
    compTable := initializeCompTable(SB, opts);    
    if opts.UseSubringGens then ( -- replace the sagbiGens with the subring gens
	compTable#"data"#"sagbiGenerators" = lift(compTable#"data"#"subalgebraGenerators", compTable#"rings"#"liftedRing"); --put into the correct ring (lifted ring)
        updateComputation(compTable);
	);
    -- Get the SPairs
    sagbiGB := gb(compTable#"ideals"#"reductionIdeal");
    k := rawMonoidNumberOfBlocks(raw monoid (compTable#"rings".tensorRing)) - 2;
    zeroGens := selectInSubring(k, gens sagbiGB);
    SPairs := compTable#"maps"#"fullSubstitution"(zeroGens) % compTable#"ideals"#"I";
    -- Reduce the SPairs
    reducedSPairs := compSubduction(compTable, SPairs);
    -- check the sagbi gens are high enough degree (i.e. higher than the subring generators)
    highEnoughDegree := max flatten (degrees compTable#"data"#"subalgebraGenerators")_1 <= max flatten (degrees compTable#"data"#"sagbiGenerators")_1;
    -- if all the reduced SPairs are zero then we have a sagbiBasis
    compTable#"data"#"sagbiDone" = zero(reducedSPairs) and highEnoughDegree;
    -- if the computation gives a sagbi basis then the don't recompute in the future
    --if compTable#"data"#"sagbiDone" then (
	--compTable#options#Recompute = false;
	--) else (
	--compTable#options#Recompute = true; 
	--);
    SB' := sagbiBasis compTable; -- note that this operation caches SB' in the subring
    if opts.UseSubringGens then ( -- return the cache of the subring to SB
	SB#"data"#"subring".cache#"SAGBIBasis" = SB;
	);
    SB'
    );

-- memoize is used to recall the result of internalIsSAGBI calls when
--  the generating set is NOT a sagbi basis. When isSAGBI is run a second
--  time on a SAGBIBasis, the groebner basis and subductions are not performed
--  again.

memoizeInternalIsSAGBIProcess = memoize internalIsSAGBIProcess;

-- memoizeInternalIsSAGBIValues: a way to check if an internalIsSAGBI computation was already performed

isInMemoizeInternalIsSAGBIValues = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
	Strategy => "Master",
        SubductionMethod => "Top",
	Limit => 100,
	PrintLevel => 0,
    	Recompute => false,
	RenewOptions => false,
	UseSubringGens => false
	}
    );

isInMemoizeInternalIsSAGBIValues(SAGBIBasis) := opts -> SB -> (
    (memoizeValues memoizeInternalIsSAGBIProcess)#?(SB, opts) 
    )


-- internalIsSAGBI is a wrapper for the memoized version of internalIsSAGBIProcess
--  it makes the input of the wrapper uniform: i.e. it passes the SAGBIBasis + Option Table in that order
--  so the memoize value table does not increase in size unnecessarily

internalIsSAGBI = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
	Strategy => "Master",
        SubductionMethod => "Top",
	Limit => 100,
	PrintLevel => 0,
    	Recompute => false,
	RenewOptions => false,
	UseSubringGens => false
	}
    );

internalIsSAGBI(SAGBIBasis) := opts -> SB -> (
    memoizeInternalIsSAGBIProcess(SB, opts)
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
	RenewOptions => false,
	UseSubringGens => false -- when performing internalIsSAGBI on a 
	}
    );

isSAGBI SAGBIBasis := opts -> SB -> (
    local SB';
    local S;
    if SB#"data"#"sagbiDone" or (not opts.Compute) then (
        SB#"data"#"sagbiDone"
	) else (
	if zero SB#"data"#"sagbiGenerators" then ( 
	    -- if there are not sagbi generators then assume we should check the subalgebra generators
	    SB' = internalIsSAGBI(SB, opts, UseSubringGens => true);
	    ) else (
	    SB' = internalIsSAGBI(SB, opts);
	    );
	if SB'#"data"#"sagbiDone" then ( -- If SB' is a SAGBIBasis then store SB' in the Subring
	    S = SB#"data"#"subring";
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
	-- if SB has no sagbiGenerators then it is possible that internalIsSAGBI was already
	-- called on SB previously.  
    	if (zero SB#"data"#"sagbiGenerators") and isInMemoizeInternalIsSAGBIValues(SB, opts, UseSubringGens => true) then (
	    return (internalIsSAGBI(SB, opts, UseSubringGens => true))#"data"#"sagbiDone";
	    );
	
	sagbi(S, Limit => max flatten (degrees gens S)_1); -- ensures that gens SB generate S
	SB = S.cache#"SAGBIBasis";
	-- do some linear programming / binomial or toric ideal comutations to check the following:
	--   check that the monomial algebra generated by the initial terms of
	--   gens S and gens SB are equal 
	LTgensS := leadTerm gens S;
	LTgensSB := leadTerm gens SB;
	subringLTgensS := subring LTgensS;
	subringLTgensSB := subring LTgensSB;
	forceSB subringLTgensS;
	forceSB subringLTgensSB;
	if zero(LTgensS % subringLTgensSB) and zero(LTgensSB % subringLTgensS) then (
	    isSAGBI(SB, opts)
	    ) else (
	    false
	    )
	) else (
	if opts.Compute then (
	    trimmedOptionKeys := delete(Compute, keys opts);
	    trimmedOptionKeys = delete(UseSubringGens, trimmedOptionKeys);
    	    trimmedOptionTable := new OptionTable from apply(trimmedOptionKeys, k -> k => opts#k);
	    SB = sagbiBasis(S, trimmedOptionTable); -- basic SAGBIBasis object that is compatible with future sagbi computations
	    SB = internalIsSAGBI(SB, opts, UseSubringGens => true);
	    -- construct a SAGBIBasis for S and verify whether it is a sagbi basis
	    --compTable = initializeCompTable(sagbiBasis(S, trimmedOptionTable), trimmedOptionTable);
	    -- add the generators to the sagbiGenerators
	    --compTable#"data"#"sagbiGenerators" = lift(gens S, compTable#"rings"#"liftedRing"); --put into the correct ring (lifted ring)
	    --updateComputation(compTable);
	    --SB = sagbiBasis compTable;
	    --SB = internalIsSAGBI(opts, SB); 
    	    -- S.cache#"SAGBIBasis" = SB;
    	    SB#"data"#"sagbiDone"
	    ) else ( 
	    null -- S has no SAGBIBasis cached and Compute is set to false
	    )
	)
    )    

isSAGBI Matrix := opts -> M -> (
    S := subring M;
    isSAGBI(opts, S)
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
    outputRing = subductionQuotientRing S;
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
	-- S has a complete sagbi basis so use subduction
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
    result := subring TtoQ intersectionGens;
    if isSAGBI SB then forceSB result;
    result
    );
