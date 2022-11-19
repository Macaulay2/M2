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
    if zero compTable#SAGBIdata#"sagbiGenerators" then (
	compTable#SAGBIdata#"sagbiGenerators" = lift(compTable#SAGBIdata#"subalgebraGenerators", compTable#SAGBIrings#"liftedRing");
	);
    if opts#AutoSubduce then (
	compTable#SAGBIdata#"sagbiGenerators" = lift(autosubduceSagbi compTable, compTable#SAGBIrings#"liftedRing");
	);
    updateComputation(compTable);
    compTable#SAGBIdata#"sagbiDone" = true;
    newSB := sagbiBasis compTable;
    S := newSB#SAGBIdata#subring;
    S.cache#SAGBIBasis = newSB;
    )

forceSB Subring := opts -> S -> (
    local SB;
    SB = sagbiBasis(S, opts);
    forceSB(SB, opts); -- cache of S is updated    
    )

-- internalIsSAGBI is a version of isSAGBI for a SAGBIBasis a object SB
-- it returns a modified SAGBIBasis object with the SB#SAGBIdata#"sagbiDone" flag correctly set
-- it is used as an intermediate step for isSAGBI when it is passed something
--   that does not have a cached SAGBIBasis object

-- the function is split up into 
-- '...Process' which is the core function
-- 'memoize...Process' a memoized form of '...Process'
-- 'internalIsSAGBI' sanitises the input and calls memoize using a consistent option table

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
	compTable#SAGBIdata#"sagbiGenerators" = lift(compTable#SAGBIdata#"subalgebraGenerators", compTable#SAGBIrings#"liftedRing"); --put into the correct ring (lifted ring)
        updateComputation(compTable);
	);
    -- Get the SPairs
    sagbiGB := gb(compTable#SAGBIideals#"reductionIdeal");
    k := rawMonoidNumberOfBlocks(raw monoid (compTable#SAGBIrings.tensorRing)) - 2;
    zeroGens := selectInSubring(k, gens sagbiGB);
    SPairs := compTable#SAGBImaps#"fullSubstitution"(zeroGens) % compTable#SAGBIideals#"I";
    -- Reduce the SPairs
    reducedSPairs := compSubduction(compTable, SPairs);
    -- check the sagbi gens are high enough degree (i.e. higher than the subring generators)
    highEnoughDegree := max flatten (degrees compTable#SAGBIdata#"subalgebraGenerators")_1 <= max flatten (degrees compTable#SAGBIdata#"sagbiGenerators")_1;
    -- if all the reduced SPairs are zero then we have a sagbiBasis
    compTable#SAGBIdata#"sagbiDone" = zero(reducedSPairs) and highEnoughDegree;
    -- if the computation gives a sagbi basis then the don't recompute in the future
    --if compTable#SAGBIdata#"sagbiDone" then (
	--compTable#SAGBIoptions#Recompute = false;
	--) else (
	--compTable#SAGBIoptions#Recompute = true; 
	--);
    SB' := sagbiBasis compTable; -- note that this operation caches SB' in the subring
    if opts.UseSubringGens then ( -- return the cache of the subring to SB
	SB#SAGBIdata#subring.cache#SAGBIBasis = SB;
	);
    SB'
    );

-- memoize is used to recall the result of internalIsSAGBI calls when
--  the generating set is NOT a sagbi basis. When isSAGBI is run a second
--  time on a SAGBIBasis, the groebner basis and subductions are not performed
--  again.

memoizeInternalIsSAGBIProcess = memoize internalIsSAGBIProcess;

-- isInMemoizeInternalIsSAGBIValues: a way to check if an internalIsSAGBI computation was already performed

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
    if SB#SAGBIdata#"sagbiDone" or (not opts.Compute) then (
        SB#SAGBIdata#"sagbiDone"
	) else (
	if zero SB#SAGBIdata#"sagbiGenerators" then ( 
	    -- if there are not sagbi generators then assume we should check the subalgebra generators
	    SB' = internalIsSAGBI(SB, opts, UseSubringGens => true);
	    ) else (
	    SB' = internalIsSAGBI(SB, opts);
	    );
	if SB'#SAGBIdata#"sagbiDone" then ( -- If SB' is a SAGBIBasis then store SB' in the Subring
	    S = SB#SAGBIdata#subring;
	    S.cache#SAGBIBasis = SB';
	    );
	SB'#SAGBIdata#"sagbiDone"
	)
    )

isSAGBI Subring := opts -> S -> (
    local SB;
    local compTable;
    if S.cache#?SAGBIBasis then (
	SB = S.cache#SAGBIBasis;
	-- if SB has no sagbiGenerators then it is possible that internalIsSAGBI was already
	-- called on SB previously.  
    	if (zero SB#SAGBIdata#"sagbiGenerators") and isInMemoizeInternalIsSAGBIValues(SB, opts, UseSubringGens => true) then (
	    return (internalIsSAGBI(SB, opts, UseSubringGens => true))#SAGBIdata#"sagbiDone";
	    );
	
	sagbi(S, Limit => max flatten (degrees gens S)_1); -- ensures that gens SB generate S
	SB = S.cache#SAGBIBasis;
	-- Future: linear programming / binomial or toric ideal comutations to check the following:
	--   the monomial algebra generated by the initial terms of
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
	    SB#SAGBIdata#"sagbiDone"
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
    subringGens := gens S;
    Q := ring subringGens;
    R := ambient Q;
    J := ideal Q;
    fLifedToR := lift(f, R);
    subringGensLiftedToR := lift(subringGens, R);
    -- construct the tensor ring
    tensorRingNumVars := (numgens R) + (numcols subringGens);
    tensorRing := QQ[Variables => tensorRingNumVars, MonomialOrder => {Eliminate(numgens R)}];    
    liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    fInTensorRing := liftToTensorRing fLifedToR;
    subringGensInTensorRing := liftToTensorRing subringGensLiftedToR;
    JInTensorRing := liftToTensorRing J;
    I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} - subringGensInTensorRing);
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
    subringGens := gens S;
    Q := ring subringGens;
    R := ambient Q;
    J := ideal Q;
    FF := coefficientRing R;
    fLifedToR := lift(f, R);
    subringGensLiftedToR := lift(subringGens, R);
    -- construct the tensor ring
    tensorRingNumVars := (numgens R) + (numcols subringGens);
    oldOrder := (options R).MonomialOrder;
    newOrder := prepend(Eliminate(numgens R), oldOrder);
    tensorRing := FF[Variables => tensorRingNumVars, MonomialOrder => oldOrder];
    liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    fInTensorRing := liftToTensorRing fLifedToR;
    subringGensInTensorRing := liftToTensorRing subringGensLiftedToR;
    JInTensorRing := liftToTensorRing J;
    I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} - subringGensInTensorRing);
    fNormalForm := fInTensorRing % (I + JInTensorRing);
    -- output fNormalForm in the subductionQuotientRing
    outputRing = subductionQuotientRing S;
    outputMap := map(outputRing, tensorRing, matrix {toList((numgens R):0)} | vars outputRing);
    outputMap fNormalForm
    )


-- RingElement // Subring 
-- returns the subduction quotient
--
-- Future:
-- >>  Implement an intrinsic subduction quotient via a modification of sagbi that keeps track of how
--     new sagbi generators are expressed in terms of previous sagbi generators (or subring generators) 
-- >>  Make // a quick-access version of a method 'subductionQuotient' which selects whether to use
--     the intrinsic or extrinsic method
--

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
--
-- Future:
-- >> It is possible to still use % modulo an incomplete sagbi basis - make this concrete
--

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
    if (S#cache#?SAGBIBasis) and (S#cache#SAGBIBasis#SAGBIdata#"sagbiDone") then (
	-- S has a complete sagbi basis so use subduction
	SB := S#cache#SAGBIBasis;
	result = subduction(SB, M);	
	) else (
	-- extrinsic subduction
	subringGens := gens S;
	Q := ring subringGens;
    	R := ambient Q;
    	J := ideal Q;
    	FF := coefficientRing R;
    	MLiftedToR := lift(M, R);
    	subringGensLiftedToR := lift(subringGens, R);
    	-- construct the tensor ring
	tensorRingNumVars := (numgens R) + (numcols subringGens);
    	oldOrder := (options R).MonomialOrder;
    	newOrder := prepend(Eliminate(numgens R), oldOrder);
    	tensorRing := FF[Variables => tensorRingNumVars, MonomialOrder => oldOrder];
	liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    	MInTensorRing := liftToTensorRing MLiftedToR;
    	subringGensInTensorRing := liftToTensorRing subringGensLiftedToR;
    	JInTensorRing := liftToTensorRing J;
    	I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} - subringGensInTensorRing);
    	MNormalForm := MInTensorRing % (I + JInTensorRing);
	projectToQ := map(Q, tensorRing, matrix {toList(numgens Q : 0_Q)} | subringGens);
        result = M - (projectToQ MNormalForm);
	);    
    result
    );

RingElement % Subring := (f, S) -> (
    first first entries (matrix{{f}} % S)
    );


-- IntersectedSubring
-- a type for subrings created by intersecting two subrings
-- keys are the usual subring keys plus the keys: 
-- "originalSubrings" = {S1, S2} -- S1 and S2 are the original subrings used for the intersection 
-- "compositeSubring" = S -- the subring in the tensor ring K[t*S1, (1-t)*S1] (see subringIntersection) 
-- this type does not support a dedicated constructor since it is created by the function subringIntersection

IntersectedSubring = new Type of Subring 

-- originalSubringGens 
-- returns the generators of the original subrings used in the intersection computation
originalSubringGens = method()
originalSubringGens(IntersectedSubring) := S -> (
    gens \ S#"originalSubrings"
    )

-- isFullIntersection 
-- returns true if the intersection of the "originalSubring" is guaranteed to be equal to the generators of intersectedSubring 
-- note that the composite subring is guaranteed to have a SAGBIBasis stored in its cache
-- note that if the function returns false, then the generators of the intersectedSubring may generate the intersection 
--  but it is not guaranteed
isFullIntersection = method()
isFullIntersection(IntersectedSubring) := S -> (
    S#"compositeSubring".cache#SAGBIBasis.SAGBIdata#"sagbiDone"
    )

-- FUTURE:
-- add functionality to resume the computation for intersected subrings which are not guaranteed to give the full intersection


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
    -------------
    -- check that both subring are subrings of the same ambient ring
    Q1 := ambient S1;
    Q2 := ambient S2;
    assert(Q1 === Q2);
    Q := Q1;
    I := ideal(Q);
    R := ambient(Q);
    ------------
    -- For notation:
    -- R = K[p_1 .. p_n]
    -- Q = R / I
    --
    -- Construct the ring T = tensor product of Q with K[t]/(t^2 - t)
    -- TAmb := [t := p_0, p_1 .. p_n] 
    -- monomial order of TAmb:
    -- 1) monomial order from R (on p_1 ... p_n)
    -- 2) eliminate p_0
    -- J := I lifted to TAmb + (t^2 - t) --> makes t and (1-t) into idemponents in T
    -- T := TAmb / J
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
    t = (gens T)_0;
    --------------
    -- Notation:
    -- Gi := generators of Si lifted to T, for each i in {1, 2}
    -- Form the subring of T given by
    -- S := K[t*G1, (1-t)*G2]
    G1 := QtoT gens S1;
    G2 := QtoT gens S2;
    use T; -- Dev note: T contains no user variables
    G := t*G1 | (1-t)*G2;
    S := subring G;
    --------------
    -- Compute a sagbi basis for S (take care of options supplied by user)
    -- SB := subalgebraBasis S
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
    ------------
    -- The intersection of S1 and S2 is generated by the elements SB|_Q of SB that do not contain t := p_0
    -- If SB is a finite sagbi basis then the intersection computation is verrified to be correct!
    -- >> In this case, the elements SB|_Q form a sagbi basis for the intersection so use forceSB 
    -- Note that, if the intersection of S1 and S2 has a finite sagbi basis, then it is NOT guaranteed
    -- that S has a finite sagbi basis.
    intersectionGens := TtoQ selectInSubring(1, gens SB);
    result := new IntersectedSubring from {
	"ambientRing" => Q,
	"generators" => intersectionGens,
	"subductionQuotientRing" => (coefficientRing Q)[Variables => numcols intersectionGens],
	cache => new CacheTable from {},
	"originalSubrings" => {S1, S2},
	"compositeSubring" => S
	}; 
    if isSAGBI SB then forceSB result;
    result
    );
