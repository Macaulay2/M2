-- forceSB(Subring / SAGBIBasis) constructs a SAGBIBasis object, using any
-- existing one as a template, applies autosubduction to the (sagbi)generators,
-- and sets the sagbiStatus to 1 (sagbi basis found) - this function should only be applied to
-- subrings whose generators are known to be a sagbi basis.
forceSB = method(
    Options => true
    )
forceSB SAGBIBasis := {
    AutoSubduce => true,
    Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
    SubductionMethod => "Engine", -- top or engine
    Limit => 20,
    RenewOptions => false,
    PrintLevel => 0, -- see print level for sagbi
    UseSubringGens => false -- default is false for SAGBIBasis objects
    } >> opts -> SB -> (
    sagbiComputation := initializeSagbiComputation(SB, new OptionTable from {
        AutoSubduce => opts.AutoSubduce,
        Strategy => opts.Strategy,
        SubductionMethod => opts.SubductionMethod,
        Limit => opts.Limit,
        RenewOptions => opts.RenewOptions,
        PrintLevel => opts.PrintLevel});
    -- When should the sagbiGenerators be replaced with the subring generators:
    -- 1. UseSubringGens => true
    -- 2. there are no sagbiGenerators
    if opts.UseSubringGens or zero sagbiComputation#SAGBIdata#"sagbiGenerators" then (
        sagbiComputation#SAGBIdata#"sagbiGenerators" = lift(
            sagbiComputation#SAGBIdata#"subalgebraGenerators",
            sagbiComputation#SAGBIrings#"liftedRing");
    );
    if opts#AutoSubduce then (
        sagbiComputation#SAGBIdata#"sagbiGenerators" = lift(
            autosubduce(sagbiComputation, UseSubringGens => opts.UseSubringGens),
            sagbiComputation#SAGBIrings#"liftedRing");
    );
    updateComputation sagbiComputation;
    sagbiComputation#SAGBIdata#"sagbiStatus" = 1;
    newSB := sagbiBasis sagbiComputation;
    S := newSB#SAGBIdata#subring;
    S.cache#SAGBIBasis = newSB;
)

forceSB Subring := {
    AutoSubduce => true,
    Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
    SubductionMethod => "Engine", -- top or engine
    Limit => 20,
    RenewOptions => false,
    PrintLevel => 0, -- see print level for sagbi
    UseSubringGens => true
    } >> opts -> S -> (
    local SB;
    SB = sagbiBasis(S, new OptionTable from {
        AutoSubduce => opts.AutoSubduce,
        Strategy => opts.Strategy,
        SubductionMethod => opts.SubductionMethod,
        Limit => opts.Limit,
        RenewOptions => opts.RenewOptions,
        PrintLevel => opts.PrintLevel});
    forceSB(SB, opts); -- cache of S is updated
)

----------
-- isSAGBI
----------
-- isSAGBIinternal is a subprocess of isSAGBI for a SAGBIBasis SB
-- it returns a modified SAGBIBasis object with the SB#SAGBIdata#"sagbiStatus" set correctly
-- If UseSubringGens is true, initialise the SAGBIBasis with the subring generators
-- and does not update SAGBIBasis associated to the subring
-- If ModifySAGBIBasis is false, keeps the original SAGBIBasis of the subring
isSAGBIinternal = method(
    TypicalValue => SAGBIBasis,
    Options => {
        Compute => true,
        Strategy => "Master",
        SubductionMethod => "Top",
        PrintLevel => 0,
        Recompute => false,
        RenewOptions => false,
        UseSubringGens => false,
        ModifySAGBIBasis => true
    }
);

isSAGBIinternal SAGBIBasis := opts -> SB -> (
    sagbiComputation := initializeSagbiComputation(SB,
        opts ++ {Limit => SB#SAGBIdata#"limit"}); -- leave the limit alone
    if opts.UseSubringGens then (
        -- replace the sagbiGens with the subring gens
        sagbiComputation#SAGBIdata#"sagbiGenerators" =
            lift(sagbiComputation#SAGBIdata#"subalgebraGenerators",
            --put into the correct ring (lifted ring)
            sagbiComputation#SAGBIrings#"liftedRing");
        updateComputation(sagbiComputation);
    );
    -- Get the SPairs
    sagbiGB := gb(sagbiComputation#SAGBIideals#"reductionIdeal");
    k := rawMonoidNumberOfBlocks(raw monoid (sagbiComputation#SAGBIrings.tensorRing)) - 2;
    zeroGens := selectInSubring(k, gens sagbiGB);
    SPairs := sagbiComputation#SAGBImaps#"fullSubstitution" zeroGens %
        sagbiComputation#SAGBIideals#"I";
    if not opts.UseSubringGens then (
        -- add the subring generators to the SPairs
        -- to check that sagbiGenerators really generate the subring
        -- add only the generators of degree above the computation degree limit
        -- (otherwise the original generators have already been processed)
        subringGens := sagbiComputation#SAGBIdata#"subalgebraGenerators";
        limit := sagbiComputation#SAGBIdata#"limit";
        SPairs = SPairs | matrix {for gen in first entries subringGens list if ((
            matrix{degree leadTerm gen})*sagbiComputation#SAGBIrings#"heftVector")_(0,0) >
            limit then gen else continue};
        );
    -- Reduce the SPairs and subring generators
    reducedSPairs := compSubduction(sagbiComputation, SPairs);
    -- if all the reduced SPairs are zero then we have a sagbiBasis
    if zero reducedSPairs then sagbiComputation#SAGBIdata#"sagbiStatus" = 1
    else sagbiComputation#SAGBIdata#"sagbiStatus" = 2;
    
    -- note that this operation caches SB' in the subring
    SB' := sagbiBasis sagbiComputation;
    -- When to restore the subring cache back to the previous SAGBIBasis:
    -- 1. UseSubringGens => true and the generators are not a sagbi basis
    -- 2. ModifySAGBIBasis => false 
    if (opts.UseSubringGens and sagbiComputation#SAGBIdata#"sagbiStatus" == 2) or
        not opts.ModifySAGBIBasis then (
            SB#SAGBIdata#subring.cache#SAGBIBasis = SB;
    );
    SB'
);

-- isSAGBI
-- Compute => true          : perform only necessary computations
-- Recompute => false       : perform all computations (only applies if Compute == true)
-- UseSubringGens => false  : use the generators of the associated Subring object
-- ModifySAGBIBasis => true : update the SAGBIBasis cached in the associated Subring
isSAGBI = method(
    TypicalValue => Boolean,
    Options => true
    );

-- isSAGBI SAGBIBasis
-- Determines if the sagbiGenerators form a sagbi basis
-- Sets the sagbiStatus of the SAGBIBasis
-- Options:
-- Compute => false       : the result is just the sagbiStatus (0 : null, 1 : true, 2 : false)
-- Recompute => true      : performs a computation to verify the sagbiStatus
-- RenewOptions => true   : sets the options of the SAGBIBasis to the ones supplied
-- UseSubringGens => true : uses generators of subring instead of sagbiGenerators
-- Control flow:
-- Compute => false : return the sagbiStatus of the given SAGBIBasis
-- Compute => true : if computation is not necessary then read off the result
-- (Recompute => false and sagbiStatus == 0) or (Recompute => true) : computation is necessary
isSAGBI SAGBIBasis := {
    Compute => true,
    Strategy => "Master",
    SubductionMethod => "Top",
    PrintLevel => 0,
    Recompute => false,
    RenewOptions => false,
    UseSubringGens => false,
    ModifySAGBIBasis => true
    } >> opts -> SB -> (
    if opts.UseSubringGens then isSAGBI(subring SB, opts)
    else (
        SB' := (
            if not opts.Compute then SB
            else if not opts.Recompute and SB#SAGBIdata#"sagbiStatus" > 0 then SB
            else isSAGBIinternal(SB, opts)
        );
        if SB'#SAGBIdata#"sagbiStatus" == 0 then null
        else if SB'#SAGBIdata#"sagbiStatus" == 1 then true
        else if SB'#SAGBIdata#"sagbiStatus" == 2 then false
        else error("-- unknown sagbiStatus: " | toString SB'#SAGBIdata#"sagbiStatus")
    )
)

-- isSAGBI Subring
-- Determines if the sagbiGenerators of the cached SAGBIBasis are a sagbiBasis
-- Options:
-- If Compute is false, the result is just the sagbiStatus (0 : null, 1 : true, 2 : false)
-- If Recompute is true, performs a computation to verify the sagbiStatus
-- If RenewOptions => true, sets the options of the SAGBIBasis to the ones supplied
-- If UseSubringGens => false, uses generators of SAGBIBasis instead of the subring generators
-- isSAGBI Subring stores computation results in
-- S.cache#"isSAGBIResults" [OptionTable -> Boolean]
-- Control flow:
-- (UseSubringGens => true) & (Recompute => false) :
-- avoid using SAGBIBasis, try sanitise options and check if it appears in isSAGBIResults
-- (UseSubringGens => true) & (Recompute => true) :
-- compute the result and cache the answer
-- (UseSubringGens => false) :
-- isSAGBI SAGBIBasis

isSAGBI Subring := {
    Compute => true,
    Strategy => "Master",
    SubductionMethod => "Top",
    PrintLevel => 0,
    Recompute => false,
    RenewOptions => false,
    UseSubringGens => true,
    ModifySAGBIBasis => true
    } >> opts -> S -> (
    result := null;
    computationRequired := false;
    if opts.UseSubringGens then (
        cleanOptions := new OptionTable from {-- sanitise options for storing results
            Strategy => opts.Strategy,
            SubductionMethod => opts.SubductionMethod,
            RenewOptions => opts.RenewOptions,
            ModifySAGBIBasis => opts.ModifySAGBIBasis
        };
        if not S.cache#?"isSAGBIResults" then S.cache#"isSAGBIResults" = new MutableHashTable;
        if not opts.Recompute then (
            -- check isSAGBIResults
            if S.cache#"isSAGBIResults"#?cleanOptions then result = S.cache#"isSAGBIResults"#cleanOptions
            else computationRequired = true;
        );
        if opts.Compute and (opts.Recompute or computationRequired) then (
            hasSAGBIBasis := S.cache#?SAGBIBasis;
            SB := sagbiBasis S;
            -- We can use the cached SAGBIBasis if all of these hold:
            -- 1. subring has a sagbi basis
            -- 2. the sagbi generators are not zero
            -- 3. the generators of the subring (of a higher degree than the computation limit)
            -- subduct to zero against the sagbiGenerators
            -- 4. the leading terms of the sagbi generators are 'the same'
            -- as the leading terms of the subring, i.e., they generate the same algebra
            SB' := if (hasSAGBIBasis and not zero SB#SAGBIdata#"sagbiGenerators" and
                isHighDegreeZero(S,SB) and isInitialInQuotient(S,SB)) then (
                -- we can use the cached SAGBIBasis for the computation
                -- if the sagbiStatus flag is non-zero then we can use it directly
                -- otherwise we need to use perform an internal check
                if SB#SAGBIdata#"sagbiStatus" == 1 or SB#SAGBIdata#"sagbiStatus" == 2 then SB
                else (
                    newOptions := applyPairs(opts, (k,v) -> if k == UseSubringGens then
                        (UseSubringGens, false) else (k,v));
                    isSAGBIinternal(SB, newOptions);
                ))
            -- definition of SB' if we cannot use the cached SAGBIBasis
            else isSAGBIinternal(SB, opts);
            if SB'#SAGBIdata#"sagbiStatus" == 1 then result = true
            else if SB'#SAGBIdata#"sagbiStatus" == 2 then result = false
            else error("-- unknown sagbiStatus: " | toString SB'#SAGBIdata#"sagbiStatus");
            S.cache#"isSAGBIResults"#cleanOptions = result;
            -- remove the SAGBIBasis from the cache if both:
            -- 1. the subring generators are not a sagbi basis (not useful for subduction etc.)
            -- 2. the subring had no SAGBIBasis to start with
            -- (user may want to start computations with different options)
            if SB'#SAGBIdata#"sagbiStatus" == 2 and
                not hasSAGBIBasis then remove(S.cache, SAGBIBasis);
        );
        result
    )
    else (
        -- UseSubringGens => false
        if S.cache#?SAGBIBasis then result = isSAGBI(S.cache#SAGBIBasis, opts);
        result
    )
)

isSAGBI Matrix := {
    Compute => true,
    Strategy => "Master",
    SubductionMethod => "Top",
    PrintLevel => 0,
    Recompute => false,
    RenewOptions => false,
    UseSubringGens => true,
    ModifySAGBIBasis => true
    } >> opts -> M -> (
    S := subring M;
    isSAGBI(S, opts)
)

isSAGBI List := {
    Compute => true,
    Strategy => "Master",
    SubductionMethod => "Top",
    PrintLevel => 0,
    Recompute => false,
    RenewOptions => false,
    UseSubringGens => true,
    ModifySAGBIBasis => true
    } >> opts -> L -> (
    S := subring L;
    isSAGBI(S, opts)
)

----------------------------------------
-- isHighDegreeZero(S, SB) = high degree generators (those above the limit) reduce to zero
-- S a subring of a polynomial ring (or quotient ring)
-- SB its SAGBI Basis
isHighDegreeZero = method()
isHighDegreeZero (Subring, SAGBIBasis) := (S,SB) -> (
    highDegreeGens := matrix {for gen in first entries gens S list if ((
        matrix{degree leadTerm gen})*SB#SAGBIrings#"heftVector")_(0,0) >
        SB#SAGBIdata#"limit" then gen else continue};
    zero highDegreeGens or zero (highDegreeGens % SB)
)

----------------------------------------
-- isInitialInQuotient(S, SB) = the initial algebra lies in the quotient by the initial ideal
-- S a subring of a polynomial ring (or quotient ring)
-- SB its SAGBI Basis
isInitialInQuotient = method()
isInitialInQuotient (Subring,SAGBIBasis) := (S, SB) -> (
    LTAmbient := (if isQuotientRing flattenedRing S then (
        Q := flattenedRing S;
        R := ambient Q;
        I := ideal Q;
        R / ideal leadTerm I)
    else flattenedRing S);
    liftMap := map(LTAmbient, flattenedRing S, gens LTAmbient);
    LTgensS := liftMap leadTerm gens S;
    LTgensSB := liftMap leadTerm gens SB;
    subringLTgensS := subring LTgensS;
    subringLTgensSB := subring LTgensSB;
    forceSB subringLTgensS;
    forceSB subringLTgensSB;
    zero(LTgensS % subringLTgensSB) and zero(LTgensSB % subringLTgensS)
)

----------------------------------------
-- groebnerMembershipTest(f, S) = (f lies in S)
-- f an element of the ambient ring of S
-- S a subring of a polynomial ring (or quotient ring)
groebnerMembershipTest = method()
groebnerMembershipTest(RingElement, Subring) := Boolean => (f, S) -> (
    subringGens := S#"flatteningMap" gens S;
    Q := ring subringGens;
    R := ambient Q;
    FF := coefficientRing R;
    J := ideal Q;
    fLifedToR := lift(S#"flatteningMap" f, R);
    subringGensLiftedToR := lift(subringGens, R);
    -- construct the tensor ring
    tensorRingNumVars := numgens R + numcols subringGens;
    tensorRing := FF(monoid[Variables => tensorRingNumVars,
        MonomialOrder => {Eliminate numgens R}]);
    liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    fInTensorRing := liftToTensorRing fLifedToR;
    subringGensInTensorRing := liftToTensorRing subringGensLiftedToR;
    JInTensorRing := liftToTensorRing J;
    I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} -
        subringGensInTensorRing);
    fNormalForm := fInTensorRing % (I + JInTensorRing);
    numcols selectInSubring(1, matrix {{fNormalForm}}) == 1
)

-- groebnerSubductionQuotient(f, S) = h
-- Computes the subduction quotient of f with respect to the generators of S
-- setup: gens S = {g_1 .. g_s} in Q = K[x_1 .. x_n]/I and f in Q
-- output: h in K[y1 .. ys] such that (f - f%S) = h(g_1 .. g_s)
groebnerSubductionQuotient = method()
groebnerSubductionQuotient(RingElement, Subring) := RingElement => (f, S) -> (
    subringGens := S#"flatteningMap" gens S;
    Q := ring subringGens;
    R := ambient Q;
    J := ideal Q;
    FF := coefficientRing R;
    fLifedToR := lift(f, R);
    subringGensLiftedToR := lift(subringGens, R);
    -- construct the tensor ring
    tensorRingNumVars := numgens R + numcols subringGens;
    oldOrder := (options R).MonomialOrder;
    newOrder := prepend(Eliminate numgens R, oldOrder);
    tensorRing := FF(monoid[Variables => tensorRingNumVars, MonomialOrder => newOrder]);
    liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    fInTensorRing := liftToTensorRing fLifedToR;
    subringGensInTensorRing := liftToTensorRing subringGensLiftedToR;
    JInTensorRing := liftToTensorRing J;
    I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} -
        subringGensInTensorRing);
    fNormalForm := fInTensorRing % (I + JInTensorRing);
    -- output fNormalForm in the presentationRing
    outputRing := presentationRing S;
    outputMap := map(outputRing, tensorRing, matrix {toList((numgens R):0)} | vars outputRing);
    outputMap fNormalForm
)

-- RingElement // Subring
-- returns the subduction quotient
RingElement // Subring := RingElement => (f, S) -> (
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
Matrix % SAGBIBasis := Matrix => (M, SB) -> (
    if not (ambient SB === ring M) then error "expected rings to be the same";
    SB#SAGBImaps#"inverseFlatteningMap" subduction(SB, M)
);

RingElement % SAGBIBasis := RingElement => (f, SB) -> (
    if not (ambient SB === ring f) then error "expected rings to be the same";
        SB#SAGBImaps#"inverseFlatteningMap" first first entries subduction(SB, matrix{{f}})
    );

Matrix % Subring := Matrix => (M, S) -> (
    if not (ambient S === ring M) then error "expected rings to be the same";
    if S#cache#?SAGBIBasis and S#cache#SAGBIBasis#SAGBIdata#"sagbiStatus" == 1 then (
        -- S has a complete sagbi basis so use subduction
        SB := S#cache#SAGBIBasis;
        S#"inverseFlatteningMap" subduction(SB, M)
    )
    else (
        -- extrinsic subduction
        subringGens := S#"flatteningMap" gens S;
        Q := ring subringGens;
        R := ambient Q;
        J := ideal Q;
        FF := coefficientRing R;
        MLiftedToR := lift(M, R);
        subringGensLiftedToR := lift(subringGens, R);
        -- construct the tensor ring
        tensorRingNumVars := (numgens R) + (numcols subringGens);
        oldOrder := (options R).MonomialOrder;
        newOrder := prepend(Eliminate numgens R, oldOrder);
        tensorRing := FF(monoid[Variables => tensorRingNumVars, MonomialOrder => newOrder]);
        liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
        MInTensorRing := liftToTensorRing MLiftedToR;
        subringGensInTensorRing := liftToTensorRing subringGensLiftedToR;
        JInTensorRing := liftToTensorRing J;
        I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} -
            subringGensInTensorRing);
        MNormalForm := MInTensorRing % (I + JInTensorRing);
        projectToQ := map(Q, tensorRing, matrix {toList(numgens Q : 0_Q)} | subringGens);
        S#"inverseFlatteningMap" (M - projectToQ MNormalForm)
    )
);

RingElement % Subring := RingElement => (f, S) -> (
    S#"inverseFlatteningMap" first first entries (matrix{{f}} % S)
);

-- IntersectedSubring
-- a type for subrings created by intersecting two subrings
-- keys are the usual subring keys plus the keys:
-- "originalSubrings" = {S1, S2}
-- S1 and S2 are the original subrings used for the intersection
-- "compositeSubring" = S
-- S is the subring in the tensor ring K[t*S1, (1-t)*S1] (see subringIntersection)
-- this type does not support a dedicated constructor
-- since it is created by the function subringIntersection

IntersectedSubring = new Type of Subring
-- originalSubringGens
-- returns the generators of the original subrings used in the intersection computation
originalSubringGens = method()
originalSubringGens IntersectedSubring := S -> (
    gens \ S#"originalSubrings"
)

-- isFullIntersection
-- returns true if the intersection of the "originalSubring"
-- is guaranteed to be equal to the generators of intersectedSubring
-- note that the composite subring is guaranteed to have a SAGBIBasis stored in its cache
-- note that if the function returns false,
-- then the generators of the intersectedSubring may generate the intersection
-- but it is not guaranteed
isFullIntersection = method()
isFullIntersection IntersectedSubring := Boolean => S -> (
    S#"compositeSubring".cache#SAGBIBasis.SAGBIdata#"sagbiStatus" == 1
)

-- subringIntersection(Subring, Subring)
-- intersects the subrings using a method analogous to the GB method
subringIntersection = method(
    Options => {
        Strategy => "Master",
        SubductionMethod => "Top",
        SAGBILimitType => "Fixed", -- "Fixed" or "Function"
        Limit => 20,
        PrintLevel => 0,
        CheckFullIntersection => true,
        Compute => true
    }
)

subringIntersection(Subring, Subring) := IntersectedSubring => opts -> (S1, S2) -> (
    local limit;
    if opts.CheckFullIntersection and (
        (class S1 === IntersectedSubring and not isFullIntersection S1) or
        (class S2 === IntersectedSubring and  not isFullIntersection S1)) then (
        print ("-- Warning! The input contains an IntersectedSubring " |
            "whose generators may not generate the entire subring");
    );
    -- check that both subring are subrings of the same ambient ring
    A := ambient S1;
    assert(A === ambient S2);
    Q := flattenedRing S1;
    I := ideal Q;
    R := ambient Q;
    -- For notation:
    -- R = K[p_1 .. p_n]
    -- Q = R / I
    -- Construct the ring T = tensor product of Q with K[t]/(t^2 - t)
    -- TAmb := [t := p_0, p_1 .. p_n] 
    -- monomial order of TAmb:
    -- 1) monomial order from R (on p_1 ... p_n)
    -- 2) eliminate p_0
    -- J := I lifted to TAmb + (t^2 - t) --> makes t and (1-t) into idemponents in T
    -- T := TAmb / J
    -- product order with p_0 in a different subring
    newMonomialOrder := prepend(GRevLex => 1, (monoid Q).Options.MonomialOrder);
    M := monoid [
        Variables => (numgens Q + 1),
        MonomialOrder => newMonomialOrder
    ];
    TAmb := (coefficientRing Q) M;
    t := (vars TAmb)_(0,0);
    RtoTAmb := map(TAmb, R, (vars TAmb)_{1 .. numgens Q});
    J := RtoTAmb I + ideal(t^2 - t); -- I + (t^2 - t)
    T := TAmb / J; -- tensor product of Q and K[t]/(t^2-t)
    QtoT := map(T, Q, (vars T)_{1 .. numgens Q});
    TtoQ := map(Q, T, matrix{{0_Q}} | vars Q);
    tinT := (gens T)_0;
    -- Notation:
    -- Gi := generators of Si lifted to T, for each i in {1, 2}
    -- Form the subring of T given by
    -- S := K[t*G1, (1-t)*G2]
    G1 := QtoT S1#"flatteningMap" gens S1;
    G2 := QtoT S1#"flatteningMap" gens S2;
    -- use T; -- Dev note: T contains no user variables
    G := tinT*G1 | (1-tinT)*G2;
    S := subring G;
    -- Compute a sagbi basis for S (take care of options supplied by user)
    -- SB := subalgebraBasis S
    if opts.SAGBILimitType == "Fixed" then (
        limit = opts.Limit;
    )
    else if opts.SAGBILimitType == "Function" then (
        heftVector := transpose matrix {heft ambient flattenedRing S1};
        limit = (max flatten entries ((
            matrix(degree \ flatten entries leadTerm gens S1))*heftVector))*
            (max flatten entries ((
            matrix(degree \ flatten entries leadTerm gens S2))*heftVector));
    );
    SB := sagbi(S,
        Strategy => opts.Strategy,
        SubductionMethod => opts.SubductionMethod,
        Limit => limit,
        PrintLevel => opts.PrintLevel
    );
    -- The intersection of S1 and S2 is generated by
    -- the elements SB|_Q of SB that do not contain t := p_0
    -- If SB is a finite sagbi basis then the intersection computation
    -- is verrified to be correct!
    -- In this case, the elements SB|_Q form a sagbi basis
    -- for the intersection so use forceSB
    -- Note that, if the intersection of S1 and S2
    -- has a finite sagbi basis, then it is NOT guaranteed
    -- that S has a finite sagbi basis.
    intersectionGens := TtoQ selectInSubring(1, gens SB);
    subductionRing := (coefficientRing Q) (monoid[Variables => numcols intersectionGens]);
    presentationMap := map(A,subductionRing,S1#"inverseFlatteningMap" intersectionGens);
    result := new IntersectedSubring from {
        "ambientRing" => A,
        "flattenedRing" => Q,
        "generators" => intersectionGens,
        "presentationRing" => subductionRing,
        "presentationMap" => presentationMap,
        cache => new CacheTable from {},
        "originalSubrings" => {S1, S2},
        "compositeSubring" => S,
        "presentationMap" => presentationMap,
        "flatteningMap" => S1#"flatteningMap",
        "inverseFlatteningMap" => S1#"inverseFlatteningMap"
    };
    if opts.Compute and isSAGBI SB then forceSB result;
    if opts.CheckFullIntersection and not isFullIntersection result then print "-- Warning! Result is not a full intersection.  Try increasing Limit.";
    result
);

-- trying to override intersect to use instead of subringIntersection
intersect(Subring, Subring) := IntersectedSubring => {
    Strategy => "Master",
    SubductionMethod => "Top",
    SAGBILimitType => "Fixed", -- "Fixed" or "Function"
    Limit => 20,
    PrintLevel => 0,
    CheckFullIntersection => true,
    Compute => true
    } >> opts -> (S1, S2) -> subringIntersection(S1, S2, opts);
