protect toambienthommodule -- cache key for the inclusion of Hom(f, X, Y) into Hom(fX, fY)
-*
These methods implement a strategy for computing Hom(M, N) that identifies it as an
appropriate sub-module of Hom(f*M, f*N) where f*_ is the pushFwd along a RingMap
f making M and N into finite source f modules.

In particular this allows the computation of Hom_S(M, N) when S is a non-commutative ring
in cases where M, N are finite over center S.
*-
Hom(RingMap, Ring, Ring)   :=
Hom(RingMap, Ring, Ideal)  :=
Hom(RingMap, Ring, Module) :=
Hom(RingMap, Ideal, Ring)   :=
Hom(RingMap, Ideal, Ideal)  :=
Hom(RingMap, Ideal, Module) :=
Hom(RingMap, Module, Ring)   :=
Hom(RingMap, Module, Ideal)  := Module => opts -> (f, M, N) -> Hom(f, module M, module N, opts)
Hom(RingMap, Module, Module) := Module => opts -> (f, M, N) -> (
    -- f: RingMap(R <- S)
    -- M: R-module
    -- N: R-module
    R := ring M;
    if R =!= ring N then error "expected modules over the same ring";
    if R =!= target f then error "expected modules over target f";

    Y := youngest(M.cache.cache, N.cache.cache);
    Y#(Hom, f, M, N, opts) ??= (
        M.cache#(computing, Hom, RingMap, Module, Module) = true;
        -- todo: allow setting PushForward Options independently from Hom Options?
        M' := pushFwd(f, M, MinimalGenerators => opts.MinimalGenerators);
        N' := pushFwd(f, N, MinimalGenerators => opts.MinimalGenerators);
        H' := Hom(M', N', opts);

        -- early return in this degenerate case. cache contracts are fulfilled
        -- by H' using the usual Hom construction without f.
        if M == 0 or N == 0 then (
            remove(Y, (computing, Hom, RingMap, Module, Module));
            return H';
        );

        M'' := asDirectSum M;
        translate := Hom(map(M'', M, id_M''), N);

        H := null;
        if #components M'' > 1 then (
            Cs := components M'';
            CHoms := apply(Cs, C -> Hom(f, C, N));
            auxH := directSum(CHoms);
            H = if opts.MinimalGenerators then trim auxH else auxH;
            remap := inducedMap(auxH, H);
            H.cache.homomorphism = (h) -> (
                -- do some decomposition song and dance here to map things around
                map(N, M'', matrix {for i from 0 to #Cs - 1 list homomorphism map(CHoms_i, , auxH^[i] * (remap * h))}) * map(M'', M, id_M'')
            );
        ) else (
            H = makeHomModule(f, M', N', H');
            H = if opts.MinimalGenerators then trim H else H;
            H.cache.homomorphism = (h) -> (
                h' := H.cache#(homomorphism, R) h;
                map(N, M, pushforward'(h' * pushforward(M', M_{0..numgens M - 1})))
            );
        );

        H.cache.toambienthommodule = inducedMap(H', H);
        H.cache#(homomorphism, R) = H'.cache.homomorphism;
        H.cache.formation = FunctionApplication { Hom, (f, M, N) };

        remove(M.cache, (computing, Hom, RingMap, Module, Module));
        H
    )
)


-- induced map: Hom(f, target F, M) -> Hom(f, source F, M)
Hom(RingMap, Matrix, Module) := Matrix => opts -> (f, F, M) -> (
    sourceModule := Hom(f, source F, M, opts);
    targetModule := Hom(f, target F, M, opts);
    if sourceModule == 0 or targetModule == 0 then return map(sourceModule, targetModule, 0);

    -- Hom_R(target F, M) -> Hom_R(source F, M)
    PushHom := Hom(
        pushFwd(f, F, MinimalGenerators => opts.MinimalGenerators),
        pushFwd(f, M, MinimalGenerators => opts.MinimalGenerators),
        opts
    );

    (PushHom * targetModule.cache.toambienthommodule) // sourceModule.cache.toambienthommodule
)

-- induced map: Hom(f, M, source F) -> Hom(f, M, target F)
Hom(RingMap, Module, Matrix) := Matrix => opts -> (f, M, F) -> (
    sourceModule := Hom(f, M, source F, opts);
    targetModule := Hom(f, M, target F, opts);
    if sourceModule == 0 or targetModule == 0 then return map(targetModule, sourceModule, 0);

    -- Hom_R(M, source F) -> Hom_R(M, target F)
    PushHom := Hom(
        pushFwd(f, M, MinimalGenerators => opts.MinimalGenerators),
        pushFwd(f, F, MinimalGenerators => opts.MinimalGenerators),
        opts
    );

    (PushHom * sourceModule.cache.toambienthommodule) // targetModule.cache.toambienthommodule
)

-- induced map: Hom(f, target F, source G) -> Hom(f, source F, target G)
Hom(RingMap, Matrix, Matrix) := Matrix => o -> (f, F, G) -> Hom(f, source F, G, o) * Hom(f, F, source G, o)

-- from a matrix L: M -> N and a RingMap f get the corresponding element of Hom(f, M, N)
homomorphism'(RingMap, Matrix) := Matrix => opts -> (f, L) -> (
    H := Hom(f, source L, target L, opts);
    result := homomorphism'(pushFwd(f, L, MinimalGenerators => opts.MinimalGenerators), opts);
    result // H.cache.toambienthommodule
)

End(RingMap, Ring)   :=
End(RingMap, Ideal)  := Module => o -> (f, X) -> Hom(f, module X, module X, o)
End(RingMap, Module) := Module => o -> (f, M) -> Hom(f, M, M, o)

--- Ext ---
Ext(ZZ, RingMap, Ring, Ring)   :=
Ext(ZZ, RingMap, Ring, Ideal)  :=
Ext(ZZ, RingMap, Ring, Module) :=
Ext(ZZ, RingMap, Ideal, Ring)   :=
Ext(ZZ, RingMap, Ideal, Ideal)  :=
Ext(ZZ, RingMap, Ideal, Module) :=
Ext(ZZ, RingMap, Module, Ring)   :=
Ext(ZZ, RingMap, Module, Ideal)  := Module => opts -> (i, f, M, N) -> Ext(i, f, module M, module N, opts)
Ext(ZZ, RingMap, Module, Module) := Module => opts -> (i, f, M, N) -> (
    -- i: ZZ
    -- f: RingMap(R <- S)
    -- M: R-module
    -- N: R-module
    R := ring M;
    if R =!= ring N then error "expected modules over the same ring";
    if R =!= target f then error "expected modules over target f";

    Y := youngest(M.cache.cache, N.cache.cache);
    Y#(Ext, i, f, M, N, opts) ??= (
        S := source f; -- result is an S-module since we are pushing forward along f.
        H := null; -- result: Ext^i(f, M, N)
        liftmap := null; -- given h: S^1 --> H, returns h': S^1 --> Hom(f, FM_i, N)
        invmap := null; -- given h': S^1 --> Hom(f, FM_i, N), returns h: S^1 --> H
        p := null;
        if i < 0 then (
            H = S^0;
            liftmap = (h) -> map(Hom(R^0, N, opts), source h, 0);
            invmap = (h') -> map(H, source h', 0);
        )
        else if i === 0 then (
            H = Hom(f, M, N, opts);
            p = Hom(f, map(M, cover M, 1), N, opts);
            liftmap = (h) -> p * h;
            invmap = (h') -> h' // p;
        ) else (
            FM := resolution(M, LengthLimit => i + 1);
            D := dd^FM;
            C := kernel Hom(f, D_(i+1), N, opts);
            B := image Hom(f, D_i, N, opts);
            H = C / B;
            p = inducedMap(Hom(f, FM_i, N, opts), C) * map(C, H, 1);
            liftmap = (h) -> p * h;
            invmap = (h') -> h' // p;
        );

        (E, liftmap', invmap') := if opts.MinimalGenerators then (
            trimmed := trim H;
            (
                trimmed,
                (h) -> liftmap(inducedMap(H, trimmed) * h),
                (h') -> invmap(h') // inducedMap(H, trimmed)
            )
        ) else (H, liftmap, invmap);

        E.cache.yonedaExtension = liftmap';
        E.cache.yonedaExtension' = invmap';
        -- i == 0 is Hom case and it has it's own formation data
        if i >= 1 then E.cache.formation = FunctionApplication { Ext, (i, f, M, N, opts) };
        E.cache.Ext = (i,M,N);

        E
    )
)

yonedaExtension'(RingMap, Complex) := Matrix => opts -> (f, C) -> (
    -- f: S -> R is a ring map
    -- given an exact complex of R-modules of the form
    -- 0 <-- M <-- C_0 <-- C_1 <-- ... <-- C_(d-1) <-- N <-- 0
    -- returns the corresponding map S^1 --> Ext^d(f, M, N).
    (lo, hi) := concentration C;
    M := complex(C_lo, Base => lo);
    -- notice that T is a shifted complex which changes the sign of the differential
    T := naiveTruncation(C, (lo+1, hi))[1];
    s := map(M, T, i -> if i == lo then dd^C_(lo+1) else map(M_i, T_i, 0));
    g := resolutionMap(M, LengthLimit => hi);
    sinverse := liftMapAlongQuasiIsomorphism(g, s);
    yonedaMap := sinverse_(hi-1);  -- map FM_d --> N
    extd := Ext^(hi-lo-1)(f, C_lo, C_hi, opts);
    extd.cache.yonedaExtension' homomorphism'(f, yonedaMap, opts)
)

-- todo: yonedaMap'(RingMap, ChainComplexMap)

-------------
-- helpers --
-------------

-- f: RingMap S -> R
-- M': Module, pushFwd of an R-module
-- N': Module, pushFwd of an R-module
-- H': Hom(M', N'), Hom as S-modules
makeHomModule = (f, M', N', H') -> (
    R := target f;
    M := pushFwdSource M';
    N := pushFwdSource N';

    if M === module R then M = module R;
    if isRankOneFree M and not inHomComputation(M) then (
        -- reduce to rank one free case here with a method that handles twisting
        return makeHomFromRankOneFreeModule(f, M, N);
    );

    C := R / (intersect(annihilator pushFwdSource M', annihilator pushFwdSource N'));
    C' := pushFwd(map(C, R) * f);
    -- puzzle: for non-commutative rings we need basis here and not just gens.
    -- possibly due to failure of some associativity relations for modules over non-commutative rings?
    testElements := if isCommutative R then C'_{0..numgens C' - 1} else basis C';
    -- checking linearity for these elements suffices
    liftedGens := lift(pushforward'(testElements), R);

    rightCompose := compose(M', M', N');
    leftCompose := compose(M', N', N');
    gensH' := H'_{0..numgens H' - 1};
    -- build the linear maps H' -> H' whose kernels witness R-linearity
    H := kernel matrix for r in first entries liftedGens list (
        rMultForM := getStructureMap(r, M');
        rMultForN := getStructureMap(r, N');
        -- wrapping in nested list so we produce the correct block matrix outside the loop
        {map(H', H', rightCompose * (rMultForM ** gensH') - leftCompose * (gensH' ** rMultForN))}
    );
    H
)

-- reduce homs out of free modules to homs out of the canonical rank one free module
makeHomFromRankOneFreeModule = (f, M, N) -> (
    (S, R) := (source f, target f);
    X := Hom(f, module R, N);
    if degreeGroup R == degreeGroup S then X ** S^(degrees M) else X
)

-- to avoid infinite recursion
inHomComputation = (M) -> M.cache#?(computing, Hom, RingMap, Module, Module)

-- compute "multiplication by r" as an element of Hom_S(M, M)
protect multiplication -- cache key
getStructureMap = (r, M) -> M.cache#(multiplication, r) ?? (
    -- r: RingElement of R
    -- M: pushFwd of M to an S-module

    -- reduce to components if they are all already pushforwards
    if #components M > 1 and all(components M, C -> pushFwdSource C =!= null) then (
        return homomorphism' directSum apply(components M, C -> homomorphism getStructureMap(r, C));
    );

    sourceM := pushFwdSource M;
    if isRankOneFree sourceM then (
        f := pushFwdRingMap M;
        M' := pushFwd(f, module target f, pushFwdOpts M);
        X := map(M', M', pushforward(M', r * pushFwdGens(M')));
        -- handle the pushFwd of module target f by hand so that caching works
        M'.cache#(multiplication, r) = homomorphism' X;
        homomorphism' map(M, M, X)
    ) else (
        homomorphism' map(M, M, pushforward(M, r * pushFwdGens(M)))
    )
)