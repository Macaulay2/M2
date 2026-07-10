protect toambienthommodule -- cache key for the inclusion of Hom(f, X, Y) into Hom(fX, fY)
-*
These methods implement a strategy for computing Hom(M, N) that identifies it as an
appropriate sub-module of Hom(f*M, f*N) where f*_ is the pushFwd along a RingMap
f making M and N into finite source f modules.

In particular this allows the computation of Hom_S(M, N) when S is a non-commutative ring
in cases where M, N are finite over center S.
*-
Hom(RingMap, Ring, Ring) :=
Hom(RingMap, Ring, Ideal) :=
Hom(RingMap, Ring, Module) :=
Hom(RingMap, Ideal, Ring) :=
Hom(RingMap, Ideal, Ideal) :=
Hom(RingMap, Ideal, Module) :=
Hom(RingMap, Module, Ring) :=
Hom(RingMap, Module, Ideal) := Module => opts -> (f, M, N) -> Hom(f, module M, module N, opts)
Hom(RingMap, Module, Module) := Module => opts -> (f, M, N) -> (
    -- f: RingMap(R <- S)
    -- M: R-module
    -- N: R-module
    R := ring M;
    if R =!= ring N then error "expected modules over the same ring";
    if R =!= target f then error "expected modules over target f";

    Y := youngest(M.cache.cache, N.cache.cache);
    Y#(Hom, f, M, N, opts) ??= (
        -- allow setting PushForwardOpts via options
        M' := pushFwd(f, M, MinimalGenerators => opts.MinimalGenerators);
        N' := pushFwd(f, N, MinimalGenerators => opts.MinimalGenerators);
        H' := Hom(M', N', opts);

        if M == 0 or N == 0 then return H';

        C := R / intersect(annihilator M, annihilator N);
        C' := first pushFwd(map(C, R) * f);
        -- checking linearity for these elements suffices
        liftedGens := lift(pushforward'(C'_{0..numgens C'-1}), R);

        rightCompose := compose(M', M', N');
        leftCompose := compose(M', N', N');
        gensH' := H'_{0..numgens H' - 1};
        -- build the linear maps H' -> H' whose kernels witness R-linearity
        H := kernel matrix for r in first entries liftedGens list (
            -- todo: exploit direct sum decomposition in cases where M is free as in Ext computations
            rMultForM := getStructureMap(f, M', r);
            rMultForN := getStructureMap(f, N', r);
            -- wrap in nested list so we can assemble these into a block matrix outside of the loop
            {map(H', H', rightCompose * (rMultForM ** gensH') - leftCompose * (gensH' ** rMultForN))}
        );

        if opts.MinimalGenerators then H = trim H;

        -- it is not enough to just have the ambient hom module as they may coincide
        -- we need to move this so it does not get clobbered by the custom homomorphism function we store next.
        H.cache#(homomorphism, R) = H'.cache.homomorphism;
        H.cache.homomorphism = (h) -> (
            h' := H.cache#(homomorphism, R) h;
            map(N, M, pushforward'(h' * pushforward(M', M_{0..numgens M - 1})))
        );
        H.cache.toambienthommodule = inducedMap(H', H);
        H.cache.formation = FunctionApplication { Hom, (f, M, N) };

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
    homomorphism'(pushFwd(f, L, MinimalGenerators => opts.MinimalGenerators), opts) // H.cache.toambienthommodule
)

End(RingMap, Ring) :=
End(RingMap, Ideal) := Module => o -> (f, X) -> Hom(f, module X, module X, o)
End(RingMap, Module) := Module => o -> (f, M) -> Hom(f, M, M, o)

--- Ext ---
Ext(ZZ, RingMap, Ring, Ring) :=
Ext(ZZ, RingMap, Ring, Ideal) :=
Ext(ZZ, RingMap, Ring, Module) :=
Ext(ZZ, RingMap, Ideal, Ring) :=
Ext(ZZ, RingMap, Ideal, Ideal) :=
Ext(ZZ, RingMap, Ideal, Module) :=
Ext(ZZ, RingMap, Module, Ring) :=
Ext(ZZ, RingMap, Module, Ideal) := Module => opts -> (i, f, M, N) -> Ext(i, f, module M, module N, opts)
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


        -- here should we trim?

        E.cache.yonedaExtension = liftmap';
        E.cache.yonedaExtension' = invmap';
        -- in i <= 1 case this is a Hom already and we don't want to clobber
        -- existing formation data so use ??= assignment
        E.cache.formation ??= FunctionApplication { Ext, (i, f, M, N, opts) };
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

-------------
-- helpers --
-------------

protect multiplication -- cache key
-- f:S -> R
-- M:a module which is a pushforward of an R-module along f
-- r:R
-- compute "multiplication by r" as an element of Hom_S(M, M)
getStructureMap = (f, M, r) -> M.cache#(multiplication, r) ??= homomorphism' map(M, M, pushforward(M, r * getPushFwdGens(M)))