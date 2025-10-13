------additional stuff for cohomological Mackey functors------
makeUniversalMapFixedCohomological = method()
-- Given a Mackey functor M and vector x in fixed module, produce map Z -> M
makeUniversalMapFixedCohomological(CpMackeyFunctor,Vector) := MackeyFunctorHomomorphism => (M,x) -> makeUniversalMapFixedCohomological(M,matrix x)
-- Given a Mackey functor M and a matrix of n vectors in fixed module, produce map Z^n -> M
makeUniversalMapFixedCohomological(CpMackeyFunctor,Matrix) := MackeyFunctorHomomorphism => (M,x) -> (
    if not isCohomological(M) then error "input is not a cohomological Mackey functor";
    n := numColumns x;
    if n == 0 then return map(M, makeZeroMackeyFunctor(M.PrimeOrder), 0);
    L := {for i to n-1 list (
        X := inducedMap(M.Fixed, , matrix x_i);
        p := M.PrimeOrder;
        Z := makeFixedPointMackeyFunctor(p,id_(ZZ^1));
        U := M.Res * X;
        F := X;
        map(M, Z, U, F)
	)};
    matrix L
)

-- Given a Cohomological Mackey functor M,
-- a matrix of n elements X in underlying, and
-- a matrix of m elements Y in fixed,
-- return the universal map B^n ++ Z^m -> M
makeUniversalMapCohomological = method()
makeUniversalMapCohomological(CpMackeyFunctor,Matrix,Matrix) := MackeyFunctorHomomorphism => (M,X,Y) -> (
    return makeUniversalMapUnderlying(M,X) | makeUniversalMapFixedCohomological(M,Y)
)

-- make a surjection from a free cohomological Mackey functor
makeFreeModuleSurjectionCohomological = method()
makeFreeModuleSurjectionCohomological(CpMackeyFunctor) := MackeyFunctorHomomorphism => (M) -> (
    gensUnderlying0 := mingens cokernel (id_(M.Underlying) - M.Conj);
    gensUnderlying := inducedMap(M.Underlying, source gensUnderlying0, gensUnderlying0);
    gensFixed0 := mingens cokernel (M.Trans * gensUnderlying);
    gensFixed := inducedMap(M.Fixed, source gensFixed0, gensFixed0);
    f := makeUniversalMapCohomological(M, gensUnderlying, gensFixed);
    gensTorsion0 := mingens ((cokernel f).Underlying);
    gensTorsion := inducedMap(M.Underlying, source gensTorsion0, gensTorsion0);
    g := makeUniversalMapUnderlying(M, gensTorsion);
    f | g
)


-- get resolution up to F_(n-1) <-- F_n
resolutionCohomological = method()
resolutionCohomological(CpMackeyFunctor,ZZ) := List => (M,n) -> (
    if n < 0 then return {};
    k := 0;

    -- check whether existing resolution exists
    -- k will be the length of the existing resolution
    if M.cache#?"CohProjRes" then (
        k = length M.cache#"CohProjRes";
    ) else (
        -- initialize resolution if doesn't exist
        M.cache#"CohProjRes" = {makeFreeModuleSurjectionCohomological M};
        k = 1
    );

    while k <= n do (
        -- add differential d_k to cache
        d := (M.cache#"CohProjRes")#(k-1);
        M.cache#"CohProjRes" |= {inducedMap(source d, kernel d) * (makeFreeModuleSurjectionCohomological (kernel d))};
        k += 1
    );

    M.cache#"CohProjRes"_(for i to n list i)
)

-- will resolve first argument
TorCoh = method()
TorCoh(ZZ,CpMackeyFunctor,CpMackeyFunctor) := CpMackeyFunctor => (i,M,N) -> (
    if not isCohomological(N) then error "second Mackey functor not cohomological";
    d := resolutionCohomological(M,i+1);
    if i == 0 then (
        coker ((d_1)**N)
    ) else (
        computeHomology((d_i)**N,((d_(i+1))**N))
    )
)

-- will resolve first argument
ExtCoh = method()
ExtCoh(ZZ,CpMackeyFunctor,CpMackeyFunctor) := CpMackeyFunctor => (i,M,N) -> (
    if not isCohomological(N) then error "second Mackey functor not cohomological";
    d := resolutionCohomological(M,i+1);
    if i == 0 then (
        -- it's a cochain complex, so ker instead of coker
        ker internalHom(d_1, N)
    ) else (
        computeHomology(internalHom(d_(i+1), N),internalHom(d_i, N))
    )
)
