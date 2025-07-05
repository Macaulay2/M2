InternalHom = method()

InternalHom (CpMackeyFunctor,CpMackeyFunctor) := CpMackeyFunctor => (M,N) -> (
    if not M.PrimeOrder === N.PrimeOrder then error "InternalHom: CpMackeyFunctors must have the same prime";
    p := M.PrimeOrder;
    A := makeBurnsideMackeyFunctor p;
    B := makeUnderlyingFreeMackeyFunctor p;
    underlying := Hom(B ** M, N);
    fixed := Hom(A ** M, N);
    tau := makeUniversalMapUnderlying(A, vector(A.Underlying,{1}));
    restriction := Hom(tau ** M, N);
    rho := makeUniversalMapFixed(B, vector(B.Fixed,{1}));
    transfer := Hom(rho ** M, N);
    gen := {0,1} | for i to p-3 list 0;
    conj := Hom(makeUniversalMapUnderlying(B, vector(B.Underlying, gen)) ** M, N);
    makeCpMackeyFunctor(
        p,
        restriction,
        transfer,
        conj
    )
)

InternalHom (CpMackeyFunctor, MackeyFunctorHomomorphism) := MackeyFunctorHomomorphism => (M,f) -> (
    if not M.PrimeOrder === (source f).PrimeOrder then error "InternalHom: Mackey functors are over different primes";
    p := M.PrimeOrder;
    A := makeBurnsideMackeyFunctor p;
    B := makeUnderlyingFreeMackeyFunctor p;
    src := InternalHom(M, source f);
    tgt := InternalHom(M, target f);
    underlyingMap := Hom(B ** M, f);
    fixedMap := Hom(A ** M, f);
    map(tgt, src, underlyingMap, fixedMap)
)

-- InternalHom (MackeyFunctorHomomorphism, CpMackeyFunctor) := MackeyFunctorHomomorphism => (f,N) -> (
InternalHom (MackeyFunctorHomomorphism, CpMackeyFunctor) := (f,N) -> (
    if not N.PrimeOrder === (source f).PrimeOrder then error "InternalHom: Mackey functors are over different primes";
    p := N.PrimeOrder;
    A := makeBurnsideMackeyFunctor p;
    B := makeUnderlyingFreeMackeyFunctor p;
    src := InternalHom(target f, N);
    tgt := InternalHom(source f, N);
    underlyingMap := Hom(B ** f, N);
    fixedMap := Hom(A ** f, N);
    map(tgt, src, underlyingMap, fixedMap)
)