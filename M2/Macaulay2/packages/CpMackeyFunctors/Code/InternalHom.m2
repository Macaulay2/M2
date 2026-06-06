internalHom = method()

internalHom (CpMackeyFunctor,CpMackeyFunctor) := CpMackeyFunctor => (M,N) -> (
    if not M.PrimeOrder === N.PrimeOrder then error "internalHom: CpMackeyFunctors must have the same prime";
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

internalHom (CpMackeyFunctor, MackeyFunctorHomomorphism) := MackeyFunctorHomomorphism => (M,f) -> (
    if not M.PrimeOrder === (source f).PrimeOrder then error "internalHom: Mackey functors are over different primes";
    p := M.PrimeOrder;
    A := makeBurnsideMackeyFunctor p;
    B := makeUnderlyingFreeMackeyFunctor p;
    src := internalHom(M, source f);
    tgt := internalHom(M, target f);
    underlyingMap := Hom(B ** M, f);
    fixedMap := Hom(A ** M, f);
    map(tgt, src, underlyingMap, fixedMap)
)

internalHom (MackeyFunctorHomomorphism, CpMackeyFunctor) := (f,N) -> (
    if not N.PrimeOrder === (source f).PrimeOrder then error "internalHom: Mackey functors are over different primes";
    p := N.PrimeOrder;
    A := makeBurnsideMackeyFunctor p;
    B := makeUnderlyingFreeMackeyFunctor p;
    src := internalHom(target f, N);
    tgt := internalHom(source f, N);
    underlyingMap := Hom(B ** f, N);
    fixedMap := Hom(A ** f, N);
    map(tgt, src, underlyingMap, fixedMap)
)