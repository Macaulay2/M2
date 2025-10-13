-- For future compatibility with Complexes package
isAbelianCategory CpMackeyFunctor := M -> true

-- Direct sums
CpMackeyFunctor.directSum = args -> (
    if not same apply(args,M -> M.PrimeOrder) then error "-- Primes not compatible";
    T := directSum apply(args, M -> M.Trans);
    R := directSum apply(args, M -> M.Res);
    C := directSum apply(args, M -> M.Conj);
    p := (args_0).PrimeOrder;
    makeCpMackeyFunctor(p,R,T,C)
    )
CpMackeyFunctor ++ CpMackeyFunctor := CpMackeyFunctor => (F, G) -> CpMackeyFunctor.directSum(F, G)
directSum CpMackeyFunctor := CpMackeyFunctor => F -> CpMackeyFunctor.directSum(1 : F)

-- Kernels
ker MackeyFunctorHomomorphism := CpMackeyFunctor => options -> F -> (
    T := ker F.FixedMap;
    B := ker F.UnderlyingMap;

    C' := inducedMap(B,B,(source F).Conj);
    T' := inducedMap(T,B,(source F).Trans);
    R' := inducedMap(B,T,(source F).Res);
    p' := (source F).PrimeOrder;

    makeCpMackeyFunctor(p',R',T',C')
)

-- Cokernels
coker MackeyFunctorHomomorphism := CpMackeyFunctor => F -> (
    T := coker F.FixedMap;
    B := coker F.UnderlyingMap;

    C' := inducedMap(B,B,(target F).Conj);
    T' := inducedMap(T,B,(target F).Trans);
    R' := inducedMap(B,T,(target F).Res);
    p' := (target F).PrimeOrder;

    makeCpMackeyFunctor(p',R',T',C')
)