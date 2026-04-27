-- given two composable morphisms f,g such that f*g == 0, return ker(f)/im(g)
computeHomology = method()
computeHomology(MackeyFunctorHomomorphism,MackeyFunctorHomomorphism) := CpMackeyFunctor => (f,g) -> (
    comp := f*g;
    if (comp != map(target comp, source comp, 0)) then (
        error "inputted maps do not form a complex"
    );
    i := inducedMap(source(f),ker(f));
    phi := inducedMap(ker(f),source(g),g);
    coker phi
)

-- will resolve first argument
Tor(ZZ,CpMackeyFunctor,CpMackeyFunctor) := CpMackeyFunctor => opts -> (i,M,N) -> (
    d := res(M,i+1);
    if i == 0 then (
        coker ((d_1)**N)
    ) else (
        computeHomology((d_i)**N,((d_(i+1))**N))
    )
)

-- will resolve first argument
Ext(ZZ,CpMackeyFunctor,CpMackeyFunctor) := CpMackeyFunctor => opts -> (i,M,N) -> (
    d := res(M,i+1);
    if i == 0 then (
        -- it's a cochain complex, so ker instead of coker
        ker internalHom(d_1, N)
    ) else (
        computeHomology(internalHom(d_(i+1), N),internalHom(d_i, N))
    )
)
