-- first make sure we hit everything on the bottom
makeFreeModuleSurjection = method()
makeFreeModuleSurjection(CpMackeyFunctor) := MackeyFunctorHomomorphism => (M) -> (
    gensUnderlying0 := mingens cokernel (id_(M.Underlying) - M.Conj);
    gensUnderlying := inducedMap(M.Underlying, source gensUnderlying0, gensUnderlying0);
    gensFixed0 := mingens cokernel (M.Trans * gensUnderlying);
    gensFixed := inducedMap(M.Fixed, source gensFixed0, gensFixed0);
    makeUniversalMap(M, gensUnderlying, gensFixed)
)

-- get resolution up to F_(n-1) <-- F_n
resolution(CpMackeyFunctor,ZZ) := List => opts -> (M,n) -> (
    if n < 0 then return {};

    -- check whether existing resolution exists
    -- k will be the length of the existing resolution
    k := 0;
    if M.cache#?"ProjRes" then (
        k = length M.cache#"ProjRes";
    ) else (
        -- initialize resolution if doesn't exist
        M.cache#"ProjRes" = {makeFreeModuleSurjection M};
        k = 1
    );

    while k <= n do (
        -- add differential d_k to cache
        d := (M.cache#"ProjRes")#(k-1);
        M.cache#"ProjRes" |= {inducedMap(source d, kernel d) * (makeFreeModuleSurjection (kernel d))};
        k += 1
    );

    M.cache#"ProjRes"_(for i to n list i)
)
