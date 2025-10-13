-----------------------------------------------------------------------------
-- Hom functor and related methods
-----------------------------------------------------------------------------
-- most of this file used to be in modules2.m2

needs "modules.m2"
needs "matrix1.m2"

Hom = method(Options => {
	DegreeLimit       => null,
	MinimalGenerators => true,
	Strategy          => null,
	})

End = options Hom >> o -> M -> Hom(M, M, o)

-- TODO: Hom(R, S) should work as well
Hom(Ring, Ring)   :=
Hom(Ring, Ideal)  :=
Hom(Ring, Module) :=
Hom(Ideal, Ring)   :=
Hom(Ideal, Ideal)  :=
Hom(Ideal, Module) :=
Hom(Module, Ring)   :=
Hom(Module, Ideal)  := Module => opts -> (M, N) -> Hom(module M, module N, opts)
Hom(Module, Module) := Module => opts -> (M, N) -> (
    -- TODO: take advantage of cached results with higher e
    e := opts.DegreeLimit;
    if e === {} then e = null;
    if e === 0  then e = degree 0_M;
    -- M.cache is a hashless (hence ageless) CacheTable, but
    -- M.cache.cache is a MutableHashTable, hence has an age.
    Y := youngest(M.cache.cache, N.cache.cache);
    if Y#?(Hom, M, N, e) then return Y#(Hom, M, N, e);
    H := runHooks((Hom, Module, Module), (opts, M, N), Strategy => opts.Strategy);
    if H === null then error "Hom: no strategy found for the given input";
    trim' := if opts.MinimalGenerators then trim else identity;
    -- a hack: we really want to type "Hom(M, N) = ..."
    H = Y#(Hom, M, N, e) = if opts.MinimalGenerators then trim H else H;
    H.cache.homomorphism = f -> map(N, M, adjoint'(f, M, N), Degree => first degrees source f + degree f);
    H.cache.formation = FunctionApplication { Hom, (M, N, DegreeLimit => e) };
    H)

basicHom = (M, N) -> kernel(transpose presentation M ** N)
addHook((Hom, Module, Module), Strategy => Default,  (opts, M, N) -> basicHom(M, N))
addHook((Hom, Module, Module), Strategy => Syzygies, (opts, M, N) -> (
    -- This algorithm accepts DegreeLimit, but is slower in some cases
    e := opts.DegreeLimit;
    -- TODO: any other cases which should be excluded?
    if e === null then return null;
    A := presentation M; (G, F) := (target A, source A); -- M <-- G <-- F
    B := presentation N; (L, K) := (target B, source B); -- N <-- L <-- K
    piN := inducedMap(N, L, generators N);
    psi := (Hom(A, N) * Hom(G, piN)) // Hom(F, piN);
    p := id_(basicHom(G, L)) | map(basicHom(G, L), basicHom(F, K), 0);
    r := syz(psi | -Hom(F, B), DegreeLimit => e);
    image(Hom(G, piN) * p * r)))

Hom(Matrix, Module) := Matrix => o -> (f, N) -> inducedMap(Hom(source f, N, o), Hom(target f, N, o), transpose cover f ** N, Verify => false)
Hom(Module, Matrix) := Matrix => o -> (M, g) -> inducedMap(Hom(M, target g, o), Hom(M, source g, o),      dual cover M ** g, Verify => false)
Hom(Matrix, Matrix) := Matrix => o -> (f, g) -> Hom(source f, g, o) * Hom(f, source g, o)

-----------------------------------------------------------------------------

-- TODO: compare speed with Hom(M, R^1)
dual Module := Module => {} >> o -> F -> F.cache.dual ??= (
     if not isFreeModule F then kernel transpose presentation F
     else new Module from (ring F,rawDual raw F))

dual Matrix := Matrix => options Hom >> o -> f -> Hom(f, module ring f, o)

-----------------------------------------------------------------------------

reshape = method()
reshape(Module, Module, Matrix) := Matrix => (F, G, m) -> map(F, G, rawReshape(raw m, raw cover F, raw cover G))

adjoint' = method()
adjoint'(Matrix, Module, Module) := Matrix => (m, G, H) -> (
    -- adjoint':  m : F --> Hom(G,H) ===> F ** G --> H
    -- warning: in versions 1.7.0.1 and older dual G was called for, instead of G, since G was assumed to be free
    F := source m;
    inducedMap(H, F ** G, reshape(super H, F ** G, super m), Verify => false))

adjoint = method(Options => options Hom)
adjoint (Matrix, Module, Module) := Matrix => opts -> (m, F, G) -> (
    -- adjoint :  m : F ** G --> H ===> F --> Hom(G,H)
    H := target m;
    inducedMap(Hom(G, H, opts), F, reshape(Hom(cover G, ambient H, opts), F, super m), Verify => false))

-----------------------------------------------------------------------------

homomorphism = method()
homomorphism Matrix := m -> homomorphism vector m
homomorphism Vector := v -> (
    -- from an element v in Hom(M, N) produce a map f:M --> N
    if (H := module v).cache.?homomorphism then H.cache.homomorphism matrix v
    else error "homomorphism: expected the input to be an element of a module of the form 'Hom(M,N)'")

homomorphism' = method(Options => options Hom)
homomorphism' Matrix := Matrix => opts -> f -> (
    -- from a map M --> N produce a map R^1 -> Hom(M, N)
    adjoint(f, module ring f, source f, opts))

-----------------------------------------------------------------------------

compose = method(Options => true)
compose(Module, Module, Module) := Matrix => options Hom >> opts -> (M, N, P) -> (
    -- produce the composition map Hom(M, N) ** Hom(N, P) --> Hom(M, P)
    -- note: see 1f4bfb3 for an older strategy which may be faster in big examples
    R := ring M;
    if not ring N === R or not ring P === R then error "expected modules over the same ring";
    if isQuotientModule N then (
	-- Now cover N === ambient N
	f := dual cover M ** reshape(R^1, cover N ** dual cover N, id_(cover N)) ** ambient P;
	g := generators Hom(M, N, opts) ** generators Hom(N, P, opts);
	m := map(dual cover M ** ambient P, Hom(M, N, opts) ** Hom(N, P, opts), f * g);
	inducedMap(Hom(M, P, opts), , m, Verify => false))
    else (
	N' := cokernel presentation N;
	compose(M, N', P, opts) * (Hom(M, map(N', N, 1), opts) ** Hom(map(N, N', 1), P, opts))))
