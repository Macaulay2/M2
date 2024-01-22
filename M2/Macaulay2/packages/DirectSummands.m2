---------------------------------------------------------------------------
-- PURPOSE : to compute direct summands of modules and coherent sheaves
--
-- UPDATE HISTORY : created Oct 2023
--
-- TODO :
-- 1. implement over Dmodules and other non-commutative rings
-- 2. try to find all irreducible idempotents from End M using representation theory
-- 3. cache maps to the summands
-- 4. rewrite (isDirectSum, Module)
---------------------------------------------------------------------------
newPackage(
    "DirectSummands",
    Version => "0.1",
    Date => "25 Oct 2023",
    Headline => "decompositions of modules and coherent sheaves",
    Authors => {
	{ Name => "Devlin Mallory", Email => "malloryd@math.utah.edu", HomePage => "https://math.utah.edu/~malloryd/"},
	{ Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",         HomePage => "https://math.umn.edu/~mahrud/"}
	},
    Keywords => { "Commutative Algebra" },
    PackageImports => {
	"RationalPoints2", -- for rationalPoints in findIdempotent
	"PushForward", -- only for frobenius.m2
	"Polyhedra", -- for coneFromVData and coneComp
	"Truncations", -- for effGenerators
	-- "Varieties",
	},
    AuxiliaryFiles => true,
    DebuggingMode => true
    )

export {
    -- methods
    "directSummands", "summands" => "directSummands",
    "findIdempotent", "findIdem" => "findIdempotent",
    -- symbols
    "ExtendGroundField",
    "Indecomposable",
    "Recursive",
    "Tries",
    -- frobenius methods
    "frobeniusMap",
    "frobeniusRing",
    "frobeniusPullback",
    "frobeniusPushforward",
    "frobeniusTwist",
    }

-----------------------------------------------------------------------------
-* Code section *-
-----------------------------------------------------------------------------

-- helpers for computing Frobenius pushforwards of modules and sheaves
-- TODO: move to PushForward package?
needs "./DirectSummands/frobenius.m2"
-- helpers for finding random idempotents of a module
needs "./DirectSummands/idempotents.m2"

-----------------------------------------------------------------------------
-- Things to move to the Core
-----------------------------------------------------------------------------

-* -- FIXME: what was this for?
importFrom_Core { "raw", "submatrixFree", "listZZ", "rawSubmatrix" };
try (
submatrixFree = (m, rows, cols) -> (if rows === null
    then rawSubmatrix(raw cover m, listZZ cols)
    else rawSubmatrix(raw cover m, listZZ rows,
	if cols =!= null then listZZ cols else 0 .. numgens source m - 1))
)
*-

-- return the submatrix with given degrees of target and source
submatrixByDegrees(Matrix, Sequence) := (m, degs) -> (
    (tar, src) := degs;
    col := if src =!= null then positions(degrees source m, deg -> member(deg, src));
    row := if tar =!= null then positions(degrees target m, deg -> member(deg, tar));
    submatrix(m, row, col))

-- TODO: perhaps also take degree into account
CoherentSheaf ? CoherentSheaf :=
Module ? Module := (M, N) -> rank M ? rank N

-- TODO: move to Core
position(ZZ, Function) := o -> (n, f) -> position(0..n-1, f)
-- TODO: this is different from position(List,List,Function)
position' = method()
position'(VisibleList, VisibleList, Function) := (B, C, f) -> for b in B do for c in C do if f(b, c) then return (b, c)
position'(ZZ,          ZZ,          Function) := (B, C, f) -> position'(0..B-1, 0..C-1, f)

-- TODO: what generality should this be in?
-- WANT:
--   R ** ZZ/101 to change characteristic
--   R ** S to change coefficient ring
-- TODO: can you change the ground field but keep the tower structure?
QuotientRing   ** GaloisField :=
PolynomialRing ** GaloisField := (R, L) -> (
    -- TODO: in general we may want to keep part of the ring tower
    A := first flattenRing(R, CoefficientRing => null);
    quotient sub(ideal A, L monoid A))

changeBaseField = (L, M) -> (
    S := first flattenRing(ring M, CoefficientRing => null);
    R := quotient sub(ideal S, L monoid S);
    directSum apply(components M,
	N -> coker sub(presentation N, R)))

nonzero = x -> select(x, i -> i != 0)
nonnull = x -> select(x, i -> i =!= null)

random Module := Vector => o -> M -> vector(gens M * random(cover M, module ring M, o))
homomorphism Vector := v -> homomorphism matrix v

checkRecursionDepth = () -> if recursionDepth() > recursionLimit - 20 then printerr(
    "Warning: the recursion depth limit may need to be extended; use `recursionLimit = N`")

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

importFrom_Truncations { "effGenerators" }

coneComp = (C, u, v) -> (
    --if u == v                            then symbol== else
    if contains(C, matrix vector(v - u)) then symbol <= else
    if contains(C, matrix vector(u - v)) then symbol > else incomparable)

-- TODO: add this as a strategy to basis
smartBasis = (deg, M) -> (
    if instance(deg, ZZ) then deg = {deg};
    degs := if #deg == 1 then select(unique degrees M, d -> d <= deg) else (
	-- FIXME: in the multigraded case sometimes just using basis is faster:
	return basis(deg, M);
        -- in the multigraded case, coneMin and coneComp can be slow
	-- but for sufficiently large modules they are still an improvement
        -- TODO: make them faster
        C := coneFromVData effGenerators ring M;
        --elapsedTime compMin(C, unique degrees M) -- TODO: this is not the right thing
        select(unique degrees M, d -> coneComp(C, d, deg) == symbol <=));
    if degs === {deg} then return map(M, , submatrixByDegrees(gens cover M, (, degs)));
    M' := subquotient(ambient M,
	if M.?generators then submatrixByDegrees(gens M, (, degs)),
	if M.?relations  then relations M);
    M'.cache.homomorphism = M.cache.homomorphism;
    basis(deg, M')) -- caching this globally causes issues!

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- this is a kludge to handle the case when h^2 = ah
reduceScalar = m -> m // scan(unique flatten entries m | {1}, x -> if isConstant x and not zero x then break x)
isIdempotent = h -> reduceScalar(h^2) == reduceScalar h

-- TODO: can we return cached summands from the closest field extension?
-- all cached keys: select(keys M.cache, k -> instance(k, Option) and k#0 === symbol directSummands)
cachedSummands = { ExtendGroundField => null } >> o -> M -> (
    if  M.cache#?(symbol summands => o.ExtendGroundField)
    then M.cache#(symbol summands => o.ExtendGroundField) else components M)

-- Note: M may need to be extended to a field extensions
-- TODO: when splitting over a field extension, use cached splitting over summands
-- TODO: cache the inclusion maps
-- Strategies:
-- 1 => use expanded hom
-- 2 => use degrees of generators as heuristic to peel off line bundles first
directSummands = method(Options => { Recursive => true, Tries => 0, Verbose => true, Strategy => 3, ExtendGroundField => null })
directSummands Module := List => opts -> (cacheValue (symbol summands => opts.ExtendGroundField)) (M -> sort(
    checkRecursionDepth();
    -- Note: rank does weird stuff if R is not a domain
    -- Note: End does not work for WeylAlgebras or AssociativeAlgebras yet, nor does basis
    R := ring M;
    if not isCommutative R and not isWeylAlgebra R then error "expected a commutative base ring";
    if 0 < debugLevel then printerr("splitting module of rank: ", toString rank M);
    if isFreeModule M then return apply(-degrees M, d -> R^{d});
    if 1 < #components M then return flatten apply(components M, directSummands_opts); -- TODO: parallelize
    if opts.ExtendGroundField =!= null then (
	L := opts.ExtendGroundField;
	L  = if instance(L, ZZ)   then GF(char R, L)
	else if instance(L, Ring) then L else error "expected an integer or a ground field";
	M = changeBaseField(L, directSum cachedSummands M);
	R = ring M);
    -- Attempt to peel off line bundles by observing the degrees of generators
    if opts.Strategy & 2 == 2 then (
	opts = opts ++ { Strategy => opts.Strategy ^^ 2 }; -- so we only try this once
	if 0 < debugLevel then stderr << " -- peeling off rank 1 summands: " << flush;
	L = directSummands(apply(-unique degrees M, d -> R^{d}), M, opts);
	if 0 < debugLevel then stderr << endl << " -- split off " << #L - 1 << " summands!" << endl;
	if 1 < #L then return directSummands(directSum L, opts));
    --
    K := coker vars R;
    zdeg := degree 0_M;
    -- TODO: make "elapsedTime" contingent on verbosity
    elapsedTime A := if opts.Strategy & 1 == 1 then Hom(M, M, zdeg) else End M; -- most time consuming step
    elapsedTime B := smartBasis(zdeg, A);
    -- FIXME: this currently does not find _all_ idempotents
    flag := true; -- whether all non-identity homomorphisms are zero mod m
    -- TODO: 10k columns for F_*(OO_X) on Gr(2,4) over ZZ/3 take a long time
    idem := position(numcols B, c -> (
	    h := homomorphism B_{c};
	    if h == id_M or h == 0 then false else (
		if flag and K ** h != 0
		then flag = false;
		isIdempotent h))
	);
    -- check if M is certifiably indecomposable
    if flag then (
	if 0 < debugLevel then printerr("\t... certified indecomposable!");
	M.cache.Indecomposable = true; return {M} );
    -- TODO: parallelize
    h := if idem =!= null then homomorphism B_{idem} else try findIdempotent M;
    -- TODO: add this when the maps M_[i] and M^[i] also work
    -- M.cache.components =
    if h === null then {M} else nonzero flatten join(
	-- TODO: restrict End M to each summand and pass it on
	-- TODO: could use 'compose' perhaps
	-- TODO: can we check if M has multiple copies of M1 or M2 quickly?
	M1 := prune image h;
	M2 := prune coker h;
	-- Projection maps to the summands
	--p1 := inverse M1.cache.pruningMap * map(image h, M, h);
	--p2 := inverse M2.cache.pruningMap * map(coker h, M, h);
	--B1.cache.homomorphism = f -> map(M1, M1, adjoint'(p1 * f * inverse p1, M1, M1), Degree => first degrees source f + degree f);
	directSummands(M1, opts),
	directSummands(M2, opts))
    ))

-- TODO: if ExtendGroundField is given, change variety
directSummands CoherentSheaf := List => opts -> F -> apply(directSummands(module F, opts), N -> sheaf(-*variety F,*- N))

-- tests whether L (perhaps a line bundle) is a summand of M
-- Recursive => true (default) attempts to peel as many copies of L as possible
-- FIXME: it's not guaranteed to work, e.g. on X_4 over ZZ/2
-- TODO: cache this
directSummands(Module, Module) := List => opts -> (L, M) -> (
    checkRecursionDepth();
    if ring L =!= ring M then error "expected objects over the same ring";
    if rank L  >= rank M then return {M};
    if 1 < #cachedSummands M then return sort flatten apply(cachedSummands M, N -> directSummands(L, N, opts));
    zdeg := degree 0_M;
    -- we look for a composition L -> M -> L which is the identity
    B := smartBasis(zdeg, Hom(L, M, zdeg));
    C := smartBasis(zdeg, Hom(M, L, zdeg));
    -- attempt to find a random isomorphism
    h := for i to opts.Tries - 1 do (
	b := homomorphism(B * random source B);
	c := homomorphism(C * random source C);
	if isIsomorphism(c * b) then break b);
    -- precomputing the Homs can be too memory intensive
    P := if h === null then position'(numcols C, numcols B, (c, b) -> isIsomorphism(homomorphism C_{c} * homomorphism B_{b}));
    if P =!= null then h = homomorphism B_(last P);
    if h === null then return {M};
    if 0 < debugLevel then stderr << concatenate(rank L : ".") << flush;
    -- TODO: can we detect multiple summands of L at once?
    sort flatten join({L}, if not opts.Recursive then {coker h}
	else directSummands(L, coker h, opts)))

directSummands(CoherentSheaf, CoherentSheaf) := List => opts -> (L, G) -> apply(
    directSummands(module L, module G, opts), N -> sheaf(variety L, N))

-- attempt to peel off summands from a given list of modules
directSummands(List, CoherentSheaf) :=
directSummands(List, Module) := List => opts -> (Ls, M) -> fold(Ls, {M},
    (L, LL) -> join(drop(LL, -1), directSummands(L, last LL, opts)))

--
isDefinitelyIndecomposable = method()
isDefinitelyIndecomposable Module := M -> M.cache.?Indecomposable
isDefinitelyIndecomposable CoherentSheaf := M -> isDefinitelyIndecomposable module M

--directSummands Matrix  := List => opts -> f -> apply(directSummands(coker f,  opts), presentation)
-* TODO: not done yet
diagonalize = M -> (
    m := presentation M;
    elapsedTime A := End M; -- most time consuming step
    elapsedTime B := basis(degree 1_(ring M), A);
    h = generalEndomorphism M;
    N0 = image homomorphism B_{idem};
    N = prune N0;
    psi = inverse N.cache.pruningMap; -- map N0 --> N
    phi = map(N0, M, homomorphism B_{idem}); -- map M --> N0
    f = psi * phi; -- map M --> N
    h' = f * h * inverse f;
    source h' == N;
    target h' == N;
    h'
    )
*-

--directSummands Complex := List => opts -> C -> () -- TODO: should be functorial

-----------------------------------------------------------------------------
-- Hom in specific degrees
-----------------------------------------------------------------------------

-- same, but without trim, which seems to be unnecessary if at least M is free!!
Hom(Module, Module) := Module => (M,N) -> (
    Y := youngest(M.cache.cache,N.cache.cache);
    if Y#?(Hom,M,N) then return Y#(Hom,M,N);
    H := -* trim *- kernel (transpose presentation M ** N);
    H.cache.homomorphism = (f) -> map(N,M,adjoint'(f,M,N), Degree => first degrees source f + degree f);
    Y#(Hom,M,N) = H; -- a hack: we really want to type "Hom(M,N) = ..."
    H.cache.formation = FunctionApplication { Hom, (M,N) };
    H)

-- TODO: add DegreeLimit as an option to Hom instead
--Hom = method(Options => { DegreeLimit => null, MinimalGenerators => null })
-- finds submodule of Hom containing at least the homomorphisms of degree e
Hom(Module, Module, ZZ)   := Module => (M, N, e) -> Hom(M, N, if e == 0 then degree 1_(ring M) else {e})
Hom(Module, Module, List) := Module => (M, N, e) -> (
    Y := youngest(M.cache.cache, N.cache.cache);
    if Y#?(Hom, M, N, e) then return Y#(Hom, M, N, e);
    A := presentation M; (G, F) := (target A, source A); -- M <-- G <-- F
    B := presentation N; (L, K) := (target B, source B); -- N <-- L <-- K
    piN := inducedMap(N, L, gens N);
    -- TODO: the trim in Hom(target A, N) used to take 2/3 of total time
    psi := (Hom(A, N) * Hom(G, piN)) // Hom(F, piN);
    p := id_(Hom(G, L)) | map(Hom(G, L), Hom(F, K), 0);
    r := syz(psi | -Hom(F, B), DegreeLimit => e);
    --trim := if opts.MinimalGenerators then trim else identity;
    Y#(Hom, M, N, e) =
    -- TODO: trim used to take 1/3 of total time here
    H := -*trim*- image(Hom(G, piN) * p * r);
    H.cache.homomorphism = f -> map(N, M, adjoint'(f, M, N), Degree => first degrees source f);
    H.cache.formation = FunctionApplication { Hom, (M, N, e) };
    H)

sameVariety := Fs -> if not same apply(Fs, variety) then error "expected coherent sheaves on the same variety"

-- TODO: confirm this; also: can sheafHom be improved?
Hom(CoherentSheaf, CoherentSheaf) := Module => (F, G) -> (
    sameVariety(F, G); HH^0 sheaf(variety F, Hom(module F, module G, 0)))

-----------------------------------------------------------------------------
-* Test section *-
-----------------------------------------------------------------------------

needs "./DirectSummands/tests.m2"

-----------------------------------------------------------------------------
-* Documentation section *-
-----------------------------------------------------------------------------

beginDocumentation()

needs "./DirectSummands/docs.m2"

end--

-----------------------------------------------------------------------------
-* Development section *-
-----------------------------------------------------------------------------

restart
check needsPackage "DirectSummands"

uninstallPackage "DirectSummands"
restart
viewHelp installPackage "DirectSummands"
viewHelp directSummands
viewHelp

end--
restart
debug needsPackage "DirectSummands"


R = kk[x,y,z];
n = 1000
d = {100}
elapsedTime smartBasis(0, Hom(R^n, R^d));
elapsedTime smartBasis(0, Hom(R^n, R^d, 0));


--------
-- summand of 4th syzygy of residue field of ring defined by
-- ideal(y*z,x*z,y^3,x*y^2+z^3,x^2*y,x^3) is indecomposable,
-- but the current method doesn't really show that definitively
