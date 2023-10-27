---------------------------------------------------------------------------
-- PURPOSE : to compute direct summands of modules and coherent sheaves
--
-- UPDATE HISTORY : created Oct 2023
--
-- TODO :
-- 1. implement over Dmodules and other non-commutative rings
-- 2. try to find all irreducible idempotents from End M using representation theory
-- 3. cache maps to the summands
---------------------------------------------------------------------------
newPackage(
    "DirectSummands",
    Version => "0.1",
    Date => "25 Oct 2023",
    Headline => "direct summands of modules and coherent sheaves",
    Authors => {
	{ Name => "Devlin Mallory", Email => "malloryd@math.utah.edu", HomePage => "https://math.utah.edu/~malloryd/"},
	{ Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",         HomePage => "https://math.umn.edu/~mahrud/"}
	},
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
    "directSummands",
    "findIdempotent",
    --
    "Indecomposable",
    "ExtendGroundField",
    --
    "frobeniusMap",
    "frobeniusRing",
    "frobeniusPullback",
    "frobeniusPushforward",
    -- aliases
    "summands" => "directSummands",
    "findIdem" => "findIdempotent",
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
position' = (B, C, f) -> for b in B do for c in C do if f(b, c) then return (b, c)

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
    degs := if #deg == 1 then {min(unique degrees M)} else (
	-- FIXME: in the multigraded case sometimes just using basis is faster:
	return basis(deg, M);
        -- in the multigraded case, coneMin and coneComp can be slow
	-- but for sufficiently large modules they are still an improvement
        -- TODO: make them faster
        C := coneFromVData effGenerators ring M;
        --elapsedTime compMin(C, unique degrees M) -- TODO: this is not the right thing
        select(unique degrees M, d -> coneComp(C, d, deg) == symbol <=));
    if degs === {deg} then map(M, , submatrixByDegrees(gens cover M, (, degs)))
    -- M_(positions(degrees M, d -> d == deg))
    -- FIXME: this line forgets the homomorphism information
    --else basis(deg, image map(M, , submatrixByDegrees(gens cover M, (, degs))))) -- TODO: confirm that this is faster
    else basis(deg, M)) -- caching this causes issues!

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- this is a kludge to handle the case when h^2 = ah
reduceScalar = m -> m // scan(unique flatten entries m | {1}, x -> if isConstant x and not zero x then break x)
isIdempotent = h -> reduceScalar(h^2) == reduceScalar h

-- TODO: can we return cached summands from the closest field extension?
-- all cached keys: select(keys M.cache, k -> instance(k, Option) and k#0 === symbol directSummands)
cachedSummands = M -> if M.cache#?(symbol summands => null) then M.cache#(symbol summands => null) else {M}

-- Note: M may need to be extended to a field extensions
-- TODO: when splitting over a field extension, use cached splitting over summands
-- TODO: cache the inclusion maps
-- Strategies:
-- 1 => use expanded hom
directSummands = method(Options => { Verbose => true, Strategy => 1, ExtendGroundField => null })
directSummands Module := List => opts -> (cacheValue (symbol summands => opts.ExtendGroundField)) (M -> sort(
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
    K := coker vars R;
    zdeg := toList(degreeLength R : 0);
    --TODO: make "elapsedTime" contingent on verbosity
    A := if opts.Strategy == 1 then Hom(M, M, zdeg) else End M; -- most time consuming step
    B := smartBasis(zdeg, A);
    -- FIXME: this currently does not find _all_ idempotents
    flag := true; -- whether all homomorphisms are zero mod m;
    idem := position(numcols B, c -> (
	    h := homomorphism B_{c};
	    if h == id_M then false else (
		if flag and K ** h != 0 then flag = false;
		isIdempotent h))
	);
    -- check if M is certifiably indecomposable
    if flag then (
	if 0 < debugLevel then printerr("\t... certified indecomposable!");
	M.cache.Indecomposable = true; return {M} );
    -- TODO: parallelize
    h := if idem =!= null then homomorphism B_{idem} else try findIdempotent M;
    if h === null then {M} else nonzero flatten join(
	-- TODO: restrict End M to each summand and pass it on
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
--directSummands Matrix  := List => opts -> f -> () -- TODO: should be functorial
--directSummands Complex := List => opts -> C -> () -- TODO: should be functorial

-- TODO: export and document
-- given sheaves L and M0, tests whether L is a summand of M0
testSplitting = (L, M0)->(
    B := smartBasis(0, module sheafHom(L, M0));
    b := rank source B;
    C := smartBasis(0, module sheafHom(M0, L));
    c := rank source C;
    isSplitting := (i,j) -> reduceScalar(homomorphism C_{j} * homomorphism B_{i}) == id_(module L);
    l := if (P := position'(0..b-1, 0..c-1, isSplitting)) === null then return else first P;
    sheaf coker homomorphism B_{l}
    )

isDefinitelyIndecomposable = method()
isDefinitelyIndecomposable Module := M -> M.cache.?Indecomposable
isDefinitelyIndecomposable CoherentSheaf := M -> isDefinitelyIndecomposable module M

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

-----------------------------------------------------------------------------
-- Hom in specific degrees
-----------------------------------------------------------------------------
-- TODO: add DegreeLimit as an option to Hom instead

-- Hom(Module, Module) := Module => (M, N) -> (
--     Y := youngest(M.cache.cache, N.cache.cache);
--     if Y#?(Hom, M, N) then return Y#(Hom, M, N);
--     H := trim kernel (transpose presentation M ** N);
--     H.cache.homomorphism = f -> map(N, M, adjoint'(f, M, N), Degree => first degrees source f);
--     H.cache.formation = FunctionApplication { Hom, (M, N) };
--     Y#(Hom, M, N) = H) -- a hack: we really want to type "Hom(M, N) = ..."
-- finds submodule of Hom containing at least the homomorphisms of degree e
Hom(Module, Module, ZZ)   := Module => (M, N, e) -> Hom(M, N, if e == 0 then degree 1_(ring M) else {e})
Hom(Module, Module, List) := Module => (M, N, e) -> (
    Y := youngest(M.cache.cache, N.cache.cache);
    if Y#?(Hom, M, N, e) then return Y#(Hom, M, N, e);
    A := presentation M;
    B := presentation N;
    (G, F, piM) := (target A, source A, inducedMap(M, G, gens M)); -- not used
    (K, H, piN) := (target B, source B, inducedMap(N, K, gens N));
    Psi := (Hom(A, N) * Hom(G, piN)) // Hom(F, piN);
    L := syz(Psi | (-Hom(F, B)), DegreeLimit => e);
    p := map(Hom(G, K), Hom(G, K) ++ Hom(F, H), (id_(Hom(G, K)) | map(Hom(G, K), Hom(F, H), 0)));
    HMN := trim image(Hom(G, piN) * p * L);
    HMN.cache.homomorphism = f -> map(N, M, adjoint'(f, M, N), Degree => first degrees source f);
    Y#(Hom, M, N, e) = HMN; -- a hack: we really want to type "Hom(M, N) = ..."
    HMN.cache.formation = FunctionApplication { Hom, (M, N, e) };
    HMN)

sameVariety := Fs -> if not same apply(Fs, variety) then error "expected coherent sheaves on the same variety"

-- TODO: confirm this; also: can sheafHom be improved?
Hom(CoherentSheaf, CoherentSheaf) := Module => (F, G) -> (
    sameVariety(F, G); HH^0 sheaf(variety F, Hom(module F, module G, 0)))

-----------------------------------------------------------------------------
-* Documentation section *-
-----------------------------------------------------------------------------

beginDocumentation()

needs "./DirectSummands/docs.m2"

-----------------------------------------------------------------------------
-* Test section *-
-----------------------------------------------------------------------------

needs "./DirectSummands/tests.m2"

end--

-----------------------------------------------------------------------------
-* Development section *-
-----------------------------------------------------------------------------

restart
debug needsPackage "DirectSummands"
check "DirectSummands"

uninstallPackage "DirectSummands"
restart
needsPackage "DirectSummands"
installPackage "DirectSummands"
viewHelp DirectSummands

end--
restart
needsPackage "DirectSummands"


--------
-- summand of 4th syzygy of residue field of ring defined by
-- ideal(y*z,x*z,y^3,x*y^2+z^3,x^2*y,x^3) is indecomposable,
-- but the current method doesn't really show that definitively
