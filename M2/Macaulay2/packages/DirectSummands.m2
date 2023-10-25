---------------------------------------------------------------------------
-- PURPOSE : to compute direct summands of modules and coherent sheaves
--
-- UPDATE HISTORY : created Oct 2023
--
-- TODO :
-- 1. implement over Dmodules and other non-commutative rings
-- 2. try to find all irreducible idempotents from End M using representation theory
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
    AuxiliaryFiles => false,
    DebuggingMode => true
    )

export {
    "directSummands",
    "Indecomposable",
    "ExtendGroundField",
    "findIdempotent",
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
Hom(Module, Module, ZZ)   := Module => (M, N, e) -> Hom(M, N, {e})
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


-----------------------------------------------------------------------------
-* Documentation section *-
-----------------------------------------------------------------------------

beginDocumentation()

///
Node
  Key
    DirectSummands
  Headline
  Description
    Text
    Tree
    Example
    CannedExample
  Acknowledgement
  Contributors
  References
  Caveat
  SeeAlso
  Subnodes
///

doc ///
Node
  Key
    directSummands
  Headline
    direct summands of a module or coherent sheaf
  Usage
    summands M
  Inputs
    M:{Module,CoherentSheaf}
  Outputs
    :List
      containing modules or coherent sheaves which are direct summands of $M$
  Description
    Text
      This function attempts to find the indecomposable summands of a module or coherent sheaf $M$.
    Example
      S = QQ[x,y]
      M = coker matrix{{x,y},{x,x}}
      L = summands M
      assert isIsomorphic(M, directSum L)
  SeeAlso
    findIdempotent
///

-- Template:
///
Node
  Key
  Headline
  Usage
  Inputs
  Outputs
  Consequences
    Item
  Description
    Text
    Example
    CannedExample
    Code
    Pre
  ExampleFiles
  Contributors
  References
  Caveat
  SeeAlso
///

-----------------------------------------------------------------------------
-* Test section *-
-----------------------------------------------------------------------------

TEST /// -- basic test
  S = QQ[x,y]
  M = coker matrix{{1,0},{1,y}}
  A = summands M
  B = summands prune M
  C = summands trim M
  assert same({prune M}, A, B, prune \ C)
///

TEST /// -- direct summands of a free module
  R = ZZ/2[x_0..x_5]
  M = R^{100:-2,100:0,100:2}
  A = summands M;
  B = summands(M, ExtendGroundField => 2);
  C = summands(M, ExtendGroundField => 4);
  D = summands(M, ExtendGroundField => ZZ/101);
  E = summands(M, ExtendGroundField => GF(2,2));
  assert same(M, directSum A)
  assert same(A, B, C, D, E)
///

TEST /// -- direct summands of a multigraded free module
  R = QQ[x,y,z] ** QQ[a,b,c]
  M = R^{{0,0},{0,1},{1,0},{-1,0},{0,-1},{1,-1},{-1,1}}
  assert same(M, directSum summands M)
  --assert first isIsomorphic(directSum elapsedTime summands M, M)
///

TEST /// -- direct summands of a ring
  S = ZZ/3[x,y,z]
  R = ZZ/3[x,y,z,w]/(x^3+y^3+z^3+w^3)
  f = map(R, S)
  M = pushForward(f, module R)
  assert(M == S^{0,-1,-2})
///

TEST /// -- direct summands over field extensions
  debug needsPackage "DirectSummands"
  R = (ZZ/7)[x,y,z]/(x^3+y^3+z^3);
  X = Proj R;
  M = module frobeniusPushforward(OO_X, 1);
  -* is smartBasis useful? yes!
  elapsedTime A = End M; -- ~0.65s
  elapsedTime B = basis({0}, A); -- ~0.23s
  elapsedTime B = smartBasis({0}, A); -- ~0.03s
  *-
  elapsedTime assert({1, 2, 2, 2} == rank \ summands M) -- 2.28s
  elapsedTime assert({1, 2, 2, 2} == rank \ summands(M, ExtendGroundField => GF 7)) -- 2.87s -> 2.05
  elapsedTime assert({1, 2, 2, 2} == rank \ summands(M, ExtendGroundField => GF(7, 3))) -- 3.77s -> 2.6
  elapsedTime assert(toList(7:1)  == rank \ summands(M, ExtendGroundField => GF(7, 2))) -- 2.18s -> 0.47
///

end--

-----------------------------------------------------------------------------
-* Development section *-
-----------------------------------------------------------------------------

restart
debug needsPackage "DirectSummands"
check "DirectSummands"

uninstallPackage "DirectSummands"
restart
installPackage "DirectSummands"
viewHelp "DirectSummands"

end--
restart
needsPackage "DirectSummands"

-- TODO: turn into documentation
-- presentation of the Horrocks-Mumford bundle
restart
needsPackage "DirectSummands"
needsPackage "BGG"
S = ZZ/32003[x_0..x_4];
E = ZZ/32003[e_0..e_4,SkewCommutative=>true];
alphad = map(E^5, E^{-2,-2}, transpose matrix{
	{ e_1*e_4, -e_0*e_2, -e_1*e_3, -e_2*e_4,  e_0*e_3},
	{-e_2*e_3, -e_3*e_4,  e_0*e_4, -e_0*e_1, -e_1*e_2}})
alpha = syz alphad
alphad = beilinson(alphad, S);
alpha = beilinson(alpha, S);
FHM = prune homology(alphad, alpha);
assert(rank FHM == 2)
elapsedTime assert(summands FHM == {FHM}) -- ~30s for End(FHM), ~110s for basis; ~35s in ZZ/2; now down to ~1s



restart
needsPackage "DirectSummands"


--------
-- summand of 4th syzygy of residue field of ring defined by
-- ideal(y*z,x*z,y^3,x*y^2+z^3,x^2*y,x^3) is indecomposable,
-- but the current method doesn't really show that definitively
