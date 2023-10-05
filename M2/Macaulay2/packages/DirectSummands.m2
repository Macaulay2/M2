newPackage(
    "DirectSummands",
    Version => "0.1",
    Date => "",
    Headline => "",
    Authors => {
	{ Name => "Devlin Mallory", Email => "malloryd@math.utah.edu", HomePage => "https://math.utah.edu/~malloryd/"},
	{ Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",         HomePage => "https://math.umn.edu/~mahrud/"}
	},
    PackageImports => {
	"RationalPoints2", -- for rationalPoints in findIdem
	-- "PushForward", -- only for frobenius.m2
	"Polyhedra", -- for coneFromVData and coneComp
	"Truncations", -- for effGenerators
	},
    AuxiliaryFiles => false,
    DebuggingMode => true
    )

export {
    "summands",
    "Indecomposable",
    "ExtendGroundField"
    }

-----------------------------------------------------------------------------
-* Code section *-
-----------------------------------------------------------------------------

--needs "helpers.m2"
--needs "frobenius.m2"
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
    coker sub(presentation M, R))

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

-- Note: M may need to be extended to a field extensions
-- TODO: cache the inclusion maps
-- Strategies:
-- 1 => use expanded hom
summands = method(Options => { Verbose => true, Strategy => 1, ExtendGroundField => null })
summands Module := List => opts -> (cacheValue (symbol summands => opts.ExtendGroundField)) (M -> sort(
    -- Note: rank does weird stuff if R is not a domain
    if 0 < debugLevel then printerr("splitting module of rank: ", toString rank M);
    if opts.ExtendGroundField =!= null then (
	L := opts.ExtendGroundField;
	L  = if instance(L, ZZ)   then GF(char ring M, L)
	else if instance(L, Ring) then L else error "expected an integer or a ground field";
	M = changeBaseField(L, M));
    if 1 < #components M then return flatten apply(components M, summands);
    zdeg := degree 1_(ring M);
    --TODO: make "elapsedTime" contingent on verbosity
    A := if opts.Strategy == 1 then Hom(M, M, zdeg) else End M; -- most time consuming step
    B := smartBasis(zdeg, A);
    K := coker vars ring M;
    -- FIXME: this currently does not find _all_ idempotents
    -- FIXME: why is B_{0} so slow for the Horrocks-Mumford example?!
    flag := true; -- whether all homomorphisms are zero mod m;
    idem := position(numcols B, c -> (
	    h := homomorphism B_{c};
	    if h == id_M then false else (
		if flag and K ** h != 0 then flag = false;
		isIdempotent h))
	);
    -- check if M is certifiably indecomposable
    -- TODO: is homomorphism cached?
    if flag then (
	-- this is the same as checking:
	-- all(numcols B, i -> homomorphism B_{i} == id_M or K ** homomorphism B_{i} == 0)
	if 0 < debugLevel then printerr("\t... certified indecomposable!");
	M.cache.Indecomposable = true; {M} );
    -- TODO: parallelize
    h := if idem =!= null then homomorphism B_{idem} else try findIdem M;
    if h === null then {M} else nonzero flatten join(
        -- TODO: restrict End M to each summand and pass it on
	M1 := prune image h;
	M2 := prune coker h;
	-- Projection maps to the summands
	--p1 := inverse M1.cache.pruningMap * map(image h, M, h);
	--p2 := inverse M2.cache.pruningMap * map(coker h, M, h);
	--B1.cache.homomorphism = f -> map(M1, M1, adjoint'(p1 * f * inverse p1, M1, M1), Degree => first degrees source f + degree f);
	summands(M1, opts),
	summands(M2, opts))
    ))

-- TODO: if ExtendGroundField is given, change variety
summands CoherentSheaf := List => opts -> F -> apply(summands(module F, opts), N -> sheaf(-*variety F,*- N))
--summands Matrix  := List => opts -> f -> () -- TODO: should be functorial
--summands Complex := List => opts -> C -> () -- TODO: should be functorial

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
    summands
  Headline
    summands of a module or coherent sheaf
  Usage
    summands M
  Inputs
    M:{Module,CoherentSheaf}
  Outputs
    :List
      containing modules or coherent sheaves which are summands of $M$
  Description
    Text
      This function attempts to find the indecomposable summands of a module or coherent sheaf $M$.
    Example
      S = QQ[x,y]
      M = coker matrix{{x,y},{x,x}}
      L = summands M
      assert isIsomorphic(M, directSum L)
  SeeAlso
    findIdem
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

TEST /// -* [insert short title for this test] *-
  S = QQ[x,y]
  M = coker matrix{{1,0},{1,y}}
  A = summands M
  B = summands prune M
  C = summands trim M
  assert same({prune M}, A, B, prune \ C)
///

TEST /// -- Direct Summands of a ring
  S = kk[x,y,z]
  R = kk[x,y,z,w]/(x^3+y^3+z^3+w^3)
  f = map(R, S)
  M = pushForward(f, module R)
  assert(M == S^{0,-1,-2})
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

-- basic free module test
R = ZZ/2[x_0..x_5]
M = R^{2:-2,2:0,2:2}
debugLevel=1
elapsedTime summands M
elapsedTime summands(M, ExtendGroundField => 2)
elapsedTime summands(M, ExtendGroundField => 4)

summands(M, ExtendGroundField => ZZ/101)
summands(M, ExtendGroundField => GF(2,2))

-- basic multigraded test
R = kk[x,y,z] ** kk[a,b,c]
M = R^{{0,0},{0,1},{1,0},{-1,0},{0,-1},{1,-1},{-1,1}}
assert isIsomorphic(directSum elapsedTime summands M, M)

-- presentation of the Horrocks-Mumford bundle
restart
needsPackage "DirectSummands"
needsPackage "BGG"
S = ZZ/32003[x_0..x_4];
E = ZZ/32003[e_0..e_4,SkewCommutative=>true];
alphad = matrix{{e_4*e_1, e_2*e_3},{e_0*e_2, e_3*e_4},
                {e_1*e_3, e_4*e_0},{e_2*e_4, e_0*e_1},
                {e_3*e_0, e_1*e_2}};
alphad=map(E^5,E^{-2,-2},alphad)
alpha=syz alphad
alphad=beilinson(alphad,S);
alpha=beilinson(alpha,S);

M = FHM = prune homology(alphad,alpha);
assert(rank FHM == 2)
elapsedTime assert(summands FHM == {FHM}) -- ~30s for End(FHM), ~110s for basis; ~35s in ZZ/2; now down to ~11

-- is smartBasis useful? yes!
restart
needsPackage "DirectSummands"
R = (ZZ/7)[x,y,z]/(x^3+y^3+z^3);
X = Proj R;
M = frobeniusPushforward(OO_X,1);
A = End M;
-- 0.39809 seconds elapsed
elapsedTime basis({3}, A);
-- 0.171702 seconds elapsed
elapsedTime smartBasis({3}, A);

assert({1, 2, 2, 2} == rank \ summands M)
assert({1, 2, 2, 2} == rank \ summands(M, ExtendGroundField => GF 7))
assert({1, 2, 2, 2} == rank \ summands(M, ExtendGroundField => GF(7, 3)))
assert(toList(7:1) == rank \ summands(M, ExtendGroundField => GF(7, 2)))

restart
needsPackage "DirectSummands"


--------
-- summand of 4th syzygy of residue field of ring defined by
-- ideal(y*z,x*z,y^3,x*y^2+z^3,x^2*y,x^3) is indecomposable,
-- but the current method doesn't really show that definitively
