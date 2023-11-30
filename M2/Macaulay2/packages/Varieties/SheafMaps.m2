newPackage(
    "SheafMaps",
    Version => "0.1",
    Date => "September 27, 2023",
    Authors => {
	{Name => "Keller VandeBogert", Email => "kvandebo@nd.edi", HomePage => "todo"}},
    Headline => "methods for working with morphisms of sheaves",
    Keywords => {"Algebraic Geometry", "Homological Algebra"},
    PackageExports => {"Truncations"},
    PackageImports => {"NormalToricVarieties"},
    DebuggingMode => true
    )

export {
    "sheafMap",
    "SheafMap",
    "isLiftable",
    "nlift",
    "SaturationMap",
    "TorsionFree",
    "GlobalSectionLimit",
    "pullback", "pullbackMaps",
    "pushout",  "pushoutMaps",
    }

-----------------------------------------------------------------------------
-- SheafHom type declarations and basic constructors
-----------------------------------------------------------------------------

SheafMap = new Type of HashTable
SheafMap.synonym = "Morphism of Sheaves"

-- TODO: if over affine variety, dehomogenize the maps
map(CoherentSheaf, CoherentSheaf, Matrix) := SheafMap => opts -> (G, F, phi) -> (
    if variety G =!= variety F then error "expected sheaves over the same variety";
    if instance(variety F, NormalToricVariety) then error "maps of sheaves not yet implemented on normal toric varieties";
    deg := if opts.Degree =!= null then opts.Degree else min flatten degrees source phi;
    new SheafMap from {
        symbol source => F,
        symbol target => G,
        symbol degree => deg,
        symbol map => phi,
        symbol cache => new CacheTable
        }
    )

-- when phi is constructed by truncation >= d
map(CoherentSheaf, CoherentSheaf, Matrix, ZZ) := SheafMap => opts -> (G, F, phi, d) -> (
    newPhi := inducedMap(module G, target phi) * phi;
    map(G, F, newPhi, Degree => d))
map(CoherentSheaf, CoherentSheaf, Matrix, InfiniteNumber) := SheafMap => opts -> (G, F, phi, d) -> (
    if d === -infinity then map(G, F, phi) else error "unexpected degree for map of sheaves")

-- TODO
isWellDefined SheafMap := g -> notImplemented()

CoherentSheaf#id = F -> map(F, F, id_(module F))
CoherentSheaf == CoherentSheaf := Boolean => (F, G) -> module prune F == module prune G
-- TODO: actually prune might be too slow, why not compute hilbert polynomial or truncate to compare with zero?
CoherentSheaf == ZZ            := Boolean => (F, z) -> module prune F == z
ZZ            == CoherentSheaf := Boolean => (z, F) -> F == z

sheafMap = method()
sheafMap Matrix      := SheafMap =>  phi     -> map(sheaf target phi, sheaf source phi, phi)
sheafMap(Matrix, ZZ) := SheafMap => (phi, d) -> map(sheaf target phi, sheaf source phi, truncate(d, phi), d)

dual SheafMap := SheafMap => {} >> opts -> phi -> map(dual source phi, dual target phi, dual matrix phi)

-- basic methods
source  SheafMap := CoherentSheaf => f -> f.source
target  SheafMap := CoherentSheaf => f -> f.target
variety SheafMap := ProjectiveVariety => f -> f.source.variety
matrix  SheafMap := Matrix => opts -> f -> f.map
degree  SheafMap := ZZ => f -> f.degree

ker     SheafMap := CoherentSheaf => opts -> phi -> (sheaf ker matrix phi)
image   SheafMap := CoherentSheaf => phi -> (sheaf image matrix phi)
coimage SheafMap := CoherentSheaf => phi -> (sheaf coimage matrix phi)
coker   SheafMap := CoherentSheaf => phi -> (sheaf coker matrix phi)

SheafMap == ZZ := Boolean => (f, z) -> image f == z
ZZ == SheafMap := Boolean => (z, f) -> image f == z

-- composition
SheafMap * SheafMap := SheafMap => (phi, psi) -> (
    d := degree phi; e := degree psi;
    if d >= e
    then map(target phi, source psi, matrix phi * truncate(d, matrix psi))
    else map(target phi, source psi, truncate(d, matrix phi) * truncate(d, matrix psi), d)
    )

-- printing
expression SheafMap := Expression => f -> (
    d := degree f;
    s := f.map;
    if s == 0 then
        new ZeroExpression from {0}
    else new VerticalList from
        RowExpression {MapExpression { target f, source f, s }}
    )
lineOnTop := s -> concatenate(width s : "-") || s
net SheafMap := Net => f -> (
    if f.map == 0 then net "0" else if f.cache.?map
    then stack horizontalJoin (
	net target f, " <--", lineOnTop(net f.cache.map), "-- ", net source f)
    else stack horizontalJoin (
	net target f, " <--", lineOnTop(net f.map), "-- ", net source f)
    )
-- TODO: texMath, toString, toExternalString

-----------------------------------------------------------------------------
-- isLiftable, nlift
-----------------------------------------------------------------------------
--general method: checks whether a map phi is in the image of
--the map Hom(eta,target phi)
isLiftable = method()
isLiftable(Matrix, Matrix) := (phi, eta) -> (
    newPhi := homomorphism' phi;
    deta := Hom(eta, target phi);
    (image newPhi / coker deta) == 0)

--checks whether a sheaf map represented by a map
--phi : M(\geq e) --> N can be factored through
--a smaller truncation of the module M
isLiftable(SheafMap, ZZ) := (shphi, d) -> (
    phi := matrix shphi;
    M := module source shphi;
    eta := inducedMap(truncate(d, M), source phi);
    isLiftable(phi, eta))

-- TODO: change back to lift
--if phi is in the image of Hom(eta,target phi), this code
--computes the actual lift
nlift = method()
nlift(Matrix, Matrix) := Matrix => (phi, eta) -> (
    newPhi := homomorphism' phi;
    homomorphism(newPhi // Hom(eta, target phi))
    )

--lifts a sheaf map shphi represented by a module map
--phi : M(\geq d) --> N to a map M(\geq e) --> N that represents
--the same morphism of sheaves, if possible
--WARNING: this method does not actually verify if the lift is possible
nlift(SheafMap,ZZ) := SheafMap => (shphi,e) -> (
    d := degree shphi;
    phi := matrix shphi;
    M := module source shphi;
    eta := inducedMap(truncate(e,M),source phi);
    sheafMap(nlift(phi,eta),e)
    )

--lifts a sheaf map shphi represented by a module map
--phi : M(\geq d) --> N to a map M(\geq e) --> N that represents
--the same morphism of sheaves, for the smallest possible value of e
--WARNING: this method does not actually verify if the lift is possible
nlift(SheafMap) := SheafMap => shphi -> (
    d := degree shphi;
    M := module source shphi;
    m := min flatten degrees M;
    while isLiftable(shphi,d-1) and d > m do d = d-1;
    nlift(shphi,d)
    )

-*lift(Matrix,Matrix) := Matrix => opts -> (phi,eta) -> (
    newPhi := homomorphism'(phi);
    newPhi//Hom(eta,target phi)
    )*-

-- TODO: this needs to be improved: there are more inducedMap methods to add
inducedMap(CoherentSheaf, CoherentSheaf) := SheafMap => opts -> (G, F) -> map(G, F, inducedMap(module G, module F, opts))
-- TODO: what should this operation be?
-- inducedMap(CoherentSheaf, CoherentSheaf, SheafMap)

-----------------------------------------------------------------------------
-- Direct sums and components
-----------------------------------------------------------------------------
--WARNING: the current direct sum of sheaves does not cache components
SheafMap.directSum = args -> (
    assert(#args>0);
    X := variety args#0;
    if not same apply(args, variety) then error "expected maps of sheaves over the same variety";
    DS := map(
	directSum apply(args, target),
	directSum apply(args, source),
	directSum apply(args, matrix));
    DS.cache.components = toList args;
    DS)

directSum SheafMap := SheafMap => phi -> directSum(1:phi)
SheafMap ++ SheafMap := SheafMap => (phi, psi) -> directSum(phi, psi)

components SheafMap := List => phi -> if phi.cache.?components then phi.cache.components else {phi}

-----------------------------------------------------------------------------
-- Tensors
-----------------------------------------------------------------------------
-- TODO: take care of the case when the rings are different
tensor(SheafMap, SheafMap) := SheafMap => (phi, psi) -> (
    map(target phi ** target psi,
	source phi ** source psi,
	matrix phi ** matrix psi))

SheafMap ** SheafMap      := SheafMap => (phi, psi) -> tensor(phi, psi)
SheafMap ** CoherentSheaf := SheafMap => (phi,   F) -> tensor(phi, id_F)
CoherentSheaf ** SheafMap := SheafMap => (F,   phi) -> tensor(id_F, phi)
-- TODO: what do these do?!
SheafMap ** SheafOfRings  := SheafMap => (phi, O) -> phi ** (O^1)
SheafOfRings ** SheafMap  := SheafMap => (O, phi) -> (O^1) ** phi

-- twist notation
SheafMap(ZZ) := SheafMap => (phi, d) -> phi ** OO_(variety phi)^1(d)

-----------------------------------------------------------------------------
-- inverse
-----------------------------------------------------------------------------
inverse SheafMap := SheafMap => f -> SheafMap.InverseMethod f
SheafMap.InverseMethod = (cacheValue symbol inverse) (f -> (
    X := variety f;
    g := matrix f;
    -- truncate the underlying map so it is an isomorphism
    -- TODO: make this more efficient, e.g. look at degrees of ann coker g
    e := (regularity ker g, regularity coker g);
    h := inverse inducedMap(
	truncate(e#1   + 1, target g),
	truncate(max e + 1, source g), g);
    -- then invert and sheafify the new map
    -- We want:
    -- source f ==  target h
    -- target f === source h
    map(source f, sheaf(X, source h),
	inducedMap(source g, target h) * h, e#1 + 1))
    )

SheafMap#1 = f -> (
    if source f === target f then id_(target f)
    else error "expected source and target to agree")
SheafMap^ZZ := SheafMap => BinaryPowerMethod

-----------------------------------------------------------------------------
-- sheafHom and Hom
-----------------------------------------------------------------------------
sheafHom(SheafMap, SheafMap)      := SheafMap => (phi, psi) -> (dual phi) ** psi
sheafHom(SheafMap, CoherentSheaf) := SheafMap => (phi, F) -> sheafHom(phi, id_F)
sheafHom(CoherentSheaf, SheafMap) := SheafMap => (F, phi) -> sheafHom(id_F, phi)
sheafHom(SheafMap, SheafOfRings)  := SheafMap => (phi, O) -> sheafHom(phi, O^1)
sheafHom(SheafOfRings, SheafMap)  := SheafMap => (O, phi) -> sheafHom(O^1, phi)

-- TODO: bring this from DirectSummands
-- this uses Hom(Module, Module, ZZ) which is faster in a specific degree
--Hom(CoherentSheaf, CoherentSheaf) := Module => (F, G) -> (
--    sameVariety(F, G); HH^0 sheaf(variety F, Hom(module F, module G, 0)))

-- See [Hartshorne, Ch. III Exercise 6.1, pp. 237]
Hom(CoherentSheaf, CoherentSheaf) := Module => (F, G) -> (
    H := prune sheafHom(F, G);
    -- Note: this is only an isomorphism of coherent sheaves,
    -- but we want the preimage of a map of global sections.
    f := matrix H.cache.pruningMap;
    d := regularity coker f + 1;
    -- so we truncate the target until it is surjective
    f  = inducedMap(truncate(d, target f), , f);
    -- FIXME: there's still a bug here where truncation
    -- strangely flips the rows; see the failing test.
    B := inducedMap(target f, , basis(0, module H));
    g := inverse f * B;
    V := source moveToField B;
    V.cache.homomorphism = h -> sheafMap homomorphism(g * h);
    V.cache.formation = FunctionApplication { Hom, (F, G) };
    V)

///
-- here's the core of the bug mentioned above:
restart
needsPackage "Truncations"
S = QQ[x,y,z]
A = S^{1}
B = truncate(0, A)
M = image basis(0, A) -- image {-1} | x y z |
N = image basis(0, B) -- image {-1} | z y x |
inducedMap(M, N) -- anti-digonal
///

-- Note: homomorphism(Matrix) is defined to use V.cache.homomorphism
homomorphism' SheafMap := h -> moveToField basis(0, homomorphism' matrix h)

-----------------------------------------------------------------------------
-- homology
-----------------------------------------------------------------------------
homology(SheafMap, SheafMap) := CoherentSheaf => opts -> (g, f) -> (
    -- Note: these checks prune the image of f and g only
    -- TODO: should we check matrix g == 0 to avoid pruning?
    if g == 0 then return cokernel f;
    if f == 0 then return kernel g;
    g = nlift g;
    d := degree g;
    M := source f;
    N := target f;
    P := target g;
    X := variety f;
    if variety g =!= X then error "expected sheaf maps on the same variety";
    -- Note: we use =!= to avoid pruning the sheaves
    -- we also don't verify g * f == 0 for the same reason
    if source g =!= N then error "expected sheaf maps to be composable";
    -- truncate matrix f to match the degree of the source of g
    f = inducedMap(truncate(d, module N), module M, matrix f);
    sheaf(X, homology(matrix g, f, opts)))

-----------------------------------------------------------------------------
-- Prune
-----------------------------------------------------------------------------

-- TODO: is there a better way to do this?
moveToField = f -> (
    kk := coefficientRing ring f;
    map(kk^(numrows f), kk^(numcols f), sub(cover f, kk)))

-- computes the pushforward via S/I <-- S
flattenMorphism = f -> (
    g := presentation ring f;
    S := ring g;
    -- TODO: sometimes lifting to ring g is enough, how can we detect this?
    -- TODO: why doesn't lift(f, ring g) do this automatically?
    map(target f ** S, source f ** S, lift(cover f, S)) ** cokernel g)

flattenModule = f -> cokernel flattenMorphism presentation f

cohomology(ZZ,                    SheafMap) := Matrix => opts -> (p,    f) -> cohomology(p, variety f, f, opts)
cohomology(ZZ, ProjectiveVariety, SheafMap) := Matrix => opts -> (p, X, f) -> (
    -- TODO: need to base change to the base field
    if p == 0 then moveToField basis(0, matrix prune f) else (
	-- pushforward F to a projective space first
	g := flattenMorphism matrix f;
	A := ring g;
	-- TODO: both n and w need to be adjusted for the multigraded case
	n := dim A-1;
	w := A^{-n-1};
	-- using Serre duality for coherent sheaves on schemes with mild
	-- singularities, Cohen–Macaulay schemes, not just smooth schemes.
	-- TODO: check that X is proper (or at least finite type)
	transpose moveToField basis(0, Ext^(n-p)(g, w)))
    )

--Some questions:
--Should prune automatically use the nlift command to find the
--simplest possible representative? I think this fits with the intent of prune,
--but maybe the user should be the one to decide whether they want the
--simplest representative
--this is code that Devlin wrote based on some discussions
--that he and I had regarding computing the maps on cohomology
Ext(ZZ, CoherentSheaf, SheafMap) := Matrix => opts -> (m, F, f) -> (
    e := 0; -- this is a sum of twists bound
    if not instance(variety F, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := module F;
    N1 := module source f;
    N2 := module target f;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    l := max(
	l1 := min(dim N1, m),
	l2 := min(dim N2, m));
    P1 := resolution flattenModule N1;
    P2 := resolution flattenModule N2;
    p := max(
	p1 := length P1,
	p2 := length P2);
    n := dim ring P1 - 1;
    -- in the first case the spectral sequence degenerates
    if p >= n-l then (
	-- the "regularity" between n-l and p indices
	a1 := max apply(n - l1 .. p1, j -> (max degrees P1_j)#0 - j);
	a2 := max apply(n - l2 .. p2, j -> (max degrees P2_j)#0 - j);
	r := max(a1, a2) - e - m + 1;
	M = truncate(r, M));
    moveToField basis(0, Ext^m(M, matrix f)))

-----------------------------------------------------------------------------
-- Prune
-----------------------------------------------------------------------------

-- Consider the sequence 0 -> m^[p] -> S -> S/m^[p] -> 0 and apply Hom(-,M)
minimalPresentation SheafMap :=
prune SheafMap := SheafMap => opts -> f -> (
    (G, F) := (target f, source f);
    prune G; prune F; -- these are pruned just to populate cached data
    -- F.cache.TorsionFree = M/H^0_B(M)
    g := inducedMap(G.cache.TorsionFree, F.cache.TorsionFree, matrix f);
    p := max(G.cache.GlobalSectionLimit, F.cache.GlobalSectionLimit);
    -- TODO: substitute with appropriate irrelevant ideal
    Bp := module (ideal vars ring F)^[p];
    nlift sheafMap prune Hom(Bp, g))

-----------------------------------------------------------------------------
-- Functions redefined from varieties.m2
-----------------------------------------------------------------------------

sheaf Module := Module ~         := CoherentSheaf =>     M  -> sheaf(Proj ring M, M)
sheaf(ProjectiveVariety, Module) := CoherentSheaf => (X, M) -> (
    if ring M =!= ring X then error "sheaf: expected module and variety to have the same ring";
    if not isHomogeneous M then error "sheaf: expected a homogeneous module";
    if M.cache#?(sheaf, X) then M.cache#(sheaf, X)
    else M.cache#(sheaf, X) = new CoherentSheaf from {
	symbol variety => X,
	symbol module => M,
	symbol cache => new CacheTable
	}
    )

-- TODO: should this also check that the variety is finite type over the field?
checkVariety := (X, F) -> (
    if not X === variety F     then error "expected coherent sheaf over the same variety";
    if not isAffineRing ring X then error "expected a variety defined over a field";
    )

-- TODO: this is called twice
-- TODO: implement for multigraded ring
degreeList := M -> (
    -- gives the exponents of the numerator of reduced Hilbert series of M
     if dim M > 0 then error "expected module of finite length";
     H := poincare M;
     T := (ring H)_0;
     H = H // (1-T)^(numgens ring M);
     exponents H / first)

-- quotienting by local H_m^0(M) to "saturate" M
-- TODO: use irrelevant ideal here
killH0 := -*(cacheValue symbol TorsionFree)*- (M -> if (H0 := saturate(0*M)) == 0 then M else M / H0)

-- TODO: add tests:
-- - global sections of sheafHom are Hom
-- TODO: implement for multigraded ring using emsbound
-- TODO: this can change F.module to the result!
-- NOTE: this may have elements in degrees below bound as well, is that a bug?
twistedGlobalSectionsModule = (F, bound) -> (
    -- compute global sections module Gamma_(d >= bound)(X, F(d))
    A := ring F;
    M := module F;
    if degreeLength A =!= 1 then error "expected degree length 1";
    -- quotient by HH^0_m(M) to kill the torsion
    -- TODO: pass the appropriate irrelevant ideal
    N := killH0 M;
    -- pushforward to the projective space
    N' := flattenModule N;
    S := ring N';
    -- TODO: both n and w need to be adjusted for the multigraded case
    n := dim S-1;
    w := S^{-n-1}; -- canonical sheaf on P^n
    -- Note: bound=infinity signals that HH^1_m(M) = 0, ie. M is saturated
    -- in other words, don't search for global sections not already in M
    -- TODO: what would pdim N' < n, hence E1 = 0, imply?
    p := if bound === infinity or pdim N' < n then 0 else (
        E1 := Ext^n(N', w); -- the top Ext
        if dim E1 <= 0 -- 0-module or 0-dim module (i.e. finite length)
        then 1 + max degreeList E1 - min degreeList E1
        else 1 - first min degrees E1 - bound);
    -- this can only happen if bound=-infinity, e.g. from calling H^0(F(*)) = H^0(F(>=(-infinity))
    if p === infinity then error "the global sections module is not finitely generated";
    -- caching these to be used later in prune SheafMap
    F.cache.TorsionFree = N;
    F.cache.GlobalSectionLimit = max(0, p);
    -- this is the module Gamma_* F
    G := minimalPresentation if p <= 0 then N else target(
        -- TODO: substitute with appropriate irrelevant ideal here
        Bp := (ideal vars A)^[p];
	-- consider the sequence 0 -> B^[p] -> A -> A/B^[p] -> 0
	inc := inducedMap(module A, module Bp);
	iso := inducedMap(Hom(A, N), N);
        -- we compute the map N -> Gamma_* F as a limit by
	-- applying Hom(-,N) to the sequence above
        -- c.f. the limit from minimalPresentation hook
        -- and emsbound in NormalToricVarieties/Sheaves.m2
        phi := Hom(inc, N) * iso);
    -- now we compute the center map in the sequence
    -- 0 -> HH^0_B(M) -> M -> Gamma_* F -> HH^1_B(M) -> 0
    iota := inverse G.cache.pruningMap; -- map from Gamma_* F to its minimal presentation
    quot := inducedMap(N, M);           -- map from M to N = M/HH^0_B(M)
    F.cache.SaturationMap = if p <= 0 then iota * quot else iota * phi * quot;
    G)

variety SumOfTwists := S -> variety S#0

-- HH^p(X, F(>=b))
cohomology(ZZ,                    SumOfTwists) := Module => opts -> (p,    S) -> cohomology(p, variety S, S, opts)
cohomology(ZZ, ProjectiveVariety, SumOfTwists) := Module => opts -> (p, X, S) -> (
    checkVariety(X, S);
    (F, b) := (S#0, S#1#0);
    if not F.cache.?HH    then F.cache.HH = new MutableHashTable;
    if F.cache.HH#?(p, b) then F.cache.HH#(p, b) else F.cache.HH#(p, b) =
    if p == 0 then twistedGlobalSectionsModule(F, b) else HH^(p+1)(module F, Degree => b))

-- This is an approximation of Gamma_* F, at least with an inclusion from Gamma_>=0 F
-- Note: HH^0 F(>=0) is cached above, so this doesn't need caching
minimalPresentation CoherentSheaf := prune CoherentSheaf := opts -> F -> (
    G := sheaf HH^0 F(>=0);
    G.cache.pruningMap = sheafMap F.cache.SaturationMap;
    G)

-----------------------------------------------------------------------------
-- pullback and pushout
-----------------------------------------------------------------------------

-- TODO: also for a list of matrices
pullback(Matrix, Matrix) := Module => (f, g) -> (
    if target f =!= target g then error "expected maps with the same target";
    h := f | -g;
    P := ker h;
    S := source h;
    P.cache.pullbackMaps = {
	map(source f, S, S^[0], Degree => - degree f) * inducedMap(S, P),
	map(source g, S, S^[1], Degree => - degree g) * inducedMap(S, P)};
    P)

-- TODO: also for a list of matrices
pushout = method()
pushout(Matrix, Matrix) := Module => (f, g) -> (
    if source f =!= source g then error "expected maps with the same source";
    h := f || -g;
    P := coker h;
    T := target h;
    P.cache.pushoutMaps = {
	inducedMap(P, T) * map(T, target f, T_[0], Degree => - degree f),
	inducedMap(P, T) * map(T, target g, T_[1], Degree => - degree g)};
    P)

-- TODO:
-- pullback(SheafMap, SheafMap) := CoherentSheaf => ...
-- pushout(SheafMap, SheafMap) := CoherentSheaf => ...

TEST ///
  S = QQ[x,y,z]
  M = S^1 ++ S^1
  assert(prune pullback(M_[0], M_[1]) == 0)
  assert(prune pushout(M^[0], M^[1]) == 0)
///

-----------------------------------------------------------------------------
-- fix for Truncations.m2
-----------------------------------------------------------------------------

truncate(List, Matrix) := Matrix => options(truncate, List, Matrix)  >> opts -> (degs, f) -> (
    F := truncate(degs, source f, opts);
    G := truncate(degs, target f, opts);
    -- FIXME, what is right?
    fgenF := (f * inducedMap(source f, F) * inducedMap(F, source gens F, gens F));
    map(G, F, inducedMap(G, source fgenF, fgenF) // inducedMap(G, source gens G, gens G)))

truncate(InfiniteNumber, Thing) := {} >> o -> (d, M) -> (
    if d === -infinity then M else error "unexpected degree for truncation")

-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

TEST ///
  S = QQ[x_1..x_3];
  X = Proj S;
  phi1 = vars S
  G = sheaf target phi1
  F = sheaf source phi1
  shphi = map(G,F,phi1)
  peek shphi
  assert(source shphi === OO_X^3(-1))
  assert(target shphi === OO_X^1)
  assert(degree shphi ===1)
  phi = truncate(3,phi1);
  shphi2 = map(G,F,phi,3)
  assert(source shphi2 === OO_X^3(-1))
  assert(target shphi2 === OO_X^1)
  assert(degree shphi2 === 3)
///

TEST ///
  S = QQ[x_1..x_3];
  X = Proj S;
  phi = vars S;
  psi = (transpose phi)**S^{1:-2}
  shphi = sheafMap(phi)
  assert(matrix entries shphi.map == matrix {{x_1, x_2, x_3}})
  shpsi = sheafMap(psi)
  shphi*shpsi
  shphi3 = sheafMap(phi,3)
  shphi3*shpsi
///

TEST /// -- tests for cached saturation map M --> Gamma_* sheaf M
  S = QQ[x_1..x_4];
  X = Proj S
  -- zero sheaf
  N = sheaf coker vars S
  assert(N == 0)
  -- structure sheaf
  F = sheaf truncate(4,S^1)
  assert(F == OO_X^1)
  G = prune F
  h = G.cache.pruningMap
  -- TODO: add assertions
  inverse h * h
  h * inverse h
  -- FIXME: prune inverse h fails
  -- TODO: isIsomorphism
  assert(prune h === id_G)
  -- cotangent bundle
  Omega = sheaf ker vars S;
  pOmega = prune Omega
  pMap = pOmega.cache.pruningMap
  assert(prune inverse pMap === id_pOmega)
  assert(source pMap === Omega)
  assert(target pMap === pOmega)
  sMap = Omega.cache.SaturationMap
  assert(source sMap === module Omega)
  assert(target sMap === module pOmega)

  F = sheaf comodule intersect(ideal(x_1,x_2,x_3), ideal(x_3,x_4))
  pF = prune F
  sMap = F.cache.SaturationMap
  assert(source sMap === module F)
  assert(target sMap === module pF)
///

TEST ///
  S = QQ[x_1..x_4];
  -- TODO: add assertions
  F = sheaf comodule intersect(ideal(x_1,x_2), ideal(x_3,x_4))
  pF = prune F
  F = sheaf S^{5} / intersect(ideal(x_1,x_2), ideal(x_3,x_4))
  pF = prune F
  F = sheaf S^{5} / intersect(ideal(x_1,x_2,x_3), ideal(x_3,x_4))
  pF = prune F
  sMap = F.cache.SaturationMap
  shsMap = sheafMap sMap
  nlift shsMap
  F = sheaf comodule intersect(ideal(x_1,x_2), ideal(x_3,x_4))
  pF = prune F
  sMap = F.cache.SaturationMap
  pF.module.cache.pruningMap
  peek F.module.cache
  shsMap = sheafMap(sMap,4)
  nlift shsMap
///

TEST ///
  S = QQ[x_0..x_2];
  X = Proj S
  --TODO: check if things have already been pruned
  f = sheafMap(truncate(2,vars S))
  prune f
  assert all(4, i -> HH^i(f) == Ext^i(OO_X^1, f))
  assert all(4, i -> HH^i(f(1)) == Ext^i(OO_X^1(-1), f))
  
  F = sheaf coker (vars S)_{0,2}
  assert(Ext^2(F, f) == matrix(QQ, {{0, 1, 0}}))
  assert(source Ext^2(F, f) == Ext^2(F, source f))
  assert(target Ext^2(F, f) == Ext^2(F, target f))

  f = sheafMap vars S ** OO_X(1)
  assert(HH^0 f == id_(QQ^3))
  f = sheafMap vars S ** OO_X(-2)
  assert(source HH^2 f == HH^2 source f)
  assert(target HH^2 f == HH^2 target f)
  f = sheafMap vars S ** OO_X(-3)
  assert(source HH^2 f == HH^2 source f)
  assert(target HH^2 f == HH^2 target f)

  kk = ZZ/101
  S = kk[x_0..x_3]
  X = Proj S
  K = dual ker vars S
  f = prune sheafMap dual wedgeProduct(1,1,K)
  --needsPackage "BGG"
  --cohomologyTable(source f, -5,5)
  assert(source HH^2 f == HH^2 source f)
  assert(target HH^2 f == HH^2 target f)
  assert(HH^2 f == matrix(kk, {{2}}))
  assert(HH^3 f == 0)
  
  kk = ZZ/2
  S = kk[x_0..x_2]
  X = Proj S
  K = ker vars S
  f = prune sheafMap wedgeProduct(1,1,K)
  assert(HH^2 f == 1)
  assert(HH^3 f == 0)

  S = QQ[x_0..x_3]
  R = S/ideal(x_0*x_1 - x_2*x_3)
  X = Proj R
  f = sheafMap vars R ** OO_X(2)
  HH^0 f
///

TEST ///
  kk = ZZ/17
  S = kk[x_0..x_3]
  j = inducedMap (ambient ker vars S, ker vars S)
  I = ideal random(3, S)
  R = quotient I; X = Proj R;
  F = sheaf coker (vars R)_{1,3} -- skyscraper sheaf
  phi = jacobian R;
  psi = sheafMap( phi // (j**R))
  OmegaX = coker psi;
  OmegaPX = target psi;
  a = inducedMap(OmegaX, OmegaPX)
  b = dual a
  HH^1(b)
  rank HH^2(psi)
  apply(3, i -> Ext^i(F, psi))
  Ext^2(F, psi)
  HH^2(psi)
///

/// -- TODO: re-enable
  S = kk[x,y,z]
  R = S/ideal(x)
  f = vars R
  f' = flattenMorphism f
  target f' == flattenModule target f
  source f' == flattenModule source f
///

TEST ///
  -- testing homology(SheafMap, SheafMap)
  S = QQ[x,y,z]
  g = sheafMap(koszul_2 vars S, 4)
  g == 0 -- FIXME: this doesn't work with something I did
  f = sheafMap koszul_3 vars S
  assert(0 == prune homology(g, f))

  g = sheafMap koszul_1 vars S
  f = sheafMap koszul_3 vars S * g(-3)
  assert(0 != prune homology(g(-1), f))

  S = QQ[x,y]
  g = sheafMap koszul_1 vars S
  f = sheafMap koszul_2 vars S * g(-2)
  assert(0 == prune homology(g, f))
///

TEST ///
  -- testing SheafMap == ZZ
  S = QQ[x,y]
  f = map(coker matrix{{x^2,y^2}}, , {{x}});
  assert(f != 0)
  g = sheafMap f;
  assert(g == 0)
///

TEST ///
  -- testing homomorphism and homomorphism'
  S = QQ[x,y,z]
  X = Proj S
  --
  F = sheaf module truncate(3, S)
  G = F(1)
  H = Hom(F, G)
  v = random(H, QQ^1)
  h = homomorphism v
  assert(source h === F)
  assert(target h === G)
  assert(homomorphism' h === v)
  assert(sub(last coefficients matrix prune h, QQ) === v)
  --
  F = OO_X^1
  G = sheaf truncate(0, S^{1})
  H = Hom(F, G)
  v = random(H, QQ^1)
  h = homomorphism v
  assert(source h === F)
  assert(target h === G)
  assert(homomorphism' h === v) -- FIXME: why is the matrix reversed?
///

TEST ///
  -- testing inverse
  S = QQ[x,y,z]
  X = Proj S
  f = map(OO_X^1, OO_X^1, truncate(2, id_(S^1)), 2)
  inverse f
///

-----------------------------------------------------------------------------
-- Documentation
-----------------------------------------------------------------------------

beginDocumentation()

doc ///
  Key
    SheafMaps
  Headline
    a package for computing with morphisms of sheaves
  Description
    Text
      ToDo
  Subnodes
    sheafMap
///

-----------------------------------------------------------------------------
-- Development
-----------------------------------------------------------------------------

end--

uninstallPackage "SheafMaps"
restart
debug needsPackage "SheafMaps"
check SheafMaps


S = QQ[x_1..x_3];
X = Proj S;
phi1 = vars S
G = sheaf target phi1
F = sheaf source phi1
shphi = map(G,F,phi1)
peek shphi
assert(source shphi === OO_X^3(-1))
assert(target shphi === OO_X^1)
assert(degree shphi ===1)

S = QQ[x_1..x_3];
X = Proj S;
phi = vars S;
psi = (transpose phi)**S^{1:-2}
shphi = sheafMap(phi)
assert(matrix entries shphi.map == matrix {{x_1, x_2, x_3}})
shpsi = sheafMap(psi)
shphi*shpsi
shphi3 = sheafMap(phi,3)
shphi3*shpsi
phii = matrix oo
eta = inducedMap(truncate(2,source phii),truncate(3,source phii))
shphi**shpsi --this output seems to be a problem: should probably truncate
             --to a common degree, first
sheafHom(shphi,OO_X^3) --this looks like the correct output
-- TODO: this fails for Mahrud
mapOnExt(1,OO_X^1,shphi**OO_X(-3))
hphi = prune mapOnExt(2,OO_X^1,shphi**OO_X(-3)) --I think this is correct actually
target hphi
source hphi
