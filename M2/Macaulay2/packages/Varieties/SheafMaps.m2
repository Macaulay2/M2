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
    "GlobalSectionLimit"
    }

-----------------------------------------------------------------------------
-- SheafHom type declarations and basic constructors
-----------------------------------------------------------------------------

SheafMap = new Type of HashTable
SheafMap.synonym = "Morphism of Sheaves"

map(CoherentSheaf, CoherentSheaf, Matrix) := SheafMap => opts -> (G, F, phi) -> (
    if variety G =!= variety F then error "Expected sheaves over the same variety";
    if instance(variety F, NormalToricVariety) then error "Maps of sheaves not yet implemented on normal toric varieties";
    deg := if opts.Degree =!= null then opts.Degree else min flatten degrees source phi;
    new SheafMap from {
        symbol source => F,
        symbol target => G,
        symbol degree => deg,
        symbol map => phi,
        symbol cache => new CacheTable
        }
    )

-- TODO: what's the point of this constructor?
map(CoherentSheaf, CoherentSheaf, Matrix, ZZ) := SheafMap => opts -> (G, F, phi, d) -> (
    newPhi := inducedMap(module G, target phi) * phi;
    map(G, F, newPhi, Degree => d))

CoherentSheaf#id = F -> map(F, F, id_(module F))

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

-- TODO: why the name nlift?
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

-- TODO: this needs to be improved: there are some subtleties to discuss
inducedMap(CoherentSheaf, CoherentSheaf) := SheafMap => opts -> (G, F) -> map(G, F, inducedMap(module G, module F))

-----------------------------------------------------------------------------
-- Direct sums and components
-----------------------------------------------------------------------------
--WARNING: the current direct sum of sheaves does not cache components
SheafMap.directSum = args -> (
    assert(#args>0);
    X := variety args#0;
    if not same apply(args, variety) then error "Expected maps of sheaves over the same variety";
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
-- TODO: this tensor command is a little too naive at the moment
tensor(SheafMap, SheafMap) := SheafMap => (phi, psi) -> (
    --m := max(degree phi, degree psi);
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
-- Hom
-----------------------------------------------------------------------------
sheafHom(SheafMap, SheafMap)      := SheafMap => (phi, psi) -> (dual phi) ** psi
sheafHom(SheafMap, CoherentSheaf) := SheafMap => (phi, F) -> sheafHom(phi, id_F)
sheafHom(CoherentSheaf, SheafMap) := SheafMap => (F, phi) -> sheafHom(id_F, phi)
sheafHom(SheafMap, SheafOfRings)  := SheafMap => (phi, O) -> sheafHom(phi, O^1)
sheafHom(SheafOfRings, SheafMap)  := SheafMap => (O, phi) -> sheafHom(O^1, phi)

-----------------------------------------------------------------------------
--
-----------------------------------------------------------------------------
--Some questions:
--Should prune automatically use the nlift command to find the
--simplest possible representative? I think this fits with the intent of prune,
--but maybe the user should be the one to decide whether they want the
--simplest representative

--this is code that Devlin wrote based on some discussions
--that he and I had regarding computing the maps on cohomology
mapOnExt = method()
mapOnExt(ZZ, CoherentSheaf, SheafMap) := (m, F, A) -> (
    e:=0;
    if not instance(variety F, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := module F;
    N1 := module source A;
    N2 := module target A;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    f := presentation R;
    S := ring f;
    n := numgens S -1;
--Is N1 the right one to use in the next lines here, vs N2?
    l := min(dim N1, m);
    P := resolution(cokernel lift(presentation N1,S) ** cokernel f);
    p := length P;
    if p < n-l then(print p; E := Ext^m(truncate(0,M), matrix A))
--truncating M at 0 here seems wrong and gives the wrong results
    else (
	a := max apply(n-l..p,j -> (max degrees P_j)#0-j);
	r := a-e-m+1;
	E = basis(0,Ext^m(truncate(r,M), matrix A))))

-----------------------------------------------------------------------------
-- Prune
-----------------------------------------------------------------------------

-- Consider the sequence 0 -> m^[p] -> S -> S/m^[p] -> 0 and apply Hom(-,M)
minimalPresentation SheafMap :=
prune SheafMap := SheafMap => opts -> f -> (
    (G, F) := (target f, source f);
    prune G; prune F; -- these are pruned just to populate cached data
    -- F.module.cache.TorsionFree = F/H^0_B(F)
    g := inducedMap(G.module.cache.TorsionFree, F.module.cache.TorsionFree, matrix f);
    p := max(G.module.cache.GlobalSectionLimit, F.module.cache.GlobalSectionLimit);
    -- TODO: substitute with appropriate irrelevant ideal
    Bp := module (ideal vars ring F)^[p];
    nlift sheafMap prune Hom(Bp, g))

-----------------------------------------------------------------------------
-- Functions redefined from varieties.m2
-----------------------------------------------------------------------------

sheaf Module := Module ~     := CoherentSheaf =>     M  -> sheaf(Proj ring M, M)
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

-- computes the pushforward via S/I <-- S
flattenModule := M -> (
    f := presentation M;
    g := presentation ring M;
    -- TODO: sometimes lifting to ring g is enough, how can we detect this?
    cokernel lift(f, ring g) ** cokernel g)

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
    -- quotient by H_m^0(M) to kill the torsion
    N := killH0 M;
    -- pushforward to the projective space
    N' := flattenModule N;
    S := ring N';
    -- TODO: both n and w need to be adjusted for the multigraded case
    n := dim S;
    w := S^{-n}; -- canonical sheaf on P^n
    -- Note: bound=infinity signals that H_m^1(M) = 0, ie. M is saturated
    -- in other words, don't search for global sections not already in M
    -- TODO: what would pdim N' < n-1, hence E1 = 0, imply?
    p := if bound === infinity or pdim N' < n-1 then 0 else (
        E1 := Ext^(n-1)(N', w); -- the top Ext
        if dim E1 <= 0 -- 0-module or 0-dim module (i.e. finite length)
        then 1 + max degreeList E1 - min degreeList E1
        else 1 - first min degrees E1 - bound);
    -- this can only happen if bound = -infinity, e.g. from calling H^0(F(>=(-infinity))
    if p === infinity then error "the global sections module is not finitely generated";
    -- caching these to be used for SaturationMap and later in prune SheafMap
    -- TODO: cache in the sheaf instead
    F.module.cache.TorsionFree = N;
    F.module.cache.GlobalSectionLimit = max(0, p);
    -- this is the module Gamma_* F
    G := minimalPresentation if p <= 0 then N else target(
        -- TODO: substitute with appropriate irrelevant ideal here
        Bp := (ideal vars A)^[p];
	-- consider the sequence 0 -> B^[p] -> A -> A/B^[p] -> 0
	inc := inducedMap(module A, module Bp);
	iso := inducedMap(Hom(module A, N), N);
        -- we compute the map N -> Gamma_* F as a limit by
	-- applying Hom(-,N) to the sequence above
        -- c.f. the limit from minimalPresentation hook
        -- and emsbound in NormalToricVarieties/Sheaves.m2
        phi := Hom(inc, N) * iso);
    -- now we compute the center map in the sequence
    -- 0 -> HH^0_B(M) -> M -> Gamma_* F -> HH^1_B(M) -> 0
    iota := inverse G.cache.pruningMap; -- map from Gamma_* F to its minimal presentation
    quot := inducedMap(N, M);           -- map from M to N = M/HH^0_B(M)
    -- TODO: cache in the sheaf instead
    F.module.cache.SaturationMap = if p <= 0 then iota * quot else iota * phi * quot;
    G)

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
minimalPresentation CoherentSheaf := prune CoherentSheaf := opts -> F -> sheaf HH^0 F(>=0)

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
  assert(module prune N == 0) -- TODO: add CoherentSheaf == ZZ?
  -- structure sheaf
  F = sheaf truncate(4,S^1)
  assert(prune F === OO_X^1) -- TODO: add CoherentSheaf == CoherentSheaf?
  -- cotangent bundle
  Omega = sheaf ker vars S;
  pOmega = prune Omega
  sMap = Omega.module.cache.SaturationMap
  assert(source sMap === module Omega)
  assert(target sMap === module pOmega)

  F = sheaf comodule intersect(ideal(x_1,x_2,x_3), ideal(x_3,x_4))
  pF = prune F
  sMap = F.module.cache.SaturationMap
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
  sMap = F.module.cache.SaturationMap
  shsMap = sheafMap sMap
  -- TODO: this fails for Mahrud
  -- FIXME: nlift shsMap
  F = sheaf comodule intersect(ideal(x_1,x_2), ideal(x_3,x_4))
  pF = prune F
  sMap = F.module.cache.SaturationMap
  pF.module.cache.pruningMap
  peek F.module.cache
  -- TODO: fails for Mahrud
  --shsMap = sheafMap(sMap,4)
  --nlift shsMap
///

TEST ///
  --TODO: check if things have already been pruned
  S = QQ[x_1..x_3];
  X = Proj S
  f = sheafMap(truncate(2,vars S))
  prune f

  K = ker vars S
  f = sheafMap dual wedgeProduct(1,1,K)
  -- TODO: this fails for Mahrud
  -- FIXME: prune f
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


cohomology(ZZ, SheafMap) := Matrix => opts -> (p, f) -> (
    if p == 0
    then basis(0, matrix prune f) -- need to base change to the base field
    else error notImplemented())
