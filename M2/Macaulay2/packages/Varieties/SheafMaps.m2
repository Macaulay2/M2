export {
    -- Types
    "SheafMap",
    -- Methods
    "sheafMap",
    "isLiftable",
--    "yonedaSheafExtension",
    }

-----------------------------------------------------------------------------
-- SheafHom type declarations and basic constructors
-----------------------------------------------------------------------------

SheafMap = new Type of HashTable
SheafMap.synonym = "Morphism of Sheaves"

-- TODO: if over affine variety, dehomogenize the maps
map(CoherentSheaf, CoherentSheaf, Matrix) := SheafMap => opts -> (G, F, phi) -> (
    if variety G =!= variety F then error "expected sheaves over the same variety";
    if not instance(variety F, ProjectiveVariety) then error "maps of sheaves not yet implemented on other varieties";
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
-- TODO: support map(F, F, 1) and map(F, G, 0) for identity and zero maps

sheafMap = method()
sheafMap Matrix      := SheafMap =>  phi     -> map(sheaf target phi, sheaf source phi, phi)
sheafMap(Matrix, ZZ) := SheafMap => (phi, d) -> map(sheaf target phi, sheaf source phi, truncate(d, phi), d)

isWellDefined SheafMap := f -> (
    (G, F) := (target f, source f);
    X := variety f;
    d := degree f;
    all({ G, F, matrix f }, isWellDefined)
    -- data type checks
    and assert'(set keys f === set { symbol source, symbol target, symbol degree, symbol map, symbol cache },
	"the hash table does not have the expected keys")
    and assert'(
	instance(f.source, CoherentSheaf) and
	instance(f.target, CoherentSheaf) and
	instance(f.cache, CacheTable) and
	instance(f.map, Matrix) and
	instance(f.degree, ZZ),
	"the hash table does not have the expected values")
    -- mathematical checks
    and assert'(ring f === ring X,
	"underlying matrix and variety do not have the same ring")
    and assert'(same {X, variety F, variety G},
	"underlying variety does not match that of the source and target")
    and assert'(not isProjective X or isHomogeneous matrix f,
	"underlying matrix of a map of coherent sheaf on a projective variety should be homogeneous")
    and assert'(G === sheaf(X, target matrix f),
	"target of the sheaf map does not match the target of the underlying matrix")
    and assert'(F  == sheaf(X, source matrix f),
	"source of the sheaf map does not match the source of the underlying matrix")
    and assert'(d >= min flatten degrees F, -- maybe not strictly necessary
	"expected the degree of the sheaf map to be at least as high as the degrees of the source")
    and assert'(try ( isWellDefined map(module G, truncate(d, module F), matrix f) ) else false,
	"expected the matrix to induce a map between a truncation of the underlying modules")
    )

-- basic methods
source  SheafMap := CoherentSheaf => f -> f.source
target  SheafMap := CoherentSheaf => f -> f.target
variety SheafMap := Variety       => f -> f.source.variety
ring    SheafMap := Ring          => f -> f.map.ring
matrix  SheafMap := Matrix => opts -> f -> f.map
degree  SheafMap := ZZ => f -> f.degree

ker     SheafMap := CoherentSheaf => opts -> phi -> (sheaf ker matrix phi)
image   SheafMap := CoherentSheaf => phi -> (sheaf image matrix phi)
coimage SheafMap := CoherentSheaf => phi -> (sheaf coimage matrix phi)
coker   SheafMap := CoherentSheaf => phi -> (sheaf coker matrix phi)

-- TODO: is sheafMap sufficient here, or should we specify source/target/degree?
cover   SheafMap := SheafMap => f -> sheafMap cover   matrix f
ambient SheafMap := SheafMap => f -> sheafMap ambient matrix f
super   SheafMap := SheafMap => f -> sheafMap super   matrix f

-- TODO: add SheafMap == SheafMap
SheafMap == ZZ := Boolean => (f, z) -> image f == z
ZZ == SheafMap := Boolean => (z, f) -> image f == z

isIsomorphism SheafMap := Boolean => f -> ker f == 0 and coker f == 0

-- composition
SheafMap * SheafMap := SheafMap => (phi, psi) -> (
    d := degree phi; e := degree psi;
    if d >= e
    then map(target phi, source psi, matrix phi * truncate(d, matrix psi))
    else map(target phi, source psi, truncate(d, matrix phi) * truncate(d, matrix psi), d)
    )

-- printing
-- TODO: use abbreviations for source and target
expression SheafMap := Expression => f -> (
    if (s := f.map) == 0 then expression 0
    else MapExpression { target f, source f, s })
net              SheafMap :=      net @@ expression
texMath          SheafMap :=  texMath @@ expression
toString         SheafMap := toString @@ expression
toExternalString SheafMap := toString @@ describe

-----------------------------------------------------------------------------
-- isLiftable, lift
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

--if phi is in the image of Hom(eta,target phi), this code
--computes the actual lift
lift' = method()
lift'(Matrix, Matrix) := Matrix => (phi, eta) -> (
    newPhi := homomorphism' phi;
    homomorphism(newPhi // Hom(eta, target phi))
    )

--lifts a sheaf map shphi represented by a module map
--phi : M(\geq d) --> N to a map M(\geq e) --> N that represents
--the same morphism of sheaves, if possible
--WARNING: this method does not actually verify if the lift is possible
lift'(SheafMap,ZZ) := SheafMap => (shphi,e) -> (
    d := degree shphi;
    phi := matrix shphi;
    M := module source shphi;
    eta := inducedMap(truncate(e,M),source phi);
    sheafMap(lift'(phi, eta), e))

--lifts a sheaf map shphi represented by a module map
--phi : M(\geq d) --> N to a map M(\geq e) --> N that represents
--the same morphism of sheaves, for the smallest possible value of e
--WARNING: this method does not actually verify if the lift is possible
lift SheafMap := SheafMap => o -> shphi -> (
    d := degree shphi;
    M := module source shphi;
    m := min flatten degrees M;
    while isLiftable(shphi,d-1) and d > m do d = d-1;
    lift'(shphi, d))

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

-- FIXME: this fails if the target is truncated
-- TODO: would f -> Hom(f, sheaf variety f) work?
dual  SheafMap := SheafMap => options(dual, Matrix) >> o -> f -> map(dual source f, dual target f, dual(matrix f, o))

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
    -- TODO: this is kudgy, but maybe it works?
    h := try inverse g else inverse inducedMap(
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
sheafHom(SheafMap, SheafMap)      := SheafMap => o -> (phi, psi) -> (dual phi) ** psi
sheafHom(SheafMap, CoherentSheaf) := SheafMap => o -> (phi, F) -> sheafHom(phi, id_F)
sheafHom(CoherentSheaf, SheafMap) := SheafMap => o -> (F, phi) -> sheafHom(id_F, phi)
sheafHom(SheafMap, SheafOfRings)  := SheafMap => o -> (phi, O) -> sheafHom(phi, id_(O^1))
sheafHom(SheafOfRings, SheafMap)  := SheafMap => o -> (O, phi) -> sheafHom(id_(O^1), phi)

-- See [Hartshorne, Ch. III Exercise 6.1, pp. 237]
-- TODO: these three calls could be simpler, but F^1 erases cached info of F
Hom(SheafOfRings, SheafOfRings)  := Module => opts -> (O, O') -> Hom(O^1, O'^1, opts)
Hom(SheafOfRings, CoherentSheaf) := Module => opts -> (O, G)  -> Hom(O^1, G, opts)
Hom(CoherentSheaf, SheafOfRings)  := Module => opts -> (F, O) -> Hom(F, O^1, opts)
Hom(CoherentSheaf, CoherentSheaf) := Module => opts -> (F, G) -> (
    -- The previous version simply returned HH^0(X, sheafHom(F, G, DegreeLimit => 0))
    -- but this version also supports homomorphism.
    -- TODO: either reduce calls to prunes, or keep
    -- the previous method as a faster strategy.
    F' := prune F;
    G' := prune G;
    H := prune sheafHom(F', G', opts, DegreeLimit => 0);
    f := matrix H.cache.pruningMap;
    -- Note: we prune F and G so that f is an isomorphism of modules,
    -- otherwise there may be morphisms in H that do not correspond
    -- to a morphism between the underlying modules of F and G.
    phi := F'.cache.pruningMap;
    psi := G'.cache.pruningMap^-1;
    -- FIXME: there's still a bug here where truncation
    -- strangely flips the rows; see the failing test.
    B := basis(0, module H);
    g := inverse f * B;
    V := source moveToField B;
    V.cache.homomorphism = h -> psi * sheafMap homomorphism(g * h) * phi;
    V.cache.formation = FunctionApplication { Hom, (F, G) };
    V)

TEST ///
  S = QQ[a..d];
  X = Proj S;
  I = ideal(a,b^3*c,b^4);
  F = sheaf module I;
  assert isWellDefined F
  H = Hom(F, OO_X^1/F);
  assert(QQ^15 === H)
  checkHoms = i -> (
      h := homomorphism H_{i};
      assert isWellDefined h;
      assert(source h === F);
      assert(target h === OO_X^1/F);
      -- FIXME: fails due to the basis/truncate bug below
      --assert(homomorphism' h === H_{i});
      -- TODO: remove this when the above works:
      assert(target homomorphism' h === H);
      assert(source homomorphism' h === QQ^1);
      )
  scan(15, checkHoms)
  --
  H = prune sheafHom(F, OO_X^1/F)
  f = H.cache.pruningMap
  g = f^-1
  -- FIXME:
  prune(g * f) === prune id_(target g)
  prune(f * g) === prune id_(target f)
///

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
homomorphism' SheafMap := o -> h -> moveToField basis(0, homomorphism'(matrix h, o))

-----------------------------------------------------------------------------
-- homology
-----------------------------------------------------------------------------
homology(SheafMap, SheafMap) := CoherentSheaf => opts -> (g, f) -> (
    -- Note: these checks prune the image of f and g only
    -- TODO: should we check matrix g == 0 to avoid pruning?
    if g == 0 then return cokernel f;
    if f == 0 then return kernel g;
    g = lift g;
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
--Should prune automatically use the lift command to find the
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
    moveToField basis(0, Ext^m(M, matrix f, opts)))

-----------------------------------------------------------------------------
-- Yoneda Ext
-----------------------------------------------------------------------------

-*
yonedaSheafExtension = method()
-- TODO: should be a complex of sheaf maps
-- FIXME: should get d, F, G from E := target f = Ext^d(F, G)
yonedaSheafExtension(List, Complex, Matrix) := List => (L, C, f) -> (
    -- -f-> -g->
    E := target f; -- Ext^d(F,G)
    (d,F,G) := toSequence L; -- E.cache.Ext;
    X := variety F;
    assert(d == 1); -- TODO
    M := module F;
    --C := freeResolution(M, LengthLimit => d+1);
    K := sheaf(X, image C.dd_1);
    i := inducedMap(sheaf(X, C_0), K);
    -- TODO: need connecting map here
    c := map(E, Hom(K, G), 1);
    -- FIXME: cheating here
    b := homomorphism f;
    P := pushout(i, b);
    -- TODO: add this constructor
    i1 := map(module P, C_0, matrix first P.cache.pushoutMaps);
    i2 := map(P, G, map(module P, module G, matrix last P.cache.pushoutMaps));
    p1 := map(F, P, map(module F, module P, transpose cover i1 // transpose cover (augmentationMap C)_0));
    (p1, i2))
*-

-*
TEST ///
  S = QQ[x,y,z]
  X = Proj S
  d = 1
  F = tangentSheaf X
  G = OO_X^1
  -- TODO: this is what we want
  --E = Ext^1(F, G)
  --f = E_{0}
  -- 0 <-- T_X <-- O_X(1)^3 <-- O_X <-- 0
  --(p, i) = yonedaSheafExtension f
  C = freeResolution(module F, LengthLimit => d+1);
  K = sheaf(X, image C.dd_1);
  H = Hom(K, G)
  -- this is what we have:
  (p, i) = prune \ yonedaSheafExtension({1, F, G}, C, matrix random H)
  assert(source i === G)
  assert(target i === source p)
  assert(target p == F) -- TODO: do we want it to be ===?
  assert(0 == p * i)
  assert(0 == homology(p, i))
  assert(0 == ker i)
  assert(0 == coker p)
///
*-

-----------------------------------------------------------------------------
-- Prune
-----------------------------------------------------------------------------

-- Consider the sequence 0 -> m^[p] -> S -> S/m^[p] -> 0 and apply Hom(-,M)
minimalPresentation SheafMap :=
prune SheafMap := SheafMap => opts -> f -> (
    (G, F) := (target f, source f);
    prune G; prune F; -- these are pruned just to populate cached data
    -- F.cache.TorsionFree = M/H^0_B(M)
    g := inducedMap(G.cache.TorsionFree, truncate(degree f, F.cache.TorsionFree), matrix f);
    p := max(G.cache.GlobalSectionLimit, F.cache.GlobalSectionLimit);
    -- TODO: substitute with appropriate irrelevant ideal
    Bp := module (ideal vars ring F)^[p];
    lift sheafMap prune Hom(Bp, g))

-----------------------------------------------------------------------------
-- Things to move to the Core
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- pullback and pushout
-----------------------------------------------------------------------------

-- TODO: also for a list of matrices
pullback(SheafMap, SheafMap) := CoherentSheaf => {} >> o -> (f, g) -> (
    -- TODO: use != instead
    if target f =!= target g then error "expected maps with the same target";
    h := map(target f, source f ++ source g, matrix f | -matrix g);
    P := ker h;
    S := source h;
    P.cache.pullbackMaps = {
	S^[0] * inducedMap(S, P),
	S^[1] * inducedMap(S, P)};
    P)

-- TODO: also for a list of matrices
pushout(SheafMap, SheafMap) := CoherentSheaf => (f, g) -> (
    -- TODO: use != here instead
    if source matrix f =!= source matrix g then error "expected maps with the same source";
    h := map(target f ++ target g, source f, matrix f || -matrix g);
    P := coker h;
    T := target h;
    P.cache.pushoutMaps = {
	inducedMap(P, T) * T_[0],
	inducedMap(P, T) * T_[1]};
    P)

trans := (C,v) -> (
    if C.cache.?indexComponents then (
	    Ci := C.cache.indexComponents;
	    apply(v, i -> if Ci#?i then Ci#i else error "expected an index of a component of the direct sum"))
    else (
        if not C.cache.?components then error "expected a direct sum of coherent sheaves";
	    Cc := C.cache.components;
	    apply(v, i -> if not Cc#?i then error "expected an index of a component of the direct sum");
	    v)
    )

CoherentSheaf _ Array := SheafMap => (F, v) -> (
    v = trans(F,v);
    G := directSum apply(toList v, j -> F.cache.components#j);
    map(F, G, (cover module F)_v))

CoherentSheaf ^ Array := SheafMap => (F, v) -> (
    v = trans(F,v);
    G := directSum apply(toList v, j -> F.cache.components#j);
    map(G, F, (cover module F)^v))

- SheafMap := phi -> map(target phi, source phi, -matrix phi)

TEST ///
  debug Varieties
  X = Proj(QQ[x,y,z])
  F = OO_X^1 ++ OO_X^1
  assert(prune pullback(F_[0], F_[1]) == 0)
  assert(prune pushout(F^[0], F^[1]) == 0)
///

-- TODO:
-- pullback(SheafMap, SheafMap) := CoherentSheaf => ...
-- pushout(SheafMap, SheafMap) := CoherentSheaf => ...

TEST ///
  debug Varieties
  S = QQ[x,y,z]
  M = S^1 ++ S^1
  assert(prune pullback(M_[0], M_[1]) == 0)
  assert(prune pushout(M^[0], M^[1]) == 0)
///

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
  -- FIXME: the assertion for dual fails!
  shphi3 = map(sheaf target phi,F,phi,3)
  assert isWellDefined shphi3
  --assert isWellDefined dual shphi3
  assert(degree shphi3 === 3)
  assert(degree dual shphi3 === 0)
  -- FIXME: first assertion fails (unless degree 3 is specified)
  shphi4 = map(G, sheaf source phi, phi)
  --assert isWellDefined shphi4
  assert isWellDefined dual shphi4
  assert(degree shphi4 === 3)
  assert(degree dual shphi4 === 0)
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
  assert(inverse h * h === id_F)
  --assert(h * inverse h === id_G) -- FIXME
  assert isIsomorphism h
  assert isIsomorphism inverse h
  assert(prune h === id_G)
  assert(prune inverse h === id_G)
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
  lift shsMap
  F = sheaf comodule intersect(ideal(x_1,x_2), ideal(x_3,x_4))
  pF = prune F
  sMap = F.cache.SaturationMap
  pF.module.cache.pruningMap
  peek F.module.cache
  shsMap = sheafMap(sMap,4)
  lift shsMap
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
  -- assert(homomorphism' h === v) -- FIXME: why is the matrix reversed?
///

TEST ///
  -- testing inverse
  S = QQ[x,y,z]
  X = Proj S
  f = map(OO_X^1, OO_X^1, truncate(2, id_(S^1)), 2)
  assert isWellDefined inverse f
  assert isIsomorphism(f * inverse f)
  assert isIsomorphism(inverse f * f)
  -- assert(prune(inverse f * f) === id_(OO_X^1)) -- FIXME
  assert(prune(f * inverse f) === id_(OO_X^1))
  --
  f = map(sheaf truncate(2, S^1), OO_X^1, truncate(2, id_(S^1)), 2)
  -- assert isWellDefined inverse f -- FIXME
  assert isIsomorphism(f * inverse f)
  assert isIsomorphism(inverse f * f)
  -- assert(prune(inverse f * f) === id_(OO_X^1)) -- FIXME
  assert(prune(f * inverse f) === id_(OO_X^1))
  --
  f = map(OO_X^1, sheaf truncate(2, S^1), truncate(2, id_(S^1)), 2)
  assert isWellDefined inverse f
  assert isIsomorphism(f * inverse f)
  assert isIsomorphism(inverse f * f)
  assert(prune(inverse f * f) === id_(OO_X^1))
  assert(prune(f * inverse f) === id_(OO_X^1))
  --
  f = map(sheaf truncate(2, S^1), sheaf truncate(2, S^1), truncate(2, id_(S^1)), 2)
  assert isWellDefined inverse f
  assert isIsomorphism(f * inverse f)
  assert isIsomorphism(inverse f * f)
  assert(prune(inverse f * f) === id_(OO_X^1))
  assert(prune(f * inverse f) === id_(OO_X^1))
///

-----------------------------------------------------------------------------
-- Documentation
-----------------------------------------------------------------------------

-*
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
*-

-----------------------------------------------------------------------------
-- Development
-----------------------------------------------------------------------------

end--

uninstallPackage "Varieties"
restart
debug needsPackage "Varieties"
check Varieties


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
