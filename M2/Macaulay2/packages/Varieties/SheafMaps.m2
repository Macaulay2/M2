export {
    -- Types
    "SheafMap",
    -- Methods
    "sheafMap",
    "isLiftable",
    "yonedaSheafExtension",
    }

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- given a graded map, truncate only the source
-- and return the inclusion composed with the map
-- TODO: should this be an option for truncate?
subtruncate = { MinimalGenerators => false } >> opts -> (degs, f) -> (
    f * inducedMap(source f, truncate(degs, source f, opts)))

-- given a list of sheaf maps with matching targets,
-- truncate only the sources of the corresponding
-- matrices until they all have the same source
autotruncate = { MinimalGenerators => false } >> opts -> L -> (
    deg := max apply(L, f -> f.degree);
    apply(L, f -> subtruncate(deg, f.map, opts)))

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
    phi  = if module G =!= target phi then inducedMap(module G, target phi) * phi else phi;
    new SheafMap from {
        symbol source => F,
        symbol target => G,
        symbol degree => deg,
        symbol map => phi,
        symbol cache => new CacheTable
        }
    )

map(CoherentSheaf, Nothing, Matrix) := SheafMap => opts -> (F, null, psi) -> map(F, sheaf(variety F, source psi), psi, opts)
map(Nothing, CoherentSheaf, Matrix) := SheafMap => opts -> (null, G, psi) -> map(sheaf(variety G, target psi), G, psi, opts)

-- when phi is constructed by truncation >= d
map(CoherentSheaf, CoherentSheaf, Matrix, ZZ)             := SheafMap => opts -> (G, F, phi, d) -> map(G, F, phi, Degree => d)
map(CoherentSheaf, CoherentSheaf, Matrix, InfiniteNumber) := SheafMap => opts -> (G, F, phi, d) -> (
    if d === -infinity then map(G, F, phi) else error "unexpected degree for map of sheaves")
-- TODO: support map(F, F, 1) and map(F, G, 0) for identity and zero maps

sheafMap = method()
sheafMap Matrix      := SheafMap =>  phi     -> map(sheaf target phi, sheaf source phi, phi)
sheafMap(Matrix, ZZ) := SheafMap => (phi, d) -> map(sheaf target phi, sheaf source phi,
    truncate(d, phi, MinimalGenerators => false), d)

random(CoherentSheaf, CoherentSheaf) := SheafMap => o -> (F, G) -> map(F, G, random(F.module, G.module, o))

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
    and assert'(try ( isWellDefined map(module G, F' := truncate(d, module F, MinimalGenerators => false),
	 -- TODO: should we use F' here or truncation of source matrix f?
         matrix f * inducedMap(source matrix f, F') ) ) else false,
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

SheafMap == SheafMap := Boolean => (psi, phi) -> psi === phi or (
    target psi == target phi and source psi == source phi and matrix prune psi == matrix prune phi)
    -- TODO: this can fails because the truncated source and target modules
    -- may be only isomorphic. What should we use isIsomorphic in this case?
    -- What do we expect from == and isIsomorphic for sheaf maps?
    -- f := if psi.cache.?minimalPresentation then psi.cache.minimalPresentation.map else psi.map;
    -- g := if phi.cache.?minimalPresentation then phi.cache.minimalPresentation.map else phi.map;
    -- if f == g then return true;
    -- r := 1 + max(
    -- 	regularity target f, regularity source f,
    -- 	regularity target g, regularity source g);
    -- truncate(r, f, MinimalGenerators => false) == truncate(r, g, MinimalGenerators => false))

SheafMap == ZZ := Boolean => (f, n) -> ( if n === 0 then image f == n else matrix(prune f) == n)
ZZ == SheafMap := Boolean => (n, f) -> f == n

isIsomorphism SheafMap := Boolean => f -> ker f == 0 and coker f == 0

isIsomorphic(CoherentSheaf, CoherentSheaf) := Sequence => o -> (F, G) -> (
    M := module prune F;
    N := module prune G;
    -- TODO: isIsomorphic should check === first
    if M === N then return (true, id_F);
    (ret, isom) := isIsomorphic(M, N, o, Strict => true);
    (ret, if ret then map(sheaf M, sheaf N, isom)))

-- TODO: perhaps better would be to construct random
-- maps F --> G and check their kernel and cokernel.
isIsomorphic(CoherentSheaf, CoherentSheaf) := Sequence => o -> (F, G) -> (
    -- Note: sometimes calling isIsomorphic(prune F, prune G) is faster,
    -- but we will leave it to the user to decide if that is the case.
    -- Check if F and G are already pruned or if their minimal presentation is cached
    M := if F.cache.?pruningMap then F.module else try F.cache.minimalPresentation.module;
    N := if G.cache.?pruningMap then G.module else try G.cache.minimalPresentation.module;
    -- Otherwise, we will compare truncated modules representing them, which works in general.
    if M === null or N === null then (M, N) = (
	-- TODO: using regularity in won't suffice in the multigraded case,
	-- and multigradedRegularity may not be optimal. What should methods
	-- that truncate the base module do instead?
	r := 1 + max(regularity F.module, regularity G.module);
	truncate(r, F.module, MinimalGenerators => false),
	truncate(r, G.module, MinimalGenerators => false));
    -- TODO: isIsomorphic should check === first
    if M === N then return (true, id_F);
    (ret, isom) := isIsomorphic(M, N, o, Strict => true);
    (ret, if ret then map(F, G, isom)))

isIsomorphic(SheafMap, SheafMap) := Sequence => o -> (psi, phi) -> isIsomorphic(coker phi, coker psi, o)

-- arithmetic ops
- SheafMap := f -> map(target f, source f, -matrix f)
ZZ * SheafMap := RingElement * SheafMap := (r, f) -> map(target f, source f, r * matrix f)
SheafMap + SheafMap := (f, g) -> map(target f, source f, sum autotruncate {f, g})
SheafMap - SheafMap := (f, g) -> f + (-g)

-- composition
SheafMap * SheafMap := SheafMap => (f, g) -> (
    (d, e) := (degree f, degree g);
    (m, n) := (matrix f, matrix g);
    if d < e then
    m = truncate(d, m, MinimalGenerators => false);
    n = truncate(d, n, MinimalGenerators => false);
    if d >= e
    then map(target f, source g, m * inducedMap(source m, target n) * n)
    else map(target f, source g, m * inducedMap(source m, target n) * n, d))

-- TODO: add factoring of one sheaf map through another

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
    eta := inducedMap(truncate(d, M, MinimalGenerators => false), source phi);
    isLiftable(phi, eta))

--if phi is in the image of Hom(eta,target phi), this code
--computes the actual lift
lift' = method()
lift'(Matrix, Matrix) := Matrix => (phi, eta) -> (
    newPhi := homomorphism' phi;
    homomorphism(newPhi // Hom(eta, target phi, MinimalGenerators => true))
    )

--lifts a sheaf map shphi represented by a module map
--phi : M(\geq d) --> N to a map M(\geq e) --> N that represents
--the same morphism of sheaves, if possible
--WARNING: this method does not actually verify if the lift is possible
lift'(SheafMap,ZZ) := SheafMap => (shphi,e) -> (
    d := degree shphi;
    phi := matrix shphi;
    M := module source shphi;
    eta := inducedMap(truncate(e, M, MinimalGenerators => false), source phi);
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
inducedMap(CoherentSheaf, CoherentSheaf)           := SheafMap => opts -> (G, F) -> inducedMap(G, F, id_(ambient G), opts)
inducedMap(CoherentSheaf, CoherentSheaf, SheafMap) := SheafMap => opts -> (G, F, f) -> map(G, F,
    inducedMap(module G, truncate(f.degree, module F), matrix f, opts))

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
-- FIXME: the source and target sheaves are not correct in this version
tensor(SheafMap, SheafMap) := SheafMap => (phi, psi) -> sheafMap(matrix phi ** matrix psi)
-- tensor(SheafMap, SheafMap) := SheafMap => (phi, psi) -> (
--     map(target phi ** target psi,
-- 	source phi ** source psi,
-- 	matrix phi ** matrix psi))

--possible fix for ill-definedness of tensor product
-*tensor(SheafMap, SheafMap) := SheafMap => (phi, psi) -> (
    (F,G,d,e) := (module source phi, module source psi, degree phi, degree psi);
    map(target phi ** target psi,
	sheaf truncate(d + e, F ** G, MinimalGenerators => false),
	truncate(d + e, matrix phi ** matrix psi), d + e))*-

SheafMap ** SheafMap      := SheafMap => (phi, psi) -> tensor(phi, psi)
SheafMap ** CoherentSheaf := SheafMap => (phi,   F) -> tensor(phi, id_F)
CoherentSheaf ** SheafMap := SheafMap => (F,   phi) -> tensor(id_F, phi)
-- TODO: what do these do?!
SheafMap ** SheafOfRings  := SheafMap => (phi, O) -> phi ** (O^1)
SheafOfRings ** SheafMap  := SheafMap => (O, phi) -> (O^1) ** phi

SheafMap^** ZZ := SheafMap => (f, n) -> BinaryPowerMethod(f, n, tensor,
    f -> id_((sheaf variety f)^1),
    f -> error "SheafMap ^** ZZ: expected non-negative integer")

-- TODO: move to Core
Matrix  ^** ZZ := Matrix   => (f, n) -> BinaryPowerMethod(f, n, tensor,
    f -> id_(module ring f),
    f -> error "Matrix ^** ZZ: expected non-negative integer")

-- twist notation
SheafMap(ZZ) := SheafMap => (phi, d) -> phi ** OO_(variety phi)^1(d)

-- TODO: can we also make the target to be the dual of the original source?
dual SheafMap := SheafMap => options(dual, Matrix) >> o -> f -> map(null, dual target f, dual(matrix f, o))

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
    -- TODO: this is kludgy, but maybe it works?
    h := try inverse g else inverse inducedMap(
	truncate(e#1   + 1, target g, MinimalGenerators => false),
	truncate(max e + 1, source g, MinimalGenerators => false), g);
    -- then invert and sheafify the new map
    -- We want:
    -- source f ==  target h
    -- target f === source h
    map(sheaf source g, sheaf(X, source h), inducedMap(source g, target h) * h, e#1 + 1))
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
    B := basis(0, module H);
    g := inverse f * B;
    V := source moveToField B;
    V.cache.homomorphism = h -> psi * sheafMap homomorphism(g * h) * phi;
    V.cache.formation = FunctionApplication { Hom, (F, G) };
    V.cache.Ext = (0, F, G);
    V)

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
    M := module source f;
    N := module target f;
    X := variety f;
    if variety g =!= X then error "expected sheaf maps on the same variety";
    -- Note: we use =!= to avoid pruning the sheaves
    -- we also don't verify g * f == 0 for the same reason
    if module source g =!= N then error "expected sheaf maps to be composable";
    -- not sure why MinimalGenerators => false was relevant in line below, so we took it out
    -- truncate matrix f to match the degree of the source of g
    f = inducedMap(truncate(d, N, MinimalGenerators => false), M, matrix f);
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

-- Given f: G -> H, leading to SES 0 -> ker f -> G -> im f -> 0 and F a sheaf,
-- this method returns Ext^i(F, im f) -> Ext^(i+1)(F, ker f)
connectingExtMap(ZZ, CoherentSheaf, SheafMap)           := Matrix => opts -> (m, F, f) -> (
    connectingExtMap(m, F, inducedMap(image f, source f, f), inducedMap(source f, ker f), opts))
connectingExtMap(ZZ, CoherentSheaf, SheafMap, SheafMap) := Matrix => opts -> (m, F, f, g) -> (
    e := 0; -- this is a sum of twists bound
    if not instance(variety F, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    if target g =!= source f then error "expected target g = source f";
    M := module F;
    -- 0 <— N1 <— N2 < — N3 <— 0
    N2 := module source f;
    N1 := module target f;
    N3 := module source g;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    l := max(
	l1 := min(dim N1, m),
	l2 := min(dim N2, m),
	l3 := min(dim N3, m));
    P1 := resolution flattenModule N1;
    P2 := resolution flattenModule N2;
    P3 := resolution flattenModule N3;
    p := max(
	p1 := length P1,
	p2 := length P2,
	p3 := length P3);
    n := dim ring P1 - 1;
    -- in the first case the spectral sequence degenerates
    if p >= n-l then (
	-- the "regularity" between n-l and p indices
	a1 := max apply(n - l1 .. p1, j -> (max degrees P1_j)#0 - j);
	a2 := max apply(n - l2 .. p2, j -> (max degrees P2_j)#0 - j);
	a3 := max apply(n - l3 .. p3, j -> (max degrees P3_j)#0 - j);
	r := max(a1, a2, a3) - e - m + 1;
        --need to truncate M in a way related to invariants of ker f
        --probably just add in l3, P3, etc., take max as above
	M = truncate(r, M, MinimalGenerators => false));
    -- TODO: can we truncate at the regularity of homology(f,g) instead?
    reg := 1 + max(regularity coker matrix f, regularity ker matrix g);
    ExtMap := connectingExtMap(M,
	truncate(reg,    matrix f, MinimalGenerators => false),
	subtruncate(reg, matrix g, MinimalGenerators => false),
	LengthLimit => opts.LengthLimit);
    moveToField basis(0, ExtMap_(-m)))

-----------------------------------------------------------------------------
-- Yoneda Ext
-----------------------------------------------------------------------------

yonedaSheafExtension = method()
yonedaSheafExtension Matrix := -* Complex => *- f -> (
    E := target f; -- Ext^d(F,G)
    (d, F, G) := if (try first formation E) === Ext then last formation E
    else error "expected target of map to be an Ext^d(F,G) module";
    X := variety F;
    M := module F;
    r := E.cache.TruncateDegree;
    E' := Ext^d(truncate(r, module F, MinimalGenerators => false), module G);
    f' := basis(0, E') * f;
    C := yonedaExtension f';
    -- TODO: should return a complex of sheaf maps
    -* complex *- apply(d + 1, i -> sheafMap C.dd_(i+1)))

-----------------------------------------------------------------------------
-- Prune
-----------------------------------------------------------------------------

-- Consider the sequence 0 -> m^[p] -> S -> S/m^[p] -> 0 and apply Hom(-,M)
prune SheafMap := minimalPresentation SheafMap := SheafMap => opts -> (cacheValue symbol minimalPresentation) (f -> (
    (G, F) := (target f, source f);
    prune G; prune F; -- these are pruned just to populate cached data
    -- F.cache.TorsionFree = M/H^0_B(M)
    g := inducedMap(G.cache.TorsionFree, truncate(degree f, F.cache.TorsionFree, MinimalGenerators => false), matrix f);
    p := max(G.cache.GlobalSectionLimit, F.cache.GlobalSectionLimit) + max(0,degree f);
    --added degree f in the line above... seems to make prune work in one step
    -- TODO: substitute with appropriate irrelevant ideal
    Bp := module (ideal vars ring F)^[p];
    lift sheafMap prune Hom(Bp, g))
    )

-----------------------------------------------------------------------------
-- Things to move to the Core
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- pullback and pushout and concatenation
-----------------------------------------------------------------------------

SheafMap |  SheafMap := SheafMap => (f, g) -> map(target f, source f ++ source g, concatCols {matrix f, matrix g})
SheafMap || SheafMap := SheafMap => (f, g) -> map(target f ++ target g, source f, concatRows autotruncate {f, g})

-- TODO: also for a list of matrices
pullback(SheafMap, SheafMap) := CoherentSheaf => {} >> o -> (f, g) -> (
    -- TODO: use != instead
    if target f =!= target g then error "expected maps with the same target";
    h := f | -g;
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
    h := f || -g;
    P := coker h;
    T := target h;
    P.cache.pushoutMaps = {
	inducedMap(P, T) * T_[0],
	inducedMap(P, T) * T_[1]};
    P)

trans := (C,v) -> (
    -- TODO: should C_[0] (or C_[0,0], etc.) always work, even if the object isn't a direct sum?
    if 1 == #components C then error "expected a direct sum of coherent sheaves";
    w := toList v;
    if C.cache.?indexComponents then (
	Ci := C.cache.indexComponents;
	apply(w, i -> if Ci#?i then Ci#i else error "expected an index of a component of the direct sum"))
    else try C.cache.components_w then v else error "expected an index of a component of the direct sum")

CoherentSheaf _ Array := SheafMap => (F, v) -> (
    v = trans(F,v);
    G := directSum apply(toList v, j -> F.cache.components#j);
    map(F, G, (cover module F)_v))

CoherentSheaf ^ Array := SheafMap => (F, v) -> (
    v = trans(F,v);
    G := directSum apply(toList v, j -> F.cache.components#j);
    map(G, F, (cover module F)^v))

-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

needs "./tests-maps.m2"

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
