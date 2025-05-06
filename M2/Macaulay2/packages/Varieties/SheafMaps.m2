export {
    -- Types
    "SheafMap",
    -- Methods
    "sheafMap",
--  "isLiftable",
    "yonedaSheafExtension",
--  "yonedaSheafExtension'",
    "cotangentSurjection",
    "eulerSequence",
    "idealSheafSequence",
    "embeddedToAbstract",
    "ExtLongExactSequence",
    }

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- given a graded map, truncate only the source
-- and return the inclusion composed with the map
subtruncate = { MinimalGenerators => false } >> opts -> (degs, f) -> truncate(, degs, f, opts)

-- given a list of sheaf maps with matching targets,
-- truncate only the sources of the corresponding
-- matrices until they all have the same source
autotruncate = { MinimalGenerators => false } >> opts -> L -> (
    deg := max apply(L, f -> f.degree);
    apply(L, f -> subtruncate(deg, f.map, opts)))

-- TODO: confirm that this is the right choice
-- should regularity defined in Complexes do this??
regularity' = M -> regularity flattenModule M

-----------------------------------------------------------------------------
-- SheafHom type declarations and basic constructors
-----------------------------------------------------------------------------

SheafMap = new Type of HashTable
SheafMap.synonym = "morphism of sheaves"

-- Temporarily moved here
importFrom_Core { "isMorphism", "isAbelianCategory" }
isMorphism SheafMap := isAbelianCategory CoherentSheaf := x -> true

-- TODO: if over affine variety, dehomogenize the maps
map(CoherentSheaf, CoherentSheaf, Matrix) := SheafMap => opts -> (G, F, phi) -> (
    if variety G =!= variety F then error "expected sheaves over the same variety";
    if not instance(variety F, ProjectiveVariety) then error "maps of sheaves not yet implemented on other varieties";
    deg := if opts.Degree =!= null then opts.Degree else min flatten degrees source phi;
    phi  = if module G =!= target phi then inducedMap(module G, target phi) * phi else phi;
    new SheafMap from {
	symbol variety => variety F,
        symbol source => F,
        symbol target => G,
        symbol degree => deg,
        symbol map => phi,
        symbol cache => new CacheTable
        }
    )

map(CoherentSheaf, Nothing, Matrix) := SheafMap => opts -> (F, null, psi) -> map(F, sheaf(variety F, source psi), psi, opts)
map(Nothing, CoherentSheaf, Matrix) := SheafMap => opts -> (null, G, psi) -> map(sheaf(variety G, target psi), G, psi, opts)

-- TODO: accept a list of lists instead of matrix

-- when phi is constructed by truncation >= d
map(CoherentSheaf, CoherentSheaf, Matrix, ZZ)             := SheafMap => opts -> (G, F, phi, d) -> map(G, F, phi, Degree => d)
map(CoherentSheaf, CoherentSheaf, Matrix, InfiniteNumber) := SheafMap => opts -> (G, F, phi, d) -> (
    if d === -infinity then map(G, F, phi) else error "unexpected degree for map of sheaves")
-- TODO: support map(F, F, 1) and map(F, G, 0) for identity and zero maps
map(CoherentSheaf, CoherentSheaf, ZZ)                     := SheafMap => opts -> (G, F, n)      -> (
    if n === 0 then sheaf map(module G, module F, 0) else
    if F === G then n * id_G else
    error "expected 0 or source and target equal")
map(CoherentSheaf, CoherentSheaf, SheafMap)               := SheafMap => opts -> (G,F,phi)      -> sheaf map(G,F,matrix phi)
map(CoherentSheaf, Module, ZZ) := SheafMap => opts -> (F, M, n) -> (
    if n === 0 then sheaf map(module F, M, 0) else
    if M === module F then n * id_F else
    error "expected 0 or source and target equal")
map(Module, CoherentSheaf, ZZ) := SheafMap => opts -> (M, F, n) -> (
    if n === 0 then sheaf map(M, module F, 0) else
    if M === module F then n * id_F else
    error "expected 0 or source and target equal")
sheaf SheafMap             := SheafMap =>  phi        -> sheaf matrix phi
sheaf Matrix := Matrix^~   := SheafMap =>  phi        -> sheaf(variety ring phi, phi)
sheaf(Matrix, ZZ)          := SheafMap => (phi, d)    -> sheaf(variety ring phi, phi, d)
sheaf(Variety, Matrix)     := SheafMap => (X, phi)    -> map(sheaf_X target phi, sheaf_X source phi, phi)
sheaf(Variety, Matrix, ZZ) := SheafMap => (X, phi, d) -> map(sheaf_X target phi, sheaf_X source phi,
    truncate(d, phi, MinimalGenerators => false), d)

-- TODO: remove by M2 1.25
sheafMapWarn = true
sheafMap = x -> (if sheafMapWarn then (sheafMapWarn = false; printerr "Note: sheafMap is deprecated; use sheaf instead."); sheaf x)

random(CoherentSheaf, CoherentSheaf) := SheafMap => o -> (F, G) -> map(F, G, random(F.module, G.module, o))

isWellDefined SheafMap := f -> (
    (G, F) := (target f, source f);
    X := variety f;
    d := f.degree;
    all({ G, F, matrix f }, isWellDefined)
    -- data type checks
    and assert'(set keys f === set {
	    symbol variety, symbol source, symbol target,
	    symbol degree,  symbol map,    symbol cache },
	"the hash table does not have the expected keys")
    and assert'(
	instance(f.source, CoherentSheaf) and
	instance(f.target, CoherentSheaf) and
	instance(f.cache, CacheTable) and
	instance(f.map, Matrix) and
	(instance(f.degree, ZZ) or f.degree == infinity),
	"the hash table does not have the expected values")
    -- mathematical checks
    and assert'(ring matrix f === ring X,
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
variety SheafMap := Variety       => f -> f.variety
ring    SheafMap := SheafOfRings  => f -> sheaf f.variety
matrix  SheafMap := Matrix   => o -> f -> f.map
-- TODO: does this make sense, or should all sheaf maps be degree zero?
degree  SheafMap := ZZ            => f -> degree f.map
-- TODO: add a method that returns f.degree

image    SheafMap := CoherentSheaf =>         f -> sheaf(f.variety, image    matrix f)
kernel   SheafMap := CoherentSheaf => opts -> f -> sheaf(f.variety, kernel   matrix f)
coimage  SheafMap := CoherentSheaf =>         f -> sheaf(f.variety, coimage  matrix f)
cokernel SheafMap := CoherentSheaf =>         f -> sheaf(f.variety, cokernel matrix f)

isSurjective SheafMap := Boolean => f -> coker f == 0

-- TODO: is sheaf sufficient here, or should we specify source/target/degree?
cover   SheafMap := SheafMap => f -> sheaf(f.variety, cover   matrix f)
ambient SheafMap := SheafMap => f -> sheaf(f.variety, ambient matrix f)
super   SheafMap := SheafMap => f -> sheaf(f.variety, super   matrix f)

SheafMap == SheafMap := Boolean => (psi, phi) -> psi === phi or (
    target psi == target phi and source psi == source phi and matrix prune psi == matrix prune phi)
    -- TODO: this can fails because the truncated source and target modules
    -- may be only isomorphic. What should we use isIsomorphic in this case?
    -- What do we expect from == and isIsomorphic for sheaf maps?
    -- f := if psi.cache.?minimalPresentation then psi.cache.minimalPresentation.map else psi.map;
    -- g := if phi.cache.?minimalPresentation then phi.cache.minimalPresentation.map else phi.map;
    -- if f == g then return true;
    -- r := 1 + max(
    -- 	regularity' target f, regularity' source f,
    -- 	regularity' target g, regularity' source g);
    -- truncate(r, f, MinimalGenerators => false) == truncate(r, g, MinimalGenerators => false))

SheafMap == ZZ := Boolean => (f, n) -> ( if n === 0 then image f == n else matrix(prune f) == n)
ZZ == SheafMap := Boolean => (n, f) -> f == n

isIsomorphism SheafMap := Boolean => f -> ker f == 0 and coker f == 0

-- TODO: isomorphisms of modules are cached under Y.cache.cache.Isomorphisms,
-- where Y is the youngest of the modules, but currently isomorphisms of sheaves
-- is simply cached under the source. We should fix this.
importFrom_Isomorphism "Isomorphisms"

isIsomorphic(CoherentSheaf, CoherentSheaf) := Boolean => o -> (F, G) -> (
    if F === G then return true;
    if G.cache.?Isomorphisms
    and G.cache.Isomorphisms#?F then return true;
    (M, N) := (module prune F, module prune G);
    if isIsomorphic(M, N, o, Strict => true, Homogeneous => true)
    then (( G.cache.Isomorphisms ??= new MutableHashTable )#F = (M, N); true)
    else false)

-- TODO: perhaps better would be to construct random
-- maps F --> G and check their kernel and cokernel.
isIsomorphic(CoherentSheaf, CoherentSheaf) := Boolean => o -> (F, G) -> F === G or (
    if F === G then return true;
    if G.cache.?Isomorphisms
    and G.cache.Isomorphisms#?F then return true;
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
	r := 1 + max(regularity' F.module, regularity' G.module);
	truncate(r, F.module, MinimalGenerators => false),
	truncate(r, G.module, MinimalGenerators => false));
    -- FIXME: this is incomplete, because we need to store pruning maps or embedding maps
    -- in order to compose/precompose with the cached isomorphism on the modules.
    if isIsomorphic(M, N, o, Strict => true, Homogeneous => true)
    then (( G.cache.Isomorphisms ??= new MutableHashTable )#F = (M, N); true)
    else false)

isomorphism(CoherentSheaf, CoherentSheaf) := SheafMap => o -> (F, G) -> (
    if F === G then id_F else if isIsomorphic(F, G, o,
	Strict => true, Homogeneous => true)
    -- FIXME: this is probably not correct yet, because we may
    -- need to compose/precompose with the pruning maps of F and G.
    then --map(F, G,
	--inverse (prune F).cache.pruningMap *
	sheaf isomorphism splice(G.cache.Isomorphisms#F,
	    Strict => true, Homogeneous => true)
	--* (prune G).cache.pruningMap)
    else error "sheaves are not isomorphic")

isIsomorphic(SheafMap, SheafMap) := Boolean => o -> (psi, phi) -> isIsomorphic(coker phi, coker psi, o)

-- arithmetic ops
- SheafMap := f -> map(target f, source f, -matrix f)
ZZ * SheafMap := RingElement * SheafMap := (r, f) -> map(target f, source f, r * matrix f)
SheafMap * ZZ := SheafMap * RingElement := (f, r) -> r * f
SheafMap + SheafMap := (f, g) -> map(target f, source f, sum autotruncate {f, g})
SheafMap - SheafMap := (f, g) -> f + (-g)

-- composition
SheafMap * SheafMap := SheafMap => (f, g) -> (
    (d, e) := (f.degree, g.degree);
    (m, n) := (matrix f, matrix g);
    if d < e then
    m = truncate(d, m, MinimalGenerators => false);
    n = truncate(d, n, MinimalGenerators => false);
    if d >= e
    then map(target f, source g, m * inducedMap(source m, target n) * n)
    else map(target f, source g, m * inducedMap(source m, target n) * n, d))

-- factoring
-- Note: when f and g are endomorphisms, the sources and targets all agree,
-- so we need quotient and quotient' to distinguish them.
SheafMap // SheafMap := SheafMap => (f, g) -> quotient(f, g)
SheafMap \\ SheafMap := SheafMap => (g, f) -> quotient'(f, g)

-- TODO: should we replace quotient with factor instead?
-- TODO: add assertLevel which causes these assertions to run
quotient(SheafMap, SheafMap) := SheafMap => opts -> (f, g) -> (
    -- given f: A-->C and g: B-->C, then find (f//g): A-->B such that g o (f//g) = f
    --if target f =!= target g then error "quotient: expected sheaf maps with the same target";
    --assert(homomorphism' f % image Hom(source f, g) == 0)
    map(source g, source f, quotient(matrix f, matrix g, opts)))

quotient'(SheafMap, SheafMap) := SheafMap => opts -> (f, g) -> (
    -- given f: A-->C and g: A-->B, then find (g\\f): B-->C such that (g\\f) o g = f
    --if source f != source g then error "quotient': expected sheaf maps with the same source";
    --assert(homomorphism' f % image Hom(g, target f) == 0)
    map(target f, target g, quotient'(autotruncate(f, g), opts)))

-- base change
-- FIXME: this is a kludge; should we define Section as the parent of SheafOfRings?
promote(SheafMap, Nothing)     := SheafMap => (f, O) -> if O === ring f         then f else error "base change of maps of sheaves is not yet implemented"
promote(SheafMap, RingElement) := SheafMap => (f, R) -> if R === ring variety f then f else error "base change of maps of sheaves is not yet implemented"

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
    d := shphi.degree;
    phi := matrix shphi;
    M := module source shphi;
    eta := inducedMap(truncate(e, M, MinimalGenerators => false), source phi);
    sheaf(lift'(phi, eta), e))

--lifts a sheaf map shphi represented by a module map
--phi : M(\geq d) --> N to a map M(\geq e) --> N that represents
--the same morphism of sheaves, for the smallest possible value of e
--WARNING: this method does not actually verify if the lift is possible
lift SheafMap := SheafMap => o -> shphi -> (
    -- TODO: what should happen when shphi is zero?
    if shphi == 0 then return shphi;
    d := shphi.degree;
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
-- Tensors and exterior/symmetric powers
-----------------------------------------------------------------------------
-- TODO: take care of the case when the rings are different
-- FIXME: the source and target sheaves are not correct in this version
tensor(SheafMap, SheafMap) := SheafMap => (phi, psi) -> sheaf(matrix phi ** matrix psi)
-- tensor(SheafMap, SheafMap) := SheafMap => (phi, psi) -> (
--     map(target phi ** target psi,
-- 	source phi ** source psi,
-- 	matrix phi ** matrix psi))

--possible fix for ill-definedness of tensor product
-*tensor(SheafMap, SheafMap) := SheafMap => (phi, psi) -> (
    (F,G,d,e) := (module source phi, module source psi, phi.degree, psi.degree);
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

exteriorPower (ZZ, SheafMap) := SheafMap => o -> (d, phi) -> sheaf(phi.variety,  exteriorPower(d, matrix phi, o))
symmetricPower(ZZ, SheafMap) := SheafMap =>      (d, phi) -> sheaf(phi.variety, symmetricPower(d, matrix phi))

-----------------------------------------------------------------------------
-- inverse
-----------------------------------------------------------------------------
inverse SheafMap := SheafMap => f -> SheafMap.InverseMethod f
SheafMap.InverseMethod = (cacheValue symbol inverse) (f -> (
    X := variety f;
    g := matrix f;
    -- truncate the underlying map so it is an isomorphism
    -- TODO: make this more efficient, e.g. look at degrees of ann coker g
    e := max(regularity' ker g, regularity' coker g);
    -- TODO: this is kludgy, but maybe it works?
    h := try inverse g else inverse truncate(e + 1, g);
    -- then invert and sheafify the new map
    -- We want:
    -- source f ==  target h
    -- target f === source h
    map(sheaf_X source g, sheaf_X source h,
	inducedMap(source g, target h) * h, e + 1))
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
    -- TODO: add DegreeLimit => 0 for efficiency again,
    -- but this causes inverse f to fail in some cases.
    H := prune sheafHom(F', G', opts);
    f := matrix H.cache.pruningMap;
    -- Note: we prune F and G so that f is an isomorphism of modules,
    -- otherwise there may be morphisms in H that do not correspond
    -- to a morphism between the underlying modules of F and G.
    phi := F'.cache.pruningMap;
    psi := G'.cache.pruningMap^-1;
    B := basis(0, module H);
    g := inverse f * B;
    V := part(0, source B);
    V.cache.homomorphism = h -> psi * sheaf(phi.variety, homomorphism(g * h)) * phi;
    V.cache.formation = FunctionApplication { Hom, (F, G) };
    V.cache.Ext = (0, F, G);
    V)

-- Note: homomorphism(Matrix) is defined to use V.cache.homomorphism
-- TODO: target should have Hom info cached
homomorphism' SheafMap := o -> h -> part(0, homomorphism'(matrix h, o))

-----------------------------------------------------------------------------
-- homology
-----------------------------------------------------------------------------
homology(SheafMap, SheafMap) := CoherentSheaf => opts -> (g, f) -> (
    -- Note: these checks prune the image of f and g only
    -- TODO: should we check matrix g == 0 to avoid pruning?
    if g == 0 then return cokernel f;
    if f == 0 then return kernel g;
    g = lift g;
    d := g.degree;
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

cohomology(ZZ,                    SheafMap) := Matrix => opts -> (p,    f) -> cohomology(p, variety f, f, opts)
cohomology(ZZ, ProjectiveVariety, SheafMap) := Matrix => opts -> (p, X, f) -> (
    -- TODO: need to base change to the base field
    if p == 0 then part(0, matrix prune f) else (
	-- pushforward F to a projective space first
	g := flattenMorphism matrix f;
	A := ring g;
	-- TODO: both n and w need to be adjusted for the multigraded case
	n := dim A-1;
	w := A^{-n-1};
	-- using Serre duality for coherent sheaves on schemes with mild
	-- singularities, Cohen–Macaulay schemes, not just smooth schemes.
	-- TODO: check that X is proper (or at least finite type)
	transpose part(0, Ext^(n-p)(g, w)))
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
    -- TODO: confirm that these length limits are correct
    S := ring presentation R;
    P1 := resolution(flattenModule N1, LengthLimit => dim S);
    P2 := resolution(flattenModule N2, LengthLimit => dim S);
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
    part(0, Ext^m(M, matrix f, opts)))

-*
-- internal method for modules only
-- TODO: move this to Complexes?
ExtLES = method(Options => {LengthLimit => null})
ExtLES(Module, Matrix, Matrix) := ComplexMap => opts -> (M, g, f) -> (
    F := freeResolution(M, opts);
    longExactSequence(Hom(F, g), Hom(F, f)))
ExtLES(Matrix, Matrix, Module) := ComplexMap => opts -> (g, f, N) -> (
    (g', f') := horseshoeResolution(g, f, opts);
    G := freeResolution(N, opts);
    -- TODO: the indexing on opts.Concentration needs to be negated
    longExactSequence(Hom(f', G), Hom(g', G)))
*-

--TODO: RHom(ZZ, SheafComplex, SheafComplex)
--TODO: TorLongExactSequence

-- Given f: G -> H, leading to SES 0 -> ker f -> G -> im f -> 0 and F a sheaf,
-- this method returns the long exact sequence in cohomology;
-- the concentration argument will give Ext^(lo) -> ... -> Ext^(hi)
-*
ExtLongExactSequence = method(Options => {Concentration => null})
ExtLongExactSequence(CoherentSheaf, SheafMap)           := Matrix => opts -> (F, f) ->
    ExtLongExactSequence(F, inducedMap(image f, source f, f), inducedMap(source f, ker f), opts)
ExtLongExactSequence(CoherentSheaf, SheafMap, SheafMap) := Matrix => opts -> (F, f, g) -> (
    d := dim variety F;
    e := 0; -- this is a sum of twists bound
    (lo, hi) := if opts.Concentration =!= null then opts.Concentration else (0, d);
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
	l1 := min(dim N1, max(hi, 0)),
	l2 := min(dim N2, max(hi, 0)),
	l3 := min(dim N3, max(hi, 0)));
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
	r := max(a1, a2, a3) - e - hi + 1;
        --need to truncate M in a way related to invariants of ker f
        --probably just add in l3, P3, etc., take max as above
	M = truncate(r, M, MinimalGenerators => false));
    -- TODO: can we truncate at the regularity of homology(f,g) instead?
    reg := 1 + max(regularity' coker matrix f, regularity' ker matrix g);
    -- TODO: verify the Base of the complex
    -- TODO: should lo be used somewhere?
    part_0 ExtLES(M,
	truncate(reg,    matrix f, MinimalGenerators => false),
	subtruncate(reg, matrix g, MinimalGenerators => false),
	LengthLimit => hi + 1))

-- Given f: G -> H, leading to SES 0 -> ker f -> G -> im f -> 0 and F a sheaf,
-- this returns the connecting homomorphism Ext^i(F, im f) -> Ext^(i+1)(F, ker f)
connectingExtMap(ZZ, CoherentSheaf, SheafMap) := Matrix => opts -> (m, F, f0) -> (
    f := inducedMap(image f0, source f0, f0);
    g := inducedMap(source f, ker f);
    h := ExtLongExactSequence(F, f, g,
	Concentration => (m, m + 1));
    h.dd_(-3*m))
*-

-----------------------------------------------------------------------------
-- Yoneda Ext
-----------------------------------------------------------------------------

-*
yonedaSheafExtension = method()
yonedaSheafExtension Matrix := Complex => f -> (
    E := target f; -- Ext^d(F,G)
    (d, F, G) := if (try first formation E) === Ext then last formation E
    else error "expected target of map to be an Ext^d(F,G) module";
    X := variety F;
    r := E.cache.TruncateDegree;
    M := truncate(r, module F, MinimalGenerators => false);
    E' := Ext^d(M, module G);
    f' := basis(0, E') * f;
    C := yonedaExtension f';
    complex apply(d + 1, i -> sheaf_X C.dd_(i+1)))
*-

--yonedaSheafExtension' = method(Options => options Ext.argument)
--yonedaSheafExtension' Complex := Matrix => opts -> C -> ()

///
    C := freeResolution(, LengthLimit => d+1)
    inducedMap(G, sheaf(X, C_d))
    -- K := sheaf(X, image C.dd_d);
    -- i := inducedMap(sheaf(X, C_(d-1)), K);
    -- TODO: need connecting map here
    -- c := map(E, Hom(K, G), ); -- ???
    -- FIXME: cheating here
    b := homomorphism f;
    P := pushout(i, b);
    -- TODO: add this constructor
    i1 := map-- depends on truncate methods
    (module P, C_0, matrix first P.cache.pushoutMaps);
    i2 := map(P, G, matrix last P.cache.pushoutMaps);
    p1 := map(F, P, transpose cover i1 // transpose cover (augmentationMap C)_0);
    (p1, i2))
///

-----------------------------------------------------------------------------
-- Prune
-----------------------------------------------------------------------------

-- Consider the sequence 0 -> m^[p] -> S -> S/m^[p] -> 0 and apply Hom(-,M)
prune SheafMap := minimalPresentation SheafMap := SheafMap => opts -> (cacheValue symbol minimalPresentation) (f -> (
    (G, F) := (target f, source f);
    if f == 0 then return map(prune G, prune F, 0);
    prune G; prune F; -- these are pruned just to populate cached data
    -- F.cache.TorsionFree = M/H^0_B(M)
    g := inducedMap(G.cache.TorsionFree, truncate(f.degree, F.cache.TorsionFree, MinimalGenerators => false), matrix f);
    -- TODO: verify that f.degree is always sufficient here
    p := max(G.cache.GlobalSectionLimit, F.cache.GlobalSectionLimit) + max(0, f.degree);
    -- TODO: substitute with appropriate irrelevant ideal
    Bp := module (ideal vars ring variety F)^[p];
    lift sheaf(f.variety, prune Hom(Bp, g)))
    )

-----------------------------------------------------------------------------
-- Things to move to the Core
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- pullback and pushout and concatenation
-----------------------------------------------------------------------------

SheafMap |  SheafMap := SheafMap => SheafMap.concatCols = maps -> map(
    target maps#0, directSum apply(maps, source), concatCols apply(maps, matrix))
SheafMap || SheafMap := SheafMap => SheafMap.concatRows = maps -> map(
    directSum apply(maps, target), source maps#0, concatRows autotruncate maps)

SheafMap.concatBlocks = maps -> SheafMap.concatRows apply(maps, SheafMap.concatCols)
-- Note: this is (symbol matrix, SheafMap), not (matrix, SheafMap)!
SheafMap.matrix = opts -> SheafMap.concatBlocks

-- TODO: the code for pullback and pushout of matrices is categorical,
-- so we should use e.g. lookup(pullback, Matrix, Matrix) here verbatim

pullback(SheafMap, SheafMap) := CoherentSheaf => {} >> o -> (f, g) -> pullback {f, g}
SheafMap.pullback = args -> (
    -- TODO: use != instead
    if not same apply(args, target) then error "expected morphisms with the same target";
    h := map(target args#0, directSum apply(args, source), concatCols apply(args, matrix));
    P := ker h;
    S := source h;
    P.cache.formation = FunctionApplication (pullback, args);
    P.cache.pullbackMaps = apply(#args, i -> S^[i] * inducedMap(S, P));
    P)

pushout(SheafMap, SheafMap) := CoherentSheaf => (f, g) -> pushout {f, g}
SheafMap.pushout = args -> (
    -- TODO: use != here instead
    if not same apply(args, source) then error "expected morphisms with the same source";
    h := map(directSum apply(args, target), source args#0, concatRows autotruncate args);
    P := coker h;
    T := target h;
    P.cache.formation = FunctionApplication (pushout, args);
    P.cache.pushoutMaps = apply(#args, i -> inducedMap(P, T) * T_[i]);
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
-- Common maps and complexes of sheaves
-----------------------------------------------------------------------------

end--

-- TODO: Beilinson resolution of the diagonal for PP^n

eulerSequence = method()
-- TODO: should return a complex of sheaves
eulerSequence ProjectiveVariety := Complex => X -> (
    -- Given a projective variety X \subset PP^n, returns the two maps
    -- 0 <-- OO_X^1 <-- OO_X^(n+1)(-1) <-- Omega_PP^n|X <-- 0
    complex { sheaf_X vars(S := ring X), sheaf_X inducedMap(source vars S, ker vars S) })

cotangentSurjection = method()
cotangentSurjection ProjectiveVariety := SheafMap => (cacheValue symbol cotangentSurjection) (X -> (
    -- Given a projective variety X \subset PP^n,
    -- returns the surjection Omega_P^n|X -> Omega_X
    C := eulerSequence X;
    OmegaX := cotangentSheaf(X, MinimalGenerators => false);
    OmegaPX := C_2;
    if gens module OmegaX != gens module OmegaPX then error "different generators";
    p := (sheaf inducedMap(coker relations module OmegaX, ambient module OmegaX)) * inducedMap(ambient OmegaPX, OmegaPX);
    inducedMap(image p, source p)))

cotangentSequence = method();
cotangentSequence ProjectiveVariety := Complex => X -> (
    p := cotangentSurjection X;
    i := inducedMap(source p, ker p);
    complex {p, i}
    )

embeddedToAbstract = method()
embeddedToAbstract(ProjectiveVariety) := Matrix => (cacheValue symbol embeddedToAbstract) (X -> (
     g := dual cotangentSurjection X;
     h := inducedMap(coker g, target g);
     connectingExtMap(0, OO_X^1, h, LengthLimit=>2)))

idealSheafSequence = method()
idealSheafSequence ProjectiveVariety := Complex => X -> (
    -- Given a projective variety X \subset PP^n, returns the sequence
    -- 0 -> I_X -> OO_P -> OO_P/I_X -> 0
    IX := idealSheaf(X);
    i := inducedMap(ambient IX, IX);
    p := inducedMap(coker i, ambient IX);
    complex {p, i}
    )

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
assert(shphi.degree ===1)

S = QQ[x_1..x_3];
X = Proj S;
phi = vars S;
psi = (transpose phi)**S^{1:-2}
shphi = sheaf(phi)
assert(matrix entries shphi.map == matrix {{x_1, x_2, x_3}})
shpsi = sheaf(psi)
shphi*shpsi
shphi3 = sheaf(phi,3)
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
