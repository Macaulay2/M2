---------------------------------------------------------------------------
-- PURPOSE : Computations with coherent sheaves and projective varieties
--
-- MAJOR UPDATES :
--  - 1998: Variety and CoherentSheaf are implemented in m2/varieties.m2.
--  - 2000: SheafOfRings, SumOfTwists are implemented.
--  - 2001: Global Ext is implemented by Greg Smith (see ae0cef36)
--  - 2023: SheafMap is implemented at AIM.
--  - 2024: Varieties is added as a package (see 0f1c1485)
--          Complexes of sheaves are implemented at workshop in Utah
---------------------------------------------------------------------------
newPackage(
    "Varieties",
    Date     => "28 Feb 2025",
    Version  => "0.3",
    Keywords => { "Algebraic Geometry", "Homological Algebra" },
    Headline => "routines for working with affine and projective varieties and coherent sheaves on them",
    Authors  => {
	{   Name => "Devlin Mallory",
	    Email => "malloryd@math.utah.edu",
	    HomePage => "https://www.math.utah.edu/~malloryd/" },
	{   Name => "Ritvik Ramkumar",
	    Email => "ritvikr@cornell.edu",
	    HomePage => "https://sites.google.com/view/ritvikramkumar/" },
	{   Name => "Mahrud Sayrafi",
	    Email => "mahrud@umn.edu",
	    HomePage => "https://math.umn.edu/~mahrud/" },
	{   Name => "Gregory G. Smith",
	    Email => "ggsmith@mast.queensu.ca",
	    HomePage => "https://www.mast.queensu.ca/~ggsmith"},
	{   Name => "Keller VandeBogert",
	    Email => "kvandebo@nd.edu",
	    HomePage => "https://sites.google.com/view/kellervandebogert/home"},
	{   Name => "John Cobb",
	    Email => "jdcobb3@gmail.com",
	    HomePage => "https://johndcobb.github.io"}
	},
    PackageExports => {
	HomologicalAlgebraPackage,
	"Saturation",
	"Truncations",
	"Isomorphism",
	},
    AuxiliaryFiles => true
    )

-- Note: more symbols are exported in Varieties/SheafMaps.m2
export {
    -- Types
    "Variety",
    "AffineVariety",
    "ProjectiveVariety",
    "CoherentSheaf",
    "SheafExpression",
    "SheafOfRings",
    "SumOfTwists",
    "LowerBound",
    -- Methods
    "variety",
    "Proj",
    "Spec",
    "sheaf",
    "sheafExt",
    "sheafHom",
    "tangentSheaf",
    "cotangentSheaf",
    "canonicalBundle",
    "idealSheaf",
    "isProjective",
    "isLocallyFree",
    -- Functors
    "hh", -- TODO: should this be defined in Core?
    "OO",
    "RHom",
    -- Symbols
    "GlobalSectionLimit",
    "SaturationMap",
    "TorsionFree",
    -- "TruncateDegree",
    }

importFrom_Core {
    "getAttribute", "hasAttribute", "ReverseDictionary",
    "applyMethod", "applyMethod''", "functorArgs",
    "toString'", "expressionValue", "unhold", -- TODO: prune these
    "concatBlocks", "concatCols", "concatRows",
    "addHook", "tryHooks", "cacheHooks",
    "isMorphism", "isAbelianCategory",
    "BinaryPowerMethod",
    }

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

checkRing := A -> (
    -- TODO: make this unnecessary
    if not degreeLength A === 1 then error "expected degreeLength of ring to be 1";
    if not same degrees A then error "expected variables all of the same degree";
    )

-- given a list {a,a,b,b,b,c,...} returns a list {{2,a}, {3,b}, {1,c}, ...}
runLengthEncoding := x -> if #x === 0 then x else (
     p := join({0}, select(1 .. #x - 1, i -> x#i =!= x#(i-1)), {#x});
     apply(#p-1, i -> (p#(i+1)-p#i, x#(p#i))))

-- prints the message only if bool is false and debugLevel > 0
-- TODO: eventually turn into a method and move to Core
assert' = (bool, msg) -> bool or ( if debugLevel > 0 then printerr msg; false )

-----------------------------------------------------------------------------
-- Variety, etc. type declarations and basic constructors
-----------------------------------------------------------------------------

Variety = new Type of MutableHashTable
Variety.synonym = "variety"
Variety.GlobalAssignHook = globalAssignFunction
Variety.GlobalReleaseHook = globalReleaseFunction

AffineVariety = new Type of Variety
AffineVariety.synonym = "affine variety"

ProjectiveVariety = new Type of Variety
ProjectiveVariety.synonym = "projective variety"

-- constructors
Spec = method(TypicalValue => AffineVariety)
Spec Ring := (stashValue symbol Spec) (R ->
    R.variety = new AffineVariety from {
	symbol ring => R,
	symbol cache => new CacheTable
	}
    )

Proj = method(TypicalValue => ProjectiveVariety)
Proj Ring := (stashValue symbol Proj) (R ->
    R.variety = new ProjectiveVariety from {
	symbol ring => if isHomogeneous R then R else error "Proj: expected a homogeneous ring",
	symbol cache => new CacheTable
	}
    )

-- TODO: PP(1,2,3) for weighted Proj and PP(V) for vector space V and PP(E) for bundle E?
--PP = new ScriptedFunctor from {
--     superscript => (
--	  i -> R -> (
--	       x := symbol x;
--	       Proj (R[ x_0 .. x_i ])
--	       )
--	  )
--     }

-- this is a kludge to make Spec ZZ/101[x,y]/(y^2-x^3) and Proj ZZ/101[x,y]/(x^2-y^2) work as expected
-- TODO: also make Spec kk{x,y} or Spec kk<|x,y|> work when they are supported
-- TODO: document this in Proj and Spec
    AffineVariety/Thing :=     AffineVariety => (X, I) -> Spec((ring X)/I)
ProjectiveVariety/Thing := ProjectiveVariety => (X, I) -> Proj((ring X)/I) -- TODO: should this be saturated?
    AffineVariety Array :=     AffineVariety => (X, M) -> Spec((ring X) M)
ProjectiveVariety Array := ProjectiveVariety => (X, M) -> Proj((ring X) M)

-- true for standard graded polynomial rings
isStandardGraded = R -> unique degrees R === {{1}}

-- Note: this can be specialized for other types of varieties.
isWellDefined Variety := X -> (
    R := ring X;
    true -- TODO: isWellDefined R
    -- data type checks
    and assert'(isSubset(keys X, { symbol ring, symbol sheaf, symbol cache }),
	"the hash table does not have the expected keys")
    and assert'(
	instance(X.ring, Ring) and
	instance(X.cache, CacheTable),
	"the hash table does not have the expected values")
    -- mathematical checks
    -- TODO: support non-graded rings (e.g. ZZ, QQ, etc.) and nonstandard gradings
    and assert'(not isProjective X or isStandardGraded R,
	"coordinate ring of a projective variety should be homogeneous and generated in degree 1")
    )

-- basic methods
ring  Variety := X -> X.ring
ideal Variety := X -> ideal ring X -- TODO: should this give the irrelevant ideal?
codim Variety := options(codim, QuotientRing) >> o -> X -> codim(ring X, o)

dim     AffineVariety := X -> dim ring X
dim ProjectiveVariety := X -> dim ring X - 1 -- TODO: - Picard rank instead?

char     AffineVariety := X -> char ring X
char ProjectiveVariety := X -> char quotient saturate ideal X -- TODO: saturate with respect to B?

-- TODO: should these be defined, but return 0 for an AffineVariety?
degree ProjectiveVariety := X -> degree ring X
genus  ProjectiveVariety := X -> genus  ring X
genera ProjectiveVariety := X -> genera ring X
-- euler ProjectiveVariety is defined further down
-- TODO: define degrees, eulers
hilbertPolynomial ProjectiveVariety := opts -> X -> hilbertPolynomial(ring X, opts)

ambient     AffineVariety :=     AffineVariety => X -> Spec ambient ring X
ambient ProjectiveVariety := ProjectiveVariety => X -> Proj ambient ring X

-- arithmetic ops
AffineVariety     **     AffineVariety :=     AffineVariety => (X, Y) -> Spec(ring X ** ring Y)
AffineVariety     ** Ring              :=     AffineVariety => (X, R) -> X ** Spec R
-- TODO: uncomment when Proj works with multigraded rings
--ProjectiveVariety ** ProjectiveVariety := ProjectiveVariety => (X, Y) -> Proj(ring X ** ring Y)
--ProjectiveVariety ** Ring              := ProjectiveVariety => (X, R) -> X ** Proj R

-- property checks
isProjective = method(TypicalValue => Boolean)
isProjective Variety           := X -> false
isProjective ProjectiveVariety := X -> true

isSmooth Variety := {} >> o -> X -> 1 == ideal singularLocus X

-- This method returns either a Variety, an AbstractVariety (from Schubert2),
-- a NormalToricVariety, or any other variety stashed in R.variety.
variety = method(TypicalValue => Variety)
variety Ring  := S -> if S.?variety then S.variety else Proj S
variety Ideal := I -> Proj quotient I -- TODO: should this be Spec or Proj?

assertSameVariety = Fs -> if not same apply(Fs, variety) then error "expected objects on the same variety"

-- printing
expression       Variety := X -> if hasAttribute(X, ReverseDictionary) then expression getAttribute(X, ReverseDictionary) else (describe X)#0
-- TODO: are these all necessary?
net              Variety :=      net @@ expression
texMath          Variety :=  texMath @@ expression
toString         Variety := toString @@ expression
toExternalString Variety := toString @@ describe

-- used to be in m2/mathml.m2
mathML Variety := lookup(mathML, Thing)

describe     AffineVariety := X -> Describe (expression Spec) (expression X.ring)
describe ProjectiveVariety := X -> Describe (expression Proj) (expression X.ring)

-----------------------------------------------------------------------------
-- Divisors
-----------------------------------------------------------------------------

-- overriding the dummy methods defined in Truncations package
nefCone       Ring := R -> if R.?variety then nefCone       R.variety
nefGenerators Ring := R -> if R.?variety then nefGenerators R.variety

-- first attempt is to use the variety, then use the degrees of the ring
addHook((effGenerators, Ring), Strategy => Default, R -> if R.?variety then effGenerators R.variety)
addHook((effCone,       Ring), Strategy => Default, R -> if R.?variety then effCone       R.variety)

-- used for algorithms that need a non-trivial Picard group
checkProjective := X -> if not isProjective X then error "expected a coherent sheaf over a projective variety"

-----------------------------------------------------------------------------
-- SheafOfRings and CoherentSheaf type declarations and basic constructors
-----------------------------------------------------------------------------

-- TODO: is this a good idea for fixing type errors?
SheafOfRings = new Type of Ring
SheafOfRings.synonym = "sheaf of rings"

CoherentSheaf = new Type of HashTable
CoherentSheaf.synonym = "coherent sheaf"
CoherentSheaf.GlobalAssignHook = globalAssignFunction
CoherentSheaf.GlobalReleaseHook = globalReleaseFunction

-- see Varieties/SheafMaps.m2
CoherentSheaf#id = F -> map(F, F, id_(module F))
CoherentSheaf#0  = X -> (sheaf X)^0

-- constructors
sheaf = method()
-- TODO: sheaf Ring and sheaf Module should return a sheaf over variety of the ring rather than Proj,
-- and if a variety doesn't already exist then either Proj or Spec should be defined and cached.
sheaf Ring := Ring^~ := SheafOfRings =>     R  -> sheaf(variety R, R)
sheaf Variety        := SheafOfRings =>  X     -> sheaf(X, ring X)
sheaf(Variety, Ring) := SheafOfRings => (X, R) -> (
    if ring X =!= R then error "sheaf: expected ring of the variety";
    -- TODO: simplify when https://github.com/Macaulay2/M2/issues/3351 is fixed
    X.sheaf = X.sheaf ?? new SheafOfRings from { symbol variety => X, symbol ring => R } )

-- TODO: should the module of a sheaf be fixed, or should it be allowed to change?
-- TODO: https://github.com/Macaulay2/M2/issues/1358
sheaf Module := Module^~ := CoherentSheaf =>     M  -> sheaf(variety ring M, M)
sheaf(Variety, Module)   := CoherentSheaf => (X, M) -> (
    if M.cache#?(sheaf, X) then return M.cache#(sheaf, X);
    M.cache#(sheaf, X) = (
	if ring M =!= ring X then error "sheaf: expected module and variety to have the same ring";
	if instance(X, ProjectiveVariety) and not isHomogeneous M then error "sheaf: expected a homogeneous module";
	new CoherentSheaf from {
	    symbol variety => X,
	    symbol module => M,
	    symbol cache => new CacheTable
	    }
	))

-- TODO: consider adding IdealSheaf or SheafOfIdeals type
sheaf Ideal := Ideal^~ := CoherentSheaf =>     I  -> sheaf(variety ring I, module I)
sheaf(Variety, Ideal)  := CoherentSheaf => (X, I) -> sheaf(X,              module I)

-- TODO: remove by M2 1.25
tildeWarn = true
Thing~ := x -> (if tildeWarn then (tildeWarn = false; printerr "Note: M~ is deprecated; use M^~ or sheaf instead."); x^~)

OO = new ScriptedFunctor from {
     subscript => X -> applyMethod((symbol _,     OO, class X), (OO, X)),
     argument  => X -> applyMethod((symbol SPACE, OO, class X), (OO, X)),
     }
OO.texMath = ///{\mathcal O}///
installMethod(symbol_, OO, Variety, SheafOfRings => (OO, X) -> sheaf(X, ring X))

isWellDefined SheafOfRings := O -> O.variety.ring === O.ring and isWellDefined O.variety
isCommutative SheafOfRings := O -> isCommutative O.ring

isWellDefined CoherentSheaf := F -> (
    M := module F;
    X := variety F;
    true -- TODO: isWellDefined M
    and isWellDefined X
    -- data type checks
    and assert'(set keys F === set { symbol variety, symbol module, symbol cache },
	"the hash table does not have the expected keys")
    and assert'(
	instance(F.variety, Variety) and
	instance(F.module, Module)   and
	instance(F.cache, CacheTable),
	"the hash table does not have the expected values")
    -- mathematical checks
    and assert'(ring M === ring X,
	"underlying module and variety do not have the same ring")
    and assert'(not isProjective X or isHomogeneous M,
	"underlying module of coherent sheaf on a projective variety should be homogeneous")
    )

-- basic methods
variety SheafOfRings  :=
variety CoherentSheaf := F -> F.variety

ring SheafOfRings  :=
ring CoherentSheaf := SheafOfRings => F -> sheaf variety F

module SheafOfRings  := Module => F -> module F.ring
module CoherentSheaf := Module => F -> F.module

codim   CoherentSheaf := options(codim, Module) >> o -> F -> codim(F.module, o)
rank    CoherentSheaf := F -> rank    F.module
numgens CoherentSheaf := F -> numgens F.module
betti   CoherentSheaf := o -> F -> betti(F.module, o)

super   CoherentSheaf := CoherentSheaf => F -> sheaf(F.variety, super   F.module)
ambient CoherentSheaf := CoherentSheaf => F -> sheaf(F.variety, ambient F.module)
cover   CoherentSheaf := CoherentSheaf => F -> sheaf(F.variety, cover   F.module)

-- TODO: do all need to be hookified? Perhaps prefixing
-- the variety to the key, like 'euler(X, F)', would be better.
degree  CoherentSheaf := F -> degree  module F
degrees CoherentSheaf := F -> degrees module F
euler   CoherentSheaf := F -> tryHooks((euler, CoherentSheaf), F, euler @@ module)
eulers  CoherentSheaf := F -> eulers  module F
genus   CoherentSheaf := F -> genus   module F
genera  CoherentSheaf := F -> genera  module F
-- TODO: this is incorrect in higher picard rank
pdim    CoherentSheaf := F -> tryHooks((pdim,  CoherentSheaf), F, pdim  @@ module)

hilbertPolynomial CoherentSheaf := opts -> F -> hilbertPolynomial(module F, opts)

-- twist and powers
-- TODO: sheaf should dehomogenize modules on Affine varieties
SheafOfRings(ZZ)   := SheafOfRings  Sequence := CoherentSheaf => (O, a) -> O^1(a)
CoherentSheaf(ZZ)  := CoherentSheaf Sequence := CoherentSheaf => (F, a) -> F ** (ring F)^{splice{a}}
SheafOfRings  ^ ZZ := SheafOfRings  ^ List   := CoherentSheaf => (O, n) -> sheaf(O.variety, (ring variety O)^n)
CoherentSheaf ^ ZZ := CoherentSheaf ^ List   := CoherentSheaf => (F, n) -> sheaf(F.variety, F.module^n)
dual CoherentSheaf := CoherentSheaf => options(dual, Module) >> o -> F -> sheaf(F.variety, dual(F.module, o))

-- There are several equivalent conditions for equality:
-- 1. Saturation of the underlying modules is the same (i.e. Gamma_* F == Gamma_* G)
-- 2. Truncation of the underlying modules is the same
-- Here we use the first, but start with comparing Hilbert polynomials, which may be faster,
-- TODO: benchmark different strategies
CoherentSheaf == CoherentSheaf := Boolean => (F, G) -> hilbertPolynomial F === hilbertPolynomial G and module prune F == module prune G
CoherentSheaf == ZZ            := Boolean => (F, z) -> if z == 0 then dim module F <= 0 else error "attempted to compare sheaf to nonzero integer"
CoherentSheaf == Module        := Boolean => (F, M) -> F == sheaf M
Module        == CoherentSheaf := Boolean => (M, F) -> sheaf M == F
ZZ            == CoherentSheaf := Boolean => (z, F) -> F == z
-- isIsomorphic is defined in SheafMaps.m2 because we return the isomorphism as well

-- use for sorting a list
CoherentSheaf ? CoherentSheaf := lookup(symbol ?, Module, Module)

-- arithmetic ops
CoherentSheaf.directSum = args -> (
    assertSameVariety args;
    F := sheaf(variety args#0, directSum apply(args, module));
    F.cache.components = toList args;
    F)
CoherentSheaf ++ CoherentSheaf := CoherentSheaf => (F, G) -> CoherentSheaf.directSum(F, G)
CoherentSheaf ** CoherentSheaf := CoherentSheaf => (F, G) -> sheaf(F.variety, F.module ** G.module)
CoherentSheaf^** ZZ            := CoherentSheaf => (F, n) -> sheaf(F.variety, F.module ^** n)
tensor(CoherentSheaf, CoherentSheaf) := CoherentSheaf => {} >> opts -> (F, G) -> sheaf(F.variety, tensor(F.module, G.module, opts))
CoherentSheaf  / CoherentSheaf := CoherentSheaf => (F, G) -> sheaf(F.variety, F.module  / G.module)
CoherentSheaf  / Ideal         := CoherentSheaf => (F, I) -> sheaf(F.variety, F.module  / I)
Ideal * CoherentSheaf          := CoherentSheaf => (I, F) -> sheaf(F.variety, I * F.module)
directSum CoherentSheaf        := CoherentSheaf =>  F     -> CoherentSheaf.directSum(1 : F)

components CoherentSheaf := List => (cacheValue symbol components) (F -> apply(components module F, N -> sheaf(F.variety, N)))

-*
component(CoherentSheaf, Thing) := (F, k) -> (
    if not F.cache.?indexComponents then error "expected Sheaf to be a direct sum with indexed components";
    if not F.cache.indexComponents#?k then error("expected "|toString k|" to be the index of a component");
    (components F)#(F.cache.indexComponents#k))
*-

-- multilinear ops
-- TODO: document
determinant        CoherentSheaf  := CoherentSheaf => o ->     F  -> exteriorPower(rank F, F, o)
exteriorPower (ZZ, CoherentSheaf) := CoherentSheaf => o -> (i, F) -> sheaf(F.variety,  exteriorPower(i, F.module, o))
symmetricPower(ZZ, CoherentSheaf) := CoherentSheaf =>      (i, F) -> sheaf(F.variety, symmetricPower(i, F.module))

annihilator CoherentSheaf := Ideal => o -> F -> annihilator(module F, o)

-- printing
expression SheafOfRings := O -> Subscript { OO, expression O.variety }
net        SheafOfRings :=      net @@ expression
texMath    SheafOfRings :=  texMath @@ expression
toString   SheafOfRings := toString @@ expression

describe   CoherentSheaf := F -> Describe (Subscript { expression sheaf, expression F.variety }) (expression F.module)
expression CoherentSheaf := F -> (
    (X, M) := (variety F, module F);
    if M.?relations or M.?generators or numgens M === 0 then return SheafExpression expression M;
    degs := runLengthEncoding(- degrees M); -- a list of O_X^r(d) for each summand
    sums := apply(degs, (r, d) -> (
	    s := new Superscript from {expression OO_X, expression r};
	    -- TODO: get rid of the extra space in OO_X^1 (1,2) when #d > 1
	    if all(d, zero) then s else new Adjacent from {
		s, (if #d == 1 then new Parenthesize from d else expression toSequence d)}));
    fold((a, b) -> a++b, sums))
net      CoherentSheaf :=      net @@ expression
texMath  CoherentSheaf :=  texMath @@ expression
toString CoherentSheaf := toString @@ expression

-- used to be in m2/mathml.m2
mathML SheafOfRings :=
mathML CoherentSheaf := lookup(mathML, Thing)

CoherentSheaf#AfterPrint = F -> ("coherent sheaf on ", variety F,
    if isFreeModule(M := module F)    then (", free of rank ",   rank F)    else
    if M.?generators and M.?relations then (", subquotient of ", ambient F) else
    if M.?generators                  then (", subsheaf of ",    ambient F) else
    if M.?relations                   then (", quotient of ",    ambient F)
    )

-- used to be in m2/expressions.m2
SheafExpression = new WrapperType of Expression;
toString'(Function, SheafExpression) := (fmt,x) -> toString'(fmt,new FunctionApplication from { sheaf, x#0 })
net SheafExpression := x -> net x#0
texMath SheafExpression := x -> texMath x#0
expressionValue SheafExpression := x -> sheaf expressionValue x#0

-----------------------------------------------------------------------------
-- SumOfTwists type declarations and basic constructors
-----------------------------------------------------------------------------

-- used as a bound for sums of twists
LowerBound = new SelfInitializingType of BasicList
-- TODO: implement for multigraded twists
>  InfiniteNumber := >  ZZ            := LowerBound => b -> LowerBound{b+1}
>= InfiniteNumber := >= ZZ := >= List := LowerBound => b -> LowerBound{b}

SumOfTwists = new Type of BasicList
SumOfTwists.synonym = "sum of twists"

-- constructors
SheafOfRings(*)  := SumOfTwists => O -> O^1(>=-infinity)
CoherentSheaf(*) := SumOfTwists => F ->   F(>=-infinity)
SheafOfRings  LowerBound := SumOfTwists => (O, b) -> O^1(b)
CoherentSheaf LowerBound := SumOfTwists => (F, b) -> (checkProjective variety F; new SumOfTwists from {F, b})

-- basic methods
ring    SumOfTwists := S ->    ring S#0
variety SumOfTwists := S -> variety S#0

-- printing
expression SumOfTwists := S -> (expression S#0) (if S#1#0 === -infinity then expression symbol(*) else (expression symbol>=) (expression S#1#0))
net        SumOfTwists :=      net @@ expression
texMath    SumOfTwists :=  texMath @@ expression
toString   SumOfTwists := toString @@ expression

-----------------------------------------------------------------------------
-- helpers for sheaf cohomology
-----------------------------------------------------------------------------

-- TODO: should this also check that the variety is finite type over the field?
checkVariety := (X, F) -> (
    if not X === variety F     then error "expected coherent sheaf over the same variety";
    if not isAffineRing ring X then error "expected a variety defined over a field";
    )

-- pushforward the module to PP^n via S/I <-- S
flattenModule   = M -> cokernel flattenMorphism presentation M
flattenMorphism = f -> (
    g := presentation ring f;
    S := ring g;
    -- TODO: sometimes lifting to ring g is enough, how can we detect this?
    -- TODO: why doesn't lift(f, ring g) do this automatically?
    map(target f ** S, source f ** S, lift(cover f, S)) ** cokernel g)
-*
flattenComplex = C -> (
    (lo, hi) := C.concentration;
    if lo === hi
    then complex(flattenModule C_lo, Base => lo)
    else complex applyValues(C.dd.map, flattenMorphism))
*-

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
    A := ring variety F;
    -- FIXME: this line, as opposed to
    --  cokernel presentation module F
    -- breaks the test added in 975d780470.
    -- However, we need to keep the information
    -- cached in M, for instance if M is a Hom module.
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
        phi := Hom(inc, N, MinimalGenerators => true) * iso);
    -- now we compute the center map in the sequence
    -- 0 -> HH^0_B(M) -> M -> Gamma_* F -> HH^1_B(M) -> 0
    iota := inverse G.cache.pruningMap; -- map from Gamma_* F to its minimal presentation
    quot := inducedMap(N, M);           -- map from M to N = M/HH^0_B(M)
    F.cache.SaturationMap = if p <= 0 then iota * quot else iota * phi * quot;
    G)

-----------------------------------------------------------------------------
-- cohomology
-----------------------------------------------------------------------------
-- TODO: add hooks for X not finite type over k?

-- HH^p(X, OO_X)
cohomology(ZZ,          SheafOfRings) := Module => opts -> (p,    O) -> cohomology(p, variety O, O^1, opts)
cohomology(ZZ, Variety, SheafOfRings) := Module => opts -> (p, X, O) -> cohomology(p,         X, O^1, opts)

-- HH^p(X, F(>=b))
cohomology(ZZ,                    SumOfTwists) := Module => opts -> (p,    S) -> cohomology(p, variety S, S, opts)
cohomology(ZZ, ProjectiveVariety, SumOfTwists) := Module => opts -> (p, X, S) -> (
    checkVariety(X, S);
    (F, b) := (S#0, S#1#0);
    F.cache.HH        ??= new MutableHashTable;
    -- TODO: when p>0, HH^p(F(*)) gives a "not implemented yet" error
    F.cache.HH#(p, b) ??= if p == 0
    then twistedGlobalSectionsModule(F, b)
    else HH^(p+1)(module F, Degree => b))

-- HH^p(X, F)
cohomology(ZZ,                    CoherentSheaf) := Module => opts -> (p,    F) -> cohomology(p, variety F, F, opts)
cohomology(ZZ,     AffineVariety, CoherentSheaf) := Module => opts -> (p, X, F) -> (
    checkVariety(X, F);
    if p == 0 then module F else (ring X)^0)
cohomology(ZZ, ProjectiveVariety, CoherentSheaf) := Module => opts -> (p, X, F) -> (
    checkVariety(X, F);
    F.cache.HH   ??= new MutableHashTable;
    if F.cache.HH#?p then return F.cache.HH#p;
    -- TODO: only need basis(0, G) in the end, is this too much computation?
    G := if p == 0 then twistedGlobalSectionsModule(F, 0) -- HH^0 F(>=0)
    else (
	-- pushforward F to PP^n
	M := flattenModule module F;
	S := ring M;
	-- TODO: both n and w need to be adjusted for the multigraded case
	n := dim S-1;
	w := S^{-n-1};
	-- using Serre duality for coherent sheaves on schemes with mild
	-- singularities, Cohen–Macaulay schemes, not just smooth schemes.
	-- TODO: check that X is proper (or at least finite type)
	Ext^(n-p)(M, w));
    k := coefficientRing ring X;
    F.cache.HH#p = k^(rank source basis(0, G)))

-----------------------------------------------------------------------------
-- Module of twisted global sections Γ_*(F)
-----------------------------------------------------------------------------

-- This is an approximation of Gamma_* F, at least with an inclusion from Gamma_>=0 F
-- TODO: optimize caching: if HH^0(F>=b) is cached above, does this need to be cached?
-- TODO: should F>=0 be hardcoded?
minimalPresentation SheafOfRings  := prune SheafOfRings  := SheafOfRings  => opts -> identity
minimalPresentation CoherentSheaf := prune CoherentSheaf := CoherentSheaf => opts -> (
    F -> F.cache#(symbol minimalPresentation => opts) ??= tryHooks(
	(minimalPresentation, CoherentSheaf), (opts, F), (opts, F) -> (
	    -- this is the default algorithm
	    G := sheaf(F.variety, HH^0 F(>=0));
	    G.cache.pruningMap = sheaf(F.variety, F.cache.SaturationMap);
	    G)))

-----------------------------------------------------------------------------
-- cotangentSheaf, tangentSheaf, and canonicalBundle
-----------------------------------------------------------------------------
-- TODO: make this work for weighted projective spaces, see c564ec04
-- this would be useful for checking things about mirror symmetry
-- weightedVars = S -> (
--      map(S^1, S^-(degrees S), {apply(generators S, flatten degrees S, times)})
--      )

-- TODO: this is the slowest part of hh and euler, look into other strategies
-- TODO: simplify caching here and in minimalPresentation
cotangentSheaf = method(TypicalValue => CoherentSheaf, Options => options exteriorPower ++ { MinimalGenerators => true })
cotangentSheaf ProjectiveVariety := opts -> (cacheValue (symbol cotangentSheaf => opts)) (X -> (
	R := ring X; checkRing R;
	S := ring(F := presentation R);
	(d, e) := (vars S ** R, jacobian F ** R); -- assert(d * e == 0);
	om := sheaf(X, homology(d, e));
	if opts.MinimalGenerators
	then minimalPresentation om else om))
cotangentSheaf(ZZ, ProjectiveVariety) := opts -> (i, X) -> exteriorPower(i, cotangentSheaf(X, opts), Strategy => opts.Strategy)

tangentSheaf = method(TypicalValue => CoherentSheaf, Options => options cotangentSheaf)
tangentSheaf ProjectiveVariety := opts -> X -> dual cotangentSheaf(X, opts)

idealSheaf = method(TypicalValue => CoherentSheaf, Options => options cotangentSheaf)
idealSheaf ProjectiveVariety := opts -> X -> sheaf ideal (ring X).relations

-- TODO: document
canonicalBundle = method(TypicalValue => CoherentSheaf, Options => options cotangentSheaf)
canonicalBundle ProjectiveVariety := opts -> X -> dual dual determinant(cotangentSheaf(X, opts), Strategy => opts.Strategy)

-----------------------------------------------------------------------------
-- singularLocus
-----------------------------------------------------------------------------

singularLocus     AffineVariety :=     AffineVariety => X -> Spec singularLocus ring X
singularLocus ProjectiveVariety := ProjectiveVariety => X -> (
     R := ring X;
     f := presentation R;
     A := ring f;
     checkRing A;
     Proj(A / saturate (minors(codim(R,Generic=>true), jacobian f) + ideal f)))

-----------------------------------------------------------------------------
-- isLocallyFree
-----------------------------------------------------------------------------

isLocallyFree = method(TypicalValue => Boolean)
isLocallyFree SumOfTwists   := S -> isLocallyFree S#0
isLocallyFree SheafOfRings  := O -> true
isLocallyFree CoherentSheaf := F -> (
    if (d := rank F) == 0 then return F == 0;
    if isFreeModule module F then return true;
    dim fittingIdeal(d,   module F) <= 0
    and fittingIdeal(d-1, module F) == ideal 0_(ring variety F))

-----------------------------------------------------------------------------
-- Sheaf Hom and Ext
-----------------------------------------------------------------------------

sheafHom = method(TypicalValue => CoherentSheaf, Options => options Hom)
sheafHom(SheafOfRings, SheafOfRings)  :=
sheafHom(SheafOfRings, CoherentSheaf) :=
sheafHom(CoherentSheaf, SheafOfRings)  :=
sheafHom(CoherentSheaf, CoherentSheaf) := CoherentSheaf => opts -> (F, G) -> (
    assertSameVariety(F, G); sheaf(variety F, Hom(module F, module G, opts)))

sheafExt = new ScriptedFunctor from {
    superscript => i -> new ScriptedFunctor from {
	-- sheafExt^1(F, G)
	argument => X -> applyMethod''(sheafExt, functorArgs(i, X))
	},
    argument => X -> applyMethod''(sheafExt, X)
    }

sheafExt(ZZ, SheafOfRings, SheafOfRings)  :=
sheafExt(ZZ, SheafOfRings, CoherentSheaf) :=
sheafExt(ZZ, CoherentSheaf, SheafOfRings)  :=
sheafExt(ZZ, CoherentSheaf, CoherentSheaf) := CoherentSheaf => options Ext.argument >> opts -> (i, F, G) -> (
    assertSameVariety(F, G); sheaf(variety F, Ext^i(module F, module G, opts)))

-----------------------------------------------------------------------------
-- The following algorithm appears in:
-- Gregory G. Smith, Computing global extension modules,
-- Journal of Symbolic Computation, 29 (2000) 729-746.
-- See documentation of Ext^ZZ(CoherentSheaf,CoherentSheaf) for examples.
-----------------------------------------------------------------------------

protect TruncateDegree
-- TODO: implement Ext with DegreeLimit as an optimization
Ext(ZZ, SheafOfRings,  SumOfTwists) :=
Ext(ZZ, CoherentSheaf, SumOfTwists) := Module => opts -> (m,F,G') -> (
    assertSameVariety(F, G');
    X := variety F;
    checkProjective X;
    checkVariety(X, F);
     G := G'#0;
     e := G'#1#0;
     M := module F;
     N := module G;
     R := ring M;
    r := -infinity;
    E := if dim M === 0 or m < 0 then R^0 else (
          l := min(dim N, m);
	  P := resolution flattenModule N;
	  p := length P;
	  n := dim ring P - 1;
	  -- global Ext is composition of sheaf Ext and cohomology
	  -- so we compute it as a Grothendieck spectral sequence
	  -- in this case, it degenerates
	if p < n-l then Ext^m(M, N, opts) else (
	       a := max apply(n-l..p,j -> (max degrees P_j)#0-j);
	    r = a+e-m+1;
	    Ext^m(truncate(r, M), N, opts)));
    -- When MinimalGenerators => false is given, we don't truncate
    -- or prune E because the majority of uses of this function are
    -- for computing Ext of sheaves, in which case only basis(0, E)
    -- is needed and truncation is an unnecessary computation.
    E = if opts.MinimalGenerators then prune truncate(e, E) else E;
    -- This is the degree at which M was truncated, which
    -- is used later for computing the Yoneda extension.
    E.cache.TruncateDegree = r;
    E)

Ext(ZZ, SheafOfRings, SheafOfRings)  :=
Ext(ZZ, SheafOfRings, CoherentSheaf) :=
Ext(ZZ, CoherentSheaf, SheafOfRings)  :=
Ext(ZZ, CoherentSheaf, CoherentSheaf) := Module => opts -> (n,F,G) -> (
    E := Ext^n(F, G(>=0), opts ++ { MinimalGenerators => false });
    k := coefficientRing ring E;
    V := k^(rank source basis(0, E));
    V.cache.formation = FunctionApplication { Ext, (n, F, G) };
    V.cache.TruncateDegree = E.cache.TruncateDegree;
    V)

-----------------------------------------------------------------------------
-- hh: Hodge decomposition
-----------------------------------------------------------------------------
-- TODO: HodgeTally for pretty printing the Hodge diamond

hh = new ScriptedFunctor from {
     superscript => (
	  pq -> new ScriptedFunctor from {
	       argument => X -> (
		    a := (pq,X);
		    f := lookup_hh ( class \ a );
		    if f === null then error "no method available";
	       	    f a
		    )
	       }
	  )
     }

hh(Sequence,ProjectiveVariety) := (pq,X) -> if X.cache.?hh and X.cache.hh#?pq then X.cache.hh#pq else (
     (p,q) := pq;
     if class p =!= ZZ or class q =!= ZZ then error "expected integer superscripts";
     d := dim X;
     pqs := { (p,q), (q,p), (d-p,d-q), (d-q,d-p) };
     (p,q) = min { (p,q), (q,p), (d-p,d-q), (d-q,d-p) };
     h := rank HH^q cotangentSheaf(p,X);
     if not X.cache.?hh then X.cache.hh = new MutableHashTable;
     scan(pqs, pq -> X.cache.hh#pq = h);
     h)

euler ProjectiveVariety := X -> (
     d := dim X;
     sum(0 .. d, j -> hh^(j,j) X + 2 * sum(0 .. j-1, i -> (-1)^(i+j) * hh^(i,j) X)))

-----------------------------------------------------------------------------
-- Subpackages
-----------------------------------------------------------------------------

load "./Varieties/SheafMaps.m2"
--load "./Varieties/SheafComplexes.m2"

-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

load "./Varieties/tests-varieties.m2"
load "./Varieties/tests-sheaves.m2"
load "./Varieties/tests-functors.m2"
load "./Varieties/tests-maps.m2"
--load "./Varieties/tests-complexes.m2"

-----------------------------------------------------------------------------
-- Documentation
-----------------------------------------------------------------------------

beginDocumentation()

-- TODO: move this to core?
syn = type -> TO2 {type, synonym type}

-- TODO: move "Tutorial: Fano varieties" and "Tutorial: Divisors" here
-- TODO: "fibers of a map between varieties"
-- TODO: update the node "varieties" in overviewA.m2 and "coherent sheaves" in overview2.m2
-- TODO: eventually move local cohomology as well?

doc ///
Node
  Key
    Varieties
  Headline
    affine and projective algebraic geometry
  Description
    Text
      This package provides routines for working with affine and projective
      varieties and coherent sheaves on them.
  Acknowledgement
    Large portions of this package were originally in @TT "m2/varieties.m2"@.
    Work on the type @TO SheafMap@ and related algorithms began at a Macaulay2
    @HREF{"https://aimath.org/pastworkshops/macaulay2efie.html", "workshop"}@ at the
    @HREF{"https://aimath.org", "American Institute of Mathematics"}@ in September 2023.
  Contributors
    @HREF("https://www3.nd.edu/~craicu/", "Claudiu Raicu")@ contributed to the development of this package.
  SeeAlso
    "Schubert2::Schubert2"
    "GKMVarieties::GKMVarieties"
    "NormalToricVarieties::NormalToricVarieties"
    "AbstractToricVarieties::AbstractToricVarieties"
    "MultiprojectiveVarieties::MultiprojectiveVarieties"
  Subnodes
    Variety
    AffineVariety
    ProjectiveVariety
    CoherentSheaf
    SheafOfRings
    SumOfTwists
    SheafMap
///

load "./Varieties/doc-varieties.m2"
load "./Varieties/doc-sheaves.m2"
load "./Varieties/doc-maps.m2"
--load "./Varieties/doc-complexes.m2"
load "./Varieties/doc-functors.m2"
load "./Varieties/euler-doc.m2"
load "./Varieties/genus-doc.m2"
load "./Varieties/genera-doc.m2"

-----------------------------------------------------------------------------
-- Development
-----------------------------------------------------------------------------

end--

uninstallPackage "Varieties"
restart
loadPackage("Truncations", FileName => currentDirectory() | "Truncations.m2", Reload => true)
loadPackage("Complexes",   FileName => currentDirectory() | "Complexes.m2",   Reload => true)
debug loadPackage("Varieties",   FileName => currentDirectory() | "Varieties.m2",   Reload => true, LoadDocumentation => true)
installPackage("Varieties",   FileName => currentDirectory() | "Varieties.m2")
viewHelp "Varieties"

restart
debug needsPackage "Varieties"
check "Varieties"


Q = QQ[x_1..x_3];
X = Proj Q;
K = koszulComplex vars Q;
sK = sheaf K
G = sheaf freeResolution ideal(x_1^2+x_2^2)
RHom(OO_X^1, sK)
RHom(OO_X^1, G)
S := coker G.dd_1
HH^0 S
RHom(G, sK)
Ext^2(OO_X^1, sK(-3))
Ext^0 (OO_X^1, G(3))
RHom(OO_X^1, G(2))
RHom(OO_X^1, G, 3)
RHom(sK, G, 0)
RHom(sK, OO_X^1,0)
RHom(S, OO_X^1(3))
F = eulerSequence X
Ext^0 (OO_X^1, F(2))
