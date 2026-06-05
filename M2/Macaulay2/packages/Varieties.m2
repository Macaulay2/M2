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
--  - 2026: Most commands work for subvarieties of weighted projective stacks.
---------------------------------------------------------------------------
newPackage(
    "Varieties",
    Date     => "05 March 2026",
    Version  => "0.6",
    Keywords => { "Algebraic Geometry", "Homological Algebra" },
    Headline => "routines for working with affine and projective varieties and coherent sheaves on them",
    Authors  => {
	{   Name => "Devlin Mallory",
	    Email => "malloryd@math.utah.edu",
	    HomePage => "https://devlin-mallory.github.io/" },
	{   Name => "Ritvik Ramkumar",
	    Email => "rramkuma@nd.edu",
	    HomePage => "https://sites.google.com/view/ritvikramkumar/" },
	{   Name => "Mahrud Sayrafi",
	    Email => "mahrud@mcmaster.edu",
	    HomePage => "https://mahrud.github.io/" },
	{   Name => "Gregory G. Smith",
	    Email => "ggsmith@mast.queensu.ca",
	    HomePage => "https://www.mast.queensu.ca/~ggsmith"},
        {   Name => "Burt Totaro",
            Email => "totaro@math.ucla.edu",
            HomePage => "https://www.math.ucla.edu/~totaro/"},
	{   Name => "Keller VandeBogert",
	    Email => "keller.v@uky.edu",
	    HomePage => "https://sites.google.com/view/kellervandebogert/home"},
	},
    PackageExports => {
	"Saturation",
	"Truncations",
	"Isomorphism",
	HomologicalAlgebraPackage
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
    "SumOfTwistsComplex",
    "LowerBound",
    -- Constructors
    "Proj",
    "Spec",
    "ProjectiveSpace",
    "ProjectiveStack",
    -- Methods
    "variety",
    "sheaf",
    "sheafExt",
    "sheafHom",
    "tangentSheaf",
    "cotangentSheaf",
    "canonicalSheaf",
    "canonicalBundle",
    "dualizingSheaf",
    "idealSheaf",
    "isProjective",
    "isLocallyFree",
    "degreeOnCurve",
    "naiveCotangentComplex",
    "reflexiveDifferentials",
    -- Functors
    "hh", -- TODO: should this be defined in Core?
    -- "ext", -- temporarily commented
    "OO",
    -- Symbols
    "GlobalSectionLimit",
    "SaturationMap",
    "TorsionFree",
    "NonPrint",
    "Direct",
    "DirectNonPrint",
    "Hsum",
    -- "TruncateDegree",
    }

protect Dualizing
protect BaseTwist

importFrom_Core {
    "nonnull", "listZZ",
    "getAttribute", "hasAttribute", "ReverseDictionary",
    "applyMethod", "applyMethod''", "applyMethodWithOpts''", "functorArgs",
    "toString'", "expressionValue", "unhold", -- TODO: prune these
    "concatBlocks", "concatCols", "concatRows",
    "addHook", "tryHooks", "cacheHooks",
    "liftModule", "liftMorphism",
    "isMorphism", "isAbelianCategory",
    "BinaryPowerMethod", "truncateSeries",
    }

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

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
Spec Ring := R -> R.Spec ??= (
    R.variety = new AffineVariety from {
	symbol ring => R,
	symbol cache => new CacheTable
	}
    )

Proj = method(TypicalValue => ProjectiveVariety)
Proj Ring := R -> R.Proj ??= (
    R.variety = new ProjectiveVariety from {
	symbol ring => if isHomogeneous R then R else error "Proj: expected a homogeneous ring",
	symbol cache => new CacheTable
	}
    )

-- Note: users may define synonym PP
ProjectiveSpace = new ScriptedFunctor from {
    subscript => K -> new ScriptedFunctor from {
	-- PP_kk^2
	superscript => X -> applyMethod''(ProjectiveSpace, functorArgs(K, 1:X)),
	-- PP_kk(1,2,3)
	argument    => X -> applyMethod''(ProjectiveSpace, functorArgs(K, 1:X)),
	},
    -- PP^2     --> Proj QQ[a,b]
    -- PP^{1,2} --> PP^1 ** PP^1
    superscript => X -> applyMethod''(ProjectiveSpace, 1:X),
    -- PP(1,2,3) --> weighted projective stack shortcut
    -- PP E      --> projective bundle Proj E
    argument    => X -> applyMethod''(ProjectiveSpace, 1:X)
    }
typicalValues#ProjectiveSpace = ProjectiveVariety

-- ProjectiveSpace_kk(n) defines projective space PP^n over a given base ring k (typically a field).
-- TODO: add options for variable names, other monoid options?
-- TODO: see base change issue https://github.com/Macaulay2/M2/issues/2351
ProjectiveSpace ZZ        := ProjectiveVariety =>      n  -> Proj(QQ[vars(0..n#0)])
ProjectiveSpace(Ring, ZZ) := ProjectiveVariety => (kk, n) -> Proj(kk[vars(0..n)])
-- TODO:
--ProjectiveSpace List        := ProjectiveVariety =>      nn  -> cartesianProduct apply(nn, n -> ProjectiveSpace^n)
--ProjectiveSpace(Ring, List) := ProjectiveVariety => (kk, nn) -> cartesianProduct apply(nn, n -> ProjectiveSpace_kk^n)
-- these two are defined only for convenience of allowing a PP synonym
ProjectiveSpace       Sequence  :=
ProjectiveSpace(Ring, Sequence) := ProjectiveVariety => args -> ProjectiveStack(args)
-- Finally, ProjectiveSpace(CoherentSheaf) is defined in Functors.m2

ProjectiveStack = new ScriptedFunctor from {
    subscript => K -> new ScriptedFunctor from {
	-- PP_kk(1,2,3)
	argument => X -> applyMethod''(ProjectiveStack, functorArgs(K, 1:X)),
	},
    -- PP(1,2,3) --> weighted projective stack shortcut
    argument => X -> applyMethod''(ProjectiveStack, X)
    }
typicalValues#ProjectiveStack = ProjectiveVariety

-- ProjectiveStack_kk(a,b,c) defines the weighted projective space PP(a,b,c) over a given base ring kk (typically a field).
-- TODO: add options for variable names, other monoid options?
ProjectiveStack       Sequence  := ProjectiveVariety =>      w  -> Proj(QQ[vars(0..#w#0-1), Degrees => listZZ w#0])
-- TODO: see base change issue https://github.com/Macaulay2/M2/issues/2351
ProjectiveStack(Ring, Sequence) := ProjectiveVariety => (kk, w) -> Proj(kk[vars(0..#w-1), Degrees => listZZ w])

-- this is a kludge to make Spec ZZ/101[x,y]/(y^2-x^3) and Proj ZZ/101[x,y]/(x^2-y^2) work as expected
-- TODO: also make Spec kk{x,y} or Spec kk<|x,y|> work when they are supported
-- TODO: document this in Proj and Spec
    AffineVariety/Thing :=     AffineVariety => (X, I) -> Spec((ring X)/I)
ProjectiveVariety/Thing := ProjectiveVariety => (X, I) -> Proj((ring X)/I) -- TODO: should this be saturated?
    AffineVariety Array :=     AffineVariety => (X, M) -> Spec((ring X) M)
ProjectiveVariety Array := ProjectiveVariety => (X, M) -> Proj((ring X) M)

-- true for standard graded rings
isStandardGraded = R -> unique degrees R === {{1}}
-- true for a singly graded rings with positive grading
isWeightedZZGraded = R -> degreeLength R === 1 and min flatten degrees R > 0

assertStandardGraded   = R -> if isStandardGraded   R then R else error "expected a subvariety of the projective space"
assertWeightedZZGraded = R -> if isWeightedZZGraded R then R else error "expected a substack of a weighted projective space"

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
    and assert'(not isProjective X or isWeightedZZGraded R,
	-- TODO: should we also check that every subset of n-1 degrees are coprime?
	"the coordinate ring of a projective variety should be positively graded")
    )

-- basic methods
ring  Variety := X -> X.ring
ideal Variety := X -> ideal ring X -- TODO: should this give the irrelevant ideal?
codim Variety := options(codim, QuotientRing) >> o -> X -> codim(ring X, o)

dim     AffineVariety := X -> dim ring X
dim ProjectiveVariety := X -> (
    -- For a singly graded algebra R, the dimension of Proj(R) is dim(R) - 1, except that
    -- it is always at least -1. For an ZZ^r-graded ring R, this definition views Proj(R)
    -- as the quotient of an (unspecified) open subset of Spec(R) by a generically stable action
    -- of a torus of dimension r. That is not the standard definition of Proj; but it may
    -- be useful if developed further. For now, many commands for projective varieties
    -- are restricted to the singly graded case.
    -- TODO: is this always correct for a Mori dream space?
    max(-1, dim ring X - rank degreeGroup ring X))

char     AffineVariety := X -> char ring X
char ProjectiveVariety := X -> char quotient saturate ideal X -- TODO: saturate with respect to B?

-- The degree of a closed subspace of a weighted projective space.
-- (This is compatible with the function hilbertPolynomial X.)
-- This function returns an integer if all weights are equal to 1, but otherwise a rational number.
-- For example, the degree of the weighted projective space P^n(a_0,...,a_n) is 1/(a_0...a_n).
degree ProjectiveVariety := X -> degree OO_X^1
genus  ProjectiveVariety := X -> genus  OO_X^1
genera ProjectiveVariety := X -> genera ring X

-- TODO: should this be for any Variety (e.g. including toric varieties)
hilbertPolynomial ProjectiveVariety := opts -> X -> hilbertPolynomial(OO_X^1, opts)

-- The topological Euler characteristic, if the given variety X is smooth. Otherwise, this is harder to interpret.
-- But, for example, this function also gives the topological Euler characteristic if X has quotient singularities
-- and the characteristic is zero.
euler ProjectiveVariety := ZZ => X -> (
    d := dim X;
    m := d // 2;
    -- alternative but slower definition in terms computing using Hodge numbers:
    -- sum(0 .. d, j -> hh^(j,j) X + 2 * sum(0 .. j-1, i -> (-1)^(i+j) * hh^(i,j) X)))
    L := for i from 0 when 2 * i < d list (
        (-1)^i * euler reflexiveDifferentials(i, X));
    -- all terms are paired, except for the middle term when d is even
    2 * sum L + if odd d then 0 else (
        (-1)^m * euler reflexiveDifferentials(m, X)))
eulers ProjectiveVariety := X -> eulers ring X

ambient     AffineVariety :=     AffineVariety => X -> Spec ambient ring X
ambient ProjectiveVariety := ProjectiveVariety => X -> Proj ambient ring X
-- Note that if ring X was defined as a quotient of another quotient ring Q,
-- then "ambient ring X" gives Q, not a polynomial ring as one might want.
-- The polynomial ring can be obtained by: S = ring presentation ring X

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

-- This method returns either a Variety, an AbstractVariety (from Schubert2),
-- a NormalToricVariety, or any other variety stashed in R.variety.
variety = method(TypicalValue => Variety)
variety Ring  := S -> if S.?variety then S.variety else if isHomogeneous S then Proj S else Spec S
variety Ideal := I -> if isHomogeneous I then Proj quotient I else Spec quotient I

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

-- TODO: if the ring is not standard graded, it should be displayed (e.g. show the degrees)
describe     AffineVariety := X -> Describe (expression Spec) (expression X.ring)
describe ProjectiveVariety := X -> Describe (expression Proj) (expression X.ring)

-----------------------------------------------------------------------------
-- Divisors
-----------------------------------------------------------------------------

-- overriding the dummy methods defined in Truncations package
nefCone       Ring := R -> R.nefCone       ??= if R.?variety then nefCone       R.variety
nefGenerators Ring := R -> R.nefGenerators ??= if R.?variety then nefGenerators R.variety

-- first attempt is to use the variety, then use the degrees of the ring
addHook((effGenerators, Ring), Strategy => Default, R -> R.effGenerators ??= if R.?variety then effGenerators R.variety)
addHook((effCone,       Ring), Strategy => Default, R -> R.effCone       ??= if R.?variety then effCone       R.variety)

-- used for algorithms that need a non-trivial Picard group
checkProjective = X -> if not isProjective X then error "expected a coherent sheaf over a projective variety"

-----------------------------------------------------------------------------
-- singularLocus
-----------------------------------------------------------------------------

jacobianIdeal = R -> R.cache.jacobianIdeal ??= (
    f := presentation R;
    -- In some situations, such as projective hypersurfaces in characteristic zero,
    -- J already contains ideal f, but that's not true in general.
    J := minors(codim(R, Generic => true), jacobian f);
    -- We return an ideal in the ambient polynomial ring A, not in the quotient ring R.
    J + ideal f)

singularLocus     AffineVariety :=     AffineVariety => X -> Spec singularLocus ring X
singularLocus ProjectiveVariety := ProjectiveVariety => X -> (
    -- For a projective scheme X over a base ring k, singularlocus X is the locus where X is not smooth over k.
    -- For a subspace X of a weighted projective space, this describes the locus
    -- where X is not smooth as a stack over k. Thus, the coarse moduli space of X is "quasi-smooth" outside
    -- singularLocus X, and in particular it has at most cyclic quotient singularities there.
    R := assertWeightedZZGraded ring X;
    A := ring presentation R;
    Proj(A / saturate jacobianIdeal R))

isSmooth     AffineVariety := {} >> o -> X -> 1 == ideal singularLocus X
isSmooth ProjectiveVariety := {} >> o -> X -> (
     R := assertWeightedZZGraded ring X;
     -- The singular locus of X is empty if the corresponding
     -- locus in the affine cone has dimension at most 0.
     -- We don't need to saturate the corresponding ideal, for this purpose.
     dim jacobianIdeal R <= 0)

-- The following functions check whether a variety over a field is Cohen-Macaulay and equidimensional.
-- (For X connected, this is equivalent to just being Cohen-Macaulay.)
-- Depth::isCohenMacaulay checks only at the origin,
-- but TestIdeals::isCohenMacaulay checks globally.
-- cf. https://github.com/Macaulay2/M2/issues/4201
isCohenMacaulay     AffineVariety := true >> o -> X -> ( needsPackage "TestIdeals"; isCohenMacaulay(ring X, o) ) -- FIXME
isCohenMacaulay ProjectiveVariety := {}   >> o -> X -> (
    M := torsionFreeLift OO_X^1;
    S := ring M; -- Thus S is a graded polynomial ring, and M is R = S/I as an S-module,
    -- for a graded ring R with X = Proj(R).
    if degreeLength S =!= 1 then error "expected degree length 1";
    degs := degrees S; -- This is a list of the form {1,9,15,22}, say.
    dimS := numgens S; -- So the ring S has dimension dimS.
    dimR := dim M;
    sumOfWeights := sum degs; -- This is sum_i |x_i|, where S = k[x_0,...,x_(n-1)].
    S.cache ??= new MutableHashTable;
    w := S.cache.Dualizing ??= S^{-sumOfWeights};
    -- We fix the dualizing module w, as a graded S-module (even though the grading is irrelevant
    -- for this function). As a result, Macaulay2 automatically remembers Ext^i(M, w)
    -- (for a number i), in case another function has computed that module earlier.
    flag := true;
    i := dimS - dimR + 1;
    while flag and (i < dimS) do ( -- Since R is graded, it suffices to check that the S-module Ext^i(M,w)
	-- is supported at the origin for dim(S)-dim(R) < i < dim(S).
	if dim Ext^i(M, w) > 0 then flag = false;
	i = i + 1);
    flag)

-----------------------------------------------------------------------------
-- Subpackages
-----------------------------------------------------------------------------

load "./Varieties/CoherentSheaves.m2"
load "./Varieties/Functors.m2"
load "./Varieties/SheafMaps.m2"
if HomologicalAlgebraPackage === "Complexes" then
load "./Varieties/SheafComplexes.m2"

-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

load "./Varieties/tests-varieties.m2"
load "./Varieties/tests-sheaves.m2"
load "./Varieties/tests-functors.m2"
load "./Varieties/tests-maps.m2"
if HomologicalAlgebraPackage === "Complexes" then
load "./Varieties/tests-complexes.m2"

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
    In 2026, most functions were revised to work for subvarieties of weighted projective stacks.
  Contributors
    @HREF("https://johndcobb.github.io/", "John Cobb")@ and
    @HREF("https://academicweb.nd.edu/~craicu/", "Claudiu Raicu")@ contributed to the development of this package.
  SeeAlso
    "varieties"
    "coherent sheaves"
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
if HomologicalAlgebraPackage === "Complexes" then
load "./Varieties/doc-complexes.m2"
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
