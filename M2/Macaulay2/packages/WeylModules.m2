newPackage("WeylModules",
    Version  => "0.3",
    Date     => "July 18, 2026",
    Authors  => {{
        Name     => "Keller VandeBogert",
        Email    => "keller.v@uky.edu",
        HomePage => "https://sites.google.com/view/kellervandebogert/home"
    }},
    Headline => "Weyl modules in positive characteristic, after S. Doty",
    HomePage => "https://sites.google.com/view/kellervandebogert/home",
    Keywords => {"Lie Groups and Lie Algebras", "Representation Theory"},
    PackageExports => {"KacMoody"},
    AuxiliaryFiles => true,
    DebuggingMode => false
)

----------------------------------------------------------------------
-- WeylModules: a Macaulay2 implementation inspired by S.R. Doty's GAP
-- package WeylModulesNew.
--
-- This version provides Chevalley structure constants in every finite
-- type, integral Weyl modules with Chevalley actions, reduction modulo a
-- prime, maximal-vector and subquotient calculations, socle series,
-- simple characters and decomposition numbers, and the type-A bridge to
-- Schur algebras and symmetric groups.
--
-- Conventions:
--   * All roots are stored as Lists in the simple-root basis (so a
--     positive root has all entries >= 0, a negative root has all
--     entries <= 0).
--   * Cartan matrices are taken from KacMoody.m2 (cartanA, cartanB, ...).
--   * Weights are by default in the fundamental-weight (Dynkin) basis,
--     matching KacMoody and Doty.
----------------------------------------------------------------------

export {
    -- Layer 2: Chevalley structure constants (ADE + non-ADE).
    "isSimplyLaced",
    "classifyFiniteCartan",
    "chevalleyEpsilonMatrix",
    "chevalleyConstants",
    "chevalleyN",
    "verifyJacobi",
    -- Layer 4: Weyl modules over Z (all finite Cartan types).
    "WeylModuleZ",
    "weylModuleZ",
    "weylModule",
    "LieType",
    "highestWeight",
    "ambientCartan",
    "weights",
    "weightSpaces",
    "weightSpace",
    "basisVecs",
    "character",
    "actionF",
    "actionE",
    "actionH",
    "applyChevalley",
    "pbwTuples",
    "maximalVectors",
    -- Layer 6: submodules and subquotients of V(lambda) mod p.
    "SubWeylModuleFp",
    "subWeylModule",
    "subWeylModuleDirectSum",
    "ambientWeylModule",
    "isWithin",
    "QuotientWeylModuleFp",
    "quotientWeylModule",
    "definingKernel",
    "simpleQuotient",
    "simpleCharacter",
    "simpleDimension",
    "socleWeyl",
    "decomposeCharacter",
    "decompositionNumbers",
    "maximalSubmodule",
    "isAmbiguous",
    "ambiguousMaxVecs",
    "gensNextSocle",
    "nextSocle",
    "socleSeries",
    "gensSocleLayers",
    "printSocleLayers",
    "socleLengthTwoQuotient",
    "twoFactorQuotientsContaining",
    -- Character utilities and weight predicates.
    "characterDim",
    "productCharacter",
    "differenceCharacter",
    "sortedCharacter",
    "isDominantWeight",
    "isRestrictedWeight",
    "highestPrimePower",
    "dominantWeights",
    "dominantWeightSpaces",
    "isMaximalVector",
    "simpleTopFactorCharacter",
    "simpleTopFactorDim",
    "socleSeriesAndPrims",
    -- Convenience converters between Dynkin and partition coordinates.
    "weightAsPartition",
    "weightAsDynkin",
    "characterAsPartition",
    "characterAsDynkin",
    -- Phase I: Schur-algebra layer (Doty's Chapter 4).
    "compositionToWeight",
    "weightToComposition",
    "allPartitions",
    "boundedPartitions",
    "conjugatePartition",
    "pRestrictedQ",
    "pRegularQ",
    "pRestrictedPartitions",
    "pRegularPartitions",
    "mullineux",
    "schurAlgebraWeylModule",
    "schurAlgebraDecompositionMatrix",
    "symmetricGroupDecompositionMatrix",
    "symmetricGroupDecompositionNumbers",
    -- Uniform module and quotient-coordinate protocol.
    "quotientProjection",
    "quotientSection",
    "quotientBasis",
    "projectToQuotient",
    "liftFromQuotient",
    "characteristic",
    "rootSystemType",
    "lieType",
    "lieRank",
    "moduleMetadata",
    "constructionData",
    "reduceWeylModule",
    "reduction",
    "isSubmoduleOf",
    "containsSubmodule",
    "equalSubmodules",
    "submoduleCodimension",
    "submoduleSum",
    "submoduleIntersection",
    "SubquotientWeylModuleFp",
    "subquotientWeylModule",
    "lowerSubmodule",
    "upperSubmodule",
    "subquotientProjection",
    "subquotientSection",
    "subquotientBasis",
    "projectToSubquotient",
    "liftFromSubquotient",
    -- Formal characters and labelled decomposition tables.
    "WeylCharacter",
    "weylCharacter",
    "coordinateConvention",
    "sourceWeightFormat",
    "isVirtualCharacter",
    "characterMetadata",
    "rawCharacter",
    "characterTerms",
    "WeightOrder",
    "tensorProductCharacter",
    "tensorProductFactors",
    "dualCharacter",
    "frobeniusTwistCharacter",
    "DecompositionTable",
    "decompositionTable",
    "rowLabels",
    "columnLabels",
    "prime",
    "algebra",
    "tableMetadata",
    "schurAlgebraDecompositionTable",
    "symmetricGroupDecompositionTable",
    -- Socle filtrations and comparison across characteristics.
    "SocleAnalysis",
    "ReductionProfile",
    "ExtensionData",
    "RequireComplete",
    "IncludeStructure",
    "IncludeSocle",
    "compositionFactors",
    "isIrreducible",
    "socleAnalysis",
    "socleLayerFactors",
    "socleLayerGenerators",
    "cumulativeSocleLayers",
    "socleLayerDimensions",
    "cumulativeSocleDimensions",
    "successiveQuotientDimensions",
    "ambiguousWeights",
    "ambiguousWeightsByLayer",
    "analysisIssues",
    "isCertified",
    "socleAnalysisComplete",
    "socleAnalysisPrime",
    "socleAnalysisModuleDimension",
    "socleLength",
    "reductionProfile",
    "reductionRows",
    "reductionPrimes",
    "reductionRow",
    "reductionWeylModule",
    "reductionHighestWeight",
    "reducibleCharacteristics",
    "extensionData",
    "isTwoFactorExtension",
    -- Global matrices, Kostant-form operators, and relation checks.
    "ChevalleyActionData",
    "WeylModuleCheck",
    "WeylGraph",
    "SocleDiagram",
    "CheckLevel",
    "pbwRoots",
    "chevalleyActionMatrices",
    "raisingMatrices",
    "loweringMatrices",
    "cartanMatrices",
    "globalBasis",
    "dividedPowerActionE",
    "dividedPowerActionF",
    "binomialCartanAction",
    "checkWeylModule",
    "verifyRepresentation",
    "isValid",
    "verificationChecks",
    "verificationFailures",
    "weylHomDimension",
    "weylHomGenerators",
    "weightGraph",
    "graphVertices",
    "graphEdges",
    "toDot",
    "toSVG",
    "viewGraph",
    "socleDiagram",
    "diagramLayers"
}

-- Resolve this package's definitions before names also exported by KacMoody.
use currentPackage;
dictionaryPath = prepend(currentPackage#"private dictionary",
    delete(currentPackage#"private dictionary", dictionaryPath));

----------------------------------------------------------------------
-- Internal helpers (not exported)
----------------------------------------------------------------------

-- The list of positive roots of A as a list of lists in simple-root
-- coordinates.  KacMoody returns them as a Matrix; we just unpack.
positiveRootsList = A -> (
    R := cartanPositiveRoots A;
    for i from 0 to numrows R - 1 list flatten entries R^{i}
)

----------------------------------------------------------------------
-- WeightFormat plumbing
----------------------------------------------------------------------
-- WeightFormat is the option (mirroring KacMoody's `WeightFormat`)
-- that lets callers express weights in classical-type partition
-- coordinates instead of the default Dynkin / fundamental-weight
-- coordinates.  Output conversion is delegated to KacMoody's
-- `fundamentalToPartition`.  For input, accessors must accept arbitrary
-- weights, not only dominant highest weights.  We therefore invert the
-- linear Dynkin-to-epsilon map directly: type A uses consecutive differences
-- modulo the determinant direction, while types B/C/D solve against the
-- epsilon coordinates of the fundamental weights.  This also respects a
-- simultaneous permutation of the rows and columns of the Cartan matrix.
--
--   "Dynkin"     : Lambda = (m_1, ..., m_n)  with m_i = (lambda, alpha_i^vee).
--   "Partition"  : Lambda = (lambda_1, ..., lambda_n)  in epsilon-basis.
--                  Type B / D admits half-integer entries (spin weights).
--                  Type C requires integer entries.
--                  Type A pads to length (n + 1) and is determined up to
--                  a global shift; KacMoody picks the canonical form.
-- Internally everything is Dynkin.

-- Convert an epsilon-coordinate type-A weight to Dynkin coordinates.
-- Unlike a highest-weight partition, an arbitrary weight need not be weakly
-- decreasing.  Type A is insensitive to a global shift of all coordinates.
typeAWeightToDynkin = (A, lam) -> (
    n := numrows A;
    w := lam;
    while #w > 0 and last w == 0 do w = drop(w, -1);
    if #w > n + 1 then
        error "WeightFormat => \"Partition\": type-A weight has more than rank+1 nonzero coordinates";
    padded := for i from 0 to n list if i < #w then w#i else 0;
    canonical := for i from 0 to n - 1 list (
        d := padded#i - padded#(i + 1);
        dQ := try (d + 0_QQ) else
            error "WeightFormat => \"Partition\": type-A coordinates must be rational";
        if denominator dQ != 1 then
            error "WeightFormat => \"Partition\": adjacent type-A coordinates must differ by integers";
        numerator dQ
    );
    -- Transport canonical Dynkin labels to A's actual node order.  Each
    -- fundamental weight omega_i is represented by the dominant partition
    -- (1^i,0^(n+1-i)), so KacMoody's checked converter supplies the needed
    -- node permutation even when A is simultaneously row/column permuted.
    out := new MutableList from toList(n:0);
    for i from 0 to n - 1 do if canonical#i != 0 then (
        probe := for j from 0 to n list if j <= i then 1 else 0;
        unit := partitionToFundamental(A, probe);
        for j from 0 to n - 1 do out#j = out#j + canonical#i * unit#j;
    );
    new List from out
);

-- Convert an arbitrary epsilon-coordinate weight in type B, C, or D to
-- Dynkin labels.  KacMoody's public partitionToFundamental routine enforces
-- dominance because it is intended for highest weights; weight-space
-- accessors need the inverse on the entire weight lattice.  The columns below
-- are the epsilon coordinates of the fundamental weights in A's actual node
-- order, so the same calculation also handles permuted Cartan matrices.
classicalWeightToDynkin = (A, lam) -> (
    n := numrows A;
    w := lam;
    while #w > 0 and last w == 0 do w = drop(w, -1);
    if #w > n then
        error "WeightFormat => \"Partition\": classical weight has more coordinates than the rank";
    paddedQ := for i from 0 to n - 1 list (
        x := if i < #w then w#i else 0;
        try (x + 0_QQ) else
            error "WeightFormat => \"Partition\": classical coordinates must be rational"
    );
    columns := for i from 0 to n - 1 list (
        unit := for j from 0 to n - 1 list if i == j then 1 else 0;
        epsilon := fundamentalToPartition(A, unit);
        if #epsilon != n then
            error "WeightFormat => \"Partition\": expected classical type B, C, or D";
        transpose matrix(QQ, {for x in epsilon list x + 0_QQ})
    );
    transform := if n == 1 then first columns
                 else fold((left, right) -> left | right, columns);
    dynkinQ := inverse(transform) * transpose matrix(QQ, {paddedQ});
    for x in flatten entries dynkinQ list (
        if denominator x != 1 then
            error concatenate(
                "WeightFormat => \"Partition\": coordinates do not define an integral weight; Dynkin label ",
                toString x, " is not an integer");
        numerator x
    )
);

validateWeightFormat = (A, fmt, caller) -> (
    if fmt === "Dynkin" then return true;
    if fmt =!= "Partition" then
        error concatenate(caller, ": WeightFormat must be \"Dynkin\" or \"Partition\", got ",
                          toString fmt);
    zeroPartition := try fundamentalToPartition(A, toList(numrows A:0))
                     else null;
    if zeroPartition === null then
        error concatenate(caller,
            ": WeightFormat => \"Partition\" requires connected classical type A, B, C, or D");
    true
);

validateSimpleRootIndex = (A, i, caller) -> (
    if i < 1 or i > numrows A then
        error concatenate(caller, ": simple-root index must lie between 1 and ",
                          toString(numrows A));
    true
);

-- toDynkin(A, lam, fmt) : convert user-supplied weight to Dynkin coords.
toDynkin = (A, lam, fmt) -> (
    validateWeightFormat(A, fmt, "weight conversion");
    if fmt === "Dynkin" then (
        if #lam != numrows A then
            error "weight conversion: Dynkin weight must have length equal to the Cartan rank";
        if not all(lam, x -> instance(x, ZZ)) then
            error "weight conversion: Dynkin coordinates must be integers";
        lam
    )
    else if fmt === "Partition" then (
        n := numrows A;
        zeroPartition := try fundamentalToPartition(A, toList(n:0)) else null;
        if zeroPartition === null then
            error "WeightFormat => \"Partition\" requires connected classical type A, B, C, or D"
        else if #zeroPartition == n + 1 then
            typeAWeightToDynkin(A, lam)
        else if #zeroPartition == n then classicalWeightToDynkin(A, lam)
        else error "WeightFormat => \"Partition\": unrecognized classical coordinate dimension"
    )
    else error concatenate("WeightFormat must be \"Dynkin\" or \"Partition\", got ",
                           toString fmt)
);

-- fromDynkin(A, lam, fmt) : convert internal Dynkin coords to user format.
fromDynkin = (A, lam, fmt) -> (
    validateWeightFormat(A, fmt, "weight conversion");
    if #lam != numrows A or not all(lam, x -> instance(x, ZZ)) then
        error "weight conversion: expected integral Dynkin coordinates of Cartan rank";
    if fmt === "Dynkin" then lam
    else if fmt === "Partition" then fundamentalToPartition(A, lam)
);

-- Convert a HashTable character (weight -> mult) to/from the user format.
characterFromDynkin = (A, ch, fmt) -> (
    validateWeightFormat(A, fmt, "character conversion");
    if fmt === "Dynkin" then ch
    else (
        new HashTable from for w in keys ch list fromDynkin(A, w, fmt) => ch#w
    )
);
characterToDynkin = (A, ch, fmt) -> (
    validateWeightFormat(A, fmt, "character conversion");
    if fmt === "Dynkin" then ch
    else (
        -- In type A, partition coordinates are defined only up to a
        -- global shift.  Distinct input keys can therefore represent the
        -- same Dynkin weight; combine their multiplicities instead of
        -- letting the HashTable constructor discard one of them.
        out := new MutableHashTable;
        for w in keys ch do (
            wDynkin := toDynkin(A, w, fmt);
            out#wDynkin = (if out#?wDynkin then out#wDynkin else 0) + ch#w;
        );
        new HashTable from out
    )
);

-- Negate a root (componentwise).
negV = v -> for x in v list -x

-- Sum two roots (componentwise).
addV = (u, v) -> for i from 0 to #u - 1 list u#i + v#i

-- Dominance comparison for Dynkin-coordinate weights.  This is kept near
-- the common helpers because accessors for submodules and quotients need it
-- before the character-decomposition layer is defined.
dominanceGE = (A, alpha, beta) -> (
    n := numrows A;
    aS := toSimple(A, alpha);
    bS := toSimple(A, beta);
    delta := for k from 0 to n - 1 list aS#k - bS#k;
    all(delta, x -> x >= 0 and (instance(x, ZZ) or denominator x == 1))
)

uniqueMaximalWeight = (A, wts, objectName) -> (
    if #wts == 0 then return null;
    maximal := select(wts, cand ->
        not any(wts, other -> other != cand and dominanceGE(A, other, cand)));
    if #maximal != 1 then
        error(objectName | " has more than one maximal weight; use dominantWeights instead");
    first maximal
)

-- Test if a list is the zero vector.
isZero = v -> all(v, x -> x == 0)

-- Test if a list (assumed a root) is positive: all entries >= 0 and at
-- least one > 0.  Negative roots have all entries <= 0.
isPosRoot = v -> all(v, x -> x >= 0) and any(v, x -> x > 0)

-- The bilinearly extended epsilon function.
--    eps(alpha, beta) = prod_{i,j} epsmat[i][j]^{alpha_i * beta_j}
-- Since epsmat entries lie in {+1, -1}, the value is +1 if the parity
-- count of (-1)-entries with odd exponent is even, -1 otherwise.
epsilonFunction = (epsmat, alpha, beta) -> (
    r := numrows epsmat;
    s := 1;
    for i from 0 to r - 1 do (
        for j from 0 to r - 1 do (
            if epsmat_(i, j) == -1 then (
                k := alpha#i * beta#j;
                if k % 2 != 0 then s = -s;
            );
        );
    );
    s
)

-- One-pass Freudenthal multiplicity table.
--
-- KacMoody's `weightMultiplicity` reruns the entire Freudenthal recursion
-- for each query, which gives O(#weights^2) total work when we want
-- every multiplicity.  This helper does the recursion just once and
-- returns a HashTable mu -> mult (Dynkin coords).  The body mirrors
-- KacMoody's `weightMultiplicity` (lib KacMoody.m2:3352), but without
-- the extra-API wrapping and with all multiplicities returned.
weightsAndMults = (A, lambdaDynkin) -> (
    r := numrows A;
    -- Get the weight set as a HashTable.  weightsOfRepHash is internal to
    -- KacMoody; weightsOfRep returns the same data sorted, so rebuild the
    -- HashTable here.
    wList := weightsOfRep(A, lambdaDynkin, WeightFormat => "Dynkin");
    visited := new MutableHashTable;
    for w in wList do visited#w = true;
    rho := weylVector A;
    D := symmetrizer A;
    AQQ := A ** QQ;
    B := D * AQQ;

    pair := (v, w) -> (
        s := 0_QQ;
        for i from 0 to r - 1 do (
            vi := v#i;
            if vi == 0 then continue;
            for j from 0 to r - 1 do
                s = s + vi * B_(i, j) * w#j;
        );
        s
    );
    addLists := (v, w) -> for i from 0 to #v - 1 list v#i + w#i;
    scalList := (c, v) -> for i from 0 to #v - 1 list c * v#i;

    Rpos := cartanPositiveRoots A;
    posRoots := for row from 0 to numrows Rpos - 1 list (
        for j from 0 to r - 1 list Rpos_(row, j)
    );

    lamSimple := toSimple(A, lambdaDynkin);
    lamRhoSimple := addLists(lamSimple, rho);
    lamRhoNorm := pair(lamRhoSimple, lamRhoSimple);

    mult := new MutableHashTable;
    mult#lambdaDynkin = 1;

    -- Process weights in decreasing height order.
    weightHt := w -> (
        s := toSimple(A, w);
        h := 0_QQ;
        for x in s do h = h + x;
        h
    );
    htPairs := apply(keys visited, w -> (weightHt w, w));
    htPairs = rsort htPairs;
    allWeights := apply(htPairs, p -> p#1);

    for w in allWeights do (
        if w == lambdaDynkin then continue;
        wSimple := toSimple(A, w);
        wRhoSimple := addLists(wSimple, rho);
        wRhoNorm := pair(wRhoSimple, wRhoSimple);
        denomVal := lamRhoNorm - wRhoNorm;
        if denomVal == 0_QQ then (
            mult#w = 0;
            continue;
        );
        numerVal := 0_QQ;
        for alpha in posRoots do (
            k := 1;
            while true do (
                shifted := addLists(wSimple, scalList(k, alpha));
                shiftedDynkin := toFundamental(A, shifted);
                shiftedDynkin = for x in shiftedDynkin list (
                    if denominator x == 1 then numerator x else x
                );
                if not visited#?shiftedDynkin then break;
                mShifted := if mult#?shiftedDynkin then mult#shiftedDynkin else 0;
                if mShifted == 0 then break;
                numerVal = numerVal + 2 * mShifted * pair(shifted, alpha);
                k = k + 1;
            );
        );
        mult#w = lift(numerVal / denomVal, ZZ);
    );
    mult
)

----------------------------------------------------------------------
-- isSimplyLaced
----------------------------------------------------------------------

isSimplyLaced = method()
isSimplyLaced Matrix := A -> (
    if not isCartanMatrix A then return false;
    if cartanType A =!= "finite" then return false;
    D := symmetrizer A;
    if D === null then return false;
    -- Simply-laced iff the symmetrizer is a multiple of the identity,
    -- i.e. all simple roots have the same norm.
    diag := for i from 0 to numrows D - 1 list D_(i, i);
    all(diag, x -> x == diag#0)
)

----------------------------------------------------------------------
-- chevalleyEpsilonMatrix
----------------------------------------------------------------------

chevalleyEpsilonMatrix = method()
chevalleyEpsilonMatrix Matrix := A -> (
    if A.cache#?"chevalleyEpsilonMatrix" then
        return A.cache#"chevalleyEpsilonMatrix";
    if not isCartanMatrix A then
        error "chevalleyEpsilonMatrix: input is not a generalized Cartan matrix";
    if cartanType A =!= "finite" then
        error "chevalleyEpsilonMatrix: only finite-type Cartan matrices supported";
    if not isSimplyLaced A then
        error("chevalleyEpsilonMatrix: only simply-laced (ADE) types supported in this version. " |
              "Non-simply-laced (B, C, F, G) support is planned for a future release.");
    r := numrows A;
    -- GAP convention (lib/algliess.gi):
    --   epsmat[i][i] = -1
    --   epsmat[i][j] = +1 for j < i
    --   epsmat[i][j] = (-1)^{A_{ij}} for j > i
    -- For ADE, A_{ij} in {-1, 0} when i != j, so (-1)^{A_{ij}} is -1
    -- when i and j are connected in the Dynkin diagram, +1 otherwise.
    M := matrix table(r, r, (i, j) -> (
        if i == j then -1
        else if j < i then 1
        else (if A_(i, j) == 0 then 1 else -1)
    ));
    A.cache#"chevalleyEpsilonMatrix" = M;
    M
)

----------------------------------------------------------------------
-- chevalleyConstants
----------------------------------------------------------------------

-- Compute N(alpha, beta) for every ordered pair of roots (alpha, beta)
-- such that alpha + beta is a nonzero root.  The result is a HashTable
-- keyed on the pair (alpha, beta) (as a Sequence of two Lists), valued
-- in the integer N(alpha, beta).
--
-- Algorithm (Frenkel-Kac, after GAP's SimpleLieAlgebraTypeA_G):
--
--   For roots alpha, beta with alpha+beta a root:
--     Let s_a, s_b, s_s be the signs of alpha, beta, alpha+beta
--     (each is +1 or -1).  Let alpha_0, beta_0 be the positive
--     versions of alpha, beta (so alpha = s_a * alpha_0, etc.).
--
--   Then
--     N(alpha, beta) = c * eps_arg
--   where c and eps_arg are determined by:
--
--     (s_a, s_b, s_s) = (+1, +1, +1):    c = +1,  eps_arg = eps(alpha_0, beta_0)
--     (s_a, s_b, s_s) = (-1, -1, -1):    c = -1,  eps_arg = eps(alpha_0, beta_0)
--     (s_a, s_b, s_s) = (+1, -1, +1):    c = -1,  eps_arg = eps(alpha_0, beta_0)
--     (s_a, s_b, s_s) = (+1, -1, -1):    c = +1,  eps_arg = eps(alpha_0, beta_0)
--     (s_a, s_b, s_s) = (-1, +1, +1):    c = +1,  eps_arg = eps(beta_0, alpha_0)
--     (s_a, s_b, s_s) = (-1, +1, -1):    c = -1,  eps_arg = eps(beta_0, alpha_0)
--
-- The cases where alpha + beta = 0 (Cartan triples) are handled
-- separately at the level of the Lie algebra and are not stored here.

-- Internal: Chevalley structure constants for ADE (uses the chevalleyEpsilonMatrix
-- and pure FK cocycle).
chevalleyConstantsADE = A -> (
    epsmat := chevalleyEpsilonMatrix A;     -- ADE only

    posRoots := positiveRootsList A;
    negRoots := for v in posRoots list negV v;
    allRoots' := join(posRoots, negRoots);

    rootSet := new MutableHashTable;
    for v in allRoots' do rootSet#v = true;
    isRoot := v -> rootSet#?v;

    N := new MutableHashTable;

    for alpha in allRoots' do (
        for beta in allRoots' do (
            ssum := addV(alpha, beta);
            if isZero ssum then continue;
            if not isRoot ssum then continue;

            sa := if isPosRoot alpha then 1 else -1;
            sb := if isPosRoot beta  then 1 else -1;
            ss := if isPosRoot ssum  then 1 else -1;

            alpha0 := if sa == 1 then alpha else negV alpha;
            beta0  := if sb == 1 then beta  else negV beta;

            local c; local eArg;
            if sa == 1 and sb == 1 then (
                c = 1;
                eArg = epsilonFunction(epsmat, alpha0, beta0);
            )
            else if sa == -1 and sb == -1 then (
                c = -1;
                eArg = epsilonFunction(epsmat, alpha0, beta0);
            )
            else if sa == 1 and sb == -1 then (
                c = -ss;
                eArg = epsilonFunction(epsmat, alpha0, beta0);
            )
            else (
                c = ss;
                eArg = epsilonFunction(epsmat, beta0, alpha0);
            );

            N#(alpha, beta) = c * eArg;
        );
    );

    N
)

chevalleyConstants = method()
chevalleyConstants Matrix := A -> (
    if A.cache#?"chevalleyConstants" then
        return A.cache#"chevalleyConstants";

    if not isCartanMatrix A then
        error "chevalleyConstants: input is not a generalized Cartan matrix";
    if cartanType A =!= "finite" then
        error "chevalleyConstants: only finite-type Cartan matrices supported";

    result := if isSimplyLaced A then chevalleyConstantsADE A
              else (
                  cls := classifyFiniteCartan A;
                  if cls === null then
                      error("chevalleyConstants: could not classify non-ADE Cartan matrix " |
                            "(supported types: B_n, C_n, F_4, G_2 in KacMoody convention)");
                  (typeStr, n) := cls;
                  chevalleyConstantsNonADE(A, typeStr, n)
              );

    -- Cache and return an immutable table so callers cannot corrupt the
    -- structure constants subsequently used to construct modules.
    result = new HashTable from result;
    A.cache#"chevalleyConstants" = result;
    result
)

----------------------------------------------------------------------
-- chevalleyN: convenient lookup
----------------------------------------------------------------------

chevalleyN = method()
chevalleyN(Matrix, List, List) := (A, alpha, beta) -> (
    N := chevalleyConstants A;
    if N#?(alpha, beta) then N#(alpha, beta) else 0
)

----------------------------------------------------------------------
-- verifyJacobi
----------------------------------------------------------------------

-- Check the Jacobi identity for every (unordered) triple of distinct
-- roots {alpha, beta, gamma}:
--
--   N(alpha, beta) * N(alpha+beta, gamma)
-- + N(beta,  gamma) * N(beta+gamma,  alpha)
-- + N(gamma, alpha) * N(gamma+alpha, beta)  =  0
--
-- This identity expresses the Jacobi identity for the Lie bracket on
-- the linear span of root vectors.  Triples with alpha+beta+gamma = 0
-- yield the "Cartan" Jacobi identity, which involves Cartan elements
-- and is not in the form above; we skip those.  Triples for which
-- alpha+beta+gamma is not a root are also vacuous (all three terms
-- automatically vanish in the sense that the relevant N values point
-- at non-roots, hence are 0 in our table).

verifyJacobi = method()
verifyJacobi Matrix := A -> (
    posRoots := positiveRootsList A;
    negRoots := for v in posRoots list negV v;
    allRoots' := join(posRoots, negRoots);

    N := chevalleyConstants A;
    nlookup := (a, b) -> if N#?(a, b) then N#(a, b) else 0;

    failures := 0;
    nTested := 0;

    for i from 0 to #allRoots' - 1 do (
        alpha := allRoots'#i;
        for j from i + 1 to #allRoots' - 1 do (
            beta := allRoots'#j;
            for k from j + 1 to #allRoots' - 1 do (
                gamma := allRoots'#k;
                ab := addV(alpha, beta);
                bg := addV(beta,  gamma);
                ga := addV(gamma, alpha);
                abg := addV(ab, gamma);
                -- Skip cases where the structure-constant Jacobi formula
                -- is not the right identity:
                --   * alpha+beta+gamma = 0: triple Cartan case, the
                --     Jacobi identity reduces to a relation between
                --     Cartan elements, not between root vectors.
                --   * Any pairwise sum is zero: the corresponding inner
                --     bracket is a Cartan element h_alpha, not a root
                --     vector E_{alpha+beta}; the formula is invalid.
                if isZero abg then continue;
                if isZero ab or isZero bg or isZero ga then continue;
                t1 := nlookup(alpha, beta) * nlookup(ab, gamma);
                t2 := nlookup(beta,  gamma) * nlookup(bg, alpha);
                t3 := nlookup(gamma, alpha) * nlookup(ga, beta);
                nTested = nTested + 1;
                if t1 + t2 + t3 != 0 then (
                    failures = failures + 1;
                    if failures <= 5 then (
                        << "Jacobi failure on triple " << (alpha, beta, gamma) << endl;
                        << "  t1 = " << t1 << ", t2 = " << t2 << ", t3 = " << t3 << endl;
                    );
                );
            );
        );
    );

    if failures > 0 then (
        << "verifyJacobi: " << failures << " of " << nTested << " triples failed" << endl;
        return false;
    );

    true
)

----------------------------------------------------------------------
-- Layer 2 (continued): Chevalley structure constants for non-ADE types
----------------------------------------------------------------------
--
-- Algorithm (faithful translation of GAP's lib/algliess.gi):
-- Each non-simply-laced finite type is realised as the sigma-fixed
-- subalgebra of an ambient simply-laced one via diagram folding:
--
--   B_n  inside  D_{n+1}   sigma = (n, n+1)             order 2
--   C_n  inside  A_{2n-1}  sigma = product (i, 2n-i)    order 2
--   F_4  inside  E_6       sigma = (1,6)(3,5)            order 2
--   G_2  inside  D_4       sigma = (1, 3, 4)             order 3
--
-- For each non-ADE root we record its sigma-orbit in the ambient.  Each
-- orbit-average (over QQ, divided by the orbit size) is the corresponding
-- non-ADE root in ambient coordinates.  The structure constant
-- N(alpha, beta) for a non-ADE pair is computed by:
--   - finding ambient r in primes(alpha), r' in primes(beta) with
--     r + r' an ambient root;
--   - applying the GAP formula
--         N(alpha, beta) = eps(r, r') * (q + 1)
--     where eps is a sigma-invariant ambient Frenkel-Kac cocycle
--     (chosen specifically for each type) and q is the chain depth in
--     non-ADE roots.
--
-- The resulting N-table is keyed identically to the ADE case (pairs
-- of root lists in simple-root coords), so verifyJacobi works uniformly.

----------------------------------------------------------------------
-- classifyFiniteCartan: detect (type, rank) of a finite-type Cartan matrix.
-- Returns a pair ("X", n) where "X" is one of A,B,C,D,E,F,G; or null.
----------------------------------------------------------------------

classifyFiniteCartan = method()
classifyFiniteCartan Matrix := A -> (
    if not isCartanMatrix A then return null;
    if cartanType A =!= "finite" then return null;
    n := numrows A;
    if n < 1 then return null;
    -- A_n via cartanA(n+1).
    if A == cartanA(n + 1) then return ("A", n);
    if n >= 2 and A == cartanB n then return ("B", n);
    if n >= 2 and A == cartanC n then return ("C", n);
    if n >= 4 and A == cartanD n then return ("D", n);
    if n == 6 and A == cartanE 6 then return ("E", 6);
    if n == 7 and A == cartanE 7 then return ("E", 7);
    if n == 8 and A == cartanE 8 then return ("E", 8);
    if n == 4 and A == cartanF4() then return ("F", 4);
    if n == 2 and A == cartanG2() then return ("G", 2);
    null
)

----------------------------------------------------------------------
-- Folding data per non-ADE type
----------------------------------------------------------------------
-- Each helper returns a piece of the GAP-style folding data.  We
-- build the ambient Cartan matrix directly (without depending on
-- KacMoody's cartanD requiring rank >= 4) so that B_2 (ambient D_3)
-- and similar small-rank cases work.

gapAmbientCartan = (typeStr, n) -> (
    -- Declare locals once at function scope to avoid M2's "shadowing"
    -- warnings on per-branch :=  redeclarations.
    rA := 0;
    M  := null;
    edges := null;
    if typeStr == "B" then (
        rA = n + 1;
        M = mutableMatrix(ZZ, rA, rA);
        for i from 0 to rA - 1 do M_(i, i) = 2;
        for i from 0 to n - 2 do (
            M_(i, i + 1) = -1;
            M_(i + 1, i) = -1;
        );
        -- Fork: simple roots n and n+1 (1-indexed) both attach to n-1.
        -- So C[n-1][n+1] = -1 and C[n+1][n-1] = -1 (1-indexed),
        -- i.e., M[n-2, n] and M[n, n-2] (0-indexed).
        M_(n - 2, n) = -1;
        M_(n, n - 2) = -1;
        matrix M
    )
    else if typeStr == "C" then (
        rA = 2 * n - 1;
        M = mutableMatrix(ZZ, rA, rA);
        for i from 0 to rA - 1 do M_(i, i) = 2;
        for i from 0 to rA - 2 do (
            M_(i, i + 1) = -1;
            M_(i + 1, i) = -1;
        );
        matrix M
    )
    else if typeStr == "F" then (
        rA = 6;
        M = mutableMatrix(ZZ, rA, rA);
        for i from 0 to rA - 1 do M_(i, i) = 2;
        edges = { (0, 2), (1, 3), (2, 3), (3, 4), (4, 5) };
        for e in edges do (
            M_(e#0, e#1) = -1;
            M_(e#1, e#0) = -1;
        );
        matrix M
    )
    else if typeStr == "G" then (
        rA = 4;
        M = mutableMatrix(ZZ, rA, rA);
        for i from 0 to rA - 1 do M_(i, i) = 2;
        edges = { (0, 1), (1, 2), (1, 3) };
        for e in edges do (
            M_(e#0, e#1) = -1;
            M_(e#1, e#0) = -1;
        );
        matrix M
    )
    else error("gapAmbientCartan: unknown type " | typeStr)
)

-- 1-indexed permutation as a List: sigmaPerm#(i-1) = i^sigma.
gapSigmaPerm = (typeStr, n) -> (
    rA := 0;
    if typeStr == "B" then (
        rA = n + 1;
        for i from 1 to rA list (
            if i == n then n + 1 else if i == n + 1 then n else i
        )
    )
    else if typeStr == "C" then (
        rA = 2 * n - 1;
        for i from 1 to rA list (
            if i < n then 2 * n - i
            else if i > n then 2 * n - i
            else i
        )
    )
    else if typeStr == "F" then (
        -- (1,6)(3,5)
        for i from 1 to 6 list (
            if i == 1 then 6
            else if i == 6 then 1
            else if i == 3 then 5
            else if i == 5 then 3
            else i
        )
    )
    else if typeStr == "G" then (
        -- (1,3,4)
        for i from 1 to 4 list (
            if i == 1 then 3
            else if i == 3 then 4
            else if i == 4 then 1
            else i
        )
    )
    else error("gapSigmaPerm: unknown type " | typeStr)
)

gapEpsmat = (typeStr, n) -> (
    rA := 0;
    M  := null;
    if typeStr == "B" then (
        rA = n + 1;
        M = mutableMatrix(ZZ, rA, rA);
        for i from 0 to rA - 1 do for j from 0 to rA - 1 do M_(i, j) = 1;
        for i from 0 to n - 2 do (
            M_(i + 1, i) = -1;
            M_(i, i) = -1;
        );
        M_(n, n - 2) = -1;
        M_(n - 1, n - 1) = -1;
        M_(n, n) = -1;
        matrix M
    )
    else if typeStr == "C" then (
        rA = 2 * n - 1;
        M = mutableMatrix(ZZ, rA, rA);
        for i from 0 to rA - 1 do for j from 0 to rA - 1 do M_(i, j) = 1;
        for i from 0 to n - 2 do (
            M_(i, i + 1) = -1;
            M_(i, i) = -1;
        );
        for i from n - 1 to 2 * n - 3 do (
            M_(i + 1, i) = -1;
            M_(i, i) = -1;
        );
        M_(2 * n - 2, 2 * n - 2) = -1;
        matrix M
    )
    else if typeStr == "F" then (
        rA = 6;
        M = mutableMatrix(ZZ, rA, rA);
        for i from 0 to rA - 1 do for j from 0 to rA - 1 do M_(i, j) = 1;
        for i from 0 to 5 do M_(i, i) = -1;
        M_(0, 2) = -1;
        M_(2, 3) = -1;
        M_(4, 3) = -1;
        M_(5, 4) = -1;
        M_(1, 3) = -1;
        matrix M
    )
    else if typeStr == "G" then (
        rA = 4;
        M = mutableMatrix(ZZ, rA, rA);
        for i from 0 to rA - 1 do for j from 0 to rA - 1 do M_(i, j) = 1;
        for i from 0 to 3 do M_(i, i) = -1;
        M_(0, 1) = -1;
        M_(3, 1) = -1;
        M_(2, 1) = -1;
        matrix M
    )
    else error("gapEpsmat: unknown type " | typeStr)
)

gapFoldOrder = (typeStr, n) -> if typeStr == "G" then 3 else 2

-- 1-indexed permutation taking GAP simple-orbit order to KacMoody simple-root order:
-- KM_i = GAP_{perm#(i-1)}.  The non-trivial case is F_4.
gapToKMPerm = (typeStr, n) -> (
    if typeStr == "B" then for i from 1 to n list i
    else if typeStr == "C" then for i from 1 to n list i
    else if typeStr == "G" then for i from 1 to 2 list i
    else if typeStr == "F" then {2, 4, 3, 1}
    else error("gapToKMPerm: unknown type " | typeStr)
)

-- Apply a GAP-style permutation to a vector: GAP's Permuted(list, sigma)
-- returns a new list newL with newL[i^sigma] = list[i] (1-indexed).
sigmaApplyVec = (sigmaPerm, v) -> (
    rA := #sigmaPerm;
    out := new MutableList from for j from 0 to rA - 1 list 0;
    for i from 1 to rA do
        out#(sigmaPerm#(i - 1) - 1) = v#(i - 1);
    new List from out
)

----------------------------------------------------------------------
-- chevalleyConstantsNonADE: structure constants via GAP-style folding
----------------------------------------------------------------------

chevalleyConstantsNonADE = (Akm, typeStr, n) -> (
    -- Akm is the KacMoody-convention non-ADE Cartan matrix.
    ambient := gapAmbientCartan(typeStr, n);
    sigmaPerm := gapSigmaPerm(typeStr, n);
    epsAmbient := gapEpsmat(typeStr, n);
    d := gapFoldOrder(typeStr, n);
    rA := numrows ambient;

    -- Ambient positive roots.
    posAmbient := positiveRootsList ambient;

    -- Set of ambient roots (positive and negative) for fast lookup.
    ambientRootSet := new MutableHashTable;
    for v in posAmbient do (
        ambientRootSet#v = true;
        ambientRootSet#(negV v) = true;
    );
    isAmbientRoot := v -> ambientRootSet#?v;

    -- sigma-orbit of an ambient root.
    orbitOf := r -> (
        orb := {r};
        cur := sigmaApplyVec(sigmaPerm, r);
        while cur != r do (
            orb = append(orb, cur);
            cur = sigmaApplyVec(sigmaPerm, cur);
        );
        orb
    );

    -- Group ambient positive roots into sigma-orbits.  GAP's convention:
    -- enumerate ambient simple roots in order 1..rA first (so the first n
    -- orbits are the simple non-ADE orbits, in GAP's standard ordering);
    -- then process non-simple positive roots.
    seenAmbient := new MutableHashTable;
    orbitsList := {};
    for i from 1 to rA do (
        s := for j from 0 to rA - 1 list (if j == i - 1 then 1 else 0);
        if seenAmbient#?s then continue;
        orb := orbitOf s;
        for v in orb do seenAmbient#v = true;
        orbitsList = append(orbitsList, orb);
    );
    -- Then non-simple ambient positive roots.
    for r in posAmbient do (
        if seenAmbient#?r then continue;
        orb := orbitOf r;
        for s in orb do seenAmbient#s = true;
        orbitsList = append(orbitsList, orb);
    );

    -- For each orbit, the corresponding non-ADE root (in ambient coords) is
    -- (1/d) * sum of orbit elements.
    nonADERootInAmbient := orb -> (
        sumR := for k from 0 to rA - 1 list 0_QQ;
        for s in orb do
            sumR = for k from 0 to rA - 1 list sumR#k + s#k;
        for k from 0 to rA - 1 list sumR#k / #orb
    );
    nonADERootsAmbient := orbitsList / nonADERootInAmbient;

    -- The first n orbits give the simple non-ADE roots in GAP enumeration order.
    simpleNonADEAmbient := nonADERootsAmbient_{0 .. n - 1};

    -- KacMoody's simple roots in ambient coordinates: KM_i = GAP_{perm(i)}.
    perm := gapToKMPerm(typeStr, n);
    kmSimpleAmbient := for i from 1 to n list simpleNonADEAmbient#(perm#(i - 1) - 1);

    -- Build M: rA x n matrix whose columns are kmSimpleAmbient.
    M := transpose matrix kmSimpleAmbient;

    -- Solve M c = y for c.  Use //, then verify the solution.
    convertAmbientToKM := y -> (
        yMat := transpose matrix {y};
        xMat := yMat // M;
        if M * xMat != yMat then
            error("convertAmbientToKM: ambient root not in span of non-ADE simples");
        for k from 0 to n - 1 list xMat_(k, 0)
    );

    -- Convert each non-ADE root to KacMoody coordinates, integerise.
    nonADERootsKM := for v in nonADERootsAmbient list (
        c := convertAmbientToKM v;
        for x in c list (
            if instance(x, ZZ) then x
            else if denominator x == 1 then numerator x
            else error("non-ADE root has non-integer coord: " | toString x)
        )
    );

    -- Sanity check: nonADERootsKM should match the set of KacMoody positive roots.
    kmPosRoots := positiveRootsList Akm;
    kmSet := new MutableHashTable;
    for v in kmPosRoots do kmSet#v = true;
    for v in nonADERootsKM do
        if not kmSet#?v then
            error("internal: non-ADE root " | toString v | " absent from KacMoody positive roots");
    if #nonADERootsKM != #kmPosRoots then
        error "internal: non-ADE positive root count mismatch";

    -- Build index from KacMoody positive root to its place in nonADERootsKM (= orbit index).
    orbitIndexOf := new MutableHashTable;
    for k from 0 to #nonADERootsKM - 1 do
        orbitIndexOf#(nonADERootsKM#k) = k;

    primesOfRoot := alpha -> orbitsList#(orbitIndexOf#alpha);

    -- isPosRoot for non-ADE: v has all entries >= 0 with at least one positive.
    nonADEPosSet := new MutableHashTable;
    for v in nonADERootsKM do nonADEPosSet#v = true;
    isPosRootKM := v -> nonADEPosSet#?v;

    -- Set of non-ADE roots (positive and negative).
    nonADERootSet := new MutableHashTable;
    for v in nonADERootsKM do (
        nonADERootSet#v = true;
        nonADERootSet#(negV v) = true;
    );
    isNonADERoot := v -> nonADERootSet#?v;

    -- Chain depth q: number of steps such that v - k * w is a non-ADE root.
    -- Specifically, max k >= 1 such that v - k*w lies in nonADERootSet.
    -- Used in the GAP formula |N(alpha, beta)| = q + 1.
    chainQ := (v, w) -> (
        q := 0;
        cur := for k from 0 to #v - 1 list v#k - w#k;
        while isNonADERoot cur do (
            q = q + 1;
            cur = for k from 0 to #v - 1 list cur#k - w#k;
        );
        q
    );

    -- Chain depth in the OTHER direction (used in mixed pair formulas):
    -- max k >= 1 such that v + k*w is a non-ADE root.
    chainQRev := (v, w) -> (
        q := 0;
        cur := for k from 0 to #v - 1 list v#k + w#k;
        while isNonADERoot cur do (
            q = q + 1;
            cur = for k from 0 to #v - 1 list cur#k + w#k;
        );
        q
    );

    -- Find a representative pair (r, r') in primes(alphaIdx) x primes(betaIdx)
    -- such that r + r' is an ambient root.  Returns (r, r') or null.
    findAmbientPair := (orbA, orbB) -> (
        for r in orbA do
            for r1 in orbB do
                if isAmbientRoot(for k from 0 to rA - 1 list r#k + r1#k) then
                    return (r, r1);
        null
    );

    -- Find (r, r') with r - r' an ambient root.  Returns (r, r') or null.
    findAmbientDiffPair := (orbA, orbB) -> (
        for r in orbA do
            for r1 in orbB do
                if isAmbientRoot(for k from 0 to rA - 1 list r#k - r1#k) then
                    return (r, r1);
        null
    );

    Nresult := new MutableHashTable;

    -- Same-sign positive-positive case.
    for i from 0 to #nonADERootsKM - 1 do (
        for j from 0 to #nonADERootsKM - 1 do (
            alpha := nonADERootsKM#i;
            beta := nonADERootsKM#j;
            ssum := for k from 0 to n - 1 list alpha#k + beta#k;
            if not isPosRootKM ssum then continue;
            -- Find ambient pair.
            orbA := primesOfRoot alpha;
            orbB := primesOfRoot beta;
            pair := findAmbientPair(orbA, orbB);
            if pair === null then continue;
            (r, r1) := pair;
            q := chainQ(alpha, beta);
            e := epsilonFunction(epsAmbient, r, r1) * (q + 1);
            Nresult#(alpha, beta) = e;
            Nresult#(negV alpha, negV beta) = - e;
        );
    );

    -- Mixed-sign cases (positive alpha, negative beta).
    for i from 0 to #nonADERootsKM - 1 do (
        for j from 0 to #nonADERootsKM - 1 do (
            alpha := nonADERootsKM#i;
            beta := nonADERootsKM#j;
            -- Consider the pair (alpha, -beta) with alpha - beta the resulting drop.
            diff := for k from 0 to n - 1 list alpha#k - beta#k;
            if all(diff, x -> x == 0) then continue;     -- alpha = beta: Cartan
            orbA := primesOfRoot alpha;
            orbB := primesOfRoot beta;
            -- GAP: find r, r' such that r - r' is an ambient root.
            pair := findAmbientDiffPair(orbA, orbB);
            if pair === null then continue;
            (r, r1) := pair;
            -- chain: q = max k >= 1 such that alpha + k*beta is non-ADE root (note +).
            q := chainQRev(alpha, beta);
            negR1 := negV r1;
            eVal := epsilonFunction(epsAmbient, r, negR1) * (q + 1);

            if isPosRootKM diff then (
                -- alpha - beta is a non-ADE positive root.
                Nresult#(alpha, negV beta) = - eVal;
                Nresult#(negV beta, alpha) = + eVal;
            )
            else (
                negDiff := negV diff;
                if isPosRootKM negDiff then (
                    -- alpha - beta is a non-ADE negative root.
                    Nresult#(alpha, negV beta) = + eVal;
                    Nresult#(negV beta, alpha) = - eVal;
                );
            );
        );
    );

    Nresult
)

----------------------------------------------------------------------
-- Layer 4: Weyl module V_Z(lambda) over Z for all finite Cartan types
----------------------------------------------------------------------
--
-- A WeylModuleZ object stores a complete description of V_Z(lambda):
-- the dominant integral highest weight, a PBW basis (over Z) indexed
-- by exponent tuples in the positive roots, and the action of the
-- simple Chevalley generators f_i, e_i, h_i as integer matrices
-- between weight strata.
--
-- The construction proceeds in three steps:
--
--   (1) Build the Verma module M_Z(lambda) restricted to weights of
--       V(lambda), with PBW basis indexed by tuples
--       (c_1,...,c_N) in N^N where N = |R^+|.  At weight mu, the
--       basis size is K(lambda - mu) (Kostant partition number).
--
--   (2) Compute the action of f_i on M_Z(lambda) via PBW commutation
--       (using the structure constants from Layer 2).  In ADE the
--       fundamental identity is
--           [f_alpha, f_beta^{(k)}] =  -N(alpha,beta) * f_beta^{(k-1)}
--                                                    * f_{alpha+beta}
--       when alpha + beta is a positive root and alpha != beta;
--       zero otherwise.  This relies on the fact that 2*beta + alpha
--       is never a root in ADE, so f_beta and f_{alpha+beta} commute.
--
--   (3) Compute the kernel of M_Z(lambda) -> V_Z(lambda) by closing
--       the seed vectors {f_{alpha_i}^{(lambda_i+1)} v_lambda} under
--       f_j actions (using BFS).  Since the seeds are singular
--       (annihilated by every e_j), the U(g)-submodule they generate
--       equals the U(n^-)-submodule, which suffices.
--
-- Quotienting M_Z(lambda)_mu by the kernel gives V_Z(lambda)_mu.  We
-- pick a Z-basis of V_Z(lambda)_mu by selecting "free" PBW tuples --
-- those whose indices are pivots after row-reducing the kernel matrix
-- in the PBW-tuple basis.  The action of f_i, e_i, h_i on V_Z(lambda)
-- is then read off as integer matrices between these chosen bases.
--
-- All weights below use the fundamental-weight (Dynkin) convention,
-- matching KacMoody and Doty.  Roots are stored in simple-root
-- coordinates throughout.

WeylModuleZ = new Type of HashTable

----------------------------------------------------------------------
-- Internal helpers for Layer 4
----------------------------------------------------------------------

-- Replace L#i by v, returning a new list (M2 has no built-in setAt).
setAt = (L, i, v) -> for k from 0 to #L - 1 list (if k == i then v else L#k)

-- Convert a 1xr matrix or list to a list of length r.
toListR = (v, r) -> (
    if instance(v, Matrix) then for j from 0 to r - 1 list v_(0, j)
    else if instance(v, List) then v
    else error "toListR: expected a Matrix or List"
)

-- Convert a Dynkin-coord weight to simple-root-coord weight (over QQ).
-- Internally we keep weights in Dynkin coords for hashing, but use
-- simple-root coords for "drop" computations (lambda - mu).
dropInSimple = (A, lambdaDynkin, muDynkin) -> (
    -- Returns lambda - mu, expressed in simple-root coords (List of QQ).
    r := numrows A;
    lamS := toSimple(A, lambdaDynkin);
    muS  := toSimple(A, muDynkin);
    for k from 0 to r - 1 list lamS#k - muS#k
)

-- Componentwise non-negative-integer check.
isNonnegIntList = L -> all(L, x -> (
    if instance(x, ZZ) then x >= 0
    else if instance(x, QQ) then (x >= 0 and denominator x == 1)
    else false
))

-- Convert a List of QQ that is actually integer-valued into a List of ZZ.
qqToZZ = L -> for x in L list (
    if instance(x, ZZ) then x
    else if denominator x == 1 then numerator x
    else error("qqToZZ: non-integer entry " | toString x)
)

-- Canonical representation-theoretic order: increasing height of the drop
-- lambda-mu in the positive root lattice, followed by reverse
-- lexicographic Dynkin coordinates.  This is independent of hash-table key
-- order and is used for every public list of module weights.
canonicalModuleWeightOrder = (A, lambda, weightList) -> (
    decorated := for mu in weightList list
        (sum dropInSimple(A, lambda, mu), for x in mu list -x, mu);
    for item in sort decorated list item#2
)

----------------------------------------------------------------------
-- pbwTuples: enumerate PBW exponent tuples summing to a given drop
----------------------------------------------------------------------

-- Given the ordered list of positive roots and a target sum
-- "delta" (a non-negative integer combination of simple roots, in
-- simple-root coords), return the list of all tuples
-- (c_1,...,c_N) with c_i >= 0 and sum_i c_i * beta_i = delta.

pbwTuples = method()
pbwTuples (List, List) := (positiveRoots, delta) -> (
    N := #positiveRoots;
    r := #delta;
    -- Recursive enumeration: choose c_1, recurse on (delta - c_1*beta_1,
    -- positiveRoots[2..N]).
    helper := null;
    helper = (idx, remaining, partial) -> (
        if idx >= N then (
            if all(remaining, x -> x == 0) then
                return {partial}
            else
                return {};
        );
        beta := positiveRoots#idx;
        -- Find max c such that c * beta_k <= remaining_k for all k
        -- with beta_k > 0.
        maxC := infinity;
        for k from 0 to r - 1 do (
            if beta#k > 0 then (
                cap := remaining#k // beta#k;
                if cap < maxC then maxC = cap;
            );
        );
        if maxC === infinity then maxC = 0;     -- beta is zero (shouldn't happen for a root)
        if maxC < 0 then return {};
        result := {};
        for c from 0 to maxC do (
            newRem := for k from 0 to r - 1 list remaining#k - c * beta#k;
            sub := helper(idx + 1, newRem, append(partial, c));
            result = result | sub;
        );
        result
    );
    helper(0, delta, {})
)

----------------------------------------------------------------------
-- applyChevalley: PBW commutation for f_alpha or e_alpha
----------------------------------------------------------------------
--
-- Internal driver shared between f-action and e-action computations.
-- For action="f", computes f_alpha applied to the PBW monomial m_c
-- and returns a HashTable from new tuples to integer coefficients.
-- For action="e", does the same for e_alpha.
--
-- The two cases share the same outer recursion (commute alpha past
-- each f_{beta_p}^{(c_p)} factor) but differ in two places:
--   * the "absorption" step (when the moving factor matches beta_p);
--   * the bracket sign and direction.
--
-- For ADE, the bracket of e_alpha (alpha simple) with f_beta^{(k)}
-- (beta a positive root, beta != alpha) yields a new factor
-- f_{beta - alpha} when beta - alpha is a positive root, with
-- coefficient N(alpha, -beta) (using the chevalleyConstants table).
--
-- The "weight of the suffix" is needed for the e-absorption case
-- (the scalar h_i - k + 1 acts on the suffix as
-- (suffix-weight, alpha_i^vee) - k + 1).  Callers must supply
-- "lambdaDynkin" (the highest weight) so we can compute suffix
-- weights from PBW tuples.

applyChevalley = (W, action, simpleIdx, c, lambdaDynkin) -> (
    -- Currently only action == "f" is implemented; action == "e" is deferred
    -- to a future version (it requires left-moving PBW commutation, or a
    -- weight-depth-induction approach using [e_i, f_j] = delta_{ij} h_i).
    if action =!= "f" then
        error "applyChevalley: only action == \"f\" is implemented in this version";

    validateSimpleRootIndex(W#"cartan", simpleIdx, "applyChevalley");

    -- Memoize on (action, simpleIdx, c) -- the result is independent of
    -- lambdaDynkin in the f-case.  vermaKernel and buildFAction call this
    -- with the same (cSrc, simpleIdx) many times, so caching yields a
    -- large speedup.
    cache := if W#?"applyChevalleyCache" then W#"applyChevalleyCache" else null;
    cacheKey := (action, simpleIdx, c);
    if cache =!= null and cache#?cacheKey then return cache#cacheKey;

    A := W#"cartan";
    posRoots := W#"positiveRoots";
    rootIdx := W#"rootIndex";
    Nconst := W#"chevalleyConstants";
    isPosRootSet := W#"isPosRootSet";
    rOver := numrows A;
    Nover := #posRoots;

    -- alpha_i in simple-root coords: unit vector at position i.
    alphaI := for k from 0 to rOver - 1 list (if k == simpleIdx - 1 then 1 else 0);

    accumulator := new MutableHashTable;
    addTo := (key, val) -> (
        cur := if accumulator#?key then accumulator#key else 0;
        newVal := cur + val;
        if newVal == 0 then (
            if accumulator#?key then remove(accumulator, key);
        ) else accumulator#key = newVal;
    );

    helper := null;
    helper = (gamma, cur, p, coeff) -> (
        -- gamma: current moving positive root (List in simple-root coords).
        -- cur: current PBW tuple (List of ZZ).
        -- p: 1-based PBW position to commute past next.
        -- coeff: accumulated integer coefficient.
        if not rootIdx#?gamma then return;
        jGamma := rootIdx#gamma;
        if p > jGamma then error "applyChevalley: passed PBW position";

        if p == jGamma then (
            -- f-absorption: f_alpha * f_alpha^{(k)} = (k+1) * f_alpha^{(k+1)}.
            cIdx := jGamma - 1;
            absorbC := setAt(cur, cIdx, cur#cIdx + 1);
            addTo(absorbC, coeff * (cur#cIdx + 1));
            return;
        );

        betaP := posRoots#(p - 1);
        cP := cur#(p - 1);

        -- Pass-through: gamma keeps moving forward.
        helper(gamma, cur, p + 1, coeff);

        -- Bracket: [f_gamma, f_{betaP}^{(c_p)}] in U_Z(g).
        --
        -- For ADE (chain length <= 1): only one term,
        --     -N(gamma, betaP) * f_{betaP}^{(c_p - 1)} * f_{gamma + betaP}.
        --
        -- For non-ADE (chain length 2: B/C/F when 2*betaP + gamma is a root):
        --     additional term
        --       -(1/2) N(gamma, betaP) N(betaP, gamma + betaP)
        --                * f_{betaP}^{(c_p - 2)} * f_{2 betaP + gamma}.
        --
        -- For G_2 (chain length 3): yet another term involving f_{3 betaP + gamma}.
        -- We extend up to chain length 3 here.

        if cP > 0 and gamma =!= betaP then (
            sumRoot := for k from 0 to rOver - 1 list gamma#k + betaP#k;
            if isPosRootSet#?sumRoot then (
                nVal := if Nconst#?(gamma, betaP) then Nconst#(gamma, betaP) else 0;
                if nVal != 0 then (
                    bracketCoeff := - nVal;
                    newC := setAt(cur, p - 1, cP - 1);
                    helper(sumRoot, newC, p + 1, coeff * bracketCoeff);
                );

                -- Chain length 2: 2*betaP + gamma is also a root.
                if cP >= 2 then (
                    twoBetaPlusGamma := for k from 0 to rOver - 1 list 2 * betaP#k + gamma#k;
                    if isPosRootSet#?twoBetaPlusGamma then (
                        n1 := if Nconst#?(gamma, betaP) then Nconst#(gamma, betaP) else 0;
                        n2 := if Nconst#?(betaP, sumRoot) then Nconst#(betaP, sumRoot) else 0;
                        prod := n1 * n2;
                        if prod != 0 then (
                            if prod % 2 != 0 then
                                error("applyChevalley: chain-2 correction has half-integer coefficient (j=" |
                                      toString simpleIdx | ", root=" | toString gamma | ")");
                            chain2Coeff := -(prod // 2);
                            newC2 := setAt(cur, p - 1, cP - 2);
                            helper(twoBetaPlusGamma, newC2, p + 1, coeff * chain2Coeff);

                            -- Chain length 3: 3*betaP + gamma is a root (G_2 case).
                            if cP >= 3 then (
                                threeBetaPlusGamma := for k from 0 to rOver - 1 list 3 * betaP#k + gamma#k;
                                if isPosRootSet#?threeBetaPlusGamma then (
                                    n3 := if Nconst#?(betaP, twoBetaPlusGamma) then Nconst#(betaP, twoBetaPlusGamma) else 0;
                                    -- The chain-3 coefficient is
                                    -- -(1/6) N(gamma, betaP) N(betaP, gamma + betaP) N(betaP, 2 betaP + gamma)
                                    triple := n1 * n2 * n3;
                                    if triple != 0 then (
                                        if triple % 6 != 0 then
                                            error("applyChevalley: chain-3 correction has non-integer coefficient (j=" |
                                                  toString simpleIdx | ", root=" | toString gamma | ")");
                                        chain3Coeff := - triple // 6;
                                        newC3 := setAt(cur, p - 1, cP - 3);
                                        helper(threeBetaPlusGamma, newC3, p + 1, coeff * chain3Coeff);
                                    );
                                );
                            );
                        );
                    );
                );
            );
        );
    );

    helper(alphaI, c, 1, 1);
    -- Convert mutable accumulator to immutable HashTable for caching
    -- (and to avoid accidental aliasing with downstream callers).
    result := new HashTable from accumulator;
    if cache =!= null then cache#cacheKey = result;
    result
)

----------------------------------------------------------------------
-- Internal: build the f-action matrix on M(lambda)|_{V-weights}
----------------------------------------------------------------------
-- For each pair (mu, mu - alpha_i) of V(lambda) weights, build the
-- matrix of f_i: M(lambda)_mu -> M(lambda)_{mu - alpha_i} in the PBW
-- basis.  Returned as a HashTable from (mu, i) to a Matrix over ZZ
-- whose columns are indexed by PBW tuples at mu and rows by tuples
-- at mu - alpha_i.

vermaFAction = (W) -> (
    A := W#"cartan";
    rOver := numrows A;
    posRoots := W#"positiveRoots";
    weightsList := W#"weightsList";        -- List of weights in Dynkin coords
    weightSet := W#"weightSet";              -- HashTable: weight (Dynkin) -> true
    pbwAt := W#"pbwAt";                      -- HashTable: weight (Dynkin) -> List of tuples
    pbwIndexAt := W#"pbwIndexAt";            -- HashTable: weight -> HashTable tuple -> index
    lambdaDynkin := W#"highestWeight";

    matrices := new MutableHashTable;

    for mu in weightsList do (
        for i from 1 to rOver do (
            -- The action f_i takes weight mu+alpha_i to weight mu.
            -- For us we want a matrix from V_{mu+alpha_i} to V_mu, but
            -- here we work in M(lambda).  Compute mu+alpha_i and skip
            -- if not a V-weight.
            alphaIDynkin := for j from 0 to rOver - 1 list A_(j, i - 1);  -- alpha_i in Dynkin coords (j-th column of A)
            sourceWeight := for j from 0 to rOver - 1 list mu#j + alphaIDynkin#j;
            if not weightSet#?sourceWeight then continue;

            sourceTuples := pbwAt#sourceWeight;
            targetTuples := pbwAt#mu;
            targetIdx := pbwIndexAt#mu;

            nSrc := #sourceTuples;
            nTgt := #targetTuples;
            M := mutableMatrix(ZZ, nTgt, nSrc);

            for jCol from 0 to nSrc - 1 do (
                cSrc := sourceTuples#jCol;
                act := applyChevalley(W, "f", i, cSrc, lambdaDynkin);
                for newC in keys act do (
                    if targetIdx#?newC then (
                        iRow := targetIdx#newC;
                        M_(iRow, jCol) = M_(iRow, jCol) + act#newC;
                    );
                    -- Else: newC is not at a V-weight tuple list; ignored.
                );
            );

            matrices#(mu, i) = matrix M;
        );
    );

    matrices
)

----------------------------------------------------------------------
-- Internal: compute the Verma kernel at V-weights
----------------------------------------------------------------------
-- BFS over the U(n^-)-orbit of the seed vectors
-- {f_{alpha_i}^{(lambda_i + 1)} v_lambda} via f_j actions.
-- At each weight reached (V-weight or non-V-weight) we maintain a
-- column-rank tracker.  We add a new vector to the kernel only if it
-- is independent of those already seen at that weight, and only
-- propagate further from independent vectors.
--
-- Termination: at each weight the rank cannot exceed K(lambda - mu),
-- so the BFS produces at most a bounded number of independent vectors
-- per weight.  We stop globally when the kernel rank at every
-- V-weight matches the target K(lambda - mu) - dim V(lambda)_mu.

vermaKernel = (W) -> (
    A := W#"cartan";
    rOver := numrows A;
    posRoots := W#"positiveRoots";
    rootIdx := W#"rootIndex";
    weightsList := W#"weightsList";
    weightSet := W#"weightSet";
    pbwAt := W#"pbwAt";
    pbwIndexAt := W#"pbwIndexAt";
    lambdaDynkin := W#"highestWeight";
    weightMultAt := W#"weightMultAt";

    -- Compute height of a Dynkin-coord weight in simple-root coords.
    -- Height = sum of simple-root coefficients.
    heightOfDynkin := mu -> (
        s := toSimple(A, mu);
        h := 0_QQ;
        for x in s do h = h + x;
        h
    );

    -- Lowest V-weight height: BFS will not propagate below this height,
    -- since further f-applications only go lower and cannot re-enter
    -- the V-weight diamond.
    lowestVHt := min apply(weightsList, mu -> heightOfDynkin mu);

    -- For each V-weight, kernelAtWeight#mu = matrix over QQ whose columns
    -- are independent kernel vectors found so far, indexed by PBW tuples
    -- at mu.
    kernelAtWeight := new MutableHashTable;
    for mu in weightsList do kernelAtWeight#mu = matrix(QQ, table(#(pbwAt#mu), 0, (i, j) -> 0));

    -- Per-weight column-echelon state for fast linear-independence testing.
    --   echelonAt#mu = list of column matrices in row-echelon form (one per pivot)
    --   pivotsAt#mu = list of pivot row indices (one per echelon column)
    -- An incoming vector v is reduced against the echelon in O(rows * #cols);
    -- if zero after reduction it is dependent, otherwise we record a new
    -- pivot.  This replaces a much more expensive per-call `solve` (which
    -- was O(rows * cols^2) for QQ matrices).  Total work for vermaKernel
    -- drops from O(K^4) to O(K^3) per weight, and in practice the
    -- difference is dramatic (~100x) for non-ADE high-rank Weyl modules
    -- such as G_2 V(3,0) where K = 110 at the deepest weights.
    echelonAt := new MutableHashTable;
    pivotsAt := new MutableHashTable;

    -- Per-weight "current rank" counters (avoid recomputing rank).
    seenRankAt := new MutableHashTable;
    kernelRankAt := new MutableHashTable;
    targetKernelRankAt := new MutableHashTable;
    for mu in weightsList do (
        kernelRankAt#mu = 0;
        targetKernelRankAt#mu = #(pbwAt#mu) - weightMultAt#mu;
    );

    -- Track total kernel rank vs total target.
    totalTarget := sum apply(weightsList, mu -> targetKernelRankAt#mu);
    totalKernelRank := 0;

    -- Add v at weight mu.  Returns true if it was independent (and added),
    -- false otherwise.  v is passed as a List of QQ.  Uses incremental
    -- column-echelon over QQ; the echelon stores each column as a
    -- (List, pivotRow, pivotVal) triple to skip repeated indexing.
    tryAdd := (mu, vL, isVWeight) -> (
        nT := #vL;
        cols := if echelonAt#?mu then echelonAt#mu else {};
        pivs := if pivotsAt#?mu then pivotsAt#mu else {};
        nCols := #cols;

        -- Reduce against existing pivots.
        for k from 0 to nCols - 1 do (
            pk := pivs#k;
            cv := vL#pk;
            if cv == 0 then continue;
            (ckList, ckPivot, ckPivotVal) := cols#k;
            factor := cv / ckPivotVal;
            vL = for r from 0 to nT - 1 list (
                ckr := ckList#r;
                if ckr == 0 then vL#r else vL#r - factor * ckr
            );
        );

        -- Find first nonzero row (= new pivot index).
        newP := -1;
        for r from 0 to nT - 1 do (
            if vL#r != 0 then (newP = r; break);
        );
        if newP < 0 then return false;     -- dependent

        newPivotVal := vL#newP;
        echelonAt#mu = append(cols, (vL, newP, newPivotVal));
        pivotsAt#mu = append(pivs, newP);
        seenRankAt#mu = nCols + 1;

        -- For V-weights, record the (reduced) vector as a kernel column.
        if isVWeight then (
            kernelMat := mutableMatrix(QQ, nT, 1);
            for r from 0 to nT - 1 do kernelMat_(r, 0) = vL#r;
            kernelAtWeight#mu = (kernelAtWeight#mu) | matrix kernelMat;
            kernelRankAt#mu = kernelRankAt#mu + 1;
            totalKernelRank = totalKernelRank + 1;
        );
        true
    );

    -- Determine target tuples for an arbitrary weight (with caching).
    tuplesAt := mu -> (
        if pbwAt#?mu then return pbwAt#mu;
        muS := toSimple(A, mu);
        lamS := toSimple(A, lambdaDynkin);
        deltaQ := for k from 0 to rOver - 1 list lamS#k - muS#k;
        if any(deltaQ, x -> x < 0) then (
            pbwAt#mu = {};
            pbwIndexAt#mu = new MutableHashTable;
            return {};
        );
        if not all(deltaQ, x -> denominator x == 1) then (
            pbwAt#mu = {};
            pbwIndexAt#mu = new MutableHashTable;
            return {};
        );
        deltaZ := qqToZZ deltaQ;
        t := pbwTuples(posRoots, deltaZ);
        pbwAt#mu = t;
        idxMap := new MutableHashTable;
        for k from 0 to #t - 1 do idxMap#(t#k) = k;
        pbwIndexAt#mu = idxMap;
        t
    );

    -- Apply f_j to a vector at weight mu, where the vector is given as a
    -- List of QQ.  Returns (newMu, newList) or null.  Working in Lists
    -- (rather than mutableMatrix) avoids per-element-access overhead and
    -- matrix construction roundtrips with tryAdd.
    applyFAt := (mu, jSimple, vL) -> (
        alphaIDynkin := for k from 0 to rOver - 1 list A_(k, jSimple - 1);
        targetMu := for k from 0 to rOver - 1 list mu#k - alphaIDynkin#k;

        -- Skip if target is below the lowest V-weight height.
        targetHt := heightOfDynkin targetMu;
        if targetHt < lowestVHt then return null;

        targetTuples := tuplesAt targetMu;
        if #targetTuples == 0 then return null;
        targetIdx := pbwIndexAt#targetMu;

        srcTuples := tuplesAt mu;
        nv := #vL;
        if nv != #srcTuples then return null;

        nTgt := #targetTuples;
        outL := new MutableList from for k from 0 to nTgt - 1 list 0_QQ;
        anyNonzero := false;
        for srcIdx from 0 to nv - 1 do (
            coeff := vL#srcIdx;
            if coeff == 0 then continue;
            cSrc := srcTuples#srcIdx;
            act := applyChevalley(W, "f", jSimple, cSrc, lambdaDynkin);
            for newC in keys act do (
                if not targetIdx#?newC then continue;
                idx := targetIdx#newC;
                outL#idx = outL#idx + coeff * act#newC;
                anyNonzero = true;
            );
        );
        if not anyNonzero then return null;
        outAsList := toList outL;
        if all(outAsList, x -> x == 0) then return null;
        (targetMu, outAsList)
    );

    -- Initialize seeds (each seed is a List of QQ entries).
    queue := new MutableList;
    qHead := 0;
    for i from 1 to rOver do (
        lamI := lambdaDynkin#(i - 1);
        seedExp := lamI + 1;
        alphaI := for k from 0 to rOver - 1 list (if k == i - 1 then 1 else 0);
        if not rootIdx#?alphaI then continue;
        jI := rootIdx#alphaI;
        seedTuple := for k from 0 to #posRoots - 1 list (if k == jI - 1 then seedExp else 0);
        seedWeight := for k from 0 to rOver - 1 list lambdaDynkin#k - seedExp * A_(k, i - 1);
        seedHt := heightOfDynkin seedWeight;
        if seedHt < lowestVHt then continue;
        seedTuples := tuplesAt seedWeight;
        seedIdx := pbwIndexAt#seedWeight;
        if not seedIdx#?seedTuple then continue;
        seedL := for k from 0 to #seedTuples - 1 list (if k == seedIdx#seedTuple then 1_QQ else 0_QQ);
        queue#(#queue) = (seedWeight, seedL);
    );

    -- BFS.  Only independent vectors are propagated, and the weight range
    -- is bounded below, so the queue is finite.
    iter := 0;
    verbose := W#?"verboseProfile" and W#"verboseProfile";

    while qHead < #queue do (
        iter = iter + 1;
        (mu, v) := queue#qHead;
        qHead = qHead + 1;

        -- Already saturated at this weight?  Saturation rank is
        -- K(lambda - mu) (the entire M-basis stratum, for both V- and
        -- non-V-weights).  At V-weights we could stop earlier (at
        -- targetKernelRankAt#mu) but the echelon test below makes the
        -- extra-vector check cheap.
        isV := weightSet#?mu;
        nT := #(tuplesAt mu);
        curRank := if seenRankAt#?mu then seenRankAt#mu else 0;
        if curRank >= nT then continue;

        if not tryAdd(mu, v, isV) then continue;

        -- Early termination: if we've reached the global target, stop.
        if totalKernelRank >= totalTarget then break;

        -- Propagate.
        for jSimple from 1 to rOver do (
            res := applyFAt(mu, jSimple, v);
            if res === null then continue;
            (newMu, newVec) := res;
            queue#(#queue) = (newMu, newVec);
        );
    );
    if verbose then
        << "    [vermaKernel] iter=" << iter << endl;

    for mu in weightsList do (
        if kernelRankAt#mu != targetKernelRankAt#mu then
            error("vermaKernel: incorrect kernel rank at weight " | toString mu |
                  "; got " | toString(kernelRankAt#mu) |
                  ", expected " | toString(targetKernelRankAt#mu));
    );

    kernelAtWeight
)

----------------------------------------------------------------------
-- Choose a V-basis at each V-weight: the "free" PBW tuples whose
-- columns are NOT in the span of the kernel matrix.  Equivalently,
-- after column-reducing the kernel matrix and extracting pivot rows,
-- the non-pivot row indices give the chosen basis.
----------------------------------------------------------------------

chooseVBasis = (W, kernelAtWeight) -> (
    weightsList := W#"weightsList";
    pbwAt := W#"pbwAt";
    weightMultAt := W#"weightMultAt";

    chosenIdxAt := new MutableHashTable;     -- weight -> list of indices into pbwAt#mu
    chosenTuplesAt := new MutableHashTable;  -- weight -> list of tuples (basis of V_mu)

    for mu in weightsList do (
        kernelMat := kernelAtWeight#mu;
        nTotal := #(pbwAt#mu);
        target := weightMultAt#mu;
        expectedKernelRank := nTotal - target;
        if numcols kernelMat != expectedKernelRank or rank kernelMat != expectedKernelRank then
            error("chooseVBasis: incorrect kernel rank at weight " | toString mu);
        if target == 0 then (
            chosenIdxAt#mu = {};
            chosenTuplesAt#mu = {};
            continue;
        );
        if numcols kernelMat == 0 then (
            -- Empty kernel: every PBW tuple is in the V-basis.
            chosenIdxAt#mu = toList(0..nTotal - 1);
            chosenTuplesAt#mu = pbwAt#mu;
            continue;
        );
        -- Greedy basis selection: iterate j from 0 to nTotal-1; include
        -- the standard basis vector e_j iff it is independent (over QQ)
        -- of the current accumulated matrix [kernel | already-chosen e's].
        currentMat := kernelMat;
        currentRank := rank currentMat;
        chosen := {};
        for j from 0 to nTotal - 1 do (
            if #chosen >= target then break;
            ej := mutableMatrix(QQ, nTotal, 1);
            ej_(j, 0) = 1;
            candidate := currentMat | matrix ej;
            if rank candidate > currentRank then (
                chosen = append(chosen, j);
                currentMat = candidate;
                currentRank = currentRank + 1;
            );
        );
        if #chosen != target then
            error("chooseVBasis: rank mismatch at weight " | toString mu |
                  "; #chosen = " | toString(#chosen) |
                  ", target = " | toString target);
        chosenIdxAt#mu = chosen;
        chosenTuplesAt#mu = for j in chosen list (pbwAt#mu)#j;
    );

    (chosenIdxAt, chosenTuplesAt)
)

----------------------------------------------------------------------
-- weylModuleZ: top-level constructor
----------------------------------------------------------------------

weylModuleZ = method(Options => {Verbose => false, WeightFormat => "Dynkin"})
weylModuleZ (Matrix, List) := opts -> (A, lambdaUser) -> (
    if not isCartanMatrix A then
        error "weylModuleZ: input is not a generalized Cartan matrix";
    if cartanType A =!= "finite" then
        error "weylModuleZ: only finite-type Cartan matrices supported";
    lambdaDynkin := toDynkin(A, lambdaUser, opts.WeightFormat);
    pTimeStart := cpuTime();
    pTimer := (label) -> if opts.Verbose then (
        << "  [weylModuleZ] " << label << ": " << (cpuTime() - pTimeStart) << "s" << endl;
        pTimeStart = cpuTime();
    );
    -- Both ADE and non-ADE finite types are supported (chain-aware
    -- divided-power Serre relations handle B/C/F via chain length 2 and
    -- G_2 via chain length 3).
    rOver := numrows A;
    if #lambdaDynkin =!= rOver then
        error("weylModuleZ: highest weight must have length " | toString rOver);
    if not all(lambdaDynkin, x -> x >= 0) then
        error "weylModuleZ: highest weight must be dominant (all Dynkin labels >= 0)";

    -- Pre-build all the cached pieces.
    posRootsMat := cartanPositiveRoots A;
    posRootsRaw := for i from 0 to numrows posRootsMat - 1 list flatten entries posRootsMat^{i};
    -- Sort positive roots by (height ascending, lex).
    sortKey := v -> (sum v, v);
    posRoots := sort apply(posRootsRaw, v -> (sortKey v, v)) / (p -> p#1);
    Nover := #posRoots;

    rootIdx := new MutableHashTable;
    for k from 0 to Nover - 1 do rootIdx#(posRoots#k) = k + 1;     -- 1-based

    isPosRootSet := new MutableHashTable;
    for v in posRoots do isPosRootSet#v = true;

    Nconst := chevalleyConstants A;     -- from Layer 2

    -- Compute weights of V(lambda) and ALL multiplicities in a single
    -- Freudenthal pass.  KacMoody's per-weight `weightMultiplicity`
    -- restarts the recursion every call, which is O(#weights) redundant.
    -- The combined helper here does the work once.
    weightMultAt := weightsAndMults(A, lambdaDynkin);
    weightsList := canonicalModuleWeightOrder(A, lambdaDynkin, keys weightMultAt);
    weightSet := new MutableHashTable;
    for mu in weightsList do weightSet#mu = true;
    pTimer "weights+mults";

    -- Enumerate PBW tuples at each V-weight.
    pbwAt := new MutableHashTable;
    pbwIndexAt := new MutableHashTable;
    lamSimple := toSimple(A, lambdaDynkin);
    for mu in weightsList do (
        muSimple := toSimple(A, mu);
        delta := qqToZZ for k from 0 to rOver - 1 list lamSimple#k - muSimple#k;
        if any(delta, x -> x < 0) then (
            pbwAt#mu = {};
        ) else (
            pbwAt#mu = pbwTuples(posRoots, delta);
        );
        idxMap := new MutableHashTable;
        for k from 0 to #(pbwAt#mu) - 1 do idxMap#((pbwAt#mu)#k) = k;
        pbwIndexAt#mu = idxMap;
    );
    pTimer "PBW enumerate";

    -- Pre-build the WeylModuleZ object enough for the Verma routines.
    -- The applyChevalleyCache is a MutableHashTable shared across
    -- vermaKernel and buildFAction; both call applyChevalley with the
    -- same (cSrc, simpleIdx) tuples many times.
    applyChevalleyCache := new MutableHashTable;
    Wpre := new HashTable from {
        "cartan" => A,
        "highestWeight" => lambdaDynkin,
        "positiveRoots" => posRoots,
        "rootIndex" => rootIdx,
        "isPosRootSet" => isPosRootSet,
        "chevalleyConstants" => Nconst,
        "weightsList" => weightsList,
        "weightSet" => weightSet,
        "pbwAt" => pbwAt,
        "pbwIndexAt" => pbwIndexAt,
        "weightMultAt" => weightMultAt,
        "applyChevalleyCache" => applyChevalleyCache,
        "verboseProfile" => opts.Verbose
    };

    -- Build the Verma kernel.
    kernelAtWeight := vermaKernel Wpre;
    pTimer "vermaKernel";

    -- Choose V-basis tuples (free indices).
    (chosenIdxAt, chosenTuplesAt) := chooseVBasis(Wpre, kernelAtWeight);
    pTimer "chooseVBasis";

    -- Build action matrices for f_i, e_i, h_i on V(lambda).
    -- Strategy: for each V-weight pair (mu+alpha_i, mu) where both are V-weights,
    -- compute f_i action on M(lambda) for the basis tuples at mu+alpha_i, then
    -- project to the V-basis at mu via column reduction modulo kernelAtWeight#mu.
    --
    -- e_i: on V(lambda), satisfies [e_i, f_j] = delta_{ij} h_i.  We compute
    -- e_i by a linear-system approach: for each basis vector b in V(lambda)_mu,
    -- write b = sum c_k * (f_{j_k} * w_k) for some w_k and apply
    -- e_i (f_j w) = f_j (e_i w) + delta_{ij} h_i w recursively.  Equivalently,
    -- e_i is determined by its kernel on the highest-weight space (= 0)
    -- and the [e_i, f_j] relations.  In implementation we use induction
    -- on weight depth: at weight lambda, e_i v_lambda = 0; at weight mu
    -- with mu = mu' - alpha_j, choose any expression of basis vectors of
    -- V_mu as f_j times V_{mu'} basis vectors and propagate.

    actF := new MutableHashTable;        -- (mu, i) -> matrix V_{mu+alpha_i} -> V_mu
    actE := new MutableHashTable;        -- (mu, i) -> matrix V_{mu-alpha_i} -> V_mu (raises)
    actH := new MutableHashTable;        -- i -> diagonal data (we just compute on demand)

    -- Per-weight V-projector: reducerAt#mu is the (nFree x nTotal) matrix
    -- whose action on a column v in M-basis returns the V-component
    -- coefficients.  Computed by inverting [freeMat | kernelMat] once
    -- per weight, then keeping the first nFree rows of the inverse.
    -- Replaces a per-call `vColMat // [freeMat | kernelMat]` (one solve
    -- per source basis vector) with a single matrix-vector multiply, an
    -- O(K^2) -> O(K * nFree) shift that dramatically reduces buildFAction
    -- and the e-action loops.
    reducerAt := new MutableHashTable;

    buildReducer := mu -> (
        if reducerAt#?mu then return reducerAt#mu;
        kernelMat := kernelAtWeight#mu;
        freeIdx := chosenIdxAt#mu;
        nTotal := #(pbwAt#mu);
        nFree := #freeIdx;
        if nFree == 0 then (
            reducerAt#mu = matrix(QQ, table(0, nTotal, (i, j) -> 0_QQ));
            return reducerAt#mu;
        );
        if numcols kernelMat == 0 then (
            -- No kernel relations: reducer just picks out free entries.
            R := mutableMatrix(QQ, nFree, nTotal);
            for k from 0 to nFree - 1 do R_(k, freeIdx#k) = 1;
            reducerAt#mu = matrix R;
            return reducerAt#mu;
        );
        freeMat := mutableMatrix(QQ, nTotal, nFree);
        for k from 0 to nFree - 1 do freeMat_(freeIdx#k, k) = 1;
        full := (matrix freeMat) | kernelMat;
        invFull := inverse full;
        -- Take the first nFree rows.
        reducer := invFull^(toList(0..nFree - 1));
        if not all(flatten entries reducer, x -> denominator x == 1) then
            error("weylModuleZ: the selected PBW quotient basis is not an integral lattice basis at weight " |
                  toString mu);
        reducerAt#mu = reducer;
        reducerAt#mu
    );

    reduceToV := (mu, vColMat) -> (
        R := buildReducer mu;
        if numrows R == 0 then return matrix(QQ, table(0, 1, (i, j) -> 0_QQ));
        R * vColMat
    );

    -- Build f-action matrices.
    for mu in weightsList do (
        for i from 1 to rOver do (
            alphaIDynkin := for k from 0 to rOver - 1 list A_(k, i - 1);
            sourceMu := for k from 0 to rOver - 1 list mu#k + alphaIDynkin#k;
            if not weightSet#?sourceMu then continue;

            srcFree := chosenIdxAt#sourceMu;
            srcTuples := chosenTuplesAt#sourceMu;
            tgtFree := chosenIdxAt#mu;
            srcAll := pbwAt#sourceMu;
            tgtAll := pbwAt#mu;
            tgtIdx := pbwIndexAt#mu;

            nSrc := #srcFree;
            nTgt := #tgtFree;
            M := mutableMatrix(QQ, nTgt, nSrc);

            for jCol from 0 to nSrc - 1 do (
                cSrc := srcTuples#jCol;
                act := applyChevalley(Wpre, "f", i, cSrc, lambdaDynkin);
                v := mutableMatrix(QQ, #tgtAll, 1);
                for newC in keys act do (
                    if tgtIdx#?newC then v_(tgtIdx#newC, 0) = v_(tgtIdx#newC, 0) + act#newC;
                );
                vV := reduceToV(mu, matrix v);
                for k from 0 to nTgt - 1 do M_(k, jCol) = vV_(k, 0);
            );

            -- Convert from QQ to ZZ if all entries are integers.
            Mq := matrix M;
            allInt := all(flatten entries Mq, x -> denominator x == 1);
            if not allInt then
                error("weylModuleZ: nonintegral f-action at weight " | toString mu |
                      ", simple root " | toString i);
            MZ := matrix(ZZ, applyTable(entries Mq, x -> numerator x));
            actF#(mu, i) = MZ;
        );
    );
    pTimer "buildFAction";

    -- e-action: build via the recurrence
    --     e_i (f_j w) = f_j (e_i w) + delta_{ij} h_i w
    -- inductively on weight depth, starting from e_i v_lambda = 0.
    --
    -- For each V-weight mu < lambda we want a matrix
    --   e_i : V(lambda)_mu -> V(lambda)_{mu + alpha_i}
    -- (when both are V-weights).  We process weights in decreasing
    -- height; at each weight mu we decompose every V-basis vector v
    -- as v = sum_l c_l f_{j_l} w_l (with w_l in V_{mu + alpha_{j_l}})
    -- by stacking the incoming f-action matrices and using QQ-rational
    -- linear-algebra solve via //.
    --
    -- Then e_i v = sum_l c_l (f_{j_l} (e_i w_l) + delta_{i, j_l} h_i w_l).
    -- Each piece on the right-hand side is a vector in V_{mu + alpha_i}
    -- expressed in its chosen Z-basis.  The result is integer-valued
    -- (V_Z(lambda) is a Z-form of the algebra action).

    htOfDynkin := mu -> (
        s := toSimple(A, mu);
        h := 0_QQ;
        for x in s do h = h + x;
        h
    );
    sortedWeights := sort apply(weightsList, mu -> (-htOfDynkin mu, mu)) / (p -> p#1);

    -- The e-action is built via the recurrence above (e_i v = sum_l c_l
    -- (f_{j_l} (e_i w_l) + delta_{i, j_l} h_i w_l)).  At each weight mu we
    -- decompose every V-basis vector as a sum c_l (f_{j_l} w_l) by stacking
    -- incoming f-action matrices.  For irreducible V(lambda), V_mu is
    -- spanned by f_j (V_{mu + alpha_j}) summed over valid j (= simple roots
    -- with mu + alpha_j a V-weight): this holds in every Lie type because
    -- V is irreducible and connected by simple-root f-paths from v_lambda.
    for muIdx from 0 to #sortedWeights - 1 do (
        mu := sortedWeights#muIdx;
        if mu == lambdaDynkin then continue;
        nSrc := weightMultAt#mu;
        if nSrc == 0 then continue;

        validJ := for j from 1 to rOver list (
            alphaJD := for k from 0 to rOver - 1 list A_(k, j - 1);
            tgtMu := for k from 0 to rOver - 1 list mu#k + alphaJD#k;
            if weightSet#?tgtMu and weightMultAt#tgtMu > 0 then j else continue
        );
        if #validJ == 0 then continue;

        -- F_stack: rows = V_mu basis, columns = (f_{j_l} applied to basis of V_{mu+alpha_{j_l}})
        -- enumerated by (j, basis-vector index of V_{mu + alpha_j}).
        -- Build FStack as a fresh QQ-matrix, stripping module tags that
        -- can otherwise make // fail with "same target" errors.
        FStackBlocks := for j in validJ list (
            M := actF#(mu, j);
            matrix(QQ, applyTable(entries M, x -> x_QQ))
        );
        FStack := FStackBlocks#0;
        for k from 1 to #FStackBlocks - 1 do FStack = FStack | FStackBlocks#k;

        for i from 1 to rOver do (
            alphaID := for k from 0 to rOver - 1 list A_(k, i - 1);
            tgtMuI := for k from 0 to rOver - 1 list mu#k + alphaID#k;
            if not (weightSet#?tgtMuI and weightMultAt#tgtMuI > 0) then continue;

            nTgt := weightMultAt#tgtMuI;
            EM := mutableMatrix(QQ, nTgt, nSrc);

            for colIdx from 0 to nSrc - 1 do (
                vColMat := mutableMatrix(QQ, nSrc, 1);
                vColMat_(colIdx, 0) = 1;
                vColMat = matrix vColMat;

                cMat := try solve(FStack, vColMat) else null;
                if cMat === null or FStack * cMat != vColMat then (
                    -- Fall back to // .
                    cMat = try (vColMat // FStack) else null;
                    if cMat === null or FStack * cMat != vColMat then
                        error("buildEAction: failed to decompose basis vector at " | toString mu);
                );

                resultVec := mutableMatrix(QQ, nTgt, 1);

                blockOffset := 0;
                for j in validJ do (
                    alphaJD := for k from 0 to rOver - 1 list A_(k, j - 1);
                    wWeight := for k from 0 to rOver - 1 list mu#k + alphaJD#k;
                    nW := weightMultAt#wWeight;

                    for kIdx from 0 to nW - 1 do (
                        cVal := cMat_(blockOffset + kIdx, 0);
                        if cVal == 0 then continue;

                        -- Term (1): f_{j} (e_i w_l).  e_i w_l is at weight wWeight + alpha_i.
                        eiwlWeight := for k from 0 to rOver - 1 list wWeight#k + alphaID#k;
                        eiwlIsV := weightSet#?eiwlWeight and weightMultAt#eiwlWeight > 0;

                        if eiwlIsV and actE#?(wWeight, i) then (
                            eiwlMat := actE#(wWeight, i) ** QQ;
                            -- e_i w_l = kIdx-th column of eiwlMat (a vector in V_eiwlWeight).
                            colVec := mutableMatrix(QQ, weightMultAt#eiwlWeight, 1);
                            for k from 0 to weightMultAt#eiwlWeight - 1 do
                                colVec_(k, 0) = eiwlMat_(k, kIdx);
                            colVec = matrix colVec;
                            -- Apply f_j: V_eiwlWeight -> V_(eiwlWeight - alpha_j) = V_tgtMuI.
                            -- The matrix actF#(tgtMuI, j) has source V_(tgtMuI + alpha_j)
                            -- = V_(mu + alpha_i + alpha_j) = V_eiwlWeight.  Good.
                            fjMatQ := actF#(tgtMuI, j) ** QQ;
                            fjEiwl := fjMatQ * colVec;
                            for k from 0 to nTgt - 1 do
                                resultVec_(k, 0) = resultVec_(k, 0) + cVal * fjEiwl_(k, 0);
                        );

                        -- Term (2): delta_{i, j} * h_i w_l.  When i = j, w_l is at weight
                        -- wWeight = mu + alpha_j = mu + alpha_i = tgtMuI, so the contribution is
                        -- the scalar (wWeight, alpha_i^vee) = wWeight#(i-1) times the kIdx-th basis
                        -- vector of V_tgtMuI (= V_wWeight).
                        if i == j then (
                            scalar := wWeight#(i - 1);
                            resultVec_(kIdx, 0) = resultVec_(kIdx, 0) + cVal * scalar;
                        );
                    );

                    blockOffset = blockOffset + nW;
                );

                for k from 0 to nTgt - 1 do
                    EM_(k, colIdx) = resultVec_(k, 0);
            );

            EMq := matrix EM;
            allInt := all(flatten entries EMq, x -> denominator x == 1);
            if not allInt then
                error("weylModuleZ: nonintegral e-action at weight " | toString mu |
                      ", simple root " | toString i);
            actE#(mu, i) = matrix(ZZ, applyTable(entries EMq, x -> numerator x));
        );
    );
    pTimer "buildEAction";

    -- h_i: diagonal scalar action on V_mu, value = (mu, alpha_i^vee) = mu#(i-1).
    -- We just store the values per weight.
    for i from 1 to rOver do (
        actH#i = new MutableHashTable;
        for mu in weightsList do
            (actH#i)#mu = mu#(i - 1);
    );

    -- Total dim and sorted weights.
    totalDim := sum apply(weightsList, mu -> weightMultAt#mu);

    W := new WeylModuleZ from {
        "cartan" => A,
        "highestWeight" => lambdaDynkin,
        "positiveRoots" => posRoots,
        "rootIndex" => rootIdx,
        "isPosRootSet" => isPosRootSet,
        "chevalleyConstants" => Nconst,
        "weightsList" => weightsList,
        "weightSet" => weightSet,
        "weightMultAt" => weightMultAt,
        "pbwAt" => pbwAt,
        "pbwIndexAt" => pbwIndexAt,
        "kernelAtWeight" => kernelAtWeight,
        "chosenIdxAt" => chosenIdxAt,
        "chosenTuplesAt" => chosenTuplesAt,
        "actF" => actF,
        "actE" => actE,
        "actH" => actH,
        "dim" => totalDim
    };
    W
)

----------------------------------------------------------------------
-- Accessors for WeylModuleZ
----------------------------------------------------------------------

dim WeylModuleZ := W -> W#"dim"

highestWeight = method(Options => {WeightFormat => "Dynkin"})
highestWeight WeylModuleZ := opts -> W ->
    fromDynkin(W#"cartan", W#"highestWeight", opts.WeightFormat)

ambientCartan = method()
ambientCartan WeylModuleZ := W -> W#"cartan"

weights = method(Options => {WeightFormat => "Dynkin", WeightOrder => "RootDepth"})
weights WeylModuleZ := opts -> W -> (
    ordered := if opts.WeightOrder === "RootDepth" then W#"weightsList"
               else if opts.WeightOrder === "Lex" then rsort W#"weightsList"
               else error "weights: WeightOrder must be \"RootDepth\" or \"Lex\"";
    for mu in ordered list fromDynkin(W#"cartan", mu, opts.WeightFormat)
)

weightSpace = method(Options => {WeightFormat => "Dynkin"})
weightSpace (WeylModuleZ, List) := opts -> (W, muUser) -> (
    A := W#"cartan";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    if not (W#"weightSet")#?mu then return {};
    (W#"chosenTuplesAt")#mu
)

weightSpaces = method(Options => {WeightFormat => "Dynkin"})
weightSpaces WeylModuleZ := opts -> W -> (
    A := W#"cartan";
    new HashTable from for mu in W#"weightsList" list
        fromDynkin(A, mu, opts.WeightFormat) => (W#"chosenTuplesAt")#mu
)

basisVecs = method(Options => {WeightFormat => "Dynkin", WeightOrder => "RootDepth"})
basisVecs WeylModuleZ := opts -> W -> (
    A := W#"cartan";
    ordered := if opts.WeightOrder === "RootDepth" then W#"weightsList"
               else if opts.WeightOrder === "Lex" then rsort W#"weightsList"
               else error "basisVecs: WeightOrder must be \"RootDepth\" or \"Lex\"";
    flatten for mu in ordered list (
        muOut := fromDynkin(A, mu, opts.WeightFormat);
        for c in (W#"chosenTuplesAt")#mu list (muOut, c)
    )
)

character = method(Options => {WeightFormat => "Dynkin"})
character WeylModuleZ := opts -> W -> (
    A := W#"cartan";
    chDynkin := new HashTable from
        for mu in W#"weightsList" list mu => (W#"weightMultAt")#mu;
    characterFromDynkin(A, chDynkin, opts.WeightFormat)
)

----------------------------------------------------------------------
-- dominantWeights / dominantWeightSpaces (Doty's accessors)
----------------------------------------------------------------------
-- Restrict the weight machinery to dominant weights only (those with
-- all Dynkin coords >= 0).  Useful when iterating over candidate
-- highest weights of subquotients.

dominantWeights = method(Options => {WeightFormat => "Dynkin"})
dominantWeights WeylModuleZ := opts -> W -> (
    A := W#"cartan";
    for mu in select(W#"weightsList", isDominantWeight) list
        fromDynkin(A, mu, opts.WeightFormat)
)

dominantWeightSpaces = method(Options => {WeightFormat => "Dynkin"})
dominantWeightSpaces WeylModuleZ := opts -> W -> (
    A := W#"cartan";
    new HashTable from for mu in select(W#"weightsList", isDominantWeight) list
        fromDynkin(A, mu, opts.WeightFormat) => (W#"chosenTuplesAt")#mu
)

actionF = method(Options => {WeightFormat => "Dynkin"})
actionF (WeylModuleZ, List, ZZ) := opts -> (W, muUser, i) -> (
    -- Returns the matrix V_{mu+alpha_i} -> V_mu, or null if (mu, i) not in table.
    validateSimpleRootIndex(W#"cartan", i, "actionF");
    mu := toDynkin(W#"cartan", muUser, opts.WeightFormat);
    H := W#"actF";
    if H#?(mu, i) then H#(mu, i) else null
)

actionE = method(Options => {WeightFormat => "Dynkin"})
actionE (WeylModuleZ, List, ZZ) := opts -> (W, muUser, i) -> (
    validateSimpleRootIndex(W#"cartan", i, "actionE");
    mu := toDynkin(W#"cartan", muUser, opts.WeightFormat);
    H := W#"actE";
    if H#?(mu, i) then H#(mu, i) else null
)

actionH = method(Options => {WeightFormat => "Dynkin"})
actionH (WeylModuleZ, ZZ, List) := opts -> (W, i, muUser) -> (
    validateSimpleRootIndex(W#"cartan", i, "actionH");
    mu := toDynkin(W#"cartan", muUser, opts.WeightFormat);
    H := W#"actH";
    Hi := H#i;
    if Hi#?mu then Hi#mu else 0
)

-- net for nice printing
net WeylModuleZ := W -> (
    "WeylModuleZ(" | toString(W#"highestWeight") | ", dim " | toString(W#"dim") | ")"
)

----------------------------------------------------------------------
-- Layer 6: Maximal vectors of V(lambda) mod p
----------------------------------------------------------------------
--
-- For a Weyl module V(lambda) over F_p, a maximal vector is a weight
-- vector annihilated by every divided-power positive simple-root
-- generator e_{alpha_j}^{(p^k)} for j = 1..rank and k = 0..height,
-- through the first p-power above the largest simple-coroot coordinate
-- occurring among the weights of V.  This is a uniform root-string bound.
--
-- These vectors classify Hom(V(mu), V(lambda)) for various dominant mu.
-- This is the central computational primitive of Doty's WeylModulesNew
-- package (lib/weylmodule.gi:269).
--
-- Algorithm:
--   1. Pick a dominant weight mu of V(lambda); get its V-basis (size d_mu).
--   2. For each simple j and each k = 0..height:
--        Build the d_target x d_mu integer matrix of e_{alpha_j}^{(p^k)}
--        as the (p^k)-fold composition of actionE divided by (p^k)!.
--   3. Reduce all blocks mod p and stack vertically.
--   4. The right kernel of the stacked matrix (over F_p) gives the
--      maximal vectors at weight mu, expressed in the V-basis as
--      column vectors of length d_mu.

-- Largest exponent index k that needs to be considered for p^k-divided
-- powers.  Under the sl_2 for a simple root alpha_i, a weight mu has
-- h_i-eigenvalue mu_i.  Hence the largest Dynkin coordinate occurring in
-- the module bounds every simple-root string.  Testing through the first
-- p-power above that bound is uniform in every root system.
dividedPowerHeight = (W, p) -> (
    bound := max apply(W#"weightsList", mu -> max mu);
    bound = max(0, bound);
    height := 0;
    pwr := 1;
    while pwr <= bound do (
        height = height + 1;
        pwr = pwr * p;
    );
    height
)

maximalVectors = method(Options => {WeightFormat => "Dynkin"})

maximalVectors(WeylModuleZ, List, ZZ) := opts -> (W, muUser, p) -> (
    if not isPrime p then error "maximalVectors: third argument must be a prime";
    A := W#"cartan";
    rO := numrows A;
    weightMultAt := W#"weightMultAt";
    weightSet := W#"weightSet";
    lambdaDynkin := W#"highestWeight";
    mu := toDynkin(A, muUser, opts.WeightFormat);

    if not weightSet#?mu or weightMultAt#mu == 0 then return {};
    dMu := weightMultAt#mu;

    height := dividedPowerHeight(W, p);

    Fp := ZZ/p;
    blocks := {};

    for j from 1 to rO do (
        for k from 0 to height do (
            pk := p^k;

            -- Compute the matrix of e_{alpha_j}^{(p^k)} : V_mu -> V_{mu + p^k alpha_j}
            -- as (matrix of e_{alpha_j})^{p^k} divided by (p^k)!.
            currentMu := mu;
            currentMat := id_(ZZ^dMu);
            success := true;
            for iter from 1 to pk do (
                alphaJD := for kk from 0 to rO - 1 list A_(kk, j - 1);
                nextMu := for kk from 0 to rO - 1 list currentMu#kk + alphaJD#kk;
                if not weightSet#?nextMu or weightMultAt#nextMu == 0 then (
                    success = false;
                    break;
                );
                em := actionE(W, currentMu, j);
                if em === null then (
                    success = false;
                    break;
                );
                currentMat = em * currentMat;
                currentMu = nextMu;
            );

            if not success then continue;

            fact := 1;
            for ii from 1 to pk do fact = fact * ii;
            for row in entries currentMat do
                for x in row do
                    if x % fact != 0 then
                        error("maximalVectors: e_alpha^{(p^k)} not Z-form integral at j=" |
                              toString j | ", k=" | toString k);

            currentMatDiv := matrix(ZZ, applyTable(entries currentMat, x -> x // fact));
            currentMatFp := currentMatDiv ** Fp;

            -- Skip zero blocks (they contribute no constraints).
            if all(flatten entries currentMatFp, x -> x == 0) then continue;

            blocks = append(blocks, currentMatFp);
        );
    );

    if #blocks == 0 then (
        -- No constraints: every vector is maximal.
        ident := id_(Fp^dMu);
        return for col from 0 to dMu - 1 list (
            for r from 0 to dMu - 1 list ident_(r, col)
        );
    );

    bigMat := blocks#0;
    for k from 1 to #blocks - 1 do bigMat = bigMat || blocks#k;

    K := mingens kernel bigMat;
    if K == 0 or numcols K == 0 then return {};

    for col from 0 to numcols K - 1 list (
        for r from 0 to dMu - 1 list K_(r, col)
    )
)

maximalVectors(WeylModuleZ, ZZ) := opts -> (W, p) -> (
    -- Returns a list of pairs (mu, v) where mu is a dominant weight (in the
    -- requested WeightFormat) and v is a length-dim(V_mu) coefficient vector
    -- over F_p.
    if not isPrime p then error "maximalVectors: second argument must be a prime";
    A := W#"cartan";
    out := {};
    for mu in weights W do (
        if not all(mu, x -> x >= 0) then continue;     -- skip non-dominant
        mvecs := maximalVectors(W, mu, p);             -- internal: Dynkin
        muOut := fromDynkin(A, mu, opts.WeightFormat);
        for v in mvecs do out = append(out, (muOut, v));
    );
    out
)

----------------------------------------------------------------------
-- Layer 6: Submodules of V(lambda) mod p
----------------------------------------------------------------------
--
-- A SubWeylModuleFp object stores the data of a U_p-stable subspace of
-- V(lambda) tensor F_p:
--   * the ambient WeylModuleZ and prime p,
--   * for each weight mu in V(lambda), a Matrix over F_p whose columns
--     are a basis of the subspace at that weight,
--   * the original generators, used for printing and re-extension.
--
-- The cyclic-closure constructor mirrors Doty's submodule.gi:8 -- a BFS
-- through the divided-power simple-root generators
-- {e_{alpha_j}^{(p^k)}, f_{alpha_j}^{(p^k)} : j = 1..rank, k = 0..height}
-- with linear-independence tracking via Q-matrix rank checks.
-- (Cartan elements act as scalars and don't generate new vectors.)

SubWeylModuleFp = new Type of HashTable

-- Internal helper: matrix of e_{alpha_j}^{(p^k)} (or f variant) on V_mu mod p,
-- cached on the WeylModuleZ.
computeDPMatrix = (W, p, j, k, mu, isE) -> (
    -- Computes the matrix of e_{alpha_j}^{(p^k)} (or f variant) on V_mu
    -- mod p, freshly each call.  Callers that loop over many vectors at
    -- the same weight should hoist the matrix computation themselves.

    A := W#"cartan";
    rO := numrows A;
    weightSet := W#"weightSet";
    weightMultAt := W#"weightMultAt";
    Fp := ZZ/p;

    if not weightSet#?mu or weightMultAt#mu == 0 then return null;

    dMu := weightMultAt#mu;
    pk := p^k;

    currentMu := mu;
    currentMat := id_(ZZ^dMu);
    sgn := if isE then 1 else -1;
    success := true;
    for iter from 1 to pk do (
        alphaJD := for kk from 0 to rO - 1 list A_(kk, j - 1);
        nextMu := for kk from 0 to rO - 1 list currentMu#kk + sgn * alphaJD#kk;
        if not weightSet#?nextMu or weightMultAt#nextMu == 0 then (
            success = false;
            break;
        );
        am := if isE then actionE(W, currentMu, j) else actionF(W, nextMu, j);
        if am === null then (
            success = false;
            break;
        );
        currentMat = am * currentMat;
        currentMu = nextMu;
    );

    if not success then return null;

    fact := 1;
    for ii from 1 to pk do fact = fact * ii;
    for row in entries currentMat do
        for x in row do
            if x % fact != 0 then
                error("computeDPMatrix: divided-power not Z-form integral");

    dpInt := matrix(ZZ, applyTable(entries currentMat, x -> x // fact));
    dpInt ** Fp
)

-- Internal: BFS-closure helper.  Given an existing basisAt + rankAt and
-- a new generator (mu, vColMat), extend the closure under
-- divided-power simple-root operators.  Mutates basisAt and rankAt.
extendSubmoduleBFS = (W, p, basisAt, rankAt, mu0, v0) -> (
    A := W#"cartan";
    rO := numrows A;
    height := dividedPowerHeight(W, p);

    addVecAt := (w, v) -> (
        if all(flatten entries v, x -> x == 0) then return false;
        cur := if basisAt#?w then basisAt#w else null;
        if cur === null then (
            basisAt#w = v;
            rankAt#w = numcols v;
            return true;
        );
        candidate := cur | v;
        rk := rank candidate;
        if rk == numcols cur then return false;
        basisAt#w = candidate;
        rankAt#w = rk;
        true
    );

    queue := new MutableList;
    if addVecAt(mu0, v0) then queue#(#queue) = (mu0, v0);

    qHead := 0;
    while qHead < #queue do (
        item := queue#qHead;
        qHead = qHead + 1;
        (curMu, curVec) := item;

        for j from 1 to rO do (
            alphaJD := for kk from 0 to rO - 1 list A_(kk, j - 1);
            for k from 0 to height do (
                pk := p^k;

                -- e_j^{(p^k)} : V_curMu -> V_{curMu + p^k alpha_j}.
                eMat := computeDPMatrix(W, p, j, k, curMu, true);
                if eMat =!= null then (
                    eNewMu := for kk from 0 to rO - 1 list curMu#kk + pk * alphaJD#kk;
                    eNewVec := eMat * curVec;
                    if not all(flatten entries eNewVec, x -> x == 0) then
                        if addVecAt(eNewMu, eNewVec) then
                            queue#(#queue) = (eNewMu, eNewVec);
                );

                -- f_j^{(p^k)} : V_curMu -> V_{curMu - p^k alpha_j}.
                fMat := computeDPMatrix(W, p, j, k, curMu, false);
                if fMat =!= null then (
                    fNewMu := for kk from 0 to rO - 1 list curMu#kk - pk * alphaJD#kk;
                    fNewVec := fMat * curVec;
                    if not all(flatten entries fNewVec, x -> x == 0) then
                        if addVecAt(fNewMu, fNewVec) then
                            queue#(#queue) = (fNewMu, fNewVec);
                );
            );
        );
    );
)

-- Build a SubWeylModuleFp from prepared basisAt / rankAt / generators.
makeSubWeylModuleFp = (W, p, basisAt, rankAt, gens) -> (
    totalDim := 0;
    for w in keys basisAt do totalDim = totalDim + rankAt#w;
    new SubWeylModuleFp from {
        "ambient" => W,
        "prime" => p,
        "basisAt" => basisAt,
        "rankAt" => rankAt,
        "generators" => gens,
        "dim" => totalDim
    }
)

-- Coerce a List of integers/QQ/Fp to a column matrix over Fp.
listToColFp = (Fp, len, vec) -> (
    if #vec != len then error("listToColFp: expected length " | toString len);
    coerced := for x in vec list (
        if instance(x, ZZ) then x_Fp
        else if instance(x, QQ) then (
            if denominator x == 1 then (numerator x)_Fp
            else error "listToColFp: non-integer rational"
        )
        else x
    );
    transpose matrix(Fp, {coerced})
)

-- Public generator data use coefficient Lists.  A few internal objects
-- retain row or column matrices, so normalize either representation when
-- crossing an API boundary.
coefficientList = v -> (
    if instance(v, List) then v
    else if instance(v, Matrix) then flatten entries v
    else error "generator coefficients must be a List or Matrix"
)

subWeylModule = method(Options => {WeightFormat => "Dynkin"})

-- Cyclic submodule generated by (mu, vec).
subWeylModule(WeylModuleZ, ZZ, List, List) := opts -> (W, p, muUser, vec) -> (
    if not isPrime p then error "subWeylModule: prime expected";
    A := W#"cartan";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    weightSet := W#"weightSet";
    weightMultAt := W#"weightMultAt";
    if not weightSet#?mu or weightMultAt#mu == 0 then
        error("subWeylModule: weight " | toString mu | " is not a weight of V(lambda)");

    Fp := ZZ/p;
    dMu := weightMultAt#mu;
    vCol := listToColFp(Fp, dMu, vec);
    if all(flatten entries vCol, x -> x == 0) then
        return makeSubWeylModuleFp(W, p, new MutableHashTable, new MutableHashTable, {});

    -- Special case: if the vector is at the highest weight (= scalar multiple of v_lambda),
    -- the submodule is the entire V(lambda).  Skip the BFS by setting
    -- basisAt#nu = identity columns at every weight.
    lambda := W#"highestWeight";
    basisAt := new MutableHashTable;
    rankAt := new MutableHashTable;
    if mu == lambda then (
        for nu in weights W do (
            d := weightMultAt#nu;
            if d == 0 then continue;
            basisAt#nu = id_(Fp^d);
            rankAt#nu = d;
        );
        return makeSubWeylModuleFp(W, p, basisAt, rankAt, {(mu, vCol)});
    );

    extendSubmoduleBFS(W, p, basisAt, rankAt, mu, vCol);
    makeSubWeylModuleFp(W, p, basisAt, rankAt, {(mu, vCol)})
)

-- Closure of a list of generators (List of (mu, vec) pairs).
subWeylModule(WeylModuleZ, ZZ, List) := opts -> (W, p, gens) -> (
    if not isPrime p then error "subWeylModule: prime expected";
    A := W#"cartan";
    Fp := ZZ/p;
    weightSet := W#"weightSet";
    weightMultAt := W#"weightMultAt";

    basisAt := new MutableHashTable;
    rankAt := new MutableHashTable;
    storedGens := {};
    for g in gens do (
        (muUser, vec) := g;
        mu := toDynkin(A, muUser, opts.WeightFormat);
        if not weightSet#?mu or weightMultAt#mu == 0 then
            error("subWeylModule: weight " | toString mu | " is not a weight of V(lambda)");
        vCol := listToColFp(Fp, weightMultAt#mu, vec);
        if all(flatten entries vCol, x -> x == 0) then continue;
        extendSubmoduleBFS(W, p, basisAt, rankAt, mu, vCol);
        storedGens = append(storedGens, (mu, vCol));
    );
    makeSubWeylModuleFp(W, p, basisAt, rankAt, storedGens)
)

-- Extend an existing submodule by a new generator.
subWeylModule(SubWeylModuleFp, List, List) := opts -> (S, muUser, vec) -> (
    W := S#"ambient";
    A := W#"cartan";
    p := S#"prime";
    Fp := ZZ/p;
    mu := toDynkin(A, muUser, opts.WeightFormat);
    weightSet := W#"weightSet";
    weightMultAt := W#"weightMultAt";
    if not weightSet#?mu or weightMultAt#mu == 0 then
        error("subWeylModule: weight " | toString mu | " is not a weight of V(lambda)");
    vCol := listToColFp(Fp, weightMultAt#mu, vec);
    if all(flatten entries vCol, x -> x == 0) then return S;

    basisAt := new MutableHashTable;
    for w in keys (S#"basisAt") do basisAt#w = (S#"basisAt")#w;
    rankAt := new MutableHashTable;
    for w in keys (S#"rankAt") do rankAt#w = (S#"rankAt")#w;
    extendSubmoduleBFS(W, p, basisAt, rankAt, mu, vCol);

    storedGens := append(S#"generators", (mu, vCol));
    makeSubWeylModuleFp(W, p, basisAt, rankAt, storedGens)
)

-- Direct sum of submodules (assumes generators are linearly independent).
subWeylModuleDirectSum = method()
subWeylModuleDirectSum(WeylModuleZ, ZZ, List) := (W, p, modules) -> (
    if not isPrime p then error "subWeylModuleDirectSum: prime expected";
    if not all(modules, S -> instance(S, SubWeylModuleFp)) then
        error "subWeylModuleDirectSum: third argument must be a list of SubWeylModuleFp";
    if not all(modules, S -> S#"ambient" === W) then
        error "subWeylModuleDirectSum: every summand must have the requested ambient Weyl module";
    if not all(modules, S -> S#"prime" == p) then
        error "subWeylModuleDirectSum: every summand must have the requested characteristic";
    -- Combine via repeated extension.  For independence-checked direct sum,
    -- we use the same BFS as "closure of all generators".
    allGens := flatten apply(modules, S -> S#"generators");
    -- Convert the column-matrix generators back to lists for the multi-gen API.
    allGensAsLists := for g in allGens list (g#0, coefficientList(g#1));
    result := subWeylModule(W, p, allGensAsLists);
    expectedDim := sum apply(modules, dim);
    if dim result != expectedDim then
        error "subWeylModuleDirectSum: the supplied submodules are not independent";
    result
)

----------------------------------------------------------------------
-- Accessors for SubWeylModuleFp
----------------------------------------------------------------------

dim SubWeylModuleFp := S -> S#"dim"

ambientWeylModule = method()
ambientWeylModule SubWeylModuleFp := S -> S#"ambient"

weights SubWeylModuleFp := opts -> S -> (
    W := S#"ambient";
    A := W#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "weights");
    ordered := select(W#"weightsList", mu -> (S#"basisAt")#?mu);
    if opts.WeightOrder === "Lex" then ordered = rsort ordered
    else if opts.WeightOrder =!= "RootDepth" then
        error "weights: WeightOrder must be \"RootDepth\" or \"Lex\"";
    for mu in ordered list fromDynkin(A, mu, opts.WeightFormat)
)

ambientCartan SubWeylModuleFp := S -> (S#"ambient")#"cartan"

highestWeight SubWeylModuleFp := opts -> S -> (
    A := (S#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "highestWeight");
    mu := uniqueMaximalWeight(A, keys(S#"basisAt"), "submodule");
    if mu === null then null else fromDynkin(A, mu, opts.WeightFormat)
)

weightSpace(SubWeylModuleFp, List) := opts -> (S, muUser) -> (
    A := (S#"ambient")#"cartan";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    if not (S#"basisAt")#?mu then return null;
    (S#"basisAt")#mu
)

weightSpaces SubWeylModuleFp := opts -> S -> (
    A := (S#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "weightSpaces");
    new HashTable from for w in keys (S#"basisAt") list
        fromDynkin(A, w, opts.WeightFormat) => (S#"basisAt")#w
)

character SubWeylModuleFp := opts -> S -> (
    A := (S#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "character");
    new HashTable from for w in keys (S#"basisAt") list
        fromDynkin(A, w, opts.WeightFormat) => (S#"rankAt")#w
)

dominantWeights SubWeylModuleFp := opts -> S -> (
    A := (S#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "dominantWeights");
    for mu in select(sort keys (S#"basisAt"), isDominantWeight) list
        fromDynkin(A, mu, opts.WeightFormat)
)

dominantWeightSpaces SubWeylModuleFp := opts -> S -> (
    A := (S#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "dominantWeightSpaces");
    new HashTable from for mu in select(keys (S#"basisAt"), isDominantWeight) list
        fromDynkin(A, mu, opts.WeightFormat) => (S#"basisAt")#mu
)

basisVecs SubWeylModuleFp := opts -> S -> (
    -- Return a flat list of (weight, F_p column matrix) pairs.
    W := S#"ambient";
    A := W#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "basisVecs");
    ordered := select(W#"weightsList", w -> (S#"basisAt")#?w);
    if opts.WeightOrder === "Lex" then ordered = rsort ordered
    else if opts.WeightOrder =!= "RootDepth" then
        error "basisVecs: WeightOrder must be \"RootDepth\" or \"Lex\"";
    flatten for w in ordered list (
        M := (S#"basisAt")#w;
        wOut := fromDynkin(A, w, opts.WeightFormat);
        for c from 0 to numcols M - 1 list (wOut, submatrix(M, , {c}))
    )
)

generators SubWeylModuleFp := opts -> S -> S#"generators"

isWithin = method(Options => {WeightFormat => "Dynkin"})
isWithin(SubWeylModuleFp, List, List) := opts -> (S, muUser, vec) -> (
    -- Returns true iff the vector at weight mu lies in S.
    A := (S#"ambient")#"cartan";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    p := S#"prime";
    Fp := ZZ/p;
    weightMultAt := (S#"ambient")#"weightMultAt";
    if not weightMultAt#?mu or weightMultAt#mu == 0 then return false;
    vCol := listToColFp(Fp, weightMultAt#mu, vec);
    if all(flatten entries vCol, x -> x == 0) then return true;
    if not (S#"basisAt")#?mu then return false;
    -- Check if vCol is in span of (S#"basisAt"#mu).
    M := (S#"basisAt")#mu;
    candidate := M | vCol;
    rank candidate == rank M
)

net SubWeylModuleFp := S -> (
    "SubWeylModuleFp(" | toString(S#"dim") | "-dim, char " | toString(S#"prime") |
    ", in " | toString((S#"ambient")#"highestWeight") | ")"
)

----------------------------------------------------------------------
-- Layer 6 (continued): QuotientWeylModuleFp
----------------------------------------------------------------------
--
-- Q = V / S where V is V(lambda) tensor F_p and S is a SubWeylModuleFp.
-- For each weight mu, we pick a "complement basis" of V_mu mod S_mu --
-- a list of standard-basis indices of V_mu whose cosets in Q_mu form a
-- basis.  Action matrices for f_i and e_i on Q are computed by projecting
-- V's action through the quotient map.

QuotientWeylModuleFp = new Type of HashTable

quotientWeylModule = method()

quotientWeylModule SubWeylModuleFp := S -> (
    W := S#"ambient";
    p := S#"prime";
    Fp := ZZ/p;
    A := W#"cartan";
    rO := numrows A;
    weightsList := weights W;
    weightSet := W#"weightSet";
    weightMultAt := W#"weightMultAt";

    -- For each weight mu, pick complement-basis indices of V_mu mod S_mu.
    complementIdxAt := new MutableHashTable;
    quotientDimAt := new MutableHashTable;

    for mu in weightsList do (
        dV := weightMultAt#mu;
        if dV == 0 then (
            complementIdxAt#mu = {};
            quotientDimAt#mu = 0;
            continue;
        );
        sBasis := if (S#"basisAt")#?mu then (S#"basisAt")#mu else null;
        sRank := if sBasis === null then 0 else (S#"rankAt")#mu;
        currentMat := sBasis;
        currentRank := sRank;
        chosen := {};
        for j from 0 to dV - 1 do (
            if #chosen >= dV - sRank then break;
            ej := mutableMatrix(Fp, dV, 1);
            ej_(j, 0) = 1_Fp;
            ej = matrix ej;
            cand := if currentMat === null then ej else (currentMat | ej);
            rk := rank cand;
            if rk > currentRank then (
                chosen = append(chosen, j);
                currentMat = cand;
                currentRank = rk;
            );
        );
        complementIdxAt#mu = chosen;
        quotientDimAt#mu = #chosen;
    );

    -- For each weight mu, build the projection helper bigMatAt#mu, the
    -- inverse of the (sRank + qDim) x dV change-of-basis matrix
    -- [S basis | complement standard vectors].
    bigMatInvAt := new MutableHashTable;
    sRankAt := new MutableHashTable;
    for mu in weightsList do (
        dV := weightMultAt#mu;
        if dV == 0 then continue;
        sBasis := if (S#"basisAt")#?mu then (S#"basisAt")#mu else null;
        sRank := if sBasis === null then 0 else (S#"rankAt")#mu;
        sRankAt#mu = sRank;
        compIdx := complementIdxAt#mu;
        cmpVecs := for j in compIdx list (
            ej := mutableMatrix(Fp, dV, 1);
            ej_(j, 0) = 1_Fp;
            matrix ej
        );
        bigM := if sBasis === null and #cmpVecs == 0 then null
                else if sBasis === null then fold((a, b) -> a | b, cmpVecs)
                else if #cmpVecs == 0 then sBasis
                else sBasis | fold((a, b) -> a | b, cmpVecs);
        if bigM === null then (
            bigMatInvAt#mu = null;
        ) else if numrows bigM == numcols bigM then (
            bigMatInvAt#mu = inverse bigM;
        ) else (
            bigMatInvAt#mu = null;
        );
    );

    -- Project a V_mu vector to Q_mu basis (just the complement-coordinate part).
    projectToQ := (mu, vColMat) -> (
        if not bigMatInvAt#?mu or bigMatInvAt#mu === null then return null;
        sRank := sRankAt#mu;
        nQ := quotientDimAt#mu;
        xFull := bigMatInvAt#mu * vColMat;
        xQ := mutableMatrix(Fp, nQ, 1);
        for k from 0 to nQ - 1 do xQ_(k, 0) = xFull_(sRank + k, 0);
        matrix xQ
    );

    actFQ := new MutableHashTable;
    actEQ := new MutableHashTable;

    -- f_i: Q_(mu+α_i) -> Q_mu.
    for mu in weightsList do (
        for i from 1 to rO do (
            alphaID := for k from 0 to rO - 1 list A_(k, i - 1);
            srcMu := for k from 0 to rO - 1 list mu#k + alphaID#k;
            if not weightSet#?srcMu or weightMultAt#srcMu == 0 then continue;
            srcCompIdx := complementIdxAt#srcMu;
            tgtCompIdx := complementIdxAt#mu;
            nSrc := #srcCompIdx;
            nTgt := #tgtCompIdx;
            if nSrc == 0 or nTgt == 0 then continue;
            fMat := actionF(W, mu, i);
            if fMat === null then continue;
            fMatFp := fMat ** Fp;

            QMat := mutableMatrix(Fp, nTgt, nSrc);
            for sk from 0 to nSrc - 1 do (
                srcEj := mutableMatrix(Fp, weightMultAt#srcMu, 1);
                srcEj_(srcCompIdx#sk, 0) = 1_Fp;
                srcEj = matrix srcEj;
                vInV := fMatFp * srcEj;
                xQ := projectToQ(mu, vInV);
                if xQ === null then continue;
                for tk from 0 to nTgt - 1 do
                    QMat_(tk, sk) = xQ_(tk, 0);
            );
            actFQ#(mu, i) = matrix QMat;
        );
    );

    -- e_i: Q_mu -> Q_(mu+α_i).
    for mu in weightsList do (
        for i from 1 to rO do (
            alphaID := for k from 0 to rO - 1 list A_(k, i - 1);
            tgtMu := for k from 0 to rO - 1 list mu#k + alphaID#k;
            if not weightSet#?tgtMu or weightMultAt#tgtMu == 0 then continue;
            srcCompIdx := complementIdxAt#mu;
            tgtCompIdx := complementIdxAt#tgtMu;
            nSrc := #srcCompIdx;
            nTgt := #tgtCompIdx;
            if nSrc == 0 or nTgt == 0 then continue;
            eMat := actionE(W, mu, i);
            if eMat === null then continue;
            eMatFp := eMat ** Fp;

            QMat := mutableMatrix(Fp, nTgt, nSrc);
            for sk from 0 to nSrc - 1 do (
                srcEj := mutableMatrix(Fp, weightMultAt#mu, 1);
                srcEj_(srcCompIdx#sk, 0) = 1_Fp;
                srcEj = matrix srcEj;
                vInV := eMatFp * srcEj;
                xQ := projectToQ(tgtMu, vInV);
                if xQ === null then continue;
                for tk from 0 to nTgt - 1 do
                    QMat_(tk, sk) = xQ_(tk, 0);
            );
            actEQ#(mu, i) = matrix QMat;
        );
    );

    totalDim := sum apply(weightsList, mu -> quotientDimAt#mu);

    new QuotientWeylModuleFp from {
        "ambient" => W,
        "kernel" => S,
        "prime" => p,
        "complementIdxAt" => complementIdxAt,
        "quotientDimAt" => quotientDimAt,
        "bigMatInvAt" => bigMatInvAt,
        "sRankAt" => sRankAt,
        "actFQ" => actFQ,
        "actEQ" => actEQ,
        "dim" => totalDim
    }
)

----------------------------------------------------------------------
-- Accessors for QuotientWeylModuleFp
----------------------------------------------------------------------

dim QuotientWeylModuleFp := Q -> Q#"dim"

ambientWeylModule QuotientWeylModuleFp := Q -> Q#"ambient"

definingKernel = method()
definingKernel QuotientWeylModuleFp := Q -> Q#"kernel"

ambientCartan QuotientWeylModuleFp := Q -> (Q#"ambient")#"cartan"

highestWeight QuotientWeylModuleFp := opts -> Q -> (
    A := (Q#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "highestWeight");
    qWeights := select(keys(Q#"quotientDimAt"), w -> (Q#"quotientDimAt")#w > 0);
    mu := uniqueMaximalWeight(A, qWeights, "quotient");
    if mu === null then null else fromDynkin(A, mu, opts.WeightFormat)
)

weights QuotientWeylModuleFp := opts -> Q -> (
    W := Q#"ambient";
    A := W#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "weights");
    ordered := select(W#"weightsList", w -> (Q#"quotientDimAt")#w > 0);
    if opts.WeightOrder === "Lex" then ordered = rsort ordered
    else if opts.WeightOrder =!= "RootDepth" then
        error "weights: WeightOrder must be \"RootDepth\" or \"Lex\"";
    for mu in ordered
        list fromDynkin(A, mu, opts.WeightFormat)
)

character QuotientWeylModuleFp := opts -> Q -> (
    A := (Q#"ambient")#"cartan";
    -- Internal weights remain in Dynkin coords; convert keys for the user.
    chDynkin := new HashTable from
        for w in sort select(keys (Q#"quotientDimAt"), w -> (Q#"quotientDimAt")#w > 0)
        list w => (Q#"quotientDimAt")#w;
    characterFromDynkin(A, chDynkin, opts.WeightFormat)
)

dominantWeights QuotientWeylModuleFp := opts -> Q -> (
    A := (Q#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "dominantWeights");
    for mu in select(sort select(keys (Q#"quotientDimAt"), w -> (Q#"quotientDimAt")#w > 0),
                     isDominantWeight)
        list fromDynkin(A, mu, opts.WeightFormat)
)

dominantWeightSpaces QuotientWeylModuleFp := opts -> Q -> (
    -- Restrict the character to dominant weights.  (Per-weight basis is
    -- given by the complement indices in `complementIdxAt`; users who
    -- want explicit basis vectors can read that directly.)
    A := (Q#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "dominantWeightSpaces");
    new HashTable from
        for mu in select(keys (Q#"quotientDimAt"),
                         w -> (Q#"quotientDimAt")#w > 0 and isDominantWeight w)
        list fromDynkin(A, mu, opts.WeightFormat) => (Q#"quotientDimAt")#mu
)

actionF(QuotientWeylModuleFp, List, ZZ) := opts -> (Q, muUser, i) -> (
    A := (Q#"ambient")#"cartan";
    validateSimpleRootIndex(A, i, "actionF");
    mu := toDynkin(A, muUser, opts.WeightFormat);
    H := Q#"actFQ";
    if H#?(mu, i) then H#(mu, i) else null
)

actionE(QuotientWeylModuleFp, List, ZZ) := opts -> (Q, muUser, i) -> (
    A := (Q#"ambient")#"cartan";
    validateSimpleRootIndex(A, i, "actionE");
    mu := toDynkin(A, muUser, opts.WeightFormat);
    H := Q#"actEQ";
    if H#?(mu, i) then H#(mu, i) else null
)

actionH(QuotientWeylModuleFp, ZZ, List) := opts -> (Q, i, muUser) -> (
    A := (Q#"ambient")#"cartan";
    validateSimpleRootIndex(A, i, "actionH");
    mu := toDynkin(A, muUser, opts.WeightFormat);
    if (Q#"quotientDimAt")#?mu and (Q#"quotientDimAt")#mu > 0
    then mu#(i - 1) else 0
)

net QuotientWeylModuleFp := Q -> (
    "QuotientWeylModuleFp(" | toString(Q#"dim") | "-dim, char " | toString(Q#"prime") |
    ", quotient of " | toString((Q#"ambient")#"highestWeight") | ")"
)

----------------------------------------------------------------------
-- maximalVectors extended to QuotientWeylModuleFp
----------------------------------------------------------------------

maximalVectors(QuotientWeylModuleFp, List) := opts -> (Q, muUser) -> (
    p := Q#"prime";
    Fp := ZZ/p;
    W := Q#"ambient";
    A := W#"cartan";
    rO := numrows A;
    qDimAt := Q#"quotientDimAt";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    if not qDimAt#?mu or qDimAt#mu == 0 then return {};
    dMu := qDimAt#mu;
    height := dividedPowerHeight(W, p);
    blocks := {};
    -- For each (j, k): compute the matrix of e_{alpha_j}^{(p^k)} on Q_mu
    -- by composing actionE(Q, ., j) p^k times then dividing by (p^k)!.
    -- Since Q's action is mod p, divided powers must be implemented at
    -- the Z-form level on the AMBIENT V; we use computeDPMatrix on V
    -- and then project through Q.
    --
    -- Specifically: e_{alpha_j}^{(p^k)} on V_mu (as a coset rep e_{compIdx_l})
    -- gives a vector in V_(mu + p^k alpha_j); project to Q.
    weightSet := W#"weightSet";
    weightMultAt := W#"weightMultAt";
    compIdx := (Q#"complementIdxAt")#mu;
    for j from 1 to rO do (
        for k from 0 to height do (
            pk := p^k;
            tgtMuV := for kk from 0 to rO - 1 list mu#kk + pk * (
                if kk == j - 1 then 2 else 0
            ) - pk * (
                -- actually, alpha_j in Dynkin = j-th row of A.
                0
            );
            -- Above approach is tangled.  Simpler: tgtMuV = mu + p^k alpha_j via
            -- alpha_j Dynkin coords:
            alphaJD := for kk from 0 to rO - 1 list A_(kk, j - 1);
            tgtMuV = for kk from 0 to rO - 1 list mu#kk + pk * alphaJD#kk;
            if not weightSet#?tgtMuV or weightMultAt#tgtMuV == 0 then continue;
            if not qDimAt#?tgtMuV or qDimAt#tgtMuV == 0 then continue;
            dpMat := computeDPMatrix(W, p, j, k, mu, true);
            if dpMat === null then continue;
            -- Build the source-to-target map on Q_mu.
            -- Source basis: e_{compIdx#sk} (in V_mu).
            -- Apply dpMat: gives vector in V_tgtMuV.
            -- Project to Q_tgtMuV.
            tgtBigMatInv := (Q#"bigMatInvAt")#?tgtMuV and (Q#"bigMatInvAt")#tgtMuV =!= null;
            if not tgtBigMatInv then continue;
            tgtSRank := (Q#"sRankAt")#tgtMuV;
            tgtQDim := qDimAt#tgtMuV;
            blockMat := mutableMatrix(Fp, tgtQDim, dMu);
            for sk from 0 to dMu - 1 do (
                srcEj := mutableMatrix(Fp, weightMultAt#mu, 1);
                srcEj_(compIdx#sk, 0) = 1_Fp;
                srcEj = matrix srcEj;
                vInTgtV := dpMat * srcEj;
                xFull := (Q#"bigMatInvAt")#tgtMuV * vInTgtV;
                for tk from 0 to tgtQDim - 1 do
                    blockMat_(tk, sk) = xFull_(tgtSRank + tk, 0);
            );
            if all(flatten entries matrix blockMat, x -> x == 0) then continue;
            blocks = append(blocks, matrix blockMat);
        );
    );

    if #blocks == 0 then (
        ident := id_(Fp^dMu);
        return for col from 0 to dMu - 1 list (
            for r from 0 to dMu - 1 list ident_(r, col)
        );
    );
    bigMat := blocks#0;
    for k from 1 to #blocks - 1 do bigMat = bigMat || blocks#k;
    K := mingens kernel bigMat;
    if K == 0 or numcols K == 0 then return {};
    for col from 0 to numcols K - 1 list (
        for r from 0 to dMu - 1 list K_(r, col)
    )
)

maximalVectors QuotientWeylModuleFp := opts -> Q -> (
    A := (Q#"ambient")#"cartan";
    out := {};
    for mu in weights Q do (
        if not all(mu, x -> x >= 0) then continue;
        mvecs := maximalVectors(Q, mu);
        muOut := fromDynkin(A, mu, opts.WeightFormat);
        for v in mvecs do out = append(out, (muOut, v));
    );
    out
)

----------------------------------------------------------------------
-- isMaximalVector: test whether a vector at weight mu is annihilated by
-- every divided-power positive-simple-root operator.  Equivalent to
-- membership in `maximalVectors(W, mu, p)`.
----------------------------------------------------------------------

isMaximalVector = method(Options => {WeightFormat => "Dynkin"})
isMaximalVector (WeylModuleZ, ZZ, List, List) := opts -> (W, p, muUser, vec) -> (
    if not isPrime p then error "isMaximalVector: prime expected";
    A := W#"cartan";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    weightSet := W#"weightSet";
    weightMultAt := W#"weightMultAt";
    if not weightSet#?mu or weightMultAt#mu == 0 then return false;
    Fp := ZZ/p;
    nMu := weightMultAt#mu;
    if #vec =!= nMu then
        error("isMaximalVector: expected vector of length " | toString nMu);
    vCol := mutableMatrix(Fp, nMu, 1);
    for k from 0 to nMu - 1 do vCol_(k, 0) = (
        x := vec#k;
        if instance(x, Fp) then x else x_Fp
    );
    vCol = matrix vCol;
    if all(flatten entries vCol, x -> x == 0) then return false;
    -- Membership test: vCol lies in the F_p-span of `maximalVectors(W, mu, p)`.
    mvecs := maximalVectors(W, mu, p);
    if #mvecs == 0 then return false;
    M := mvecs#0#0;       -- mvecs#0 is itself a column-list; convert to matrix
    -- Each entry of mvecs is a List-of-Fp-elements representing a column;
    -- assemble as a matrix.
    cols := for v in mvecs list (
        c := mutableMatrix(Fp, nMu, 1);
        for k from 0 to nMu - 1 do c_(k, 0) = v#k;
        matrix c
    );
    bigM := cols#0;
    for k from 1 to #cols - 1 do bigM = bigM | cols#k;
    -- Check vCol lies in column span of bigM.
    M2 := bigM | vCol;
    rank M2 == rank bigM
)

----------------------------------------------------------------------
-- Layer 6 (continued): simpleQuotient, simpleCharacter, socleWeyl
----------------------------------------------------------------------
--
-- simpleQuotient(W, p): the simple quotient L(lambda) of V(lambda) tensor F_p.
-- Algorithm (Doty's lib/weylmodule.gi:395):
--   S = zero submodule.
--   While V/S has a non-top maximal vector m:
--     S := S + U_p . m  (lift m to V, then closure).
--   return V/S.
-- This terminates because each iteration strictly enlarges S.
--
-- simpleCharacter(A, p, lambda): the character of L(lambda).
-- For p-restricted lambda: take character of simpleQuotient.
-- Otherwise (Steinberg): lambda = lambda_0 + p lambda_1 with lambda_0
-- p-restricted; ch(L(lambda)) = ch(L(lambda_0)) tensor Frob(ch(L(lambda_1))).
--
-- socleWeyl(W, p): the socle of V(lambda) tensor F_p.
-- For each non-top maximal vector m at weight mu:
--   s = U_p . m.
--   if dim s == dim L(mu): s is simple, include in socle.
-- Return direct sum.

simpleQuotient = method(Options => {LieType => "A", WeightFormat => "Auto"})

simpleQuotient(WeylModuleZ, ZZ) := opts -> (W, p) -> (
    if not isPrime p then error "simpleQuotient: prime expected";
    Fp := ZZ/p;
    lambda := W#"highestWeight";

    -- Start with zero submodule.
    S := subWeylModule(W, p, {});
    Q := quotientWeylModule S;
    mvecs := maximalVectors Q;

    while #mvecs > 1 do (
        -- Find a non-top maximal vector.
        nonTop := null;
        for x in mvecs do (
            if x#0 != lambda then (nonTop = x; break);
        );
        if nonTop === null then break;
        (mu, vQ) := nonTop;
        -- Lift vQ from Q-basis to V-basis.
        compIdx := (Q#"complementIdxAt")#mu;
        dV := (W#"weightMultAt")#mu;
        vV := mutableMatrix(Fp, dV, 1);
        for k from 0 to #vQ - 1 do vV_(compIdx#k, 0) = vQ#k;
        vList := for k from 0 to dV - 1 list vV_(k, 0);
        oldDim := dim S;
        S = subWeylModule(S, mu, vList);
        if dim S <= oldDim then
            error "simpleQuotient: adding a non-top maximal vector did not enlarge the kernel";
        Q = quotientWeylModule S;
        mvecs = maximalVectors Q;
    );
    Q
)

----------------------------------------------------------------------
-- weylModule(p, lambda) and simpleQuotient(p, lambda): top-level
-- convenience constructors.
----------------------------------------------------------------------
-- These wrappers accept a characteristic and a highest weight directly,
-- with the underlying Lie algebra selected by the @TT "LieType"@ option.
-- For the classical types A/B/C/D the highest weight is interpreted
-- as a partition (or half-integer spin partition for B/D); the rank is
-- read off from the length of the input.  For the exceptional types
-- E_6 / E_7 / E_8 / F_4 / G_2 the highest weight is interpreted in
-- Dynkin (fundamental-weight) coordinates.
--
--   weylModule(p, lambda)                  -- defaults to LieType => "A"
--   weylModule(p, lambda, LieType => "B")
--   simpleQuotient(p, lambda)              -- L(lambda) over F_p, type A
--   simpleQuotient(p, lambda, Type => "G_2")

-- Internal: select the Cartan matrix and the natural-default WeightFormat
-- given the LieType string, the user-supplied input length, and the
-- effective input format.  For type A, the partition has length one more
-- than the Dynkin tuple; for B / C / D / exceptional they agree.
selectTypeCartan = (typeStr, len, effectiveFmt) -> (
    if typeStr === "A" then (
        -- type A_(rank): Dynkin has length rank, partition has length rank+1.
        rank := if effectiveFmt === "Dynkin" then len else len - 1;
        if rank < 1 then error "weylModule: type A needs rank >= 1";
        (cartanA(rank + 1), if effectiveFmt === "Dynkin" then "Dynkin" else "Partition")
    )
    else if typeStr === "B" then (
        if len < 2 then error "weylModule: type B needs length >= 2";
        (cartanB len, if effectiveFmt === "Dynkin" then "Dynkin" else "Partition")
    )
    else if typeStr === "C" then (
        if len < 2 then error "weylModule: type C needs length >= 2";
        (cartanC len, if effectiveFmt === "Dynkin" then "Dynkin" else "Partition")
    )
    else if typeStr === "D" then (
        if len < 4 then error "weylModule: type D needs length >= 4";
        (cartanD len, if effectiveFmt === "Dynkin" then "Dynkin" else "Partition")
    )
    else if typeStr === "E_6" or typeStr === "E6" then (cartanE 6, "Dynkin")
    else if typeStr === "E_7" or typeStr === "E7" then (cartanE 7, "Dynkin")
    else if typeStr === "E_8" or typeStr === "E8" then (cartanE 8, "Dynkin")
    else if typeStr === "F_4" or typeStr === "F4" or typeStr === "F" then
        (cartanF4(), "Dynkin")
    else if typeStr === "G_2" or typeStr === "G2" or typeStr === "G" then
        (cartanG2(), "Dynkin")
    else error("weylModule: unknown LieType \"" | toString typeStr |
               "\"; expected \"A\", \"B\", \"C\", \"D\", \"E_6\", \"E_7\", \"E_8\", \"F_4\", or \"G_2\"")
)

-- Resolve the effective WeightFormat for a (LieType, user override) pair.
-- If the user did not pass WeightFormat (left at "Auto"), we use the
-- "natural" default for the type: Partition for classical (A/B/C/D),
-- Dynkin for exceptional (E/F/G).  Otherwise we honor the user's choice,
-- with the caveat that Partition input is unavailable for exceptional types.
resolveWeightFormat = (typeStr, defaultFmt, userFmt) -> (
    if userFmt === "Auto" then return defaultFmt;
    if userFmt === "Dynkin" or userFmt === "Partition" then (
        if userFmt === "Partition" and member(typeStr, {"E_6","E6","E_7","E7","E_8","E8","F_4","F4","F","G_2","G2","G"}) then
            error("weylModule: WeightFormat => \"Partition\" is not available for exceptional type \"" |
                  toString typeStr | "\"; use Dynkin coordinates");
        return userFmt;
    );
    error concatenate("weylModule: WeightFormat must be \"Dynkin\", \"Partition\", or \"Auto\", got ",
                      toString userFmt)
)

weylModule = method(Options => {LieType => "A", WeightFormat => "Auto"})
weylModule(ZZ, List) := opts -> (p, lambda) -> (
    if not isPrime p then error "weylModule: prime expected";
    -- First pin down the effective format (so we know whether `len` means
    -- partition length or Dynkin length when picking the rank for type A).
    naturalFmt := if member(opts.LieType,
                            {"A", "B", "C", "D"}) then "Partition" else "Dynkin";
    fmt := resolveWeightFormat(opts.LieType, naturalFmt, opts.WeightFormat);
    (A, fmtIgnored) := selectTypeCartan(opts.LieType, #lambda, fmt);
    W := weylModuleZ(A, lambda, WeightFormat => fmt);
    -- Return V(lambda) reduced mod p as the trivial quotient
    -- (kernel = 0).  All existing accessors on QuotientWeylModuleFp
    -- (dim, weights, character, maximalVectors, ...) work uniformly.
    quotientWeylModule(subWeylModule(W, p, {}))
)

simpleQuotient(ZZ, List) := opts -> (p, lambda) -> (
    if not isPrime p then error "simpleQuotient: prime expected";
    naturalFmt := if member(opts.LieType,
                            {"A", "B", "C", "D"}) then "Partition" else "Dynkin";
    fmt := resolveWeightFormat(opts.LieType, naturalFmt, opts.WeightFormat);
    (A, fmtIgnored) := selectTypeCartan(opts.LieType, #lambda, fmt);
    W := weylModuleZ(A, lambda, WeightFormat => fmt);
    simpleQuotient(W, p)
)

----------------------------------------------------------------------
-- simpleCharacter: cached recursive Steinberg.
----------------------------------------------------------------------

-- Internal global cache for simple characters.  Keyed on (A, p, lambda).
simpleCharacterCache = new MutableHashTable

simpleCharacter = method(Options => {WeightFormat => "Dynkin"})

simpleCharacter(Matrix, ZZ, List) := opts -> (A, p, lambdaUser) -> (
    if not isPrime p then error "simpleCharacter: prime expected";
    n := numrows A;
    lambda := toDynkin(A, lambdaUser, opts.WeightFormat);
    if #lambda != n then error "simpleCharacter: weight has wrong length";
    if not all(lambda, x -> x >= 0) then
        error "simpleCharacter: lambda must be dominant";

    cacheKey := (A, p, lambda);
    local result;
    if simpleCharacterCache#?cacheKey then (
        result = simpleCharacterCache#cacheKey;
        return characterFromDynkin(A, result, opts.WeightFormat);
    );

    -- p-restricted base case.
    if all(lambda, x -> x < p) then (
        W := weylModuleZ(A, lambda);
        Q := simpleQuotient(W, p);
        result = new HashTable from for w in keys (Q#"quotientDimAt") list (
            if (Q#"quotientDimAt")#w > 0 then w => (Q#"quotientDimAt")#w else continue
        );
        simpleCharacterCache#cacheKey = result;
        return characterFromDynkin(A, result, opts.WeightFormat);
    );

    -- Steinberg: split lambda = lambda0 + p * lambda1.
    lambda0 := for x in lambda list (x % p);
    lambda1 := for x in lambda list (x // p);
    ch0 := simpleCharacter(A, p, lambda0);   -- internally Dynkin
    ch1 := simpleCharacter(A, p, lambda1);

    -- Frobenius twist of ch1: each weight w becomes p * w.
    out := new MutableHashTable;
    for w0 in keys ch0 do (
        for w1 in keys ch1 do (
            wSum := for k from 0 to n - 1 list w0#k + p * w1#k;
            mult := ch0#w0 * ch1#w1;
            cur := if out#?wSum then out#wSum else 0;
            out#wSum = cur + mult;
        );
    );
    result = new HashTable from out;
    simpleCharacterCache#cacheKey = result;
    characterFromDynkin(A, result, opts.WeightFormat)
)

simpleDimension = method(Options => {WeightFormat => "Dynkin"})
simpleDimension(Matrix, ZZ, List) := opts -> (A, p, lambda) -> (
    sum values simpleCharacter(A, p, lambda, WeightFormat => opts.WeightFormat)
)

-- Aliases matching Doty's GAP names.  In Doty's package the "top simple
-- factor" of V(lambda) mod p is L(lambda); these spellings are used in
-- some examples.
simpleTopFactorCharacter = method(Options => {WeightFormat => "Dynkin"})
simpleTopFactorCharacter (Matrix, ZZ, List) := opts -> (A, p, lambda) ->
    simpleCharacter(A, p, lambda, WeightFormat => opts.WeightFormat)

simpleTopFactorDim = method(Options => {WeightFormat => "Dynkin"})
simpleTopFactorDim (Matrix, ZZ, List) := opts -> (A, p, lambda) ->
    simpleDimension(A, p, lambda, WeightFormat => opts.WeightFormat)

----------------------------------------------------------------------
-- socleWeyl
----------------------------------------------------------------------

socleWeyl = method()

socleWeyl(WeylModuleZ, ZZ) := (W, p) -> (
    if not isPrime p then error "socleWeyl: prime expected";
    A := W#"cartan";
    lambda := W#"highestWeight";
    mvecs := maximalVectors(W, p);
    if #mvecs == 1 then (
        -- V is irreducible; socle = V.
        return subWeylModule(W, p, lambda, {1});
    );
    components := {};
    for x in mvecs do (
        (mu, v) := x;
        if mu == lambda then continue;
        s := subWeylModule(W, p, mu, v);
        dimSimple := simpleDimension(A, p, mu);
        if dim s == dimSimple then components = append(components, s);
    );
    subWeylModuleDirectSum(W, p, components)
)

----------------------------------------------------------------------
-- Phase E: decomposeCharacter + decompositionNumbers
----------------------------------------------------------------------
--
-- decomposeCharacter(A, p, ch): given a character `ch` (HashTable
-- weight -> multiplicity), express it as a non-negative integer combination
-- of simple characters in the dominance order.  Greedy algorithm: pick a
-- maximal weight, subtract its multiplicity copies of the simple character.
-- Returns a HashTable (highest weight -> multiplicity).
--
-- decompositionNumbers(W, p): just decomposeCharacter(A, p, character W).

decomposeCharacter = method(Options => {WeightFormat => "Dynkin"})

decomposeCharacter(Matrix, ZZ, HashTable) := opts -> (A, p, chUser) -> (
    if not isPrime p then error "decomposeCharacter: prime expected";
    if numrows A != numcols A or not isCartanMatrix A then
        error "decomposeCharacter: expected a Cartan matrix";
    if cartanType A =!= "finite" then
        error "decomposeCharacter: the Cartan matrix must be of finite type";
    ch := characterToDynkin(A, chUser, opts.WeightFormat);
    if any(keys ch, w -> #w != numrows A) then
        error "decomposeCharacter: every weight must have the Cartan rank";
    if any(values ch, m -> not instance(m, ZZ) or m < 0) then
        error "decomposeCharacter: multiplicities must be non-negative integers";
    -- Work with a mutable copy in Dynkin coords.
    chMut := new MutableHashTable;
    for w in keys ch do if ch#w > 0 then chMut#w = ch#w;
    out := new MutableHashTable;
    while true do (
        -- Find a maximal weight (in dominance order) with positive multiplicity.
        domWts := select(keys chMut, w -> chMut#w > 0 and all(w, x -> x >= 0));
        if #domWts == 0 then break;
        -- Find one that no other domWt strictly dominates.
        maxWt := null;
        for cand in domWts do (
            isMax := true;
            for other in domWts do (
                if other != cand and dominanceGE(A, other, cand) then (
                    isMax = false;
                    break;
                );
            );
            if isMax then (maxWt = cand; break);
        );
        if maxWt === null then break;
        m := chMut#maxWt;
        out#maxWt = (if out#?maxWt then out#maxWt else 0) + m;
        -- Subtract m copies of simpleCharacter(A, p, maxWt).
        sCh := simpleCharacter(A, p, maxWt);   -- Dynkin internally
        for w in keys sCh do (
            cur := if chMut#?w then chMut#w else 0;
            newVal := cur - m * sCh#w;
            if newVal < 0 then
                error("decomposeCharacter: input is not a non-negative sum of simple characters; negative residual at weight " |
                      toString w);
            if newVal == 0 then (
                if chMut#?w then remove(chMut, w);
            ) else (
                chMut#w = newVal;
            );
        );
    );
    if any(values chMut, m -> m != 0) then
        error "decomposeCharacter: input has a nonzero residual with no dominant highest weight";
    characterFromDynkin(A, new HashTable from out, opts.WeightFormat)
)

decompositionNumbers = method(Options => {WeightFormat => "Dynkin"})
decompositionNumbers(WeylModuleZ, ZZ) := opts -> (W, p) -> (
    A := W#"cartan";
    -- Build the V-character in Dynkin coords directly (the internal
    -- representation), then decompose, then convert to the requested format.
    chV := new HashTable from
        for w in W#"weightsList" list w => (W#"weightMultAt")#w;
    result := decomposeCharacter(A, p, chV);   -- WeightFormat default = Dynkin
    characterFromDynkin(A, result, opts.WeightFormat)
)

----------------------------------------------------------------------
-- Phase F: maximalSubmodule, isAmbiguous, ambiguousMaxVecs,
-- nextSocle, gensNextSocle, socleSeries, gensSocleLayers, printSocleLayers
----------------------------------------------------------------------

-- maximalSubmodule(W, p): the unique maximal proper submodule of V(lambda) tensor F_p.
-- Doty (lib/weylmodule.gi:416): equals the defining kernel of simpleQuotient(W, p).

maximalSubmodule = method()
maximalSubmodule(WeylModuleZ, ZZ) := (W, p) -> (
    Q := simpleQuotient(W, p);
    Q#"kernel"
)

-- isAmbiguous + ambiguousMaxVecs: detect Weyl modules with two or more linearly
-- independent maximal vectors at the same weight.

ambiguousMaxVecs = method(Options => {WeightFormat => "Dynkin"})
ambiguousMaxVecs(WeylModuleZ, ZZ) := opts -> (W, p) -> (
    A := W#"cartan";
    mvecs := maximalVectors(W, p);    -- internal: Dynkin
    -- Group by weight.
    byWeight := new MutableHashTable;
    for x in mvecs do (
        (mu, v) := x;
        cur := if byWeight#?mu then byWeight#mu else {};
        byWeight#mu = append(cur, v);
    );
    out := {};
    for mu in keys byWeight do (
        if #(byWeight#mu) > 1 then (
            muOut := fromDynkin(A, mu, opts.WeightFormat);
            for v in byWeight#mu do out = append(out, (muOut, v));
        );
    );
    out
)

isAmbiguous = method()
isAmbiguous(WeylModuleZ, ZZ) := (W, p) -> #(ambiguousMaxVecs(W, p)) > 0

-- gensNextSocle(S): generators of the next socle layer.
-- Doty (lib/submodule.gi:477): returns generators of socle(V/S) lifted to V.

-- Internal helper: compute the dimension of the cyclic Q-submodule generated
-- by a Q-vector via BFS through Q's e and f divided-power actions.
dimQSubmoduleAt = (Q, mu, vQ) -> (
    p := Q#"prime";
    Fp := ZZ/p;
    W := Q#"ambient";
    A := W#"cartan";
    rO := numrows A;
    qDimAt := Q#"quotientDimAt";
    weightSet := W#"weightSet";
    weightMultAt := W#"weightMultAt";
    height := dividedPowerHeight(W, p);

    -- Project a V_mu vector to Q_mu (via bigMatInvAt).
    projectV2Q := (mw, vCol) -> (
        if not (Q#"bigMatInvAt")#?mw or (Q#"bigMatInvAt")#mw === null then return null;
        sRank := (Q#"sRankAt")#mw;
        nQ := qDimAt#mw;
        xFull := (Q#"bigMatInvAt")#mw * vCol;
        out := mutableMatrix(Fp, nQ, 1);
        for k from 0 to nQ - 1 do out_(k, 0) = xFull_(sRank + k, 0);
        matrix out
    );

    -- Embed Q_mu vector into V_mu (just place at complement indices).
    embedQ2V := (mw, vQCol) -> (
        compIdx := (Q#"complementIdxAt")#mw;
        dV := weightMultAt#mw;
        out := mutableMatrix(Fp, dV, 1);
        for k from 0 to numrows vQCol - 1 do
            out_(compIdx#k, 0) = vQCol_(k, 0);
        matrix out
    );

    -- Initialize.
    basisAt := new MutableHashTable;
    rankAt := new MutableHashTable;
    queue := new MutableList;

    addAt := (w, vCol) -> (
        if all(flatten entries vCol, x -> x == 0) then return false;
        cur := if basisAt#?w then basisAt#w else null;
        if cur === null then (
            basisAt#w = vCol;
            rankAt#w = 1;
            return true;
        );
        cand := cur | vCol;
        rk := rank cand;
        if rk == numcols cur then return false;
        basisAt#w = cand;
        rankAt#w = rk;
        true
    );

    vQCol := if instance(vQ, Matrix) then vQ
             else (
                 t := mutableMatrix(Fp, #vQ, 1);
                 for k from 0 to #vQ - 1 do t_(k, 0) = vQ#k;
                 matrix t
             );
    if addAt(mu, vQCol) then queue#(#queue) = (mu, vQCol);

    qHead := 0;
    while qHead < #queue do (
        item := queue#qHead;
        qHead = qHead + 1;
        (curMu, curVec) := item;

        -- Embed curVec to V_curMu.
        vV := embedQ2V(curMu, curVec);

        for j from 1 to rO do (
            alphaJD := for kk from 0 to rO - 1 list A_(kk, j - 1);
            for k from 0 to height do (
                pk := p^k;
                -- e^{(p^k)} on V at curMu -> V_{curMu + p^k alpha_j}.
                eMatV := computeDPMatrix(W, p, j, k, curMu, true);
                if eMatV =!= null then (
                    eTgtMu := for kk from 0 to rO - 1 list curMu#kk + pk * alphaJD#kk;
                    eVec := eMatV * vV;
                    eVecQ := projectV2Q(eTgtMu, eVec);
                    if eVecQ =!= null and not all(flatten entries eVecQ, x -> x == 0) then
                        if addAt(eTgtMu, eVecQ) then queue#(#queue) = (eTgtMu, eVecQ);
                );
                -- f^{(p^k)}.
                fMatV := computeDPMatrix(W, p, j, k, curMu, false);
                if fMatV =!= null then (
                    fTgtMu := for kk from 0 to rO - 1 list curMu#kk - pk * alphaJD#kk;
                    fVec := fMatV * vV;
                    fVecQ := projectV2Q(fTgtMu, fVec);
                    if fVecQ =!= null and not all(flatten entries fVecQ, x -> x == 0) then
                        if addAt(fTgtMu, fVecQ) then queue#(#queue) = (fTgtMu, fVecQ);
                );
            );
        );
    );

    totalDim := 0;
    for w in keys rankAt do totalDim = totalDim + rankAt#w;
    totalDim
)

-- Generators of socle(V/S) lifted to V.
-- Returns a list of (mu, vV) pairs.
gensSocleOfQuotient = (Q, p) -> (
    W := Q#"ambient";
    A := W#"cartan";
    lambda := W#"highestWeight";
    weightMultAt := W#"weightMultAt";
    Fp := ZZ/p;

    embedQ2V := (mw, vQCol) -> (
        compIdx := (Q#"complementIdxAt")#mw;
        dV := weightMultAt#mw;
        out := mutableMatrix(Fp, dV, 1);
        for k from 0 to numrows vQCol - 1 do
            out_(compIdx#k, 0) = vQCol_(k, 0);
        matrix out
    );

    mvecs := maximalVectors Q;
    if #mvecs == 1 then (
        -- Q is irreducible; "socle" = whole Q, generated by the (single) max vec.
        (mu, vQ) := mvecs#0;
        vQCol := transpose matrix(Fp, {vQ});
        vV := embedQ2V(mu, vQCol);
        vList := for k from 0 to numrows vV - 1 list vV_(k, 0);
        return {(mu, vList)};
    );
    out := {};
    for x in mvecs do (
        (mu, vQ) := x;
        if mu == lambda then continue;
        vQCol := transpose matrix(Fp, {vQ});
        d := dimQSubmoduleAt(Q, mu, vQCol);
        dimSimple := simpleDimension(A, p, mu);
        if d == dimSimple then (
            vV := embedQ2V(mu, vQCol);
            vList := for k from 0 to numrows vV - 1 list vV_(k, 0);
            out = append(out, (mu, vList));
        );
    );
    out
)

gensNextSocle = method()
gensNextSocle SubWeylModuleFp := S -> (
    W := S#"ambient";
    p := S#"prime";
    if dim S == dim W then return {};
    Q := quotientWeylModule S;
    gensSocleOfQuotient(Q, p)
)

nextSocle = method()
nextSocle SubWeylModuleFp := S -> (
    gens := gensNextSocle S;
    if #gens == 0 then return null;
    -- Extend S by these generators.
    for g in gens do S = subWeylModule(S, g#0, g#1);
    S
)

socleSeries = method()
socleSeries(WeylModuleZ, ZZ) := (W, p) -> (
    soc := socleWeyl(W, p);
    series := {soc};
    while dim (last series) < dim W do (
        nxt := nextSocle (last series);
        if nxt === null then
            error "socleSeries: failed to find the next layer of a nonzero quotient";
        if dim nxt <= dim(last series) then
            error "socleSeries: the next layer did not enlarge the filtration";
        series = append(series, nxt);
    );
    series
)

gensSocleLayers = method()
gensSocleLayers(WeylModuleZ, ZZ) := (W, p) -> (
    -- For each socle layer S_k / S_{k-1}, return the generators that
    -- extend S_{k-1} to S_k (in V-basis at their weights).
    series := socleSeries(W, p);
    if #series == 0 then return {};
    -- First layer: generators of socleWeyl(W, p).
    -- We need to retrieve these.  They are stored in S_1#"generators".
    -- For subsequent layers: generators come from gensNextSocle.
    layers := {};
    -- Layer 1: generators of socle as recorded.
    firstLayer := for g in (series#0)#"generators" list
        (g#0, coefficientList(g#1));
    layers = append(layers, firstLayer);
    for k from 1 to #series - 1 do (
        prevS := series#(k - 1);
        gens := gensNextSocle prevS;
        layers = append(layers, gens);
    );
    layers
)

printSocleLayers = method()
printSocleLayers(WeylModuleZ, ZZ) := (W, p) -> (
    layers := gensSocleLayers(W, p);
    << "Printing highest weights of simples in socle layers of V(" |
       toString (W#"highestWeight") | ") mod " | toString p << endl;
    for k from 0 to #layers - 1 do (
        weightsHere := apply(layers#k, g -> g#0);
        << "Layer " | toString (k + 1) | ": " | toString weightsHere << endl;
    );
)

----------------------------------------------------------------------
-- Phase G: socleSeries / gensSocleLayers for SubWeylModuleFp,
-- socleLengthTwoQuotient, twoFactorQuotientsContaining
----------------------------------------------------------------------
--
-- These mirror Doty's GAP operations for socle structure of the
-- quotient V(lambda) / S, and for "two-factor" quotients realizing
-- nontrivial Ext^1's between simple modules.
--
-- socleSeries(S): for a SubWeylModuleFp S of V = V(lambda), the socle
-- filtration of V/S, lifted back to V.  Returns a list S_1 ⊂ S_2 ⊂ ...
-- ⊂ V (each S_k a SubWeylModuleFp containing S) such that S_k/S equals
-- the k-th element of the socle filtration of V/S.  The length of the
-- returned list equals the socle length of V/S.
--
-- gensSocleLayers(S): the layer-by-layer generators of the socle
-- filtration of V/S.  Returns a list of lists of (mu, vec) pairs:
-- the k-th sublist contains the generators that, when added to S_{k-1},
-- produce S_k.  These are exactly the maximal-vector generators of
-- soc(V/S_{k-1}).
--
-- socleLengthTwoQuotient(S): an extension S' ⊇ S obtained by truncating
-- the lower socle layers so that V/S' has socle length ≤ 2.  Returns the
-- QuotientWeylModuleFp V/S'.  No minimality assertion is made.
-- Algorithm (Doty's lib/submodule.gi:517): repeatedly extend S by the
-- generators of the second-to-top element of socleSeries(S) until the
-- socle length drops to ≤ 2.
--
-- twoFactorQuotientsContaining(S): selected QuotientWeylModuleFp's of V
-- whose kernels contain S and which exhibit two-factor extensions.  As in
-- Doty's routine, this is a search inside V/S, not an exhaustive computation
-- or parametrization of Ext^1.

socleSeries QuotientWeylModuleFp := Q -> (
    -- Socle series of Q = V/S, identified with the lifts in V.
    socleSeries (Q#"kernel")
)

socleSeries SubWeylModuleFp := S -> (
    series := {};
    curS := S;
    W := S#"ambient";
    while dim curS < dim W do (
        nxt := nextSocle curS;
        if nxt === null then
            error "socleSeries: failed to find the next layer of a nonzero quotient";
        if dim nxt <= dim curS then
            error "socleSeries: the next layer did not enlarge the filtration";
        series = append(series, nxt);
        curS = nxt;
    );
    series
)

gensSocleLayers QuotientWeylModuleFp := Q -> (
    gensSocleLayers (Q#"kernel")
)

gensSocleLayers SubWeylModuleFp := S -> (
    layers := {};
    curS := S;
    W := S#"ambient";
    while dim curS < dim W do (
        gens := gensNextSocle curS;
        if #gens == 0 then
            error "gensSocleLayers: failed to find generators for a nonzero quotient";
        layers = append(layers, gens);
        oldDim := dim curS;
        for g in gens do curS = subWeylModule(curS, g#0, g#1);
        if dim curS <= oldDim then
            error "gensSocleLayers: generators did not enlarge the filtration";
    );
    layers
)

printSocleLayers QuotientWeylModuleFp := Q -> (
    layers := gensSocleLayers Q;
    W := Q#"ambient";
    << "Printing highest weights of simples in socle layers of "
       << net Q << endl;
    for k from 0 to #layers - 1 do (
        weightsHere := apply(layers#k, g -> g#0);
        << "Layer " | toString (k + 1) | ": " | toString weightsHere << endl;
    );
)

-- socleSeriesAndPrims: bundle (socleSeries, gensSocleLayers) into the
-- pair Doty's @TT "SocleSeriesAndPrims"@ returns -- the socle filtration
-- together with the generating "primitive" vectors at each layer.
socleSeriesAndPrims = method()
socleSeriesAndPrims SubWeylModuleFp := S -> (socleSeries S, gensSocleLayers S)
socleSeriesAndPrims (WeylModuleZ, ZZ) := (W, p) -> (
    S0 := subWeylModule(W, p, {});
    socleSeriesAndPrims S0
)
socleSeriesAndPrims QuotientWeylModuleFp := Q -> socleSeriesAndPrims (Q#"kernel")

socleLengthTwoQuotient = method()
socleLengthTwoQuotient SubWeylModuleFp := S -> (
    -- Iterate: while length of socleSeries(V/S) > 2, extend S by
    -- the generators recorded for the (length-2)-th element
    -- (i.e., everything below the top two layers).
    ss := socleSeries S;
    while #ss > 2 do (
        -- ss is 0-indexed; ss#(#ss - 1) is the top (= V).
        -- We extend S by the generators recorded in ss#(#ss - 3),
        -- the cumulative term two layers below the top.  In GAP's 1-indexed notation
        -- this is ss[Length(ss) - 2].
        sIdx := #ss - 3;
        if sIdx < 0 then break;
        gensList := (ss#sIdx)#"generators";
        oldDim := dim S;
        for g in gensList do (
            (mu, v) := g;
            S = subWeylModule(S, mu, coefficientList v);
        );
        if dim S <= oldDim then
            error "socleLengthTwoQuotient: selected generators did not enlarge the kernel";
        ss = socleSeries S;
    );
    quotientWeylModule S
)

socleLengthTwoQuotient WeylModuleZ := W -> (
    error "socleLengthTwoQuotient: missing characteristic; use socleLengthTwoQuotient(SubWeylModuleFp) with a SubWeylModuleFp from subWeylModule(W, p, ...)"
)

twoFactorQuotientsContaining = method()
twoFactorQuotientsContaining SubWeylModuleFp := S -> (
    -- Returns selected two-factor quotients V/S' (with S' ⊇ S) found
    -- inside V/S.  A quotient is "two-factor" iff it has socle length
    -- 2 with a simple bottom layer (its head is automatically simple for a
    -- nonzero highest-weight quotient).  Algorithm (Doty's lib/submodule.gi:534):
    --   Q = socleLengthTwoQuotient(S);  g = gensSocleLayers(Q).
    --   If #g == 2 and #g#0 == 1, return {Q}.
    --   Else for each generator gen in g#0, take U = subWeylModule(S, gen),
    --     and append socleLengthTwoQuotient(U) (if it is a 2-factor).
    W := S#"ambient";
    p := S#"prime";
    out := {};
    Q := socleLengthTwoQuotient S;
    g := gensSocleLayers Q;
    if #g == 2 and #(g#0) == 1 then (
        out = {Q};
        return out;
    );
    if #g == 0 then return {};
    -- Iterate over generators of the first (bottom) layer.
    bottomGens := g#0;
    for gen in bottomGens do (
        U := subWeylModule(S, gen#0, coefficientList(gen#1));
        QU := socleLengthTwoQuotient U;
        gU := gensSocleLayers QU;
        if #gU == 2 and #(gU#0) == 1 then out = append(out, QU);
    );
    out
)

----------------------------------------------------------------------
-- Convenience converters between Dynkin and partition coordinates
----------------------------------------------------------------------
-- These wrap KacMoody's `partitionToFundamental` / `fundamentalToPartition`
-- and additionally know how to dispatch on the Cartan stored inside a
-- WeylModuleZ.  All four accept either an explicit Cartan matrix as the
-- first argument or a WeylModuleZ / SubWeylModuleFp / QuotientWeylModuleFp
-- (whose ambient Cartan we read off).

weightAsPartition = method()
weightAsPartition (Matrix, List)         := (A, mu) -> fundamentalToPartition(A, mu)
weightAsPartition (WeylModuleZ, List)    := (W, mu) -> fundamentalToPartition(W#"cartan", mu)
weightAsPartition (SubWeylModuleFp, List)        := (S, mu) -> fundamentalToPartition((S#"ambient")#"cartan", mu)
weightAsPartition (QuotientWeylModuleFp, List)   := (Q, mu) -> fundamentalToPartition((Q#"ambient")#"cartan", mu)

weightAsDynkin = method()
weightAsDynkin (Matrix, List)         := (A, lam) -> toDynkin(A, lam, "Partition")
weightAsDynkin (WeylModuleZ, List)    := (W, lam) -> toDynkin(W#"cartan", lam, "Partition")
weightAsDynkin (SubWeylModuleFp, List)        := (S, lam) -> toDynkin((S#"ambient")#"cartan", lam, "Partition")
weightAsDynkin (QuotientWeylModuleFp, List)   := (Q, lam) -> toDynkin((Q#"ambient")#"cartan", lam, "Partition")

characterAsPartition = method()
characterAsPartition (Matrix, HashTable)        := (A, ch) -> characterFromDynkin(A, ch, "Partition")
characterAsPartition (WeylModuleZ, HashTable)   := (W, ch) -> characterFromDynkin(W#"cartan", ch, "Partition")
characterAsPartition (SubWeylModuleFp, HashTable)       := (S, ch) -> characterFromDynkin((S#"ambient")#"cartan", ch, "Partition")
characterAsPartition (QuotientWeylModuleFp, HashTable)  := (Q, ch) -> characterFromDynkin((Q#"ambient")#"cartan", ch, "Partition")

characterAsDynkin = method()
characterAsDynkin (Matrix, HashTable)        := (A, ch) -> characterToDynkin(A, ch, "Partition")
characterAsDynkin (WeylModuleZ, HashTable)   := (W, ch) -> characterToDynkin(W#"cartan", ch, "Partition")
characterAsDynkin (SubWeylModuleFp, HashTable)       := (S, ch) -> characterToDynkin((S#"ambient")#"cartan", ch, "Partition")
characterAsDynkin (QuotientWeylModuleFp, HashTable)  := (Q, ch) -> characterToDynkin((Q#"ambient")#"cartan", ch, "Partition")

----------------------------------------------------------------------
-- Phase I: Schur-algebra layer (Doty's Chapter 4)
----------------------------------------------------------------------
--
-- Partition utilities (= Doty's Section 4.3) plus the Schur-algebra
-- bridge for Type A: each n-part partition mu of degree r gives the
-- highest weight CompositionToWeight(mu) of a Weyl module for SL_n,
-- and the decomposition matrix for the Schur algebra S(n, r) is
-- assembled by running decompositionNumbers across all partitions.
-- Symmetric-group decomposition follows by restricting columns to
-- p-restricted partitions.

-- compositionToWeight(mu): take consecutive differences of a composition.
compositionToWeight = method()
compositionToWeight List := mu -> (
    n := #mu;
    if n < 2 then error "compositionToWeight: composition must have length >= 2";
    for k from 0 to n - 2 list mu#k - mu#(k + 1)
)

-- weightToComposition(r, wt): inverse of compositionToWeight at a given degree.
-- Returns a composition (List) or null if impossible.
weightToComposition = method()
weightToComposition (ZZ, List) := (r, wt) -> (
    -- Given wt of length n-1, find composition of degree r and length n.
    -- The relation: mu_k - mu_{k+1} = wt_k for k = 0..n-2.
    -- So mu_k = sum_{j>=k} wt_j + mu_{n-1}; pick mu_{n-1} so total = r.
    n := #wt + 1;
    suffix := for k from 0 to n - 1 list (
        if k == n - 1 then 0
        else sum for j from k to n - 2 list wt#j
    );
    s := sum suffix;
    if (r - s) % n != 0 then return null;
    shift := (r - s) // n;
    result := for k from 0 to n - 1 list suffix#k + shift;
    if any(result, x -> x < 0) then return null;
    result
)

-- allPartitions(n): all partitions of n.
allPartitions = method()
allPartitions ZZ := n -> (
    if n < 0 then error "allPartitions: n must be non-negative";
    if n == 0 then return {{}};
    -- Use M2's built-in partitions and convert to Lists.
    apply(partitions n, p -> toList p)
)

-- Test the common public representation of a partition: a weakly
-- decreasing list of non-negative integers, with optional trailing zeros.
validPartitionList = mu -> (
    all(mu, x -> instance(x, ZZ) and x >= 0) and
    (#mu < 2 or all(0..(#mu - 2), i -> mu#i >= mu#(i + 1)))
)

-- boundedPartitions(n, r, s): n-part partitions of r with each part in [0, s].
-- (Padded with zeros to length n.)
boundedPartitions = method()
boundedPartitions (ZZ, ZZ, ZZ) := (n, r, s) -> (
    if n < 1 or r < 0 or s < 0 then return {};
    helper := null;
    helper = (parts, remaining, maxAllowed) -> (
        if parts == 0 then (
            if remaining == 0 then return {{}}
            else return {};
        );
        results := {};
        upper := min(maxAllowed, remaining);
        lower := if remaining > (parts - 1) * maxAllowed then remaining - (parts - 1) * maxAllowed else 0;
        -- Descending first part gives dominance-compatible order, matching
        -- allPartitions and Doty's Schur/symmetric-group tables.
        for x in reverse toList(lower..upper) do (
            for sub in helper(parts - 1, remaining - x, x) do
                results = append(results, prepend(x, sub));
        );
        results
    );
    helper(n, r, s)
)
boundedPartitions (ZZ, ZZ) := (n, r) -> boundedPartitions(n, r, r)

-- conjugate(mu): conjugate (transpose) partition.
conjugatePartition = method()
conjugatePartition List := mu -> (
    if not validPartitionList mu then
        error "conjugatePartition: expected a weakly decreasing list of non-negative integers";
    if #mu == 0 then return {};
    if mu#0 == 0 then return {};
    L := mu#0;
    for j from 1 to L list (
        cnt := 0;
        for x in mu do if x >= j then cnt = cnt + 1;
        cnt
    )
)

----------------------------------------------------------------------
-- Character utilities (Doty's Chapter 1)
----------------------------------------------------------------------
--
-- Characters here are HashTable's: weight (List of Dynkin coords) ->
-- multiplicity (ZZ).  Doty's GAP package stores them as flat lists
-- [w_1, m_1, w_2, m_2, ...]; we use HashTable for O(1) lookup.

-- characterDim(ch): sum of multiplicities = total dimension.
characterDim = method()
characterDim HashTable := ch -> sum values ch

-- productCharacter(ch1, ch2): tensor-product character.  The product of
-- characters of two G-modules is the character of their tensor product.
productCharacter = method()
productCharacter (HashTable, HashTable) := (ch1, ch2) -> (
    out := new MutableHashTable;
    for w1 in keys ch1 do (
        m1 := ch1#w1;
        for w2 in keys ch2 do (
            m2 := ch2#w2;
            w := for k from 0 to #w1 - 1 list w1#k + w2#k;
            out#w = (if out#?w then out#w else 0) + m1 * m2;
        );
    );
    new HashTable from out
)

-- differenceCharacter(ch1, ch2): ch1 - ch2 if it is a character (i.e.,
-- non-negative everywhere).  Returns null if not.
differenceCharacter = method()
differenceCharacter (HashTable, HashTable) := (ch1, ch2) -> (
    out := new MutableHashTable;
    for w in keys ch1 do out#w = ch1#w;
    for w in keys ch2 do (
        cur := if out#?w then out#w else 0;
        newVal := cur - ch2#w;
        if newVal < 0 then return null;
        if newVal == 0 then (
            if out#?w then remove(out, w);
        ) else out#w = newVal;
    );
    new HashTable from out
)

-- sortedCharacter(ch): returns a list of (weight, mult) pairs sorted by
-- reverse-height order (high weights first).  Useful for display and
-- iteration order.
sortedCharacter = method()
sortedCharacter HashTable := ch -> (
    -- A raw HashTable carries no Cartan datum, so root depth is not defined.
    -- Use deterministic reverse lexicographic order.  For intrinsic root-depth
    -- order, construct a WeylCharacter and call characterTerms.
    for w in rsort keys ch list (w, ch#w)
)

----------------------------------------------------------------------
-- Weight predicates
----------------------------------------------------------------------

-- isDominantWeight(wt): all Dynkin coordinates are non-negative integers.
-- Doty calls this
-- @TT "IsDominant"@; we use a different name because KacMoody already
-- exports an `isDominant` that takes a Cartan matrix.
isDominantWeight = method()
isDominantWeight List := wt -> all(wt, x -> instance(x, ZZ) and x >= 0)

-- isRestrictedWeight(p, wt): p is prime and all Dynkin coordinates are
-- integers in [0, p-1].
isRestrictedWeight = method()
isRestrictedWeight (ZZ, List) := (p, wt) -> (
    if not isPrime p then error "isRestrictedWeight: prime expected";
    all(wt, x -> instance(x, ZZ) and x >= 0 and x < p)
)

-- highestPrimePower(p, n): the smallest non-negative k such that p^k > n.
-- For n > 0 this is 1 + floor(log_p(n)); at n = 0 it is zero.
-- The modular routines use dividedPowerHeight instead;
-- this remains a public arithmetic utility.
highestPrimePower = method()
highestPrimePower (ZZ, ZZ) := (p, n) -> (
    if not isPrime p then error "highestPrimePower: prime expected";
    if n < 0 then error "highestPrimePower: n must be non-negative";
    k := 0;
    pk := 1;
    while pk <= n do (k = k + 1; pk = pk * p);
    k
)

-- pRegular: a partition is p-regular iff its conjugate is p-restricted.
-- A partition is p-restricted iff every consecutive difference (and the last
-- part) is < p.

pRestrictedQ = method()
pRestrictedQ (ZZ, List) := (p, mu) -> (
    if p < 2 then error "pRestrictedQ: p must be at least 2";
    if not validPartitionList mu then
        error "pRestrictedQ: expected a weakly decreasing list of non-negative integers";
    if #mu == 0 then return true;
    n := #mu;
    if mu#(n - 1) >= p then return false;
    for i from 0 to n - 2 do
        if mu#i - mu#(i + 1) >= p then return false;
    true
)

pRegularQ = method()
pRegularQ (ZZ, List) := (p, mu) -> pRestrictedQ(p, conjugatePartition mu)

pRestrictedPartitions = method()
pRestrictedPartitions (ZZ, ZZ) := (p, n) -> (
    if not isPrime p then error "pRestrictedPartitions: p must be prime";
    select(allPartitions n, mu -> pRestrictedQ(p, mu))
)

pRegularPartitions = method()
pRegularPartitions (ZZ, ZZ) := (p, n) -> (
    if not isPrime p then error "pRegularPartitions: p must be prime";
    select(allPartitions n, mu -> pRegularQ(p, mu))
)

-- Mullineux map: the involution on p-regular partitions characterized by
-- D^(m_p(lambda)) = D^lambda tensor sgn.
mullineux = method()
mullineux (ZZ, List) := (p, mu) -> (
    if not isPrime p then error "mullineux: p must be prime";
    if #mu == 0 then return {};
    if not pRegularQ(p, mu) then error "mullineux: partition is p-singular";
    if p == 2 then return mu;     -- p=2 case: identity
    -- Recursive Mullineux algorithm (row-stripping form).
    helper := null;
    helper = (lambda) -> (
        if #lambda == 0 then return {};
        if lambda#0 == 0 then return {};
        r := #lambda;
        prim := 0;
        newLambda := new MutableList from lambda;
        row := 0;
        while row < #newLambda and newLambda#row > 0 do (
            newLambda#row = newLambda#row - 1;
            prim = prim + 1;
            if prim % p == 0 or (
                row + 1 < #newLambda and newLambda#row < newLambda#(row + 1)
            ) then row = row + 1;
        );
        -- Strip trailing zeros.
        cleaned := toList newLambda;
        while #cleaned > 0 and cleaned#(#cleaned - 1) == 0 do
            cleaned = drop(cleaned, -1);
        recurseResult := helper cleaned;
        newr := if prim % p == 0 then prim - r else prim + 1 - r;
        if newr < 0 then newr = 0;
        result := new MutableList from recurseResult;
        while #result < newr do result#(#result) = 0;
        rowRev := newr - 1;
        while prim > 0 and rowRev >= 0 do (
            if rowRev >= #result then result#rowRev = 0;
            result#rowRev = result#rowRev + 1;
            prim = prim - 1;
            if prim % p == 0 or (
                rowRev > 0 and result#rowRev > result#(rowRev - 1)
            ) then rowRev = rowRev - 1;
        );
        toList result
    );
    helper mu
)

-- schurAlgebraWeylModule(p, mu): the Weyl module for the Schur algebra
-- S(n, r) where n = length(mu) and r = sum(mu), of highest weight given
-- by compositionToWeight(mu).  Returns a WeylModuleZ.
schurAlgebraWeylModule = method()
schurAlgebraWeylModule (ZZ, List) := (p, mu) -> (
    if not isPrime p then error "schurAlgebraWeylModule: prime expected";
    if #mu == 0 then error "schurAlgebraWeylModule: empty partition";
    if not validPartitionList mu then
        error "schurAlgebraWeylModule: expected a partition";
    muPadded := if #mu == 1 then append(mu, 0) else mu;
    n := #muPadded;
    if n < 2 then error "schurAlgebraWeylModule: need length >= 2";
    A := cartanA n;     -- Type A_{n-1} (= sl_n)
    lambda := compositionToWeight muPadded;
    weylModuleZ(A, lambda)
)

-- schurAlgebraDecompositionMatrix(p, n, r): matrix where rows and columns
-- are indexed by boundedPartitions(n, r, r), in that order, and entry (i, j)
-- is the multiplicity of L(comp_j) in V(comp_i) mod p.
schurAlgebraDecompositionMatrix = method()
schurAlgebraDecompositionMatrix (ZZ, ZZ, ZZ) := (p, n, r) -> (
    if not isPrime p then error "schurAlgebraDecompositionMatrix: prime expected";
    if n < 1 then error "schurAlgebraDecompositionMatrix: n must be positive";
    if r < 0 then error "schurAlgebraDecompositionMatrix: r must be non-negative";
    -- S(1,r) is the ground field and has one Weyl and one simple module.
    if n == 1 then return matrix(ZZ, {{1}});
    ptns := boundedPartitions(n, r, r);
    if #ptns == 0 then return matrix(ZZ, {{}});
    A := cartanA n;
    rows := for mu in ptns list (
        W := weylModuleZ(A, compositionToWeight mu);
        dn := decompositionNumbers(W, p);
        for nu in ptns list (
            wtNu := compositionToWeight nu;
            if dn#?wtNu then dn#wtNu else 0
        )
    );
    matrix rows
)

-- symmetricGroupDecompositionMatrix(p, n): the decomposition matrix of S_n
-- in characteristic p.  Rows = partitions of n (ordered as in allPartitions),
-- columns = p-restricted partitions of n.  Computed by restricting the
-- Schur-algebra decomposition matrix for S(n, n).
symmetricGroupDecompositionMatrix = method()
symmetricGroupDecompositionMatrix (ZZ, ZZ) := (p, n) -> (
    if not isPrime p then error "symmetricGroupDecompositionMatrix: prime expected";
    if n < 1 then
        error "symmetricGroupDecompositionMatrix: n must be positive";
    if n == 1 then return matrix(ZZ, {{1}});
    ptns := boundedPartitions(n, n, n);
    M := schurAlgebraDecompositionMatrix(p, n, n);
    -- Strip trailing zeros from each ptn for matching against pRestricted.
    strip := mu -> (
        c := mu;
        while #c > 0 and c#(#c - 1) == 0 do c = drop(c, -1);
        c
    );
    isPRestrictedColumn := for mu in ptns list pRestrictedQ(p, strip mu);
    keepCols := positions(isPRestrictedColumn, b -> b);
    if #keepCols == 0 then return matrix(ZZ, {{}});
    submatrix(M, , keepCols)
)

-- symmetricGroupDecompositionNumbers(p, mu): the decomposition numbers
-- of the dual Specht module S^mu in characteristic p.  Returns a
-- HashTable nu -> [S^mu : D^nu], indexed by p-restricted partitions of
-- the same degree.  Mirrors Doty's GAP @TT "SymmetricGroupDecompositionNumbers"@.
symmetricGroupDecompositionNumbers = method()
symmetricGroupDecompositionNumbers (ZZ, List) := (p, mu) -> (
    if not isPrime p then error "symmetricGroupDecompositionNumbers: prime expected";
    if not validPartitionList mu then
        error "symmetricGroupDecompositionNumbers: expected a partition";
    -- Trailing zeros do not change a partition and must not change the
    -- rank selected below.
    muStripped := mu;
    while #muStripped > 0 and muStripped#(#muStripped - 1) == 0 do
        muStripped = drop(muStripped, -1);
    n := sum muStripped;
    if n < 1 then
        error "symmetricGroupDecompositionNumbers: expected a partition of a positive integer";
    if n == 1 then return new HashTable from {{1} => 1};
    -- Pad mu to length n (so it is a composition of n).
    padded := muStripped | for k from #muStripped + 1 to n list 0;
    A := cartanA n;
    W := weylModuleZ(A, compositionToWeight padded);
    dn := decompositionNumbers(W, p);
    -- Restrict to p-restricted partitions (= the irreducible labels).
    out := new MutableHashTable;
    for nu in pRestrictedPartitions(p, n) do (
        nuPadded := nu | for k from #nu + 1 to n list 0;
        wt := compositionToWeight nuPadded;
        if dn#?wt and dn#wt > 0 then out#nu = dn#wt;
    );
    new HashTable from out
)

----------------------------------------------------------------------
-- Module invariants and quotient coordinates
----------------------------------------------------------------------
--
-- These methods extend the core implementation without changing the representation of WeylModuleZ,
-- SubWeylModuleFp, or QuotientWeylModuleFp.

----------------------------------------------------------------------
-- Quotient weight spaces and the chosen coordinate splitting
----------------------------------------------------------------------

-- Coerce a vector, or a block of column vectors, to a matrix over R.
qolProtocolMatrix = (R, numberOfRows, value, caller) -> (
    if instance(value, List) then (
        if #value != numberOfRows then
            error(caller | ": expected a vector of length " |
                  toString numberOfRows);
        return transpose matrix(R, {value});
    );
    if not instance(value, Matrix) then
        error(caller | ": expected a List or Matrix");
    if numrows value != numberOfRows then
        error(caller | ": expected a matrix with " |
              toString numberOfRows | " rows");
    if numberOfRows == 0 then
        return matrix mutableMatrix(R, 0, numcols value);
    if ring value === R then value else matrix(R, entries value)
)

-- The abstract quotient weight space is represented in its own chosen
-- coordinates.  Its displayed basis is therefore the identity matrix.
weightSpace(QuotientWeylModuleFp, List) := opts -> (Q, muUser) -> (
    W := Q#"ambient";
    A := W#"cartan";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    qDimAt := Q#"quotientDimAt";
    if not qDimAt#?mu then return null;
    id_((ZZ/(Q#"prime"))^(qDimAt#mu))
)

weightSpaces QuotientWeylModuleFp := opts -> Q -> (
    W := Q#"ambient";
    A := W#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "weightSpaces");
    Fp := ZZ/(Q#"prime");
    qDimAt := Q#"quotientDimAt";
    new HashTable from
        for mu in sort select(keys qDimAt, w -> qDimAt#w > 0) list
            fromDynkin(A, mu, opts.WeightFormat) => id_(Fp^(qDimAt#mu))
)

basisVecs QuotientWeylModuleFp := opts -> Q -> (
    W := Q#"ambient";
    A := W#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "basisVecs");
    Fp := ZZ/(Q#"prime");
    qDimAt := Q#"quotientDimAt";
    ordered := select(W#"weightsList", w -> qDimAt#w > 0);
    if opts.WeightOrder === "Lex" then ordered = rsort ordered
    else if opts.WeightOrder =!= "RootDepth" then
        error "basisVecs: WeightOrder must be \"RootDepth\" or \"Lex\"";
    flatten for mu in ordered list (
        qDim := qDimAt#mu;
        identity := id_(Fp^qDim);
        muOut := fromDynkin(A, mu, opts.WeightFormat);
        for j from 0 to qDim - 1 list
            (muOut, submatrix(identity, , {j}))
    )
)

-- quotientProjection(Q,mu) is the matrix V_mu -> Q_mu in the ambient
-- basis and the chosen quotient basis.  It is canonical once the quotient
-- coordinates stored by Q have been fixed.
quotientProjection = method(Options => {WeightFormat => "Dynkin"})
quotientProjection(QuotientWeylModuleFp, List) := opts -> (Q, muUser) -> (
    W := Q#"ambient";
    A := W#"cartan";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    weightMultAt := W#"weightMultAt";
    qDimAt := Q#"quotientDimAt";
    if not weightMultAt#?mu or not qDimAt#?mu then return null;
    dAmbient := weightMultAt#mu;
    qDim := qDimAt#mu;
    Fp := ZZ/(Q#"prime");
    if qDim == 0 then return matrix mutableMatrix(Fp, 0, dAmbient);
    inverseChange := (Q#"bigMatInvAt")#mu;
    if inverseChange === null then
        error "quotientProjection: quotient change-of-basis matrix is unavailable";
    sRank := (Q#"sRankAt")#mu;
    quotientRows := for j from 0 to qDim - 1 list sRank + j;
    ambientColumns := for j from 0 to dAmbient - 1 list j;
    submatrix(inverseChange, quotientRows, ambientColumns)
)

-- quotientSection(Q,mu) records the standard-basis representatives chosen
-- during quotient construction.  It is a linear section of the projection,
-- but it is not a canonical map of representations.
quotientSection = method(Options => {WeightFormat => "Dynkin"})
quotientSection(QuotientWeylModuleFp, List) := opts -> (Q, muUser) -> (
    W := Q#"ambient";
    A := W#"cartan";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    weightMultAt := W#"weightMultAt";
    qDimAt := Q#"quotientDimAt";
    if not weightMultAt#?mu or not qDimAt#?mu then return null;
    dAmbient := weightMultAt#mu;
    qDim := qDimAt#mu;
    Fp := ZZ/(Q#"prime");
    section := mutableMatrix(Fp, dAmbient, qDim);
    complementIndices := (Q#"complementIdxAt")#mu;
    for j from 0 to qDim - 1 do
        section_(complementIndices#j, j) = 1_Fp;
    matrix section
)

-- The chosen quotient basis, displayed by its ambient representatives.
-- Thus quotientBasis and quotientSection intentionally return the same
-- matrix; weightSpace(Q,mu) gives the identity basis in abstract quotient
-- coordinates.
quotientBasis = method(Options => {WeightFormat => "Dynkin"})
quotientBasis(QuotientWeylModuleFp, List) := opts -> (Q, muUser) ->
    quotientSection(Q, muUser, WeightFormat => opts.WeightFormat)

projectToQuotient = method(Options => {WeightFormat => "Dynkin"})
projectToQuotient(QuotientWeylModuleFp, List, List) := opts ->
    (Q, muUser, value) -> (
        projection := quotientProjection(Q, muUser,
                                         WeightFormat => opts.WeightFormat);
        if projection === null then return null;
        block := qolProtocolMatrix(ZZ/(Q#"prime"), numcols projection,
                                   value, "projectToQuotient");
        projection * block
    )

projectToQuotient(QuotientWeylModuleFp, List, Matrix) := opts ->
    (Q, muUser, value) -> (
        projection := quotientProjection(Q, muUser,
                                         WeightFormat => opts.WeightFormat);
        if projection === null then return null;
        block := qolProtocolMatrix(ZZ/(Q#"prime"), numcols projection,
                                   value, "projectToQuotient");
        projection * block
    )

liftFromQuotient = method(Options => {WeightFormat => "Dynkin"})
liftFromQuotient(QuotientWeylModuleFp, List, List) := opts ->
    (Q, muUser, value) -> (
        section := quotientSection(Q, muUser,
                                   WeightFormat => opts.WeightFormat);
        if section === null then return null;
        block := qolProtocolMatrix(ZZ/(Q#"prime"), numcols section,
                                   value, "liftFromQuotient");
        section * block
    )

liftFromQuotient(QuotientWeylModuleFp, List, Matrix) := opts ->
    (Q, muUser, value) -> (
        section := quotientSection(Q, muUser,
                                   WeightFormat => opts.WeightFormat);
        if section === null then return null;
        block := qolProtocolMatrix(ZZ/(Q#"prime"), numcols section,
                                   value, "liftFromQuotient");
        section * block
    )

----------------------------------------------------------------------
-- Module invariants and construction choices
----------------------------------------------------------------------

characteristic = method()
characteristic WeylModuleZ := W -> 0
characteristic SubWeylModuleFp := S -> S#"prime"
characteristic QuotientWeylModuleFp := Q -> Q#"prime"

coefficientRing WeylModuleZ := W -> ZZ
coefficientRing SubWeylModuleFp := S -> ZZ/(S#"prime")
coefficientRing QuotientWeylModuleFp := Q -> ZZ/(Q#"prime")

-- An integral Weyl module is its own ambient module.
ambientWeylModule WeylModuleZ := W -> W

rootSystemType = method()
rootSystemType WeylModuleZ := W -> classifyFiniteCartan(W#"cartan")
rootSystemType SubWeylModuleFp := S ->
    rootSystemType(S#"ambient")
rootSystemType QuotientWeylModuleFp := Q ->
    rootSystemType(Q#"ambient")

-- lieType returns the Cartan letter; rootSystemType retains both the letter
-- and rank as the pair returned by classifyFiniteCartan.
lieType = method()
lieType WeylModuleZ := W -> (
    datum := rootSystemType W;
    if datum === null then null else datum#0
)
lieType SubWeylModuleFp := S -> lieType(S#"ambient")
lieType QuotientWeylModuleFp := Q -> lieType(Q#"ambient")

lieRank = method()
lieRank WeylModuleZ := W -> numrows(W#"cartan")
lieRank SubWeylModuleFp := S -> lieRank(S#"ambient")
lieRank QuotientWeylModuleFp := Q -> lieRank(Q#"ambient")

weightMultiplicity(WeylModuleZ, List) := opts -> (W, muUser) -> (
    mu := toDynkin(W#"cartan", muUser, opts.WeightFormat);
    if (W#"weightMultAt")#?mu then (W#"weightMultAt")#mu else 0
)

weightMultiplicity(SubWeylModuleFp, List) := opts -> (S, muUser) -> (
    mu := toDynkin((S#"ambient")#"cartan", muUser, opts.WeightFormat);
    if (S#"rankAt")#?mu then (S#"rankAt")#mu else 0
)

weightMultiplicity(QuotientWeylModuleFp, List) := opts -> (Q, muUser) -> (
    mu := toDynkin((Q#"ambient")#"cartan", muUser, opts.WeightFormat);
    if (Q#"quotientDimAt")#?mu then (Q#"quotientDimAt")#mu else 0
)

moduleMetadata = method(Options => {WeightFormat => "Dynkin"})

moduleMetadata WeylModuleZ := opts -> W -> new HashTable from {
    "moduleType" => "WeylModuleZ",
    "rootSystemType" => rootSystemType W,
    "lieType" => lieType W,
    "rank" => lieRank W,
    "cartanMatrix" => ambientCartan W,
    "characteristic" => characteristic W,
    "coefficientRing" => coefficientRing W,
    "highestWeight" => highestWeight(W, WeightFormat => opts.WeightFormat),
    "dimension" => dim W
}

moduleMetadata SubWeylModuleFp := opts -> S -> (
    W := S#"ambient";
    topWeight := try highestWeight(S, WeightFormat => opts.WeightFormat)
                 else null;
    new HashTable from {
        "moduleType" => "SubWeylModuleFp",
        "rootSystemType" => rootSystemType S,
        "lieType" => lieType S,
        "rank" => lieRank S,
        "cartanMatrix" => ambientCartan S,
        "characteristic" => characteristic S,
        "coefficientRing" => coefficientRing S,
        "highestWeight" => topWeight,
        "ambientHighestWeight" =>
            highestWeight(W, WeightFormat => opts.WeightFormat),
        "dimension" => dim S,
        "ambientDimension" => dim W,
        "codimension" => dim W - dim S
    }
)

moduleMetadata QuotientWeylModuleFp := opts -> Q -> (
    W := Q#"ambient";
    topWeight := try highestWeight(Q, WeightFormat => opts.WeightFormat)
                 else null;
    new HashTable from {
        "moduleType" => "QuotientWeylModuleFp",
        "rootSystemType" => rootSystemType Q,
        "lieType" => lieType Q,
        "rank" => lieRank Q,
        "cartanMatrix" => ambientCartan Q,
        "characteristic" => characteristic Q,
        "coefficientRing" => coefficientRing Q,
        "highestWeight" => topWeight,
        "ambientHighestWeight" =>
            highestWeight(W, WeightFormat => opts.WeightFormat),
        "dimension" => dim Q,
        "ambientDimension" => dim W,
        "kernelDimension" => dim(Q#"kernel")
    }
)

qolProtocolPackageVersion = () ->
    (options package "WeylModules")#Version

qolProtocolMacaulay2Version = () -> version#"VERSION"

constructionData = method()

constructionData WeylModuleZ := W -> new HashTable from {
    "moduleType" => "WeylModuleZ",
    "packageVersion" => qolProtocolPackageVersion(),
    "macaulay2Version" => qolProtocolMacaulay2Version(),
    "rootSystemType" => rootSystemType W,
    "cartanMatrix" => W#"cartan",
    "highestWeightDynkin" => W#"highestWeight",
    "characteristic" => 0,
    "coefficientRing" => ZZ,
    "dimension" => dim W,
    "positiveRootOrder" => W#"positiveRoots",
    "pbwOrdering" =>
        "positive roots by increasing height, then lexicographically; divided-power exponent tuples",
    "basisConvention" => "chosen integral PBW cosets",
    "algorithmVersion" => "integral-pbw-v1"
}

constructionData SubWeylModuleFp := S -> (
    W := S#"ambient";
    new HashTable from {
        "moduleType" => "SubWeylModuleFp",
        "packageVersion" => qolProtocolPackageVersion(),
        "macaulay2Version" => qolProtocolMacaulay2Version(),
        "rootSystemType" => rootSystemType S,
        "cartanMatrix" => W#"cartan",
        "ambientHighestWeightDynkin" => W#"highestWeight",
        "characteristic" => S#"prime",
        "coefficientRing" => coefficientRing S,
        "dimension" => dim S,
        "ambientDimension" => dim W,
        "positiveRootOrder" => W#"positiveRoots",
        "basisConvention" => "ambient Weyl-module coordinates by weight",
        "algorithmVersion" => "divided-power-closure-v1"
    }
)

constructionData QuotientWeylModuleFp := Q -> (
    W := Q#"ambient";
    new HashTable from {
        "moduleType" => "QuotientWeylModuleFp",
        "packageVersion" => qolProtocolPackageVersion(),
        "macaulay2Version" => qolProtocolMacaulay2Version(),
        "rootSystemType" => rootSystemType Q,
        "cartanMatrix" => W#"cartan",
        "ambientHighestWeightDynkin" => W#"highestWeight",
        "characteristic" => Q#"prime",
        "coefficientRing" => coefficientRing Q,
        "dimension" => dim Q,
        "ambientDimension" => dim W,
        "kernelDimension" => dim(Q#"kernel"),
        "positiveRootOrder" => W#"positiveRoots",
        "basisConvention" =>
            "quotient coordinates from standard ambient complement columns",
        "sectionIsCanonical" => false,
        "algorithmVersion" => "weightwise-quotient-v1"
    }
)

----------------------------------------------------------------------
-- Direct reduction of an already constructed integral Weyl module
----------------------------------------------------------------------

reduceWeylModule = method()
reduceWeylModule(WeylModuleZ, ZZ) := (W, p) -> (
    if not isPrime p then error "reduceWeylModule: prime expected";
    quotientWeylModule(subWeylModule(W, p, {}))
)

reduction = method()
reduction(WeylModuleZ, ZZ) := (W, p) -> reduceWeylModule(W, p)

-- Existing-constructor vocabulary, without rebuilding the integral module.
weylModule(WeylModuleZ, ZZ) := opts -> (W, p) -> reduceWeylModule(W, p)

weylModule(Matrix, ZZ, List) := opts -> (A, p, lambdaUser) -> (
    -- With an explicit Cartan matrix there is no LieType-dependent natural
    -- convention to infer.  Resolve the high-level constructor's "Auto"
    -- default to the package's canonical Dynkin convention.
    fmt := if opts.WeightFormat === "Auto" then "Dynkin"
           else opts.WeightFormat;
    W := weylModuleZ(A, lambdaUser, WeightFormat => fmt);
    reduceWeylModule(W, p)
)

----------------------------------------------------------------------
-- Linear-algebra operations on submodules of the same ambient module
----------------------------------------------------------------------

qolProtocolCompatibleSubmodules = (S, T) ->
    S#"ambient" === T#"ambient" and S#"prime" == T#"prime"

qolProtocolIndependentColumns = M -> (
    R := ring M;
    numberOfRows := numrows M;
    selected := null;
    selectedRank := 0;
    for j from 0 to numcols M - 1 do (
        column := submatrix(M, , {j});
        candidate := if selected === null then column else selected | column;
        candidateRank := rank candidate;
        if candidateRank > selectedRank then (
            selected = candidate;
            selectedRank = candidateRank;
        );
    );
    if selected === null then matrix mutableMatrix(R, numberOfRows, 0)
    else selected
)

qolProtocolGeneratorsFromBases = basisAt ->
    flatten for mu in keys basisAt list (
        basis := basisAt#mu;
        for j from 0 to numcols basis - 1 list
            (mu, submatrix(basis, , {j}))
    )

qolProtocolSubmoduleFromBases = (W, p, basisAt) -> (
    rankAt := new MutableHashTable;
    for mu in keys basisAt do rankAt#mu = numcols(basisAt#mu);
    makeSubWeylModuleFp(W, p, basisAt, rankAt,
                        qolProtocolGeneratorsFromBases basisAt)
)

isSubmoduleOf = method()
isSubmoduleOf(SubWeylModuleFp, SubWeylModuleFp) := (S, T) -> (
    if not qolProtocolCompatibleSubmodules(S, T) then return false;
    contained := true;
    for mu in keys(S#"basisAt") do (
        sourceBasis := (S#"basisAt")#mu;
        if not (T#"basisAt")#?mu then (
            contained = false;
            break;
        );
        targetBasis := (T#"basisAt")#mu;
        if rank(targetBasis | sourceBasis) > numcols targetBasis then (
            contained = false;
            break;
        );
    );
    contained
)

containsSubmodule = method()
containsSubmodule(SubWeylModuleFp, SubWeylModuleFp) := (S, T) ->
    isSubmoduleOf(T, S)

equalSubmodules = method()
equalSubmodules(SubWeylModuleFp, SubWeylModuleFp) := (S, T) ->
    qolProtocolCompatibleSubmodules(S, T) and
    dim S == dim T and isSubmoduleOf(S, T)

submoduleCodimension = method()
submoduleCodimension SubWeylModuleFp := S -> dim(S#"ambient") - dim S

submoduleSum = method()
submoduleSum(SubWeylModuleFp, SubWeylModuleFp) := (S, T) -> (
    if not qolProtocolCompatibleSubmodules(S, T) then
        error "submoduleSum: submodules must have the same ambient module and characteristic";
    W := S#"ambient";
    p := S#"prime";
    basisAt := new MutableHashTable;
    for mu in W#"weightsList" do (
        hasLeftWeight := (S#"basisAt")#?mu;
        hasRightWeight := (T#"basisAt")#?mu;
        if not hasLeftWeight and not hasRightWeight then continue;
        combined := if hasLeftWeight and hasRightWeight then
                        (S#"basisAt")#mu | (T#"basisAt")#mu
                    else if hasLeftWeight then (S#"basisAt")#mu
                    else (T#"basisAt")#mu;
        independent := qolProtocolIndependentColumns combined;
        if numcols independent > 0 then basisAt#mu = independent;
    );
    qolProtocolSubmoduleFromBases(W, p, basisAt)
)

submoduleSum List := modules -> (
    if #modules == 0 then error "submoduleSum: expected a nonempty list";
    if not all(modules, S -> instance(S, SubWeylModuleFp)) then
        error "submoduleSum: every entry must be a SubWeylModuleFp";
    fold((S, T) -> submoduleSum(S, T), modules)
)

submoduleIntersection = method()
submoduleIntersection(SubWeylModuleFp, SubWeylModuleFp) := (S, T) -> (
    if not qolProtocolCompatibleSubmodules(S, T) then
        error "submoduleIntersection: submodules must have the same ambient module and characteristic";
    W := S#"ambient";
    p := S#"prime";
    basisAt := new MutableHashTable;
    for mu in W#"weightsList" do (
        if not (S#"basisAt")#?mu or not (T#"basisAt")#?mu then
            continue;
        leftBasis := (S#"basisAt")#mu;
        rightBasis := (T#"basisAt")#mu;
        leftColumns := numcols leftBasis;
        relationKernel := syz(leftBasis | (-rightBasis));
        if numcols relationKernel == 0 then continue;
        leftRows := for j from 0 to leftColumns - 1 list j;
        kernelColumns := for j from 0 to numcols relationKernel - 1 list j;
        leftCoefficients := submatrix(relationKernel, leftRows,
                                      kernelColumns);
        intersectionBasis :=
            qolProtocolIndependentColumns(leftBasis * leftCoefficients);
        if numcols intersectionBasis > 0 then
            basisAt#mu = intersectionBasis;
    );
    qolProtocolSubmoduleFromBases(W, p, basisAt)
)

submoduleIntersection List := modules -> (
    if #modules == 0 then
        error "submoduleIntersection: expected a nonempty list";
    if not all(modules, S -> instance(S, SubWeylModuleFp)) then
        error "submoduleIntersection: every entry must be a SubWeylModuleFp";
    fold((S, T) -> submoduleIntersection(S, T), modules)
)

----------------------------------------------------------------------
-- Prime-free Schur-algebra constructor
----------------------------------------------------------------------

-- The returned module is integral; the characteristic belongs to a later
-- reduction step.  The historical (p,mu) overload remains available.
schurAlgebraWeylModule List := mu -> (
    if #mu == 0 then error "schurAlgebraWeylModule: empty partition";
    if not validPartitionList mu then
        error "schurAlgebraWeylModule: expected a partition";
    muPadded := if #mu == 1 then append(mu, 0) else mu;
    A := cartanA(#muPadded);
    weylModuleZ(A, compositionToWeight muPadded)
)

----------------------------------------------------------------------
-- More informative compact displays
----------------------------------------------------------------------

qolProtocolTypeLabel = M -> (
    datum := rootSystemType M;
    if datum === null then "rank " | toString(lieRank M)
    else datum#0 | "_" | toString(datum#1)
)

net WeylModuleZ := W ->
    "WeylModuleZ(type " | qolProtocolTypeLabel W |
    ", highest weight " | toString(W#"highestWeight") |
    ", dim " | toString(dim W) | ", over ZZ)"

net SubWeylModuleFp := S ->
    "SubWeylModuleFp(type " | qolProtocolTypeLabel S |
    ", dim " | toString(dim S) |
    ", codim " | toString(submoduleCodimension S) |
    ", char " | toString(characteristic S) |
    ", in V(" | toString((S#"ambient")#"highestWeight") | "))"

net QuotientWeylModuleFp := Q ->
    "QuotientWeylModuleFp(type " | qolProtocolTypeLabel Q |
    ", dim " | toString(dim Q) |
    ", kernel dim " | toString(dim(Q#"kernel")) |
    ", char " | toString(characteristic Q) |
    ", from V(" | toString((Q#"ambient")#"highestWeight") | "))"

----------------------------------------------------------------------
-- Genuine subquotients T/S
----------------------------------------------------------------------
--
-- A SubquotientWeylModuleFp represents T/S for submodules S <= T of a
-- common Weyl module V over F_p.  At each weight, a complement to S_mu in
-- T_mu is selected from the stored basis columns of T_mu.  All displayed
-- quotient vectors use these coordinates.  The associated section into V
-- is useful computationally but is neither canonical nor a module map.

SubquotientWeylModuleFp = new Type of HashTable

-- For a full-column-rank d x t matrix B, construct a t x d left inverse.
-- The extension away from image(B) depends on the selected pivot rows; its
-- restriction to image(B) is the actual coordinate map.
qolProtocolLeftInverse = B -> (
    R := ring B;
    d := numrows B;
    t := numcols B;
    if t == 0 then return matrix mutableMatrix(R, 0, d);
    selectedRows := {};
    rowBlock := null;
    rowRank := 0;
    for i from 0 to d - 1 do (
        row := submatrix(B, {i}, );
        candidate := if rowBlock === null then row else rowBlock || row;
        candidateRank := rank candidate;
        if candidateRank > rowRank then (
            selectedRows = append(selectedRows, i);
            rowBlock = candidate;
            rowRank = candidateRank;
        );
        if rowRank == t then break;
    );
    if rowRank != t then
        error "subquotientWeylModule: a stored upper-submodule basis is not independent";
    rowSelector := mutableMatrix(R, t, d);
    for i from 0 to t - 1 do rowSelector_(i, selectedRows#i) = 1_R;
    inverse(rowBlock) * matrix rowSelector
)

subquotientWeylModule = method()
subquotientWeylModule(SubWeylModuleFp, SubWeylModuleFp) :=
    (lower, upper) -> (
        if not qolProtocolCompatibleSubmodules(lower, upper) then
            error "subquotientWeylModule: lower and upper submodules must have the same ambient module and characteristic";
        if not isSubmoduleOf(lower, upper) then
            error "subquotientWeylModule: the first submodule must be contained in the second";

        W := lower#"ambient";
        p := lower#"prime";
        Fp := ZZ/p;
        A := W#"cartan";
        r := numrows A;
        weightMultAt := W#"weightMultAt";

        quotientDimAt := new MutableHashTable;
        complementBasisAt := new MutableHashTable;
        projectionAt := new MutableHashTable;
        sectionAt := new MutableHashTable;

        for mu in W#"weightsList" do (
            dAmbient := weightMultAt#mu;
            lowerBasis := if (lower#"basisAt")#?mu then
                              (lower#"basisAt")#mu else null;
            upperBasis := if (upper#"basisAt")#?mu then
                              (upper#"basisAt")#mu else null;
            lowerRank := if lowerBasis === null then 0 else numcols lowerBasis;
            upperRank := if upperBasis === null then 0 else numcols upperBasis;
            quotientDim := upperRank - lowerRank;
            if quotientDim < 0 then
                error "subquotientWeylModule: inconsistent weight-space dimensions";
            quotientDimAt#mu = quotientDim;

            if upperRank == 0 then (
                complementBasisAt#mu = matrix mutableMatrix(Fp, dAmbient, 0);
                projectionAt#mu = matrix mutableMatrix(Fp, 0, dAmbient);
                sectionAt#mu = matrix mutableMatrix(Fp, dAmbient, 0);
                continue;
            );

            currentBasis := lowerBasis;
            currentRank := lowerRank;
            complementColumns := {};
            for j from 0 to upperRank - 1 do (
                if #complementColumns == quotientDim then break;
                column := submatrix(upperBasis, , {j});
                candidate := if currentBasis === null then column
                             else currentBasis | column;
                candidateRank := rank candidate;
                if candidateRank > currentRank then (
                    complementColumns = append(complementColumns, column);
                    currentBasis = candidate;
                    currentRank = candidateRank;
                );
            );
            if #complementColumns != quotientDim then
                error "subquotientWeylModule: could not complement the lower weight space inside the upper weight space";

            section := if quotientDim == 0 then
                           matrix mutableMatrix(Fp, dAmbient, 0)
                       else if quotientDim == 1 then first complementColumns
                       else fold((left, right) -> left | right,
                                 complementColumns);
            sectionAt#mu = section;
            complementBasisAt#mu = section;

            fullBasis := if lowerBasis === null then section
                         else if quotientDim == 0 then lowerBasis
                         else lowerBasis | section;
            coordinateMap := qolProtocolLeftInverse fullBasis;
            if quotientDim == 0 then
                projectionAt#mu = matrix mutableMatrix(Fp, 0, dAmbient)
            else (
                quotientRows := for j from 0 to quotientDim - 1 list
                                    lowerRank + j;
                ambientColumns := for j from 0 to dAmbient - 1 list j;
                projectionAt#mu = submatrix(coordinateMap, quotientRows,
                                            ambientColumns);
            );
        );

        actFQ := new MutableHashTable;
        actEQ := new MutableHashTable;

        -- f_i : (T/S)_(mu+alpha_i) -> (T/S)_mu.
        for mu in W#"weightsList" do (
            for i from 1 to r do (
                alphaIDynkin := for k from 0 to r - 1 list A_(k, i - 1);
                sourceWeight := for k from 0 to r - 1 list
                                    mu#k + alphaIDynkin#k;
                if not quotientDimAt#?sourceWeight then continue;
                if quotientDimAt#sourceWeight == 0 or
                   quotientDimAt#mu == 0 then continue;
                ambientAction := actionF(W, mu, i);
                if ambientAction === null then continue;
                actFQ#(mu, i) = projectionAt#mu *
                    (ambientAction ** Fp) * sectionAt#sourceWeight;
            );
        );

        -- e_i : (T/S)_mu -> (T/S)_(mu+alpha_i).
        for mu in W#"weightsList" do (
            for i from 1 to r do (
                alphaIDynkin := for k from 0 to r - 1 list A_(k, i - 1);
                targetWeight := for k from 0 to r - 1 list
                                    mu#k + alphaIDynkin#k;
                if not quotientDimAt#?targetWeight then continue;
                if quotientDimAt#mu == 0 or
                   quotientDimAt#targetWeight == 0 then continue;
                ambientAction := actionE(W, mu, i);
                if ambientAction === null then continue;
                actEQ#(mu, i) = projectionAt#targetWeight *
                    (ambientAction ** Fp) * sectionAt#mu;
            );
        );

        totalDimension := sum apply(W#"weightsList",
                                    mu -> quotientDimAt#mu);
        new SubquotientWeylModuleFp from {
            "ambient" => W,
            "lower" => lower,
            "upper" => upper,
            "prime" => p,
            "quotientDimAt" => quotientDimAt,
            "complementBasisAt" => complementBasisAt,
            "projectionAt" => projectionAt,
            "sectionAt" => sectionAt,
            "actFQ" => actFQ,
            "actEQ" => actEQ,
            "dim" => totalDimension
        }
    )

dim SubquotientWeylModuleFp := M -> M#"dim"

ambientWeylModule SubquotientWeylModuleFp := M -> M#"ambient"

lowerSubmodule = method()
lowerSubmodule SubquotientWeylModuleFp := M -> M#"lower"

upperSubmodule = method()
upperSubmodule SubquotientWeylModuleFp := M -> M#"upper"

ambientCartan SubquotientWeylModuleFp := M -> (M#"ambient")#"cartan"

characteristic SubquotientWeylModuleFp := M -> M#"prime"

coefficientRing SubquotientWeylModuleFp := M -> ZZ/(M#"prime")

rootSystemType SubquotientWeylModuleFp := M ->
    rootSystemType(M#"ambient")

lieType SubquotientWeylModuleFp := M -> lieType(M#"ambient")

lieRank SubquotientWeylModuleFp := M -> lieRank(M#"ambient")

highestWeight SubquotientWeylModuleFp := opts -> M -> (
    A := (M#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "highestWeight");
    quotientWeights := select(keys(M#"quotientDimAt"),
                              mu -> (M#"quotientDimAt")#mu > 0);
    mu := uniqueMaximalWeight(A, quotientWeights, "subquotient");
    if mu === null then null else fromDynkin(A, mu, opts.WeightFormat)
)

weights SubquotientWeylModuleFp := opts -> M -> (
    W := M#"ambient";
    A := W#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "weights");
    ordered := select(W#"weightsList",
                      w -> (M#"quotientDimAt")#w > 0);
    if opts.WeightOrder === "Lex" then ordered = rsort ordered
    else if opts.WeightOrder =!= "RootDepth" then
        error "weights: WeightOrder must be \"RootDepth\" or \"Lex\"";
    for mu in ordered list
        fromDynkin(A, mu, opts.WeightFormat)
)

weightSpace(SubquotientWeylModuleFp, List) := opts -> (M, muUser) -> (
    A := (M#"ambient")#"cartan";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    if not (M#"quotientDimAt")#?mu then return null;
    id_((ZZ/(M#"prime"))^((M#"quotientDimAt")#mu))
)

weightSpaces SubquotientWeylModuleFp := opts -> M -> (
    A := (M#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "weightSpaces");
    Fp := ZZ/(M#"prime");
    qDimAt := M#"quotientDimAt";
    new HashTable from
        for mu in sort select(keys qDimAt, w -> qDimAt#w > 0) list
            fromDynkin(A, mu, opts.WeightFormat) => id_(Fp^(qDimAt#mu))
)

basisVecs SubquotientWeylModuleFp := opts -> M -> (
    W := M#"ambient";
    A := W#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "basisVecs");
    Fp := ZZ/(M#"prime");
    qDimAt := M#"quotientDimAt";
    ordered := select(W#"weightsList", w -> qDimAt#w > 0);
    if opts.WeightOrder === "Lex" then ordered = rsort ordered
    else if opts.WeightOrder =!= "RootDepth" then
        error "basisVecs: WeightOrder must be \"RootDepth\" or \"Lex\"";
    flatten for mu in ordered list (
        qDim := qDimAt#mu;
        identity := id_(Fp^qDim);
        muOut := fromDynkin(A, mu, opts.WeightFormat);
        for j from 0 to qDim - 1 list
            (muOut, submatrix(identity, , {j}))
    )
)

character SubquotientWeylModuleFp := opts -> M -> (
    A := (M#"ambient")#"cartan";
    chDynkin := new HashTable from
        for mu in sort select(keys(M#"quotientDimAt"),
                              w -> (M#"quotientDimAt")#w > 0) list
            mu => (M#"quotientDimAt")#mu;
    characterFromDynkin(A, chDynkin, opts.WeightFormat)
)

weightMultiplicity(SubquotientWeylModuleFp, List) := opts ->
    (M, muUser) -> (
        A := (M#"ambient")#"cartan";
        mu := toDynkin(A, muUser, opts.WeightFormat);
        if (M#"quotientDimAt")#?mu then (M#"quotientDimAt")#mu else 0
    )

dominantWeights SubquotientWeylModuleFp := opts -> M -> (
    A := (M#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "dominantWeights");
    for mu in select(sort select(keys(M#"quotientDimAt"),
                                  w -> (M#"quotientDimAt")#w > 0),
                          isDominantWeight) list
        fromDynkin(A, mu, opts.WeightFormat)
)

dominantWeightSpaces SubquotientWeylModuleFp := opts -> M -> (
    A := (M#"ambient")#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "dominantWeightSpaces");
    Fp := ZZ/(M#"prime");
    qDimAt := M#"quotientDimAt";
    new HashTable from
        for mu in select(keys qDimAt,
                         w -> qDimAt#w > 0 and isDominantWeight w) list
            fromDynkin(A, mu, opts.WeightFormat) => id_(Fp^(qDimAt#mu))
)

actionF(SubquotientWeylModuleFp, List, ZZ) := opts -> (M, muUser, i) -> (
    A := (M#"ambient")#"cartan";
    validateSimpleRootIndex(A, i, "actionF");
    mu := toDynkin(A, muUser, opts.WeightFormat);
    H := M#"actFQ";
    if H#?(mu, i) then H#(mu, i) else null
)

actionE(SubquotientWeylModuleFp, List, ZZ) := opts -> (M, muUser, i) -> (
    A := (M#"ambient")#"cartan";
    validateSimpleRootIndex(A, i, "actionE");
    mu := toDynkin(A, muUser, opts.WeightFormat);
    H := M#"actEQ";
    if H#?(mu, i) then H#(mu, i) else null
)

actionH(SubquotientWeylModuleFp, ZZ, List) := opts -> (M, i, muUser) -> (
    A := (M#"ambient")#"cartan";
    validateSimpleRootIndex(A, i, "actionH");
    mu := toDynkin(A, muUser, opts.WeightFormat);
    if (M#"quotientDimAt")#?mu and (M#"quotientDimAt")#mu > 0
    then mu#(i - 1) else 0
)

-- For a subquotient, this matrix restricts to the quotient projection
-- T_mu -> T_mu/S_mu.  Its values on ambient vectors outside T_mu depend
-- on the stored pivot-row extension and carry no intrinsic meaning.
quotientProjection(SubquotientWeylModuleFp, List) := opts ->
    (M, muUser) -> (
        A := (M#"ambient")#"cartan";
        mu := toDynkin(A, muUser, opts.WeightFormat);
        if not (M#"projectionAt")#?mu then null
        else (M#"projectionAt")#mu
    )

quotientSection(SubquotientWeylModuleFp, List) := opts ->
    (M, muUser) -> (
        A := (M#"ambient")#"cartan";
        mu := toDynkin(A, muUser, opts.WeightFormat);
        if not (M#"sectionAt")#?mu then null
        else (M#"sectionAt")#mu
    )

quotientBasis(SubquotientWeylModuleFp, List) := opts ->
    (M, muUser) -> quotientSection(M, muUser,
                                   WeightFormat => opts.WeightFormat)

projectToQuotient(SubquotientWeylModuleFp, List, List) := opts ->
    (M, muUser, value) -> (
        projection := quotientProjection(M, muUser,
                                         WeightFormat => opts.WeightFormat);
        if projection === null then return null;
        projection * qolProtocolMatrix(ZZ/(M#"prime"), numcols projection,
                                       value, "projectToQuotient")
    )

projectToQuotient(SubquotientWeylModuleFp, List, Matrix) := opts ->
    (M, muUser, value) -> (
        projection := quotientProjection(M, muUser,
                                         WeightFormat => opts.WeightFormat);
        if projection === null then return null;
        projection * qolProtocolMatrix(ZZ/(M#"prime"), numcols projection,
                                       value, "projectToQuotient")
    )

liftFromQuotient(SubquotientWeylModuleFp, List, List) := opts ->
    (M, muUser, value) -> (
        section := quotientSection(M, muUser,
                                   WeightFormat => opts.WeightFormat);
        if section === null then return null;
        section * qolProtocolMatrix(ZZ/(M#"prime"), numcols section,
                                    value, "liftFromQuotient")
    )

liftFromQuotient(SubquotientWeylModuleFp, List, Matrix) := opts ->
    (M, muUser, value) -> (
        section := quotientSection(M, muUser,
                                   WeightFormat => opts.WeightFormat);
        if section === null then return null;
        section * qolProtocolMatrix(ZZ/(M#"prime"), numcols section,
                                    value, "liftFromQuotient")
    )

-- Explicit aliases make code involving both V/S and T/S self-documenting.
subquotientProjection = method(Options => {WeightFormat => "Dynkin"})
subquotientProjection(SubquotientWeylModuleFp, List) := opts ->
    (M, muUser) -> quotientProjection(M, muUser,
                                      WeightFormat => opts.WeightFormat)

subquotientSection = method(Options => {WeightFormat => "Dynkin"})
subquotientSection(SubquotientWeylModuleFp, List) := opts ->
    (M, muUser) -> quotientSection(M, muUser,
                                   WeightFormat => opts.WeightFormat)

subquotientBasis = method(Options => {WeightFormat => "Dynkin"})
subquotientBasis(SubquotientWeylModuleFp, List) := opts ->
    (M, muUser) -> quotientBasis(M, muUser,
                                 WeightFormat => opts.WeightFormat)

projectToSubquotient = method(Options => {WeightFormat => "Dynkin"})
projectToSubquotient(SubquotientWeylModuleFp, List, List) := opts ->
    (M, muUser, value) -> projectToQuotient(
        M, muUser, value, WeightFormat => opts.WeightFormat)
projectToSubquotient(SubquotientWeylModuleFp, List, Matrix) := opts ->
    (M, muUser, value) -> projectToQuotient(
        M, muUser, value, WeightFormat => opts.WeightFormat)

liftFromSubquotient = method(Options => {WeightFormat => "Dynkin"})
liftFromSubquotient(SubquotientWeylModuleFp, List, List) := opts ->
    (M, muUser, value) -> liftFromQuotient(
        M, muUser, value, WeightFormat => opts.WeightFormat)
liftFromSubquotient(SubquotientWeylModuleFp, List, Matrix) := opts ->
    (M, muUser, value) -> liftFromQuotient(
        M, muUser, value, WeightFormat => opts.WeightFormat)

moduleMetadata SubquotientWeylModuleFp := opts -> M -> (
    W := M#"ambient";
    topWeight := try highestWeight(M, WeightFormat => opts.WeightFormat)
                 else null;
    new HashTable from {
        "moduleType" => "SubquotientWeylModuleFp",
        "rootSystemType" => rootSystemType M,
        "lieType" => lieType M,
        "rank" => lieRank M,
        "cartanMatrix" => ambientCartan M,
        "characteristic" => characteristic M,
        "coefficientRing" => coefficientRing M,
        "highestWeight" => topWeight,
        "ambientHighestWeight" =>
            highestWeight(W, WeightFormat => opts.WeightFormat),
        "dimension" => dim M,
        "lowerDimension" => dim(M#"lower"),
        "upperDimension" => dim(M#"upper"),
        "ambientDimension" => dim W
    }
)

constructionData SubquotientWeylModuleFp := M -> (
    W := M#"ambient";
    new HashTable from {
        "moduleType" => "SubquotientWeylModuleFp",
        "packageVersion" => qolProtocolPackageVersion(),
        "macaulay2Version" => qolProtocolMacaulay2Version(),
        "rootSystemType" => rootSystemType M,
        "cartanMatrix" => W#"cartan",
        "ambientHighestWeightDynkin" => W#"highestWeight",
        "characteristic" => M#"prime",
        "coefficientRing" => coefficientRing M,
        "dimension" => dim M,
        "lowerDimension" => dim(M#"lower"),
        "upperDimension" => dim(M#"upper"),
        "ambientDimension" => dim W,
        "positiveRootOrder" => W#"positiveRoots",
        "basisConvention" =>
            "weightwise complements of the lower basis inside the upper basis",
        "sectionIsCanonical" => false,
        "algorithmVersion" => "weightwise-subquotient-v1"
    }
)

net SubquotientWeylModuleFp := M ->
    "SubquotientWeylModuleFp(type " | qolProtocolTypeLabel M |
    ", dim " | toString(dim M) |
    ", interval " | toString(dim(M#"upper")) | "/" |
        toString(dim(M#"lower")) |
    ", char " | toString(characteristic M) |
    ", in V(" | toString((M#"ambient")#"highestWeight") | "))"

weightAsPartition (SubquotientWeylModuleFp, List) := (M, mu) ->
    fundamentalToPartition((M#"ambient")#"cartan", mu)

weightAsDynkin (SubquotientWeylModuleFp, List) := (M, lam) ->
    toDynkin((M#"ambient")#"cartan", lam, "Partition")

characterAsPartition (SubquotientWeylModuleFp, HashTable) := (M, ch) ->
    characterFromDynkin((M#"ambient")#"cartan", ch, "Partition")

characterAsDynkin (SubquotientWeylModuleFp, HashTable) := (M, ch) ->
    characterToDynkin((M#"ambient")#"cartan", ch, "Partition")

-- Weightwise Chevalley actions restricted to a stored submodule basis.
actionE(SubWeylModuleFp, List, ZZ) := opts -> (S, muUser, i) -> (
    W := S#"ambient";
    A := W#"cartan";
    validateSimpleRootIndex(A, i, "actionE");
    mu := toDynkin(A, muUser, opts.WeightFormat);
    alpha := for k from 0 to numrows A - 1 list A_(k, i - 1);
    target := for k from 0 to numrows A - 1 list mu#k + alpha#k;
    if not (S#"basisAt")#?mu or not (S#"basisAt")#?target then return null;
    ambientAction := actionE(W, mu, i);
    if ambientAction === null then return null;
    sourceBasis := (S#"basisAt")#mu;
    targetBasis := (S#"basisAt")#target;
    qolProtocolLeftInverse(targetBasis) *
        (ambientAction ** (ZZ/(S#"prime"))) * sourceBasis
)

actionF(SubWeylModuleFp, List, ZZ) := opts -> (S, muUser, i) -> (
    W := S#"ambient";
    A := W#"cartan";
    validateSimpleRootIndex(A, i, "actionF");
    mu := toDynkin(A, muUser, opts.WeightFormat);
    alpha := for k from 0 to numrows A - 1 list A_(k, i - 1);
    source := for k from 0 to numrows A - 1 list mu#k + alpha#k;
    if not (S#"basisAt")#?mu or not (S#"basisAt")#?source then return null;
    ambientAction := actionF(W, mu, i);
    if ambientAction === null then return null;
    sourceBasis := (S#"basisAt")#source;
    targetBasis := (S#"basisAt")#mu;
    qolProtocolLeftInverse(targetBasis) *
        (ambientAction ** (ZZ/(S#"prime"))) * sourceBasis
)

actionH(SubWeylModuleFp, ZZ, List) := opts -> (S, i, muUser) -> (
    A := (S#"ambient")#"cartan";
    validateSimpleRootIndex(A, i, "actionH");
    mu := toDynkin(A, muUser, opts.WeightFormat);
    if (S#"rankAt")#?mu and (S#"rankAt")#mu > 0
    then mu#(i - 1) else 0
)

----------------------------------------------------------------------
-- Socle filtrations, composition factors, and reduction profiles
----------------------------------------------------------------------
--
-- The original no-option structural methods remain in place.  The
-- RequireComplete overloads below are additional signatures, so existing
-- code keeps its historical behaviour while callers may explicitly demand
-- that every successive quotient used by a socle computation be
-- unambiguous.

SocleAnalysis = new Type of HashTable
ReductionProfile = new Type of HashTable
ExtensionData = new Type of HashTable

----------------------------------------------------------------------
-- Deterministic composition-factor data
----------------------------------------------------------------------

-- Put dominant weights in a deterministic linear extension of dominance:
-- maximal weights first, and reverse lexicographic order among incomparable
-- maxima.  All input weights are in Dynkin coordinates.
qolDominanceOrder = (A, weightList) -> (
    remaining := unique weightList;
    ordered := {};
    while #remaining > 0 do (
        maxima := select(remaining, mu -> not any(remaining, nu ->
            nu != mu and dominanceGE(A, nu, mu)));
        -- A failure here would indicate inconsistent dominance data rather
        -- than a representation-theoretic ambiguity.
        if #maxima == 0 then
            error "compositionFactors: could not order dominant weights";
        maxima = reverse sort maxima;
        ordered = ordered | maxima;
        remaining = select(remaining, mu -> not member(mu, maxima));
    );
    ordered
)

qolCompositionPairs = (A, factorTableDynkin, weightFormat) ->
    (
        validateWeightFormat(A, weightFormat, "compositionFactors");
        for mu in qolDominanceOrder(A, keys factorTableDynkin) list
            (fromDynkin(A, mu, weightFormat), factorTableDynkin#mu)
    )

-- The character decomposition itself is canonical.  The List returned by
-- compositionFactors is only a deterministic presentation of that table.
decompositionNumbers SubWeylModuleFp := opts -> S -> (
    A := (S#"ambient")#"cartan";
    p := S#"prime";
    ch := character(S, WeightFormat => "Dynkin");
    result := decomposeCharacter(A, p, ch);
    characterFromDynkin(A, result, opts.WeightFormat)
)

decompositionNumbers QuotientWeylModuleFp := opts -> Q -> (
    A := (Q#"ambient")#"cartan";
    p := Q#"prime";
    ch := character(Q, WeightFormat => "Dynkin");
    result := decomposeCharacter(A, p, ch);
    characterFromDynkin(A, result, opts.WeightFormat)
)

decompositionNumbers SubquotientWeylModuleFp := opts -> M -> (
    A := (M#"ambient")#"cartan";
    p := M#"prime";
    ch := character(M, WeightFormat => "Dynkin");
    result := decomposeCharacter(A, p, ch);
    characterFromDynkin(A, result, opts.WeightFormat)
)

compositionFactors = method(Options => {WeightFormat => "Dynkin"})

compositionFactors(WeylModuleZ, ZZ) := opts -> (W, p) -> (
    A := W#"cartan";
    factors := decompositionNumbers(W, p, WeightFormat => "Dynkin");
    qolCompositionPairs(A, factors, opts.WeightFormat)
)

compositionFactors SubWeylModuleFp := opts -> S -> (
    A := (S#"ambient")#"cartan";
    factors := decompositionNumbers(S, WeightFormat => "Dynkin");
    qolCompositionPairs(A, factors, opts.WeightFormat)
)

compositionFactors QuotientWeylModuleFp := opts -> Q -> (
    A := (Q#"ambient")#"cartan";
    factors := decompositionNumbers(Q, WeightFormat => "Dynkin");
    qolCompositionPairs(A, factors, opts.WeightFormat)
)

compositionFactors SubquotientWeylModuleFp := opts -> M -> (
    A := (M#"ambient")#"cartan";
    factors := decompositionNumbers(M, WeightFormat => "Dynkin");
    qolCompositionPairs(A, factors, opts.WeightFormat)
)

isIrreducible = method()

isIrreducible(WeylModuleZ, ZZ) := (W, p) -> (
    if not isPrime p then error "isIrreducible: prime expected";
    factors := decompositionNumbers(W, p);
    dim W > 0 and sum values factors == 1
)

isIrreducible SubWeylModuleFp := S -> (
    factors := decompositionNumbers S;
    dim S > 0 and sum values factors == 1
)

isIrreducible QuotientWeylModuleFp := Q -> (
    factors := decompositionNumbers Q;
    dim Q > 0 and sum values factors == 1
)

isIrreducible SubquotientWeylModuleFp := M -> (
    factors := decompositionNumbers M;
    dim M > 0 and sum values factors == 1
)

----------------------------------------------------------------------
-- Determination of the full socle filtration
----------------------------------------------------------------------

-- Group maximal vectors by highest weight.  A weight is ambiguous precisely
-- when its maximal-vector space has dimension greater than one.  This test is
-- repeated on every quotient encountered below; checking only V(lambda)
-- would not suffice to determine the full socle filtration.
qolAmbiguityData = Q -> (
    mvecs := maximalVectors Q;
    byWeight := new MutableHashTable;
    for x in mvecs do (
        mu := x#0;
        byWeight#mu = (if byWeight#?mu then byWeight#mu else {}) | {x#1};
    );
    ambiguous := sort select(keys byWeight, mu -> #(byWeight#mu) > 1);
    (mvecs, byWeight, ambiguous)
)

qolFactorMultiplicityTable = (A, generatorsList, weightFormat) -> (
    tableDynkin := new MutableHashTable;
    for g in generatorsList do (
        mu := g#0;
        tableDynkin#mu = (if tableDynkin#?mu then tableDynkin#mu else 0) + 1;
    );
    tableOut := new MutableHashTable;
    for mu in keys tableDynkin do
        tableOut#(fromDynkin(A, mu, weightFormat)) = tableDynkin#mu;
    (new HashTable from tableDynkin, new HashTable from tableOut)
)

qolSocleFailure = (requireComplete, messageText) -> (
    if requireComplete then error("socleAnalysis: " | messageText);
    messageText
)

-- Analyze V/S, where S is represented inside its ambient Weyl module.
-- Cumulative terms are returned as submodules of V and therefore contain S.
qolSocleAnalysisFromKernel = (sourceObject, initialKernel, requireComplete, weightFormat) -> (
    if requireComplete =!= true and requireComplete =!= false then
        error "socleAnalysis: RequireComplete must be true or false";
    W := initialKernel#"ambient";
    p := initialKernel#"prime";
    A := W#"cartan";
    validateWeightFormat(A, weightFormat, "socleAnalysis");
    initialDim := dim initialKernel;
    targetDim := dim W - initialDim;

    curS := initialKernel;
    cumulativeLayers := {};
    layerGenerators := {};
    layerFactorMultiplicities := {};
    layerFactorMultiplicitiesDynkin := {};
    layerDimensions := {};
    cumulativeDimensions := {};
    quotientDimensions := {};
    ambiguousByLayer := {};
    ambiguousDynkinByLayer := {};
    allAmbiguousDynkin := {};
    dimensionMismatches := {};
    issues := {};
    stalled := false;

    while dim curS < dim W do (
        Q := quotientWeylModule curS;
        quotientDimensions = append(quotientDimensions, dim Q);
        (mvecs, byWeight, ambiguousHereDynkin) := qolAmbiguityData Q;
        ambiguousHere := for mu in ambiguousHereDynkin list
            fromDynkin(A, mu, weightFormat);
        ambiguousDynkinByLayer = append(
            ambiguousDynkinByLayer, ambiguousHereDynkin);
        ambiguousByLayer = append(ambiguousByLayer, ambiguousHere);
        allAmbiguousDynkin = unique(allAmbiguousDynkin | ambiguousHereDynkin);

        if #ambiguousHereDynkin > 0 then (
            msg := "the maximal-vector space has dimension greater than one at weight(s) " |
                   toString ambiguousHere | " in successive quotient " |
                   toString(#cumulativeLayers + 1);
            issues = append(issues, msg);
            if requireComplete then error("socleAnalysis: " | msg);
        );

        gens := gensSocleOfQuotient(Q, p);
        if #gens == 0 then (
            msg = qolSocleFailure(requireComplete,
                "no simple socle generator was found in a nonzero successive quotient");
            issues = append(issues, msg);
            stalled = true;
            break;
        );

        (factorTableDynkin, factorTableOut) :=
            qolFactorMultiplicityTable(A, gens, weightFormat);
        nextS := curS;
        for g in gens do
            nextS = subWeylModule(nextS, g#0, coefficientList(g#1));
        increment := dim nextS - dim curS;
        if increment <= 0 then (
            msg = qolSocleFailure(requireComplete,
                "the proposed socle generators did not enlarge the filtration");
            issues = append(issues, msg);
            stalled = true;
            break;
        );

        expectedIncrement := sum for mu in keys factorTableDynkin list
            factorTableDynkin#mu * simpleDimension(A, p, mu);
        if expectedIncrement != increment then (
            mismatch := new HashTable from {
                "layer" => #cumulativeLayers + 1,
                "actualDimension" => increment,
                "factorDimension" => expectedIncrement
            };
            dimensionMismatches = append(dimensionMismatches, mismatch);
            msg = "the simple-factor dimensions in layer " |
                  toString(#cumulativeLayers + 1) |
                  " sum to " | toString expectedIncrement |
                  ", but the layer has dimension " | toString increment;
            issues = append(issues, msg);
            if requireComplete then error("socleAnalysis: " | msg);
        );

        layerGenerators = append(layerGenerators, gens);
        layerFactorMultiplicities = append(
            layerFactorMultiplicities, factorTableOut);
        layerFactorMultiplicitiesDynkin = append(
            layerFactorMultiplicitiesDynkin, factorTableDynkin);
        layerDimensions = append(layerDimensions, increment);
        cumulativeDimensions = append(
            cumulativeDimensions, dim nextS - initialDim);
        cumulativeLayers = append(cumulativeLayers, nextS);
        curS = nextS;
    );

    complete := (not stalled) and dim curS == dim W;
    certified := complete and #allAmbiguousDynkin == 0 and
                 #dimensionMismatches == 0;
    if requireComplete and not certified then
        error "socleAnalysis: the full socle filtration could not be determined";

    allAmbiguous := for mu in qolDominanceOrder(A, allAmbiguousDynkin) list
        fromDynkin(A, mu, weightFormat);
    new SocleAnalysis from {
        "source" => sourceObject,
        "ambient" => W,
        "initialKernel" => initialKernel,
        "prime" => p,
        "weightFormat" => weightFormat,
        "moduleDimension" => targetDim,
        "cumulativeLayers" => cumulativeLayers,
        "cumulativeSubmodules" => cumulativeLayers,
        "layerGenerators" => layerGenerators,
        "layerFactorMultiplicities" => layerFactorMultiplicities,
        "layerFactorMultiplicitiesDynkin" => layerFactorMultiplicitiesDynkin,
        "layerDimensions" => layerDimensions,
        "cumulativeDimensions" => cumulativeDimensions,
        "successiveQuotientDimensions" => quotientDimensions,
        "ambiguousWeightsByLayer" => ambiguousByLayer,
        "ambiguousWeightsDynkinByLayer" => ambiguousDynkinByLayer,
        "ambiguousWeights" => allAmbiguous,
        "ambiguousWeightsDynkin" => qolDominanceOrder(A, allAmbiguousDynkin),
        "dimensionMismatches" => dimensionMismatches,
        "issues" => issues,
        "complete" => complete,
        "isCertified" => certified
    }
)

socleAnalysis = method(Options => {
    RequireComplete => false,
    WeightFormat => "Dynkin"
})

socleAnalysis(WeylModuleZ, ZZ) := opts -> (W, p) -> (
    if not isPrime p then error "socleAnalysis: prime expected";
    S0 := subWeylModule(W, p, {});
    qolSocleAnalysisFromKernel(W, S0, opts.RequireComplete, opts.WeightFormat)
)

-- Consistent with socleSeries(S), socleAnalysis(S) analyzes V/S rather
-- than the submodule S as an abstract module.
socleAnalysis SubWeylModuleFp := opts -> S ->
    qolSocleAnalysisFromKernel(S, S, opts.RequireComplete, opts.WeightFormat)

socleAnalysis QuotientWeylModuleFp := opts -> Q ->
    qolSocleAnalysisFromKernel(Q, Q#"kernel", opts.RequireComplete, opts.WeightFormat)

socleLayerFactors = method()
socleLayerFactors SocleAnalysis := result -> result#"layerFactorMultiplicities"

-- Mathematical layer numbers are one-based here.
socleLayerFactors(SocleAnalysis, ZZ) := (result, layerNumber) -> (
    tables := result#"layerFactorMultiplicities";
    if layerNumber < 1 or layerNumber > #tables then
        error("socleLayerFactors: layer number must lie between 1 and " |
              toString #tables);
    tables#(layerNumber - 1)
)

cumulativeSocleLayers = method()
cumulativeSocleLayers SocleAnalysis := result -> result#"cumulativeLayers"

socleLayerGenerators = method()
socleLayerGenerators SocleAnalysis := result -> result#"layerGenerators"

socleLayerDimensions = method()
socleLayerDimensions SocleAnalysis := result -> result#"layerDimensions"

cumulativeSocleDimensions = method()
cumulativeSocleDimensions SocleAnalysis := result -> result#"cumulativeDimensions"

successiveQuotientDimensions = method()
successiveQuotientDimensions SocleAnalysis := result ->
    result#"successiveQuotientDimensions"

ambiguousWeights = method()
ambiguousWeights SocleAnalysis := result -> result#"ambiguousWeights"

ambiguousWeightsByLayer = method()
ambiguousWeightsByLayer SocleAnalysis := result -> result#"ambiguousWeightsByLayer"

analysisIssues = method()
analysisIssues SocleAnalysis := result -> result#"issues"

socleAnalysisPrime = method()
socleAnalysisPrime SocleAnalysis := result -> result#"prime"

socleAnalysisModuleDimension = method()
socleAnalysisModuleDimension SocleAnalysis := result -> result#"moduleDimension"

isCertified = method()
isCertified SocleAnalysis := result -> result#"isCertified"

-- A package-local name avoids changing the optional Polyhedra method
-- isComplete, which need not be loaded in every Macaulay2 session.
socleAnalysisComplete = method()
socleAnalysisComplete SocleAnalysis := result -> result#"complete"

socleLength = method()
socleLength SocleAnalysis := result -> #result#"cumulativeLayers"
socleLength(WeylModuleZ, ZZ) := (W, p) -> socleLength socleAnalysis(W, p)
socleLength SubWeylModuleFp := S -> socleLength socleAnalysis S
socleLength QuotientWeylModuleFp := Q -> socleLength socleAnalysis Q

net SocleAnalysis := result -> (
    status := if result#"isCertified" then "complete socle filtration"
              else if result#"complete" then "socle search may be incomplete"
              else "partial filtration; socle search may be incomplete";
    "SocleAnalysis(length " | toString(socleLength result) |
    ", dim " | toString(result#"moduleDimension") |
    ", char " | toString(result#"prime") |
    ", " | status |
    ")"
)

----------------------------------------------------------------------
-- Explicit RequireComplete overloads (non-destructive API extension)
----------------------------------------------------------------------

qolReadRequireCompleteOption = (methodName, optionValue) -> (
    if first optionValue =!= RequireComplete then
        error(methodName | ": the only positional option accepted here is RequireComplete");
    value := last optionValue;
    if value =!= true and value =!= false then
        error(methodName | ": RequireComplete must be true or false");
    value
)

socleWeyl(WeylModuleZ, ZZ, Option) := (W, p, optionValue) -> (
    required := qolReadRequireCompleteOption("socleWeyl", optionValue);
    if not required then return socleWeyl(W, p);
    result := socleAnalysis(W, p, RequireComplete => true);
    layers := result#"cumulativeLayers";
    if #layers == 0 then
        error "socleWeyl: a nonzero Weyl module has no computed socle";
    layers#0
)

socleSeries(WeylModuleZ, ZZ, Option) := (W, p, optionValue) -> (
    required := qolReadRequireCompleteOption("socleSeries", optionValue);
    if not required then return socleSeries(W, p);
    (socleAnalysis(W, p, RequireComplete => true))#"cumulativeLayers"
)

socleSeries(SubWeylModuleFp, Option) := (S, optionValue) -> (
    required := qolReadRequireCompleteOption("socleSeries", optionValue);
    if not required then return socleSeries S;
    (socleAnalysis(S, RequireComplete => true))#"cumulativeLayers"
)

socleSeries(QuotientWeylModuleFp, Option) := (Q, optionValue) -> (
    required := qolReadRequireCompleteOption("socleSeries", optionValue);
    if not required then return socleSeries Q;
    (socleAnalysis(Q, RequireComplete => true))#"cumulativeLayers"
)

-- This helper mirrors the historical implementation, but every socle
-- filtration consulted along the way must be fully determined before use.
qolCertifiedSocleLengthTwoQuotient = S -> (
    ss := socleSeries(S, RequireComplete => true);
    while #ss > 2 do (
        sIdx := #ss - 3;
        if sIdx < 0 then break;
        gensList := (ss#sIdx)#"generators";
        oldDim := dim S;
        for g in gensList do
            S = subWeylModule(S, g#0, coefficientList(g#1));
        if dim S <= oldDim then
            error "socleLengthTwoQuotient: selected generators did not enlarge the kernel";
        ss = socleSeries(S, RequireComplete => true);
    );
    quotientWeylModule S
)

socleLengthTwoQuotient(SubWeylModuleFp, Option) := (S, optionValue) -> (
    required := qolReadRequireCompleteOption(
        "socleLengthTwoQuotient", optionValue);
    if required then qolCertifiedSocleLengthTwoQuotient S
    else socleLengthTwoQuotient S
)

twoFactorQuotientsContaining(SubWeylModuleFp, Option) := (S, optionValue) -> (
    required := qolReadRequireCompleteOption(
        "twoFactorQuotientsContaining", optionValue);
    if not required then return twoFactorQuotientsContaining S;

    out := {};
    Q := qolCertifiedSocleLengthTwoQuotient S;
    analysis := socleAnalysis(Q, RequireComplete => true);
    layerGens := analysis#"layerGenerators";
    if #layerGens == 2 and #(layerGens#0) == 1 then return {Q};
    if #layerGens == 0 then return {};

    for gen in layerGens#0 do (
        U := subWeylModule(S, gen#0, coefficientList(gen#1));
        QU := qolCertifiedSocleLengthTwoQuotient U;
        analysisU := socleAnalysis(QU, RequireComplete => true);
        gensU := analysisU#"layerGenerators";
        if #gensU == 2 and #(gensU#0) == 1 then
            out = append(out, QU);
    );
    out
)

----------------------------------------------------------------------
-- Multi-prime reduction profiles
----------------------------------------------------------------------

reductionProfile = method(Options => {
    IncludeStructure => false,
    IncludeSocle => false,
    RequireComplete => false,
    WeightFormat => "Dynkin"
})

reductionProfile(WeylModuleZ, List) := opts -> (W, primeList) -> (
    if any(primeList, p -> not instance(p, ZZ) or not isPrime p) then
        error "reductionProfile: expected a list of primes";
    if opts.IncludeStructure =!= true and opts.IncludeStructure =!= false then
        error "reductionProfile: IncludeStructure must be true or false";
    if opts.IncludeSocle =!= true and opts.IncludeSocle =!= false then
        error "reductionProfile: IncludeSocle must be true or false";
    if opts.RequireComplete =!= true and opts.RequireComplete =!= false then
        error "reductionProfile: RequireComplete must be true or false";

    A := W#"cartan";
    primesUsed := unique primeList;
    rows := {};
    for p in primesUsed do (
        factorsDynkin := decompositionNumbers(W, p, WeightFormat => "Dynkin");
        factorsOut := qolCompositionPairs(A, factorsDynkin, opts.WeightFormat);
        simpleDim := simpleDimension(A, p, W#"highestWeight");
        compositionLengthValue := sum values factorsDynkin;
        radicalDim := dim W - simpleDim;

        maximalVectorCount := null;
        initialAmbiguity := null;
        socleDim := null;
        socleLengthValue := null;
        socleCertified := null;
        allAmbiguity := null;
        structuralRequested := opts.IncludeStructure or opts.IncludeSocle or
                               opts.RequireComplete;
        if structuralRequested then (
            S0 := subWeylModule(W, p, {});
            Q0 := quotientWeylModule S0;
            (mvecs, grouped, ambiguityDynkin) := qolAmbiguityData Q0;
            maximalVectorCount = #mvecs;
            initialAmbiguity = for mu in ambiguityDynkin list
                fromDynkin(A, mu, opts.WeightFormat);
        );
        if opts.IncludeSocle or opts.RequireComplete then (
            analysis := socleAnalysis(W, p,
                RequireComplete => opts.RequireComplete,
                WeightFormat => opts.WeightFormat);
            socleDim = if #(analysis#"layerDimensions") == 0 then 0
                        else (analysis#"layerDimensions")#0;
            socleLengthValue = socleLength analysis;
            socleCertified = isCertified analysis;
            allAmbiguity = ambiguousWeights analysis;
        );

        rows = append(rows, new HashTable from {
            "prime" => p,
            "weylDimension" => dim W,
            "simpleDimension" => simpleDim,
            "radicalDimension" => radicalDim,
            "compositionLength" => compositionLengthValue,
            "compositionFactors" => factorsOut,
            "irreducible" => (compositionLengthValue == 1),
            "maximalVectorCount" => maximalVectorCount,
            "initialAmbiguousWeights" => initialAmbiguity,
            "socleDimension" => socleDim,
            "socleLength" => socleLengthValue,
            "socleCertified" => socleCertified,
            "ambiguousWeights" => allAmbiguity
        });
    );
    new ReductionProfile from {
        "weylModule" => W,
        "highestWeight" => fromDynkin(A, W#"highestWeight", opts.WeightFormat),
        "cartan" => A,
        "weightFormat" => opts.WeightFormat,
        "primes" => primesUsed,
        "rows" => rows,
        "includesStructure" => (opts.IncludeStructure or opts.IncludeSocle or
                                 opts.RequireComplete),
        "includesSocle" => (opts.IncludeSocle or opts.RequireComplete)
    }
)

reductionProfile(WeylModuleZ, ZZ) := opts -> (W, primeBound) -> (
    if primeBound < 2 then
        return reductionProfile(W, {},
            IncludeStructure => opts.IncludeStructure,
            IncludeSocle => opts.IncludeSocle,
            RequireComplete => opts.RequireComplete,
            WeightFormat => opts.WeightFormat);
    primeList := select(toList(2..primeBound), isPrime);
    reductionProfile(W, primeList,
        IncludeStructure => opts.IncludeStructure,
        IncludeSocle => opts.IncludeSocle,
        RequireComplete => opts.RequireComplete,
        WeightFormat => opts.WeightFormat)
)

reductionRows = method()
reductionRows ReductionProfile := P -> P#"rows"

reductionPrimes = method()
reductionPrimes ReductionProfile := P -> P#"primes"

reductionRow = method()
reductionRow(ReductionProfile, ZZ) := (P, p) -> (
    matches := select(P#"rows", row -> row#"prime" == p);
    if #matches == 0 then null else matches#0
)

reductionWeylModule = method()
reductionWeylModule ReductionProfile := P -> P#"weylModule"

reductionHighestWeight = method()
reductionHighestWeight ReductionProfile := P -> P#"highestWeight"

reducibleCharacteristics = method()

reducibleCharacteristics(WeylModuleZ, List) := (W, primeList) -> (
    if any(primeList, p -> not instance(p, ZZ) or not isPrime p) then
        error "reducibleCharacteristics: expected a list of primes";
    select(unique primeList, p -> not isIrreducible(W, p))
)

reducibleCharacteristics(WeylModuleZ, ZZ) := (W, primeBound) -> (
    if primeBound < 2 then return {};
    reducibleCharacteristics(W, select(toList(2..primeBound), isPrime))
)

reducibleCharacteristics ReductionProfile := P ->
    for row in P#"rows" list
        if row#"irreducible" then continue else row#"prime"

net ReductionProfile := P -> (
    rows := P#"rows";
    tableRows := {{"p", "dim L", "length", "radical", "socle", "socle determined"}};
    for row in rows do tableRows = append(tableRows, {
        toString(row#"prime"),
        toString(row#"simpleDimension"),
        toString(row#"compositionLength"),
        toString(row#"radicalDimension"),
        if row#"socleDimension" === null then "--"
            else toString(row#"socleDimension"),
        if row#"socleCertified" === null then "--"
            else if row#"socleCertified" then "yes" else "no"
    });
    netList tableRows
)

----------------------------------------------------------------------
-- Concise extension records
----------------------------------------------------------------------

extensionData = method(Options => {
    RequireComplete => true,
    WeightFormat => "Dynkin"
})

extensionData QuotientWeylModuleFp := opts -> Q -> (
    analysis := socleAnalysis(Q,
        RequireComplete => opts.RequireComplete,
        WeightFormat => opts.WeightFormat);
    factorsDynkin := decompositionNumbers(Q, WeightFormat => "Dynkin");
    A := (Q#"ambient")#"cartan";
    factors := qolCompositionPairs(A, factorsDynkin, opts.WeightFormat);
    lengthValue := sum values factorsDynkin;
    layers := socleLayerFactors analysis;
    bottom := if #layers == 0 then new HashTable else layers#0;
    twoFactor := isCertified analysis and lengthValue == 2 and
                 socleLength analysis == 2 and sum values bottom == 1;
    new ExtensionData from {
        "module" => Q,
        "prime" => Q#"prime",
        "topFactor" => highestWeight(Q, WeightFormat => opts.WeightFormat),
        "compositionFactors" => factors,
        "compositionLength" => lengthValue,
        "socleLayerFactors" => layers,
        "bottomFactors" => bottom,
        "socleLength" => socleLength analysis,
        "socleAnalysis" => analysis,
        "isCertified" => isCertified analysis,
        "isTwoFactorExtension" => twoFactor,
        "isNonSplit" => twoFactor
    }
)

extensionData SubWeylModuleFp := opts -> S ->
    extensionData(quotientWeylModule S,
        RequireComplete => opts.RequireComplete,
        WeightFormat => opts.WeightFormat)

extensionData(WeylModuleZ, ZZ) := opts -> (W, p) -> (
    if not isPrime p then error "extensionData: prime expected";
    Q := quotientWeylModule subWeylModule(W, p, {});
    extensionData(Q,
        RequireComplete => opts.RequireComplete,
        WeightFormat => opts.WeightFormat)
)

isCertified ExtensionData := data -> data#"isCertified"

isTwoFactorExtension = method()
isTwoFactorExtension ExtensionData := data -> data#"isTwoFactorExtension"

net ExtensionData := data -> (
    status := if data#"isCertified" then "socle filtration determined"
              else "socle filtration not determined";
    "ExtensionData(length " | toString(data#"compositionLength") |
    ", socle length " | toString(data#"socleLength") |
    ", char " | toString(data#"prime") |
    ", " | status |
    ")"
)

----------------------------------------------------------------------
-- Formal characters and labelled decomposition tables
----------------------------------------------------------------------
--
-- A WeylCharacter stores
-- its terms canonically in Dynkin coordinates.  Consequently character
-- arithmetic never depends on a presentation in partition coordinates, while
-- every public read operation can still request WeightFormat => "Partition".

WeylCharacter = new Type of HashTable

-- Test equality of Cartan data without relying on object identity.  This is
-- deliberately stricter than merely comparing ranks: addition and tensor
-- product only make sense after the root datum has been identified.
sameCharacterCartan = (A, B) -> (
    instance(A, Matrix) and instance(B, Matrix)
    and numrows A == numrows B
    and numcols A == numcols B
    and entries A == entries B
)

-- Remove zero terms and freeze a character table.  Negative coefficients are
-- allowed: WeylCharacter is an element of the integral group ring of the
-- weight lattice, whether or not the sum is the character of a module.
normalizeCharacterTerms = (A, ch) -> (
    r := numrows A;
    out := new MutableHashTable;
    for w in keys ch do (
        if not instance(w, List) or #w != r then
            error "weylCharacter: every weight must be a list of length equal to the Cartan rank";
        if not all(w, x -> instance(x, ZZ)) then
            error "weylCharacter: Dynkin coordinates must be integers";
        m := ch#w;
        if not instance(m, ZZ) then
            error "weylCharacter: character coefficients must be integers";
        if m != 0 then out#w = (if out#?w then out#w else 0) + m;
    );
    new HashTable from for w in keys out list if out#w != 0 then w => out#w
)

-- The maximal support weights determine intrinsic root-depth levels.  This
-- also works for reducible and virtual characters: each maximal support
-- weight starts a depth-zero component.
maximalCharacterWeights = (A, ch) -> (
    wts := keys ch;
    select(wts, cand ->
        not any(wts, other -> other != cand and dominanceGE(A, other, cand)))
)

characterRootDepth = (A, maxima, w) -> (
    candidates := {};
    for top in maxima do if dominanceGE(A, top, w) then (
        delta := for i from 0 to #w - 1 list top#i - w#i;
        simpleDelta := toSimple(A, delta);
        if all(simpleDelta, x ->
            x >= 0 and (instance(x, ZZ) or (instance(x, QQ) and denominator x == 1)))
        then candidates = append(candidates, sum simpleDelta);
    );
    -- In a finite partially ordered support, w itself is a maximal element of
    -- its lower component if no different maximum dominates it.
    if #candidates == 0 then 0 else min candidates
)

makeWeylCharacter = (A, ch, sourceFormat, characteristicValue, provenance) -> (
    normalized := normalizeCharacterTerms(A, ch);
    new WeylCharacter from {
        "cartan" => A,
        "terms" => normalized,
        "coordinateConvention" => "Dynkin",
        "sourceWeightFormat" => sourceFormat,
        "characteristic" => characteristicValue,
        "provenance" => provenance,
        "isVirtual" => any(values normalized, m -> m < 0)
    }
)

weylCharacter = method(Options => {WeightFormat => "Dynkin"})

weylCharacter(Matrix, HashTable) := opts -> (A, chUser) -> (
    if numrows A != numcols A or not isCartanMatrix A then
        error "weylCharacter: expected a Cartan matrix";
    if cartanType A =!= "finite" then
        error "weylCharacter: the Cartan matrix must be of finite type";
    chDynkin := characterToDynkin(A, chUser, opts.WeightFormat);
    makeWeylCharacter(A, chDynkin, opts.WeightFormat, null, "explicit character")
)

weylCharacter WeylModuleZ := opts -> W -> (
    validateWeightFormat(W#"cartan", opts.WeightFormat, "weylCharacter");
    chDynkin := new HashTable from
        for w in W#"weightsList" list w => (W#"weightMultAt")#w;
    makeWeylCharacter(W#"cartan", chDynkin, opts.WeightFormat, 0,
                      "integral Weyl module")
)

weylCharacter SubWeylModuleFp := opts -> S -> (
    W := S#"ambient";
    validateWeightFormat(W#"cartan", opts.WeightFormat, "weylCharacter");
    chDynkin := new HashTable from
        for w in keys(S#"rankAt") list if (S#"rankAt")#w > 0
        then w => (S#"rankAt")#w;
    makeWeylCharacter(W#"cartan", chDynkin, opts.WeightFormat, S#"prime",
                      "Weyl submodule")
)

weylCharacter QuotientWeylModuleFp := opts -> Q -> (
    W := Q#"ambient";
    validateWeightFormat(W#"cartan", opts.WeightFormat, "weylCharacter");
    chDynkin := new HashTable from
        for w in keys(Q#"quotientDimAt") list if (Q#"quotientDimAt")#w > 0
        then w => (Q#"quotientDimAt")#w;
    makeWeylCharacter(W#"cartan", chDynkin, opts.WeightFormat, Q#"prime",
                      "Weyl quotient")
)

weylCharacter SubquotientWeylModuleFp := opts -> M -> (
    W := M#"ambient";
    validateWeightFormat(W#"cartan", opts.WeightFormat, "weylCharacter");
    chDynkin := new HashTable from
        for w in keys(M#"quotientDimAt") list if (M#"quotientDimAt")#w > 0
        then w => (M#"quotientDimAt")#w;
    makeWeylCharacter(W#"cartan", chDynkin, opts.WeightFormat, M#"prime",
                      "Weyl subquotient")
)

ambientCartan WeylCharacter := C -> C#"cartan"

-- `characteristic` is the shared method defined above for Weyl modules.
characteristic WeylCharacter := C -> C#"characteristic"

coordinateConvention = method()
coordinateConvention WeylCharacter := C -> C#"coordinateConvention"

sourceWeightFormat = method()
sourceWeightFormat WeylCharacter := C -> C#"sourceWeightFormat"

isVirtualCharacter = method()
isVirtualCharacter WeylCharacter := C -> C#"isVirtual"

characterMetadata = method()
characterMetadata WeylCharacter := C -> new HashTable from {
    "coordinateConvention" => C#"coordinateConvention",
    "sourceWeightFormat" => C#"sourceWeightFormat",
    "characteristic" => C#"characteristic",
    "provenance" => C#"provenance",
    "isVirtual" => C#"isVirtual"
}

-- Return an ordinary HashTable when interoperability with the original API
-- is required.  The returned table is a fresh immutable table.
rawCharacter = method(Options => {WeightFormat => "Dynkin"})
rawCharacter WeylCharacter := opts -> C -> (
    A := C#"cartan";
    characterFromDynkin(A, new HashTable from C#"terms", opts.WeightFormat)
)

-- Intrinsic, deterministic term order.  RootDepth first orders weights by
-- the height of the positive-root drop from a maximal support weight, and
-- then by descending lexicographic Dynkin coordinates.  Lex requests only
-- the latter order.
characterTerms = method(Options => {
    WeightFormat => "Dynkin",
    WeightOrder => "RootDepth"
})

characterTerms WeylCharacter := opts -> C -> (
    A := C#"cartan";
    validateWeightFormat(A, opts.WeightFormat, "characterTerms");
    ch := C#"terms";
    order := opts.WeightOrder;
    if order =!= "RootDepth" and order =!= "Lex" then
        error "characterTerms: WeightOrder must be \"RootDepth\" or \"Lex\"";
    wts := keys ch;
    ordered := if order === "Lex" then rsort wts else (
        maxima := maximalCharacterWeights(A, ch);
        decorated := for w in wts list
            (characterRootDepth(A, maxima, w), for x in w list -x, w);
        for item in sort decorated list item#2
    );
    for w in ordered list
        (fromDynkin(A, w, opts.WeightFormat), ch#w)
)

-- Canonical support is returned in Dynkin coordinates.  Use `weights` when
-- a formatted support is desired.
support WeylCharacter := C -> for t in characterTerms C list t#0

weights WeylCharacter := opts -> C ->
    for t in characterTerms(C,
        WeightFormat => opts.WeightFormat,
        WeightOrder => opts.WeightOrder) list t#0

dim WeylCharacter := C -> sum values(C#"terms")
characterDim WeylCharacter := C -> dim C

weightMultiplicity(WeylCharacter, List) := opts -> (C, muUser) -> (
    A := C#"cartan";
    mu := toDynkin(A, muUser, opts.WeightFormat);
    if (C#"terms")#?mu then (C#"terms")#mu else 0
)

-- Return the coefficients as a hash table indexed by weights.
character WeylCharacter := opts -> C ->
    rawCharacter(C, WeightFormat => opts.WeightFormat)

sortedCharacter WeylCharacter := C -> characterTerms C
sortedCharacter(Matrix, HashTable) := (A, ch) -> characterTerms weylCharacter(A, ch)

compatibleCharacterPair = (C, D, who) -> (
    if not sameCharacterCartan(C#"cartan", D#"cartan") then
        error(who | ": the characters have different Cartan data");
    true
)

combinedCharacteristic = (C, D) -> (
    c := C#"characteristic";
    d := D#"characteristic";
    if c === d then c else null
)

-- Equality is equality of formal characters in the same based root datum.
-- Provenance and characteristic metadata do not change the element of the
-- representation ring represented by the normalized term table.
WeylCharacter == WeylCharacter := (C, D) -> (
    if not sameCharacterCartan(C#"cartan", D#"cartan") then return false;
    leftTerms := C#"terms";
    rightTerms := D#"terms";
    #keys leftTerms == #keys rightTerms and
    all(keys leftTerms, w -> rightTerms#?w and leftTerms#w == rightTerms#w)
)

WeylCharacter + WeylCharacter := (C, D) -> (
    compatibleCharacterPair(C, D, "WeylCharacter addition");
    out := new MutableHashTable;
    for w in keys(C#"terms") do out#w = (C#"terms")#w;
    for w in keys(D#"terms") do
        out#w = (if out#?w then out#w else 0) + (D#"terms")#w;
    makeWeylCharacter(C#"cartan", new HashTable from out, "Dynkin",
                      combinedCharacteristic(C, D), "sum")
)

- WeylCharacter := C -> (
    out := new HashTable from
        for w in keys(C#"terms") list w => -(C#"terms")#w;
    makeWeylCharacter(C#"cartan", out, "Dynkin", C#"characteristic",
                      "additive inverse")
)

WeylCharacter - WeylCharacter := (C, D) -> C + (-D)

ZZ * WeylCharacter := (a, C) -> (
    out := new HashTable from
        for w in keys(C#"terms") list w => a * (C#"terms")#w;
    makeWeylCharacter(C#"cartan", out, "Dynkin", C#"characteristic",
                      "integral multiple")
)

WeylCharacter * ZZ := (C, a) -> a * C

tensorProductCharacter = method()
tensorProductCharacter(WeylCharacter, WeylCharacter) := (C, D) -> (
    compatibleCharacterPair(C, D, "tensorProductCharacter");
    A := C#"cartan";
    ch := productCharacter(C#"terms", D#"terms");
    makeWeylCharacter(A, ch, "Dynkin", combinedCharacteristic(C, D),
                      "tensor product")
)

tensorProductFactors = method(Options => {WeightFormat => "Dynkin"})
tensorProductFactors(WeylCharacter, WeylCharacter, ZZ) := opts -> (C, D, p) -> (
    product := tensorProductCharacter(C, D);
    factors := decomposeCharacter(product, p, WeightFormat => "Dynkin");
    qolCompositionPairs(C#"cartan", factors, opts.WeightFormat)
)

WeylCharacter * WeylCharacter := (C, D) -> tensorProductCharacter(C, D)

productCharacter(WeylCharacter, WeylCharacter) := (C, D) ->
    tensorProductCharacter(C, D)

dualCharacter = method()
dualCharacter WeylCharacter := C -> (
    out := new MutableHashTable;
    for w in keys(C#"terms") do (
        dualWeight := for x in w list -x;
        out#dualWeight = (if out#?dualWeight then out#dualWeight else 0)
                       + (C#"terms")#w;
    );
    makeWeylCharacter(C#"cartan", new HashTable from out, "Dynkin",
                      C#"characteristic", "contragredient dual")
)

frobeniusTwistCharacter = method()

frobeniusTwistCharacter(WeylCharacter, ZZ, ZZ) := (C, p, e) -> (
    if not isPrime p then
        error "frobeniusTwistCharacter: the second argument must be prime";
    if e < 0 then
        error "frobeniusTwistCharacter: the twist exponent must be non-negative";
    c := C#"characteristic";
    if c =!= null and c != 0 and c != p then
        error "frobeniusTwistCharacter: the requested prime differs from the characteristic of the character";
    scale := p^e;
    out := new MutableHashTable;
    for w in keys(C#"terms") do (
        twistedWeight := for x in w list scale * x;
        out#twistedWeight = (if out#?twistedWeight then out#twistedWeight else 0)
                          + (C#"terms")#w;
    );
    makeWeylCharacter(C#"cartan", new HashTable from out, "Dynkin", p,
                      "Frobenius twist")
)

frobeniusTwistCharacter(WeylCharacter, ZZ) := (C, p) ->
    frobeniusTwistCharacter(C, p, 1)

-- The Cartan matrix is part of the character, so decomposition requires
-- only the characteristic in addition to the character.
decomposeCharacter(WeylCharacter, ZZ) := opts -> (C, p) -> (
    if isVirtualCharacter C then
        error concatenate(
            "decomposeCharacter: a virtual character cannot be decomposed ",
            "as a non-negative module character");
    -- A WeylCharacter is stored in Dynkin coordinates regardless of the
    -- requested output convention.  Decompose that canonical table first,
    -- then convert only the resulting highest-weight keys.
    factorsDynkin := decomposeCharacter(C#"cartan", p, C#"terms",
                                        WeightFormat => "Dynkin");
    characterFromDynkin(C#"cartan", factorsDynkin, opts.WeightFormat)
)

net WeylCharacter := C -> (
    kind := if C#"isVirtual" then "virtual, " else "";
    charText := if C#"characteristic" === null then "unspecified"
                else toString(C#"characteristic");
    "WeylCharacter(" | kind | toString(#keys(C#"terms")) | " weights, dim " |
    toString(dim C) | ", char " | charText | ")"
)

----------------------------------------------------------------------
-- Labelled decomposition tables
----------------------------------------------------------------------

DecompositionTable = new Type of HashTable

decompositionTable = method()
decompositionTable(Matrix, List, List, HashTable) := (M, rows, columns, metadata) -> (
    if ring M =!= ZZ then
        error "decompositionTable: the matrix must have integer entries";
    if any(flatten entries M, x -> x < 0) then
        error "decompositionTable: decomposition multiplicities must be nonnegative";
    if #rows != numrows M then
        error "decompositionTable: the number of row labels must equal the number of matrix rows";
    if #columns != numcols M then
        error concatenate(
            "decompositionTable: the number of column labels must equal ",
            "the number of matrix columns");
    if #(unique rows) != #rows then
        error "decompositionTable: row labels must be distinct";
    if #(unique columns) != #columns then
        error "decompositionTable: column labels must be distinct";
    if not metadata#?"prime" or not isPrime(metadata#"prime") then
        error "decompositionTable: metadata must contain a prime under the key \"prime\"";
    if not metadata#?"algebra" then
        error "decompositionTable: metadata must contain an algebra name under the key \"algebra\"";
    new DecompositionTable from {
        "matrix" => M,
        "rowLabels" => rows,
        "columnLabels" => columns,
        "metadata" => new HashTable from metadata
    }
)

matrix DecompositionTable := opts -> T -> T#"matrix"

rowLabels = method()
rowLabels DecompositionTable := T -> T#"rowLabels"

columnLabels = method()
columnLabels DecompositionTable := T -> T#"columnLabels"

prime = method()
prime DecompositionTable := T -> (T#"metadata")#"prime"

algebra = method()
algebra DecompositionTable := T -> (T#"metadata")#"algebra"

tableMetadata = method()
tableMetadata DecompositionTable := T -> T#"metadata"

net DecompositionTable := T -> (
    M := T#"matrix";
    cols := T#"columnLabels";
    rows := T#"rowLabels";
    heading := (T#"metadata")#"algebra" | " decomposition table, characteristic " |
               toString((T#"metadata")#"prime");
    grid := {prepend("rows / columns", cols)} |
        for i from 0 to numrows M - 1 list
            prepend(rows#i, for j from 0 to numcols M - 1 list M_(i, j));
    stack(heading, netList(grid, Boxes => true, Alignment => Center))
)

schurAlgebraDecompositionTable = method()
schurAlgebraDecompositionTable(ZZ, ZZ, ZZ) := (p, n, r) -> (
    if not isPrime p then
        error "schurAlgebraDecompositionTable: prime expected";
    if n < 1 then
        error "schurAlgebraDecompositionTable: n must be positive";
    if r < 0 then
        error "schurAlgebraDecompositionTable: r must be non-negative";
    labels := boundedPartitions(n, r, r);
    M := schurAlgebraDecompositionMatrix(p, n, r);
    metadata := new HashTable from {
        "algebra" => ("Schur algebra S(" | toString n | "," | toString r | ")"),
        "family" => "Schur",
        "prime" => p,
        "n" => n,
        "degree" => r,
        "rowMeaning" => "Weyl modules indexed by partitions",
        "columnMeaning" => "simple polynomial modules indexed by partitions"
    };
    decompositionTable(M, labels, labels, metadata)
)

stripPartitionZeros = mu -> (
    stripped := mu;
    while #stripped > 0 and last stripped == 0 do stripped = drop(stripped, -1);
    stripped
)

symmetricGroupDecompositionTable = method()
symmetricGroupDecompositionTable(ZZ, ZZ) := (p, n) -> (
    if not isPrime p then
        error "symmetricGroupDecompositionTable: prime expected";
    if n < 1 then
        error "symmetricGroupDecompositionTable: n must be positive";
    padded := boundedPartitions(n, n, n);
    rows := apply(padded, stripPartitionZeros);
    columns := select(rows, mu -> pRestrictedQ(p, mu));
    M := symmetricGroupDecompositionMatrix(p, n);
    metadata := new HashTable from {
        "algebra" => ("symmetric group algebra k[S_" | toString n | "]"),
        "family" => "SymmetricGroup",
        "prime" => p,
        "degree" => n,
        "rowMeaning" => "dual Specht modules indexed by partitions",
        "columnMeaning" => "simple modules indexed by p-restricted partitions",
        "labelConvention" => "p-restricted (dual Specht convention)"
    };
    decompositionTable(M, rows, columns, metadata)
)

----------------------------------------------------------------------
-- Action matrices, relation checks, Hom spaces, and graphs
----------------------------------------------------------------------

-- These routines use the bases stored by the module, submodule, and
-- quotient objects: the resulting matrices therefore describe the actual
-- coordinate realizations returned by WeylModules.

ChevalleyActionData = new Type of HashTable
WeylModuleCheck = new Type of HashTable
WeylGraph = new Type of HashTable
SocleDiagram = new Type of HashTable

----------------------------------------------------------------------
-- Global bases and coordinate changes
----------------------------------------------------------------------

qolWeightDepth = (A, lambda, mu) -> sum dropInSimple(A, lambda, mu)

qolCanonicalWeightList = M -> (
    W := if instance(M, WeylModuleZ) then M else ambientWeylModule M;
    A := W#"cartan";
    lambda := W#"highestWeight";
    ws := if instance(M, WeylModuleZ) then W#"weightsList"
          else if instance(M, SubWeylModuleFp) then keys(M#"basisAt")
          else if instance(M, QuotientWeylModuleFp) then
              select(keys(M#"quotientDimAt"), mu -> M#"quotientDimAt"#mu > 0)
          else if instance(M, SubquotientWeylModuleFp) then
              select(keys(M#"quotientDimAt"), mu -> M#"quotientDimAt"#mu > 0)
          else error "canonical weight order: unsupported module type";
    keyed := for mu in ws list (qolWeightDepth(A, lambda, mu), mu);
    apply(sort keyed, x -> x#1)
)

qolModuleWeightDimension = (M, mu) -> (
    if instance(M, WeylModuleZ) then (
        if M#"weightMultAt"#?mu then M#"weightMultAt"#mu else 0
    ) else if instance(M, SubWeylModuleFp) then (
        if M#"rankAt"#?mu then M#"rankAt"#mu else 0
    ) else if instance(M, QuotientWeylModuleFp) then (
        if M#"quotientDimAt"#?mu then M#"quotientDimAt"#mu else 0
    ) else if instance(M, SubquotientWeylModuleFp) then (
        if M#"quotientDimAt"#?mu then M#"quotientDimAt"#mu else 0
    ) else error "weight dimension: unsupported module type"
)

qolCoefficientRing = M -> (
    if instance(M, WeylModuleZ) then ZZ
    else if instance(M, SubWeylModuleFp) or instance(M, QuotientWeylModuleFp)
            or instance(M, SubquotientWeylModuleFp) then ZZ/(M#"prime")
    else error "coefficient ring: unsupported module type"
)

-- For a full-column-rank matrix B, return a left coordinate map L satisfying
-- L B = I.  The map is canonical relative to the ambient standard basis: use
-- the lexicographically first set of independent rows.
qolLeftCoordinates = B -> (
    R := ring B;
    k := numcols B;
    d := numrows B;
    if k == 0 then return matrix(R, {});
    chosen := {};
    currentRank := 0;
    for row from 0 to d - 1 do (
        trial := append(chosen, row);
        T := submatrix(B, trial, );
        newRank := rank T;
        if newRank > currentRank then (
            chosen = trial;
            currentRank = newRank;
        );
        if #chosen == k then break;
    );
    if #chosen != k then error "left coordinates: basis matrix is not column-independent";
    square := submatrix(B, chosen, );
    squareInv := inverse square;
    Lm := mutableMatrix(R, k, d);
    for a from 0 to k - 1 do
        for b from 0 to k - 1 do Lm_(a, chosen#b) = squareInv_(a, b);
    matrix Lm
)

-- Return the global embedding J : M -> W and coordinate projection
-- P : W -> M in canonical weight order.  For submodules P is only asserted
-- to be an inverse on the submodule; for quotients it is the quotient map.
qolGlobalCoordinates = M -> (
    W := if instance(M, WeylModuleZ) then M else ambientWeylModule M;
    R := qolCoefficientRing M;
    wWeights := qolCanonicalWeightList W;
    mWeights := qolCanonicalWeightList M;

    wOffsets := new MutableHashTable;
    off := 0;
    for mu in wWeights do (
        wOffsets#mu = off;
        off = off + W#"weightMultAt"#mu;
    );
    ambientDim := off;

    mOffsets := new MutableHashTable;
    off = 0;
    for mu in mWeights do (
        mOffsets#mu = off;
        off = off + qolModuleWeightDimension(M, mu);
    );
    moduleDim := off;

    if instance(M, WeylModuleZ) then return new HashTable from {
        "weights" => mWeights,
        "offsets" => mOffsets,
        "ambientWeights" => wWeights,
        "ambientOffsets" => wOffsets,
        "section" => id_(ZZ^moduleDim),
        "projection" => id_(ZZ^moduleDim)
    };

    Jm := mutableMatrix(R, ambientDim, moduleDim);
    Pm := mutableMatrix(R, moduleDim, ambientDim);
    for mu in mWeights do (
        dV := W#"weightMultAt"#mu;
        dM := qolModuleWeightDimension(M, mu);
        wo := wOffsets#mu;
        mo := mOffsets#mu;
        if instance(M, SubWeylModuleFp) then (
            B := M#"basisAt"#mu;
            L := qolLeftCoordinates B;
            for a from 0 to dV - 1 do
                for b from 0 to dM - 1 do Jm_(wo + a, mo + b) = B_(a, b);
            for a from 0 to dM - 1 do
                for b from 0 to dV - 1 do Pm_(mo + a, wo + b) = L_(a, b);
        ) else if instance(M, QuotientWeylModuleFp) then (
            comp := M#"complementIdxAt"#mu;
            invChange := M#"bigMatInvAt"#mu;
            sRank := M#"sRankAt"#mu;
            for b from 0 to dM - 1 do Jm_(wo + comp#b, mo + b) = 1_R;
            for a from 0 to dM - 1 do
                for b from 0 to dV - 1 do
                    Pm_(mo + a, wo + b) = invChange_(sRank + a, b);
        ) else (
            sectionBlock := M#"sectionAt"#mu;
            projectionBlock := M#"projectionAt"#mu;
            for a from 0 to dV - 1 do
                for b from 0 to dM - 1 do
                    Jm_(wo + a, mo + b) = sectionBlock_(a, b);
            for a from 0 to dM - 1 do
                for b from 0 to dV - 1 do
                    Pm_(mo + a, wo + b) = projectionBlock_(a, b);
        );
    );
    new HashTable from {
        "weights" => mWeights,
        "offsets" => mOffsets,
        "ambientWeights" => wWeights,
        "ambientOffsets" => wOffsets,
        "section" => matrix Jm,
        "projection" => matrix Pm
    }
)

----------------------------------------------------------------------
-- Global Chevalley matrices
----------------------------------------------------------------------

qolAmbientChevalleyMatrices = W -> (
    A := W#"cartan";
    r := numrows A;
    coords := qolGlobalCoordinates W;
    ws := coords#"weights";
    offsets := coords#"offsets";
    n := dim W;
    Es := {};
    Fs := {};
    Hs := {};
    for i from 1 to r do (
        Em := mutableMatrix(ZZ, n, n);
        Fm := mutableMatrix(ZZ, n, n);
        Hm := mutableMatrix(ZZ, n, n);
        alpha := for k from 0 to r - 1 list A_(k, i - 1);
        for mu in ws do (
            srcOffset := offsets#mu;
            dSrc := W#"weightMultAt"#mu;
            for a from 0 to dSrc - 1 do Hm_(srcOffset + a, srcOffset + a) = mu#(i - 1);

            tgt := for k from 0 to r - 1 list mu#k + alpha#k;
            eBlock := actionE(W, mu, i);
            if eBlock =!= null and offsets#?tgt then (
                tgtOffset := offsets#tgt;
                for a from 0 to numrows eBlock - 1 do
                    for b from 0 to numcols eBlock - 1 do
                        Em_(tgtOffset + a, srcOffset + b) = eBlock_(a, b);
            );

            fBlock := actionF(W, mu, i);
            if fBlock =!= null then (
                upper := for k from 0 to r - 1 list mu#k + alpha#k;
                if offsets#?upper then (
                    upperOffset := offsets#upper;
                    for a from 0 to numrows fBlock - 1 do
                        for b from 0 to numcols fBlock - 1 do
                            Fm_(srcOffset + a, upperOffset + b) = fBlock_(a, b);
                );
            );
        );
        Es = append(Es, matrix Em);
        Fs = append(Fs, matrix Fm);
        Hs = append(Hs, matrix Hm);
    );
    new HashTable from {
        "E" => Es,
        "F" => Fs,
        "H" => Hs,
        "weights" => ws,
        "offsets" => offsets,
        "ring" => ZZ
    }
)

chevalleyActionMatrices = method()

qolChevalleyActionMatrices = M -> (
    W := if instance(M, WeylModuleZ) then M else ambientWeylModule M;
    R := qolCoefficientRing M;
    ambientData := qolAmbientChevalleyMatrices W;
    coords := qolGlobalCoordinates M;
    J := coords#"section";
    P := coords#"projection";
    transform := L -> for X in L list P * (X ** R) * J;
    Es := transform(ambientData#"E");
    Fs := transform(ambientData#"F");
    Hs := transform(ambientData#"H");
    new ChevalleyActionData from {
        "module" => M,
        "ring" => R,
        "weights" => coords#"weights",
        "offsets" => coords#"offsets",
        "basis" => flatten for mu in coords#"weights" list
            for j from 0 to qolModuleWeightDimension(M, mu) - 1 list (mu, j),
        "E" => Es,
        "F" => Fs,
        "H" => Hs
    }
)

chevalleyActionMatrices WeylModuleZ := qolChevalleyActionMatrices
chevalleyActionMatrices SubWeylModuleFp := qolChevalleyActionMatrices
chevalleyActionMatrices QuotientWeylModuleFp := qolChevalleyActionMatrices
chevalleyActionMatrices SubquotientWeylModuleFp := qolChevalleyActionMatrices

raisingMatrices = method()
raisingMatrices ChevalleyActionData := D -> D#"E"

loweringMatrices = method()
loweringMatrices ChevalleyActionData := D -> D#"F"

cartanMatrices = method()
cartanMatrices ChevalleyActionData := D -> D#"H"

globalBasis = method()
globalBasis ChevalleyActionData := D -> D#"basis"

net ChevalleyActionData := D -> (
    r := #(D#"E");
    n := if r == 0 then 0 else numrows((D#"E")#0);
    "ChevalleyActionData(rank " | toString r | ", dimension " | toString n |
    ", over " | toString(D#"ring") | ")"
)

----------------------------------------------------------------------
-- Divided powers and the Kostant-form Cartan operators
----------------------------------------------------------------------

qolFactorial = n -> (
    if n < 0 then error "factorial: exponent must be non-negative";
    if n == 0 then return 1;
    product for k from 1 to n list k
)

qolExactMatrixQuotient = (X, d) -> (
    if d == 0 then error "exact matrix quotient: zero divisor";
    if any(flatten entries X, x -> x % d != 0) then
        error "divided-power matrix is not integral in the stored basis";
    matrix(ZZ, applyTable(entries X, x -> x // d))
)

qolAmbientDividedPower = (W, i, n, isE) -> (
    if n < 0 then error "dividedPowerAction: exponent must be non-negative";
    if i < 1 or i > numrows(W#"cartan") then error "dividedPowerAction: simple-root index out of range";
    D := qolAmbientChevalleyMatrices W;
    X := if isE then (D#"E")#(i - 1) else (D#"F")#(i - 1);
    if n == 0 then return id_(ZZ^(dim W));
    qolExactMatrixQuotient(X^n, qolFactorial n)
)

qolModuleDividedPower = (M, i, n, isE) -> (
    W := if instance(M, WeylModuleZ) then M else ambientWeylModule M;
    R := qolCoefficientRing M;
    X := qolAmbientDividedPower(W, i, n, isE);
    coords := qolGlobalCoordinates M;
    coords#"projection" * (X ** R) * coords#"section"
)

dividedPowerActionE = method()
dividedPowerActionE (WeylModuleZ, ZZ, ZZ) := (M, i, n) -> qolModuleDividedPower(M, i, n, true)
dividedPowerActionE (SubWeylModuleFp, ZZ, ZZ) := (M, i, n) -> qolModuleDividedPower(M, i, n, true)
dividedPowerActionE (QuotientWeylModuleFp, ZZ, ZZ) := (M, i, n) -> qolModuleDividedPower(M, i, n, true)
dividedPowerActionE (SubquotientWeylModuleFp, ZZ, ZZ) := (M, i, n) -> qolModuleDividedPower(M, i, n, true)

dividedPowerActionF = method()
dividedPowerActionF (WeylModuleZ, ZZ, ZZ) := (M, i, n) -> qolModuleDividedPower(M, i, n, false)
dividedPowerActionF (SubWeylModuleFp, ZZ, ZZ) := (M, i, n) -> qolModuleDividedPower(M, i, n, false)
dividedPowerActionF (QuotientWeylModuleFp, ZZ, ZZ) := (M, i, n) -> qolModuleDividedPower(M, i, n, false)
dividedPowerActionF (SubquotientWeylModuleFp, ZZ, ZZ) := (M, i, n) -> qolModuleDividedPower(M, i, n, false)

binomialCartanAction = method()
qolBinomialCartanAction = (M, i, n) -> (
    if n < 0 then error "binomialCartanAction: exponent must be non-negative";
    A := ambientCartan M;
    if i < 1 or i > numrows A then error "binomialCartanAction: simple-root index out of range";
    R := qolCoefficientRing M;
    coords := qolGlobalCoordinates M;
    Dm := mutableMatrix(R, dim M, dim M);
    for mu in coords#"weights" do (
        off := coords#"offsets"#mu;
        d := qolModuleWeightDimension(M, mu);
        c := (binomial(mu#(i - 1), n))_R;
        for j from 0 to d - 1 do Dm_(off + j, off + j) = c;
    );
    matrix Dm
)

binomialCartanAction (WeylModuleZ, ZZ, ZZ) := qolBinomialCartanAction
binomialCartanAction (SubWeylModuleFp, ZZ, ZZ) := qolBinomialCartanAction
binomialCartanAction (QuotientWeylModuleFp, ZZ, ZZ) := qolBinomialCartanAction
binomialCartanAction (SubquotientWeylModuleFp, ZZ, ZZ) := qolBinomialCartanAction

pbwRoots = method()
pbwRoots WeylModuleZ := W -> W#"positiveRoots"
pbwRoots SubWeylModuleFp := S -> (S#"ambient")#"positiveRoots"
pbwRoots QuotientWeylModuleFp := Q -> (Q#"ambient")#"positiveRoots"
pbwRoots SubquotientWeylModuleFp := M -> (M#"ambient")#"positiveRoots"

----------------------------------------------------------------------
-- Representation relations
----------------------------------------------------------------------

qolZeroMatrixQ = X -> all(flatten entries X, x -> x == 0)
qolCommutator = (X, Y) -> X * Y - Y * X

qolRecordCheck = (checks, failures, label, residual, context) -> (
    ok := qolZeroMatrixQ residual;
    checks#label = ok;
    if not ok then failures#(#failures) = new HashTable from {
        "check" => label,
        "context" => context,
        "residual" => residual
    };
    ok
)

checkWeylModule = method(Options => {CheckLevel => "Relations"})

qolCheckWeylModule = opts -> M -> (
    level := opts.CheckLevel;
    if not member(level, {"Basic", "Relations", "Serre"}) then
        error "checkWeylModule: CheckLevel must be \"Basic\", \"Relations\", or \"Serre\"";
    A := ambientCartan M;
    r := numrows A;
    D := chevalleyActionMatrices M;
    Es := raisingMatrices D;
    Fs := loweringMatrices D;
    Hs := cartanMatrices D;
    R := D#"ring";
    n := dim M;
    checks := new MutableHashTable;
    failures := new MutableList;
    recordFailure := (label, context, residual) ->
        failures#(#failures) = new HashTable from {
            "check" => label,
            "context" => context,
            "residual" => residual
        };

    checks#"dimension" = (sum values character M == n);
    if not checks#"dimension" then failures#(#failures) = new HashTable from {
        "check" => "dimension",
        "context" => "sum of weight multiplicities",
        "residual" => sum values character M - n
    };
    checks#"matrixShapes" = all(Es | Fs | Hs, X -> numrows X == n and numcols X == n);
    if not checks#"matrixShapes" then failures#(#failures) = new HashTable from {
        "check" => "matrixShapes", "context" => "global Chevalley matrices", "residual" => null
    };
    checks#"integrality" = if instance(M, WeylModuleZ) then
        all(Es | Fs | Hs, X -> ring X === ZZ) else true;
    if not checks#"integrality" then failures#(#failures) = new HashTable from {
        "check" => "integrality", "context" => "integral Weyl-module action", "residual" => null
    };

    -- Audit the stored action tables themselves.  Global assembly necessarily
    -- ignores a block whose source or target lies outside the module support;
    -- without this pass an extra illegal block could therefore evade every
    -- matrix relation below.  Shapes and coefficient rings are checked here
    -- before any comparison of weightwise blocks.
    storedActionsOK := true;
    weightPresent := mu -> qolModuleWeightDimension(M, mu) > 0;
    tableSpecs := if instance(M, WeylModuleZ) then
        {{"E", M#"actE"}, {"F", M#"actF"}}
    else if instance(M, QuotientWeylModuleFp) then
        {{"E", M#"actEQ"}, {"F", M#"actFQ"}}
    else if instance(M, SubquotientWeylModuleFp) then
        {{"E", M#"actEQ"}, {"F", M#"actFQ"}}
    else {};
    for spec in tableSpecs do (
        familyName := spec#0;
        actionTable := spec#1;
        for actionKey in keys actionTable do (
            keyData := try {actionKey#0, actionKey#1} else null;
            if keyData === null or not instance(keyData#0, List) or
               not instance(keyData#1, ZZ) then (
                storedActionsOK = false;
                recordFailure("storedActionKey", (familyName, actionKey), null);
                continue;
            );
            muKey := keyData#0;
            iKey := keyData#1;
            if #muKey != r or iKey < 1 or iKey > r then (
                storedActionsOK = false;
                recordFailure("storedActionKey", (familyName, actionKey), null);
                continue;
            );
            alphaKey := for k from 0 to r - 1 list A_(k, iKey - 1);
            upperKey := for k from 0 to r - 1 list muKey#k + alphaKey#k;
            lowerWeight := muKey;
            upperWeight := upperKey;
            lowerDim := qolModuleWeightDimension(M, lowerWeight);
            upperDim := qolModuleWeightDimension(M, upperWeight);
            block := actionTable#actionKey;
            expectedRows := if familyName == "E" then upperDim else lowerDim;
            expectedCols := if familyName == "E" then lowerDim else upperDim;
            validBlock := lowerDim > 0 and upperDim > 0 and
                          instance(block, Matrix) and ring block === R and
                          numrows block == expectedRows and
                          numcols block == expectedCols;
            if not validBlock then (
                storedActionsOK = false;
                recordFailure("storedActionBlock", (familyName, actionKey), block);
            );
        );
    );

    -- The integral object stores h_i separately.  Check every required value
    -- and reject extra indices or weights; the global H matrices are rebuilt
    -- from the weight labels and would not otherwise notice corrupted data.
    if instance(M, WeylModuleZ) then (
        storedH := M#"actH";
        for i from 1 to r do (
            if not storedH#?i then (
                storedActionsOK = false;
                recordFailure("storedCartanAction", i, null);
            ) else (
                Hi := storedH#i;
                for mu in M#"weightsList" do
                    if not Hi#?mu or Hi#mu != mu#(i - 1) then (
                        storedActionsOK = false;
                        recordFailure("storedCartanAction", (i, mu),
                            if Hi#?mu then Hi#mu - mu#(i - 1) else null);
                    );
            );
        );
        for i in keys storedH do (
            if not instance(i, ZZ) or i < 1 or i > r then (
                storedActionsOK = false;
                recordFailure("storedCartanAction", i, null);
                continue;
            );
            HiExtra := storedH#i;
            for mu in keys HiExtra do if not weightPresent mu then (
                storedActionsOK = false;
                recordFailure("storedCartanAction", (i, mu), HiExtra#mu);
            );
        );
    );
    checks#"storedActionTables" = storedActionsOK;

    -- Compare every public weightwise action with the corresponding block of
    -- the global matrices, including the boundary where the adjacent weight
    -- does not exist.  For submodules and quotient objects the global block is
    -- induced independently from the ambient integral module.
    coordsPublic := qolGlobalCoordinates M;
    publicWeights := coordsPublic#"weights";
    publicOffsets := coordsPublic#"offsets";
    weightwiseOK := true;
    for mu in publicWeights do for i from 1 to r do (
        alpha := for k from 0 to r - 1 list A_(k, i - 1);
        upper := for k from 0 to r - 1 list mu#k + alpha#k;
        lowerOff := publicOffsets#mu;
        lowerDim := qolModuleWeightDimension(M, mu);

        ePublic := actionE(M, mu, i);
        if publicOffsets#?upper then (
            upperOff := publicOffsets#upper;
            upperDim := qolModuleWeightDimension(M, upper);
            raisingBlockValid := ePublic === null or
                (instance(ePublic, Matrix) and ring ePublic === R and
                 numrows ePublic == upperDim and numcols ePublic == lowerDim);
            if not raisingBlockValid then (
                weightwiseOK = false;
                recordFailure("weightwiseActionE", (mu, i), ePublic);
            ) else (
                eRows := toList(upperOff .. upperOff + upperDim - 1);
                eCols := toList(lowerOff .. lowerOff + lowerDim - 1);
                eExpected := submatrix(Es#(i - 1), eRows, eCols);
                eActual := if ePublic === null then
                    matrix mutableMatrix(R, upperDim, lowerDim) else ePublic;
                if eExpected != eActual then (
                    weightwiseOK = false;
                    recordFailure("weightwiseActionE", (mu, i),
                                  eExpected - eActual);
                );
            );
        ) else if ePublic =!= null then (
            weightwiseOK = false;
            recordFailure("weightwiseActionE", (mu, i), ePublic);
        );

        fPublic := actionF(M, mu, i);
        if publicOffsets#?upper then (
            upperOff = publicOffsets#upper;
            upperDim = qolModuleWeightDimension(M, upper);
            validF := fPublic === null or
                (instance(fPublic, Matrix) and ring fPublic === R and
                 numrows fPublic == lowerDim and numcols fPublic == upperDim);
            if not validF then (
                weightwiseOK = false;
                recordFailure("weightwiseActionF", (mu, i), fPublic);
            ) else (
                fRows := toList(lowerOff .. lowerOff + lowerDim - 1);
                fCols := toList(upperOff .. upperOff + upperDim - 1);
                fExpected := submatrix(Fs#(i - 1), fRows, fCols);
                fActual := if fPublic === null then
                    matrix mutableMatrix(R, lowerDim, upperDim) else fPublic;
                if fExpected != fActual then (
                    weightwiseOK = false;
                    recordFailure("weightwiseActionF", (mu, i),
                                  fExpected - fActual);
                );
            );
        ) else if fPublic =!= null then (
            weightwiseOK = false;
            recordFailure("weightwiseActionF", (mu, i), fPublic);
        );

        hPublic := actionH(M, i, mu);
        if hPublic != mu#(i - 1) then (
            weightwiseOK = false;
            recordFailure("weightwiseActionH", (mu, i),
                          hPublic - mu#(i - 1));
        );
    );
    checks#"weightwiseActions" = weightwiseOK;

    -- Validate the stored submodule or quotient coordinates independently of
    -- the Lie relations.  For a submodule every ambient generator must
    -- preserve the stored column span.  For a quotient, the public weightwise
    -- action matrices must agree with projection of the ambient action.
    if instance(M, SubWeylModuleFp) then (
        W := ambientWeylModule M;
        ambientD := qolAmbientChevalleyMatrices W;
        subCoords := qolGlobalCoordinates M;
        J := subCoords#"section";
        P := subCoords#"projection";
        ambientIdentity := id_(R^(dim W));
        stable := true;
        for family in {ambientD#"E", ambientD#"F", ambientD#"H"} do
            for X in family do (
                residual := (ambientIdentity - J * P) * (X ** R) * J;
                if not qolZeroMatrixQ residual then (
                    stable = false;
                    failures#(#failures) = new HashTable from {
                        "check" => "submoduleStability",
                        "context" => "stored weight-space basis",
                        "residual" => residual
                    };
                );
            );
        checks#"objectConsistency" = stable;
    ) else if instance(M, QuotientWeylModuleFp) or
                instance(M, SubquotientWeylModuleFp) then (
        quotCoords := qolGlobalCoordinates M;
        ws := quotCoords#"weights";
        offsets := quotCoords#"offsets";
        consistent := true;
        for mu in ws do for i from 1 to r do (
            alpha := for k from 0 to r - 1 list A_(k, i - 1);
            srcOff := offsets#mu;
            srcDim := qolModuleWeightDimension(M, mu);

            upper := for k from 0 to r - 1 list mu#k + alpha#k;
            if offsets#?upper then (
                upperOff := offsets#upper;
                upperDim := qolModuleWeightDimension(M, upper);
                eRows := toList(upperOff .. upperOff + upperDim - 1);
                eCols := toList(srcOff .. srcOff + srcDim - 1);
                eExpected := submatrix(Es#(i - 1), eRows, eCols);
                eStored := actionE(M, mu, i);
                if eStored === null then eStored = matrix mutableMatrix(R, upperDim, srcDim);
                if eExpected != eStored then (
                    consistent = false;
                    failures#(#failures) = new HashTable from {
                        "check" => "quotientActionE",
                        "context" => (mu, i),
                        "residual" => eExpected - eStored
                    };
                );
            );

            if offsets#?upper then (
                upperOff = offsets#upper;
                upperDim = qolModuleWeightDimension(M, upper);
                fRows := toList(srcOff .. srcOff + srcDim - 1);
                fCols := toList(upperOff .. upperOff + upperDim - 1);
                fExpected := submatrix(Fs#(i - 1), fRows, fCols);
                fStored := actionF(M, mu, i);
                if fStored === null then fStored = matrix mutableMatrix(R, srcDim, upperDim);
                if fExpected != fStored then (
                    consistent = false;
                    failures#(#failures) = new HashTable from {
                        "check" => "quotientActionF",
                        "context" => (mu, i),
                        "residual" => fExpected - fStored
                    };
                );
            );
        );
        checks#"objectConsistency" = consistent;
        quotientDimensionOK := if instance(M, QuotientWeylModuleFp) then
            dim M + dim(definingKernel M) == dim(ambientWeylModule M)
        else dim M + dim(lowerSubmodule M) == dim(upperSubmodule M);
        checks#"quotientDimension" = quotientDimensionOK;
        if not quotientDimensionOK then failures#(#failures) = new HashTable from {
            "check" => "quotientDimension",
            "context" => if instance(M, QuotientWeylModuleFp) then
                "dim Q + dim ker = dim V" else "dim(T/S) + dim S = dim T",
            "residual" => if instance(M, QuotientWeylModuleFp) then
                dim M + dim(definingKernel M) - dim(ambientWeylModule M)
                else dim M + dim(lowerSubmodule M) - dim(upperSubmodule M)
        };
    ) else checks#"objectConsistency" = true;

    if member(level, {"Relations", "Serre"}) then (
        for i from 1 to r do for j from 1 to r do (
            qolRecordCheck(checks, failures,
                "[h" | toString i | ",h" | toString j | "]",
                qolCommutator(Hs#(i - 1), Hs#(j - 1)),
                (i, j));
            rhsEF := if i == j then Hs#(i - 1) else matrix mutableMatrix(R, n, n);
            qolRecordCheck(checks, failures,
                "[e" | toString i | ",f" | toString j | "]",
                qolCommutator(Es#(i - 1), Fs#(j - 1)) - rhsEF,
                (i, j));
            qolRecordCheck(checks, failures,
                "[h" | toString i | ",e" | toString j | "]",
                qolCommutator(Hs#(i - 1), Es#(j - 1)) -
                    (A_(i - 1, j - 1))_R * Es#(j - 1),
                (i, j));
            qolRecordCheck(checks, failures,
                "[h" | toString i | ",f" | toString j | "]",
                qolCommutator(Hs#(i - 1), Fs#(j - 1)) +
                    (A_(i - 1, j - 1))_R * Fs#(j - 1),
                (i, j));
        );
    );

    if level == "Serre" then (
        for i from 1 to r do for j from 1 to r do if i != j then (
            exponent := 1 - A_(i - 1, j - 1);
            resE := Es#(j - 1);
            resF := Fs#(j - 1);
            for k from 1 to exponent do (
                resE = qolCommutator(Es#(i - 1), resE);
                resF = qolCommutator(Fs#(i - 1), resF);
            );
            qolRecordCheck(checks, failures,
                "Serre(e" | toString i | ",e" | toString j | ")",
                resE, (i, j, exponent));
            qolRecordCheck(checks, failures,
                "Serre(f" | toString i | ",f" | toString j | ")",
                resF, (i, j, exponent));
        );
    );

    new WeylModuleCheck from {
        "module" => M,
        "level" => level,
        "checks" => new HashTable from checks,
        "failures" => toList failures,
        "valid" => #failures == 0
    }
)

checkWeylModule WeylModuleZ := qolCheckWeylModule
checkWeylModule SubWeylModuleFp := qolCheckWeylModule
checkWeylModule QuotientWeylModuleFp := qolCheckWeylModule
checkWeylModule SubquotientWeylModuleFp := qolCheckWeylModule

verifyRepresentation = method(Options => {CheckLevel => "Serre"})
verifyRepresentation WeylModuleZ := opts -> M -> checkWeylModule(M, CheckLevel => opts.CheckLevel)
verifyRepresentation SubWeylModuleFp := opts -> M -> checkWeylModule(M, CheckLevel => opts.CheckLevel)
verifyRepresentation QuotientWeylModuleFp := opts -> M -> checkWeylModule(M, CheckLevel => opts.CheckLevel)
verifyRepresentation SubquotientWeylModuleFp := opts -> M -> checkWeylModule(M, CheckLevel => opts.CheckLevel)

isValid = method()
isValid WeylModuleCheck := C -> C#"valid"

verificationChecks = method()
verificationChecks WeylModuleCheck := C -> C#"checks"

verificationFailures = method()
verificationFailures WeylModuleCheck := C -> C#"failures"

net WeylModuleCheck := C -> (
    status := if C#"valid" then "passed" else "failed";
    "WeylModuleCheck(" | C#"level" | ": " | status | ", " |
    toString(#(C#"failures")) | " failure(s))"
)

----------------------------------------------------------------------
-- Maximal-vector/Hom correspondence
----------------------------------------------------------------------

weylHomGenerators = method(Options => {WeightFormat => "Dynkin"})
weylHomGenerators (WeylModuleZ, ZZ, List) := opts -> (W, p, mu) ->
    maximalVectors(W, mu, p, WeightFormat => opts.WeightFormat)
weylHomGenerators (QuotientWeylModuleFp, List) := opts -> (Q, mu) ->
    maximalVectors(Q, mu, WeightFormat => opts.WeightFormat)

weylHomDimension = method(Options => {WeightFormat => "Dynkin"})
weylHomDimension (WeylModuleZ, ZZ, List) := opts -> (W, p, mu) ->
    #(weylHomGenerators(W, p, mu, WeightFormat => opts.WeightFormat))
weylHomDimension (QuotientWeylModuleFp, List) := opts -> (Q, mu) ->
    #(weylHomGenerators(Q, mu, WeightFormat => opts.WeightFormat))

----------------------------------------------------------------------
-- Weight graphs
----------------------------------------------------------------------

weightGraph = method()

qolWeightGraph = M -> (
    A := ambientCartan M;
    r := numrows A;
    D := chevalleyActionMatrices M;
    coords := qolGlobalCoordinates M;
    ws := coords#"weights";
    vertices := for mu in ws list new HashTable from {
        "weight" => mu,
        "multiplicity" => qolModuleWeightDimension(M, mu),
        "label" => toString mu | " (mult " | toString(qolModuleWeightDimension(M, mu)) | ")"
    };
    edges := {};
    for mu in ws do for i from 1 to r do (
        alpha := for k from 0 to r - 1 list A_(k, i - 1);
        nu := for k from 0 to r - 1 list mu#k - alpha#k;
        if not member(nu, ws) then continue;
        -- Extract the f_i block from the global matrix.
        srcOff := coords#"offsets"#mu;
        tgtOff := coords#"offsets"#nu;
        srcDim := qolModuleWeightDimension(M, mu);
        tgtDim := qolModuleWeightDimension(M, nu);
        rows := toList(tgtOff .. tgtOff + tgtDim - 1);
        cols := toList(srcOff .. srcOff + srcDim - 1);
        block := submatrix((D#"F")#(i - 1), rows, cols);
        edgeRank := rank block;
        if edgeRank > 0 then edges = append(edges, new HashTable from {
            "from" => mu,
            "to" => nu,
            "simpleRoot" => i,
            "rank" => edgeRank,
            "label" => "f_" | toString i | " (rank " | toString edgeRank | ")"
        });
    );
    new WeylGraph from {
        "module" => M,
        "vertices" => vertices,
        "edges" => edges,
        "certified" => true
    }
)

weightGraph WeylModuleZ := qolWeightGraph
weightGraph SubWeylModuleFp := qolWeightGraph
weightGraph QuotientWeylModuleFp := qolWeightGraph
weightGraph SubquotientWeylModuleFp := qolWeightGraph

graphVertices = method()
graphVertices WeylGraph := G -> G#"vertices"

graphEdges = method()
graphEdges WeylGraph := G -> G#"edges"

toDot = method()
toDot WeylGraph := G -> (
    vs := G#"vertices";
    es := G#"edges";
    out := "digraph WeylWeightGraph {\n  rankdir=TB;\n";
    for j from 0 to #vs - 1 do
        out = out | "  v" | toString j | " [label=\"" | (vs#j)#"label" | "\"];\n";
    indexAt := new MutableHashTable;
    for j from 0 to #vs - 1 do indexAt#((vs#j)#"weight") = j;
    for e in es do out = out | "  v" | toString(indexAt#(e#"from")) |
        " -> v" | toString(indexAt#(e#"to")) | " [label=\"" | e#"label" | "\"];\n";
    out | "}\n"
)

net WeylGraph := G -> (
    lines := {"Weyl weight graph: " | toString(#(G#"vertices")) | " vertices, " |
        toString(#(G#"edges")) | " nonzero simple-root edges"};
    for v in G#"vertices" do lines = append(lines,
        "  weight " | toString(v#"weight") | ": multiplicity " |
        toString(v#"multiplicity"));
    for e in G#"edges" do lines = append(lines,
        "  " | toString(e#"from") | " --" | e#"label" | "--> " | toString(e#"to"));
    demark(newline, lines)
)

----------------------------------------------------------------------
-- Socle-layer diagrams
----------------------------------------------------------------------

-- A socle analysis determines the semisimple factors in each layer, but it
-- does not determine individual extension arrows between those factors.
-- SocleDiagram therefore displays the computed layer factors and does not
-- infer an incidence relation from them.
socleDiagram = method()

socleDiagram SocleAnalysis := analysis -> (
    factorTables := socleLayerFactors analysis;
    layerDims := analysis#"layerDimensions";
    layers := for k from 0 to #factorTables - 1 list new HashTable from {
        "layer" => k + 1,
        "factors" => factorTables#k,
        "dimension" => layerDims#k,
        "ambiguousWeights" => (analysis#"ambiguousWeightsByLayer")#k
    };
    new SocleDiagram from {
        "analysis" => analysis,
        "layers" => layers,
        "certified" => isCertified analysis,
        "complete" => analysis#"complete",
        "incidenceDetermined" => false
    }
)

socleDiagram(WeylModuleZ, ZZ) := (W, p) -> socleDiagram socleAnalysis(W, p)
socleDiagram SubWeylModuleFp := S -> socleDiagram socleAnalysis S
socleDiagram QuotientWeylModuleFp := Q -> socleDiagram socleAnalysis Q

diagramLayers = method()
diagramLayers SocleDiagram := D -> D#"layers"

isCertified SocleDiagram := D -> D#"certified"

qolFactorTableText = T -> (
    if #keys T == 0 then return "0";
    terms := for mu in sort keys T list (
        m := T#mu;
        (if m == 1 then "" else toString m | " ") | "L(" | toString mu | ")"
    );
    concatenate between(" + ", terms)
)

net SocleDiagram := D -> (
    status := if D#"certified" then "complete socle filtration" else "socle search may be incomplete";
    lines := {"Socle diagram (bottom to top; " | status | ")"};
    for layer in D#"layers" do lines = append(lines,
        "  layer " | toString(layer#"layer") | " [dim " |
        toString(layer#"dimension") | "]: " | qolFactorTableText(layer#"factors") |
        (if #(layer#"ambiguousWeights") == 0 then "" else
            "  ambiguous at " | toString(layer#"ambiguousWeights")));
    lines = append(lines,
        "  Individual extension arrows are not determined by socle-layer data.");
    demark(newline, lines)
)

toDot SocleDiagram := D -> (
    out := "digraph SocleDiagram {\n  rankdir=BT;\n";
    if not D#"certified" then
        out = out | "  status [shape=note,label=\"socle search may be incomplete\"];\n";
    nodeIndex := 0;
    previousLayerNodes := {};
    for layer in D#"layers" do (
        currentLayerNodes := {};
        out = out | "  subgraph cluster_" | toString(layer#"layer") |
            " { rank=same; label=\"socle layer " | toString(layer#"layer") |
            " (dim " | toString(layer#"dimension") | ")" |
            (if #(layer#"ambiguousWeights") == 0 then "" else
                "\\nambiguous at " | toString(layer#"ambiguousWeights")) | "\";\n";
        for mu in sort keys(layer#"factors") do (
            multiplicity := (layer#"factors")#mu;
            nodeName := "n" | toString nodeIndex;
            currentLayerNodes = append(currentLayerNodes, nodeName);
            out = out | "    " | nodeName | " [label=\"" |
                (if multiplicity == 1 then "" else toString multiplicity | " x ") |
                "L(" | toString mu | ")\"];\n";
            nodeIndex = nodeIndex + 1;
        );
        out = out | "  }\n";
        -- Invisible layout edges communicate no mathematical incidence.
        if #previousLayerNodes > 0 and #currentLayerNodes > 0 then
            out = out | "  " | first previousLayerNodes | " -> " |
                first currentLayerNodes | " [style=invis];\n";
        previousLayerNodes = currentLayerNodes;
    );
    out | "}\n"
)

----------------------------------------------------------------------
-- Self-contained SVG drawings of weights and socle layers
----------------------------------------------------------------------

qolSVGXml = s -> (
    s = replace("&", "&amp;", toString s);
    s = replace("<", "&lt;", s);
    s = replace(">", "&gt;", s);
    s = replace("\"", "&quot;", s);
    replace("'", "&apos;", s)
)

qolSVGWeight = mu -> "(" | concatenate between(", ", apply(mu, toString)) | ")"

qolSVGText = (x, y, size, content, extra) ->
    "<text x=\"" | toString x | "\" y=\"" | toString y |
    "\" font-size=\"" | toString size | "\" " | extra | ">" |
    qolSVGXml content | "</text>\n"

qolSVGStart = (width, height, title, description) ->
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" | toString width |
    "\" height=\"" | toString height | "\" viewBox=\"0 0 " | toString width |
    " " | toString height | "\" role=\"img\" aria-label=\"" | qolSVGXml title |
    "\" style=\"max-width:100%;height:auto;background:#fff\">\n<title>" |
    qolSVGXml title | "</title>\n<desc>" | qolSVGXml description | "</desc>\n" |
    "<rect width=\"100%\" height=\"100%\" fill=\"#fff\"/>\n" |
    "<g font-family=\"Georgia, 'Times New Roman', serif\" fill=\"#182330\">\n" |
    qolSVGText(width // 2, 32, 22, title, "text-anchor=\"middle\"")

qolSVGEnd = "</g>\n</svg>\n"

toSVG = method()

toSVG WeylGraph := G -> (
    vs := graphVertices G;
    es := graphEdges G;
    if #vs == 0 then return
        qolSVGStart(460, 144, "Weight graph", "The module is zero: there are no weights or lowering edges.") |
        qolSVGText(230, 90, 18, "Zero module: no weights", "text-anchor=\"middle\"") |
        qolSVGEnd;
    M := G#"module";
    W := if instance(M, WeylModuleZ) then M else ambientWeylModule M;
    A := W#"cartan";
    lambda := W#"highestWeight";
    rows := new MutableHashTable;
    depthAt := new MutableHashTable;
    for v in vs do (
        mu := v#"weight";
        depth := lift(qolWeightDepth(A, lambda, mu), ZZ);
        depthAt#mu = depth;
        if not rows#?depth then rows#depth = {};
        rows#depth = append(rows#depth, mu);
    );
    depths := sort keys rows;
    firstDepth := first depths;
    lastDepth := last depths;
    boxWidth := max(154, 18 + 10 * max apply(vs, v -> #(qolSVGWeight(v#"weight"))));
    gapX := 56;
    gapY := 142;
    maxInRow := max apply(depths, d -> #(rows#d));
    width := max(480, 60 + maxInRow * boxWidth + (maxInRow - 1) * gapX);
    height := 184 + gapY * (lastDepth - firstDepth);
    centerAt := new MutableHashTable;
    for d in depths do (
        weights := sort rows#d;
        rowWidth := #weights * boxWidth + (#weights - 1) * gapX;
        left := (width - rowWidth) // 2;
        for j from 0 to #weights - 1 do
            centerAt#(weights#j) = {left + boxWidth // 2 + j * (boxWidth + gapX),
                110 + gapY * (d - firstDepth)};
    );
    out := qolSVGStart(width, height, "Weight graph",
        "Vertices are weights in fundamental-weight coordinates, with their multiplicities. An arrow labelled f_i goes from mu to mu minus alpha_i and gives the rank of the ordinary lowering map. Vertical position is the height below the ambient highest weight. Higher divided powers are not shown.");
    moduleLabel := if instance(M, WeylModuleZ) then "Over ZZ; rank " | toString dim M
        else "Characteristic " | toString(M#"prime") | "; dimension " | toString dim M;
    out = out | qolSVGText(width // 2, 59, 14, moduleLabel,
        "text-anchor=\"middle\" fill=\"#45576b\"");
    -- Edges precede the vertices, so their ends cannot cover weight labels.
    for e in es do (
        source := centerAt#(e#"from");
        target := centerAt#(e#"to");
        x1 := source#0;
        y1 := source#1 + 29;
        x2 := target#0;
        y2 := target#1 - 29;
        dx := x2 - x1;
        dy := y2 - y1;
        arrowBaseX := x2 - (10 * dx) // dy;
        label := "f_" | toString(e#"simpleRoot") | ", rank " | toString(e#"rank");
        explanation := "From weight " | qolSVGWeight(e#"from") | " to weight " |
            qolSVGWeight(e#"to") | ": " | label;
        labelX := (x1 + x2) // 2;
        labelY := (y1 + y2) // 2;
        labelWidth := 12 + 8 * #label;
        out = out | "<g role=\"group\" aria-label=\"" | qolSVGXml explanation |
            "\"><title>" | qolSVGXml explanation | "</title>\n" |
            "<path d=\"M " | toString x1 | " " | toString y1 | " L " |
            toString x2 | " " | toString y2 | "\" fill=\"none\" stroke=\"#435d7c\" stroke-width=\"1.8\"/>\n" |
            "<path d=\"M " | toString x2 | " " | toString y2 | " L " |
            toString(arrowBaseX - 4) | " " | toString(y2 - 10) | " L " |
            toString(arrowBaseX + 4) | " " | toString(y2 - 10) |
            " Z\" fill=\"#435d7c\"/>\n" |
            "<rect x=\"" | toString(labelX - labelWidth // 2) | "\" y=\"" |
            toString(labelY - 13) | "\" width=\"" | toString labelWidth |
            "\" height=\"23\" rx=\"4\" fill=\"#fff\"/>\n" |
            qolSVGText(labelX, labelY + 4, 14, label, "text-anchor=\"middle\"") |
            "</g>\n";
    );
    for v in vs do (
        mu := v#"weight";
        center := centerAt#mu;
        description := "Weight " | qolSVGWeight mu | ", multiplicity " | toString(v#"multiplicity");
        out = out | "<g role=\"group\" aria-label=\"" | qolSVGXml description |
            "\"><title>" | qolSVGXml description | "</title>\n" |
            "<rect x=\"" | toString(center#0 - boxWidth // 2) | "\" y=\"" |
            toString(center#1 - 28) | "\" width=\"" | toString boxWidth |
            "\" height=\"56\" rx=\"7\" fill=\"#f5f8fc\" stroke=\"#435d7c\"/>\n" |
            qolSVGText(center#0, center#1 - 3, 18, qolSVGWeight mu, "text-anchor=\"middle\"") |
            qolSVGText(center#0, center#1 + 17, 13, "multiplicity " | toString(v#"multiplicity"),
                "text-anchor=\"middle\" fill=\"#45576b\"") | "</g>\n";
    );
    out = out | qolSVGText(width // 2, height - 16, 13,
        "Ordinary lowering maps; higher divided powers are not shown.", "text-anchor=\"middle\" fill=\"#45576b\"");
    out | qolSVGEnd
)

toSVG SocleDiagram := D -> (
    layers := diagramLayers D;
    status := if isCertified D then "Complete socle filtration" else "Socle search may be incomplete";
    if #layers == 0 then return
        qolSVGStart(540, 166, "Socle layers", "No nonzero socle layers are recorded. " | status) |
        qolSVGText(270, 73, 15, status, "text-anchor=\"middle\"") |
        qolSVGText(270, 118, 18, "No nonzero layers", "text-anchor=\"middle\"") |
        qolSVGEnd;
    factorLabel := (mu, multiplicity) ->
        (if multiplicity == 1 then "" else toString multiplicity | " copies of ") |
        "L" | qolSVGWeight mu;
    factorLabels := flatten apply(layers, layer -> apply(sort keys(layer#"factors"),
        mu -> factorLabel(mu, (layer#"factors")#mu)));
    boxWidth := max(150, if #factorLabels == 0 then 150 else 20 + 9 * max apply(factorLabels, s -> #s));
    maxFactors := max(1, max apply(layers, layer -> #keys(layer#"factors")));
    gapX := 26;
    width := max(600, 76 + maxFactors * boxWidth + (maxFactors - 1) * gapX);
    ambiguousLabels := apply(layers, layer -> if #(layer#"ambiguousWeights") == 0 then "" else
        "Ambiguous maximal vectors at " | concatenate between(", ", apply(layer#"ambiguousWeights", qolSVGWeight)));
    width = max(width, 52 + 8 * max apply(ambiguousLabels, s -> #s));
    layerHeight := 116;
    height := 128 + layerHeight * #layers;
    out := qolSVGStart(width, height, "Socle layers",
        "Recorded layer 1 is at the bottom. Each layer lists its simple factors with multiplicities and its dimension. No arrows between factors are asserted. " | status);
    out = out | qolSVGText(width // 2, 60, 15, status,
        "text-anchor=\"middle\" fill=\"" | (if isCertified D then "#45576b" else "#8b3e00") | "\"");
    for j from 0 to #layers - 1 do (
        layer := layers#j;
        top := 78 + layerHeight * (#layers - 1 - j);
        label := "Layer " | toString(layer#"layer") | " — dimension " | toString(layer#"dimension");
        explanation := label | ". " | (if ambiguousLabels#j == "" then "" else ambiguousLabels#j | ". ") |
            "Simple factors in this layer form a direct sum; layers are not asserted to split off from the module.";
        out = out | "<g role=\"group\" aria-label=\"" | qolSVGXml explanation |
            "\"><title>" | qolSVGXml explanation | "</title>\n" |
            "<rect x=\"18\" y=\"" | toString top | "\" width=\"" | toString(width - 36) |
            "\" height=\"104\" rx=\"8\" fill=\"#f8fafc\" stroke=\"#aab7c7\"/>\n" |
            qolSVGText(34, top + 23, 15, label, "");
        mus := sort keys(layer#"factors");
        rowWidth := #mus * boxWidth + (#mus - 1) * gapX;
        left := (width - rowWidth) // 2;
        for k from 0 to #mus - 1 do (
            mu := mus#k;
            factor := factorLabel(mu, (layer#"factors")#mu);
            x := left + k * (boxWidth + gapX);
            out = out | "<g role=\"group\" aria-label=\"" | qolSVGXml factor |
                "\"><title>" | qolSVGXml factor | "</title>\n" |
                "<rect x=\"" | toString x | "\" y=\"" | toString(top + 35) |
                "\" width=\"" | toString boxWidth | "\" height=\"39\" rx=\"6\" fill=\"#fff\" stroke=\"#435d7c\"/>\n" |
                qolSVGText(x + boxWidth // 2, top + 60, 18, factor, "text-anchor=\"middle\"") |
                "</g>\n";
        );
        if #mus == 0 then out = out | qolSVGText(width // 2, top + 60, 18, "0", "text-anchor=\"middle\"");
        if ambiguousLabels#j != "" then out = out |
            qolSVGText(width // 2, top + 94, 13, ambiguousLabels#j,
                "text-anchor=\"middle\" fill=\"#8b3e00\"");
        out = out | "</g>\n";
    );
    out = out | qolSVGText(width // 2, height - 22, 13,
        (if isCertified D then "Layer 1 is the socle. " else "Recorded layer 1 is at the bottom. ") |
        "Individual extension arrows are not determined.",
        "text-anchor=\"middle\" fill=\"#45576b\"");
    out | qolSVGEnd
)

html WeylGraph := G -> toSVG G
html SocleDiagram := D -> toSVG D

----------------------------------------------------------------------
-- Saving and viewing diagrams
----------------------------------------------------------------------

qolSaveGraph = (filename, contents) -> (
    if filename == "" then error "graph export: a nonempty filename is required";
    -- M2 treats leading ! and $ as processes/sockets, not literal files.
    -- Prefix these names before normalizing: toAbsolutePath preserves them.
    if member(first filename, {"!", "$"}) then filename = "./" | filename;
    filename = toAbsolutePath filename;
    if fileExists filename or readlink filename =!= null then
        error("graph export: file already exists: " | filename);
    filename << contents << close;
    filename
)

toDot (WeylGraph, String) := (G, filename) -> qolSaveGraph(filename, toDot G)
toDot (SocleDiagram, String) := (D, filename) -> qolSaveGraph(filename, toDot D)
toSVG (WeylGraph, String) := (G, filename) -> qolSaveGraph(filename, toSVG G)
toSVG (SocleDiagram, String) := (D, filename) -> qolSaveGraph(filename, toSVG D)

-- urlEncode preserves # for web anchors, but in a filename it is literal.
qolGraphFileURL = filename ->
    replace("#", "%23", urlEncode(rootURI | realpath filename))

viewGraph = method()
qolViewGraph = G -> (
    filename := toSVG(G, temporaryFileName() | ".svg");
    show URL qolGraphFileURL filename;
    filename
)
viewGraph WeylGraph := qolViewGraph
viewGraph SocleDiagram := qolViewGraph
show WeylGraph := G -> viewGraph G
show SocleDiagram := D -> viewGraph D

----------------------------------------------------------------------
-- Documentation
----------------------------------------------------------------------

beginDocumentation()

load "./WeylModules/docs.m2"

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

load "./WeylModules/tests.m2"

end--
