newPackage("KacMoody",
    Version  => "2.0",
    Date     => "July 18, 2026",
    Authors  => {{
        Name     => "Keller VandeBogert",
        Email    => "keller.v@uky.edu",
        HomePage => "https://sites.google.com/view/kellervandebogert/home"
    }},
    HomePage => "https://sites.google.com/view/kellervandebogert/home",
    Headline => "Computations with symmetrizable Kac-Moody algebras and their representations",
    Keywords => {"Lie Groups and Lie Algebras", "Representation Theory"},
    PackageExports => {},
    PackageImports => {},
    AuxiliaryFiles => true,
    DebuggingMode => false
)

--------------------------------------------------------------------
-- KacMoody: symmetrizable Kac-Moody algebras in Macaulay2
--
-- Scope:
--   * Generalized Cartan matrices of finite, affine, and indefinite
--     type, handled uniformly via @TT "cartanType"@.
--   * Immutable parent-aware Cartan data, minimal realizations, root data,
--     global character lattices, and typed roots, coroots, weights, and
--     coweights.
--   * Positive-root enumeration, Weyl vector, Weyl-group elements,
--     Bruhat order, formal characters, and exact or certified-truncated
--     highest-weight modules.
--   * Parabolic/Levi graded decompositions of the nilradical as a
--     module for a finite-type derived Levi, with full reductive-Levi
--     central characters and automatic Cartan-type dispatch.
--   * The Kac--Moody Kostant formula for arbitrary symmetrizable type,
--     computed degree by degree when the parabolic Weyl quotient is
--     infinite, with compact derived-Levi output and optional
--     self-describing full Levi central characters.
--   * Exact root-multiplicity snapshots for affine and indefinite
--     types, with automatic backend selection.
--   * Interval-local Kazhdan--Lusztig calculations and exact bounded BGG
--     strata in arbitrary symmetrizable type.
--   * Finite structural invariants, Borel--Weil--Bott and Demazure
--     operators, lambda-ring operations, and finite highest-weight crystals.
--
-- Related packages:
--   * LieAlgebraRepresentations (P. Zinn-Justin):
--       Weyl character formula, fusion products, branching rules,
--       Gelfand-Tsetlin bases, explicit matrix representations.
--   * WeylGroups:
--       rendered Bruhat-order Hasse diagrams, parabolic coset
--       machinery with typed WeylGroupElement objects.
--
-- Bridges @TT "toLieAlgebra"@ and @TT "toRootSystem"@ convert a
-- finite-type Cartan matrix from this package into objects accepted
-- by those packages.
--------------------------------------------------------------------

export {
    -- Type-specific Cartan matrices
    "cartanA",
    "cartanB",
    "cartanC",
    "cartanD",
    "cartanE",
    "cartanF4",
    "cartanG2",
    -- Generic Dynkin-diagram constructors (arbitrary type)
    "cartanT",
    "cartanStar",
    "cartanRank2",
    -- Affine (untwisted) Cartan matrices
    "cartanExtendUntwisted",
    "cartanAffineA",
    "cartanAffineB",
    "cartanAffineC",
    "cartanAffineD",
    "cartanAffineE",
    "cartanAffineF4",
    "cartanAffineG2",
    -- Over- and very-extended (Lorentzian / hyperbolic) algebras
    "cartanOverExtended",
    "cartanVeryExtended",
    -- Type-specific simple roots (epsilon basis)
    "simpleRootsA",
    "simpleRootsB",
    "simpleRootsC",
    "simpleRootsD",
    "simpleRootsG2",
    "simpleRootsF4",
    "simpleRootsE6",
    "simpleRootsE7",
    "simpleRootsE8",
    -- Type-specific positive roots (epsilon basis)
    "positiveRootsA",
    "positiveRootsB",
    "positiveRootsC",
    "positiveRootsD",
    "positiveRootsG2",
    "positiveRootsF4",
    "positiveRootsE6",
    "positiveRootsE7",
    "positiveRootsE8",
    -- General positive root enumeration
    "cartanPositiveRoots",
    "cartanPositiveRootsDegree",
    "rootSnapshot",
    -- Parabolic decomposition
    "leviHighestWeights",
    "parabolicCosetReps",
    "dotActionOnWeight",
    "parabolicDotImages",
    "leviSubalgebra",
    "parabolicData",
    "parabolicDiagram",
    "nilradicalRoots",
    "parabolicDegrees",
    "leviConnectedComponents",
    -- Option symbols for parabolic output formatting
    "WeightFormat",
    "IncludeCentralCharacter",
    "Irrep",
    "LeviIrrep",
    "irrepData",
    "RootSnapshot",
    "LeviData",
    "WeylElement",
    "KMWeylGroup",
    "StandardParabolic",
    "GradedDecomposition",
    -- Weyl group
    "coxeterMatrix",
    "reflectionMatrix",
    "weylElementMatrix",
    "weylInversionData",
    "weylLength",
    "bruhatLessOrEqual",
    "longElement",
    "weylGroupOrder",
    "weylGroupElements",
    "weylLengthGenerating",
    "reducedWord",
    "leftDescentSet",
    "rightDescentSet",
    "weylGroup",
    "weylElement",
    "weylParent",
    "identityWeylElement",
    "simpleWeylReflection",
    "inverseWeylElement",
    "weylAction",
    "standardParabolic",
    "RemovedNodes",
    "LeviNodes",
    "removedNodes",
    "leviNodes",
    "parabolicLevi",
    "parabolicDegree",
    "isLeviRoot",
    "MaxElements",
    "MaxDegree",
    -- Distinguished Cartan matrices
    "partitionToFundamental",
    "fundamentalToPartition",
    "specialCartanFromPartition",
    -- General Cartan utilities
    "isCartanMatrix",
    "symmetrizer",
    "cartanType",
    "cartanRank",
    "cartanData",
    -- Parent-aware Cartan data, realizations, and lattice elements
    "CartanDatum",
    "Realization",
    "RootDatum",
    "KMRoot",
    "KMCoroot",
    "KMWeight",
    "KMCoweight",
    "GlobalForm",
    "CharacterLattice",
    "NodeLabels",
    "NodeFormat",
    "cartanDatum",
    "datumCartanMatrix",
    "standardRealization",
    "realization",
    "rootDatum",
    "kmRoot",
    "kmCoroot",
    "kmWeight",
    "kmCoweight",
    "weightFromRealizationCoordinates",
    "coweightFromRealizationCoordinates",
    "parentDatum",
    "datumCoordinates",
    "coordinateBasis",
    "realizationCoordinates",
    "weightBasis",
    "coweightBasis",
    "pairing",
    "toWeight",
    "toCoweight",
    "toRoot",
    "toCoroot",
    "weightToRootCoordinates",
    "coweightToCorootCoordinates",
    "isRootLatticeWeight",
    "isCorootLatticeCoweight",
    "pModQInvariants",
    "characterLatticeBasis",
    "cocharacterLatticeBasis",
    "integratesToGlobalForm",
    "integratesCoweightToGlobalForm",
    "simpleRoots",
    "coroots",
    "fundamentalWeights",
    "toFundamental",
    "toSimple",
    "toEpsilon",
    "fromEpsilon",
    "rootHeight",
    "rootInnerProduct",
    "rootNormSquared",
    -- Root system data
    "negativeRoots",
    "allRoots",
    "highestRoot",
    "highestShortRoot",
    "weylVector",
    "isDominant",
    "coxeterNumber",
    "dualCoxeterNumber",
    "dynkinDiagramData",
    "dynkinDiagram",
    "dualCartanMatrix",
    -- Finite structural and representation-theoretic data
    "dominantizeWeight",
    "weightStabilizer",
    "dualWeight",
    "isOppositionFixed",
    "isSelfDual",
    "fundamentalCoweights",
    "highestCoroot",
    "marks",
    "comarks",
    "lacingNumber",
    "weylExponents",
    "invariantDegrees",
    "minusculeNodes",
    "cominusculeNodes",
    "isMinusculeNode",
    "isCominusculeNode",
    "isMinusculeWeight",
    "isCominusculeWeight",
    "normalizedInvariantForm",
    "quadraticCasimir",
    -- Formal characters, modules, and reductive Levi restriction
    "FormalCharacter",
    "LeviFormalCharacter",
    "KMRepresentation",
    "ReductiveLeviDatum",
    "LeviWeight",
    "LeviHighestWeight",
    "CharacterStatus",
    "Truncation",
    "Depth",
    "formalCharacter",
    "characterDatum",
    "characterTerms",
    "characterSupport",
    "characterCoefficient",
    "isExactCharacter",
    "characterDimension",
    "finiteCharacter",
    "dualCharacter",
    "decomposeCharacter",
    "characterFromDecomposition",
    "integrableHighestWeightModule",
    "vermaModule",
    "moduleCharacter",
    "reductiveLeviDatum",
    "leviCenterBasis",
    "restrictWeight",
    "semisimplePart",
    "centralCharacter",
    "restrictCharacter",
    "decomposeLeviCharacter",
    -- Finite highest-weight crystals
    "Crystal",
    "CrystalVertex",
    "highestWeightCrystal",
    "tensorProductCrystal",
    "minusculeCrystal",
    "crystalDatum",
    "crystalHighestWeight",
    "crystalParent",
    "crystalVertexData",
    "crystalVertex",
    "crystalVertices",
    "crystalContains",
    "crystalTableau",
    "crystalWeight",
    "crystalF",
    "crystalE",
    "crystalEpsilon",
    "crystalPhi",
    "crystalEdges",
    "crystalCharacter",
    "adamsOperation",
    "symmetricPowerCharacter",
    "exteriorPowerCharacter",
    "symmetricPowerDecomposition",
    "exteriorPowerDecomposition",
    "frobeniusSchurIndicator",
    "representationDynkinIndex",
    -- Weyl dimension and representation theory
    "weylDimension",
    "weightsOfRep",
    "weightMultiplicity",
    "weightMultiplicityTable",
    "kostantPartitionFunction",
    "kostantWeightMultiplicity",
    "dominantWeightsInRep",
    "weightOrbit",
    "tensorDecomposition",
    "tensorCoefficient",
    "irrepDimension",
    "decompositionDimension",
    "cohomologyDimensions",
    "representationCacheInfo",
    "clearRepresentationCache",
    "kostantLieAlgCohomology",
    "BorelWeilBottResult",
    "LineBundleConvention",
    "borelWeilBott",
    "borelWeilBottCharacter",
    "demazureOperator",
    "demazureCharacter",
    "KazhdanLusztigPolynomialRing",
    "BGGResolution",
    "kazhdanLusztigVariable",
    "kazhdanLusztigCoefficients",
    "kazhdanLusztigPolynomial",
    "dotWeylAction",
    "simpleHighestWeightModule",
    "vermaCompositionMultiplicity",
    "vermaCompositionFactors",
    "bggResolution",
    "bggEulerCharacter",
    -- Affine Lie algebra functionality
    "affineRootSnapshot",
    "ImaginaryMultiplicity",
    "affineMarks",
    "affineComarks",
    "affineNullRoot",
    "affineCentralElement",
    "affineCartanA2Twisted",
    "affineCartanAOddTwisted",
    "affineCartanDTwisted",
    "affineCartanE6Twisted",
    "affineCartanD4Twisted",
    "affineTypeData",
    "affineFinitePart",
    "affineLevel",
    "affineClassicalPart",
    "affineDerivation",
    "affineDegree",
    "affineWeightFromDynkinLabels",
    "affineStructureCacheInfo",
    "clearAffineStructureCache",
    -- Indefinite Kac-Moody functionality
    "indefiniteRootSnapshot",
    -- Human-readable view of a root snapshot
    "rootSnapshotByHeight",
    "rootSnapshotRoots",
    "rootMultiplicity",
    "isRootInSnapshot",
    "rootSnapshotCacheInfo",
    "clearRootSnapshotCache",
    "Kind",
    -- Bridges to existing Macaulay2 packages
    "toLieAlgebra",
    "toRootSystem",
    "infrastructureCacheInfo",
    "clearInfrastructureCaches"
}

------------------------------------------------------------
-- Internal helpers (not exported)
------------------------------------------------------------

-- Optional heavy dependencies are loaded only by the features that need
-- them.  Most Cartan, root, Weyl, and representation computations therefore
-- do not pay Normaliz startup costs.
kacMoodyOptionalFunctions := new MutableHashTable;

optionalPackageFunction = (packageName, functionName) -> (
    key := (packageName, functionName);
    if kacMoodyOptionalFunctions#?key then
        return kacMoodyOptionalFunctions#key;
    try needsPackage packageName else
        error("KacMoody: could not load optional package " | packageName);
    packageObject := value packageName;
    dictionary := packageObject#"private dictionary";
    if not dictionary#?functionName then error concatenate(
        "KacMoody: optional package ", packageName,
        " does not provide ", functionName);
    f := value dictionary#functionName;
    kacMoodyOptionalFunctions#key = f;
    f
);

isPositiveVector = v -> (
    all(v, c -> c >= 0) and any(v, c -> c > 0)
);

isNegativeVector = v -> (
    all(v, c -> c <= 0) and any(v, c -> c < 0)
);

cartanCompositions = (h, r) -> (
    if h < 0 then error "cartanCompositions: h must be >= 0";
    if r < 1 then error "cartanCompositions: r must be >= 1";
    if r == 1 then (
        {{h}}
    ) else (
        L := {};
        for i from 0 to h do (
            tails := cartanCompositions(h - i, r-1);
            for t in tails do (
                L = append(L, {i} | t);
            );
        );
        L
    )
);


------------------------------------------------------------
------------------------------------------------------------
--
--         TYPE A LIE THEORY
--
------------------------------------------------------------
------------------------------------------------------------

-- Cartan matrix of type A_(n-1)
-- Input:  n >= 2 (corresponding to sl_n)
-- Output: (n-1) x (n-1) matrix over ZZ

cartanA = n -> (
    if not instance(n, ZZ) or n < 2 then error "cartanA: n must be an integer at least 2";
    r := n - 1;
    matrix table(r, r, (i,j) -> (
        if i == j then 2
        else if abs(i - j) == 1 then -1
        else 0
    ))
)

-- Simple roots: alpha_i = e_i - e_{i+1}, i = 1..n-1
simpleRootsA = n -> (
    if not instance(n, ZZ) or n < 2 then error "simpleRootsA: n must be an integer at least 2";
    r := n - 1;
    matrix table(r, n, (i,j) -> (
        if j == i then 1
        else if j == i+1 then -1
        else 0
    ))
)

-- Positive roots for type A_(n-1) in epsilon-basis
positiveRootsA = n -> (
    if not instance(n, ZZ) or n < 2 then error "positiveRootsA: n must be an integer at least 2";
    L := {};
    for i from 0 to n-1 do (
        for j from i+1 to n-1 do (
            r := for k from 0 to n-1 list (
                if k == i then 1
                else if k == j then -1
                else 0
            );
            L = append(L, r);
        )
    );
    matrix L
)


------------------------------------------------------------
------------------------------------------------------------
--
--         TYPE B LIE THEORY
--
------------------------------------------------------------
------------------------------------------------------------

-- Cartan matrix of type B_ell, rank ell >= 2
cartanB = ell -> (
    if not instance(ell, ZZ) or ell < 2 then error "cartanB: rank ell must be an integer at least 2";
    r := ell;
    matrix table(r, r, (i,j) -> (
        if i == j then 2
        else if abs(i - j) == 1 then (
            if i == r-2 and j == r-1 then -1
            else if i == r-1 and j == r-2 then -2
            else -1
        )
        else 0
    ))
)

-- Simple roots for type B_ell
simpleRootsB = ell -> (
    if not instance(ell, ZZ) or ell < 2 then error "simpleRootsB: ell must be an integer at least 2";
    r := ell;
    matrix table(r, r, (i,j) -> (
        if i < r-1 then (
            if j == i then 1
            else if j == i+1 then -1
            else 0
        ) else (
            if j == r-1 then 1 else 0
        )
    ))
)

-- Positive roots for type B_ell in epsilon-basis
positiveRootsB = ell -> (
    if not instance(ell, ZZ) or ell < 2 then error "positiveRootsB: ell must be an integer at least 2";
    L := {};
    for i from 0 to ell-1 do (
        for j from i+1 to ell-1 do (
            r1 := for k from 0 to ell-1 list (
                if k == i then 1
                else if k == j then -1
                else 0
            );
            r2 := for k from 0 to ell-1 list (
                if k == i or k == j then 1 else 0
            );
            L = append(L, r1);
            L = append(L, r2);
        )
    );
    for k from 0 to ell-1 do (
        r := for j from 0 to ell-1 list (if j == k then 1 else 0);
        L = append(L, r);
    );
    matrix L
)


------------------------------------------------------------
------------------------------------------------------------
--
--         TYPE C LIE THEORY
--
------------------------------------------------------------
------------------------------------------------------------

-- Cartan matrix of type C_ell, rank ell >= 2
cartanC = ell -> (
    if not instance(ell, ZZ) or ell < 2 then error "cartanC: rank ell must be an integer at least 2";
    r := ell;
    matrix table(r, r, (i,j) -> (
        if i == j then 2
        else if abs(i - j) == 1 then (
            if i == r-2 and j == r-1 then -2
            else if i == r-1 and j == r-2 then -1
            else -1
        )
        else 0
    ))
)

-- Simple roots for type C_ell
simpleRootsC = ell -> (
    if not instance(ell, ZZ) or ell < 2 then error "simpleRootsC: ell must be an integer at least 2";
    r := ell;
    matrix table(r, r, (i,j) -> (
        if i < r-1 then (
            if j == i then 1
            else if j == i+1 then -1
            else 0
        ) else (
            if j == r-1 then 2 else 0
        )
    ))
)

-- Positive roots for type C_ell in epsilon-basis
positiveRootsC = ell -> (
    if not instance(ell, ZZ) or ell < 2 then error "positiveRootsC: ell must be an integer at least 2";
    L := {};
    for i from 0 to ell-1 do (
        for j from i+1 to ell-1 do (
            r1 := for k from 0 to ell-1 list (
                if k == i then 1
                else if k == j then -1
                else 0
            );
            r2 := for k from 0 to ell-1 list (
                if k == i or k == j then 1 else 0
            );
            L = append(L, r1);
            L = append(L, r2);
        )
    );
    for k from 0 to ell-1 do (
        r := for j from 0 to ell-1 list (if j == k then 2 else 0);
        L = append(L, r);
    );
    matrix L
)


------------------------------------------------------------
------------------------------------------------------------
--
--         TYPE D LIE THEORY
--
------------------------------------------------------------
------------------------------------------------------------

-- Cartan matrix of type D_ell, rank ell >= 4
cartanD = ell -> (
    if not instance(ell, ZZ) or ell < 4 then error "cartanD: rank ell must be an integer at least 4";
    r := ell;
    matrix table(r, r, (i,j) -> (
        if i == j then 2
        else if (
            (abs(i - j) == 1 and max(i,j) <= r-3)
            or (i == r-3 and (j == r-2 or j == r-1))
            or (j == r-3 and (i == r-2 or i == r-1))
        ) then -1
        else 0
    ))
)

-- Simple roots for type D_ell (Bourbaki)
simpleRootsD = ell -> (
    if not instance(ell, ZZ) or ell < 4 then error "simpleRootsD: ell must be an integer at least 4";
    r := ell;
    matrix table(r, r, (i,j) -> (
        if i < r-1 then (
            if j == i then 1
            else if j == i+1 then -1
            else 0
        ) else (
            if j == r-2 then 1
            else if j == r-1 then 1
            else 0
        )
    ))
)

-- Positive roots for type D_ell in epsilon-basis
positiveRootsD = ell -> (
    if not instance(ell, ZZ) or ell < 4 then error "positiveRootsD: ell must be an integer at least 4";
    L := {};
    for i from 0 to ell-1 do (
        for j from i+1 to ell-1 do (
            r1 := for k from 0 to ell-1 list (
                if k == i then 1
                else if k == j then -1
                else 0
            );
            r2 := for k from 0 to ell-1 list (
                if k == i or k == j then 1 else 0
            );
            L = append(L, r1);
            L = append(L, r2);
        )
    );
    matrix L
)


------------------------------------------------------------
------------------------------------------------------------
--
--         EXCEPTIONAL TYPES
--
------------------------------------------------------------
------------------------------------------------------------

-- Cartan matrix of type E_ell (ell = 6, 7, or 8)
-- Bourbaki numbering:
--       2
--       |
-- 1--3--4--5--6    (E6)
cartanE = ell -> (
    if not instance(ell, ZZ) or not member(ell, {6,7,8}) then error "cartanE: ell must be the integer 6, 7, or 8";
    if ell == 6 then (
        matrix {
          { 2,  0, -1,  0,  0,  0},
          { 0,  2,  0, -1,  0,  0},
          {-1,  0,  2, -1,  0,  0},
          { 0, -1, -1,  2, -1,  0},
          { 0,  0,  0, -1,  2, -1},
          { 0,  0,  0,  0, -1,  2}
        }
    ) else if ell == 7 then (
        matrix {
          { 2,  0, -1,  0,  0,  0,  0},
          { 0,  2,  0, -1,  0,  0,  0},
          {-1,  0,  2, -1,  0,  0,  0},
          { 0, -1, -1,  2, -1,  0,  0},
          { 0,  0,  0, -1,  2, -1,  0},
          { 0,  0,  0,  0, -1,  2, -1},
          { 0,  0,  0,  0,  0, -1,  2}
        }
    ) else (
        matrix {
          { 2,  0, -1,  0,  0,  0,  0,  0},
          { 0,  2,  0, -1,  0,  0,  0,  0},
          {-1,  0,  2, -1,  0,  0,  0,  0},
          { 0, -1, -1,  2, -1,  0,  0,  0},
          { 0,  0,  0, -1,  2, -1,  0,  0},
          { 0,  0,  0,  0, -1,  2, -1,  0},
          { 0,  0,  0,  0,  0, -1,  2, -1},
          { 0,  0,  0,  0,  0,  0, -1,  2}
        }
    )
)

-- Cartan matrix of type F4
-- Bourbaki diagram: 1 - 2 => 3 - 4
-- Cartan matrix of type F4 (Bourbaki convention: a_{ij} = 2(alpha_i,alpha_j)/(alpha_i,alpha_i))
-- Diagram: 1 --- 2 => 3 --- 4 (alpha_1, alpha_2 long; alpha_3, alpha_4 short)
cartanF4 = () -> (
    matrix {
      { 2, -1,  0,  0},
      {-1,  2, -1,  0},
      { 0, -2,  2, -1},
      { 0,  0, -1,  2}
    }
)

-- Cartan matrix of type G2 (Bourbaki convention: a_{ij} = 2(alpha_i,alpha_j)/(alpha_i,alpha_i))
-- Diagram: 1 <# 2  (alpha_1 short, alpha_2 long)
cartanG2 = () -> (
    matrix {
      { 2, -3},
      {-1,  2}
    }
)

------------------------------------------------------------
-- Tripod / star-shaped / rank-2 Cartan matrix constructors.
--
-- All matrices below follow the same sign convention as cartanA..G2:
-- A_{ij} = <alpha_j, alpha_i^vee> = 2(alpha_i,alpha_j)/(alpha_i,alpha_i),
-- so the row of a long root has -1 in the column of an adjacent short
-- root, and the row of a short root has -2 (or -3) in the column of
-- the long one.  Everything that follows is simply laced (all
-- off-diagonals in {0,-1}) except where a long/short pair is
-- explicitly indicated.
--
-- cartanT(p,q,r) returns the Cartan matrix of the tripod Dynkin
-- diagram with three arms emanating from a central trivalent node;
-- the arms have p, q, r nodes counting the center.  By the Coxeter
-- classification when p,q,r are all at least 2,
--   1/p + 1/q + 1/r > 1  finite    (spherical)
--   1/p + 1/q + 1/r = 1  affine    (Euclidean)
--   1/p + 1/q + 1/r < 1  hyperbolic/Lorentzian (indefinite)
-- which cartanType will detect automatically.
--
-- Node ordering: the leaf of the first arm is node 1, the center is
-- node p, and arms 2, 3 extend outward from the center.
------------------------------------------------------------

cartanT = (p, q, r) -> (
    if not (instance(p, ZZ) and instance(q, ZZ) and instance(r, ZZ)) then
        error "cartanT: p, q, r must be integers";
    if p < 1 or q < 1 or r < 1 then
        error "cartanT: p, q, r must be positive integers";
    n := p + q + r - 2;
    if n < 1 then error "cartanT: total rank p+q+r-2 must be positive";
    -- Three arms as lists of 0-indexed matrix positions, each starting
    -- at the center.
    centerIdx := p - 1;
    arm1 := reverse for i from 0 to p-1 list i;
    arm2 := for i from 0 to q-1 list centerIdx + i;
    arm3 := prepend(centerIdx,
        for i from 0 to r-2 list centerIdx + q + i);
    M := mutableMatrix(ZZ, n, n);
    for i from 0 to n-1 do M_(i,i) = 2;
    addChain := pth -> (
        for k from 0 to #pth-2 do (
            M_(pth#k, pth#(k+1)) = -1;
            M_(pth#(k+1), pth#k) = -1;
        );
    );
    addChain arm1;
    addChain arm2;
    addChain arm3;
    matrix M
)

-- cartanStar({p_1, ..., p_k}) returns the Cartan matrix of a
-- star-shaped simply-laced Dynkin diagram with k arms of lengths
-- p_1,...,p_k (each counting the central node).  The center is node 1
-- and arm i occupies nodes of matrix-index contributing (p_i - 1)
-- positions after the center.  For k=3 this coincides with cartanT up
-- to a permutation of indices; for k<=2 this gives a chain (type A);
-- for k>=4 the diagrams are always indefinite.

cartanStar = armLens -> (
    if not instance(armLens, List) then
        error "cartanStar: argument must be a list of positive integers";
    if #armLens == 0 then error "cartanStar: need at least one arm";
    for p in armLens do (
        if not instance(p, ZZ) then
            error "cartanStar: arm lengths must be integers";
        if p < 1 then
            error "cartanStar: arm lengths must be positive";
    );
    n := 1 + sum(armLens, p -> p - 1);
    M := mutableMatrix(ZZ, n, n);
    for i from 0 to n-1 do M_(i,i) = 2;
    -- center is matrix index 0; arm i starts just after the previous
    -- arm's last node.
    cursor := 1;
    for p in armLens do (
        if p >= 2 then (
            -- edge from center to first arm node
            M_(0, cursor) = -1; M_(cursor, 0) = -1;
            for k from 0 to p-3 do (
                M_(cursor + k, cursor + k + 1) = -1;
                M_(cursor + k + 1, cursor + k) = -1;
            );
            cursor = cursor + p - 1;
        );
    );
    matrix M
)

-- cartanRank2(a,b) wraps the 2x2 generalized Cartan matrix
-- [[2,-a],[-b,2]] with a,b positive integers.  The product a*b
-- controls the type:
--   a*b = 1   -> A_2
--   a*b = 2   -> B_2 = C_2
--   a*b = 3   -> G_2
--   a*b = 4   -> affine A_1^(1) (untwisted, if a=b=2) or A_2^(2)
--   a*b >= 5  -> rank-2 hyperbolic

cartanRank2 = (a, b) -> (
    if not (instance(a, ZZ) and instance(b, ZZ)) then
        error "cartanRank2: a, b must be integers";
    if a <= 0 or b <= 0 then
        error "cartanRank2: a, b must be positive";
    matrix {{2, -a}, {-b, 2}}
)

------------------------------------------------------------
-- Untwisted affine Cartan matrices.
--
-- The affine extension adjoins a new simple root alpha_0 = -theta
-- to a finite-type Cartan, where theta is the highest root.  The
-- resulting (r+1) x (r+1) matrix is the "extended" generalized Cartan,
-- which is positive semidefinite with 1-dimensional kernel spanned by
-- the null root.  cartanType detects these as "affine".
--
-- We place the affine node alpha_0 at matrix index 0 and retain the
-- finite-type ordering of alpha_1,...,alpha_r at indices 1..r.
-- The specific diagrams are those of Kac's Table Aff 1 (untwisted).
------------------------------------------------------------

-- cartanExtendUntwisted(A) computes the untwisted affine extension
-- by the general rule alpha_0 = -theta, returning the (r+1)x(r+1)
-- generalized Cartan.  Works for any connected finite-type A.
cartanExtendUntwisted = A -> (
    requireConnectedFiniteCartanType("cartanExtendUntwisted", A);
    r := numrows A;
    theta := highestRoot A;   -- simple-root coordinates, a List
    thetaNorm := rootInnerProduct(A, theta, theta);
    M := mutableMatrix(QQ, r+1, r+1);
    for i from 0 to r do M_(i,i) = 2;
    for i from 0 to r-1 do for j from 0 to r-1 do M_(i+1, j+1) = A_(i,j);
    for j from 0 to r-1 do (
        ej := for k from 0 to r-1 list (if k == j then 1 else 0);
        tj := rootInnerProduct(A, theta, ej);  -- (theta, alpha_j)
        ajNorm := rootInnerProduct(A, ej, ej); -- (alpha_j, alpha_j)
        -- A_{0, j+1} = <alpha_{j+1}_{new label}, alpha_0^vee>
        --            = 2(alpha_0, alpha_j)/(alpha_0, alpha_0)
        --            = 2(-theta, alpha_j)/(theta, theta)
        a0j := -2 * tj / thetaNorm;
        -- A_{j+1, 0} = 2(alpha_j, -theta)/(alpha_j, alpha_j)
        aj0 := -2 * tj / ajNorm;
        M_(0, j+1) = a0j;
        M_(j+1, 0) = aj0;
    );
    -- All affine-extension entries should be nonpositive small integers
    MZ := mutableMatrix(ZZ, r+1, r+1);
    for i from 0 to r do for j from 0 to r do (
        e := M_(i,j);
        if denominator e =!= 1 then
            error "cartanExtendUntwisted: nonintegral affine extension (unexpected)";
        MZ_(i,j) = numerator e;
    );
    matrix MZ
)

-- Untwisted affine A_n: (n+1) nodes in a cycle.  Special case n=1
-- is the two-node A_1^(1) = [[2,-2],[-2,2]].

cartanAffineA = n -> (
    if not instance(n, ZZ) then error "cartanAffineA: n must be an integer";
    if n < 1 then error "cartanAffineA: need n >= 1";
    if n == 1 then matrix{{2, -2}, {-2, 2}}
    else (
        r := n + 1;
        matrix table(r, r, (i,j) ->
            if i == j then 2
            else if (i - j) % r == 1 or (j - i) % r == 1 then -1
            else 0)
    )
)

-- Untwisted affine B_n (n >= 3).  Affine node attached to original
-- node 2 by a single bond; original B_n occupies indices 1..n.

cartanAffineB = n -> (
    if not instance(n, ZZ) then error "cartanAffineB: n must be an integer";
    if n < 3 then error "cartanAffineB: need n >= 3";
    r := n + 1;
    M := mutableMatrix(ZZ, r, r);
    for i from 0 to r-1 do M_(i,i) = 2;
    -- affine node at index 0, attached to index 2
    M_(0, 2) = -1; M_(2, 0) = -1;
    -- B_n chain on indices 1..n
    for i from 1 to n-2 do (M_(i, i+1) = -1; M_(i+1, i) = -1);
    -- double bond between node n-1 (long) and node n (short)
    M_(n-1, n) = -1; M_(n, n-1) = -2;
    matrix M
)

-- Untwisted affine C_n (n >= 2).  Both end bonds are double; all
-- internal bonds single.

cartanAffineC = n -> (
    if not instance(n, ZZ) then error "cartanAffineC: n must be an integer";
    if n < 2 then error "cartanAffineC: need n >= 2";
    r := n + 1;
    M := mutableMatrix(ZZ, r, r);
    for i from 0 to r-1 do M_(i,i) = 2;
    -- affine node 0 (long), node 1 (short): double bond with arrow 0 -> 1
    M_(0, 1) = -1; M_(1, 0) = -2;
    -- internal chain 1-2-...-(n-1) (all short)
    for i from 1 to n-2 do (M_(i, i+1) = -1; M_(i+1, i) = -1);
    -- double bond (n-1) short, n long
    M_(n-1, n) = -2; M_(n, n-1) = -1;
    matrix M
)

-- Untwisted affine D_n (n >= 4).  Two "forks" on each end, joined by
-- a chain.

cartanAffineD = n -> (
    if not instance(n, ZZ) then error "cartanAffineD: n must be an integer";
    if n < 4 then error "cartanAffineD: need n >= 4";
    r := n + 1;
    M := mutableMatrix(ZZ, r, r);
    for i from 0 to r-1 do M_(i,i) = 2;
    -- leaves 0 and 1, both attached to index 2
    M_(0, 2) = -1; M_(2, 0) = -1;
    M_(1, 2) = -1; M_(2, 1) = -1;
    -- chain 2-3-...-(n-2)
    for i from 2 to n-3 do (M_(i, i+1) = -1; M_(i+1, i) = -1);
    -- leaves n-1 and n attached to n-2
    M_(n-2, n-1) = -1; M_(n-1, n-2) = -1;
    M_(n-2, n)   = -1; M_(n,   n-2) = -1;
    matrix M
)

-- Untwisted affine E_n (n in {6,7,8}).  Realized as T-type diagrams:
--   affine E_6 = T(3,3,3)
--   affine E_7 = T(2,4,4)
--   affine E_8 = T(2,3,6)
-- so cartanAffineE delegates to cartanT.  The resulting node
-- numbering differs from Kac's Table Aff 1 but describes the same
-- Dynkin diagram (all affine functionality is permutation-invariant).

cartanAffineE = n -> (
    if not instance(n, ZZ) or not member(n, {6,7,8}) then error "cartanAffineE: n must be the integer 6, 7, or 8";
    if n == 6 then cartanT(3,3,3)
    else if n == 7 then cartanT(2,4,4)
    else cartanT(2,3,6)
)

-- Untwisted affine F_4: 5 nodes, 0 - 1 - 2 => 3 - 4.

cartanAffineF4 = () -> (
    matrix {
      { 2, -1,  0,  0,  0},
      {-1,  2, -1,  0,  0},
      { 0, -1,  2, -1,  0},
      { 0,  0, -2,  2, -1},
      { 0,  0,  0, -1,  2}
    }
)

-- Untwisted affine G_2: 3 nodes, 0 - 2 with 1 attached by the G_2
-- triple bond to 2.  (Indices 1, 2 match package's cartanG2.)

cartanAffineG2 = () -> (
    matrix {
      { 2,  0, -1},
      { 0,  2, -3},
      {-1, -1,  2}
    }
)

------------------------------------------------------------
-- Over- and very-extended Kac-Moody Cartan matrices.
--
-- cartanOverExtended(A) takes a finite-type Cartan A, affinises it
-- via cartanExtendUntwisted to get A^+ (= A^(1)), and then appends a
-- further simple root attached by a single bond to alpha_0 (the
-- affine node).  The result A^{++} is a hyperbolic/Lorentzian
-- Kac-Moody matrix whenever such an extension makes sense.  When A
-- is of type E_n, the iterated extensions give the algebras
-- E_9 = E_8^+, E_{10} = E_8^{++}, E_{11} = E_8^{+++}.
------------------------------------------------------------

cartanOverExtended = A -> (
    Aaff := cartanExtendUntwisted A;
    r := numrows Aaff;
    M := mutableMatrix(ZZ, r+1, r+1);
    for i from 0 to r-1 do for j from 0 to r-1 do M_(i+1, j+1) = Aaff_(i,j);
    M_(0, 0) = 2;
    -- attach new node at index 0 to affine node (now at index 1) by
    -- a single bond (both long, all simply laced in the extension)
    M_(0, 1) = -1; M_(1, 0) = -1;
    matrix M
)

-- Helper: single over-extension step that works on any GCM (not just
-- finite-type).  Used by cartanVeryExtended so the result prepends a
-- new node to an already over-extended matrix.
cartanOverExtendStep = A -> (
    r := numrows A;
    M := mutableMatrix(ZZ, r+1, r+1);
    for i from 0 to r-1 do for j from 0 to r-1 do M_(i+1, j+1) = A_(i,j);
    M_(0, 0) = 2;
    M_(0, 1) = -1; M_(1, 0) = -1;
    matrix M
)

-- Very-extended: over-extend once, then prepend one more node.
cartanVeryExtended = A -> cartanOverExtendStep cartanOverExtended A

-- G2 simple roots (rank 2, ambient dimension 3)
simpleRootsG2 = () -> (
    matrix {
        {  1, -1,  0 },
        { -2,  1,  1 }
    }
)

-- F4 simple roots (rank 4, ambient dimension 4)
simpleRootsF4 = () -> (
    matrix {
        {  0,  1, -1,  0 },
        {  0,  0,  1, -1 },
        {  0,  0,  0,  1 },
        { 1/2, -1/2, -1/2, -1/2 }
    }
)

-- E6 simple roots (rank 6, ambient dimension 8)
simpleRootsE6 = () -> (
    matrix {
        { 1/2,-1/2,-1/2,-1/2,-1/2,-1/2,-1/2, 1/2 },
        {  1,   1,   0,   0,   0,   0,   0,   0 },
        { -1,   1,   0,   0,   0,   0,   0,   0 },
        {  0,  -1,   1,   0,   0,   0,   0,   0 },
        {  0,   0,  -1,   1,   0,   0,   0,   0 },
        {  0,   0,   0,  -1,   1,   0,   0,   0 }
    }
)

-- E7 simple roots (rank 7, ambient dimension 8)
simpleRootsE7 = () -> (
    matrix {
        { 1/2,-1/2,-1/2,-1/2,-1/2,-1/2,-1/2, 1/2 },
        {  1,   1,   0,   0,   0,   0,   0,   0 },
        { -1,   1,   0,   0,   0,   0,   0,   0 },
        {  0,  -1,   1,   0,   0,   0,   0,   0 },
        {  0,   0,  -1,   1,   0,   0,   0,   0 },
        {  0,   0,   0,  -1,   1,   0,   0,   0 },
        {  0,   0,   0,   0,  -1,   1,   0,   0 }
    }
)

-- E8 simple roots (rank 8, ambient dimension 8)
simpleRootsE8 = () -> (
    matrix {
        { 1/2,-1/2,-1/2,-1/2,-1/2,-1/2,-1/2, 1/2 },
        {  1,   1,   0,   0,   0,   0,   0,   0 },
        { -1,   1,   0,   0,   0,   0,   0,   0 },
        {  0,  -1,   1,   0,   0,   0,   0,   0 },
        {  0,   0,  -1,   1,   0,   0,   0,   0 },
        {  0,   0,   0,  -1,   1,   0,   0,   0 },
        {  0,   0,   0,   0,  -1,   1,   0,   0 },
        {  0,   0,   0,   0,   0,  -1,   1,   0 }
    }
)


------------------------------------------------------------
------------------------------------------------------------
--
--  EXCEPTIONAL TYPE: POSITIVE ROOTS (EPSILON AND CARTAN BASIS)
--
------------------------------------------------------------
------------------------------------------------------------

-- G2 positive roots in epsilon-basis (ambient dim 3)
-- 6 positive roots: 3 short + 3 long
positiveRootsG2 = () -> (
    -- Short roots: e1-e2, e1-e3, e2-e3 (nope -- need the G2 root system)
    -- G2 in the hyperplane e1+e2+e3=0 of R^3:
    -- Short positive: e1-e2, -e1+2e2-e3, e2-e3  (norms 2)
    -- Long positive: 2e1-e2-e3, e1+e2-2e3, -e1+2e2-e3 ...
    -- Actually let's use the standard G2 roots in R^3 (hyperplane sum=0):
    -- Positive roots (Bourbaki with alpha1 short, alpha2 long):
    --   alpha1 = e1 - e2
    --   alpha2 = -2e1 + e2 + e3
    --   alpha1+alpha2 = -e1 + e3
    --   2*alpha1+alpha2 = -e2 + e3  (wait, let me be careful)
    -- alpha1 = (1,-1,0), alpha2 = (-2,1,1)
    -- alpha1+alpha2 = (-1,0,1)
    -- 2*alpha1+alpha2 = (0,-1,1)
    -- 3*alpha1+alpha2 = (1,-2,1)
    -- 3*alpha1+2*alpha2 = (-1,-1,2)
    matrix {
        {  1, -1,  0 },    -- alpha1
        { -2,  1,  1 },    -- alpha2
        { -1,  0,  1 },    -- alpha1 + alpha2
        {  0, -1,  1 },    -- 2*alpha1 + alpha2
        {  1, -2,  1 },    -- 3*alpha1 + alpha2
        { -1, -1,  2 }     -- 3*alpha1 + 2*alpha2
    }
)

-- F4 positive roots in epsilon-basis (ambient dim 4)
-- 24 positive roots
positiveRootsF4 = () -> (
    L := {};
    -- Long roots: e_i +/- e_j (i < j), 12 of these are positive
    for i from 0 to 3 do (
        for j from i+1 to 3 do (
            r1 := for k from 0 to 3 list (
                if k == i then 1 else if k == j then -1 else 0
            );
            r2 := for k from 0 to 3 list (
                if k == i or k == j then 1 else 0
            );
            L = append(L, r1);
            L = append(L, r2);
        )
    );
    -- Short roots: e_i, 4 of these
    for i from 0 to 3 do (
        L = append(L, for k from 0 to 3 list (if k == i then 1 else 0));
    );
    -- Short roots: (1/2)(+/-e1 +/- e2 +/- e3 +/- e4), 8 positive ones
    -- positive means the first nonzero sign from left determines positivity;
    -- actually for F4 the positive half-integer roots are all with e1 coeff = +1/2
    for s2 from 0 to 1 do (
        for s3 from 0 to 1 do (
            for s4 from 0 to 1 do (
                sgn2 := if s2 == 0 then 1 else -1;
                sgn3 := if s3 == 0 then 1 else -1;
                sgn4 := if s4 == 0 then 1 else -1;
                L = append(L, {1/2, sgn2 * (1/2), sgn3 * (1/2), sgn4 * (1/2)});
            );
        );
    );
    matrix L
)

-- E6 positive roots in epsilon-basis (ambient dim 8)
-- 36 positive roots
positiveRootsE6 = () -> (
    S := simpleRootsE6();
    A := cartanE 6;
    -- Get roots in Cartan basis via the general algorithm
    Rc := cartanPositiveRoots A;
    -- Convert to epsilon basis: each row of Rc gives coefficients in
    -- simple root basis, multiply by S to get epsilon coords
    Rc * (S ** QQ)
)

-- E7 positive roots in epsilon-basis (ambient dim 8)
-- 63 positive roots
positiveRootsE7 = () -> (
    S := simpleRootsE7();
    A := cartanE 7;
    Rc := cartanPositiveRoots A;
    Rc * (S ** QQ)
)

-- E8 positive roots in epsilon-basis (ambient dim 8)
-- 120 positive roots
positiveRootsE8 = () -> (
    S := simpleRootsE8();
    A := cartanE 8;
    Rc := cartanPositiveRoots A;
    Rc * (S ** QQ)
)


------------------------------------------------------------
------------------------------------------------------------
--
--         GENERAL POSITIVE ROOT ENUMERATION
--
------------------------------------------------------------
------------------------------------------------------------

cartanPositiveRoots = method();

-- Core algorithm with an optional upper bound.
-- Uses a MutableHashTable "seen" for O(1) duplicate check (BFS with
-- list-based membership was O(|R|^2) in the rank).
cartanPositiveRootsCore = (A, maxCoeff) -> (
    A = integralCartanMatrix(A, "cartanPositiveRoots");
    r := numrows A;

    R := new MutableList;
    Q := new MutableList;
    seen := new MutableHashTable;

    for i from 0 to r-1 do (
        v := for j from 0 to r-1 list (if j == i then 1 else 0);
        if maxCoeff === null or 1 <= maxCoeff then (
            R#(#R) = v;
            Q#(#Q) = v;
            seen#v = true;
        );
    );

    head := 0;
    while head < #Q do (
        lambda := Q#head;
        head = head + 1;

        for k from 0 to r-1 do (
            m := 0;
            for i from 0 to r-1 do (
                m = m + lambda#i * A_(k,i)
            );

            mu := for j from 0 to r-1 list (
                if j == k then lambda#j - m else lambda#j
            );

            isPos := true;
            for j from 0 to r-1 do (
                if mu#j < 0 then (
                    isPos = false;
                    break;
                );
            );
            if not isPos then continue;

            if maxCoeff =!= null then (
                okBound := true;
                for j from 0 to r-1 do (
                    if mu#j > maxCoeff then (
                        okBound = false;
                        break;
                    );
                );
                if not okBound then continue;
            );

            if not seen#?mu then (
                seen#mu = true;
                R#(#R) = mu;
                Q#(#Q) = mu;
            );
        );
    );

    matrix toList R
);

-- Positive real roots of total height at most H.  A positive root of
-- height at most H has every simple-root coefficient at most H, so the
-- coefficient-box enumeration is a safe internal implementation detail;
-- the final height filter gives the public height semantics.
cartanPositiveRealRootsByHeight = (A, H) -> (
    A = integralCartanMatrix(A, "cartanPositiveRoots");
    r := numrows A;
    Rbox := cartanPositiveRootsCore(A, H);
    L := select(entries Rbox, v -> sum v <= H);
    if #L == 0 then matrix mutableMatrix(ZZ, 0, r)
    else matrix L
);

-- Mode 1: no bound (finite-type case).  Memoized on A.cache.
cartanPositiveRoots Matrix := A -> (
    A = integralCartanMatrix(A, "cartanPositiveRoots");
    requireFiniteCartanType("cartanPositiveRoots", A);
    if A.cache#?"cartanPositiveRoots" then return A.cache#"cartanPositiveRoots";
    M := cartanPositiveRootsCore(A, null);
    A.cache#"cartanPositiveRoots" = M;
    M
);

-- Mode 2: all positive roots of total height at most H.  Infinite types
-- need their imaginary roots as well as the real Weyl-orbit roots.
cartanPositiveRoots(Matrix, ZZ) := (A, H) -> (
    if H < 1 then error "cartanPositiveRoots: height bound must be at least 1";
    snap := rootSnapshot(A, H);
    Rreal := snap#"real";
    Rimag := snap#"imag";
    if numrows Rreal == 0 then Rimag
    else if numrows Rimag == 0 then Rreal
    else Rreal || Rimag
);


------------------------------------------------------------
-- cartanPositiveRootsDegree :
--   A : r x r Cartan matrix
--   d : ZZ >= 0, desired degree
--   parabolic : list/set of simple roots (1..r) of degree 1
-- Returns: matrix of positive roots with parabolic degree d
------------------------------------------------------------

cartanPositiveRootsDegree = method()
cartanPositiveRootsDegree (Matrix, ZZ, List) := (A, d, parabolic) -> (
    A = integralCartanMatrix(A, "cartanPositiveRootsDegree");
    r := numrows A;
    requireFiniteCartanType("cartanPositiveRootsDegree", A);
    if not instance(d, ZZ) or d < 0 then
        error "cartanPositiveRootsDegree: degree d must be a nonnegative integer";

    P := validateNodeSet(A, parabolic, "cartanPositiveRootsDegree");
    parIdx := apply(P, i -> i-1);

    rootDegree := v -> (
        s := 0;
        for j in parIdx do s = s + v#j;
        s
    );

    R := new MutableList;
    Q := new MutableList;
    seen := new MutableHashTable;

    for i from 0 to r-1 do (
        v := for j from 0 to r-1 list (if j == i then 1 else 0);
        degv := rootDegree v;
        if degv <= d then (
            R#(#R) = v;
            Q#(#Q) = v;
            seen#v = true;
        )
    );

    head := 0;
    while head < #Q do (
        lambda := Q#head;
        head = head + 1;

        for k from 0 to r-1 do (
            m := 0;
            for i from 0 to r-1 do (
                m = m + lambda#i * A_(k,i)
            );

            mu := for j from 0 to r-1 list (
                if j == k then lambda#j - m else lambda#j
            );

            isPos := true;
            for j from 0 to r-1 do (
                if mu#j < 0 then (
                    isPos = false;
                    break;
                )
            );
            if not isPos then continue;

            degmu := rootDegree mu;
            if degmu > d then continue;

            if not seen#?mu then (
                seen#mu = true;
                R#(#R) = mu;
                Q#(#Q) = mu;
            );
        );
    );

    L := {};
    for v in toList R do (
        if rootDegree v == d then L = append(L, v);
    );

    if #L == 0 then (
        matrix mutableMatrix(ZZ, 0, r)
    ) else (
        matrix L
    )
);

cartanPositiveRootsDegree (Matrix, ZZ, Set) := (A, d, parabolic) ->
    cartanPositiveRootsDegree(A, d, toList parabolic);


------------------------------------------------------------
------------------------------------------------------------
--
--         PARABOLIC DECOMPOSITION
--
------------------------------------------------------------
------------------------------------------------------------

------------------------------------------------------------
-- Irrep type
------------------------------------------------------------

-- An Irrep encodes a highest-weight irreducible representation, or a
-- tensor product of such (one factor per simple direct-summand of the
-- acting Lie algebra -- typical for Levi subalgebras whose Dynkin
-- diagram is disconnected).  Internally it is a BasicList whose
-- elements are the per-component weight lists (Dynkin labels or
-- partitions, depending on how it was produced).  A single-component
-- representation V(a,b,c) is stored as (new Irrep from {{a,b,c}}), a
-- tensor product V(a,b) ** V(c) as (new Irrep from {{a,b},{c}}).
--
-- The net method prints it as, e.g.,   V(a,b)   or   V(a,b) ** V(c).

Irrep = new Type of BasicList

-- Structured result types retain ordinary HashTable indexing for backward
-- compatibility while allowing callers and documentation to distinguish
-- mathematically different records reliably.
RootSnapshot = new Type of HashTable
LeviData = new Type of HashTable
WeylElement = new Type of HashTable
GradedDecomposition = new Type of HashTable
LeviIrrep = new Type of HashTable

toString RootSnapshot := S -> toString draftRootSnapshotDisplay S;
net RootSnapshot := S -> draftRootSnapshotDisplay S;

toString LeviData := L -> concatenate(
    "LeviData(nodes=", if L#?"nodes" then toString L#"nodes" else "?",
    ", removed=", if L#?"removedNodes" then toString L#"removedNodes" else "?",
    ", semisimpleRank=", if L#?"semisimpleRank" then toString L#"semisimpleRank" else "?",
    ", centerDimension=", if L#?"centerDimension" then toString L#"centerDimension" else "?",
    ")");

net LeviData := L -> net toString L;

-- Display the element as mathematics, while retaining the complete action
-- matrix and parent record internally.  Parent-aware elements use the
-- datum's public node labels; legacy enumerated elements fall back to their
-- canonical 1-based positions.
toString WeylElement := w -> (
    if not w#?"word" then return "WeylElement(?)";
    word := w#"word";
    if #word == 0 then return "1_W";
    labels := if w#?"parent"
        and (w#"parent")#?"cartanDatum"
        and ((w#"parent")#"cartanDatum")#?"nodeLabels"
        then (
            publicLabels := ((w#"parent")#"cartanDatum")#"nodeLabels";
            apply(word,i ->
                if i >= 1 and i <= #publicLabels then publicLabels#(i-1)
                else i)
        ) else word;
    demark(" ",apply(labels,label -> "s_" | toString label))
);

net WeylElement := w -> net toString w;

toString Irrep := w -> (
    L := toList w;
    if #L == 0 then "V()"
    else (
        pieces := for piece in L list (
            if #piece == 0 then "V()"
            else concatenate("V(", demark(",", apply(piece, toString)), ")")
        );
        demark(" ** ", pieces)
    )
);

net Irrep := w -> net toString w;

-- A LeviIrrep is a self-describing wrapper around the deliberately small,
-- backward-compatible Irrep type.  In particular, structured Kostant output
-- can retain the character of the center of the reductive Levi without
-- changing the default Tally{Irrep => ZZ} interface.
toString LeviIrrep := V -> (
    semisimple := if V#?"semisimple" then V#"semisimple" else null;
    central := if V#?"centralCharacter" then V#"centralCharacter" else null;
    parentLabel := if not V#?"parentCartan"
                   or not instance(V#"parentCartan", Matrix) then "parent=?"
                   else (
                       parent := V#"parentCartan";
                       if numrows parent == 0 then "rank 0"
                       else try (
                           data := cartanData parent;
                           demark(" + ", apply(data#"components",
                               component -> component#"label"))
                       ) else concatenate("rank ",toString numrows parent)
                   );
    centralString := if central === null then "?"
                     else concatenate("(", demark(",", apply(central, toString)), ")");
    concatenate(toString semisimple, " [", parentLabel,
        "; central=", centralString, "]")
);

net LeviIrrep := V -> net toString V;
LeviIrrep == LeviIrrep := (V,W) -> V === W;

------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------

-- leviConnectedComponents(AL)
--   AL : square Cartan (sub)matrix (the Levi Cartan)
-- Returns a list of lists of 0-based column indices; each inner list
-- is one connected component of the Dynkin diagram of AL.

leviConnectedComponents0 = AL -> (
    n := numrows AL;
    visited := new MutableList from for i from 0 to n-1 list false;
    comps := {};
    for i from 0 to n-1 do (
        if visited#i then continue;
        stack := {i};
        comp := {};
        while #stack > 0 do (
            j := last stack;
            stack = drop(stack, -1);
            if visited#j then continue;
            visited#j = true;
            comp = append(comp, j);
            for k from 0 to n-1 do (
                if not visited#k and (AL_(j,k) != 0 or AL_(k,j) != 0) then (
                    stack = append(stack, k);
                );
            );
        );
        comps = append(comps, sort comp);
    );
    comps
);

-- Public node labels follow the same 1-based convention as every other
-- node-facing routine in the package.  Internal matrix indexing uses the
-- explicitly named 0-based helper above.
leviConnectedComponents = method()
leviConnectedComponents Matrix := AL -> (
    if numcols AL =!= numrows AL then
        error "leviConnectedComponents: Cartan matrix must be square";
    if numrows AL == 0 then return {};
    AL = integralCartanMatrix(AL, "leviConnectedComponents");
    apply(leviConnectedComponents0 AL, comp -> apply(comp, i -> i+1))
);

-- classifyConnectedCartan(M) : returns (type, rank, permutation), where
-- permutation lists the original node indices in the package's canonical
-- ordering.  Thus submatrix(M,p,p) is the standard Cartan matrix of the
-- reported finite connected type.  Recognition is graph based: unlike the
-- old permutation search, its running time is polynomial in the rank.
--
-- This helper recognizes A--G (not merely the classical families), so it is
-- also the single internal source of detailed finite-type information for
-- bridges and user-facing Cartan-data summaries.
classifyConnectedCartan = M -> (
    if not instance(M, Matrix) then return ("other", 0, {});
    n := numrows M;
    if numcols M =!= n then return ("other", 0, {});
    if n == 0 then return ("other", 0, {});
    if isCartanMatrix M then M = integralCartanMatrix(M, "classifyConnectedCartan");
    idperm := toList(0..n-1);
    -- A_1 (1x1 matrix [2]) is unambiguously A_1.
    if n == 1 then (
        if M_(0,0) == 2 then return ("A", 1, {0})
        else return ("other", 0, {});
    );
    standards := {
        ("A", cartanA(n+1)),
        ("B", cartanB n),
        ("C", cartanC n)
    };
    if n >= 4 then standards = append(standards, ("D", cartanD n));
    if member(n, {6,7,8}) then standards = append(standards, ("E", cartanE n));
    if n == 4 then standards = append(standards, ("F", cartanF4()));
    if n == 2 then standards = append(standards, ("G", cartanG2()));

    -- Preserve the established convention when M is already canonical.
    for candidate in standards do (
        if M == candidate#1 then return (candidate#0, n, idperm);
    );

    -- Undirected support graph.  Finite Dynkin diagrams are trees; edge
    -- multiplicities and orientations are checked at the end by comparing
    -- the resulting relabeling with the canonical Cartan matrices.
    neighbors := for i from 0 to n-1 list
        select(idperm, j -> j != i and (M_(i,j) != 0 or M_(j,i) != 0));
    degrees := apply(neighbors, L -> #L);
    candidatePerms := {};

    -- Path types A, B, C, F and G have only the two endpoint orderings.
    endpoints := select(idperm, i -> degrees#i == 1);
    if #endpoints == 2 and sum degrees == 2*(n-1)
       and all(degrees, d -> d <= 2) then (
        pathFrom := start -> (
            p := {};
            previous := -1;
            current := start;
            while current =!= null do (
                p = append(p, current);
                nextNodes := select(neighbors#current, j -> j != previous);
                if #nextNodes == 0 then current = null
                else (
                    previousNew := current;
                    current = first nextNodes;
                    previous = previousNew;
                );
            );
            p
        );
        p := pathFrom first endpoints;
        if #p == n then candidatePerms = candidatePerms | {p, reverse p};
    );

    -- Simply-laced fork types D and E.  Record each arm from the branch
    -- outward.  Its length profile distinguishes D_n and E_6,E_7,E_8.
    branches := select(idperm, i -> degrees#i == 3);
    if #branches == 1 and sum degrees == 2*(n-1)
       and all(degrees, d -> d <= 3) then (
        branch := first branches;
        armFrom := start -> (
            arm := {start};
            previous := branch;
            current := start;
            while degrees#current == 2 do (
                nextNodes := select(neighbors#current, j -> j != previous);
                if #nextNodes == 0 then break;
                previousNew := current;
                current = first nextNodes;
                previous = previousNew;
                arm = append(arm, current);
            );
            arm
        );
        arms := apply(neighbors#branch, armFrom);
        orderedArms := sort apply(arms, arm -> {#arm, arm});
        armLengths := apply(orderedArms, pair -> pair#0);

        if armLengths == {1,1,n-3} then (
            dLongArm := (orderedArms#2)#1;
            shortArm1 := (orderedArms#0)#1;
            shortArm2 := (orderedArms#1)#1;
            candidatePerms = append(candidatePerms,
                reverse dLongArm | {branch, first shortArm1, first shortArm2});
        );

        expectedE := if n == 6 then {1,2,2}
                     else if n == 7 then {1,2,3}
                     else if n == 8 then {1,2,4}
                     else {};
        if armLengths == expectedE then (
            shortArm := (orderedArms#0)#1;
            leftArm := (orderedArms#1)#1;
            eLongArm := (orderedArms#2)#1;
            pE := new MutableList from toList(n:0);
            pE#0 = leftArm#1;
            pE#1 = first shortArm;
            pE#2 = first leftArm;
            pE#3 = branch;
            for j from 0 to #eLongArm-1 do pE#(4+j) = eLongArm#j;
            candidatePerms = append(candidatePerms, toList pE);
        );
    );

    -- A final exact comparison certifies the family, bond multiplicities,
    -- arrow directions and canonical relabeling.
    for p in candidatePerms do (
        Mp := submatrix(M, p, p);
        for candidate in standards do (
            if Mp == candidate#1 then return (candidate#0, n, p);
        );
    );
    ("other", 0, {})
);

canonicalizeClassicalLabels = (typeInfo, labels) -> (
    p := typeInfo#2;
    if #p =!= #labels then error "classical label permutation has the wrong length";
    for i in p list labels#i
);

decanonicalizeClassicalLabels = (typeInfo, labels) -> (
    p := typeInfo#2;
    if #p =!= #labels then error "classical label permutation has the wrong length";
    result := new MutableList from toList(#labels:0);
    for i from 0 to #p-1 do result#(p#i) = labels#i;
    toList result
);

-- formatLeviWeight(AL, comps, leviDynkin, fmt)
--   AL         : Levi Cartan matrix
--   comps      : output of leviConnectedComponents AL
--   leviDynkin : list of length numrows AL, Dynkin labels in the Levi
--   fmt        : "Dynkin" or "Partition"
-- Returns either a flat list (single component) or a list-of-lists
-- (multiple components), each entry formatted per fmt.
formatLeviWeight = (AL, comps, leviDynkin, fmt) -> (
    formatComp := idx -> (
        labels := for i in idx list leviDynkin#i;
        Mcomp := submatrix(AL, idx, idx);
        if fmt === "Dynkin" then labels
        else if fmt === "Partition" then (
            tnp := classifyConnectedCartan Mcomp;
            t := tnp#0;
            canonicalLabels := if t === "other" then labels
                               else canonicalizeClassicalLabels(tnp, labels);
            if      t === "A" then fundamentalToPartitionA canonicalLabels
            else if t === "B" then fundamentalToPartitionB canonicalLabels
            else if t === "C" then fundamentalToPartitionC canonicalLabels
            else if t === "D" then fundamentalToPartitionD canonicalLabels
            else error concatenate(
                "WeightFormat => \"Partition\" requires every Levi ",
                "component to be of classical type A, B, C, or D; ",
                "got a non-classical component of size ", toString(#idx))
        )
        else error concatenate("unknown WeightFormat: \"", toString fmt, "\"")
    );
    new Irrep from for idx in comps list formatComp idx
);

-- Degree zero, restricted to the derived Levi.  The ambient Cartan
-- subalgebra has dimension 2r-rank(A) in the minimal realization; after
-- the Cartan directions inside [l,l] are included in the component
-- adjoints, the remaining directions are trivial derived-Levi modules.
degreeZeroDerivedLeviTally = (A, AL, comps, fmt) -> (
    byIrrep := new MutableHashTable;
    addPiece := (irr, m) -> (
        if m != 0 then (
            if byIrrep#?irr then byIrrep#irr = byIrrep#irr + m
            else byIrrep#irr = m;
        );
    );

    for comp in comps do (
        Mcomp := submatrix(AL, comp, comp);
        theta := highestRoot Mcomp;
        thetaDynkin := normalizeIntegralDynkinWeight(
            Mcomp, toFundamental(Mcomp, theta),
            "degreeZeroDerivedLeviTally", true);
        fullWeight := new MutableList from toList(numrows AL:0);
        for j from 0 to #comp-1 do fullWeight#(comp#j) = thetaDynkin#j;
        addPiece(formatLeviWeight(AL, comps, toList fullWeight, fmt), 1);
    );

    rAmbient := numrows A;
    dimH := 2*rAmbient - rank(A ** QQ);
    centerDim := dimH - numrows AL;
    if centerDim < 0 then
        error "degreeZeroDerivedLeviTally: inconsistent Cartan realization dimensions";
    if centerDim > 0 then
        addPiece(formatLeviWeight(AL, comps, toList(numrows AL:0), fmt), centerDim);

    new Tally from for irr in keys byIrrep list irr => byIrrep#irr
);

-- decomposeLeviWeightList(AL, weightList)
--   AL         : Levi Cartan matrix
--   weightList : list of Dynkin-label vectors (the weights, with
--                repetition, of some finite-dimensional Levi rep V)
-- Returns a list of pairs {mu, n_mu} where V = sum n_mu * V_L(mu).
decomposeLeviWeightList = (AL, weightList) -> (
    if #weightList == 0 then return {};
    if numrows AL == 0 then (
        if not all(weightList, w -> #w == 0) then
            error "decomposeLeviWeightList: nonempty weights for a rank-zero Levi";
        return {{{}, #weightList}};
    );

    -- count[mu] = multiplicity of mu in V, for mu dominant
    counts := new MutableHashTable;
    for w in weightList do (
        if all(w, c -> c >= 0) then (
            if counts#?w then counts#w = counts#w + 1
            else counts#w = 1;
        );
    );

    doms := keys counts;
    -- Simple-root height is a genuine linear extension of dominance:
    -- if mu-nu is a nonzero nonnegative combination of simple roots,
    -- then sum(toSimple(mu)) > sum(toSimple(nu)).  The sum of Dynkin
    -- labels does not have this property in nonsimply-laced type.
    weightOrder := w -> sum toSimple(AL, w);
    sortedDoms := reverse sort (doms / (w -> {weightOrder w, w})) / last;

    mults := {};
    for mu in sortedDoms do (
        m := if counts#?mu then counts#mu else 0;
        if m < 0 then
            error concatenate(
                "decomposeLeviWeightList: negative residual multiplicity at ",
                toString mu, "; input is not a finite-dimensional Levi character");
        if m > 0 then (
            mults = append(mults, {mu, m});
            counts#mu = 0;
            -- Subtract m copies of V_L(mu)'s dominant-weight multiplicities
            domWeightsMu := dominantWeightsInRep(AL, mu);
            for nu in domWeightsMu do (
                if nu === mu then continue;
                wm := weightMultiplicity(AL, mu, nu);
                if wm == 0 then continue;
                if not counts#?nu then
                    error concatenate(
                        "decomposeLeviWeightList: missing dominant weight ",
                        toString nu, " required below highest weight ", toString mu);
                counts#nu = counts#nu - m * wm;
            );
        );
    );

    residual := select(keys counts, w -> counts#w != 0);
    if #residual > 0 then
        error concatenate(
            "decomposeLeviWeightList: nonzero residual dominant weights ",
            toString apply(residual, w -> {w, counts#w}),
            "; input is not a finite-dimensional Levi character");

    mults
);


-- cartanLeviHighestWeights :
--   A         : Cartan matrix (r x r)
--   d         : ZZ >= 0, degree in the grading
--   parabolic : list/set of simple roots (1..r) with degree 1
--   WeightFormat => "Dynkin" (default) or "Partition"
--
-- Returns a Tally whose keys are the Levi-highest weights in g_d
-- (formatted according to WeightFormat) and whose values are the
-- multiplicities with which the corresponding irreducible Levi
-- representation V_L(mu) appears in g_d.

cartanLeviHighestWeights = method(Options => {WeightFormat => "Dynkin"})
cartanLeviHighestWeights (Matrix, ZZ, List) := opts -> (A, d, parabolic) -> (
    A = integralCartanMatrix(A, "cartanLeviHighestWeights");
    r := validateFiniteCartanForRepresentations(A, "cartanLeviHighestWeights");
    if d < 0 then error "cartanLeviHighestWeights: degree must be nonnegative";
    parabolic = validateNodeSet(A, parabolic, "cartanLeviHighestWeights");

    fmt := opts.WeightFormat;
    if fmt =!= "Dynkin" and fmt =!= "Partition" then
        error "cartanLeviHighestWeights: WeightFormat must be \"Dynkin\" or \"Partition\"";

    P := set parabolic;
    Levi := {};
    for i from 1 to r do (
        if not member(i, P) then Levi = append(Levi, i);
    );
    Levi0 := for i in Levi list i-1;
    AL := submatrix(A, Levi0, Levi0);
    comps := leviConnectedComponents0 AL;

    -- The zero piece is the derived Levi algebra together with its
    -- center.  As a module for the derived Levi, this is one adjoint
    -- representation per simple component plus dim(z(l)) trivial copies.
    if d == 0 then return degreeZeroDerivedLeviTally(A, AL, comps, fmt);

    Rdeg := cartanPositiveRootsDegree(A, d, parabolic);
    if numrows Rdeg == 0 then return tally {};

    -- Compute the Levi Dynkin label of each root in g_d,
    -- collecting them as a list (with repetition, since roots
    -- with distinct alpha-basis coords can share a Levi label).
    Wall := for row from 0 to numrows Rdeg - 1 list (
        lambda := for i from 0 to r-1 list Rdeg_(row, i);
        for j in Levi0 list (
            s := 0;
            for i from 0 to r-1 do s = s + A_(j, i) * lambda#i;
            s
        )
    );

    -- Decompose into V_L(mu) with multiplicities.
    mults := decomposeLeviWeightList(AL, Wall);

    -- Build the output Tally, formatting each mu per the requested basis.
    pairs := for pair in mults list (
        mu := pair#0;
        m  := pair#1;
        formatted := formatLeviWeight(AL, comps, mu, fmt);
        formatted => m
    );
    new Tally from pairs
);

cartanLeviHighestWeights (Matrix, ZZ, Set) := opts -> (A, d, parabolic) ->
    cartanLeviHighestWeights(A, d, toList parabolic,
        WeightFormat => opts.WeightFormat);


------------------------------------------------------------
------------------------------------------------------------
--
--         PARABOLIC COSET REPRESENTATIVES
--
------------------------------------------------------------
------------------------------------------------------------

-- Default safety ceiling for APIs whose stated output is an explicit list of
-- Weyl-group or parabolic-quotient elements.  Closed-form invariants never
-- use this limit.  An advanced caller may deliberately raise MaxElements or
-- set it to null after deciding that the corresponding memory cost is safe.
weylEnumerationLimit = 10000;

validateMaxElements = (who, maxElements) -> (
    if maxElements =!= null and
       (not instance(maxElements, ZZ) or maxElements < 1) then
        error(who | ": MaxElements must be a positive integer or null");
    maxElements
);

-- Return |W/W_I| when the standard parabolic quotient is finite, and null
-- when it is infinite.  The Weyl group is a product over the connected
-- components of A.  A proper standard parabolic subgroup of an infinite
-- irreducible Coxeter group has infinite index, while finite components can
-- be handled by their exact Weyl-group orders.
parabolicQuotientOrder = (A, I) -> (
    A = integralCartanMatrix(A, "parabolicCosetReps");
    I = validateNodeSet(A, I, "parabolicCosetReps");
    Iset := set I;
    answer := 1;
    for comp in leviConnectedComponents0 A do (
        C := submatrix(A, comp, comp);
        localI := select(toList(0..#comp-1),
            j -> member(comp#j + 1, Iset));
        if cartanType C === "finite" then (
            componentOrder := weylGroupOrder C;
            leviOrder := if #localI == 0 then 1
                else weylGroupOrder submatrix(C, localI, localI);
            answer = answer * (componentOrder // leviOrder);
        ) else if #localI < #comp then return null;
    );
    answer
);

-- Enumerate the minimal representatives of W/W_I by length.  The set W^I
-- is a lower ideal for left weak order, so we may grow it directly by left
-- ascents and discard nonminimal products immediately.  Each level is
-- computed once; this is substantially smaller than enumerating all of W
-- and is especially important when the Levi Weyl group is large.
parabolicCosetRepsByLengthInternal = (
    A, maxLen, I, maxElements, precomputedQuotientOrder) -> (
    A = integralCartanMatrix(A, "parabolicCosetReps");
    if maxLen =!= null and maxLen < 0 then
        error "parabolicCosetReps: length must be nonnegative";
    I = validateNodeSet(A, I, "parabolicCosetReps");
    maxElements = validateMaxElements("parabolicCosetReps", maxElements);
    R := ring A;
    n := numrows A;
    I0 := apply(I, i -> i-1);
    quotientOrder := if maxLen === null then (
        if precomputedQuotientOrder === false then
            parabolicQuotientOrder(A, I)
        else precomputedQuotientOrder
    ) else null;
    if maxLen === null then (
        if quotientOrder === null then error concatenate(
            "parabolicCosetReps: W/W_I is infinite; a finite length bound is required");
        if maxElements =!= null and quotientOrder > maxElements then
            error concatenate(
                "parabolicCosetReps: quotient has ", toString quotientOrder,
                " elements, exceeding MaxElements => ", toString maxElements);
    );
    -- Simple-reflection matrices have only O(n) nonzero entries.  The
    -- Macaulay2 sparse matrix engine exploits that structure, and is
    -- measurably faster here than spelling out equivalent interpreted
    -- row/column updates.
    Slist := apply(n, i -> reflectionMatrix(A, i+1));
    isPositiveColumn := (M, i) -> (
        coeffs := for j from 0 to n-1 list M_(j,i);
        isPositiveVector coeffs
    );
    identityM := id_(R^n);
    keyOf := M -> flatten entries M;
    seen := new MutableHashTable;
    seen#(keyOf identityM) = true;
    seenCount := 1;

    levels := new MutableList;
    levels#0 = {new WeylElement from {
        "matrix" => identityM,
        "inverse" => identityM,
        "word" => {}
    }};
    ell := 0;
    while ((maxLen === null or ell < maxLen) and #(levels#ell) > 0
           and (quotientOrder === null or seenCount < quotientOrder)) do (
        next := new MutableList;
        nextCount := 0;
        for elt in levels#ell do (
            M := elt#"matrix";
            Minv := elt#"inverse";
            word := elt#"word";
            for i from 0 to n-1 do (
                -- l(s_i w)>l(w) iff w^{-1}(alpha_i)>0.
                if isPositiveColumn(Minv, i) then (
                    M2 := Slist#i * M;
                    key := keyOf M2;
                    if not seen#?key and all(I0, j -> isPositiveColumn(M2,j)) then (
                        Minv2 := Minv * Slist#i;
                        seen#key = true;
                        seenCount = seenCount + 1;
                        if maxElements =!= null and seenCount > maxElements then error concatenate(
                            "parabolicCosetReps: computation would materialize more than ",
                            toString maxElements, " quotient elements; increase MaxElements only if that cost is intentional");
                        next#nextCount = new WeylElement from {
                            "matrix" => M2,
                            "inverse" => Minv2,
                            "word" => {i+1} | word
                        };
                        nextCount = nextCount + 1;
                    );
                );
            );
        );
        ell = ell + 1;
        levels#ell = toList next;
    );
    if maxLen === null and seenCount != quotientOrder then error concatenate(
        "parabolicCosetReps: internal traversal error: expected ",
        toString quotientOrder, " quotient elements but found ",
        toString seenCount);
    levels
);

-- Backward-compatible internal entry point.  Kostant cohomology can pass a
-- quotient order it has already certified, while other callers retain the
-- original four-argument interface.
parabolicCosetRepsByLength = (A, maxLen, I, maxElements) ->
    parabolicCosetRepsByLengthInternal(A, maxLen, I, maxElements, false);

parabolicCosetReps = method(Options => {MaxElements => weylEnumerationLimit})

parabolicCosetReps(Matrix, ZZ, List) := opts -> (A, k, I) -> (
    A = integralCartanMatrix(A, "parabolicCosetReps");
    if k < 0 then error "parabolicCosetReps: length k must be nonnegative";
    I = validateNodeSet(A, I, "parabolicCosetReps");
    levels := parabolicCosetRepsByLength(A, k, I, opts.MaxElements);
    if k >= #levels then return {};
    apply(levels#k, elt -> elt#"word")
)

parabolicCosetReps(Matrix, ZZ, Set) := opts -> (A, k, I) ->
    parabolicCosetReps(A, k, toList I,
        MaxElements => opts.MaxElements);


-- dotActionOnWeight(A, word, lambdaList)
-- A          : Cartan matrix
-- word       : list of simple indices, e.g. {3,2,3}
-- lambdaList : list in Dynkin coordinates (fundamental weights)
-- returns: list giving w . lambda (dot action)

dotActionOnWeight = (A, word, lambdaList) -> (
    A = integralCartanMatrix(A, "dotActionOnWeight");
    n := numrows A;
    word = validateWeylWord(A, word, "dotActionOnWeight");
    lam := normalizeIntegralDynkinWeight(A, lambdaList,
        "dotActionOnWeight", false);

    if #word == 0 then return lam;

    dotSimple := (lamR, sIndex) -> (
        colIdx := sIndex - 1;
        lam2 := new MutableList from lamR;
        for rowIdx from 0 to n-1 do (
            lam2#rowIdx = lamR#rowIdx - (lamR#colIdx + 1) * A_(rowIdx, colIdx);
        );
        toList lam2
    );

    m := #word;
    lamCurrent := lam;
    for t from 0 to m-1 do (
        pos := m-1 - t;
        lamCurrent = dotSimple(lamCurrent, word#pos);
    );
    lamCurrent
);

parabolicDotImages = method(Options => {MaxElements => weylEnumerationLimit})
parabolicDotImages (Matrix, ZZ, List, List) := opts -> (A, k, I, lambdaList) -> (
    A = integralCartanMatrix(A, "parabolicDotImages");
    if k < 0 then error "parabolicDotImages: length k must be nonnegative";
    I = validateNodeSet(A, I, "parabolicDotImages");
    -- Reject a malformed weight before potentially traversing a large
    -- quotient stratum.  dotActionOnWeight validates as well, but doing it
    -- here makes this wrapper fail fast even when the requested stratum is
    -- empty or exceeds MaxElements.
    lambdaList = normalizeIntegralDynkinWeight(A, lambdaList,
        "parabolicDotImages", false);
    maxElements := validateMaxElements("parabolicDotImages", opts.MaxElements);
    words := parabolicCosetReps(A, k, I, MaxElements => maxElements);
    apply(words, w -> {reverse w, dotActionOnWeight(A, reverse w, lambdaList)})
);

parabolicDotImages (Matrix, ZZ, Set, List) := opts -> (A, k, I, lambdaList) ->
    parabolicDotImages(A, k, toList I, lambdaList,
        MaxElements => opts.MaxElements);


------------------------------------------------------------
------------------------------------------------------------
--
--         WEYL GROUP
--
------------------------------------------------------------
------------------------------------------------------------

-- Convert a Cartan-entry product to the corresponding Coxeter exponent.
coxeterExponentFromProduct = p -> (
    if p == 0 then 2
    else if p == 1 then 3
    else if p == 2 then 4
    else if p == 3 then 6
    else infinity
);

-- The full Coxeter matrix is returned as a nested list because a genuine
-- matrix over ZZ cannot contain the built-in value infinity.  The three-
-- argument form uses the package-wide 1-based node convention.
coxeterMatrix = method()
coxeterMatrix Matrix := A -> (
    A = integralCartanMatrix(A, "coxeterMatrix");
    cacheName := "KacMoody.coxeterMatrix";
    if A.cache#?cacheName then return A.cache#cacheName;
    r := numrows A;
    result := for i from 0 to r-1 list for j from 0 to r-1 list
        if i == j then 1
        else coxeterExponentFromProduct(A_(i,j)*A_(j,i));
    A.cache#cacheName = result;
    result
);

coxeterMatrix(Matrix, ZZ, ZZ) := (A, i, j) -> (
    A = integralCartanMatrix(A, "coxeterMatrix");
    r := numrows A;
    if i < 1 or i > r or j < 1 or j > r then
        error "coxeterMatrix: node indices must be between 1 and rank";
    if i == j then 1
    else coxeterExponentFromProduct(A_(i-1,j-1)*A_(j-1,i-1))
);

-- Simple reflection matrix in the simple-root basis
reflectionMatrix = method()
reflectionMatrix (Matrix, ZZ) := (A, i) -> (
    A = integralCartanMatrix(A, "reflectionMatrix");
    n := numrows A;
    if i < 1 or i > n then error "reflectionMatrix: simple-root index must be between 1 and rank";
    R := ring A;
    i0 := i - 1;
    Smut := mutableMatrix id_(R^n);
    for j from 0 to n-1 do (
        if j == i0 then (
            Smut_(i0, j) = -1_R;
        ) else (
            Smut_(i0, j) = - A_(i0, j);
        );
    );
    matrix Smut
);

-- weylElementMatrix(A, w)
-- Matrix representing the Weyl group element whose reduced word is w,
-- acting on the simple-root basis by column conventions:
--    (weylElementMatrix(A,w)) * e_i = w(alpha_i)
-- where e_i is the i-th simple root column.  Here w is a list of 1-based
-- simple-reflection indices.

weylElementMatrix = method()
weylElementMatrix (Matrix, List) := (A, w) -> (
    A = integralCartanMatrix(A, "weylElementMatrix");
    w = validateWeylWord(A, w, "weylElementMatrix");
    n := numrows A;
    R := ring A;
    M := id_(R^n);
    for s in w do M = M * reflectionMatrix(A, s);
    M
);


-- weylInversionData(A, w)
-- Tracks length and the inversion set of the initial prefixes of w.
-- Returns a HashTable with keys "length", "inversions", "word".

weylInversionData = method()
weylInversionData (Matrix, List) := (A, w) -> (
    A = integralCartanMatrix(A, "weylInversionData");
    w = validateWeylWord(A, w, "weylInversionData");
    R := ring A;
    n := numrows A;

    Slist := apply(n, i -> reflectionMatrix(A, i+1));

    M := id_(R^n);
    len := 0;
    invs := {};

    for t from 0 to #w-1 do (
        sIndex := w#t - 1;
        alpha := for row from 0 to n-1 list M_(row, sIndex);

        if isPositiveVector alpha then (
            len = len + 1;
            invs = append(invs, alpha);
        ) else if isNegativeVector alpha then (
            len = len - 1;
            betaPos := for j from 0 to n-1 list (- alpha#j);
            invs = select(invs, beta -> beta =!= betaPos);
        ) else (
            error "weylInversionData: encountered a root that is neither positive nor negative"
        );

        M = M * Slist#sIndex;
    );

    new HashTable from {
        "word"       => w,
        "length"     => len,
        "inversions" => invs
    }
);

-- Coxeter length of a word (Weyl length of the element it represents).

weylLength = method()
weylLength (Matrix, List) := (A, w) -> (weylInversionData(A, w))#"length";

-- Strong Bruhat order, tested by the reduced-subword criterion.

bruhatLessOrEqual = method()
bruhatLessOrEqual (Matrix, List, List) := (A, u, v) -> (
    A = integralCartanMatrix(A, "bruhatLessOrEqual");
    u = validateWeylWord(A, u, "bruhatLessOrEqual");
    v = validateWeylWord(A, v, "bruhatLessOrEqual");
    n := numrows A;
    vRed := reducedWord(A, v);

    -- Strong Bruhat subword criterion, implemented through the lifting
    -- property.  Scan a reduced word for v from right to left; whenever
    -- its next reflection is a right descent of the current u, strip that
    -- descent.  A reduced expression for u occurs as a subword exactly
    -- when this greedy scan strips u to the identity.
    R := ring A;
    M := weylElementMatrix(A, u);
    Slist := apply(n, i -> reflectionMatrix(A, i+1));
    positions := if #vRed == 0 then {} else reverse toList(0..#vRed-1);
    for pos in positions do (
        s := vRed#pos;
        col := for j from 0 to n-1 list M_(j,s-1);
        if isNegativeVector col then M = M * Slist#(s-1);
    );
    M == id_(R^n)
);


------------------------------------------------------------
------------------------------------------------------------
--
--         DISTINGUISHED CARTAN MATRICES FOR PARABOLIC SUBALGEBRAS
--
------------------------------------------------------------
------------------------------------------------------------

trimTrailingZeroes = v -> (
    w := v;
    while #w > 0 and last w == 0 do w = drop(w, -1);
    w
);

normalizeClassicalPartitionCoordinates = (lam, n, who) -> (
    if not instance(lam, List) then error(who | ": partition must be a list");
    lam = trimTrailingZeroes lam;
    if #lam > n then error(who | ": partition has more parts than the rank");
    for x in lam list (
        try (x + 0_QQ) else error(who | ": partition entries must be rational numbers")
    )
);

-- General epsilon-coordinate conversion in type A.  Differences must be
-- integral, but the vector need not be dominant; this is used for arbitrary
-- weights in weightMultiplicity and weightOrbit.
partitionToFundamentalAUncheckedWithName = (rankA, lambdaPart, who) -> (
    n := rankA;
    if not instance(n, ZZ) or n < 1 then
        error(who | ": rank must be a positive integer");
    if not instance(lambdaPart, List) then
        error(who | ": partition must be a list");
    lambdaPart = trimTrailingZeroes lambdaPart;
    if #lambdaPart > n+1 then
        error(who | ": partition has more than rank+1 nonzero parts");
    lamPad := for i from 0 to n list (
        if i < #lambdaPart then lambdaPart#i else 0
    );
    for i from 0 to n-1 list (
        x := lamPad#i - lamPad#(i+1);
        xQ := try (x + 0_QQ) else
            error(who | ": entries must be rational numbers");
        if denominator xQ =!= 1 then
            error concatenate(
                who, ": adjacent difference ", toString x,
                " is not integral, so the vector is not an A-type weight");
        numerator xQ
    )
);

partitionToFundamentalAUnchecked = (rankA, lambdaPart) ->
    partitionToFundamentalAUncheckedWithName(
        rankA, lambdaPart, "partitionToFundamentalA");

partitionToFundamentalAWithName = (rankA, lambdaPart, who) -> (
    n := rankA;
    if not instance(n, ZZ) or n < 1 then
        error(who | ": rank must be a positive integer");
    if not instance(lambdaPart, List) then
        error(who | ": partition must be a list");
    lambdaPart = trimTrailingZeroes lambdaPart;
    if #lambdaPart > n+1 then
        error(who | ": partition has more than rank+1 nonzero parts");
    for x in lambdaPart do (
        xQ := try (x + 0_QQ) else
            error(who | ": partition entries must be integers");
        if denominator xQ =!= 1 or xQ < 0 then
            error(who | ": a partition must have nonnegative integer entries");
    );
    for i from 0 to #lambdaPart-2 do (
        if lambdaPart#i < lambdaPart#(i+1) then
            error(who | ": entries must be weakly decreasing");
    );
    partitionToFundamentalAUncheckedWithName(n, lambdaPart, who)
);

partitionToFundamentalA = (rankA, lambdaPart) ->
    partitionToFundamentalAWithName(
        rankA, lambdaPart, "partitionToFundamentalA");

fundamentalToPartitionA = (mList) -> (
    n := #mList;
    lamMut := new MutableList from for i from 0 to n list 0;
    lamMut#n = 0;

    runningSum := 0;
    scan(reverse toList(0..n-1), i -> (
        runningSum = runningSum + mList#i;
        lamMut#i = runningSum;
    ));

    toList lamMut
);

------------------------------------------------------------
-- Types B, C, D : Dynkin labels <-> partition form.
--
-- In the standard epsilon-basis a dominant integral weight of a
-- classical simple Lie algebra is a vector (lambda_1, ..., lambda_n)
-- with lambda_1 >= lambda_2 >= ... >= lambda_n >= 0 in type B,C,
-- and lambda_1 >= ... >= lambda_{n-1} >= |lambda_n| in type D.
-- The package's Dynkin labels (m_1, ..., m_n) are related to that
-- partition as follows, using the orderings fixed by cartanB,
-- cartanC, cartanD:
--
--   Type B_n (last node short):
--     m_i = lambda_i - lambda_{i+1}  for i = 1..n-1
--     m_n = 2 * lambda_n
--   Type C_n (last node long):
--     m_i = lambda_i - lambda_{i+1}  for i = 1..n-1
--     m_n = lambda_n
--   Type D_n (fork at positions n-1, n):
--     m_i        = lambda_i - lambda_{i+1}    for i = 1..n-2
--     m_{n-1}    = lambda_{n-1} - lambda_n
--     m_n        = lambda_{n-1} + lambda_n
--
-- For types B and D the weight lattice is strictly larger than the
-- root lattice: the parities of m_n (type B) or m_{n-1}+m_n (type D)
-- determine whether the partition has integer or half-integer entries
-- (a "spin" weight).  The converters below return rational entries in
-- the half-integer case and integer entries otherwise.
-- In type D the last entry lambda_n can be negative (second spinor).
------------------------------------------------------------

-- partitionToFundamentalB(rankB, lambda)
-- Convert a type-B partition (lambda_1, ..., lambda_n), all-integer or
-- all-half-integer, to Dynkin labels (m_1, ..., m_n).
partitionToFundamentalBUncheckedWithName = (rankB, lam, who) -> (
    n := rankB;
    if not instance(n, ZZ) or n < 2 then
        error(who | ": rank must be an integer at least 2");
    lam = normalizeClassicalPartitionCoordinates(lam, n, who);
    lamP := for i from 0 to n-1 list (if i < #lam then lam#i else 0);
    result := for i from 0 to n-2 list lamP#i - lamP#(i+1);
    result = append(result, 2 * lamP#(n-1));
    for x in result list (
        xQ := x + 0_QQ;
        if denominator xQ =!= 1 then
            error concatenate(
                who, ": Dynkin label ", toString x,
                " is not an integer; a B-weight must be all-integer or all-half-integer");
        numerator xQ
    )
);

partitionToFundamentalBUnchecked = (rankB, lam) ->
    partitionToFundamentalBUncheckedWithName(
        rankB, lam, "partitionToFundamentalB");

partitionToFundamentalBWithName = (rankB, lam, who) -> (
    result := partitionToFundamentalBUncheckedWithName(rankB, lam, who);
    if any(result, x -> x < 0) then
        error(who | ": entries must be weakly decreasing and nonnegative");
    result
);

-- fundamentalToPartitionB mList
-- Inverse of partitionToFundamentalB.  Returns a list of integers if
-- all entries are integers, else a list of rationals (half-integer
-- spin weights).
fundamentalToPartitionB = (mList) -> (
    n := #mList;
    if n < 2 then error "fundamentalToPartitionB: Dynkin vector must have length at least 2";
    lamQ := new MutableList from for i from 0 to n-1 list 0_QQ;
    lamQ#(n-1) = mList#(n-1) / 2_QQ;
    scan(reverse toList(0..n-2), i -> lamQ#i = mList#i + lamQ#(i+1));
    result := toList lamQ;
    if all(result, x -> denominator x === 1) then
        for x in result list numerator x
    else result
);

-- partitionToFundamentalC(rankC, lambda)
-- Convert a type-C integer partition (lambda_1, ..., lambda_n) to
-- Dynkin labels (m_1, ..., m_n).
partitionToFundamentalCUncheckedWithName = (rankC, lam, who) -> (
    n := rankC;
    if not instance(n, ZZ) or n < 2 then
        error(who | ": rank must be an integer at least 2");
    lam = normalizeClassicalPartitionCoordinates(lam, n, who);
    lamP := for i from 0 to n-1 list (if i < #lam then lam#i else 0);
    for x in lamP do (
        xQ := x + 0_QQ;
        if denominator xQ =!= 1 then
            error concatenate(
                who, ": entry ", toString x,
                " is not an integer; C_n dominant weights are integer partitions");
    );
    result := for i from 0 to n-2 list lamP#i - lamP#(i+1);
    result = append(result, lamP#(n-1));
    for x in result list numerator(x + 0_QQ)
);

partitionToFundamentalCUnchecked = (rankC, lam) ->
    partitionToFundamentalCUncheckedWithName(
        rankC, lam, "partitionToFundamentalC");

partitionToFundamentalCWithName = (rankC, lam, who) -> (
    result := partitionToFundamentalCUncheckedWithName(rankC, lam, who);
    if any(result, x -> x < 0) then
        error(who | ": entries must be weakly decreasing and nonnegative");
    result
);

-- fundamentalToPartitionC mList
-- Inverse of partitionToFundamentalC.  Always returns an integer list.
fundamentalToPartitionC = (mList) -> (
    n := #mList;
    if n < 2 then error "fundamentalToPartitionC: Dynkin vector must have length at least 2";
    lamMut := new MutableList from for i from 0 to n-1 list 0;
    lamMut#(n-1) = mList#(n-1);
    scan(reverse toList(0..n-2), i -> lamMut#i = mList#i + lamMut#(i+1));
    toList lamMut
);

-- partitionToFundamentalD(rankD, lambda)
-- Convert a type-D partition-form weight (lambda_1, ..., lambda_n),
-- all-integer or all-half-integer, satisfying
-- lambda_1 >= ... >= lambda_{n-1} >= |lambda_n|, to Dynkin labels.
partitionToFundamentalDUncheckedWithName = (rankD, lam, who) -> (
    n := rankD;
    if not instance(n, ZZ) or n < 4 then
        error(who | ": rank must be an integer at least 4");
    lam = normalizeClassicalPartitionCoordinates(lam, n, who);
    lamP := for i from 0 to n-1 list (if i < #lam then lam#i else 0);
    mHead := for i from 0 to n-3 list lamP#i - lamP#(i+1);
    mNm1 := lamP#(n-2) - lamP#(n-1);
    mN   := lamP#(n-2) + lamP#(n-1);
    result := append(append(mHead, mNm1), mN);
    for x in result list (
        xQ := x + 0_QQ;
        if denominator xQ =!= 1 then
            error concatenate(
                who, ": Dynkin label ", toString x,
                " is not an integer; a D-weight must be all-integer or all-half-integer");
        numerator xQ
    )
);

partitionToFundamentalDUnchecked = (rankD, lam) ->
    partitionToFundamentalDUncheckedWithName(
        rankD, lam, "partitionToFundamentalD");

partitionToFundamentalDWithName = (rankD, lam, who) -> (
    result := partitionToFundamentalDUncheckedWithName(rankD, lam, who);
    if any(result, x -> x < 0) then
        error(who | ": entries must satisfy lambda_1 >= ... >= lambda_(n-1) >= |lambda_n|");
    result
);

-- fundamentalToPartitionD mList
-- Inverse of partitionToFundamentalD.  The last entry lambda_n may be
-- negative (second half-spin).  Returns integers when possible, else
-- rationals.
fundamentalToPartitionD = (mList) -> (
    n := #mList;
    if n < 4 then error "fundamentalToPartitionD: Dynkin vector must have length at least 4";
    lamQ := new MutableList from for i from 0 to n-1 list 0_QQ;
    mNm1 := mList#(n-2) + 0_QQ;
    mN   := mList#(n-1) + 0_QQ;
    lamQ#(n-2) = (mNm1 + mN) / 2;
    lamQ#(n-1) = (mN - mNm1) / 2;
    scan(reverse toList(0..n-3), i -> lamQ#i = mList#i + lamQ#(i+1));
    result := toList lamQ;
    if all(result, x -> denominator x === 1) then
        for x in result list numerator x
    else result
);

-- fundamentalToPartition(A, mList) : dispatch on the detected classical
-- type of A.  partitionToFundamental(A, lam) is the inverse.
fundamentalToPartition = method()
fundamentalToPartition (Matrix, List) := (A, mList) -> (
    A = integralCartanMatrix(A, "fundamentalToPartition");
    n := numrows A;
    if #mList =!= n then
        error "fundamentalToPartition: Dynkin vector length must equal numrows A";
    for x in mList do (
        xQ := try (x + 0_QQ) else
            error "fundamentalToPartition: Dynkin labels must be integers";
        if denominator xQ =!= 1 then
            error "fundamentalToPartition: Dynkin labels must be integers";
    );
    tnp := classifyConnectedCartan A;
    t := tnp#0;
    canonicalLabels := if t === "other" then mList
                       else canonicalizeClassicalLabels(tnp, mList);
    if      t === "A" then fundamentalToPartitionA canonicalLabels
    else if t === "B" then fundamentalToPartitionB canonicalLabels
    else if t === "C" then fundamentalToPartitionC canonicalLabels
    else if t === "D" then fundamentalToPartitionD canonicalLabels
    else error "fundamentalToPartition: A must be a connected Cartan matrix of classical type A, B, C, or D"
);

partitionToFundamental = method()
partitionToFundamental (Matrix, List) := (A, lam) -> (
    A = integralCartanMatrix(A, "partitionToFundamental");
    n := numrows A;
    tnp := classifyConnectedCartan A;
    t := tnp#0;
    canonicalLabels := if      t === "A" then partitionToFundamentalAWithName(n, lam, "partitionToFundamental")
                       else if t === "B" then partitionToFundamentalBWithName(n, lam, "partitionToFundamental")
                       else if t === "C" then partitionToFundamentalCWithName(n, lam, "partitionToFundamental")
                       else if t === "D" then partitionToFundamentalDWithName(n, lam, "partitionToFundamental")
                       else error "partitionToFundamental: A must be a connected Cartan matrix of classical type A, B, C, or D";
    decanonicalizeClassicalLabels(tnp, canonicalLabels)
);

------------------------------------------------------------
-- Internal helpers for routines that accept a WeightFormat option
-- (either "Dynkin", default, or "Partition").  weightFromFormat
-- takes the user's input and normalizes it to Dynkin labels.
-- For "Partition" it requires a connected classical Cartan matrix
-- (type A, B, C, or D).
------------------------------------------------------------

partitionToFundamentalGeneral = (A, lam) -> (
    A = integralCartanMatrix(A, "partitionToFundamental");
    n := numrows A;
    tnp := classifyConnectedCartan A;
    t := tnp#0;
    canonicalLabels := if      t === "A" then partitionToFundamentalAUnchecked(n, lam)
                       else if t === "B" then partitionToFundamentalBUnchecked(n, lam)
                       else if t === "C" then partitionToFundamentalCUnchecked(n, lam)
                       else if t === "D" then partitionToFundamentalDUnchecked(n, lam)
                       else error "WeightFormat => \"Partition\" requires connected classical type A, B, C, or D";
    decanonicalizeClassicalLabels(tnp, canonicalLabels)
);

weightFromFormat = (A, lambda, fmt) -> (
    if fmt === "Dynkin" then lambda
    else if fmt === "Partition" then partitionToFundamentalGeneral(A, lambda)
    else error "WeightFormat must be \"Dynkin\" or \"Partition\""
);

validateFiniteCartanForRepresentations = (A, who) -> (
    A = integralCartanMatrix(A, who);
    r := numrows A;
    if cartanType A =!= "finite" then error concatenate(who, ": A must be of finite type");
    r
);

normalizeIntegralDynkinWeight = (A, w, who, requireDominant) -> (
    if not instance(w, List) then error concatenate(who, ": weight must be a list");
    r := numrows A;
    if #w =!= r then
        error concatenate(who, ": weight must have length equal to rank");
    result := for x in w list (
        xQ := try (x + 0_QQ) else
            error concatenate(who, ": Dynkin labels must be integers");
        if denominator xQ =!= 1 then
            error concatenate(who, ": Dynkin labels must be integers");
        xi := numerator xQ;
        if requireDominant and xi < 0 then
            error concatenate(who, ": highest weight must be dominant (nonnegative Dynkin labels)");
        xi
    );
    result
);

highestWeightFromFormat = (A, lambda, fmt, who) -> (
    w := if fmt === "Dynkin" then lambda
         else if fmt === "Partition" then partitionToFundamental(A, lambda)
         else error concatenate(who, ": WeightFormat must be \"Dynkin\" or \"Partition\"");
    normalizeIntegralDynkinWeight(A, w, who, true)
);

-- specialCartanFromPartition(rankA, k, lambdaPart)
-- Builds a distinguished Cartan matrix for a parabolic subalgebra.
specialCartanFromPartition = (rankA, k, lambdaPart) -> (
    if not instance(rankA, ZZ) or rankA < 1 then
        error "specialCartanFromPartition: rankA must be a positive integer";
    if not instance(k, ZZ) or k < 1 then
        error "specialCartanFromPartition: k must be a positive integer";
    if #lambdaPart > rankA + 1 then
        error "specialCartanFromPartition: partition has more than rankA+1 parts";
    for x in lambdaPart do (
        if not instance(x, ZZ) or x < 0 then
            error "specialCartanFromPartition: partition parts must be nonnegative integers";
    );
    for i from 0 to #lambdaPart - 2 do (
        if lambdaPart#i < lambdaPart#(i+1) then
            error "specialCartanFromPartition: partition parts must be weakly decreasing";
    );

    n := rankA;
    mList := partitionToFundamentalA(n, lambdaPart);

    numNodes := k + n;
    R := ZZ;

    base := 2 * id_(R^numNodes);
    Mmut := mutableMatrix base;

    for j from 0 to k-2 do (
        Mmut_(j, j+1) = -1;
        Mmut_(j+1, j) = -1;
    );

    for j from k to k+n-2 do (
        Mmut_(j, j+1) = -1;
        Mmut_(j+1, j) = -1;
    );

    for i from 0 to n-1 do (
        mi := mList#i;
        if mi != 0 then (
            nodeIdx := (k - 1) + (i + 1);
            Mmut_(0, nodeIdx) = -1;
            Mmut_(nodeIdx, 0) = -mi;
        );
    );

    result := matrix Mmut;
    if not isCartanMatrix result then
        error "specialCartanFromPartition: internal construction did not produce a generalized Cartan matrix";
    result
);


-- A rigorous sufficient height bound for a fixed positive parabolic degree
-- when the degree-zero Levi block is finite.  Completing the square in the
-- positive-definite Levi block of the symmetrized form bounds every Levi
-- coordinate; real-root norms are simple-root norms and imaginary-root norms
-- are nonpositive.
leviRootHeightCompletenessBound = (A, d, parIdx, leviIdx) -> (
    if d < 1 then
        error "leviRootHeightCompletenessBound: degree must be positive";
    if #parIdx == 0 or #leviIdx == 0 then
        error "leviRootHeightCompletenessBound: both node sets must be nonempty";

    D := symmetrizer A;
    if D === null then
        error "leviRootHeightCompletenessBound: Cartan matrix must be symmetrizable";
    B := D * (A ** QQ);
    BLL := submatrix(B, leviIdx, leviIdx);
    invBLL := inverse BLL;

    maxSimpleNorm := B_(0,0);
    for i from 1 to numrows A - 1 do
        maxSimpleNorm = max(maxSimpleNorm, B_(i,i));

    ceilNonnegativeQQ := x -> (
        xQ := x + 0_QQ;
        if xQ <= 0 then return 0;
        (numerator xQ + denominator xQ - 1) // denominator xQ
    );
    sqrtCeilQQ := x -> (
        xQ := x + 0_QQ;
        if xQ <= 0 then return 0;
        nn := numerator xQ;
        dd := denominator xQ;
        k := 0;
        while k * k * dd < nn do k = k + 1;
        k
    );
    coordinateUpper := (center, radiusTerm) -> (
        cQ := center + 0_QQ;
        zQ := radiusTerm + 0_QQ;
        u := ceilNonnegativeQQ(cQ) + sqrtCeilQQ(zQ);
        while u > cQ and (u - cQ)^2 > zQ do u = u - 1;
        u
    );

    bound := d;
    for p in cartanCompositions(d, #parIdx) do (
        g := for a from 0 to #leviIdx - 1 list (
            s := 0_QQ;
            for b from 0 to #parIdx - 1 do
                s = s + B_(leviIdx#a, parIdx#b) * p#b;
            s
        );
        center := for a from 0 to #leviIdx - 1 list (
            s := 0_QQ;
            for b from 0 to #leviIdx - 1 do
                s = s - invBLL_(a,b) * g#b;
            s
        );
        pNorm := 0_QQ;
        for a from 0 to #parIdx - 1 do for b from 0 to #parIdx - 1 do
            pNorm = pNorm + p#a * B_(parIdx#a, parIdx#b) * p#b;
        correction := 0_QQ;
        for a from 0 to #leviIdx - 1 do for b from 0 to #leviIdx - 1 do
            correction = correction + g#a * invBLL_(a,b) * g#b;
        radius := maxSimpleNorm - (pNorm - correction);
        if radius < 0 then continue;

        hCandidate := d;
        for a from 0 to #leviIdx - 1 do (
            hCandidate = hCandidate
                + coordinateUpper(center#a, radius * invBLL_(a,a));
        );
        bound = max(bound, hCandidate);
    );
    bound
);

-- Detect an untwisted affine extension without assuming a node ordering.
isUntwistedAffineCartan = (A, delta) -> (
    r := numrows A;
    for affineNode from 0 to r - 1 do (
        if delta#affineNode != 1 then continue;
        finiteNodes := select(toList(0..r-1), i -> i != affineNode);
        AF := submatrix(A, finiteNodes, finiteNodes);
        if cartanType AF =!= "finite" then continue;
        -- A coefficient-one deletion in a twisted affine diagram can be
        -- finite but disconnected.  Such a deletion cannot be the finite
        -- connected diagram underlying an untwisted affinization, and the
        -- constructor below intentionally accepts connected input only.
        if #leviConnectedComponents0 AF != 1 then continue;
        order := {affineNode} | finiteNodes;
        reordered := submatrix(A, order, order);
        if (reordered ** QQ) == (cartanExtendUntwisted(AF) ** QQ) then
            return true;
    );
    false
);


------------------------------------------------------------
------------------------------------------------------------
--
--         GENERAL CARTAN UTILITIES
--
------------------------------------------------------------
------------------------------------------------------------

-- Check the generalized Cartan axioms
isCartanMatrix = A -> (
    if not instance(A, Matrix) then return false;
    r := numrows A;
    if numcols A =!= r then return false;

    -- Generalized Cartan entries are integers, even when the ambient
    -- matrix ring is QQ (or another ring containing ZZ).
    R := ring A;
    characteristic := try char R else null;
    if characteristic =!= 0 then return false;
    -- The empty matrix is the rank-zero generalized Cartan matrix.  It is
    -- needed for the derived Levi of a Borel and has no axioms left to
    -- check once squareness and characteristic zero are known.
    if r == 0 then return true;
    entriesZZ := new MutableList;
    for i from 0 to r-1 do (
        rowZZ := new MutableList;
        for j from 0 to r-1 do (
            aij := A_(i,j);
            z := try lift(aij, ZZ) else null;
            if z === null then return false;
            if aij != z * 1_R then return false;
            rowZZ#j = z;
        );
        entriesZZ#i = toList rowZZ;
    );

    for i from 0 to r-1 do (
        if entriesZZ#i#i != 2 then return false;
    );

    for i from 0 to r-1 do (
        for j from 0 to r-1 do (
            if i == j then continue;
            if entriesZZ#i#j > 0 then return false;
            if entriesZZ#i#j == 0 and entriesZZ#j#i != 0 then return false;
        );
    );
    true
);

-- Return the canonical integral representative of a generalized Cartan
-- matrix.  Accepting an integral matrix over QQ is useful at the public
-- boundary, but roots and weights must never inherit QQ entries: equal
-- lists over ZZ and QQ are different hash keys in Macaulay2.  Keeping every
-- combinatorial computation over ZZ prevents duplicate roots and failed
-- multiplicity-table lookups.
integralCartanMatrix = (A, who) -> (
    if not instance(A, Matrix) then error(who | ": A must be a matrix");
    r := numrows A;
    if numcols A =!= r then error(who | ": Cartan matrix must be square");
    if not isCartanMatrix A then
        error(who | ": input is not a generalized Cartan matrix");
    if ring A === ZZ then return A;
    if r == 0 then return matrix(ZZ,{});
    cacheName := "KacMoody.integralCartanMatrix";
    if A.cache#?cacheName then return A.cache#cacheName;
    AZZ := matrix apply(entries A, row -> for x in row list lift(x, ZZ));
    A.cache#cacheName = AZZ;
    AZZ
);

-- Shared public-input validators.  Node labels and reflection words use the
-- package-wide 1-based convention.  Node sets are deduplicated and sorted so
-- their order cannot affect a result.
validateNodeSet = (A, nodes, who) -> (
    if not instance(nodes, List) and not instance(nodes, Set) then
        error(who | ": node set must be a list or set");
    r := numrows A;
    L := toList nodes;
    for i in L do (
        if not instance(i, ZZ) or i < 1 or i > r then
            error(who | ": node indices must be integers between 1 and rank");
    );
    sort toList set L
);

validateWeylWord = (A, w, who) -> (
    if not instance(w, List) then error(who | ": word must be a list");
    r := numrows A;
    for i in w do (
        if not instance(i, ZZ) or i < 1 or i > r then
            error(who | ": simple-reflection indices must be integers between 1 and rank");
    );
    w
);

validateRootCoordinateVector = (A, v, who) -> (
    r := numrows A;
    if instance(v, Matrix) then (
        if numrows v =!= 1 or numcols v =!= r then
            error(who | ": vector must be a 1 by rank matrix or a list of length rank");
        for j from 0 to r-1 list v_(0,j)
    ) else if instance(v, List) then (
        if #v =!= r then
            error(who | ": vector must be a 1 by rank matrix or a list of length rank");
        v
    ) else error(who | ": vector must be a 1 by rank matrix or a list of length rank")
);

-- symmetrizer(A)
-- Returns diagonal matrix D over QQ such that D*A is symmetric,
-- or null if not symmetrizable.  Memoized on A.cache.
symmetrizer = A -> (
    A = integralCartanMatrix(A, "symmetrizer");
    r := numrows A;
    if A.cache#?"symmetrizer" then return A.cache#"symmetrizer";

    AQQ := A ** QQ;

    Dtab := new MutableHashTable;
    inconsistent := false;

    for i from 0 to r-1 do (
        if not Dtab#?i then (
            Dtab#i = 1_QQ;
            Q := {i};
            while (#Q > 0 and not inconsistent) do (
                u := first Q;
                Q = drop(Q, 1);
                for v from 0 to r-1 do (
                    if u == v then continue;
                    aUV := AQQ_(u,v);
                    aVU := AQQ_(v,u);
                    if aUV == 0 or aVU == 0 then continue;

                    candidate := Dtab#u * aUV / aVU;

                    if not Dtab#?v then (
                        Dtab#v = candidate;
                        Q = append(Q, v);
                    ) else (
                        if Dtab#v != candidate then (
                            inconsistent = true;
                            break;
                        );
                    );
                );
            );
        );
    );

    if inconsistent then return null;

    dList := for i from 0 to r-1 list (
        if Dtab#?i then Dtab#i else 1_QQ
    );

    for d in dList do (
        if d <= 0 then return null;
    );

    Dmat := diagonalMatrix dList;
    B := Dmat * AQQ;

    for i from 0 to r-1 do (
        for j from i+1 to r-1 do (
            if B_(i,j) != B_(j,i) then return null;
        );
    );

    A.cache#"symmetrizer" = Dmat;
    Dmat
);

-- Exact inertia classification of a symmetric rational matrix, specialized
-- to the three possibilities needed for a connected symmetrized GCM.  A
-- symmetric Schur-complement elimination is polynomial in the rank and
-- avoids checking all 2^r proper principal submatrices in affine type.
symmetricCartanComponentType = C -> (
    C = C ** QQ;
    m := numrows C;
    nullity := 0;
    while m > 0 do (
        pivotIndex := null;
        for i from 0 to m-1 do (
            if C_(i,i) != 0 then (
                pivotIndex = i;
                break;
            );
        );

        if pivotIndex === null then (
            hasOffDiagonal := false;
            for i from 0 to m-1 do for j from i+1 to m-1 do (
                if C_(i,j) != 0 then (
                    hasOffDiagonal = true;
                    break;
                );
            );
            -- A nonzero symmetric matrix with zero diagonal has both a
            -- positive and a negative direction.
            if hasOffDiagonal then return "indefinite";
            nullity = nullity + m;
            m = 0;
        ) else (
            pivot := C_(pivotIndex,pivotIndex);
            if pivot < 0 then return "indefinite";
            J := select(toList(0..m-1), j -> j != pivotIndex);
            if #J == 0 then (
                m = 0;
            ) else (
                column := submatrix(C, J, {pivotIndex});
                row := submatrix(C, {pivotIndex}, J);
                C = submatrix(C, J, J) - (1_QQ / pivot) * (column * row);
                m = m - 1;
            );
        );
    );
    if nullity == 0 then "finite"
    else if nullity == 1 then "affine"
    else "indefinite"
);

-- Classify a Cartan matrix as finite, affine, indefinite, or nonsymmetrizable
cartanType = A -> (
    A = integralCartanMatrix(A, "cartanType");
    if A.cache#?"cartanType" then return A.cache#"cartanType";

    r := numrows A;
    D := symmetrizer A;
    if D === null then (
        A.cache#"cartanType" = "nonsymmetrizable";
        return "nonsymmetrizable";
    );

    AQQ := A ** QQ;
    B := D * AQQ;

    componentTypes := apply(leviConnectedComponents0 A,
        idx -> symmetricCartanComponentType submatrix(B, idx, idx));
    result := if all(componentTypes, t -> t === "finite") then "finite"
    else if any(componentTypes, t -> t === "affine")
            and all(componentTypes, t -> t === "finite" or t === "affine")
        then "affine"
    else "indefinite";
    A.cache#"cartanType" = result;
    result
);

cartanRank = method()
cartanRank Matrix := A -> (
    A = integralCartanMatrix(A, "cartanRank");
    numrows A
);

-- A compact, self-describing summary suitable for inspection, notebooks,
-- and programmatic dispatch.  Component node labels are public 1-based
-- labels; canonicalNodeOrder records how a recognized finite component is
-- relabeled to the package's standard A--G constructor.
cartanData = method()
cartanData Matrix := A -> (
    A = integralCartanMatrix(A, "cartanData");
    r := numrows A;
    D := symmetrizer A;
    componentRecords := for comp in leviConnectedComponents0 A list (
        C := submatrix(A, comp, comp);
        componentType := cartanType C;
        finiteInfo := if componentType === "finite" then
            classifyConnectedCartan C else ("other", 0, {});
        family := if finiteInfo#0 === "other" then null else finiteInfo#0;
        canonicalNodes := if family === null then apply(comp, i -> i+1)
            else apply(finiteInfo#2, i -> comp#i + 1);
        label := if family === null then componentType
            else family | "_" | toString finiteInfo#1;
        new HashTable from {
            "nodes" => apply(comp, i -> i+1),
            "type" => componentType,
            "family" => family,
            "rank" => #comp,
            "label" => label,
            "canonicalNodeOrder" => canonicalNodes,
            "cartanMatrix" => C
        }
    );
    matrixRank := rank(A ** QQ);
    new HashTable from {
        "cartanMatrix" => A,
        "rank" => r,
        "matrixRank" => matrixRank,
        "corank" => r - matrixRank,
        "type" => cartanType A,
        "symmetrizable" => (D =!= null),
        "symmetrizer" => D,
        "components" => componentRecords,
        "nodeConvention" => "1-based"
    }
);

-- Internal domain guards used by APIs whose unbounded algorithms or
-- distinguished invariants only make mathematical sense in finite type.
requireFiniteCartanType = (name, A) -> (
    A = integralCartanMatrix(A, name);
    if cartanType A =!= "finite" then
        error(name | ": Cartan matrix must be of finite type");
    true
);

requireConnectedFiniteCartanType = (name, A) -> (
    A = integralCartanMatrix(A, name);
    requireFiniteCartanType(name, A);
    if #(leviConnectedComponents0 A) =!= 1 then
        error(name | ": Cartan matrix must be connected");
    true
);


-- Abstract simple roots (identity matrix in simple-root basis)
simpleRoots = A -> (
    A = integralCartanMatrix(A, "simpleRoots");
    r := numrows A;
    matrix table(r, r, (i,j) -> if i == j then 1_QQ else 0_QQ)
);

-- Simple coroots as functionals on the root lattice
coroots = A -> (
    A = integralCartanMatrix(A, "coroots");
    A ** QQ
);

-- Fundamental weights in simple-root basis
fundamentalWeights = A -> (
    A = integralCartanMatrix(A, "fundamentalWeights");
    r := numrows A;
    AQQ := A ** QQ;
    if det AQQ == 0 then error "fundamentalWeights: Cartan matrix is singular, so fundamental weights are not determined in the simple-root span";
    transpose inverse AQQ
);

-- Convert simple-root coords to fundamental-weight coords
toFundamental = (A, lambdaSimple) -> (
    A = integralCartanMatrix(A, "toFundamental");
    r := numrows A;
    AQQ := A ** QQ;
    cList := validateRootCoordinateVector(A, lambdaSimple, "toFundamental");

    for j from 0 to r-1 list (
        s := 0_QQ;
        for i from 0 to r-1 do (
            s = s + AQQ_(j,i) * cList#i;
        );
        s
    )
);

-- Convert fundamental-weight coords to simple-root coords
toSimple = (A, lambdaFund) -> (
    A = integralCartanMatrix(A, "toSimple");
    r := numrows A;

    W := fundamentalWeights A;
    mList := validateRootCoordinateVector(A, lambdaFund, "toSimple");

    for i from 0 to r-1 list (
        s := 0_QQ;
        for j from 0 to r-1 do (
            s = s + mList#j * W_(j,i);
        );
        s
    )
);

-- Height of a root/weight in simple-root coordinates
rootHeight = v -> (
    if instance(v, Matrix) then (
        if numrows v =!= 1 then error "rootHeight: matrix input must be 1xr";
        r := numcols v;
        sM := 0;
        for j from 0 to r-1 do (
            sM = sM + v_(0,j);
        );
        sM
    ) else (
        sL := 0;
        for x in v do sL = sL + x;
        sL
    )
);

-- (v,v) with respect to the symmetrized bilinear form B = D*A
rootNormSquared = (A, v) -> (
    A = integralCartanMatrix(A, "rootNormSquared");
    r := numrows A;

    D := symmetrizer A;
    if D === null then error "rootNormSquared: Cartan matrix is not symmetrizable";

    AQQ := A ** QQ;
    B := D * AQQ;

    vMat := (
        if instance(v, Matrix) then (
            if numrows v =!= 1 or numcols v =!= r then (
                error "rootNormSquared: v must be a 1xr matrix or a list of length r";
            );
            v ** QQ
        ) else (
            if #v =!= r then error "rootNormSquared: vector length must equal rank";
            (matrix{ for j from 0 to r-1 list v#j }) ** QQ
        )
    );

    (vMat * B * transpose vMat)_(0,0)
);


------------------------------------------------------------
------------------------------------------------------------
--
--         ROOT INNER PRODUCT
--
------------------------------------------------------------
------------------------------------------------------------

-- rootInnerProduct(A, v, w)
-- The symmetrized bilinear form (v,w) = v^T B w where B = D*A.
-- v, w are in simple-root coordinates (lists or 1xr matrices).

rootInnerProduct = (A, v, w) -> (
    A = integralCartanMatrix(A, "rootInnerProduct");
    r := numrows A;

    D := symmetrizer A;
    if D === null then error "rootInnerProduct: Cartan matrix is not symmetrizable";

    AQQ := A ** QQ;
    B := D * AQQ;

    vList := validateRootCoordinateVector(A, v, "rootInnerProduct");
    wList := validateRootCoordinateVector(A, w, "rootInnerProduct");

    s := 0_QQ;
    for i from 0 to r-1 do (
        for j from 0 to r-1 do (
            s = s + vList#i * B_(i,j) * wList#j;
        );
    );
    s
);


------------------------------------------------------------
------------------------------------------------------------
--
--         NEGATIVE ROOTS, ALL ROOTS
--
------------------------------------------------------------
------------------------------------------------------------

-- negativeRoots(A) : negate all positive roots
negativeRoots = method();

negativeRoots Matrix := A -> (
    R := cartanPositiveRoots A;
    - R
);

negativeRoots(Matrix, ZZ) := (A, H) -> (
    R := cartanPositiveRoots(A, H);
    - R
);

-- allRoots(A) : positive and negative roots (does not include zero)
allRoots = method();

allRoots Matrix := A -> (
    R := cartanPositiveRoots A;
    R || (- R)
);

allRoots(Matrix, ZZ) := (A, H) -> (
    R := cartanPositiveRoots(A, H);
    R || (- R)
);


------------------------------------------------------------
------------------------------------------------------------
--
--         WEYL VECTOR, DOMINANCE, HIGHEST ROOT
--
------------------------------------------------------------
------------------------------------------------------------

-- weylVector(A)
-- Returns rho = (1/2) * sum of positive roots, in simple-root coords.
-- Equivalently, rho = sum of fundamental weights, so Dynkin labels are all 1.
-- For finite type only (uses cartanPositiveRoots).

weylVector = A -> (
    A = integralCartanMatrix(A, "weylVector");
    r := numrows A;
    requireFiniteCartanType("weylVector", A);
    -- rho = sum of fundamental weights = A^{-1} * (1,...,1)
    -- This is faster than summing all positive roots
    AQQ := A ** QQ;
    if det AQQ == 0 then error "weylVector: Cartan matrix not invertible (use finite type)";
    W := fundamentalWeights A;  -- rows = omega_j in alpha-basis
    rho := for i from 0 to r-1 list (
        s := 0_QQ;
        for j from 0 to r-1 do s = s + W_(j,i);
        s
    );
    rho
);


-- isDominant(A, lambda)
-- Test if a weight lambda (in fundamental-weight / Dynkin coordinates)
-- has all coordinates >= 0.
-- lambda can be a list or a 1xr matrix.

isDominant = method(Options => {WeightFormat => "Dynkin"})
isDominant (Matrix, List) := opts -> (A, lambda) -> (
    A = integralCartanMatrix(A, "isDominant");
    lam := weightFromFormat(A, lambda, opts.WeightFormat);
    lam = normalizeIntegralDynkinWeight(A, lam, "isDominant", false);
    all(lam, c -> c >= 0)
);
isDominant (Matrix, Matrix) := opts -> (A, lambdaMat) -> (
    A = integralCartanMatrix(A, "isDominant");
    r := numrows A;
    if numrows lambdaMat =!= 1 or numcols lambdaMat =!= r then
        error "isDominant: matrix weight must have size 1 by rank";
    lam := for j from 0 to r-1 list lambdaMat_(0,j);
    lam2 := weightFromFormat(A, lam, opts.WeightFormat);
    lam2 = normalizeIntegralDynkinWeight(A, lam2, "isDominant", false);
    all(lam2, c -> c >= 0)
);


-- highestRoot(A)
-- Returns the highest root (= longest root in partial order) in simple-root coords.
-- For finite type: the unique root with maximal height.

highestRoot = A -> (
    A = integralCartanMatrix(A, "highestRoot");
    requireConnectedFiniteCartanType("highestRoot", A);
    r := numrows A;
    R := cartanPositiveRoots A;
    best := null;
    bestHt := -1;
    for row from 0 to numrows R - 1 do (
        v := for j from 0 to r-1 list R_(row,j);
        ht := 0;
        for x in v do ht = ht + x;
        if ht > bestHt then (
            bestHt = ht;
            best = v;
        );
    );
    best
);


-- highestShortRoot(A)
-- For non-simply-laced types (B, C, F4, G2), returns the highest short root.
-- For simply-laced types, same as highestRoot.

highestShortRoot = A -> (
    A = integralCartanMatrix(A, "highestShortRoot");
    requireConnectedFiniteCartanType("highestShortRoot", A);
    r := numrows A;
    R := cartanPositiveRoots A;
    D := symmetrizer A;
    if D === null then error "highestShortRoot: Cartan matrix not symmetrizable";
    AQQ := A ** QQ;
    B := D * AQQ;

    -- Find the minimal norm among all roots (= short root norm)
    norms := {};
    roots := {};
    for row from 0 to numrows R - 1 do (
        v := for j from 0 to r-1 list R_(row,j);
        n := 0_QQ;
        for i from 0 to r-1 do (
            for j from 0 to r-1 do (
                n = n + v#i * B_(i,j) * v#j;
            );
        );
        norms = append(norms, n);
        roots = append(roots, v);
    );

    minNorm := min norms;

    -- Among short roots, find the one with maximal height
    best := null;
    bestHt := -1;
    for idx from 0 to #roots - 1 do (
        if norms#idx == minNorm then (
            ht := 0;
            for x in roots#idx do ht = ht + x;
            if ht > bestHt then (
                bestHt = ht;
                best = roots#idx;
            );
        );
    );
    best
);


-- coxeterNumber(A)
-- h = height(theta) + 1 where theta is the highest root

coxeterNumber = A -> (
    theta := highestRoot A;
    ht := 0;
    for x in theta do ht = ht + x;
    ht + 1
);


-- dualCoxeterNumber(A)
-- h^v = height of highest short root of the dual (= highest coroot) + 1
-- Equivalently: sum of comarks + 1, where comarks = coefficients of
-- highest short root in fundamental weight basis of the dual.
-- For simply-laced: h^v = h.

dualCoxeterNumber = A -> (
    -- The dual Coxeter number is (rho, theta^v) + 1 where theta^v
    -- is the highest coroot.  Equivalently, it equals the height
    -- of the highest short root of A^T plus 1.
    At := transpose A;
    theta := highestShortRoot At;
    ht := 0;
    for x in theta do ht = ht + x;
    ht + 1
);


-- dualCartanMatrix(A)
-- Returns the transpose A^T, which is the Cartan matrix of the dual (Langlands dual).

dualCartanMatrix = A -> transpose(integralCartanMatrix(A, "dualCartanMatrix"));


-- dynkinDiagramData(A)
-- Returns a hash table describing the Dynkin diagram:
--   "nodes"      => {1,..,r}
--   "edges"      => list of {i,j, a_ij, a_ji} for connected pairs i < j
--   "rank"       => r
--
-- Typed overloads follow the definitions of CartanDatum,
-- Realization, and RootDatum.  Their NodeFormat option makes custom labels
-- available without conflating an integer label with a node position.

dynkinDiagramData = method(Options => {NodeFormat => null});

dynkinDiagramData Matrix := opts -> A -> (
    A = integralCartanMatrix(A, "dynkinDiagramData");
    if opts.NodeFormat =!= null and opts.NodeFormat =!= "Positions" then
        error "dynkinDiagramData: Matrix input only supports NodeFormat => \"Positions\"";
    r := numrows A;
    edges := {};
    for i from 0 to r-1 do (
        for j from i+1 to r-1 do (
            if A_(i,j) != 0 or A_(j,i) != 0 then (
                edges = append(edges, {i+1, j+1, A_(i,j), A_(j,i)});
            );
        );
    );
    new HashTable from {
        "nodes" => toList(1..r),
        "edges" => edges,
        "rank"  => r
    }
);


-- dynkinDiagram(A)
-- Produce an ASCII (with unicode arrows) rendering of the Dynkin diagram
-- for the Cartan matrix A. Works for any Cartan matrix (simply-laced,
-- non-simply-laced, possibly disconnected, including affine/Kac-Moody).
--
-- Returns a String suitable for "print" or << output.
--
-- Conventions:
--   single bond:           i ----- j
--   double bond (i long):  i ==>== j    (arrow from long to short)
--   double bond (j long):  i ==<== j
--   triple bond (i long):  i #=>=# j
--   triple bond (j long):  i #=<=# j
--
-- Nodes are labeled 1..r. For recognized shapes (path, or path with
-- a single branch of degree 3 as in D/E types), a linear/branched
-- layout is produced. For unusual shapes (cycles, higher-valence
-- vertices, affine types with the extra node), a compact edge-list
-- layout is produced instead.

dynkinDiagram = method(Options => {NodeFormat => null});

-- The graph layout is independent of the objects used to name its nodes.
-- Keeping the label strings as an explicit argument lets typed overloads
-- reuse every layout and bond-direction decision made for Matrix input.
-- centerLabels is false for Matrix input so its historical output, including
-- the alignment of multi-digit numeric branch labels, remains unchanged.
dynkinDraftDiagram = (A,nodeStrings,centerLabels) -> (
    A = integralCartanMatrix(A, "dynkinDiagram");
    r := numrows A;
    if not instance(nodeStrings,List) or #nodeStrings != r
        or not all(nodeStrings,s -> instance(s,String)) then
        error "dynkinDiagram: internal node-label rendering mismatch";
    nodeText := i -> nodeStrings#i;
    if r == 0 then return "";

    -- Collect edges (undirected), with bond count and arrow direction.
    -- Convention: our matrix A satisfies A_(i,j) = 2(alpha_j, alpha_i)/(alpha_i, alpha_i),
    -- so |A_(i,j)| / |A_(j,i)| = (alpha_j, alpha_j) / (alpha_i, alpha_i).
    -- Therefore |A_(i,j)| > |A_(j,i)|  <=>  alpha_j is longer  <=>  arrow j -> i.
    -- arrow = 0 : no arrow (equal lengths)
    -- arrow = 1 : arrow from i to j (alpha_i is longer)
    -- arrow = -1: arrow from j to i (alpha_j is longer)
    edgeList := {};
    for i from 0 to r-1 do (
        for j from i+1 to r-1 do (
            aij := A_(i,j);
            aji := A_(j,i);
            if aij != 0 or aji != 0 then (
                absIj := if aij < 0 then -aij else aij;
                absJi := if aji < 0 then -aji else aji;
                bondCount := if absIj > absJi then absIj else absJi;
                dir := if absIj == absJi then 0
                       else if absIj > absJi then -1
                       else 1;
                edgeList = append(edgeList, {i, j, bondCount, dir});
            );
        );
    );

    -- Build adjacency list (0-indexed)
    adj := new MutableHashTable;
    for i from 0 to r-1 do adj#i = {};
    edgeLookup := new MutableHashTable;
    for e in edgeList do (
        ii := e#0; jj := e#1;
        adj#ii = append(adj#ii, jj);
        adj#jj = append(adj#jj, ii);
        edgeLookup#(ii, jj) = e;
        edgeLookup#(jj, ii) = e;
    );

    -- Given an edge record {i, j, bondCount, dir} and the direction of
    -- travel (from fromNode to toNode), return a 5-character bond glyph.
    bondGlyph := (fromNode, toNode) -> (
        e := edgeLookup#(fromNode, toNode);
        bc := e#2;
        d := e#3;
        -- Does the arrow point in the direction of travel?
        arrowInTravelDir := if d == 0 then 0
            else if (d == 1 and e#0 == fromNode) or (d == -1 and e#1 == fromNode) then 1
            else -1;
        if bc == 1 then " --- "
        else if bc == 2 then (
            if arrowInTravelDir == 0 then " === "
            else if arrowInTravelDir == 1 then " =>= "
            else " =<= "
        )
        else (
            -- triple (bc >= 3)
            if arrowInTravelDir == 0 then " ### "
            else if arrowInTravelDir == 1 then " #>= "
            else " #<= "
        )
    );

    -- Compute degree of each node
    deg := i -> #(adj#i);

    -- Process connected components separately.
    visited := new MutableHashTable;
    for i from 0 to r-1 do visited#i = false;

    componentOf := startNode -> (
        comp := {};
        stack := {startNode};
        while #stack > 0 do (
            v := last stack;
            stack = drop(stack, -1);
            if not visited#v then (
                visited#v = true;
                comp = append(comp, v);
                for u in adj#v do (
                    if not visited#u then stack = append(stack, u);
                );
            );
        );
        comp
    );

    -- Render a single connected component.
    renderComp := comp -> (
        if #comp == 1 then (
            singletonText := nodeText(comp#0);
            if not centerLabels then
                return "  o  " | newline | "  " | singletonText;
            singletonPad := concatenate for kk from 1 to
                (#singletonText - 1)//2 list " ";
            return singletonPad | "o" | newline | singletonText;
        );
        compSet := set comp;
        -- Complete, lossless fallback used whenever a planar ASCII layout
        -- would omit part of an arm or hide a multiple-bond arrow.
        renderEdgeList := () -> concatenate between(", ", apply(
            select(edgeList, e -> member(e#0, compSet)),
            e -> (
                g := if e#2 == 1 then "--"
                     else if e#2 == 2 then (if e#3 == 1 then "=>" else if e#3 == -1 then "<=" else "==")
                     else (if e#3 == 1 then "#>" else if e#3 == -1 then "<#" else "##");
                nodeText(e#0) | g | nodeText(e#1)
            )
        ));
        -- Determine shape: path, path-with-branch (D/E), or other
        degrees := apply(comp, i -> deg i);
        leaves := select(comp, i -> deg(i) == 1);
        highDeg := select(comp, i -> deg(i) >= 3);

        -- Is it a path? every node has degree <= 2
        isPath := all(comp, i -> deg(i) <= 2);

        if isPath then (
            -- Order nodes linearly starting from a leaf (or any node if cycle)
            if #leaves == 0 then (
                -- Every node has degree 2: this is a genuine cycle
                -- (e.g. the untwisted affine A_n^{(1)} diagram).  Render as
                -- a linear sequence with the starting label repeated at the
                -- end to indicate cycle closure, and a note "(cycle)".
                cycStart := comp#0;
                cycOrder := {cycStart};
                cycPrev := -1;
                cycCur := cycStart;
                cycDone := false;
                while not cycDone do (
                    cycNxt := -1;
                    for u in adj#cycCur do (
                        if u != cycPrev then cycNxt = u;
                    );
                    if cycNxt == -1 or cycNxt == cycStart then cycDone = true
                    else (
                        cycOrder = append(cycOrder, cycNxt);
                        cycPrev = cycCur;
                        cycCur = cycNxt;
                        if #cycOrder > #comp then cycDone = true;
                    );
                );
                cycParts := {};
                for k from 0 to #cycOrder-1 do (
                    cycParts = append(cycParts, nodeText(cycOrder#k));
                    nextNode := if k < #cycOrder-1 then cycOrder#(k+1) else cycStart;
                    cycParts = append(cycParts, bondGlyph(cycOrder#k, nextNode));
                );
                cycParts = append(cycParts, nodeText(cycStart) | "  (cycle)");
                return concatenate cycParts;
            );
            startN := first leaves;
            order := {startN};
            prev := -1;
            cur := startN;
            while #(adj#cur) > 0 and (prev == -1 or #(adj#cur) > 1 or adj#cur#0 != prev) do (
                nxt := -1;
                for u in adj#cur do (
                    if u != prev then nxt = u;
                );
                if nxt == -1 then break;
                order = append(order, nxt);
                prev = cur;
                cur = nxt;
                if #order > #comp then break;  -- safety
            );
            -- Build the rendered string
            -- Top line: labels separated by bond glyphs
            topParts := {};
            for k from 0 to #order-1 do (
                topParts = append(topParts, nodeText(order#k));
                if k < #order - 1 then (
                    topParts = append(topParts, bondGlyph(order#k, order#(k+1)));
                );
            );
            concatenate topParts
        )
        else if #highDeg == 1 and (deg highDeg#0) == 3 and #select(comp, i -> deg(i) > 3) == 0 then (
            -- D/E-type: one branch node of degree 3
            branchN := highDeg#0;
            -- The branch node has 3 neighbors. Pick two "main" branches
            -- (the two longer ones) and one "side" branch.
            -- For simplicity, identify the "short branch" as the neighbor
            -- whose subtree has size 1 (a single leaf). If multiple leaves
            -- are neighbors, pick the first one.
            nbrs := adj#branchN;
            -- Subtree sizes from each neighbor (excluding branchN)
            subtreeSize := nbr -> (
                seen := new MutableHashTable;
                seen#branchN = true;
                stk := {nbr};
                cnt := 0;
                while #stk > 0 do (
                    v := last stk;
                    stk = drop(stk, -1);
                    if not seen#?v then (
                        seen#v = true;
                        cnt = cnt + 1;
                        for u in adj#v do (
                            if not seen#?u then stk = append(stk, u);
                        );
                    );
                );
                cnt
            );
            sizes := apply(nbrs, n -> (n, subtreeSize n));
            -- Sort so the smallest subtree is the side branch
            sorted := sort apply(sizes, p -> (p#1, p#0));
            sideN := sorted#0#1;
            left := sorted#2#1;
            right := sorted#1#1;

            -- The compact branched layout has room for exactly one
            -- single-bond side leaf.  Anything more complicated is rendered
            -- as an edge list so that every node and bond remains visible.
            sideSize := sorted#0#0;
            sideEdge := edgeLookup#(branchN, sideN);
            if sideSize != 1 or sideEdge#2 != 1 then return renderEdgeList();

            -- Walk from left leaf through the branch node to right leaf
            walkToLeaf := (start, forbid) -> (
                order := {start};
                prev := forbid;
                cur := start;
                while true do (
                    nxt := -1;
                    for u in adj#cur do (
                        if u != prev then nxt = u;
                    );
                    if nxt == -1 then break;
                    order = append(order, nxt);
                    prev = cur;
                    cur = nxt;
                    if #order > #comp then break;
                );
                order
            );
            leftChain := reverse walkToLeaf(left, branchN);  -- leaf ... left-neighbor
            rightChain := walkToLeaf(right, branchN);        -- right-neighbor ... leaf
            mainOrder := leftChain | {branchN} | rightChain;

            -- Main line
            mainParts := {};
            for k from 0 to #mainOrder-1 do (
                mainParts = append(mainParts, nodeText(mainOrder#k));
                if k < #mainOrder - 1 then (
                    mainParts = append(mainParts, bondGlyph(mainOrder#k, mainOrder#(k+1)));
                );
            );
            mainLine := concatenate mainParts;
            -- Find the column index of branchN in mainLine.
            -- Each displayed node takes its rendered width, and each bond
            -- takes five columns.
            col := 0;
            for k from 0 to #mainOrder-1 do (
                if mainOrder#k == branchN then break;
                col = col + #(nodeText(mainOrder#k)) + 5;
            );
            if centerLabels then
                col = col + (#(nodeText branchN) - 1)//2;
            -- Vertical bond (single) to side branch leaf
            pad := concatenate for kk from 1 to col list " ";
            -- For display, just use "|" for vertical bond (only single bonds have branches)
            vertLine := pad | "|";
            sideCol := if centerLabels then
                max(0,col - (#(nodeText sideN) - 1)//2)
                else col;
            sidePad := concatenate for kk from 1 to sideCol list " ";
            sideLine := sidePad | nodeText(sideN);
            mainLine | newline | vertLine | newline | sideLine
        )
        else (
            renderEdgeList()
        )
    );

    components := {};
    for i from 0 to r-1 do (
        if not visited#i then components = append(components, componentOf i);
    );

    concatenate between(newline | newline, apply(components, renderComp))
);

dynkinDiagram Matrix := opts -> A -> (
    A = integralCartanMatrix(A, "dynkinDiagram");
    if opts.NodeFormat =!= null and opts.NodeFormat =!= "Positions" then
        error "dynkinDiagram: Matrix input only supports NodeFormat => \"Positions\"";
    dynkinDraftDiagram(
        A,apply(numrows A,i -> toString(i+1)),false)
);

-- Dispatch by type letter and rank, e.g. dynkinDiagram("A", 4) or dynkinDiagram("E", 6).
-- For F4 and G2 the rank is fixed; passing any nonzero rank (or 4/2) also works.
dynkinDiagram(String, ZZ) := opts -> (typeLetter, rank) -> (
    t := typeLetter;
    if t === "A" then dynkinDiagram(cartanA(rank + 1),NodeFormat=>opts.NodeFormat)
    else if t === "B" then dynkinDiagram(cartanB rank,NodeFormat=>opts.NodeFormat)
    else if t === "C" then dynkinDiagram(cartanC rank,NodeFormat=>opts.NodeFormat)
    else if t === "D" then dynkinDiagram(cartanD rank,NodeFormat=>opts.NodeFormat)
    else if t === "E" then dynkinDiagram(cartanE rank,NodeFormat=>opts.NodeFormat)
    else if t === "F" then (
        if rank != 4 then error "dynkinDiagram: type F only defined for rank 4";
        dynkinDiagram(cartanF4(),NodeFormat=>opts.NodeFormat)
    )
    else if t === "G" then (
        if rank != 2 then error "dynkinDiagram: type G only defined for rank 2";
        dynkinDiagram(cartanG2(),NodeFormat=>opts.NodeFormat)
    )
    else error ("dynkinDiagram: unknown type letter \"" | t | "\"; expected one of A,B,C,D,E,F,G")
)

-- Dispatch by a single string such as "A4", "B3", "E6", "F4", "G2".
dynkinDiagram String := opts -> s -> (
    if #s < 2 then error "dynkinDiagram: type string must be a letter followed by a rank, e.g. \"A4\"";
    letter := substring(s, 0, 1);
    rankStr := substring(s, 1, #s - 1);
    digits := ascii rankStr;
    if #digits == 0 or any(digits, c -> c < 48 or c > 57) then
        error ("dynkinDiagram: rank in \"" | s | "\" must contain digits only");
    rank := 0;
    for c in digits do rank = 10 * rank + c - 48;
    if rank < 1 then error "dynkinDiagram: rank must be a positive integer";
    -- For type A, "An" denotes rank n (i.e. Lie algebra sl_{n+1}).
    if letter === "A" then
        dynkinDiagram(cartanA(rank + 1),NodeFormat=>opts.NodeFormat)
    else dynkinDiagram(letter,rank,NodeFormat=>opts.NodeFormat)
)


------------------------------------------------------------
------------------------------------------------------------
--
--         COORDINATE CONVERSIONS (EPSILON BASIS)
--
------------------------------------------------------------
------------------------------------------------------------

-- toEpsilon(S, lambda)
-- Convert a weight from simple-root coordinates to epsilon (ambient) coordinates.
-- S : the simple root matrix (rows = simple roots in epsilon basis),
--     e.g. simpleRootsA(n) or simpleRootsE6()
-- lambda : list or 1xr matrix in simple-root coords

toEpsilon = (S, lambda) -> (
    r := numrows S;
    d := numcols S;
    SQQ := S ** QQ;
    lam := if instance(lambda, Matrix) then (
        if numrows lambda =!= 1 or numcols lambda =!= r then error "toEpsilon: lambda must be 1xr";
        lambda ** QQ
    ) else (
        if #lambda =!= r then error "toEpsilon: lambda must have length numrows S";
        (matrix {for x in lambda list x}) ** QQ
    );
    -- result = lambda * S (row vector times matrix)
    result := lam * SQQ;
    for j from 0 to d-1 list result_(0,j)
);

-- fromEpsilon(S, v)
-- Convert a vector in epsilon (ambient) coordinates to simple-root coordinates.
-- S must be invertible (or at least left-invertible for non-square S).
-- For rank r < ambient dim d, we use the pseudo-inverse S^T (S S^T)^{-1}.

fromEpsilon = (S, v) -> (
    r := numrows S;
    d := numcols S;
    SQQ := S ** QQ;
    vMat := if instance(v, Matrix) then (
        if numrows v =!= 1 or numcols v =!= d then error "fromEpsilon: v must be 1xd";
        v ** QQ
    ) else (
        if #v =!= d then error "fromEpsilon: v must have length numcols S";
        (matrix {for x in v list x}) ** QQ
    );
    if r == d then (
        -- Square case: just invert
        invS := inverse SQQ;
        resSq := vMat * invS;
        for j from 0 to r-1 list resSq_(0,j)
    ) else (
        -- Non-square: use right pseudo-inverse S^T (S S^T)^{-1}
        -- v * S^T (S S^T)^{-1} gives simple-root coords
        SSt := SQQ * transpose SQQ;
        if det SSt == 0_QQ then error "fromEpsilon: rows of S are linearly dependent";
        invSSt := inverse SSt;
        resRect := vMat * transpose SQQ * invSSt;
        if resRect * SQQ != vMat then
            error "fromEpsilon: vector is outside the span of the simple roots";
        for j from 0 to r-1 list resRect_(0,j)
    )
);


------------------------------------------------------------
------------------------------------------------------------
--
--         WEYL DIMENSION FORMULA
--
------------------------------------------------------------
------------------------------------------------------------

-- weylDimension(A, lambda)
-- Computes dim V(lambda) via the Weyl dimension formula:
--   dim V(lambda) = prod_{alpha > 0} (lambda + rho, alpha) / (rho, alpha)
-- lambda is in fundamental weight (Dynkin) coordinates.
-- A is a finite-type Cartan matrix.

weylDimension = method(Options => {WeightFormat => "Dynkin"})
weylDimension (Matrix, List) := opts -> (A, lambda) -> (
    A = integralCartanMatrix(A, "weylDimension");
    r := validateFiniteCartanForRepresentations(A, "weylDimension");
    lambda = highestWeightFromFormat(A, lambda, opts.WeightFormat, "weylDimension");

    -- Convert lambda from fundamental coords to simple-root coords
    lamSimple := toSimple(A, lambda);

    -- rho in simple-root coords
    rho := weylVector A;

    -- lambda + rho in simple-root coords
    lamRho := for i from 0 to r-1 list (lamSimple#i + rho#i);

    -- Positive roots in simple-root coords (cached on A)
    R := cartanPositiveRoots A;

    -- Compute and cache the symmetric form B = D*A (over QQ).  Also
    -- cache a pre-pairing "R * transpose(B)" of size (#roots x r) so
    -- that <v, alpha_i> = v * RB_i^T is a single rank-r dot product.
    D := symmetrizer A;
    if D === null then error "weylDimension: Cartan matrix not symmetrizable";
    RB := if A.cache#?"weylDimension.RB" then A.cache#"weylDimension.RB"
          else (
              B := D * (A ** QQ);
              M := (R ** QQ) * transpose B;     -- rows = B * alpha_i (in QQ)
              A.cache#"weylDimension.RB" = M;
              M
          );

    -- Vectors as 1xr QQ-row-matrices; <v,alpha_i> = (v * RB^T)_(0,i).
    nroots := numrows RB;
    lamRhoVec := matrix{for x in lamRho list x*1_QQ};
    rhoVec    := matrix{for x in rho    list x*1_QQ};
    numerRow := lamRhoVec * transpose RB;
    denomRow := rhoVec    * transpose RB;

    numer := 1_QQ;
    denom := 1_QQ;
    for i from 0 to nroots - 1 do (
        numer = numer * numerRow_(0,i);
        denom = denom * denomRow_(0,i);
    );

    if denom == 0_QQ then error "weylDimension: zero Weyl denominator";
    answer := numer / denom;
    if denominator answer =!= 1 then
        error concatenate("weylDimension: nonintegral result ", toString answer);
    numerator answer
);


------------------------------------------------------------
------------------------------------------------------------
--
--         LONG ELEMENT AND WEYL GROUP
--
------------------------------------------------------------
------------------------------------------------------------

-- longElement(A)
-- Returns a reduced word for w_0 (the longest element of W).
-- Starting at the identity, repeatedly take a right ascent.  In a finite
-- Coxeter group an element with no right ascent is the unique longest
-- element, so this takes |Phi^+| reflection steps instead of enumerating W.

longElement = method()
longElement Matrix := A -> (
    A = integralCartanMatrix(A, "longElement");
    requireFiniteCartanType("longElement", A);
    if A.cache#?"longElement" then return A.cache#"longElement";
    R := ring A;
    n := numrows A;
    Slist := apply(n, i -> reflectionMatrix(A, i+1));
    M := id_(R^n);
    word := new MutableList;
    wordLength := 0;
    targetLength := numrows cartanPositiveRoots A;

    while wordLength < targetLength do (
        ascent := null;
        for i from 0 to n-1 do (
            column := for j from 0 to n-1 list M_(j,i);
            if isPositiveVector column then (
                ascent = i;
                break;
            );
        );
        if ascent === null then
            error "longElement: ascent algorithm terminated before reaching the longest element";
        M = M * Slist#ascent;
        word#wordLength = ascent + 1;
        wordLength = wordLength + 1;
    );
    answer := toList word;
    if not all(toList(0..n-1), i ->
        isNegativeVector(for j from 0 to n-1 list M_(j,i))) then
        error "longElement: final element does not send every simple root to a negative root";
    A.cache#"longElement" = answer;
    answer
);


-- Shared helper: enumerate W level-by-level via BFS, collecting a list
-- of {word, matrix, length} hash tables per length stratum.  Returns a
-- MutableList L where L#ell is the list of hash tables at length ell.
-- If maxLen is null, enumerates the entire group; otherwise truncates
-- at length maxLen.

weylGroupEnumerate = (A, maxLen, maxElements) -> (
    A = integralCartanMatrix(A, "weylGroupElements");
    maxElements = validateMaxElements("weylGroupElements", maxElements);
    R := ring A;
    n := numrows A;
    Slist := apply(n, i -> reflectionMatrix(A, i+1));
    isPositiveColumn := (M, i) -> (
        coeffs := for j from 0 to n-1 list M_(j,i);
        all(coeffs, c -> c >= 0) and any(coeffs, c -> c > 0)
    );
    identityM := id_(R^n);
    keyOf := M -> flatten entries M;
    seen := new MutableHashTable;
    seen#(keyOf identityM) = true;
    seenCount := 1;
    L := new MutableList;
    L#0 = {new WeylElement from {"word" => {}, "matrix" => identityM, "length" => 0}};
    ell := 0;
    done := false;
    while not done do (
        current := L#ell;
        if #current == 0 then (done = true; break);
        if maxLen =!= null and ell >= maxLen then (done = true; break);
        next := new MutableList;
        nextCount := 0;
        for h in current do (
            M := h#"matrix";
            word := h#"word";
            for i from 0 to n-1 do (
                if isPositiveColumn(M, i) then (
                    M2 := M * Slist#i;
                    key := keyOf M2;
                    if not seen#?key then (
                        seen#key = true;
                        seenCount = seenCount + 1;
                        if maxElements =!= null and seenCount > maxElements then error concatenate(
                            "weylGroupElements: computation would materialize more than ",
                            toString maxElements, " elements across its length strata");
                        next#nextCount = new WeylElement from {
                            "word"   => append(word, i+1),
                            "matrix" => M2,
                            "length" => ell + 1
                        };
                        nextCount = nextCount + 1;
                    );
                );
            );
        );
        ell = ell + 1;
        L#ell = toList next;
    );
    L
);



-- weylGroupOrder(A): cardinality of the finite Weyl group.  If m_i are its
-- exponents then |W| = product_i (m_i+1).

weylGroupOrder = method()
weylGroupOrder Matrix := A -> (
    A = integralCartanMatrix(A, "weylGroupOrder");
    requireFiniteCartanType("weylGroupOrder", A);
    product apply(weylExponents A, m -> m + 1)
);


-- weylGroupElements(A): every Weyl-group element, given as a list of
-- hash tables with keys "word" (a reduced word, 1-based), "matrix"
-- (the matrix acting on the simple-root basis), and "length".
--
-- weylGroupElements(A, k): only the elements of length exactly k.

weylGroupElements = method(Options => {MaxElements => weylEnumerationLimit})

weylGroupElements Matrix := opts -> A -> (
    A = integralCartanMatrix(A, "weylGroupElements");
    requireFiniteCartanType("weylGroupElements", A);
    order := weylGroupOrder A;
    maxElements := validateMaxElements("weylGroupElements", opts.MaxElements);
    if maxElements =!= null and order > maxElements then error concatenate(
        "weylGroupElements: refusing to materialize ", toString order,
        " elements with MaxElements => ", toString maxElements,
        "; use weylGroupElements(A,k) to request one length stratum");
    L := weylGroupEnumerate(A, null, maxElements);
    flatten for ell from 0 to #L - 1 list L#ell
);

weylGroupElements (Matrix, ZZ) := opts -> (A, k) -> (
    A = integralCartanMatrix(A, "weylGroupElements");
    if k < 0 then error "weylGroupElements: length must be nonnegative";
    maxElements := validateMaxElements("weylGroupElements", opts.MaxElements);
    if cartanType A === "finite" then (
        if k > numrows cartanPositiveRoots A then return {};
        distribution := draftWeylLengthCoefficients A;
        countAtLength := distribution#k;
        if maxElements =!= null and countAtLength > maxElements then error concatenate(
            "weylGroupElements: refusing to materialize ", toString countAtLength,
            " elements of length ", toString k);
        cumulativeCount := sum for ell from 0 to k list distribution#ell;
        if maxElements =!= null and cumulativeCount > maxElements then error concatenate(
            "weylGroupElements: computing length ", toString k,
            " would first materialize ", toString cumulativeCount,
            " elements across lower length strata");
    );
    L := weylGroupEnumerate(A, k, maxElements);
    if k >= #L then {} else L#k
);


-- Retain the coefficient vector internally for enumeration bounds.  The
-- public generating function is an ordinary polynomial, not this vector.
draftWeylLengthCoefficients = A -> (
    if A.cache#?"weylLengthCoefficients" then return A.cache#"weylLengthCoefficients";
    coefficients := {1};
    for m in weylExponents A do (
        next := new MutableList from toList((#coefficients + m):0);
        for i from 0 to #coefficients-1 do for j from 0 to m do
            next#(i+j) = next#(i+j) + coefficients#i;
        coefficients = toList next;
    );
    A.cache#"weylLengthCoefficients" = coefficients;
    coefficients
);

-- weylLengthGenerating(A): the Poincare polynomial sum_w q^length(w).

weylLengthGenerating = method()
weylLengthGenerating Matrix := A -> (
    A = integralCartanMatrix(A, "weylLengthGenerating");
    requireFiniteCartanType("weylLengthGenerating", A);
    if A.cache#?"weylLengthGenerating" then (
        cached := A.cache#"weylLengthGenerating";
        if instance(cached,RingElement) and
            ring cached === KazhdanLusztigPolynomialRing then return cached;
    );
    coefficients := draftWeylLengthCoefficients A;
    q := KazhdanLusztigPolynomialRing_0;
    answer := sum apply(toList(0..#coefficients-1),ell ->
        coefficients#ell*q^ell);
    A.cache#"weylLengthGenerating" = answer;
    answer
);


-- reducedWord(A, w)
-- Given a (possibly non-reduced) word w in simple reflections (1-based),
-- return a reduced word for the same Weyl group element.

reducedWord = method()
reducedWord (Matrix, List) := (A, w) -> (
    A = integralCartanMatrix(A, "reducedWord");
    w = validateWeylWord(A, w, "reducedWord");
    if #w == 0 then return {};
    R := ring A;
    n := numrows A;
    Slist := apply(n, i -> reflectionMatrix(A, i+1));
    M := weylElementMatrix(A, w);
    identityM := id_(R^n);
    descents := new MutableList;
    count := 0;
    while M != identityM do (
        descent := null;
        for i from 0 to n-1 do (
            column := for j from 0 to n-1 list M_(j,i);
            if isNegativeVector column then (
                descent = i;
                break;
            );
        );
        if descent === null then
            error "reducedWord: nonidentity Weyl element has no right descent";
        M = M * Slist#descent;
        descents#count = descent + 1;
        count = count + 1;
    );
    -- If w s_{i_1} ... s_{i_k}=1, then
    -- w=s_{i_k} ... s_{i_1}.
    reverse toList descents
);


-- leftDescentSet(A, w)
-- Returns the set of simple reflections s_i such that l(s_i w) < l(w).
-- Equivalently, {i : w^{-1}(alpha_i) < 0}.

leftDescentSet = method()
leftDescentSet (Matrix, List) := (A, w) -> (
    A = integralCartanMatrix(A, "leftDescentSet");
    w = validateWeylWord(A, w, "leftDescentSet");
    n := numrows A;

    -- w acts on the simple-root basis by M with M*e_j = w(alpha_j);
    -- i is a left descent iff w^{-1}(alpha_i) < 0, i.e. column i of M^{-1}
    -- contains a negative entry.
    M := weylElementMatrix(A, w);
    Minv := inverse(M ** QQ);

    descs := {};
    for i from 0 to n-1 do (
        col := for j from 0 to n-1 list Minv_(j,i);
        if any(col, c -> c < 0) then descs = append(descs, i+1);
    );
    descs
);


-- rightDescentSet(A, w)
-- Returns {i : l(w s_i) < l(w)}, equivalently {i : w(alpha_i) is a negative root}.

rightDescentSet = method()
rightDescentSet (Matrix, List) := (A, w) -> (
    A = integralCartanMatrix(A, "rightDescentSet");
    w = validateWeylWord(A, w, "rightDescentSet");
    n := numrows A;
    M := weylElementMatrix(A, w);

    descs := {};
    for i from 0 to n-1 do (
        col := for j from 0 to n-1 list M_(j,i);
        if any(col, c -> c < 0) then descs = append(descs, i+1);
    );
    descs
);


------------------------------------------------------------
------------------------------------------------------------
--
--         WEIGHT MULTIPLICITY (FREUDENTHAL)
--
------------------------------------------------------------
------------------------------------------------------------

-- weightsOfRepHash(A, lambda) : internal helper.  Returns a cached
-- HashTable of all weights of V(lambda) found by the string method
-- (keys are Dynkin-coordinate weight lists, all values `true`).
--
-- The old implementation repeatedly rescanned every weight discovered so
-- far.  Here weights are processed by their depth below the highest weight.
-- When a weight at depth d is processed, every possible upward neighbor is
-- already known, so the root-string calculation is final and no global
-- fixed-point rescan is necessary.

weightsOfRepHash = (A, lambda) -> (
    A = integralCartanMatrix(A, "weightsOfRep");
    r := validateFiniteCartanForRepresentations(A, "weightsOfRep");
    lambda = normalizeIntegralDynkinWeight(A, lambda, "weightsOfRep", true);
    cacheName := "representation.weightSets";
    if not A.cache#?cacheName then A.cache#cacheName = new MutableHashTable;
    weightSetCache := A.cache#cacheName;
    if weightSetCache#?lambda then return weightSetCache#lambda;

    alphasDynkin := for i from 0 to r-1 list (
        for j from 0 to r-1 list A_(j,i)
    );
    lam := if instance(lambda, List) then lambda
           else (for j from 0 to r-1 list lambda_(0,j));
    visited := new MutableHashTable;
    visited#lam = true;

    -- levels#d consists of weights d simple roots below lambda.
    levels := new MutableList;
    levels#0 = {lam};
    depth := 0;
    while depth < #levels do (
        for w in levels#depth do (
            for i from 0 to r-1 do (
                -- Compute p_i: steps up in alpha_i direction that are weights
                p := 0;
                test := for j from 0 to r-1 list (w#j + alphasDynkin#i#j);
                while visited#?test do (
                    p = p + 1;
                    test = for j from 0 to r-1 list (test#j + alphasDynkin#i#j);
                );
                -- String length going down: q_i = w_i + p
                qi := w#i + p;
                -- Add w - k*alpha_i for k = 1..qi
                cur := w;
                for k from 1 to qi do (
                    cur = for j from 0 to r-1 list (cur#j - alphasDynkin#i#j);
                    if not visited#?cur then (
                        visited#cur = true;
                        targetDepth := depth + k;
                        while #levels <= targetDepth do levels#(#levels) = {};
                        levels#targetDepth = append(levels#targetDepth, cur);
                    );
                );
            );
        );
        depth = depth + 1;
    );
    frozen := new HashTable from for w in keys visited list w => true;
    weightSetCache#lambda = frozen;
    frozen
);

-- weightsOfRep(A, lambda) : returns the sorted list of all weights
-- (in Dynkin coords) appearing in V(lambda).  Multiplicities are not
-- returned here; use @TO weightMultiplicity@ for those.

weightsOfRep = method(Options => {WeightFormat => "Dynkin"})
weightsOfRep (Matrix, List) := opts -> (A, lambda) -> (
    fmt := opts.WeightFormat;
    A = integralCartanMatrix(A, "weightsOfRep");
    validateFiniteCartanForRepresentations(A, "weightsOfRep");
    lambda = highestWeightFromFormat(A, lambda, fmt, "weightsOfRep");
    visited := weightsOfRepHash(A, lambda);
    ks := rsort keys visited;
    if fmt === "Dynkin" then ks
    else apply(ks, w -> fundamentalToPartition(A, w))
);


-- dominantWeightsInRep(A, lambda) : the dominant weights (Dynkin
-- coords with all entries >= 0) appearing in V(lambda), sorted in
-- reverse lexicographic order.

dominantWeightsInRep = method(Options => {WeightFormat => "Dynkin"})
dominantWeightsInRep (Matrix, List) := opts -> (A, lambda) -> (
    fmt := opts.WeightFormat;
    A = integralCartanMatrix(A, "dominantWeightsInRep");
    validateFiniteCartanForRepresentations(A, "dominantWeightsInRep");
    lambda = highestWeightFromFormat(A, lambda, fmt, "dominantWeightsInRep");
    visited := weightsOfRepHash(A, lambda);
    ks := rsort select(keys visited, w -> all(w, c -> c >= 0));
    if fmt === "Dynkin" then ks
    else apply(ks, w -> fundamentalToPartition(A, w))
);


-- weightMultiplicityTableInternal(A, lambda, who)
-- Compute and cache the complete Dynkin-coordinate character of V(lambda)
-- using Freudenthal's formula.  The returned HashTable is immutable and has
-- weights as keys and their multiplicities as values.
--
-- Freudenthal's recursion:
--   [(lambda+rho, lambda+rho) - (mu+rho, mu+rho)] * m(mu)
--     = 2 * sum_{alpha > 0} sum_{k >= 1} (mu + k*alpha, alpha) * m(mu + k*alpha)

weightMultiplicityTableInternal = (A, lambda, who) -> (
    A = integralCartanMatrix(A, who);
    r := validateFiniteCartanForRepresentations(A, who);
    lambda = normalizeIntegralDynkinWeight(A, lambda, who, true);

    cacheName := "representation.weightMultiplicityTables";
    if not A.cache#?cacheName then A.cache#cacheName = new MutableHashTable;
    tableCache := A.cache#cacheName;
    if tableCache#?lambda then return tableCache#lambda;

    -- Do coordinate conversion locally with a single inverse of A rather
    -- than recomputing fundamentalWeights for every weight and every root.
    W := fundamentalWeights A;
    dynkinToSimple := v -> for i from 0 to r-1 list (
        s := 0_QQ;
        for j from 0 to r-1 do s = s + v#j * W_(j,i);
        s
    );

    D := symmetrizer A;
    if D === null then error concatenate(who, ": Cartan matrix not symmetrizable");
    B := D * (A ** QQ);
    dot := (v, w) -> (
        s := 0_QQ;
        for i from 0 to r-1 do s = s + v#i * w#i;
        s
    );

    -- Positive roots in simple-root coords
    Rpos := cartanPositiveRoots A;
    -- For each positive root store:
    --   {simple coords, Dynkin coords, B*alpha, (alpha,alpha)}.
    -- This makes every term of the inner Freudenthal sum a pair of short
    -- list operations rather than a matrix conversion and a quadratic form.
    rootData := for row from 0 to numrows Rpos - 1 list (
        alpha := for j from 0 to r-1 list Rpos_(row,j);
        alphaDynkin := for j from 0 to r-1 list (
            s := 0;
            for i from 0 to r-1 do s = s + A_(j,i) * alpha#i;
            s
        );
        Balpha := for i from 0 to r-1 list (
            s := 0_QQ;
            for j from 0 to r-1 do s = s + B_(i,j) * alpha#j;
            s
        );
        {alpha, alphaDynkin, Balpha, dot(alpha, Balpha)}
    );

    -- Enumerate all weights of V(lambda) using the string method
    visited := weightsOfRepHash(A, lambda);

    -- Multiplicity table keyed by Dynkin-coord lists
    mult := new MutableHashTable;
    mult#lambda = 1;  -- highest weight has multiplicity 1

    -- Convert every weight once, then process in decreasing simple-root
    -- height.  Every shifted weight occurring on the right side of
    -- Freudenthal has therefore already been assigned a multiplicity.
    allWeights := keys visited;
    simpleByWeight := new MutableHashTable;
    htPairs := for w in allWeights list (
        wSimple := dynkinToSimple w;
        simpleByWeight#w = wSimple;
        ht := 0_QQ;
        for x in wSimple do ht = ht + x;
        (ht, w)
    );
    htPairs = rsort htPairs;
    allWeights = apply(htPairs, p -> p#1);

    lamSimple := simpleByWeight#lambda;
    rho := weylVector A;
    lamRhoSimple := for i from 0 to r-1 list (lamSimple#i + rho#i);
    Bchar := B;
    norm := v -> (
        s := 0_QQ;
        for i from 0 to r-1 do (
            for j from 0 to r-1 do s = s + v#i * Bchar_(i,j) * v#j;
        );
        s
    );
    lamRhoNorm := norm lamRhoSimple;

    for w in allWeights do (
        if w == lambda then continue;  -- already set to 1

        wSimple := simpleByWeight#w;
        wRhoSimple := for i from 0 to r-1 list (wSimple#i + rho#i);
        wRhoNorm := norm wRhoSimple;

        denomVal := lamRhoNorm - wRhoNorm;

        if denomVal == 0_QQ then (
            error concatenate(
                who, ": zero Freudenthal denominator below highest weight at ",
                toString w);
        );

        numerVal := 0_QQ;
        for rd in rootData do (
            alphaDynkin := rd#1;
            Balpha := rd#2;
            alphaNorm := rd#3;
            basePair := dot(wSimple, Balpha);
            k := 1;
            while true do (
                shiftedDynkin := for j from 0 to r-1 list (
                    w#j + k * alphaDynkin#j
                );
                if not visited#?shiftedDynkin then break;
                mShifted := if mult#?shiftedDynkin then mult#shiftedDynkin else 0;
                if not mult#?shiftedDynkin then
                    error concatenate(who, ": internal Freudenthal ordering failure at ",
                        toString shiftedDynkin);
                numerVal = numerVal + 2 * mShifted * (basePair + k * alphaNorm);
                k = k + 1;
            );
        );

        mAnswer := numerVal / denomVal;
        if denominator mAnswer =!= 1 or numerator mAnswer < 0 then
            error concatenate(
                who, ": invalid Freudenthal multiplicity ",
                toString mAnswer, " at weight ", toString w);
        mult#w = numerator mAnswer;
    );

    frozen := new HashTable from for w in keys mult list w => mult#w;
    tableCache#lambda = frozen;
    frozen
);


-- weightMultiplicityTable(A, lambda)
-- Return the complete formal character of V(lambda) as a Tally whose keys
-- are weights and whose values are multiplicities.

weightMultiplicityTable = method(Options => {
    WeightFormat => "Dynkin",
    Strategy => "Freudenthal"
})
weightMultiplicityTable (Matrix, List) := opts -> (A, lambda) -> (
    fmt := opts.WeightFormat;
    A = integralCartanMatrix(A, "weightMultiplicityTable");
    validateFiniteCartanForRepresentations(A, "weightMultiplicityTable");
    lambda = highestWeightFromFormat(A, lambda, fmt, "weightMultiplicityTable");
    table := if opts.Strategy === "Freudenthal" or opts.Strategy === "Auto" then
        weightMultiplicityTableInternal(A, lambda, "weightMultiplicityTable")
    else if opts.Strategy === "Kostant" then new HashTable from
        apply(weightsOfRep(A,lambda),mu ->
            mu => kostantWeightMultiplicity(A,lambda,mu))
    else error "weightMultiplicityTable: Strategy must be \"Freudenthal\", \"Kostant\", or \"Auto\"";
    new Tally from for w in keys table list (
        formattedWeight := if fmt === "Dynkin" then w
                     else fundamentalToPartition(A, w);
        formattedWeight => table#w
    )
);


-- weightMultiplicity(A, lambda, mu)
-- Compute the multiplicity of mu in V(lambda).  The complete Freudenthal
-- table is cached, so subsequent lookups for the same highest weight are
-- constant-time hash-table operations.

weightMultiplicity = method(Options => {
    WeightFormat => "Dynkin",
    Strategy => "Freudenthal"
})
weightMultiplicity (Matrix, List, List) := opts -> (A, lambda, mu) -> (
    fmt := opts.WeightFormat;
    A = integralCartanMatrix(A, "weightMultiplicity");
    validateFiniteCartanForRepresentations(A, "weightMultiplicity");
    lambda = highestWeightFromFormat(A, lambda, fmt, "weightMultiplicity");
    mu = weightFromFormat(A, mu, fmt);
    mu = normalizeIntegralDynkinWeight(A, mu, "weightMultiplicity", false);
    if opts.Strategy === "Kostant" then
        kostantWeightMultiplicity(A,lambda,mu)
    else if opts.Strategy === "Freudenthal" or opts.Strategy === "Auto" then (
        table := weightMultiplicityTableInternal(A, lambda, "weightMultiplicity");
        if table#?mu then table#mu else 0
    ) else error "weightMultiplicity: Strategy must be \"Freudenthal\", \"Kostant\", or \"Auto\""
);


------------------------------------------------------------
------------------------------------------------------------
--
--         WEIGHT ORBIT AND TENSOR DECOMPOSITION
--
------------------------------------------------------------
------------------------------------------------------------

-- weightOrbit(A, lambda)
-- Returns the full Weyl group orbit of a weight lambda (in Dynkin coords).
-- Output: list of lists (Dynkin coords), no duplicates.

weightOrbit = method(Options => {WeightFormat => "Dynkin"})
weightOrbit (Matrix, List) := opts -> (A, lambda) -> (
    A = integralCartanMatrix(A, "weightOrbit");
    r := validateFiniteCartanForRepresentations(A, "weightOrbit");
    fmt := opts.WeightFormat;
    lambda = weightFromFormat(A, lambda, fmt);
    lambda = normalizeIntegralDynkinWeight(A, lambda, "weightOrbit", false);

    -- Simple reflection on Dynkin coords:
    -- (s_i . mu)_j = mu_j - mu_i * a_{ji}
    -- (Note: this is the linear action, not the dot action)
    reflectDynkin := (mu, i) -> (
        -- i is 0-based
        for j from 0 to r-1 list (
            mu#j - mu#i * A_(j,i)
        )
    );

    orbit := new MutableHashTable;
    orbit#lambda = true;
    queue := new MutableList from {lambda};

    head := 0;
    while head < #queue do (
        mu := queue#head;
        head = head + 1;
        for i from 0 to r-1 do (
            nu := reflectDynkin(mu, i);
            if not orbit#?nu then (
                orbit#nu = true;
                queue#(#queue) = nu;
            );
        );
    );

    ks := rsort keys orbit;
    if fmt === "Dynkin" then ks
    else apply(ks, w -> fundamentalToPartition(A, w))
);


-- regularDominantizeDynkin(A, v)
-- Push an integral Dynkin-coordinate weight v to the strictly dominant
-- chamber.  Returns (sign, dominantWeight), or null when v is singular.
-- This is the alternating Weyl straightening step in Racah--Speiser.

regularDominantizeDynkin = (A, v) -> (
    r := numrows A;
    cur := v;
    sgn := 1;
    steps := 0;
    maxSteps := numrows(cartanPositiveRoots A) + 1;
    while true do (
        if any(cur, x -> x == 0) then return null;
        negIdx := null;
        for i from 0 to r-1 do (
            if cur#i < 0 then (negIdx = i; break);
        );
        if negIdx === null then return (sgn, cur);
        coeff := cur#negIdx;
        cur = for j from 0 to r-1 list (cur#j - coeff * A_(j,negIdx));
        sgn = -sgn;
        steps = steps + 1;
        if steps > maxSteps then
            error "regularDominantizeDynkin: chamber reduction failed to terminate";
    )
);


-- tensorRacahSpeiserTable(A, lambda, mu, who)
-- Return a HashTable {dominant highest weight => multiplicity} for the tensor
-- product.  Only the smaller factor's character is traversed; no Weyl-group
-- enumeration and no candidate dominant-weight box are needed.

tensorRacahSpeiserTable = (A, lambda, mu, who) -> (
    A = integralCartanMatrix(A, who);
    r := validateFiniteCartanForRepresentations(A, who);
    lambda = normalizeIntegralDynkinWeight(A, lambda, who, true);
    mu = normalizeIntegralDynkinWeight(A, mu, who, true);

    cacheName := "representation.tensorProducts";
    if not A.cache#?cacheName then A.cache#cacheName = new MutableHashTable;
    tensorCache := A.cache#cacheName;
    cacheKey := {lambda, mu};
    if tensorCache#?cacheKey then return tensorCache#cacheKey;

    dimLam := weylDimension(A, lambda);
    dimMu := weylDimension(A, mu);
    (smallHW, largeHW) := if dimLam <= dimMu then (lambda, mu)
                          else (mu, lambda);
    smallCharacter := weightMultiplicityTableInternal(A, smallHW, who);

    totals := new MutableHashTable;
    for eta in keys smallCharacter do (
        m := smallCharacter#eta;
        if m == 0 then continue;
        shiftedRho := for j from 0 to r-1 list (
            largeHW#j + eta#j + 1
        );
        straightened := regularDominantizeDynkin(A, shiftedRho);
        if straightened === null then continue;
        (sgn, dominantRho) := straightened;
        highest := for j from 0 to r-1 list (dominantRho#j - 1);
        if not all(highest, x -> x >= 0) then
            error concatenate(who, ": internal Racah--Speiser dominance failure");
        contribution := sgn * m;
        if totals#?highest then totals#highest = totals#highest + contribution
        else totals#highest = contribution;
    );

    resultPairs := {};
    for highest in keys totals do (
        multiplicity := totals#highest;
        if multiplicity < 0 then
            error concatenate(who, ": negative Racah--Speiser multiplicity ",
                toString multiplicity, " at highest weight ", toString highest);
        if multiplicity > 0 then
            resultPairs = append(resultPairs, highest => multiplicity);
    );
    frozen := new HashTable from resultPairs;
    tensorCache#cacheKey = frozen;
    -- Tensor products are symmetric, so either argument order should reuse
    -- the same straightening computation.
    tensorCache#{mu, lambda} = frozen;
    frozen
);


-- tensorDecomposition(A, lambda, mu)
-- Decompose V(lambda) tensor V(mu) by Racah--Speiser straightening of the
-- smaller factor's cached formal character.

tensorDecomposition = method(Options => {WeightFormat => "Dynkin"})
tensorDecomposition (Matrix, List, List) := opts -> (A, lambda, mu) -> (
    A = integralCartanMatrix(A, "tensorDecomposition");
    r := validateFiniteCartanForRepresentations(A, "tensorDecomposition");
    fmt := opts.WeightFormat;
    if fmt =!= "Dynkin" and fmt =!= "Partition" then
        error "tensorDecomposition: WeightFormat must be \"Dynkin\" or \"Partition\"";
    tensorTypeInfo := if fmt === "Partition" then classifyConnectedCartan A else ("other", 0, {});
    if fmt === "Partition" and tensorTypeInfo#0 === "other" then
        error "tensorDecomposition: WeightFormat => \"Partition\" requires A to be a connected Cartan of classical type A, B, C, or D";
    -- When fmt = "Partition", interpret the user's inputs as partition-form.
    lambda = highestWeightFromFormat(A, lambda, fmt, "tensorDecomposition");
    mu     = highestWeightFromFormat(A, mu,     fmt, "tensorDecomposition");
    result := tensorRacahSpeiserTable(A, lambda, mu, "tensorDecomposition");

    tensorComponents := leviConnectedComponents0 A;
    new Tally from for nu in keys result list (
        pieces := if fmt === "Partition" then {fundamentalToPartition(A, nu)}
                  else for comp in tensorComponents list (
                      for i in comp list nu#i
                  );
        (new Irrep from pieces) => result#nu
    )
);


-- tensorCoefficient(A, lambda, mu, nu)
-- Return the multiplicity c^nu_{lambda, mu} of V(nu) in V(lambda) ⊗ V(mu)
-- without computing the full decomposition.
--
-- Uses the Racah-Speiser formula:
--   V(lambda) ⊗ V(mu) = ⊕_{eta wt of V(mu)} m_mu(eta) · [V(lambda + eta)]*
-- where [V(tau)]* = sgn(w) V(w(tau + rho) - rho) if tau + rho is regular
-- (with w the unique Weyl element taking it to the strictly dominant
-- chamber), and 0 otherwise.
-- Iterating over weights of whichever of V(lambda), V(mu) is smaller.

tensorCoefficient = method(Options => {WeightFormat => "Dynkin"})
tensorCoefficient (Matrix, List, List, List) := opts -> (A, lambda, mu, nu) -> (
    A = integralCartanMatrix(A, "tensorCoefficient");
    r := validateFiniteCartanForRepresentations(A, "tensorCoefficient");
    fmt := opts.WeightFormat;
    if fmt =!= "Dynkin" and fmt =!= "Partition" then
        error "tensorCoefficient: WeightFormat must be \"Dynkin\" or \"Partition\"";
    if fmt === "Partition" then (
        info := classifyConnectedCartan A;
        if info#0 === "other" then
            error "tensorCoefficient: WeightFormat => \"Partition\" requires A to be a connected Cartan of classical type A, B, C, or D";
    );
    lambda = highestWeightFromFormat(A, lambda, fmt, "tensorCoefficient");
    mu     = highestWeightFromFormat(A, mu,     fmt, "tensorCoefficient");
    nu     = highestWeightFromFormat(A, nu,     fmt, "tensorCoefficient");
    table := tensorRacahSpeiserTable(A, lambda, mu, "tensorCoefficient");
    if table#?nu then table#nu else 0
);


------------------------------------------------------------
-- Dimensions of structured representation output
------------------------------------------------------------

-- irrepDimension(AL, V)
-- Return the dimension of an Irrep relative to the (possibly reducible)
-- finite Cartan matrix AL.  Existing Irrep values intentionally remain
-- list-like and backward compatible, so WeightFormat records how their
-- component labels should be interpreted.

irrepDimension = method(Options => {WeightFormat => "Dynkin"})
irrepDimension (Matrix, Irrep) := opts -> (AL, V) -> (
    fmt := opts.WeightFormat;
    if fmt =!= "Dynkin" and fmt =!= "Partition" then
        error "irrepDimension: WeightFormat must be \"Dynkin\" or \"Partition\"";
    if numcols AL =!= numrows AL then
        error "irrepDimension: Levi Cartan matrix must be square";

    pieces := toList V;
    if numrows AL == 0 then (
        if #pieces =!= 0 then
            error "irrepDimension: a rank-zero Levi requires V()";
        return 1;
    );

    AL = integralCartanMatrix(AL, "irrepDimension");
    validateFiniteCartanForRepresentations(AL, "irrepDimension");
    comps := leviConnectedComponents0 AL;
    if #pieces =!= #comps then
        error concatenate("irrepDimension: expected ", toString(#comps),
            " irreducible component weight(s), but received ",
            toString(#pieces));

    answer := 1;
    for c from 0 to #comps - 1 do (
        idx := comps#c;
        Mcomp := submatrix(AL, idx, idx);
        answer = answer * weylDimension(
            Mcomp, pieces#c, WeightFormat => fmt);
    );
    answer
);


-- irrepData(AL,V)
-- Validate a legacy Irrep against its finite (possibly disconnected)
-- semisimple parent and attach enough metadata to make the value
-- self-describing.  A standalone Irrep has no reductive-center data, so its
-- central character is explicitly recorded as unknown rather than guessed.

irrepData = method(Options => {WeightFormat => "Dynkin"})
irrepData (Matrix, Irrep) := opts -> (AL, V) -> (
    fmt := opts.WeightFormat;
    if fmt =!= "Dynkin" and fmt =!= "Partition" then
        error "irrepData: WeightFormat must be \"Dynkin\" or \"Partition\"";
    if numcols AL =!= numrows AL then
        error "irrepData: parent Cartan matrix must be square";

    -- This call performs the full component-count, dominance, format, and
    -- finite-type validation.  Its value is intentionally not cached in the
    -- record: dimension is a derived property of the semisimple factor.
    irrepDimension(AL, V, WeightFormat => fmt);
    if numrows AL > 0 then AL = integralCartanMatrix(AL, "irrepData");
    comps0 := leviConnectedComponents0 AL;

    new LeviIrrep from {
        "semisimple" => V,
        "parentCartan" => AL,
        "components" => apply(comps0,
            comp -> apply(comp, i -> i+1)),
        "weightFormat" => fmt,
        "centralCharacter" => null,
        "centralCharacterStatus" => "unknown",
        "centralCharacterBasis" => "not supplied"
    }
);

leviIrrepSemisimple = (V, who) -> (
    if not V#?"semisimple" or not instance(V#"semisimple", Irrep) then
        error concatenate(who, ": malformed LeviIrrep: missing semisimple Irrep");
    V#"semisimple"
);

leviIrrepFormat = (V, who) -> (
    if not V#?"weightFormat" then
        error concatenate(who, ": malformed LeviIrrep: missing weightFormat");
    fmt := V#"weightFormat";
    if fmt =!= "Dynkin" and fmt =!= "Partition" then
        error concatenate(who, ": malformed LeviIrrep: invalid weightFormat");
    fmt
);

irrepDimension (Matrix, LeviIrrep) := opts -> (AL, V) -> (
    semisimple := leviIrrepSemisimple(V, "irrepDimension");
    fmt := leviIrrepFormat(V, "irrepDimension");
    if V#?"parentCartan" then (
        parent := V#"parentCartan";
        if not instance(parent, Matrix) then
            error "irrepDimension: malformed LeviIrrep: parentCartan must be a matrix";
        if numrows AL > 0 then AL = integralCartanMatrix(AL, "irrepDimension");
        if numrows parent > 0 then
            parent = integralCartanMatrix(parent, "irrepDimension");
        if AL != parent then
            error "irrepDimension: supplied Cartan matrix does not match the LeviIrrep parentCartan";
    );
    irrepDimension(AL, semisimple, WeightFormat => fmt)
);

-- The one-argument overload uses the parent carried by the structured value.
irrepDimension LeviIrrep := opts -> V -> (
    if not V#?"parentCartan" or not instance(V#"parentCartan", Matrix) then
        error "irrepDimension: malformed LeviIrrep: missing parentCartan matrix";
    irrepDimension(V#"parentCartan", V)
);


-- decompositionDimension(AL, T)
-- Total dimension of a Tally{Irrep => ZZ}.

decompositionDimension = method(Options => {WeightFormat => "Dynkin"})
decompositionDimension (Matrix, Tally) := opts -> (AL, T) -> (
    total := 0;
    for V in keys T do (
        if not instance(V, Irrep) and not instance(V, LeviIrrep) then
            error "decompositionDimension: every Tally key must be an Irrep or LeviIrrep";
        m := T#V;
        if not instance(m, ZZ) then
            error "decompositionDimension: every multiplicity must be an integer";
        total = total + m * (
            if instance(V, LeviIrrep) then irrepDimension(AL, V)
            else irrepDimension(AL, V, WeightFormat => opts.WeightFormat));
    );
    total
);

-- A tally consisting entirely of LeviIrrep keys is already self-describing.
decompositionDimension Tally := opts -> T -> (
    total := 0;
    for V in keys T do (
        if not instance(V, LeviIrrep) then
            error "decompositionDimension: the one-argument form requires LeviIrrep keys";
        m := T#V;
        if not instance(m, ZZ) then
            error "decompositionDimension: every multiplicity must be an integer";
        total = total + m * irrepDimension V;
    );
    total
);


-- cohomologyDimensions(AL, H)
-- Convert a graded HashTable{ZZ => Tally{Irrep => ZZ}} to its graded
-- dimensions.  The three-argument overload accepts the ambient Cartan
-- matrix and its removed parabolic nodes directly.

cohomologyDimensions = method(Options => {WeightFormat => "Dynkin"})
cohomologyDimensions (Matrix, HashTable) := opts -> (AL, H) -> (
    pairs := for d in sort keys H list (
        if not instance(H#d, Tally) then
            error "cohomologyDimensions: every graded piece must be a Tally";
        d => decompositionDimension(
            AL, H#d, WeightFormat => opts.WeightFormat)
    );
    new HashTable from pairs
);

cohomologyDimensions (Matrix, List, HashTable) := opts -> (A, parabolic, H) -> (
    L := leviSubalgebra(A, parabolic);
    cohomologyDimensions(
        L#"cartanMatrix", H, WeightFormat => opts.WeightFormat)
);

cohomologyDimensions (Matrix, Set, HashTable) := opts -> (A, parabolic, H) ->
    cohomologyDimensions(A, toList parabolic, H,
        WeightFormat => opts.WeightFormat);

-- Structured Kostant output carries its own parent Cartan matrices.
cohomologyDimensions GradedDecomposition := opts -> H -> (
    new HashTable from for d in sort keys H list (
        if not instance(H#d, Tally) then
            error "cohomologyDimensions: every graded piece must be a Tally";
        d => decompositionDimension(H#d)
    )
);

representationCacheInfo = method()
representationCacheInfo Matrix := A -> (
    A = integralCartanMatrix(A, "representationCacheInfo");
    cacheSize := name -> (
        if A.cache#?name then #keys(A.cache#name) else 0
    );
    new HashTable from {
        "weightSets" => cacheSize "representation.weightSets",
        "multiplicityTables" => cacheSize "representation.weightMultiplicityTables",
        "tensorProducts" => cacheSize "representation.tensorProducts"
    }
);

clearRepresentationCache = method()
clearRepresentationCache Matrix := A -> (
    A = integralCartanMatrix(A, "clearRepresentationCache");
    A.cache#"representation.weightSets" = new MutableHashTable;
    A.cache#"representation.weightMultiplicityTables" = new MutableHashTable;
    A.cache#"representation.tensorProducts" = new MutableHashTable;
    null
);


------------------------------------------------------------
------------------------------------------------------------
--
--         KOSTANT LIE ALGEBRA COHOMOLOGY
--
------------------------------------------------------------
------------------------------------------------------------

-- kostantLieAlgCohomology(A, parabolic, lambda)
-- Computes the Kostant decomposition for the nilradical of the parabolic
-- subalgebra specified by 'parabolic'.  In infinite type this is the
-- restricted (weight-graded) cohomology convention, equivalently the
-- opposite-nilradical homology formula under contragredient duality.
--
-- If W^I_r is the set of right-minimal representatives of W/W_I, the
-- convention used here is
--   H^k_res(n_I^+, L(lambda))
--     = bigoplus_{v in W^I_r, l(v)=k} L_I(v^(-1) . lambda).
-- This is Kumar's opposite-nilradical homology formula under the standard
-- contragredient identification; the inverse matches the quotient
-- orientation used by parabolicCosetReps.
--
-- Input:
--   A         : Cartan matrix
--   parabolic : list of removed simple roots (1-based); its complement is I
--   lambda    : dominant weight in Dynkin coords
--
-- Output: GradedDecomposition{degree => Tally{Irrep => multiplicity}}.

kostantLieAlgCohomology = method(Options => {
    WeightFormat => "Dynkin",
    MaxElements => weylEnumerationLimit,
    MaxDegree => null,
    IncludeCentralCharacter => false
})
kostantLieAlgCohomology (Matrix, List, List) := opts -> (A, parabolic, lambda) -> (
    A = integralCartanMatrix(A, "kostantLieAlgCohomology");
    r := numrows A;
    if symmetrizer A === null then
        error "kostantLieAlgCohomology: A must be symmetrizable";
    parabolic = validateNodeSet(A, parabolic, "kostantLieAlgCohomology");

    fmt := opts.WeightFormat;
    if fmt =!= "Dynkin" and fmt =!= "Partition" then
        error "kostantLieAlgCohomology: WeightFormat must be \"Dynkin\" or \"Partition\"";
    includeCentral := opts.IncludeCentralCharacter;
    if includeCentral =!= true and includeCentral =!= false then
        error "kostantLieAlgCohomology: IncludeCentralCharacter must be true or false";
    maxDegree := opts.MaxDegree;
    if maxDegree =!= null and
       (not instance(maxDegree, ZZ) or maxDegree < 0) then
        error "kostantLieAlgCohomology: MaxDegree must be a nonnegative integer or null";
    if includeCentral and det(A ** QQ) == 0 then error concatenate(
        "kostantLieAlgCohomology: IncludeCentralCharacter => true requires ",
        "a nonsingular Cartan matrix; Dynkin labels alone do not determine ",
        "all full-Levi central characters for a singular realization");
    lambda = highestWeightFromFormat(A, lambda, fmt, "kostantLieAlgCohomology");

    P := set parabolic;
    Levi := {};
    for i from 1 to r do (
        if not member(i, P) then Levi = append(Levi, i);
    );
    Levi0 := for i in Levi list i-1;
    AL := submatrix(A, Levi0, Levi0);
    comps := leviConnectedComponents0 AL;
    publicComponents := apply(comps,
        comp -> apply(comp, i -> i+1));
    ambientComponents := apply(comps,
        comp -> apply(comp, i -> Levi#i));
    if fmt === "Partition" then for comp in comps do (
        componentType := (classifyConnectedCartan(
            submatrix(AL, comp, comp)))#0;
        if not member(componentType, {"A","B","C","D"}) then
            error concatenate(
                "kostantLieAlgCohomology: WeightFormat => \"Partition\" ",
                "requires every Levi component to be of finite classical ",
                "type A, B, C, or D");
    );
    inverseTransposedCartan := if includeCentral and #parabolic > 0 then
        inverse(transpose(A ** QQ)) else null;

    -- Enumerate W^L (minimal coset reps of W/W_Levi) once, already
    -- stratified by length.  A finite quotient is returned in full when no
    -- degree cap is supplied.  An infinite quotient must have an explicit
    -- cohomological degree bound,
    -- which makes every requested stratum finite and prevents an accidental
    -- unbounded traversal.
    -- A bounded traversal is finite by construction and does not need the
    -- global quotient order.  In the unbounded case, compute that order once
    -- both to certify finiteness and to stop the BFS exactly.
    quotientOrder := if maxDegree === null then
        parabolicQuotientOrder(A, Levi) else null;
    if quotientOrder === null and maxDegree === null then error concatenate(
        "kostantLieAlgCohomology: W/W_Levi is infinite; specify ",
        "MaxDegree => k to compute the complete cohomology through degree k");
    traversalBound := maxDegree;
    levels := parabolicCosetRepsByLengthInternal(
        A, traversalBound, Levi, opts.MaxElements,
        if maxDegree === null then quotientOrder else false);

    byDeg := new MutableHashTable;

    for k from 0 to #levels-1 do (
        for representative in levels#k do (
            word := representative#"word";
            -- The quotient traversal returns right-minimal representatives.
            -- With the package's Kostant convention the required element is
            -- its inverse, whose reduced word is the reverse word.
            wDotLam := dotActionOnWeight(A, reverse word, lambda);
            leviWt := for i in Levi0 list wDotLam#i;
            -- For dominant lambda, leviWt is automatically Levi-dominant;
            -- failure here signals an implementation error and must never
            -- silently discard a theorem summand.
            if not all(leviWt, c -> c >= 0) then error concatenate(
                "kostantLieAlgCohomology: internal error: a minimal coset ",
                "representative produced a non-dominant Levi weight");

            formatted := formatLeviWeight(AL, comps, leviWt, fmt);
            outputKey := if not includeCentral then formatted else (
                centralCharacter := for j in parabolic list sum(
                    for i from 0 to r-1 list
                        wDotLam#i * inverseTransposedCartan_(i,j-1));
                new LeviIrrep from {
                    "semisimple" => formatted,
                    "semisimpleDynkinWeight" => leviWt,
                    "parentCartan" => AL,
                    "components" => publicComponents,
                    "ambientComponents" => ambientComponents,
                    "weightFormat" => fmt,
                    "centralCharacter" => centralCharacter,
                    "centralCharacterStatus" => "known",
                    "centralCharacterBasis" => concatenate(
                        "removed-node ambient fundamental coweights, in removedNodes order; ",
                        "chi_j = sum_i weight_i (inverse(transpose(A)))_(i,j)"),
                    "ambientCartan" => A,
                    "ambientWeight" => wDotLam,
                    "leviNodes" => Levi,
                    "removedNodes" => parabolic
                }
            );
            if not byDeg#?k then byDeg#k = new MutableHashTable;
            inner := byDeg#k;
            if inner#?outputKey then inner#outputKey = inner#outputKey + 1
            else inner#outputKey = 1;
        );
    );

    -- Freeze each inner MutableHashTable into a Tally, then the
    -- outer one into a HashTable.
    frozen := for k in sort keys byDeg list (
        inner := byDeg#k;
        innerTally := new Tally from for key in keys inner list key => inner#key;
        k => innerTally
    );
    new GradedDecomposition from frozen
);

kostantLieAlgCohomology (Matrix, Set, List) := opts -> (A, parabolic, lambda) ->
    kostantLieAlgCohomology(A, toList parabolic, lambda,
        WeightFormat => opts.WeightFormat,
        MaxElements => opts.MaxElements,
        MaxDegree => opts.MaxDegree,
        IncludeCentralCharacter => opts.IncludeCentralCharacter);


------------------------------------------------------------
------------------------------------------------------------
--
--         STRUCTURED PARABOLIC DATA
--
------------------------------------------------------------
------------------------------------------------------------

-- leviSubalgebra(A, parabolic)
-- Returns a hash table with structured data about the Levi factor:
--   "cartanMatrix" => the Levi Cartan submatrix
--   "nodes"        => list of Levi node indices (1-based)
--   "embedding"    => map from Levi simple roots to full algebra simple roots
--   "rank"         => rank of the Levi

leviSubalgebra = method()
leviSubalgebra (Matrix,List) := (A, parabolic) -> (
    A = integralCartanMatrix(A, "leviSubalgebra");
    r := numrows A;

    parabolic = validateNodeSet(A, parabolic, "leviSubalgebra");
    P := set parabolic;
    Levi := {};
    for i from 1 to r do (
        if not member(i, P) then Levi = append(Levi, i);
    );
    Levi0 := for i in Levi list i-1;

    AL := submatrix(A, Levi0, Levi0);
    embeddingMutable := mutableMatrix(ZZ, #Levi, r);
    if #Levi > 0 then for i from 0 to #Levi-1 do
        embeddingMutable_(i, Levi0#i) = 1;
    embedding := matrix embeddingMutable;
    components0 := leviConnectedComponents0 AL;
    components := apply(components0, comp -> apply(comp, i -> Levi#i));
    realizationRank := 2*r - rank(A ** QQ);

    new LeviData from {
        "cartanMatrix"          => AL,
        "ambientCartanMatrix"   => A,
        "nodes"                 => Levi,
        "nodes0"                => Levi0,
        "removedNodes"          => parabolic,
        "parabolic"             => parabolic,
        "rank"                  => #Levi,
        "semisimpleRank"        => #Levi,
        "ambientRank"           => r,
        "realizationRank"       => realizationRank,
        "centerDimension"       => realizationRank - #Levi,
        "embedding"             => embedding,
        "components"            => components,
        "components0"           => components0,
        "centralCharactersKept" => false
    }
);

leviSubalgebra (Matrix,Set) := (A,parabolic) ->
    leviSubalgebra(A,toList parabolic);

parabolicDiagram = method()
parabolicDiagram (Matrix, List) := (A, parabolic) -> (
    A = integralCartanMatrix(A, "parabolicDiagram");
    parabolic = validateNodeSet(A, parabolic, "parabolicDiagram");
    P := set parabolic;
    r := numrows A;
    roles := demark(", ", for i from 1 to r list
        toString i | if member(i, P) then " [removed/grading]" else " [Levi]");
    "Node roles: " | roles | newline | dynkinDiagram A
);

parabolicDiagram (Matrix, Set) := (A, parabolic) ->
    parabolicDiagram(A, toList parabolic);

parabolicData = method()
parabolicData (Matrix, List) := (A, parabolic) -> (
    A = integralCartanMatrix(A, "parabolicData");
    parabolic = validateNodeSet(A, parabolic, "parabolicData");
    P := set parabolic;
    L := leviSubalgebra(A, parabolic);
    finite := cartanType A === "finite";
    nilRoots := if finite then nilradicalRoots(A, parabolic) else null;
    new HashTable from {
        "ambientCartanMatrix" => A,
        "ambientType" => cartanType A,
        "removedNodes" => parabolic,
        "leviNodes" => L#"nodes",
        "levi" => L,
        "gradingVector" => for i from 1 to numrows A list
            if member(i, P) then 1 else 0,
        "diagram" => parabolicDiagram(A, parabolic),
        "nilradicalRoots" => nilRoots,
        "nilradicalDimension" => if finite then numrows nilRoots else null,
        "finiteNilradicalDataAvailable" => finite,
        "nodeConvention" => "1-based"
    }
);

parabolicData (Matrix, Set) := (A, parabolic) ->
    parabolicData(A, toList parabolic);


-- nilradicalRoots(A, parabolic)
-- Returns a matrix whose rows are the positive roots lying in the nilradical
-- (i.e., those with positive parabolic degree) in simple-root coords.

nilradicalRoots = method()
nilradicalRoots (Matrix, List) := (A, parabolic) -> (
    A = integralCartanMatrix(A, "nilradicalRoots");
    r := numrows A;

    parabolic = validateNodeSet(A, parabolic, "nilradicalRoots");
    parIdx := apply(parabolic, i -> i-1);

    R := cartanPositiveRoots A;
    L := {};
    for row from 0 to numrows R - 1 do (
        v := for j from 0 to r-1 list R_(row,j);
        deg := 0;
        for j in parIdx do deg = deg + v#j;
        if deg > 0 then L = append(L, v);
    );

    if #L == 0 then (
        matrix mutableMatrix(ZZ, 0, r)
    ) else (
        matrix L
    )
);

nilradicalRoots (Matrix, Set) := (A, parabolic) ->
    nilradicalRoots(A, toList parabolic);


-- parabolicDegrees(A, parabolic)
-- Returns a list of {root, degree} for each positive root.

parabolicDegrees = method()
parabolicDegrees (Matrix, List) := (A, parabolic) -> (
    A = integralCartanMatrix(A, "parabolicDegrees");
    r := numrows A;

    parabolic = validateNodeSet(A, parabolic, "parabolicDegrees");
    parIdx := apply(parabolic, i -> i-1);

    R := cartanPositiveRoots A;
    result := {};
    for row from 0 to numrows R - 1 do (
        v := for j from 0 to r-1 list R_(row,j);
        deg := 0;
        for j in parIdx do deg = deg + v#j;
        result = append(result, {v, deg});
    );

    result
);

parabolicDegrees (Matrix, Set) := (A, parabolic) ->
    parabolicDegrees(A, toList parabolic);


------------------------------------------------------------
------------------------------------------------------------
--
--         AFFINE LIE ALGEBRA FUNCTIONALITY
--
------------------------------------------------------------
------------------------------------------------------------

-- affineRootSnapshot(A, H)
-- A : r x r symmetrizable affine Cartan matrix of corank 1
-- H : positive integer height bound
-- ImaginaryMultiplicity : null (default, computes exact values), or a
-- positive integer used only to assert the exact untwisted value r-1.
-- Twisted periodic values are computed by sparse Peterson recursion.
-- Returns HashTable with keys "real", "imag", "delta", "mult".

rootsToIntegralMatrix = (roots, r) -> (
    if #roots == 0 then matrix mutableMatrix(ZZ, 0, r)
    else matrix roots
);

decorateRootSnapshot = (A, H, typ, algorithm, raw) -> (
    base := {
        "real"                  => raw#"real",
        "imag"                  => raw#"imag",
        "mult"                  => raw#"mult",
        "cartanMatrix"          => A,
        "rank"                  => numrows A,
        "heightBound"           => H,
        "completeThroughHeight" => H,
        "globallyComplete"      => if raw#?"globallyComplete"
                                      then raw#"globallyComplete" else false,
        "type"                  => typ,
        "algorithm"             => algorithm,
        "basis"                 => "SimpleRoots"
    };
    if raw#?"delta" then base = append(base, "delta" => raw#"delta");
    new RootSnapshot from base
);

-- The finite backend is deliberately kept separate from the affine and
-- indefinite implementations.  It enumerates the finite positive root
-- system once, then applies an optional height cutoff while retaining the
-- same typed metadata and query semantics as the infinite-type snapshots.
finiteRootSnapshotInternal = (A, H) -> (
    A = integralCartanMatrix(A, "rootSnapshot");
    requireFiniteCartanType("rootSnapshot", A);
    r := numrows A;
    allReal := cartanPositiveRoots A;
    allRows := for i from 0 to numrows allReal - 1 list
        for j from 0 to r-1 list lift(allReal_(i,j), ZZ);
    maximumHeight := if #allRows == 0 then 0
                     else max apply(allRows, beta -> sum beta);
    requestedHeight := if H === null then maximumHeight else H;
    realRows := if H === null then allRows
                else select(allRows, beta -> sum beta <= H);
    raw := new HashTable from {
        "real" => rootsToIntegralMatrix(realRows, r),
        "imag" => rootsToIntegralMatrix({}, r),
        "mult" => new HashTable from for beta in realRows list beta => 1,
        "globallyComplete" => (H === null or H >= maximumHeight)
    };
    decorateRootSnapshot(A, requestedHeight, "finite", "FiniteEnumeration", raw)
);

truncateRootSnapshot = (snap, H) -> (
    if H > snap#"heightBound" then
        error "truncateRootSnapshot: requested height exceeds cached snapshot";
    r := snap#"rank";
    selected := kind -> (
        R := snap#kind;
        for i from 0 to numrows R-1 list
            for j from 0 to r-1 list lift(R_(i,j), ZZ)
    );
    real := select(selected "real", beta -> sum beta <= H);
    imag := select(selected "imag", beta -> sum beta <= H);
    keep := real | imag;
    mults := snap#"mult";
    rawPairs := {
        "real" => rootsToIntegralMatrix(real, r),
        "imag" => rootsToIntegralMatrix(imag, r),
        "mult" => new HashTable from for beta in keep list beta => mults#beta
    };
    if snap#?"delta" then rawPairs = append(rawPairs, "delta" => snap#"delta");
    answer := decorateRootSnapshot(
        snap#"cartanMatrix", H, snap#"type", snap#"algorithm",
        new HashTable from rawPairs);
    if snap#?"untwistedMultiplicity" then answer = new RootSnapshot from append(
        for k in keys answer list k => answer#k,
        "untwistedMultiplicity" => snap#"untwistedMultiplicity");
    answer
);

affineRootSnapshot = method(Options => {ImaginaryMultiplicity => null})
affineRootSnapshot (Matrix, ZZ) := opts -> (A, H) -> (
    if H < 1 then error "affineRootSnapshot: H must be at least 1";

    A = integralCartanMatrix(A, "affineRootSnapshot");
    r := numrows A;
    if symmetrizer A === null then
        error "affineRootSnapshot: Cartan matrix must be symmetrizable";
    if cartanType A =!= "affine" then
        error "affineRootSnapshot: Cartan matrix must be of affine type";
    if #leviConnectedComponents0 A != 1 then
        error "affineRootSnapshot: affine Cartan matrix must be indecomposable";

    requestedImagMult := opts.ImaginaryMultiplicity;
    if requestedImagMult =!= null and
       (not instance(requestedImagMult, ZZ) or requestedImagMult < 1) then
        error "affineRootSnapshot: ImaginaryMultiplicity must be a positive integer";

    cacheName := "affineRootSnapshot-1.3";
    if not A.cache#?cacheName then A.cache#cacheName = new MutableHashTable;
    snapshotCache := A.cache#cacheName;
    if snapshotCache#?"snapshot" then (
        cached := snapshotCache#"snapshot";
        if cached#"heightBound" >= H then (
            -- Validate a supplied assertion even when the numerical data is cached.
            if requestedImagMult =!= null then (
                if not cached#?"untwistedMultiplicity" then
                    error "affineRootSnapshot: a constant ImaginaryMultiplicity is not supported for twisted affine type";
                if requestedImagMult != cached#"untwistedMultiplicity" then
                    error "affineRootSnapshot: ImaginaryMultiplicity conflicts with the exact untwisted value";
            );
            return truncateRootSnapshot(cached, H);
        );
    );

    AQQ := A ** QQ;
    if rank AQQ =!= r-1 then
        error "affineRootSnapshot: affine Cartan matrix must have corank one";

    -- Use the internal real-root enumerator: the public bounded root API
    -- dispatches to this snapshot in affine type.
    Rall := cartanPositiveRealRootsByHeight(A, H);

    realList := {};
    for row from 0 to numrows Rall - 1 do (
        v := for j from 0 to r-1 list lift(Rall_(row, j), ZZ);
        h := 0;
        for x in v do h = h + x;
        if h <= H then realList = append(realList, v);
    );

    rootsToRingMatrix := roots -> (
        if #roots == 0 then matrix mutableMatrix(ZZ, 0, r)
        else matrix roots
    );
    Rreal := rootsToRingMatrix realList;

    -- Root coefficients of the primitive null root form ker(A).
    Kmod := kernel AQQ;
    G    := gens Kmod;
    if numcols G =!= 1 then error "affineRootSnapshot: nullity of A is not equal to one";

    deltaRat := for i from 0 to r-1 list G_(i,0);
    denoms := for x in deltaRat list denominator x;
    L := 1;
    for d in denoms do L = lcm(L, d);
    -- Coerce to ZZ so downstream keys hash consistently with real-root keys.
    deltaInt := for x in deltaRat list lift(L * x, ZZ);

    g := 0;
    for x in deltaInt do g = gcd(g, x);
    if g < 0 then g = -g;
    if g == 0 then error "affineRootSnapshot: null root computation failed (all zeros)";
    deltaInt = for x in deltaInt list (x // g);

    if all(deltaInt, x -> x < 0) then
        deltaInt = for x in deltaInt list -x;
    if not all(deltaInt, x -> x > 0) then
        error "affineRootSnapshot: null root is not strictly positive; input is not an indecomposable affine GCM";

    hDelta := 0;
    for x in deltaInt do hDelta = hDelta + x;
    if hDelta <= 0 then error "affineRootSnapshot: computed null root has non-positive height";

    imagList := {};
    m := 1;
    while m * hDelta <= H do (
        imagList = append(imagList, for x in deltaInt list (m * x));
        m = m + 1;
    );

    Rimag := rootsToRingMatrix imagList;

    untwisted := isUntwistedAffineCartan(A, deltaInt);
    imagMultTable := new MutableHashTable;
    if untwisted then (
        exactMult := r - 1;
        if requestedImagMult =!= null and requestedImagMult != exactMult then
            error concatenate(
                "affineRootSnapshot: ImaginaryMultiplicity conflicts with the exact untwisted value ",
                toString exactMult);
        for v in imagList do imagMultTable#v = exactMult;
    ) else (
        if requestedImagMult =!= null then
            error "affineRootSnapshot: a constant ImaginaryMultiplicity is not supported for twisted affine type; exact periodic multiplicities are computed automatically";
        -- Twisted affine multiplicities can depend on k modulo the twist
        -- order.  Compute them exactly with sparse Peterson recursion.
        peterson := indefiniteRootSnapshotSparsePeterson(A, H);
        petersonMult := peterson#"mult";
        for v in imagList do (
            if not petersonMult#?v then
                error("affineRootSnapshot: Peterson backend did not find null root " | toString v);
            imagMultTable#v = petersonMult#v;
        );
    );

    realSeen := new MutableHashTable;
    for v in realList do realSeen#v = true;
    for v in imagList do if realSeen#?v then
        error("affineRootSnapshot: internal real/imaginary overlap at " | toString v);

    multPairs := join(
        for v in realList list v => 1,
        for v in imagList list v => imagMultTable#v
    );

    rawSnapshot := new HashTable from {
        "real"  => Rreal,
        "imag"  => Rimag,
        "delta" => matrix{deltaInt},
        "mult"  => new HashTable from multPairs
    };
    snapshot := decorateRootSnapshot(A, H, "affine", "AffineExact", rawSnapshot);
    if untwisted then snapshot = new RootSnapshot from append(
        for k in keys snapshot list k => snapshot#k,
        "untwistedMultiplicity" => r-1);
    snapshotCache#"snapshot" = snapshot;
    snapshot
);


-- Snapshot displays use numerical simple-root coordinates.  A bounded
-- preview retains only the first few rows, rather than constructing a
-- second copy of a potentially large root table.  This changes no data
-- or completeness guarantees in the snapshot itself.
draftRootSnapshotDisplayLimit := 20;

draftRootSnapshotRows = (snap,limit) -> (
    if not snap#?"real" or not snap#?"imag" or not snap#?"mult" then
        error "rootSnapshotByHeight: input is not a root snapshot HashTable (missing \"real\", \"imag\", or \"mult\" key)";
    Rreal := snap#"real";
    Rimag := snap#"imag";
    mults := snap#"mult";
    rCols := max(numcols Rreal, numcols Rimag);
    rows := {};
    for spec in {{Rreal,"real"},{Rimag,"imaginary"}} do (
        R := spec#0;
        kind := spec#1;
        for i from 0 to numrows R - 1 do (
            v := for j from 0 to rCols-1 list lift(R_(i,j),ZZ);
            mv := if mults#?v then mults#v
                  else if kind === "real" then 1 else "?";
            rows = append(rows,{sum v,v,mv,kind});
            if limit =!= null then (
                rows = sort rows;
                if #rows > limit then rows = take(rows,limit);
            );
        );
    );
    sort rows
);

draftRootSnapshotTuple = beta ->
    "(" | concatenate between(", ",apply(beta,toString)) | ")";

draftRootSnapshotTable = rows -> (
    header := {"height","root","multiplicity","type"};
    body := apply(rows,row -> {
        toString row#0,draftRootSnapshotTuple(row#1),
        toString row#2,row#3});
    netList(prepend(header, body), Boxes => true, HorizontalSpace => 1, Alignment => Center)
);

draftRootSnapshotDisplay = snap -> (
    if not snap#?"real" or not snap#?"imag" or not snap#?"mult" then
        return net "RootSnapshot (root data unavailable)";
    complete := snap#?"globallyComplete" and snap#"globallyComplete";
    bound := if snap#?"completeThroughHeight" then snap#"completeThroughHeight"
             else if snap#?"heightBound" then snap#"heightBound" else null;
    title := if complete then "All positive roots"
             else if bound =!= null then "Positive roots through height " | toString bound
             else "Positive roots";
    if snap#?"type" then title = title | " (" | toString snap#"type" | " type)";
    rows := draftRootSnapshotRows(snap,draftRootSnapshotDisplayLimit);
    total := numrows(snap#"real") + numrows(snap#"imag");
    if total == 0 then return stack {title,"No positive roots in this range."};
    omitted := total - #rows;
    parts := {title,draftRootSnapshotTable rows,
        "Root tuples are coefficients in the simple-root basis."};
    if omitted > 0 then parts = join(parts,{
        toString omitted | " additional computed roots not displayed.",
        "Use rootSnapshotByHeight to display all " | toString total | " roots."});
    stack parts
);

-- The explicit table view displays every root, without the default
-- preview limit.  Its row order agrees with the snapshot display.
rootSnapshotByHeight = method()
rootSnapshotByHeight HashTable := snap ->
    draftRootSnapshotTable draftRootSnapshotRows(snap,null);

validateRootSnapshot = (snap, who) -> (
    if not instance(snap, HashTable) or
       not snap#?"real" or not snap#?"imag" or not snap#?"mult" then
        error(who | ": expected a root snapshot");
    r := max(numcols(snap#"real"), numcols(snap#"imag"));
    if snap#?"rank" and snap#"rank" != r then
        error(who | ": inconsistent root-snapshot rank metadata");
    r
);

rootSnapshotRoots = method(Options => {Kind => "All"})
rootSnapshotRoots HashTable := opts -> snap -> (
    r := validateRootSnapshot(snap, "rootSnapshotRoots");
    kind := opts.Kind;
    if kind === "Real" or kind === "real" then snap#"real"
    else if kind === "Imaginary" or kind === "imag" or kind === "imaginary" then
        snap#"imag"
    else if kind === "All" or kind === "all" then (
        Rreal := snap#"real";
        Rimag := snap#"imag";
        if numrows Rreal == 0 then Rimag
        else if numrows Rimag == 0 then Rreal
        else Rreal || Rimag
    ) else error "rootSnapshotRoots: Kind must be \"All\", \"Real\", or \"Imaginary\""
);

normalizeSnapshotRoot = (snap, beta, who) -> (
    r := validateRootSnapshot(snap, who);
    if not instance(beta, List) or #beta =!= r or
       not all(beta, x -> instance(x, ZZ)) then
        error(who | ": root must be a list of rank-many integers in the simple-root basis");
    beta
);

isRootInSnapshot = method()
isRootInSnapshot (HashTable, List) := (snap, beta) -> (
    beta = normalizeSnapshotRoot(snap, beta, "isRootInSnapshot");
    (snap#"mult")#?beta
);

rootMultiplicity = method()
rootMultiplicity (HashTable, List) := (snap, beta) -> (
    beta = normalizeSnapshotRoot(snap, beta, "rootMultiplicity");
    if not isPositiveVector beta then return 0;
    if snap#?"completeThroughHeight" and sum beta > snap#"completeThroughHeight" then (
        if snap#?"globallyComplete" and snap#"globallyComplete" then return 0;
        error concatenate(
            "rootMultiplicity: height ", toString sum beta,
            " exceeds the certified snapshot bound ",
            toString snap#"completeThroughHeight");
    );
    if (snap#"mult")#?beta then (snap#"mult")#beta else 0
);

rootSnapshotCacheInfo = method()
rootSnapshotCacheInfo Matrix := A -> (
    A = integralCartanMatrix(A, "rootSnapshotCacheInfo");
    affineHeight := null;
    if A.cache#?"affineRootSnapshot-1.3" then (
        affineCache := A.cache#"affineRootSnapshot-1.3";
        if affineCache#?"snapshot" then
            affineHeight = (affineCache#"snapshot")#"heightBound";
    );
    indefiniteHeights := new HashTable from {};
    if A.cache#?"indefiniteRootSnapshot-1.3" then (
        indefiniteCache := A.cache#"indefiniteRootSnapshot-1.3";
        indefiniteHeights = new HashTable from for alg in keys indefiniteCache list
            alg => (indefiniteCache#alg)#"heightBound";
    );
    new HashTable from {
        "affineHeight" => affineHeight,
        "indefiniteHeights" => indefiniteHeights
    }
);

clearRootSnapshotCache = method()
clearRootSnapshotCache Matrix := A -> (
    A = integralCartanMatrix(A, "clearRootSnapshotCache");
    A.cache#"affineRootSnapshot-1.3" = new MutableHashTable;
    A.cache#"indefiniteRootSnapshot-1.3" = new MutableHashTable;
    null
);


-- affineLeviHighestWeights(A, d, parabolic, H)
-- Parabolic decomposition for affine Lie algebras, with multiplicities.
-- The Levi (complement of parabolic) must be finite-type; for any
-- non-empty parabolic in an affine Cartan matrix this is automatic.

affineLeviHighestWeights = method(Options => {
    WeightFormat => "Dynkin",
    ImaginaryMultiplicity => null
})
affineLeviHighestWeights (Matrix, ZZ, List, ZZ) := opts -> (A, d, parabolic, H) -> (
    A = integralCartanMatrix(A, "affineLeviHighestWeights");
    r := numrows A;
    if cartanType A =!= "affine" then
        error "affineLeviHighestWeights: Cartan matrix must be of affine type";
    if #(leviConnectedComponents0 A) =!= 1 then
        error "affineLeviHighestWeights: affine Cartan matrix must be indecomposable";
    if d < 0 then error "affineLeviHighestWeights: degree d must be >= 0";
    if H < 1 then error "affineLeviHighestWeights: height bound H must be >= 1";

    fmt := opts.WeightFormat;
    if fmt =!= "Dynkin" and fmt =!= "Partition" then
        error "affineLeviHighestWeights: WeightFormat must be \"Dynkin\" or \"Partition\"";

    parabolic = validateNodeSet(A, parabolic, "affineLeviHighestWeights");
    P := set parabolic;
    parIdx := apply(toList P, i -> i-1);
    Levi := {};
    for i from 1 to r do (
        if not member(i, P) then Levi = append(Levi, i);
    );
    if d > 0 and #parIdx == 0 then return tally {};
    Levi0 := for i in Levi list i-1;
    AL := submatrix(A, Levi0, Levi0);
    if #Levi > 0 and cartanType AL =!= "finite" then
        error "affineLeviHighestWeights: Levi subalgebra is not of finite type; cannot decompose into finite-dim irreducibles";
    comps := leviConnectedComponents0 AL;
    if d == 0 then return degreeZeroDerivedLeviTally(A, AL, comps, fmt);

    requiredH := if #Levi == 0 then d
                 else leviRootHeightCompletenessBound(A, d, parIdx, Levi0);
    if H < requiredH then
        error concatenate(
            "affineLeviHighestWeights: height bound is not certified complete for degree ",
            toString d, "; use H >= ", toString requiredH);

    snap := affineRootSnapshot(A, H,
        ImaginaryMultiplicity => opts.ImaginaryMultiplicity);
    Rreal := snap#"real";
    Rimag := snap#"imag";
    mults := snap#"mult";

    rootDegree := v -> (
        s := 0;
        for j in parIdx do s = s + v#j;
        s
    );

    -- Collect Levi Dynkin labels of all roots of parabolic degree d,
    -- with each root contributing mult(root) copies.
    leviLabel := v -> (
        for j in Levi0 list (
            s := 0;
            for i from 0 to r-1 do s = s + A_(j, i) * v#i;
            s
        )
    );

    Wall := {};
    R := if numrows Rimag == 0 then Rreal else Rreal || Rimag;
    for row from 0 to numrows R - 1 do (
        v := for j from 0 to r-1 list R_(row, j);
        if rootDegree v != d then continue;
        m := if mults#?v then mults#v else 1;
        w := leviLabel v;
        for i from 1 to m do Wall = append(Wall, w);
    );

    if #Wall == 0 then return tally {};

    pieces := for pair in decomposeLeviWeightList(AL, Wall) list (
        mu := pair#0;
        multMu := pair#1;
        formatLeviWeight(AL, comps, mu, fmt) => multMu
    );
    new Tally from pieces
);


------------------------------------------------------------
------------------------------------------------------------
--
--         INDEFINITE KAC-MOODY FUNCTIONALITY
--
------------------------------------------------------------
------------------------------------------------------------

-- Common exact integral form data used by both indefinite backends.
-- If D symmetrizes A, a common rescaling makes d=diag(D) and B=DA
-- integral.  Peterson's recurrence is unchanged by this rescaling, while
-- (rho,beta)=sum_i d_i beta_i avoids ever inverting B.  In particular,
-- singular indefinite Cartan matrices are supported.
indefiniteRootFormData = A -> (
    A = integralCartanMatrix(A, "indefiniteRootSnapshot");
    r := numrows A;
    D := symmetrizer A;
    if D === null then
        error "indefiniteRootSnapshot: Cartan matrix must be symmetrizable";

    AZZ := matrix apply(entries A, row -> for x in row list lift(x, ZZ));
    BQQ := D * (A ** QQ);
    scale := 1;
    for i from 0 to r-1 do (
        scale = lcm(scale, denominator D_(i,i));
        for j from 0 to r-1 do
            scale = lcm(scale, denominator BQQ_(i,j));
    );
    d := for i from 0 to r-1 list lift(scale * D_(i,i), ZZ);
    B := for i from 0 to r-1 list
        for j from 0 to r-1 list lift(scale * BQQ_(i,j), ZZ);
    {AZZ, d, B}
);

indefiniteVectorGCD = v -> (
    g := 0;
    for x in v do g = gcd(g, x);
    if g < 0 then -g else g
);

indefiniteRootsToMatrix = (A, L, r) -> (
    if #L == 0 then matrix mutableMatrix(ZZ, 0, r)
    else matrix toList L
);

indefiniteConnectedSupport = (A, v) -> (
    r := #v;
    supp := select(toList(0..r-1), i -> v#i > 0);
    if #supp == 0 then return false;
    seen := new MutableHashTable;
    Q := new MutableList from {first supp};
    seen#(first supp) = true;
    head := 0;
    while head < #Q do (
        i := Q#head;
        head = head + 1;
        for j in supp do (
            if not seen#?j and (A_(i,j) != 0 or A_(j,i) != 0) then (
                seen#j = true;
                Q#(#Q) = j;
            );
        );
    );
    #keys seen == #supp
);

-- Sparse Peterson backend.  Rather than visit every lattice composition,
-- it convolves only the already-known nonzero support of c(beta).  Every
-- multiple of a known root is also seeded explicitly; this is necessary for
-- divisor-only and isotropic cases where the Peterson numerator can vanish.
indefiniteRootSnapshotSparsePeterson = (A, H) -> (
    r := numrows A;
    data := indefiniteRootFormData A;
    d := data#1;
    B := data#2;

    bTimes := v -> for i from 0 to r-1 list (
        s := 0;
        for j from 0 to r-1 do s = s + B#i#j * v#j;
        s
    );
    dot := (v, w) -> (
        s := 0;
        for i from 0 to r-1 do s = s + v#i * w#i;
        s
    );

    cTable := new MutableHashTable;
    bTable := new MutableHashTable;
    multTable := new MutableHashTable;
    realRoots := new MutableList;
    imagRoots := new MutableList;
    cByHeight := new MutableList from for h from 0 to H list new MutableList;
    rootsByHeight := new MutableList from for h from 0 to H list new MutableList;

    for i from 0 to r-1 do (
        beta := for j from 0 to r-1 list if i == j then 1 else 0;
        cTable#beta = 1_QQ;
        bTable#beta = for j from 0 to r-1 list B#j#i;
        multTable#beta = 1;
        realRoots#(#realRoots) = beta;
        (cByHeight#1)#(#(cByHeight#1)) = beta;
        (rootsByHeight#1)#(#(rootsByHeight#1)) = beta;
    );

    for h from 2 to H do (
        half := h // 2;
        STable := new MutableHashTable;

        -- For unequal heights the reverse ordered pair gives the same term,
        -- hence factor 2.  At equal heights the full Cartesian product is
        -- already the ordered Peterson sum.
        for h1 from 1 to half do (
            h2 := h - h1;
            factor := if h1 < h2 then 2 else 1;
            for gamma in cByHeight#h1 do (
                cgamma := cTable#gamma;
                for delta in cByHeight#h2 do (
                    pair := dot(gamma, bTable#delta);
                    if pair == 0 then continue;
                    beta := for i from 0 to r-1 list gamma#i + delta#i;
                    term := factor * pair * cgamma * cTable#delta;
                    if STable#?beta then STable#beta = STable#beta + term
                    else STable#beta = term;
                );
            );
        );

        -- If beta=n*alpha for a known root alpha then c(beta) may be nonzero
        -- even when every contribution along the ray has pairing zero.
        for n from 2 to h do (
            if h % n =!= 0 then continue;
            for alpha in rootsByHeight#(h // n) do (
                beta := for x in alpha list n * x;
                if not STable#?beta then STable#beta = 0_QQ;
            );
        );

        for beta in sort keys STable do (
            bBeta := bTimes beta;
            norm := dot(beta, bBeta);
            denom := norm - 2 * dot(beta, d);
            S := STable#beta;

            -- Proper-divisor part of c(beta).  It must be computed before
            -- the singular-denominator branch and uses only lower heights.
            g := indefiniteVectorGCD beta;
            sumDiv := 0_QQ;
            for n from 2 to g do (
                if g % n =!= 0 then continue;
                divBeta := for x in beta list x // n;
                if multTable#?divBeta then
                    sumDiv = sumDiv + multTable#divBeta / n;
            );

            cBeta := if denom == 0 then (
                if S =!= 0_QQ then
                    error("indefiniteRootSnapshot: singular Peterson identity violated at " | toString beta);
                sumDiv
            ) else S / denom;
            mBeta := cBeta - sumDiv;

            if denominator mBeta =!= 1 then
                error("indefiniteRootSnapshot: nonintegral multiplicity at " | toString beta);
            mBetaZZ := numerator mBeta;
            if mBetaZZ < 0 then
                error("indefiniteRootSnapshot: negative multiplicity at " | toString beta);

            if cBeta =!= 0_QQ then (
                cTable#beta = cBeta;
                bTable#beta = bBeta;
                (cByHeight#h)#(#(cByHeight#h)) = beta;
            );
            if mBetaZZ > 0 then (
                if norm > 0 and mBetaZZ != 1 then
                    error("indefiniteRootSnapshot: positive-norm root has multiplicity other than 1 at " | toString beta);
                multTable#beta = mBetaZZ;
                (rootsByHeight#h)#(#(rootsByHeight#h)) = beta;
                if norm > 0 then realRoots#(#realRoots) = beta
                else imagRoots#(#imagRoots) = beta;
            );
        );
    );

    new HashTable from {
        "real" => indefiniteRootsToMatrix(A, realRoots, r),
        "imag" => indefiniteRootsToMatrix(A, imagRoots, r),
        "mult" => new HashTable from for beta in keys multTable list beta => multTable#beta
    }
);

-- Lexicographic comparison used to take one representative from an unordered
-- Peterson decomposition in the graded-ascent backend.
indefiniteLexLessOrEqual = (u, v) -> (
    for i from 0 to #u-1 do (
        if u#i < v#i then return true;
        if u#i > v#i then return false;
    );
    true
);

-- Backus--Connick--Lin graded ascent on one connected Cartan component.
-- Normaliz enumerates the bounded lattice points of
--     { beta >= 0 : A beta <= 0, height(beta) <= H }
-- directly.  This is equivalent to the Hilbert-basis chamber step but avoids
-- nonunique semigroup decompositions and includes the positive orthant in the
-- cone definition (which the published prototype did not do in general).
indefiniteRootSnapshotBCLConnected = (A, H) -> (
    r := numrows A;
    data := indefiniteRootFormData A;
    AZZ := data#0;
    d := data#1;
    B := data#2;
    ARows := entries AZZ;

    bTimes := v -> for i from 0 to r-1 list (
        s := 0;
        for j from 0 to r-1 do s = s + B#i#j * v#j;
        s
    );
    dot := (v, w) -> (
        s := 0;
        for i from 0 to r-1 do s = s + v#i * w#i;
        s
    );

    rootMult := new MutableHashTable;
    rootC := new MutableHashTable;
    rootNorm := new MutableHashTable;
    realSet := new MutableHashTable;
    realByHeight := new MutableList from for h from 0 to H list new MutableList;
    imagByHeight := new MutableList from for h from 0 to H list new MutableList;

    registerRoot := (v, m, c, normv) -> (
        if rootMult#?v then return false;
        rootMult#v = m;
        rootC#v = c;
        rootNorm#v = normv;
        h := sum v;
        if normv > 0 then (
            if m != 1 then
                error("indefiniteRootSnapshot: invalid real root at " | toString v);
            realSet#v = true;
            (realByHeight#h)#(#(realByHeight#h)) = v;
        ) else (
            (imagByHeight#h)#(#(imagByHeight#h)) = v;
        );
        true
    );

    pingpong := (seed, m, c, normSeed) -> (
        if rootMult#?seed then return null;
        Q := new MutableList from {seed};
        registerRoot(seed, m, c, normSeed);
        head := 0;
        while head < #Q do (
            v := Q#head;
            head = head + 1;
            for i from 0 to r-1 do (
                k := 0;
                for j from 0 to r-1 do k = k + ARows#i#j * v#j;
                if k == 0 then continue;
                w := for j from 0 to r-1 list if j == i then v#j - k else v#j;
                hw := sum w;
                if hw < 1 or hw > H or any(w, x -> x < 0) then continue;
                if registerRoot(w, m, c, normSeed) then Q#(#Q) = w;
            );
        );
        null
    );

    -- Generate every positive real root up to the height cutoff.
    for i from 0 to r-1 do (
        e := for j from 0 to r-1 list if i == j then 1 else 0;
        pingpong(e, 1, 1_QQ, 2 * d#i);
    );

    lookupC := v -> (
        if rootC#?v then return rootC#v;
        g := indefiniteVectorGCD v;
        if g <= 1 then return 0_QQ;
        primitive := for x in v list x // g;
        if realSet#?primitive then 1_QQ / g else 0_QQ
    );

    properDivisorContribution := beta -> (
        g := indefiniteVectorGCD beta;
        s := 0_QQ;
        for n from 2 to g do (
            if g % n =!= 0 then continue;
            b := for x in beta list x // n;
            if rootMult#?b then s = s + rootMult#b / n;
        );
        s
    );

    -- Direct bounded lattice-point enumeration in the fundamental imaginary
    -- chamber.  A final homogenizing coordinate equal to 1 is returned.
    coneIneq := (-AZZ) || id_(ZZ^r);
    zeroCol := map(ZZ^(2*r), ZZ^1, 0);
    heightBound := matrix {toList(r:-1) | {H}};
    normalizFunction := optionalPackageFunction("Normaliz", "normaliz");
    chamberRows := (normalizFunction((coneIneq | zeroCol) || heightBound,
            "inhom_inequalities"))#"gen";
    chamberByHeight := new MutableList from for h from 0 to H list new MutableList;
    for row from 0 to numrows chamberRows - 1 do (
        if chamberRows_(row,r) != 1 then
            error "indefiniteRootSnapshot: unexpected recession generator in bounded chamber";
        beta := for j from 0 to r-1 list chamberRows_(row,j);
        h := sum beta;
        if h == 0 then continue;
        if not indefiniteConnectedSupport(AZZ, beta) then continue;
        (chamberByHeight#h)#(#(chamberByHeight#h)) = beta;
    );

    -- Graded ascent computes one fundamental-chamber representative of each
    -- imaginary Weyl orbit, then ping-pong fills that orbit up to height H.
    for h from 2 to H do (
        for beta in chamberByHeight#h do (
            if rootMult#?beta then continue;
            Bbeta := bTimes beta;
            normBeta := dot(beta, Bbeta);
            denom := normBeta - 2 * dot(beta, d);
            if denom >= 0 then
                error("indefiniteRootSnapshot: invalid fundamental imaginary candidate " | toString beta);
            half := h // 2;
            S := 0_QQ;

            -- Positive-norm c-support consists of multiples n*alpha of
            -- primitive real roots alpha, with c(n*alpha)=1/n.
            for hAlpha from 1 to half do (
                for alpha in realByHeight#hAlpha do (
                    normAlpha := rootNorm#alpha;
                    pairAlphaBeta := dot(alpha, Bbeta);
                    maxN := half // hAlpha;
                    for i from 0 to r-1 do (
                        if alpha#i > 0 then maxN = min(maxN, beta#i // alpha#i);
                    );
                    for n from 1 to maxN do (
                        gamma := for x in alpha list n * x;
                        delta := for i from 0 to r-1 list beta#i - gamma#i;
                        if sum gamma == sum delta and not indefiniteLexLessOrEqual(gamma, delta) then continue;
                        cDelta := lookupC delta;
                        if cDelta == 0_QQ then continue;
                        factor := if gamma === delta then 1 else 2;
                        S = S + factor * (pairAlphaBeta - n * normAlpha) * cDelta;
                    );
                );
            );

            -- Nonpositive-norm c-support consists of actual imaginary roots.
            for hGamma from 2 to half do (
                for gamma in imagByHeight#hGamma do (
                    contained := true;
                    for i from 0 to r-1 do if gamma#i > beta#i then (contained = false; break);
                    if not contained then continue;
                    delta := for i from 0 to r-1 list beta#i - gamma#i;
                    if sum gamma == sum delta and not indefiniteLexLessOrEqual(gamma, delta) then continue;
                    cDelta := lookupC delta;
                    if cDelta == 0_QQ then continue;
                    factor := if gamma === delta then 1 else 2;
                    S = S + factor * (dot(gamma, Bbeta) - rootNorm#gamma)
                        * rootC#gamma * cDelta;
                );
            );

            cBeta := S / denom;
            mBeta := cBeta - properDivisorContribution(beta);
            if denominator mBeta =!= 1 or numerator mBeta <= 0 then
                error("indefiniteRootSnapshot: invalid graded-ascent multiplicity at " | toString beta);
            pingpong(beta, numerator mBeta, cBeta, normBeta);
        );
    );

    realRoots := new MutableList;
    imagRoots := new MutableList;
    for h from 1 to H do (
        for beta in realByHeight#h do realRoots#(#realRoots) = beta;
        for beta in imagByHeight#h do imagRoots#(#imagRoots) = beta;
    );
    new HashTable from {
        "real" => indefiniteRootsToMatrix(A, realRoots, r),
        "imag" => indefiniteRootsToMatrix(A, imagRoots, r),
        "mult" => new HashTable from for beta in keys rootMult list beta => rootMult#beta
    }
);

-- Run connected components independently: roots never mix distinct Dynkin
-- components, and this prevents Normaliz from forming a needless Cartesian
-- product of their chamber cones.
indefiniteRootSnapshotBCL = (A, H) -> (
    r := numrows A;
    data := indefiniteRootFormData A;
    AZZ := data#0;
    comps := leviConnectedComponents0 AZZ;
    if #comps == 1 then return indefiniteRootSnapshotBCLConnected(A, H);

    realRoots := new MutableList;
    imagRoots := new MutableList;
    multTable := new MutableHashTable;
    for comp in comps do (
        AC := submatrix(A, comp, comp);
        snap := indefiniteRootSnapshotBCLConnected(AC, H);
        for kind in {"real", "imag"} do (
            R := snap#kind;
            for row from 0 to numrows R - 1 do (
                small := for j from 0 to #comp-1 list lift(R_(row,j), ZZ);
                beta := toList(r:0);
                beta = for j from 0 to r-1 list (
                    p := position(comp, i -> i == j);
                    if p === null then 0 else small#p
                );
                if kind === "real" then realRoots#(#realRoots) = beta
                else imagRoots#(#imagRoots) = beta;
                multTable#beta = (snap#"mult")#small;
            );
        );
    );
    new HashTable from {
        "real" => indefiniteRootsToMatrix(A, realRoots, r),
        "imag" => indefiniteRootsToMatrix(A, imagRoots, r),
        "mult" => new HashTable from for beta in keys multTable list beta => multTable#beta
    }
);

indefiniteAlgorithmName = alg -> (
    if alg === "BackusConnickLin" or alg === "BCL" or alg === "GradedAscent" then
        "BackusConnickLin"
    else if alg === "SparsePeterson" or alg === "Peterson" or alg === "Sparse" then
        "SparsePeterson"
    else error concatenate(
        "indefiniteRootSnapshot: Algorithm must be \"BackusConnickLin\" ",
        "(or \"BCL\"/\"GradedAscent\") or \"SparsePeterson\" ",
        "(or \"Peterson\"/\"Sparse\")")
);

indefiniteRootSnapshotInternal = (A, H, requestedAlgorithm) -> (
    A = integralCartanMatrix(A, "indefiniteRootSnapshot");
    alg := if requestedAlgorithm === "Auto" then (
        r := numrows A;
        -- Sparse Peterson has the lower startup cost in symmetric rank 2,
        -- while chamber ascent is already faster for nonsymmetric rank 2.
        if (r <= 2 and A == transpose A and H <= 80) or
           (r == 3 and H <= 10) then
            "SparsePeterson" else "BackusConnickLin"
    ) else indefiniteAlgorithmName requestedAlgorithm;
    cacheName := "indefiniteRootSnapshot-1.3";
    if not A.cache#?cacheName then A.cache#cacheName = new MutableHashTable;
    cache := A.cache#cacheName;
    if cache#?alg and (cache#alg)#"heightBound" >= H then
        return truncateRootSnapshot(cache#alg, H);
    raw := if alg === "BackusConnickLin" then indefiniteRootSnapshotBCL(A, H)
        else indefiniteRootSnapshotSparsePeterson(A, H);
    snap := decorateRootSnapshot(A, H, cartanType A, alg, raw);
    -- Retain only the largest snapshot for each backend.  A smaller request
    -- is served by truncation, so a sequence of height queries cannot grow
    -- the matrix cache without bound.
    cache#alg = snap;
    snap
);

-- indefiniteRootSnapshot(A,H,Algorithm=>...)
-- Auto uses sparse Peterson for the very small low-rank range where its lower
-- startup cost wins, and BCL graded ascent otherwise.  Snapshots are cached on
-- A by the selected backend and height because Levi decompositions commonly
-- request the same data again.
indefiniteRootSnapshot = method(Options => {Algorithm => "Auto"})
indefiniteRootSnapshot (Matrix, ZZ) := opts -> (A, H) -> (
    if H < 1 then error "indefiniteRootSnapshot: H must be at least 1";
    A = integralCartanMatrix(A, "indefiniteRootSnapshot");
    if symmetrizer A === null then
        error "indefiniteRootSnapshot: Cartan matrix must be symmetrizable";
    typ := cartanType A;
    if typ =!= "affine" and typ =!= "indefinite" then
        error "indefiniteRootSnapshot: Cartan matrix must be of affine or indefinite type";
    indefiniteRootSnapshotInternal(A, H, opts.Algorithm)
);

-- Unified public root-snapshot interface.  The one-argument form returns the
-- complete positive root system in finite type.  Infinite root systems need
-- a height bound.  The bounded form chooses the exact finite, exact
-- indecomposable-affine, or automatic infinite backend from cartanType(A).
-- Backend-specific options remain available through the specialized APIs.
rootSnapshot = method()
rootSnapshot Matrix := A -> (
    A = integralCartanMatrix(A, "rootSnapshot");
    if symmetrizer A === null then
        error "rootSnapshot: Cartan matrix must be symmetrizable";
    if cartanType A =!= "finite" then
        error "rootSnapshot: infinite root systems require a positive height bound";
    finiteRootSnapshotInternal(A, null)
);

rootSnapshot (Matrix, ZZ) := (A, H) -> (
    if H < 1 then error "rootSnapshot: height bound must be at least 1";
    A = integralCartanMatrix(A, "rootSnapshot");
    if symmetrizer A === null then
        error "rootSnapshot: Cartan matrix must be symmetrizable";
    typ := cartanType A;
    if typ === "finite" then finiteRootSnapshotInternal(A, H)
    else if typ === "affine" and #(leviConnectedComponents0 A) == 1 then
        affineRootSnapshot(A, H)
    else if typ === "affine" or typ === "indefinite" then
        indefiniteRootSnapshotInternal(A, H, "Auto")
    else error "rootSnapshot: Cartan matrix must be of finite, affine, or indefinite type"
);

-- indefiniteLeviHighestWeights(A, d, parabolic, H)
-- Parabolic decomposition for indefinite Kac-Moody algebras

indefiniteLeviHighestWeights = method(Options => {
    WeightFormat => "Dynkin",
    Algorithm => "Auto"
})
indefiniteLeviHighestWeights (Matrix, ZZ, List, ZZ) := opts -> (A, d, parabolic, H) -> (
    A = integralCartanMatrix(A, "indefiniteLeviHighestWeights");
    r := numrows A;
    if cartanType A =!= "indefinite" then
        error "indefiniteLeviHighestWeights: Cartan matrix must be of indefinite type";
    if d < 0 then error "indefiniteLeviHighestWeights: degree d must be >= 0";
    if H < 1 then error "indefiniteLeviHighestWeights: height bound H must be >= 1";

    fmt := opts.WeightFormat;
    if fmt =!= "Dynkin" and fmt =!= "Partition" then
        error "indefiniteLeviHighestWeights: WeightFormat must be \"Dynkin\" or \"Partition\"";

    parabolic = validateNodeSet(A, parabolic, "indefiniteLeviHighestWeights");
    P := set parabolic;
    parIdx := apply(toList P, i -> i-1);
    Levi := {};
    for i from 1 to r do (
        if not member(i, P) then Levi = append(Levi, i);
    );
    if d > 0 and #parIdx == 0 then return tally {};
    Levi0 := for i in Levi list i-1;
    AL := submatrix(A, Levi0, Levi0);
    if #Levi > 0 and cartanType AL =!= "finite" then
        error "indefiniteLeviHighestWeights: Levi subalgebra is not of finite type; cannot decompose into finite-dim irreducibles";
    comps := leviConnectedComponents0 AL;
    if d == 0 then return degreeZeroDerivedLeviTally(A, AL, comps, fmt);

    requiredH := if #Levi == 0 then d
                 else leviRootHeightCompletenessBound(A, d, parIdx, Levi0);
    if H < requiredH then
        error concatenate(
            "indefiniteLeviHighestWeights: height bound is not certified complete for degree ",
            toString d, "; use H >= ", toString requiredH);

    snap := indefiniteRootSnapshot(A, H, Algorithm => opts.Algorithm);
    Rreal := snap#"real";
    Rimag := snap#"imag";
    mults := snap#"mult";

    rootDegree := v -> (
        s := 0;
        for j in parIdx do s = s + v#j;
        s
    );

    leviLabel := v -> (
        for j in Levi0 list (
            s := 0;
            for i from 0 to r-1 do s = s + A_(j, i) * v#i;
            s
        )
    );

    -- Collect Levi Dynkin labels of every root of parabolic degree d,
    -- repeated according to its root multiplicity.
    Wall := {};
    R := if numrows Rimag == 0 then Rreal else Rreal || Rimag;
    for row from 0 to numrows R - 1 do (
        v := for j from 0 to r-1 list R_(row, j);
        if rootDegree v != d then continue;
        m := if mults#?v then mults#v else 1;
        w := leviLabel v;
        for i from 1 to m do Wall = append(Wall, w);
    );

    if #Wall == 0 then return tally {};

    pieces := for pair in decomposeLeviWeightList(AL, Wall) list (
        mu := pair#0;
        multMu := pair#1;
        formatLeviWeight(AL, comps, mu, fmt) => multMu
    );
    new Tally from pieces
);

-- A certified sufficient height for the infinite-type Levi routines.
-- This makes the unified interface independent of a backend cutoff while
-- retaining the completeness guarantee enforced by the specialized APIs.
automaticLeviHeightBound = (A, d, parabolic, who) -> (
    if d == 0 or #parabolic == 0 then return 1;
    r := numrows A;
    P := set parabolic;
    parIdx := apply(toList P, i -> i-1);
    leviIdx := select(toList(0..r-1), i -> not member(i+1, P));
    if #leviIdx == 0 then return max(1, d);
    AL := submatrix(A, leviIdx, leviIdx);
    if cartanType AL =!= "finite" then
        error(who | ": Levi subalgebra is not of finite type; cannot decompose into finite-dimensional irreducibles");
    leviRootHeightCompletenessBound(A, d, parIdx, leviIdx)
);

-- The specialized affine routine requires an indecomposable affine Cartan
-- matrix.  A reducible matrix with one affine component is nevertheless a
-- valid affine-type GCM, so the unified wrapper performs the same character
-- decomposition from its componentwise automatic snapshot.
leviHighestWeightsFromSnapshot = (A, d, parabolic, fmt, snap, who) -> (
    r := numrows A;
    P := set parabolic;
    parIdx := apply(toList P, i -> i-1);
    if d > 0 and #parIdx == 0 then return tally {};
    Levi := select(toList(1..r), i -> not member(i, P));
    Levi0 := apply(Levi, i -> i-1);
    AL := submatrix(A, Levi0, Levi0);
    if #Levi > 0 and cartanType AL =!= "finite" then
        error(who | ": Levi subalgebra is not of finite type; cannot decompose into finite-dimensional irreducibles");
    comps := leviConnectedComponents0 AL;
    if d == 0 then return degreeZeroDerivedLeviTally(A, AL, comps, fmt);

    rootDegree := beta -> sum apply(parIdx, j -> beta#j);
    leviLabel := beta -> for j in Levi0 list
        sum for i from 0 to r-1 list A_(j,i) * beta#i;
    mults := snap#"mult";
    Wall := {};
    for kind in {"real", "imag"} do (
        R := snap#kind;
        for row from 0 to numrows R-1 do (
            beta := for j from 0 to r-1 list lift(R_(row,j), ZZ);
            if rootDegree beta != d then continue;
            w := leviLabel beta;
            m := if mults#?beta then mults#beta else 1;
            for copy from 1 to m do Wall = append(Wall, w);
        );
    );
    if #Wall == 0 then return tally {};
    new Tally from for pair in decomposeLeviWeightList(AL, Wall) list
        formatLeviWeight(AL, comps, pair#0, fmt) => pair#1
);

-- Unified public parabolic decomposition.  The Cartan type, infinite-type
-- backend, and certified height cutoff are inferred.  The type-specific
-- routines above are implementation details, not separate user APIs.
leviHighestWeights = method(Options => {WeightFormat => "Dynkin"})
leviHighestWeights (Matrix, ZZ, List) := opts -> (A, d, parabolic) -> (
    A = integralCartanMatrix(A, "leviHighestWeights");
    if symmetrizer A === null then
        error "leviHighestWeights: Cartan matrix must be symmetrizable";
    if d < 0 then error "leviHighestWeights: degree must be nonnegative";
    parabolic = validateNodeSet(A, parabolic, "leviHighestWeights");
    fmt := opts.WeightFormat;
    if fmt =!= "Dynkin" and fmt =!= "Partition" then
        error "leviHighestWeights: WeightFormat must be \"Dynkin\" or \"Partition\"";
    typ := cartanType A;
    -- At degree zero the automatic height helper is intentionally bypassed,
    -- so perform the same finite-Levi preflight here.  Besides giving the
    -- right mathematical domain check, this keeps a private backend name out
    -- of errors raised through the public wrapper.
    if d == 0 and typ =!= "finite" then (
        P0 := set parabolic;
        levi0 := select(toList(0..numrows(A)-1), i -> not member(i+1,P0));
        AL0 := submatrix(A,levi0,levi0);
        if #levi0 > 0 and cartanType AL0 =!= "finite" then
            error "leviHighestWeights: Levi subalgebra is not of finite type; cannot decompose into finite-dimensional irreducibles";
    );
    certifiedHeight := null;
    if typ === "finite" then
        cartanLeviHighestWeights(A, d, parabolic, WeightFormat => fmt)
    else if typ === "affine" and #(leviConnectedComponents0 A) == 1 then (
        certifiedHeight = automaticLeviHeightBound(A, d, parabolic, "leviHighestWeights");
        affineLeviHighestWeights(A, d, parabolic, certifiedHeight, WeightFormat => fmt)
    ) else if typ === "indefinite" then (
        certifiedHeight = automaticLeviHeightBound(A, d, parabolic, "leviHighestWeights");
        indefiniteLeviHighestWeights(A, d, parabolic, certifiedHeight, WeightFormat => fmt)
    ) else if typ === "affine" then (
        certifiedHeight = automaticLeviHeightBound(A, d, parabolic, "leviHighestWeights");
        leviHighestWeightsFromSnapshot(A, d, parabolic, fmt,
            rootSnapshot(A, certifiedHeight), "leviHighestWeights")
    ) else error "leviHighestWeights: Cartan matrix must be of finite, affine, or indefinite type"
);

leviHighestWeights (Matrix, ZZ, Set) := opts -> (A, d, parabolic) ->
    leviHighestWeights(A, d, toList parabolic, WeightFormat => opts.WeightFormat);


-- Parent-aware data and representation-theoretic constructions.

-------------------------------------------------------------------------
-- Datum
-------------------------------------------------------------------------

-- Parent-aware Cartan data, realizations, kmRoot data, and lattice elements.
--
-- Equality-bearing objects contain only immutable canonical data.
-- In particular, no Matrix is stored inside an equality-bearing object:
-- Macaulay2 matrices are mutable hash tables and are therefore identity-
-- hashed.  Matrix accessors reconstruct matrices from immutable entry lists.

--------------------------------------------------------------------------
-- Immutable public types
--------------------------------------------------------------------------

CartanDatum = new Type of HashTable
Realization = new Type of HashTable
RootDatum = new Type of HashTable

KMRoot = new Type of HashTable
KMCoroot = new Type of HashTable
KMWeight = new Type of HashTable
KMCoweight = new Type of HashTable

-- Explicit == methods document the intended mathematical equality.  Strong
-- equality (===), which Macaulay2 hash tables use for keys, is already
-- content equality for these immutable HashTable descendants.
CartanDatum == CartanDatum := (D,E) -> D === E
Realization == Realization := (R,S) -> R === S
RootDatum == RootDatum := (D,E) -> D === E
KMRoot == KMRoot := (x,y) -> x === y
KMCoroot == KMCoroot := (x,y) -> x === y
KMWeight == KMWeight := (x,y) -> x === y
KMCoweight == KMCoweight := (x,y) -> x === y

--------------------------------------------------------------------------
-- Validation and canonicalization helpers
--------------------------------------------------------------------------

draftQQ = x -> (
    q := try (x + 0_QQ) else
        error "KacMoody: coordinates must be rational numbers";
    if denominator q == 1 then numerator q else q
)

-- Public node labels use the same equality convention as the crystal
-- interface: exact identity first, then mathematical equality when that
-- comparison is defined.  This lets an integral label stored over QQ be
-- addressed by the corresponding ZZ input without permitting two
-- indistinguishable labels in one datum.
draftSameNodeLabel = (x,y) -> (
    if x === y then true
    else try ((x == y) === true) else false
)

draftRationalVector = (v,n,who) -> (
    if instance(v, Matrix) then (
        if numrows v == 1 then v = flatten entries v
        else if numcols v == 1 then v = flatten entries transpose v
        else error(who | ": a matrix coordinate vector must have one row or one column")
    );
    if not instance(v,List) then error(who | ": coordinates must be a list or a one-row/one-column matrix");
    if #v != n then error concatenate(who, ": expected ", toString n,
        " coordinates, but received ", toString(#v));
    apply(v,draftQQ)
)

draftIntegralVector = (v,n,who) -> (
    q := draftRationalVector(v,n,who);
    if not all(q,x -> instance(x,ZZ)) then
        error(who | ": kmRoot-lattice coordinates must be integers");
    q
)

draftIntegralCartan = (A,who) -> (
    if not instance(A,Matrix) then error(who | ": expected a matrix");
    if numrows A != numcols A then error(who | ": Cartan matrix must be square");
    qrows := apply(entries(A ** QQ), row -> apply(row,draftQQ));
    if not all(flatten qrows,x -> instance(x,ZZ)) then
        error(who | ": Cartan matrix entries must be integers");
    AZZ := matrix(ZZ,qrows);
    if not isCartanMatrix AZZ then
        error(who | ": matrix is not a generalized Cartan matrix");
    AZZ
)

draftDot = (u,v) -> (
    if #u != #v then error "KacMoody: internal dot-product length mismatch";
    sum apply(u,v,(x,y) -> x*y)
)

draftAdd = (u,v) -> apply(u,v,(x,y) -> draftQQ(x+y))
draftScale = (a,u) -> apply(u,x -> draftQQ(a*x))

draftRealizationOfRootDatum = D -> D#"realization"

-- Immutable realizations store coordinate rows as lists.  For positive
-- rank, matrix(QQ,rows) recovers the shape.  At rank zero the empty row list
-- does not remember the ambient dimension, so reconstruct the genuine
-- 0-by-d matrix explicitly.  This is essential for nontrivial tori.
draftRealizationCoordinateMatrix = (R,key) -> (
    r := (R#"cartanDatum")#"rank";
    d := R#"dimension";
    if r == 0 then matrix mutableMatrix(QQ,0,d)
    else matrix(QQ,R#key)
)

draftSimpleRootMatrix = R ->
    draftRealizationCoordinateMatrix(R,"simpleRootCoordinates")

draftSimpleCorootMatrix = R ->
    draftRealizationCoordinateMatrix(R,"simpleCorootCoordinates")

draftRequireSameParent = (x,y,who) -> (
    if parentDatum x =!= parentDatum y then
        error(who | ": operands belong to different kmRoot data");
    parentDatum x
)

draftComponentRecord = (C,nodeLabels) -> new HashTable from {
    "nodes" => C#"nodes",
    "nodeLabels" => apply(C#"nodes",i -> nodeLabels#(i-1)),
    "type" => C#"type",
    "family" => C#"family",
    "rank" => C#"rank",
    "label" => C#"label",
    "canonicalNodeOrder" => C#"canonicalNodeOrder"
}

--------------------------------------------------------------------------
-- CartanDatum
--------------------------------------------------------------------------

cartanDatum = method(Options => {NodeLabels => null})

cartanDatum Matrix := opts -> A -> (
    -- The empty generalized Cartan matrix is the rank-zero finite datum.
    -- It occurs naturally as the derived Levi of a Borel parabolic.
    if instance(A,Matrix) and numrows A == 0 and numcols A == 0 then (
        if opts.NodeLabels =!= null and opts.NodeLabels =!= {} then
            error "cartanDatum: a rank-zero datum requires NodeLabels => {}";
        return new CartanDatum from {
            "cartanEntries" => {},
            "rank" => 0,
            "matrixRank" => 0,
            "corank" => 0,
            "type" => "finite",
            "symmetrizable" => true,
            "symmetrizerDiagonal" => {},
            "components" => {},
            "nodeLabels" => {},
            "nodeConvention" => "1-based"
        };
    );
    A = draftIntegralCartan(A,"cartanDatum");
    r := numrows A;
    labels := if opts.NodeLabels === null then toList(1..r)
        else (
            if not instance(opts.NodeLabels,List) or #opts.NodeLabels != r then
                error "cartanDatum: NodeLabels must be a list with one entry per node";
            for i from 0 to r-1 do for j from i+1 to r-1 do
                if draftSameNodeLabel(opts.NodeLabels#i,opts.NodeLabels#j) then
                    error concatenate(
                        "cartanDatum: NodeLabels must be pairwise distinct ",
                        "under mathematical equality");
            opts.NodeLabels
        );
    info := cartanData A;
    D := symmetrizer A;
    new CartanDatum from {
        "cartanEntries" => entries A,
        "rank" => r,
        "matrixRank" => rank(A ** QQ),
        "corank" => numrows A - rank(A ** QQ),
        "type" => cartanType A,
        "symmetrizable" => (D =!= null),
        "symmetrizerDiagonal" => if D === null then null
            else apply(numrows A,i -> draftQQ D_(i,i)),
        "components" => apply(info#"components",C -> draftComponentRecord(C,labels)),
        "nodeLabels" => labels,
        "nodeConvention" => if opts.NodeLabels === null then "1-based" else "user-supplied"
    }
)

cartanDatum CartanDatum := opts -> D -> (
    if opts.NodeLabels =!= null and opts.NodeLabels =!= D#"nodeLabels" then
        error "cartanDatum: an existing CartanDatum cannot be relabeled in place";
    D
)
cartanDatum Realization := opts -> R -> cartanDatum(R#"cartanDatum",NodeLabels=>opts.NodeLabels)
cartanDatum RootDatum := opts -> D -> cartanDatum(D#"cartanDatum",NodeLabels=>opts.NodeLabels)

draftFiniteCartanFromTypeString = s -> (
    if #s < 2 then error "cartanDatum: a type string must look like \"A3\" or \"E8\"";
    letter := substring(s,0,1);
    rankString := substring(s,1,#s-1);
    digits := ascii rankString;
    if #digits == 0 or any(digits,c -> c < 48 or c > 57) then
        error("cartanDatum: the rank in \"" | s | "\" must contain digits only");
    rankValue := 0;
    for c in digits do rankValue = 10*rankValue + c - 48;
    if rankValue < 1 then error "cartanDatum: rank must be positive";
    if letter === "A" then cartanA(rankValue+1)
    else if letter === "B" then cartanB rankValue
    else if letter === "C" then cartanC rankValue
    else if letter === "D" then cartanD rankValue
    else if letter === "E" then cartanE rankValue
    else if letter === "F" and rankValue == 4 then cartanF4()
    else if letter === "G" and rankValue == 2 then cartanG2()
    else error("cartanDatum: unsupported finite Dynkin type \"" | s | "\"")
)

cartanDatum String := opts -> s -> cartanDatum(
    draftFiniteCartanFromTypeString s,NodeLabels=>opts.NodeLabels)

datumCartanMatrix = method()
datumMatrixCache := new MutableHashTable
datumInfrastructureCacheSize = () -> #keys datumMatrixCache
clearDatumInfrastructureCache = () -> (datumMatrixCache = new MutableHashTable; null)
datumCartanMatrix CartanDatum := D -> (
    if datumMatrixCache#?D then return datumMatrixCache#D;
    A := matrix(ZZ,D#"cartanEntries");
    datumMatrixCache#D = A;
    A
)
datumCartanMatrix Realization := R -> datumCartanMatrix(R#"cartanDatum")
datumCartanMatrix RootDatum := D -> datumCartanMatrix(D#"cartanDatum")

toString CartanDatum := D -> concatenate(
    "CartanDatum(rank=",toString D#"rank",
    ", type=",D#"type",
    ", components=",if #D#"components" == 0 then "trivial"
        else demark(" + ",apply(D#"components",C -> C#"label")),")")
net CartanDatum := D -> net toString D

--------------------------------------------------------------------------
-- Realizations
--
-- Row i of simpleCorootCoordinates is alpha_i^vee in a basis of h.
-- Row j of simpleRootCoordinates is alpha_j in the dual basis of h^*.
-- Thus C * transpose(R) = A, i.e. <alpha_j,alpha_i^vee> = a_ij.
--------------------------------------------------------------------------

realization = method()
standardRealization = method()

realization (CartanDatum,Matrix,Matrix) := (D,R,C) -> (
    A := datumCartanMatrix D;
    r := D#"rank";
    if numrows R != r or numrows C != r then
        error "realization: kmRoot and kmCoroot matrices must each have one row per Cartan node";
    if numcols R != numcols C then
        error "realization: kmRoot and kmCoroot matrices must use dual bases of the same dimension";
    d := numcols R;
    if d < r then error "realization: ambient Cartan dimension must be at least the Cartan rank";
    RQ := R ** QQ;
    CQ := C ** QQ;
    if rank RQ != r then error "realization: simple roots must be linearly independent";
    if rank CQ != r then error "realization: simple coroots must be linearly independent";
    if CQ * transpose RQ != A ** QQ then
        error "realization: required pairing C * transpose(R) = A failed";
    minDimension := 2*r - D#"matrixRank";
    new Realization from {
        "cartanDatum" => D,
        "dimension" => d,
        "simpleRootCoordinates" => apply(entries RQ,row -> apply(row,draftQQ)),
        "simpleCorootCoordinates" => apply(entries CQ,row -> apply(row,draftQQ)),
        "minimalDimension" => minDimension,
        "isMinimal" => (d == minDimension),
        "basisConvention" => "rows in dual realization bases; C*transpose(R)=A"
    }
)

realization (Matrix,Matrix,Matrix) := (A,R,C) ->
    realization(cartanDatum A,R,C)

standardRealization CartanDatum := D -> (
    A := datumCartanMatrix D;
    r := D#"rank";
    k := D#"corank";

    -- Begin with the kmRoot evaluations on the independent simple coroots.
    -- Add standard coordinate columns until the simple-kmRoot rows have full
    -- rank.  Exactly corank(A) columns are needed.
    R := transpose A;
    currentRank := rank(R ** QQ);
    j := 0;
    while currentRank < r do (
        e := matrix table(r,1,(i,l) -> if i == j then 1 else 0);
        candidate := R | e;
        candidateRank := rank(candidate ** QQ);
        if candidateRank > currentRank then (
            R = candidate;
            currentRank = candidateRank;
        );
        j = j + 1;
        if j > r then error "standardRealization: failed to construct a complement";
    );
    if numcols R != r+k then
        error "standardRealization: internal complement dimension mismatch";
    C := if k == 0 then id_(ZZ^r)
         else id_(ZZ^r) | matrix mutableMatrix(ZZ,r,k);
    realization(D,R,C)
)

standardRealization Matrix := A -> standardRealization cartanDatum A
standardRealization RootDatum := D -> D#"realization"

realization RootDatum := D -> D#"realization"

toString Realization := R -> concatenate(
    "Realization(rank=",toString((R#"cartanDatum")#"rank"),
    ", dim(h)=",toString R#"dimension",
    if R#"isMinimal" then ", minimal)" else ", nonminimal)")
net Realization := R -> net toString R

--------------------------------------------------------------------------
-- P/Q and RootDatum
--------------------------------------------------------------------------

draftPmodQ = D -> (
    A := datumCartanMatrix D;
    S := smithNormalForm(A,ChangeMatrix=>{false,false});
    n := min(numrows S,numcols S);
    diagonal := apply(n,i -> abs S_(i,i));
    nonzero := select(diagonal,d -> d != 0);
    torsion := select(nonzero,d -> d > 1);
    freeRank := D#"rank" - #nonzero;
    new HashTable from {
        "presentation" => "coker(A : Q -> P) in simple-kmRoot/fundamental-kmWeight bases",
        "smithDiagonal" => diagonal,
        "torsionInvariants" => torsion,
        "freeRank" => freeRank,
        "finite" => (freeRank == 0),
        "order" => if freeRank == 0 then product torsion else null
    }
)

draftGlobalFormMetadata = (form,D) -> (
    if form === "SimplyConnected" then new HashTable from {
        "name" => form,
        "characterLattice" => "P",
        "cocharacterLattice" => "Qvee",
        "validated" => true
    }
    else if form === "Adjoint" then new HashTable from {
        "name" => form,
        "characterLattice" => "Q",
        "cocharacterLattice" => "Pvee",
        "validated" => true
    }
    else if form === "Unspecified" then new HashTable from {
        "name" => form,
        "characterLattice" => "unspecified intermediate lattice between Q and P",
        "cocharacterLattice" => "unspecified dual lattice between Qvee and Pvee",
        "validated" => false
    }
    else error "rootDatum: GlobalForm must be \"SimplyConnected\", \"Adjoint\", or \"Unspecified\""
)

-- Extend a full-row-rank rational matrix to a square basis by adjoining the
-- earliest standard basis rows that increase rank.  The deterministic choice
-- is part of the canonical datum: in singular type a realization includes
-- Cartan directions beyond the spans of the simple roots and coroots.
draftExtendRowsToBasis = (M,who) -> (
    M = M ** QQ;
    d := numcols M;
    if numrows M > d or rank M != numrows M then
        error(who | ": rows must be linearly independent");
    B := M;
    j := 0;
    while numrows B < d do (
        e := matrix table(1,d,(i,k) -> if k == j then 1_QQ else 0_QQ);
        if rank(B || e) > rank B then B = B || e;
        j = j + 1;
        if j > d then error(who | ": failed to extend rows to a basis");
    );
    B
)

draftAllIntegralEntries = M -> all(flatten entries M,x -> denominator x == 1)

-- A character-lattice basis B is written by rows in h^*.  Its perfect dual
-- cocharacter basis is Y=(B^{-1})^T.  A genuine root datum requires every
-- simple root to lie in X=ZB and every simple coroot to lie in Y=ZY.
draftValidateCharacterLattice = (R,B,who) -> (
    d := R#"dimension";
    if not instance(B,Matrix) or numrows B != d or numcols B != d then
        error(who | ": CharacterLattice must be an invertible dim(h) by dim(h) matrix whose rows are a basis in h^*");
    BQ := B ** QQ;
    if rank BQ != d then error(who | ": CharacterLattice basis must be invertible");
    YQ := transpose inverse BQ;
    roots := draftSimpleRootMatrix R;
    coroots := draftSimpleCorootMatrix R;
    if not draftAllIntegralEntries(roots * inverse BQ) then
        error(who | ": the character lattice does not contain every simple root");
    if not draftAllIntegralEntries(coroots * inverse YQ) then
        error(who | ": the dual cocharacter lattice does not contain every simple coroot");
    {BQ,YQ}
)

draftCanonicalGlobalLattices = (R,form) -> (
    roots := draftSimpleRootMatrix R;
    coroots := draftSimpleCorootMatrix R;
    if form === "SimplyConnected" then (
        Y := draftExtendRowsToBasis(coroots,"rootDatum");
        simplyConnectedBasis := transpose inverse Y;
        draftValidateCharacterLattice(R,simplyConnectedBasis,"rootDatum")
    ) else if form === "Adjoint" then (
        adjointBasis := draftExtendRowsToBasis(roots,"rootDatum");
        draftValidateCharacterLattice(R,adjointBasis,"rootDatum")
    ) else null
)

rootDatum = method(Options => {
    GlobalForm => "SimplyConnected",
    CharacterLattice => null
})

rootDatum Realization := opts -> R -> (
    D := R#"cartanDatum";
    latticePair := if opts.CharacterLattice === null then
        draftCanonicalGlobalLattices(R,opts.GlobalForm)
    else draftValidateCharacterLattice(R,opts.CharacterLattice,"rootDatum");
    form := if opts.CharacterLattice === null then
        draftGlobalFormMetadata(opts.GlobalForm,D)
    else new HashTable from {
        "name" => "Custom",
        "characterLattice" => "the lattice generated by CharacterLattice",
        "cocharacterLattice" => "the perfect dual of CharacterLattice",
        "validated" => true
    };
    new RootDatum from {
        "cartanDatum" => D,
        "realization" => R,
        "globalForm" => form,
        "characterLatticeBasis" => if latticePair === null then null
            else apply(entries(latticePair#0),row -> apply(row,draftQQ)),
        "cocharacterLatticeBasis" => if latticePair === null then null
            else apply(entries(latticePair#1),row -> apply(row,draftQQ)),
        "PmodQ" => draftPmodQ D,
        "rootBasis" => "simple roots",
        "corootBasis" => "simple coroots",
        "weightCoordinates" => "Dynkin labels; full realization coordinates retained when needed",
        "coweightCoordinates" => "simple-kmRoot evaluations; full realization coordinates retained when needed"
    }
)

rootDatum CartanDatum := opts -> D -> rootDatum(
    standardRealization D,GlobalForm=>opts.GlobalForm,
    CharacterLattice=>opts.CharacterLattice)

rootDatum Matrix := opts -> A -> rootDatum(
    cartanDatum A,GlobalForm=>opts.GlobalForm,
    CharacterLattice=>opts.CharacterLattice)

rootDatum String := opts -> s -> rootDatum(
    cartanDatum s,GlobalForm=>opts.GlobalForm,
    CharacterLattice=>opts.CharacterLattice)

pModQInvariants = method()
pModQInvariants CartanDatum := draftPmodQ
pModQInvariants Realization := R -> draftPmodQ(R#"cartanDatum")
pModQInvariants RootDatum := D -> D#"PmodQ"
pModQInvariants Matrix := A -> draftPmodQ cartanDatum A

toString RootDatum := D -> (
    components := (D#"cartanDatum")#"components";
    componentName := if #components == 0 then "trivial"
        else demark(" + ",apply(components,C -> C#"label"));
    concatenate(
        "RootDatum(",componentName,
        ", form=",(D#"globalForm")#"name",
        ", dim(h)=",toString((D#"realization")#"dimension"),")")
)
net RootDatum := D -> net toString D

--------------------------------------------------------------------------
-- Parent-aware lattice elements
--------------------------------------------------------------------------

kmRoot = method()
kmCoroot = method()
kmWeight = method()
kmCoweight = method()
weightFromRealizationCoordinates = method()
coweightFromRealizationCoordinates = method()

kmRoot (RootDatum,List) := (D,v) -> new KMRoot from {
    "parent" => D,
    "coordinates" => draftIntegralVector(v,(D#"cartanDatum")#"rank","kmRoot"),
    "basis" => "SimpleRoots"
}
kmRoot (RootDatum,Matrix) := (D,v) -> kmRoot(D,draftIntegralVector(v,(D#"cartanDatum")#"rank","kmRoot"))
kmRoot (CartanDatum,List) := (D,v) -> kmRoot(rootDatum D,v)
kmRoot (Matrix,List) := (A,v) -> kmRoot(rootDatum A,v)

kmCoroot (RootDatum,List) := (D,v) -> new KMCoroot from {
    "parent" => D,
    "coordinates" => draftIntegralVector(v,(D#"cartanDatum")#"rank","kmCoroot"),
    "basis" => "SimpleCoroots"
}
kmCoroot (RootDatum,Matrix) := (D,v) -> kmCoroot(D,draftIntegralVector(v,(D#"cartanDatum")#"rank","kmCoroot"))
kmCoroot (CartanDatum,List) := (D,v) -> kmCoroot(rootDatum D,v)
kmCoroot (Matrix,List) := (A,v) -> kmCoroot(rootDatum A,v)

kmWeight (RootDatum,List) := (D,v) -> new KMWeight from {
    "parent" => D,
    "coordinates" => draftRationalVector(v,(D#"cartanDatum")#"rank","kmWeight"),
    "basis" => "DynkinLabels",
    "realizationCoordinates" => null,
    "coordinateStatus" => if (D#"realization")#"dimension" == (D#"cartanDatum")#"rank"
        then "complete" else "modulo annihilator of the simple coroots"
}
kmWeight (RootDatum,Matrix) := (D,v) -> kmWeight(D,draftRationalVector(v,(D#"cartanDatum")#"rank","kmWeight"))
kmWeight (CartanDatum,List) := (D,v) -> kmWeight(rootDatum D,v)
kmWeight (Matrix,List) := (A,v) -> kmWeight(rootDatum A,v)

kmCoweight (RootDatum,List) := (D,v) -> new KMCoweight from {
    "parent" => D,
    "coordinates" => draftRationalVector(v,(D#"cartanDatum")#"rank","kmCoweight"),
    "basis" => "SimpleRootEvaluations",
    "realizationCoordinates" => null,
    "coordinateStatus" => if (D#"realization")#"dimension" == (D#"cartanDatum")#"rank"
        then "complete" else "modulo annihilator of the simple roots"
}
kmCoweight (RootDatum,Matrix) := (D,v) -> kmCoweight(D,draftRationalVector(v,(D#"cartanDatum")#"rank","kmCoweight"))
kmCoweight (CartanDatum,List) := (D,v) -> kmCoweight(rootDatum D,v)
kmCoweight (Matrix,List) := (A,v) -> kmCoweight(rootDatum A,v)

weightFromRealizationCoordinates (RootDatum,List) := (D,v) -> (
    R := D#"realization";
    d := R#"dimension";
    x := draftRationalVector(v,d,"weightFromRealizationCoordinates");
    C := draftSimpleCorootMatrix R;
    labels := apply(flatten entries(matrix(QQ,{x}) * transpose C),draftQQ);
    if d == (D#"cartanDatum")#"rank" and det C != 0_QQ then kmWeight(D,labels)
    else new KMWeight from {
        "parent" => D,
        "coordinates" => labels,
        "basis" => "DynkinLabels",
        "realizationCoordinates" => x,
        "coordinateStatus" => "complete realization kmWeight"
    }
)
weightFromRealizationCoordinates (RootDatum,Matrix) := (D,v) ->
    weightFromRealizationCoordinates(D,draftRationalVector(v,(D#"realization")#"dimension","weightFromRealizationCoordinates"))

coweightFromRealizationCoordinates (RootDatum,List) := (D,v) -> (
    Rz := D#"realization";
    d := Rz#"dimension";
    y := draftRationalVector(v,d,"coweightFromRealizationCoordinates");
    SR := draftSimpleRootMatrix Rz;
    labels := apply(flatten entries(matrix(QQ,{y}) * transpose SR),draftQQ);
    if d == (D#"cartanDatum")#"rank" and det SR != 0_QQ then kmCoweight(D,labels)
    else new KMCoweight from {
        "parent" => D,
        "coordinates" => labels,
        "basis" => "SimpleRootEvaluations",
        "realizationCoordinates" => y,
        "coordinateStatus" => "complete realization kmCoweight"
    }
)
coweightFromRealizationCoordinates (RootDatum,Matrix) := (D,v) ->
    coweightFromRealizationCoordinates(D,draftRationalVector(v,(D#"realization")#"dimension","coweightFromRealizationCoordinates"))

parentDatum = method()
parentDatum KMRoot := x -> x#"parent"
parentDatum KMCoroot := x -> x#"parent"
parentDatum KMWeight := x -> x#"parent"
parentDatum KMCoweight := x -> x#"parent"

datumCoordinates = method()
datumCoordinates KMRoot := x -> x#"coordinates"
datumCoordinates KMCoroot := x -> x#"coordinates"
datumCoordinates KMWeight := x -> x#"coordinates"
datumCoordinates KMCoweight := x -> x#"coordinates"

coordinateBasis = method()
coordinateBasis KMRoot := x -> x#"basis"
coordinateBasis KMCoroot := x -> x#"basis"
coordinateBasis KMWeight := x -> x#"basis"
coordinateBasis KMCoweight := x -> x#"basis"

-- Bases of the ambient weight and coweight spaces.  The auxiliary choices
-- live in a cache, not in immutable equality-bearing lattice elements.
draftNotationNodeCache := new MutableHashTable;
draftNotationBasisCache := new MutableHashTable;

draftNotationCacheInfo = () -> new HashTable from {
    "nodeConventions" => #keys draftNotationNodeCache,
    "latticeBases" => #keys draftNotationBasisCache
}

draftClearNotationCache = () -> (
    draftNotationNodeCache = new MutableHashTable;
    draftNotationBasisCache = new MutableHashTable;
    null
)

draftNotationNodeInfo = D -> (
    CD := D#"cartanDatum";
    if draftNotationNodeCache#?CD then return draftNotationNodeCache#CD;
    indecomposableAffine := CD#"type" === "affine" and #(CD#"components") == 1;
    data := if indecomposableAffine then affineTypeData(datumCartanMatrix CD) else null;
    affineNode := if data =!= null and data#"recognized"
        then data#"standardAffineNode" else if indecomposableAffine then 1 else null;
    info := new HashTable from {
        "isAffine" => indecomposableAffine,
        "affineNode" => affineNode
    };
    draftNotationNodeCache#CD = info;
    info
)

draftNotationBasisInfo = (D,dual) -> (
    key := {D,dual};
    if draftNotationBasisCache#?key then return draftNotationBasisCache#key;
    Rz := D#"realization";
    r := (D#"cartanDatum")#"rank";
    d := Rz#"dimension";
    nodeInfo := draftNotationNodeInfo D;
    SR := draftSimpleRootMatrix Rz;
    SC := draftSimpleCorootMatrix Rz;
    evaluations := if dual then SR else SC;
    affineBasis := nodeInfo#"isAffine" and d == r+1;
    equations := if affineBasis then (
        -- The row-space representative fixes the same degree-zero lift as
        -- affineDerivation.  For coweights use the corresponding dual choice.
        definingRows := if dual then SC else SR;
        marks := if dual then affineComarks D else affineMarks D;
        a := (nodeInfo#"affineNode")-1;
        target := matrix(QQ,{apply(r,i -> if i == a then 1_QQ/(marks#i) else 0_QQ)});
        degreeRow := target * inverse(definingRows * transpose definingRows) * definingRows;
        evaluations || degreeRow
    ) else draftExtendRowsToBasis(evaluations,"mathematical lattice notation");
    basis := transpose inverse equations;
    info := new HashTable from {"basisRows" => entries basis};
    draftNotationBasisCache#key = info;
    info
)

-- Internal basis descriptions.  Rows are in the realization's given
-- coordinates, including all supplementary directions.
draftWeightNotationInfo = D -> draftNotationBasisInfo(D,false)
draftCoweightNotationInfo = D -> draftNotationBasisInfo(D,true)

weightBasis = method()
weightBasis RootDatum := D -> apply((draftWeightNotationInfo D)#"basisRows",
    row -> weightFromRealizationCoordinates(D,row))

coweightBasis = method()
coweightBasis RootDatum := D -> apply((draftCoweightNotationInfo D)#"basisRows",
    row -> coweightFromRealizationCoordinates(D,row))

-- Coordinate tuples retain the original Cartan-matrix order.  A complete
-- singular/nonminimal weight must show all of its realization coordinates;
-- labels alone distinguish only its class modulo the relevant annihilator.
draftCoordinateTuple = coordinates ->
    "(" | concatenate between(", ",apply(coordinates,toString)) | ")"

draftWeightNotation = x -> (
    D := parentDatum x;
    dual := instance(x,KMCoweight);
    r := (D#"cartanDatum")#"rank";
    d := (D#"realization")#"dimension";
    if x#"realizationCoordinates" === null and d > r then (
        return draftCoordinateTuple(datumCoordinates x) | " mod " |
            (if dual then "ann(Q)" else "ann(h')");
    );
    draftCoordinateTuple(if d == r then datumCoordinates x else realizationCoordinates x)
)

draftLatticeNotation = x -> draftCoordinateTuple(datumCoordinates x)

toString KMRoot := draftLatticeNotation
toString KMCoroot := draftLatticeNotation
toString KMWeight := draftWeightNotation
toString KMCoweight := draftWeightNotation
net KMRoot := x -> net toString x
net KMCoroot := x -> net toString x
net KMWeight := x -> net toString x
net KMCoweight := x -> net toString x

--------------------------------------------------------------------------
-- Arithmetic with strict parent checks
--------------------------------------------------------------------------

KMRoot + KMRoot := (x,y) -> (D := draftRequireSameParent(x,y,"KMRoot + KMRoot"); kmRoot(D,draftAdd(datumCoordinates x,datumCoordinates y)))
KMCoroot + KMCoroot := (x,y) -> (D := draftRequireSameParent(x,y,"KMCoroot + KMCoroot"); kmCoroot(D,draftAdd(datumCoordinates x,datumCoordinates y)))

draftAddWeight = (x,y,who) -> (
    D := draftRequireSameParent(x,y,who);
    fx := x#"realizationCoordinates";
    fy := y#"realizationCoordinates";
    if fx === null and fy === null then kmWeight(D,draftAdd(datumCoordinates x,datumCoordinates y))
    else if fx =!= null and fy =!= null then weightFromRealizationCoordinates(D,draftAdd(fx,fy))
    else error(who | ": cannot add a complete singular-realization kmWeight to a Dynkin-label class")
)

draftAddCoweight = (x,y,who) -> (
    D := draftRequireSameParent(x,y,who);
    fx := x#"realizationCoordinates";
    fy := y#"realizationCoordinates";
    if fx === null and fy === null then kmCoweight(D,draftAdd(datumCoordinates x,datumCoordinates y))
    else if fx =!= null and fy =!= null then coweightFromRealizationCoordinates(D,draftAdd(fx,fy))
    else error(who | ": cannot add a complete singular-realization kmCoweight to an evaluation-label class")
)

KMWeight + KMWeight := (x,y) -> draftAddWeight(x,y,"KMWeight + KMWeight")
KMCoweight + KMCoweight := (x,y) -> draftAddCoweight(x,y,"KMCoweight + KMCoweight")

- KMRoot := x -> kmRoot(parentDatum x,draftScale(-1,datumCoordinates x))
- KMCoroot := x -> kmCoroot(parentDatum x,draftScale(-1,datumCoordinates x))
- KMWeight := x -> (
    if x#"realizationCoordinates" === null
    then kmWeight(parentDatum x,draftScale(-1,datumCoordinates x))
    else weightFromRealizationCoordinates(parentDatum x,draftScale(-1,x#"realizationCoordinates"))
)
- KMCoweight := x -> (
    if x#"realizationCoordinates" === null
    then kmCoweight(parentDatum x,draftScale(-1,datumCoordinates x))
    else coweightFromRealizationCoordinates(parentDatum x,draftScale(-1,x#"realizationCoordinates"))
)

KMRoot - KMRoot := (x,y) -> x + (-y)
KMCoroot - KMCoroot := (x,y) -> x + (-y)
KMWeight - KMWeight := (x,y) -> x + (-y)
KMCoweight - KMCoweight := (x,y) -> x + (-y)

ZZ * KMRoot := (a,x) -> kmRoot(parentDatum x,draftScale(a,datumCoordinates x))
KMRoot * ZZ := (x,a) -> a*x
ZZ * KMCoroot := (a,x) -> kmCoroot(parentDatum x,draftScale(a,datumCoordinates x))
KMCoroot * ZZ := (x,a) -> a*x
ZZ * KMWeight := (a,x) -> (
    if x#"realizationCoordinates" === null
    then kmWeight(parentDatum x,draftScale(a,datumCoordinates x))
    else weightFromRealizationCoordinates(parentDatum x,draftScale(a,x#"realizationCoordinates"))
)
KMWeight * ZZ := (x,a) -> a*x
ZZ * KMCoweight := (a,x) -> (
    if x#"realizationCoordinates" === null
    then kmCoweight(parentDatum x,draftScale(a,datumCoordinates x))
    else coweightFromRealizationCoordinates(parentDatum x,draftScale(a,x#"realizationCoordinates"))
)
KMCoweight * ZZ := (x,a) -> a*x

--------------------------------------------------------------------------
-- Pairings and basis conversions
--------------------------------------------------------------------------

pairing = method()

pairing (KMWeight,KMCoroot) := (x,y) -> (
    draftRequireSameParent(x,y,"pairing(KMWeight,KMCoroot)");
    draftDot(datumCoordinates x,datumCoordinates y)
)

pairing (KMRoot,KMCoweight) := (x,y) -> (
    draftRequireSameParent(x,y,"pairing(KMRoot,KMCoweight)");
    draftDot(datumCoordinates x,datumCoordinates y)
)

pairing (KMRoot,KMCoroot) := (x,y) -> pairing(toWeight x,y)

pairing (KMWeight,KMCoweight) := (x,y) -> (
    draftRequireSameParent(x,y,"pairing(KMWeight,KMCoweight)");
    draftDot(realizationCoordinates x,realizationCoordinates y)
)

toWeight = method()
toWeight KMRoot := x -> weightFromRealizationCoordinates(
    parentDatum x,realizationCoordinates x)

toCoweight = method()
toCoweight KMCoroot := x -> coweightFromRealizationCoordinates(
    parentDatum x,realizationCoordinates x)

weightToRootCoordinates = method()
weightToRootCoordinates KMWeight := x -> (
    D := parentDatum x;
    if x#"realizationCoordinates" === null and
       ((D#"cartanDatum")#"corank" > 0) then
        error "weightToRootCoordinates: Dynkin labels omit realization directions for this singular parent";
    ambient := realizationCoordinates x;
    R := draftSimpleRootMatrix(D#"realization");
    gram := R * transpose R;
    coeffs := matrix(QQ,{ambient}) * transpose R * inverse gram;
    if coeffs * R != matrix(QQ,{ambient}) then
        error "weightToRootCoordinates: weight is outside the simple-root span";
    apply(flatten entries coeffs,draftQQ)
)

coweightToCorootCoordinates = method()
coweightToCorootCoordinates KMCoweight := x -> (
    D := parentDatum x;
    if x#"realizationCoordinates" === null and
       ((D#"cartanDatum")#"corank" > 0) then
        error "coweightToCorootCoordinates: evaluation labels omit realization directions for this singular parent";
    ambient := realizationCoordinates x;
    C := draftSimpleCorootMatrix(D#"realization");
    gram := C * transpose C;
    coeffs := matrix(QQ,{ambient}) * transpose C * inverse gram;
    if coeffs * C != matrix(QQ,{ambient}) then
        error "coweightToCorootCoordinates: coweight is outside the simple-coroot span";
    apply(flatten entries coeffs,draftQQ)
)

isRootLatticeWeight = method()
isRootLatticeWeight KMWeight := x ->
    all(weightToRootCoordinates x,c -> instance(c,ZZ))

isCorootLatticeCoweight = method()
isCorootLatticeCoweight KMCoweight := x ->
    all(coweightToCorootCoordinates x,c -> instance(c,ZZ))

toRoot = method()
toRoot KMWeight := x -> (
    coords := weightToRootCoordinates x;
    if not all(coords,c -> instance(c,ZZ)) then
        error "toRoot: kmWeight is not in the kmRoot lattice";
    kmRoot(parentDatum x,coords)
)

toCoroot = method()
toCoroot KMCoweight := x -> (
    coords := coweightToCorootCoordinates x;
    if not all(coords,c -> instance(c,ZZ)) then
        error "toCoroot: kmCoweight is not in the kmCoroot lattice";
    kmCoroot(parentDatum x,coords)
)

realizationCoordinates = method()

realizationCoordinates KMRoot := x -> (
    R := draftSimpleRootMatrix(
        draftRealizationOfRootDatum parentDatum x);
    apply(flatten entries(matrix(QQ,{datumCoordinates x}) * R),draftQQ)
)

realizationCoordinates KMCoroot := x -> (
    C := draftSimpleCorootMatrix(
        draftRealizationOfRootDatum parentDatum x);
    apply(flatten entries(matrix(QQ,{datumCoordinates x}) * C),draftQQ)
)

realizationCoordinates KMWeight := x -> (
    if x#"realizationCoordinates" =!= null then return x#"realizationCoordinates";
    D := parentDatum x;
    Rz := draftRealizationOfRootDatum D;
    C := draftSimpleCorootMatrix Rz;
    if numrows C != numcols C or det C == 0_QQ then
        error "realizationCoordinates(KMWeight): Dynkin labels omit realization directions for this parent";
    v := matrix(QQ,{datumCoordinates x}) * inverse(transpose C);
    apply(flatten entries v,draftQQ)
)

realizationCoordinates KMCoweight := x -> (
    if x#"realizationCoordinates" =!= null then return x#"realizationCoordinates";
    D := parentDatum x;
    Rz := draftRealizationOfRootDatum D;
    R := draftSimpleRootMatrix Rz;
    if numrows R != numcols R or det R == 0_QQ then
        error "realizationCoordinates(KMCoweight): evaluation labels omit realization directions for this parent";
    v := matrix(QQ,{datumCoordinates x}) * inverse(transpose R);
    apply(flatten entries v,draftQQ)
)

--------------------------------------------------------------------------
-- Global-form membership
--------------------------------------------------------------------------

integratesToGlobalForm = method()
integratesToGlobalForm KMWeight := x -> (
    D := parentDatum x;
    Bentries := D#"characterLatticeBasis";
    if Bentries === null then return null;
    ambient := try realizationCoordinates x else return null;
    B := matrix(QQ,Bentries);
    all(flatten entries(matrix(QQ,{ambient}) * inverse B),
        c -> denominator c == 1)
)

integratesCoweightToGlobalForm = method()
integratesCoweightToGlobalForm KMCoweight := x -> (
    D := parentDatum x;
    Yentries := D#"cocharacterLatticeBasis";
    if Yentries === null then return null;
    ambient := try realizationCoordinates x else return null;
    Y := matrix(QQ,Yentries);
    all(flatten entries(matrix(QQ,{ambient}) * inverse Y),
        c -> denominator c == 1)
)

characterLatticeBasis = method()
characterLatticeBasis RootDatum := D -> (
    if D#"characterLatticeBasis" === null then null
    else matrix(QQ,D#"characterLatticeBasis")
)

cocharacterLatticeBasis = method()
cocharacterLatticeBasis RootDatum := D -> (
    if D#"cocharacterLatticeBasis" === null then null
    else matrix(QQ,D#"cocharacterLatticeBasis")
)


-------------------------------------------------------------------------
-- Affine Structure
-------------------------------------------------------------------------

-- Structural affine Kac--Moody functionality, following the root-data
-- definitions.
--
-- All public node labels are 1-based, as in KacMoody.
--
-- MATHEMATICAL SCOPE
-- ------------------
-- The following data are determined by an indecomposable affine GCM A:
--
--   * the primitive positive right-kernel vector a, hence
--         delta = sum_i a_i alpha_i;
--   * the primitive positive left-kernel vector a^vee, hence
--         c = sum_i a_i^vee alpha_i^vee;
--   * the level lambda(c) of a weight from its Dynkin labels;
--   * restriction of Dynkin labels to a chosen finite deletion;
--   * the affine Dynkin type, up to simultaneous node permutation.
--
-- A GCM and Dynkin labels do NOT determine a derivation d, the value
-- lambda(d), or a full affine weight: lambda and lambda+k*delta have the
-- same Dynkin labels.  RootDatum overloads below retain the missing
-- realization coordinates and compute lambda(d) only when the user supplies
-- a complete, normalized derivation.

-----------------------------------------------------------------------
-- Validation and primitive kernel vectors
-----------------------------------------------------------------------

affineStructureIntegralCartan = (A, who) -> (
    if not instance(A, Matrix) then error(who | ": input must be a matrix");
    if numrows A != numcols A then error(who | ": Cartan matrix must be square");
    if not isCartanMatrix A then error(who | ": input is not a generalized Cartan matrix");
    if ring A === ZZ then A
    else matrix apply(entries A, row -> apply(row, x -> lift(x, ZZ)))
);

-- Matrix-determined affine structure is cached by the immutable entry list,
-- rather than by Matrix identity.  Typed CartanDatum/Realization/RootDatum
-- enrichment is deliberately never cached here: equal Cartan matrices may
-- carry different public node labels and different realizations.
affineStructureCache := new MutableHashTable;

affineStructureMatrixKey = A -> entries A;

affineStructureCacheEntry = A -> (
    key := affineStructureMatrixKey A;
    if not affineStructureCache#?key then
        affineStructureCache#key = new MutableHashTable;
    affineStructureCache#key
);

affineStructureCacheInfo = () -> (
    cacheKeys := keys affineStructureCache;
    countField := field -> sum apply(cacheKeys,key ->
        if (affineStructureCache#key)#?field then 1 else 0);
    new HashTable from {
        "cartanMatrices" => #cacheKeys,
        "validatedAffineMatrices" => countField "validatedAffine",
        "marks" => countField "marks",
        "comarks" => countField "comarks",
        "typeData" => countField "typeData"
    }
);

clearAffineStructureCache = () -> (
    affineStructureCache = new MutableHashTable;
    null
);

affineStructureRequireIndecomposable = (A, who) -> (
    A = affineStructureIntegralCartan(A, who);
    entry := affineStructureCacheEntry A;
    if entry#?"validatedAffine" then return A;
    if cartanType A =!= "affine" then
        error(who | ": Cartan matrix must be of affine type");
    if #(leviConnectedComponents A) != 1 then
        error(who | ": Cartan matrix must be indecomposable");
    if rank(A ** QQ) != numrows(A)-1 then
        error(who | ": affine Cartan matrix must have corank one");
    entry#"validatedAffine" = true;
    A
);

affineStructurePrimitivePositiveKernelValidated = (A, transposeInput, who) -> (
    entry := affineStructureCacheEntry A;
    field := if transposeInput then "comarks" else "marks";
    if entry#?field then return entry#field;
    M := if transposeInput then transpose(A ** QQ) else A ** QQ;
    G := gens kernel M;
    if numcols G != 1 then
        error(who | ": expected a one-dimensional kernel");
    vQQ := for i from 0 to numrows A - 1 list G_(i,0);
    commonDenominator := 1;
    for x in vQQ do
        commonDenominator = lcm(commonDenominator, denominator x);
    v := apply(vQQ, x -> lift(commonDenominator*x, ZZ));
    g := 0;
    for x in v do g = gcd(g,x);
    if g < 0 then g = -g;
    if g == 0 then error(who | ": kernel generator is zero");
    v = apply(v, x -> x // g);
    if all(v, x -> x < 0) then v = apply(v, x -> -x);
    if not all(v, x -> x > 0) then
        error(who | ": kernel has no strictly positive primitive generator");
    entry#field = v;
    v
);

affineStructurePrimitivePositiveKernel = (A, transposeInput, who) -> (
    A = affineStructureRequireIndecomposable(A, who);
    affineStructurePrimitivePositiveKernelValidated(A,transposeInput,who)
);

-- The affine marks a_i: delta = sum_i a_i alpha_i and A*a=0.  The name is
-- deliberately affine-prefixed; finite marks/comarks remain a separate API.
affineMarks = method();
affineMarks Matrix := A ->
    affineStructurePrimitivePositiveKernel(A, false, "affineMarks");
affineMarks CartanDatum := D -> affineMarks(datumCartanMatrix D);
affineMarks Realization := R -> affineMarks(datumCartanMatrix R);
affineMarks RootDatum := D -> affineMarks(datumCartanMatrix D);

-- The affine comarks a_i^vee: c = sum_i a_i^vee alpha_i^vee and
-- transpose(A)*a^vee=0.
affineComarks = method();
affineComarks Matrix := A ->
    affineStructurePrimitivePositiveKernel(A, true, "affineComarks");
affineComarks CartanDatum := D -> affineComarks(datumCartanMatrix D);
affineComarks Realization := R -> affineComarks(datumCartanMatrix R);
affineComarks RootDatum := D -> affineComarks(datumCartanMatrix D);

-- A bare matrix has no parent in which to place delta, so its overload
-- returns the coefficient list.  A RootDatum retains delta as an honest
-- KMRoot, including its nonzero realization direction in singular type.
affineNullRoot = method();
affineNullRoot Matrix := A -> affineMarks A;
affineNullRoot CartanDatum := D -> affineMarks D;
affineNullRoot Realization := R -> affineMarks R;
affineNullRoot RootDatum := D -> kmRoot(D,affineMarks D);

-- The canonical central element c belongs to the simple-coroot lattice of a
-- RootDatum.  It is intentionally distinct from its bare coefficient list.
affineCentralElement = method();
affineCentralElement RootDatum := D ->
    kmCoroot(D,affineComarks D);

-----------------------------------------------------------------------
-- Canonical twisted affine matrices used by recognition
-----------------------------------------------------------------------

-- Type A_(2n)^(2), with n+1 nodes.  For n=1 this is the exceptional
-- quadruple-bond matrix [[2,-1],[-4,2]].  For n>=2, deleting node 1
-- (matrix index 0) leaves the package's B_n Cartan matrix.
affineCartanA2Twisted = n -> (
    if not instance(n, ZZ) or n < 1 then
        error "affineCartanA2Twisted: n must be a positive integer";
    if n == 1 then return matrix{{2,-1},{-4,2}};
    M := mutableMatrix(ZZ,n+1,n+1);
    for i from 0 to n do M_(i,i) = 2;
    M_(0,1) = -1;
    M_(1,0) = -2;
    if n >= 3 then for i from 1 to n-2 do (
        M_(i,i+1) = -1;
        M_(i+1,i) = -1;
    );
    M_(n-1,n) = -1;
    M_(n,n-1) = -2;
    matrix M
);

-- The other twisted families are Langlands dual to the indicated
-- untwisted affine matrices.  cartanExtendUntwisted handles the B_2
-- low-rank coincidence omitted by the named cartanAffineB constructor.
affineCartanAOddTwisted = n -> (
    if not instance(n, ZZ) or n < 2 then
        error "affineCartanAOddTwisted: n must be an integer at least 2";
    transpose(if n == 2 then cartanExtendUntwisted(cartanB n)
        else cartanAffineB n)
);

affineCartanDTwisted = n -> (
    if not instance(n, ZZ) or n < 2 then
        error "affineCartanDTwisted: n must be an integer at least 2";
    transpose cartanAffineC n
);

affineCartanE6Twisted = () -> transpose cartanAffineF4();
affineCartanD4Twisted = () -> transpose cartanAffineG2();

-----------------------------------------------------------------------
-- Cartan-matrix isomorphism under simultaneous node permutation
-----------------------------------------------------------------------

affineStructureNodeSignature = (A, i) -> (
    incident := sort for j from 0 to numrows A - 1 list
        if i == j or (A_(i,j) == 0 and A_(j,i) == 0) then continue
        else {A_(i,j),A_(j,i)};
    {#incident,incident}
);

-- Internal pointed version: forcedPairs is a list of {input,output}
-- constraints in 0-based indexing.  This lets us compute automorphism
-- orbits without mistaking two isomorphic deletion submatrices for two
-- genuinely interchangeable affine nodes.
affineStructureCartanIsomorphismConstrained = (A, B, forcedPairs) -> (
    A = affineStructureIntegralCartan(A, "affineStructureCartanIsomorphism");
    B = affineStructureIntegralCartan(B, "affineStructureCartanIsomorphism");
    if numrows A != numrows B then return null;
    r := numrows A;
    sigA := apply(r, i -> affineStructureNodeSignature(A,i));
    sigB := apply(r, i -> affineStructureNodeSignature(B,i));
    candidates := apply(r, i -> select(toList(0..r-1),
        j -> sigA#i == sigB#j));
    if any(candidates, L -> #L == 0) then return null;

    mapping := new MutableList from toList(r:null);
    used := new MutableList from toList(r:false);
    answer := null;

    forcedValid := true;
    for pair in forcedPairs do (
        if not instance(pair,List) or #pair != 2 then
            error "affineStructureCartanIsomorphism: forced node data must consist of pairs";
        inputNode := pair#0;
        outputNode := pair#1;
        if not instance(inputNode,ZZ) or not instance(outputNode,ZZ) or
           inputNode < 0 or inputNode >= r or outputNode < 0 or outputNode >= r then
            error "affineStructureCartanIsomorphism: forced node indices are out of range";
        if mapping#inputNode =!= null and mapping#inputNode != outputNode then
            forcedValid = false;
        if used#outputNode and mapping#inputNode != outputNode then
            forcedValid = false;
        if not member(outputNode,candidates#inputNode) then forcedValid = false;
        if forcedValid then (
            mapping#inputNode = outputNode;
            used#outputNode = true;
        );
    );
    if not forcedValid then return null;
    for i from 0 to r-1 do if mapping#i =!= null then
        for k from 0 to r-1 do if mapping#k =!= null then (
            if A_(i,k) != B_(mapping#i,mapping#k) then forcedValid = false;
        );
    if not forcedValid then return null;

    search := assignedCount -> (
        if answer =!= null then return null;
        if assignedCount == r then (
            answer = toList mapping;
            return null;
        );

        -- Choose the unassigned input node with the fewest unused image
        -- candidates.  Prefer a node adjacent to already assigned nodes.
        chosen := null;
        chosenAvailable := null;
        bestCount := r+1;
        bestAssignedNeighbors := -1;
        for i from 0 to r-1 do if mapping#i === null then (
            available := select(candidates#i, j -> not used#j);
            assignedNeighbors := #(select(toList(0..r-1), k ->
                mapping#k =!= null and (A_(i,k) != 0 or A_(k,i) != 0)));
            if #available < bestCount or
               (#available == bestCount and assignedNeighbors > bestAssignedNeighbors) then (
                chosen = i;
                chosenAvailable = available;
                bestCount = #available;
                bestAssignedNeighbors = assignedNeighbors;
            );
        );
        if chosen === null or bestCount == 0 then return null;

        for j in chosenAvailable do (
            compatible := true;
            for k from 0 to r-1 do if mapping#k =!= null then (
                imageK := mapping#k;
                if A_(chosen,k) != B_(j,imageK) or
                   A_(k,chosen) != B_(imageK,j) then (
                    compatible = false;
                    break;
                );
            );
            if compatible then (
                mapping#chosen = j;
                used#j = true;
                search(assignedCount+1);
                if answer =!= null then return null;
                used#j = false;
                mapping#chosen = null;
            );
        );
        null
    );

    assignedInitially := 0;
    for i from 0 to r-1 do if mapping#i =!= null then
        assignedInitially = assignedInitially + 1;
    search assignedInitially;
    answer
);

-- Return a list p with p_i=j when input node i maps to canonical node j,
-- or null when no simultaneous row/column permutation identifies A and B.
affineStructureCartanIsomorphism = (A, B) ->
    affineStructureCartanIsomorphismConstrained(A,B,{});

affineStructureCandidate = (label, kind, twistOrder, finiteFamily, finiteRank,
                        canonicalAffineNode, canonicalSpecialNodes,
                        canonicalDualSpecialNodes, A) ->
    new HashTable from {
        "type" => label,
        "kind" => kind,
        "twistOrder" => twistOrder,
        "finiteFamily" => finiteFamily,
        "finiteRank" => finiteRank,
        "matrix" => A,
        "canonicalAffineNode" => canonicalAffineNode,
        "canonicalSpecialNodes" => canonicalSpecialNodes,
        "canonicalDualSpecialNodes" => canonicalDualSpecialNodes
    };

affineStructureCandidateTable = r -> (
    candidates := {};
    nRank := r-1;
    add := (label, kind, twistOrder, finiteFamily, finiteRank, A) ->
        candidates = append(candidates,
            affineStructureCandidate(label,kind,twistOrder,
                finiteFamily,finiteRank,1,{1},{1},A));
    addWithSpecialNodes := (label, kind, twistOrder, finiteFamily, finiteRank,
                            canonicalAffineNode, canonicalSpecialNodes, A) ->
        candidates = append(candidates,
            affineStructureCandidate(label,kind,twistOrder,
                finiteFamily,finiteRank,canonicalAffineNode,
                canonicalSpecialNodes,canonicalSpecialNodes,A));
    addWithDualSpecialNodes := (label, kind, twistOrder, finiteFamily,
                                finiteRank, canonicalAffineNode,
                                canonicalSpecialNodes,
                                canonicalDualSpecialNodes, A) ->
        candidates = append(candidates,
            affineStructureCandidate(label,kind,twistOrder,
                finiteFamily,finiteRank,canonicalAffineNode,
                canonicalSpecialNodes,canonicalDualSpecialNodes,A));

    -- Untwisted families.
    if r >= 2 then (
        addWithSpecialNodes("A_" | toString nRank | "^(1)",
            "untwisted",1,"A",nRank,1,toList(1..r),cartanAffineA nRank);
    );
    if r >= 3 then (
        addWithSpecialNodes("B_" | toString nRank | "^(1)",
            "untwisted",1,"B",nRank,1,{1,2},
            if nRank == 2 then cartanExtendUntwisted(cartanB nRank)
            else cartanAffineB nRank);
        addWithSpecialNodes("C_" | toString nRank | "^(1)",
            "untwisted",1,"C",nRank,1,{1,r},cartanAffineC nRank);
    );
    if r >= 5 then (
        addWithSpecialNodes("D_" | toString nRank | "^(1)",
            "untwisted",1,"D",nRank,1,{1,2,r-1,r},cartanAffineD nRank);
    );
    -- cartanAffineE uses tripod numbering rather than placing the affine
    -- node first.  The indicated leaves are the ones whose deletion gives
    -- E_6, E_7, and E_8, respectively.  In E_7 the other length-four arm
    -- leaf lies in the same special-node orbit.
    if r == 7 then addWithSpecialNodes("E_6^(1)","untwisted",1,"E",6,
        1,{1,5,7},cartanAffineE 6);
    if r == 8 then addWithSpecialNodes("E_7^(1)","untwisted",1,"E",7,
        5,{5,8},cartanAffineE 7);
    if r == 9 then addWithSpecialNodes("E_8^(1)","untwisted",1,"E",8,
        9,{9},cartanAffineE 8);
    if r == 5 then add("F_4^(1)","untwisted",1,"F",4,cartanAffineF4());
    if r == 3 then add("G_2^(1)","untwisted",1,"G",2,cartanAffineG2());

    -- Twisted families.
    if r >= 2 then (
        -- Transposition reverses the distinguished end in A_(2n)^(2).
        addWithDualSpecialNodes("A_" | toString(2*nRank) | "^(2)",
            "twisted",2,"B",nRank,1,{1},{r},
            affineCartanA2Twisted nRank);
    );
    if r >= 3 then (
        addWithSpecialNodes("A_" | toString(2*nRank-1) | "^(2)",
            "twisted",2,"C",nRank,1,{1,2},
            affineCartanAOddTwisted nRank);
        addWithSpecialNodes("D_" | toString(nRank+1) | "^(2)",
            "twisted",2,"B",nRank,1,{1,r},
            affineCartanDTwisted nRank);
    );
    if r == 5 then add("E_6^(2)","twisted",2,"F",4,
        affineCartanE6Twisted());
    if r == 3 then add("D_4^(3)","twisted",3,"G",2,
        affineCartanD4Twisted());
    candidates
);

-----------------------------------------------------------------------
-- Affine recognition and structural metadata
-----------------------------------------------------------------------

affineStructureFinitePartRecord = (A, node, who) -> (
    r := numrows A;
    if not instance(node,ZZ) or node < 1 or node > r then
        error(who | ": node must be between 1 and rank");
    keep0 := select(toList(0..r-1), i -> i != node-1);
    C := submatrix(A,keep0,keep0);
    new HashTable from {
        "deletedNode" => node,
        "nodes" => apply(keep0,i -> i+1),
        "cartanMatrix" => C,
        "type" => cartanType C,
        "cartanData" => cartanData C,
        "connected" => (#(leviConnectedComponents C) == 1)
    }
);

-- Test connectedness of every one-node deletion from one adjacency list.
-- Rebuilding r principal submatrices and running a component classification
-- on each of them is cubic even for a sparse affine Dynkin diagram.
affineStructureConnectedDeletionNodes = A -> (
    r := numrows A;
    if r <= 1 then return {};
    adjacency := apply(r,i -> select(toList(0..r-1),j ->
        i != j and (A_(i,j) != 0 or A_(j,i) != 0)));
    deletionConnected := deleted -> (
        remaining := select(toList(0..r-1),i -> i != deleted);
        if #remaining <= 1 then return true;
        visited := new MutableList from toList(r:false);
        queue := {first remaining};
        visited#(first remaining) = true;
        cursor := 0;
        while cursor < #queue do (
            i := queue#cursor;
            cursor = cursor + 1;
            for j in adjacency#i do if j != deleted and not visited#j then (
                visited#j = true;
                queue = append(queue,j);
            );
        );
        all(remaining,i -> visited#i)
    );
    apply(select(toList(0..r-1),deletionConnected),i -> i+1)
);

-- Add typed information to one of the matrix-level records without changing
-- any of its established keys.  Typed overloads below always retain both the
-- finite object they construct and the ambient object from which it came.
affineStructureAddFields = (record, fields) ->
    new HashTable from join(toList pairs record,fields);

affineTypeData = method();
affineTypeData Matrix := A -> (
    A = affineStructureRequireIndecomposable(A,"affineTypeData");
    cacheEntry := affineStructureCacheEntry A;
    if cacheEntry#?"typeData" then
        return affineStructureAddFields(cacheEntry#"typeData",{
            "cartanMatrix" => A
        });
    r := numrows A;
    -- A was validated once above; do not repeat affine classification and
    -- connectedness tests merely to compute its two kernel vectors.
    marks := affineStructurePrimitivePositiveKernelValidated(
        A,false,"affineTypeData");
    comarks := affineStructurePrimitivePositiveKernelValidated(
        A,true,"affineTypeData");
    candidates := affineStructureCandidateTable r;
    identityPermutation := toList(0..r-1);
    exactCandidates := select(candidates,candidate -> A == candidate#"matrix");
    matches := if #exactCandidates > 0 and r > 3 then
        -- Above three nodes there are no low-rank naming coincidences, so a
        -- canonical constructor match need not be compared with every other
        -- infinite family by graph isomorphism.
        apply(exactCandidates,candidate -> {candidate,identityPermutation})
    else (
        found := {};
        for candidate in candidates do (
            p := if A == candidate#"matrix" then identityPermutation
                else affineStructureCartanIsomorphism(A,candidate#"matrix");
            if p =!= null then found = append(found,{candidate,p});
        );
        found
    );

    recognized := #matches > 0;
    primary := if recognized then (first matches)#0 else null;
    inputToCanonical := if recognized then (first matches)#1 else null;
    canonicalNodeOrder := if not recognized then null else (
        inverseOrder := new MutableList from toList(r:null);
        for i from 0 to r-1 do inverseOrder#(inputToCanonical#i) = i+1;
        toList inverseOrder
    );
    standardAffineNode := if recognized then
        canonicalNodeOrder#((primary#"canonicalAffineNode")-1) else null;
    markOneNodes := apply(select(toList(0..r-1),i -> marks#i == 1),i -> i+1);
    comarkOneNodes := apply(select(toList(0..r-1),i -> comarks#i == 1),i -> i+1);
    -- Every proper principal submatrix of an indecomposable affine GCM is
    -- finite type.  Use the theorem here instead of reclassifying r large
    -- rational matrices.  Connectedness is still computed explicitly.
    finiteDeletionNodes := toList(1..r);
    connectedFiniteDeletionNodes := affineStructureConnectedDeletionNodes A;
    primaryFinitePart := if standardAffineNode === null then null
        else affineStructureFinitePartRecord(
            A,standardAffineNode,"affineTypeData");
    -- Recognition carries the special-node orbit in canonical numbering;
    -- transport it through the computed diagram isomorphism.  This avoids
    -- solving r separate pointed graph-isomorphism problems in type A.
    specialNodes := if not recognized then {} else sort apply(
        primary#"canonicalSpecialNodes",node -> canonicalNodeOrder#(node-1));
    dualSpecialNodes := if not recognized then {} else sort apply(
        primary#"canonicalDualSpecialNodes",
        node -> canonicalNodeOrder#(node-1));

    result := new HashTable from {
        "cartanMatrix" => A,
        -- "rank" historically meant the number of Cartan nodes.  Preserve
        -- it, but also expose names which cannot be confused with matrix
        -- rank or the rank of the associated finite root system.
        "rank" => r,
        "nodeCount" => r,
        "cartanMatrixRank" => rank(A ** QQ),
        "corank" => 1,
        "finiteRank" => if recognized then primary#"finiteRank" else r-1,
        "finiteFamily" => if recognized then primary#"finiteFamily" else null,
        "finiteFamilyAliases" => if recognized then unique apply(matches,
            m -> (m#0)#"finiteFamily") else {},
        "rankConvention" => concatenate(
            "rank and nodeCount are the number of affine Cartan nodes; ",
            "cartanMatrixRank and finiteRank are one less"),
        "recognized" => recognized,
        "type" => if recognized then primary#"type" else null,
        "typeAliases" => if recognized then unique apply(matches,m -> (m#0)#"type") else {},
        "typeIsAmbiguous" => recognized and
            #(unique apply(matches,m -> (m#0)#"type")) > 1,
        "kind" => if recognized then primary#"kind" else null,
        "twistOrder" => if recognized then primary#"twistOrder" else null,
        "marks" => marks,
        "comarks" => comarks,
        "nullRoot" => marks,
        "centralCoefficients" => comarks,
        "markSum" => sum marks,
        "comarkSum" => sum comarks,
        "markOneNodes" => markOneNodes,
        "comarkOneNodes" => comarkOneNodes,
        "specialNodes" => specialNodes,
        "specialNodeConvention" => concatenate(
            "orbit of standardAffineNode under automorphisms of the affine ",
            "Cartan datum; each deletion gives the associated classical datum"),
        "dualSpecialNodes" => dualSpecialNodes,
        "dualSpecialNodeConvention" => concatenate(
            "special nodes reported by affineTypeData for the transposed ",
            "Cartan datum, in the original node positions"),
        "finiteDeletionNodes" => finiteDeletionNodes,
        "connectedFiniteDeletionNodes" => connectedFiniteDeletionNodes,
        "standardAffineNode" => standardAffineNode,
        "canonicalNodeOrder" => canonicalNodeOrder,
        "inputToCanonicalNodeMap" => if inputToCanonical === null then null
            else apply(inputToCanonical,j -> j+1),
        "canonicalNodeOrderConvention" => concatenate(
            "canonicalNodeOrder#(j-1) is the input position occupied by ",
            "canonical node j"),
        "inputToCanonicalNodeMapConvention" => concatenate(
            "inputToCanonicalNodeMap#(i-1) is the canonical position of ",
            "input node i"),
        "finitePart" => primaryFinitePart,
        "matrixDetermined" => {
            "marks/null root coefficients",
            "comarks/canonical central coefficients",
            "level from Dynkin labels",
            "finite-node Dynkin-label restriction",
            "affine type up to node permutation"
        },
        "requiresRealization" => {
            "a derivation d and lambda(d)",
            "full affine weights and fundamental weights",
            "a splitting of weights modulo the delta direction",
            "grade-sensitive Weyl actions and formal characters"
        }
    };
    cacheEntry#"typeData" = result;
    result
);

-- Deleting a node from an affine datum produces a finite semisimple Cartan
-- datum.  Node labels are inherited in their original order.  There is no
-- global-form shortcut that covers every ambient lattice: until the induced
-- Levi character lattice is computed explicitly, the corresponding finite
-- RootDatum is deliberately marked GlobalForm=>"Unspecified".
affineStructureNodeRecord = (D,node) -> new HashTable from {
    "position" => node,
    "label" => D#"nodeLabels"#(node-1)
};

affineStructureNodeRecords = (D,nodes) ->
    apply(nodes,node -> affineStructureNodeRecord(D,node));

affineStructureNodeLabels = (D,nodes) ->
    apply(nodes,node -> D#"nodeLabels"#(node-1));

affineStructureNodePosition = (D,x,who) -> (
    r := D#"rank";
    if instance(x,HashTable) and x#?"position" and x#?"label" then (
        i := x#"position";
        if not instance(i,ZZ) or i < 1 or i > r then
            error(who | ": node record has an invalid position");
        if not draftSameNodeLabel(D#"nodeLabels"#(i-1),x#"label") then
            error(who | ": node record label does not match its position");
        return i;
    );
    if instance(x,ZZ) and x >= 1 and x <= r then return x;
    labels := D#"nodeLabels";
    matches := select(toList(0..#labels-1),i ->
        draftSameNodeLabel(labels#i,x));
    if #matches == 0 then error concatenate(
        who,": unknown node label ",toString x,
        "; available labels are ",toString labels);
    first(matches)+1
);

-- Match the package-wide NodeFormat convention.  Bare custom integer labels
-- which collide with another public position are unsafe as round-trippable
-- output, so typed companion fields use records in exactly that case.
affineStructureTypedNodeFormat = D -> (
    labels := D#"nodeLabels";
    r := #labels;
    if any(toList(0..r-1),i ->
        instance(labels#i,ZZ)
        and labels#i >= 1 and labels#i <= r
        and labels#i != i+1)
    then "Records"
    else "Labels"
);

affineStructureTypedNodes = (D,nodes) -> (
    if ((affineStructureTypedNodeFormat D) === "Records")
    then affineStructureNodeRecords(D,nodes)
    else affineStructureNodeLabels(D,nodes)
);

affineStructureTypedNodeFields = (D,data) -> (
    standard := data#"standardAffineNode";
    standardRecord := if standard === null then null
        else affineStructureNodeRecord(D,standard);
    canonicalOrder := data#"canonicalNodeOrder";
    canonicalRecords := if canonicalOrder === null then null
        else apply(#canonicalOrder,j -> new HashTable from {
            "canonicalPosition" => j+1,
            "position" => canonicalOrder#j,
            "label" => D#"nodeLabels"#((canonicalOrder#j)-1)
        });
    inputToCanonical := data#"inputToCanonicalNodeMap";
    inputToCanonicalRecords := if inputToCanonical === null then null
        else apply(#inputToCanonical,i -> new HashTable from {
            "position" => i+1,
            "label" => D#"nodeLabels"#i,
            "canonicalPosition" => inputToCanonical#i
        });
    typedSingle := record -> (
        if record === null then null
        else if ((affineStructureTypedNodeFormat D) === "Records") then record
        else record#"label"
    );
    nodeListFields := (positionStem,companionStem,nodes) -> {
        companionStem | "Labels" => affineStructureNodeLabels(D,nodes),
        companionStem | "Records" => affineStructureNodeRecords(D,nodes),
        positionStem | "Typed" => affineStructureTypedNodes(D,nodes)
    };
    markRecords := apply(data#"nodeCount",i -> new HashTable from {
        "position" => i+1,
        "label" => D#"nodeLabels"#i,
        "coefficient" => data#"marks"#i
    });
    comarkRecords := apply(data#"nodeCount",i -> new HashTable from {
        "position" => i+1,
        "label" => D#"nodeLabels"#i,
        "coefficient" => data#"comarks"#i
    });
    flatten {
        {
            "nodeLabels" => D#"nodeLabels",
            "nodeRecords" => affineStructureNodeRecords(
                D,toList(1..data#"nodeCount")),
            "typedNodeFormat" => affineStructureTypedNodeFormat D,
            "standardAffineNodeLabel" => if standardRecord === null
                then null else standardRecord#"label",
            "standardAffineNodeRecord" => standardRecord,
            "standardAffineNodeTyped" => typedSingle standardRecord,
            "canonicalNodeRecords" => canonicalRecords,
            "inputToCanonicalNodeRecords" => inputToCanonicalRecords,
            "markRecords" => markRecords,
            "comarkRecords" => comarkRecords,
            "nullRootByNode" => markRecords,
            "centralElementByNode" => comarkRecords
        },
        nodeListFields("markOneNodes","markOneNode",data#"markOneNodes"),
        nodeListFields(
            "comarkOneNodes","comarkOneNode",data#"comarkOneNodes"),
        nodeListFields("specialNodes","specialNode",data#"specialNodes"),
        nodeListFields(
            "dualSpecialNodes","dualSpecialNode",data#"dualSpecialNodes"),
        nodeListFields("finiteDeletionNodes","finiteDeletionNode",
            data#"finiteDeletionNodes"),
        nodeListFields("connectedFiniteDeletionNodes",
            "connectedFiniteDeletionNode",
            data#"connectedFiniteDeletionNodes")
    }
);

affineStructureTypedFinitePartRecord = (D, part) -> (
    node := part#"deletedNode";
    ambientLabels := D#"nodeLabels";
    finiteLabels := apply(part#"nodes",i -> ambientLabels#(i-1));
    finiteDatum := cartanDatum(
        part#"cartanMatrix",NodeLabels=>finiteLabels);
    affineStructureAddFields(part,{
        "cartanDatum" => finiteDatum,
        "finiteCartanDatum" => finiteDatum,
        "ambientCartanDatum" => D,
        "nodeLabels" => finiteLabels,
        "nodeRecords" => affineStructureNodeRecords(D,part#"nodes"),
        "ambientNodeRecords" => affineStructureNodeRecords(D,part#"nodes"),
        "finiteNodeRecords" => affineStructureNodeRecords(
            finiteDatum,toList(1..#finiteLabels)),
        "nodePositionConvention" => concatenate(
            "nodes, nodeRecords, and ambientNodeRecords use ambient affine ",
            "positions; finiteNodeRecords use positions in finiteCartanDatum"),
        "deletedNodeLabel" => ambientLabels#(node-1),
        "deletedNodeRecord" => affineStructureNodeRecord(D,node),
        "nodeCount" => #(part#"nodes"),
        "cartanMatrixRank" => rank((part#"cartanMatrix") ** QQ),
        "finiteRank" => #(part#"nodes"),
        "finiteFamily" => if part#"connected"
            then (part#"cartanData"#"components"#0)#"family" else null
    })
);
affineStructureFinitePartDatumRecord = (D, node, who) -> (
    A := affineStructureRequireIndecomposable(datumCartanMatrix D,who);
    part := affineStructureFinitePartRecord(A,node,who);
    if part#"type" =!= "finite" then
        error(who | ": deleting the requested node did not give finite type");
    affineStructureTypedFinitePartRecord(D,part)
);

affineFinitePart = method();
affineFinitePart Matrix := A -> (
    data := affineTypeData A;
    if not data#"recognized" then
        error "affineFinitePart: affine type was not recognized";
    data#"finitePart"
);
affineFinitePart (Matrix,ZZ) := (A,node) -> (
    A = affineStructureRequireIndecomposable(A,"affineFinitePart");
    part := affineStructureFinitePartRecord(A,node,"affineFinitePart");
    if part#"type" =!= "finite" then
        error "affineFinitePart: deleting the requested node did not give finite type";
    part
);
affineFinitePart CartanDatum := D -> (
    data := affineTypeData D;
    if not data#"recognized" then
        error "affineFinitePart: affine type was not recognized";
    data#"finitePart"
);
affineFinitePart (CartanDatum,ZZ) := (D,node) ->
    affineStructureFinitePartDatumRecord(D,
        affineStructureNodePosition(D,node,"affineFinitePart"),
        "affineFinitePart");
affineFinitePart (CartanDatum,Thing) := (D,node) ->
    affineStructureFinitePartDatumRecord(D,
        affineStructureNodePosition(D,node,"affineFinitePart"),
        "affineFinitePart");
affineStructureFiniteRealizationRecord = (R,part) -> (
    finiteRealization := standardRealization part#"cartanDatum";
    affineStructureAddFields(part,{
        "realization" => finiteRealization,
        "finiteRealization" => finiteRealization,
        "ambientRealization" => R,
        "realizationConvention" => concatenate(
            "standard minimal realization of the finite semisimple Cartan ",
            "datum; no embedding into ambientRealization is asserted")
    })
);
affineFinitePart Realization := R -> (
    part := affineFinitePart cartanDatum R;
    affineStructureFiniteRealizationRecord(R,part)
);
affineFinitePart (Realization,ZZ) := (R,node) -> (
    part := affineFinitePart(cartanDatum R,node);
    affineStructureFiniteRealizationRecord(R,part)
);
affineFinitePart (Realization,Thing) := (R,node) -> (
    part := affineFinitePart(cartanDatum R,node);
    affineStructureFiniteRealizationRecord(R,part)
);
affineStructureFiniteRootDatumRecord = (D, part) -> (
    finiteRootDatum := rootDatum(
        part#"cartanDatum",GlobalForm=>"Unspecified");
    affineStructureAddFields(part,{
        "rootDatum" => finiteRootDatum,
        "finiteRootDatum" => finiteRootDatum,
        "ambientRootDatum" => D,
        "globalFormConvention" => concatenate(
            "the finite semisimple RootDatum has unspecified global form; ",
            "this routine does not infer the induced Levi character lattice")
    })
);
affineFinitePart RootDatum := D ->
    affineStructureFiniteRootDatumRecord(
        D,affineFinitePart cartanDatum D);
affineFinitePart (RootDatum,ZZ) := (D,node) ->
    affineStructureFiniteRootDatumRecord(
        D,affineFinitePart(cartanDatum D,node));
affineFinitePart (RootDatum,Thing) := (D,node) ->
    affineStructureFiniteRootDatumRecord(
        D,affineFinitePart(cartanDatum D,node));

-- Typed type-data results carry a typed finite part at the same level as
-- their ambient object.  This is reconstructed per call so cached
-- matrix-determined data can never leak node labels or realization parents.
affineTypeData CartanDatum := D -> (
    data := affineTypeData datumCartanMatrix D;
    finitePart := if data#"finitePart" === null then null
        else affineStructureTypedFinitePartRecord(D,data#"finitePart");
    affineStructureAddFields(data,join(
        affineStructureTypedNodeFields(D,data),{
            "cartanDatum" => D,
            "finitePart" => finitePart,
            "matrixFinitePart" => data#"finitePart"
        }))
);
affineTypeData Realization := R -> (
    data := affineTypeData cartanDatum R;
    finitePart := if data#"finitePart" === null then null
        else affineStructureFiniteRealizationRecord(R,data#"finitePart");
    affineStructureAddFields(data,{
        "realization" => R,
        "finitePart" => finitePart
    })
);
affineTypeData RootDatum := D -> (
    data := affineTypeData cartanDatum D;
    finitePart := if data#"finitePart" === null then null
        else affineStructureFiniteRootDatumRecord(D,data#"finitePart");
    affineStructureAddFields(data,{
        "rootDatum" => D,
        "realization" => realization D,
        "finitePart" => finitePart,
        "nullRootElement" => kmRoot(D,data#"marks"),
        "centralElement" => kmCoroot(D,data#"comarks")
    })
);

-----------------------------------------------------------------------
-- Weight helpers that are genuinely matrix-determined
-----------------------------------------------------------------------

affineStructureDynkinLabels = (A,lambda,who) -> (
    r := numrows A;
    labels := if instance(lambda,List) then lambda
        else if instance(lambda,Matrix) and numrows lambda == 1 and numcols lambda == r then
            for j from 0 to r-1 list lambda_(0,j)
        else if instance(lambda,Matrix) and numrows lambda == r and numcols lambda == 1 then
            for i from 0 to r-1 list lambda_(i,0)
        else error(who | ": weight must be a list, a 1 by rank row matrix, or a rank by 1 column matrix of Dynkin labels");
    if #labels != r then error(who | ": Dynkin-label list must have length equal to rank");
    labels
);

-- lambda(c) = sum_i a_i^vee <lambda,alpha_i^vee>.  This remains meaningful
-- without a derivation or a full realization.
affineLevel = method();
affineLevel (Matrix,List) := (A,lambda) -> (
    A = affineStructureRequireIndecomposable(A,"affineLevel");
    labels := affineStructureDynkinLabels(A,lambda,"affineLevel");
    comarks := affineStructurePrimitivePositiveKernelValidated(
        A,true,"affineLevel");
    sum for i from 0 to numrows A - 1 list comarks#i * labels#i
);
affineLevel (Matrix,Matrix) := (A,lambda) ->
    affineLevel(A,affineStructureDynkinLabels(A,lambda,"affineLevel"));
affineLevel (CartanDatum,List) := (D,lambda) ->
    affineLevel(datumCartanMatrix D,lambda);
affineLevel (CartanDatum,Matrix) := (D,lambda) ->
    affineLevel(datumCartanMatrix D,lambda);
affineLevel (Realization,List) := (R,lambda) ->
    affineLevel(datumCartanMatrix R,lambda);
affineLevel (Realization,Matrix) := (R,lambda) ->
    affineLevel(datumCartanMatrix R,lambda);
affineLevel (RootDatum,List) := (D,lambda) ->
    affineLevel(datumCartanMatrix D,lambda);
affineLevel (RootDatum,Matrix) := (D,lambda) ->
    affineLevel(datumCartanMatrix D,lambda);
affineLevel KMWeight := lambda -> (
    D := parentDatum lambda;
    -- Constructing c validates that the parent is indecomposable affine.
    pairing(lambda,affineCentralElement D)
);
affineLevel KMRoot := beta -> affineLevel(toWeight beta);

affineClassicalPart = method();
affineStructureClassicalPartValidated = (A,lambda,node) -> (
    labels := affineStructureDynkinLabels(A,lambda,"affineClassicalPart");
    part := affineStructureFinitePartRecord(A,node,"affineClassicalPart");
    if part#"type" =!= "finite" then
        error "affineClassicalPart: deleting the requested node did not give finite type";
    nodes := part#"nodes";
    new HashTable from {
        "deletedNode" => node,
        "nodes" => nodes,
        "cartanMatrix" => part#"cartanMatrix",
        "dynkinLabels" => apply(nodes,i -> labels#(i-1)),
        "meaning" => "restriction to the derived finite subalgebra on the retained nodes"
    }
);
affineClassicalPart (Matrix,List) := (A,lambda) -> (
    data := affineTypeData A;
    if not data#"recognized" then
        error "affineClassicalPart: affine type was not recognized";
    affineStructureClassicalPartValidated(
        data#"cartanMatrix",lambda,data#"standardAffineNode")
);
affineClassicalPart (Matrix,List,ZZ) := (A,lambda,node) -> (
    A = affineStructureRequireIndecomposable(A,"affineClassicalPart");
    affineStructureClassicalPartValidated(A,lambda,node)
);
affineClassicalPart (Matrix,Matrix) := (A,lambda) -> (
    labels := affineStructureDynkinLabels(A,lambda,"affineClassicalPart");
    affineClassicalPart(A,labels)
);
affineClassicalPart (Matrix,Matrix,ZZ) := (A,lambda,node) -> (
    labels := affineStructureDynkinLabels(A,lambda,"affineClassicalPart");
    affineClassicalPart(A,labels,node)
);
affineStructureTypedClassicalPartFromRecord = (lambda,part) -> (
    nodes := part#"nodes";
    finiteLabels := apply(nodes,i -> (datumCoordinates lambda)#(i-1));
    finiteWeight := kmWeight(part#"rootDatum",finiteLabels);
    affineStructureAddFields(part,{
        "weight" => finiteWeight,
        "finiteWeight" => finiteWeight,
        "dynkinLabels" => finiteLabels,
        "ambientWeight" => lambda,
        "meaning" => concatenate(
            "typed restriction to the derived finite semisimple subalgebra ",
            "on the retained nodes")
    })
);
affineClassicalPart KMWeight := lambda -> (
    D := parentDatum lambda;
    data := affineTypeData D;
    if not data#"recognized" then
        error "affineClassicalPart: affine type was not recognized";
    affineStructureTypedClassicalPartFromRecord(lambda,data#"finitePart")
);
affineClassicalPart (KMWeight,ZZ) := (lambda,node) -> (
    D := parentDatum lambda;
    node = affineStructureNodePosition(
        cartanDatum D,node,"affineClassicalPart");
    finiteDatumPart := affineStructureFinitePartDatumRecord(
        cartanDatum D,node,"affineClassicalPart");
    part := affineStructureFiniteRootDatumRecord(D,finiteDatumPart);
    affineStructureTypedClassicalPartFromRecord(lambda,part)
);
affineClassicalPart (KMWeight,Thing) := (lambda,node) -> (
    D := parentDatum lambda;
    node = affineStructureNodePosition(
        cartanDatum D,node,"affineClassicalPart");
    finiteDatumPart := affineStructureFinitePartDatumRecord(
        cartanDatum D,node,"affineClassicalPart");
    part := affineStructureFiniteRootDatumRecord(D,finiteDatumPart);
    affineStructureTypedClassicalPartFromRecord(lambda,part)
);

-----------------------------------------------------------------------
-- Complete affine derivations and degree-sensitive weight lifts
-----------------------------------------------------------------------

affineStructureRequireMinimalRootDatum = (D,who) -> (
    A := affineStructureRequireIndecomposable(datumCartanMatrix D,who);
    R := realization D;
    r := numrows A;
    if not R#"isMinimal" or R#"dimension" != r+1 then
        error concatenate(
            who,": requires a minimal realization of an indecomposable ",
            "corank-one affine Cartan datum (dimension nodeCount+1); the ",
            "requested lift is underdetermined in a nonminimal realization");
    {A,R}
);

-- Return the deterministic row-space representative of a normalized
-- derivation.  For the chosen node i it has
--     alpha_i(d)=1/a_i and alpha_j(d)=0 (j != i),
-- hence delta(d)=1.  Adding a central element gives another normalized
-- derivation, so this representative is a realization-basis section rather
-- than an intrinsic choice modulo the center.
affineDerivation = method();
affineDerivation RootDatum := D -> (
    validated := affineStructureRequireMinimalRootDatum(
        D,"affineDerivation");
    data := affineTypeData validated#0;
    if not data#"recognized" or data#"standardAffineNode" === null then
        error concatenate(
            "affineDerivation: affine type was not recognized, so no ",
            "default affine node is available; supply an explicit node");
    affineDerivation(D,data#"standardAffineNode")
);
affineDerivation (RootDatum,Thing) := (D,nodeSpecifier) -> (
    validated := affineStructureRequireMinimalRootDatum(
        D,"affineDerivation");
    A := validated#0;
    R := validated#1;
    node := affineStructureNodePosition(
        cartanDatum D,nodeSpecifier,"affineDerivation");
    marks := affineStructurePrimitivePositiveKernelValidated(
        A,false,"affineDerivation");
    r := numrows A;
    target := matrix(QQ,{apply(r,j ->
        if j == node-1 then 1_QQ/(marks#j) else 0_QQ)});
    simpleRoots := matrix(QQ,R#"simpleRootCoordinates");
    coordinates := flatten entries(
        target * inverse(simpleRoots * transpose simpleRoots) * simpleRoots);
    derivation := coweightFromRealizationCoordinates(D,coordinates);
    if pairing(affineNullRoot D,derivation) != 1 then
        error "affineDerivation: internal normalization failure";
    derivation
);
affineDerivation Thing := D -> error concatenate(
    "affineDerivation: expected an affine RootDatum with a minimal ",
    "realization");

affineStructureWeightFromDynkinDegree = (D,labels,degree,derivation) -> (
    validated := affineStructureRequireMinimalRootDatum(
        D,"affineWeightFromDynkinLabels");
    A := validated#0;
    R := validated#1;
    labels = affineStructureDynkinLabels(
        A,labels,"affineWeightFromDynkinLabels");
    labels = apply(labels,draftQQ);
    degree = try draftQQ degree else
        error "affineWeightFromDynkinLabels: degree must be rational";
    if parentDatum derivation =!= D then
        error concatenate(
            "affineWeightFromDynkinLabels: derivation belongs to a ",
            "different RootDatum parent");
    if derivation#"realizationCoordinates" === null then
        error concatenate(
            "affineWeightFromDynkinLabels: derivation is incomplete; use ",
            "affineDerivation or coweightFromRealizationCoordinates");
    delta := affineNullRoot D;
    if pairing(delta,derivation) != 1 then
        error concatenate(
            "affineWeightFromDynkinLabels: derivation must be normalized ",
            "by delta(d) = 1");
    simpleCoroots := matrix(QQ,R#"simpleCorootCoordinates");
    derivationRow := matrix(QQ,{realizationCoordinates derivation});
    equations := simpleCoroots || derivationRow;
    if numrows equations != R#"dimension" or
       numcols equations != R#"dimension" or
       rank equations != R#"dimension" then
        error concatenate(
            "affineWeightFromDynkinLabels: Dynkin labels and degree do not ",
            "determine a unique realization weight for the supplied datum");
    target := transpose matrix(QQ,{append(labels,degree)});
    coordinates := flatten entries transpose(inverse equations * target);
    lambda := weightFromRealizationCoordinates(D,coordinates);
    if datumCoordinates lambda =!= labels or
       pairing(lambda,derivation) != degree then
        error "affineWeightFromDynkinLabels: internal lifting failure";
    lambda
);

affineWeightFromDynkinLabels = method();
affineWeightFromDynkinLabels (RootDatum,List,Thing) :=
    (D,labels,degree) ->
        affineStructureWeightFromDynkinDegree(
            D,labels,degree,affineDerivation D);
affineWeightFromDynkinLabels (RootDatum,Matrix,Thing) :=
    (D,labels,degree) ->
        affineStructureWeightFromDynkinDegree(
            D,labels,degree,affineDerivation D);
affineWeightFromDynkinLabels (RootDatum,List,Thing,KMCoweight) :=
    (D,labels,degree,derivation) ->
        affineStructureWeightFromDynkinDegree(
            D,labels,degree,derivation);
affineWeightFromDynkinLabels (RootDatum,Matrix,Thing,KMCoweight) :=
    (D,labels,degree,derivation) ->
        affineStructureWeightFromDynkinDegree(
            D,labels,degree,derivation);
affineWeightFromDynkinLabels (Thing,Thing,Thing) :=
    (D,labels,degree) -> error concatenate(
        "affineWeightFromDynkinLabels: expected an affine RootDatum, ",
        "Dynkin labels, and a rational degree");
affineWeightFromDynkinLabels (Thing,Thing,Thing,Thing) :=
    (D,labels,degree,derivation) -> error concatenate(
        "affineWeightFromDynkinLabels: expected an affine RootDatum, ",
        "Dynkin labels, a rational degree, and a complete normalized ",
        "KMCoweight derivation");

-- The degree is lambda(d).  A normalized affine derivation must satisfy
-- delta(d)=1.  Both objects must carry complete realization coordinates;
-- a Dynkin-label class cannot distinguish lambda from lambda+k*delta.
affineDegree = method();
affineDegree (KMWeight,KMCoweight) := (lambda,derivation) -> (
    D := parentDatum lambda;
    if parentDatum derivation =!= D then
        error "affineDegree: weight and derivation belong to different RootDatum parents";
    if lambda#"realizationCoordinates" === null then
        error concatenate(
            "affineDegree: the weight is incomplete; Dynkin labels determine ",
            "only its class modulo the delta direction. Use ",
            "affineWeightFromDynkinLabels or weightFromRealizationCoordinates to construct ",
            "a complete KMWeight");
    if derivation#"realizationCoordinates" === null then
        error concatenate(
            "affineDegree: the derivation is incomplete; simple-root ",
            "evaluations determine it only modulo the central direction. ",
            "Use affineDerivation or coweightFromRealizationCoordinates");
    delta := affineNullRoot D;
    if pairing(delta,derivation) != 1 then
        error "affineDegree: the supplied derivation must be normalized by delta(d) = 1";
    pairing(lambda,derivation)
);
affineDegree (KMRoot,KMCoweight) := (beta,derivation) ->
    affineDegree(toWeight beta,derivation);
affineDegree (Matrix,List) := (A,lambda) -> error concatenate(
    "affineDegree: lambda(d) requires a complete KMWeight and an explicitly ",
    "supplied complete KMCoweight derivation; Dynkin labels alone identify ",
    "lambda and lambda+k*delta");
affineDegree (Matrix,Matrix) := (A,lambda) -> error concatenate(
    "affineDegree: lambda(d) requires a complete KMWeight and an explicitly ",
    "supplied complete KMCoweight derivation");
affineDegree (Thing,Thing) := (lambda,derivation) -> error concatenate(
    "affineDegree: expected a complete KMWeight or KMRoot and a complete ",
    "normalized KMCoweight derivation belonging to the same affine RootDatum");


-------------------------------------------------------------------------
-- Weyl Parabolic
-------------------------------------------------------------------------

-- Parent-aware Weyl groups and standard parabolics.
--
-- The legacy matrix/list routines remain available.  These objects add the
-- information that a bare reflection word or node list cannot carry: its
-- Cartan parent and an unambiguous removed-node versus Levi-node convention.

KMWeylGroup = new Type of HashTable
StandardParabolic = new Type of HashTable

WeylElement == WeylElement := Boolean => (u,v) -> (
    if not (u#?"parent" and v#?"parent") then
        error "WeylElement == WeylElement: equality requires parent-aware elements constructed with weylElement";
    weylParent u === weylParent v
        and weylElementMatrix u == weylElementMatrix v
)
KMWeylGroup == KMWeylGroup := (W,V) -> W === V
StandardParabolic == StandardParabolic := (P,Q) -> P === Q

-- Resolve a public node specifier against a Cartan datum.  Integers in
-- 1..rank retain the package-wide positional convention; every other value
-- is matched against the datum's pairwise-distinct NodeLabels.  This agrees
-- with the typed crystal API and still permits labels such as 10,20,30.
draftNodePosition = (D,x,who) -> (
    r := D#"rank";
    -- A position/label record is the unambiguous form used by APIs such as
    -- minusculeNodes(...,NodeFormat=>"Records").  Accepting it here makes
    -- those outputs composable with every labeled Weyl and parabolic
    -- command, including when custom integer labels collide with positions.
    if instance(x,HashTable) and x#?"position" and x#?"label" then (
        i := x#"position";
        if not instance(i,ZZ) or i < 1 or i > r then
            error(who | ": node record has an invalid position");
        if not draftSameNodeLabel(D#"nodeLabels"#(i-1),x#"label") then
            error(who | ": node record label does not match its position");
        return i;
    );
    if instance(x,ZZ) and x >= 1 and x <= r then return x;
    labels := D#"nodeLabels";
    matches := select(toList(0..#labels-1),i ->
        draftSameNodeLabel(labels#i,x));
    if #matches == 0 then error concatenate(
        who,": unknown node label ",toString x,
        "; available labels are ",toString labels);
    first(matches)+1
)

draftResolveNodeSet = (D,nodes,who) -> (
    if not instance(nodes,List) and not instance(nodes,Set) then
        error(who | ": node set must be a list or set");
    validateNodeSet(datumCartanMatrix D,
        apply(toList nodes,x -> draftNodePosition(D,x,who)),who)
)

draftResolveWeylWord = (D,word,who) -> (
    if not instance(word,List) then error(who | ": word must be a list");
    validateWeylWord(datumCartanMatrix D,
        apply(word,x -> draftNodePosition(D,x,who)),who)
)

weylGroup = method()
weylGroup CartanDatum := D -> new KMWeylGroup from {"cartanDatum" => D}
weylGroup RootDatum := D -> weylGroup cartanDatum D
weylGroup Realization := R -> weylGroup cartanDatum R
weylGroup Matrix := A -> weylGroup cartanDatum A
weylGroup String := s -> weylGroup cartanDatum s

toString KMWeylGroup := W -> concatenate(
    "WeylGroup(",toString(W#"cartanDatum"),")")
net KMWeylGroup := W -> net toString W

weylElement = method()
weylElement (KMWeylGroup,List) := (W,word) -> (
    A := datumCartanMatrix(W#"cartanDatum");
    resolvedWord := draftResolveWeylWord(W#"cartanDatum",word,"weylElement");
    reduced := reducedWord(A,resolvedWord);
    new WeylElement from {
        "parent" => W,
        "word" => reduced,
        "length" => #reduced,
        "rootActionEntries" => entries weylElementMatrix(A,reduced)
    }
)
weylElement (CartanDatum,List) := (D,word) -> weylElement(weylGroup D,word)
weylElement (RootDatum,List) := (D,word) -> weylElement(weylGroup D,word)
weylElement (Realization,List) := (R,word) -> weylElement(weylGroup R,word)
weylElement (Matrix,List) := (A,word) -> weylElement(weylGroup A,word)
weylElement (String,List) := (s,word) -> weylElement(weylGroup s,word)

weylParent = method()
weylParent WeylElement := w -> (
    if not w#?"parent" then
        error "weylParent: this legacy WeylElement has no parent; construct it with weylElement";
    w#"parent"
)

identityWeylElement = method()
identityWeylElement KMWeylGroup := W -> weylElement(W,{})

simpleWeylReflection = method()
simpleWeylReflection (KMWeylGroup,ZZ) := (W,i) -> weylElement(W,{i})
simpleWeylReflection (KMWeylGroup,String) := (W,label) ->
    weylElement(W,{label})
simpleWeylReflection (KMWeylGroup,Thing) := (W,label) ->
    weylElement(W,{label})

inverseWeylElement = method()
inverseWeylElement WeylElement := w -> weylElement(weylParent w,reverse reducedWord w)

WeylElement * WeylElement := (u,v) -> (
    if weylParent u =!= weylParent v then
        error "WeylElement * WeylElement: elements have different Weyl-group parents";
    weylElement(weylParent u,reducedWord u | reducedWord v)
)

weylElementMatrix WeylElement := w -> (
    if w#?"rootActionEntries" then matrix(ZZ,w#"rootActionEntries")
    else w#"matrix"
)
weylLength WeylElement := w -> if w#?"length" then w#"length" else #(w#"word")
reducedWord WeylElement := w -> w#"word"
leftDescentSet WeylElement := w -> leftDescentSet(
    datumCartanMatrix((weylParent w)#"cartanDatum"),reducedWord w)
rightDescentSet WeylElement := w -> rightDescentSet(
    datumCartanMatrix((weylParent w)#"cartanDatum"),reducedWord w)
bruhatLessOrEqual (WeylElement,WeylElement) := (u,v) -> (
    if weylParent u =!= weylParent v then
        error "bruhatLessOrEqual: elements have different Weyl-group parents";
    bruhatLessOrEqual(
        datumCartanMatrix((weylParent u)#"cartanDatum"),reducedWord u,reducedWord v)
)

draftWeylActionRootCoordinates = (w,x,dual) -> (
    W := weylParent w;
    if (W#"cartanDatum") =!= cartanDatum(parentDatum x) then
        error "weylAction: the Weyl element and lattice element have different Cartan parents";
    A := datumCartanMatrix(W#"cartanDatum");
    M := if dual then weylElementMatrix(transpose A,reducedWord w)
        else weylElementMatrix w;
    flatten entries(M * transpose matrix{datumCoordinates x})
)

weylAction = method()
weylAction (WeylElement,KMRoot) := (w,x) ->
    kmRoot(parentDatum x,draftWeylActionRootCoordinates(w,x,false))
weylAction (WeylElement,KMCoroot) := (w,x) ->
    kmCoroot(parentDatum x,draftWeylActionRootCoordinates(w,x,true))

draftSimpleWeightLabels = (A,i,labels) -> apply(#labels,j ->
    labels#j - labels#(i-1)*A_(j,i-1))
draftSimpleCoweightLabels = (A,i,labels) -> apply(#labels,j ->
    labels#j - labels#(i-1)*A_(i-1,j))

weylAction (WeylElement,KMWeight) := (w,x) -> (
    W := weylParent w;
    D := parentDatum x;
    if W#"cartanDatum" =!= cartanDatum D then
        error "weylAction: the Weyl element and weight have different Cartan parents";
    A := datumCartanMatrix(W#"cartanDatum");
    labels := datumCoordinates x;
    ambient := x#"realizationCoordinates";
    positions := if #(reducedWord w) == 0 then {} else reverse toList(0..#(reducedWord w)-1);
    for pos in positions do (
        i := (reducedWord w)#pos;
        coefficient := labels#(i-1);
        labels = draftSimpleWeightLabels(A,i,labels);
        if ambient =!= null then (
            alpha := (D#"realization")#"simpleRootCoordinates"#(i-1);
            ambient = draftAdd(ambient,draftScale(-coefficient,alpha));
        );
    );
    if ambient === null then kmWeight(D,labels)
    else weightFromRealizationCoordinates(D,ambient)
)

weylAction (WeylElement,KMCoweight) := (w,x) -> (
    W := weylParent w;
    D := parentDatum x;
    if W#"cartanDatum" =!= cartanDatum D then
        error "weylAction: the Weyl element and coweight have different Cartan parents";
    A := datumCartanMatrix(W#"cartanDatum");
    labels := datumCoordinates x;
    ambient := x#"realizationCoordinates";
    positions := if #(reducedWord w) == 0 then {} else reverse toList(0..#(reducedWord w)-1);
    for pos in positions do (
        i := (reducedWord w)#pos;
        coefficient := labels#(i-1);
        labels = draftSimpleCoweightLabels(A,i,labels);
        if ambient =!= null then (
            alphaVee := (D#"realization")#"simpleCorootCoordinates"#(i-1);
            ambient = draftAdd(ambient,draftScale(-coefficient,alphaVee));
        );
    );
    if ambient === null then kmCoweight(D,labels)
    else coweightFromRealizationCoordinates(D,ambient)
)

-- Typed forwarding for Coxeter data and root-action matrices.  Matrix
-- overloads remain the implementation layer because they also support
-- arbitrary generalized Cartan matrices containing infinite Coxeter entries.
coxeterMatrix CartanDatum := D -> coxeterMatrix(datumCartanMatrix D)
coxeterMatrix Realization := R -> coxeterMatrix(datumCartanMatrix R)
coxeterMatrix RootDatum := D -> coxeterMatrix(datumCartanMatrix D)
coxeterMatrix KMWeylGroup := W ->
    coxeterMatrix(datumCartanMatrix(W#"cartanDatum"))
coxeterMatrix (CartanDatum,ZZ,ZZ) := (D,i,j) ->
    coxeterMatrix(datumCartanMatrix D,
        draftNodePosition(D,i,"coxeterMatrix"),
        draftNodePosition(D,j,"coxeterMatrix"))
coxeterMatrix (Realization,ZZ,ZZ) := (R,i,j) ->
    coxeterMatrix(cartanDatum R,i,j)
coxeterMatrix (RootDatum,ZZ,ZZ) := (D,i,j) ->
    coxeterMatrix(cartanDatum D,i,j)
coxeterMatrix (KMWeylGroup,ZZ,ZZ) := (W,i,j) ->
    coxeterMatrix(W#"cartanDatum",i,j)
coxeterMatrix (CartanDatum,String,String) := (D,i,j) ->
    coxeterMatrix(D,draftNodePosition(D,i,"coxeterMatrix"),
        draftNodePosition(D,j,"coxeterMatrix"))
coxeterMatrix (Realization,String,String) := (R,i,j) ->
    coxeterMatrix(cartanDatum R,i,j)
coxeterMatrix (RootDatum,String,String) := (D,i,j) ->
    coxeterMatrix(cartanDatum D,i,j)
coxeterMatrix (KMWeylGroup,String,String) := (W,i,j) ->
    coxeterMatrix(W#"cartanDatum",i,j)
coxeterMatrix (CartanDatum,Thing,Thing) := (D,i,j) ->
    coxeterMatrix(D,draftNodePosition(D,i,"coxeterMatrix"),
        draftNodePosition(D,j,"coxeterMatrix"))
coxeterMatrix (Realization,Thing,Thing) := (R,i,j) ->
    coxeterMatrix(cartanDatum R,i,j)
coxeterMatrix (RootDatum,Thing,Thing) := (D,i,j) ->
    coxeterMatrix(cartanDatum D,i,j)
coxeterMatrix (KMWeylGroup,Thing,Thing) := (W,i,j) ->
    coxeterMatrix(W#"cartanDatum",i,j)

reflectionMatrix (CartanDatum,ZZ) := (D,i) ->
    reflectionMatrix(datumCartanMatrix D,
        draftNodePosition(D,i,"reflectionMatrix"))
reflectionMatrix (Realization,ZZ) := (R,i) ->
    reflectionMatrix(cartanDatum R,i)
reflectionMatrix (RootDatum,ZZ) := (D,i) ->
    reflectionMatrix(cartanDatum D,i)
reflectionMatrix (KMWeylGroup,ZZ) := (W,i) ->
    reflectionMatrix(W#"cartanDatum",i)
reflectionMatrix (CartanDatum,String) := (D,i) ->
    reflectionMatrix(D,draftNodePosition(D,i,"reflectionMatrix"))
reflectionMatrix (Realization,String) := (R,i) ->
    reflectionMatrix(cartanDatum R,i)
reflectionMatrix (RootDatum,String) := (D,i) ->
    reflectionMatrix(cartanDatum D,i)
reflectionMatrix (KMWeylGroup,String) := (W,i) ->
    reflectionMatrix(W#"cartanDatum",i)
reflectionMatrix (CartanDatum,Thing) := (D,i) ->
    reflectionMatrix(D,draftNodePosition(D,i,"reflectionMatrix"))
reflectionMatrix (Realization,Thing) := (R,i) ->
    reflectionMatrix(cartanDatum R,i)
reflectionMatrix (RootDatum,Thing) := (D,i) ->
    reflectionMatrix(cartanDatum D,i)
reflectionMatrix (KMWeylGroup,Thing) := (W,i) ->
    reflectionMatrix(W#"cartanDatum",i)

-- A StandardParabolic records both complementary node sets.  Positional
-- input follows the package's historical convention: it lists removed
-- (grading) nodes.  The option form makes either convention explicit.
standardParabolic = method(Options => {RemovedNodes => null,LeviNodes => null})

draftStandardParabolic = (D,removed,levi,who) -> (
    r := D#"rank";
    allNodes := toList(1..r);
    if removed =!= null and levi =!= null then
        error(who | ": specify RemovedNodes or LeviNodes, not both");
    if removed === null and levi === null then
        error(who | ": specify RemovedNodes or LeviNodes");
    if removed =!= null then (
        removed = draftResolveNodeSet(D,removed,who);
        levi = select(allNodes,i -> not member(i,set removed));
    ) else (
        levi = draftResolveNodeSet(D,levi,who);
        removed = select(allNodes,i -> not member(i,set levi));
    );
    levi0 := apply(levi,i -> i-1);
    AL := submatrix(datumCartanMatrix D,levi0,levi0);
    leviLabels := apply(levi,i -> D#"nodeLabels"#(i-1));
    -- The parabolic with no removed nodes has the whole Cartan datum as
    -- its Levi.  Reuse that immutable parent rather than manufacturing an
    -- isomorphic copy with different identity.
    LD := if levi === allNodes then D
        else cartanDatum(AL,NodeLabels=>leviLabels);
    new StandardParabolic from {
        "cartanDatum" => D,
        "removedNodes" => removed,
        "leviNodes" => levi,
        "leviCartanDatum" => LD,
        "gradingVector" => apply(allNodes,i -> if member(i,set removed) then 1 else 0),
        "nodeConvention" => "removed nodes are precisely the grading nodes; all public node numbers are 1-based"
    }
)

standardParabolic CartanDatum := opts -> D ->
    draftStandardParabolic(D,opts.RemovedNodes,opts.LeviNodes,"standardParabolic")
standardParabolic RootDatum := opts -> D -> standardParabolic(cartanDatum D,
    RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic Matrix := opts -> A -> standardParabolic(cartanDatum A,
    RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic Realization := opts -> R -> standardParabolic(cartanDatum R,
    RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic String := opts -> s -> standardParabolic(cartanDatum s,
    RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic (CartanDatum,List) := opts -> (D,J) -> (
    if opts.RemovedNodes =!= null or opts.LeviNodes =!= null then
        error "standardParabolic: positional node input cannot be combined with RemovedNodes or LeviNodes";
    draftStandardParabolic(D,J,null,"standardParabolic")
)
standardParabolic (RootDatum,List) := opts -> (D,J) -> standardParabolic(
    cartanDatum D,J,RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic (Realization,List) := opts -> (R,J) -> standardParabolic(
    cartanDatum R,J,RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic (Matrix,List) := opts -> (A,J) -> standardParabolic(
    cartanDatum A,J,RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic (String,List) := opts -> (s,J) -> standardParabolic(
    cartanDatum s,J,RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic (CartanDatum,Set) := opts -> (D,J) -> standardParabolic(
    D,toList J,RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic (RootDatum,Set) := opts -> (D,J) -> standardParabolic(
    cartanDatum D,toList J,
    RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic (Realization,Set) := opts -> (R,J) -> standardParabolic(
    cartanDatum R,toList J,
    RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic (Matrix,Set) := opts -> (A,J) -> standardParabolic(
    cartanDatum A,toList J,
    RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)
standardParabolic (String,Set) := opts -> (s,J) -> standardParabolic(
    cartanDatum s,toList J,
    RemovedNodes=>opts.RemovedNodes,LeviNodes=>opts.LeviNodes)

toString StandardParabolic := P -> concatenate(
    "StandardParabolic(removed=",toString P#"removedNodes",
    ", Levi=",toString P#"leviNodes",")")
net StandardParabolic := P -> net toString P

removedNodes = method()
removedNodes StandardParabolic := P -> P#"removedNodes"
leviNodes = method()
leviNodes StandardParabolic := P -> P#"leviNodes"
parabolicLevi = method()
parabolicLevi StandardParabolic := P -> P#"leviCartanDatum"

parabolicDegree = method()
parabolicDegree (StandardParabolic,KMRoot) := (P,x) -> (
    if P#"cartanDatum" =!= cartanDatum(parentDatum x) then
        error "parabolicDegree: parabolic and root have different Cartan parents";
    sum apply(removedNodes P,i -> (datumCoordinates x)#(i-1))
)

isLeviRoot = method()
isLeviRoot (StandardParabolic,KMRoot) := (P,x) -> parabolicDegree(P,x) == 0

leviSubalgebra StandardParabolic := P -> (
    L := leviSubalgebra(datumCartanMatrix(P#"cartanDatum"),removedNodes P);
    new LeviData from join(toList pairs L,{
        "parabolicObject" => P,
        "cartanDatum" => parabolicLevi P
    })
)

parabolicData StandardParabolic := P -> (
    legacy := parabolicData(datumCartanMatrix(P#"cartanDatum"),removedNodes P);
    new HashTable from join(toList pairs legacy,{
        "parabolicObject" => P,
        "cartanDatum" => P#"cartanDatum",
        "leviCartanDatum" => parabolicLevi P
    })
)

-- StandardParabolic overloads are the preferred public interface.  They
-- remove the historical convention hazard: root grading and Levi
-- decompositions use removed nodes, whereas W/W_L routines use Levi nodes.
parabolicDiagram StandardParabolic := P -> (
    D := P#"cartanDatum";
    labels := D#"nodeLabels";
    positions := toList(1..D#"rank");
    legend := if labels === positions then "" else
        "Node labels: " |
        demark(", ",apply(positions,i ->
            toString i | " = " | toString labels#(i-1))) | newline;
    legend | parabolicDiagram(datumCartanMatrix D,removedNodes P)
)

cartanPositiveRootsDegree (StandardParabolic,ZZ) := (P,d) ->
    cartanPositiveRootsDegree(
        datumCartanMatrix(P#"cartanDatum"),d,removedNodes P)

nilradicalRoots StandardParabolic := P ->
    nilradicalRoots(datumCartanMatrix(P#"cartanDatum"),removedNodes P)

parabolicDegrees StandardParabolic := P ->
    parabolicDegrees(datumCartanMatrix(P#"cartanDatum"),removedNodes P)

-- Components are reported in ambient node positions, matching
-- (leviSubalgebra P)#"components", rather than silently renumbering them.
leviConnectedComponents StandardParabolic := P ->
    (leviSubalgebra P)#"components"

leviHighestWeights (StandardParabolic,ZZ) := opts -> (P,d) ->
    leviHighestWeights(
        datumCartanMatrix(P#"cartanDatum"),d,removedNodes P,
        WeightFormat=>opts.WeightFormat)

irrepData (StandardParabolic,Irrep) := opts -> (P,V) ->
    irrepData(datumCartanMatrix(parabolicLevi P),V,
        WeightFormat=>opts.WeightFormat)

irrepDimension (StandardParabolic,Irrep) := opts -> (P,V) ->
    irrepDimension(datumCartanMatrix(parabolicLevi P),V,
        WeightFormat=>opts.WeightFormat)

decompositionDimension (StandardParabolic,Tally) := opts -> (P,T) ->
    decompositionDimension(datumCartanMatrix(parabolicLevi P),T,
        WeightFormat=>opts.WeightFormat)

parabolicCosetReps (StandardParabolic,ZZ) := opts -> (P,k) ->
    parabolicCosetReps(
        datumCartanMatrix(P#"cartanDatum"),k,leviNodes P,
        MaxElements=>opts.MaxElements)

parabolicDotImages (StandardParabolic,ZZ,List) := opts -> (P,k,lambda) ->
    parabolicDotImages(
        datumCartanMatrix(P#"cartanDatum"),k,leviNodes P,lambda,
        MaxElements=>opts.MaxElements)

kostantLieAlgCohomology (StandardParabolic,List) := opts -> (P,lambda) ->
    kostantLieAlgCohomology(
        datumCartanMatrix(P#"cartanDatum"),removedNodes P,lambda,
        WeightFormat=>opts.WeightFormat,
        MaxElements=>opts.MaxElements,
        MaxDegree=>opts.MaxDegree,
        IncludeCentralCharacter=>opts.IncludeCentralCharacter)

cohomologyDimensions (StandardParabolic,HashTable) := opts -> (P,H) ->
    cohomologyDimensions(
        datumCartanMatrix(P#"cartanDatum"),removedNodes P,H,
        WeightFormat=>opts.WeightFormat)


-------------------------------------------------------------------------
-- Finite Structure
-------------------------------------------------------------------------

-- Finite structural and representation-theoretic utilities.
-- This internal source module uses the same conventions as KacMoody:
--
--   * Cartan entries are A_(i,j) = <alpha_j, alpha_i^vee>;
--   * public node labels and Weyl words are 1-based;
--   * weights are integral Dynkin-label lists;
--   * a word {i_1,...,i_k} denotes s_(i_1)...s_(i_k), so its rightmost
--     reflection acts first;
--   * the normalized invariant form makes every long root have norm 2.

------------------------------------------------------------
-- Boundary validation
------------------------------------------------------------

finiteDraftCartan = (A, who) -> (
    if not instance(A, Matrix) then error(who | ": A must be a matrix");
    validationCache := "KacMoodyFiniteDraft.validatedCartan";
    if ring A === ZZ then (
        if A.cache#?validationCache then return A;
        if not isCartanMatrix A then
            error(who | ": input is not a generalized Cartan matrix");
        A.cache#validationCache = true;
        return A;
    );
    -- Reuse the package-wide canonical integral representative.  Besides
    -- keeping all root combinatorics over ZZ, this makes caches persistent
    -- when a caller supplied an integral Cartan matrix over QQ.
    integralCache := "KacMoody.integralCartanMatrix";
    if A.cache#?integralCache then (
        cachedAZZ := A.cache#integralCache;
        cachedAZZ.cache#validationCache = true;
        return cachedAZZ;
    );
    AZZ := integralCartanMatrix(A, who);
    AZZ.cache#validationCache = true;
    AZZ
);

finiteDraftRequireFinite = (A, who) -> (
    A = finiteDraftCartan(A, who);
    type := if A.cache#?"cartanType" then A.cache#"cartanType"
        else cartanType A;
    if type =!= "finite" then
        error(who | ": Cartan matrix must be of finite type");
    A
);

finiteDraftRequireConnectedFinite = (A, who) -> (
    A = finiteDraftRequireFinite(A, who);
    if #(leviConnectedComponents A) =!= 1 then
        error(who | ": Cartan matrix must be connected");
    A
);

finiteDraftDynkinWeight = (A, lambda, who, requireDominant) -> (
    r := numrows A;
    raw := if instance(lambda, List) then lambda
    else if instance(lambda, Matrix) then (
        if numrows lambda =!= 1 or numcols lambda =!= r then
            error(who | ": matrix weight must have size 1 by rank");
        for j from 0 to r-1 list lambda_(0,j)
    ) else error(who | ": weight must be a list or a 1 by rank matrix");

    if #raw =!= r then error(who | ": weight must have length equal to rank");
    for x in raw list (
        xQ := try (x + 0_QQ) else
            error(who | ": Dynkin labels must be integers");
        if not instance(xQ, QQ) or denominator xQ =!= 1 then
            error(who | ": Dynkin labels must be integers");
        z := numerator xQ;
        if requireDominant and z < 0 then
            error(who | ": highest weight must be dominant");
        z
    )
);

finiteDraftWeylWord = (A, word, who) -> (
    if not instance(word, List) then error(who | ": word must be a list");
    r := numrows A;
    for i in word do if not instance(i, ZZ) or i < 1 or i > r then
        error(who | ": reflection indices must be integers between 1 and rank");
    word
);

finiteDraftNode = (A, i, who) -> (
    if not instance(i, ZZ) or i < 1 or i > numrows A then
        error(who | ": node must be an integer between 1 and rank");
    i
);

finiteDraftSimpleAction = (A, i, lambda) -> (
    i0 := i - 1;
    coefficient := lambda#i0;
    for j from 0 to numrows A - 1 list
        lambda#j - coefficient * A_(j,i0)
);

finiteDraftTypedWeightData = (lambda, who, requireDominant) -> (
    D := parentDatum lambda;
    A := finiteDraftRequireFinite(datumCartanMatrix D, who);
    labels := finiteDraftDynkinWeight(
        A, datumCoordinates lambda, who, requireDominant);
    {D,A,labels}
);

finiteDraftNodeLabels = (D, nodes) -> (
    labels := (cartanDatum D)#"nodeLabels";
    apply(nodes, i -> labels#(i-1))
);

finiteDraftWeightInRootDatum = (D,lambda,who) -> (
    if parentDatum lambda =!= D then
        error(who | ": the weight and explicit RootDatum have different parents");
    lambda
);

------------------------------------------------------------
-- Ordinary Weyl action, dominant representatives, stabilizers
------------------------------------------------------------

-- Ordinary (not dot) action on integral Dynkin coordinates.  The result is
-- always a list, even when the input weight is a 1 by rank matrix.
weylActionOnWeight = method();

weylActionOnWeight (Matrix, List, List) := (A, word, lambda) -> (
    A = finiteDraftCartan(A, "weylActionOnWeight");
    word = finiteDraftWeylWord(A, word, "weylActionOnWeight");
    mu := finiteDraftDynkinWeight(A, lambda, "weylActionOnWeight", false);
    positions := if #word == 0 then {} else reverse toList(0..#word-1);
    for pos in positions do
        mu = finiteDraftSimpleAction(A, word#pos, mu);
    mu
);

weylActionOnWeight (Matrix, List, Matrix) := (A, word, lambda) -> (
    A = finiteDraftCartan(A, "weylActionOnWeight");
    word = finiteDraftWeylWord(A, word, "weylActionOnWeight");
    mu := finiteDraftDynkinWeight(A, lambda, "weylActionOnWeight", false);
    weylActionOnWeight(A, word, mu)
);

-- Move an integral weight to the unique dominant member of its finite Weyl
-- orbit.  The returned word w is a checkable witness satisfying
--
--                      w(inputWeight) = dominantWeight.
--
-- Reflecting in a negative simple wall is the standard chamber walk.  Its
-- length is at most the number of positive roots; the explicit bound is also
-- a defensive termination check.
dominantizeWeight = method();

dominantizeWeight (Matrix, List) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "dominantizeWeight");
    input := finiteDraftDynkinWeight(A, lambda, "dominantizeWeight", false);
    mu := input;
    chronologicalReflections := {};
    maxSteps := numrows cartanPositiveRoots A;

    while any(mu, x -> x < 0) do (
        i0 := first select(toList(0..numrows A-1), j -> mu#j < 0);
        i := i0 + 1;
        mu = finiteDraftSimpleAction(A, i, mu);
        chronologicalReflections = append(chronologicalReflections, i);
        if #chronologicalReflections > maxSteps then
            error "dominantizeWeight: chamber walk exceeded the finite positive-root bound";
    );

    witness := reverse chronologicalReflections;
    walls := for j from 0 to numrows A-1 list
        if mu#j == 0 then j+1 else continue;
    new HashTable from {
        "inputWeight" => input,
        "dominantWeight" => mu,
        "word" => witness,
        "inverseWord" => reverse witness,
        "wallNodes" => walls,
        "regular" => (#walls == 0),
        "wordConvention" => "rightmost reflection acts first"
    }
);

dominantizeWeight (Matrix, Matrix) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "dominantizeWeight");
    dominantizeWeight(A,
        finiteDraftDynkinWeight(A, lambda, "dominantizeWeight", false))
);

-- The typed form retains the complete realization weight, including any
-- Weyl-fixed directions invisible in Dynkin labels.  It also supplies a
-- composable WeylElement and the user's public node labels, while retaining
-- the legacy integer-word fields for callers that need them.
dominantizeWeight KMWeight := lambda -> (
    typed := finiteDraftTypedWeightData(
        lambda, "dominantizeWeight", false);
    D := typed#0;
    A := typed#1;
    raw := dominantizeWeight(A, typed#2);
    w := weylElement(cartanDatum D, raw#"word");
    winv := inverseWeylElement w;
    dominant := weylAction(w, lambda);
    if datumCoordinates dominant =!= raw#"dominantWeight" then
        error "dominantizeWeight: internal typed Weyl-action mismatch";
    new HashTable from {
        "inputWeight" => lambda,
        "dominantWeight" => dominant,
        "word" => raw#"word",
        "inverseWord" => raw#"inverseWord",
        "toDominantElement" => w,
        "inverseElement" => winv,
        "wallNodes" => raw#"wallNodes",
        "wallNodeLabels" => finiteDraftNodeLabels(D,raw#"wallNodes"),
        "regular" => raw#"regular",
        "wordConvention" => raw#"wordConvention"
    }
);

dominantizeWeight (RootDatum, List) := (D, lambda) ->
    dominantizeWeight kmWeight(D,lambda);
dominantizeWeight (RootDatum, Matrix) := (D, lambda) ->
    dominantizeWeight kmWeight(D,lambda);
dominantizeWeight (CartanDatum, List) := (D, lambda) ->
    dominantizeWeight kmWeight(rootDatum D,lambda);
dominantizeWeight (CartanDatum, Matrix) := (D, lambda) ->
    dominantizeWeight kmWeight(rootDatum D,lambda);
dominantizeWeight (Realization, List) := (R, lambda) ->
    dominantizeWeight kmWeight(rootDatum R,lambda);
dominantizeWeight (Realization, Matrix) := (R, lambda) ->
    dominantizeWeight kmWeight(rootDatum R,lambda);
dominantizeWeight (String, List) := (s, lambda) ->
    dominantizeWeight kmWeight(rootDatum s,lambda);
dominantizeWeight (String, Matrix) := (s, lambda) ->
    dominantizeWeight kmWeight(rootDatum s,lambda);

-- Return generators for the full stabilizer of lambda, without enumerating
-- it.  If w(lambda)=mu is dominant and J={j:mu_j=0}, then
--
--              Stab(lambda) = w^(-1) W_J w.
--
-- Each word in "generatingWords" therefore fixes the original input weight.
weightStabilizer = method();

weightStabilizer (Matrix, List) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "weightStabilizer");
    data := dominantizeWeight(A, lambda);
    w := data#"word";
    walls := data#"wallNodes";
    generators := apply(walls, j ->
        reducedWord(A, (reverse w) | {j} | w));
    stabilizerOrder := if #walls == 0 then 1 else (
        indices := apply(walls, j -> j-1);
        weylGroupOrder submatrix(A, indices, indices)
    );
    new HashTable from {
        "inputWeight" => data#"inputWeight",
        "dominantWeight" => data#"dominantWeight",
        "toDominantWord" => w,
        "dominantWallNodes" => walls,
        "dominantParabolicGenerators" => apply(walls, j -> {j}),
        "generatingWords" => generators,
        "order" => stabilizerOrder,
        "wordConvention" => "rightmost reflection acts first"
    }
);

weightStabilizer (Matrix, Matrix) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "weightStabilizer");
    weightStabilizer(A,
        finiteDraftDynkinWeight(A, lambda, "weightStabilizer", false))
);

weightStabilizer KMWeight := lambda -> (
    typed := finiteDraftTypedWeightData(
        lambda, "weightStabilizer", false);
    D := typed#0;
    A := typed#1;
    raw := weightStabilizer(A, typed#2);
    w := weylElement(cartanDatum D, raw#"toDominantWord");
    dominant := weylAction(w,lambda);
    walls := raw#"dominantWallNodes";
    generators := apply(raw#"generatingWords",
        word -> weylElement(cartanDatum D,word));
    new HashTable from {
        "inputWeight" => lambda,
        "dominantWeight" => dominant,
        "toDominantWord" => raw#"toDominantWord",
        "toDominantElement" => w,
        "dominantWallNodes" => walls,
        "dominantWallNodeLabels" => finiteDraftNodeLabels(D,walls),
        "dominantParabolicGenerators" =>
            raw#"dominantParabolicGenerators",
        "dominantParabolic" =>
            standardParabolic(cartanDatum D,LeviNodes=>walls),
        "generatingWords" => raw#"generatingWords",
        "generatingElements" => generators,
        "order" => raw#"order",
        "wordConvention" => raw#"wordConvention"
    }
);

weightStabilizer (RootDatum, List) := (D, lambda) ->
    weightStabilizer kmWeight(D,lambda);
weightStabilizer (RootDatum, Matrix) := (D, lambda) ->
    weightStabilizer kmWeight(D,lambda);
weightStabilizer (CartanDatum, List) := (D, lambda) ->
    weightStabilizer kmWeight(rootDatum D,lambda);
weightStabilizer (CartanDatum, Matrix) := (D, lambda) ->
    weightStabilizer kmWeight(rootDatum D,lambda);
weightStabilizer (Realization, List) := (R, lambda) ->
    weightStabilizer kmWeight(rootDatum R,lambda);
weightStabilizer (Realization, Matrix) := (R, lambda) ->
    weightStabilizer kmWeight(rootDatum R,lambda);
weightStabilizer (String, List) := (s, lambda) ->
    weightStabilizer kmWeight(rootDatum s,lambda);
weightStabilizer (String, Matrix) := (s, lambda) ->
    weightStabilizer kmWeight(rootDatum s,lambda);

------------------------------------------------------------
-- Dual highest weights
------------------------------------------------------------

-- For a dominant lambda this is the highest weight -w_0(lambda) of the
-- contragredient representation.  The linear involution is also meaningful
-- for a nondominant integral weight, so dominance is not imposed here.
dualWeight = method();

-- Recover the opposition involution from one regular chamber walk.  If
-- eta=sum_i i*omega_i, then -eta is regular anti-dominant and the unique
-- Weyl element carrying it to the dominant chamber is w_0.  Consequently
-- w_0(-eta)=(-w_0)(eta), whose distinct coordinates record the permutation
-- i |-> i*.  This avoids constructing the longest word and is cached; warm
-- duality calls are therefore only coordinate permutations.
finiteDraftOppositionPermutation = A -> (
    cacheName := "KacMoodyFiniteDraft.oppositionPermutation";
    if A.cache#?cacheName then return A.cache#cacheName;

    r := numrows A;
    if r == 0 then (
        A.cache#cacheName = {};
        return {};
    );

    indices := toList(0..r-1);
    probe := toList(1..r);
    image := if A.cache#?"longElement" then
        apply(weylActionOnWeight(A,A.cache#"longElement",probe),x -> -x)
    else (
        mu := new MutableList from apply(probe,x -> -x);
        columnSupports := apply(indices,i ->
            select(indices,j -> A_(j,i) != 0));
        pending := new MutableList from indices;
        top := r-1;
        steps := 0;
        -- The walk has |Phi^+| steps.  Classification gives
        -- |Phi^+| <= 2r^2 for every finite crystallographic root system,
        -- including exceptional types and reducible products.  This bound
        -- guards termination.  A stack records exactly those coordinates
        -- that are negative or may have changed; a simple reflection then
        -- touches only the nonzero entries of its Cartan column.  Thus the
        -- cold walk does not copy or rescan an entire rank-r weight at every
        -- positive root.
        maxSteps := 2*r*r;
        while top >= 0 do (
            i0 := pending#top;
            top = top-1;
            if mu#i0 >= 0 then continue;
            coefficient := mu#i0;
            for j in columnSupports#i0 do (
                mu#j = mu#j - coefficient*A_(j,i0);
                if mu#j < 0 then (
                    top = top+1;
                    pending#top = j;
                );
            );
            steps = steps + 1;
            if steps > maxSteps then
                error "dualWeight: internal opposition chamber walk exceeded the finite root bound";
        );
        answer := toList mu;
        if any(answer,x -> x < 0) then
            error "dualWeight: internal opposition stack lost a negative wall";
        answer
    );

    -- permutation#j is the zero-based input coordinate that becomes output
    -- coordinate j.  Opposition is an involutive automorphism of the full
    -- oriented Cartan matrix, not merely of its underlying Coxeter graph.
    permutation := apply(image,x -> x-1);
    if sort permutation =!= indices then
        error "dualWeight: internal opposition calculation did not produce a node permutation";
    if apply(permutation,i -> permutation#i) =!= indices then
        error "dualWeight: internal opposition permutation is not an involution";
    if any(indices,i -> any(indices,j ->
        A_(permutation#i,permutation#j) != A_(i,j))) then
        error "dualWeight: internal opposition permutation does not preserve the Cartan matrix";

    A.cache#cacheName = permutation;
    permutation
);

dualWeight (Matrix, List) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "dualWeight");
    lam := finiteDraftDynkinWeight(A, lambda, "dualWeight", false);
    apply(finiteDraftOppositionPermutation A, i -> lam#i)
);

dualWeight (Matrix, Matrix) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "dualWeight");
    matrix(ring lambda,{dualWeight(A,
        finiteDraftDynkinWeight(A, lambda, "dualWeight", false))})
);

-- A complete KMWeight in a nonminimal realization may have a Weyl-fixed
-- component z that Dynkin labels do not see.  Writing lambda=lambda_ss+z,
-- duality gives (-w_0 lambda_ss)-z.  The root-span representative of Dynkin
-- labels l is l*(A^T)^(-1)*R, where the rows of R are the simple roots.
-- This formula both preserves the exact parent and negates the hidden
-- central character without applying a quadratic-length longest word.
dualWeight KMWeight := lambda -> (
    typed := finiteDraftTypedWeightData(lambda, "dualWeight", false);
    D := typed#0;
    A := typed#1;
    lam := typed#2;
    dualLabels := dualWeight(A,lam);
    ambient := lambda#"realizationCoordinates";
    if ambient === null then return kmWeight(D,dualLabels);
    if numrows A == 0 then
        return weightFromRealizationCoordinates(
            D,apply(ambient,x -> -x));
    R := draftSimpleRootMatrix(realization D);
    sectionMatrix := inverse transpose(A ** QQ) * R;
    semisimplePart := matrix(QQ,{lam}) * sectionMatrix;
    dualSemisimplePart := matrix(QQ,{dualLabels}) * sectionMatrix;
    dualAmbient := dualSemisimplePart + semisimplePart
        - matrix(QQ,{ambient});
    answer := weightFromRealizationCoordinates(
        D,flatten entries dualAmbient);
    if datumCoordinates answer =!= dualLabels then
        error "dualWeight: internal complete-weight reconstruction mismatch";
    answer
);

dualWeight (RootDatum, List) := (D, lambda) ->
    dualWeight kmWeight(D,lambda);
dualWeight (RootDatum, Matrix) := (D, lambda) ->
    dualWeight kmWeight(D,lambda);
dualWeight (CartanDatum, List) := (D, lambda) ->
    dualWeight kmWeight(rootDatum D,lambda);
dualWeight (CartanDatum, Matrix) := (D, lambda) ->
    dualWeight kmWeight(rootDatum D,lambda);
dualWeight (Realization, List) := (R, lambda) ->
    dualWeight kmWeight(rootDatum R,lambda);
dualWeight (Realization, Matrix) := (R, lambda) ->
    dualWeight kmWeight(rootDatum R,lambda);
dualWeight (String, List) := (s, lambda) ->
    dualWeight kmWeight(rootDatum s,lambda);
dualWeight (String, Matrix) := (s, lambda) ->
    dualWeight kmWeight(rootDatum s,lambda);

-- Opposition-fixedness makes sense for every integral lattice vector.
-- Representation-theoretic self-duality is deliberately the stricter
-- dominant-highest-weight predicate below.
isOppositionFixed = method();

isOppositionFixed (Matrix, List) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "isOppositionFixed");
    lam := finiteDraftDynkinWeight(
        A, lambda, "isOppositionFixed", false);
    dualWeight(A, lam) === lam
);

isOppositionFixed (Matrix, Matrix) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "isOppositionFixed");
    isOppositionFixed(A,
        finiteDraftDynkinWeight(A,lambda,"isOppositionFixed",false))
);

isOppositionFixed KMWeight := lambda -> (
    finiteDraftTypedWeightData(lambda,"isOppositionFixed",false);
    dualWeight lambda === lambda
);

isOppositionFixed (RootDatum, List) := (D, lambda) ->
    isOppositionFixed kmWeight(D,lambda);
isOppositionFixed (RootDatum, Matrix) := (D, lambda) ->
    isOppositionFixed kmWeight(D,lambda);
isOppositionFixed (CartanDatum, List) := (D, lambda) ->
    isOppositionFixed kmWeight(rootDatum D,lambda);
isOppositionFixed (CartanDatum, Matrix) := (D, lambda) ->
    isOppositionFixed kmWeight(rootDatum D,lambda);
isOppositionFixed (Realization, List) := (R, lambda) ->
    isOppositionFixed kmWeight(rootDatum R,lambda);
isOppositionFixed (Realization, Matrix) := (R, lambda) ->
    isOppositionFixed kmWeight(rootDatum R,lambda);
isOppositionFixed (String, List) := (s, lambda) ->
    isOppositionFixed kmWeight(rootDatum s,lambda);
isOppositionFixed (String, Matrix) := (s, lambda) ->
    isOppositionFixed kmWeight(rootDatum s,lambda);

isSelfDual = method();

isSelfDual (Matrix, List) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "isSelfDual");
    lam := finiteDraftDynkinWeight(A, lambda, "isSelfDual", true);
    dualWeight(A,lam) === lam
);

isSelfDual (Matrix, Matrix) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "isSelfDual");
    isSelfDual(A,
        finiteDraftDynkinWeight(A,lambda,"isSelfDual",true))
);

isSelfDual KMWeight := lambda -> (
    typed := finiteDraftTypedWeightData(lambda,"isSelfDual",true);
    D := typed#0;
    if lambda#"realizationCoordinates" === null
       and (realization D)#"dimension" > (cartanDatum D)#"rank" then
        error concatenate(
            "isSelfDual: Dynkin labels do not determine the central ",
            "character in this nonminimal realization; supply a complete ",
            "weight with weightFromRealizationCoordinates");
    dualWeight lambda === lambda
);

isSelfDual (RootDatum, List) := (D, lambda) ->
    isSelfDual kmWeight(D,lambda);
isSelfDual (RootDatum, Matrix) := (D, lambda) ->
    isSelfDual kmWeight(D,lambda);
isSelfDual (CartanDatum, List) := (D, lambda) ->
    isSelfDual kmWeight(rootDatum D,lambda);
isSelfDual (CartanDatum, Matrix) := (D, lambda) ->
    isSelfDual kmWeight(rootDatum D,lambda);
isSelfDual (Realization, List) := (R, lambda) ->
    isSelfDual kmWeight(rootDatum R,lambda);
isSelfDual (Realization, Matrix) := (R, lambda) ->
    isSelfDual kmWeight(rootDatum R,lambda);
isSelfDual (String, List) := (s, lambda) ->
    isSelfDual kmWeight(rootDatum s,lambda);
isSelfDual (String, Matrix) := (s, lambda) ->
    isSelfDual kmWeight(rootDatum s,lambda);

------------------------------------------------------------
-- Coweights, marks, comarks, and root lengths
------------------------------------------------------------

-- Rows are the fundamental coweights in the simple-coroot basis.  Thus, if
-- F=fundamentalCoweights(A), then F*A is the identity pairing matrix.
fundamentalCoweights = method();

fundamentalCoweights Matrix := A -> (
    A = finiteDraftRequireFinite(A, "fundamentalCoweights");
    cacheName := "KacMoodyFiniteDraft.fundamentalCoweights";
    if A.cache#?cacheName then return A.cache#cacheName;
    F := inverse(A ** QQ);
    A.cache#cacheName = F;
    F
);

-- Gram matrix in the simple-root basis, normalized independently on every
-- connected component so that long roots have squared norm 2.  In a
-- nonsimply-laced component short roots consequently have norm 2/r, where r
-- is the lacing number.
normalizedInvariantForm = method();

normalizedInvariantForm Matrix := A -> (
    A = finiteDraftRequireFinite(A, "normalizedInvariantForm");
    cacheName := "KacMoodyFiniteDraft.normalizedInvariantForm";
    if A.cache#?cacheName then return A.cache#cacheName;
    D := symmetrizer A;
    if D === null then
        error "normalizedInvariantForm: Cartan matrix is not symmetrizable";
    normalizedDiagonal := new MutableList from
        for i from 0 to numrows A-1 list 0_QQ;
    for component in leviConnectedComponents A do (
        scale := max apply(component, i -> D_(i-1,i-1));
        for i in component do
            normalizedDiagonal#(i-1) = D_(i-1,i-1) / scale;
    );
    B := diagonalMatrix(toList normalizedDiagonal) * (A ** QQ);
    if B != transpose B then
        error "normalizedInvariantForm: internal symmetrization failure";
    A.cache#cacheName = B;
    B
);

-- Coefficients of the highest root in the simple-root basis.
marks = method();
marks Matrix := A -> (
    A = finiteDraftRequireConnectedFinite(A, "marks");
    highestRoot A
);

-- The coroot attached to the highest (long) root, in simple-coroot
-- coordinates.  Equivalently, this is the highest short root of the dual
-- root system.  It is intentionally not called the highest root of the full
-- coroot system; the latter is used by the minuscule criterion below.
highestCoroot = method();
highestCoroot Matrix := A -> (
    A = finiteDraftRequireConnectedFinite(A, "highestCoroot");
    highestShortRoot transpose A
);

-- Coefficients of the highest root in the simple-coroot basis.  With the
-- long-root-norm-2 convention, sum(comarks(A))+1 is the dual Coxeter number.
comarks = method();
comarks Matrix := A -> (
    A = finiteDraftRequireConnectedFinite(A, "comarks");
    highestCoroot A
);

lacingNumber = method();
lacingNumber Matrix := A -> (
    A = finiteDraftRequireConnectedFinite(A, "lacingNumber");
    D := symmetrizer A;
    diagonal := for i from 0 to numrows A-1 list D_(i,i);
    ratio := (max diagonal) / (min diagonal);
    if denominator ratio =!= 1 or not member(numerator ratio, {1,2,3}) then
        error "lacingNumber: unexpected finite root-length ratio";
    numerator ratio
);

------------------------------------------------------------
-- Weyl exponents and invariant degrees
------------------------------------------------------------

-- The exponents are the conjugate partition to the positive-root height
-- distribution.  This works componentwise without a classification table.
weylExponents = method();
weylExponents Matrix := A -> (
    A = finiteDraftRequireFinite(A, "weylExponents");
    cacheName := "KacMoodyFiniteDraft.weylExponents";
    if A.cache#?cacheName then return A.cache#cacheName;
    if numrows A == 0 then (
        A.cache#cacheName = {};
        return {};
    );
    roots := entries cartanPositiveRoots A;
    maximumHeight := max apply(roots, rootHeight);
    heightCounts := for h from 1 to maximumHeight list
        #(select(roots, beta -> rootHeight beta == h));
    answer := sort for i from 1 to numrows A list
        #(select(heightCounts, count -> count >= i));
    A.cache#cacheName = answer;
    answer
);

invariantDegrees = method();
invariantDegrees Matrix := A -> (
    A = finiteDraftRequireFinite(A, "invariantDegrees");
    apply(weylExponents A, m -> m + 1)
);

------------------------------------------------------------
-- Minuscule and cominuscule criteria
------------------------------------------------------------

-- For a dominant weight lambda, minuscule means
-- <lambda,beta^vee> <= 1 for every positive root beta.  Positive roots of
-- transpose(A), written in its simple-root basis, are precisely the needed
-- positive-coroot coefficient vectors.
isMinusculeWeight = method();

isMinusculeWeight (Matrix, List) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "isMinusculeWeight");
    lam := finiteDraftDynkinWeight(A, lambda, "isMinusculeWeight", true);
    positiveCoroots := entries cartanPositiveRoots transpose A;
    all(positiveCoroots, beta ->
        (sum for j from 0 to numrows A-1 list lam#j * beta#j) <= 1)
);

isMinusculeWeight (Matrix, Matrix) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "isMinusculeWeight");
    isMinusculeWeight(A,
        finiteDraftDynkinWeight(A, lambda, "isMinusculeWeight", true))
);

isMinusculeWeight KMWeight := lambda -> (
    typed := finiteDraftTypedWeightData(
        lambda,"isMinusculeWeight",true);
    isMinusculeWeight(typed#1,typed#2)
);

isMinusculeWeight (RootDatum, List) := (D, lambda) ->
    isMinusculeWeight kmWeight(D,lambda);
isMinusculeWeight (RootDatum, Matrix) := (D, lambda) ->
    isMinusculeWeight kmWeight(D,lambda);
isMinusculeWeight (CartanDatum, List) := (D, lambda) ->
    isMinusculeWeight kmWeight(rootDatum D,lambda);
isMinusculeWeight (CartanDatum, Matrix) := (D, lambda) ->
    isMinusculeWeight kmWeight(rootDatum D,lambda);
isMinusculeWeight (Realization, List) := (R, lambda) ->
    isMinusculeWeight kmWeight(rootDatum R,lambda);
isMinusculeWeight (Realization, Matrix) := (R, lambda) ->
    isMinusculeWeight kmWeight(rootDatum R,lambda);
isMinusculeWeight (String, List) := (s, lambda) ->
    isMinusculeWeight kmWeight(rootDatum s,lambda);
isMinusculeWeight (String, Matrix) := (s, lambda) ->
    isMinusculeWeight kmWeight(rootDatum s,lambda);

-- A weight is cominuscule when the corresponding coweight is minuscule.
-- In the same node-label coordinates this replaces positive coroots by
-- positive roots.  Both predicates regard the zero weight as minuscule, as
-- the trivial representation has a single Weyl orbit of weights.
isCominusculeWeight = method();

isCominusculeWeight (Matrix, List) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "isCominusculeWeight");
    lam := finiteDraftDynkinWeight(A, lambda, "isCominusculeWeight", true);
    positiveRoots := entries cartanPositiveRoots A;
    all(positiveRoots, beta ->
        (sum for j from 0 to numrows A-1 list lam#j * beta#j) <= 1)
);

isCominusculeWeight (Matrix, Matrix) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "isCominusculeWeight");
    isCominusculeWeight(A,
        finiteDraftDynkinWeight(A, lambda, "isCominusculeWeight", true))
);

isCominusculeWeight KMWeight := lambda -> (
    typed := finiteDraftTypedWeightData(
        lambda,"isCominusculeWeight",true);
    isCominusculeWeight(typed#1,typed#2)
);

isCominusculeWeight (RootDatum, List) := (D, lambda) ->
    isCominusculeWeight kmWeight(D,lambda);
isCominusculeWeight (RootDatum, Matrix) := (D, lambda) ->
    isCominusculeWeight kmWeight(D,lambda);
isCominusculeWeight (CartanDatum, List) := (D, lambda) ->
    isCominusculeWeight kmWeight(rootDatum D,lambda);
isCominusculeWeight (CartanDatum, Matrix) := (D, lambda) ->
    isCominusculeWeight kmWeight(rootDatum D,lambda);
isCominusculeWeight (Realization, List) := (R, lambda) ->
    isCominusculeWeight kmWeight(rootDatum R,lambda);
isCominusculeWeight (Realization, Matrix) := (R, lambda) ->
    isCominusculeWeight kmWeight(rootDatum R,lambda);
isCominusculeWeight (String, List) := (s, lambda) ->
    isCominusculeWeight kmWeight(rootDatum s,lambda);
isCominusculeWeight (String, Matrix) := (s, lambda) ->
    isCominusculeWeight kmWeight(rootDatum s,lambda);

finiteDraftSpecialNodeOutput = (D,nodes,format,defaultFormat,who) -> (
    fmt := if format === null then defaultFormat else format;
    labels := (cartanDatum D)#"nodeLabels";
    if fmt === "Positions" then nodes
    else if fmt === "Labels" then
        apply(nodes,i -> labels#(i-1))
    else if fmt === "Records" then
        apply(nodes,i -> new HashTable from {
            "position" => i,
            "label" => labels#(i-1)
        })
    else error(who |
        ": NodeFormat must be \"Positions\", \"Labels\", or \"Records\"")
);

-- An integer in 1..rank is interpreted positionally throughout the package.
-- If custom integer labels permute that range, returning bare labels would
-- therefore not round-trip.  In precisely that case the safe typed default
-- is a position/label record rather than an ambiguous integer.
finiteDraftTypedNodeFormat = D -> (
    labels := (cartanDatum D)#"nodeLabels";
    r := #labels;
    if r == 0 then return "Labels";
    if any(toList(0..r-1),i ->
        instance(labels#i,ZZ)
        and labels#i >= 1 and labels#i <= r
        and labels#i != i+1)
    then "Records"
    else "Labels"
);

finiteDraftNodePositionOrRecord = (D,x,who) -> (
    draftNodePosition(cartanDatum D,x,who)
);

minusculeNodes = method(Options => {NodeFormat => null});
minusculeNodes Matrix := opts -> A -> (
    A = finiteDraftRequireFinite(A, "minusculeNodes");
    nodes := flatten for component in leviConnectedComponents A list (
        indices := apply(component, i -> i-1);
        C := submatrix(A, indices, indices);
        dualHighestRoot := highestRoot transpose C;
        for j from 0 to #component-1 list
            if dualHighestRoot#j == 1 then component#j else continue
    );
    finiteDraftSpecialNodeOutput(
        cartanDatum A,nodes,opts.NodeFormat,"Positions","minusculeNodes")
);

minusculeNodes CartanDatum := opts -> D -> finiteDraftSpecialNodeOutput(
    D,minusculeNodes(datumCartanMatrix D,NodeFormat=>"Positions"),
    opts.NodeFormat,finiteDraftTypedNodeFormat D,"minusculeNodes");
minusculeNodes Realization := opts -> R ->
    minusculeNodes(cartanDatum R,NodeFormat=>opts.NodeFormat);
minusculeNodes RootDatum := opts -> D ->
    minusculeNodes(cartanDatum D,NodeFormat=>opts.NodeFormat);
minusculeNodes String := opts -> s ->
    minusculeNodes(cartanDatum s,NodeFormat=>opts.NodeFormat);
minusculeNodes KMWeylGroup := opts -> W ->
    minusculeNodes(W#"cartanDatum",NodeFormat=>opts.NodeFormat);

cominusculeNodes = method(Options => {NodeFormat => null});
cominusculeNodes Matrix := opts -> A -> (
    A = finiteDraftRequireFinite(A, "cominusculeNodes");
    nodes := flatten for component in leviConnectedComponents A list (
        indices := apply(component, i -> i-1);
        C := submatrix(A, indices, indices);
        highest := highestRoot C;
        for j from 0 to #component-1 list
            if highest#j == 1 then component#j else continue
    );
    finiteDraftSpecialNodeOutput(
        cartanDatum A,nodes,opts.NodeFormat,"Positions","cominusculeNodes")
);

cominusculeNodes CartanDatum := opts -> D -> finiteDraftSpecialNodeOutput(
    D,cominusculeNodes(datumCartanMatrix D,NodeFormat=>"Positions"),
    opts.NodeFormat,finiteDraftTypedNodeFormat D,"cominusculeNodes");
cominusculeNodes Realization := opts -> R ->
    cominusculeNodes(cartanDatum R,NodeFormat=>opts.NodeFormat);
cominusculeNodes RootDatum := opts -> D ->
    cominusculeNodes(cartanDatum D,NodeFormat=>opts.NodeFormat);
cominusculeNodes String := opts -> s ->
    cominusculeNodes(cartanDatum s,NodeFormat=>opts.NodeFormat);
cominusculeNodes KMWeylGroup := opts -> W ->
    cominusculeNodes(W#"cartanDatum",NodeFormat=>opts.NodeFormat);

isMinusculeNode = method();
isMinusculeNode (Matrix, ZZ) := (A, i) -> (
    A = finiteDraftRequireFinite(A, "isMinusculeNode");
    i = finiteDraftNode(A, i, "isMinusculeNode");
    member(i, minusculeNodes(A,NodeFormat=>"Positions"))
);

isMinusculeNode (CartanDatum, Thing) := (D, i) ->
    isMinusculeNode(datumCartanMatrix D,
        finiteDraftNodePositionOrRecord(D,i,"isMinusculeNode"));
isMinusculeNode (Realization, Thing) := (R, i) ->
    isMinusculeNode(cartanDatum R,i);
isMinusculeNode (RootDatum, Thing) := (D, i) ->
    isMinusculeNode(cartanDatum D,i);
isMinusculeNode (KMWeylGroup, Thing) := (W, i) ->
    isMinusculeNode(W#"cartanDatum",i);
isMinusculeNode (String, Thing) := (s, i) ->
    isMinusculeNode(cartanDatum s,i);

isCominusculeNode = method();
isCominusculeNode (Matrix, ZZ) := (A, i) -> (
    A = finiteDraftRequireFinite(A, "isCominusculeNode");
    i = finiteDraftNode(A, i, "isCominusculeNode");
    member(i, cominusculeNodes(A,NodeFormat=>"Positions"))
);

isCominusculeNode (CartanDatum, Thing) := (D, i) ->
    isCominusculeNode(datumCartanMatrix D,
        finiteDraftNodePositionOrRecord(D,i,"isCominusculeNode"));
isCominusculeNode (Realization, Thing) := (R, i) ->
    isCominusculeNode(cartanDatum R,i);
isCominusculeNode (RootDatum, Thing) := (D, i) ->
    isCominusculeNode(cartanDatum D,i);
isCominusculeNode (KMWeylGroup, Thing) := (W, i) ->
    isCominusculeNode(W#"cartanDatum",i);
isCominusculeNode (String, Thing) := (s, i) ->
    isCominusculeNode(cartanDatum s,i);

------------------------------------------------------------
-- Quadratic Casimir
------------------------------------------------------------

-- Return (lambda,lambda+2 rho), with lambda in dominant Dynkin labels and
-- long roots normalized to squared norm 2.  This is the standard
-- representation-theoretic Casimir eigenvalue; conventions using one half
-- of the invariant form return half this value.
quadraticCasimir = method();

quadraticCasimir (Matrix, List) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "quadraticCasimir");
    lam := finiteDraftDynkinWeight(A, lambda, "quadraticCasimir", true);
    W := fundamentalWeights A;
    B := normalizedInvariantForm A;
    weightGram := W * B * transpose W;
    value := 0_QQ;
    for i from 0 to numrows A-1 do for j from 0 to numrows A-1 do
        value = value + lam#i * weightGram_(i,j) * (lam#j + 2);
    value
);

quadraticCasimir (Matrix, Matrix) := (A, lambda) -> (
    A = finiteDraftRequireFinite(A, "quadraticCasimir");
    quadraticCasimir(A,
        finiteDraftDynkinWeight(A, lambda, "quadraticCasimir", true))
);

-- This is the Casimir of the derived semisimple algebra.  Complete weights
-- in a nonminimal realization may carry additional central coordinates, but
-- no canonical form on that toral complement is part of a Cartan datum, so
-- those coordinates correctly make no contribution here.
quadraticCasimir KMWeight := lambda -> (
    typed := finiteDraftTypedWeightData(
        lambda,"quadraticCasimir",true);
    quadraticCasimir(typed#1,typed#2)
);

quadraticCasimir (RootDatum, List) := (D, lambda) ->
    quadraticCasimir kmWeight(D,lambda);
quadraticCasimir (RootDatum, Matrix) := (D, lambda) ->
    quadraticCasimir kmWeight(D,lambda);
quadraticCasimir (CartanDatum, List) := (D, lambda) ->
    quadraticCasimir kmWeight(rootDatum D,lambda);
quadraticCasimir (CartanDatum, Matrix) := (D, lambda) ->
    quadraticCasimir kmWeight(rootDatum D,lambda);
quadraticCasimir (Realization, List) := (R, lambda) ->
    quadraticCasimir kmWeight(rootDatum R,lambda);
quadraticCasimir (Realization, Matrix) := (R, lambda) ->
    quadraticCasimir kmWeight(rootDatum R,lambda);
quadraticCasimir (String, List) := (s, lambda) ->
    quadraticCasimir kmWeight(rootDatum s,lambda);
quadraticCasimir (String, Matrix) := (s, lambda) ->
    quadraticCasimir kmWeight(rootDatum s,lambda);

------------------------------------------------------------
-- Parent-aware forwarding for structural finite data
------------------------------------------------------------

fundamentalCoweights CartanDatum := D ->
    fundamentalCoweights datumCartanMatrix D;
fundamentalCoweights Realization := R ->
    fundamentalCoweights datumCartanMatrix R;
fundamentalCoweights RootDatum := D ->
    fundamentalCoweights datumCartanMatrix D;
fundamentalCoweights String := s ->
    fundamentalCoweights cartanDatum s;
fundamentalCoweights KMWeylGroup := W ->
    fundamentalCoweights(W#"cartanDatum");

normalizedInvariantForm CartanDatum := D ->
    normalizedInvariantForm datumCartanMatrix D;
normalizedInvariantForm Realization := R ->
    normalizedInvariantForm datumCartanMatrix R;
normalizedInvariantForm RootDatum := D ->
    normalizedInvariantForm datumCartanMatrix D;
normalizedInvariantForm String := s ->
    normalizedInvariantForm cartanDatum s;
normalizedInvariantForm KMWeylGroup := W ->
    normalizedInvariantForm(W#"cartanDatum");

marks CartanDatum := D -> marks datumCartanMatrix D;
marks Realization := R -> marks datumCartanMatrix R;
marks RootDatum := D -> marks datumCartanMatrix D;
marks String := s -> marks cartanDatum s;
marks KMWeylGroup := W -> marks(W#"cartanDatum");

highestCoroot CartanDatum := D -> highestCoroot datumCartanMatrix D;
highestCoroot Realization := R -> highestCoroot datumCartanMatrix R;
highestCoroot RootDatum := D -> highestCoroot datumCartanMatrix D;
highestCoroot String := s -> highestCoroot cartanDatum s;
highestCoroot KMWeylGroup := W -> highestCoroot(W#"cartanDatum");

comarks CartanDatum := D -> comarks datumCartanMatrix D;
comarks Realization := R -> comarks datumCartanMatrix R;
comarks RootDatum := D -> comarks datumCartanMatrix D;
comarks String := s -> comarks cartanDatum s;
comarks KMWeylGroup := W -> comarks(W#"cartanDatum");

lacingNumber CartanDatum := D -> lacingNumber datumCartanMatrix D;
lacingNumber Realization := R -> lacingNumber datumCartanMatrix R;
lacingNumber RootDatum := D -> lacingNumber datumCartanMatrix D;
lacingNumber String := s -> lacingNumber cartanDatum s;
lacingNumber KMWeylGroup := W -> lacingNumber(W#"cartanDatum");

weylExponents CartanDatum := D -> weylExponents datumCartanMatrix D;
weylExponents Realization := R -> weylExponents datumCartanMatrix R;
weylExponents RootDatum := D -> weylExponents datumCartanMatrix D;
weylExponents String := s -> weylExponents cartanDatum s;
weylExponents KMWeylGroup := W -> weylExponents(W#"cartanDatum");

invariantDegrees CartanDatum := D ->
    invariantDegrees datumCartanMatrix D;
invariantDegrees Realization := R ->
    invariantDegrees datumCartanMatrix R;
invariantDegrees RootDatum := D ->
    invariantDegrees datumCartanMatrix D;
invariantDegrees String := s ->
    invariantDegrees cartanDatum s;
invariantDegrees KMWeylGroup := W ->
    invariantDegrees(W#"cartanDatum");

-- Explicit-parent forms are convenient in code that already threads a
-- RootDatum through several representation-theoretic commands.  They are
-- strict parent checks, not coercions.
dominantizeWeight (RootDatum, KMWeight) := (D,lambda) ->
    dominantizeWeight finiteDraftWeightInRootDatum(
        D,lambda,"dominantizeWeight");
weightStabilizer (RootDatum, KMWeight) := (D,lambda) ->
    weightStabilizer finiteDraftWeightInRootDatum(
        D,lambda,"weightStabilizer");
dualWeight (RootDatum, KMWeight) := (D,lambda) ->
    dualWeight finiteDraftWeightInRootDatum(D,lambda,"dualWeight");
isOppositionFixed (RootDatum, KMWeight) := (D,lambda) ->
    isOppositionFixed finiteDraftWeightInRootDatum(
        D,lambda,"isOppositionFixed");
isSelfDual (RootDatum, KMWeight) := (D,lambda) ->
    isSelfDual finiteDraftWeightInRootDatum(D,lambda,"isSelfDual");
isMinusculeWeight (RootDatum, KMWeight) := (D,lambda) ->
    isMinusculeWeight finiteDraftWeightInRootDatum(
        D,lambda,"isMinusculeWeight");
isCominusculeWeight (RootDatum, KMWeight) := (D,lambda) ->
    isCominusculeWeight finiteDraftWeightInRootDatum(
        D,lambda,"isCominusculeWeight");
quadraticCasimir (RootDatum, KMWeight) := (D,lambda) ->
    quadraticCasimir finiteDraftWeightInRootDatum(
        D,lambda,"quadraticCasimir");


-------------------------------------------------------------------------
-- Formal Characters
-------------------------------------------------------------------------

-- Parent-aware formal characters and highest-weight modules.
--
-- A FormalCharacter is a finite exact character or a certified finite
-- truncation of a completed character.  Exactness is data, not an informal
-- promise: routines such as characterDimension and decomposeCharacter refuse
-- to interpret a truncation as a complete finite-dimensional answer.

FormalCharacter = new Type of HashTable
LeviFormalCharacter = new Type of HashTable
KMRepresentation = new Type of HashTable
ReductiveLeviDatum = new Type of HashTable
LeviWeight = new Type of HashTable
LeviHighestWeight = new Type of HashTable

FormalCharacter == FormalCharacter := (C,D) -> C === D
LeviFormalCharacter == LeviFormalCharacter := (C,D) -> C === D
KMRepresentation == KMRepresentation := (M,N) -> M === N
ReductiveLeviDatum == ReductiveLeviDatum := (L,K) -> L === K
LeviWeight == LeviWeight := (x,y) -> x === y
LeviHighestWeight == LeviHighestWeight := (x,y) -> x === y

-- Completed Verma denominators recur throughout BGG and character
-- calculations.  Cache immutable partition tables by Cartan entries and
-- depth; package-wide cache controls below expose their lifecycle.
draftVermaPartitionCache := new MutableHashTable
draftCharacterOrderCache := new MutableHashTable
draftCharacterParentCache := new MutableHashTable
formalCharacterInfrastructureCacheSize = () -> #keys draftVermaPartitionCache
draftCharacterParentCacheSize = () -> #keys draftCharacterParentCache
clearFormalCharacterInfrastructureCache = () -> (
    draftVermaPartitionCache = new MutableHashTable;
    draftCharacterOrderCache = new MutableHashTable;
    draftCharacterParentCache = new MutableHashTable;
    null
)

draftCharacterCoefficient = (c,who) -> (
    q := try (c + 0_QQ) else error(who | ": coefficients must be integers");
    if denominator q != 1 then error(who | ": coefficients must be integers");
    numerator q
)

draftCanonicalTerms = (parent,T,weightType,who) -> (
    if not instance(T,HashTable) and not instance(T,Tally) then
        error(who | ": terms must be a Tally or HashTable");
    accum := new MutableHashTable;
    coordinateMode := null;
    for p in pairs T do (
        wt := p#0;
        c := draftCharacterCoefficient(p#1,who);
        if not instance(wt,weightType) then
            error(who | ": every key has the wrong weight type");
        actualParent := if weightType === KMWeight then parentDatum wt else wt#"parent";
        if actualParent =!= parent then error(who | ": a weight belongs to a different parent");
        if c != 0 then (
            if weightType === KMWeight and
               (parent#"realization")#"dimension" > (cartanDatum parent)#"rank" then (
                currentMode := if wt#"realizationCoordinates" === null then
                    "DynkinLabelClass" else "CompleteRealization";
                if coordinateMode === null then coordinateMode = currentMode
                else if coordinateMode =!= currentMode then error concatenate(
                    who, ": cannot mix complete realization weights with ",
                    "Dynkin-label classes in one formal character");
            );
            if accum#?wt then accum#wt = accum#wt + c else accum#wt = c;
        );
    );
    new Tally from for wt in keys accum list if accum#wt != 0 then wt => accum#wt else continue
)

formalCharacter = method(Options => {
    CharacterStatus => "Exact",
    Truncation => null
})

draftFormalCharacter = (D,T,status,truncation) -> (
    if status =!= "Exact" and status =!= "Truncated" then
        error "formalCharacter: CharacterStatus must be \"Exact\" or \"Truncated\"";
    if status === "Exact" and truncation =!= null then
        error "formalCharacter: an exact character cannot carry truncation metadata";
    if status === "Truncated" and truncation === null then
        error "formalCharacter: a truncated character must describe its Truncation";
    C := new FormalCharacter from {
        "parent" => D,
        "terms" => draftCanonicalTerms(D,T,KMWeight,"formalCharacter"),
        "status" => status,
        "truncation" => truncation
    };
    data := draftRootDepthCharacterData C;
    if instance(truncation,HashTable) and truncation#?"coefficientCertificate" and
        truncation#"coefficientCertificate" === true and truncation#?"kind" and
        truncation#"kind" === "root-depth" and data === null then
        error "formalCharacter: invalid highest-weight root-depth certificate";
    if data =!= null then (
        draftCheckCharacterQueryMode(C,data#0);
        for mu in keys characterTerms C do (
            beta := draftCharacterRootOffset(data#0,mu);
            if beta === null or any(beta,b -> denominator(b+0_QQ) != 1 or b < 0)
                or sum beta > data#1 then
                error "formalCharacter: a certified root-depth table contains a weight outside its stated range";
        );
    );
    C
)

formalCharacter (RootDatum,HashTable) := opts -> (D,T) ->
    draftFormalCharacter(D,T,opts.CharacterStatus,opts.Truncation)
formalCharacter (RootDatum,Tally) := opts -> (D,T) ->
    draftFormalCharacter(D,T,opts.CharacterStatus,opts.Truncation)

-- Print in the group-ring notation e^mu.  A bounded preview still shows
-- actual monomials, and mathematical truncation is labeled separately.
draftCharacterDisplayLimit := 16
draftCharacterLineWidth := 78

draftCharacterOrderData = D -> (
    if draftCharacterOrderCache#?D then return draftCharacterOrderCache#D;
    R := draftSimpleRootMatrix(D#"realization");
    r := numrows R;
    d := numcols R;
    direction := if r == 0 then toList(d:0_QQ) else flatten entries(
        transpose R * inverse(R * transpose R) *
        transpose matrix(QQ,{toList(r:1)}));
    basis := (draftWeightNotationInfo D)#"basisRows";
    finiteDirection := if cartanType datumCartanMatrix D === "finite" then
        apply(entries fundamentalWeights datumCartanMatrix D,sum) else null;
    answer := {direction,apply(basis,row -> draftDot(row,direction)),finiteDirection};
    draftCharacterOrderCache#D = answer;
    answer
)

-- This linear functional increases by one on each simple root.  Thus
-- comparable complete weights appear in dominance order without an
-- expensive pairwise comparison of the entire character.
draftCharacterWeightOrder = weight -> (
    labels := datumCoordinates weight;
    ambient := weight#"realizationCoordinates";
    orderData := draftCharacterOrderData(parentDatum weight);
    score := if ambient === null then (
        if orderData#2 =!= null then draftDot(labels,orderData#2)
        else draftDot(labels,take(orderData#1,#labels))
    )
    else draftDot(ambient,orderData#0);
    {score,labels,if ambient === null then {} else ambient}
)

draftCharacterPreviewPairs = (C,orderFunction) -> (
    leading := {};
    for p in pairs(C#"terms") do (
        leading = rsort append(leading,{orderFunction(p#0),p});
        if #leading > draftCharacterDisplayLimit then
            leading = take(leading,draftCharacterDisplayLimit);
    );
    apply(leading,p -> p#1)
)

draftCharacterNumericalMonomial = coordinates ->
    if all(coordinates,c -> c == 0) then "1" else
        "e^(" | concatenate between(", ",apply(coordinates,toString)) | ")"

draftCharacterNumericalCoordinates = weight -> (
    D := parentDatum weight;
    if (D#"realization")#"dimension" > (cartanDatum D)#"rank" and
        weight#"realizationCoordinates" =!= null then realizationCoordinates weight
    else datumCoordinates weight
)

draftCharacterTermBody = record -> (
    coefficient := abs record#2;
    monomial := record#1;
    if monomial === "1" then toString coefficient
    else if coefficient == 1 then monomial
    else toString coefficient | "*" | monomial
)

draftCharacterExpression = records -> (
    if #records == 0 then return "0";
    answer := "";
    for j from 0 to #records-1 do (
        record := records#j;
        body := draftCharacterTermBody record;
        answer = answer | (if j == 0 then
            (if record#2 < 0 then "-" else "")
            else if record#2 < 0 then " - " else " + ") | body;
    );
    answer
)

draftCharacterExpressionLines = records -> (
    if #records == 0 then return {"0"};
    lines := {};
    line := "";
    for j from 0 to #records-1 do (
        record := records#j;
        body := draftCharacterTermBody record;
        part := (if j == 0 then (if record#2 < 0 then "-" else "")
            else if record#2 < 0 then " - " else " + ") | body;
        if line =!= "" and #line + #part > draftCharacterLineWidth then (
            lines = append(lines,line);
            line = part;
        ) else line = line | part;
    );
    append(lines,line)
)

draftFormalCharacterRecords = C -> apply(
    draftCharacterPreviewPairs(C,draftCharacterWeightOrder),p ->
        {draftCharacterWeightOrder(p#0),
         draftCharacterNumericalMonomial(draftCharacterNumericalCoordinates(p#0)),
         p#1,p#0})

draftCharacterCoordinateConvention = C -> (
    if instance(C,LeviFormalCharacter) then (
        L := C#"parent";
        return concatenate("coordinates: ",
            toString((cartanDatum L#"semisimpleRootDatum")#"rank"),
            " Levi Dynkin, then ",toString L#"centerDimension"," central");
    );
    D := C#"parent";
    if (D#"realization")#"dimension" == (cartanDatum D)#"rank" then
        return "Dynkin coordinates";
    if #keys(C#"terms") == 0 then return "";
    weight := first keys(C#"terms");
    if weight#"realizationCoordinates" === null then
        "Dynkin-label classes modulo ann(h')" else "realization coordinates"
)

draftCharacterDisplayNote = (C,omitted) -> (
    convention := draftCharacterCoordinateConvention C;
    prefix := if convention === "" then "" else convention | "; ";
    if C#"status" === "Exact" then (
        if omitted == 0 then return
            if convention === "" then "" else "[" | convention | "]";
        coefficientName := if any(values C#"terms",c -> c < 0)
            then "virtual dimension" else "dimension";
        return concatenate("[",prefix,toString omitted," further weights not shown; ",
            coefficientName," ",toString sum values C#"terms","; exact character]");
    );
    T := C#"truncation";
    data := draftRootDepthCharacterData C;
    if data === null and instance(T,HashTable) and T#?"coefficientCertificate" and
        T#"coefficientCertificate" === true and T#?"operation" and
        T#"operation" === "restriction" then data = draftRootDepthCharacterData(T#"source");
    bound := if data =!= null then "through root depth " | toString data#1 else "truncated";
    "[" | prefix | bound | (if omitted == 0 then "" else
        "; " | toString omitted | " further computed weights not shown") | "]"
)

draftCharacterDisplay = (C,typeName,recordFunction) -> (
    records := recordFunction C;
    expression := draftCharacterExpression records;
    lines := draftCharacterExpressionLines records;
    omitted := #keys(C#"terms") - #records;
    hasRemainder := C#"status" =!= "Exact" or omitted > 0;
    if hasRemainder then (
        expression = expression | " + ...";
        lines = apply(toList(0..#lines-1),i ->
            if i == #lines-1 then lines#i | " + ..." else lines#i);
    );
    note := draftCharacterDisplayNote(C,omitted);
    new HashTable from {
        "string" => expression | (if note === "" then "" else " " | note),
        "net" => stack join(lines,if note === "" then {} else {note})
    }
)

toString FormalCharacter := C ->
    (draftCharacterDisplay(C,"FormalCharacter",draftFormalCharacterRecords))#"string"
net FormalCharacter := C ->
    (draftCharacterDisplay(C,"FormalCharacter",draftFormalCharacterRecords))#"net"

characterDatum = method()
characterDatum FormalCharacter := C -> C#"parent"
characterDatum LeviFormalCharacter := C -> C#"parent"

characterTerms = method()
characterTerms FormalCharacter := C -> C#"terms"
characterTerms LeviFormalCharacter := C -> C#"terms"

characterSupport = method()
characterSupport FormalCharacter := C -> keys characterTerms C
characterSupport LeviFormalCharacter := C -> keys characterTerms C

characterCoefficient = method()
characterCoefficient (FormalCharacter,KMWeight) := (C,mu) -> (
    if parentDatum mu =!= characterDatum C then
        error "characterCoefficient: weight and character have different parents";
    draftCheckCharacterQueryMode(C,mu);
    value := draftKnownCharacterCoefficient(C,mu);
    if value === null then error "characterCoefficient: this coefficient is outside the computed range; increase Depth in the original character";
    value
)
characterCoefficient (LeviFormalCharacter,LeviWeight) := (C,mu) -> (
    if mu#"parent" =!= characterDatum C then
        error "characterCoefficient: weight and character have different parents";
    value := draftKnownCharacterCoefficient(C,mu);
    if value === null then error "characterCoefficient: this coefficient is outside the computed ambient range; increase Depth before restriction";
    value
)

isExactCharacter = method()
isExactCharacter FormalCharacter := C -> C#"status" === "Exact"
isExactCharacter LeviFormalCharacter := C -> C#"status" === "Exact"

-- A missing table entry is zero only when its coefficient is known.  Native
-- highest-weight expansions certify the simplex lambda-Q_+ of depth <= H
-- and vanish outside lambda-Q_+.  Other operations transport this knowledge
-- or restrict the displayed terms to coefficients known from both inputs.
draftCharacterCoordinateMode = C -> (
    if not instance(C,FormalCharacter) then return null;
    D := characterDatum C;
    if (D#"realization")#"dimension" == (cartanDatum D)#"rank" then
        return "Complete";
    weights := keys characterTerms C;
    if #weights > 0 then return (if (first weights)#"realizationCoordinates" === null then "Labels" else "Complete");
    if isExactCharacter C then return null;
    T := C#"truncation";
    if not instance(T,HashTable) or not T#?"coefficientCertificate" or
        T#"coefficientCertificate" =!= true then return null;
    if T#?"highestWeight" and instance(T#"highestWeight",KMWeight) and
        parentDatum T#"highestWeight" === D then return (if (T#"highestWeight")#"realizationCoordinates" === null then "Labels" else "Complete");
    if T#?"source" then return draftCharacterCoordinateMode(T#"source");
    if T#?"left" then (
        mode := draftCharacterCoordinateMode(T#"left");
        return if mode =!= null then mode else draftCharacterCoordinateMode(T#"right");
    );
    if T#?"finite" then (
        finiteMode := draftCharacterCoordinateMode(T#"finite");
        return if finiteMode =!= null then finiteMode else draftCharacterCoordinateMode(T#"other");
    );
    null
)

draftCheckCharacterQueryMode = (C,mu) -> (
    mode := draftCharacterCoordinateMode C;
    if mode =!= null and ((parentDatum mu)#"realization")#"dimension" >
        (cartanDatum parentDatum mu)#"rank" then (
        queryMode := if mu#"realizationCoordinates" === null then "Labels" else "Complete";
        if mode =!= queryMode then error "characterCoefficient: cannot compare a complete weight with a Dynkin-label class; specify full realization coordinates";
    );
    null
)

draftCheckCharacterModes = (C,D,who) -> (
    left := draftCharacterCoordinateMode C;
    right := draftCharacterCoordinateMode D;
    if left =!= null and right =!= null and left =!= right then
        error(who | ": cannot mix complete weights and Dynkin-label classes");
    null
)

-- RootDatum equality is structural, so independently constructed identical
-- data are already compatible.  Remember both successful and unsuccessful
-- comparisons, without identifying different lattices, central directions,
-- or coordinate conventions merely because their Cartan matrices agree.
draftCheckCharacterParents = (C,D,who) -> (
    left := characterDatum C;
    right := characterDatum D;
    key := {left,right};
    if not draftCharacterParentCache#?key then (
        reverseKey := {right,left};
        if draftCharacterParentCache#?reverseKey then key = reverseKey
        else draftCharacterParentCache#key = left === right;
    );
    if not draftCharacterParentCache#key then error(who | ": parents differ");
    -- The same root datum can support both complete weights and label
    -- classes.  Parent compatibility never licenses mixing these modes.
    draftCheckCharacterModes(C,D,who)
)

draftCharacterUnit = C -> (
    D := characterDatum C;
    zeroWeight := if draftCharacterCoordinateMode C === "Complete" then
        weightFromRealizationCoordinates(D,toList((D#"realization")#"dimension":0))
    else kmWeight(D,toList((cartanDatum D)#"rank":0));
    formalCharacter(D,new Tally from {zeroWeight=>1})
)

draftRootDepthCharacterData = C -> (
    if not instance(C,FormalCharacter) or isExactCharacter C then return null;
    T := C#"truncation";
    if not instance(T,HashTable) or not T#?"coefficientCertificate" or
        T#"coefficientCertificate" =!= true or not T#?"kind" or
        T#"kind" =!= "root-depth" or not T#?"support" or
        T#"support" =!= "highest-weight cone" or not T#?"highestWeight" or
        not T#?"completeThroughDepth" then return null;
    lambda := T#"highestWeight";
    H := T#"completeThroughDepth";
    if not instance(lambda,KMWeight) or parentDatum lambda =!= characterDatum C or
        not instance(H,ZZ) or H < 0 then return null;
    {lambda,H}
)

draftCharacterRootOffset = (lambda,mu) -> (
    if lambda#"realizationCoordinates" === null and
        mu#"realizationCoordinates" === null then (
        A := datumCartanMatrix parentDatum lambda;
        if det A == 0 then return null;
        return toSimple(A,draftAdd(datumCoordinates lambda,
            draftScale(-1,datumCoordinates mu)));
    );
    try weightToRootCoordinates(lambda-mu) else null
)

draftRootDepthTruncation = (lambda,H,operation) -> new HashTable from {
    "coefficientCertificate"=>true,
    "kind"=>"root-depth", "support"=>"highest-weight cone",
    "highestWeight"=>lambda, "depth"=>H, "completeThroughDepth"=>H,
    "operation"=>operation
}

draftKnownCharacterCoefficient = (C,mu) -> (
    if instance(C,FormalCharacter) then draftCheckCharacterQueryMode(C,mu);
    if isExactCharacter C then return (if (characterTerms C)#?mu then (characterTerms C)#mu else 0);
    T := C#"truncation";
    if instance(T,HashTable) and T#?"coefficientCertificate" and
        T#"coefficientCertificate" === true and T#?"operation" then (
        op := T#"operation";
        if op === "dual" then return draftKnownCharacterCoefficient(T#"source",-mu);
        if op === "weyl" then return draftKnownCharacterCoefficient(
            T#"source",weylAction(T#"inverseWeylElement",mu));
        if op === "restriction" then return draftKnownCharacterCoefficient(
            T#"source",draftLiftLeviWeight mu);
        if op === "Levi lift" then return draftKnownCharacterCoefficient(
            T#"source",restrictWeight(T#"leviDatum",mu));
        if op === "scale" then (
            value := draftKnownCharacterCoefficient(T#"source",mu);
            return if value === null then null else T#"scalar"*value;
        );
        if op === "sum" then (
            left := draftKnownCharacterCoefficient(T#"left",mu);
            right := draftKnownCharacterCoefficient(T#"right",mu);
            return if left === null or right === null then null else left+right;
        );
        if op === "finite product" then (
            finite := T#"finite";
            other := T#"other";
            total := 0;
            for p in pairs characterTerms finite do (
                value := draftKnownCharacterCoefficient(other,mu-p#0);
                if value === null then return null;
                total = total+p#1*value;
            );
            return total;
        );
    );
    data := draftRootDepthCharacterData C;
    if data =!= null then (
        beta := draftCharacterRootOffset(data#0,mu);
        if beta === null or any(beta,b -> denominator(b+0_QQ) != 1 or b < 0) then return 0;
        if sum beta > data#1 then return null;
        return if (characterTerms C)#?mu then (characterTerms C)#mu else 0;
    );
    -- With unstructured, user-supplied truncation information, only the
    -- explicitly provided coefficients are known; an absent one is unknown.
    if (characterTerms C)#?mu then (characterTerms C)#mu else null
)

draftCertifiedCharacterTerms = C -> new HashTable from
    for mu in keys characterTerms C list (
        value := draftKnownCharacterCoefficient(C,mu);
        if value === null or value == 0 then continue;
        mu => value
    )

FormalCharacter + FormalCharacter := (C,D) -> (
    draftCheckCharacterParents(C,D,"FormalCharacter + FormalCharacter");
    S := new MutableHashTable;
    for p in pairs characterTerms C do S#(p#0) = p#1;
    for p in pairs characterTerms D do
        if S#?(p#0) then S#(p#0) = S#(p#0)+p#1 else S#(p#0)=p#1;
    if isExactCharacter C and isExactCharacter D then
        return formalCharacter(characterDatum C,new HashTable from pairs S);
    left := draftRootDepthCharacterData C;
    right := draftRootDepthCharacterData D;
    workingMetadata := new HashTable from {
        "coefficientCertificate"=>true,"operation"=>"sum","left"=>C,"right"=>D};
    metadata := if left =!= null and right =!= null and left#0 === right#0 then
        new HashTable from join(pairs draftRootDepthTruncation(
            left#0,min(left#1,right#1),"sum"),{"left"=>C,"right"=>D})
    else workingMetadata;
    result := formalCharacter(characterDatum C,new HashTable from pairs S,
        CharacterStatus=>"Truncated",Truncation=>workingMetadata);
    formalCharacter(characterDatum C,draftCertifiedCharacterTerms result,
        CharacterStatus=>"Truncated",Truncation=>metadata)
)

- FormalCharacter := C -> (-1)*C

ZZ * FormalCharacter := (a,C) -> (
    if a == 0 then return formalCharacter(characterDatum C,new Tally);
    metadata := if isExactCharacter C then null else new HashTable from join(
        if draftRootDepthCharacterData C =!= null then pairs C#"truncation" else {},
        {"coefficientCertificate"=>true,"operation"=>"scale","source"=>C,"scalar"=>a});
    formalCharacter(characterDatum C,
        new HashTable from apply(pairs characterTerms C,p -> p#0 => a*p#1),
        CharacterStatus=>C#"status",Truncation=>metadata)
)
FormalCharacter * ZZ := (C,a) -> a*C
FormalCharacter - FormalCharacter := (C,D) -> C + (-D)

-- Multiplication is convolution in the group algebra, hence the character of
-- a tensor product whenever both inputs are actual module characters.
FormalCharacter * FormalCharacter := (C,D) -> (
    draftCheckCharacterParents(C,D,"FormalCharacter * FormalCharacter");
    if (isExactCharacter C and #keys characterTerms C == 0) or
       (isExactCharacter D and #keys characterTerms D == 0) then
        return formalCharacter(characterDatum C,new Tally);
    left := draftRootDepthCharacterData C;
    right := draftRootDepthCharacterData D;
    if not isExactCharacter C and not isExactCharacter D and
        (left === null or right === null) then error "FormalCharacter * FormalCharacter: two truncated factors require compatible highest-weight root-depth expansions; arbitrary infinite convolutions need not be defined";
    bounded := not isExactCharacter C and not isExactCharacter D;
    H := if bounded then min(left#1,right#1) else null;
    -- A pair can contribute below H only when both individual offsets and
    -- their sum have depth <= H.  Cut these pairs before constructing sums,
    -- rather than computing and then discarding the deeper convolution.
    leftTerms := apply(pairs characterTerms C,p -> {p#0,p#1,
        if bounded then sum draftCharacterRootOffset(left#0,p#0) else 0});
    rightTerms := apply(pairs characterTerms D,p -> {p#0,p#1,
        if bounded then sum draftCharacterRootOffset(right#0,p#0) else 0});
    if bounded then (
        leftTerms = select(leftTerms,p -> p#2 <= H);
        rightTerms = select(rightTerms,p -> p#2 <= H);
    );
    S := new MutableHashTable;
    for pc in leftTerms do for pd in rightTerms do (
        if bounded and pc#2+pd#2 > H then continue;
        wt := pc#0 + pd#0;
        value := pc#1*pd#1;
        if S#?wt then S#wt = S#wt+value else S#wt=value;
    );
    if isExactCharacter C and isExactCharacter D then
        return formalCharacter(characterDatum C,new HashTable from pairs S);
    metadata := if bounded then
        draftRootDepthTruncation(left#0+right#0,H,"root-depth product")
    else new HashTable from {
        "coefficientCertificate"=>true,
        "operation"=>"finite product",
        "finite"=>if isExactCharacter C then C else D,
        "other"=>if isExactCharacter C then D else C};
    result := formalCharacter(characterDatum C,new HashTable from pairs S,
        CharacterStatus=>"Truncated",Truncation=>metadata);
    if bounded then return result;
    formalCharacter(characterDatum C,draftCertifiedCharacterTerms result,
        CharacterStatus=>"Truncated",Truncation=>metadata)
)

-- Ordinary powers are repeated tensor products, not symmetric powers.
-- Start with an unset accumulator rather than unit*C: multiplying a native
-- partial expansion by an exact unit produces a transported certificate,
-- which would prevent later native root-depth products from being formed.
FormalCharacter ^ ZZ := (C,k) -> (
    if k < 0 then error "FormalCharacter ^ ZZ: the exponent must be nonnegative";
    if k == 0 then return draftCharacterUnit C;
    if k == 1 then return C;
    result := null;
    factor := C;
    remaining := k;
    while remaining > 0 do (
        if remaining % 2 == 1 then
            result = if result === null then factor else result*factor;
        remaining = remaining // 2;
        if remaining > 0 then factor = factor*factor;
    );
    result
)

dualCharacter = method()
dualCharacter FormalCharacter := C -> formalCharacter(
    characterDatum C,
    new HashTable from apply(pairs characterTerms C,p -> (-p#0) => p#1),
    CharacterStatus=>C#"status",Truncation=>if isExactCharacter C then null
        else new HashTable from {"coefficientCertificate"=>true,"operation"=>"dual","source"=>C})

weylAction (WeylElement,FormalCharacter) := (w,C) -> (
    if (weylParent w)#"cartanDatum" =!= cartanDatum characterDatum C then
        error "weylAction: Weyl element and character have different Cartan data";
    formalCharacter(characterDatum C,
        new HashTable from apply(pairs characterTerms C,p ->
            weylAction(w,p#0) => p#1),
        CharacterStatus=>C#"status",Truncation=>if isExactCharacter C then null
            else new HashTable from {"coefficientCertificate"=>true,"operation"=>"weyl","source"=>C,
                "inverseWeylElement"=>inverseWeylElement w})
)

characterDimension = method()
characterDimension FormalCharacter := C -> (
    if not isExactCharacter C then
        error "characterDimension: a truncated completed character has no certified finite dimension";
    sum values characterTerms C
)
characterDimension LeviFormalCharacter := C -> (
    if not isExactCharacter C then
        error "characterDimension: a partial Levi character has no certified finite dimension";
    sum values characterTerms C
)

-- Subtract a root without conflating two legitimate kinds of weight object.
-- A complete weight retains every realization coordinate (including a
-- Weyl-fixed central direction); a Dynkin-label class remains a class.  The
-- mixed arithmetic operation is intentionally rejected by weight arithmetic, so
-- character construction must make this choice explicitly.
draftWeightMinusRoot = (lambda,beta) -> (
    D := parentDatum lambda;
    if parentDatum beta =!= D then
        error "KacMoody: internal weight/root parent mismatch";
    betaWeight := toWeight beta;
    if lambda#"realizationCoordinates" === null then
        kmWeight(D,draftAdd(datumCoordinates lambda,
            draftScale(-1,datumCoordinates betaWeight)))
    else lambda-betaWeight
)

finiteCharacter = method()
finiteCharacter (RootDatum,KMWeight) := (D,lambda) -> (
    if parentDatum lambda =!= D then
        error "finiteCharacter: highest weight has the wrong parent";
    A := datumCartanMatrix D;
    if cartanType A =!= "finite" then
        error "finiteCharacter: the Cartan datum must be of finite type";
    labels := datumCoordinates lambda;
    if not all(labels,x -> instance(x,ZZ) and x >= 0) then
        error "finiteCharacter: highest weight must be dominant integral";
    legacy := weightMultiplicityTable(A,labels);
    formalCharacter(D,new Tally from apply(pairs legacy,p -> (
        beta := toSimple(A,draftAdd(labels,draftScale(-1,p#0)));
        if not all(beta,x -> denominator(x+0_QQ) == 1) then
            error "KacMoody: internal finite-character weight is outside the root lattice translate";
        beta = apply(beta,x -> numerator(x+0_QQ));
        draftWeightMinusRoot(lambda,kmRoot(D,beta)) => p#1
    )))
)
finiteCharacter (RootDatum,List) := (D,lambda) -> finiteCharacter(D,kmWeight(D,lambda))
finiteCharacter (CartanDatum,List) := (D,lambda) -> finiteCharacter(rootDatum D,lambda)
finiteCharacter (Matrix,List) := (A,lambda) -> finiteCharacter(rootDatum A,lambda)

decomposeCharacter = method()
decomposeCharacter FormalCharacter := C -> (
    if not isExactCharacter C then
        error "decomposeCharacter: exact finite support is required";
    D := characterDatum C;
    A := datumCartanMatrix D;
    if cartanType A =!= "finite" then
        error "decomposeCharacter: finite Cartan type is required";
    remaining := new MutableHashTable from pairs characterTerms C;
    if any(keys remaining,wt -> not all(datumCoordinates wt,x -> instance(x,ZZ))) then
        error "decomposeCharacter: every weight must have integral Dynkin labels";
    -- In finite type, h(mu)=sum(A^(-1)*Dynkin(mu)) increases by one
    -- on each simple root.  A greatest-height dominant weight is therefore
    -- maximal in its own full root-lattice coset.  Distinct central
    -- characters (or distinct cosets) may tie, but do not dominate each
    -- other.  This linear scan also avoids pairwise dominance tests.
    heightDirection := (draftCharacterOrderData D)#2;
    heights := new HashTable from apply(keys remaining,wt ->
        wt => draftDot(datumCoordinates wt,heightDirection));
    answer := new MutableHashTable;
    while any(values remaining,c -> c != 0) do (
        if any(values remaining,c -> c < 0) then
            error "decomposeCharacter: character is not a nonnegative sum of irreducible characters";
        dominant := select(keys remaining,wt -> remaining#wt != 0 and
            all(datumCoordinates wt,x -> x >= 0));
        if #dominant == 0 then
            error "decomposeCharacter: no dominant highest weight remains";
        maximalHeight := max apply(dominant,wt -> heights#wt);
        maximal := first select(dominant,wt -> heights#wt == maximalHeight);
        multiplicity := remaining#maximal;
        constituent := finiteCharacter(D,maximal);
        for p in pairs characterTerms constituent do (
            if not remaining#?(p#0) then remaining#(p#0)=0;
            remaining#(p#0) = remaining#(p#0)-multiplicity*p#1;
        );
        answer#maximal = if answer#?maximal then answer#maximal+multiplicity else multiplicity;
    );
    new Tally from pairs answer
)

-- The inverse basis change: irreducible highest weights with their
-- multiplicities become a sum of complete finite weight characters.
characterFromDecomposition = method()

draftCharacterFromDecomposition = (D,T) -> (
    if cartanType datumCartanMatrix D =!= "finite" then
        error "characterFromDecomposition: finite Cartan type is required";
    summands := draftCanonicalTerms(D,T,KMWeight,"characterFromDecomposition");
    if any(values summands,m -> m < 0) then
        error "characterFromDecomposition: multiplicities must be nonnegative integers";
    if any(keys summands,lambda -> not all(datumCoordinates lambda,
        x -> instance(x,ZZ) and x >= 0)) then
        error "characterFromDecomposition: highest weights must have dominant integral Dynkin labels";
    terms := new MutableHashTable;
    for p in pairs summands do for q in pairs characterTerms finiteCharacter(D,p#0) do (
        value := p#1*q#1;
        if terms#?(q#0) then terms#(q#0) = terms#(q#0)+value
        else terms#(q#0) = value;
    );
    formalCharacter(D,new HashTable from pairs terms)
)

characterFromDecomposition (RootDatum,HashTable) := draftCharacterFromDecomposition
characterFromDecomposition (RootDatum,Tally) := draftCharacterFromDecomposition

draftCharacterFromInferredDecomposition = T -> (
    weights := keys T;
    if #weights == 0 then
        error "characterFromDecomposition: an empty decomposition needs an explicit RootDatum; use characterFromDecomposition(D,T)";
    if not all(weights,lambda -> instance(lambda,KMWeight)) then
        error "characterFromDecomposition: every key must be a KMWeight highest weight";
    draftCharacterFromDecomposition(parentDatum(first weights),T)
)

characterFromDecomposition HashTable := draftCharacterFromInferredDecomposition
characterFromDecomposition Tally := draftCharacterFromInferredDecomposition

integrableHighestWeightModule = method()
integrableHighestWeightModule (RootDatum,KMWeight) := (D,lambda) -> (
    if parentDatum lambda =!= D then
        error "integrableHighestWeightModule: highest weight has the wrong parent";
    if not all(datumCoordinates lambda,x -> instance(x,ZZ) and x >= 0) then
        error "integrableHighestWeightModule: highest weight must be dominant integral";
    new KMRepresentation from {
        "parent" => D,
        "highestWeight" => lambda,
        "kind" => "IntegrableHighestWeight"
    }
)
integrableHighestWeightModule (RootDatum,List) := (D,lambda) ->
    integrableHighestWeightModule(D,kmWeight(D,lambda))

vermaModule = method()
vermaModule (RootDatum,KMWeight) := (D,lambda) -> (
    if parentDatum lambda =!= D then error "vermaModule: highest weight has the wrong parent";
    new KMRepresentation from {
        "parent" => D,
        "highestWeight" => lambda,
        "kind" => "Verma"
    }
)
vermaModule (RootDatum,List) := (D,lambda) -> vermaModule(D,kmWeight(D,lambda))

toString KMRepresentation := M -> concatenate(
    if M#"kind" === "Verma" then "M" else "L",
    draftWeightNotation M#"highestWeight")
net KMRepresentation := M -> net toString M

draftCompositions = (h,r) -> (
    if r == 0 then (if h == 0 then {{}} else {})
    else if r == 1 then {{h}}
    else flatten for x from 0 to h list
        apply(draftCompositions(h-x,r-1),tail -> {x}|tail)
)

draftPositiveRootMultiplicityData = (A,H) -> (
    -- Height zero contains no positive roots.  Handling it here is both the
    -- mathematically correct empty product and essential for depth-zero
    -- Verma, integrable, and BGG truncations: rootSnapshot intentionally
    -- starts at height one.
    if H == 0 then return {};
    S := rootSnapshot(A,H);
    roots := join(entries(S#"real"),entries(S#"imag"));
    apply(roots,alpha -> {alpha,(S#"mult")#alpha})
)

draftAddToTable = (T,key,value) -> (
    if T#?key then T#key = T#key + value else T#key = value
)

-- Coefficients of prod_(alpha>0)(1-e^-alpha)^(-mult(alpha)) through
-- total simple-root height H.  For multiplicity m, the k-th coefficient
-- of (1-x)^(-m) is binomial(m+k-1,k).
draftVermaPartitionTable = (A,H) -> (
    cacheKey := {entries A,H};
    if draftVermaPartitionCache#?cacheKey then
        return draftVermaPartitionCache#cacheKey;
    r := numrows A;
    zero := toList(r:0);
    T := new MutableHashTable;
    T#zero = 1;
    for rootData in draftPositiveRootMultiplicityData(A,H) do (
        alpha := rootData#0;
        m := rootData#1;
        old := new HashTable from pairs T;
        next := new MutableHashTable;
        for p in pairs old do (
            beta := p#0;
            maxK := (H-rootHeight beta) // rootHeight alpha;
            for k from 0 to maxK do (
                gamma := draftAdd(beta,draftScale(k,alpha));
                coefficient := p#1 * binomial(m+k-1,k);
                draftAddToTable(next,gamma,coefficient);
            );
        );
        T = next;
    );
    answer := new HashTable from pairs T;
    draftVermaPartitionCache#cacheKey=answer;
    answer
)

draftRequireCompletedHighestWeight = (M,H,who) -> (
    if not instance(H,ZZ) or H < 0 then error(who | ": Depth must be a nonnegative integer");
    D := M#"parent";
    lambda := M#"highestWeight";
    if cartanType datumCartanMatrix D =!= "finite" then
        try realizationCoordinates lambda else
            error(who | ": a nonfinite character needs a highest weight with full realization coordinates; Dynkin labels alone cannot distinguish delta shifts");
    {D,lambda}
)

vermaCharacter = method(Options => {Depth => null})
vermaCharacter KMRepresentation := opts -> M -> (
    if M#"kind" =!= "Verma" then error "moduleCharacter: expected a Verma module";
    data := draftRequireCompletedHighestWeight(M,opts.Depth,"moduleCharacter");
    D := data#0;
    lambda := data#1;
    H := opts.Depth;
    P := draftVermaPartitionTable(datumCartanMatrix D,H);
    terms := new MutableHashTable;
    for p in pairs P do (
        betaRoot := kmRoot(D,p#0);
        mu := draftWeightMinusRoot(lambda,betaRoot);
        terms#mu = p#1;
    );
    formalCharacter(D,new HashTable from pairs terms,
        CharacterStatus=>"Truncated",
        Truncation=>new HashTable from {
            "kind"=>"root-depth",
            "depth"=>H,
            "completeThroughDepth"=>H,
            "highestWeight"=>lambda,
            "coefficientCertificate"=>true,
            "support"=>"highest-weight cone",
            "module"=>"Verma"
        })
)

draftWeightRootInner = (A,labels,alpha) -> (
    Dsym := symmetrizer A;
    sum apply(toList(0..numrows(A)-1),i ->
        labels#i * Dsym_(i,i) * alpha#i)
)

integrableCharacter = method(Options => {Depth => null})
integrableCharacter KMRepresentation := opts -> M -> (
    if M#"kind" =!= "IntegrableHighestWeight" then
        error "moduleCharacter: expected an integrable highest-weight module";
    if opts.Depth === null and cartanType datumCartanMatrix(M#"parent") === "finite" then
        return finiteCharacter(M#"parent",M#"highestWeight");
    data := draftRequireCompletedHighestWeight(M,opts.Depth,"moduleCharacter");
    D := data#0;
    lambda := data#1;
    H := opts.Depth;
    A := datumCartanMatrix D;
    r := numrows A;
    lambdaLabels := datumCoordinates lambda;
    rootData := draftPositiveRootMultiplicityData(A,H);
    multiplicities := new MutableHashTable;
    zero := toList(r:0);
    multiplicities#zero = 1;
    for h from 1 to H do for beta in draftCompositions(h,r) do (
        denominatorValue := 2*draftWeightRootInner(A,
            apply(lambdaLabels,x -> x+1),beta) - rootInnerProduct(A,beta,beta);
        numeratorValue := 0_QQ;
        for rd in rootData do (
            alpha := rd#0;
            alphaMult := rd#1;
            if rootHeight alpha > h then continue;
            gamma := beta;
            j := 0;
            while true do (
                j = j+1;
                gamma = draftAdd(beta,draftScale(-j,alpha));
                if any(gamma,x -> x < 0) then break;
                if multiplicities#?gamma then (
                    inner := draftWeightRootInner(A,lambdaLabels,alpha)
                        - rootInnerProduct(A,gamma,alpha);
                    numeratorValue = numeratorValue +
                        2*alphaMult*inner*multiplicities#gamma;
                );
            );
        );
        if denominatorValue == 0 then (
            if numeratorValue != 0 then error concatenate(
                "moduleCharacter: Freudenthal denominator vanished with nonzero numerator at offset ",
                toString beta);
        ) else (
            value := numeratorValue/denominatorValue;
            if denominator(value+0_QQ) != 1 or value < 0 then
                error concatenate(
                    "moduleCharacter: Freudenthal recurrence produced invalid multiplicity ",
                    toString value," at offset ",toString beta);
            value = numerator(value+0_QQ);
            if value > 0 then multiplicities#beta = value;
        );
    );
    terms := new MutableHashTable;
    for p in pairs multiplicities do (
        mu := draftWeightMinusRoot(lambda,kmRoot(D,p#0));
        terms#mu = p#1;
    );
    formalCharacter(D,new HashTable from pairs terms,
        CharacterStatus=>"Truncated",
        Truncation=>new HashTable from {
            "kind"=>"root-depth",
            "depth"=>H,
            "completeThroughDepth"=>H,
            "highestWeight"=>lambda,
            "coefficientCertificate"=>true,
            "support"=>"highest-weight cone",
            "module"=>"IntegrableHighestWeight",
            "algorithm"=>"Freudenthal"
        })
)

moduleCharacter = method(Options => {Depth => null})
moduleCharacter KMRepresentation := opts -> M -> (
    if M#"kind" === "IntegrableHighestWeight" then
        integrableCharacter(M,Depth=>opts.Depth)
    else if M#"kind" === "Verma" then
        vermaCharacter(M,Depth=>opts.Depth)
    else error "moduleCharacter: unknown representation kind"
)

-- Reductive Levi restriction ------------------------------------------------

reductiveLeviDatum = method()
reductiveLeviDatum (StandardParabolic,RootDatum) := (P,D) -> (
    if P#"cartanDatum" =!= cartanDatum D then
        error "reductiveLeviDatum: parabolic and root datum have different Cartan parents";
    R := draftSimpleRootMatrix(D#"realization");
    I0 := apply(leviNodes P,i -> i-1);
    RI := if #I0 == 0 then matrix mutableMatrix(QQ,0,numcols R)
        else submatrix(R,I0,toList(0..numcols(R)-1));
    centerColumns := gens kernel RI;
    centerRows := transpose centerColumns;
    -- Restriction records evaluations on the retained simple coroots and
    -- on the Levi center.  These rows must span the entire ambient Cartan
    -- subalgebra.  For a singular retained Cartan matrix they intersect:
    -- their union then loses derivation directions, even when the ambient
    -- weights have complete realization coordinates.  In particular, an
    -- affine weight and its translate by the null root would be conflated.
    C := draftSimpleCorootMatrix(D#"realization");
    CI := if #I0 == 0 then matrix mutableMatrix(QQ,0,numcols C)
        else submatrix(C,I0,toList(0..numcols(C)-1));
    restrictionRows := CI || centerRows;
    if rank restrictionRows != numcols R then
        error concatenate(
            "reductiveLeviDatum: the retained Levi Cartan matrix is singular; ",
            "Levi Dynkin labels and center coordinates do not determine ",
            "the full ambient weight");
    new ReductiveLeviDatum from {
        "parabolic" => P,
        "ambientRootDatum" => D,
        "semisimpleRootDatum" => rootDatum(parabolicLevi P),
        "centerBasis" => apply(entries centerRows,row -> apply(row,draftQQ)),
        "centerDimension" => numrows centerRows,
        "weightLiftMatrix" => inverse transpose restrictionRows,
        "basisConvention" => "kernel basis in ambient realization coordinates"
    }
)

leviCenterBasis = method()
leviCenterBasis ReductiveLeviDatum := L -> (
    if L#"centerDimension" == 0 then matrix mutableMatrix(QQ,0,
        (realization L#"ambientRootDatum")#"dimension")
    else matrix(QQ,L#"centerBasis")
)

restrictWeight = method()
restrictWeight (ReductiveLeviDatum,KMWeight) := (L,x) -> (
    D := L#"ambientRootDatum";
    if parentDatum x =!= D then error "restrictWeight: weight has the wrong ambient root datum";
    labels := apply(leviNodes(L#"parabolic"),i -> (datumCoordinates x)#(i-1));
    semi := kmWeight(L#"semisimpleRootDatum",labels);
    ambient := try realizationCoordinates x else
        error "restrictWeight: full realization coordinates are required to retain the Levi-center character";
    central := apply(flatten entries(matrix(QQ,{ambient}) *
        transpose leviCenterBasis L),draftQQ);
    new LeviWeight from {
        "parent" => L,
        "semisimplePart" => semi,
        "centralCharacter" => central
    }
)
restrictWeight (StandardParabolic,KMWeight) := (P,x) ->
    restrictWeight(reductiveLeviDatum(P,parentDatum x),x)

-- Inverse to the faithful full-Cartan restriction above.  A row of Levi
-- Dynkin coordinates followed by central evaluations is x*transpose(B),
-- where B consists of the retained simple coroots and the center basis.
draftLiftLeviWeight = x -> (
    L := x#"parent";
    if not instance(L,ReductiveLeviDatum) or
        parentDatum semisimplePart x =!= L#"semisimpleRootDatum" then
        error "KacMoody: the Levi weight has incompatible semisimple data";
    central := draftRationalVector(centralCharacter x,L#"centerDimension",
        "KacMoody: lifting a Levi weight");
    coordinates := join(datumCoordinates semisimplePart x,central);
    ambient := flatten entries(matrix(QQ,{coordinates}) * L#"weightLiftMatrix");
    weightFromRealizationCoordinates(L#"ambientRootDatum",ambient)
)

semisimplePart = method()
semisimplePart LeviWeight := x -> x#"semisimplePart"
semisimplePart LeviHighestWeight := x -> x#"semisimplePart"
centralCharacter = method()
centralCharacter LeviWeight := x -> x#"centralCharacter"
centralCharacter LeviHighestWeight := x -> x#"centralCharacter"

toString LeviWeight := x -> draftLeviWeightNotation x
net LeviWeight := x -> net toString x

restrictCharacter = method()
restrictCharacter (ReductiveLeviDatum,FormalCharacter) := (L,C) -> (
    if L#"ambientRootDatum" =!= characterDatum C then
        error "restrictCharacter: character has the wrong ambient root datum";
    if draftCharacterCoordinateMode C === "Labels" then
        error "restrictCharacter: full realization coordinates are required to retain the Levi-center character";
    T := new MutableHashTable;
    for p in pairs characterTerms C do (
        wt := restrictWeight(L,p#0);
        if T#?wt then T#wt=T#wt+p#1 else T#wt=p#1;
    );
    new LeviFormalCharacter from {
        "parent" => L,
        "terms" => draftCanonicalTerms(L,new HashTable from pairs T,LeviWeight,"restrictCharacter"),
        "status" => C#"status",
        "truncation" => if isExactCharacter C then null else new HashTable from {
            "coefficientCertificate"=>true,"operation"=>"restriction","source"=>C}
    }
)
restrictCharacter (StandardParabolic,FormalCharacter) := (P,C) ->
    restrictCharacter(reductiveLeviDatum(P,characterDatum C),C)

-- Full-Cartan Levi restriction is an invertible linear change of weight
-- coordinates.  Lift arithmetic to the ambient weight space so the same
-- coefficient certificates and convolution safeguards apply in both forms.
draftLiftLeviCharacter = C -> (
    L := characterDatum C;
    if not instance(L,ReductiveLeviDatum) then
        error "LeviFormalCharacter arithmetic: expected a reductive Levi datum";
    D := L#"ambientRootDatum";
    status := C#"status";
    truncation := C#"truncation";
    if not member(status,{"Exact","Truncated"}) or
        (status === "Exact" and truncation =!= null) or
        (status === "Truncated" and truncation === null) then
        error "LeviFormalCharacter arithmetic: inconsistent completeness information";
    terms := draftCanonicalTerms(L,characterTerms C,LeviWeight,
        "LeviFormalCharacter arithmetic");
    if status === "Truncated" and instance(truncation,HashTable) and
        truncation#?"operation" and truncation#"operation" === "restriction" then (
        if not truncation#?"source" or
            not instance(truncation#"source",FormalCharacter) then
            error "LeviFormalCharacter arithmetic: invalid restriction certificate";
        source := truncation#"source";
        if characterDatum source =!= D or isExactCharacter source then
            error "LeviFormalCharacter arithmetic: restriction certificate has incompatible ambient data";
        restricted := restrictCharacter(L,source);
        if characterTerms restricted != terms then
            error "LeviFormalCharacter arithmetic: restriction certificate does not agree with the weight table";
        return source;
    );
    liftedTerms := new Tally from apply(pairs terms,p ->
        draftLiftLeviWeight(p#0) => p#1);
    if status === "Exact" then return formalCharacter(D,liftedTerms);
    formalCharacter(D,liftedTerms,CharacterStatus=>"Truncated",
        Truncation=>new HashTable from {
            "coefficientCertificate"=>true,"operation"=>"Levi lift",
            "leviDatum"=>L,"source"=>C})
)

LeviFormalCharacter + LeviFormalCharacter := (C,D) -> (
    draftCheckCharacterParents(C,D,"LeviFormalCharacter + LeviFormalCharacter");
    restrictCharacter(characterDatum C,draftLiftLeviCharacter C+draftLiftLeviCharacter D)
)

- LeviFormalCharacter := C -> (-1)*C

ZZ * LeviFormalCharacter := (a,C) ->
    restrictCharacter(characterDatum C,a*draftLiftLeviCharacter C)
LeviFormalCharacter * ZZ := (C,a) -> a*C
LeviFormalCharacter - LeviFormalCharacter := (C,D) -> C+(-D)

LeviFormalCharacter * LeviFormalCharacter := (C,D) -> (
    draftCheckCharacterParents(C,D,"LeviFormalCharacter * LeviFormalCharacter");
    restrictCharacter(characterDatum C,draftLiftLeviCharacter C*draftLiftLeviCharacter D)
)

LeviFormalCharacter ^ ZZ := (C,k) -> (
    if k < 0 then error "LeviFormalCharacter ^ ZZ: the exponent must be nonnegative";
    if k == 1 then return C;
    L := characterDatum C;
    if k == 0 then (
        D := L#"ambientRootDatum";
        zeroWeight := weightFromRealizationCoordinates(D,
            toList((D#"realization")#"dimension":0));
        return restrictCharacter(L,formalCharacter(D,new Tally from {zeroWeight=>1}));
    );
    restrictCharacter(L,(draftLiftLeviCharacter C)^k)
)

draftLeviWeightNotation = weight -> (
    coordinates := join(datumCoordinates semisimplePart weight,
        centralCharacter weight);
    "(" | concatenate between(", ",apply(coordinates,toString)) | ")"
)

draftLeviCharacterWeightOrder = weight ->
    {draftCharacterWeightOrder(semisimplePart weight),centralCharacter weight}

draftLeviFormalCharacterRecords = C -> apply(
    draftCharacterPreviewPairs(C,draftLeviCharacterWeightOrder),p ->
        {draftLeviCharacterWeightOrder(p#0),
         draftCharacterNumericalMonomial(join(
             datumCoordinates semisimplePart p#0,centralCharacter p#0)),p#1,p#0})

toString LeviFormalCharacter := C ->
    (draftCharacterDisplay(C,"LeviFormalCharacter",
        draftLeviFormalCharacterRecords))#"string"
net LeviFormalCharacter := C ->
    (draftCharacterDisplay(C,"LeviFormalCharacter",
        draftLeviFormalCharacterRecords))#"net"

decomposeLeviCharacter = method()
decomposeLeviCharacter LeviFormalCharacter := C -> (
    if not isExactCharacter C then
        error "decomposeLeviCharacter: exact finite support is required";
    L := characterDatum C;
    grouped := new MutableHashTable;
    for p in pairs characterTerms C do (
        z := centralCharacter p#0;
        if not grouped#?z then grouped#z = new MutableHashTable;
        inner := grouped#z;
        mu := semisimplePart p#0;
        if inner#?mu then inner#mu=inner#mu+p#1 else inner#mu=p#1;
    );
    answer := new MutableHashTable;
    for z in keys grouped do (
        semichar := formalCharacter(L#"semisimpleRootDatum",
            new HashTable from pairs grouped#z);
        decomposition := decomposeCharacter semichar;
        for p in pairs decomposition do (
            key := new LeviHighestWeight from {
                "parent" => L,
                "semisimplePart" => p#0,
                "centralCharacter" => z
            };
            answer#key = p#1;
        );
    );
    new Tally from pairs answer
)

toString LeviHighestWeight := x -> "V" | draftLeviWeightNotation(x)
net LeviHighestWeight := x -> net toString x


-------------------------------------------------------------------------
-- Crystals
-------------------------------------------------------------------------

-- Finite highest-weight crystals.
--
-- The type-A implementation is the semistandard Young-tableau crystal.  A
-- tableau is read by columns from right to left and, within each column,
-- from top to bottom (the Far-Eastern reading word).  For node i we write
-- + under i and - under i+1, cancel +- pairs, and let f_i change the first
-- surviving + while e_i changes the last surviving -.  These conventions
-- make the operators literal partial inverses and agree with the package's
-- convention A_(j,i)=<alpha_i,alpha_j^vee>.
--
-- A minuscule crystal is constructed classification-free from its Weyl
-- orbit.  Since every i-string has length at most one, f_i(mu)=mu-alpha_i
-- precisely when <mu,alpha_i^vee>=1.
-- Other finite highest-weight crystals use Littelmann's exact rational
-- path model.  Tensor products use the usual Kashiwara rule, with e_i
-- acting on the left factor when phi_i(left)>=epsilon_i(right) and f_i
-- acting there when the inequality is strict.

Crystal = new Type of HashTable
CrystalVertex = new Type of HashTable

Crystal == Crystal := (B,C) -> B === C
CrystalVertex == CrystalVertex := (b,c) -> b === c

-- Canonical vertices and their data index are shared by all accessors and
-- Kashiwara operators.  Declare these before any closure refers to them.
draftCrystalVerticesCache := new MutableHashTable
draftCrystalVertexIndexCache := new MutableHashTable

-------------------------------------------------------------------------
-- Validation and canonical node coordinates
-------------------------------------------------------------------------

draftCrystalMaxElements = (value,who) -> (
    if value === null then return null;
    if not instance(value,ZZ) or value < 1 then
        error(who | ": MaxElements must be a positive integer or null");
    value
)

draftCrystalFiniteConnected = (D,who) -> (
    A := datumCartanMatrix D;
    if cartanType A =!= "finite" then
        error(who | ": the root datum must be of finite type");
    components := (cartanDatum D)#"components";
    if #components != 1 then
        error(who | ": the Cartan datum must be connected");
    {A,first components}
)

draftCrystalDominantWeight = (D,lambda,who) -> (
    if not instance(lambda,KMWeight) then
        error(who | ": expected a KMWeight");
    if parentDatum lambda =!= D then
        error(who | ": the highest weight belongs to a different root datum");
    labels := datumCoordinates lambda;
    if not all(labels,x -> instance(x,ZZ) and x >= 0) then
        error(who | ": the highest weight must have nonnegative integral Dynkin labels");
    labels
)

-- Node arguments may be their 1-based positions or user-supplied labels.
-- Integers in 1..rank mean positions; otherwise label lookup is attempted,
-- allowing deliberately numeric labels such as 10 and 20.
draftCrystalNode = (B,node,who) -> (
    r := B#"rank";
    -- Ordinary 1-based positions take precedence.  An integer outside that
    -- range may still be a user-supplied node label (for example 10 or 20).
    if instance(node,ZZ) and node >= 1 and node <= r then return node;
    labels := (cartanDatum(B#"parent"))#"nodeLabels";
    p := position(labels,x -> x === node or x == node);
    if p === null then error(who | ": node is neither a valid position nor a label of this Cartan datum");
    p+1
)

draftCrystalCanonicalPosition = (B,node) -> (
    p := position(B#"canonicalNodeOrder",j -> j == node);
    if p === null then error "KacMoody: internal crystal node-order mismatch";
    p+1
)

draftCrystalCheckParent = (B,b,who) -> (
    if not instance(b,CrystalVertex) then error(who | ": expected a CrystalVertex");
    if b#"parent" =!= B then error(who | ": the vertex belongs to a different crystal");
    vertices := crystalVertices B;
    if not (draftCrystalVertexIndexCache#B)#?(b#"data") then
        error(who | ": the vertex data do not occur in this crystal");
    b
)

-------------------------------------------------------------------------
-- Semistandard tableaux and Kashiwara signatures
-------------------------------------------------------------------------

draftCrystalPartition = labels -> for i from 0 to #labels-1 list
    sum for j from i to #labels-1 list labels#j

draftCrystalTableaux = (shape,alphabetSize,maxElements) -> (
    -- Trailing zero rows contain no information and are omitted from the
    -- public tableau.  The empty shape still has its unique empty tableau.
    shape = select(shape,n -> n > 0);
    cells := flatten for row from 0 to #shape-1 list
        for column from 0 to shape#row-1 list {row,column};
    entriesSoFar := new MutableHashTable;
    answer := {};
    visit := null;
    visit = positionInCells -> (
        if positionInCells == #cells then (
            tableau := for row from 0 to #shape-1 list
                for column from 0 to shape#row-1 list
                    entriesSoFar#{row,column};
            answer = append(answer,tableau);
            if maxElements =!= null and #answer > maxElements then
                error concatenate(
                    "highestWeightCrystal: construction exceeded MaxElements => ",
                    toString maxElements,
                    "; increase the guard only if this output size is intentional");
            return null;
        );
        cell := cells#positionInCells;
        row := cell#0;
        column := cell#1;
        lower := 1;
        if column > 0 then lower = max(lower,entriesSoFar#{row,column-1});
        if row > 0 and shape#(row-1) > column then
            lower = max(lower,entriesSoFar#{row-1,column}+1);

        -- A strict column needs one distinct larger entry for every box
        -- below the current one.  This bound prunes branches that cannot be
        -- completed, without changing the set of tableaux.
        boxesBelow := 0;
        for lowerRow from row+1 to #shape-1 do
            if shape#lowerRow > column then boxesBelow = boxesBelow+1;
        upper := alphabetSize-boxesBelow;
        for value from lower to upper do (
            entriesSoFar#cell = value;
            visit(positionInCells+1);
        );
        null
    );
    visit 0;
    answer
)

draftCrystalReadingCells = T -> (
    if #T == 0 then return {};
    width := max apply(T,row -> #row);
    flatten for column in reverse toList(0..width-1) list
        for row from 0 to #T-1 list
            if #T#row > column then {row,column} else continue
)

draftCrystalSignature = (B,T,actualNode) -> (
    canonicalNode := draftCrystalCanonicalPosition(B,actualNode);
    survivingPlus := {};
    survivingMinus := {};
    for cell in draftCrystalReadingCells T do (
        value := T#(cell#0)#(cell#1);
        if value == canonicalNode then
            survivingPlus = append(survivingPlus,cell)
        else if value == canonicalNode+1 then (
            if #survivingPlus > 0 then
                survivingPlus = if #survivingPlus == 1 then {}
                    else take(survivingPlus,#survivingPlus-1)
            else survivingMinus = append(survivingMinus,cell);
        );
    );
    new HashTable from {
        "plus"=>survivingPlus,
        "minus"=>survivingMinus
    }
)

draftCrystalReplaceTableauEntry = (T,cell,value) ->
    for row from 0 to #T-1 list (
        for column from 0 to #T#row-1 list (
            if row == cell#0 and column == cell#1 then value
            else T#row#column
        )
    )

draftCrystalTableauWeight = (B,T) -> (
    alphabetSize := B#"rank"+1;
    counts := new MutableList from toList(alphabetSize:0);
    for row in T do for value in row do counts#(value-1)=counts#(value-1)+1;
    canonicalLabels := for i from 0 to B#"rank"-1 list counts#i-counts#(i+1);
    actualLabels := new MutableList from toList(B#"rank":0);
    for i from 0 to B#"rank"-1 do
        actualLabels#((B#"canonicalNodeOrder"#i)-1)=canonicalLabels#i;
    draftCrystalLiftWeight(B,toList actualLabels)
)

-- A crystal model is most naturally enumerated by Dynkin labels.  When its
-- highest weight is a complete weight in a nonminimal realization, lift each
-- label back into the same root-lattice translate so that all Weyl-fixed
-- central coordinates are retained.  For label-class input the output stays
-- a label class, as it should.
draftCrystalLiftWeight = (B,labels) -> (
    D := B#"parent";
    highest := B#"highestWeight";
    beta := toSimple(datumCartanMatrix D,
        draftAdd(datumCoordinates highest,draftScale(-1,labels)));
    if not all(beta,x -> denominator(x+0_QQ) == 1) then
        error "KacMoody: internal crystal weight is outside the highest-weight root-lattice translate";
    beta = apply(beta,x -> numerator(x+0_QQ));
    draftWeightMinusRoot(highest,kmRoot(D,beta))
)

-------------------------------------------------------------------------
-- Littelmann paths, in exact rational Dynkin coordinates
-------------------------------------------------------------------------

-- A path is a list { {t_0,pi(t_0)}, ..., {t_s,pi(t_s)} }, with endpoints
-- t_0=0 and t_s=1.  Remove collinear breakpoints so equal paths have equal
-- keys.  The fixed central part is t times that of the highest weight;
-- endpoint lifting below therefore retains complete realization weights.
draftCrystalNormalizePath = path -> (
    answer := {};
    for point in path do (
        answer = append(answer,point);
        while #answer >= 3 do (
            a := answer#(#answer-3);
            b := answer#(#answer-2);
            c := last answer;
            if draftScale(c#0-b#0,draftAdd(b#1,draftScale(-1,a#1))) !=
                draftScale(b#0-a#0,draftAdd(c#1,draftScale(-1,b#1))) then break;
            answer = append(take(answer,#answer-2),c);
        );
    );
    answer
)

draftCrystalPathPoint = (path,t) -> (
    for point in path do if point#0 == t then return point;
    for k from 0 to #path-2 do (
        a := path#k;
        b := path#(k+1);
        if a#0 < t and t < b#0 then return {t,
            apply(draftAdd(a#1,draftScale((t-a#0)/(b#0-a#0),
                draftAdd(b#1,draftScale(-1,a#1)))),draftQQ)};
    );
    error "KacMoody: internal path parameter is outside [0,1]"
)

draftCrystalPathCrossings = (path,i,height) -> (
    times := apply(select(path,p -> p#1#i == height),p -> p#0);
    for k from 0 to #path-2 do (
        a := path#k;
        b := path#(k+1);
        if (a#1#i < height and height < b#1#i) or
           (b#1#i < height and height < a#1#i) then
            times = append(times,draftQQ(a#0+
                (b#0-a#0)*(height-a#1#i)/(b#1#i-a#1#i)));
    );
    sort unique times
)

draftCrystalPathStep = (A,path,node,raising) -> (
    i := node-1;
    minimum := min apply(path,p -> p#1#i);
    if not instance(minimum,ZZ) then
        error "KacMoody: internal Littelmann path has a nonintegral minimum";
    t0 := null;
    t1 := null;
    baseline := null;
    sign := null;
    if raising then (
        if minimum == 0 then return null;
        t1 = first draftCrystalPathCrossings(path,i,minimum);
        t0 = last select(draftCrystalPathCrossings(path,i,minimum+1),t -> t < t1);
        baseline = minimum+1;
        sign = 1;
    ) else (
        if (last path)#1#i-minimum == 0 then return null;
        t0 = last draftCrystalPathCrossings(path,i,minimum);
        t1 = first select(draftCrystalPathCrossings(path,i,minimum+1),t -> t > t0);
        baseline = minimum;
        sign = -1;
    );
    times := sort unique join(apply(path,p -> p#0),{t0,t1});
    column := for j from 0 to numrows A-1 list A_(j,i);
    draftCrystalNormalizePath for t in times list (
        point := draftCrystalPathPoint(path,t);
        labels := point#1;
        if t > t0 and t <= t1 then
            labels = draftAdd(labels,draftScale(baseline-labels#i,column))
        else if t > t1 then labels = draftAdd(labels,draftScale(sign,column));
        {t,apply(labels,draftQQ)}
    )
)

draftCrystalPathData = (A,labels,maxElements) -> (
    highest := {{0,toList(#labels:0)},{1,labels}};
    seen := new MutableHashTable;
    seen#highest = true;
    queue := new MutableList;
    queue#0 = highest;
    head := 0;
    while head < #queue do (
        path := queue#head;
        head = head+1;
        for i from 1 to #labels do (
            lower := draftCrystalPathStep(A,path,i,false);
            if lower =!= null and not seen#?lower then (
                seen#lower = true;
                queue#(#queue) = lower;
                if maxElements =!= null and #queue > maxElements then
                    error "highestWeightCrystal: path construction exceeded MaxElements";
            );
        );
    );
    toList queue
)

-------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------

highestWeightCrystal = method(Options => {MaxElements=>100000})

highestWeightCrystal (RootDatum,KMWeight) := opts -> (D,lambda) -> (
    maxElements := draftCrystalMaxElements(opts.MaxElements,"highestWeightCrystal");
    A := datumCartanMatrix D;
    if cartanType A =!= "finite" then
        error "highestWeightCrystal: the root datum must be of finite type";
    labels := draftCrystalDominantWeight(D,lambda,"highestWeightCrystal");
    expectedCardinality := weylDimension(A,labels);
    if maxElements =!= null and expectedCardinality > maxElements then
        error concatenate(
            "highestWeightCrystal: the Weyl dimension is ",
            toString expectedCardinality,
            ", exceeding MaxElements => ",toString maxElements);
    components := (cartanDatum D)#"components";
    if #components != 1 or (first components)#"family" =!= "A" then (
        paths := draftCrystalPathData(A,labels,maxElements);
        if #paths != expectedCardinality then
            error "KacMoody: internal path enumeration failed the Weyl-dimension check";
        return new Crystal from {
            "parent"=>D,
            "model"=>"LittelmannPaths",
            "highestWeight"=>lambda,
            "rank"=>#labels,
            "canonicalNodeOrder"=>toList(1..#labels),
            "cardinality"=>expectedCardinality,
            "vertexData"=>paths
        };
    );
    canonicalOrder := (first components)#"canonicalNodeOrder";
    canonicalLabels := apply(canonicalOrder,node -> labels#(node-1));
    shape := draftCrystalPartition canonicalLabels;
    tableaux := draftCrystalTableaux(shape,#labels+1,maxElements);
    if #tableaux != expectedCardinality then
        error "KacMoody: internal tableau enumeration failed the Weyl-dimension check";
    new Crystal from {
        "parent"=>D,
        "model"=>"SemistandardTableaux",
        "highestWeight"=>lambda,
        "rank"=>#labels,
        "canonicalNodeOrder"=>canonicalOrder,
        "shape"=>select(shape,n -> n > 0),
        "alphabet"=>toList(1..#labels+1),
        "cardinality"=>expectedCardinality,
        "vertexData"=>tableaux
    }
)

highestWeightCrystal (RootDatum,List) := opts -> (D,lambda) ->
    highestWeightCrystal(D,kmWeight(D,lambda),MaxElements=>opts.MaxElements)
highestWeightCrystal (RootDatum,Matrix) := opts -> (D,lambda) ->
    highestWeightCrystal(D,kmWeight(D,lambda),MaxElements=>opts.MaxElements)
highestWeightCrystal (CartanDatum,List) := opts -> (D,lambda) ->
    highestWeightCrystal(rootDatum D,lambda,MaxElements=>opts.MaxElements)
highestWeightCrystal (CartanDatum,Matrix) := opts -> (D,lambda) ->
    highestWeightCrystal(rootDatum D,lambda,MaxElements=>opts.MaxElements)
highestWeightCrystal (Matrix,List) := opts -> (A,lambda) ->
    highestWeightCrystal(rootDatum A,lambda,MaxElements=>opts.MaxElements)
highestWeightCrystal (Matrix,Matrix) := opts -> (A,lambda) ->
    highestWeightCrystal(rootDatum A,lambda,MaxElements=>opts.MaxElements)

tensorProductCrystal = method(Options => {MaxElements=>100000})
tensorProductCrystal (Crystal,Crystal) := opts -> (B,C) -> (
    maxElements := draftCrystalMaxElements(opts.MaxElements,"tensorProductCrystal");
    if crystalDatum B =!= crystalDatum C then
        error "tensorProductCrystal: the factors belong to different root data";
    -- Addition also checks that complete weights and label classes are not
    -- mixed in a nonminimal realization.
    highest := crystalHighestWeight B+crystalHighestWeight C;
    cardinality := #(B#"vertexData")*#(C#"vertexData");
    if maxElements =!= null and cardinality > maxElements then
        error concatenate("tensorProductCrystal: the product has ",
            toString cardinality," vertices, exceeding MaxElements => ",
            toString maxElements);
    new Crystal from {
        "parent"=>crystalDatum B,
        "model"=>"TensorProduct",
        "highestWeight"=>highest,
        "rank"=>B#"rank",
        "canonicalNodeOrder"=>toList(1..B#"rank"),
        "factors"=>{B,C},
        "cardinality"=>cardinality,
        "vertexData"=>flatten for b in B#"vertexData" list
            for c in C#"vertexData" list {b,c}
    }
)

draftMinusculeOrbit = (A,highest,maxElements) -> (
    seen := new MutableHashTable;
    seen#highest = true;
    queue := new MutableList;
    queue#0 = highest;
    head := 0;
    while head < #queue do (
        mu := queue#head;
        head = head+1;
        for i from 0 to numrows A-1 do if mu#i == 1 then (
            lower := for j from 0 to numrows A-1 list mu#j-A_(j,i);
            if not seen#?lower then (
                seen#lower = true;
                queue#(#queue) = lower;
                if maxElements =!= null and #queue > maxElements then
                    error concatenate(
                        "minusculeCrystal: construction exceeded MaxElements => ",
                        toString maxElements,
                        "; increase the guard only if this output size is intentional");
            );
        );
    );
    toList queue
)

minusculeCrystal = method(Options => {MaxElements=>100000})

minusculeCrystal (RootDatum,KMWeight) := opts -> (D,lambda) -> (
    maxElements := draftCrystalMaxElements(opts.MaxElements,"minusculeCrystal");
    finiteData := draftCrystalFiniteConnected(D,"minusculeCrystal");
    A := finiteData#0;
    labels := draftCrystalDominantWeight(D,lambda,"minusculeCrystal");
    if not isMinusculeWeight(A,labels) then
        error "minusculeCrystal: the highest weight is not minuscule";
    expectedCardinality := weylDimension(A,labels);
    if maxElements =!= null and expectedCardinality > maxElements then
        error concatenate(
            "minusculeCrystal: the Weyl dimension is ",
            toString expectedCardinality,
            ", exceeding MaxElements => ",toString maxElements);
    weights := draftMinusculeOrbit(A,labels,maxElements);
    if #weights != expectedCardinality then
        error "KacMoody: internal minuscule-orbit enumeration failed the Weyl-dimension check";
    new Crystal from {
        "parent"=>D,
        "model"=>"MinusculeOrbit",
        "highestWeight"=>lambda,
        "rank"=>#labels,
        "canonicalNodeOrder"=>(finiteData#1)#"canonicalNodeOrder",
        "cardinality"=>expectedCardinality,
        "vertexData"=>weights
    }
)

minusculeCrystal (RootDatum,List) := opts -> (D,lambda) ->
    minusculeCrystal(D,kmWeight(D,lambda),MaxElements=>opts.MaxElements)
minusculeCrystal (RootDatum,Matrix) := opts -> (D,lambda) ->
    minusculeCrystal(D,kmWeight(D,lambda),MaxElements=>opts.MaxElements)
minusculeCrystal (RootDatum,ZZ) := opts -> (D,node) -> (
    finiteData := draftCrystalFiniteConnected(D,"minusculeCrystal");
    r := numrows(finiteData#0);
    if node < 1 or node > r then (
        availableNodeLabels := (cartanDatum D)#"nodeLabels";
        p := position(availableNodeLabels,x -> x === node or x == node);
        if p === null then
            error "minusculeCrystal: node is neither a valid position nor a label of this Cartan datum";
        node = p+1;
    );
    if not member(node,minusculeNodes(finiteData#0)) then
        error "minusculeCrystal: the selected fundamental weight is not minuscule";
    labels := for i from 1 to r list if i == node then 1 else 0;
    minusculeCrystal(D,labels,MaxElements=>opts.MaxElements)
)
minusculeCrystal (RootDatum,Thing) := opts -> (D,nodeLabel) -> (
    labels := (cartanDatum D)#"nodeLabels";
    p := position(labels,x -> x === nodeLabel or x == nodeLabel);
    if p === null then
        error "minusculeCrystal: node is not a label of this Cartan datum";
    minusculeCrystal(D,p+1,MaxElements=>opts.MaxElements)
)
minusculeCrystal (CartanDatum,List) := opts -> (D,lambda) ->
    minusculeCrystal(rootDatum D,lambda,MaxElements=>opts.MaxElements)
minusculeCrystal (CartanDatum,Matrix) := opts -> (D,lambda) ->
    minusculeCrystal(rootDatum D,lambda,MaxElements=>opts.MaxElements)
minusculeCrystal (CartanDatum,ZZ) := opts -> (D,node) ->
    minusculeCrystal(rootDatum D,node,MaxElements=>opts.MaxElements)
minusculeCrystal (CartanDatum,Thing) := opts -> (D,nodeLabel) ->
    minusculeCrystal(rootDatum D,nodeLabel,MaxElements=>opts.MaxElements)
minusculeCrystal (Matrix,List) := opts -> (A,lambda) ->
    minusculeCrystal(rootDatum A,lambda,MaxElements=>opts.MaxElements)
minusculeCrystal (Matrix,Matrix) := opts -> (A,lambda) ->
    minusculeCrystal(rootDatum A,lambda,MaxElements=>opts.MaxElements)
minusculeCrystal (Matrix,ZZ) := opts -> (A,node) ->
    minusculeCrystal(rootDatum A,node,MaxElements=>opts.MaxElements)
minusculeCrystal (Matrix,Thing) := opts -> (A,nodeLabel) ->
    minusculeCrystal(rootDatum A,nodeLabel,MaxElements=>opts.MaxElements)

-------------------------------------------------------------------------
-- Vertices and accessors
-------------------------------------------------------------------------

crystalDatum = method()
crystalDatum Crystal := B -> B#"parent"

crystalHighestWeight = method()
crystalHighestWeight Crystal := B -> B#"highestWeight"

crystalParent = method()
crystalParent CrystalVertex := b -> b#"parent"

crystalVertexData = method()
crystalVertexData CrystalVertex := b -> b#"data"

crystalVertex = method()
crystalVertex (Crystal,List) := (B,data) -> (
    -- Build the canonical index lazily.  This turns every Kashiwara step
    -- from a linear scan of all vertices into a hash lookup.
    vertices := crystalVertices B;
    index := draftCrystalVertexIndexCache#B;
    if not index#?data then
        error "crystalVertex: the supplied data do not label a vertex of this crystal";
    index#data
)

crystalVertices = method()
crystalVertices Crystal := B -> (
    if draftCrystalVerticesCache#?B then return draftCrystalVerticesCache#B;
    index := new MutableHashTable;
    vertices := apply(B#"vertexData",data -> (
        vertex := new CrystalVertex from {"parent"=>B,"data"=>data};
        index#data=vertex;
        vertex
    ));
    draftCrystalVerticesCache#B=vertices;
    draftCrystalVertexIndexCache#B=index;
    vertices
)

crystalContains = method()
crystalContains (Crystal,CrystalVertex) := (B,b) -> (
    if crystalParent b =!= B then return false;
    vertices := crystalVertices B;
    (draftCrystalVertexIndexCache#B)#?(crystalVertexData b)
)

crystalTableau = method()
crystalTableau CrystalVertex := b -> (
    B := crystalParent b;
    draftCrystalCheckParent(B,b,"crystalTableau");
    if B#"model" =!= "SemistandardTableaux" then
        error "crystalTableau: this vertex belongs to a non-tableau crystal";
    crystalVertexData b
)

crystalWeight = method()
draftCrystalWeightCache := new MutableHashTable
crystalWeight CrystalVertex := b -> (
    draftCrystalCheckParent(crystalParent b,b,"crystalWeight");
    if draftCrystalWeightCache#?b then return draftCrystalWeightCache#b;
    B := crystalParent b;
    answer := if B#"model" === "SemistandardTableaux" then
        draftCrystalTableauWeight(B,crystalVertexData b)
    else if B#"model" === "LittelmannPaths" then
        draftCrystalLiftWeight(B,(last crystalVertexData b)#1)
    else if B#"model" === "TensorProduct" then (
        data := crystalVertexData b;
        crystalWeight crystalVertex(B#"factors"#0,data#0)+
            crystalWeight crystalVertex(B#"factors"#1,data#1)
    )
    else draftCrystalLiftWeight(B,crystalVertexData b);
    draftCrystalWeightCache#b=answer;
    answer
)
crystalWeight (Crystal,CrystalVertex) := (B,b) -> (
    draftCrystalCheckParent(B,b,"crystalWeight");
    crystalWeight b
)

draftCrystalF = (b,node) -> (
    B := crystalParent b;
    draftCrystalCheckParent(B,b,"crystalF");
    i := draftCrystalNode(B,node,"crystalF");
    if B#"model" === "SemistandardTableaux" then (
        T := crystalVertexData b;
        signature := draftCrystalSignature(B,T,i);
        if #(signature#"plus") == 0 then return null;
        cell := first(signature#"plus");
        canonicalNode := draftCrystalCanonicalPosition(B,i);
        return crystalVertex(B,
            draftCrystalReplaceTableauEntry(T,cell,canonicalNode+1));
    );
    if B#"model" === "LittelmannPaths" then (
        path := draftCrystalPathStep(datumCartanMatrix(B#"parent"),crystalVertexData b,i,false);
        return if path === null then null else crystalVertex(B,path);
    );
    if B#"model" === "TensorProduct" then (
        data := crystalVertexData b;
        left := crystalVertex(B#"factors"#0,data#0);
        right := crystalVertex(B#"factors"#1,data#1);
        target := if crystalPhi(left,i) > crystalEpsilon(right,i) then (
            lower := crystalF(left,i);
            if lower === null then null else {crystalVertexData lower,data#1}
        ) else (
            rightLower := crystalF(right,i);
            if rightLower === null then null else {data#0,crystalVertexData rightLower}
        );
        return if target === null then null else crystalVertex(B,target);
    );
    mu := crystalVertexData b;
    if mu#(i-1) != 1 then return null;
    A := datumCartanMatrix(B#"parent");
    lowerLabels := for j from 0 to B#"rank"-1 list mu#j-A_(j,i-1);
    crystalVertex(B,lowerLabels)
)

draftCrystalE = (b,node) -> (
    B := crystalParent b;
    draftCrystalCheckParent(B,b,"crystalE");
    i := draftCrystalNode(B,node,"crystalE");
    if B#"model" === "SemistandardTableaux" then (
        T := crystalVertexData b;
        signature := draftCrystalSignature(B,T,i);
        if #(signature#"minus") == 0 then return null;
        cell := last(signature#"minus");
        canonicalNode := draftCrystalCanonicalPosition(B,i);
        return crystalVertex(B,
            draftCrystalReplaceTableauEntry(T,cell,canonicalNode));
    );
    if B#"model" === "LittelmannPaths" then (
        path := draftCrystalPathStep(datumCartanMatrix(B#"parent"),crystalVertexData b,i,true);
        return if path === null then null else crystalVertex(B,path);
    );
    if B#"model" === "TensorProduct" then (
        data := crystalVertexData b;
        left := crystalVertex(B#"factors"#0,data#0);
        right := crystalVertex(B#"factors"#1,data#1);
        target := if crystalPhi(left,i) >= crystalEpsilon(right,i) then (
            upper := crystalE(left,i);
            if upper === null then null else {crystalVertexData upper,data#1}
        ) else (
            rightUpper := crystalE(right,i);
            if rightUpper === null then null else {data#0,crystalVertexData rightUpper}
        );
        return if target === null then null else crystalVertex(B,target);
    );
    mu := crystalVertexData b;
    if mu#(i-1) != -1 then return null;
    A := datumCartanMatrix(B#"parent");
    upperLabels := for j from 0 to B#"rank"-1 list mu#j+A_(j,i-1);
    crystalVertex(B,upperLabels)
)

crystalF = method()
crystalF (CrystalVertex,Thing) := (b,node) -> draftCrystalF(b,node)
crystalF (Crystal,CrystalVertex,Thing) := (B,b,node) -> (
    draftCrystalCheckParent(B,b,"crystalF");
    draftCrystalF(b,node)
)

crystalE = method()
crystalE (CrystalVertex,Thing) := (b,node) -> draftCrystalE(b,node)
crystalE (Crystal,CrystalVertex,Thing) := (B,b,node) -> (
    draftCrystalCheckParent(B,b,"crystalE");
    draftCrystalE(b,node)
)

crystalEpsilon = method()
crystalEpsilon (CrystalVertex,Thing) := (b,node) -> (
    B := crystalParent b;
    draftCrystalCheckParent(B,b,"crystalEpsilon");
    i := draftCrystalNode(B,node,"crystalEpsilon");
    if B#"model" === "SemistandardTableaux" then
        #((draftCrystalSignature(B,crystalVertexData b,i))#"minus")
    else if B#"model" === "LittelmannPaths" then
        -min apply(crystalVertexData b,p -> p#1#(i-1))
    else if B#"model" === "TensorProduct" then (
        data := crystalVertexData b;
        left := crystalVertex(B#"factors"#0,data#0);
        right := crystalVertex(B#"factors"#1,data#1);
        crystalEpsilon(left,i)+max(0,crystalEpsilon(right,i)-crystalPhi(left,i))
    )
    else if (crystalVertexData b)#(i-1) == -1 then 1 else 0
)

crystalPhi = method()
crystalPhi (CrystalVertex,Thing) := (b,node) -> (
    B := crystalParent b;
    draftCrystalCheckParent(B,b,"crystalPhi");
    i := draftCrystalNode(B,node,"crystalPhi");
    if B#"model" === "SemistandardTableaux" then
        #((draftCrystalSignature(B,crystalVertexData b,i))#"plus")
    else if B#"model" === "LittelmannPaths" then (
        path := crystalVertexData b;
        (last path)#1#(i-1)-min apply(path,p -> p#1#(i-1))
    )
    else if B#"model" === "TensorProduct" then (
        data := crystalVertexData b;
        left := crystalVertex(B#"factors"#0,data#0);
        right := crystalVertex(B#"factors"#1,data#1);
        crystalPhi(right,i)+max(0,crystalPhi(left,i)-crystalEpsilon(right,i))
    )
    else if (crystalVertexData b)#(i-1) == 1 then 1 else 0
)

crystalEdges = method()
crystalEdges Crystal := B -> flatten for b in crystalVertices B list
    for i from 1 to B#"rank" list (
        target := crystalF(b,i);
        if target === null then continue;
        new HashTable from {
            "source"=>b,
            "node"=>i,
            "nodeLabel"=>(cartanDatum(B#"parent"))#"nodeLabels"#(i-1),
            "target"=>target
        }
    )

crystalCharacter = method()
draftCrystalCharacterCache := new MutableHashTable
crystalCharacter Crystal := B -> (
    if draftCrystalCharacterCache#?B then return draftCrystalCharacterCache#B;
    terms := new MutableHashTable;
    for b in crystalVertices B do (
        mu := crystalWeight b;
        if terms#?mu then terms#mu=terms#mu+1 else terms#mu=1;
    );
    answer := formalCharacter(B#"parent",new Tally from pairs terms);
    draftCrystalCharacterCache#B=answer;
    answer
)

-- These hooks are intentionally private.  The package-wide infrastructure
-- layer can report and clear the process-local memoization without exposing
-- mutable cache tables as part of the mathematical API.
crystalInfrastructureCacheSize = () ->
    #keys draftCrystalVerticesCache+
    #keys draftCrystalVertexIndexCache+
    #keys draftCrystalWeightCache+
    #keys draftCrystalCharacterCache

clearCrystalInfrastructureCache = () -> (
    draftCrystalVerticesCache = new MutableHashTable;
    draftCrystalVertexIndexCache = new MutableHashTable;
    draftCrystalWeightCache = new MutableHashTable;
    draftCrystalCharacterCache = new MutableHashTable;
    null
)

toString Crystal := B -> concatenate(
    if B#"model" === "TensorProduct" then
        "B"|draftWeightNotation crystalHighestWeight(B#"factors"#0)|" (x) B"|
            draftWeightNotation crystalHighestWeight(B#"factors"#1)
    else "B"|draftWeightNotation(B#"highestWeight")," [",
    toString(#(B#"vertexData")),
    if #(B#"vertexData") == 1 then " vertex]" else " vertices]")
net Crystal := B -> net toString B

-- The weight does not identify a tableau when a weight space has dimension
-- greater than one.  Display the tableau itself, with semicolons separating
-- rows, so distinct vertices of the same weight remain distinguishable.
toString CrystalVertex := b -> (
    wt := draftWeightNotation crystalWeight b;
    B := crystalParent b;
    if B#"model" === "LittelmannPaths" then
        return concatenate("Path [",demark(", ",apply(crystalVertexData b,p ->
            "("|toString(p#0)|"; "|draftCoordinateTuple(p#1)|")")),"]; wt = ",wt);
    if B#"model" === "TensorProduct" then (
        data := crystalVertexData b;
        leftIndex := position(B#"factors"#0#"vertexData",d -> d == data#0)+1;
        rightIndex := position(B#"factors"#1#"vertexData",d -> d == data#1)+1;
        return concatenate("b_",toString leftIndex," (x) b_",toString rightIndex,"; wt = ",wt);
    );
    if (crystalParent b)#"model" =!= "SemistandardTableaux" then
        return "b" | wt;
    T := crystalTableau b;
    if sum apply(T,row -> #row) > 80 then
        return concatenate("Tableau of shape ",toString apply(T,row -> #row),
            " [entries omitted]; wt = ",wt);
    concatenate("T = [",demark("; ",apply(T,row ->
        demark(" ",apply(row,toString)))),"]; wt(T) = ",wt)
)
net CrystalVertex := b -> net toString b


-------------------------------------------------------------------------
-- Kostant Multiplicity
-------------------------------------------------------------------------

-- Independent finite-type weight multiplicities from Kostant's formula.
-- This is deliberately separate from the default Freudenthal backend: two
-- mathematically independent algorithms are valuable both to users and as a
-- correctness oracle for the representation-theory test suite.

draftKostantBeta = (A,beta,who) -> (
    raw := validateRootCoordinateVector(A,beta,who);
    q := apply(raw,x -> x+0_QQ);
    if any(q,x -> denominator x != 1) then
        error(who | ": simple-root coordinates must be integers");
    apply(q,numerator)
)

draftKostantPartitionTable = (A,bound) -> (
    roots := entries cartanPositiveRoots A;
    zero := toList(numrows(A):0);
    table := new MutableHashTable;
    table#zero = 1;
    for alpha in roots do (
        old := new HashTable from pairs table;
        next := new MutableHashTable from pairs old;
        for p in pairs old do (
            gamma := p#0;
            while true do (
                gamma = draftAdd(gamma,alpha);
                if any(toList(0..#bound-1),j -> gamma#j > bound#j) then break;
                if next#?gamma then next#gamma=next#gamma+p#1
                else next#gamma=p#1;
            );
        );
        table = next;
    );
    new HashTable from pairs table
)

kostantPartitionFunction = method()
kostantPartitionFunction (Matrix,List) := (A,beta) -> (
    A = integralCartanMatrix(A,"kostantPartitionFunction");
    requireFiniteCartanType("kostantPartitionFunction",A);
    beta = draftKostantBeta(A,beta,"kostantPartitionFunction");
    if any(beta,x -> x < 0) then return 0;
    P := draftKostantPartitionTable(A,beta);
    if P#?beta then P#beta else 0
)
kostantPartitionFunction (CartanDatum,List) := (D,beta) ->
    kostantPartitionFunction(datumCartanMatrix D,beta)
kostantPartitionFunction (RootDatum,KMRoot) := (D,beta) -> (
    if parentDatum beta =!= D then error "kostantPartitionFunction: root has the wrong parent";
    kostantPartitionFunction(datumCartanMatrix D,datumCoordinates beta)
)

kostantWeightMultiplicity = method()
kostantWeightMultiplicity (Matrix,List,List) := (A,lambda,mu) -> (
    A = integralCartanMatrix(A,"kostantWeightMultiplicity");
    validateFiniteCartanForRepresentations(A,"kostantWeightMultiplicity");
    lambda = normalizeIntegralDynkinWeight(A,lambda,"kostantWeightMultiplicity",true);
    mu = normalizeIntegralDynkinWeight(A,mu,"kostantWeightMultiplicity",false);
    difference := toSimple(A,draftAdd(lambda,draftScale(-1,mu)));
    if any(difference,x -> denominator(x+0_QQ) != 1 or x < 0) then return 0;

    shiftedLambda := apply(lambda,x -> x+1);
    shiftedMu := apply(mu,x -> x+1);
    candidates := {};
    for w in weylGroupElements A do (
        image := weylActionOnWeight(A,w#"word",shiftedLambda);
        nuDynkin := draftAdd(image,draftScale(-1,shiftedMu));
        nuSimple := toSimple(A,nuDynkin);
        if all(nuSimple,x -> denominator(x+0_QQ) == 1 and x >= 0) then
            candidates = append(candidates,{
                apply(nuSimple,x -> numerator(x+0_QQ)),
                (-1)^(w#"length")
            });
    );
    if #candidates == 0 then return 0;
    bound := apply(numrows A,j -> max apply(candidates,p -> p#0#j));
    P := draftKostantPartitionTable(A,bound);
    sum apply(candidates,p -> if P#?(p#0) then p#1*P#(p#0) else 0)
)

kostantWeightMultiplicity (RootDatum,KMWeight,KMWeight) := (D,lambda,mu) -> (
    if parentDatum lambda =!= D or parentDatum mu =!= D then
        error "kostantWeightMultiplicity: weights have the wrong parent";
    A := integralCartanMatrix(datumCartanMatrix D,"kostantWeightMultiplicity");
    validateFiniteCartanForRepresentations(A,"kostantWeightMultiplicity");
    lambdaLabels := normalizeIntegralDynkinWeight(A,datumCoordinates lambda,
        "kostantWeightMultiplicity",true);
    muLabels := normalizeIntegralDynkinWeight(A,datumCoordinates mu,
        "kostantWeightMultiplicity",false);
    fullLambda := lambda#"realizationCoordinates";
    fullMu := mu#"realizationCoordinates";
    if (fullLambda === null) =!= (fullMu === null) then
        error concatenate("kostantWeightMultiplicity: cannot compare a complete ",
            "realization weight with a Dynkin-label class; supply both complete ",
            "weights or both label classes");
    if fullLambda =!= null then (
        -- A finite-type module keeps one central character.  Dynkin labels
        -- determine the possible root offset, but they do not establish
        -- that this offset equals the difference of the complete weights.
        -- Check the root span in the actual realization, without choosing
        -- supplementary coordinates or assuming a block-diagonal basis.
        beta := toSimple(A,draftAdd(lambdaLabels,draftScale(-1,muLabels)));
        if any(beta,x -> denominator(x+0_QQ) != 1 or x < 0) then return 0;
        beta = apply(beta,x -> numerator(x+0_QQ));
        rootDifference := realizationCoordinates kmRoot(D,beta);
        if draftAdd(fullLambda,draftScale(-1,fullMu)) != rootDifference then
            return 0;
    );
    kostantWeightMultiplicity(A,lambdaLabels,muLabels)
)


-------------------------------------------------------------------------
-- Geometry
-------------------------------------------------------------------------

-- Borel--Weil--Bott and Demazure character operators.

BorelWeilBottResult = new Type of HashTable
BorelWeilBottResult == BorelWeilBottResult := (R,S) -> R === S

toString BorelWeilBottResult := R -> if R#"vanishes" then
    "0" else concatenate(
    "H^",toString R#"degree"," = V",
    draftWeightNotation R#"cohomologyHighestWeight")
net BorelWeilBottResult := R -> net toString R

borelWeilBott = method(Options => {
    LineBundleConvention => "FiberMinusWeight"
})

borelWeilBott (RootDatum,KMWeight) := opts -> (D,lambda) -> (
    if parentDatum lambda =!= D then error "borelWeilBott: weight has the wrong parent";
    if opts.LineBundleConvention =!= "FiberMinusWeight" then
        error "borelWeilBott: LineBundleConvention must be \"FiberMinusWeight\"";
    A := datumCartanMatrix D;
    if cartanType A =!= "finite" then error "borelWeilBott: finite type is required";
    labels := datumCoordinates lambda;
    if not all(labels,x -> instance(x,ZZ)) then
        error "borelWeilBott: the line-bundle weight must be integral";
    shifted := apply(labels,x -> x+1);
    word := {};
    degree := 0;
    bound := numrows cartanPositiveRoots A;
    while true do (
        if any(shifted,x -> x == 0) then return new BorelWeilBottResult from {
            "parent"=>D,
            "inputWeight"=>lambda,
            "vanishes"=>true,
            "degree"=>null,
            "dominantWeight"=>null,
            "cohomologyHighestWeight"=>null,
            "weylElement"=>weylElement(weylGroup D,word),
            "lineBundleConvention"=>"G_sc x^{B_sc} C_{-lambda}; cohomology is V(w dot lambda)^*"
        };
        negatives := select(toList(0..numrows(A)-1),i -> shifted#i < 0);
        negative := if #negatives == 0 then null else first negatives;
        if negative === null then (
            -- Applying the dot action to the original weight retains any
            -- central character in a nonminimal finite realization.  Its
            -- cohomology is the dual module, including the opposite central
            -- character; rebuilding from Dynkin labels would lose it.
            dominant := dotWeylAction(weylElement(weylGroup D,word),lambda);
            cohomologyHighest := dualWeight dominant;
            return new BorelWeilBottResult from {
                "parent"=>D,
                "inputWeight"=>lambda,
                "vanishes"=>false,
                "degree"=>degree,
                "dominantWeight"=>dominant,
                "cohomologyHighestWeight"=>cohomologyHighest,
                "weylElement"=>weylElement(weylGroup D,word),
                "lineBundleConvention"=>"G_sc x^{B_sc} C_{-lambda}; H^degree is V(w dot lambda)^*"
            };
        );
        coefficient := shifted#negative;
        shifted = draftSimpleWeightLabels(A,negative+1,shifted);
        word = {negative+1}|word;
        degree = degree+1;
        if degree > bound then error "borelWeilBott: chamber walk exceeded the positive-root bound";
    )
)

borelWeilBott (RootDatum,List) := opts -> (D,lambda) ->
    borelWeilBott(D,kmWeight(D,lambda),LineBundleConvention=>opts.LineBundleConvention)
borelWeilBott (Matrix,List) := opts -> (A,lambda) ->
    borelWeilBott(rootDatum A,lambda,LineBundleConvention=>opts.LineBundleConvention)

borelWeilBottCharacter = method(Options => {
    LineBundleConvention => "FiberMinusWeight"
})
borelWeilBottCharacter (RootDatum,KMWeight) := opts -> (D,lambda) -> (
    result := borelWeilBott(D,lambda,LineBundleConvention=>opts.LineBundleConvention);
    if result#"vanishes" then formalCharacter(D,new Tally)
    else finiteCharacter(D,result#"cohomologyHighestWeight")
)
borelWeilBottCharacter (RootDatum,List) := opts -> (D,lambda) ->
    borelWeilBottCharacter(D,kmWeight(D,lambda),
        LineBundleConvention=>opts.LineBundleConvention)

draftShiftWeightBySimpleRoot = (lambda,i,k) -> (
    D := parentDatum lambda;
    if lambda#"realizationCoordinates" === null then (
        A := datumCartanMatrix D;
        labels := apply(numrows A,j ->
            (datumCoordinates lambda)#j-k*A_(j,i-1));
        kmWeight(D,labels)
    ) else lambda-k*toWeight(kmRoot(D,apply((D#"cartanDatum")#"rank",j ->
        if j == i-1 then 1 else 0)))
)

demazureOperator = method()
demazureOperator (ZZ,FormalCharacter) := (i,C) -> (
    if not isExactCharacter C then error "demazureOperator: an exact finite-support character is required; use demazureCharacter for a highest-weight Demazure module";
    D := characterDatum C;
    r := (D#"cartanDatum")#"rank";
    if i < 1 or i > r then error "demazureOperator: node must be between 1 and rank";
    totals := new MutableHashTable;
    for p in pairs characterTerms C do (
        lambda := p#0;
        coefficient := p#1;
        m := (datumCoordinates lambda)#(i-1);
        if not instance(m,ZZ) then
            error "demazureOperator: the selected Dynkin label must be integral";
        if m >= 0 then for k from 0 to m do
            draftAddToTable(totals,draftShiftWeightBySimpleRoot(lambda,i,k),coefficient)
        else if m <= -2 then for k from 1 to -m-1 do
            draftAddToTable(totals,draftShiftWeightBySimpleRoot(lambda,i,-k),-coefficient);
    );
    formalCharacter(D,new HashTable from pairs totals,
        CharacterStatus=>C#"status",Truncation=>C#"truncation")
)
demazureOperator (FormalCharacter,ZZ) := (C,i) -> demazureOperator(i,C)

demazureCharacter = method()
demazureCharacter (RootDatum,KMWeight,List) := (D,lambda,word) -> (
    if parentDatum lambda =!= D then error "demazureCharacter: highest weight has the wrong parent";
    if not all(datumCoordinates lambda,x -> instance(x,ZZ) and x >= 0) then
        error "demazureCharacter: highest weight must be dominant integral";
    W := weylGroup D;
    w := weylElement(W,word);
    answer := formalCharacter(D,new Tally from {lambda=>1});
    positions := if #(reducedWord w) == 0 then {} else reverse toList(0..#(reducedWord w)-1);
    for pos in positions do answer = demazureOperator((reducedWord w)#pos,answer);
    answer
)
demazureCharacter (RootDatum,List,List) := (D,lambda,word) ->
    demazureCharacter(D,kmWeight(D,lambda),word)
demazureCharacter (Matrix,List,List) := (A,lambda,word) ->
    demazureCharacter(rootDatum A,lambda,word)


-------------------------------------------------------------------------
-- Category O
-------------------------------------------------------------------------

-- BGG category O: interval-local Kazhdan--Lusztig polynomials, Verma
-- composition multiplicities, and finite homological truncations of the
-- BGG resolution for arbitrary symmetrizable Kac--Moody type.

-- A local generator has the conventional printed name without replacing a
-- user's value of q when this package is loaded.
KazhdanLusztigPolynomialRing = ZZ[local q]
BGGResolution = new Type of HashTable
BGGResolution == BGGResolution := (B,C) -> B === C

kazhdanLusztigVariable = () -> KazhdanLusztigPolynomialRing_0

draftKLTrim = p -> (
    answer := p;
    while #answer > 1 and last answer == 0 do answer = drop(answer,-1);
    answer
)

draftKLAdd = (p,q) -> draftKLTrim apply(max(#p,#q),i ->
    (if i < #p then p#i else 0)+(if i < #q then q#i else 0))
draftKLScale = (a,p) -> draftKLTrim apply(p,c -> a*c)
draftKLShift = (d,p) -> (
    if all(p,c -> c==0) then {0} else toList(d:0)|p
)
draftKLCoefficient = (p,d) -> if d < 0 or d >= #p then 0 else p#d
draftKLMultiply = (p,q) -> draftKLTrim apply(#p+#q-1,d ->
    sum apply(toList(0..d),i -> draftKLCoefficient(p,i)*draftKLCoefficient(q,d-i)))

draftKLGlobalCache := new MutableHashTable
categoryOInfrastructureCacheSize = () -> #keys draftKLGlobalCache
clearCategoryOInfrastructureCache = () -> (draftKLGlobalCache = new MutableHashTable; null)

-- A reduced expression y=s_1...s_m contains a reduced expression for every
-- z<=y as a subword.  For connected support, grow products of subwords from
-- left to right and identify equal products after each step.  Disconnected
-- support is handled by its factors instead.  Both cases describe precisely
-- the finite principal Bruhat interval [e,y], even in an infinite ambient
-- group; MaxElements bounds its full cardinality, not just stored factors.
draftKLState = W -> (
    D := W#"cartanDatum";
    if draftKLGlobalCache#?D then return draftKLGlobalCache#D;
    state := new MutableHashTable from {
        "weylGroup"=>W,
        "intervals"=>new MutableHashTable,
        "memo"=>new MutableHashTable,
        "cartanEntries"=>entries datumCartanMatrix D,
        "identityEntries"=>entries id_(ZZ^(D#"rank")),
        "elements"=>new MutableHashTable,
        "products"=>new MutableHashTable,
        "bruhat"=>new MutableHashTable,
        "descents"=>new MutableHashTable,
        "correctionCandidates"=>new MutableHashTable,
        "supportComponents"=>new MutableHashTable
    };
    draftKLGlobalCache#D = state;
    state
)

-- The Cartan parent and reduced words have already been validated by the
-- public constructors.  Within the KL recurrence, use the stored integral
-- root action rather than repeatedly reconstructing matrices and reducing
-- the same reflection products.  Right multiplication by s_i replaces
-- column j by column j - a_(i,j) column i.
draftKLReflectEntries = (state,M,i) -> (
    rowA := (state#"cartanEntries")#(i-1);
    apply(M,row -> apply(#row,j -> row#j-rowA#j*row#(i-1)))
)

draftKLHasRightDescent = (M,i) -> any(M,row -> row#(i-1)<0)

draftKLElementFromEntries = (state,M) -> (
    cache := state#"elements";
    if cache#?M then return cache#M;
    reduced := new MutableList;
    count := 0;
    remaining := M;
    rank := #(state#"cartanEntries");
    while remaining =!= state#"identityEntries" do (
        i := first select(toList(1..rank),j ->
            draftKLHasRightDescent(remaining,j));
        reduced#count = i;
        count = count+1;
        remaining = draftKLReflectEntries(state,remaining,i);
    );
    answer := new WeylElement from {
        "parent"=>state#"weylGroup",
        "word"=>reverse toList reduced,
        "length"=>count,
        "rootActionEntries"=>M
    };
    cache#M = answer;
    answer
)

draftKLRightMultiply = (state,w,i) -> (
    key := {reducedWord w,i};
    products := state#"products";
    if products#?key then return products#key;
    answer := draftKLElementFromEntries(state,
        draftKLReflectEntries(state,w#"rootActionEntries",i));
    products#key = answer;
    answer
)

draftKLRightDescents = (state,w) -> (
    key := reducedWord w;
    cache := state#"descents";
    if cache#?key then return cache#key;
    answer := select(toList(1..#(state#"cartanEntries")),i ->
        draftKLHasRightDescent(w#"rootActionEntries",i));
    cache#key = answer;
    answer
)

draftKLBruhat = (state,x,y) -> (
    key := {reducedWord x,reducedWord y};
    cache := state#"bruhat";
    if cache#?key then return cache#key;
    if weylLength x > weylLength y then (cache#key=false; return false);
    remaining := x#"rootActionEntries";
    for i in reverse reducedWord y do if draftKLHasRightDescent(remaining,i) then
        remaining = draftKLReflectEntries(state,remaining,i);
    answer := remaining === state#"identityEntries";
    cache#key = answer;
    answer
)

-- A standard parabolic subgroup depends only on the support of its upper
-- word.  Distinct connected components of that support commute, even if
-- omitted nodes connect them in the ambient Dynkin diagram.  Their Bruhat
-- intervals and KL polynomials are direct products.
draftKLSupportComponents = (state,w) -> (
    key := reducedWord w;
    cache := state#"supportComponents";
    if cache#?key then return cache#key;
    A := state#"cartanEntries";
    unseen := sort unique key;
    components := {};
    while #unseen > 0 do (
        component := {first unseen};
        unseen = drop(unseen,1);
        frontier := component;
        while #frontier > 0 do (
            adjacent := select(unseen,j -> any(frontier,i -> A#(i-1)#(j-1)!=0));
            component = component|adjacent;
            unseen = select(unseen,j -> not member(j,adjacent));
            frontier = adjacent;
        );
        components = append(components,sort component);
    );
    cache#key = components;
    components
)

draftKLComponentElement = (state,w,nodes) -> (
    remaining := state#"identityEntries";
    for i in reducedWord w do if member(i,nodes) then
        remaining = draftKLReflectEntries(state,remaining,i);
    draftKLElementFromEntries(state,remaining)
)

draftKLPrincipalInterval = (W,y,maxElements) -> (
    maxElements = validateMaxElements("kazhdanLusztigPolynomial",maxElements);
    state := draftKLState W;
    intervals := state#"intervals";
    upperKey := reducedWord y;
    if intervals#?upperKey then (
        cached := intervals#upperKey;
        if maxElements =!= null and cached#"elementCount" > maxElements then
            error concatenate(
                "kazhdanLusztigPolynomial: cached principal Bruhat interval has ",
                toString cached#"elementCount",
                " elements, exceeding MaxElements => ",toString maxElements);
        return cached;
    );
    components := draftKLSupportComponents(state,y);
    if #components > 1 then (
        factors := {};
        factorCount := 1;
        for nodes in components do (
            upper := draftKLComponentElement(state,y,nodes);
            factor := draftKLPrincipalInterval(W,upper,maxElements);
            factorCount = factorCount*factor#"elementCount";
            if maxElements =!= null and factorCount > maxElements then
                error concatenate(
                    "kazhdanLusztigPolynomial: principal Bruhat interval [e,v] ",
                    "has ",toString factorCount," or more elements, exceeding MaxElements => ",
                    toString maxElements);
            factors = append(factors,{nodes,upper});
        );
        factorizedInterval := new HashTable from {
            "upper"=>y,"factors"=>factors,"elementCount"=>factorCount
        };
        intervals#upperKey = factorizedInterval;
        return factorizedInterval;
    );
    identity := draftKLElementFromEntries(state,state#"identityEntries");
    elements := {identity};
    seen := new MutableHashTable;
    seen#(reducedWord identity) = true;
    count := 1;
    for i in reducedWord y do (
        old := elements;
        additions := new MutableList;
        additionCount := 0;
        for z in old do (
            candidate := draftKLRightMultiply(state,z,i);
            key := reducedWord candidate;
            if not seen#?key then (
                seen#key = true;
                count = count+1;
                if maxElements =!= null and count > maxElements then
                    error concatenate(
                        "kazhdanLusztigPolynomial: principal Bruhat interval [e,v] ",
                        "would contain more than ",toString maxElements,
                        " elements; increase MaxElements only if that cost is intentional");
                additions#additionCount = candidate;
                additionCount = additionCount+1;
            );
        );
        elements = elements | toList additions;
    );
    interval := new HashTable from {
        "upper"=>y,
        "elements"=>elements,
        "elementCount"=>count
    };
    intervals#upperKey = interval;
    interval
)

draftKazhdanLusztigCoefficients = (u,v,maxElements) -> (
    W := weylParent u;
    if weylParent v =!= W then
        error "kazhdanLusztigPolynomial: Weyl elements have different parents";
    maxElements = validateMaxElements("kazhdanLusztigPolynomial",maxElements);
    state := draftKLState W;
    if not draftKLBruhat(state,u,v) then return {0};
    if u === v then return {1};
    interval := draftKLPrincipalInterval(W,v,maxElements);
    memo := state#"memo";
    pairKey := {reducedWord u,reducedWord v};
    if memo#?pairKey then return memo#pairKey;
    if interval#?"factors" then (
        answer := {1};
        for factor in interval#"factors" do answer = draftKLMultiply(answer,
            draftKazhdanLusztigCoefficients(
                draftKLComponentElement(state,u,factor#0),factor#1,maxElements));
        memo#pairKey = answer;
        return answer;
    );
    elements := interval#"elements";
    KL := null;
    KL = (x,y) -> (
        key := {reducedWord x,reducedWord y};
        if memo#?key then return memo#key;
        if not draftKLBruhat(state,x,y) then (memo#key={0}; return {0});
        if x === y then (memo#key={1}; return {1});
        descents := draftKLRightDescents(state,y);
        if #descents == 0 then
            error "kazhdanLusztigPolynomial: nonidentity upper element has no descent";
        i := first descents;
        ys := draftKLRightMultiply(state,y,i);
        xs := draftKLRightMultiply(state,x,i);
        c := if weylLength xs < weylLength x then 1 else 0;
        answer := draftKLAdd(
            draftKLShift(1-c,KL(xs,ys)),
            draftKLShift(c,KL(x,ys)));
        candidatesKey := {reducedWord y,i};
        candidatesCache := state#"correctionCandidates";
        if not candidatesCache#?candidatesKey then
            candidatesCache#candidatesKey = select(elements,z ->
                weylLength z < weylLength ys and
                (weylLength ys-weylLength z)%2 == 1 and
                draftKLHasRightDescent(z#"rootActionEntries",i) and
                draftKLBruhat(state,z,ys));
        for z in candidatesCache#candidatesKey do if draftKLBruhat(state,x,z) then (
                difference := weylLength ys-weylLength z;
                mu := draftKLCoefficient(KL(z,ys),(difference-1)//2);
                if mu != 0 then (
                    exponent := (weylLength y-weylLength z)//2;
                    answer = draftKLAdd(answer,
                        draftKLScale(-mu,draftKLShift(exponent,KL(x,z))));
                );
            );
        answer = draftKLTrim answer;
        memo#key = answer;
        answer
    );
    KL(u,v)
)

kazhdanLusztigCoefficients = method(Options => {MaxElements=>100000})
kazhdanLusztigCoefficients (WeylElement,WeylElement) := opts -> (u,v) ->
    draftKazhdanLusztigCoefficients(u,v,opts.MaxElements)
kazhdanLusztigCoefficients (Matrix,List,List) := opts -> (A,u,v) -> (
    W := weylGroup A;
    kazhdanLusztigCoefficients(weylElement(W,u),weylElement(W,v),
        MaxElements=>opts.MaxElements)
)

-- The recurrence stores integral coefficient lists independently of the
-- chosen polynomial ring.  A user's generator changes only the ring in
-- which that exact polynomial is written; it does not change the interval
-- bound or reuse a polynomial from a previous coefficient ring.
draftKLPolynomialVariable = variable -> (
    if variable === null then return kazhdanLusztigVariable();
    if not instance(variable,RingElement) then
        error "kazhdanLusztigPolynomial: Variable must be a polynomial-ring generator over ZZ or QQ";
    R := ring variable;
    if not instance(R,PolynomialRing) then
        error "kazhdanLusztigPolynomial: Variable must be a polynomial-ring generator over ZZ or QQ";
    if not isCommutative R or
        (coefficientRing R =!= ZZ and coefficientRing R =!= QQ) then
        error "kazhdanLusztigPolynomial: Variable must be a polynomial-ring generator over ZZ or QQ";
    if not any(gens R,g -> variable === g) then
        error "kazhdanLusztigPolynomial: Variable must be a polynomial-ring generator, not an arbitrary polynomial";
    variable
)

kazhdanLusztigPolynomial = method(Options => {MaxElements=>100000,Variable=>null})
kazhdanLusztigPolynomial (WeylElement,WeylElement) := opts -> (u,v) -> (
    q := draftKLPolynomialVariable opts.Variable;
    coefficients := kazhdanLusztigCoefficients(u,v,MaxElements=>opts.MaxElements);
    sum apply(toList(0..#coefficients-1),i ->
        (coefficients#i)*q^i)
)
kazhdanLusztigPolynomial (Matrix,List,List) := opts -> (A,u,v) -> (
    W := weylGroup A;
    kazhdanLusztigPolynomial(weylElement(W,u),weylElement(W,v),
        MaxElements=>opts.MaxElements,Variable=>opts.Variable)
)

-- The rho lift is canonical relative to the chosen realization basis.  Any
-- two lifts differ by a coroot-annihilating direction, which the Weyl group
-- fixes, so the resulting dot action is independent of this section.
draftRhoWeight = D -> (
    C := draftSimpleCorootMatrix(D#"realization");
    labels := matrix(QQ,{toList((D#"cartanDatum")#"rank":1)});
    ambient := labels * inverse(C*transpose C) * C;
    weightFromRealizationCoordinates(D,flatten entries ambient)
)

dotWeylAction = method()
dotWeylAction (WeylElement,KMWeight) := (w,lambda) -> (
    D := parentDatum lambda;
    if (weylParent w)#"cartanDatum" =!= cartanDatum D then
        error "dotWeylAction: Weyl element and weight have different parents";
    if lambda#"realizationCoordinates" === null then (
        A := datumCartanMatrix D;
        shifted := apply(datumCoordinates lambda,x -> x+1);
        positions := if #(reducedWord w)==0 then {} else reverse toList(0..#(reducedWord w)-1);
        for pos in positions do shifted = draftSimpleWeightLabels(
            A,(reducedWord w)#pos,shifted);
        kmWeight(D,apply(shifted,x -> x-1))
    ) else (
        rho := draftRhoWeight D;
        weylAction(w,lambda+rho)-rho
    )
)

simpleHighestWeightModule = method()
simpleHighestWeightModule (RootDatum,KMWeight) := (D,lambda) -> (
    if parentDatum lambda =!= D then
        error "simpleHighestWeightModule: highest weight has the wrong parent";
    new KMRepresentation from {
        "parent"=>D,
        "highestWeight"=>lambda,
        "kind"=>"SimpleHighestWeight"
    }
)
simpleHighestWeightModule (RootDatum,List) := (D,lambda) ->
    simpleHighestWeightModule(D,kmWeight(D,lambda))

vermaCompositionMultiplicity = method(Options => {MaxElements=>100000})
vermaCompositionMultiplicity (RootDatum,KMWeight,WeylElement,WeylElement) :=
    opts -> (D,lambda,w,x) -> (
        if parentDatum lambda =!= D then
            error "vermaCompositionMultiplicity: block weight has the wrong parent";
        if (weylParent w)#"cartanDatum" =!= cartanDatum D or
           weylParent x =!= weylParent w then
            error "vermaCompositionMultiplicity: Weyl parents do not match the root datum";
        if not (cartanDatum D)#"symmetrizable" then
            error "vermaCompositionMultiplicity: the Cartan datum must be symmetrizable";
        if not all(datumCoordinates lambda,a -> instance(a,ZZ) and a >= 0) then
            error "vermaCompositionMultiplicity: lambda must be dominant integral (hence dot-regular)";
        sum kazhdanLusztigCoefficients(w,x,MaxElements=>opts.MaxElements)
    )

draftCategoryOFiniteWeylElements = (W,maxElements,who) -> (
    A := datumCartanMatrix(W#"cartanDatum");
    if cartanType A =!= "finite" then
        error(who | ": the complete answer has infinitely many Weyl-group candidates; query a specified Weyl element instead");
    apply(weylGroupElements(A,MaxElements=>maxElements),
        h -> weylElement(W,h#"word"))
)

vermaCompositionFactors = method(Options => {MaxElements=>100000})
vermaCompositionFactors (RootDatum,KMWeight,WeylElement) := opts -> (D,lambda,w) -> (
    W := weylParent w;
    if parentDatum lambda =!= D then
        error "vermaCompositionFactors: block weight has the wrong parent";
    if W#"cartanDatum" =!= cartanDatum D then
        error "vermaCompositionFactors: Weyl parent does not match the root datum";
    elements := draftCategoryOFiniteWeylElements(
        W,opts.MaxElements,"vermaCompositionFactors");
    answer := new MutableHashTable;
    for x in elements do if bruhatLessOrEqual(w,x) then (
        multiplicity := vermaCompositionMultiplicity(D,lambda,w,x,
            MaxElements=>opts.MaxElements);
        if multiplicity != 0 then (
            module := simpleHighestWeightModule(D,dotWeylAction(x,lambda));
            answer#module = multiplicity;
        );
    );
    new Tally from pairs answer
)

-- Return parent-aware Weyl elements in homological length strata.  A null
-- degree is used only for a finite Weyl group and means the complete group;
-- a finite degree uses the package's breadth-first enumeration and therefore
-- also works for affine and indefinite Coxeter groups.
draftCategoryOWeylLevels = (W,maxDegree,maxElements) -> (
    A := datumCartanMatrix(W#"cartanDatum");
    if maxDegree === null then (
        elements := draftCategoryOFiniteWeylElements(
            W,maxElements,"bggResolution");
        longest := max apply(elements,weylLength);
        return for d from 0 to longest list
            select(elements,w -> weylLength w == d);
    );
    raw := weylGroupEnumerate(A,maxDegree,maxElements);
    for d from 0 to maxDegree list (
        if d >= #raw then {}
        else apply(raw#d,h -> weylElement(W,h#"word"))
    )
)

bggResolution = method(Options => {MaxElements=>100000,MaxDegree=>null})
bggResolution (RootDatum,KMWeight) := opts -> (D,lambda) -> (
    if parentDatum lambda =!= D then error "bggResolution: weight has the wrong parent";
    A := datumCartanMatrix D;
    if not (cartanDatum D)#"symmetrizable" then
        error "bggResolution: the Cartan datum must be symmetrizable";
    if not all(datumCoordinates lambda,a -> instance(a,ZZ) and a >= 0) then
        error "bggResolution: highest weight must be dominant integral";
    maxDegree := opts.MaxDegree;
    if maxDegree =!= null and (not instance(maxDegree,ZZ) or maxDegree < 0) then
        error "bggResolution: MaxDegree must be null or a nonnegative integer";
    finiteType := cartanType A === "finite";
    if not finiteType and maxDegree === null then
        error "bggResolution: nonfinite Weyl type requires a finite MaxDegree";
    finiteLength := if finiteType then numrows cartanPositiveRoots A else null;
    exact := finiteType and
        (maxDegree === null or maxDegree >= finiteLength);
    computedDegree := if exact then finiteLength else maxDegree;
    W := weylGroup D;
    levels := draftCategoryOWeylLevels(
        W,if exact then null else computedDegree,opts.MaxElements);
    byDegree := new MutableHashTable;
    for d from 0 to computedDegree do (
        byDegree#d = new MutableHashTable;
        for w in levels#d do (
            module := vermaModule(D,dotWeylAction(w,lambda));
            byDegree#d#module = 1;
        );
    );
    terms := new HashTable from for d in keys byDegree list
        d => new Tally from pairs byDegree#d;
    new BGGResolution from {
        "parent"=>D,
        "highestWeight"=>lambda,
        "terms"=>terms,
        "length"=>computedDegree,
        "exact"=>exact,
        "computedThroughDegree"=>computedDegree,
        "truncation"=>if exact then null else new HashTable from {
            "kind"=>"homological-degree",
            "maxDegree"=>computedDegree,
            "completeThroughDegree"=>computedDegree
        },
        "status"=>if exact then
            "all terms of the classical BGG resolution; differentials are not materialized"
        else concatenate(
            "terms of the classical BGG resolution through homological degree ",
            toString computedDegree,"; differentials are not materialized")
    }
)
bggResolution (RootDatum,List) := opts -> (D,lambda) ->
    bggResolution(D,kmWeight(D,lambda),MaxElements=>opts.MaxElements,
        MaxDegree=>opts.MaxDegree)

-- Show actual Verma summands for small resolutions, and their numbers for
-- larger ones.  Arrows would suggest that differential maps had also been
-- computed; only the terms of the BGG resolution are recorded here.
draftBGGTermNotation = T -> (
    summands := sort apply(pairs T,p -> (
        moduleName := "M" | draftWeightNotation(p#0#"highestWeight");
        if p#1 == 1 then moduleName
        else concatenate(moduleName,"^(",toString p#1,")")
    ));
    if #summands == 0 then "0" else demark(" (+) ",summands)
)

toString BGGResolution := B -> (
    degrees := sort keys B#"terms";
    expandSummands := sum apply(degrees,d -> #(keys(B#"terms"#d))) <= 16;
    shownDegrees := if #degrees <= 8 then degrees
        else take(degrees,4)|{null}|take(degrees,-3);
    rows := apply(shownDegrees,d -> if d === null then "  ..." else (
        T := B#"terms"#d;
        concatenate("  C_",toString d,if expandSummands then " = " else ": ",
            if expandSummands then draftBGGTermNotation T
            else concatenate(toString sum values T," Verma summand",
                if sum values T == 1 then "" else "s"))
    ));
    demark("\n",{
        "BGG terms for L" | draftWeightNotation B#"highestWeight",
        demark("\n",rows),
        if B#"exact" then "All homological degrees; differential maps not computed."
        else concatenate("Through homological degree ",
            toString B#"computedThroughDegree",
            "; differential maps not computed.")
    })
)
net BGGResolution := B -> net toString B

-- The difference of two complete weights is computed in the realization.
-- If both are Dynkin-label classes for a nonminimal finite realization, the
-- finite Cartan matrix is invertible and their simple-root offset is still
-- determined uniquely by the label difference.  This lets category O work
-- either with an explicitly fixed central character or modulo that character.
draftCategoryORootOffset = (lambda,highest) -> (
    D := parentDatum lambda;
    if parentDatum highest =!= D then
        error "bggEulerCharacter: weights have different parents";
    lambdaFull := lambda#"realizationCoordinates";
    highestFull := highest#"realizationCoordinates";
    if lambdaFull === null and highestFull === null then (
        if (D#"cartanDatum")#"corank" > 0 then
            error concatenate(
                "bggEulerCharacter: a singular Cartan datum requires complete ",
                "realization coordinates to distinguish null-root shifts");
        offset := toSimple(datumCartanMatrix D,
            draftAdd(datumCoordinates lambda,
                draftScale(-1,datumCoordinates highest)));
        if not all(offset,x -> denominator(x+0_QQ) == 1) then
            error "bggEulerCharacter: a BGG highest-weight difference is not in the root lattice";
        apply(offset,x -> numerator(x+0_QQ))
    )
    else if lambdaFull =!= null and highestFull =!= null then
        weightToRootCoordinates(lambda-highest)
    else error "bggEulerCharacter: resolution mixes complete weights and Dynkin-label classes"
)

bggEulerCharacter = method(Options => {Depth=>null})
bggEulerCharacter BGGResolution := opts -> B -> (
    H := opts.Depth;
    if not instance(H,ZZ) or H < 0 then
        error "bggEulerCharacter: Depth must be a nonnegative integer";
    D := B#"parent";
    lambda := B#"highestWeight";
    exactResolution := if B#?"exact" then B#"exact" else true;
    -- For dominant integral lambda, lambda-w.lambda is a sum of l(w)
    -- positive inversion roots with positive integral coefficients.  Its
    -- simple-root height is therefore at least l(w).  Terms beyond
    -- homological degree k cannot affect absolute root depth at most k.
    if not exactResolution and H > B#"computedThroughDegree" then
        error concatenate(
            "bggEulerCharacter: Depth => ",toString H,
            " exceeds the BGG resolution's certified homological cutoff ",
            toString B#"computedThroughDegree");
    totals := new MutableHashTable;
    for d in keys B#"terms" do for p in pairs(B#"terms"#d) do (
        module := p#0;
        highest := module#"highestWeight";
        offset := draftCategoryORootOffset(lambda,highest);
        if all(offset,a -> instance(a,ZZ) and a >= 0) then (
            offsetDepth := sum offset;
            if offsetDepth <= H then (
                C := vermaCharacter(module,Depth=>H-offsetDepth);
                sign := (-1)^d;
                for term in pairs characterTerms C do
                    draftAddToTable(totals,term#0,sign*p#1*term#1);
            );
        );
    );
    formalCharacter(D,new HashTable from pairs totals,
        CharacterStatus=>"Truncated",
        Truncation=>new HashTable from {
            "kind"=>"root-depth",
            "depth"=>H,
            "completeThroughDepth"=>H,
            "construction"=>"BGG Euler characteristic",
            "highestWeight"=>lambda,
            "coefficientCertificate"=>true,
            "support"=>"highest-weight cone",
            "resolutionExact"=>exactResolution,
            "resolutionComputedThroughDegree"=>B#"computedThroughDegree"
        })
)


-------------------------------------------------------------------------
-- Character Operations
-------------------------------------------------------------------------

-- Lambda-ring operations on exact finite-support formal characters.

draftRequireActualFiniteCharacter = (C,who) -> (
    if not isExactCharacter C then error(who | ": an exact finite-support character is required");
    if any(values characterTerms C,m -> m < 0) then
        error(who | ": a genuine character with nonnegative weight multiplicities is required");
    C
)

adamsOperation = method()
adamsOperation (ZZ,FormalCharacter) := (k,C) -> (
    if k < 1 then error "adamsOperation: k must be positive";
    if not isExactCharacter C then error "adamsOperation: an exact finite-support character is required";
    formalCharacter(characterDatum C,new HashTable from apply(pairs characterTerms C,p ->
        (k*p#0) => p#1))
)
adamsOperation (FormalCharacter,ZZ) := (C,k) -> adamsOperation(k,C)

draftCharacterPower = (C,k,exterior,who) -> (
    draftRequireActualFiniteCharacter(C,who);
    if not instance(k,ZZ) or k < 0 then error(who | ": k must be a nonnegative integer");
    D := characterDatum C;
    support := characterSupport C;
    -- Zero must live in the same version of the weight space as the input:
    -- a complete realization weight or a Dynkin-label class.  Using a
    -- complete zero unconditionally makes nonminimal label-class characters
    -- impossible to add to during the lambda-ring recurrence.
    zeroWeight := if #support == 0 then
        kmWeight(D,toList((cartanDatum D)#"rank":0))
    else 0*(first support);
    if k == 0 then return formalCharacter(D,new Tally from {zeroWeight=>1});
    dimension := sum values characterTerms C;
    if #support == 0 or (exterior and k > dimension) then
        return formalCharacter(D,new Tally);
    if exterior and k == dimension then (
        -- The determinant is one-dimensional, with the sum of all weights
        -- counted with their multiplicities.  No intermediate powers are
        -- needed, even when the original representation is very large.
        determinantWeight := zeroWeight;
        for p in pairs characterTerms C do
            determinantWeight = determinantWeight+p#1*p#0;
        return formalCharacter(D,new Tally from {determinantWeight=>1});
    );
    -- For a single weight of multiplicity m, all degree-k monomials have
    -- weight k*mu, and their number is one binomial coefficient.
    if #support == 1 then (
        mu := first support;
        m := (characterTerms C)#mu;
        coefficient := if exterior then binomial(m,k) else binomial(m+k-1,k);
        return formalCharacter(D,new Tally from {(k*mu)=>coefficient});
    );
    tables := new MutableList from apply(k+1,d -> new MutableHashTable);
    tables#0#zeroWeight = 1;
    for p in pairs characterTerms C do (
        mu := p#0;
        multiplicity := p#1;
        next := new MutableList from apply(k+1,d -> new MutableHashTable);
        for d from 0 to k do for old in pairs tables#d do (
            maxJ := if exterior then min(multiplicity,k-d) else k-d;
            for j from 0 to maxJ do (
                coefficient := if exterior then binomial(multiplicity,j)
                    else binomial(multiplicity+j-1,j);
                weight := old#0+j*mu;
                draftAddToTable(next#(d+j),weight,old#1*coefficient);
            );
        );
        tables = next;
    );
    formalCharacter(D,new HashTable from pairs tables#k)
)

symmetricPowerCharacter = method()
symmetricPowerCharacter (FormalCharacter,ZZ) := (C,k) ->
    draftCharacterPower(C,k,false,"symmetricPowerCharacter")
symmetricPowerCharacter (ZZ,FormalCharacter) := (k,C) -> symmetricPowerCharacter(C,k)

exteriorPowerCharacter = method()
exteriorPowerCharacter (FormalCharacter,ZZ) := (C,k) ->
    draftCharacterPower(C,k,true,"exteriorPowerCharacter")
exteriorPowerCharacter (ZZ,FormalCharacter) := (k,C) -> exteriorPowerCharacter(C,k)

symmetricPowerDecomposition = method()
symmetricPowerDecomposition (FormalCharacter,ZZ) := (C,k) ->
    decomposeCharacter symmetricPowerCharacter(C,k)

exteriorPowerDecomposition = method()
exteriorPowerDecomposition (FormalCharacter,ZZ) := (C,k) ->
    decomposeCharacter exteriorPowerCharacter(C,k)

-- In simple-coroot coordinates, the sum c of all positive coroots is
-- characterized by <alpha_j,c>=2 for every simple root alpha_j.  Thus
-- c^T=2*(1,...,1)*A^(-1).  This is 2*rho^vee, obtained without enumerating
-- roots or Weyl elements.  Its integral coordinates depend only on A.
draftPositiveCorootSum = A -> (
    cacheName := "KacMoodyCharacterOperations.positiveCorootSum";
    if A.cache#?cacheName then return A.cache#cacheName;
    r := numrows A;
    coefficients := if r == 0 then {} else
        apply(flatten entries(matrix(QQ,{toList(r:2)}) * inverse(A ** QQ)),
            x -> (
                if denominator x != 1 then
                    error "frobeniusSchurIndicator: positive-coroot sum is not integral";
                numerator x
            ));
    A.cache#cacheName = coefficients;
    coefficients
)

frobeniusSchurIndicator = method()
frobeniusSchurIndicator (RootDatum,KMWeight) := (D,lambda) -> (
    if parentDatum lambda =!= D then error "frobeniusSchurIndicator: weight has the wrong parent";
    A := finiteDraftRequireFinite(datumCartanMatrix D,"frobeniusSchurIndicator");
    labels := datumCoordinates lambda;
    if not all(labels,a -> instance(a,ZZ) and a >= 0) then
        error "frobeniusSchurIndicator: highest weight must be dominant integral";
    -- In a nonminimal realization, semisimple labels can be self-dual while
    -- a nonzero central character changes sign under duality.  Compare the
    -- full highest weights, not merely their Dynkin labels.  Label classes
    -- retain their previous semisimple interpretation.
    if dualWeight lambda != lambda then return 0;
    -- Tits' theorem: a self-dual irreducible has indicator
    -- (-1)^<lambda,2*rho^vee>.  The same sign applies to a reductive datum
    -- once the full central character has passed the self-duality check.
    -- Reference: Abu-Hamed--Gelaki, arXiv:0704.0165, Theorem 3.6.
    exponent := sum apply(labels,draftPositiveCorootSum A,(x,y) -> x*y);
    if exponent % 2 == 0 then 1 else -1
)
frobeniusSchurIndicator (RootDatum,List) := (D,lambda) ->
    frobeniusSchurIndicator(D,kmWeight(D,lambda))
frobeniusSchurIndicator (Matrix,List) := (A,lambda) ->
    frobeniusSchurIndicator(rootDatum A,lambda)

representationDynkinIndex = method()
representationDynkinIndex (Matrix,List) := (A,lambda) -> (
    A = finiteDraftRequireFinite(A,"representationDynkinIndex");
    if #((cartanDatum A)#"components") != 1 then
        error concatenate(
            "representationDynkinIndex: connected finite type is required; ",
            "a semisimple product has one trace-form index for each simple factor");
    labels := finiteDraftDynkinWeight(A,lambda,"representationDynkinIndex",true);
    dimV := weylDimension(A,labels);
    dimG := numrows A + 2*numrows cartanPositiveRoots A;
    dimV*quadraticCasimir(A,labels)/dimG
)
representationDynkinIndex (RootDatum,KMWeight) := (D,lambda) -> (
    if parentDatum lambda =!= D then error "representationDynkinIndex: weight has the wrong parent";
    representationDynkinIndex(datumCartanMatrix D,datumCoordinates lambda)
)


-------------------------------------------------------------------------
-- Infrastructure
-------------------------------------------------------------------------

-- Cache visibility and lifecycle for auxiliary infrastructure.

infrastructureCacheInfo = () -> new HashTable from {
    "affineStructure" => affineStructureCacheInfo(),
    "datumMatrices" => datumInfrastructureCacheSize(),
    "weightNotation" => draftNotationCacheInfo(),
    "kazhdanLusztigData" => categoryOInfrastructureCacheSize(),
    "crystalData" => crystalInfrastructureCacheSize(),
    "vermaPartitions" => formalCharacterInfrastructureCacheSize(),
    "characterParents" => draftCharacterParentCacheSize(),
    "scope" => "process-local caches keyed by immutable Cartan data",
    "note" => "matrix-level root and representation caches are reported by rootSnapshotCacheInfo and representationCacheInfo"
}

clearInfrastructureCaches = () -> (
    clearAffineStructureCache();
    clearDatumInfrastructureCache();
    draftClearNotationCache();
    clearCategoryOInfrastructureCache();
    clearCrystalInfrastructureCache();
    clearFormalCharacterInfrastructureCache();
    null
)


-- Label-aware Dynkin diagrams, following the root-data and finite-structure
-- definitions and the package-wide NodeFormat conventions.

dynkinDraftSafeBareLabel = s -> (
    if not instance(s,String) or #s == 0 then return false;
    codes := ascii s;
    firstCode := first codes;
    firstSafe := (firstCode >= 65 and firstCode <= 90)
        or (firstCode >= 97 and firstCode <= 122)
        or firstCode == 95;
    firstSafe and all(drop(codes,1),c ->
        (c >= 65 and c <= 90)
        or (c >= 97 and c <= 122)
        or (c >= 48 and c <= 57)
        or c == 95)
);

-- Common identifier-like string labels remain pleasant to read.  Quotes are
-- retained whenever they carry information or prevent punctuation inside a
-- label from masquerading as part of the diagram.
dynkinDraftLabelText = x -> (
    if instance(x,String) and dynkinDraftSafeBareLabel x then x
    else toExternalString x
);

dynkinDraftTypedNodeFormat = (D,format,who) -> (
    C := cartanDatum D;
    labels := C#"nodeLabels";
    positions := toList(1..#labels);
    -- A typed diagram with genuinely custom labels displays positions and
    -- labels together by default.  The picture can therefore be copied
    -- directly into position-taking APIs without asking the reader to
    -- reconstruct the label-to-position map.
    fmt := if format === null then
        (if labels === positions then "Positions" else "Records")
        else format;
    if fmt =!= "Positions" and fmt =!= "Labels" and fmt =!= "Records" then
        error(who |
            ": NodeFormat must be \"Positions\", \"Labels\", or \"Records\"");
    fmt
);

dynkinDraftNodeRecord = (labels,i) -> new HashTable from {
    "position" => i,
    "label" => labels#(i-1)
};

dynkinDraftFormattedNode = (labels,i,format) -> (
    if format === "Positions" then i
    else if format === "Labels" then labels#(i-1)
    else dynkinDraftNodeRecord(labels,i)
);

dynkinDraftTypedDiagramData = (D,format) -> (
    C := cartanDatum D;
    A := datumCartanMatrix C;
    raw := dynkinDiagramData A;
    labels := C#"nodeLabels";
    fmt := dynkinDraftTypedNodeFormat(C,format,"dynkinDiagramData");
    positions := raw#"nodes";
    records := apply(positions,i -> dynkinDraftNodeRecord(labels,i));
    recordEndpoint := i -> records#(i-1);
    labeledEdges := apply(raw#"edges",e ->
        {labels#(e#0-1),labels#(e#1-1),e#2,e#3});
    recordEdges := apply(raw#"edges",e ->
        {recordEndpoint(e#0),recordEndpoint(e#1),e#2,e#3});
    endpoint := i -> dynkinDraftFormattedNode(labels,i,fmt);
    formattedEdges := apply(raw#"edges",e ->
        {endpoint(e#0),endpoint(e#1),e#2,e#3});
    new HashTable from {
        "nodes" => apply(positions,endpoint),
        "edges" => formattedEdges,
        "rank" => raw#"rank",
        "nodeFormat" => fmt,
        "positions" => positions,
        "nodeLabels" => labels,
        "nodeRecords" => records,
        "labeledEdges" => labeledEdges,
        "recordEdges" => recordEdges,
        "positionEdges" => raw#"edges",
        "cartanDatum" => C
    }
);

dynkinDiagramData CartanDatum := opts -> D ->
    dynkinDraftTypedDiagramData(D,opts.NodeFormat);
dynkinDiagramData Realization := opts -> R ->
    dynkinDraftTypedDiagramData(cartanDatum R,opts.NodeFormat);
dynkinDiagramData RootDatum := opts -> D ->
    dynkinDraftTypedDiagramData(cartanDatum D,opts.NodeFormat);

dynkinDraftTypedDiagram = (D,format) -> (
    C := cartanDatum D;
    A := datumCartanMatrix C;
    labels := C#"nodeLabels";
    fmt := dynkinDraftTypedNodeFormat(C,format,"dynkinDiagram");
    nodeStrings := apply(toList(1..#labels),i ->
        if fmt === "Positions" then toString i
        else if fmt === "Labels" then dynkinDraftLabelText(labels#(i-1))
        else toString i | "[" |
            dynkinDraftLabelText(labels#(i-1)) | "]");
    standardLabels := labels === toList(1..#labels);
    dynkinDraftDiagram(A,nodeStrings,
        fmt =!= "Positions" and not standardLabels)
);

dynkinDiagram CartanDatum := opts -> D ->
    dynkinDraftTypedDiagram(D,opts.NodeFormat);
dynkinDiagram Realization := opts -> R ->
    dynkinDraftTypedDiagram(cartanDatum R,opts.NodeFormat);
dynkinDiagram RootDatum := opts -> D ->
    dynkinDraftTypedDiagram(cartanDatum D,opts.NodeFormat);


------------------------------------------------------------
-- Bridges to other Macaulay2 Lie-theory packages
------------------------------------------------------------

-- toLieAlgebra(C) : convert a Cartan matrix from this package into a
-- LieAlgebra object of the LieAlgebraRepresentations package, so the
-- user gets access to character, fusionProduct, branchingRule, etc.
-- The dependency is lazy: we only load the package on first call.
-- Note: LieAlgebraRepresentations and WeylGroups are NOT auto-loaded
-- because their exports (simpleRoots, positiveRoots, highestRoot,
-- rootSystem, dynkinDiagram, dynkinType, cartanMatrix,
-- dualCoxeterNumber, allRoots) collide with our own.  We lazy-load
-- them on demand and reach into their private dictionaries to fetch
-- the needed symbols.
toLieAlgebra = method()
toLieAlgebra Matrix := C -> (
    C = integralCartanMatrix(C, "toLieAlgebra");
    if cartanType C =!= "finite" then
        error "toLieAlgebra: LieAlgebraRepresentations only supports finite-type Cartan matrices";
    try needsPackage "LieAlgebraRepresentations" else
        error "toLieAlgebra: could not load package LieAlgebraRepresentations";
    LA := value (value "LieAlgebraRepresentations")#"private dictionary"#"LieAlgebra";
    new LA from C
);
toLieAlgebra CartanDatum := D -> toLieAlgebra datumCartanMatrix D
toLieAlgebra Realization := R -> toLieAlgebra datumCartanMatrix R
toLieAlgebra RootDatum := D -> toLieAlgebra datumCartanMatrix D

-- toRootSystem(C) : convert to a WeylGroups RootSystem object.
-- WeylGroups' rootSystem accepts a DynkinType (not a Cartan matrix),
-- so we classify C against the standard constructors and build the
-- corresponding DynkinType tag.  Connected components and arbitrary node
-- orderings are classified graph-theoretically before the tag is built.
toRootSystem = method()
toRootSystem Matrix := C -> (
    C = integralCartanMatrix(C, "toRootSystem");
    if cartanType C =!= "finite" then
        error "toRootSystem: WeylGroups only supports finite-type root systems";
    tags := for comp in leviConnectedComponents0 C list (
        component := submatrix(C, comp, comp);
        info := classifyConnectedCartan component;
        if info#0 === "other" then error concatenate(
            "toRootSystem: could not classify the finite component on nodes ",
            toString apply(comp, i -> i+1));
        {info#0, info#1}
    );
    try needsPackage "WeylGroups" else
        error "toRootSystem: could not load package WeylGroups";
    WGdict    := (value "WeylGroups")#"private dictionary";
    dynTypeFn := value WGdict#"dynkinType";
    rootSysFn := value WGdict#"rootSystem";
    rootSysFn dynTypeFn tags
);
toRootSystem CartanDatum := D -> toRootSystem datumCartanMatrix D
toRootSystem Realization := R -> toRootSystem datumCartanMatrix R
toRootSystem RootDatum := D -> toRootSystem datumCartanMatrix D

------------------------------------------------------------
------------------------------------------------------------

-- Documentation and tests are kept separate from the implementation.
beginDocumentation()
load (currentFileDirectory | "KacMoody/Documentation.m2")
load (currentFileDirectory | "KacMoody/Tests.m2")

end
