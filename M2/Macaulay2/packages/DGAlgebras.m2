-*- coding: utf-8 -*-
newPackage("DGAlgebras",
     Headline => "Data type for DG algebras",
     Version => "2.0",
     Date => "April 27, 2026",
     Authors => {
	  {Name => "Frank Moore",
	   HomePage => "http://www.math.wfu.edu/Faculty/Moore.html",
	   Email => "moorewf@wfu.edu"},
	  {Name => "Keller VandeBogert",
	   HomePage => "https://sites.google.com/view/kellervandebogert/home",
	   Email => "keller.v@uky.edu"}
       },
     Keywords => {"Commutative Algebra"},
     DebuggingMode => false,
     PackageExports => {"IntegralClosure", "OldChainComplexes", "Complexes"},
     AuxiliaryFiles => true
     )
export {"DGAlgebra", 
        "DGAlgebraMap", 
	"dgAlgebraMap", 
	"freeDGAlgebra", 
	"setDiff", 
	"natural", 
	"cycles",
	"getBasis", 
	"koszulComplexDGA",
	"acyclicClosure",
	"minimalModel",
	"toComplex", 
	"toComplexMap", 
	"liftToDGMap",
        "killCycles", 
	"getGenerators", 
	"adjoinVariables", 
	"deviations", 
	"deviationsToPoincare", 
	"expandGeomSeries", 
	"zerothHomology",
        "torMap", 
	"homologyAlgebra", 
	"torAlgebra", 
	"maxDegree", 
	"StartDegree", 
	"EndDegree", 
	"ringMap",
	"isHomologyAlgebraTrivial", 
	"findTrivialMasseyOperation", 
	"findNaryTrivialMasseyOperation", 
	"AssertWellDefined",
	"isGolod", 
	"isGolodHomomorphism", 
	"GenDegreeLimit", 
	"RelDegreeLimit", 
	"TMOLimit",
	"InitializeDegreeZeroHomology", 
	"InitializeComplex", 
	"isAcyclic", 
	"getDegNModule",
	"masseyTripleProduct",
	"getBoundaryPreimage",
	 "homologyClass",
	"homologyModule",
	"dgAlgebraMultMap",
	"displayBlockDiff",
        "blockDiff",
	-- v1.2 overhaul: accessor / invariant layer
	"underlyingRing",
	"underlyingAlgebra",
	"differential",
	"generatorDegrees",
	"isValidDGAlgebra",
	"isWellDefinedDifferential",
	"ensureDGAlgebraCaches",
	"invalidateDGAlgebraCache",
	-- v1.2 overhaul: ring-manipulation operations
	"baseChange",
	"subDGAlgebra",
	"truncateGenerators",
	"restrictDifferential",
	"killHomologyAtDegree",
	-- v1.2 overhaul: now-public differential helpers
	"polyDifferential",
	"polyDiffMonomial",
	-- v1.2 overhaul: lazy complex accessor
	"dgComplex",
	-- v2: DGModule type and constructors
	"DGModule",
	"koszulComplexDGM",
	"moduleDifferential",
	"isValidDGModule",
	"freeDGModule",
	"adjoinGenerators",
	"semifreeResolution",
	"minimalSemifreeResolution",
	"isMinimalSemifreeResolution",
	"generatorTable",
	"dgModuleSummary",
	"moduleBlockDiff",
	"displayModuleBlockDiff",
	-- v2: DGModuleMap type and operations
	"DGModuleMap",
	"dgModuleMap",
	"identityDGModuleMap",
	"zeroDGModuleMap",
	"liftToDGModuleMap",
	-- v2: DGAlgebraMap additions
	"identityDGAlgebraMap",
	-- v2: tensor-product infrastructure
	"tensorFactors",
	"tensorInclusions",
	-- v2: sub/quotient types + image/kernel/cokernel of DGModuleMap
	"DGIdeal",
	"dgIdeal",
	"DGSubmodule",
	"dgSubmodule",
	"DGQuotientModule",
	"dgQuotientModule",
	"isDGIdeal",
	"isDGSubmodule",
	-- Accessor methods for the new types (also match the symbol keys used
	-- internally, so `S.inclusion`, `Q.subDGModule`, etc. resolve to the
	-- same symbol in user scope as in package scope).
	"inclusion",
	"subDGModule",
	"projection",
	"dgAlgebra",
	-- v2: basic module-like operations on DG modules
	"isFreeDGModule",
	"isZero"
	-- isQuasiIsomorphism is re-exported from Complexes; we add a method for DGModuleMap.
	-- image / kernel / cokernel on DGModuleMap extend the core generics.
	-- A / I (DGAlgebra / DGIdeal), M / S (DGModule / DGSubmodule) use `symbol /`.
}

-- Questions:
-- is there a way to present graded pieces of graded A-modules as modules over A_0?
-- is there a way to make f act like a function for f a DGAlgebraMap?

-- Things to do before version 2
-- [user v1.5] Change toComplex to complex as per conversation with Dan on the M2 Google group (9/14/2010)
-- [functionality v1.5] Single degree homology(DGAlgebraMap,ZZ)
-- [functionality v1.5] Lift a map from a semifree DGA to another DGA along a quism
-- [functionality v1.5] Minimal Models
-- [functionality v1.5] Fix setDiff preparation preferences (symbols, etc)
-- [functionality v2] Present a degree of the homology algebra as a module over H_0(A) *using the monomial basis* of HA as generators
--                    In fact, one should do the following:  Let R be a ring, A a f.g. graded R-algebra with A_0 = R, and M a f.g. graded A-module.
--                                                           Is it possible to compute a presentation of M_i as an R-module (graded, if R is)?
-- [functionality v2] Allow non-polynomial underlying algebras (Major undertaking.  Would have to test extensively.)
--                    [--- In order to do this, the 'basis' command needs to be a bit more robust; things have changed in 1.4 but the package does not use the new basis yet. ---]
-- [functionality v2] Koszul Models and other things with Alex's results regarding Koszul Models (requires non-polynomial underlying algebras)
-- [functionality v2] A check that the algebra, together with the differential, is indeed a DG Algebra (up to a certain degree...)
-- [functionality v2] Update isWellDefined for DGAlgebraMaps between non-polynomial DGAs
-- [functionality v2] taylorResolutionDGA - Needs non-polynomial underlying algebra
-- [functionality v2] ekResolutionDGA - Needs non-polynomial underlying algebra
-- [functionality v2] Gorenstein pdim3 Pfaffians resolution (pg 15 of green book) - needs non-polynomial underlying algebra
-- [functionality v2] isSemiFree
-- [functionality v2] DGIdeals
-- [functionality v2] DGModules
-- [functionality v2] DGModuleMap
-- [functionality v2] torDelta - is this possible?  Would be great.
-- [functionality v2] Golod/Levin/Avramov index? (see paper of Liana)
-- [functionality v2] Matrix Massey products? (see Gulliksen-Levin)
-- [functionality v2] Computing Betti numbers using Massey products? (Ref?)

-- Not sure if the below are possible
-- [functionality v?] torModule - is this possible? 
-- [functionality v?] incorporate divided powers somehow?
-- [functionality v?] Compute obstructions for the existence of a minimal DG Algebra resolution (see pg 31 of the green book) (req. torModule)

-----------------------------------------------------
-- Set DG algebra types and constructor functions. -- 
-----------------------------------------------------

-- protected symbols (I don't want to export these symbols, but I use them internally in the code at various places)
protect diffs
protect basisAlgebra
-- dgAlgebra is now exported as an accessor (auto-protected); do not re-protect here
protect prunedComplex

-- Defining the new type DGAlgebra
DGAlgebra = new Type of MutableHashTable
globalAssignment DGAlgebra
--DGAlgebraMap still in development
DGAlgebraMap = new Type of MutableHashTable
globalAssignment DGAlgebraMap
-- [functionality v2] DGModule: a DG module over a DGAlgebra.
-- Storage mirrors DGAlgebra:
--   M.dgAlgebra  -- the DGAlgebra A
--   M.ring       -- A.ring (convenience copy)
--   M.natural    -- the underlying graded A.natural-module
--   M.Degrees    -- list of generator homological-degree vectors
--   M.diff       -- list of images of the module generators under d_M,
--                  stored as elements of M.natural (one per generator);
--                  parallels A.diff's "images on gens" convention.
--   M.cache      -- MutableHashTable with M.cache.diffs#n caching the
--                  R-linear differential at hom-degree n.
-- Constructors (koszulComplexDGM) may attach additional fields such as
-- M.module (the underlying R-module) for reference.
DGModule = new Type of MutableHashTable
globalAssignment DGModule

-- DGModuleMap: a morphism of DG modules over a common DG algebra A.
-- Storage:
--   f.source     -- DGModule M (the domain)
--   f.target     -- DGModule N (the codomain)
--   f.dgAlgebra  -- the shared DGAlgebra A
--   f.natural    -- Matrix N.natural <- M.natural; the i-th column
--                   holds the image of the i-th natural generator of M,
--                   expressed as an element of N.natural.
--   f.cache      -- CacheTable (holds e.g. per-degree toComplexMap pieces).
-- Constructors (see dgModuleMap): accept either a full Matrix N.natural <-
-- M.natural or a list of image Vectors (one per M-generator).
DGModuleMap = new Type of MutableHashTable
globalAssignment DGModuleMap

-- this command is in the core, but we need it here.
-- spots  = C -> select(keys C, i -> class i === ZZ)

-- Return the underlying Symbol of a variable-like object (Symbol,
-- IndexedVariable, or a RingElement that is a single generator).
-- Note: baseName on an IndexedVariable is the identity, so we must peel
-- into its first slot to reach the Symbol.
baseSymbolOf = v -> (
   s := baseName v;
   if instance(s, IndexedVariable) then s = s#0;
   s
)

-- Build a list of doubly-indexed generator symbols base_(i,j), where i is
-- the homological degree (first entry of the corresponding degree vector)
-- and j is a running count of generators with that homological degree.
-- startingCounts is an (optionally empty) HashTable mapping a homological
-- degree to the number of generators already present at that degree;
-- numbering continues past those counts.
makeDoubleIndexSymbols = (baseSym, degList, startingCounts) -> (
   counters := new MutableHashTable;
   scan(keys startingCounts, k -> counters#k = startingCounts#k);
   apply(degList, d -> (
      i := first d;
      if not counters#?i then counters#i = 0;
      counters#i = counters#i + 1;
      baseSym_(i, counters#i)
   ))
)

-- Modify the standard output for a DGAlgebra
net DGAlgebra := A -> (
   local diffList;
   myOutput := {net "Ring => " | net A.ring};
   myOutput = myOutput | {net "Underlying algebra => " | net A.natural};
   if A.diff =!= {} then diffList = take(flatten entries matrix (A.diff),numgens A.natural);
   myOutput = myOutput | {net "Differential => " | net diffList};
   -- Take out this part of the output.
   --myOutput = myOutput | {net "isHomogeneous => " | net A.isHomogeneous};
   horizontalJoin flatten ("{", stack myOutput, "}")
)

freeDGAlgebra = method(TypicalValue => DGAlgebra, Options => {Variable => "T"})
freeDGAlgebra (Ring,List) := opts -> (R,degList) -> (
   -- Input:  A ring, a list of degrees of the variables, and a list that defines the differential
   -- Output:  A hash table of type DGAlgebra
   A := new MutableHashTable;
   A#(symbol ring) = R;
   -- Build the generator names. Two modes, determined by the Variable option:
   --   Variable => <String or Symbol>    (default "T")
   --     Auto-generate doubly-indexed names base_(i,j), where i is the
   --     homological degree (first entry of a degree vector) and j is the
   --     running count of generators at that homological degree.
   --   Variable => <List>
   --     Use the supplied symbols verbatim (Strings are promoted via
   --     getSymbol). Length must match #degList. This is the mode used by
   --     adjoinVariables / acyclicClosure to preserve existing gen identity.
   varsList := if instance(opts.Variable, VisibleList) then (
      if #opts.Variable != #degList then
         error("freeDGAlgebra: Variable list has " | toString(#opts.Variable)
            | " entries but degree list has " | toString(#degList) | ".");
      apply(toList opts.Variable, v -> if instance(v, String) then getSymbol v else v)
   ) else (
      baseSym := if instance(opts.Variable, Symbol) then opts.Variable
                 else getSymbol(toString opts.Variable);
      makeDoubleIndexSymbols(baseSym, degList, hashTable{})
   );
   -- Symbol-safety: detect collisions between the DG generator names and
   -- existing variables in R. Previously silent; surfaced as bizarre
   -- downstream errors (wrong differentials, stale substitutions).
   if #varsList > 0 then (
      existingSymbols := set apply(gens R, g -> toString g);
      collisions := select(varsList, v -> member(toString v, existingSymbols));
      if #collisions > 0 then
         error ("freeDGAlgebra: DG generator name(s) " | toString collisions
            | " collide with variable(s) already in the base ring. "
            | "Pass Variable => \"<fresh base name>\" (or an explicit list of symbols) to freeDGAlgebra / koszulComplexDGA to avoid the clash.");
   );
   A#(symbol diff) = {};
   -- Handle empty degree list: build a trivial polynomial ring over R with no T-generators.
   if #degList == 0 then (
      A#(symbol natural) = R[varsList, Join => false];
   )
   else if isHomogeneous R then (
      -- make sure the degree list has the right form.
      if #(first degList) != #(first degrees A.ring) + 1 then degList = apply(degList, i -> i | {0});
      A#(symbol natural) = (A.ring)[varsList, Degrees => degList, Join => false, SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
   )
   else (
      A#(symbol natural) = (A.ring)[varsList, Degrees => degList, SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
   );
   A#(symbol isHomogeneous) = false;
   A.natural.cache = new CacheTable;
   if #degList == 0 then
      A.natural.cache#(symbol basisAlgebra) = (A.ring)[varsList, Join => false, DegreeMap => (d -> {0}), MonomialOrder => GRevLex]
   else
      A.natural.cache#(symbol basisAlgebra) = (A.ring)[varsList, Join => false, DegreeMap => (d -> {0}), MonomialOrder => GRevLex, Degrees => apply(degList, i -> {first i}), SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
   use A.natural;
   A#(symbol Degrees) = degList;
   A#(symbol cache) = new CacheTable;
   A.cache#(symbol homology) = new MutableHashTable;
   A.cache#(symbol homologyAlgebra) = new MutableHashTable;
   A.cache#(symbol diffs) = new MutableHashTable;
   -- *should* verify that the differential is indeed of degree -1
   new DGAlgebra from A
)     

maxDegree = method(TypicalValue => ZZ)
maxDegree DGAlgebra := A -> (
   degreesList := degrees A.natural / first;
   if any(degreesList, i -> even i) then infinity else sum degreesList
)

totalOddDegree := A -> sum select(degrees A.natural / first, i -> odd i)

setDiff = method(TypicalValue => DGAlgebra, Options => {InitializeDegreeZeroHomology => true, InitializeComplex => true})
setDiff (DGAlgebra,List) := opts -> (A,diffList) -> (
   ensureDGAlgebraCaches A;
   invalidateDGAlgebraCache A;
   if #diffList == 0 then
      A.diff = map(A.natural, A.natural, {})
   else
      A.diff = map(A.natural,A.natural, substitute(matrix {diffList}, A.natural));
   A.isHomogeneous = isHomogeneous A.ring and checkIsHomogeneous(A);
   if opts.InitializeDegreeZeroHomology then (
      if #diffList == 0 then A#(symbol zerothHomology) = A.ring
      else (
         definingIdeal := ideal mingens (ideal A.ring + sub(ideal polyDifferential(1,A), ambient A.ring));
         if definingIdeal == ideal vars ambient A.ring then A#(symbol zerothHomology) = coefficientRing A.ring else A#(symbol zerothHomology) = (ambient A.ring)/definingIdeal;
      );
   );
   if opts.InitializeComplex and #diffList > 0 then A.dd = (toComplex(A,totalOddDegree(A)+1)).dd;
   A
)

setKoszulDiff = method(TypicalValue => DGAlgebra, Options => {InitializeDegreeZeroHomology => true, InitializeComplex => true})
setKoszulDiff (DGAlgebra,List) := opts -> (A,diffList) -> (
   ensureDGAlgebraCaches A;
   invalidateDGAlgebraCache A;
   A.diff = map(A.natural,A.natural, substitute(matrix {diffList}, A.natural));
   A.isHomogeneous = isHomogeneous A.ring and checkIsHomogeneous(A);
   if opts.InitializeDegreeZeroHomology then (
      definingIdeal := ideal mingens (ideal A.ring + sub(ideal polyDifferential(1,A), ambient A.ring));
      if definingIdeal == ideal vars ambient A.ring then A#(symbol zerothHomology) = coefficientRing A.ring else A#(symbol zerothHomology) = (ambient A.ring)/definingIdeal;
   );
   if opts.InitializeComplex then A.dd = (koszul(matrix{diffList})).dd;
   A
)

checkIsHomogeneous = method()
checkIsHomogeneous DGAlgebra := A -> (
   gensList := gens A.natural;
   if #gensList == 0 then return true;  -- trivially homogeneous
   diffList := apply(gensList, f -> A.diff(f));
   homDegreeShift := {1} | (toList ((#(degree first gensList)-1):0));
   all(#diffList, i -> diffList#i == 0 or degree gensList#i - homDegreeShift == degree diffList#i)
)

-- cache the basis of a DGAlgebra?
getBasis = method(TypicalValue => Matrix, Options => {Limit => -1})
getBasis (ZZ,DGAlgebra) := opts -> (homDegree,A) -> getBasis(homDegree,A.natural, Limit => opts.Limit)

getBasis (ZZ,Ring) := opts -> (homDegree,R) -> (
   local retVal;
   myMap := map(R, R.cache.basisAlgebra);
   tempList := (flatten entries basis(homDegree, R.cache.basisAlgebra, Limit => opts.Limit, Variables => 0 .. numgens R-1)) / myMap;
   if tempList == {} then retVal = map((R)^1,(R)^0, 0) else
   (
      -- move this to an assert?
      tempList = reverse sort tempList;
      degList := apply(tempList, m -> -degree m);
      retVal = map(R^1, R^degList, matrix {tempList});
   );
   retVal
)

isHomogeneous DGAlgebra := A -> A.isHomogeneous

isAcyclic = method(TypicalValue => Boolean, Options => {EndDegree => -1})
isAcyclic DGAlgebra := opts -> A -> (
  endDegree := maxDegree A;
  if endDegree == infinity and opts.EndDegree == -1 then
    error "Must supply an upper bound to check for acyclicity.";
  if opts.EndDegree != -1 then endDegree = opts.EndDegree;
  not any(1..endDegree,i -> prune HH_i(A) != 0)
)

koszulComplexDGA = method(TypicalValue => DGAlgebra, Options => {Variable => "T"})
koszulComplexDGA Ring := opts -> R -> (
   local A;
   local initComplex;
   --initComplex = (numgens R < 8);
   initComplex = true;
   if isHomogeneous R then (
      degList := apply(degrees R, i -> {1} | i);
      A = freeDGAlgebra(R, degList, opts);
      use A.ring;
      setKoszulDiff(A, gens R, InitializeComplex=>initComplex);
   )
   else (
      A = freeDGAlgebra(R, toList ((numgens R):{1}),opts);
      use A.ring;
      setKoszulDiff(A, gens R, InitializeComplex=>initComplex);
   );
   A
)

koszulComplexDGA Ideal := opts -> I -> (
   local A;
   if isHomogeneous I then (
      degList := apply(flatten entries gens I, i -> {1} | degree i); 
      A = freeDGAlgebra(ring I, degList, opts);
      use A.ring;
      setDiff(A,I_*);
   )
   else
   (
      A = freeDGAlgebra(ring I, toList ((numgens I):{1}), opts);
      use A.ring;
      setDiff(A, I_*);
   );
   A
)

koszulComplexDGA List := opts -> ringElts -> koszulComplexDGA(ideal ringElts, opts);

toComplex = method(TypicalValue=>Complex)
toComplex DGAlgebra := A -> (
   maxDeg := maxDegree A;
   if maxDeg == infinity then error "Must specify an upper degree bound if an even generator exists.";
   toComplex(A,maxDeg)
)

toComplex (DGAlgebra,ZZ) := (A,n) -> complex(apply(n, i -> polyDifferential(i+1,A)))

killCycles = method(TypicalValue=>DGAlgebra,Options => {StartDegree => 1, EndDegree => -1, Variable => null})
killCycles DGAlgebra := opts -> A -> (
   -- for now, this will only work for DG algebras with H_0(A) = k
   retVal := 0;
   endDegree := if opts.EndDegree == -1 then opts.StartDegree else opts.EndDegree;
   if opts.StartDegree > endDegree then error "Starting degree is not less than or equal to ending degree.";
   n := opts.StartDegree;
   foundHomology := false;
   if opts.StartDegree > endDegree then error "Starting degree is not less than or equal to ending degree.";
   n = opts.StartDegree;
   foundHomology = false;
   nthHomology := 0;
   while n <= endDegree and not foundHomology do (
      nthHomology = prune homology(n,A);
      if nthHomology == 0 then n = n + 1 else foundHomology = true
   );
   -- at this point we have found a degree with nontrivial homology.
   -- we now add variables in one degree higher to make these cycles boundaries.
   if not foundHomology then retVal = A else (  
      homologyGenerators := entries transpose gens image (nthHomology.cache.pruningMap);
      basisList := flatten entries getBasis(n,A);
      cycleList := apply(homologyGenerators, gen -> sum apply(#gen, i -> gen#i*basisList#i));
      retVal = adjoinVariables(A, cycleList, Variable => opts.Variable);
   );
   retVal
)

adjoinVariables = method(TypicalValue=>DGAlgebra, Options => {Variable => null})
adjoinVariables (DGAlgebra, List) := opts -> (A, cycleList) -> (
   -- Add new generators whose differentials are the given cycles, producing
   -- a new DG algebra semifree over A. Existing generator names/identities
   -- are preserved; new generators continue the doubly-indexed naming
   -- base_(i,j) using per-homological-degree counters carried over from A.
   local newDegreesList;
   local newCycleDegrees;
   tempDegree := {1} | toList ((#(degree first cycleList)-1):0);
   if A.isHomogeneous then
      newCycleDegrees = apply(cycleList, z -> degree z + tempDegree)
   else
      newCycleDegrees = apply(cycleList, z -> {first degree z + 1});
   newDegreesList = A.Degrees | newCycleDegrees;
   existingGenNames := apply(gens A.natural, baseName);
   baseSymForNew := if opts.Variable === null then (
      if #(gens A.natural) > 0 then baseSymbolOf first gens A.natural
      else getSymbol "T"
   ) else if instance(opts.Variable, Symbol) then opts.Variable
   else getSymbol toString opts.Variable;
   startingCounts := new MutableHashTable;
   scan(A.Degrees, d -> (
      i := first d;
      if not startingCounts#?i then startingCounts#i = 0;
      startingCounts#i = startingCounts#i + 1;
   ));
   newGenNames := makeDoubleIndexSymbols(baseSymForNew, newCycleDegrees,
      hashTable pairs startingCounts);
   allNames := existingGenNames | newGenNames;
   B := freeDGAlgebra(A.ring, newDegreesList, Variable => allNames);
   phi := map(B.natural, A.natural, matrix {take(gens B.natural, numgens A.natural)});
   newDiffList := (take(flatten entries matrix A.diff, numgens A.natural) | cycleList) / phi;
   setDiff(B, newDiffList, InitializeComplex=>false);
   B
)

acyclicClosure = method(TypicalValue=>DGAlgebra, Options => {StartDegree => 1, EndDegree => 3, Variable => null})
acyclicClosure DGAlgebra := opts -> A -> (
  n := opts.StartDegree;
  endDegree := 3;
  if opts.EndDegree != 3 then endDegree = opts.EndDegree;
  while n <= endDegree do (
     A = killCycles(A, StartDegree => n, Variable => opts.Variable);
     n = n + 1;
  );
  A
)

acyclicClosure Ring := opts -> R -> (
   varName := if opts.Variable === null then "T" else opts.Variable;
   K := koszulComplexDGA(R, Variable => varName);
   acyclicClosure(K, StartDegree => opts.StartDegree, EndDegree => opts.EndDegree, Variable => opts.Variable)
)

-- minimalModel: build the minimal DG algebra resolution of a quotient ring
-- R = Q/I over the polynomial ring Q.  For an Ideal I, this is
-- acyclicClosure(koszulComplexDGA I) -- a DG algebra over ring I whose
-- degree-0 homology is (ring I)/I and whose higher homology is killed
-- step by step via Tate's construction.  For a QuotientRing R, we use
-- the defining ideal of R inside its ambient polynomial ring.
minimalModel = method(TypicalValue => DGAlgebra,
     Options => {StartDegree => 1, EndDegree => 3, Variable => null})
minimalModel Ideal := opts -> I -> (
   varName := if opts.Variable === null then "T" else opts.Variable;
   K := koszulComplexDGA(I, Variable => varName);
   acyclicClosure(K, StartDegree => opts.StartDegree, EndDegree => opts.EndDegree, Variable => opts.Variable)
)
minimalModel QuotientRing := opts -> R -> minimalModel(ideal R, opts)

polyDiffMonomial = method()
polyDiffMonomial (DGAlgebra, RingElement) := (A,m) -> (
  -- uses the Leibniz rule to compute the differential of a traditional monomial
  if m == 0 then return 0_(A.natural);
  dgSign := 1;
  justMon := first flatten entries first coefficients m;
  justCoeff := substitute(first flatten entries last coefficients m, ring m);
  monSupport := support justMon;
  monExponents := select(first exponents justMon, i -> i > 0);
  monSupportPowers := apply(#monSupport, i -> (monSupport#i)^(monExponents#i));
  allTerms := apply(#monSupport, i -> (product apply(i, j -> (-1)^((first degree monSupport#j)*(monExponents#j))))*
                                      (product take(monSupportPowers,i))*
  -- below are both versions of differential, for divided powers and without, should we want to use divided powers later.
  --                                  (A.diff(monSupport#i)*(monSupport#i)^((monExponents#i)-1))*  -- divided powers
                                      (A.diff(monSupport#i)*(monExponents#i)*(monSupport#i)^((monExponents#i)-1))* -- polynomial subalgebra
                                      (product drop(monSupportPowers,i+1)));
  justCoeff * (sum allTerms)
)

polyDifferential = method(TypicalValue=>Matrix)
polyDifferential (ZZ,DGAlgebra) := (n,A) -> (
  local newDiffl;
  if A.cache.diffs#?n then A.cache.diffs#n
  else if n == 0 then map((A.ring)^0,(A.ring)^1,0)
  else (
     -- here, check to see if the ring A is graded with graded differential.  If so, then produce
     -- a graded map.  Otherwise, just treat things as ungraded (should be slower)
     sourceList := getBasis(n,A);
     sourceDegreeList := apply(degrees source sourceList, l -> -drop(l,1));
     sourceList = flatten entries sourceList;
     targetList := getBasis(n-1,A);
     targetDegreeList := apply(degrees source targetList, l -> -drop(l,1));
     targetList = flatten entries targetList;
     mDegree := maxDegree A;
     if (n == mDegree + 1) then newDiffl = map((A.ring)^(targetDegreeList), (A.ring)^0, 0)
     else if n > mDegree + 1 then newDiffl = map((A.ring)^0,(A.ring)^0,0)
     -- empty-basis guards: without these, coefficients() below errors on an
     -- empty monomials list. Formerly users had to override polyDifferential
     -- (see KoszulHomHelpers.m2:767-791). Fixed upstream in v1.2.
     else if #sourceList == 0 then newDiffl = map((A.ring)^(targetDegreeList), (A.ring)^0, 0)
     else if #targetList == 0 then newDiffl = map((A.ring)^0, (A.ring)^(sourceDegreeList), 0)
     else (
        diffList := matrix {apply(sourceList, m -> polyDiffMonomial(A,m))};
        coeffMatrix := substitute((coefficients(diffList, Monomials => targetList))#1, A.ring);
        newDiffl = map((A.ring)^(targetDegreeList), (A.ring)^(sourceDegreeList), coeffMatrix);
        A.cache.diffs#n = newDiffl;
     );
     newDiffl
  )
)

polyDifferential (DGAlgebra,RingElement) := (A,f) -> (
    ts := terms f;
    if #ts == 0 then 0_(A.natural)
    else sum apply(ts, m -> polyDiffMonomial(A,m))
)

diff (DGAlgebra,RingElement) := (A,f) -> polyDifferential(A,f);

homology (ZZ,DGAlgebra) := opts -> (n,A) -> (
  dn := 0;
  dnplus1 := 0;
  retVal := 0;
  if A.cache.homology#?n then retVal = A.cache.homology#n
  else if #(flatten entries getBasis(n, A, Limit => 1)) != 0 then (
     if n == 0 then dn = map((A.ring)^0, (A.ring)^1, 0) else dn = polyDifferential(n,A);
     if #(flatten entries getBasis(n+1, A, Limit => 1)) != 0 then
        dnplus1 = polyDifferential(n+1,A)
     else
        dnplus1 = map(source dn, (A.ring)^0, 0);
     retVal = homology(dn,dnplus1);
     A.cache.homology#n = retVal;
  )
  else
     retVal = (A.ring)^0;
  retVal
)

homology DGAlgebra := opts -> A -> homologyAlgebra(A)

zerothHomology = method(TypicalValue => Ring)
zerothHomology (DGAlgebra) := A -> A.zerothHomology

getDegNModule = method(TypicalValue => Module)
getDegNModule (ZZ,Ring,Ring) := (n,R,A) -> (
   -- first, set up the 'alternate' ring that forgets the grading (if any) of R from A
   local myRowDegree;
   --A := ring M;
   varsList := gens A;
   degList := apply(degrees A, i -> {first i});
   skewList := {};
   if A#?SkewCommutative then skewList = A.SkewCommutative;
   if not A#?cache then A#cache = new CacheTable;
   if not A.cache#?basisAlgebra then (
      tempRing := R[varsList, Join => false, Degrees => degList, SkewCommutative => skewList];
      A.cache#(symbol basisAlgebra) = tempRing/substitute(ideal A,tempRing);
   );
   myBasis := getBasis(n,A);
   tempMatrix := syz myBasis;
   colDegList := degrees source tempMatrix;
   rowDegList := degrees target tempMatrix;
   myCols := select(toList (0..rank source tempMatrix - 1), i -> first colDegList#i == n);
   -- Pad or truncate internal-degree vectors to match degreeLength R so that
   -- passing an ungraded R (e.g. the coefficient field from zerothHomology of
   -- a Koszul DGA on a quotient ring) still works.
   dlR := degreeLength R;
   adjustDeg := h -> (
      d := -drop(h,1);
      if #d == dlR then d
      else if #d > dlR then take(d, dlR)
      else d | toList((dlR - #d):0)
   );
   myColDegree := apply(colDegList_myCols, adjustDeg);
   tempMatrix = tempMatrix_myCols;
   myRowDegree = apply(rowDegList, adjustDeg);
   tempMatrix = map(R^myRowDegree, R^myColDegree,substitute(tempMatrix,R));
   prune coker tempMatrix
)

deviations = method(TypicalValue=>Tally,Options=>{DegreeLimit=>3})
deviations Ring := opts -> R -> (
  --tally degrees torAlgebra(R,GenDegreeLimit=>opts.DegreeLimit)
  kRes := freeResolution(coker vars R, LengthLimit => opts.DegreeLimit);
  deviations kRes
)

deviations Complex := opts -> C -> (
  R := ring C;
  pSeries := poincareN C;
  degreesR := toList ((numgens degreesRing R):0);
  if isHomogeneous R then degreesR = degrees R;
  deviations(pSeries,degreesR)
)

deviations (RingElement,List) := opts -> (pSeries,degreesR) -> (
  isHomogeneousR := any(degreesR, deg -> deg =!= 0);
  A := ring pSeries;
  homVar := first gens A;
  homDegree := degree(homVar,pSeries);
  internalVars := drop(gens A,1);
  if not isHomogeneousR then pSeries = substitute(pSeries, (first internalVars)=>1);
  subB := ZZ[internalVars];
  tempB := subB[homVar];
  B := tempB/ideal{(substitute(homVar,tempB))^(homDegree+1)};
  pSeries = substitute(pSeries,B);
  n := 1;
  tempSeries := 1_B;
  tempDiff := pSeries - tempSeries;
  returnHash := hashTable {};
  allDeviations := {};
  while (n <= homDegree and tempDiff != 0) do (
     tempCoeffs := coefficients someTerms(tempDiff,-1,1);
     tempCoeffs = (first flatten entries first tempCoeffs, substitute(first flatten entries last tempCoeffs,subB));
     -- here, tally up the deviations.
     tempDegrees := (terms last tempCoeffs) / exponents / flatten;
     leadCoeffs := (terms last tempCoeffs) / leadCoefficient;
     if isHomogeneousR then allDeviations = allDeviations | apply(#tempDegrees, i -> ((n,tempDegrees#i),leadCoeffs#i))
                       else allDeviations = allDeviations | apply(#tempDegrees, i -> (n,leadCoeffs#i));
     newSeries := 1_B;
     if even n then newSeries = product apply(terms last tempCoeffs, v -> (expandGeomSeries((leadMonomial v)*(first tempCoeffs), homDegree))^(substitute(leadCoefficient v,ZZ)))
               else newSeries = product apply(terms last tempCoeffs, v -> (1+(leadMonomial v)*(first tempCoeffs))^(substitute(leadCoefficient v,ZZ)));
     tempSeries = tempSeries*newSeries;
     n = n + 1;
     tempDiff = pSeries - tempSeries;
  );
  hashTable allDeviations
)

expandGeomSeries = method()
expandGeomSeries (RingElement,ZZ) := (f,n) -> expandGeomSeries({f},n)

expandGeomSeries (List,ZZ) := (fList,n) -> (
   A := ring first fList;
   homVar := first gens A;
   B := A/ideal (homVar^(n+1));
   phi := map(B,A);
   substitute(product apply(fList, f -> sum apply(n,i -> phi(f)^i)),A)
)

deviationsToPoincare = method(Options=>{DegreeLimit=>0})
deviationsToPoincare HashTable := opts -> devHash -> (
   S := getSymbol("S");
   T := getSymbol("T");
   pairsDevHash := pairs devHash;
   if instance(first first pairsDevHash, Sequence) then pairsDevHash = apply(pairsDevHash, p -> (p#0#0,p#0#1,p#1));
   numTVars := #(first pairsDevHash) - 3;
   tVars := toList (T_0..T_numTVars);
   n := opts.DegreeLimit;
   n = max(n,max apply(pairsDevHash, p -> first p));
   A := ZZ[S,tVars];
   B := A/ideal (first gens A)^(n+1);
   tVars = drop(gens B,1);
   powerFunc := p -> if numTVars == -1 then 1_B else product apply(#(p#1), i -> (tVars#i)^(p#1#i));
   product apply(pairsDevHash, p -> if even first p then (expandGeomSeries((first gens B)^(first p)*(powerFunc p),n))^(last p)
                                                    else (1+((first gens B)^(first p)*(powerFunc p)))^(last p))
)

torAlgebra = method(TypicalValue=>Ring, Options=>{GenDegreeLimit=>infinity,RelDegreeLimit=>infinity})
torAlgebra Ring := opts -> R -> (
  -- NOTE: This single-argument version returns the *free* graded-commutative
  -- algebra on the deviations of R. This is the true Tor algebra Tor^R(k,k)
  -- only when R is a complete intersection (Tate/Gulliksen). For a general
  -- local ring it only recovers the graded k-vector-space structure (Hilbert
  -- function); multiplicative relations are NOT computed here. For the actual
  -- multiplicative structure, use torAlgebra(R, R/ideal vars R).
  n := 3;
  if opts.GenDegreeLimit != infinity then n = opts.GenDegreeLimit;
  baseRing := coefficientRing R;
  -- precompute a partial free resolution so that deviations/... have something
  -- finite to chew on; without this, infinite resolutions hang.
  kRes := freeResolution(coker vars R, LengthLimit => n);
  X := getSymbol("X");
  RDevs := deviations(R,DegreeLimit=>opts.GenDegreeLimit);
  degreesList := sort flatten apply(pairs RDevs, p -> toList ((p#1):(flatten{p#0#0,p#0#1})));
  skewList := select(#degreesList, i -> odd first degreesList#i);
  torVars := toList(X_1..X_(#degreesList));
  polyRing := baseRing[torVars, Degrees => degreesList, SkewCommutative => skewList];
  -- set up the basisAlgebra cache so getBasis(n, T) works downstream.
  polyRing' := baseRing[torVars, Degrees => (degreesList / first), SkewCommutative => skewList];
  polyRing.cache = new CacheTable;
  polyRing.cache#(symbol basisAlgebra) = polyRing';
  polyRing
)

torAlgebra (Ring,Ring) := opts -> (R,S) -> homologyAlgebra(acyclicClosure(R,EndDegree=>opts.GenDegreeLimit) ** S, opts)

representativeCycles = (n,A) -> (
  temp := transpose generators image ((prune homology(n,A)).cache.pruningMap);
  homologyGenerators := entries temp;
  basisList := flatten entries getBasis(n,A);
  cycleList := apply(homologyGenerators, gen -> sum apply(#gen, i -> gen#i*basisList#i));
  cycleList
)

makeHomologyRing = method()
makeHomologyRing (DGAlgebra,List,List,Boolean) := (A, cycleList, relList, ForceGB) -> (
  local HA;
  local degreesList;
  baseRing := A.zerothHomology;
  if isHomogeneous A then degreesList = (cycleList / degree) else degreesList = pack((cycleList / degree) / first, 1);
  X := getSymbol("X");
  varList := toList (X_1..X_(#cycleList));
  polyRing := baseRing[varList, Join => false, Degrees => degreesList, SkewCommutative => select(toList(0..(#degreesList-1)), i -> odd first degreesList#i)];
  polyRing' := baseRing[varList, Join => false, Degrees => (degreesList / first), SkewCommutative => select(toList(0..(#degreesList-1)), i -> odd first degreesList#i)];
  if relList == {} then (
     HA = polyRing;
     HA.cache = new CacheTable;
     HA.cache#(symbol basisAlgebra) = polyRing';
  )
  else (
     I := ideal relList;
     myMap := map(polyRing, ring I, gens polyRing);
     I = myMap(I);
     -- this command causes hard crashes on occasion...
     --if ForceGB then forceGB gens I else if A.isHomogeneous then I = ideal gens gb(I,Algorithm=>Homogeneous2) else I = ideal gens gb I;
     if ForceGB then forceGB gens I else if A.isHomogeneous then I = ideal gens gb I else I = ideal gens gb I;
     HA = polyRing/I;
     --- set up the cached algebra for basis computations too
     myMap' := map(polyRing', polyRing, gens polyRing');
     I' := myMap'(I);
     if myMap'(leadTerm(gens I)) - leadTerm gens I' != 0 then error "Monomial order error.";
     forceGB gens I';
     HA.cache = new CacheTable;
     HA.cache#(symbol basisAlgebra) = polyRing'/I';
  );
  use A.natural;
  use A.ring;
  HA
)

-- This code finds the relations that exist in the homology algebra that come from simply the relations that exist
-- in the ring, not including the ones that come because one must include the boundaries in determining the relations
--findEasyRelations = method(Options => {Hilbert => null})
--findEasyRelations(DGAlgebra,List) := opts -> (A, cycleList) -> (
findEasyRelations = method()
findEasyRelations (DGAlgebra,List) := (A, cycleList) -> (
  -- need to document this code!
  -- this function should only be called (at this point) if H_0(A) is the residue field.  Not sure how to use this trick to quickly compute the easy relations
  -- unless this is the case.
  X := getSymbol("X");
  baseRing := coefficientRing A.ring;
  varsList := apply(gens A.ring | gens A.natural, f -> sub(f,A.natural)) | toList (X_1..X_(#cycleList));
  numAvars := numgens A.ring + numgens A.natural;
  naturalGens := gens A.natural;
  skewList := apply(select(toList(0..#naturalGens-1), i -> odd first degree naturalGens#i), i -> i + numgens A.ring);
  skewList = skewList | apply(select(toList(0..#cycleList-1), i -> odd first degree cycleList#i), i -> i + numgens A.natural + numgens A.ring);
  degList := apply(numgens A.natural + numgens A.ring, i -> degree varsList#i);
  degList = degList | apply(cycleList, i -> degree i);
  if (not isHomogeneous A) then degList = pack(degList / first, 1);
  monB := monoid [varsList,MonomialOrder=>{numgens A.natural + numgens A.ring,#cycleList},Degrees=>degList, SkewCommutative=>skewList];
  B := baseRing monB;
  K := substitute(ideal A.natural, B) + substitute(ideal A.ring, B) + ideal apply(#cycleList, i -> B_(numAvars+i) - substitute(cycleList#i,B));
  if A.isHomogeneous then assert(isHomogeneous K);
  easyRels := 0;
  --if isHomogeneous K then easyRels = ideal selectInSubring(1,gens gb(K,Algorithm=>Homogeneous2)) else easyRels = ideal selectInSubring(1,gens gb K);
  if isHomogeneous K then easyRels = ideal selectInSubring(1,gens gb K) else easyRels = ideal selectInSubring(1,gens gb K);
  degList = apply(cycleList, i -> degree i);
  skewList = select(toList(0..#degList-1), i -> odd first degList#i);
  C := baseRing[X_1..X_(#cycleList), Degrees => degList, SkewCommutative=>skewList];
  makeHomologyRing(A,cycleList,(sub(easyRels,C))_*,true)
)

getCycleProductMatrix = method()
getCycleProductMatrix (DGAlgebra,Ring,List,ZZ) := (A,HA,cycleList,N) -> (
  -- the input is the dga A, the homology algebra HA (so far), the list of cycle generators, and the degree.
  -- the output is a matrix over A.ring that expresses the monomials in HA as elements of A in the standard basis
  -- this version does use the knowledge of the homologyAlgebra so far to return the cycles products in a given degree.
  local retVal;
  local myMap;
  monListHA := flatten entries getBasis(N,HA);
  monListA := flatten entries getBasis(N,A);
  myMap = map(A.natural,HA,cycleList);
  -- this is needed when there is nothing in HA in this degree so far.
  if monListHA != {} then (
     (junk,cycleProductMatrix) := coefficients(myMap(matrix{monListHA}),Monomials=>monListA);
     -- make sure the degrees are correct if the ring is homogeneous
     if isHomogeneous A then (
        sourceDegreeList := apply((monListHA / degree), l -> -drop(l,1));
        targetDegreeList := apply((monListA / degree), l -> -drop(l,1));
        cycleProductMatrix = map((A.ring)^targetDegreeList, (A.ring)^sourceDegreeList, sub(cycleProductMatrix, A.ring));
     )
     else (
        myMap = map(A.ring, A.natural);
        cycleProductMatrix = map((A.ring)^(#monListA), (A.ring)^(#monListHA), sub(cycleProductMatrix, A.ring));
     );
     retVal = (cycleProductMatrix,monListHA);
  )
  else retVal = (map((A.ring)^(#monListA), (A.ring)^0,0), {});
  retVal
)

getCycleProductMatrix (DGAlgebra,List,ZZ) := (A,cycleList,N) -> getCycleProductMatrix(A,makeHomologyRing(A,cycleList,{},true),cycleList,N)

-- The goal of this function is to return the generators in degree n.
findDegNGenerators := (A,oldCycleList,N) -> (
  cycleList := {};
  relsList := {};
  varList := {};
  if oldCycleList == {} then (
     -- here, we know all the degree 1 elements are generators
     cycleList = representativeCycles(N,A);
  )
  else if flatten entries getBasis(N,A, Limit => 1) == {} then cycleList = {}
  else (
     nthHomology := homology(N,A);
     if prune nthHomology != 0 then (
	(cycleProductMatrix,monListHA) := getCycleProductMatrix(A,oldCycleList,N);
	if monListHA != {} then (
	   -- TODO: Document the below block of code.
	   newHomology := prune (nthHomology / (image cycleProductMatrix));
	   monListA := flatten entries getBasis(N,A);
           cycleList = apply(entries transpose gens image newHomology.cache.pruningMap, zList -> apply(#zList, i -> zList#i*monListA#i)) / sum;
        )
        else (
	   -- if we are here, then we need to add all of this degree as generators.
	   cycleList = representativeCycles(N,A);
	);
     ); 
  );
  cycleList
)

findDegNRelations := (A,HA,algGens,N) -> (
  -- this function tries to find the relations in degree N that involve the generators in the list algGens
  -- no checking is done to see if algGens are actually minimal generators at this point.
  local cycleProductMatrix;
  local monListHA;
  retVal := {0_HA};
  -- check if DGA is zero in this degree. If so, just return back the monomials in the given degree
  if flatten entries getBasis(N, A, Limit => 1) != {} or #algGens == 0 then (
     -- using HA, check if there are indeed any new relations in degree n
     nthHomology := homology(N,A);
     pruneNthHomology := prune nthHomology;
     rankOfNthHomology := numgens pruneNthHomology;
     rankOfAlgebraSoFar := #(flatten entries getBasis(N,HA));
     if rankOfNthHomology != rankOfAlgebraSoFar or not isField A.zerothHomology then (
       -- when in here, we know there is a relation in degree N.
       -- so take each monomial of the correct degree, build the cycle corresponding to that
       -- and define a map from the residue field to the homology class representing each cycle.
       -- then take the kernel, prune, and use cache.pruningMap to get the actual minimal generating
       -- set of the kernel.  Finally, reconstruct the elements from the monomials and viola!
       if pruneNthHomology == 0 then (
          -- if we are here, all monomials in the HA of this degree are zero.
	  retVal = flatten entries getBasis(N,HA);
       )
       else (
          (cycleProductMatrix,monListHA) = getCycleProductMatrix(A,HA,algGens,N);
          if monListHA != {} then (
             -- TODO: Document this code.
	     multMap := map(coker relations nthHomology,(A.ring)^(rank source cycleProductMatrix),cycleProductMatrix);
             kerMultMap := gens ker multMap;
	     kerMultMap = compress sub(kerMultMap,A.zerothHomology);
	     kernelGens := entries transpose kerMultMap;
	     retVal = apply(kernelGens, z -> sum apply(#z, i -> (monListHA#i)*(z#i)));
          );
       );
     );
  )
  else (
     retVal = flatten entries getBasis(N,HA); 
  );
  -- return the nonzero relations
  select(retVal, i -> i != 0)
)

getGenerators = method(TypicalValue=>List, Options => {StartDegree => 1, DegreeLimit => -1, Verbosity => 0})
getGenerators DGAlgebra := opts -> A -> (
  maxDeg := maxDegree A;
  if opts.DegreeLimit != -1 then maxDeg = opts.DegreeLimit;
  if maxDeg == infinity then error "Must specify maximum homological degree of generators.";
  n := opts.StartDegree;
  cycleList := {};
  local newCycleList;
  while n <= maxDeg do (
     if opts.Verbosity >= 2 then (
        << "Computing generators in degree " << n << " : ";
        time newCycleList = findDegNGenerators(A,cycleList,n);
     ) else newCycleList = findDegNGenerators(A,cycleList,n);
     cycleList = cycleList | newCycleList;
     n = n + 1;
  );
  cycleList
)

getRelations = method(TypicalValue=>Ring, Options => {Verbosity => 0})
getRelations (DGAlgebra,Ring,List,ZZ) := opts -> (A,HA,cycleList,relDegreeLimit) -> (
   relList := (ideal HA)_*;
   n := 1;
   local newRelList;
   while n <= relDegreeLimit do (
      if opts.Verbosity >= 2 then (
         << "Computing relations in degree " << n << "  : ";
         time newRelList = findDegNRelations(A,HA,cycleList,n);
      )
      else newRelList = findDegNRelations(A,HA,cycleList,n);
      if relList == {} then relList = newRelList
      else if newRelList != {} then (
         -- make sure newRelList and relList are in the same ring
         myMap := map(ring first relList, ring first newRelList, flatten entries vars ring first relList);
         relList = relList | (newRelList / myMap);
      );
      -- now reset HA using relList for the next iteration.
      doForceGB := (n != relDegreeLimit);
      HA = makeHomologyRing(A,cycleList,relList,doForceGB);
      n = n + 1;
   );
   -- put the cycles that the variables represent in the cache.
   HA.cache#cycles = cycleList;
   HA
)

homologyAlgebra = method(TypicalValue=>Ring,Options=>{GenDegreeLimit=>infinity,RelDegreeLimit=>infinity,Verbosity=>0})
homologyAlgebra DGAlgebra := opts -> A -> (
  local HA;
  if A.cache.homologyAlgebra#?GenDegreeLimit and A.cache.homologyAlgebra#GenDegreeLimit >= opts.GenDegreeLimit and
     A.cache.homologyAlgebra#?RelDegreeLimit and A.cache.homologyAlgebra#RelDegreeLimit >= opts.RelDegreeLimit then HA = A.cache.homologyAlgebra#homologyAlgebra else (
     maxDeg := maxDegree A;

     if maxDeg == infinity and (opts.GenDegreeLimit == infinity or opts.RelDegreeLimit == infinity) then
        return "Must supply upper degree bound on generators and relations if there is a DG algebra generator of even degree.";
     if opts.GenDegreeLimit != infinity then maxDeg = opts.GenDegreeLimit;

     cycleList := getGenerators(A,DegreeLimit=>maxDeg,Verbosity=>opts.Verbosity);

     -- Default RelDegreeLimit: relations must be searched at least up to
     -- 2 * maxGenDegree so that products of two top-degree generators are
     -- reducible. The lastNonZeroH+1 heuristic alone is unsound because
     -- generators in degree d can produce products in degree 2d that are
     -- not in H_*(A) and must be killed by relations.
     maxHomologyDegree := if opts.RelDegreeLimit != infinity then opts.RelDegreeLimit
        else (
           n := maxDeg;
           while n >= 0 and prune homology(n,A) == 0 do n = n - 1;
           lastNonZero := n;
           maxGenDeg := if cycleList == {} then 0 else max apply(cycleList, c -> first degree c);
           max(lastNonZero + 1, 2 * maxGenDeg)
        );

     if cycleList == {} then (
        -- put the cycles that the variables represent in the cache.
        -- return H_0(A) as a ring.
        HA = A.zerothHomology;
        HA.cache = new CacheTable;
        HA.cache#cycles = cycleList;
        A.cache.homologyAlgebra#homologyAlgebra = HA;
	A.cache.homologyAlgebra#GenDegreeLimit = opts.GenDegreeLimit;
	A.cache.homologyAlgebra#RelDegreeLimit = opts.RelDegreeLimit;
     )
     else (
        if (isField A.zerothHomology) then (
           << "Finding easy relations           : ";
           time HA = findEasyRelations(A,cycleList);
        )
        else HA = makeHomologyRing(A,cycleList,{},true);
        HA = getRelations(A,HA,cycleList,maxHomologyDegree,Verbosity=>opts.Verbosity);
        A.cache.homologyAlgebra#homologyAlgebra = HA;
	A.cache.homologyAlgebra#GenDegreeLimit = opts.GenDegreeLimit;
	A.cache.homologyAlgebra#RelDegreeLimit = opts.RelDegreeLimit;
     );
  );
  HA     
)

--------- homologyModule code ----------------------

--- this function takes a DGAlgebra A and a cycle z as input
--- it returns the ComplexMap corresponding to left multiplication
--- by z
dgAlgebraMultMap = method()
dgAlgebraMultMap (DGAlgebra,RingElement) := (A,z) -> (
   R := A.ring;
   cxA := toComplex A;
   if z == 0 then return map(cxA, cxA, i -> map(cxA_i, cxA_i, 0), Degree => 0);
   d := first degree z;
   cxA2 := cxA ** R^(-(drop(degree z,1)));
   zChainMap := map(cxA,cxA2,
          i -> map(cxA_(i+d), cxA2_i, sub(last coefficients(z*getBasis(i,A),Monomials=>getBasis(i+d,A)), R)),
	  Degree=>d);
   -- uncomment the next line if you would like this function
   -- to check that the result is indeed a chain map.
   --assert(zChainMap*KR.dd - (-1)^d*KR.dd*zChainMap);
   zChainMap
)

-- internal use function
moduleRelationsFromCycleAction = method()
moduleRelationsFromCycleAction (DGAlgebra,RingElement,Module) := (A,z,M) -> (
   R := ring M;
   if A.ring =!= R then error "Expected a DGAlgebra and module over same ring.";
   HA := HH(A);
   h := homologyClass(A,z);
   d := first degree z;

   -- left multiplication by z, tensored with M
   zChainMap := dgAlgebraMultMap(A,z);
   zCMtensM := zChainMap ** M;
   KM := target zCMtensM;

   -- Pre-compute pruned homology and pruning-map matrices at each hom degree.
   -- The pruning map (prune H_i).cache.pruningMap : (prune H_i) --> H_i
   -- lets us translate raw chain-map homology into maps between pruned pieces.
   (lo,hi) := concentration KM;
   pruneHM := new MutableHashTable;
   pruneMapMat := new MutableHashTable;
   for i from lo to hi do (
      pruneHM#i = prune HH_i(KM);
      pruneMapMat#i = matrix (pruneHM#i).cache.pruningMap;
   );

   -- Correctly-lifted action matrix on pruned homology, indexed by source hom degree.
   -- actionOnPruned#i : (prune H_i) --> (prune H_{i+d})
   -- obtained by:
   --   (prune H_i) --pruningMap--> H_i --HH_i(z)--> H_{i+d} --lift through pTgt--> (prune H_{i+d})
   -- This replaces the old (broken) use of (prune HH(zCMtensM))_i, which can
   -- produce pruning maps inconsistent with those of the individual H_i's,
   -- yielding action matrices that look like zero after tensoring with HA.
   actionOnPruned := new MutableHashTable;
   for i from lo to hi - d do (
      hhMat := matrix HH_i(zCMtensM);
      composed := hhMat * pruneMapMat#i;
      actionOnPruned#i = composed // pruneMapMat#(i+d);
   );

   -- Build the block matrix representing the action on the total graded module.
   ll := hi - lo + 1;
   -- Degrees (with internal part) so the HA-module presentation is graded.
   degsHMTarget := flatten apply(toList(lo..hi),
       p -> apply(degrees pruneHM#p, dd -> {p} | dd));
   degsHMSource := flatten apply(toList(lo..hi),
       p -> apply(degrees pruneHM#p, dd -> ({p} | dd) + degree z));

   -- block (i,j): action from pruneHM#j into pruneHM#i, nonzero only when i = j + d.
   buildBlock := (iIdx, jIdx) -> (
      i := lo + iIdx;
      j := lo + jIdx;
      if j + d != i then map(pruneHM#i, pruneHM#j, 0)
      else if not actionOnPruned#?j then map(pruneHM#i, pruneHM#j, 0)
      else (
         -- actionOnPruned#j is a matrix; wrap it as a map between the pruned modules
         aMat := actionOnPruned#j;
         map(pruneHM#i, pruneHM#j, aMat)
      )
   );
   matActOfZ := map(HA^(-degsHMTarget), HA^(-degsHMSource),
       tensor(map(HA,R), matrix table(ll, ll, buildBlock)));
   -- relations: h * e_j - (z acting on e_j) = 0
   relsFromZAction := map(HA^(-degsHMTarget), HA^(-degsHMSource), h) - matActOfZ;
   relsFromZAction
)

homologyModule = method()
homologyModule (DGAlgebra,Module) := (A,M) -> (
   -- A is a DG module over a ring R, and M is an R-module.
   -- the return value is a minimal presentation of H(A ** M) as
   -- an H(A)-module.
   HA := HH(A);
   -- first compute all the 'relations' from a cycle acting on HH(A ** M)
   allActions := apply(HA.cache.cycles, z -> moduleRelationsFromCycleAction(A,z,M));
   -- place these in a block row vector and compute a minimal presentation
   -- of the result
   HMoverHA := minimalPresentation coker matrix {allActions};
   HMoverHA
)

isHomologyAlgebraTrivial = method(TypicalValue=>Boolean,Options=>{GenDegreeLimit=>infinity})
isHomologyAlgebraTrivial DGAlgebra := opts -> A -> first findTrivialMasseyOperation(A,opts,TMOLimit=>2)

isGolod = method(TypicalValue=>Boolean)
isGolod Ring := R -> first findTrivialMasseyOperation(koszulComplexDGA(R))

isGolodHomomorphism = method(TypicalValue=>Boolean,Options=>{GenDegreeLimit=>infinity,TMOLimit=>infinity})
isGolodHomomorphism QuotientRing := opts -> R -> first findTrivialMasseyOperation(acyclicClosure(ambient R, EndDegree=>opts.GenDegreeLimit) ** R, opts)

DGAlgebra ** Ring := (A,S) -> (
  -- Cache A ** S on A so repeated calls return the same DGAlgebra.
  -- Essential for functoriality: if f : M -> N is a DGModuleMap and we
  -- base-change, source(f ** S) and target(f ** S) must live in the
  -- SAME (A ** S)-world, otherwise their naturals sit over distinct
  -- rings and matrix assembly fails.
  if A.cache#?"tensorWithRing" and (A.cache#"tensorWithRing")#?S then
      return (A.cache#"tensorWithRing")#S;
  B := freeDGAlgebra(S, A.Degrees);
  newDiff := apply(flatten entries matrix (A.diff), f -> substitute(f,B.natural));
  setDiff(B,newDiff);
  if not A.cache#?"tensorWithRing" then
      A.cache#"tensorWithRing" = new MutableHashTable;
  (A.cache#"tensorWithRing")#S = B;
  B
)

-- DGAlgebra ** DGAlgebra: tensor product over the common ground ring A.ring.
-- The result C is a free DGA on the disjoint union of A.Degrees and B.Degrees,
-- graded-commutative automatically via SkewCommutative on odd-degree generators.
-- The differential on generators is d(a ⊗ 1) = d_A(a) ⊗ 1 and
-- d(1 ⊗ b) = 1 ⊗ d_B(b); no sign issues appear on generators (|1|=0),
-- and the Leibniz rule on products is handled by M2's graded-commutative
-- multiplication when we set differentials only on generators.
--
-- Implementation note: we deliberately avoid `substitute(_, C.natural)` for
-- this pushforward because substitute matches variables by NAME. When A
-- and B share variable names (e.g. A === B), substitute silently maps
-- B's generators onto A's copy inside C and the tensor product comes
-- out wrong. Using explicit inclusion ring maps `iotaA, iotaB` fixes
-- this.
--
-- On return, C.cache is annotated with:
--   tensorFactors      : the ordered pair (A, B)
--   tensorInclusions   : the natural inclusion DGAlgebraMaps (A->C, B->C)
-- so downstream tensor-product machinery can recover them.
DGAlgebra ** DGAlgebra := (A, B) -> (
    if A.ring =!= B.ring then
        error "DGAlgebras must be defined over the same ring.";
    -- Cache A ** B on A so that repeated calls return the same object
    -- (important for UX: (f ** g) has source = A ** B and target =
    -- A' ** B'; if A == A' and B == B' we want the source and target
    -- to coincide, not be two fresh copies of the same algebra).
    if A.cache#?"tensorWith" and (A.cache#"tensorWith")#?B then
        return (A.cache#"tensorWith")#B;
    nA := numgens A.natural;
    nB := numgens B.natural;
    C := freeDGAlgebra(A.ring, A.Degrees | B.Degrees);
    Cgens := gens C.natural;
    iotaA := map(C.natural, A.natural, take(Cgens, nA));
    iotaB := map(C.natural, B.natural, drop(Cgens, nA));
    -- Pushforward the differentials on the T-generators. We intentionally
    -- iterate over `gens X.natural` rather than `matrix (X.diff)` because
    -- the latter, for a tower ring, also includes images of the base-ring
    -- generators (d acts as the identity on R, not as zero), which would
    -- be wrong to thread into C as differentials of new generators.
    dA := apply(gens A.natural, t -> iotaA (A.diff t));
    dB := apply(gens B.natural, t -> iotaB (B.diff t));
    setDiff(C, dA | dB);
    -- Record factor data and natural inclusions (DGAlgebraMaps) on C.cache.
    C.cache#(symbol tensorFactors) = (A, B);
    incAmat := matrix {take(Cgens, nA)};
    incBmat := matrix {drop(Cgens, nA)};
    C.cache#(symbol tensorInclusions) = (
        dgAlgebraMap(C, A, incAmat),
        dgAlgebraMap(C, B, incBmat)
    );
    if not A.cache#?"tensorWith" then
        A.cache#"tensorWith" = new MutableHashTable;
    (A.cache#"tensorWith")#B = C;
    C
)

-- tensorFactors C: the ordered pair of DGA factors from which C was
-- built as A ** B. Errors if C was not built as a tensor product.
tensorFactors = method()
tensorFactors DGAlgebra := C -> (
    if not C.cache#?(symbol tensorFactors) then
        error "tensorFactors DGAlgebra: this DGAlgebra was not built as a tensor product via `DGAlgebra ** DGAlgebra`.";
    C.cache#(symbol tensorFactors)
)

-- tensorInclusions C: the pair of natural inclusion DGAlgebraMaps
-- (A -> C, B -> C). Errors if C was not built as a tensor product.
tensorInclusions = method()
tensorInclusions DGAlgebra := C -> (
    if not C.cache#?(symbol tensorInclusions) then
        error "tensorInclusions DGAlgebra: this DGAlgebra was not built as a tensor product via `DGAlgebra ** DGAlgebra`.";
    C.cache#(symbol tensorInclusions)
)

-- DGAlgebraMap ** DGAlgebraMap: given f : A -> A' and g : B -> B' of
-- DGAlgebras over the common ring R (assumed), build the tensor-product
-- DGAlgebraMap A**B -> A'**B' defined on generators by sending each
-- A-generator to f(·) (embedded in A'**B' via iotaA') and each
-- B-generator to g(·) (embedded via iotaB').
DGAlgebraMap ** DGAlgebraMap := (f, g) -> (
    A := source f; Ap := target f;
    B := source g; Bp := target g;
    if A.ring =!= B.ring or Ap.ring =!= Bp.ring then
        error "DGAlgebraMap **: factors must share a common ground ring on each side.";
    C := A ** B;
    Cp := Ap ** Bp;
    (iotaAp, iotaBp) := tensorInclusions Cp;
    -- Iterate over actual T-generators of A and B (avoiding the
    -- base-ring prefix that `matrix f.natural` would include for
    -- tower rings with Join => false), and lift via the natural
    -- inclusions into Cp.natural.
    liftedF := apply(gens A.natural, t -> (iotaAp.natural)(f.natural t));
    liftedG := apply(gens B.natural, t -> (iotaBp.natural)(g.natural t));
    dgAlgebraMap(Cp, C, matrix {liftedF | liftedG})
)

getBoundaryPreimage = method()
getBoundaryPreimage (DGAlgebra,List) := (A,boundaryList) -> (
   nonzeroes := select(boundaryList, b -> b != 0);
   if nonzeroes == {} then return (true,boundaryList);
   homDegree := first degree first nonzeroes;
   if any(boundaryList, b -> b != 0 and first degree b != homDegree) then
      error "Expected a list of elements of the same homological degree.";
   dnplus1 := polyDifferential(homDegree+1,A);
   Anbasis := flatten entries getBasis(homDegree,A);
   Anplus1basis := getBasis(homDegree+1,A);
   local retVal;
   isBoundary := true;
   if Anbasis == {} then retVal = 0 else (
      boundaryVec := (coefficients(matrix{boundaryList}, Monomials => Anbasis))#1;
      degreeList := apply(degrees target boundaryVec, l -> -drop(l,1));
      boundaryVec = map((A.ring)^degreeList,(A.ring)^(rank source boundaryVec), sub(boundaryVec,A.ring));
      retVal = boundaryVec // dnplus1;
      -- if not all elements of the list are boundaries, then return null - the DGA does not admit a trivial massey operation.
      if dnplus1 * retVal != boundaryVec then (
         -- the below error is just for debugging purposes.
	 -- error "err";

	 -- previously, null was returned if dnplus1 * retVal != boundaryVec.
	 isBoundary = false;
	 retVal = (false, boundaryVec - dnplus1*retVal);
	 -- now, we wish to return a pair.  The first entry is whether the lift is possible
	 -- The second entry the reduction of the input mod the image.
      );
   );
   if isBoundary then
      if retVal == 0 then
         retVal = (true,apply(#boundaryList, i -> 0_(A.natural)))
      else 
         retVal = (true,flatten entries (Anplus1basis * substitute(retVal, A.ring)));
   retVal
)

getBoundaryPreimage (DGAlgebra,RingElement) := (A,b) -> (
   (lifted,myLift) := getBoundaryPreimage(A,{b});
   if lifted then return (lifted, first myLift);
   -- Non-boundary: the list form returns a coefficient matrix (reduction
   -- of the input mod the image of the differential). Convert the single
   -- column back to an element of A.natural so both branches return a
   -- RingElement in the second slot.
   if b == 0 then return (false, 0_(A.natural));
   d := first degree b;
   Anbasis := getBasis(d, A);
   remElement := first flatten entries (Anbasis * substitute(myLift, A.ring));
   (false, remElement)
)

findTrivialMasseyOperation = method(TypicalValue=>Sequence, Options=>{GenDegreeLimit=>infinity,TMOLimit=>infinity})
findTrivialMasseyOperation DGAlgebra := opts -> A -> (
   maxDeg := maxDegree A;
   if maxDeg == infinity and opts.GenDegreeLimit == infinity then error "Must specify an upper bound on the generating degree";
   if maxDeg == infinity and opts.TMOLimit == infinity then error "Must specify an upper bound of order of Massey operations.";
   if opts.GenDegreeLimit != infinity then maxDeg = opts.GenDegreeLimit; 
   cycleList := flatten apply(maxDeg, i -> representativeCycles(i+1,A));
   tmoSoFar := hashTable apply(#cycleList, i -> ({i},cycleList#i));
   hasTMO := true;
   n := 2;
   maxMasseys := min(maxDeg, opts.TMOLimit);
   while n <= maxMasseys do (
      (hasTMO,tmoSoFar) = findNaryTrivialMasseyOperation(A,cycleList,tmoSoFar,n);
      if not hasTMO then ( 
         -- if we are here, tmoSoFar instead has the TMOs to degree n-1, as well as the degree n 
	 -- nonvanishing Massey operations (at least I think)
	 -- << "No trivial Massey operation exists for this algebra.  The " << n << "-ary Massey operations" << endl;
	 -- << "do not vanish and the lifts made thus far are reported in the return value.";
     	 n = maxMasseys;
      );
      n = n + 1;
   );
   (hasTMO, tmoSoFar)
)

-- This method computes all the Nary TMOs.  It takes as input a DGAlgebra A, a list of cycles whose classes
-- form a basis of the homology of the input (possibly up to a certain degree), a hash table containing the
-- previous TMOs, and the order of TMOs we are currently computing.
-- The hash table has (key,values) of the form (list giving the 'tensor monomial' of the MO, the MO itself)
-- It returns a hash table which, given the 'tensor monomial' gives the element of Massey operation for that monomial.
-- If at some stage, this can't be computed, then null is returned.
findNaryTrivialMasseyOperation = method(TypicalValue=>Sequence)
findNaryTrivialMasseyOperation(DGAlgebra,List,HashTable,ZZ) := (A,cycleList,prevTMOs,N) -> (
   -- build the list of 'monomials'
   degreeList := cycleList / degree / first;
   tensMons := masseyMonomials(degreeList,prevTMOs,N);
   -- below are some auxiliary functions to make things a little easier.
   getTMO := (h,m) -> if h#?m then h#m else 0_(A.natural);
   cleanTMOHash := h -> select(h, z -> z != 0_(A.natural));
   tensMonDegree := m -> sum apply(m, i -> degreeList#i);
   -- the elements of prodList are of the form (degree of cycle, corresponding tensor monomial, cycle).  I realize this is redundant, but it makes
   -- the code cleaner.
   prodList := apply(tensMons, m -> ((tensMonDegree m) + #m - 2, m, sum apply(#m-1, i -> ((-1)^((tensMonDegree take(m,i+1)) + i + 1))*(getTMO(prevTMOs,take(m,i+1)))*(getTMO(prevTMOs,drop(m,i+1))))));
   n := min (prodList / first);
   maxDegree := max (prodList / first);
   retVal := hashTable {};
   hasTrivialMasseyOperation := true;
   while n <= maxDegree do (
      boundaryList := select(prodList, z -> z#0 == n);
      if boundaryList != {} then (
         (isBoundary,tempVar) := getBoundaryPreimage(A,boundaryList / last);
	 if (isBoundary == false) then (
	    -- if we are in here, then no trivial Massey operation exists, but tempVar contains
	    -- the reduction of the possible nary products modulo the boundaries, and
	    -- all lower massey operations vanish.  Can we report what the products are?
	    -- yes, by using cycleList, together with boundaryList.  We wish to also
	    -- put this information in retVal.
            hasTrivialMasseyOperation = false;
	    nontrivialTMOs := boundaryList_(select(#boundaryList, i -> tempVar_{i} != 0));
      	    retVal = merge(prevTMOs, hashTable apply(nontrivialTMOs, p -> (p#1,p#2)), first);
	    n = maxDegree;
	 )
	 else retVal = merge(retVal, hashTable apply(#tempVar, i -> (boundaryList#i#1,tempVar#i)),first);
      );
      n = n + 1;
   );
   -- at this point, we want to concatenate all the new TMOs to the old hash table.
   if hasTrivialMasseyOperation then retVal = cleanTMOHash merge(prevTMOs, retVal, first);
   (hasTrivialMasseyOperation,retVal)
)

masseyMonomials = method(TypicalValue=>List)
masseyMonomials (List,HashTable,ZZ) := (degreeList, nonzeroMons, n) -> (
   select((cartesianPower(2,keys nonzeroMons)) / flatten, m -> #m == n)
)

cartesianPower = (n,myList) -> (
   retVal := if n == 2 then toList ((set myList) ** (set myList)) else toList ((set myList) ** (set cartesianPower(n-1,myList)));
   if n > 2 then apply(retVal, p -> flatten {p#0,flatten p#1}) else retVal / toList
)

----- Triple Massey operations code  ------

representativeCycle = method()
representativeCycle(DGAlgebra, RingElement) := (A, h) -> (
   --- this function takes an element h in HH(A) and returns
   --- a cycle representing h
   H := HH(A);
   phi := map(A.natural,H,H.cache.cycles);
   phi h
)

homologyClass = method()
homologyClass(DGAlgebra, RingElement) := (A,z) -> (
   -- this function returns an element of H that is represented by
   -- the element z
   if diff(A,z) != 0 then
      error "Expected a cycle.";
   H := HH(A);
   if z == 0 then return 0_H;
   R := A.ring;
   d := degree z;
   basisHd := basis(d,H);
   phi := map(A.natural,H,H.cache.cycles);
   basisHdCycles := phi basisHd;
   bound := A.dd_((first d)+1);
   cycleCoeffs := sub(last coefficients(z, Monomials => getBasis(first d,A)),R);
   homolCoeffs := sub(last coefficients(basisHdCycles, Monomials => getBasis(first d,A)),R);
   cycleInHomol := sub((cycleCoeffs // (homolCoeffs | bound))^{0..(numcols homolCoeffs - 1)},coefficientRing R);
   first flatten entries (basisHd * cycleInHomol)
)

masseyTripleProductOnCycles = method()
masseyTripleProductOnCycles(DGAlgebra, RingElement, RingElement, RingElement) := (A,z1,z2,z3) -> (
   -- this function computes a cycle representing the Massey operation
   -- of <h1,h2,h3> where hi is represented by zi.
   if ring z1 =!= A.natural or ring z2 =!= A.natural or ring z3 =!= A.natural then
      error "Expected elements of the underlying algebra of input DGAlgebra.";
   l := first degree z1;
   m := first degree z2;
   n := first degree z3;
   --lift12 := diffPreimage(A,(-1)^(l+1)*z1*z2);
   --lift23 := diffPreimage(A,(-1)^(m+1)*z2*z3);
   (lifted12,lift12) := getBoundaryPreimage(A,(-1)^(l+1)*z1*z2);
   if not lifted12 then error "The product z1z2 was not a boundary.";
   (lifted23,lift23) := getBoundaryPreimage(A,(-1)^(m+1)*z2*z3);
   if not lifted23 then error "The product z2z3 was not a boundary.";
   result := (-1)^(l+m)*lift12*z3 + (-1)^(l+1)*z1*lift23;
   result
)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%  Brief note on quadruple Massey operations   %%%%%%

-- To compute Massey quadruple product, one computes:
-- lift13*z4 + lift12*lift34 + z1*lift24

-- Furthermore, according to O'Niell's paper, the two Massey
-- operations must vanish *simultaneously*.  That is, one must
-- trivialize the Massey operations defining lift13 and lift24 with
-- the same lifts lift12, lift23 and lift34.  to check that they
-- simultaneously vanish, one must perform a calculation similar to
-- the findTMO code in DGAlgebras, but only for those two Massey
-- operations.

-- Note that if all (r-1)-ary Massey operations are defined and
-- vanish, then all r-ary Massey operations are defined and uniquely
-- determined.  This doesn't really help us in the general case of
-- defining a quadruple product as discussed above, however.

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

masseyTripleProduct = method()
masseyTripleProduct(DGAlgebra, RingElement, RingElement, RingElement) := (A,h1,h2,h3) -> (
   -- this function computes the Massey triple product <h1,h2,h3>
   H := HH(A);
   if ring h1 =!= H or ring h2 =!= H or ring h3 =!= H then
   (
      if ring h1 =!= A.natural or ring h2 =!= A.natural or ring h3 =!= A.natural then
         error "Expected elements of the homology algebra of input DGAlgebra."
      else
         return masseyTripleProductOnCycles(A,h1,h2,h3);
   );
   if (h1*h2 != 0 or h2*h3 != 0) then return 0_H;
   z1 := representativeCycle(A,h1);
   z2 := representativeCycle(A,h2);
   z3 := representativeCycle(A,h3);
   homologyClass(A, masseyTripleProductOnCycles(A,z1,z2,z3))
)

masseyTripleProduct(DGAlgebra, ZZ, ZZ, ZZ) := (A,l,m,n) -> (
   -- this function computes the map representing the
   -- triple Massey products H_l \otimes H_m \otimes H_n \to H_{l+m+n+1}
   H := HH(A);
   basisHl := flatten entries basis(l,H);
   basisHm := flatten entries basis(m,H);
   basisHn := flatten entries basis(n,H);
   domainBasis := (basisHl ** basisHm ** basisHn) / splice;
   codomainBasis := basis(l+m+n+1,H);
   masseyProducts := matrix {apply(domainBasis, d -> masseyTripleProduct(A,d#0,d#1,d#2))};
   sub(last coefficients(masseyProducts,Monomials=>codomainBasis), coefficientRing A.ring)
)

----- DGAlgebraMap functions --------

net DGAlgebraMap := f -> net f.natural

dgAlgebraMap = method(TypicalValue => DGAlgebraMap)
dgAlgebraMap (DGAlgebra,DGAlgebra,Matrix) := (B,A,fnMatrix) -> (
   f := new MutableHashTable;
   f#(symbol source) = A;
   f#(symbol target) = B;
   f#(symbol natural) = map(B.natural,A.natural,fnMatrix);
   f#(symbol ringMap) = map(B.ring,A.ring,drop(flatten entries matrix f.natural,numgens A.natural) / (f -> substitute(f,B.ring)));
   new DGAlgebraMap from f
)

target DGAlgebraMap := f -> f.target
source DGAlgebraMap := f -> f.source

-- isWellDefined DGAlgebraMap.  Modeled on `isWellDefined ComplexMap`
-- (Complexes/ChainComplexMap.m2 line 134): key-shape check, type checks
-- on each slot, ring compatibility, and the semantic chain-map condition
-- on every DG generator.  Diagnostic messages are emitted when
-- `debugLevel > 0`.
isWellDefined DGAlgebraMap := f -> (
    k := keys f;
    expectedKeys := set {symbol source, symbol target, symbol natural, symbol ringMap, symbol cache};
    -- `cache` is not always present on legacy DGAlgebraMap objects (net
    -- and arithmetic constructors predate the cache slot); tolerate its
    -- absence.
    kExtra := k - expectedKeys;
    kMissing := (expectedKeys - set {symbol cache}) - k;
    if #kExtra > 0 or #kMissing > 0 then (
        if debugLevel > 0 then (
            if #kExtra > 0 then << "-- DGAlgebraMap: unexpected key(s): " << toString(toList kExtra) << endl;
            if #kMissing > 0 then << "-- DGAlgebraMap: missing key(s): " << toString(toList kMissing) << endl;
        );
        return false;
    );
    if not instance(f.source, DGAlgebra) or not instance(f.target, DGAlgebra) then (
        if debugLevel > 0 then << "-- DGAlgebraMap: expected source and target to be DGAlgebras" << endl;
        return false;
    );
    if not isWellDefined f.source then (
        if debugLevel > 0 then << "-- DGAlgebraMap: source is not a well-defined DGAlgebra" << endl;
        return false;
    );
    if not isWellDefined f.target then (
        if debugLevel > 0 then << "-- DGAlgebraMap: target is not a well-defined DGAlgebra" << endl;
        return false;
    );
    if not instance(f.natural, RingMap) then (
        if debugLevel > 0 then << "-- DGAlgebraMap: expected f.natural to be a RingMap" << endl;
        return false;
    );
    if source f.natural =!= (f.source).natural or target f.natural =!= (f.target).natural then (
        if debugLevel > 0 then << "-- DGAlgebraMap: f.natural does not have the expected source/target rings" << endl;
        return false;
    );
    if not instance(f.ringMap, RingMap) then (
        if debugLevel > 0 then << "-- DGAlgebraMap: expected f.ringMap to be a RingMap" << endl;
        return false;
    );
    -- Semantic: chain-map condition on every DG generator.
    A := f.source;
    B := f.target;
    ok := all(gens A.natural, x -> f.natural(A.diff(x)) == B.diff(f.natural(x)));
    if not ok and debugLevel > 0 then
        << "-- DGAlgebraMap: chain-map condition f(d_A x) = d_B(f x) fails on some generator" << endl;
    ok
)

toComplexMap = method(TypicalValue=>ComplexMap,Options=>{EndDegree=>-1,AssertWellDefined=>true})
toComplexMap DGAlgebraMap := opts -> f -> (
   A := source f;
   B := target f;
   maxDeg := maxDegree A;
   if (opts.EndDegree != -1) then maxDeg = opts.EndDegree;
   if maxDeg == infinity then error "Must specify an upper degree bound if an even generator exists.";
   if (opts.AssertWellDefined) then assert isWellDefined f;
   Acc := toComplex(A,maxDeg);
   Bcc := toComplex(B,maxDeg);
   if A.ring === B.ring then
      map(Bcc,Acc,i -> toComplexMap(f,i,opts))
   else
      -- the below doesn't work yet since I haven't made pushForward functorial.
      map(pushForward(f.ringMap,Bcc),Acc, t -> toComplexMap(f,t,opts))
)

toComplexMap (DGAlgebraMap,ZZ) := opts -> (f,n) -> (
   A := source f;
   B := target f;
   R := A.ring;
   S := B.ring;
   sourceList := {};
   targetList := {};
   coeffMatrix := {};
   -- If the rings of A and B differ, then there needs to be a ring map from A.ring to B.ring (unless substitute would work)
   -- then, need to pullback the rings to be modules over the base so that M2 will recognize the module maps.
   -- The code below is if the base rings agree.
   aDiff := polyDifferential(n,A);
   bDiff := polyDifferential(n,B);
   -- At hom-degrees beyond max degree of A or B, the basis is empty; in that
   -- case short-circuit to the zero map between the (possibly-zero) target
   -- and source modules so we don't call `coefficients` on an empty matrix.
   if R === S then (
      sourceList = flatten entries getBasis(n,A);
      targetList = flatten entries getBasis(n,B);
      if sourceList == {} or targetList == {} then
         return map(source bDiff, source aDiff, 0);
      coeffMatrix = substitute((coefficients(matrix {sourceList / f.natural}, Monomials => targetList))#1, B.ring);
      map(source bDiff, source aDiff, coeffMatrix)
   )
   else (
      sourceList = flatten entries getBasis(n,A);
      targetList = flatten entries getBasis(n,B);
      if sourceList == {} or targetList == {} then (
         sPushForward0 := pushForward(f.ringMap,source bDiff);
         return map(sPushForward0, source aDiff, 0);
      );
      sCoeffMatrix := substitute((coefficients(matrix {sourceList / f.natural}, Monomials => targetList))#1, B.ring);
      -- the rest of this code converts the matrix over S to a matrix over R.  It's slightly
      -- different than pushForward(RingMap,Matrix), since the map is between free modules over
      -- different rings.
      J := graphIdeal(f.ringMap,VariableBaseName=>(local XX));
      T := ring J;
      Tbar := T/J;
      h := map(Tbar,S,matrix {take((gens T) / (k -> substitute(k,Tbar)),numgens S)});
      I := ideal 0_Tbar;
      -- have to be a little careful if graphIdeal is zero.
      if (Tbar =!= S) then I = image matrix {drop(gens Tbar,numgens S)};
      Tbarbar := Tbar/I;
      phi := map(Tbar,Tbarbar);
      sVars := (flatten entries basis Tbarbar) / (z -> phi(z));
      -- sVars is so that the coefficients are in the same order as the generators
      -- chosen in pushForward
      h2 := map(R,Tbar,matrix map(R^1,R^(numgens S),0_R) | matrix {gens R});
      entriesOverR := apply(entries sCoeffMatrix, row -> h2(last coefficients(matrix{row / h}, Variables=>take(gens Tbar, numgens R),Monomials=>sVars)));
      sPushForward := pushForward(f.ringMap,source bDiff);
      sRank := rank target first entriesOverR;
      rRank := rank source aDiff;
      -- finally build the map
      map(sPushForward, source aDiff, (i,j) -> (entries entriesOverR#(i//sRank))#(i%sRank)#(j%rRank))
   )
)

-- make pushForward functorial for maps of free modules
pushForward(RingMap,Matrix) := opts -> (f,M) -> (
   -- converts a map of free S-modules (a finite R-algebra) to a map over R
   R := source f;
   S := target f;
   assert (ring M === S);
   J := graphIdeal(f,VariableBaseName=>(local XX));
   T := ring J;
   Tbar := T/J;
   I := ideal 0_Tbar;
   -- have to be a little careful if graphIdeal is zero.
   if (Tbar =!= S) then I = image matrix {drop(gens Tbar,numgens S)};
   Tbarbar := Tbar/I;
   phi := map(Tbar,Tbarbar);
   h := map(Tbar,S,matrix {take(gens Tbar,numgens R)});
   sVars := (flatten entries basis Tbarbar) / (z -> phi(z));
   -- sVars is so that the coefficients are in the same order as the generators
   -- chosen in pushForward (is this true?)
   h2 := map(R,Tbar,matrix map(R^1,R^(numgens S),0_R) | matrix {gens R});
   MEntriesOverR := apply(entries M, row -> apply(row, p -> h2(last coefficients(matrix{h(p)*sVars}, Variables=>take(gens Tbar, numgens R),Monomials=>sVars))));
   targetM := pushForward(f,target M,opts);
   sourceM := pushForward(f,source M,opts);
   sRank := #sVars;
   map(targetM, sourceM, (i,j) -> (entries MEntriesOverR#(i//sRank)#(j//sRank))#(i%sRank)#(j%sRank))
)

pushForward(RingMap,Complex) := opts -> (f,C) -> complex toList apply(0..((length C)-1), i -> pushForward(f,C.dd_(i+1),opts))

-- The function below will return HH(f) as a module map over HH_0(A) (provided HH_0(B)
-- is a finite HH_0(A)-module). 
--homology (DGAlgebraMap,ZZ) := opts -> (f,n) -> (
--)

homology DGAlgebraMap := opts -> f -> (
   A := source f;
   B := target f;
   if not isWellDefined f then
      error "homology DGAlgebraMap: input is not a DG algebra map (does not commute with differentials); cannot induce a map on homology.";
   -- the following commands will fail if A and B have generators in even degree, since one
   -- needs to specify the degrees to look for gens and relations.
   HA := HH A;
   HB := HH B;
   R := B.ring;
   HBToB := map(B.natural,HB,HB.cache.cycles);
   BToR := map(R,B.natural);
   RToH0B := map(zerothHomology B,R); 
   -- now need to take the generators of HA, and write their image in terms of the basis
   -- we have chosen of HB.
   cycleImages := pack(2,mingle {gens HA,HA.cache.cycles / f.natural});
   -- this way we use whatever grading the image has to speed up this process.
   cycleDegrees := cycleImages / (z -> degree last z);
   degreeTable := hashTable(join, pack(2,mingle {cycleDegrees,apply(cycleImages, z -> {z})}));
   -- now for each degree (i.e. each (k,v) pair in the hash table), we need to compute a GB for the submodule generated by the cycles of that
   -- particular degree, together with the image.  Then take the coefficients of the cycles (using getChangeMatrix)
   -- modulo this GB, and this will give us the element in HB to map that variable from HA to.
   imageHash := hashTable flatten apply(pairs degreeTable, (cycleDegree,cycleImageGroup) -> (
	                      hbBasis := flatten entries basis(cycleDegree,HB);
   	                      bBasis := flatten entries getBasis(first cycleDegree,B);
			      thisDiff := polyDifferential((first cycleDegree)+1,B);
			      cycleTargetCoeff := last coefficients(matrix{hbBasis / HBToB},Monomials=>bBasis);
			      cycleImageGroupCoeff := last coefficients(matrix{apply(cycleImageGroup, z -> last z)},Monomials=>bBasis);
			      -- the following commands fix the degrees since function application does not seem to maintain them.
			      cycleTargetCoeff = map(target thisDiff, R^(apply(degrees source cycleTargetCoeff, d -> drop(d,1))), BToR(cycleTargetCoeff));
			      cycleImageGroupCoeff = map(target thisDiff, R^(apply(degrees source cycleImageGroupCoeff, d->drop(d,1))), BToR(cycleImageGroupCoeff));
			      hGB := gb (cycleTargetCoeff | thisDiff, ChangeMatrix => true);
			      -- the first (#cycleTargetCoeff) many rows have the coefficients that we want.  
			      imageHBCoeffs := RToH0B((cycleImageGroupCoeff // hGB)^{0..(#hbBasis)-1});
			      images := basis(cycleDegree,HB)*imageHBCoeffs;
			      pack(2, mingle {apply(cycleImageGroup, z -> first z), flatten entries images})
			   )
		        ); 
   -- now imageHash is a hashTable with key/value pair (source variable, image)
   map(HB,HA,apply(gens HA, x -> imageHash#x))
)

-- This function lifts a ring map in degree zero to a map between DGAlgebras
-- (Conditions on RingMap based on differentials?)
liftToDGMap = method(TypicalValue => DGAlgebraMap, Options=>{EndDegree=>-1})
liftToDGMap (DGAlgebra,DGAlgebra,RingMap) := opts -> (B,A,f) -> (
   -- We assume below that f is an R-algebra map, where R = A.ring, S is a finite R-algebra,
   -- and that f(\del_1(A)) \subseteq \del_1(B).
   R := A.ring;
   S := B.ring;
   -- phi0 := map(S^1,R^1,f,{{1_R}});
   -- not ok yet, only works for quotients.
   phi0 := map(pushForward(f,S^1),R^1,matrix{{1_R}});
   numgensA := numgens A.natural;
   maxLiftDegree := max((degrees A.natural) / first);
   -- if EndDegree is specified, we will send all variables beyond that degree to zero; of course this
   -- may no longer be a DG algebra map.
   if opts.EndDegree != -1 then maxLiftDegree = opts.EndDegree;
   -- degree by degree, we lift.  Because A is semifree (which we will have to check at a later date
   -- should we add non-polynomial underlying algebras), we can send the variables to any lift we choose.
   previousMap := phi0;
   currentMap := 0;
   n := 1;
   imageList := {};
   AGens := select(gens A.natural, z -> first degree z <= maxLiftDegree);
   gensHash := hashTable(join,pack(2,mingle {(AGens / degree / first),AGens / (i -> {i})}));
   while n <= maxLiftDegree do (
      gensList := gensHash#n;
      -- select the relevant columns of the nth differential
      diffA := polyDifferential(n,A);
      basisA := flatten entries getBasis(n,A);
      -- select the columns corresponding to generators
      diffA = diffA_(select(#basisA, i -> (first support basisA_i == basisA_i)));
      diffB := polyDifferential(n,B);
      -- make diffB into a map of R-modules via pushForward
      -- must use pushForward so that all modules are over the same ring so we can lift.
      diffB = map(pushForward(f,target diffB), pushForward(f,source diffB), substitute(diffB,R));
      -- factor previousMap*diffA through the image of diffB
      -- note: previousMap must be a map of R-modules so that // will work.
      factorMap := previousMap*diffA // diffB;
      images := flatten entries (getBasis(n,B)*(f ** factorMap));
      -- images has the images of the generators under the lift of f.
      imageList = imageList | images;
      padVarList := imageList | apply(numgensA-#imageList,i -> 0) | flatten entries matrix f;
      -- make a temporary dgAlgebraMap so we can use existing code to build the dg algebra map in degree n
      tempPhi := dgAlgebraMap(B,A,matrix{padVarList});
      previousMap = toComplexMap(tempPhi,n,AssertWellDefined=>false);
      n = n + 1;
   );
   dgAlgebraMap(B,A,matrix {imageList})
)

----- DGAlgebraMap arithmetic / composition / equality / identity --------
-- Ring-map arithmetic is not meaningful for ring maps as a whole (the
-- sum of two ring maps is not a ring map), so we intentionally skip +/-
-- and scalar multiplication. The operations below are the analogues of
-- the DGModuleMap ops that *do* make sense for ring-valued maps.

-- Composition: (g * f) : A -> C for f : A -> B and g : B -> C
DGAlgebraMap * DGAlgebraMap := (g, f) -> (
   if (target f).natural =!= (source g).natural then
      error "DGAlgebraMap composition: target of first does not match source of second.";
   dgAlgebraMap(target g, source f, matrix (g.natural * f.natural))
)

-- Equality of DGAlgebraMaps: same source/target DGAlgebras and same
-- image matrix of A.natural generators in B.natural.
DGAlgebraMap == DGAlgebraMap := (f, g) -> (
   (source f) === (source g)
   and (target f) === (target g)
   and matrix f.natural == matrix g.natural
)

-- Identity: sends each A.natural generator to itself.
identityDGAlgebraMap = method(TypicalValue => DGAlgebraMap)
identityDGAlgebraMap DGAlgebra := A -> dgAlgebraMap(A, A, vars A.natural)

torMap = method(TypicalValue => RingMap, Options => {GenDegreeLimit=>3})
torMap RingMap := opts -> f -> (
   R := source f;
   S := target f;
   rTorAlg := torAlgebra(R,opts);
   sTorAlg := torAlgebra(S,opts);
   A := acyclicClosure(R,EndDegree=>(opts.GenDegreeLimit-1));
   B := acyclicClosure(S,EndDegree=>(opts.GenDegreeLimit-1));
   sSub := map(sTorAlg, B.natural,matrix{gens sTorAlg});
   phi := liftToDGMap(B,A,f);
   map(sTorAlg,rTorAlg,matrix{take(flatten entries matrix phi.natural,numgens A.natural) / sSub})
)



--------------------
getVariableChunks = method()
getVariableChunks DGAlgebra := A -> (
   ns := values (tally (gens A.natural) / degree / first);
   partSumNs := {0} | apply(#ns, i -> sum take(ns, i+1));
   varsA := apply(#ns, i -> (gens A.natural)_(toList(partSumNs#i..partSumNs#(i+1)-1)));
   varsA
)

chunkDegree = method()
chunkDegree (RingElement,List) := (f,L) -> (sum apply(L, l -> degree(l,f)))*(first degree first L)

getPhiMap = method()
getPhiMap (DGAlgebra, List, List) := (A, inVec, outVec) -> (
   if (sum inVec != sum outVec + 1) then return null;
   varsA := getVariableChunks A;
   h := #varsA;
   if h != #inVec or h != #outVec then return null;
   inBasis := tensor apply(h, i -> basis(inVec#i,A.natural,Variables => varsA#i));
   outBasis := tensor apply(h, i -> basis(outVec#i,A.natural,Variables => varsA#i));
   diffs := matrix {apply(flatten entries inBasis, m -> diff(A,m))};
   tempM := sub(last coefficients(diffs, Monomials => outBasis), A.ring);
   R := A.ring;
   tempM = if not isHomogeneous A then tempM else (
      sourceM := R^(-((flatten entries inBasis) / degree / (e -> drop(e,1))));
      targetM := R^(-((flatten entries outBasis) / degree / (e -> drop(e,1))));
      map(targetM,sourceM,tempM)
   );
   tempM
)

degreeVecs = method()
degreeVecs (DGAlgebra,ZZ) := (A,d) -> (
   varsA := getVariableChunks A;
   basisA := flatten entries basis(d,A.natural);
   sort unique apply(basisA, m -> apply(varsA, l -> chunkDegree(m,l)))
)

displayBlockDiff = method()
displayBlockDiff (DGAlgebra, ZZ) := (A,d) -> (
   M := blockDiff(A,d);
   inputDegs := indices source M;
   outputDegs := indices target M;
   firstRow := {"."} | inputDegs;
   myNet := {firstRow} | apply(outputDegs, o -> {o} | apply(inputDegs, i -> M^[o]_[i]));
   netList(myNet,Alignment => Center)
)

blockDiff = method()
blockDiff (DGAlgebra, ZZ) := (A,d) -> (
   if A.cache#?"blockDiffs" and A.cache#"blockDiffs"#?d then return A.cache#"blockDiffs"#d;
   inputDegs := degreeVecs(A,d);
   outputDegs := degreeVecs(A,d-1);
   tempM := table(#outputDegs, #inputDegs, (a,b) -> getPhiMap(A,inputDegs#b,outputDegs#a));
   sourceMods := (first tempM) / source;
   targetMods := tempM / first / target;
   sourceM := directSum apply(#inputDegs, i -> inputDegs#i => sourceMods#i);
   targetM := directSum apply(#outputDegs, i -> outputDegs#i => targetMods#i);
   M := map(targetM,sourceM,matrix tempM);
   if not A.cache#?"blockDiffs" then
      A.cache#"blockDiffs" = new MutableHashTable from {};
   A.cache#"blockDiffs"#d = M;
   M
)

displayBlockDiff (DGAlgebra, Array, Array) := Matrix => (A, sour, tar) -> (
    --tar: a list of mulit-indices occurring in the (i-1)-th step of the complex associated to A
    --sour: a list of mulit-indices occurring in the (i)-th step of the complex associated to A
    i := sum sour_0;
    X := blockDiff(A,i);
    X_sour^tar
    )
displayBlockDiff (DGAlgebra, List, List) := Matrix => (A, sour, tar) -> (
    --tar: a  mulit-index occurring in the (i-1)-th step of the complex associated to A
    --sour: a index occurring in the (i)-th step of the complex associated to A
    i := sum sour;
    X := blockDiff(A,i);
    X_[sour]^[tar]
    )
displayBlockDiff (DGAlgebra,  VisibleList) := Matrix => (A, tar) -> (
    --tar: a  mulit-index occurring in the (i-1)-th step of the complex associated to A
    --let i = 1+ sum tar, and d = C.dd_i the i-th differential in the complex C = toComplex(A, i).
    --this returns all the blocks in d whose target is the summand corresponding to tar.
    t := tar;
    if class tar === Sequence then t = new Array from {toList t};
    if class tar === List then t = new Array from {t};
    if class t =!= Array or class (t_0) =!= List then 
        error "argument 1 should be a sequence, list or array of one element which is a list.";
    i := 1+sum t_0;
    X := blockDiff(A,i);
    sour := new Array from indices source X;
    X_sour^t
    )
///
restart
loadPackage("DGAlgebras", Reload => true)
R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
A = acyclicClosure(R,EndDegree => 2)
L = indices source blockDiff(A,5)
displayBlockDiff(A,  [{0,2,3}], [{1,0,3},{0,4,0}])
displayBlockDiff(A,  {0,2,3}, {0,4,0})
displayBlockDiff(A , (1,0,3))
displayBlockDiff(A,  {1,0,3})
displayBlockDiff(A,  [{1,0,3}])
///

------------------------------------------------------------
-- v1.2 overhaul: accessor / invariant layer               --
------------------------------------------------------------
-- Rationale: the original DGAlgebra type is a bare
-- MutableHashTable with no encapsulation, forcing downstream
-- code (e.g. KoszulHomHelpers.m2) to reach into A#(symbol …)
-- and to defensively reinitialize caches. The accessors and
-- invariant helpers below let callers treat DGAlgebra as an
-- opaque value with guaranteed cache structure.

-- ensureDGAlgebraCaches(A): guarantee all standard cache
-- sub-tables exist with the expected types. Idempotent; safe
-- to call on any DGAlgebra, even one in a partially-built
-- state (as happens when user code constructs a DGAlgebra by
-- hand from internal keys).
ensureDGAlgebraCaches = method(TypicalValue => DGAlgebra)
ensureDGAlgebraCaches DGAlgebra := A -> (
    if not A#?(symbol cache)
       or A#(symbol cache) === null
       or not instance(A#(symbol cache), CacheTable)
    then A#(symbol cache) = new CacheTable;
    if not A.cache#?(symbol homology)
       or not instance(A.cache#(symbol homology), MutableHashTable)
    then A.cache#(symbol homology) = new MutableHashTable;
    if not A.cache#?(symbol homologyAlgebra)
       or not instance(A.cache#(symbol homologyAlgebra), MutableHashTable)
    then A.cache#(symbol homologyAlgebra) = new MutableHashTable;
    if not A.cache#?(symbol diffs)
       or not instance(A.cache#(symbol diffs), MutableHashTable)
    then A.cache#(symbol diffs) = new MutableHashTable;
    A
)

-- invalidateDGAlgebraCache(A): clear all cached computed
-- values. Called at the top of setDiff / setKoszulDiff so
-- re-setting the differential never leaves stale homology or
-- differential matrices behind.
invalidateDGAlgebraCache = method(TypicalValue => DGAlgebra)
invalidateDGAlgebraCache DGAlgebra := A -> (
    ensureDGAlgebraCaches A;
    A.cache#(symbol homology) = new MutableHashTable;
    A.cache#(symbol homologyAlgebra) = new MutableHashTable;
    A.cache#(symbol diffs) = new MutableHashTable;
    if A.cache#?(symbol dgComplex) then remove(A.cache, symbol dgComplex);
    A
)

-- Public accessors.
underlyingRing = method(TypicalValue => Ring)
underlyingRing DGAlgebra := A -> A.ring

underlyingAlgebra = method(TypicalValue => Ring)
underlyingAlgebra DGAlgebra := A -> A.natural

differential = method()
differential DGAlgebra := A -> A.diff

generatorDegrees = method(TypicalValue => List)
generatorDegrees DGAlgebra := A -> A.Degrees

ring DGAlgebra := A -> A.ring
generators DGAlgebra := opts -> A -> gens A.natural
numgens DGAlgebra := A -> numgens A.natural

-- isValidDGAlgebra(A): structural invariant check. Returns
-- true if the object has the required keys with the expected
-- types. Does *not* verify d^2 = 0; for that use
-- isWellDefinedDifferential below.
isValidDGAlgebra = method(TypicalValue => Boolean)
isValidDGAlgebra DGAlgebra := A -> (
    if not A#?(symbol ring) or not instance(A#(symbol ring), Ring) then return false;
    if not A#?(symbol natural) or not instance(A#(symbol natural), Ring) then return false;
    if not A#?(symbol diff) then return false;
    if A.diff =!= {} and not instance(A.diff, RingMap) then return false;
    if not A#?(symbol Degrees) or not instance(A#(symbol Degrees), List) then return false;
    if #A.Degrees != numgens A.natural then return false;
    true
)

-- isWellDefined DGAlgebra.  Modeled on `isWellDefined Complex`
-- (Complexes/ChainComplex.m2 line 161): key-shape + type checks
-- via isValidDGAlgebra, then the semantic d^2 = 0 check on every
-- DG generator.  Diagnostics via debugLevel > 0.
isWellDefined DGAlgebra := A -> (
    -- Structural check (keys + types).
    if not isValidDGAlgebra A then (
        if debugLevel > 0 then (
            missing := select(
                {symbol ring, symbol natural, symbol diff, symbol Degrees},
                s -> not A#?s);
            if #missing > 0 then
                << "-- DGAlgebra: missing required key(s): " << toString missing << endl;
            if A#?(symbol Degrees) and A#?(symbol natural)
                and #A.Degrees != numgens A.natural then
                << "-- DGAlgebra: #A.Degrees (" << #A.Degrees << ") != numgens A.natural ("
                   << numgens A.natural << ")" << endl;
            if A#?(symbol diff) and A.diff =!= {} and not instance(A.diff, RingMap) then
                << "-- DGAlgebra: A.diff is present but not a RingMap or {}" << endl;
        );
        return false;
    );
    -- Ring consistency: A.natural should be a polynomial-like ring over A.ring.
    if coefficientRing A.natural =!= A.ring and not (ambient A.natural === A.ring) then (
        -- Allow the case where A.ring is a quotient or base ring of A.natural's coefficients.
        -- Soft check: just warn in debug mode.
        if debugLevel > 1 then
            << "-- DGAlgebra: coefficientRing A.natural does not equal A.ring (this is usually OK)" << endl;
    );
    -- Differential degree: d must shift homological degree by -1 on every generator.
    if A.diff =!= {} then (
        for g in gens A.natural do (
            dg := diff(A, g);
            if dg != 0 then (
                degG := first degree g;
                degDG := first degree dg;
                if degDG != degG - 1 then (
                    if debugLevel > 0 then
                        << "-- DGAlgebra: d(" << g << ") has homological degree "
                           << degDG << ", expected " << (degG - 1) << endl;
                    return false;
                );
            );
        );
    );
    -- Semantic: d^2 = 0 on every generator (Leibniz extends to all of A.natural).
    if not isWellDefinedDifferential A then (
        if debugLevel > 0 then << "-- DGAlgebra: d^2 fails to vanish on some generator" << endl;
        return false;
    );
    true
)

-- isWellDefinedDifferential(A): semantic check that d^2 = 0
-- on every DG generator. Sufficient because d is an R-linear
-- derivation, so d^2 = 0 on generators implies d^2 = 0
-- globally. Returns true for the trivial (empty-generator)
-- DG algebra.
isWellDefinedDifferential = method(TypicalValue => Boolean)
isWellDefinedDifferential DGAlgebra := A -> (
    if not isValidDGAlgebra A then return false;
    if A.diff === {} then return true;
    gensList := gens A.natural;
    all(gensList, g -> diff(A, diff(A, g)) == 0)
)

------------------------------------------------------------
-- v1.2 overhaul: ring-manipulation operations             --
------------------------------------------------------------
-- These expose, as user-level operations, constructions that previously
-- required downstream code to rebuild the DGAlgebra from
-- internal hash keys.

-- Private helper: transport an element f of A.natural to
-- B.natural, given phi : A.ring -> B.ring and a matrix
-- giving images of the DG generators T_i of A.natural as
-- elements of B.natural. Coefficients (which live in A.ring)
-- are mapped via phi; monomials in T_i are mapped via the
-- supplied image matrix.
transportNaturalElement := (phi, f, Bnat, genImages) -> (
    if f == 0 then return 0_Bnat;
    (mons, coeffs) := coefficients f;
    monList := flatten entries mons;
    coeffList := flatten entries coeffs;
    sum apply(#monList, i -> (
        c := sub(coeffList#i, source phi);
        -- sub each monomial in T_i's into Bnat via genImages
        mImage := sub(monList#i, genImages);
        (phi c) * mImage
    ))
)

-- baseChange(A, phi): given a ring map phi : R -> S where
-- R = A.ring, produce a DGAlgebra B over S with "the same"
-- DG generators and differential (after applying phi to the
-- scalar coefficients appearing in the differential).
-- Accepts either a RingMap or a target Ring S (in which
-- case map(S, R) is used).
baseChange = method(TypicalValue => DGAlgebra,
    Options => {InitializeDegreeZeroHomology => true, InitializeComplex => false})
baseChange (DGAlgebra, Ring) := opts -> (A, S) -> baseChange(A, map(S, A.ring), opts)
baseChange (DGAlgebra, RingMap) := opts -> (A, phi) -> (
    if source phi =!= A.ring then
        error "baseChange: source of ring map must equal underlyingRing of DGAlgebra";
    S := target phi;
    B := freeDGAlgebra(S, A.Degrees);
    n := numgens A.natural;
    -- Images of the T_i of A.natural inside B.natural
    genImages := matrix {take(gens B.natural, n)};
    origDiff := take(flatten entries matrix A.diff, n);
    newDiffList := apply(origDiff, f -> transportNaturalElement(phi, f, B.natural, genImages));
    setDiff(B, newDiffList,
        InitializeDegreeZeroHomology => opts.InitializeDegreeZeroHomology,
        InitializeComplex => opts.InitializeComplex);
    B
)

-- subDGAlgebra(A, keepIdx): build a new DGAlgebra over the
-- same underlying ring whose DG generators are A's generators
-- at the indices in keepIdx (a List of integers in the range
-- 0..numgens A.natural-1), with the restricted differential.
-- Raises an error if the differential of any kept generator
-- involves a dropped generator (i.e. the subset of indices
-- does not actually span a sub-DG algebra).
subDGAlgebra = method(TypicalValue => DGAlgebra,
    Options => {InitializeDegreeZeroHomology => true, InitializeComplex => false})
subDGAlgebra (DGAlgebra, List) := opts -> (A, keepIdx) -> (
    n := numgens A.natural;
    if not all(keepIdx, i -> instance(i, ZZ) and 0 <= i and i < n) then
        error "subDGAlgebra: keepIdx must be a List of integers in 0 .. numgens-1";
    if keepIdx != unique keepIdx then
        error "subDGAlgebra: keepIdx contains duplicates";
    keepIdx = sort keepIdx;
    droppedIdx := select(toList(0..n-1), i -> not member(i, keepIdx));
    newDegList := A.Degrees_keepIdx;
    B := freeDGAlgebra(A.ring, newDegList);
    -- Build a substitution matrix: each kept T_i -> corresponding gen of B.natural;
    -- each dropped T_i -> 0 in B.natural.
    subList := for i from 0 to n-1 list (
        j := position(keepIdx, k -> k == i);
        if j === null then 0_(B.natural)
        else (gens B.natural)#j
    );
    imageMatrix := matrix {subList};
    origDiff := take(flatten entries matrix A.diff, n);
    -- Safety check: a kept generator's differential must not involve a dropped generator.
    droppedGens := (gens A.natural)_droppedIdx;
    for i in keepIdx do (
        f := origDiff#i;
        suppf := set support f;
        if any(droppedGens, g -> member(g, suppf)) then
            error ("subDGAlgebra: differential of generator " | toString (gens A.natural)#i
                | " involves dropped generator(s) " | toString select(droppedGens, g -> member(g, suppf))
                | "; the subset does not form a sub-DG algebra");
    );
    newDiffList := apply(keepIdx, i -> sub(origDiff#i, imageMatrix));
    setDiff(B, newDiffList,
        InitializeDegreeZeroHomology => opts.InitializeDegreeZeroHomology,
        InitializeComplex => opts.InitializeComplex);
    B
)

-- restrictDifferential: alias that takes a list of indices
-- to KEEP (same semantics as subDGAlgebra). Named to match
-- the vocabulary used in KoszulHomHelpers.m2.
restrictDifferential = method(TypicalValue => DGAlgebra,
    Options => {InitializeDegreeZeroHomology => true, InitializeComplex => false})
restrictDifferential (DGAlgebra, List) := opts -> (A, keepIdx) -> subDGAlgebra(A, keepIdx, opts)

-- truncateGenerators(A, n): return the sub-DG algebra whose
-- DG generators are those of A with first homological
-- degree > n. Replaces the hand-rolled truncateTerm pattern
-- in KoszulHomHelpers.m2:707.
truncateGenerators = method(TypicalValue => DGAlgebra,
    Options => {InitializeDegreeZeroHomology => true, InitializeComplex => false})
truncateGenerators (DGAlgebra, ZZ) := opts -> (A, n) -> (
    keepIdx := select(toList(0..#A.Degrees-1), i -> first A.Degrees#i > n);
    subDGAlgebra(A, keepIdx, opts)
)

-- killHomologyAtDegree(A, n): find a representing set of
-- cycles for H_n(A) and adjoin variables to make them
-- boundaries, returning a new DGAlgebra. Unlike killCycles
-- (which searches for the first nontrivial degree starting
-- from StartDegree), this targets a specific degree.
killHomologyAtDegree = method(TypicalValue => DGAlgebra)
killHomologyAtDegree (DGAlgebra, ZZ) := (A, n) -> (
    Hn := prune homology(n, A);
    if Hn == 0 then return A;
    homologyGens := entries transpose gens image (Hn.cache.pruningMap);
    basisList := flatten entries getBasis(n, A);
    cycleList := apply(homologyGens, gen -> sum apply(#gen, i -> gen#i * basisList#i));
    adjoinVariables(A, cycleList)
)

-- dgComplex(A): lazy chain-complex accessor. Computes and caches
-- a Complex representing A's underlying chain complex of free
-- A.ring-modules. Unlike setDiff's InitializeComplex option
-- (which eagerly builds A.dd), this pays only when the caller
-- actually wants the complex, and reuses it on repeat calls.
-- Invalidated by invalidateDGAlgebraCache.
dgComplex = method(TypicalValue => Complex)
dgComplex DGAlgebra := A -> (
    ensureDGAlgebraCaches A;
    if A.cache#?(symbol dgComplex) then return A.cache#(symbol dgComplex);
    n := maxDegree A;
    if n === infinity then (
        -- even generators present; default to totalOddDegree+1 (matches setDiff's choice)
        n = sum select(degrees A.natural / first, i -> odd i) + 1;
    );
    C := toComplex(A, max(n, 1));
    A.cache#(symbol dgComplex) = C;
    C
)

-------------------------------------------------------------------------
-- DGModule: DG modules over a DGAlgebra.
--
-- A DGModule M over a DGAlgebra A is a graded A.natural-module M.natural
-- together with an R-linear differential of homological degree -1
-- satisfying the Leibniz rule
--     d_M(a . x) = d_A(a) . x + (-1)^{|a|} a . d_M(x).
-- Storage follows the DGAlgebra pattern (see the type declaration).
--
-- The first concrete constructor is koszulComplexDGM, producing
-- K^R \otimes_R M as a DG module over koszulComplexDGA R.
-------------------------------------------------------------------------

-- Pretty-print a DGModule. Shows base ring, DG algebra, the natural
-- graded module, and the generator hom-degrees. (Mirrors `net DGAlgebra`.)
net DGModule := M -> (
    myOutput := {net "Base ring => " | net M.ring};
    myOutput = myOutput | {net "DG algebra => " | net M.dgAlgebra.natural};
    myOutput = myOutput | {net "Natural module => " | net M.natural};
    myOutput = myOutput | {net "Generator degrees => " | net M.Degrees};
    myOutput = myOutput | {net "Differentials on gens => " | net M.diff};
    horizontalJoin flatten ("{", stack myOutput, "}")
)

-- Accessor methods: extend the v1.2 accessor layer to DGModules.
underlyingRing DGModule := M -> M.ring
underlyingAlgebra DGModule := M -> M.natural
generatorDegrees DGModule := M -> M.Degrees
differential DGModule := M -> M.diff

-- koszulComplexDGM(M): build K^R \otimes_R M as a DG module over
-- koszulComplexDGA R. The module generators sit in hom-degree 0
-- (so d_M vanishes on them) and the full differential is determined by
-- Leibniz + d on A.natural.
--
-- Two forms:
--   koszulComplexDGM M            -- builds (or reuses) koszulComplexDGA(ring M)
--   koszulComplexDGM(A, M)        -- use a caller-supplied DG algebra whose
--                                    ring must match ring M
koszulComplexDGM = method(TypicalValue => DGModule)
koszulComplexDGM Module := M -> (
    R := ring M;
    A := koszulComplexDGA R;
    koszulComplexDGM(A, M)
)
koszulComplexDGM (DGAlgebra, Module) := (A, M) -> (
    R := ring M;
    if A.ring =!= R then
        error "koszulComplexDGM: DG algebra and module must share the same base ring.";
    -- Lift the R-presentation of M to an A.natural-presentation, then take
    -- the cokernel. Relations sit in hom-degree 0, so d(rel) = 0 and the
    -- differential descends to the quotient.
    presM := presentation M;
    numGens := numRows presM;
    presOverA := sub(presM, A.natural);
    -- Degree padding: A.natural has one extra degree slot (hom-degree)
    -- relative to R, so prepend a 0 to each target-row degree of presM.
    rDegs := if numGens == 0 then {} else (degrees target presM);
    weightPad := if #rDegs == 0 then (
        if isHomogeneous A.natural and #degrees A.natural > 0
            then #(first degrees A.natural) else 1
    ) else (1 + #(first rDegs));
    genDegs := if numGens == 0 then {}
               else apply(rDegs, d -> {0} | d);
    -- Free graded A.natural-module with generators in hom-degree 0.
    F := (A.natural)^(-genDegs);
    naturalModule := coker map(F, , presOverA);
    result := new MutableHashTable;
    result#(symbol dgAlgebra) = A;
    result#(symbol ring) = R;
    result#(symbol module) = M;
    result#(symbol natural) = naturalModule;
    result#(symbol Degrees) = genDegs;
    -- d vanishes on hom-degree-0 generators (would land in hom-degree -1 = 0).
    result#(symbol diff) = apply(numGens, i -> 0_(naturalModule));
    result#(symbol cache) = new CacheTable;
    result.cache#(symbol diffs) = new MutableHashTable;
    new DGModule from result
)

-- moduleBasisPairs(M, n): list of pairs (i, m) describing an A.ring-basis
-- of the hom-degree-n piece of a free DG module M. Here i indexes a free
-- generator e_i of M.natural (hom-degree d_i) and m is an A.natural
-- monomial of hom-degree n - d_i; the corresponding basis element is
-- m * e_i in M.natural. Used by moduleDifferential's general path.
moduleBasisPairs = (M, n) -> (
    A := M.dgAlgebra;
    numGens := rank M.natural;
    flatten apply(numGens, i -> (
        di := first M.Degrees#i;
        needed := n - di;
        if needed < 0 then {}
        else (
            mons := flatten entries getBasis(needed, A);
            apply(mons, m -> (i, m))
        )
    ))
)

-- moduleLeibniz(M, i, m): the Leibniz differential of the basis element
-- m * e_i in a free DG module. Returns a Vector in M.natural.
--   d(m . e_i) = d_A(m) . e_i + (-1)^|m| m . d_M(e_i)
-- where |m| is the hom-degree of m in A.
moduleLeibniz = (M, i, m) -> (
    A := M.dgAlgebra;
    ei := (M.natural)_i;
    term1 := polyDifferential(A, m) * ei;
    signExp := first degree m;
    sign := if odd signExp then -1 else 1;
    term2 := sign * m * M.diff#i;
    term1 + term2
)

-- moduleDifferential(n, M): the degree-n piece of the DG module's
-- differential, returned as an R-linear map M_n -> M_{n-1}.
--
-- Two code paths:
--   * Fast path: when M was built by koszulComplexDGM (M.module is set)
--     AND d_M vanishes on every generator, the differential is the
--     tensor d_A \otimes id_M, computed as polyDifferential(n,A) ** M.module.
--   * General Leibniz path: for free DG modules (freeDGModule output) with
--     arbitrary differentials on generators. Works monomial-by-monomial
--     using moduleLeibniz and extracts the R-linear matrix per target
--     generator via `coefficients`.
--
-- Caching: matches polyDifferential — one slot per n in M.cache.diffs.
moduleDifferential = method(TypicalValue => Matrix)
moduleDifferential (ZZ, DGModule) := (n, M) -> (
    if M.cache.diffs#?n then return M.cache.diffs#n;
    A := M.dgAlgebra;
    R := A.ring;
    -- Fast path for koszulComplexDGM-style DG modules: d_A ⊗ id_M.
    if M.?module and all(M.diff, z -> z == 0) then (
        dA := polyDifferential(n, A);
        fastResult := dA ** M.module;
        M.cache.diffs#n = fastResult;
        return fastResult;
    );
    -- General Leibniz path. Requires M.natural to be a free graded
    -- A.natural-module (freeDGModule output). Bail for non-free M.natural
    -- since `entries` on a Vector in a cokernel is not the right semantics.
    if not isFreeModule M.natural then
        error "moduleDifferential: general path requires M.natural to be a free module (use freeDGModule).";
    if n < 0 then return map(R^0, R^0, 0);
    sourcePairs := moduleBasisPairs(M, n);
    targetPairs := moduleBasisPairs(M, n-1);
    numSrc := #sourcePairs;
    numTgt := #targetPairs;
    -- R-module degrees (drop hom-degree slot) for source/target
    pairDegree := p -> (
        (i, m) := p;
        totalDeg := degree(m * (M.natural)_i);
        -drop(totalDeg, 1)
    );
    srcDegs := apply(sourcePairs, pairDegree);
    tgtDegs := apply(targetPairs, pairDegree);
    if numSrc == 0 then (
        r0 := map(R^tgtDegs, R^0, 0);
        M.cache.diffs#n = r0;
        return r0;
    );
    if numTgt == 0 then (
        r1 := map(R^0, R^srcDegs, 0);
        M.cache.diffs#n = r1;
        return r1;
    );
    numNatGens := rank M.natural;
    -- Group target pairs by generator index j; keep original column index.
    tgtByGen := new MutableHashTable;
    scan(numNatGens, j -> tgtByGen#j = {});
    scan(numTgt, k -> (
        (j, mp) := targetPairs#k;
        tgtByGen#j = append(tgtByGen#j, (k, mp));
    ));
    mat := mutableMatrix(R, numTgt, numSrc);
    scan(numSrc, c -> (
        (i, m) := sourcePairs#c;
        db := moduleLeibniz(M, i, m);
        dbList := entries db;
        scan(numNatGens, j -> (
            fj := dbList#j;
            if fj == 0 then return;
            rowsForJ := tgtByGen#j;
            if #rowsForJ == 0 then return;
            tgtMonsJ := apply(rowsForJ, pr -> pr#1);
            coefMat := (coefficients(matrix{{fj}}, Monomials => matrix{tgtMonsJ}))#1;
            coefMatR := sub(coefMat, R);
            scan(#rowsForJ, idx -> (
                rowIdx := (rowsForJ#idx)#0;
                cij := coefMatR_(idx, 0);
                if cij != 0 then mat_(rowIdx, c) = cij;
            ));
        ));
    ));
    result := map(R^tgtDegs, R^srcDegs, matrix mat);
    M.cache.diffs#n = result;
    result
)

-- homology(n, M): H_n(M) as an R-module.
homology (ZZ, DGModule) := opts -> (n, M) -> (
    dn := moduleDifferential(n, M);
    dnplus1 := moduleDifferential(n+1, M);
    homology(dn, dnplus1)
)

-------------------------------------------------------------------------
-- freeDGModule: semifree DG modules over a DGAlgebra.
--
-- A free DG module over A is a graded A.natural-module F = A.natural^(-degList)
-- with an A.ring-linear differential of hom-degree -1 satisfying Leibniz:
--   d(a . x) = d_A(a) . x + (-1)^|a| a . d_M(x)
-- Storage mirrors DGAlgebra's freeDGAlgebra (M.natural is a free module
-- rather than a cokernel, so Leibniz on monomials is well-defined). The
-- differential is initialized to 0 on every generator; set it via
-- setDiff(M, diffList).
-------------------------------------------------------------------------
freeDGModule = method(TypicalValue => DGModule)
freeDGModule (DGAlgebra, List) := (A, degList) -> (
    -- Normalize: each entry of degList should be a list compatible with
    -- A.natural's degree group. Scalar ZZ is wrapped as {n}; too-short
    -- lists are right-padded with zeros.
    degList = apply(degList, d -> if instance(d, ZZ) then {d} else toList d);
    if #degList > 0 and #(degrees A.natural) > 0 then (
        aDegLen := #(first degrees A.natural);
        degList = apply(degList, d -> if #d < aDegLen then d | toList((aDegLen - #d):0) else d);
    );
    F := if #degList == 0 then (A.natural)^0 else (A.natural)^(-degList);
    result := new MutableHashTable;
    result#(symbol dgAlgebra) = A;
    result#(symbol ring) = A.ring;
    result#(symbol natural) = F;
    result#(symbol Degrees) = degList;
    result#(symbol diff) = apply(#degList, i -> 0_F);
    result#(symbol cache) = new CacheTable;
    result.cache#(symbol diffs) = new MutableHashTable;
    new DGModule from result
)

-- setDiff(M, diffList): record the differential of each free generator.
-- diffList#i is a Vector in M.natural (or an element coercible to one)
-- living in hom-degree (d_i - 1) where d_i is the hom-degree of the i-th
-- generator. Invalidates all cached differentials.
setDiff (DGModule, List) := opts -> (M, diffList) -> (
    if not isFreeModule M.natural then
        error "setDiff DGModule: currently only supported for free DG modules (freeDGModule output).";
    if #diffList != #M.Degrees then
        error ("setDiff DGModule: expected " | toString(#M.Degrees)
            | " differentials, got " | toString(#diffList) | ".");
    ensureDGAlgebraCaches M;
    invalidateDGAlgebraCache M;
    -- Coerce each entry to a Vector in M.natural. Accept Vector as-is;
    -- accept a 1-column Matrix by taking its first column; accept 0.
    coerced := apply(diffList, e -> (
        if e === 0 then 0_(M.natural)
        else if instance(e, Vector) then e
        else if instance(e, Matrix) and numcols e == 1 then (
            -- fold the column into a vector in M.natural
            rowDegs := degrees target e;
            if rank M.natural != #rowDegs then
                error "setDiff DGModule: matrix differential has wrong number of rows.";
            sum apply(#rowDegs, j -> e_(j,0) * (M.natural)_j)
        )
        else try (sub(e, M.natural)) else
            error "setDiff DGModule: could not coerce differential entry to M.natural."
    ));
    M.diff = coerced;
    M
)
-- setDiff returns the mutated DGModule for a DGModule input, even though
-- the method's default TypicalValue is DGAlgebra.  Inform the documentation
-- system so per-signature doc nodes for (setDiff, DGModule, List) see the
-- right return type.
typicalValues#(setDiff, DGModule, List) = DGModule

-------------------------------------------------------------------------
-- adjoinGenerators / killCycles / semifreeResolution for DGModule.
-- Module-theoretic analogs of adjoinVariables / killCycles / acyclicClosure
-- on DGAlgebra.
-------------------------------------------------------------------------

-- adjoinGenerators(M, cycleList): produce a new free DG module M' that
-- agrees with M on its existing generators and adds one fresh generator
-- per element of cycleList, whose differential is the corresponding cycle.
-- Each cycle must be a Vector in M.natural of hom-degree d; the new
-- generator has hom-degree d+1.
-- Existing generator indices and differentials are preserved verbatim.
adjoinGenerators = method(TypicalValue => DGModule)
adjoinGenerators (DGModule, List) := (M, cycleList) -> (
    A := M.dgAlgebra;
    if #cycleList == 0 then return M;
    -- Derive new generator degrees: hom-degree = deg(z)#0 + 1; remaining
    -- degree components copied from z (so the new generator's differential
    -- is homogeneous w.r.t. the ambient multi-grading).
    newDegs := apply(cycleList, z -> (
        d := degree z;
        {first d + 1} | drop(d, 1)
    ));
    combinedDegs := M.Degrees | newDegs;
    -- Build a fresh free DG module with the combined generator list.
    Mnew := freeDGModule(A, combinedDegs);
    -- Lift each existing differential from M.natural to Mnew.natural.
    -- M.natural has rank r = #M.Degrees; Mnew.natural has rank r + #newDegs.
    -- The first r generators of Mnew correspond to those of M.
    r := #M.Degrees;
    phi := v -> (
        if v === 0 then 0_(Mnew.natural)
        else (
            vEntries := entries v;
            sum apply(r, j -> (vEntries#j) * (Mnew.natural)_j)
        )
    );
    oldDiffs := apply(M.diff, phi);
    newDiffs := apply(cycleList, phi);
    setDiff(Mnew, oldDiffs | newDiffs);
    -- Cache migration: if all new gens sit at hom-degree >= minNewDeg, then
    -- F_i(Mnew) = F_i(M) for all i < minNewDeg (new gens don't contribute
    -- below their own hom-degree, assuming A is nonnegatively graded in
    -- hom-deg). Reuse the already-computed d_i for those i. Also, since F_i
    -- and F_{i-1} are literally equal, any downstream code that composes
    -- the cached matrix still works.
    -- setDiff above invalidated Mnew's cache, so do this after.
    if M.cache.?diffs then (
        minNewDeg := min apply(newDegs, d -> first d);
        scan(keys M.cache.diffs, n -> (
            if n < minNewDeg then Mnew.cache.diffs#n = M.cache.diffs#n;
        ));
    );
    Mnew
)

-- killCycles M: find the smallest n (within the requested range) for which
-- H_n(M) is nonzero, and adjoin free generators in hom-degree n+1 whose
-- differentials are representative cycles of a minimal generating set of
-- H_n(M). Mirrors killCycles DGAlgebra.
killCycles DGModule := opts -> M -> (
    endDegree := if opts.EndDegree == -1 then opts.StartDegree else opts.EndDegree;
    if opts.StartDegree > endDegree then
        error "killCycles DGModule: StartDegree exceeds EndDegree.";
    n := opts.StartDegree;
    foundHomology := false;
    nthHomology := null;
    while n <= endDegree and not foundHomology do (
        nthHomology = prune homology(n, M);
        if nthHomology == 0 then n = n + 1 else foundHomology = true;
    );
    if not foundHomology then return M;
    -- Lift minimal homology generators from H_n back to cycles in M_n.
    -- moduleDifferential(n, M) is a map over A.ring from the free A.ring
    -- basis of M at hom-degree n; cycles are in the kernel of that map.
    -- We reconstruct cycles as Vectors in M.natural by pairing the kernel
    -- coefficients with the hom-degree-n basis pairs (i, mon) of M.
    pruningMap := nthHomology.cache.pruningMap;
    homGensMat := gens image pruningMap;  -- columns over A.ring
    sourcePairs := moduleBasisPairs(M, n);
    if #sourcePairs == 0 then return M;
    moduleGens := apply(rank M.natural, i -> (M.natural)_i);
    cycleList := apply(numcols homGensMat, c -> (
        sum apply(#sourcePairs, k -> (
            (i, mon) := sourcePairs#k;
            cK := homGensMat_(k, c);
            if cK == 0 then 0_(M.natural) else cK * mon * moduleGens#i
        ))
    ));
    -- Drop any accidentally-zero cycles (shouldn't happen, but be safe).
    cycleList = select(cycleList, z -> z != 0);
    if #cycleList == 0 then return M;
    adjoinGenerators(M, cycleList)
)
-- killCycles returns a DGModule when given a DGModule, even though
-- the method's default TypicalValue is DGAlgebra.  Inform the documentation
-- system so per-signature doc nodes for (killCycles, DGModule) see the
-- right return type.
typicalValues#(killCycles, DGModule) = DGModule

-- semifreeResolution(A, M, EndDegree => n): build a semifree DG module
-- over A approximating a resolution of (an R-module or H_0 shape) M up to
-- hom-degree n. Starts with a free DG module whose hom-degree-0 generators
-- match a generating set for M and whose hom-degree-1 differentials are
-- the presentation matrix columns; iteratively applies killCycles to kill
-- H_i for i = 1..n.
-- Mirrors acyclicClosure DGAlgebra: iteratively kills homology up to the
-- requested degree, producing a semifree DG module resolution.
semifreeResolution = method(TypicalValue => DGModule, Options => {StartDegree => 1, EndDegree => 3})
semifreeResolution (DGAlgebra, Module) := opts -> (A, M) -> (
    R := A.ring;
    if ring M =!= R then
        error "semifreeResolution: module must be over the DG algebra's base ring.";
    -- Free cover: one hom-degree-0 generator per generator of M.
    numGensM := numgens M;
    genDegs := apply(numGensM, i -> (
        rd := (degrees target presentation M)#i;
        {0} | toList rd
    ));
    Mdg := freeDGModule(A, genDegs);
    -- Kill the presentation relations: adjoin hom-degree-1 gens whose
    -- differentials are the relations (viewed as elements of M.natural).
    presM := presentation M;
    if numcols presM > 0 then (
        natGens := apply(rank Mdg.natural, i -> (Mdg.natural)_i);
        presOverA := sub(presM, A.natural);
        relCycles := apply(numcols presOverA, c -> (
            sum apply(numGensM, j -> presOverA_(j, c) * natGens#j)
        ));
        relCycles = select(relCycles, z -> z != 0);
        if #relCycles > 0 then Mdg = adjoinGenerators(Mdg, relCycles);
    );
    -- Now iterate killCycles from StartDegree up to EndDegree.
    n := opts.StartDegree;
    while n <= opts.EndDegree do (
        Mdg = killCycles(Mdg, StartDegree => n);
        n = n + 1;
    );
    Mdg
)
semifreeResolution Module := opts -> M -> (
    R := ring M;
    A := koszulComplexDGA R;
    semifreeResolution(A, M, opts)
)

-------------------------------------------------------------------------
-- minimalSemifreeResolution(A, M, EndDegree => n): MINIMAL semifree
-- resolution of M over A, up to hom-degree n.
--
-- "Minimal" here means that when we tensor with the residue field k of
-- R along the augmentation A -> k (substituting T_i = 0 and reducing mod
-- m_R), every differential becomes zero. Equivalently, each generator's
-- differential lives in the augmentation ideal of A.natural.
--
-- Strategy:
--   (1) Start from prune M, giving mu(M) hom-degree-0 generators (minimal
--       count over R).
--   (2) Check each column of presentation(prune M) against the DGA-induced
--       differential: x * e_0 may already be d(T_i * e_0) and thus already
--       a boundary in F_0. Using getBoundaryPreimage, we compute the
--       NON-BOUNDARY RESIDUE r of each relation column, and adjoin a
--       hom-deg-1 generator only when r is nonzero — with differential r,
--       not the original relation. This is the key step that makes the
--       resolution minimal OVER A (not just over R): blind adjoinment of
--       presentation columns duplicates Koszul-style relations and
--       produces extra cycles at later stages.
--   (3) Iterate killCycles. The existing killCycles DGModule uses
--       `prune homology(n, M).cache.pruningMap` to pick representative
--       cycles, which already gives a MINIMAL generating set of H_n over R
--       (prune over a graded-local ring = minimal presentation). So each
--       iteration contributes exactly mu(H_n(M)) new generators.
--
-- Efficiency: relies on the cache migration in adjoinGenerators to avoid
-- recomputing low-degree differentials after each killCycles step.
--
-- Precondition: A.ring should be (graded-)local so that prune gives a
-- minimal presentation. For A = koszulComplexDGA R with R a standard
-- graded quotient of a polynomial ring over a field, this is satisfied.
-------------------------------------------------------------------------
minimalSemifreeResolution = method(TypicalValue => DGModule,
    Options => {StartDegree => 1, EndDegree => 3})
minimalSemifreeResolution (DGAlgebra, Module) := opts -> (A, M) -> (
    R := A.ring;
    if ring M =!= R then
        error "minimalSemifreeResolution: module must be over the DG algebra's base ring.";
    -- Replace M by a minimally-presented copy. prune gives a minimal
    -- presentation over a (graded-)local ring; over a polynomial ring it
    -- gives a minimal graded presentation.
    Mmin := prune M;
    numGensM := numgens Mmin;
    presMin := presentation Mmin;
    -- Generator degrees: copy from the target of the minimal presentation.
    genDegs := if numGensM == 0 then {}
               else apply(numGensM, i -> (
                   rd := (degrees target presMin)#i;
                   {0} | toList rd
               ));
    Mdg := freeDGModule(A, genDegs);
    -- Hom-deg 1 stage: each minimal relation of M becomes an element of
    -- F_0(Mdg). If it is already a boundary under the DGA-induced
    -- differential d_1 (from d(A_1 * e_j) terms), skip; otherwise adjoin
    -- a hom-deg-1 gen killing the non-boundary RESIDUE (not the original
    -- column), which is the minimal extension.
    if numcols presMin > 0 and numGensM > 0 then (
        natGens := apply(rank Mdg.natural, i -> (Mdg.natural)_i);
        presOverA := sub(presMin, A.natural);
        relCycles := apply(numcols presOverA, c -> (
            sum apply(numGensM, j -> presOverA_(j, c) * natGens#j)
        ));
        nonZeroRels := select(relCycles, z -> z != 0);
        if #nonZeroRels > 0 then (
            (liftOk, liftsOrResidues) := getBoundaryPreimage(Mdg, nonZeroRels);
            toAdjoin := if liftOk then {} else
                select(liftsOrResidues, r -> r != 0);
            if #toAdjoin > 0 then Mdg = adjoinGenerators(Mdg, toAdjoin);
        );
    );
    -- Iterate killCycles from StartDegree up to EndDegree. The existing
    -- killCycles DGModule uses prune on H_n and takes representative cycles
    -- from the pruning map's image — which is a minimal generating set.
    n := opts.StartDegree;
    while n <= opts.EndDegree do (
        Mdg = killCycles(Mdg, StartDegree => n);
        n = n + 1;
    );
    Mdg
)
minimalSemifreeResolution Module := opts -> M -> (
    R := ring M;
    A := koszulComplexDGA R;
    minimalSemifreeResolution(A, M, opts)
)

-------------------------------------------------------------------------
-- isMinimalSemifreeResolution(M): true iff each generator differential
-- lies in the augmentation ideal of A.natural. Equivalently: reducing
-- A.natural modulo (vars R, positive-hom-deg generators), each component
-- of each M.diff#i collapses to zero.
--
-- Does NOT check acyclicity — use isAcyclic with an EndDegree for that.
-------------------------------------------------------------------------
isMinimalSemifreeResolution = method(TypicalValue => Boolean)
isMinimalSemifreeResolution DGModule := M -> (
    if not isValidDGModule M then return false;
    if not isFreeModule M.natural then return false;
    A := M.dgAlgebra;
    R := A.ring;
    -- Build the augmentation map A.natural -> k:
    --   first set all T_i = 0 (sub to R), then reduce mod ideal vars R.
    killT := map(R, A.natural,
        matrix{apply(numgens A.natural, i -> 0_R)});
    mR := if numgens R > 0 then ideal vars R else ideal 0_R;
    augIsZero := f -> (
        if f == 0 then true
        else (killT f) % mR == 0
    );
    all(M.diff, v -> (
        if v == 0 then true
        else all(entries v, c -> augIsZero c)
    ))
)

-------------------------------------------------------------------------
-- DG module visualization: generatorTable, dgModuleSummary,
-- moduleBlockDiff, displayModuleBlockDiff.
--
-- Parallels the DGAlgebra-side blockDiff / displayBlockDiff machinery but
-- augments the chunk-vector labels with a module-generator index so each
-- block knows which generator it belongs to.
-------------------------------------------------------------------------

-- generatorTable(M): a per-generator table — i, hom-deg, ext-deg, d(e_i).
-- Useful for eyeballing a semifree resolution's generator list and
-- differentials at a glance.
generatorTable = method()
generatorTable DGModule := M -> (
    if #M.Degrees == 0 then return netList({{"no generators"}}, Alignment => Center);
    hdrs := {"i", "hom-deg", "ext-deg", "d(e_i)"};
    rows := apply(#M.Degrees, i -> (
        d := M.Degrees#i;
        hd := first d;
        ed := drop(d, 1);
        {i, hd, ed, M.diff#i}
    ));
    netList({hdrs} | rows, Alignment => Center, HorizontalSpace => 1, BaseRow => 0)
)

-- dgModuleSummary(M, n) / dgModuleSummary(M):
--   Report, for each hom-degree k in [0, n]:
--     * # A.ring-basis elements of F_k (#moduleBasisPairs)
--     * # generators of M.natural at hom-degree k (adjoined in that row)
--   When n is omitted, uses maxDegree(M) if finite, else errors.
dgModuleSummary = method()
dgModuleSummary (DGModule, ZZ) := (M, n) -> (
    if n < 0 then error "dgModuleSummary: degree bound must be nonnegative.";
    genCounts := new MutableHashTable;
    scan(M.Degrees, d -> (
        h := first d;
        if not genCounts#?h then genCounts#h = 0;
        genCounts#h = genCounts#h + 1;
    ));
    hdrs := {"hom-deg", "#gens adjoined", "rank F_n"};
    rows := apply(toList(0..n), k -> (
        g := if genCounts#?k then genCounts#k else 0;
        rF := #moduleBasisPairs(M, k);
        {k, g, rF}
    ));
    netList({hdrs} | rows, Alignment => Center, HorizontalSpace => 1, BaseRow => 0)
)
dgModuleSummary DGModule := M -> (
    d := maxDegree M;
    if d === infinity then
        error "dgModuleSummary: hom-degree is unbounded; supply an integer bound.";
    dgModuleSummary(M, d)
)

-- moduleBlockPhiMap(M, i, inVec, j, outVec): the (tgt <- src) block of d_n
-- where n = (first M.Degrees#i) + sum inVec, mapping
--   basis of chunk-vec-inVec monomials * e_i  ->
--   basis of chunk-vec-outVec monomials * e_j.
-- Returns a matrix over A.ring (or null on degree mismatch).
moduleBlockPhiMap = (M, i, inVec, j, outVec) -> (
    A := M.dgAlgebra;
    R := A.ring;
    varsA := getVariableChunks A;
    h := #varsA;
    if h != #inVec or h != #outVec then return null;
    srcHomDeg := sum inVec + first M.Degrees#i;
    tgtHomDeg := sum outVec + first M.Degrees#j;
    if srcHomDeg - 1 != tgtHomDeg then return null;
    inBasisMat := if h == 0 then matrix{{1_(A.natural)}}
                  else tensor apply(h, k -> basis(inVec#k, A.natural, Variables => varsA#k));
    outBasisMat := if h == 0 then matrix{{1_(A.natural)}}
                   else tensor apply(h, k -> basis(outVec#k, A.natural, Variables => varsA#k));
    inMons := flatten entries inBasisMat;
    outMons := flatten entries outBasisMat;
    numSrc := #inMons;
    numTgt := #outMons;
    shifts := dd -> -drop(dd, 1);
    srcDegs := apply(inMons, m -> shifts degree(m * (M.natural)_i));
    tgtDegs := apply(outMons, m -> shifts degree(m * (M.natural)_j));
    if numSrc == 0 or numTgt == 0 then
        return map(R^tgtDegs, R^srcDegs, 0);
    mat := mutableMatrix(R, numTgt, numSrc);
    scan(numSrc, c -> (
        m := inMons#c;
        db := moduleLeibniz(M, i, m);
        dbEntries := entries db;
        fj := dbEntries#j;
        if fj == 0 then return;
        coefMat := (coefficients(matrix{{fj}}, Monomials => matrix{outMons}))#1;
        coefMatR := sub(coefMat, R);
        scan(numTgt, a -> (
            cij := coefMatR_(a, 0);
            if cij != 0 then mat_(a, c) = cij;
        ));
    ));
    map(R^tgtDegs, R^srcDegs, matrix mat)
)

-- moduleDegreeLabels(M, n): sorted list of block labels (i, chunkVec) for
-- hom-degree n. A label (i, v) means "generator e_i times an A-monomial of
-- chunk-degree v".
moduleDegreeLabels = (M, n) -> (
    A := M.dgAlgebra;
    sort flatten apply(#M.Degrees, i -> (
        di := first M.Degrees#i;
        needed := n - di;
        if needed < 0 then {}
        else apply(degreeVecs(A, needed), v -> {i, v})
    ))
)

-- moduleBlockDiff(M, n): the hom-degree-n differential of M as a block
-- matrix indexed by module-gen-chunk labels. Parallels blockDiff(A, n).
-- Cached in M.cache#"moduleBlockDiffs".
moduleBlockDiff = method()
moduleBlockDiff (DGModule, ZZ) := (M, n) -> (
    ensureDGAlgebraCaches M;
    if not M.cache#?"moduleBlockDiffs" then
        M.cache#"moduleBlockDiffs" = new MutableHashTable;
    if M.cache#"moduleBlockDiffs"#?n then return M.cache#"moduleBlockDiffs"#n;
    A := M.dgAlgebra;
    R := A.ring;
    srcLabels := moduleDegreeLabels(M, n);
    tgtLabels := moduleDegreeLabels(M, n - 1);
    numSrc := #srcLabels;
    numTgt := #tgtLabels;
    -- Gather per-block matrices.
    tempM := table(numTgt, numSrc, (a, b) -> (
        tgtLbl := tgtLabels#a;
        srcLbl := srcLabels#b;
        moduleBlockPhiMap(M, srcLbl#0, srcLbl#1, tgtLbl#0, tgtLbl#1)
    ));
    -- Helper: build the labeled source free module (one labeled summand per
    -- src label); or labeled target free module. Used to assemble the
    -- direct-sum structure even when tempM has no per-block maps to
    -- derive the components from (empty one side).
    varsA := getVariableChunks A;
    labelFreeModule := (lbls, genIdxPos) -> (
        mods := apply(lbls, lbl -> (
            gi := lbl#(genIdxPos#0);
            v := lbl#(genIdxPos#1);
            basisMat := if #varsA == 0 then matrix{{1_(A.natural)}}
                else tensor apply(#varsA, k -> basis(v#k, A.natural, Variables => varsA#k));
            mons := flatten entries basisMat;
            R^(apply(mons, m -> -drop(degree(m * (M.natural)_gi), 1)))
        ));
        if #lbls == 0 then R^0
        else directSum apply(#lbls, a -> lbls#a => mods#a)
    );
    srcLabelFM := labelFreeModule(srcLabels, (0, 1));
    tgtLabelFM := labelFreeModule(tgtLabels, (0, 1));
    result := if numSrc == 0 or numTgt == 0 then
            map(tgtLabelFM, srcLabelFM, 0)
        else
            map(tgtLabelFM, srcLabelFM, matrix tempM);
    M.cache#"moduleBlockDiffs"#n = result;
    result
)

-- displayModuleBlockDiff(M, n): pretty-print moduleBlockDiff with block
-- labels in the header row / column. Mirrors displayBlockDiff(A, d).
displayModuleBlockDiff = method()
displayModuleBlockDiff (DGModule, ZZ) := (M, n) -> (
    dn := moduleBlockDiff(M, n);
    inputLbls := indices source dn;
    outputLbls := indices target dn;
    if #inputLbls == 0 and #outputLbls == 0 then return "(empty)";
    firstRow := {"."} | inputLbls;
    if #outputLbls == 0 then return netList({firstRow}, Alignment => Center);
    body := apply(outputLbls, o -> (
        {o} | apply(inputLbls, i -> dn^[o]_[i])
    ));
    netList({firstRow} | body, Alignment => Center)
)

-- Expose the underlying label machinery for introspection.
displayModuleBlockDiff (DGModule, List, List) := (M, srcLbl, tgtLbl) -> (
    -- srcLbl is a single label {i, v}; tgtLbl is {j, w}.
    n := (first M.Degrees#(srcLbl#0)) + sum(srcLbl#1);
    dn := moduleBlockDiff(M, n);
    dn_[srcLbl]^[tgtLbl]
)

-------------------------------------------------------------------------
-- DG module element <-> coefficient-over-A.ring bridge.
--
-- A free DG module F = (A.natural)^(-degList) has, at hom-degree n, an
-- A.ring-basis consisting of pairs (i, m) with m an A.natural monomial of
-- hom-degree n - d_i. moduleBasisPairs gives these pairs; the two helpers
-- below convert between Vectors in M.natural and their coefficient
-- columns over A.ring in that basis.
-------------------------------------------------------------------------
-- moduleCoefficients(M, v, n): column vector over A.ring of coefficients
-- of v (a Vector in M.natural, assumed at hom-degree n) in the F_n basis.
moduleCoefficients = method()
moduleCoefficients (DGModule, Vector, ZZ) := (M, v, n) -> (
    if not isFreeModule M.natural then
        error "moduleCoefficients: requires a free DGModule (freeDGModule output).";
    A := M.dgAlgebra;
    R := A.ring;
    pairs := moduleBasisPairs(M, n);
    if #pairs == 0 then return map(R^0, R^1, 0);
    vEntries := entries v;
    numNatGens := rank M.natural;
    byGen := new MutableHashTable;
    scan(numNatGens, i -> byGen#i = {});
    scan(#pairs, k -> (
        (i, mon) := pairs#k;
        byGen#i = append(byGen#i, (k, mon));
    ));
    col := mutableMatrix(R, #pairs, 1);
    scan(numNatGens, i -> (
        fi := vEntries#i;
        if fi == 0 then return;
        rows := byGen#i;
        if #rows == 0 then return;
        tgtMons := apply(rows, pr -> pr#1);
        coefMat := (coefficients(matrix{{fi}}, Monomials => matrix{tgtMons}))#1;
        coefMatR := sub(coefMat, R);
        scan(#rows, idx -> (
            rowIdx := (rows#idx)#0;
            col_(rowIdx, 0) = coefMatR_(idx, 0);
        ));
    ));
    matrix col
)

-- moduleFromCoefficients(M, col, n): reconstitute a Vector in M.natural
-- (at hom-degree n) from a column vector of A.ring coefficients.
moduleFromCoefficients = method()
moduleFromCoefficients (DGModule, Matrix, ZZ) := (M, col, n) -> (
    if not isFreeModule M.natural then
        error "moduleFromCoefficients: requires a free DGModule.";
    pairs := moduleBasisPairs(M, n);
    if #pairs == 0 then return 0_(M.natural);
    natGens := apply(rank M.natural, i -> (M.natural)_i);
    sum apply(#pairs, k -> (
        (i, mon) := pairs#k;
        c := col_(k, 0);
        if c == 0 then 0_(M.natural) else c * mon * natGens#i
    ))
)

-- diff(M, v): apply the DG module differential to a Vector v in M.natural.
-- Uses Leibniz directly (no basis reconstruction needed).
diff (DGModule, Vector) := (M, v) -> (
    A := M.dgAlgebra;
    vEntries := entries v;
    numGens := rank M.natural;
    natGens := apply(numGens, i -> (M.natural)_i);
    sum apply(numGens, i -> (
        fi := vEntries#i;
        if fi == 0 then 0_(M.natural)
        else (
            term1 := polyDifferential(A, fi) * natGens#i;
            signExp := first degree fi;
            sign := if odd signExp then -1 else 1;
            term2 := sign * fi * M.diff#i;
            term1 + term2
        )
    ))
)


-------------------------------------------------------------------------
-- getBoundaryPreimage(DGModule, ...): lift a boundary b in M to a
-- preimage x with d_M(x) = b. Returns a pair
--   (true, preimage)  -- if b is a boundary
--   (false, residue)  -- if not; residue is (b - d_M(liftAttempt))
-- Parallels getBoundaryPreimage(DGAlgebra, ...).
-------------------------------------------------------------------------
getBoundaryPreimage (DGModule, List) := (M, boundaryList) -> (
    if not isFreeModule M.natural then
        error "getBoundaryPreimage DGModule: requires a free DGModule.";
    A := M.dgAlgebra;
    R := A.ring;
    nonzeros := select(boundaryList, b -> b != 0);
    if nonzeros == {} then return (true, boundaryList);
    homDegree := first degree first nonzeros;
    if any(boundaryList, b -> b != 0 and first degree b != homDegree) then
        error "getBoundaryPreimage DGModule: boundary elements must share a hom-degree.";
    srcPairs := moduleBasisPairs(M, homDegree);
    if #srcPairs == 0 then (
        -- No basis at this degree => only zero can be a boundary.
        return (true, apply(#boundaryList, i -> 0_(M.natural)));
    );
    coefCols := apply(boundaryList, b ->
        if b == 0 then map(R^(#srcPairs), R^1, 0) else moduleCoefficients(M, b, homDegree)
    );
    boundaryMat := matrix {coefCols};
    dnplus1 := moduleDifferential(homDegree + 1, M);
    -- Align boundaryMat's target with dnplus1's target so that // sees
    -- a common graded target (moduleCoefficients builds an ungraded
    -- R^(#srcPairs) by default; retarget it to R^(tgtDegs) of dn).
    dn := moduleDifferential(homDegree, M);
    boundaryMat = map(source dn, source boundaryMat, boundaryMat);
    liftMat := boundaryMat // dnplus1;
    residual := boundaryMat - dnplus1 * liftMat;
    if residual != 0 then (
        residueVectors := apply(numcols boundaryMat, j ->
            moduleFromCoefficients(M, residual_{j}, homDegree));
        return (false, residueVectors);
    );
    preimages := apply(numcols boundaryMat, j ->
        moduleFromCoefficients(M, liftMat_{j}, homDegree + 1));
    (true, preimages)
)

getBoundaryPreimage (DGModule, Vector) := (M, b) -> (
    (lifted, myLift) := getBoundaryPreimage(M, {b});
    (lifted, first myLift)
)

-------------------------------------------------------------------------
-- homologyClass(DGModule, Vector): given a cycle z of hom-degree d,
-- return an element of (prune homology(d, M)) representing [z]. Parallels
-- homologyClass(DGAlgebra, RingElement), but returns per-degree pruned
-- homology rather than the full H_*(A)-module (the latter requires a
-- richer cycle-representative cache; see homologyModule DGModule).
-------------------------------------------------------------------------
homologyClass (DGModule, Vector) := (M, z) -> (
    if not isFreeModule M.natural then
        error "homologyClass DGModule: requires a free DGModule.";
    if z == 0 then (
        -- Hom-degree of the zero vector is ambiguous; pick d=0 by convention.
        return 0_(prune homology(0, M));
    );
    d := first degree z;
    -- Verify z is a cycle (Leibniz-based).
    if diff(M, z) != 0 then
        error "homologyClass DGModule: expected a cycle.";
    H := prune homology(d, M);
    if H == 0 then return 0_H;
    zCol := moduleCoefficients(M, z, d);
    pruneMat := matrix H.cache.pruningMap;
    boundaryMat := moduleDifferential(d + 1, M);
    -- Solve zCol = pruneMat * hCoef + boundaryMat * t.
    -- Concatenate and let M2 do the division.
    combined := if numcols boundaryMat == 0 then pruneMat else pruneMat | boundaryMat;
    sol := zCol // combined;
    if combined * sol != zCol then
        error "homologyClass DGModule: failed to express cycle in homology basis (cycle not in image of pruning map + boundaries).";
    hCoef := sol^{0 .. (numcols pruneMat - 1)};
    -- Build the element: sum h_i * H_i. pruneMat has numcols = numgens H.
    sum apply(numcols pruneMat, i -> (
        c := hCoef_(i, 0);
        if c == 0 then 0_H else c * H_i
    ))
)

-------------------------------------------------------------------------
-- moduleMultMap(M, z): the chain map "left multiplication by z" on
-- toComplex M, where z is a cycle of hom-degree d in A.natural. Parallels
-- dgAlgebraMultMap. Source at deg i: F_i; target at deg i+d: F_{i+d}.
-- Leibniz guarantees this is a chain map when z is a cycle.
-------------------------------------------------------------------------
moduleMultMap = method()
moduleMultMap (DGModule, RingElement) := (M, z) -> (
    if not isFreeModule M.natural then
        error "moduleMultMap: requires a free DGModule.";
    A := M.dgAlgebra;
    R := A.ring;
    cxM := toComplex M;
    if z == 0 then
        return map(cxM, cxM, i -> map(cxM_i, cxM_i, 0), Degree => 0);
    d := first degree z;
    cxM2 := cxM ** R^(-(drop(degree z, 1)));
    buildDegMap := i -> (
        srcPairs := moduleBasisPairs(M, i);
        tgtPairs := moduleBasisPairs(M, i + d);
        tgtMod := cxM_(i + d);
        srcMod := cxM2_i;
        if #srcPairs == 0 or #tgtPairs == 0 then
            return map(tgtMod, srcMod, 0);
        tgtByGen := new MutableHashTable;
        scan(rank M.natural, j -> tgtByGen#j = {});
        scan(#tgtPairs, k -> (
            (j, mp) := tgtPairs#k;
            tgtByGen#j = append(tgtByGen#j, (k, mp));
        ));
        mat := mutableMatrix(R, #tgtPairs, #srcPairs);
        scan(#srcPairs, c -> (
            (j, mon) := srcPairs#c;
            zMon := z * mon;
            if zMon == 0 then return;
            rows := tgtByGen#j;
            if #rows == 0 then return;
            tgtMonsJ := apply(rows, pr -> pr#1);
            coefMat := (coefficients(matrix{{zMon}}, Monomials => matrix{tgtMonsJ}))#1;
            coefMatR := sub(coefMat, R);
            scan(#rows, idx -> (
                rowIdx := (rows#idx)#0;
                cij := coefMatR_(idx, 0);
                if cij != 0 then mat_(rowIdx, c) = cij;
            ));
        ));
        map(tgtMod, srcMod, matrix mat)
    );
    map(cxM, cxM2, buildDegMap, Degree => d)
)

-------------------------------------------------------------------------
-- moduleRelationsFromCycleActionDGM: DGModule analog of the
-- ModuleRelationsFromCycleAction helper used by homologyModule. Given a
-- DGModule M and a cycle z in A = M.dgAlgebra, build the action of the
-- homology class [z] on H_*(M) as a map over H_*(A), and return the
-- relation h - z.action (where h is left-mult by [z] in HA).
-------------------------------------------------------------------------
moduleRelationsFromCycleActionDGM = (M, z) -> (
    A := M.dgAlgebra;
    R := A.ring;
    HA := HH(A);
    h := homologyClass(A, z);
    d := first degree z;
    zChainMap := moduleMultMap(M, z);
    cxM := target zChainMap;
    (lo, hi) := concentration cxM;
    pruneHM := new MutableHashTable;
    pruneMapMat := new MutableHashTable;
    for i from lo to hi do (
        pruneHM#i = prune HH_i(cxM);
        pruneMapMat#i = matrix (pruneHM#i).cache.pruningMap;
    );
    actionOnPruned := new MutableHashTable;
    for i from lo to hi - d do (
        hhMat := matrix HH_i(zChainMap);
        composed := hhMat * pruneMapMat#i;
        actionOnPruned#i = composed // pruneMapMat#(i + d);
    );
    ll := hi - lo + 1;
    degsHMTarget := flatten apply(toList(lo..hi),
        p -> apply(degrees pruneHM#p, dd -> {p} | dd));
    degsHMSource := flatten apply(toList(lo..hi),
        p -> apply(degrees pruneHM#p, dd -> ({p} | dd) + degree z));
    buildBlock := (iIdx, jIdx) -> (
        i := lo + iIdx;
        j := lo + jIdx;
        if j + d != i then map(pruneHM#i, pruneHM#j, 0)
        else if not actionOnPruned#?j then map(pruneHM#i, pruneHM#j, 0)
        else map(pruneHM#i, pruneHM#j, actionOnPruned#j)
    );
    matActOfZ := map(HA^(-degsHMTarget), HA^(-degsHMSource),
        tensor(map(HA, R), matrix table(ll, ll, buildBlock)));
    map(HA^(-degsHMTarget), HA^(-degsHMSource), h) - matActOfZ
)

-------------------------------------------------------------------------
-- homologyModule DGModule: generalized to arbitrary free DGModules.
-- Previously restricted to koszulComplexDGM form (via M.module); now
-- also supports freeDGModule-built M with general generator differentials.
--
-- Fast path: when M.?module and d vanishes on every generator, defer to
-- homologyModule(A, M.module) (unchanged, uses existing Koszul-tensor logic).
--
-- General path: iterate over HA.cache.cycles, use moduleMultMap to build
-- the action of each cycle class on toComplex M, and assemble the
-- resulting H_*(A)-module presentation.
-------------------------------------------------------------------------
homologyModule DGModule := M -> (
    A := M.dgAlgebra;
    if M.?module and all(M.diff, z -> z == 0) then
        return homologyModule(A, M.module);
    if not isFreeModule M.natural then
        error "homologyModule DGModule: non-free DGModules with nonzero differentials are not supported.";
    HA := HH(A);
    allActions := apply(HA.cache.cycles, z -> moduleRelationsFromCycleActionDGM(M, z));
    minimalPresentation coker matrix {allActions}
)

-------------------------------------------------------------------------
-- DGModule ** Ring: base change along R -> S. For a free DGModule M
-- over a DGAlgebra A, builds M \otimes_R S = a free DG module over A \otimes S.
-- For koszulComplexDGM-built M, base-changes M.module and rebuilds.
-- Mirrors DGAlgebra ** Ring.
-------------------------------------------------------------------------
DGModule ** Ring := (M, S) -> (
    -- Cache M ** S so repeated base-changes agree (critical for
    -- DGModuleMap ** Ring to produce source/target in the same world).
    if M.cache#?"tensorWithRing" and (M.cache#"tensorWithRing")#?S then
        return (M.cache#"tensorWithRing")#S;
    A := M.dgAlgebra;
    B := A ** S;
    Mnew := null;
    if M.?module and all(M.diff, z -> z == 0) then (
        -- koszulComplexDGM form: base-change the underlying R-module.
        newUnderlying := M.module ** S;
        Mnew = koszulComplexDGM(B, newUnderlying);
    ) else (
        if not isFreeModule M.natural then
            error "DGModule ** Ring: base change requires either koszulComplexDGM form or a free DGModule.";
        -- Free path: lift each generator differential into B.natural.
        Mnew = freeDGModule(B, M.Degrees);
        numG := rank M.natural;
        natGensNew := apply(numG, i -> (Mnew.natural)_i);
        newDiffs := apply(M.diff, v -> (
            if v === 0 then 0_(Mnew.natural)
            else (
                vEntries := entries v;
                sum apply(numG, i -> (
                    fi := vEntries#i;
                    if fi == 0 then 0_(Mnew.natural)
                    else (sub(fi, B.natural)) * natGensNew#i
                ))
            )
        ));
        setDiff(Mnew, newDiffs);
    );
    if not M.cache#?"tensorWithRing" then
        M.cache#"tensorWithRing" = new MutableHashTable;
    (M.cache#"tensorWithRing")#S = Mnew;
    Mnew
)

-------------------------------------------------------------------------
-- DGModule ** DGModule: EXTERIOR tensor product.
--
-- Inputs:  M a free DGModule over A, N a free DGModule over B, where
--          A.ring === B.ring.
-- Output:  a free DGModule P over C = A ** B of rank (rank M)(rank N).
--          Generators: e_i ⊗ f_j for 0 ≤ i < rM, 0 ≤ j < rN, ordered
--          row-major (generator (i, j) has linear index i*rN + j).
--          Multidegree of e_i ⊗ f_j: M.Degrees#i + N.Degrees#j.
--          Differential (Koszul sign):
--            d(e_i ⊗ f_j) = d_M(e_i) ⊗ f_j + (-1)^{|e_i|} e_i ⊗ d_N(f_j)
--          where |e_i| is the hom-degree (first component) of e_i.
--
-- Use cases: build small DGAs and modules over them separately, then
-- assemble their exterior product to get a DG module over A ** B.
-- This differs from the internal tensor product M ⊗_A N (which is
-- NOT what is built here).
--
-- Restrictions:
--   * Both M and N must be free DGModules. koszulComplexDGM-style M
--     with M.module attached is not (yet) supported.
--   * A.ring === B.ring required (matches DGAlgebra **).
-------------------------------------------------------------------------
DGModule ** DGModule := (M, N) -> (
    A := M.dgAlgebra;
    B := N.dgAlgebra;
    if A.ring =!= B.ring then
        error "DGModule **: factors must be DGModules over DGAlgebras sharing a common ground ring.";
    if not isFreeModule M.natural or not isFreeModule N.natural then
        error "DGModule **: both modules must be free DGModules (koszulComplexDGM/cokernel form not supported).";
    -- Cache M ** N on M so that repeated calls return the same DGModule
    -- (important for UX: (F ** G) has source = M ** N, and we want a
    -- fresh `M ** N` call to agree with the internal one).
    if M.cache#?"tensorWith" and (M.cache#"tensorWith")#?N then
        return (M.cache#"tensorWith")#N;
    C := A ** B;
    (iotaA, iotaB) := tensorInclusions C;
    iA := iotaA.natural;  -- RingMap A.natural -> C.natural
    iB := iotaB.natural;  -- RingMap B.natural -> C.natural
    rM := rank M.natural;
    rN := rank N.natural;
    -- Combined generator multidegrees.
    combinedDegrees := flatten apply(rM, i -> apply(rN, j -> (
        dM := M.Degrees#i;
        dN := N.Degrees#j;
        apply(#dM, k -> dM#k + dN#k)
    )));
    P := freeDGModule(C, combinedDegrees);
    Pgens := apply(rank P.natural, k -> (P.natural)_k);
    idxOf := (i, j) -> i * rN + j;
    -- Leibniz differential on each generator (i, j).
    diffs := flatten apply(rM, i -> apply(rN, j -> (
        sign := (-1)^(first M.Degrees#i);
        dMe := M.diff#i;
        firstTerm := if dMe == 0 then 0_(P.natural)
            else (
                entriesM := entries dMe;
                sum apply(rM, k -> (
                    coef := entriesM#k;
                    if coef == 0 then 0_(P.natural)
                    else (iA coef) * Pgens#(idxOf(k, j))
                ))
            );
        dNf := N.diff#j;
        secondTerm := if dNf == 0 then 0_(P.natural)
            else (
                entriesN := entries dNf;
                sum apply(rN, l -> (
                    coef := entriesN#l;
                    if coef == 0 then 0_(P.natural)
                    else sign * (iB coef) * Pgens#(idxOf(i, l))
                ))
            );
        firstTerm + secondTerm
    )));
    setDiff(P, diffs);
    P.cache#"tensorFactors" = (M, N);
    if not M.cache#?"tensorWith" then
        M.cache#"tensorWith" = new MutableHashTable;
    (M.cache#"tensorWith")#N = P;
    P
)

-- ring DGModule / numgens DGModule: trivial accessors
ring DGModule := M -> M.ring

-- maxDegree(M): the max hom-degree in the DG module is the max hom-degree
-- of the ambient DG algebra plus the largest hom-degree shift of a module
-- generator. If an even generator is present in A, A has infinite
-- hom-degree and so does M (return infinity).
maxDegree DGModule := M -> (
    A := M.dgAlgebra;
    aMax := maxDegree A;
    if aMax === infinity then return infinity;
    if #M.Degrees == 0 then return aMax;
    aMax + max(apply(M.Degrees, d -> first d))
)

-- getBasis(n, M): an A.ring-basis of the hom-degree-n piece of M.natural.
-- Delegates to basis on the underlying A.natural-module.
getBasis (ZZ, DGModule) := opts -> (n, M) -> (
    if opts.Limit == -1 then basis(n, M.natural, Variables => 0 .. numgens (M.dgAlgebra).natural - 1)
    else basis(n, M.natural, Limit => opts.Limit, Variables => 0 .. numgens (M.dgAlgebra).natural - 1)
)

-- toComplex(M, n): the chain complex of underlying A.ring-modules,
-- truncated at hom-degree n.
-- toComplex(M): use M's maxDegree (errors if infinite, matching DGAlgebra).
toComplex DGModule := M -> (
    maxDeg := maxDegree M;
    if maxDeg === infinity then
        error "Must specify an upper degree bound if an even generator exists in underlying DG algebra.";
    toComplex(M, maxDeg)
)
toComplex (DGModule, ZZ) := (M, n) -> (
    if n <= 0 then complex({moduleDifferential(0, M)})
    else complex(apply(n, i -> moduleDifferential(i+1, M)))
)

-- dgComplex(M): lazily-cached Complex of free A.ring-modules for M.
-- Mirrors dgComplex(DGAlgebra). Invalidation: clear M.cache.dgComplex.
dgComplex DGModule := M -> (
    ensureDGAlgebraCaches M;
    if M.cache#?(symbol dgComplex) then return M.cache#(symbol dgComplex);
    n := maxDegree M;
    if n === infinity then (
        A := M.dgAlgebra;
        n = sum select(degrees A.natural / first, i -> odd i) + 1;
    );
    C := toComplex(M, max(n, 1));
    M.cache#(symbol dgComplex) = C;
    C
)

-- homology(M) unary: the full H_*(A)-module structure on H_*(M), matching
-- the DGAlgebra convention that HH with no integer returns the homology
-- algebra (line 544: `homology DGAlgebra := A -> homologyAlgebra(A)`).
-- For a per-degree R-module, use homology(n, M).
-- (homologyModule DGModule is defined below alongside the general-path
-- machinery for non-trivial generator differentials.)
homology DGModule := opts -> M -> homologyModule M

-- isAcyclic(M): H_i(M) = 0 for i >= 1 up to a finite bound. Requires
-- EndDegree when M has infinite hom-degree, matching isAcyclic DGAlgebra.
isAcyclic DGModule := opts -> M -> (
    endDegree := maxDegree M;
    if endDegree === infinity and opts.EndDegree == -1 then
        error "Must supply an upper bound to check for acyclicity.";
    if opts.EndDegree != -1 then endDegree = opts.EndDegree;
    not any(1..endDegree, i -> prune homology(i, M) != 0)
)

-- isValidDGModule(M): structural-invariant check on the stored object.
-- Parallels isValidDGAlgebra: required keys with expected types, and
-- generator-degree list length matches differential list length. Does
-- not check d_M^2 = 0 (see isWellDefinedDifferential DGModule).
isValidDGModule = method(TypicalValue => Boolean)
isValidDGModule DGModule := M -> (
    if not M#?(symbol dgAlgebra) or not instance(M#(symbol dgAlgebra), DGAlgebra) then return false;
    if not isValidDGAlgebra M.dgAlgebra then return false;
    if not M#?(symbol ring) or not instance(M#(symbol ring), Ring) then return false;
    if not M#?(symbol natural) or not instance(M#(symbol natural), Module) then return false;
    if not M#?(symbol Degrees) or not instance(M#(symbol Degrees), List) then return false;
    if not M#?(symbol diff) or not instance(M#(symbol diff), List) then return false;
    if #M.diff != #M.Degrees then return false;
    true
)

-- isWellDefined DGModule.  Analog of isWellDefined DGAlgebra: checks
-- keys/types via isValidDGModule, then verifies the ambient DGAlgebra is
-- well-defined and d_M^2 = 0 up to maxDegree.  Diagnostic messages
-- emitted when debugLevel > 0.
isWellDefined DGModule := M -> (
    if not isValidDGModule M then (
        if debugLevel > 0 then (
            missing := select(
                {symbol dgAlgebra, symbol ring, symbol natural, symbol Degrees, symbol diff},
                s -> not M#?s);
            if #missing > 0 then
                << "-- DGModule: missing required key(s): " << toString missing << endl;
            if M#?(symbol diff) and M#?(symbol Degrees) and #M.diff != #M.Degrees then
                << "-- DGModule: #M.diff (" << #M.diff << ") != #M.Degrees (" << #M.Degrees << ")" << endl;
        );
        return false;
    );
    if not isWellDefined M.dgAlgebra then (
        if debugLevel > 0 then << "-- DGModule: M.dgAlgebra is not a well-defined DGAlgebra" << endl;
        return false;
    );
    if M.ring =!= M.dgAlgebra.ring then (
        if debugLevel > 0 then << "-- DGModule: M.ring does not equal M.dgAlgebra.ring" << endl;
        return false;
    );
    -- Each entry of M.diff must be an element of M.natural (or coercible).
    for i from 0 to #M.diff - 1 do (
        d := M.diff#i;
        try (
            -- Test coercion: the entry should be in M.natural's module structure.
            if not (instance(d, Vector) or instance(d, Matrix) or d == 0_(M.natural)) then (
                if debugLevel > 0 then
                    << "-- DGModule: M.diff#" << i << " is not a module element of M.natural" << endl;
                return false;
            );
        ) else (
            if debugLevel > 0 then
                << "-- DGModule: M.diff#" << i << " cannot be interpreted in M.natural" << endl;
            return false;
        );
    );
    -- Semantic: d_M^2 = 0 up to a finite bound.
    if not isWellDefinedDifferential M then (
        if debugLevel > 0 then << "-- DGModule: d_M^2 fails to vanish" << endl;
        return false;
    );
    true
)

-- isWellDefinedDifferential(M): semantic check d_M^2 = 0.
-- For a DG module we need this on every hom-degree up to some bound,
-- since Leibniz-via-A.diff does not reduce the check to generators alone:
-- d_M(a x) = d_A(a) x + (-1)^|a| a d_M(x) — even if d_M vanishes on x,
-- we must know d_A^2 = 0 on A to conclude d_M^2 = 0 globally, and in the
-- general case the generator differentials can mix. We therefore check
-- d_{n-1} d_n = 0 up to maxDegree.
isWellDefinedDifferential DGModule := M -> (
    if not isValidDGModule M then return false;
    if not isWellDefinedDifferential M.dgAlgebra then return false;
    n := maxDegree M;
    if n === infinity then (
        A := M.dgAlgebra;
        n = sum select(degrees A.natural / first, i -> odd i) + 1;
    );
    all(1..max(n,1), i -> moduleDifferential(i-1, M) * moduleDifferential(i, M) == 0)
)

-- ensureDGAlgebraCaches(M) / invalidateDGAlgebraCache(M): parallel to
-- the DGAlgebra versions. Creates the expected cache subtables so later
-- code can assume they exist; invalidation wipes derived state.
ensureDGAlgebraCaches DGModule := M -> (
    if not M#?(symbol cache)
       or M#(symbol cache) === null
       or not instance(M#(symbol cache), CacheTable)
    then M#(symbol cache) = new CacheTable;
    if not M.cache#?(symbol diffs)
       or not instance(M.cache#(symbol diffs), MutableHashTable)
    then M.cache#(symbol diffs) = new MutableHashTable;
    M
)

invalidateDGAlgebraCache DGModule := M -> (
    ensureDGAlgebraCaches M;
    M.cache#(symbol diffs) = new MutableHashTable;
    if M.cache#?(symbol dgComplex) then remove(M.cache, symbol dgComplex);
    if M.cache#?"moduleBlockDiffs" then remove(M.cache, "moduleBlockDiffs");
    M
)

-------------------------------------------------------------------------
-- DGModuleMap: morphisms of DG modules over a common DGAlgebra A.
--
-- A DG module map f: M -> N of hom-degree 0 is:
--   * A-linear:    f(a . m) = a . f(m)          for a in A.natural, m in M
--   * chain map:   d_N( f(m) ) = f( d_M(m) )
-- Only same-DGA maps (M.dgAlgebra === N.dgAlgebra) are supported. Degree-
-- shifted variants and change-of-algebra maps are deferred (moduleMultMap
-- already provides the degree-shifted ComplexMap use case directly).
--
-- A-linearity means the action on a general element is determined by the
-- images of the natural generators of M:
--   f( sum a_i e_i ) = sum a_i f(e_i)
-- So the underlying data is stored as a Matrix N.natural <- M.natural
-- (A.natural-linear), whose i-th column is f(e_i).
-------------------------------------------------------------------------

net DGModuleMap := f -> (
    hdr := {net "Source => " | net f.source.natural};
    hdr = hdr | {net "Target => " | net f.target.natural};
    hdr = hdr | {net "Natural => " | net f.natural};
    horizontalJoin flatten ("{", stack hdr, "}")
)

target DGModuleMap := f -> f.target
source DGModuleMap := f -> f.source

-- Internal helper: build a Matrix N.natural <- (A.ring)^(#imgList) whose
-- columns are the given Vectors in N.natural. Handles zero entries.
vectorsToColumnMatrix = (N, imgList) -> (
    if #imgList == 0 then map(N.natural, (ring N.natural)^0, 0)
    else (
        -- Each Vector's entries are elements of the ambient ring of N.natural
        -- (i.e. A.natural); transposing makes them columns.
        rows := apply(imgList, v -> if v === 0 then apply(rank ambient N.natural, i -> 0) else entries v);
        transpose matrix rows
    )
)

dgModuleMap = method(TypicalValue => DGModuleMap)

-- Primary constructor: image matrix is an A.natural-linear Matrix
-- N.natural <- M.natural, whose i-th column gives f(e_i).
dgModuleMap (DGModule, DGModule, Matrix) := (N, M, img) -> (
    if M.dgAlgebra =!= N.dgAlgebra then
        error "dgModuleMap: source and target must share the same DG algebra.";
    A := M.dgAlgebra;
    -- Coerce the caller-supplied matrix into a proper map N.natural <- M.natural.
    natMap := try map(N.natural, M.natural, img) else
        error "dgModuleMap: could not coerce matrix into a map N.natural <- M.natural.";
    f := new MutableHashTable;
    f#(symbol source)    = M;
    f#(symbol target)    = N;
    f#(symbol dgAlgebra) = A;
    f#(symbol natural)   = natMap;
    f#(symbol cache)     = new CacheTable;
    new DGModuleMap from f
)

-- Convenience constructor: imgList is a list of Vectors in N.natural (or
-- 0), one per M-generator.
dgModuleMap (DGModule, DGModule, List) := (N, M, imgList) -> (
    if M.dgAlgebra =!= N.dgAlgebra then
        error "dgModuleMap: source and target must share the same DG algebra.";
    if #imgList != #M.Degrees then
        error ("dgModuleMap: expected " | toString(#M.Degrees)
            | " image entries (one per source generator); got " | toString(#imgList) | ".");
    -- Coerce entries to Vectors in N.natural.
    coerced := apply(imgList, v -> (
        if v === 0 then 0_(N.natural)
        else if instance(v, Vector) then v
        else try (sub(v, N.natural)) else
            error "dgModuleMap: could not coerce image entry to N.natural."
    ));
    mat := vectorsToColumnMatrix(N, coerced);
    dgModuleMap(N, M, mat)
)

-- identityDGModuleMap M: the identity map M -> M. Useful as the base of
-- recursive constructions and for testing isWellDefined.
identityDGModuleMap = method(TypicalValue => DGModuleMap)
identityDGModuleMap DGModule := M -> dgModuleMap(M, M, id_(M.natural))

-- zeroDGModuleMap(N, M): the zero map M -> N.
zeroDGModuleMap = method(TypicalValue => DGModuleMap)
zeroDGModuleMap (DGModule, DGModule) := (N, M) -> (
    if M.dgAlgebra =!= N.dgAlgebra then
        error "zeroDGModuleMap: source and target must share the same DG algebra.";
    dgModuleMap(N, M, map(N.natural, M.natural, 0))
)

-- Apply a DGModuleMap to an element of M.natural. Returns an element of
-- N.natural. Because f is A-linear, f(v) = f.natural * v.
DGModuleMap Vector := (f, v) -> f.natural * v

-- Composition: for g: L -> M and f: M -> N, build f * g: L -> N.
DGModuleMap * DGModuleMap := (f, g) -> (
    if target g =!= source f then
        error "DGModuleMap composition: target of right operand must equal source of left operand.";
    dgModuleMap(target f, source g, f.natural * g.natural)
)

-- Scalar multiplication: an element of A.ring scales a DGModuleMap.
RingElement * DGModuleMap := (r, f) -> (
    A := f.dgAlgebra;
    if ring r =!= A.ring and ring r =!= A.natural then
        error "RingElement * DGModuleMap: scalar ring must agree with A.ring or A.natural.";
    rA := if ring r === A.natural then r else sub(r, A.natural);
    dgModuleMap(target f, source f, rA * f.natural)
)

-- Addition (same source, same target).
DGModuleMap + DGModuleMap := (f, g) -> (
    if source f =!= source g or target f =!= target g then
        error "DGModuleMap +: both maps must have the same source and target.";
    dgModuleMap(target f, source f, f.natural + g.natural)
)

DGModuleMap - DGModuleMap := (f, g) -> (
    if source f =!= source g or target f =!= target g then
        error "DGModuleMap -: both maps must have the same source and target.";
    dgModuleMap(target f, source f, f.natural - g.natural)
)

-- Negation.
- DGModuleMap := f -> dgModuleMap(target f, source f, - f.natural)

-- DGModuleMap ** DGModuleMap: exterior tensor product of chain maps.
--
-- Given F : M -> M' over A and G : N -> N' over B (with A.ring = B.ring),
-- build F ** G : M ** N -> M' ** N' over A ** B, acting on generators
-- by (e_i ⊗ f_j) -> F(e_i) ⊗ G(f_j).
--
-- Concretely: F.natural is rM' × rM over A.natural, G.natural is rN' × rN
-- over B.natural. The tensor product has image vectors indexed by (k, l)
-- in the target's generator set of size rM' * rN', with entries
--   coef((e_i ⊗ f_j) -> (e'_k ⊗ f'_l)) =
--       iA(F_{k,i}) * iB(G_{l,j})
-- where iA, iB are the tensor inclusions into the target's DG algebra.
DGModuleMap ** DGModuleMap := (F, G) -> (
    M := source F; Mp := target F;
    N := source G; Np := target G;
    if (M.dgAlgebra).ring =!= (N.dgAlgebra).ring or
       (Mp.dgAlgebra).ring =!= (Np.dgAlgebra).ring then
        error "DGModuleMap **: factors must share a common ground ring on each side.";
    P := M ** N;
    Pp := Mp ** Np;
    Cp := Pp.dgAlgebra;
    (iotaAp, iotaBp) := tensorInclusions Cp;
    iAp := iotaAp.natural;
    iBp := iotaBp.natural;
    rM := rank M.natural; rMp := rank Mp.natural;
    rN := rank N.natural; rNp := rank Np.natural;
    Fmat := F.natural;
    Gmat := G.natural;
    -- Build the image columns.  Column (i,j) of the tensor map sends
    -- e_i ⊗ f_j to Σ_{k,l} iA(F_{k,i}) iB(G_{l,j}) (e'_k ⊗ f'_l).
    Pgens := apply(rank Pp.natural, m -> (Pp.natural)_m);
    idxOf := (k, l) -> k * rNp + l;
    imgCols := flatten apply(rM, i -> apply(rN, j -> (
        sum apply(rMp, k -> sum apply(rNp, l -> (
            fki := Fmat_(k, i);
            glj := Gmat_(l, j);
            if fki == 0 or glj == 0 then 0_(Pp.natural)
            else (iAp fki) * (iBp glj) * Pgens#(idxOf(k, l))
        )))
    )));
    dgModuleMap(Pp, P, imgCols)
)

-- Equality: same source and target DGModules, and equal natural matrices.
DGModuleMap == DGModuleMap := (f, g) -> (
    source f === source g
    and target f === target g
    and f.natural == g.natural
)

-- Right-scalar multiplication by a ring element (mirrors left version).
DGModuleMap * RingElement := (f, r) -> r * f

-- ZZ / QQ / Number scalar multiplication (coerced through the DG algebra's
-- underlying polynomial ring so arithmetic lives in A.natural).
ZZ * DGModuleMap     := (n, f) -> (sub(n, (f.dgAlgebra).natural)) * f
QQ * DGModuleMap     := (q, f) -> (sub(q, (f.dgAlgebra).natural)) * f
Number * DGModuleMap := (n, f) -> (sub(n, (f.dgAlgebra).natural)) * f
DGModuleMap * ZZ     := (f, n) -> n * f
DGModuleMap * QQ     := (f, q) -> q * f
DGModuleMap * Number := (f, n) -> n * f

-- Compare a DGModuleMap to 0 (zero map) or 1 (identity on source = target).
DGModuleMap == ZZ := (f, n) -> (
    if n == 0 then f.natural == 0
    else if n == 1 then (source f === target f and f.natural == id_((source f).natural))
    else error "DGModuleMap == ZZ: comparison only defined against 0 or 1."
)
ZZ == DGModuleMap := (n, f) -> f == n

-- Base ring of the underlying DG algebra.
ring DGModuleMap := f -> (f.dgAlgebra).ring

-- Chain-degree of a DGModuleMap: our DGModuleMaps are always degree-0
-- chain maps (they commute with d strictly).
degree DGModuleMap := f -> 0

-- Injective / surjective / isomorphism tests go through the DGSubmodule
-- and DGQuotientModule machinery.
isInjective DGModuleMap := f -> isZero kernel f
isSurjective DGModuleMap := f -> isZero cokernel f
isIsomorphism DGModuleMap := f -> isInjective f and isSurjective f

-- id_(M) for a DGModule M -- same dispatch pattern used by Complexes for
-- id_(C).  Produces the identity DGModuleMap M -> M.
DGModule#id = M -> identityDGModuleMap M

-- isHomogeneous: true iff f.natural is homogeneous as an A.natural-matrix.
isHomogeneous DGModuleMap := f -> isHomogeneous f.natural

-- map(N, M, ZZ): degree-shifted identity / zero.  Matches the ComplexMap
-- constructor map(D, C, i) where i=0 builds the identity (and requires
-- D == C) and i=1 is allowed only when both sides agree generator-by-
-- generator. For simplicity we support the two common cases:
--    map(M, M, 1)  ~ identityDGModuleMap M
--    map(N, M, 0)  ~ zeroDGModuleMap(M, N)
-- (Note the ComplexMap convention: the target comes first.)
map (DGModule, DGModule, ZZ) := o -> (N, M, i) -> (
    if i == 0 then zeroDGModuleMap(N, M)
    else if i == 1 then (
        if N =!= M then
            error "map(N, M, 1): identity requested but source and target differ.";
        identityDGModuleMap M
    )
    else error "map(DGModule, DGModule, ZZ): only 0 (zero map) and 1 (identity) are supported."
)

-- isQuasiIsomorphism f: defer to the ComplexMap version applied to
-- toComplexMap f.  Inherits the Concentration option from Complexes.
isQuasiIsomorphism DGModuleMap := o -> f -> isQuasiIsomorphism(toComplexMap f, o)

-- isQuasiIsomorphism for DGAlgebraMap, same strategy.
isQuasiIsomorphism DGAlgebraMap := o -> f -> isQuasiIsomorphism(toComplexMap f, o)

-- isWellDefined DGModuleMap.  Modeled on isWellDefined ComplexMap
-- (Complexes/ChainComplexMap.m2 line 134).  Checks:
--   (1) expected key shape (source, target, dgAlgebra, natural, cache),
--   (2) types of each slot,
--   (3) source/target share the same DGAlgebra,
--   (4) f.natural is a module map with the right source/target modules,
--   (5) hom-degrees are preserved (f is a hom-degree-0 chain map),
--   (6) chain-map condition on every generator: d_N(f(e_i)) == f(d_M(e_i)).
-- Diagnostic messages emitted when debugLevel > 0.
isWellDefined DGModuleMap := f -> (
    k := keys f;
    expectedKeys := set {symbol source, symbol target, symbol dgAlgebra, symbol natural, symbol cache};
    if set k =!= expectedKeys then (
        if debugLevel > 0 then (
            added := toList(k - expectedKeys);
            missing := toList(expectedKeys - k);
            if #added > 0 then << "-- DGModuleMap: unexpected key(s): " << toString added << endl;
            if #missing > 0 then << "-- DGModuleMap: missing key(s): " << toString missing << endl;
        );
        return false;
    );
    if not (instance(f.source, DGModule) or instance(f.source, DGQuotientModule)) then (
        if debugLevel > 0 then << "-- DGModuleMap: f.source is not a DGModule or DGQuotientModule" << endl;
        return false;
    );
    if not (instance(f.target, DGModule) or instance(f.target, DGQuotientModule)) then (
        if debugLevel > 0 then << "-- DGModuleMap: f.target is not a DGModule or DGQuotientModule" << endl;
        return false;
    );
    if instance(f.source, DGModule) and not isWellDefined f.source then (
        if debugLevel > 0 then << "-- DGModuleMap: f.source is not a well-defined DGModule" << endl;
        return false;
    );
    if instance(f.source, DGQuotientModule) and not isWellDefined f.source then (
        if debugLevel > 0 then << "-- DGModuleMap: f.source is not a well-defined DGQuotientModule" << endl;
        return false;
    );
    -- DGQuotientModule targets go through a slightly different path; we
    -- still require the ambient to be well-defined.
    if instance(f.target, DGModule) and not isWellDefined f.target then (
        if debugLevel > 0 then << "-- DGModuleMap: f.target is not a well-defined DGModule" << endl;
        return false;
    );
    if instance(f.target, DGQuotientModule) and not isWellDefined f.target then (
        if debugLevel > 0 then << "-- DGModuleMap: f.target is not a well-defined DGQuotientModule" << endl;
        return false;
    );
    if f.source.dgAlgebra =!= f.target.dgAlgebra then (
        if debugLevel > 0 then << "-- DGModuleMap: source and target must share the same DGAlgebra" << endl;
        return false;
    );
    if f.dgAlgebra =!= f.source.dgAlgebra then (
        if debugLevel > 0 then << "-- DGModuleMap: f.dgAlgebra disagrees with f.source.dgAlgebra" << endl;
        return false;
    );
    if not instance(f.natural, Matrix) then (
        if debugLevel > 0 then << "-- DGModuleMap: f.natural is not a Matrix" << endl;
        return false;
    );
    if source f.natural =!= (f.source).natural then (
        if debugLevel > 0 then << "-- DGModuleMap: source of f.natural is not f.source.natural" << endl;
        return false;
    );
    if target f.natural =!= (f.target).natural then (
        if debugLevel > 0 then << "-- DGModuleMap: target of f.natural is not f.target.natural" << endl;
        return false;
    );
    M := source f;
    N := target f;
    -- Hom-degree check: for each M-generator e_i, f(e_i) must sit in
    -- hom-degree (first M.Degrees#i) of N.natural.  Internal-degree shifts
    -- are allowed (only the first component matters).
    homDegOk := all(#M.Degrees, i -> (
        fei := f.natural * (M.natural)_i;
        if fei == 0 then true
        else first degree fei == first M.Degrees#i
    ));
    if not homDegOk then (
        if debugLevel > 0 then << "-- DGModuleMap: f fails to preserve homological degree on some generator" << endl;
        return false;
    );
    -- Chain-map condition, generator by generator.  diff(N, ·) calls
    -- on DGModule or DGQuotientModule (both supply the Leibniz formula).
    ok := all(#M.Degrees, i -> (
        ei    := (M.natural)_i;
        fei   := f.natural * ei;
        dMei  := M.diff#i;
        dNfei := diff(N, fei);
        fdMei := f.natural * dMei;
        dNfei == fdMei
    ));
    if not ok and debugLevel > 0 then
        << "-- DGModuleMap: chain-map condition d_N(f e_i) = f(d_M e_i) fails on some generator" << endl;
    ok
)

-- toComplexMap(f, n): the n-th component of f as an A.ring-linear map
-- F_n(M) -> F_n(N), where F_n(M) is the hom-deg-n piece of M.natural
-- expressed in its A.ring-basis (moduleBasisPairs).
--
-- Strategy: enumerate srcPairs = basis of F_n(M); for each (i, mon) in
-- srcPairs, compute f(mon * e_i) = mon * (f.natural * e_i) in N.natural,
-- then decompose over the target basis.
toComplexMap (DGModuleMap, ZZ) := opts -> (f, n) -> (
    M := source f;
    N := target f;
    A := M.dgAlgebra;
    R := A.ring;
    srcPairs := moduleBasisPairs(M, n);
    tgtPairs := moduleBasisPairs(N, n);
    -- Source/target free R-modules are canonical via moduleDifferential's
    -- source/target, but we build them independently to avoid forcing the
    -- differential computation just for module shape.
    srcDeg := p -> (
        (i, m) := p;
        totalDeg := degree(m * (M.natural)_i);
        -drop(totalDeg, 1)
    );
    tgtDeg := p -> (
        (j, m) := p;
        totalDeg := degree(m * (N.natural)_j);
        -drop(totalDeg, 1)
    );
    srcDegs := apply(srcPairs, srcDeg);
    tgtDegs := apply(tgtPairs, tgtDeg);
    srcMod := R^srcDegs;
    tgtMod := R^tgtDegs;
    if #srcPairs == 0 or #tgtPairs == 0 then
        return map(tgtMod, srcMod, 0);
    numSrc := #srcPairs;
    numTgt := #tgtPairs;
    -- Precompute f(e_i) as Vectors in N.natural.
    fImages := apply(rank M.natural, i -> f.natural * (M.natural)_i);
    -- Group target pairs by N-generator index.
    tgtByGen := new MutableHashTable;
    scan(rank N.natural, j -> tgtByGen#j = {});
    scan(numTgt, k -> (
        (j, mp) := tgtPairs#k;
        tgtByGen#j = append(tgtByGen#j, (k, mp));
    ));
    mat := mutableMatrix(R, numTgt, numSrc);
    scan(numSrc, c -> (
        (i, mon) := srcPairs#c;
        imgVec := mon * fImages#i;
        if imgVec == 0 then return;
        imgEntries := entries imgVec;
        scan(rank N.natural, j -> (
            fj := imgEntries#j;
            if fj == 0 then return;
            rowsForJ := tgtByGen#j;
            if #rowsForJ == 0 then return;
            tgtMonsJ := apply(rowsForJ, pr -> pr#1);
            coefMat := (coefficients(matrix{{fj}}, Monomials => matrix{tgtMonsJ}))#1;
            coefMatR := sub(coefMat, R);
            scan(#rowsForJ, idx -> (
                rowIdx := (rowsForJ#idx)#0;
                cij := coefMatR_(idx, 0);
                if cij != 0 then mat_(rowIdx, c) = cij;
            ));
        ));
    ));
    map(tgtMod, srcMod, matrix mat)
)
-- toComplexMap(f, n) returns a Matrix (per-degree component), while the
-- method's default TypicalValue is ComplexMap.  Inform the documentation
-- system of the correct per-signature return type.
typicalValues#(toComplexMap, DGModuleMap, ZZ) = Matrix

-- toComplexMap f: assemble all per-degree pieces into a ComplexMap
-- toComplex M -> toComplex N. An EndDegree bound is required when either
-- side has infinite hom-degree.
toComplexMap DGModuleMap := opts -> f -> (
    M := source f;
    N := target f;
    -- Specialized branch: target is a DGQuotientModule.  At the DGModule
    -- level a DGQuotientModule has no free basis for `moduleBasisPairs`,
    -- so the generic per-degree assembly below would give the wrong
    -- answer.  For the canonical projection f = Q.projection the chain
    -- map is exactly the induced quotient map from the ambient complex
    -- to the cokernel of the inclusion.  We only support that case.
    if instance(N, DGQuotientModule) then (
        if not (N.?projection and f === N.projection) then
            error "toComplexMap: into a DGQuotientModule only the canonical projection is supported.";
        cxI := toComplexMap(N.subDGModule.inclusion, opts);
        return inducedMap(cokernel cxI, target cxI);
    );
    mDeg := maxDegree M;
    nDeg := maxDegree N;
    maxDeg := if mDeg === infinity or nDeg === infinity then infinity
              else max(mDeg, nDeg);
    if opts.EndDegree != -1 then maxDeg = opts.EndDegree;
    if maxDeg === infinity then
        error "toComplexMap DGModuleMap: hom-degrees are unbounded; supply EndDegree.";
    if opts.AssertWellDefined then assert isWellDefined f;
    cxM := toComplex(M, maxDeg);
    cxN := toComplex(N, maxDeg);
    map(cxN, cxM, i -> toComplexMap(f, i, opts), Degree => 0)
)

-- homology(f, n): the induced map H_n(M) -> H_n(N) as an R-module map.
homology (DGModuleMap, ZZ) := opts -> (f, n) -> (
    M := source f;
    N := target f;
    dMn    := moduleDifferential(n, M);
    dMnp1  := moduleDifferential(n + 1, M);
    dNn    := moduleDifferential(n, N);
    dNnp1  := moduleDifferential(n + 1, N);
    hM := homology(dMn, dMnp1);
    hN := homology(dNn, dNnp1);
    cm := toComplexMap(f, n);
    inducedMap(hN, hM, cm)
)

-- homology f: the induced H_*(A)-module map H_*(M) -> H_*(N).
--
-- Conventions mirror `homology DGAlgebraMap` (which returns a RingMap
-- HA -> HB). For f : M -> N a DGModuleMap, the underlying A.natural-
-- linear chain map induces an HA-linear map on H_*(-). We assemble that
-- map degree-by-degree from the pruned per-degree homologies of
-- toComplexMap f, then tensor up to HA to match the free HA-presentation
-- used by `homologyModule DGModule`.
--
-- For users who want the bare ComplexMap of homologies (no HA-module
-- structure), use `HH toComplexMap f` directly.
--
-- Restriction: currently requires both M.natural and N.natural to be
-- free A.natural-modules (this matches the general path of
-- homologyModule DGModule). The koszulComplexDGM-with-M.module fast
-- path is not yet handled here.
homology DGModuleMap := opts -> f -> (
    M := source f;
    N := target f;
    if M.dgAlgebra =!= N.dgAlgebra then
        error "homology DGModuleMap: source and target must share a DG algebra.";
    A := M.dgAlgebra;
    R := A.ring;
    HA := HH(A);
    if not isFreeModule M.natural or not isFreeModule N.natural then
        error "homology DGModuleMap: currently requires both source and target to be free DGModules (koszulComplexDGM form not yet supported).";
    cm := toComplexMap f;
    cxM := source cm;
    cxN := target cm;
    (loM, hiM) := concentration cxM;
    (loN, hiN) := concentration cxN;
    -- Pruned per-degree homologies on each side (matches the basis used
    -- by moduleRelationsFromCycleActionDGM, so degrees line up).
    pruneHM := new MutableHashTable;
    for i from loM to hiM do pruneHM#i = prune HH_i(cxM);
    pruneHN := new MutableHashTable;
    for j from loN to hiN do pruneHN#j = prune HH_j(cxN);
    -- Pruned per-degree maps induced by f.
    prunedMap := new MutableHashTable;
    for i from max(loM, loN) to min(hiM, hiN) do
        prunedMap#i = prune HH_i(cm);
    -- Target-degree lists of the raw HA-free modules covering
    -- homologyModule M and homologyModule N, respectively.
    degsHMSource := flatten apply(toList(loM..hiM),
        p -> apply(degrees pruneHM#p, dd -> {p} | dd));
    degsHNTarget := flatten apply(toList(loN..hiN),
        p -> apply(degrees pruneHN#p, dd -> {p} | dd));
    rowsCount := hiN - loN + 1;
    colsCount := hiM - loM + 1;
    buildBlock := (rowIdx, colIdx) -> (
        i := loM + colIdx;
        j := loN + rowIdx;
        if i != j or not prunedMap#?i then
            map(pruneHN#j, pruneHM#i, 0)
        else (
            pm := prunedMap#i;
            -- Make sure source / target agree with the pruneHM / pruneHN
            -- objects (they usually do via M2's caching, but coerce to
            -- be safe).
            if source pm === pruneHM#i and target pm === pruneHN#j then pm
            else map(pruneHN#j, pruneHM#i, matrix pm)
        )
    );
    rawBlock := if rowsCount == 0 or colsCount == 0 then
        map(R^(-degsHNTarget), R^(-degsHMSource), 0)
    else
        matrix table(rowsCount, colsCount, buildBlock);
    -- Base-change to HA.
    haBlock := map(HA^(-degsHNTarget), HA^(-degsHMSource),
        tensor(map(HA, R), rawBlock));
    -- Build the raw cokers on each side (same construction as
    -- homologyModule DGModule, but without minimalPresentation), then
    -- induce the map and prune to match homologyModule's output.
    HMraw := coker matrix {apply(HA.cache.cycles,
        z -> moduleRelationsFromCycleActionDGM(M, z))};
    HNraw := coker matrix {apply(HA.cache.cycles,
        z -> moduleRelationsFromCycleActionDGM(N, z))};
    raw := map(HNraw, HMraw, haBlock);
    minimalPresentation raw
)

-------------------------------------------------------------------------
-- liftToDGModuleMap(N, M, h0): lift an R-module map h0 on the degree-0
-- generator piece of M to a full DGModuleMap M -> N.
--
-- Requires:
--   * M semifree (M.natural a free A.natural-module).
--   * N acyclic in positive hom-degrees up to EndDegree (so successive
--     Leibniz-style lifts exist).
-- h0 is a Matrix over A.natural with:
--   * rows indexing natural generators of N
--   * columns indexing the hom-degree-0 generators of M (in positional
--     order within M.Degrees)
-- For each hom-degree d >= 1, we lift generator-by-generator: given an
-- M-generator e_i of hom-degree d, compute bd = f_partial( d_M(e_i) ) in
-- hom-degree (d-1) of N.natural, then find an x in hom-degree d of
-- N.natural with d_N(x) = bd. x becomes f(e_i).
--
-- EndDegree caps the lift; M-generators of hom-degree > EndDegree are
-- sent to 0 (and the result may fail isWellDefined beyond EndDegree).
-------------------------------------------------------------------------
liftToDGModuleMap = method(TypicalValue => DGModuleMap, Options => {EndDegree => -1})
liftToDGModuleMap (DGModule, DGModule, Matrix) := opts -> (N, M, h0) -> (
    if M.dgAlgebra =!= N.dgAlgebra then
        error "liftToDGModuleMap: source and target must share the same DG algebra.";
    if not isFreeModule M.natural then
        error "liftToDGModuleMap: source M must be semifree (use freeDGModule/adjoinGenerators).";
    numGenM := #M.Degrees;
    genDegs := apply(M.Degrees, first);
    maxDeg := if #genDegs == 0 then 0 else max genDegs;
    if opts.EndDegree != -1 then maxDeg = opts.EndDegree;
    -- Group M-generator indices by hom-degree.
    genIdxByDeg := new MutableHashTable;
    scan(numGenM, i -> (
        d := genDegs#i;
        if not genIdxByDeg#?d then genIdxByDeg#d = {};
        genIdxByDeg#d = append(genIdxByDeg#d, i);
    ));
    deg0Indices := if genIdxByDeg#?0 then genIdxByDeg#0 else {};
    if numcols h0 != #deg0Indices then
        error ("liftToDGModuleMap: h0 must have " | toString(#deg0Indices)
            | " columns (one per hom-degree-0 M-generator); got "
            | toString(numcols h0) | ".");
    if numrows h0 != rank N.natural then
        error ("liftToDGModuleMap: h0 must have " | toString(rank N.natural)
            | " rows (one per natural generator of N); got "
            | toString(numrows h0) | ".");
    -- images#i = Vector in N.natural (current image of M-gen i), or null
    images := new MutableList from apply(numGenM, i -> null);
    -- Seed hom-deg-0 images from h0.
    scan(#deg0Indices, k -> (
        col := apply(rank N.natural, j -> h0_(j, k));
        vec := sum apply(rank N.natural, j -> col#j * (N.natural)_j);
        images#(deg0Indices#k) = vec;
    ));
    -- Helper: apply partial f to a Vector v in M.natural.
    applyPartial := v -> (
        if v === 0 or v == 0 then return 0_(N.natural);
        vEntries := entries v;
        sum apply(numGenM, j -> (
            cj := vEntries#j;
            if cj == 0 then 0_(N.natural)
            else if images#j === null then (
                error "liftToDGModuleMap: encountered M-generator with unassigned image while lifting.";
            )
            else cj * images#j
        ))
    );
    for d from 1 to maxDeg do (
        if not genIdxByDeg#?d then continue;
        for i in genIdxByDeg#d do (
            bd := applyPartial(M.diff#i);
            if bd == 0 then (
                images#i = 0_(N.natural);
            )
            else (
                (ok, pre) := getBoundaryPreimage(N, bd);
                if not ok then
                    error ("liftToDGModuleMap: cannot lift M-generator #" | toString i
                        | " at hom-degree " | toString d
                        | " (target N is not acyclic at this degree).");
                images#i = pre;
            );
        );
    );
    -- Fill in zeros for any M-generator beyond EndDegree.
    finalImgs := apply(numGenM, i -> if images#i === null then 0_(N.natural) else images#i);
    dgModuleMap(N, M, finalImgs)
)

-- Convenience: if M has exactly one hom-deg-0 generator mapping to a
-- Vector v in N.natural, accept v directly.
liftToDGModuleMap (DGModule, DGModule, Vector) := opts -> (N, M, v) -> (
    genDegs := apply(M.Degrees, first);
    deg0Count := #select(genDegs, d -> d == 0);
    if deg0Count != 1 then
        error ("liftToDGModuleMap(Vector): M has " | toString deg0Count
            | " hom-degree-0 generators; use the Matrix or List form instead.");
    liftToDGModuleMap(N, M, {v}, opts)
)

-- Convenience: pass a list of Vectors, one per hom-degree-0 M-generator.
liftToDGModuleMap (DGModule, DGModule, List) := opts -> (N, M, vList) -> (
    if M.dgAlgebra =!= N.dgAlgebra then
        error "liftToDGModuleMap: source and target must share the same DG algebra.";
    genDegs := apply(M.Degrees, first);
    deg0Count := #select(genDegs, d -> d == 0);
    if #vList != deg0Count then
        error ("liftToDGModuleMap(List): M has " | toString deg0Count
            | " hom-degree-0 generators; got " | toString(#vList) | " images.");
    coerced := apply(vList, v -> (
        if v === 0 then 0_(N.natural)
        else if instance(v, Vector) then v
        else try (sub(v, N.natural)) else
            error "liftToDGModuleMap(List): could not coerce image to N.natural."
    ));
    h0 := if #coerced == 0 then map(N.natural, (N.ring)^0, 0)
          else vectorsToColumnMatrix(N, coerced);
    liftToDGModuleMap(N, M, h0, opts)
)

-------------------------------------------------------------------------
-- v2: DGIdeal, DGSubmodule, DGQuotientModule, and
--     image / kernel / cokernel of DGModuleMap.
--
-- Conventions.  These types mirror M2's built-in Ideal / Module / quotient
-- conventions wherever possible, and add a differential-compatibility
-- layer on top:
--
--   * DGIdeal         -- a graded ideal of A.natural closed under d_A.
--                       `ambient` returns A; `ideal I` returns the
--                       underlying Ideal.  `A / I` yields a new DGAlgebra.
--
--   * DGSubmodule     -- a DGModule S equipped with an inclusion
--                       DGModuleMap S -> M. S.natural is a free A.natural-
--                       module on the chosen generators; `inclusion S`
--                       returns the chain-map S -> M. Because S inherits
--                       from DGModule, generic DGModule operations apply.
--
--   * DGQuotientModule -- quotient M / S of a DGModule M by a DGSubmodule S.
--                       Q.natural is the cokernel of the inclusion matrix,
--                       so it is NOT a free module; Q does NOT inherit
--                       from DGModule (to avoid silently breaking free-
--                       only operations).  `ambient Q` returns M,
--                       `subDGModule Q` returns S, `projection Q` returns
--                       the DGModuleMap M -> Q.
--
-- All three carry d-closure invariants; isWellDefined verifies them.
-------------------------------------------------------------------------

DGIdeal = new Type of MutableHashTable
globalAssignment DGIdeal

DGSubmodule = new Type of DGModule
globalAssignment DGSubmodule

DGQuotientModule = new Type of MutableHashTable
globalAssignment DGQuotientModule

-- Pretty-printing --------------------------------------------------------

net DGIdeal := I -> (
    hdr := net "DGIdeal of " | net I.dgAlgebra.natural;
    gs := net I.generators;
    stack {hdr, net "generators => " | gs}
)

net DGSubmodule := S -> (
    stack {
        net "DGSubmodule of ambient DGModule",
        net "Degrees  => " | net S.Degrees,
        net "natural  => " | net S.natural,
        net "inclusion => " | net S.inclusion.natural
    }
)

net DGQuotientModule := Q -> (
    stack {
        net "DGQuotientModule Q = M / S",
        net "Q.natural = " | net Q.natural,
        net "Degrees   = " | net Q.Degrees
    }
)

-------------------------------------------------------------------------
-- DGIdeal
--
--   dgIdeal(A, gens) takes a List or Matrix of homogeneous elements of
--   A.natural and returns the smallest DG ideal containing them, i.e.
--   the ideal of A.natural generated by {g, d(g), d^2(g), ...} iterated
--   until the ideal (as an A.natural-Ideal) stabilizes.
--
--   isDGIdeal(A, I) (A:DGAlgebra, I:Ideal) checks d-closure without
--   saturation.
-------------------------------------------------------------------------

dgIdeal = method(TypicalValue => DGIdeal)

isDGIdeal = method(TypicalValue => Boolean)
isDGIdeal (DGAlgebra, Ideal) := (A, I) -> (
    if ring I =!= A.natural then
        error "isDGIdeal: ideal must live in A.natural.";
    all(flatten entries mingens I, g -> (polyDifferential(A, g)) % I == 0)
)

dgIdeal (DGAlgebra, Ideal) := (A, I) -> (
    if ring I =!= A.natural then
        error "dgIdeal: ideal must live in A.natural.";
    -- Iteratively d-close.
    curr := I;
    iter := 0;
    while (
        iter = iter + 1;
        if iter > 200 then error "dgIdeal: d-closure failed to stabilize.";
        derivs := apply(flatten entries mingens curr, g -> polyDifferential(A, g));
        newIdeal := curr + ideal derivs;
        newIdeal != curr
    ) do curr = trim newIdeal;
    J := trim curr;
    result := new MutableHashTable;
    result#(symbol dgAlgebra) = A;
    result#(symbol ring) = A.ring;
    result#(symbol natural) = J;
    result#(symbol generators) = gens J;
    result#(symbol cache) = new CacheTable;
    new DGIdeal from result
)

dgIdeal (DGAlgebra, Matrix) := (A, m) -> (
    if ring m =!= A.natural then
        error "dgIdeal(Matrix): matrix must have entries in A.natural.";
    dgIdeal(A, ideal m)
)

dgIdeal (DGAlgebra, List) := (A, gs) -> (
    if #gs == 0 then dgIdeal(A, ideal(0_(A.natural)))
    else dgIdeal(A, ideal matrix {apply(gs, g -> sub(g, A.natural))})
)

ambient DGIdeal := I -> I.dgAlgebra
ideal DGIdeal := I -> I.natural
generators DGIdeal := opts -> I -> I.generators
ring DGIdeal := I -> I.ring
numgens DGIdeal := I -> numgens I.natural

-- Internal helper: assert two DGIdeals share an ambient DGAlgebra.
sameAmbientDG := (I, J, opname) -> (
    if I.dgAlgebra =!= J.dgAlgebra then
        error(opname | ": DGIdeals must share the same ambient DGAlgebra.");
)

-- Wrap an Ideal of A.natural as a DGIdeal, running the d-closure
-- saturation machinery in dgIdeal.  For operations that preserve
-- d-closure (sum, product, intersection, power, colon) this is an
-- identity-on-the-ideal pass but still re-creates the DGIdeal wrapper.
-- The saturation loop terminates immediately when the input is already
-- d-closed.
-------------------------------------------------------------------------
-- Ideal-algebra operations.  Each returns a new DGIdeal whose .natural
-- is the corresponding Ideal operation result.  d-closure is preserved
-- by each of these operations (sum: d(I+J) ⊂ dI+dJ ⊂ I+J; product:
-- d(IJ) ⊂ (dI)J + I(dJ) ⊂ IJ; intersection: direct; power: induction;
-- colon: if fJ ⊂ I then d(fJ) = (df)J + f(dJ) ⊂ I + f(dJ) ⊂ I + fJ ⊂ I,
-- so (df)J ⊂ I, i.e. df ∈ I:J).  We run through dgIdeal() anyway so the
-- saturation loop certifies closure; for already-closed inputs this is
-- just a re-wrap.
-------------------------------------------------------------------------

DGIdeal + DGIdeal := (I, J) -> (
    sameAmbientDG(I, J, "DGIdeal + DGIdeal");
    dgIdeal(I.dgAlgebra, I.natural + J.natural)
)

DGIdeal * DGIdeal := (I, J) -> (
    sameAmbientDG(I, J, "DGIdeal * DGIdeal");
    dgIdeal(I.dgAlgebra, I.natural * J.natural)
)

intersect(DGIdeal, DGIdeal) := DGIdeal => {} >> opts -> (I, J) -> (
    sameAmbientDG(I, J, "intersect DGIdeal");
    dgIdeal(I.dgAlgebra, intersect(I.natural, J.natural))
)

DGIdeal : DGIdeal := (I, J) -> (
    sameAmbientDG(I, J, "DGIdeal : DGIdeal");
    dgIdeal(I.dgAlgebra, I.natural : J.natural)
)

DGIdeal ^ ZZ := (I, n) -> (
    if n < 0 then error "DGIdeal ^ ZZ: exponent must be nonnegative.";
    if n == 0 then dgIdeal(I.dgAlgebra, ideal 1_(I.dgAlgebra.natural))
    else dgIdeal(I.dgAlgebra, (I.natural)^n)
)

-- Containment / equality.  I ⊆ J iff I.natural ⊆ J.natural (both live
-- in the same ambient A.natural by sameAmbientDG).
isSubset(DGIdeal, DGIdeal) := (I, J) -> (
    sameAmbientDG(I, J, "isSubset DGIdeal");
    isSubset(I.natural, J.natural)
)

DGIdeal == DGIdeal := (I, J) -> (
    if I.dgAlgebra =!= J.dgAlgebra then false
    else I.natural == J.natural
)

-- mingens: return the A.natural-level minimal generators.  (Note that
-- trim at the Ideal level doesn't enlarge the d-saturated closure; the
-- underlying ideal is already d-closed.)
mingens DGIdeal := opts -> I -> mingens I.natural

-- Reduction of a ring element mod I.
RingElement % DGIdeal := (f, I) -> f % I.natural
ZZ % DGIdeal := (n, I) -> (sub(n, I.dgAlgebra.natural)) % I.natural

-- isWellDefined DGIdeal.  Key-shape + type checks, then the semantic
-- d-closure check (isDGIdeal).  Diagnostic messages via debugLevel > 0.
isWellDefined DGIdeal := I -> (
    k := keys I;
    expectedKeys := set {symbol dgAlgebra, symbol ring, symbol natural, symbol generators, symbol cache};
    if set k =!= expectedKeys then (
        if debugLevel > 0 then (
            added := toList(k - expectedKeys);
            missing := toList(expectedKeys - k);
            if #added > 0 then << "-- DGIdeal: unexpected key(s): " << toString added << endl;
            if #missing > 0 then << "-- DGIdeal: missing key(s): " << toString missing << endl;
        );
        return false;
    );
    if not instance(I.dgAlgebra, DGAlgebra) then (
        if debugLevel > 0 then << "-- DGIdeal: I.dgAlgebra is not a DGAlgebra" << endl;
        return false;
    );
    if not isWellDefined I.dgAlgebra then (
        if debugLevel > 0 then << "-- DGIdeal: I.dgAlgebra is not well-defined" << endl;
        return false;
    );
    if not instance(I.natural, Ideal) then (
        if debugLevel > 0 then << "-- DGIdeal: I.natural is not an Ideal" << endl;
        return false;
    );
    if ring I.natural =!= I.dgAlgebra.natural then (
        if debugLevel > 0 then << "-- DGIdeal: I.natural is not an ideal in I.dgAlgebra.natural" << endl;
        return false;
    );
    ok := isDGIdeal(I.dgAlgebra, I.natural);
    if not ok and debugLevel > 0 then
        << "-- DGIdeal: I.natural is not d-closed (some generator has d(g) not in I)" << endl;
    ok
)

-- toComplex DGIdeal: build the short-exact-sequence-style complex of
-- I viewed as a DG submodule of the regular DG module A.natural.
-- Equivalent to the kernel of the canonical chain map A ->> A/I.
--
-- NOTE: This requires viewing A.natural as a DGModule over A (the
-- "regular representation"). We do not yet have a canonical wrapper for
-- that view, so we defer implementation and emit a clear error.
--
-- For now, users can access the ideal's underlying ring-theoretic data
-- via `ideal I`, the generating matrix via `gens I`, and the quotient
-- algebra's complex via `toComplex(A/I)`.
toComplex DGIdeal := I -> error(
    "toComplex DGIdeal is not yet implemented. "
    | "Use toComplex(ambient I / I) for the quotient's complex, "
    | "or access underlying data via `ideal I` and `gens I`."
)

-- A / I : quotient DGAlgebra.  Builds a fresh DGAlgebra whose underlying
-- algebra is A.natural / I.natural, and whose differential is inherited
-- from A (d descends since I is d-closed).
DGAlgebra / DGIdeal := (A, I) -> (
    if I.dgAlgebra =!= A then
        error "DGAlgebra / DGIdeal: ideal must be a DG ideal of the given DG algebra.";
    Bnat := A.natural / I.natural;
    proj := map(Bnat, A.natural);
    result := new MutableHashTable;
    result#(symbol ring) = A.ring;
    result#(symbol natural) = Bnat;
    result#(symbol Degrees) = A.Degrees;
    -- Descend the differential: for each generator t of Bnat (same names as
    -- gens A.natural), its image under d is proj(d_A t). A.diff is a
    -- RingMap A.natural -> A.natural, so A.diff t applies it. Build
    -- B.diff as a RingMap Bnat -> Bnat.
    diffImgs := apply(gens A.natural, t -> proj(A.diff t));
    result#(symbol diff) = map(Bnat, Bnat, matrix {diffImgs});
    result#(symbol isHomogeneous) = A.isHomogeneous;
    result#(symbol cache) = new CacheTable;
    result.cache#(symbol diffs) = new MutableHashTable;
    result#(symbol zerothHomology) = if A.?zerothHomology then A.zerothHomology else A.ring;
    new DGAlgebra from result
)

-------------------------------------------------------------------------
-- DGSubmodule
--
--   dgSubmodule(M, incMat): incMat is a Matrix whose target is M.natural
--   and whose source is a free A.natural-module; its columns are the
--   chosen A-generators of the submodule. The constructor d-closes by
--   iteratively adjoining derivatives of current generators until the
--   column span is d-stable.
--
--   The resulting DGSubmodule S has:
--     S.natural    -- source (or an extended free module) of the inclusion
--     S.inclusion  -- DGModuleMap S -> M encoding the inclusion
--     S.Degrees    -- degree list (one per S.natural generator)
--     S.diff       -- list of Vectors in S.natural, one per S.natural
--                     generator, giving d_S
-------------------------------------------------------------------------

-- Internal helper: columnwise application of d_M, returning a Matrix
-- with target M.natural and source a free A.natural-module whose i-th
-- generator's hom-degree is (degrees source incMat)#i with its hom slot
-- decreased by 1 (d_M lowers hom-degree by 1).  This makes the result a
-- degree-0 homogeneous matrix and ensures that when new columns get
-- appended during saturation (`incMat | dMat_newIdx`), the combined
-- source carries correct degrees for its generators.
applyDiffColumns := (M, incMat) -> (
    A := M.dgAlgebra;
    n := rank M.natural;
    ncols := numcols incMat;
    srcDegs := degrees source incMat;
    shiftedDegs := apply(srcDegs, d -> prepend(first d - 1, drop(d, 1)));
    srcShifted := if #shiftedDegs == 0 then (A.natural)^0
                  else (A.natural)^(-shiftedDegs);
    if ncols == 0 then return map(M.natural, srcShifted, 0);
    -- For each column, diff returns a Vector in M.natural.  Collect the
    -- entries of each and assemble a matrix by rows.
    colEntries := apply(ncols, i -> entries diff(M, (incMat)_i));
    rowsList := apply(n, r -> apply(ncols, j -> (colEntries#j)#r));
    map(M.natural, srcShifted, rowsList)
)

-- isDGSubmodule(M, incMat): test whether image(incMat) is d_M-closed
-- without saturating. Returns true/false.
isDGSubmodule = method(TypicalValue => Boolean)
isDGSubmodule (DGModule, Matrix) := (M, incMat) -> (
    if target incMat =!= M.natural then
        error "isDGSubmodule: target of incMat must equal M.natural.";
    if numcols incMat == 0 then return true;
    -- Assemble d_M applied column-wise; check containment in the column
    -- span of incMat via a single `//` solve.
    dMat := applyDiffColumns(M, incMat);
    residual := dMat - incMat * (dMat // incMat);
    residual == 0
)

dgSubmodule = method(TypicalValue => DGSubmodule)

dgSubmodule (DGModule, Matrix) := (M, incMat0) -> (
    if target incMat0 =!= M.natural then
        error "dgSubmodule: target of inclusion matrix must equal M.natural.";
    A := M.dgAlgebra;
    -- Step 1: d-saturate. Iteratively compute d of every current generator
    -- and adjoin only those that are not already in the current column span.
    incMat := incMat0;
    iter := 0;
    notDone := true;
    while notDone do (
        iter = iter + 1;
        if iter > 200 then error "dgSubmodule: d-closure failed to stabilize.";
        dMat := applyDiffColumns(M, incMat);
        if numcols dMat == 0 then (notDone = false; break);
        residual := dMat - incMat * (dMat // incMat);
        newIdx := select(numcols dMat, j -> residual_{j} != 0);
        if #newIdx == 0 then (notDone = false; break);
        incMat = incMat | dMat_newIdx;
    );
    -- Step 2: compute degrees of the columns of the final incMat.
    -- Invariant: for DGModule M, M.Degrees equals degrees M.natural (same
    -- sign), matching freeDGModule's convention that F = (A.natural)^(-degList)
    -- has degrees degList.  So S.Degrees should likewise equal degrees F.
    F := source incMat;
    nGens := rank F;
    subDegs := apply(nGens, i -> (degrees F)#i);
    -- Step 3: compute d_S on each free generator by lifting d_M(col) back
    -- through incMat.
    -- Build a single matrix of d_M applied column-wise to incMat, then do
    -- one bulk `//` solve.  This avoids per-column Vector->Matrix coercion
    -- and matches the style of the d-saturation loop above.
    dFullMat := applyDiffColumns(M, incMat);
    liftFull := dFullMat // incMat;
    residualFull := dFullMat - incMat * liftFull;
    if residualFull != 0 then
        error("dgSubmodule: internal error -- d-closure appeared to fail after saturation.");
    dSList := apply(nGens, i -> liftFull_i);
    -- Step 4: package.
    result := new MutableHashTable;
    result#(symbol dgAlgebra) = A;
    result#(symbol ring) = A.ring;
    result#(symbol natural) = F;
    result#(symbol Degrees) = subDegs;
    result#(symbol diff) = dSList;
    result#(symbol cache) = new CacheTable;
    result.cache#(symbol diffs) = new MutableHashTable;
    result#(symbol ambient) = M;
    S := new DGSubmodule from result;
    S#(symbol inclusion) = dgModuleMap(M, S, incMat);
    S
)

dgSubmodule (DGModule, List) := (M, gs) -> (
    if #gs == 0 then (
        -- The empty submodule: target is M.natural (over A.natural), and
        -- source is a zero-rank free module over the same ring A.natural
        -- (NOT M.ring = A.ring, which would be a different ring).
        emptyMat := map(M.natural, (M.dgAlgebra.natural)^0, 0);
        return dgSubmodule(M, emptyMat);
    );
    -- Coerce each entry to a Vector in M.natural.
    coerced := apply(gs, g -> (
        if instance(g, Vector) then g
        else if instance(g, Matrix) and numcols g == 1 then (g)_0
        else try (g * 1_(M.natural)) else error("dgSubmodule(List): could not coerce generator to M.natural.")
    ));
    -- Build a matrix with these columns.
    cols := matrix {coerced};
    dgSubmodule(M, cols)
)

ambient DGSubmodule := S -> S.ambient
inclusion = method()
inclusion DGSubmodule := S -> S.inclusion
-- `module S` returns the underlying A.natural-module (free in this encoding);
-- for the set-theoretic submodule image, use `image S.inclusion.natural`.
module DGSubmodule := S -> S.natural

-- isWellDefined DGSubmodule.  Key-shape + type checks, then verify
-- d-closure: the column span of the inclusion matrix is stable under
-- d_M.  Diagnostics via debugLevel > 0.
isWellDefined DGSubmodule := S -> (
    k := keys S;
    expectedKeys := set {
        symbol dgAlgebra, symbol ring, symbol natural, symbol Degrees,
        symbol diff, symbol cache, symbol ambient, symbol inclusion
    };
    if set k =!= expectedKeys then (
        if debugLevel > 0 then (
            added := toList(k - expectedKeys);
            missing := toList(expectedKeys - k);
            if #added > 0 then << "-- DGSubmodule: unexpected key(s): " << toString added << endl;
            if #missing > 0 then << "-- DGSubmodule: missing key(s): " << toString missing << endl;
        );
        return false;
    );
    if not instance(S.ambient, DGModule) then (
        if debugLevel > 0 then << "-- DGSubmodule: S.ambient is not a DGModule" << endl;
        return false;
    );
    if not isWellDefined S.ambient then (
        if debugLevel > 0 then << "-- DGSubmodule: S.ambient is not a well-defined DGModule" << endl;
        return false;
    );
    if not instance(S.inclusion, DGModuleMap) then (
        if debugLevel > 0 then << "-- DGSubmodule: S.inclusion is not a DGModuleMap" << endl;
        return false;
    );
    -- Semantic: the column span of S.inclusion.natural must be d-stable
    -- in M = S.ambient.  The free presentation S.natural and its lifted
    -- differential S.diff are derived from that; d-closure fails iff the
    -- lift has nontrivial residual.
    M := S.ambient;
    incMat := S.inclusion.natural;
    if target incMat =!= M.natural then (
        if debugLevel > 0 then << "-- DGSubmodule: target of inclusion matrix is not M.natural" << endl;
        return false;
    );
    if numcols incMat == 0 then return true;
    dMat := applyDiffColumns(M, incMat);
    residual := dMat - incMat * (dMat // incMat);
    ok := residual == 0;
    if not ok and debugLevel > 0 then
        << "-- DGSubmodule: inclusion's column span is not d-closed in the ambient DGModule" << endl;
    ok
)

-------------------------------------------------------------------------
-- DGQuotientModule
--
--   dgQuotientModule(M, S) or `M / S` constructs the quotient Q.
--   Q.natural   = coker S.inclusion.natural
--   Q.Degrees   = M.Degrees
--   Q.projection = DGModuleMap M -> Q with natural = identity composed
--                  with the quotient map (built via `map(Q.natural, ...)`).
-------------------------------------------------------------------------

dgQuotientModule = method(TypicalValue => DGQuotientModule)
dgQuotientModule (DGModule, DGSubmodule) := (M, S) -> (
    if S.ambient =!= M then
        error "dgQuotientModule: submodule's ambient must equal the given DGModule.";
    A := M.dgAlgebra;
    Qnat := cokernel S.inclusion.natural;
    -- Projection natural matrix: M.natural -> Qnat, the canonical quotient.
    projNat := map(Qnat, M.natural, id_(M.natural));
    result := new MutableHashTable;
    result#(symbol dgAlgebra) = A;
    result#(symbol ring) = A.ring;
    result#(symbol natural) = Qnat;
    result#(symbol Degrees) = M.Degrees;
    -- The DGModule-like diff list: for each M-generator, push its image
    -- in M.natural under d through proj to land in Qnat.
    result#(symbol diff) = apply(#M.Degrees, i -> projNat * M.diff#i);
    result#(symbol ambient) = M;
    result#(symbol subDGModule) = S;
    result#(symbol cache) = new CacheTable;
    Q := new DGQuotientModule from result;
    -- Build the projection DGModuleMap only after Q is instantiated so we
    -- can point its `target` at Q.
    -- Note: Q is NOT a DGModule, so we construct a bare DGModuleMap record.
    projMap := new MutableHashTable;
    projMap#(symbol source) = M;
    projMap#(symbol target) = Q;
    projMap#(symbol dgAlgebra) = A;
    projMap#(symbol natural) = projNat;
    projMap#(symbol cache) = new CacheTable;
    Q#(symbol projection) = new DGModuleMap from projMap;
    Q
)

DGModule / DGSubmodule := (M, S) -> dgQuotientModule(M, S)

-- diff(Q, v) for DGQuotientModule Q: the same Leibniz formula used on
-- DGModule works verbatim because Q has .natural (a cokernel), .diff
-- (a list of columns in Q.natural), and .dgAlgebra.  Used by
-- isWellDefined DGModuleMap to verify chain-map conditions when source
-- or target is a DGQuotientModule.
diff (DGQuotientModule, Vector) := (Q, v) -> (
    A := Q.dgAlgebra;
    vEntries := entries v;
    -- Use #vEntries (= presentation generator count) rather than rank,
    -- which would try codim on a cokernel module and may fail over
    -- non-affine graded ambients.
    numGens := #vEntries;
    natGens := apply(numGens, i -> (Q.natural)_i);
    sum apply(numGens, i -> (
        fi := vEntries#i;
        if fi == 0 then 0_(Q.natural)
        else (
            term1 := polyDifferential(A, fi) * natGens#i;
            signExp := first degree fi;
            sign := if odd signExp then -1 else 1;
            term2 := sign * fi * Q.diff#i;
            term1 + term2
        )
    ))
)

ambient DGQuotientModule := Q -> Q.ambient
subDGModule = method()
subDGModule DGQuotientModule := Q -> Q.subDGModule
projection = method()
projection DGQuotientModule := Q -> Q.projection

-- maxDegree on a DGQuotientModule: inherit from the ambient DGModule.
-- (DGQuotientModule is not currently a subtype of DGModule, so the
-- DGModule method at line 3204 does not fire; supply it here so that
-- toComplexMap can take a DGQuotientModule as target.)
maxDegree DGQuotientModule := Q -> maxDegree Q.ambient

-- toComplex: build per-degree quotient complex directly from the ambient
-- DGModule and the submodule's inclusion-as-ComplexMap.  If Q has been
-- through `prune` (see prune DGQuotientModule), return the cached
-- minimally-presented complex.
toComplex DGQuotientModule := Q -> (
    if Q.cache#?(symbol prunedComplex) then
        return Q.cache#(symbol prunedComplex);
    cxM := toComplex Q.ambient;
    cxIncl := toComplexMap Q.subDGModule.inclusion;
    cokernel cxIncl
)

homology (ZZ, DGQuotientModule) := opts -> (n, Q) -> (
    HH_n (toComplex Q)
)

-- Unary homology on a DGQuotientModule: the graded HA-module obtained
-- by taking H_i(toComplex Q) at each hom-degree i ∈ [concentration cxQ]
-- and packaging the pieces as an HA = HH(A)-module via extension of
-- scalars along R → HA.
--
-- This mirrors homologyModule DGModule, but uses toComplex(Q) directly
-- because Q.natural is a cokernel rather than a free A.natural-module,
-- so the moduleMultMap path (which requires a free natural module)
-- cannot be used generically.  The result therefore captures the
-- R-module structure and the HA-action induced by R → HA only; any
-- nontrivial cycle-class action on the quotient must be descended
-- separately (not yet implemented).
homologyModule DGQuotientModule := Q -> (
    A := Q.dgAlgebra;
    HA := HH A;
    cxQ := toComplex Q;
    (lo, hi) := concentration cxQ;
    if lo > hi then return HA^0;
    -- Per-degree homology as R-modules, then tensor up to HA-modules.
    pieces := apply(toList(lo..hi), i -> HA ** prune HH_i cxQ);
    directSum pieces
)

homology DGQuotientModule := opts -> Q -> homologyModule Q

-- isAcyclic on a DGQuotientModule: H_i(Q) = 0 for all i >= 1 up to the
-- finite bound implied by maxDegree Q.ambient (or the user-supplied
-- EndDegree).  Matches isAcyclic DGModule's semantics.
isAcyclic DGQuotientModule := opts -> Q -> (
    endDegree := maxDegree Q;
    if endDegree === infinity and opts.EndDegree == -1 then
        error "Must supply an upper bound to check for acyclicity.";
    if opts.EndDegree != -1 then endDegree = opts.EndDegree;
    not any(1..endDegree, i -> prune homology(i, Q) != 0)
)

-- isWellDefined DGQuotientModule.  Key-shape + type checks, then delegate
-- to the subDGModule's d-closure check (the quotient is well-defined
-- exactly when the sub is).  Also verifies projection structure.
isWellDefined DGQuotientModule := Q -> (
    k := keys Q;
    expectedKeys := set {
        symbol dgAlgebra, symbol ring, symbol natural, symbol Degrees,
        symbol diff, symbol cache, symbol ambient, symbol subDGModule,
        symbol projection
    };
    if set k =!= expectedKeys then (
        if debugLevel > 0 then (
            added := toList(k - expectedKeys);
            missing := toList(expectedKeys - k);
            if #added > 0 then << "-- DGQuotientModule: unexpected key(s): " << toString added << endl;
            if #missing > 0 then << "-- DGQuotientModule: missing key(s): " << toString missing << endl;
        );
        return false;
    );
    if not instance(Q.ambient, DGModule) then (
        if debugLevel > 0 then << "-- DGQuotientModule: Q.ambient is not a DGModule" << endl;
        return false;
    );
    if not instance(Q.subDGModule, DGSubmodule) then (
        if debugLevel > 0 then << "-- DGQuotientModule: Q.subDGModule is not a DGSubmodule" << endl;
        return false;
    );
    if Q.subDGModule.ambient =!= Q.ambient then (
        if debugLevel > 0 then << "-- DGQuotientModule: Q.subDGModule.ambient does not match Q.ambient" << endl;
        return false;
    );
    if not instance(Q.projection, DGModuleMap) then (
        if debugLevel > 0 then << "-- DGQuotientModule: Q.projection is not a DGModuleMap" << endl;
        return false;
    );
    if source Q.projection =!= Q.ambient or target Q.projection =!= Q then (
        if debugLevel > 0 then << "-- DGQuotientModule: Q.projection has wrong source/target" << endl;
        return false;
    );
    -- Semantic: delegate to the submodule's well-definedness (which
    -- verifies d-closure of the relations).
    ok := isWellDefined Q.subDGModule;
    if not ok and debugLevel > 0 then
        << "-- DGQuotientModule: Q.subDGModule is not a well-defined DGSubmodule" << endl;
    ok
)

-------------------------------------------------------------------------
-- prune for DG types.
--
-- Modeled on `prune Complex` (Complexes package, ChainComplex.m2 line
-- 662): that method minimalPresentation's each term, drops trivially-zero
-- pieces from the concentration, and transports differentials via
-- minimalPresentation at the ComplexMap level.  We provide DG analogs:
--
--   * prune DGModule           : identity on a freeDGModule (M.natural
--                                is already minimal).  For non-free
--                                DGModules (currently none), this would
--                                minimalPresentation M.natural.
--   * prune DGSubmodule S      : rebuild S using mingens of its inclusion
--                                matrix (A.natural-level minimization).
--                                d-closure preserved (span unchanged).
--   * prune DGQuotientModule Q : (1) prune Q.subDGModule; (2) cache the
--                                complex-level pruned toComplex inside Q
--                                so subsequent toComplex Q calls return
--                                the minimally-presented complex.  This
--                                closes the commutation gap: on a pruned
--                                Q we have toComplex Q == prune toComplex Q.
--                                In particular, trivially-zero cokernels
--                                now produce the zero complex.
--   * prune DGIdeal I          : trim I.natural (ideal-level minimization).
--
-- The DGQuotientModule branch uses the same transport idea as
-- `prune Complex` (minimalPresentation commutes with the differential)
-- without trying to rebuild Q.natural itself -- which would require
-- changing the ambient module M, a nontrivial refactor.
-------------------------------------------------------------------------

-- minimalPresentation is the non-prune synonym; both keys installed to
-- match M2's convention (see ChainComplex.m2 line 661-662 for Complex).
--
-- Following the Complex convention (ChainComplex.m2 ~line 685), every
-- prune method caches a `pruningMap` in the *output* object's cache,
-- encoding the canonical comparison map (pruned) --> (original).  That
-- map is a well-defined DGModuleMap / DGAlgebraMap / DGIdealWrapper and
-- can be verified via isWellDefined.
--
-- We do not materialize DGModule-level prune (prune DGModule = identity),
-- so the cached pruningMap on a DGModule is literally the identity.
minimalPresentation DGModule := o -> M -> M
prune DGModule := o -> M -> (
    M.cache#(symbol pruningMap) = identityDGModuleMap M;
    M
)

-- Trivial prune fallbacks for DGAlgebra and maps.  These mirror the
-- Complexes pattern (prune is the identity on already-minimal objects)
-- and ensure that prune commutes with every constructor uniformly so
-- that calling code never has to guard against "prune not defined".
minimalPresentation DGAlgebra := o -> A -> A
prune DGAlgebra := o -> A -> A
minimalPresentation DGAlgebraMap := o -> f -> f
prune DGAlgebraMap := o -> f -> f
minimalPresentation DGModuleMap := o -> f -> f
prune DGModuleMap := o -> f -> f

prune DGSubmodule := o -> S -> (
    M := S.ambient;
    incMat := S.inclusion.natural;
    -- No redundancy to trim if there are no generators or mingens is
    -- already the same width: cache an identity pruningMap and return.
    minIm := if numcols incMat == 0 then incMat else mingens image incMat;
    if numcols minIm == numcols incMat then (
        S.cache#(symbol pruningMap) = identityDGModuleMap S;
        return S;
    );
    -- Build pruned Sp with fewer generators.
    Sp := dgSubmodule(M, minIm);
    -- Build pruningMap: Sp --> S as a DGModuleMap.  Factor minIm = incMat * T
    -- via `minIm // incMat` (solves the A.natural-linear system), then
    -- coerce T to a map with the correct degree labels.
    T := minIm // incMat;
    Tcoerced := map(S.natural, Sp.natural, T);
    pm := dgModuleMap(S, Sp, Tcoerced);
    Sp.cache#(symbol pruningMap) = pm;
    Sp
)
minimalPresentation DGSubmodule := o -> S -> prune S

prune DGQuotientModule := o -> Q -> (
    -- Step 1: minimize the submodule presentation (A.natural-level prune).
    prunedS := prune Q.subDGModule;
    Qp := if prunedS === Q.subDGModule then Q else Q.ambient / prunedS;
    -- Step 2: build pruningMap: Qp --> Q as a raw DGModuleMap.  Both are
    -- cokernels of inclusions of the same image(S) in M (equal sets), so
    -- the identity on M.natural descends to a canonical map
    -- Qp.natural --> Q.natural.  Construct it directly (dgModuleMap does
    -- not accept DGQuotientModule source/target because the type doesn't
    -- inherit from DGModule).
    idAmb := id_(Q.ambient.natural);
    pmNat := try inducedMap(Q.natural, Qp.natural, idAmb) else
             map(Q.natural, Qp.natural, idAmb);
    pm := new DGModuleMap from {
        symbol source    => Qp,
        symbol target    => Q,
        symbol dgAlgebra => Q.dgAlgebra,
        symbol natural   => pmNat,
        symbol cache     => new CacheTable
    };
    Qp.cache#(symbol pruningMap) = pm;
    -- Step 3: compute the complex-level prune and cache it.  This mirrors
    -- `prune Complex` and closes the commutation gap: after this, calling
    -- toComplex on Qp returns the minimally-presented complex.
    cxQ := toComplex Qp;
    prunedCxQ := prune cxQ;
    Qp.cache#(symbol prunedComplex) = prunedCxQ;
    Qp
)

prune DGIdeal := o -> I -> (
    A := I.dgAlgebra;
    minI := trim I.natural;
    if numgens minI == numgens I.natural then (
        -- DGIdeals don't support maps directly (no DGIdealMap type); we
        -- cache the identity as the pruningMap so downstream code can
        -- share a uniform calling convention.  The pruningMap for a DGIdeal is
        -- recorded as the identity DGAlgebraMap on the ambient, which is
        -- well-defined and trivially an iso.
        I.cache#(symbol pruningMap) = identityDGAlgebraMap A;
        return I;
    );
    Ip := dgIdeal(A, (generators minI)_*);
    -- For a pruned DGIdeal, the ambient DGAlgebra is unchanged and the
    -- underlying ideal is equal (as a set) -- mingens just chooses a
    -- smaller generating set.  So the canonical comparison Ip --> I is
    -- the identity on the ambient DG algebra.
    Ip.cache#(symbol pruningMap) = identityDGAlgebraMap A;
    Ip
)
minimalPresentation DGIdeal := o -> I -> prune I
minimalPresentation DGQuotientModule := o -> Q -> prune Q

-------------------------------------------------------------------------
-- Basic module-like operations for DGModule / DGSubmodule / DGQuotientModule
--
-- These mirror M2's standard Module/Ideal methods (numgens, rank,
-- degrees, isHomogeneous, isFreeModule, super, cover, relations).
-- They work uniformly across the DG-module type hierarchy so that
-- code written against Modules can often be reused on DG objects
-- without modification.
-------------------------------------------------------------------------

numgens DGModule := M -> #M.Degrees
numgens DGSubmodule := S -> #S.Degrees
numgens DGQuotientModule := Q -> #Q.Degrees

rank DGModule := M -> rank M.natural
rank DGSubmodule := S -> rank S.natural

-- `rank DGQuotientModule` would ask for rank of a cokernel module, which
-- triggers codim computations that fail for non-affine graded ambients
-- (see the diff(DGQuotientModule, ...) rank-vs-numgens note).  Instead,
-- expose it as numgens of the cokernel presentation.
numColumnsDGQ := Q -> numgens Q.natural

degrees DGModule := M -> M.Degrees
degrees DGSubmodule := S -> S.Degrees
degrees DGQuotientModule := Q -> Q.Degrees

isHomogeneous DGModule := M -> isHomogeneous M.natural
isHomogeneous DGSubmodule := S -> isHomogeneous S.natural
isHomogeneous DGQuotientModule := Q -> isHomogeneous Q.natural
isHomogeneous DGIdeal := I -> isHomogeneous I.natural

-- `isFreeDGModule` — every DGModule in our encoding has M.natural a free
-- graded A.natural-module, so this returns true unless someone has
-- manually built one with a non-free natural (which isWellDefined would
-- still tolerate as long as the differential closes).  We still check
-- to be safe.
isFreeDGModule = method(TypicalValue => Boolean)
isFreeDGModule DGModule := M -> isFreeModule M.natural
isFreeDGModule DGSubmodule := S -> isFreeModule S.natural

-- M2-style structural accessors on DGSubmodule / DGQuotientModule.
--   super S       : ambient DGModule of S.
--   cover Q       : ambient DGModule M (of which Q is a quotient).
--   relations Q   : inclusion matrix encoding the killed relations.
-- For a DGSubmodule the relations are (by construction) the zero
-- submodule of S.natural, since S.natural is a free A.natural-module.
super DGSubmodule := S -> S.ambient
super DGQuotientModule := Q -> Q.ambient
cover DGQuotientModule := Q -> Q.ambient
relations DGQuotientModule := Q -> Q.subDGModule.inclusion.natural

-- isZero check: does the DG-thing have no generators (or equivalently
-- numgens == 0 / natural module is zero)?
isZero = method(TypicalValue => Boolean)
isZero DGModule := M -> numgens M.natural == 0
isZero DGSubmodule := S -> numcols S.inclusion.natural == 0 or S.inclusion.natural == 0
isZero DGQuotientModule := Q -> Q.natural == 0
isZero DGIdeal := I -> numgens I.natural == 0 or I.natural == 0

-------------------------------------------------------------------------
-- DGSubmodule algebra: sum, intersection, containment, equality.
--
-- All operations are in the same ambient DGModule.  d-closure is
-- preserved because:
--   * (S + T) : d(s + t) = d(s) + d(t) ∈ S + T.
--   * (S ∩ T) : if x ∈ S ∩ T, then d(x) ∈ S and d(x) ∈ T.
--   * containment / equality are read off from the image submodules.
-------------------------------------------------------------------------

sameAmbientDGSub := (S, T, opname) -> (
    if S.ambient =!= T.ambient then
        error(opname | ": DGSubmodules must share the same ambient DGModule.")
)

DGSubmodule + DGSubmodule := (S, T) -> (
    sameAmbientDGSub(S, T, "DGSubmodule +");
    combined := S.inclusion.natural | T.inclusion.natural;
    dgSubmodule(S.ambient, combined)
)

intersect(DGSubmodule, DGSubmodule) := DGSubmodule => {} >> opts -> (S, T) -> (
    sameAmbientDGSub(S, T, "intersect DGSubmodule");
    interMod := intersect(image S.inclusion.natural, image T.inclusion.natural);
    dgSubmodule(S.ambient, generators interMod)
)

isSubset(DGSubmodule, DGSubmodule) := (S, T) -> (
    sameAmbientDGSub(S, T, "isSubset DGSubmodule");
    isSubset(image S.inclusion.natural, image T.inclusion.natural)
)

DGSubmodule == DGSubmodule := (S, T) -> (
    if S.ambient =!= T.ambient then false
    else image S.inclusion.natural == image T.inclusion.natural
)

-------------------------------------------------------------------------
-- Scalar ideal action: I * S and I * M.
--
-- I * S has generators g · s for g a generator of I, s a generator of S
-- (viewed in the ambient M.natural).  This is d-closed:
--   d(g · s) = d(g) · s ± g · d(s) ∈ I·S + I·S = I·S.
-------------------------------------------------------------------------

DGIdeal * DGSubmodule := (I, S) -> (
    if S.ambient.dgAlgebra =!= I.dgAlgebra then
        error "DGIdeal * DGSubmodule: DG algebras disagree between I and S.ambient.";
    prodMod := I.natural * image S.inclusion.natural;
    dgSubmodule(S.ambient, generators prodMod)
)

DGIdeal * DGModule := (I, M) -> (
    if M.dgAlgebra =!= I.dgAlgebra then
        error "DGIdeal * DGModule: DG algebras disagree between I and M.";
    -- Whole-ambient submodule, then scale.
    I * dgSubmodule(M, id_(M.natural))
)

-------------------------------------------------------------------------
-- Annihilator of a DGSubmodule / DGQuotientModule.
--
-- Given S ⊂ M, ann(S) = { a ∈ A.natural : a · s = 0 for all s ∈ S }.
-- This is d-closed: if a · s = 0 for all s, then d(a · s) = 0 = d(a)·s
-- ± a·d(s), so d(a)·s = ∓ a·d(s).  Since d(s) ∈ S, a·d(s) = 0, hence
-- d(a)·s = 0 for all s ∈ S, so d(a) ∈ ann(S).  Same argument for Q.
-------------------------------------------------------------------------

annihilator DGSubmodule := opts -> S -> (
    annIdeal := annihilator(image S.inclusion.natural, opts);
    dgIdeal(S.ambient.dgAlgebra, annIdeal)
)

annihilator DGQuotientModule := opts -> Q -> (
    annIdeal := annihilator(Q.natural, opts);
    dgIdeal(Q.dgAlgebra, annIdeal)
)

-------------------------------------------------------------------------
-- DGIdeal: `module I` exposes the underlying Module-view of the ideal
-- (unifying with Ideal in M2, where `module I` returns I.module).
-------------------------------------------------------------------------

module DGIdeal := I -> module I.natural

-------------------------------------------------------------------------
-- Direct sum of DGModules: M ++ N.
--
-- The natural module is M.natural ++ N.natural (M2 handles the degree
-- book-keeping).  Hom-degrees are preserved component-wise.  The new
-- differential acts block-diagonally: d(M ⊕ N)(m, n) = (d_M m, d_N n).
-------------------------------------------------------------------------

DGModule ++ DGModule := (M, N) -> (
    if M.dgAlgebra =!= N.dgAlgebra then
        error "DGModule ++ DGModule: DG algebras must agree.";
    A := M.dgAlgebra;
    sumNat := M.natural ++ N.natural;
    mGens := #M.Degrees;
    nGens := #N.Degrees;
    -- Hom-degree/internal-degree labels concatenate: the first mGens
    -- generators come from M (degrees M.Degrees), the remaining nGens
    -- come from N (degrees N.Degrees).  This mirrors how M2 handles
    -- degrees on direct sums of free modules.
    sumDegrees := M.Degrees | N.Degrees;
    -- Differentials: d on M sits in the first mGens columns; d on N
    -- sits in the last nGens columns; each column in sumNat is a
    -- block vector (M-part, N-part).
    -- sumNat == M.natural ++ N.natural, indexed so that gens 0..mGens-1
    -- are M's and gens mGens..mGens+nGens-1 are N's.  Build the diff
    -- list as per-generator Vectors in sumNat.
    sumGens := apply(mGens + nGens, i -> (sumNat)_i);
    mInj := map(sumNat, M.natural, (sumNat)_{0..mGens-1});
    nInj := map(sumNat, N.natural, (sumNat)_{mGens..mGens+nGens-1});
    sumDiff := apply(mGens + nGens, i -> (
        if i < mGens then mInj * M.diff#i
        else nInj * N.diff#(i - mGens)
    ));
    result := new MutableHashTable;
    result#(symbol dgAlgebra) = A;
    result#(symbol ring) = A.ring;
    result#(symbol natural) = sumNat;
    result#(symbol Degrees) = sumDegrees;
    result#(symbol diff) = sumDiff;
    result#(symbol cache) = new CacheTable;
    -- Initialize the `diffs` caching table that moduleDifferential
    -- needs (otherwise isWellDefined / toComplex / homology all crash
    -- on key-not-found).
    result.cache#(symbol diffs) = new MutableHashTable;
    -- Record summands for `components`.
    result.cache#(symbol components) = {M, N};
    new DGModule from result
)

-- components DGModule: when M was built via ++ or directSum, return the
-- summand list; otherwise return {M}.
components DGModule := M -> (
    if M.cache#?(symbol components) then M.cache#(symbol components)
    else {M}
)

-- directSum of a sequence/list of DGModules.  Mirrors `directSum` for
-- Modules: fold ++ over the list.  Empty list -> zero DGModule over the
-- first-available DGAlgebra (error if we cannot infer).
directSum DGModule := M -> M
DGModule.directSum = args -> (
    if #args == 0 then error "directSum DGModule: need at least one summand.";
    fold((a, b) -> a ++ b, args)
)

-------------------------------------------------------------------------
-- image / kernel / cokernel of a DGModuleMap
--
--   image F     : DGSubmodule of target F.
--                 inclusion.natural = F.natural
--                 S.natural = source F.natural (same as M.natural)
--                 Since F is a chain map, F.natural's image is d-closed.
--
--   kernel F    : DGSubmodule of source F.
--                 inclusion.natural = gens kernel F.natural
--                 Closed under d since F is a chain map.
--
--   cokernel F  : DGQuotientModule target F / image F.
-------------------------------------------------------------------------

image DGModuleMap := F -> (
    -- Construct image(F) as a DG submodule of target(F). F.natural is
    -- automatically d-closed (F is a chain map), so no saturation is
    -- needed -- we just need a good free presentation.
    --
    -- Convention (matches M2's Matrix-level `image`): drop zero columns so
    -- that numgens (image F) reflects the rank of the underlying image
    -- rather than the arbitrary presentation width.  For a 0 map this
    -- yields the 0 submodule; for an injection the full source.
    N := target F;
    nat := F.natural;
    nz := select(numcols nat, j -> nat_{j} != 0);
    prunedMat := if #nz == numcols nat then nat
                 else if #nz == 0 then map(N.natural, (N.dgAlgebra.natural)^0, 0)
                 else nat_nz;
    dgSubmodule(N, prunedMat)
)

kernel DGModuleMap := opts -> F -> (
    M := source F;
    kerMat := gens kernel F.natural;
    dgSubmodule(M, kerMat)
)

cokernel DGModuleMap := F -> (
    N := target F;
    imgF := image F;
    dgQuotientModule(N, imgF)
)

-- In M2, `coker` and `cokernel` are aliases for the same MethodFunction
-- (cokernel = method(); coker = cokernel), so installing at one signature
-- also installs at the other.  We do NOT add a separate `coker` alias line
-- because `coker DGModuleMap := cokernel` would silently overwrite the
-- method with the MethodFunction itself, creating infinite self-recursion.

-------------------------------------------------------------------------
-- Backward-compatible tensor operations with non-DG types.
--
-- Semantics overview:
--   * `** Ring`   — base-change along the canonical substitute map
--                   A.ring -> S.  The result inherits the same DG shape
--                   with coefficients pushed forward to S.  When S is a
--                   QuotientRing R/I, this is "reduction mod I."
--   * `** Module` — exterior tensor M ⊗_R N for M a DGModule and N a
--                   free R-module.  The differential acts as d_M ⊗ id_N.
--   * `** Ideal`  — sugar for `** module I`.
--
-- Functoriality requirements that every implementation satisfies:
--   * identity  ** S   is isomorphic to the identity on (... ** S).
--   * (f o g)   ** S   agrees with (f ** S) o (g ** S) at the natural
--                      matrix level.
--   * isWellDefined holds on every tensor output (checked in regression).
-------------------------------------------------------------------------

-- Helper: canonical substitute from (any ring containing the source
-- generator-names) into a fresh target ring.  Errors out with a
-- contextualized message rather than the opaque M2 substitute error.
coerceAcrossRings := (src, tgt, opname) -> (
    try substitute(src, tgt)
    else error (opname | ": cannot substitute across rings; ensure the target ring's generator names include the source's.")
)

-- DGSubmodule ** Ring: base-change of ambient and inclusion.  Invariant:
-- if S' = S ** R is the base-change of S in ambient M, and M' = M ** R,
-- then S' is a well-defined DGSubmodule of M' with the pushed-forward
-- inclusion matrix.  The differential stays d-closed because d (the
-- base-changed differential on M') sends the S'-image to itself by the
-- same columns that d(S) landed in.
DGSubmodule ** Ring := (Sub, S) -> (
    Mnew := (Sub.ambient) ** S;
    Bnat := ring Mnew.natural;
    incOld := Sub.inclusion.natural;
    newEntries := apply(entries incOld, row ->
        apply(row, f -> coerceAcrossRings(f, Bnat, "DGSubmodule ** Ring"))
    );
    newInc := if numcols incOld == 0 then map(Mnew.natural, (Bnat)^0, 0)
              else map(Mnew.natural, , matrix newEntries);
    dgSubmodule(Mnew, newInc)
)

-- QuotientRing inherits from the Ring method above.

-- DGQuotientModule ** Ring: base-change the ambient and pushforward the
-- inclusion matrix onto the SAME freshly base-changed ambient, then
-- requote.  (Calling `(Q.subDGModule) ** S` recursively would build a
-- second copy of the base-changed ambient and trip the "submodule's
-- ambient must equal the given DGModule" check in dgQuotientModule.)
DGQuotientModule ** Ring := (Q, S) -> (
    Mnew := (Q.ambient) ** S;
    Bnat := ring Mnew.natural;
    incOld := Q.subDGModule.inclusion.natural;
    newEntries := apply(entries incOld, row ->
        apply(row, f -> coerceAcrossRings(f, Bnat, "DGQuotientModule ** Ring")));
    newInc := if numcols incOld == 0 then map(Mnew.natural, (Bnat)^0, 0)
              else map(Mnew.natural, , matrix newEntries);
    Subnew := dgSubmodule(Mnew, newInc);
    Mnew / Subnew
)

-- (QuotientRing handled via its Ring parent.)

-- DGIdeal ** Ring: base-change the ambient DGAlgebra and pushforward the
-- ideal generators into the new .natural ring.
DGIdeal ** Ring := (I, S) -> (
    Anew := (I.dgAlgebra) ** S;
    Bnat := Anew.natural;
    oldGens := first entries generators I.natural;
    newGens := apply(oldGens, g -> coerceAcrossRings(g, Bnat, "DGIdeal ** Ring"));
    dgIdeal(Anew, newGens)
)

-- (QuotientRing handled via its Ring parent.)

-- DGAlgebraMap ** Ring: base-change.  Given phi : A -> B, produces
-- phi ⊗ id_S : A ** S -> B ** S whose natural matrix is the image
-- matrix pushed into (B ** S).natural.
DGAlgebraMap ** Ring := (phi, S) -> (
    Anew := (phi.source) ** S;
    Bnew := (phi.target) ** S;
    Bnat := Bnew.natural;
    oldImgs := first entries matrix phi.natural;
    newImgs := apply(oldImgs, f ->
        coerceAcrossRings(f, Bnat, "DGAlgebraMap ** Ring"));
    dgAlgebraMap(Bnew, Anew, matrix {newImgs})
)

-- (QuotientRing handled via its Ring parent.)

-- DGModuleMap ** Ring: base-change of a DGModuleMap.  f : M -> N becomes
-- f ⊗ id_S : M ** S -> N ** S.  The natural matrix entries push forward
-- via the canonical substitute.
DGModuleMap ** Ring := (f, S) -> (
    Mnew := (f.source) ** S;
    Nnew := (f.target) ** S;
    Bnat := ring Nnew.natural;
    oldMat := f.natural;
    newEntries := apply(entries oldMat, row ->
        apply(row, g -> coerceAcrossRings(g, Bnat, "DGModuleMap ** Ring")));
    newMat := if numcols oldMat == 0 then map(Nnew.natural, Mnew.natural, 0)
              else map(Nnew.natural, Mnew.natural, matrix newEntries);
    dgModuleMap(Nnew, Mnew, newMat)
)

-- (QuotientRing handled via its Ring parent.)

-- DGModule ** Module: exterior tensor M ⊗_R N where M is a DGModule over
-- A and N is a free R-module.  The result is a free DGModule over A,
-- with generators {e_i ⊗ n_j}, hom-degree deg(e_i) (n_j contributes
-- internal degree), and differential d(e_i ⊗ n_j) = d(e_i) ⊗ n_j.
--
-- We insist N be a free module because M.natural ** N needs to come out
-- free over A.natural for our DGModule semantics.
DGModule ** Module := (M, N) -> (
    -- Cache M ** N on M so repeated calls agree (needed for sub/quot
    -- compatibility: `(Q.ambient) ** N` must match the ambient we
    -- expect in consuming operations).
    if M.cache#?"tensorWithModule" and (M.cache#"tensorWithModule")#?N then
        return (M.cache#"tensorWithModule")#N;
    A := M.dgAlgebra;
    Anat := A.natural;
    if not isFreeModule M.natural then
        error "DGModule ** Module: M.natural must be free (koszulComplexDGM/cokernel form not supported).";
    if not isFreeModule N then
        error "DGModule ** Module: N must be a free module.";
    rM := rank M.natural;
    rN := rank N;
    Ndegs := degrees N;
    -- Generator (i, j) of M ⊗_R N has hom-degree = first(M.Degrees#i)
    -- (N contributes nothing to hom-degree; it's an ordinary R-module)
    -- and internal degree = rest(M.Degrees#i) + Ndegs#j (component-wise).
    -- We pad Ndegs#j with zeros if the internal-degree vectors of M are
    -- longer than the R-degree vectors of N (e.g., multigraded A).
    padAdd := (lst1, lst2) -> apply(#lst1, k -> (
        a := lst1#k;
        b := if k < #lst2 then lst2#k else 0;
        a + b
    ));
    combinedDegrees := flatten apply(rM, i -> apply(rN, j -> (
        dM := M.Degrees#i;
        dN := Ndegs#j;
        {first dM} | padAdd(drop(dM, 1), dN)
    )));
    P := freeDGModule(A, combinedDegrees);
    Pgens := apply(rank P.natural, k -> (P.natural)_k);
    idxOf := (i, j) -> i * rN + j;
    newDiffs := flatten apply(rM, i -> apply(rN, j -> (
        dMe := M.diff#i;
        if dMe == 0 then 0_(P.natural)
        else (
            dEnts := entries dMe;
            sum apply(rM, k -> (
                coef := dEnts#k;
                if coef == 0 then 0_(P.natural)
                else coef * Pgens#(idxOf(k, j))
            ))
        )
    )));
    setDiff(P, newDiffs);
    if not M.cache#?"tensorWithModule" then
        M.cache#"tensorWithModule" = new MutableHashTable;
    (M.cache#"tensorWithModule")#N = P;
    P
)

-- Commuted form: Module ** DGModule.  By convention the "primary" factor
-- is the DG one, so we just forward.
Module ** DGModule := (N, M) -> M ** N

-- DGModule ** Ideal is intentionally NOT defined, because `module I` is
-- typically non-free and we require free N.  For quotient-style base
-- changes use `M ** (R/I)`; for I·M inside M use `I' * M` with a DGIdeal
-- I' whose generators are the ideal generators lifted to A.natural.

-- DGSubmodule ** Module: base-change the ambient by exterior tensor with
-- N, then pushforward the inclusion to the new natural module.  Column
-- e_ℓ of the old inclusion becomes (e_ℓ ⊗ n_0, e_ℓ ⊗ n_1, ...) in the
-- product indexing (flat index ℓ * rN + j) — i.e. one copy per generator
-- of N.  d-closure is preserved because d does not touch the N-factor.
DGSubmodule ** Module := (Sub, N) -> (
    M := Sub.ambient;
    Mnew := M ** N;
    if not isFreeModule N then
        error "DGSubmodule ** Module: N must be a free module.";
    rN := rank N;
    rM := rank M.natural;
    incOld := Sub.inclusion.natural;
    numCols := numcols incOld;
    Bnat := ring Mnew.natural;
    -- Build the (rM*rN) x (numCols*rN) coefficient matrix directly.
    -- Column index (c, j) with c in 0..numCols-1, j in 0..rN-1 has
    -- entries: row (i*rN + j') = (incOld)_(i,c) if j' == j, else 0.
    newRows := apply(rM * rN, r -> (
        i := r // rN;
        jprime := r % rN;
        apply(numCols * rN, colIdx -> (
            c := colIdx // rN;
            j := colIdx % rN;
            if j != jprime then 0_Bnat
            else sub(incOld_(i, c), Bnat)
        ))
    ));
    newInc := if numCols == 0 then map(Mnew.natural, (Bnat)^0, 0)
              else map(Mnew.natural, , matrix newRows);
    dgSubmodule(Mnew, newInc)
)

-- DGSubmodule ** Ideal: see the note above `DGModule ** Ideal` — not
-- defined because the ideal's module is typically non-free.

-- DGQuotientModule ** Module: base-change the ambient and pushforward
-- the killed submodule against the SAME base-changed ambient, then
-- requote.  (Same subtlety as `** Ring`: two separate ambients would
-- trip the sub-ambient equality check in dgQuotientModule.)
DGQuotientModule ** Module := (Q, N) -> (
    if not isFreeModule N then
        error "DGQuotientModule ** Module: N must be a free module.";
    M := Q.ambient;
    Mnew := M ** N;
    rN := rank N;
    rM := rank M.natural;
    incOld := Q.subDGModule.inclusion.natural;
    numCols := numcols incOld;
    Bnat := ring Mnew.natural;
    newRows := apply(rM * rN, r -> (
        i := r // rN;
        jprime := r % rN;
        apply(numCols * rN, colIdx -> (
            c := colIdx // rN;
            j := colIdx % rN;
            if j != jprime then 0_Bnat
            else sub(incOld_(i, c), Bnat)
        ))
    ));
    newInc := if numCols == 0 then map(Mnew.natural, (Bnat)^0, 0)
              else map(Mnew.natural, , matrix newRows);
    Subnew := dgSubmodule(Mnew, newInc);
    Mnew / Subnew
)

-- DGQuotientModule ** Ideal: see the `** Ideal` note above.

-- Commuted forms for sub/quot-module ** Module.
Module ** DGSubmodule        := (N, Sub) -> Sub ** N
Module ** DGQuotientModule   := (N, Q)   -> Q ** N

--------------------
-- Documentation  --
--------------------

beginDocumentation()

load "./DGAlgebras/doc.m2"

-------------------------------
--          Testing          --
-------------------------------
load "./DGAlgebras/tests.m2"

end--

-- How to install the package
restart
loadPackage("DGAlgebras", Reload =>true)
restart
uninstallPackage "DGAlgebras"
restart
installPackage "DGAlgebras"


check "DGAlgebras"
viewHelp DGAlgebras



-- Below, we provide some of the examples used in development, unsupported
-- and undocumented for the user.

--- homologyModule code
restart
debug needsPackage "DGAlgebras"
Q = QQ[x,y,z,w]
I = ideal (w^2, y*w+z*w, x*w, y*z+z^2, y^2+z*w, x*y+x*z, x^2+z*w)
R = Q/I
KR = koszulComplexDGA R
cxKR = toComplex KR
HKR = HH(KR)
degList = first entries vars Q / degree / first
-- This is the (graded) canonical module of M
M = Ext^4(Q^1/I,Q^{-(sum degList)}) ** R
KM = cxKR ** M
HKM = homologyModule(KR,M)
reduceHilbert hilbertSeries HKR
reduceHilbert hilbertSeries HKM

--- massey operations code
restart
needsPackage "DGAlgebras"
Q = QQ[x_1,x_2,y_1,y_2,z]
I = ideal (x_1*x_2^2,y_1*y_2^2,z^3,x_1*x_2*y_1*y_2,y_2^2*z^2,x_2^2*z^2,x_1*y_1*z,x_2^2*y_2^2*z)
R = Q/I
KR = koszulComplexDGA R
z1 = z^2*T_5
z2 = y_2^2*T_3
z3 = x_2^2*T_1
getBoundaryPreimage(KR,z1*z2)
getBoundaryPreimage(KR,z2*z3)
z123 = masseyTripleProduct(KR,z1,z2,z3)
H = HH(KR)
h1 = homologyClass(KR,z1)
h2 = homologyClass(KR,z2)
h3 = homologyClass(KR,z3)
--- BUG - need to check the h's are in the homology algebra
h123 = masseyTripleProduct(KR,h1,h2,h3)
h123 == homologyClass z123

-- Bug from the Macaulay2 Google Group
restart
needsPackage "DGAlgebras"
R = ZZ/32003[t, Inverses=>true, MonomialOrder=>RevLex]
I = ideal t^2
A = koszulComplexDGA(I)
skewList = select(toList(0..(#degList-1)), i -> odd first degList#i)
(A.ring)[varsList, Degrees=>{{1}}, Join => false, SkewCommutative => skewList]
(A.ring)[varsList]
-- so the DGAlgebras package does not work over local rings?

-- Demo
-- 'Finite' DGAlgebras: the Koszul Complex
restart
loadPackage "DGAlgebras"
R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
KR = koszulComplexDGA(R)
toComplex KR
KR.dd
HKR = HH KR
describe HKR
peek HKR.cache

S = R/ideal{a^2*b^2*c^2*d^2}
KS = koszulComplexDGA(S)
HKS = HH KS;
numgens HKS
numgens ideal HKS
(ideal HKS)_*
peek HKS.cache

-- (potentially) infinite partial DGAlgebras : acyclic closures
restart
loadPackage "DGAlgebras"
R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
XR = acyclicClosure(R)
apply(gens XR.natural, x -> XR.diff(x))

S = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3,a^2*b^2*c^2*d^2}
XS = acyclicClosure(S)
netList apply(gens XS.natural, x -> XS.diff(x))

-- homotopy fiber of projection R --> S
homotopyFiber = XR ** S
homologyHomotopyFiber = HH homotopyFiber
homologyHomotopyFiber = homologyAlgebra( homotopyFiber, GenDegreeLimit=>4, RelDegreeLimit=>8)
numgens homologyHomotopyFiber
numgens ideal homologyHomotopyFiber
peek homologyHomotopyFiber.cache

-- maps on Tor algebras of rings
phi = map(S,R)
torPhi = torMap phi
matrix torPhi
-- Note: need to add ker for ring maps from skew rings :)
ker torPhi

-- lifting ring maps to DG maps
phiTilde = liftToDGMap(XS,XR,phi)
apply(gens XR.natural, x -> phiTilde.natural x)

-- deviations
restart
loadPackage "DGAlgebras"
R1 = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
R2 = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3,a^2*b^2*c^2*d^2}
devR1 = deviations R1
devR2 = deviations R2
poincR2 = deviationsToPoincare devR2
coefficients(poincR2, Variables=>{first gens ring poincR2})
freeResolution coker vars R2
R3 = ZZ/101[a,b,c,d,Degrees=>entries id_(ZZ^4)]/ideal{a^3,b^3,c^3,d^3,a^2*b^2*c^2*d^2}
degrees R3
devR3 = deviations R3
-- bug here!
deviationsToPoincare devR3

-- Golod DG Algebras and trivial Massey operations
restart
loadPackage "DGAlgebras"
Q = ZZ/101[x_1..x_6]
I = ideal (x_3*x_5,x_4*x_5,x_1*x_6,x_3*x_6,x_4*x_6)
R = Q/I
A = koszulComplexDGA(R)
isHomologyAlgebraTrivial A
cycleList = getGenerators(A)
tmo = findTrivialMasseyOperation(A)

-- stub function documentation node
doc ///
  Key
    myStub
    (myStub,DGAlgebra)
  Headline
    Stub headline
  Usage
    tmo = myStub(A)
  Inputs
    A:DGAlgebra
  Outputs
    tmo:List
      Stub Output
  Description
    Text
      This is a stub.
    Example
      Q = ZZ/101[x_1..x_6]
      I = ideal (x_3*x_5,x_4*x_5,x_1*x_6,x_3*x_6,x_4*x_6)
    Text
      More stub.
///

--stub option node

doc ///
  Key
    StubOption
  Headline
    Stub Option Headline
  Usage
    homologyAlgebra(...,StubOption=>n)
///

-- JSAG examples
restart
needsPackage "DGAlgebras";
R = ZZ/101[a,b,c]/ideal{a^3,b^3,a^2*b^2};
KR = koszulComplexDGA R
HKR = HH KR;
gens HKR
ideal HKR

-- trying to make pushForward functorial
restart
loadPackage "DGAlgebras"
R = ZZ/101[x,y,z]
S = R[w]/ideal{w^3,x*w^2}
f = map(S,R)
M = matrix {{x + x*w + w^2,w},{0,w^2}}
A = source M
B = target M
Mpush = pushForward(f,M)
-- test functoriality of pushForward
kSRes = freeResolution(coker matrix {{x,y,z,w}}, LengthLimit=>5)
kSRes1push = pushForward(f,kSRes.dd_1)
kSRes2push = pushForward(f,kSRes.dd_2)
kSRes3push = pushForward(f,kSRes.dd_3)
kSRes1push*kSRes2push
kSRes2push*kSRes3push
prune homology(kSRes1push,kSRes2push)
prune homology(kSRes2push,kSRes3push)
-- pushforward the Complex
kSResPush = pushForward(f,kSRes)
prune HH kSResPush

-- lifting functions
restart
loadPackage "DGAlgebras"
R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
S = R/ideal{a^2*b^2*c^2}
f = map(S,R)
A = acyclicClosure(R,EndDegree=>3)
time(B = acyclicClosure(S,EndDegree=>3))
phi = liftToDGMap(B,A,f)
-- can't do the following yet, since M2 can't handle maps between chain complexes
-- over different rings. (Should pushforward the target and then try?)
toComplexMap(phi,EndDegree=>3)

-- torMap
restart
loadPackage "DGAlgebras"
printWidth=74
R = ZZ/101[a,b]/ideal{a^3,b^3,a^2*b^2}
S = R/ideal{a*b^2,a^2*b}
f = map(S,R)
Torf = torMap(f,GenDegreeLimit=>3);
TorR = source Torf
TorS = target Torf
Torf

-- below is a good test, since the target is less complicated than the source
-- which is backwards than the usual behavior
restart
loadPackage "DGAlgebras"
R = ZZ/101[a,b,c]/ideal{a^2,b^2,c^2,d^2,a*c,a*d,b*c,b*d}
S = ZZ/101[a,b]/ideal{a^2,b^2}
f = map(S,R,matrix{{a,b,0,0}})
Torf = torMap(f,GenDegreeLimit=>4);
TorR = source Torf;
TorS = target Torf;
-- note the homomorphism is large
matrix Torf

-- DGAlgebraMap Testing
restart
loadPackage "DGAlgebras"
R = ZZ/101[a,b,c]/ideal{a^3+b^3+c^3,a*b*c}
K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
K2 = koszulComplexDGA(ideal {b,c},Variable=>"T")
f = dgAlgebraMap(K2,K1,matrix{{0,T_1,T_2}})
isWellDefined f
g = dgAlgebraMap(K1,K2,matrix{{Y_2,Y_3}})
isWellDefined g
source g
target g
toComplexMap g
HHg = HH g

restart
loadPackage "DGAlgebras"
R = ZZ/101[a,b,c]/ideal{a^3+b^3+c^3,a*b*c}
K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
K2 = koszulComplexDGA(ideal vars R,Variable=>"Z")
g = dgAlgebraMap(K2,K1,matrix{{Z_1,Z_2,Z_3}})
isWellDefined g
HH g

-- change of rings DGAlgebraMap currently does not work yet, since M2 expects
-- matrices to be defined between free modules over the same ring.  Need to use
-- pushForward (for Complex; not yet written) for this to work.
restart
loadPackage "DGAlgebras"
R = ZZ/101[a,b,c]/ideal{a^2+b^2+c^2}
K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
S = ZZ/101[a,b]/ideal{a^2+b^2}
K2 = koszulComplexDGA(ideal vars S,Variable=>"T")
f = dgAlgebraMap(K2,K1,matrix{{T_1,T_2,0}})
isWellDefined f
toComplexMap f

-- Some examples and things that will eventually make it into the program and documentation.
------------------
--- Taylor's resolution code
restart
loadPackage "DGAlgebras"
R = ZZ/101[a,b]
I = monomialIdeal (a^2,a*b,b^2)
degList = reverse {{1,2,0},{1,1,1},{1,0,2},{2,2,1},{2,2,2},{2,1,2},{3,2,2}}
skewList = toList select(0..#degList-1, i -> odd first degList#i)
A = R[t123,t23,t13,t12,t3,t2,t1,MonomialOrder=>{4,3},SkewCommutative=>skewList, Degrees=>degList]/ideal(a*t12-t1*t2, t13-t1*t3, b*t23-t2*t3, a*t123-t1*t23, a*b*t123+t2*t13, b*t123-t12*t3,t12^2,t23^2,t12*t1,t12*t2,t23*t2,t23*t3,t123*t1,t123*t2,t123*t3,t123*t12,t123*t13,t123*t23,t12*t23)
-- above is how to represent the algebra in M2; not really a better way to do it.
basis(A)
I = sub(ideal (a^4,b^4),A)
B = A/I
basis(B)
-- note that the command basis(A) does not return the desired answer.  There are two problems.
-- first of all, it thinks that the module is not finite over the base (R), even though it is.
-- secondly, if we add in a^n and b^n to make it finite over ZZ/101, the answer given is not a basis -
--   the basis should be 1,t1,t2,t3,t12,t13,t23,t123 (should not have t1*t2, t1*t3, etc)
-- Note that A is a free R-module, with basis t1,t2,t3,t12,t13,t23,t123.
-- How can we get this basis in general, at least in the case that A is a free R-module?
--------------

--Tutorial (Include in a separate file?)
-- Koszul Complex and homology algebras
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R1 = ZZ/32003[x,y,z]
A1 = koszulComplexDGA(R1)
A1Complex = toComplex A1
A1Complex.dd
HA1 = homologyAlgebra(A1)
HA1 = HH A
describe HA1
R2 = R1/ideal{x^3,y^4,z^5}
A2 = koszulComplexDGA(R2)
time HA2 = homologyAlgebra(A2)
describe HA2
reduceHilbert hilbertSeries HA2
use R1
R3 = R1/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
A3 = koszulComplexDGA(R3)
time HA3 = homologyAlgebra(A3)
describe HA3
reduceHilbert hilbertSeries HA3

restart
loadPackage "DGAlgebras"
Q = QQ[x,y,z]
I = ideal{y^3,z*x^2,y*(z^2+y*x),z^3+2*x*y*z,x*(z^2+y*x),z*y^2,x^3,z*(z^2+2*x*y)}
R = Q/I
ann ideal vars R
A = koszulComplexDGA(R)
time HA = homologyAlgebra(A)
reduceHilbert hilbertSeries HA
ann ideal vars HA

-- the following example baffles me.  The 'same' ideal is Gorenstein in characteristic 2, and Golod in characteristic 32003 (probably)
Q2 = ZZ/2[x,y,z]
f_1 = x^3*y + x^3*z + x*z^3+y*z^3
f_2 = x*y^3+y^3*z+x*z^3+y*z^3
f_3 = x*y^2*z+x*y*z^2+x*y^3+x^3*y+x*z^3+x^3*z
f_4 = x^2*y*z+x*y^2*z+x^3*z+x*z^3+y^3*z+y*z^3
f_5 = x^4+y^4+z^4+x^2*y^2+x^2*z^2+y^2*z^2+x^2*y*z+x*y^2*z+x*y*z^2+x^3*y+x^3*z
I2 = ideal{f_1,f_2,f_3,f_4,f_5}
R2 = Q2/I2
ann ideal vars R2
A2 = koszulComplexDGA(R2)
time HA2 = homologyAlgebra(A2)
reduceHilbert hilbertSeries HA2
ann ideal vars HA2

Q = ZZ/32003[x,y,z]
f_1 = x^3*y + x^3*z + x*z^3+y*z^3
f_2 = x*y^3+y^3*z+x*z^3+y*z^3
f_3 = x*y^2*z+x*y*z^2+x*y^3+x^3*y+x*z^3+x^3*z
f_4 = x^2*y*z+x*y^2*z+x^3*z+x*z^3+y^3*z+y*z^3
f_5 = x^4+y^4+z^4+x^2*y^2+x^2*z^2+y^2*z^2+x^2*y*z+x*y^2*z+x*y*z^2+x^3*y+x^3*z
I = ideal{f_1,f_2,f_3,f_4,f_5}
R = Q/I
ann ideal vars R
A = koszulComplexDGA(R)
time HA = homologyAlgebra(A)
-- should check HA by hand since the homology algebra is still monomial.
reduceHilbert hilbertSeries HA
isHomologyAlgebraTrivial(A)
ann ideal vars HA

-- fiber product example
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^4,a*x,a*y,b*x,b*y}
apply((numgens R) + 1, i -> numgens prune HH_i(koszul vars R))
A = koszulComplexDGA(R)
-- 1.17 seconds on mbp
time HA = homologyAlgebra(A)
HA.cache.cycles
socHAgens = (ann ideal vars HA)_*
-- kill all elements of the socle of the 'wrong degree'
-- the generators we are killing are elements in W from the theorem,
-- and are zero b/c they are part of a trivial extension.  The
-- others are actual problem elements that are actually zero in the
-- connected sum.
HB = HA / ideal (select(socHAgens, i -> first degree i < 4))
-- identify the generators of the right degree
HB = HB / ideal (X_7*X_25-X_5*X_24)
-- now have a PD algebra.
mingens ann ideal vars HB
-- now we trivially extend by a graded vector space, as well as its dual to get a new PD algebra, the
-- Koszul homology algebra of a connected sum (computed below)
reduceHilbert hilbertSeries HA
reduceHilbert hilbertSeries HB
peek HA.cache

-- ungraded connected sum example
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^4,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^3}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
-- 3.8 seconds on mbp 
-- error here now - not sure why
time HA = homologyAlgebra(A)
socHA = ideal getBasis(4,HA)
HA.cache.cycles
reduceHilbert hilbertSeries HA
socHA = ideal getBasis(4,HA)
ann ideal vars HA
peek HA.cache

-- connected sum example
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
-- 2.7 seconds on mbp, with graded differentials
time HA = homologyAlgebra(A)
reduceHilbert hilbertSeries HA

-- connected sum example
-- goal: get this example to run quickly
restart
loadPackage "DGAlgebras"
R2 = ZZ/32003[a,b,x,y,z]/ideal{a^4,b^4,x^3,y^3,z^3,a*x,a*y,a*z,b*x,b*y,b*z,a^3*b^3-x^2*y^2*z^2}
A2 = koszulComplexDGA(R2)
time apply(6, i -> numgens prune homology(i,A2))
koszulR2 = koszul vars R2
time apply(6,i -> numgens prune HH_i(koszulR2))
-- 56 seconds on mbp
time HA2 = homologyAlgebra(A2)
numgens HA2
numgens ideal HA2
tally ((flatten entries basis HA2) / degree)
tally (((flatten entries basis HA2) / degree) / first)

-- This toric algebra is CM and not Koszul or Golod.  Is its homology algebra trivial?
restart
loadPackage "DGAlgebras"
R = QQ[x_1..x_6]/ideal(x_2^2-x_1*x_4,x_3^2-x_2*x_5,x_3*x_4-x_1*x_6,x_4^2-x_3*x_5,x_5^2-x_2*x_6)
A = koszulComplexDGA(R)
HA = HH A
isHomologyAlgebraTrivial(A)
-- no.

-- This algebra is not Golod, since its Poincare series is irrational.  But is its homology algebra trivial?
restart
loadPackage "DGAlgebras"
Q = QQ[a,b,c,d,e,f,g,h,i]
I = ideal (h^2-a*i,g^2-c*h,f^2-e*g,e*f-b*h,e^2-d*g,d*e-a*h,d^2-c*e,c*g-a*h,c*d-b*f,c^2-a*g,b*d-a*f,b^2-a*c)
freeResolution coker gens I
oo.dd
R = Q/I
A = koszulComplexDGA(R)
isHomologyAlgebraTrivial(A)
-- no.

-- connected sum example
-- goal: get this example to run quicker?
restart
loadPackage "DGAlgebras"
gbTrace = 2
R2 = ZZ/32003[a,b,c,x,y,z]/ideal{a^3,b^3,c^3,x^3,y^3,z^3,a*x,a*y,a*z,b*x,b*y,b*z,c*x,c*y,c*z,a^2*b^2*c^2-x^2*y^2*z^2}
A2 = koszulComplexDGA(R2)
time apply(7, i -> numgens prune homology(i,A2))
koszulR2 = koszul vars R2
time apply(7,i -> numgens prune HH_i(koszulR2))
time HA2 = homologyAlgebra(A2)
tally ((flatten entries basis HA2) / degree)
tally (((flatten entries basis HA2) / degree) / first)
-- 146 generators and 10662 relations (at least; didn't forceGB properly when I ran it before)

-- Tate resolution, toComplex
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = QQ[x,y,z,w]/ideal{x^3,y^4,z^5}
A = acyclicClosure(R,EndDegree=>1)
time Add = toComplex(A,20);
time kRes = freeResolution(coker vars R, LengthLimit => 20)

-- Homology
restart
loadPackage "DGAlgebras"
R3 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
A3 = acyclicClosure(R3,EndDegree=>1)
time apply(7, i -> time numgens prune homology(i,A3))
time kRes = freeResolution(coker vars R3, LengthLimit=> 18)
time apply(17, i -> time HH_i(kRes));

-- Tor algebras
restart
loadPackage "DGAlgebras"
R3 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
time TorR3 = torAlgebra(R3)
apply(16, i -> hilbertFunction(i,TorR3))
time freeResolution(coker vars R3, LengthLimit => 15)
R4 = QQ[x,y,z]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
TorR4 = torAlgebra(R4,GenDegreeLimit=>8)
apply(10, i -> hilbertFunction(i,TorR4))
freeResolution(coker vars R4, LengthLimit => 9)
TorR3R4 = torAlgebra(R3,R4,GenDegreeLimit=>4,RelDegreeLimit=>10)
reduceHilbert hilbertSeries TorR3R4
use R3
R4mod = coker matrix {{x^2*y^3*z^4}}
freeResolution(R4mod, LengthLimit => 6)

-- Acyclic closures
restart
loadPackage "DGAlgebras"
R3 = ZZ/32003[x,y]/ideal{x^3,y^4,x^2*y^3}
time A3 = acyclicClosure(R3,EndDegree=>5)
time HA3 = homologyAlgebra(A3,GenDegreeLimit=>6,RelDegreeLimit=>12)
time apply(12, i -> #(flatten entries getBasis(i,HA3)))
-- need to check the mult structure from Lucho's book.

-- The examples below are related to work in progress by Berest-Khatchatryan-Ramadoss on derived representation varieties
-- CC[x,y]; n = 2
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = QQ[x_11,x_12,x_21,x_22,y_11,y_12,y_21,y_22]
A = freeDGAlgebra(R,{{1,2},{1,2},{1,2}})
setDiff(A,{x_12*y_21 - x_21*y_12,
	   x_21*y_11+x_22*y_21-x_11*y_21-x_21*y_22,
	   x_11*y_12+x_12*y_22-x_12*y_11-x_22*y_12})
homList = apply(5,i -> numgens prune homology(i,A))
HA = homologyAlgebra(A)
HA2 = zerothHomology A
describe HA
describe HA2
degrees HA
degrees HA2
M1 = getDegNModule(0,HA2,HA)
reduceHilbert hilbertSeries M1
dim M1
M2 = getDegNModule(1,HA2,HA)
reduceHilbert hilbertSeries M2
dim M2
K = koszul vars HA2
-- is HA2 CM?
prune HH(K)
-- is M CM?
prune HH(K ** M)
getDegNModule(2,HA2,HA)

-- U(sl_2) example
restart
loadPackage "DGAlgebras"
debug DGAlgebras
x = symbol x; y = symbol y; z = symbol z; T = symbol T;
n = 2
pairsList = toList (set(1..n)**set(1..n))
symbolList = var -> apply(pairsList, i -> var_i)
R = QQ[a,b,symbolList x, symbolList y, symbolList z, Degrees=>{0,0}|toList (3*n^2:1)]
A = freeDGAlgebra(R,toList (((n^2*3):{1,2}) | (n^2:{2,3})))
tDiffList = apply(pairsList, p -> sum apply(toList (1..n), i -> x_(p#0,i)*T_(p#1+2*(i-1)) - T_(2*(p#0-1)+i)*x_(i,p#1) +
	                                                        y_(p#0,i)*T_(n^2+p#1+2*(i-1)) - T_(n^2+2*(p#0-1)+i)*y_(i,p#1) +
	                                                        z_(p#0,i)*T_(2*n^2+p#1+2*(i-1)) - T_(2*n^2+2*(p#0-1)+i)*z_(i,p#1)))
netList tDiffList
xDiffList = apply(pairsList, p -> sum apply(toList (1..n), i -> a*y_(p#0,i)*z_(i,p#1) + b*z_(p#0,i)*y_(i,p#1) + (1-a-b)*x_(p#0,i)*x_(i,p#1)))
netList xDiffList
yDiffList = apply(pairsList, p -> sum apply(toList (1..n), i -> a*z_(p#0,i)*x_(i,p#1) + b*x_(p#0,i)*z_(i,p#1) + (1-a-b)*y_(p#0,i)*y_(i,p#1)))
netList yDiffList
zDiffList = apply(pairsList, p -> sum apply(toList (1..n), i -> a*x_(p#0,i)*y_(i,p#1) + b*y_(p#0,i)*x_(i,p#1) + (1-a-b)*z_(p#0,i)*z_(i,p#1)))
netList zDiffList
allDiffs = xDiffList | yDiffList | zDiffList | tDiffList
netList allDiffs
#allDiffs
setDiff(A, allDiffs, InitializeDegreeZeroHomology => false)
complexA = toComplex(A,5)
-- will not finish
numgens HH_0(complexA)
numgens HH_1(complexA)
numgens HH_2(complexA)
numgens HH_3(complexA)
numgens HH_4(complexA)

-- CC[x,y,z] n=2 example
restart
loadPackage "DGAlgebras"
debug DGAlgebras
x = symbol x; y = symbol y; z = symbol z; T = symbol T;
n = 2
pairsList = toList (set(1..n)**set(1..n))
symbolList = var -> apply(pairsList, i -> var_i)
R = QQ[symbolList x, symbolList y, symbolList z, Degrees=>toList (3*n^2:1)]
A = freeDGAlgebra(R,toList (((n^2*3):{1,2}) | (n^2:{2,3})))
tDiffList = apply(pairsList, p -> sum apply(toList (1..n), i -> x_(p#0,i)*T_(p#1+2*(i-1)) - T_(2*(p#0-1)+i)*x_(i,p#1) +
	                                                        y_(p#0,i)*T_(n^2+p#1+2*(i-1)) - T_(n^2+2*(p#0-1)+i)*y_(i,p#1) +
	                                                        z_(p#0,i)*T_(2*n^2+p#1+2*(i-1)) - T_(2*n^2+2*(p#0-1)+i)*z_(i,p#1)))
netList tDiffList
xDiffList = apply(pairsList, p -> sum apply(toList (1..n), i -> y_(p#0,i)*z_(i,p#1) - z_(p#0,i)*y_(i,p#1)))
netList xDiffList
yDiffList = apply(pairsList, p -> sum apply(toList (1..n), i -> z_(p#0,i)*x_(i,p#1) - x_(p#0,i)*z_(i,p#1)))
netList yDiffList
zDiffList = apply(pairsList, p -> sum apply(toList (1..n), i -> x_(p#0,i)*y_(i,p#1) - y_(p#0,i)*x_(i,p#1)))
netList zDiffList
allDiffs = xDiffList | yDiffList | zDiffList | tDiffList
netList allDiffs
-- I checked carefully that the code above generates the proper differentials.
setDiff(A, allDiffs, InitializeComplex => false)
H0Ring = zerothHomology A
dim H0Ring
K = koszul vars H0Ring
-- is H0Ring CM?
prune HH(K)
-- is M CM?
H1 = prune HH_1(A); numgens H1
H2 = prune HH_2(A); numgens H2
H3 = prune HH_3(A); numgens H3
H4 = prune HH_4(A); numgens H4
H5 = prune HH_5(A); numgens H5
-- compute the homology algebra?
-- degree zero is not a field, so the GB trick no longer works.  Think about a better way of doing this?
HA = homologyAlgebra(A, GenDegreeLimit=>2, RelDegreeLimit=>4);
netList select(flatten entries gens ideal HA, f -> first degree f == 2)
tally apply(degrees ideal HA, i -> first i)
tally apply(degrees HA, i -> first i)
M = getDegNModule(1,H0Ring,HA)
prune HH(K ** M)
genList = getGenerators(A,DegreeLimit=>5);

-- CC[x,y] n = 3 example
restart
loadPackage "DGAlgebras"
debug DGAlgebras
x = symbol x; y = symbol y; T = symbol T;
n = 3
pairsList = toList (set(1..n)**set(1..n))
symbolList = var -> apply(pairsList, i -> var_i)
R = QQ[symbolList x, symbolList y, Degrees=>toList (2*n^2:1)]
A = freeDGAlgebra(R,toList ((n^2):{1,2}))
tDiffList = apply(pairsList, p -> sum apply(toList (1..n), i -> x_(p#0,i)*y_(i,p#1) - y_(p#0,i)*x_(i,p#1)))
netList tDiffList
setDiff(A, tDiffList, InitializeComplex => false)
homologyList = apply(5,i -> numgens prune homology(i,A))
H0 = HH_0(A); numgens H0
H1 = prune HH_1(A); numgens H1
H2 = prune HH_2(A); numgens H2
H3 = prune HH_3(A); numgens H3
H4 = prune HH_4(A); numgens H4
H5 = prune HH_5(A); numgens H5
H6 = prune HH_6(A); numgens H6
H7 = prune HH_7(A); numgens H7
H8 = prune HH_8(A); numgens H8
H9 = prune HH_9(A); numgens H9
genList = getGenerators(A, DegreeLimit=>3)
HA = homologyAlgebra(A)

-- CC[x]/x^2 n=2
restart
loadPackage "DGAlgebras"
debug DGAlgebras
x = symbol x; T = symbol T;
n = 2
pairsList = toList (set(1..n)**set(1..n))
symbolList = var -> apply(pairsList, i -> var_i)
R = QQ[symbolList x, Degrees=>toList (n^2:1)]
A = freeDGAlgebra(R,toList ((n^2):{1,2}))
tDiffList = apply(pairsList, p -> x_(p#0,1)*x_(1,p#1) + x_(p#0,2)*x_(2,p#1))
netList tDiffList
setDiff(A, tDiffList, InitializeComplex => false)
HA = HH(A)
homologyList = apply(5,i -> numgens prune homology(i,A))
H0 = HH_0(A); numgens H0
H1 = prune HH_1(A); numgens H1
H2 = prune HH_2(A); numgens H2
H3 = prune HH_3(A); numgens H3
H4 = prune HH_4(A); numgens H4
H5 = prune HH_5(A); numgens H5
H6 = prune HH_6(A); numgens H6
H7 = prune HH_7(A); numgens H7

-- CC[x]/x^2 n=3
restart
loadPackage "DGAlgebras"
debug DGAlgebras
x = symbol x; T = symbol T;
n = 3
pairsList = toList (set(1..n)**set(1..n))
symbolList = var -> apply(pairsList, i -> var_i)
R = QQ[symbolList x, Degrees=>toList (n^2:1)]
A = freeDGAlgebra(R,toList ((n^2):{1,2}))
tDiffList = apply(pairsList, p -> x_(p#0,1)*x_(1,p#1) + x_(p#0,2)*x_(2,p#1)+ x_(p#0,3)*x_(3,p#1))
netList tDiffList
setDiff(A, tDiffList, InitializeComplex => false)
HA = HH(A)
homologyList = apply(5,i -> numgens prune homology(i,A))
H0 = HH_0(A); numgens H0
H1 = prune HH_1(A); numgens H1
H2 = prune HH_2(A); numgens H2
H3 = prune HH_3(A); numgens H3
H4 = prune HH_4(A); numgens H4
H5 = prune HH_5(A); numgens H5
H6 = prune HH_6(A); numgens H6
H7 = prune HH_7(A); numgens H7


------
--- From Google Group
restart
needsPackage "DGAlgebras"
R = ZZ/101[x,y,z]
A = freeDGAlgebra(R,{{1},{1},{1},{3}})
A.natural
setDiff(A,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
isHomogeneous(A)



restart
uninstallPackage "DGAlgebras"
restart
installPackage "DGAlgebras"
check "DGAlgebras"
debug needsPackage "DGAlgebras"
loadPackage ("DGAlgebras", Reload => true)
--load "blockDiff.m2"


R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
A = acyclicClosure(R,EndDegree => 2)
displayBlockDiff(A,2)
X = blockDiff(A,2)
X^[{1,0,0}]_[{2,0,0}]
X^[{1,0,0}]_[{0,2,0}]
X = blockDiff(A,5)

-- this is the differential returned by DGAlgebras by default
polyDifferential(7,A)




restart
debug needsPackage "DGAlgebras"
load "blockDiff.m2"

R = QQ[x,y,z]/(ideal(x^3,y^3,z^3))
A = acyclicClosure(R)

-- another example
R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
A = acyclicClosure(R,EndDegree => 3)

-- this displays the differential with headings corresponding to the (i,j) and (p,q)
displayBlockDiff(A,7)
-- this creates the matrix as a "matrix of matrices"
elapsedTime X = blockDiff(A,9);
-- which one may then select blocks using X^[...]_[...]
X^[0]
X^[0]_[0]
-- this is the differential returned by DGAlgebras by default
polyDifferential(7,A)


--- most recent example:
restart
needsPackage "DGAlgebras"
R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
A = acyclicClosure(R,EndDegree => 2)
displayBlockDiff(A,5)
X = blockDiff(A,5)
indices source X
indices target X
Y = X^[{1,0,3}]_[{0,2,3}]  -- works now!
X^[{1,0,3},{0,4,0}]_[{0,2,3}]
tar = [{1,0,3},{0,4,0}]
sour = [{0,2,3}] 
X_tar^sour
isHomogeneous Y
degrees source Y
degrees target Y


///
R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
A = acyclicClosure(R,EndDegree => 2)
L = indices source blockDiff(A,5)
displayBlockDiff(A,  [{0,2,3}], [{1,0,3},{0,4,0}])
displayBlockDiff(A,  {0,2,3}, {0,4,0})
displayBlockDiff(A,  {1,0,3})
displayBlockDiff(A,  [{1,0,3}])

///
