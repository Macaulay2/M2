-*- coding: utf-8 -*-
newPackage("DGAlgebras",
     Headline => "Data type for DG algebras",
     Version => "2.0",
     Date => "August 20, 2020",
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
     PackageExports => {"IntegralClosure", "OldChainComplexes", "Complexes"}
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

doc ///
  Key
    DGAlgebras
  Headline
    Data types and basic functions on differential graded (DG) Algebras.
  Description
    Text
      This package is used to define and manipulate DG algebras.
  Contributors
    Some documentation was added by Daniel Rostamloo and David Eisenbud.
  Subnodes
    "Basic operations on DG Algebras"
    "The Koszul complex as a DG Algebra"
    "Basic operations on DG Algebra Maps"
    "Basic operations on DG Module Maps"
    "Base change and tensor with non-DG types"
    "Operations on DG Ideals"
    "Building DG modules, submodules, and quotients"
    "Module-like operations on DG modules"
    "Operations on DG Submodules"
    "Image, kernel, and cokernel of DG module maps"
    "Homology of DG modules and DG module maps"
    "Pruning DG modules, submodules, quotients, and maps"
    "Well-definedness, acyclicity, and quasi-isomorphism"
    "Semifree resolutions of DG modules"
    "Computing module differentials and visualizing DG modules"
    "Building DG algebras from existing DG algebras"
    "Low-level differential computations and validity checks"
    "Accessors and cache management"
    "DG module primitives and chain-complex export"
///

doc ///
  Key
    "Basic operations on DG Algebras"
  Headline
    Outlines some basic operations on DG Algebras
  Description
    Text
      There are several ways to define a DGAlgebra.  One can start by defining one 'from scratch'.  One does
      this by specifying the ring over which the DGAlgebra is defined and the degrees of the generators.  The
      name of the generators of the DGAlgebra by default is $T_i$, but one may change this by specifying the
      optional (string) argument 'Variable'.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
      A = freeDGAlgebra(R,{{1,1},{1,1},{1,1},{1,1}})
    Text
      The command freeDGAlgebra only defines the underlying algebra of A, and not the differential.  To set the differential of A,
      one uses the command setDiff.
    Example
      setDiff(A, gens R)
    Text
      Note that the above is the (graded) Koszul complex on a set of generators of R.  A much easier way to define this is to use the
      function koszulComplexDGA.
    Example
      B = koszulComplexDGA(R, Variable=>"S")
    Text
      One can compute the homology algebra of a DGAlgebra using the homology (or HH) command.
    Example
      HB = HH B
      describe HB
      degrees HB
    Text
      Note that since R is a complete intersection, its Koszul homology algebra is an exterior algebra, which is a
      free graded commutative algebra.  Note that the internal degree is preserved in the computation of the homology algebra
      of B.
    Text
      One can also adjoin variables to kill cycles in homology.  The command killCycles looks for the first positive degree
      nonzero homology (say i), and adjoins variables in homological degree i+1 that differentiate to a minimal generating set of this homology, so that the
      resulting DGAlgebra now only has homology in degree greater than i (note of course this could introduce new homology in higher degrees).
      The command adjoinVariables allows finer control over this procedure.  See @ TO adjoinVariables @ for an example.
    Example
      HB.cache.cycles
      C = adjoinVariables(B,{first HB.cache.cycles})
      homologyAlgebra(C,GenDegreeLimit=>4,RelDegreeLimit=>4)
      C = killCycles(B)
      homologyAlgebra(C,GenDegreeLimit=>4,RelDegreeLimit=>4)
    Text
      Again, note that since R is a complete intersection, once we adjoin the variables in homological degree two to kill the cycles in degree one,
      we obtain a minimal DG Algebra resolution of the residue field of R.  Also, note that since C has generators in even degree, one must specify the
      optional arguments GenDegreeLimit and RelDegreeLimit to specify the max degree of the computation.  To do this, one uses the homologyAlgebra command
      rather than the HH command.
    Text
      This computation could have also been done with the command acyclicClosure.  The command acyclicClosure performs the command killCycles sequentially to ensure that the
      result has homology in higher and higher degrees, thereby computing (part of) a minimal DG Algebra resolution of the residue field.  acyclicClosure has an optional
      argument EndDegree that allows the user to specify the maximum homological degree with which to perform this adjunction of variables.  The default value of this is 3, since if there
      are any variables of degree 3 that need to be added, then each subsequent homological degree will require some variables to be adjoined (Halperin's rigidity theorem).
    Example
      D = acyclicClosure R
      R' = ZZ/101[x,y,z]/ideal{x^2,y^2,z^2,x*y*z}
      E = acyclicClosure(R',EndDegree=>5)
      tally degrees E.natural
    Text
      As you can see, since R' is not a complete intersection, the acyclic closure of E requires infinitely many variables; we display the degrees of the first 6 here.
      The tally that is displayed gives the deviations of the ring R.  One can compute the deviations directly from any minimal free resolution of the residue field
      of R', so that using the one provided by res coker vars R is faster.  To do this, use the command @ TO deviations @.
    Example
      deviations(R,DegreeLimit=>6)
      deviations(R',DegreeLimit=>6)
    Text
      As a brief warning, the command @ TO poincareN @ which is used in @ TO deviations @ uses the symbols S and T internally, and may cause problems referring to such rings at the Macaulay2 prompt.
///

doc ///
  Key
    "The Koszul complex as a DG Algebra"
  Headline
    an example
  Description
    Text
      The Koszul complex on a sequence of elements $f_1,\dots,f_r$ is a complex of R-modules whose underlying graded R-module
      is the exterior algebra on R^r generated in homological degree one.  This algebra structure also respects the boundary map
      of the complex in the sense that it satisfies the Liebniz rule.  That is, $d(ab) = d(a)b + (-1)^{deg a}ad(b)$.  When one
      speaks of 'the' Koszul complex of a ring, one means the Koszul complex on a minimal set of generators of the maximal ideal of R.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
      KR = koszulComplexDGA R
    Text
      One can specify the name of the variable to easily handle multiple Koszul complexes at once.
    Example
      S = ZZ/101[x,y,z]/ideal{x^3,y^3,z^3,x^2*y^2,y^2*z^2}
      KS = koszulComplexDGA(S,Variable=>"U")
    Text
      To obtain the chain complex associated to the Koszul complex, one may use toComplex.  One can also obtain this complex
      directly without using the DGAlgebras package by using the command @ TO koszul @.
    Example
      cxKR = toComplex KR
      prune HH cxKR
    Text
      Since the Koszul complex is a DG algebra, its homology is itself an algebra.  One can obtain this algebra using the command
      homology, homologyAlgebra, or HH (all commands work).  This algebra structure can detect whether or not the ring is a complete
      intersection or Gorenstein.
    Example
      HKR = HH KR
      ideal HKR
      R' = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3,a*c,a*d,b*c,b*d,a^2*b^2-c^2*d^2}
      HKR' = HH koszulComplexDGA R'
      numgens HKR'
      ann ideal gens HKR'
    Text
      Note that since the socle of HKR' is one dimensional, HKR' has Poincare duality, and hence R' is Gorenstein.
    Text
      One can also consider the Koszul complex of an ideal, or a sequence of elements.
    Example
      Q = ambient R
      I = ideal {a^3,b^3,c^3,d^3}
      KI = koszulComplexDGA I
      HKI = HH KI
      describe HKI
      use Q
      I' = I + ideal{a^2*b^2*c^2*d^2}
      KI' = koszulComplexDGA I'
      HKI' = HH KI'
      describe HKI'
      HKI'.cache.cycles
    Text
      Note that since I is a Q-regular sequence, the Koszul complex is acyclic, and that both homology algebras are algebras over the zeroth homology
      of the Koszul complex.
///

doc ///
  Key
    "Basic operations on DG Algebra Maps"
  Headline
    Outlines some basic operations on DGAlgebraMaps
  Description
    Text
      An algebra map between the underlying graded algebras that satisfies the Leibniz rule is a morphism of DG algebras.  Such objects
      are created using the DGAlgebraMap class.  As with DGAlgebras, one can define a DGAlgebraMap 'from scratch' using @ TO dgAlgebraMap @.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3+b^3+c^3,a*b*c}
      K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
      K2 = koszulComplexDGA(ideal {b,c},Variable=>"T")
      f = dgAlgebraMap(K2,K1,matrix{{0,T_(1,1),T_(1,2)}})
    Text
      Once we define the DGAlgebraMap, it is a good idea to check to see if it indeed satisfies the Leibniz rule.  This can be checked by using
      isWellDefined.
    Example
      isWellDefined f
    Text
      Oops!  Let's try that again.
    Example
      g = dgAlgebraMap(K1,K2,matrix{{Y_(1,2),Y_(1,3)}})
      isWellDefined g
    Text
      One can lift a ring homomorphism in degree zero to a map of DGAlgebras (up to a specified degree) using liftToDGMap.  This is helpful
      in some of the internal functions of the DGAlgebras package, such as computing the map induced on Tor algebras by a RingMap.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      S = R/ideal{a^2*b^2*c^2}
      f = map(S,R)
      A = acyclicClosure(R,EndDegree=>3)
      B = acyclicClosure(S,EndDegree=>3)
      phi = liftToDGMap(B,A,f)
    Text
      Once one has a DGAlgebraMap, one can also obtain the underlying map of complexes via toComplexMap.
    Example
      cmPhi = toComplexMap(phi,EndDegree=>3)
    Text
      There are also some auxiliary commands associated with DGAlgebraMaps
    Example
      source phi
      target phi
    Text
      One can also obtain the map on homology induced by a DGAlgebra map.
    Example
      HHg = HH g
      matrix HHg
///

doc ///
  Key
    "Basic operations on DG Module Maps"
  Headline
    Outlines some basic operations on DGModuleMaps
  Description
    Text
      A morphism of DG modules over a fixed DG algebra A is an A-linear map of underlying
      graded A.natural-modules that commutes with the differentials.  Such objects are
      created using the DGModuleMap class.  The workhorse constructor is @ TO dgModuleMap @,
      which accepts either a full Matrix encoding of the map or a list of image Vectors
      (one per natural generator of the source).
    Example
      R = QQ[x,y]/ideal(x^2,y^2)
      A = koszulComplexDGA R
      k = R^1 / ideal(x, y)
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 2)
      idM = identityDGModuleMap Mdg
      isWellDefined idM
    Text
      As with DGAlgebraMaps, once you have a candidate map it is a good idea to check
      @ TO isWellDefined @.  The check verifies (per natural generator of the source) both
      the hom-degree condition and the chain-map condition $d_N \circ f = f \circ d_M$.
    Example
      natGens = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
      xMap = dgModuleMap(Mdg, Mdg, apply(natGens, g -> x * g))
      isWellDefined xMap
    Text
      A DGModuleMap induces a chain map between the underlying complexes via
      @ TO toComplexMap @, and a module map on every homology degree via
      @ TO homology @.
    Example
      H0 = homology(xMap, 0)
      H0 == map(target H0, source H0, 0)
    Text
      Classical construction: given a hom-degree-0 seed (the images of the hom-deg-0
      generators of M in N.natural), @ TO liftToDGModuleMap @ inductively solves
      $d_N(x) = f(d_M(e))$ degree by degree, producing a full chain map M -> N.
      Requires N to be acyclic up to the requested EndDegree.
    Example
      f = liftToDGModuleMap(Mdg, Mdg, {(Mdg.natural)_0}, EndDegree => 2)
      isWellDefined f
      h0 = homology(f, 0)
      h0 == id_(source h0)
    Text
      Auxiliary operations: addition, subtraction, scalar multiplication by elements of
      A.ring, negation, and composition via $*$ are all supported, mirroring the
      matrix-level operations on f.natural.
    Example
      (2_R * idM).natural == 2 * idM.natural
      (idM * idM).natural == idM.natural
  SeeAlso
    DGModuleMap
    dgModuleMap
    identityDGModuleMap
    zeroDGModuleMap
    liftToDGModuleMap
    isWellDefined
    toComplexMap
    homology
///

doc ///
  Key
    DGModuleMap
  Headline
    The class of morphisms of DG modules over a common DG algebra
  Description
    Text
      A DGModuleMap encodes an A-linear chain map between two DGModules sharing the
      same underlying DG algebra A.  Create one via @ TO dgModuleMap @.  See
      @ TO "Basic operations on DG Module Maps" @ for a guided tour.
  SeeAlso
    DGModule
    dgModuleMap
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    dgModuleMap
    (dgModuleMap, DGModule, DGModule, Matrix)
    (dgModuleMap, DGModule, DGModule, List)
  Headline
    Construct a DGModuleMap from a matrix or from a list of image Vectors
  Usage
    f = dgModuleMap(N, M, img)
  Inputs
    N:DGModule
      The target DGModule.
    M:DGModule
      The source DGModule; must share the same DG algebra as N.
    img:
      Either a Matrix whose i-th column encodes the image of the i-th natural
      generator of M in N.natural, or a List of Vectors of the same length as
      M.Degrees (one Vector per source generator).
  Outputs
    f:DGModuleMap
  Description
    Text
      The map @ TT "f" @ satisfies the @ TT "A" @-linearity constraint by
      construction: the image of a general element @ TT "\\sum a_i e_i" @ is
      @ TT "\\sum a_i f(e_i)" @, where @ TT "f(e_i)" @ is the @ TT "i" @-th entry
      supplied.  Whether @ TT "f" @ is also a chain map (i.e.\ commutes with
      the differentials) can be verified with @ TO isWellDefined @.
    Text
      A particularly useful application is to build multiplication-by-a-ring-element
      chain maps on a minimal semifree resolution, whose induced action on
      homology recovers the action of the element on @ TT "Tor" @.  Over the
      complete intersection @ TT "R = k[x, y]/(x^2, y^2)" @, the variable
      @ TT "y" @ acts as zero on the residue field, and indeed the induced map
      on homology of the multiplication-by-@ TT "y" @ chain map is zero in
      every degree:
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 3)
      natGens = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
      fy = dgModuleMap(Mdg, Mdg, apply(natGens, g -> y * g))
      isWellDefined fy
      cmy = toComplexMap fy
      apply(0..3, n -> prune HH_n cmy)
    Text
      The same constructor also accepts a @ TO Matrix @ whose columns give
      the images of the natural generators.  For instance, supplying the
      identity matrix of @ TT "Mdg.natural" @ reproduces the identity chain
      map:
    Example
      idmat = id_(Mdg.natural)
      g = dgModuleMap(Mdg, Mdg, idmat)
      isWellDefined g
      g == identityDGModuleMap Mdg
  SeeAlso
    identityDGModuleMap
    zeroDGModuleMap
    liftToDGModuleMap
    (isWellDefined, DGModuleMap)
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    identityDGModuleMap
    (identityDGModuleMap, DGModule)
  Headline
    The identity DGModuleMap on a DG module
  Usage
    idM = identityDGModuleMap M
  Inputs
    M:DGModule
  Outputs
    idM:DGModuleMap
  Description
    Text
      Returns the identity endomorphism of M as a DGModuleMap.  Its underlying
      natural matrix is $\mathrm{id}_{M.natural}$ and its induced map on every
      homology degree is the identity.
    Text
      The shorthand @ TT "id_M" @ (mirroring the @ TO "Complexes::Complexes" @
      convention @ TT "id_C" @ for a Complex @ TT "C" @) is an alias, and both
      forms compare equal to the scalar @ TT "1" @:
    Example
      R = ZZ/101[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      id_KM == identityDGModuleMap KM
      id_KM == 1
    Text
      The identity is the neutral element for composition of DG module maps.
      On a semifree resolution of the residue field, the mult-by-y chain map
      is idempotent under pre- and post-composition with the identity:
    Example
      Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 2)
      natGens = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
      fy = dgModuleMap(Mdg, Mdg, apply(natGens, v -> y * v))
      idMdg = identityDGModuleMap Mdg
      idMdg * fy == fy
      fy * idMdg == fy
      idMdg * idMdg == idMdg
  SeeAlso
    dgModuleMap
    zeroDGModuleMap
///

doc ///
  Key
    zeroDGModuleMap
    (zeroDGModuleMap, DGModule, DGModule)
  Headline
    The zero DGModuleMap between two DG modules over a common algebra
  Usage
    zM = zeroDGModuleMap(N, M)
  Inputs
    N:DGModule
    M:DGModule
  Outputs
    zM:DGModuleMap
      The zero map M -> N.  Its natural matrix is zero, and its induced map on
      every homology degree is zero.
  Description
    Text
      The zero endomorphism is the neutral element for addition and the
      absorbing element for composition of DG module maps:
    Example
      R = ZZ/101[x] / ideal(x^2)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, R^1 / ideal(x), EndDegree => 2)
      idM = identityDGModuleMap Mdg
      zM  = zeroDGModuleMap(Mdg, Mdg)
      idM + zM == idM
      zM * idM == zM
      idM * zM == zM
    Text
      Taking the cokernel of the zero endomorphism recovers the original DG
      module, while taking its kernel yields the full module as a DG
      submodule:
    Example
      Q = cokernel zM
      rank Q.natural == rank Mdg.natural
      K = kernel zM
      instance(K, DGSubmodule)
  SeeAlso
    dgModuleMap
    identityDGModuleMap
///

doc ///
  Key
    liftToDGModuleMap
    (liftToDGModuleMap, DGModule, DGModule, Matrix)
    (liftToDGModuleMap, DGModule, DGModule, List)
    (liftToDGModuleMap, DGModule, DGModule, Vector)
    [liftToDGModuleMap, EndDegree]
  Headline
    Lift an image of hom-degree-0 generators to a full DGModuleMap
  Usage
    f = liftToDGModuleMap(N, M, h0)
  Inputs
    N:DGModule
      The target DGModule.  Must be acyclic in positive hom-degrees up to
      EndDegree so that lifts exist.
    M:DGModule
      The source DGModule.  Must be semifree (M.natural a free module).
    h0:
      A Matrix with one column per hom-degree-0 generator of M and
      (rank N.natural)-many rows whose entries lie in A.natural, or a List of
      Vectors (in N.natural), or a single Vector when M has exactly one
      hom-degree-0 generator.
  Outputs
    f:DGModuleMap
  Description
    Text
      Starting from the supplied hom-degree-0 assignment, the routine inductively
      constructs images for each M-generator of hom-degree $d \geq 1$ by solving
      $d_N(x) = f(d_M(e))$ in N.natural at hom-degree $d$.  Existence of a lift
      at every step is what requires N to be acyclic.  The lift is not unique in
      general: different choices of preimage at each step produce chain-homotopic
      lifts.
    Example
      R = QQ[x,y]/ideal(x^2,y^2)
      A = koszulComplexDGA R
      Mmin = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 2)
      Mnon = semifreeResolution(A, R^1 / ideal(x, y), EndDegree => 2)
      quism = liftToDGModuleMap(Mnon, Mmin, {(Mnon.natural)_0}, EndDegree => 2)
      isWellDefined quism
  SeeAlso
    dgModuleMap
    identityDGModuleMap
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    (homology, DGModuleMap)
  Headline
    The induced map on graded homology modules over HH(A)
  Usage
    h = homology f
  Inputs
    f:DGModuleMap
      A morphism M -> N of DG modules over a common DG algebra A.  Both
      M.natural and N.natural must be free (the free / semifree case).
  Outputs
    h:
      The induced A.homology-module map H_*(M) -> H_*(N), where H_*(M) and
      H_*(N) carry their natural HH(A)-module structures via the action of
      cycles of A on M and N.  The result lives over the ring HH(A).
  Description
    Text
      The per-degree restrictions of h are given by @ TO (homology, DGModuleMap, ZZ) @.
      The present method assembles them into a single map of HH(A)-modules by
      forming the direct sum of the per-degree pieces and imposing the
      cycle-action relations on both sides.  Functorial in f.
    Example
      R = QQ[x,y]/ideal(x^2, y^2)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, R^1 / ideal(x,y), EndDegree => 2)
      idM = identityDGModuleMap Mdg
      h = homology idM
      ring source h === HH A
  SeeAlso
    (homology, DGModuleMap, ZZ)
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    (symbol ==, DGModuleMap, DGModuleMap)
    (isHomogeneous, DGModuleMap)
    (map, DGModule, DGModule, ZZ)
    (isQuasiIsomorphism, DGModuleMap)
  Headline
    Elementary predicates and constructors for DGModuleMaps
  Description
    Text
      Auxiliary operations mirroring those on ComplexMap.
      For maps f, g between DG modules over a common DG algebra:
    Text
      @ TT "f == g" @ -- equality of source, target, and underlying natural matrix.
    Text
      @ TT "isHomogeneous f" @ -- agrees with @ TT "isHomogeneous f.natural" @.
    Text
      @ TT "map(N, M, 0)" @ -- returns the zero map M -> N.
      @ TT "map(M, M, 1)" @ -- returns the identity on M.
    Text
      @ TT "isQuasiIsomorphism f" @ -- returns true iff the induced chain map on
      @ TO toComplexMap @ is a quasi-isomorphism.
    Example
      R = QQ[x,y]/ideal(x^2, y^2)
      Mdg = minimalSemifreeResolution(koszulComplexDGA R, R^1 / ideal(x,y), EndDegree => 2)
      idM = identityDGModuleMap Mdg
      zM = zeroDGModuleMap(Mdg, Mdg)
      idM == idM
      map(Mdg, Mdg, 0) == zM
      map(Mdg, Mdg, 1) == idM
      isQuasiIsomorphism idM
  SeeAlso
    DGModuleMap
    identityDGModuleMap
    zeroDGModuleMap
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    identityDGAlgebraMap
    (identityDGAlgebraMap, DGAlgebra)
  Headline
    The identity DGAlgebraMap on a DG algebra
  Usage
    idA = identityDGAlgebraMap A
  Inputs
    A:DGAlgebra
  Outputs
    idA:DGAlgebraMap
  Description
    Text
      Returns the identity self-map of A as a DGAlgebraMap.  Under the hood the
      underlying RingMap A.natural -> A.natural is the identity map on every
      generator (including the base-ring generators).
    Example
      R = QQ[x,y]/ideal(x^2,y^2)
      A = koszulComplexDGA R
      idA = identityDGAlgebraMap A
      (idA * idA) == idA
  SeeAlso
    dgAlgebraMap
    (symbol *, DGAlgebraMap, DGAlgebraMap)
///

doc ///
  Key
    (symbol *, DGAlgebraMap, DGAlgebraMap)
  Headline
    Composition of DG algebra maps
  Usage
    h = g * f
  Inputs
    g:DGAlgebraMap
      A map with source = target of f.
    f:DGAlgebraMap
  Outputs
    h:DGAlgebraMap
      The composite DGAlgebraMap whose underlying RingMap is g.natural * f.natural.
  SeeAlso
    identityDGAlgebraMap
    dgAlgebraMap
///

doc ///
  Key
    (symbol **, DGAlgebra, DGAlgebra)
    tensorFactors
    (tensorFactors, DGAlgebra)
    tensorInclusions
    (tensorInclusions, DGAlgebra)
  Headline
    Exterior tensor product of DG algebras over a common ground ring
  Usage
    C = A ** B
    (A1, A2) = tensorFactors C
    (iotaA, iotaB) = tensorInclusions C
  Inputs
    A:DGAlgebra
    B:DGAlgebra
      Must satisfy A.ring === B.ring.
  Outputs
    C:DGAlgebra
      A DGAlgebra over A.ring whose underlying algebra has #A.Degrees + #B.Degrees
      generators, with the obvious multi-degrees, and whose differential is
      d_A on the A-generators and d_B on the B-generators (transported via the
      canonical inclusions).
  Description
    Text
      This is the exterior tensor product of A and B as DGAs: both are viewed
      as augmented DG algebras over their common ground ring, and the tensor
      is taken componentwise in multi-degrees.  Internally the result is cached
      on A.cache, so C1 = A ** B and C2 = A ** B return the SAME DGAlgebra
      object, which is essential for composition identities to hold downstream.
    Text
      The helpers @ TO tensorFactors @ and @ TO tensorInclusions @ recover the
      pair (A, B) and the canonical inclusions A -> C and B -> C (as DGAlgebraMaps).
    Example
      R = QQ[x,y]
      A = koszulComplexDGA R
      C = A ** A
      numgens C.natural
      (iotaA, iotaB) = tensorInclusions C
  SeeAlso
    (symbol **, DGAlgebraMap, DGAlgebraMap)
    (symbol **, DGModule, DGModule)
///

doc ///
  Key
    (symbol **, DGAlgebraMap, DGAlgebraMap)
  Headline
    Tensor product of DG algebra maps
  Usage
    h = f ** g
  Inputs
    f:DGAlgebraMap
      A:A -> A'
    g:DGAlgebraMap
      B:B -> B'
  Outputs
    h:DGAlgebraMap
      The map A ** B -> A' ** B' that restricts to f on the A-generators and
      g on the B-generators, transported via the canonical inclusions.
  Description
    Text
      Functorial with the DGA tensor product: identityDGAlgebraMap(A) ** identityDGAlgebraMap(B)
      equals identityDGAlgebraMap(A ** B).
  SeeAlso
    (symbol **, DGAlgebra, DGAlgebra)
    identityDGAlgebraMap
///

doc ///
  Key
    (symbol **, DGModule, DGModule)
  Headline
    Exterior tensor product of DG modules over DG algebras sharing a ground ring
  Usage
    P = M ** N
  Inputs
    M:DGModule
      A (free) DG module over some DG algebra A.
    N:DGModule
      A (free) DG module over some DG algebra B, with A.ring === B.ring.
  Outputs
    P:DGModule
      A (free) DG module over A ** B whose natural generators are indexed
      lexicographically by (i, j) with i ranging over M-generators and j over
      N-generators.  The multi-degree of (i, j) is M.Degrees#i + N.Degrees#j
      coordinate-wise, and the differential is
      $d(e_i \otimes f_j) = d_M(e_i) \otimes f_j + (-1)^{|e_i|}\, e_i \otimes d_N(f_j)$.
  Description
    Text
      This is the EXTERIOR tensor product: we do not quotient by an A- or B-action.
      The resulting DGModule lives over A ** B via the canonical inclusions.
      The result is cached on M.cache, so M ** N returns the same object on repeat.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      ng = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * ng#0})
      P = M ** M
      P.dgAlgebra === (A ** A)
  SeeAlso
    (symbol **, DGAlgebra, DGAlgebra)
    (symbol **, DGModuleMap, DGModuleMap)
    freeDGModule
///

doc ///
  Key
    (symbol **, DGModuleMap, DGModuleMap)
  Headline
    Tensor product of DG module maps
  Usage
    h = F ** G
  Inputs
    F:DGModuleMap
      M -> M'
    G:DGModuleMap
      N -> N'
  Outputs
    h:DGModuleMap
      The map M ** N -> M' ** N' acting by F on the first factor and G on the second factor.
  Description
    Text
      Functorial: idM ** idN equals identityDGModuleMap(M ** N), and tensor with
      a zero map is a zero map.
  SeeAlso
    (symbol **, DGModule, DGModule)
    identityDGModuleMap
    zeroDGModuleMap
///

doc ///
  Key
    DGAlgebra
  Headline
    The class of all DGAlgebras
  Description
    Text
      Some common ways to create DGAlgebras include @ TO koszulComplexDGA @, @ TO freeDGAlgebra @, @ TO setDiff @, and @ TO acyclicClosure @.
  SeeAlso
    "Basic operations on DG Algebras"
///

doc ///
  Key
    freeDGAlgebra
    (freeDGAlgebra,Ring,List)
    [freeDGAlgebra,Variable]
  Headline
    Construct a free DG algebra with given generator degrees
  Usage
    A = freeDGAlgebra(R, degreeList)
    A = freeDGAlgebra(R, degreeList, Variable => "U")
  Inputs
    R:Ring
      The base ring over which the DG algebra is defined.
    degreeList:List
      One degree per algebra generator.  Each degree is itself a
      @ TO List @ of integers; the @ EM "first" @ entry is interpreted
      as the homological degree, and any remaining entries are internal
      gradings carried through to the underlying algebra.
    Variable => String
      Either a base name (@ TO String @ or @ TO Symbol @) or an explicit
      list of symbols.  Defaults to @ TT "\"T\"" @.  See below.
  Outputs
    A:DGAlgebra
      A DG algebra over @ TT "R" @ whose @ TT "A.natural" @ is a
      graded-commutative polynomial ring on @ TT "#degreeList" @
      generators.  The differential is not set by this constructor; call
      @ TO setDiff @ on the result to install it.
  Description
    Text
      The output is a @ TO DGAlgebra @ whose underlying ring is a
      polynomial ring in graded-commutative generators: generators of
      @ EM "odd" @ homological degree are skew-commutative and square
      to zero (i.e.\ exterior), while generators of @ EM "even" @
      homological degree are fully commutative.  The resulting algebra
      is always free as a graded @ TT "R" @-algebra; the current version
      of the package does @ EM "not" @ handle quotient DG algebras whose
      underlying ring is a non-polynomial quotient.
    Text
      @ BOLD "Variable-naming convention." @  Generators are named
      @ TT "base_(i, j)" @, where @ TT "i" @ is the homological degree
      (the first entry of the degree vector) and @ TT "j" @ is a
      @ EM "1-indexed" @ counter among generators at that hom-degree.
      So @ TT "freeDGAlgebra(R, {{1}, {1}, {1}, {3}})" @ produces four
      generators @ TT "T_(1,1), T_(1,2), T_(1,3), T_(3,1)" @.  The
      @ TT "Variable" @ option changes the base name:
    Example
      R = ZZ/101[x, y, z]
      A = freeDGAlgebra(R, {{1}, {1}, {1}, {3}}, Variable => "U")
      gens A.natural
    Text
      Passing a @ TO List @ of symbols for @ TT "Variable" @ overrides
      the doubly-indexed naming entirely and uses the given names
      verbatim.  The list length must match @ TT "#degreeList" @:
    Example
      B = freeDGAlgebra(R, {{1}, {1}, {1}}, Variable => {getSymbol "P", getSymbol "Q", getSymbol "RR"})
      gens B.natural
    Text
      This is the mode used internally by @ TO adjoinVariables @ and
      @ TO acyclicClosure @ to preserve the identities of existing
      generators when extending a DG algebra.
    Text
      @ BOLD "Multi-grading." @  Any degree entries beyond the first are
      carried through as additional gradings of the underlying ring
      @ TT "A.natural" @.  To make the differential homogeneous with
      respect to both the homological and internal gradings, pair each
      hom-degree with the matching internal-degree of its image under
      @ TO setDiff @.  Here, @ TT "T_(1,i)" @ has degree @ TT "(1,1)" @
      (hom-degree 1, internal-degree 1) matching @ TT "x, y, z" @, and
      @ TT "T_(3,1)" @ has degree @ TT "(3,3)" @ matching its
      cubic-monomial differential:
    Example
      Bmulti = freeDGAlgebra(R, {{1, 1}, {1, 1}, {1, 1}, {3, 3}})
      degrees Bmulti.natural
      setDiff(Bmulti, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
      isHomogeneous Bmulti
    Text
      Dropping the internal grading (hom-degree only) breaks homogeneity
      because the differential mixes @ EM "different" @ internal
      polynomial degrees of @ TT "R" @:
    Example
      Bhomo = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
      setDiff(Bhomo, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
      isHomogeneous Bhomo
    Text
      The differential is set after construction (via @ TO setDiff @)
      rather than passed to @ TT "freeDGAlgebra" @ directly: the
      differential's ring is @ TT "A.natural" @, which does not exist
      until after the constructor runs.  For common DG algebras arising
      in commutative algebra (Koszul complex, Tate resolution,
      acyclic closure, etc.) dedicated constructors are available; see
      @ TO koszulComplexDGA @, @ TO acyclicClosure @, @ TO minimalModel @.
  Caveat
    There is currently a bug handling DG algebras that have no
    generators in some homological degree but some in a later
    homological degree; for example @ TT "freeDGAlgebra(R, {{1}, {5}})" @
    may produce incorrect behavior if no hom-degree 3 generators are
    adjoined first.  The safer workflow is @ TT "freeDGAlgebra" @ with
    hom-degrees only up to what is needed, followed by
    @ TO adjoinVariables @ to extend incrementally.
  SeeAlso
    koszulComplexDGA
    acyclicClosure
    minimalModel
    adjoinVariables
    setDiff
    DGAlgebra
///

doc ///
  Key
    koszulComplexDGA
    (koszulComplexDGA,Ring)
    [koszulComplexDGA,Variable]
  Headline
    The Koszul complex on the variables of a ring, as a DG algebra
  Usage
    A = koszulComplexDGA R
    A = koszulComplexDGA(R, Variable => "S")
  Inputs
    R:Ring
      A polynomial ring or quotient of one; the Koszul complex is built on
      @ TT "gens R" @.
    Variable => String
      Base name for the Koszul generators.  Defaults to @ TT "\"T\"" @.
      Both @ TT "String" @ and @ TT "Symbol" @ values are accepted.
  Outputs
    A:DGAlgebra
      The Koszul complex on @ TT "gens R" @ equipped with the structure of
      a DG algebra over @ TT "R" @.
  Description
    Text
      Given a ring @ TT "R" @ with @ TT "n = numgens R" @ generators
      @ TT "x_1, ..., x_n" @, the returned DG algebra has underlying
      graded-commutative polynomial ring
      @ TT "R[T_(1,1), ..., T_(1,n)]" @ on @ TT "n" @ exterior (odd,
      square-zero) generators in hom-degree 1, with differential
      @ TT "d(T_(1,i)) = x_i" @ extended by the Leibniz rule.  In the
      @ TT "R" @-module direction, the complex is identical to the
      classical Koszul complex @ TT "K_\\bullet(x_1, ..., x_n; R)" @.
    Text
      @ BOLD "Variable naming convention." @  Generators are named
      @ TT "T_(i, j)" @, where @ TT "i" @ is the homological degree (here
      always @ TT "1" @) and @ TT "j" @ is a @ EM "1-indexed" @ counter
      among generators at that hom-degree.  Pass @ TT "Variable => \"S\"" @
      to use base name @ TT "S" @ instead.
    Example
      R = ZZ/101[a, b, c] / ideal(a^3, b^3, c^3)
      A = koszulComplexDGA R
      gens A.natural
      flatten entries matrix A.diff
    Text
      Converting to a @ TO Complex @ gives the ordinary Koszul complex on
      the variables (up to a choice of monomial order on the exterior
      product); taking homology recovers the classical Koszul homology:
    Example
      complexA = toComplex A
      complexA.dd_1
      ranks = apply(4, i -> numgens prune HH_i complexA)
      ranks == apply(4, i -> numgens prune HH_i koszul vars R)
    Text
      The homology can be computed directly from @ TT "A" @ via
      @ TO (homology,ZZ,DGAlgebra) @, or as an algebra via
      @ TO homologyAlgebra @.
    Text
      To rename the Koszul generators, pass the @ TT "Variable" @ option:
    Example
      S = ZZ/101[a, b]
      AS = koszulComplexDGA(S, Variable => "U")
      gens AS.natural
  Caveat
    The Koszul complex is built on @ EM "all" @ elements of @ TT "gens R" @,
    not on a minimal generating set of a possibly non-irrelevant ideal.
    To build a Koszul complex on a specific sequence of ring elements, use
    @ TO (koszulComplexDGA, List) @; for a full generating set of an ideal,
    use @ TO (koszulComplexDGA, Ideal) @.  Also, @ TO toComplex @ uses a
    different monomial order than the @ TO "Complexes::koszul" @ command,
    so individual differentials may differ by a basis permutation even
    though the complexes are isomorphic.
  SeeAlso
    (koszulComplexDGA, Ideal)
    (koszulComplexDGA, List)
    freeDGAlgebra
    acyclicClosure
    koszulComplexDGM
    (homology, ZZ, DGAlgebra)
    homologyAlgebra
///

doc ///
  Key
    (koszulComplexDGA,Ideal)
  Headline
    The Koszul complex on the generators of an ideal, as a DG algebra
  Usage
    A = koszulComplexDGA I
  Inputs
    I:Ideal
      An ideal of a ring @ TT "R" @.
  Outputs
    A:DGAlgebra
      The Koszul complex over @ TT "R" @ on the chosen generators of
      @ TT "I" @.
  Description
    Text
      Given generators @ TT "f_1, ..., f_r" @ of @ TT "I" @, the output is
      a DG algebra over @ TT "R" @ whose underlying graded algebra is
      @ TT "R[T_(1,1), ..., T_(1,r)]" @ on @ TT "r" @ exterior generators
      in hom-degree 1, with differential @ TT "d(T_(1,i)) = f_i" @
      extended by the Leibniz rule.  The variable naming convention and
      @ TT "Variable" @ option mirror @ TO (koszulComplexDGA, Ring) @.
    Example
      R = ZZ/101[a, b, c]
      I = ideal(a^3, b^3, c^3, a^2 * b^2 * c^2)
      A = koszulComplexDGA I
      gens A.natural
      flatten entries matrix A.diff
    Text
      The resulting complex coincides with @ TO "Complexes::koszul" @
      applied to @ TT "gens I" @, up to monomial order:
    Example
      complexA = toComplex A
      ranks = apply(5, i -> numgens prune HH_i complexA)
      ranks == apply(5, i -> numgens prune HH_i koszul gens I)
    Text
      In particular, @ TT "I" @ contains the redundant generator
      @ TT "a^2 b^2 c^2" @ (it is in @ TT "(a^3, b^3, c^3)" @), so the
      Koszul complex is not exact at @ TT "H_1" @; nonzero higher
      Koszul homology encodes the relations and syzygies between the
      chosen generators.
  SeeAlso
    (koszulComplexDGA, Ring)
    (koszulComplexDGA, List)
    (homology, ZZ, DGAlgebra)
///

doc ///
  Key
    (koszulComplexDGA,List)
  Headline
    The Koszul complex on a list of ring elements, as a DG algebra
  Usage
    A = koszulComplexDGA diffList
  Inputs
    diffList:List
      A nonempty list of @ TO RingElement @ s, all lying in a common ring
      @ TT "R" @.  These become the images of the Koszul generators under
      the differential.
  Outputs
    A:DGAlgebra
      The Koszul complex over @ TT "R" @ on the sequence @ TT "diffList" @.
  Description
    Text
      This is the lowest-level form: the generators of @ TT "I" @ or
      @ TT "R" @ are replaced by an arbitrary user-supplied list of ring
      elements.  Useful for building a Koszul complex on a partial
      regular sequence or on arbitrary elements that may not generate an
      ideal of interest.  The variable naming convention follows
      @ TO (koszulComplexDGA, Ring) @: generators are named
      @ TT "T_(1, j)" @ with @ TT "j = 1, ..., #diffList" @.
    Example
      R = ZZ/101[x, y, z]
      A = koszulComplexDGA({x^2, y*z})
      gens A.natural
      flatten entries matrix A.diff
      apply(3, i -> numgens prune HH_i toComplex A)
    Text
      Here the sequence @ TT "(x^2, y z)" @ is regular in
      @ TT "ZZ/101[x, y, z]" @, so the Koszul complex is acyclic in
      positive homology and @ TT "H_0" @ is the quotient
      @ TT "R/(x^2, y z)" @ (of dimension 1 as an @ TT "R" @-module).
  SeeAlso
    (koszulComplexDGA, Ring)
    (koszulComplexDGA, Ideal)
///

doc ///
  Key
    (homology,ZZ,DGAlgebra)
  Headline
    Computes the homology of a DG algebra as a module
  Usage
    H = homology(n,A)
  Inputs
    n:ZZ
    A:DGAlgebra 
  Outputs
    H:Module
      The nth homology of A.
  Description
    Example
      R = ZZ/32003[x,y,z]
      A = koszulComplexDGA(R)
      apply(numgens R+1, i -> numgens prune homology(i,A))
///

doc ///
  Key
    setDiff
    (setDiff,DGAlgebra,List)
    InitializeComplex
    [setDiff,InitializeComplex]
    InitializeDegreeZeroHomology
    [setDiff,InitializeDegreeZeroHomology]
  Headline
    Install or replace the differential on a DG algebra
  Usage
    A = setDiff(A, diffList)
    A = setDiff(A, diffList, InitializeComplex => false)
  Inputs
    A:DGAlgebra
    diffList:List
      A list of elements of @ TT "A.natural" @, one per algebra generator,
      given in the @ EM "same order" @ as @ TT "gens A.natural" @ (which is
      the order of @ TT "A.Degrees" @).  The @ TT "i" @-th entry becomes
      @ TT "d((gens A.natural)_i)" @ and is extended to all of
      @ TT "A.natural" @ by @ TT "R" @-linearity and the Leibniz rule.
      Each entry must have homological degree one less than its generator.
    InitializeComplex => Boolean
      Whether to eagerly compute all differential matrices up to
      @ TT "maxDegree A" @.  Defaults to @ TT "true" @; set to
      @ TT "false" @ to skip the precomputation when only a low-degree
      fragment is needed.
    InitializeDegreeZeroHomology => Boolean
      Whether to compute the quotient ring @ TT "H_0(A)" @ (needed by
      @ TO homologyAlgebra @).  Defaults to @ TT "true" @; set to
      @ TT "false" @ to skip the Grobner-basis computation when not
      needed.
  Outputs
    A:DGAlgebra
      The same @ TT "A" @, now with its @ TT "diff" @ field populated;
      returned for convenience in a chained call.
  Description
    Text
      Because the ring @ TT "A.natural" @ does not exist until after
      @ TO freeDGAlgebra @ runs, the differential cannot be passed to
      the constructor and must be set afterwards with @ TT "setDiff" @.
      The list @ TT "diffList" @ supplies the image of each algebra
      generator; Leibniz and @ TT "R" @-linearity extend this to every
      element of @ TT "A.natural" @.
    Example
      R = ZZ/101[x, y, z]
      A = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
      gens A.natural
      setDiff(A, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
      flatten entries matrix A.diff
    Text
      Here @ TT "d(T_(1,i)) = x_i" @ (so the Koszul piece on
      @ TT "T_(1,*)" @ is installed) and @ TT "d(T_(3,1))" @ is the
      Koszul-relation 2-cycle @ TT "x T_(1,2) T_(1,3) - y T_(1,1) T_(1,3)
      + z T_(1,1) T_(1,2)" @, turning @ TT "A" @ into a Tate-resolution
      fragment for @ TT "R/(x, y, z)" @.  Converting to a @ TO Complex @
      shows the installed differentials:
    Example
      Acomplex = toComplex A
      Acomplex.dd_1
      Acomplex.dd_2
    Text
      @ BOLD "Validity." @  The package does @ EM "not" @ check that the
      installed differential squares to zero; a user-supplied
      @ TT "diffList" @ that violates @ TT "d^2 = 0" @ yields a malformed
      DG algebra with silently wrong downstream behavior.  The chain-map
      conditions can be checked after the fact via
      @ TO (isWellDefined, DGAlgebra) @.
    Text
      @ BOLD "Options." @  @ TT "InitializeComplex" @ controls whether
      @ TT "setDiff" @ eagerly builds all differential matrices up to
      @ TT "maxDegree A" @ (the sum of the hom-degrees of the
      odd-hom-degree generators).  Set it to @ TT "false" @ to defer this
      computation when only low-degree information is needed:
    Example
      Abig = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
      setDiff(Abig, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)}, InitializeComplex => false)
    Text
      @ TT "InitializeDegreeZeroHomology" @ controls whether the quotient
      ring @ TT "H_0(A) = A_0 / image(d_1)" @ is computed on the spot
      (a Grobner-basis computation, deferred to @ TO homologyAlgebra @
      if turned off).  Default: @ TT "true" @.  Turn it off if you have
      many hom-degree-1 generators and are not about to compute
      @ TT "HH A" @ as a DG algebra.
  Caveat
    The order of entries in @ TT "diffList" @ must match the order of
    generators in @ TT "gens A.natural" @ (equivalently, of
    @ TT "A.Degrees" @).  For constructors that adjoin generators
    incrementally (@ TO adjoinVariables @, @ TO acyclicClosure @,
    @ TO killCycles @), the new generators come after all existing
    ones in this ordering.
  SeeAlso
    freeDGAlgebra
    koszulComplexDGA
    adjoinVariables
    (isWellDefined, DGAlgebra)
    homologyAlgebra
///

doc ///
  Key
    (isHomogeneous, DGAlgebra)
  Headline
    Determine if the DGAlgebra respects the gradings of the ring it is defined over.
  Usage
    isHom = isHomogeneous(A)
  Inputs
    A:DGAlgebra
  Outputs
    isHom:Boolean
      Whether or not the DGA respects the grading
  Description
    Example
      R = ZZ/101[x,y,z]
      A = freeDGAlgebra(R,{{1},{1},{1},{3}})
      setDiff(A,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
      isHomogeneous A
      B = freeDGAlgebra(R,{{1,1},{1,1},{1,1},{3,3}})
      setDiff(B,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
      isHomogeneous B
///

doc ///
  Key
    natural
  Headline
    The underlying graded-commutative ring of a DGAlgebra or DGModule
  Usage
    Anat = A.natural
  Description
    Text
      For a @ TO DGAlgebra @ @ TT "A" @, the key @ TT "A.natural" @ holds
      the underlying graded-commutative polynomial ring of @ TT "A" @
      with the differential @ EM "forgotten" @.  This is the ambient ring
      in which algebra generators live, in which cycles and boundaries
      are expressed, and against which differentials are compared.  It is
      always a polynomial ring over @ TT "A.ring" @ (with the
      skew-commutative flag set on odd-hom-degree generators), never a
      quotient.
    Example
      R = ZZ/101[a, b, c, d]
      A = koszulComplexDGA R
      A.natural
      A.ring === R
      gens A.natural
    Text
      For a @ TO DGModule @ @ TT "M" @, the same key holds the underlying
      free @ TT "A.natural" @-module with the differential forgotten.  The
      differential is recorded separately in @ TT "M.diff" @ and the
      multi-degrees of the natural generators in @ TT "M.Degrees" @:
    Example
      B = freeDGModule(A, {0, 1, 2})
      B.natural
      rank B.natural
  SeeAlso
    DGAlgebra
    DGModule
    ring
///

doc ///
  Key
    cycles
  Headline
    Chosen cycle representatives for homology generators of a DG algebra
  Usage
    A.cycles
  Description
    Text
      When @ TO acyclicClosure @, @ TO killCycles @, or
      @ TO homologyAlgebra @ adjoin new generators to kill homology of a
      DG algebra @ TT "A" @, the chosen cycle representatives are cached
      on the output so the exact Tate construction can be inspected
      afterwards.  The key is @ TT "A.cycles" @ on a DG algebra, or
      @ TT "HA.cache.cycles" @ on the output of @ TO homologyAlgebra @.
    Example
      R = ZZ/101[a, b, c, d] / ideal(a^3, b^4, c^5, d^6)
      A = koszulComplexDGA R
      apply(maxDegree A + 1, i -> numgens prune homology(i, A))
      HA = homologyAlgebra A
      numgens HA
      HA.cache.cycles
    Text
      The entries of @ TT "HA.cache.cycles" @ are elements of
      @ TT "A.natural" @: cycles chosen to represent the generators of
      @ TT "HA" @ as an algebra.  These are the same representatives
      that would be used as the differentials of newly adjoined
      generators in @ TO acyclicClosure @.
  SeeAlso
    homologyAlgebra
    acyclicClosure
    killCycles
///

doc ///
  Key
    getBasis
    (getBasis,ZZ,DGAlgebra)
  Headline
    Get a basis for a particular homological degree of a DG algebra.
  Usage
    M = getBasis(n,A)
  Inputs
    n:ZZ
    A:DGAlgebra
  Outputs
    M:Matrix
      The basis of the desired homological degree of the DG Algebra.
  Description
    Text
      This function is to allow for the retrieval of a basis of a particular homological degree of a @ TO DGAlgebra @
      when the underlying algebra A.natural is multigraded.  In the code, the homological grading is always the first
      integer in the degree tuple, and so this function returns a matrix consisting of all monomials in homological
      degree n.  
    Example
      R = ZZ/101[a..d, Degrees=>{1,1,1,2}]
      A =  koszulComplexDGA(R)
      getBasis(3,A)
///

doc ///
  Key
    (getBasis,ZZ,Ring)
  Headline
    Get a basis for a degree of a ring.
  Usage
    M = getBasis(n,R)
  Inputs
    n:ZZ
    R:Ring
  Outputs
    M:Matrix
      The basis of the desired degree
  Description
    Text
      This function was not meant for general use, but it fixes the first degree in the degree tuple
      of the ring R, and finds a basis of that 'slice' of the ring.  It does this by using a cached
      version of the ring that forgets all other degrees.  A Ring object in Macaulay2 will not have this
      cached ring by default, but the rings used internally in the DGAlgebras package will.
///

doc ///
  Key
    toComplex
    (toComplex,DGAlgebra)
  Headline
    Converts a DGAlgebra to a Complex
  Usage
    C = toComplex A or C = toComplex(A,n)
  Inputs
    A:DGAlgebra
  Outputs
    C:Complex
      The DG algebra A as a Complex
  Description
    Example
      R = ZZ/101[x_1..x_10]
      A = koszulComplexDGA(R)
      C = toComplex A
    Text
      Warning:  The term order that the internal command koszul uses to order the monomials is not GRevLex, and so the differentials
      used in koszul and koszulComplexDGA will not match up exactly.  Also, this command will only execute if all of the variables
      of the @ TO DGAlgebra @ A are of odd homological degree.  Otherwise, you need to use the function @ TO (toComplex, DGAlgebra, ZZ) @.
///

doc ///
  Key
    (toComplex,DGAlgebra,ZZ)
  Headline
    Converts a DGAlgebra to a Complex
  Usage
    C = toComplex A or C = toComplex(A,n)
  Inputs
    A:DGAlgebra
    n:ZZ
  Outputs
    C:Complex
      The DG algebra A as a Complex
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
      A = acyclicClosure(R,EndDegree=>3)
    Text
      The above will be a resolution of the residue field over R, since R is a complete intersection.
    Example
      C = toComplex(A, 10)
      apply(10, i -> prune HH_i(C))
///

doc ///
  Key
    acyclicClosure
    (acyclicClosure,DGAlgebra)
  Headline
    Tate's construction of a semifree acyclic DG algebra extension
  Usage
    B = acyclicClosure A
    B = acyclicClosure(A, EndDegree => n)
  Inputs
    A:DGAlgebra
      The starting DG algebra.
    StartDegree => ZZ
      First hom-degree at which to begin killing homology.  Defaults to
      @ TT "1" @.
    EndDegree => ZZ
      Last hom-degree at which homology is killed.  Defaults to
      @ TT "3" @.
    Variable => String
      Base name used for any new adjoined generators (Strings and Symbols
      are both accepted).  Defaults to reusing the base symbol already in
      use by @ TT "A" @, or @ TT "\"T\"" @ if A has no generators.
  Outputs
    B:DGAlgebra
      A semifree extension of @ TT "A" @ with the same ring and the same
      degree-zero homology, whose positive-degree homology has been killed
      in every hom-degree from @ TT "StartDegree" @ through @ TT "EndDegree" @.
  Description
    Text
      @ BOLD "Tate's construction." @  Starting from @ TT "A" @,
      @ TT "acyclicClosure" @ iterates @ TO killCycles @ from
      @ TT "StartDegree" @ up through @ TT "EndDegree" @.  At each step
      @ TT "n" @, a minimal set of cycle representatives of @ TT "H_n(A)" @
      is computed and a new DG algebra generator is adjoined for each,
      placed in hom-degree @ TT "n + 1" @ and whose differential is the
      chosen cycle.  Adjoining follows the graded-commutative convention:
      generators of odd hom-degree are exterior (skew-commutative,
      square-zero) while generators of even hom-degree are polynomial, so
      the result is a free DG algebra in the sense of Tate/Gulliksen.  If
      @ TT "A" @ is the Koszul complex on a regular sequence, no
      homology exists to kill and @ TT "acyclicClosure" @ returns @ TT "A" @
      unchanged; otherwise new generators appear, recording the deviations
      of @ TT "A" @.
    Text
      @ BOLD "Variable-naming convention." @  Both the existing generators of
      @ TT "A" @ and the newly adjoined generators use a doubly-indexed
      naming scheme @ TT "base_(i, j)" @, where @ TT "i" @ is the
      homological degree (the first entry of the multi-degree) and
      @ TT "j" @ is a @ EM "1-indexed" @ counter among generators at that
      hom-degree.  Counters continue past those already present in
      @ TT "A" @.  For example, if @ TT "A" @ already has three generators
      @ TT "T_(1,1), T_(1,2), T_(1,3)" @ in hom-degree 1 and two new
      hom-degree-2 cycles need killing, the output has new generators
      @ TT "T_(2,1)" @ and @ TT "T_(2,2)" @.  The @ TT "Variable" @ option
      only affects the base symbol for @ EM "new" @ generators; existing
      names are preserved.
    Text
      Over a complete intersection @ TT "R = k[a, b, c]/(a^3, b^3, c^3)" @,
      the Koszul complex has three nonzero hom-degree-2 cycles (one per
      relation) which must be killed, introducing three new polynomial
      generators @ TT "T_(2,1), T_(2,2), T_(2,3)" @:
    Example
      R = ZZ/101[a, b, c] / ideal(a^3, b^3, c^3)
      A = koszulComplexDGA R
      gens A.natural
      B = acyclicClosure(A, EndDegree => 2)
      gens B.natural
      take(flatten entries matrix B.diff, 6)
    Text
      Here the new generators satisfy
      @ TT "d(T_(2,i)) = a_i^2 T_(1,i)" @, the cycle representatives of
      @ TT "H_2" @ for each element of the regular sequence.  The result
      is quasi-isomorphic to @ TT "R/(a,b,c)" @ in hom-degrees
      @ TT "<= 2" @.
    Text
      For larger @ TT "EndDegree" @, the construction keeps going; over a
      c.i. no further generators appear because the Koszul-plus-polynomial
      DG algebra is already a resolution of the residue field (this is
      Tate's theorem):
    Example
      B2 = acyclicClosure(A, EndDegree => 4)
      numgens B2.natural == numgens B.natural
    Text
      Over a non-c.i., higher deviations are nontrivial and
      @ TT "acyclicClosure" @ adjoins further generators at each stage.
      Tallying by hom-degree gives the deviation sequence of the ring
      (excluding hom-degree 0, where the ring itself sits):
    Example
      S = QQ[x, y] / ideal(x^2, x*y, y^2)
      C = acyclicClosure(koszulComplexDGA S, EndDegree => 3)
      tally apply(C.Degrees, d -> first d)
    Text
      The hom-degree-1 count of @ TT "2" @ comes from the two Koszul
      exterior generators (one per variable of @ TT "S" @); hom-degree
      @ TT "2" @ has three polynomial generators killing the three
      relations; and hom-degrees @ TT "3" @ and beyond have nontrivial
      deviations, signaling that @ TT "S" @ is @ EM "not" @ a complete
      intersection.  Over a complete intersection, all hom-degrees
      @ TT ">= 3" @ would be empty.
    Text
      To choose a different base symbol for the newly adjoined generators,
      use the @ TT "Variable" @ option.  Existing generators of @ TT "A" @
      keep their names:
    Example
      D = acyclicClosure(A, EndDegree => 2, Variable => "U")
      gens D.natural
  Caveat
    For a DG algebra @ TT "A" @ whose degree-zero part already has
    inhomogeneous structure (e.g. one built via @ TO freeDGAlgebra @
    with a hand-written differential that is not square-zero), the
    package assumes d^2 = 0 without checking; the resulting
    @ TT "acyclicClosure" @ is only meaningful when this holds.
  SeeAlso
    (acyclicClosure,Ring)
    killCycles
    adjoinVariables
    minimalModel
    koszulComplexDGA
    StartDegree
    EndDegree
///

doc ///
  Key
    (acyclicClosure,Ring)
  Headline
    Tate's acyclic closure of the residue field, starting from the Koszul complex
  Usage
    A = acyclicClosure R
    A = acyclicClosure(R, EndDegree => n)
  Inputs
    R:Ring
      A local or graded ring (in practice, a quotient of a polynomial ring
      by a homogeneous ideal).
  Outputs
    A:DGAlgebra
      A semifree DG algebra over @ TT "R" @ whose degree-zero homology is
      @ TT "R/m = R_0" @ (the residue field, or more generally
      @ TT "R/" @ @ TO (ideal, Ring) @@ TT " R" @) and whose positive
      homology is killed through hom-degree @ TT "EndDegree" @.
  Description
    Text
      This form is shorthand for
      @ TT "acyclicClosure(koszulComplexDGA R, ...)" @: the Koszul complex
      on a chosen generating set of the maximal/irrelevant ideal is used
      as the starting DG algebra, and @ TO (acyclicClosure, DGAlgebra) @
      iteratively adjoins new generators to kill homology up to
      @ TT "EndDegree" @.  The output is a truncation of Avramov's
      acyclic closure of @ TT "R" @ over itself -- equivalently, a free
      DG algebra resolution of the residue field in hom-degrees
      @ TT "0" @ through @ TT "EndDegree" @.
    Text
      @ BOLD "Variable convention." @  The hom-degree-1 exterior generators
      come from @ TO koszulComplexDGA @ and are named @ TT "T_(1,j)" @,
      one for each generator of the maximal/irrelevant ideal.  Higher
      generators (adjoined by @ TO killCycles @ at each stage) are named
      @ TT "T_(n,j)" @, with @ TT "n" @ the homological degree and
      @ TT "j" @ a 1-indexed counter.  Generators of odd hom-degree are
      exterior; generators of even hom-degree are polynomial.  Pass
      @ TT "Variable => \"S\"" @ (or any string/symbol) to rename the base.
    Example
      R = ZZ/101[a, b, c, d] / ideal(a^3, b^3, c^4 - d^3)
      A = acyclicClosure(R, EndDegree => 3)
      tally apply(A.Degrees, d -> first d)
      take(flatten entries matrix A.diff, numgens A.natural)
    Text
      Four hom-degree-1 exterior generators @ TT "T_(1,1), ..., T_(1,4)" @
      (one per variable of @ TT "R" @) and three hom-degree-2 polynomial
      generators (one for each defining relation, since @ TT "R" @ is a
      complete intersection of codimension 3).  In the resulting DG
      algebra the residue field has been resolved through hom-degree 3.
    Text
      For a complete intersection, the Koszul complex already has zero
      higher homology beyond what the Tate polynomial closure kills at
      hom-degree 2, so the number of generators stabilizes quickly:
    Example
      S = QQ[x, y, z] / ideal(x^2, y^2, z^2)
      C = acyclicClosure(S, EndDegree => 4)
      tally apply(C.Degrees, d -> first d)
    Text
      For a non-c.i., further deviations appear:
    Example
      T = QQ[x, y] / ideal(x^2, x*y, y^2)
      D = acyclicClosure(T, EndDegree => 3)
      tally apply(D.Degrees, d -> first d)
  SeeAlso
    (acyclicClosure, DGAlgebra)
    koszulComplexDGA
    minimalModel
    killCycles
    StartDegree
    EndDegree
///

doc ///
  Key
    minimalModel
    (minimalModel, Ideal)
    (minimalModel, QuotientRing)
    [minimalModel, StartDegree]
    [minimalModel, EndDegree]
    [minimalModel, Variable]
  Headline
    Build the minimal DG algebra resolution of a quotient ring
  Usage
    A = minimalModel I
    A = minimalModel R
    A = minimalModel(I, EndDegree => n)
  Inputs
    I:Ideal
      An ideal in a polynomial ring @ TT "Q" @.
    R:QuotientRing
      A quotient @ TT "Q/I" @, where @ TT "Q" @ is a polynomial ring.
      The defining ideal @ TT "ideal R" @ (in @ TT "ambient R = Q" @)
      is used.
    StartDegree => ZZ
      First hom-degree at which to start killing homology.  Defaults
      to @ TT "1" @.
    EndDegree => ZZ
      Last hom-degree at which to kill homology.  Defaults to
      @ TT "3" @.
    Variable => String
      Name of the divided-power generators; defaults to
      @ TT "\"T\"" @.
  Outputs
    A:DGAlgebra
      A DG algebra over @ TT "Q" @ whose degree-zero homology is
      @ TT "Q/I" @ and whose positive-degree homology is killed
      through hom-degree @ TT "EndDegree" @.  In other words, a
      truncation of the minimal DG algebra resolution of
      @ TT "Q/I" @ as a @ TT "Q" @-algebra.
  Description
    Text
      Given an ideal @ TT "I" @ in a polynomial ring @ TT "Q" @,
      @ TT "minimalModel I" @ is a shorthand for
    Text
      @ TT "acyclicClosure(koszulComplexDGA I)" @ .
    Text
      The Koszul DG algebra on a generating set of @ TT "I" @ has
      @ TT "H_0 = Q/I" @, and acyclicClosure adjoins divided-power
      variables in successively higher hom-degrees to kill all
      positive homology, following Tate's construction.  The result
      is a semifree DG algebra resolution of @ TT "Q/I" @ over
      @ TT "Q" @ in the sense of Avramov, minimal in each hom-degree.
      When @ TT "R = Q/I" @ is passed, the defining ideal of
      @ TT "R" @ inside its ambient polynomial ring is used.
    Text
      For a complete intersection, the Koszul complex on a minimal
      generating set is already acyclic in positive degrees, so
      @ TT "minimalModel" @ returns just that Koszul DG algebra:
    Example
      Q = ZZ/101[x, y]
      I = ideal(x^2, y^2)
      A = minimalModel(I, EndDegree => 3)
      apply(0..3, n -> numcols getBasis(n, A))
    Text
      The @ TT "QuotientRing" @ form is equivalent:
    Example
      R = Q / I
      A' = minimalModel(R, EndDegree => 3)
      numgens A.natural == numgens A'.natural
    Text
      For a non-complete-intersection ideal the acyclic closure
      genuinely adjoins higher divided-power generators:
    Example
      J = ideal(x^2, x*y, y^2)
      B = minimalModel(J, EndDegree => 3)
      apply(0..3, n -> numcols getBasis(n, B))
    Text
      As with @ TO acyclicClosure @, @ TT "EndDegree" @ truncates
      the construction and @ TT "Variable" @ renames the adjoined
      generators:
    Example
      C = minimalModel(I, EndDegree => 2, Variable => "S")
      gens C.natural
    Text
      @ BOLD "Deviations and Gulliksen's theorem." @  The rank of
      @ TT "A" @ in hom-degree @ TT "n" @ is the @ TT "n" @-th
      @ EM "deviation" @ of @ TT "Q/I" @, a classical invariant
      which vanishes for @ TT "n >= 2" @ if and only if @ TT "Q/I" @
      is a complete intersection (Gulliksen-Avramov).  Compare the
      two shapes:
    Example
      A1 = minimalModel(ideal(x^2, y^2), EndDegree => 4);
      apply(0..4, n -> numcols getBasis(n, A1))
      A2 = minimalModel(ideal(x^2, x*y, y^2), EndDegree => 4);
      apply(0..4, n -> numcols getBasis(n, A2))
    Text
      @ TT "A1" @ concentrates in hom-degrees @ TT "0, 1" @ (the
      hallmark of a c.i.), while @ TT "A2" @ keeps acquiring
      divided-power generators, showing that @ TT "Q/(x^2, xy, y^2)" @
      is not a c.i.  Running
      @ TO minimalSemifreeResolution @ on the residue field over the
      minimal model then gives a test of Gulliksen's theorem, which
      states that over a codimension-@ TT "c" @ complete intersection
      the Betti numbers of any finitely generated module are
      eventually polynomial in the hom-degree, of degree at most
      @ TT "c - 1" @:
    Example
      A = minimalModel(ideal(x^2, y^2), EndDegree => 8);
      k = Q^1 / ideal(x, y);
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 8);
      b = apply(0..8, n -> numcols moduleDifferential(n, Mdg))
      apply(0..7, n -> b#(n+1) - b#n)
      apply(0..6, n -> b#(n+2) - 2*b#(n+1) + b#n)
    Text
      The Betti sequence is @ TT "(1, 4, 8, 12, 16, 20, 24, 28, 32)" @;
      first differences stabilize to a constant and second
      differences are eventually @ TT "0" @, so the numbers are
      eventually linear -- a degree-@ TT "1" @ polynomial, matching
      codimension @ TT "2" @.  A codimension-@ TT "3" @ c.i. such as
      @ TT "(x^2, y^2, z^2)" @ would instead show second differences
      stabilizing nonzero and third differences vanishing.
  Caveat
    The ring of @ TT "A" @ is always the polynomial ring containing
    the relations -- that is, @ TT "ring I" @ or @ TT "ambient R" @,
    not the quotient itself.  This matches the standard convention
    that the minimal DG algebra resolution of @ TT "Q/I" @ lives
    over @ TT "Q" @.
  SeeAlso
    acyclicClosure
    (acyclicClosure, Ring)
    koszulComplexDGA
    (koszulComplexDGA, Ideal)
///

doc ///
  Key
    (symbol **, DGAlgebra, Ring)
  Headline
    Base change of a DG algebra to another ring
  Usage
    B = A ** S
  Inputs
    A:DGAlgebra
    S:Ring
      A ring that admits substitution from A.ring.  The common case is
      @ TT "S = A.ring / I" @ for some ideal I, but any ring into which the
      variables of A.natural substitute is allowed.
  Outputs
    B:DGAlgebra
      A DG algebra over S with the same generator degrees as A and with
      differential obtained by substituting every entry of the differential
      matrix of A into B.natural.
  Description
    Text
      Base change: the underlying graded algebra is the same exterior / symmetric
      shape over S, and the differential is transported by @ TO substitute @.
      When S = A.ring / I, this models the DG algebra "A mod I" used, for
      instance, in building the Koszul complex of a quotient ring.
    Text
      The result is cached on @ TT "A.cache" @: repeated calls
      @ TT "A ** S" @ with the same pair (A, S) return the SAME DGAlgebra
      object.  This identity is essential for functoriality of base change on
      DGAlgebraMaps and DGModules — see @ TO "Base change and tensor with non-DG types" @.
    Example
      R = ZZ/101[a,b,c,d]
      A = koszulComplexDGA(R)
      S = R/ideal{a^3,a*b*c}
      B = A ** S
      B === A ** S
      Bdd = toComplex B
      Bdd.dd
    Text
      Base change along the trivial quotient @ TT "R / ideal 0_R" @ is structure-
      preserving: the result is a well-defined DGAlgebra over a ring isomorphic
      to R.
    Example
      R' = ZZ/101[x]
      A' = koszulComplexDGA R'
      QR' = R' / ideal(0_(R'))
      isWellDefined(A' ** QR')
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Ring)
    (symbol **, DGAlgebraMap, Ring)
///

doc ///
  Key
    "Base change and tensor with non-DG types"
  Headline
    Tensoring DG objects with ordinary rings and modules
  Description
    Text
      The DGAlgebras package extends @ TO2((symbol **, DGAlgebra, Ring), "** ") @
      so that every DG object (@ TO DGAlgebra @, @ TO DGModule @, @ TO DGSubmodule @,
      @ TO DGQuotientModule @, @ TO DGIdeal @, @ TO DGAlgebraMap @, @ TO DGModuleMap @)
      can be base-changed along any ring map out of the ground ring.  It also defines
      the exterior tensor of a DGModule with an ordinary @ TO Module @, subsuming the
      usual ring-of-scalars base change as the special case @ TT "M ** S^1" @.
    Text
      Two design choices make these operations usable in larger constructions.
    Text
      @BOLD "Caching (object identity)."@  Both @ TO (symbol **, DGAlgebra, Ring) @ and
      @ TO (symbol **, DGModule, Ring) @ cache their result, so repeated calls with the
      same operands return the SAME object.  This matters because a DGAlgebraMap or
      DGModuleMap is tagged by its source and target DG-objects, and without identity
      the source of @ TT "id_M ** S" @ and the target of @ TT "id_M ** S" @ would be two
      distinct (though isomorphic) DGModules.
    Text
      @BOLD "Functoriality."@  These operations are functorial: if @ TT "id" @ is the
      identity DGModuleMap on M, then @ TT "id ** S" @ is the identity DGModuleMap on
      @ TT "M ** S" @; likewise for DGAlgebraMaps.  Composition is preserved.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      QR = R / ideal(x^2)
      MQ = M ** QR
      MQ === M ** QR
      idM = identityDGModuleMap M
      (idM ** QR).source === MQ
      (idM ** QR).target === MQ
      isWellDefined(idM ** QR)
    Text
      For a DGModule M and an ordinary free module N, @ TT "M ** N" @ forms the
      exterior tensor product over @ TT "A.natural" @.  This lets you tensor by
      copies of a ring, by direct sums, or by arbitrary graded shifts without
      leaving the DG world.
    Example
      N = R^{0, -1}
      MN = M ** N
      class MN
      numgens MN
      rank MN.natural == (rank M.natural) * (rank N)
    Text
      The tensor is associative in the natural way: you can chain
      @ TT "(M ** N) ** K" @ for a sequence of ordinary free modules, producing
      a DGModule whose natural rank is the product of the ranks.
    Example
      K = R^3
      MNK = (M ** R^2) ** K
      isWellDefined MNK
      numgens MNK == 2 * 2 * 3
    Text
      A deliberate omission: there is no @ TT "** Ideal" @ method.  For an ideal
      I, the underlying module @ TT "module I" @ is almost never free, so exterior
      tensor with a DGModule is not well-defined in our framework.  Use
      @ TT "** (R/I)" @ for base change along the quotient instead.
  Subnodes
    (symbol **, DGAlgebra, Ring)
    (symbol **, DGModule, Ring)
    (symbol **, DGSubmodule, Ring)
    (symbol **, DGQuotientModule, Ring)
    (symbol **, DGIdeal, Ring)
    (symbol **, DGAlgebraMap, Ring)
    (symbol **, DGModuleMap, Ring)
    (symbol **, DGModule, Module)
    (symbol **, DGSubmodule, Module)
    (symbol **, DGQuotientModule, Module)
///

doc ///
  Key
    (symbol **, DGModule, Ring)
  Headline
    Base change of a DG module along a ring map
  Usage
    MS = M ** S
  Inputs
    M:DGModule
      A (free) DG module over some DG algebra A.
    S:Ring
      A ring admitting substitution from A.ring.  Typically S = A.ring / I.
  Outputs
    MS:DGModule
      A DG module over @ TT "A ** S" @ with the same generator multi-degrees as M
      and with differential obtained by substituting every entry of each
      differential column into the base-changed algebra.
  Description
    Text
      This is the base change of M along the ring map A.ring -> S, lifted to the
      DG level.  Since A ** S and M ** S are both cached, repeated tensoring
      with S is idempotent up to object identity:
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      QR = R / ideal(x^2)
      MS = M ** QR
      MS === M ** QR
      MS.dgAlgebra === A ** QR
      isWellDefined MS
    Text
      When M is the Koszul complex on a sequence and the map A.ring -> S is a
      quotient map, @ TT "M ** S" @ is the Koszul complex of the same sequence
      computed over S.
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGAlgebra, Ring)
    (symbol **, DGModuleMap, Ring)
///

doc ///
  Key
    (symbol **, DGSubmodule, Ring)
  Headline
    Base change of a DG submodule along a ring map
  Usage
    SubS = Sub ** S
  Inputs
    Sub:DGSubmodule
      A DG submodule of some ambient DG module M.
    S:Ring
      A ring admitting substitution from the ground ring of Sub.
  Outputs
    SubS:DGSubmodule
      The DG submodule of @ TT "M ** S" @ whose inclusion matrix is obtained by
      substituting every entry of the original inclusion into the base-changed
      ring.
  Description
    Text
      The ambient is @ TT "(Sub.ambient) ** S" @, so that for any DGQuotientModule
      built from Sub the quotient @ TT "(M/Sub) ** S" @ equals
      @ TT "(M ** S) / (Sub ** S) " @ up to canonical equality.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      Sub = dgSubmodule(M, matrix {{x_Anat, y_Anat}})
      QR = R / ideal(x^2)
      SubQ = Sub ** QR
      isWellDefined SubQ
      SubQ.ambient === M ** QR
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Ring)
    (symbol **, DGQuotientModule, Ring)
///

doc ///
  Key
    (symbol **, DGQuotientModule, Ring)
  Headline
    Base change of a DG quotient module along a ring map
  Usage
    QS = Q ** S
  Inputs
    Q:DGQuotientModule
    S:Ring
  Outputs
    QS:DGQuotientModule
      The quotient @ TT "(M ** S) / (Sub ** S)" @ where M = Q.ambient and
      Sub = Q.subDGModule.
  Description
    Text
      Internally, the submodule is rebuilt directly inside the base-changed
      ambient (rather than constructing @ TT "Sub ** S" @ separately and then
      quotienting); this is what ensures the ambient DGModule of the result is
      the cached @ TT "M ** S" @, not a distinct isomorphic copy.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      Sub = dgSubmodule(M, matrix {{x_Anat}})
      Q = M / Sub
      QR = R / ideal(x^2)
      QS = Q ** QR
      isWellDefined QS
      QS.ambient === M ** QR
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGSubmodule, Ring)
    (symbol **, DGModule, Ring)
///

doc ///
  Key
    (symbol **, DGIdeal, Ring)
  Headline
    Base change of a DG ideal along a ring map
  Usage
    IS = I ** S
  Inputs
    I:DGIdeal
      A DG ideal of some DG algebra A.
    S:Ring
  Outputs
    IS:DGIdeal
      The DG ideal of @ TT "A ** S" @ generated by the images of the original
      generators of I under @ TO substitute @.
  Description
    Text
      Unlike @ TT "** Ideal" @ (which is not defined — see the note in
      @ TO "Base change and tensor with non-DG types" @), this operation does
      land inside the DG world: DGIdeal is always generated by elements, so
      substitution along the ring map of base change produces a well-formed
      DGIdeal of @ TT "A ** S" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, y_Anat})
      QR = R / ideal(x^2)
      IQ = I ** QR
      isWellDefined IQ
      IQ.dgAlgebra === A ** QR
    Text
      Base change preserves the zero and unit DG ideals: the zero ideal stays
      zero, and the unit ideal stays the whole algebra.
    Example
      Z = dgIdeal(A, {})
      ZQ = Z ** QR
      isWellDefined ZQ and isZero ZQ
      U = dgIdeal(A, {1_Anat})
      UQ = U ** QR
      isWellDefined UQ and not isZero UQ
    Text
      Multivariate quotients and multi-generator DG ideals interact as expected.
    Example
      R3 = ZZ/101[x,y,z]
      A3 = koszulComplexDGA R3
      Anat3 = A3.natural
      I3 = dgIdeal(A3, {x_Anat3, y_Anat3, z_Anat3})
      QR3 = R3 / ideal(x*y, y*z)
      isWellDefined(I3 ** QR3)
  SeeAlso
    "Base change and tensor with non-DG types"
    dgIdeal
    (symbol **, DGAlgebra, Ring)
///

doc ///
  Key
    (symbol **, DGAlgebraMap, Ring)
  Headline
    Base change of a DG algebra map along a ring map
  Usage
    phiS = phi ** S
  Inputs
    phi:DGAlgebraMap
      A DG algebra map A -> B.
    S:Ring
  Outputs
    phiS:DGAlgebraMap
      A DG algebra map @ TT "A ** S -> B ** S" @ whose underlying RingMap
      sends each generator of A.natural to the image under phi, substituted
      into @ TT "(B ** S).natural" @.
  Description
    Text
      Functorial and compatible with object identity: because @ TT "A ** S" @
      and @ TT "B ** S" @ are cached, repeated evaluations of @ TT "phi ** S" @
      land in the same source/target, and
      @ TT "identityDGAlgebraMap(A) ** S" @ equals
      @ TT "identityDGAlgebraMap(A ** S)" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      idA = identityDGAlgebraMap A
      QR = R / ideal(x^2)
      idAQ = idA ** QR
      isWellDefined idAQ
      (idAQ.source) === (A ** QR)
      (idAQ.target) === (A ** QR)
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGAlgebra, Ring)
    identityDGAlgebraMap
///

doc ///
  Key
    (symbol **, DGModuleMap, Ring)
  Headline
    Base change of a DG module map along a ring map
  Usage
    fS = f ** S
  Inputs
    f:DGModuleMap
      A DG module map M -> N over a DG algebra A.
    S:Ring
  Outputs
    fS:DGModuleMap
      A DG module map @ TT "M ** S -> N ** S" @ (over @ TT "A ** S" @) whose
      underlying matrix is obtained entry-wise by substituting each entry of
      f.natural into the base-changed ring.
  Description
    Text
      Functorial: @ TT "identityDGModuleMap(M) ** S" @ equals
      @ TT "identityDGModuleMap(M ** S)" @, and composition is preserved.
      Cache-identity is what ensures the source and target of the result are
      the same cached DG modules as @ TT "M ** S" @ and @ TT "N ** S" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      idM = identityDGModuleMap M
      QR = R / ideal(x^2)
      idMQ = idM ** QR
      isWellDefined idMQ
      (idMQ.source) === (M ** QR)
      (idMQ.target) === (M ** QR)
    Text
      Functoriality is literal at the matrix level: the natural matrix of
      @ TT "idM ** QR" @ is the identity matrix on @ TT "(M ** QR).natural" @.
    Example
      MQ = M ** QR
      idMQ.natural == id_(MQ.natural)
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Ring)
    identityDGModuleMap
///

doc ///
  Key
    (symbol **, DGModule, Module)
    (symbol **, Module, DGModule)
  Headline
    Exterior tensor product of a DG module with an ordinary free module
  Usage
    P = M ** N
    P = N ** M
  Inputs
    M:DGModule
      Must be semifree: M.natural must be a free A.natural-module.
    N:Module
      An ordinary module over A.ring (or a compatible ring) that is also free.
  Outputs
    P:DGModule
      A DG module over the same DG algebra A, with
      @ TT "rank P.natural = (rank M.natural) * (rank N)" @ generators.  The
      multi-degree of the (i, j)-th natural generator is
      @ TT "M.Degrees#i + (0, (degrees N)#j)" @ (homological degree from M,
      internal degrees shifted by the internal degree of the j-th generator of N).
      The differential is @ TT "d_M \\otimes id_N" @.
  Description
    Text
      This is the exterior tensor product of DG-modules, where @ TT "N" @ is
      viewed as a complex concentrated in homological degree 0 with zero
      differential.  The commuted form @ TT "N ** M" @ is defined as
      @ TT "M ** N" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      N = R^2
      MN = M ** N
      class MN
      numgens MN
      isWellDefined MN
    Text
      The special case @ TT "M ** R^1" @ is canonically isomorphic to M, and
      @ TT "M ** R^{d}" @ is a shift of M by the internal degree d.
    Text
      Multi-degree M tensors predictably: if M has multiple homological degrees
      and N is free of rank r, the result has @ TT "(rank M.natural) * r" @
      natural generators and is well-defined.
    Example
      M2m = freeDGModule(A, {0, 1})
      N3 = R^3
      M2N = M2m ** N3
      numgens M2N == 2 * 3
      isWellDefined M2N
    Text
      Edge case: tensoring with the zero module @ TT "R^0" @ gives a well-defined
      zero DGModule.
    Example
      MZ = M ** R^0
      isWellDefined MZ
      numgens MZ == 0
  Caveat
    Both M and N must be free (semifree on the DG side).  For the non-free case,
    take an initial free resolution and then tensor.
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Ring)
    (symbol **, DGModule, DGModule)
///

doc ///
  Key
    (symbol **, DGSubmodule, Module)
    (symbol **, Module, DGSubmodule)
  Headline
    Exterior tensor product of a DG submodule with an ordinary free module
  Usage
    SubN = Sub ** N
    SubN = N ** Sub
  Inputs
    Sub:DGSubmodule
    N:Module
      A free module over a compatible ring.
  Outputs
    SubN:DGSubmodule
      The DG submodule of @ TT "(Sub.ambient) ** N" @ whose inclusion acts as
      @ TT "inclusion(Sub) \\otimes id_N" @ on natural generators indexed by
      (i, j).
  Description
    Text
      Equivalently: for each natural generator of N, embed a copy of Sub's
      inclusion matrix into the corresponding block of the tensored ambient.
      The commuted form @ TT "N ** Sub" @ is defined as @ TT "Sub ** N" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      Sub = dgSubmodule(M, matrix {{x_Anat, y_Anat}})
      N = R^2
      SubN = Sub ** N
      isWellDefined SubN
      SubN.ambient === M ** N
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Module)
    (symbol **, DGSubmodule, Ring)
///

doc ///
  Key
    (symbol **, DGQuotientModule, Module)
    (symbol **, Module, DGQuotientModule)
  Headline
    Exterior tensor product of a DG quotient module with an ordinary free module
  Usage
    QN = Q ** N
    QN = N ** Q
  Inputs
    Q:DGQuotientModule
    N:Module
      A free module over a compatible ring.
  Outputs
    QN:DGQuotientModule
      The DG quotient module @ TT "(Q.ambient ** N) / (Q.subDGModule ** N)" @,
      built directly inside the tensored ambient so that the ambient of the
      result equals @ TT "Q.ambient ** N" @ on the nose.
  Description
    Text
      The commuted form @ TT "N ** Q" @ is defined as @ TT "Q ** N" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      Sub = dgSubmodule(M, matrix {{x_Anat}})
      Q = M / Sub
      N = R^2
      QN = Q ** N
      isWellDefined QN
      QN.ambient === M ** N
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Module)
    (symbol **, DGQuotientModule, Ring)
///

doc ///
  Key
    "Operations on DG Ideals"
  Headline
    Sum, product, power, intersection, colon, and other ideal-algebra operations on DG ideals
  Description
    Text
      A DG ideal of a DG algebra A is a graded ideal of @ TT "A.natural" @ that is
      closed under the differential d of A.  The @ TO DGIdeal @ type wraps such an
      ideal and exposes the standard M2 @ TO Ideal @ operations, each of which
      preserves d-closure and returns a new @ TO DGIdeal @.
    Text
      Constructor: @ TO dgIdeal @ takes a List, Matrix, or Ideal of elements of
      @ TT "A.natural" @ and iteratively d-saturates the ideal they generate
      until it is closed under d.  Warning: if an input element is not a cycle,
      the d-saturation enlarges the ideal (possibly to the unit ideal).
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      J = dgIdeal(A, {y_Anat})
      isWellDefined I
      isDGIdeal(A, I.natural)
    Text
      Ideal-algebra operations all preserve d-closure by elementary arguments
      (sum: @ TT "d(I+J) \\subset dI + dJ \\subset I + J" @; product: by the
      Leibniz rule; intersection: direct; power: induction; colon: Leibniz +
      @ TT "f J \\subset I" @ implies @ TT "(df) J \\subset I + f(dJ) \\subset I" @).
      Concretely, they all return well-defined DGIdeals.
    Example
      isWellDefined(I + J)
      isWellDefined(I * J)
      isWellDefined(I^3)
      isWellDefined intersect(I, J)
      isWellDefined(I : J)
    Text
      Equality, containment, and reduction mirror the usual behavior on @ TO Ideal @.
    Example
      isSubset(I, I + J)
      I != I + J
      (x_Anat^2 % I) == 0
      (y_Anat^2 % I) == y_Anat^2
    Text
      Quotient DG algebras: @ TT "A / I" @ builds the DG algebra whose
      underlying ring is @ TT "A.natural / I.natural" @, with differential
      descending from d (well-defined precisely because I is d-closed).
    Example
      B = A / I
      class B
      isWellDefined B
    Text
      Compatibility: any operation that takes an @ TO Ideal @ and lives on
      @ TT "I.natural" @ can be reached via @ TT "I.natural" @, but the
      wrapped operations on DGIdeal return DGIdeals (so d-closure is
      certified on the result by running through @ TO dgIdeal @ once more).
  Subnodes
    DGIdeal
    dgIdeal
    isDGIdeal
    (isWellDefined, DGIdeal)
    (symbol +, DGIdeal, DGIdeal)
    (symbol *, DGIdeal, DGIdeal)
    (symbol ^, DGIdeal, ZZ)
    (intersect, DGIdeal, DGIdeal)
    (symbol :, DGIdeal, DGIdeal)
    (symbol ==, DGIdeal, DGIdeal)
    (symbol %, RingElement, DGIdeal)
    (mingens, DGIdeal)
    (prune, DGIdeal)
    (symbol /, DGAlgebra, DGIdeal)
    (isHomogeneous, DGIdeal)
  SeeAlso
    DGIdeal
    dgIdeal
    (symbol **, DGIdeal, Ring)
///

doc ///
  Key
    DGIdeal
  Headline
    The class of d-closed graded ideals of a DG algebra
  Description
    Text
      A @ TT "DGIdeal" @ wraps a graded ideal of @ TT "A.natural" @ that is
      closed under the differential d of A.  It records the ambient DG
      algebra, the underlying Ideal, and a chosen set of generators.  Create
      one via @ TO dgIdeal @.
    Text
      The standard ideal-algebra operations (@ TO (symbol +, DGIdeal, DGIdeal) @,
      @ TO (symbol *, DGIdeal, DGIdeal) @, @ TO (symbol ^, DGIdeal, ZZ) @,
      @ TO (intersect, DGIdeal, DGIdeal) @, @ TO (symbol :, DGIdeal, DGIdeal) @)
      all preserve d-closure and return new DGIdeals.  See
      @ TO "Operations on DG Ideals" @ for a guided tour.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat * y_Anat})
      class I
      ambient I === A
      ring I === R
      numgens I
  SeeAlso
    dgIdeal
    "Operations on DG Ideals"
    (symbol /, DGAlgebra, DGIdeal)
///

doc ///
  Key
    dgIdeal
    (dgIdeal, DGAlgebra, List)
    (dgIdeal, DGAlgebra, Matrix)
    (dgIdeal, DGAlgebra, Ideal)
  Headline
    Construct a DG ideal from a list, matrix, or ideal of generators
  Usage
    I = dgIdeal(A, gs)
  Inputs
    A:DGAlgebra
    gs:
      A List of elements of @ TT "A.natural" @, a Matrix of such, or an
      @ TO Ideal @ of @ TT "A.natural" @.
  Outputs
    I:DGIdeal
      The smallest d-closed ideal of @ TT "A.natural" @ containing the given
      generators.  Constructed by iteratively adjoining derivatives until the
      ideal stabilizes.
  Description
    Text
      If the inputs are already cycles, no new generators are introduced and
      @ TT "I.natural" @ is the ordinary ideal they generate.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      xA = sub(x, Anat)
      I = dgIdeal(A, {xA})
      numgens I.natural == 1
      isDGIdeal(A, I.natural)
    Text
      If a seed element is not a cycle, d-saturation may enlarge the ideal.
      For the Koszul complex on {x}, @ TT "d(T_1) = x" @, so
      @ TT "dgIdeal(A, {T_1}) " @ ends up containing both @ TT "T_1" @ and
      @ TT "x" @.
    Example
      R' = ZZ/101[x]
      A' = koszulComplexDGA R'
      T = (A'.natural)_0
      I' = dgIdeal(A', {T})
      numgens I'.natural
      isDGIdeal(A', I'.natural)
    Text
      A more interesting multi-variable example: over the Koszul algebra of
      @ TT "QQ[x, y]" @, the top-degree exterior product @ TT "T_1 T_2" @ has
      @ TT "d(T_1 T_2) = -y T_1 + x T_2" @, and d-saturation records both the
      seed and its differential as minimal generators of the resulting
      d-closed ideal:
    Example
      R2 = QQ[x, y]
      A2 = koszulComplexDGA R2
      T1 = (A2.natural)_0
      T2 = (A2.natural)_1
      polyDifferential(A2, T1 * T2)
      J = dgIdeal(A2, {T1 * T2})
      mingens J.natural
    Text
      An empty seed or a list of zero seeds yields the zero DG ideal.
    Example
      Z = dgIdeal(A, {})
      isWellDefined Z
      isZero Z
      Z' = dgIdeal(A, {0_Anat, 0_Anat})
      isZero Z'
    Text
      A seed containing @ TT "1" @ gives the unit DG ideal.
    Example
      U = dgIdeal(A, {1_Anat, x_Anat})
      U.natural == ideal 1_Anat
  SeeAlso
    DGIdeal
    isDGIdeal
    "Operations on DG Ideals"
///

doc ///
  Key
    isDGIdeal
    (isDGIdeal, DGAlgebra, Ideal)
  Headline
    Test whether an ideal of A.natural is closed under the differential of A
  Usage
    b = isDGIdeal(A, I)
  Inputs
    A:DGAlgebra
    I:Ideal
      An ideal of @ TT "A.natural" @.
  Outputs
    b:Boolean
      True iff every minimal generator g of I satisfies
      @ TT "polyDifferential(A, g) % I == 0" @.
  Description
    Text
      Unlike @ TO dgIdeal @ (which d-saturates), @ TT "isDGIdeal" @ simply
      checks d-closure without modifying the input.  Use it to validate that
      an externally-produced Ideal is already a DG ideal.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      isDGIdeal(A, ideal(x_Anat, y_Anat))
      T = Anat_0   -- first Koszul variable T_{1,1}; d(T) = x
      isDGIdeal(A, ideal(T))
    Text
      The second test returns false: the ideal @ TT "(T)" @ is not d-closed
      because @ TT "d(T) = x" @ is not in it.  Passing the same seed to
      @ TO dgIdeal @ produces the d-closure.
    Example
      J = dgIdeal(A, {T})
      isDGIdeal(A, J.natural)
  SeeAlso
    dgIdeal
    DGIdeal
///

doc ///
  Key
    (isWellDefined, DGIdeal)
  Headline
    Verify that a DGIdeal has correct structure and is d-closed
  Usage
    b = isWellDefined I
  Inputs
    I:DGIdeal
  Outputs
    b:Boolean
  Description
    Text
      Performs (1) a key-shape check — the expected fields (@ TT "dgAlgebra, ring,
      natural, generators, cache" @) are present and have the correct types —
      and (2) the semantic d-closure check via @ TO isDGIdeal @.  Set
      @ TT "debugLevel > 0" @ to see a diagnostic message on failure.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, y_Anat})
      isWellDefined I
  SeeAlso
    dgIdeal
    isDGIdeal
    DGIdeal
///

doc ///
  Key
    (symbol +, DGIdeal, DGIdeal)
  Headline
    Sum of two DG ideals
  Usage
    K = I + J
  Inputs
    I:DGIdeal
    J:DGIdeal
      With @ TT "I.dgAlgebra === J.dgAlgebra" @.
  Outputs
    K:DGIdeal
      The DGIdeal with @ TT "K.natural = I.natural + J.natural" @.
  Description
    Text
      Preserves d-closure because @ TT "d(I + J) \\subset dI + dJ \\subset I + J" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      J = dgIdeal(A, {y_Anat})
      K = I + J
      isWellDefined K
      (I + J) == (J + I)
    Text
      The zero DGIdeal is a two-sided identity for @ TT "+" @.
    Example
      Z = dgIdeal(A, {})
      (Z + I) == I
  SeeAlso
    "Operations on DG Ideals"
    (symbol *, DGIdeal, DGIdeal)
    (intersect, DGIdeal, DGIdeal)
///

doc ///
  Key
    (symbol *, DGIdeal, DGIdeal)
  Headline
    Product of two DG ideals
  Usage
    K = I * J
  Inputs
    I:DGIdeal
    J:DGIdeal
      With @ TT "I.dgAlgebra === J.dgAlgebra" @.
  Outputs
    K:DGIdeal
      The DGIdeal with @ TT "K.natural = I.natural * J.natural" @.
  Description
    Text
      Preserves d-closure by the Leibniz rule: @ TT "d(ab) = d(a) b \\pm a d(b)" @
      shows @ TT "d(IJ) \\subset (dI) J + I (dJ) \\subset IJ" @.  When the ambient
      ring is commutative, multiplication is commutative.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      J = dgIdeal(A, {y_Anat})
      K = I * J
      isWellDefined K
      (I * J) == (J * I)
    Text
      The unit DGIdeal is a two-sided identity for @ TT "*" @ (as ideals).
    Example
      U = dgIdeal(A, {1_Anat})
      (U * I) == I
  SeeAlso
    "Operations on DG Ideals"
    (symbol +, DGIdeal, DGIdeal)
    (symbol ^, DGIdeal, ZZ)
///

doc ///
  Key
    (symbol ^, DGIdeal, ZZ)
  Headline
    Nonnegative integer power of a DG ideal
  Usage
    K = I^n
  Inputs
    I:DGIdeal
    n:ZZ
      A nonnegative integer.
  Outputs
    K:DGIdeal
      @ TT "I^0" @ is the unit DGIdeal @ TT "(1)" @; @ TT "I^n" @ for
      @ TT "n >= 1" @ is the n-fold product @ TT "I * I * ... * I" @.
  Description
    Text
      Distributes with multiplication: @ TT "I^2 * I == I^3" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, y_Anat})
      isWellDefined(I^2)
      isWellDefined(I^3)
      (I^2 * I) == I^3
    Text
      @ TT "I^0" @ always returns the unit DGIdeal @ TT "(1)" @.  Taking
      powers of the zero DG ideal is well-defined at every exponent.
    Example
      I^0
      Z = dgIdeal(A, {})
      isWellDefined(Z^0) and isWellDefined(Z^5)
  Caveat
    Negative exponents are rejected with an error.
  SeeAlso
    "Operations on DG Ideals"
    (symbol *, DGIdeal, DGIdeal)
///

doc ///
  Key
    (intersect, DGIdeal, DGIdeal)
  Headline
    Intersection of two DG ideals
  Usage
    K = intersect(I, J)
  Inputs
    I:DGIdeal
    J:DGIdeal
      With @ TT "I.dgAlgebra === J.dgAlgebra" @.
  Outputs
    K:DGIdeal
      The DGIdeal with @ TT "K.natural = intersect(I.natural, J.natural)" @.
  Description
    Text
      Intersections preserve d-closure directly: if g lies in both I and J,
      then d(g) lies in both (by d-closure of each), hence in their intersection.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      J = dgIdeal(A, {y_Anat})
      K = intersect(I, J)
      isWellDefined K
    Text
      Intersecting with the zero DGIdeal yields the zero DGIdeal.
    Example
      Z = dgIdeal(A, {})
      ZI = intersect(Z, I)
      isWellDefined ZI
      numgens ZI.natural == 0
  SeeAlso
    "Operations on DG Ideals"
    (symbol +, DGIdeal, DGIdeal)
    (symbol *, DGIdeal, DGIdeal)
///

doc ///
  Key
    (symbol :, DGIdeal, DGIdeal)
  Headline
    Ideal quotient (colon) of two DG ideals
  Usage
    K = I : J
  Inputs
    I:DGIdeal
    J:DGIdeal
      With @ TT "I.dgAlgebra === J.dgAlgebra" @.
  Outputs
    K:DGIdeal
      The DGIdeal with @ TT "K.natural = I.natural : J.natural" @ — i.e.
      @ TT "{ f \\in A.natural | f J \\subset I }" @.
  Description
    Text
      Preserves d-closure: if @ TT "f J \\subset I" @ then by Leibniz
      @ TT "d(f) J = d(fJ) - f d(J) \\subset dI + f J \\subset I + I = I" @,
      so @ TT "d(f) \\in I : J" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat * y_Anat})
      J = dgIdeal(A, {y_Anat})
      K = I : J
      isWellDefined K
  SeeAlso
    "Operations on DG Ideals"
    (symbol *, DGIdeal, DGIdeal)
///

doc ///
  Key
    (symbol ==, DGIdeal, DGIdeal)
    (isSubset, DGIdeal, DGIdeal)
  Headline
    Equality and containment of DG ideals
  Usage
    b = I == J
    b = isSubset(I, J)
  Inputs
    I:DGIdeal
    J:DGIdeal
  Outputs
    b:Boolean
  Description
    Text
      Both compare DGIdeals via their underlying Ideals @ TT "I.natural" @
      and @ TT "J.natural" @.  Equality additionally requires the DG algebras
      to be the same object (@ TT "=== " @ — object identity, not just
      isomorphism).  Containment also asserts that I and J share an ambient
      DG algebra.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      J = dgIdeal(A, {x_Anat, y_Anat})
      isSubset(I, J)
      I != J
      I == I
  SeeAlso
    "Operations on DG Ideals"
    (symbol +, DGIdeal, DGIdeal)
///

doc ///
  Key
    (symbol %, RingElement, DGIdeal)
    (symbol %, ZZ, DGIdeal)
  Headline
    Reduce a ring element modulo a DG ideal
  Usage
    r = f % I
  Inputs
    f:
      A RingElement of @ TT "I.dgAlgebra.natural" @, or an integer (coerced
      into @ TT "I.dgAlgebra.natural" @ before reduction).
    I:DGIdeal
  Outputs
    r:RingElement
      The normal form of f modulo @ TT "I.natural" @.
  Description
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      (x_Anat^2 % I) == 0
      (y_Anat^2 % I) == y_Anat^2
  SeeAlso
    "Operations on DG Ideals"
    (symbol ==, DGIdeal, DGIdeal)
///

doc ///
  Key
    (mingens, DGIdeal)
    (numgens, DGIdeal)
    (module, DGIdeal)
    (ring, DGIdeal)
    (generators, DGIdeal)
  Headline
    Basic accessors on a DG ideal
  Description
    Text
      Thin wrappers around the corresponding @ TO Ideal @ operations on
      @ TT "I.natural" @, with the exception of @ TT "ambient I" @, which
      returns the ambient @ TO DGAlgebra @ (not its underlying ring).
    Text
      @ TT "mingens I" @ returns the minimal-generator matrix of
      @ TT "I.natural" @.  Note that the d-saturation in @ TO dgIdeal @ may
      have produced redundant generators; @ TT "mingens I" @ re-extracts a
      minimal set.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, x_Anat * y_Anat})
      mingens I
      numgens I
    Text
      @ TT "numgens I" @ equals @ TT "numgens I.natural" @ — the number of
      (stored, possibly redundant) generators of the underlying ideal.
    Example
      numgens I == numgens I.natural
    Text
      @ TT "module I" @ returns the underlying Module-view of the ideal, and
      @ TT "ring I, ambient I, generators I" @ expose the ground ring, the
      ambient DG algebra, and the chosen generator matrix, respectively.
    Example
      ring I === R
      ambient I === A
      module I
  SeeAlso
    "Operations on DG Ideals"
    dgIdeal
    (prune, DGIdeal)
///

doc ///
  Key
    (prune, DGIdeal)
    (minimalPresentation, DGIdeal)
  Headline
    Trim a DG ideal down to a minimal generating set
  Usage
    J = prune I
    J = minimalPresentation I
  Inputs
    I:DGIdeal
  Outputs
    J:DGIdeal
      A DGIdeal with @ TT "J.natural == I.natural" @ (as ideals) but with a
      possibly smaller generator list obtained by @ TO trim @.
  Description
    Text
      @ TT "prune" @ computes @ TT "trim I.natural" @ and wraps the result.
      It caches a @ TT "pruningMap" @ in @ TT "J.cache" @ giving the canonical
      comparison map @ TT "J -> I" @; since DGIdeals have the same ambient
      DG algebra before and after pruning, this is recorded as the identity
      DGAlgebraMap on @ TT "I.dgAlgebra" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, x_Anat * y_Anat, x_Anat^2})
      J = prune I
      isWellDefined J
      J.natural == I.natural
    Text
      Idempotent: pruning an already-minimal DGIdeal is a no-op at the
      ideal level (the stored generator list already agrees with
      @ TT "mingens" @).  Repeated pruning is safe.
    Example
      isWellDefined prune prune prune J
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @ on
      DGIdeals.
  SeeAlso
    "Operations on DG Ideals"
    (mingens, DGIdeal)
    dgIdeal
///

doc ///
  Key
    (symbol /, DGAlgebra, DGIdeal)
  Headline
    Quotient DG algebra by a DG ideal
  Usage
    B = A / I
  Inputs
    A:DGAlgebra
    I:DGIdeal
      A DG ideal of A.  Must satisfy @ TT "I.dgAlgebra === A" @.
  Outputs
    B:DGAlgebra
      A DG algebra whose underlying ring is @ TT "A.natural / I.natural" @
      and whose differential is the (well-defined) descent of d_A.
  Description
    Text
      The descent is well-defined precisely because I is d-closed: for any
      lift g' of a class g in B, the difference @ TT "g - g'" @ lies in I,
      so @ TT "d_A(g) - d_A(g') \\in d_A(I) \\subset I" @ and the image
      @ TT "[d_A(g)] \\in B" @ is independent of the chosen lift.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      Anat = A.natural
      T = Anat_0
      I = dgIdeal(A, {T})
      B = A / I
      isWellDefined B
      B.natural
  SeeAlso
    "Operations on DG Ideals"
    DGIdeal
    dgIdeal
///

doc ///
  Key
    (isHomogeneous, DGIdeal)
    (isZero, DGIdeal)
  Headline
    Predicates on a DG ideal
  Description
    Text
      @ TT "isHomogeneous I" @ returns @ TT "isHomogeneous I.natural" @ —
      i.e. tests whether the underlying ideal is homogeneous with respect to
      the grading of @ TT "A.natural" @.
    Text
      @ TT "isZero I" @ returns true iff @ TT "I.natural" @ is the zero
      ideal (equivalently, @ TT "numgens I.natural == 0" @).
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, y_Anat})
      isHomogeneous I
      isZero I
      Z = dgIdeal(A, {})
      isZero Z
  SeeAlso
    "Operations on DG Ideals"
    DGIdeal
///

doc ///
  Key
    "Building DG modules, submodules, and quotients"
  Headline
    The DGModule / DGSubmodule / DGQuotientModule hierarchy and its constructors
  Description
    Text
      The package provides three related types of DG-module-like objects
      over a fixed @ TO DGAlgebra @ A:
    Text
      @ TO DGModule @ — a free graded @ TT "A.natural" @-module equipped
      with a differential that satisfies the Leibniz rule against A.
      Built via @ TO freeDGModule @, with differentials set by
      @ TO setDiff @.
    Text
      @ TO DGSubmodule @ — a subobject of a DGModule M, represented by a
      matrix of generators whose column span is closed under the
      differential of M.  Built via @ TO dgSubmodule @, which
      @ TT "d" @-saturates the seed generators automatically.
    Text
      @ TO DGQuotientModule @ — the cokernel M / S of the inclusion
      S → M, with the induced differential.  Built via @ TO dgQuotientModule @
      or the shorthand @ TT "M / S" @.
    Text
      Every object in this hierarchy carries an underlying graded
      @ TT "A.natural" @-module accessible via @ TT "M.natural" @, a list of
      multi-degrees @ TT "M.Degrees" @, and a list of differentials
      @ TT "M.diff" @ (one per natural generator for DGModule and
      DGSubmodule; per quotient-presentation generator for
      DGQuotientModule).  Structural correctness can be checked with
      @ TO isWellDefined @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      isWellDefined M
      Anat = A.natural
      S = dgSubmodule(M, (id_(M.natural))_{0})
      isWellDefined S
      Q = M / S
      isWellDefined Q
  Subnodes
    DGModule
    DGSubmodule
    DGQuotientModule
    freeDGModule
    dgSubmodule
    dgQuotientModule
    (symbol /, DGModule, DGSubmodule)
  SeeAlso
    "Module-like operations on DG modules"
    "Operations on DG Submodules"
    DGAlgebra
///

doc ///
  Key
    DGModule
  Headline
    The class of DG modules over a DG algebra
  Description
    Text
      A @ TT "DGModule" @ M over a @ TO DGAlgebra @ A is a free graded
      @ TT "A.natural" @-module together with a differential that squares
      to zero and satisfies the Leibniz rule against A.  Internally M is
      a hashtable with keys:
    Text
      @ TT "M.dgAlgebra" @ — the ambient @ TO DGAlgebra @.
    Text
      @ TT "M.natural" @ — the underlying graded @ TT "A.natural" @-module.
    Text
      @ TT "M.Degrees" @ — the list of multi-degrees of the natural generators.
    Text
      @ TT "M.diff" @ — a list of differentials, one per natural generator,
      each living in @ TT "M.natural" @ in the appropriate homological degree.
    Text
      New DGModules are constructed by @ TO freeDGModule @; the differential
      on generators is set via @ TO setDiff @.  Every
      @ TO DGSubmodule @ is also a DGModule (as a subtype); by contrast a
      @ TO DGQuotientModule @ is a separate type with a compatible presentation.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      isWellDefined M
      M.Degrees
  SeeAlso
    "Building DG modules, submodules, and quotients"
    freeDGModule
    setDiff
    DGSubmodule
    DGQuotientModule
///

doc ///
  Key
    DGSubmodule
  Headline
    The class of d-closed submodules of a DG module
  Description
    Text
      A @ TT "DGSubmodule" @ S of a @ TO DGModule @ M is a free
      A.natural-module together with an inclusion
      @ TT "S.inclusion : S → M" @ of DGModules whose column span in
      @ TT "M.natural" @ is closed under the differential of M.
      @ TT "DGSubmodule" @ is a subtype of @ TO DGModule @, so every
      DG-module operation applies to submodules as well.
    Text
      Beyond the DGModule keys, a DGSubmodule carries:
    Text
      @ TT "S.ambient" @ — the enclosing DGModule M.
    Text
      @ TT "S.inclusion" @ — a @ TO DGModuleMap @ S → M; use
      @ TO inclusion @ to access it.
    Text
      New DGSubmodules are constructed by @ TO dgSubmodule @, which
      takes either an inclusion matrix or a list of generators, and
      d-saturates the input automatically.  See
      @ TO "Operations on DG Submodules" @ for the lattice operations
      (sum, intersection, equality, containment).
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, id_(M.natural))
      ambient S === M
      source inclusion S === S
      target inclusion S === M
  SeeAlso
    "Building DG modules, submodules, and quotients"
    "Operations on DG Submodules"
    dgSubmodule
    DGModule
    inclusion
///

doc ///
  Key
    DGQuotientModule
  Headline
    The class of quotient DG modules M / S
  Description
    Text
      A @ TT "DGQuotientModule" @ Q = M / S is the cokernel of the
      inclusion @ TT "S.inclusion : S → M" @, equipped with the induced
      differential.  Unlike @ TO DGSubmodule @, @ TT "DGQuotientModule" @
      is NOT a subtype of @ TO DGModule @: its @ TT ".natural" @ is a
      cokernel module, which does not support the free-module operations
      (rank, basis by index) that DG algorithms assume on a generic
      DGModule.
    Text
      Beyond the DGModule-like keys (@ TT ".dgAlgebra" @, @ TT ".natural" @,
      @ TT ".Degrees" @, @ TT ".diff" @), a DGQuotientModule carries:
    Text
      @ TT "Q.ambient" @ — the DGModule M from which Q was built;
      accessible via @ TO ambient @.
    Text
      @ TT "Q.subDGModule" @ — the killed submodule S; accessible via
      @ TO subDGModule @.
    Text
      @ TT "Q.projection" @ — the quotient map M → Q as a
      @ TO DGModuleMap @; accessible via @ TO projection @.
    Text
      New DGQuotientModules are built by @ TO dgQuotientModule @ or the
      shorthand @ TT "M / S" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Z = dgSubmodule(M, {})
      Q = M / Z
      instance(Q, DGQuotientModule)
      ambient Q === M
      subDGModule Q === Z
      source projection Q === M
      target projection Q === Q
  SeeAlso
    "Building DG modules, submodules, and quotients"
    "Module-like operations on DG modules"
    dgQuotientModule
    (symbol /, DGModule, DGSubmodule)
///

doc ///
  Key
    freeDGModule
    (freeDGModule, DGAlgebra, List)
  Headline
    Construct a free DG module over a DG algebra
  Usage
    M = freeDGModule(A, degList)
  Inputs
    A:DGAlgebra
    degList:List
      of multi-degrees.  Each entry is either an integer (interpreted as
      a homological degree with trivial internal degree) or a list whose
      first entry is the homological degree and whose remaining entries
      are internal degrees.  Short lists are right-padded with zeros to
      match the length of a degree in @ TT "degrees A.natural" @.
  Outputs
    M:DGModule
      A DGModule whose @ TT ".natural" @ is the graded free
      @ TT "A.natural" @-module of rank @ TT "#degList" @ with the
      specified generator degrees, and whose differential is initialized
      to zero on every generator.
  Description
    Text
      The differential is initially set to zero on every generator, and is
      customized afterward via @ TO setDiff @.  Because the constructor
      returns a free @ TO DGModule @, the result can serve as a building block
      for semifree resolutions (see @ TO adjoinGenerators @ and @ TO semifreeResolution @)
      or as the starting point for a hand-built DG module, as illustrated below.
    Text
      Here we build the 2-periodic minimal free resolution of the residue field
      over the hypersurface @ TT "R = k[x]/(x^2)" @ — the prototypical Eisenbud
      matrix factorization — as an infinite semifree DG module over the Koszul
      DG algebra:
    Example
      R = QQ[x] / ideal(x^2)
      A = koszulComplexDGA R
      F = freeDGModule(A, toList(0..4))
      natGens = apply(rank F.natural, i -> (F.natural)_i)
      setDiff(F, {0, x * natGens#0, x * natGens#1, x * natGens#2, x * natGens#3})
      apply(1..4, n -> moduleDifferential(n, F))
    Text
      The differential alternates between multiplication by @ TT "x" @ in
      every degree, matching the classical 2-periodic resolution of the
      residue field.  Its homology is concentrated in degree zero:
    Example
      apply(0..3, n -> prune homology(n, F))
    Text
      More elaborate differentials are possible once multiple generators
      sit in overlapping hom-degrees.  Here is a rank-3 toy example where
      @ TT "F_1" @ has two generators and @ TT "d_1" @ sends them to
      @ TT "x" @ and @ TT "y" @ respectively, recovering the truncation of
      the Koszul complex on @ TT "(x, y)" @:
    Example
      S = QQ[x, y]
      AS = koszulComplexDGA S
      G = freeDGModule(AS, {0, 1, 1})
      natG = apply(rank G.natural, i -> (G.natural)_i)
      setDiff(G, {0, x * natG#0, y * natG#0})
      moduleDifferential(1, G)
    Text
      Internal degrees from a multi-graded base ring are inherited; a
      plain homological degree is padded with zeros to match.
    Example
      Rg = ZZ/101[x, y, Degrees => {{1, 0}, {0, 1}}]
      Ag = koszulComplexDGA Rg
      Mg = freeDGModule(Ag, {{0, 0, 0}, {1, 0, 0}})
      degrees Mg
    Text
      The empty-list form gives the zero DG module.
    Example
      M0 = freeDGModule(A, {})
      isZero M0
  SeeAlso
    "Building DG modules, submodules, and quotients"
    DGModule
    setDiff
    adjoinGenerators
    semifreeResolution
    (isWellDefined, DGModule)
///

doc ///
  Key
    dgSubmodule
    (dgSubmodule, DGModule, Matrix)
    (dgSubmodule, DGModule, List)
  Headline
    Construct a d-closed submodule of a DG module
  Usage
    S = dgSubmodule(M, incMat)
    S = dgSubmodule(M, gs)
  Inputs
    M:DGModule
    incMat:Matrix
      with target @ TT "M.natural" @, whose columns are proposed
      generators of the submodule.
    gs:List
      of elements of @ TT "M.natural" @ (as Vectors, 1-column Matrices,
      or anything coercible to @ TT "M.natural" @).  The empty list gives
      the zero submodule.
  Outputs
    S:DGSubmodule
      The smallest d-closed submodule of M containing the columns of
      @ TT "incMat" @ (respectively the elements of @ TT "gs" @).
  Description
    Text
      The constructor iteratively adjoins the M-differentials of the
      current generators until the column span is closed, then builds
      the lifted differential on the resulting free module.  If d-closure
      fails to stabilize within 200 iterations, an error is raised — in
      practice this only happens if the ambient module has unbounded
      homological degree and the seed is chosen poorly.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, id_(M.natural))
      instance(S, DGSubmodule)
      ambient S === M
      isWellDefined S
    Text
      The list form coerces each entry to an element of
      @ TT "M.natural" @; passing the empty list produces the zero
      submodule.
    Example
      Z = dgSubmodule(M, {})
      numcols (inclusion Z).natural == 0
      isZero Z
    Text
      d-closure is performed automatically.  Seeding with a non-closed
      generator pulls in its M-differential:
    Example
      M2 = freeDGModule(A, {0, 1})
      natGens = apply(rank M2.natural, i -> (M2.natural)_i)
      setDiff(M2, {0, x * natGens#0})
      Anat = A.natural
      seed = (id_(M2.natural))_{1}
      S2 = dgSubmodule(M2, seed)
      xe0 = (id_(M2.natural))_{0} * matrix {{x_Anat}}
      incMat = (inclusion S2).natural
      (xe0 - incMat * (xe0 // incMat)) == 0
    Text
      In a richer setting, consider the minimal semifree resolution of the
      residue field over the complete intersection
      @ TT "R = QQ[x, y]/(x^2, y^2)" @.  Seeding the submodule with the
      hom-degree-2 natural generators, d-closure automatically pulls in the
      hom-degree-4 generators that sit below them in the Koszul-style
      differential:
    Example
      S = QQ[x, y] / ideal(x^2, y^2)
      B = koszulComplexDGA S
      Mdg = minimalSemifreeResolution(B, S^1 / ideal(x, y), EndDegree => 3)
      homDegs = apply(Mdg.Degrees, d -> d#0)
      pos2 = positions(Mdg.Degrees, d -> d#0 == 2)
      seedHi = (id_(Mdg.natural))_pos2
      Shi = dgSubmodule(Mdg, seedHi)
      apply(Shi.Degrees, d -> d#0)
      isWellDefined Shi
  Caveat
    The target of the inclusion matrix must be @ TT "M.natural" @ on the
    nose (object identity, not just isomorphism).  This catches common
    shape mistakes up front — for example, building
    @ TT "matrix {{a, b}}" @ for a rank-n ambient produces a 1×2 matrix
    whose target is rank 1, not rank n.
  SeeAlso
    "Building DG modules, submodules, and quotients"
    "Operations on DG Submodules"
    DGSubmodule
    inclusion
    (isWellDefined, DGModule)
///

doc ///
  Key
    dgQuotientModule
    (dgQuotientModule, DGModule, DGSubmodule)
  Headline
    Construct the quotient DG module M / S
  Usage
    Q = dgQuotientModule(M, S)
  Inputs
    M:DGModule
    S:DGSubmodule
      With @ TT "S.ambient === M" @.
  Outputs
    Q:DGQuotientModule
      The cokernel of @ TT "S.inclusion.natural" @ equipped with the
      induced differential.
  Description
    Text
      The shorthand @ TT "M / S" @ calls this constructor.  The resulting
      Q records the ambient M (via @ TO ambient @), the killed submodule
      S (via @ TO subDGModule @), and the quotient map M → Q (via
      @ TO projection @).
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Z = dgSubmodule(M, {})
      Q = dgQuotientModule(M, Z)
      instance(Q, DGQuotientModule)
      ambient Q === M
      subDGModule Q === Z
    Text
      Quotienting by the zero submodule gives a DGQuotientModule whose
      underlying cokernel is isomorphic to @ TT "M.natural" @; quotienting
      by the whole ambient yields the zero DGQuotientModule.
    Example
      Sfull = dgSubmodule(M, id_(M.natural))
      Qzero = M / Sfull
      isZero Qzero
  Caveat
    @ TT "S.ambient" @ must equal @ TT "M" @ as an object (===), not
    merely be isomorphic.  This enforces a strict lattice-of-submodules
    discipline on the resulting quotient.
  SeeAlso
    "Building DG modules, submodules, and quotients"
    (symbol /, DGModule, DGSubmodule)
    DGQuotientModule
    projection
    subDGModule
///

doc ///
  Key
    (symbol /, DGModule, DGSubmodule)
  Headline
    Quotient DG module: M / S
  Usage
    Q = M / S
  Inputs
    M:DGModule
    S:DGSubmodule
      With @ TT "S.ambient === M" @.
  Outputs
    Q:DGQuotientModule
  Description
    Text
      Shorthand for @ TT "dgQuotientModule(M, S)" @.  See
      @ TO dgQuotientModule @ for semantics and invariants.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Z = dgSubmodule(M, {})
      Q = M / Z
      ambient Q === M
      isWellDefined Q
    Text
      The zero and top-dimensional boundary cases are handled
      consistently: @ TT "M / 0" @ is a DGQuotientModule wrapping a
      cokernel isomorphic to M, and @ TT "M / M" @ is the zero
      DGQuotientModule.
    Example
      Sfull = dgSubmodule(M, id_(M.natural))
      isZero (M / Sfull)
  SeeAlso
    "Building DG modules, submodules, and quotients"
    dgQuotientModule
    DGQuotientModule
///

doc ///
  Key
    ambient
    (ambient, DGIdeal)
    (ambient, DGSubmodule)
    (ambient, DGQuotientModule)
    inclusion
    (inclusion, DGSubmodule)
    projection
    (projection, DGQuotientModule)
    subDGModule
    (subDGModule, DGQuotientModule)
  Headline
    Accessors for the ambient object, inclusion, projection, and killed submodule
  Description
    Text
      These accessors navigate between the DG types in the
      DGModule / DGSubmodule / DGQuotientModule hierarchy:
    Text
      @ TT "ambient" @ on a @ TO DGIdeal @ returns the enclosing
      @ TO DGAlgebra @; on a @ TO DGSubmodule @ or
      @ TO DGQuotientModule @ it returns the enclosing @ TO DGModule @.
    Text
      @ TT "inclusion" @ on a @ TO DGSubmodule @ S returns the
      @ TO DGModuleMap @ S → ambient(S).
    Text
      @ TT "projection" @ on a @ TO DGQuotientModule @ Q returns the
      @ TO DGModuleMap @ ambient(Q) → Q.
    Text
      @ TT "subDGModule" @ on a @ TO DGQuotientModule @ Q returns the
      @ TO DGSubmodule @ that was killed to form Q.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, id_(M.natural))
      Q = M / S
      ambient S === M
      ambient Q === M
      source inclusion S === S
      target inclusion S === M
      source projection Q === M
      target projection Q === Q
      subDGModule Q === S
    Text
      For a DGIdeal the analogous accessor returns the DGAlgebra:
    Example
      I = dgIdeal(A, {x_(A.natural)})
      ambient I === A
  SeeAlso
    "Building DG modules, submodules, and quotients"
    DGSubmodule
    DGQuotientModule
    DGIdeal
///

doc ///
  Key
    "Module-like operations on DG modules"
  Headline
    Basic queries (generators, rank, degrees, homogeneity) extended to DG modules and their sub- and quotient modules
  Description
    Text
      Every @ TO DGModule @ (and its @ TO DGSubmodule @ / @ TO DGQuotientModule @ variants)
      carries an underlying graded @ TT "A.natural" @-module, accessible via
      @ TT "M.natural" @.  The standard @ TO Module @ methods — @ TO numgens @,
      @ TO rank @, @ TO degrees @, @ TO isHomogeneous @, @ TO super @,
      @ TO cover @, @ TO relations @ — are lifted directly onto the DG types,
      so code written against ordinary modules can often be reused on DG
      objects unchanged.
    Text
      Two DG-specific predicates are added:
    Text
      @ TO isFreeDGModule @ — is the underlying @ TT "M.natural" @ a free
      graded @ TT "A.natural" @-module?  Every DGModule built via
      @ TO freeDGModule @ satisfies this; user-built DGModules with
      non-free @ TT ".natural" @ slots are tolerated by @ TO isWellDefined @
      as long as the differential closes.
    Text
      @ TO isZero @ — does the DG object have zero natural generators?
      Consistent across DGModule, DGSubmodule, DGQuotientModule, and
      DGIdeal, so predicates can be written generically.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1, 2})
      numgens M
      rank M
      degrees M
      isHomogeneous M
      isFreeDGModule M
      isZero M
    Text
      On quotient DG modules, @ TT "super" @ and @ TT "cover" @ both return
      the ambient DGModule, and @ TT "relations" @ returns the matrix of
      the killed inclusion — mirroring M2's cokernel encoding of a quotient.
    Example
      Anat = A.natural
      Mone = freeDGModule(A, {0})
      S = dgSubmodule(Mone, matrix {{x_Anat, y_Anat}})
      Q = Mone / S
      super Q === Mone
      cover Q === Mone
      relations Q
  Subnodes
    (numgens, DGModule)
    (rank, DGModule)
    (degrees, DGModule)
    (isHomogeneous, DGModule)
    isFreeDGModule
    (isZero, DGModule)
    (super, DGSubmodule)
  SeeAlso
    "Operations on DG Submodules"
    "Operations on DG Ideals"
    DGModule
///

doc ///
  Key
    (numgens, DGModule)
    (numgens, DGSubmodule)
    (numgens, DGQuotientModule)
  Headline
    Number of natural generators of a DG module (or sub- or quotient module)
  Usage
    n = numgens M
  Inputs
    M:
      A DGModule, DGSubmodule, or DGQuotientModule.
  Outputs
    n:ZZ
      The number of natural generators of the underlying graded @ TT "A.natural" @-module,
      equal to the length of its list of degrees.
  Description
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1, 2})
      numgens M == 3
    Text
      For a DGSubmodule the count is the number of columns of the inclusion
      matrix (= number of generators being included).  For a DGQuotientModule
      it is the number of natural generators of the quotient presentation,
      which by construction equals @ TT "numgens (cover Q).natural" @.
    Example
      Anat = A.natural
      M1 = freeDGModule(A, {0})
      S = dgSubmodule(M1, matrix {{x_Anat, y_Anat}})
      numgens S == 2
      Q = M1 / S
      numgens Q == numgens M1
    Text
      Boundary case: a DGModule freely generated on an empty list has zero
      generators and is @ TO isZero @.
    Example
      M0 = freeDGModule(A, {})
      numgens M0 == 0
      isZero M0
  SeeAlso
    "Module-like operations on DG modules"
    (rank, DGModule)
    (degrees, DGModule)
    isZero
///

doc ///
  Key
    (rank, DGModule)
    (rank, DGSubmodule)
  Headline
    Rank of the underlying free A.natural-module
  Usage
    r = rank M
  Inputs
    M:
      A DGModule or DGSubmodule whose @ TT ".natural" @ is free.
  Outputs
    r:ZZ
      @ TT "rank M.natural" @.
  Description
    Text
      For a free DGModule, @ TT "rank" @ and @ TO numgens @ coincide.
      For a DGSubmodule, @ TT "rank" @ reports the rank of the included
      free module (which is @ TT "numgens S" @ when the inclusion is
      injective on natural generators — always the case for submodules
      built by @ TO dgSubmodule @).
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      rank M == numgens M
      Anat = A.natural
      M1 = freeDGModule(A, {0})
      S = dgSubmodule(M1, matrix {{x_Anat, y_Anat}})
      rank S
    Text
      After pruning a DGSubmodule with redundant inclusions, @ TT "rank" @
      equals @ TT "numgens" @ on the pruned object.
    Example
      Sbig = dgSubmodule(M1, matrix {{1_Anat, x_Anat, y_Anat}})
      Sp = prune Sbig
      numgens Sp == rank Sp
  Caveat
    @ TT "rank" @ is not defined on @ TO DGQuotientModule @: the rank of a
    cokernel module triggers codim computations that fail over non-affine
    graded ambients.  Use @ TO numgens @ for the generator count.
  SeeAlso
    "Module-like operations on DG modules"
    (numgens, DGModule)
///

doc ///
  Key
    (degrees, DGModule)
    (degrees, DGSubmodule)
    (degrees, DGQuotientModule)
  Headline
    Multi-degrees of the natural generators of a DG module
  Usage
    ds = degrees M
  Inputs
    M:
      A DGModule, DGSubmodule, or DGQuotientModule.
  Outputs
    ds:List
      A list of multi-degrees, one per natural generator.  Each multi-degree
      is a list whose first entry is the homological degree and whose
      remaining entries are the internal degrees.
  Description
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1, 2})
      degrees M == {{0,0},{1,0},{2,0}}
    Text
      Internal degrees come from the ambient ring's grading and are
      concatenated onto the homological degree.
    Example
      Rg = ZZ/101[x, y, Degrees => {{1,0}, {0,1}}]
      Ag = koszulComplexDGA Rg
      Mg = freeDGModule(Ag, {{0,0,0}, {1,0,0}})
      degrees Mg
  SeeAlso
    "Module-like operations on DG modules"
    (numgens, DGModule)
    (isHomogeneous, DGModule)
///

doc ///
  Key
    (isHomogeneous, DGModule)
    (isHomogeneous, DGSubmodule)
    (isHomogeneous, DGQuotientModule)
  Headline
    Test whether the underlying graded module is homogeneous
  Usage
    b = isHomogeneous M
  Inputs
    M:
      A DGModule, DGSubmodule, or DGQuotientModule.
  Outputs
    b:Boolean
      @ TT "isHomogeneous M.natural" @.
  Description
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 2})
      isHomogeneous M
      Anat = A.natural
      M1 = freeDGModule(A, {0})
      S = dgSubmodule(M1, matrix {{x_Anat, y_Anat}})
      isHomogeneous S
      Q = M1 / S
      isHomogeneous Q
  SeeAlso
    "Module-like operations on DG modules"
    (degrees, DGModule)
///

doc ///
  Key
    isFreeDGModule
    (isFreeDGModule, DGModule)
    (isFreeDGModule, DGSubmodule)
  Headline
    Is the underlying A.natural-module free?
  Usage
    b = isFreeDGModule M
  Inputs
    M:
      A DGModule or DGSubmodule.
  Outputs
    b:Boolean
      @ TT "isFreeModule M.natural" @.
  Description
    Text
      Every DGModule built via @ TO freeDGModule @ (and every DGSubmodule of
      one) has free @ TT ".natural" @, so this returns true in the typical
      case.  The check exists because it is possible to build a DGModule
      directly whose underlying @ TT ".natural" @ module is non-free, and @ TO isWellDefined @ will
      tolerate such an object as long as the differential closes.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 2})
      isFreeDGModule M
      Anat = A.natural
      M1 = freeDGModule(A, {0})
      S = dgSubmodule(M1, matrix {{x_Anat}})
      isFreeDGModule S
    Text
      Tensor products, base changes, and direct sums constructed by this
      package all land in the free case, so @ TT "isFreeDGModule" @ remains
      true after any of those operations.
    Example
      MNK = (M ** R^2) ** R^2
      isFreeDGModule MNK
  SeeAlso
    "Module-like operations on DG modules"
    freeDGModule
    (isWellDefined, DGModule)
///

doc ///
  Key
    (isZero, DGModule)
    (isZero, DGSubmodule)
    (isZero, DGQuotientModule)
    isZero
  Headline
    Does the DG object have no natural generators?
  Usage
    b = isZero X
  Inputs
    X:
      A DGModule, DGSubmodule, DGQuotientModule, or @ TO DGIdeal @.
  Outputs
    b:Boolean
      True iff the natural presentation is zero.  Concretely:
      @ TT "numgens M.natural == 0" @ for a DGModule, no inclusion columns
      (or a zero inclusion matrix) for a DGSubmodule, and
      @ TT "Q.natural == 0" @ for a DGQuotientModule.  For a DGIdeal,
      see @ TO (isZero, DGIdeal) @.
  Description
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M0 = freeDGModule(A, {})
      isZero M0
      M = freeDGModule(A, {0})
      not isZero M
    Text
      A zero DGSubmodule can be built explicitly via a zero inclusion matrix.
    Example
      Anat = A.natural
      Z = dgSubmodule(M, map(M.natural, (Anat)^0, 0))
      isZero Z
    Text
      Quotienting a DGModule by its whole ambient yields a zero
      DGQuotientModule.
    Example
      Full = dgSubmodule(M, id_(M.natural))
      Q = M / Full
      isZero Q
  SeeAlso
    "Module-like operations on DG modules"
    (numgens, DGModule)
    (isZero, DGIdeal)
///

doc ///
  Key
    (super, DGSubmodule)
    (super, DGQuotientModule)
    (cover, DGQuotientModule)
    (relations, DGQuotientModule)
  Headline
    Ambient DGModule, cover, and relation matrix of a DG sub- or quotient module
  Description
    Text
      These mirror the analogous M2 Module accessors.
    Text
      @ TT "super S" @ (for a DGSubmodule S) and @ TT "super Q, cover Q" @
      (for a DGQuotientModule Q) all return the ambient DGModule M from
      which the sub- or quotient was built.
    Text
      @ TT "relations Q" @ returns the inclusion matrix of the killed
      submodule, encoding the cokernel presentation of Q.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Anat = A.natural
      S = dgSubmodule(M, matrix {{x_Anat, y_Anat}})
      super S === M
      Q = M / S
      super Q === M
      cover Q === M
      numcols relations Q == 2
  SeeAlso
    "Module-like operations on DG modules"
    DGSubmodule
    DGQuotientModule
///

doc ///
  Key
    "Operations on DG Submodules"
  Headline
    Sum, intersection, equality, and containment of DG submodules
  Description
    Text
      DGSubmodules of a common ambient DGModule M form a lattice under sum,
      intersection, and inclusion — each operation preserves d-closure by
      elementary arguments paralleling the @ TO "Operations on DG Ideals" @
      story:
    Text
      @BOLD "Sum"@: @ TT "d(s + t) = d(s) + d(t) \\in S + T" @ when
      @ TT "s \\in S" @ and @ TT "t \\in T" @.
    Text
      @BOLD "Intersection"@: if @ TT "x \\in S \\cap T" @ then
      @ TT "d(x) \\in S" @ (by d-closure of S) and @ TT "d(x) \\in T" @
      (by d-closure of T), so @ TT "d(x) \\in S \\cap T" @.
    Text
      @BOLD "Containment / equality"@: read off directly from the images of
      the inclusions in @ TT "M.natural" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Anat = A.natural
      S = dgSubmodule(M, matrix {{x_Anat}})
      T = dgSubmodule(M, matrix {{y_Anat}})
      ST = S + T
      isWellDefined ST
      isSubset(S, ST) and isSubset(T, ST)
      cap = intersect(S, T)
      isWellDefined cap
      isSubset(cap, S) and isSubset(cap, T)
    Text
      Sum is idempotent (@ TT "S + S == S" @) and intersecting with the
      zero submodule yields the zero submodule.
    Example
      (S + S) == S
      Z = dgSubmodule(M, map(M.natural, (Anat)^0, 0))
      isZero intersect(S, Z)
    Text
      All four operations require the DGSubmodules to share an ambient
      DGModule (via @ TT "===" @ — object identity, not just isomorphism),
      and raise an error otherwise.
  Subnodes
    (symbol +, DGSubmodule, DGSubmodule)
    (intersect, DGSubmodule, DGSubmodule)
    (symbol ==, DGSubmodule, DGSubmodule)
  SeeAlso
    DGSubmodule
    "Operations on DG Ideals"
    "Module-like operations on DG modules"
///

doc ///
  Key
    (symbol +, DGSubmodule, DGSubmodule)
  Headline
    Sum of two DG submodules of a common ambient
  Usage
    U = S + T
  Inputs
    S:DGSubmodule
    T:DGSubmodule
      With @ TT "S.ambient === T.ambient" @.
  Outputs
    U:DGSubmodule
      The DGSubmodule of @ TT "S.ambient" @ whose inclusion matrix is the
      horizontal concatenation of @ TT "S.inclusion.natural" @ and
      @ TT "T.inclusion.natural" @.
  Description
    Text
      Preserves d-closure and contains both summands.  Idempotent:
      @ TT "S + S == S" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Anat = A.natural
      S = dgSubmodule(M, matrix {{x_Anat}})
      T = dgSubmodule(M, matrix {{y_Anat}})
      ST = S + T
      isWellDefined ST
      isSubset(S, ST) and isSubset(T, ST)
      (S + S) == S
  Caveat
    If the two ambients are distinct objects (even if isomorphic), this
    raises an error — the operation is about lattice structure within a
    fixed ambient.
  SeeAlso
    "Operations on DG Submodules"
    (intersect, DGSubmodule, DGSubmodule)
    (isSubset, DGSubmodule, DGSubmodule)
///

doc ///
  Key
    (intersect, DGSubmodule, DGSubmodule)
  Headline
    Intersection of two DG submodules of a common ambient
  Usage
    U = intersect(S, T)
  Inputs
    S:DGSubmodule
    T:DGSubmodule
      With @ TT "S.ambient === T.ambient" @.
  Outputs
    U:DGSubmodule
      The DGSubmodule of @ TT "S.ambient" @ whose image is
      @ TT "image(S.inclusion.natural) \\cap image(T.inclusion.natural)" @.
  Description
    Text
      Preserves d-closure directly.  Intersecting with the zero submodule
      yields the zero submodule; intersecting with the same submodule is
      idempotent (@ TT "intersect(S, S) == S" @ as submodules).
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Anat = A.natural
      S = dgSubmodule(M, matrix {{x_Anat}})
      T = dgSubmodule(M, matrix {{x_Anat * y_Anat}})
      cap = intersect(S, T)
      isWellDefined cap
      isSubset(cap, S) and isSubset(cap, T)
    Text
      Boundary case: intersection with the zero submodule is zero.
    Example
      Z = dgSubmodule(M, map(M.natural, (Anat)^0, 0))
      isZero intersect(S, Z)
  SeeAlso
    "Operations on DG Submodules"
    (symbol +, DGSubmodule, DGSubmodule)
///

doc ///
  Key
    (symbol ==, DGSubmodule, DGSubmodule)
    (isSubset, DGSubmodule, DGSubmodule)
  Headline
    Equality and containment of DG submodules
  Usage
    b = S == T
    b = isSubset(S, T)
  Inputs
    S:DGSubmodule
    T:DGSubmodule
  Outputs
    b:Boolean
  Description
    Text
      Equality requires the ambients to be the same object
      (@ TT "===" @) and the images of the inclusions to agree.
      Containment further requires the DGSubmodules share an ambient and
      tests the image inclusions.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Anat = A.natural
      S = dgSubmodule(M, matrix {{x_Anat}})
      U = dgSubmodule(M, matrix {{x_Anat, y_Anat}})
      isSubset(S, U)
      not isSubset(U, S)
      S == S
    Text
      Two DGSubmodules built from distinct ambient DGModules are never
      equal, even if the ambient modules are abstractly isomorphic.
    Example
      M' = freeDGModule(A, {0})
      S' = dgSubmodule(M', matrix {{x_Anat}})
      not (S == S')
  SeeAlso
    "Operations on DG Submodules"
    (symbol +, DGSubmodule, DGSubmodule)
    (intersect, DGSubmodule, DGSubmodule)
///

doc ///
  Key
    "Image, kernel, and cokernel of DG module maps"
  Headline
    Building DG submodules and DG quotient modules from a DG module map
  Description
    Text
      For a DG module map @ TT "f : M -> N" @ of DG modules over a common
      DG algebra A, the package provides three chain-level constructions:
    Text
      @ TO (image, DGModuleMap) @ — the image of f, built as a
      @ TO DGSubmodule @ of the target N.  A chain map sends cycles to
      cycles and boundaries to boundaries, so the column span of
      @ TT "f.natural" @ is automatically d-closed in @ TT "N.natural" @;
      no d-saturation is needed.
    Text
      @ TO (kernel, DGModuleMap) @ — the kernel of f, built as a
      @ TO DGSubmodule @ of the source M.  Again chain-map-ness gives
      d-closure for free.
    Text
      @ TO (cokernel, DGModuleMap) @ — the cokernel of f, built as a
      @ TO DGQuotientModule @ equal to @ TT "target f / image f" @.
    Text
      These three operations fit together in the usual short exact
      sequence: for any f, we have
      @ TT "rank(source f) == numgens(kernel f) + numgens(image f)" @
      and @ TT "rank(target f) == numgens(image f) + numgens(cokernel f)" @
      whenever the natural modules are free.
    Text
      A genuinely informative example: let
      @ TT "R = k[x, y]/(x^2, y^2)" @ and let
      @ TT "KM = koszulComplexDGM R^1" @ be its Koszul DG module.
      Multiplication by @ TT "x" @ is a degree-0 endomorphism of
      @ TT "KM" @ because @ TT "x" @ is central in the underlying
      ring:
    Example
      R = ZZ/101[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      mx = dgModuleMap(KM, KM, x * id_(KM.natural))
      isWellDefined mx
    Text
      Since @ TT "x^2 = 0" @ in @ TT "R" @, the composition
      @ TT "mx o mx" @ is the zero map, which chain-level is the
      statement @ TT "image mx \\subseteq kernel mx" @.  The three
      constructions expose this directly:
    Example
      imgMx = image mx;
      kerMx = kernel mx;
      cokMx = cokernel mx;
      all({imgMx, kerMx, cokMx}, isWellDefined)
      numcols (inclusion imgMx).natural
      numcols (inclusion kerMx).natural
      isSubset(imgMx, kerMx)
    Text
      The cokernel is the quotient DG module
      @ TT "KM / x \\cdot KM" @; at the natural level it is
      @ TT "KM \\otimes_R R/(x) = k[y]/(y^2) \\otimes_k \\Lambda(T_1, T_2)" @,
      presented by the @ TT "1 x 1" @ matrix @ TT "[x]" @ per
      generator:
    Example
      presentation cokMx.natural
    Text
      The three operations are used throughout the package -- notably
      by @ TO2((homology, DGModuleMap, ZZ), "homology at degree n") @,
      which computes @ TT "kernel(d_n) / image(d_{n+1})" @ via these
      primitives.
    Text
      Finally, each of @ TT "image mx" @, @ TT "kernel mx" @,
      @ TT "cokernel mx" @ is itself a DG module, so
      @ TO (toComplex, DGModule) @ (or its
      @ TO DGSubmodule @ / @ TO DGQuotientModule @ variants) converts
      them into ordinary chain complexes one can hand to
      @ TO "Complexes::Complexes" @.  For the cokernel in particular,
      the underlying complex is @ TT "KM / x \\cdot KM" @ and its
      homology picks up the classes that become new cycles when the
      @ TT "x" @-direction is killed:
    Example
      CcokMx = toComplex cokMx
      apply(0..(maxDegree cokMx), n -> prune HH_n CcokMx)
  Subnodes
    (image, DGModuleMap)
    (kernel, DGModuleMap)
    (cokernel, DGModuleMap)
  SeeAlso
    DGModuleMap
    "Building DG modules, submodules, and quotients"
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    (image, DGModuleMap)
  Headline
    Image of a DG module map as a DG submodule of its target
  Usage
    S = image f
  Inputs
    f:DGModuleMap
      A morphism @ TT "M -> N" @ of DG modules over a common DG algebra.
  Outputs
    S:DGSubmodule
      The DG submodule of @ TT "N = target f" @ whose column span in
      @ TT "N.natural" @ is the image of @ TT "f.natural" @.  Zero
      columns of @ TT "f.natural" @ are dropped, so the number of
      generators of the result reflects the rank of the image rather
      than the width of the presentation.
  Description
    Text
      Since f is a chain map, the column span of @ TT "f.natural" @ is
      already d-closed in @ TT "N.natural" @; @ TO dgSubmodule @ is
      called without a d-saturation loop.
    Text
      A substantive example: over
      @ TT "R = k[x, y]/(x^2, y^2)" @ take the Koszul DG module
      @ TT "KM = koszulComplexDGM R^1" @ and the endomorphism
      "multiplication by @ TT "x" @."  Its image is a proper
      DG submodule -- it cannot be all of @ TT "KM" @ because
      @ TT "x" @ annihilates the class of @ TT "1 \\in R" @ modulo
      its own image (since @ TT "x^2 = 0" @):
    Example
      R = ZZ/101[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      mx = dgModuleMap(KM, KM, x * id_(KM.natural))
      imgMx = image mx
      isWellDefined imgMx
      numcols (inclusion imgMx).natural
      isSubset(imgMx, dgSubmodule(KM, id_(KM.natural)))
    Text
      The image of the inclusion of a DG submodule
      @ TT "S \\subset M" @ recovers @ TT "S" @ itself:
    Example
      Sfull = dgSubmodule(KM, id_(KM.natural));
      (image (inclusion Sfull)) == Sfull
  SeeAlso
    "Image, kernel, and cokernel of DG module maps"
    (kernel, DGModuleMap)
    (cokernel, DGModuleMap)
    DGSubmodule
///

doc ///
  Key
    (kernel, DGModuleMap)
  Headline
    Kernel of a DG module map as a DG submodule of its source
  Usage
    S = kernel f
  Inputs
    f:DGModuleMap
      A morphism @ TT "M -> N" @ of DG modules over a common DG algebra.
  Outputs
    S:DGSubmodule
      The DG submodule of @ TT "M = source f" @ generated by the kernel
      of @ TT "f.natural" @ as a module over @ TT "M.dgAlgebra.natural" @.
  Description
    Text
      A chain map sends cycles to cycles, so the kernel of
      @ TT "f.natural" @ at the module level is already d-closed.  The
      constructor takes the natural-level kernel generators and wraps
      them with @ TO dgSubmodule @.
    Text
      Mathematically interesting example: over
      @ TT "R = k[x, y]/(x^2, y^2)" @ the endomorphism
      "multiplication by @ TT "x" @" of the Koszul DG module
      @ TT "KM" @ squares to zero, so its kernel contains its image:
    Example
      R = ZZ/101[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      mx = dgModuleMap(KM, KM, x * id_(KM.natural))
      kerMx = kernel mx
      imgMx = image mx
      isWellDefined kerMx
      numcols (inclusion kerMx).natural
      isSubset(imgMx, kerMx)
    Text
      The inclusion of a DG submodule is injective, so its kernel is
      zero:
    Example
      Sfull = dgSubmodule(KM, id_(KM.natural));
      isZero kernel (inclusion Sfull)
  SeeAlso
    "Image, kernel, and cokernel of DG module maps"
    (image, DGModuleMap)
    (cokernel, DGModuleMap)
    DGSubmodule
///

doc ///
  Key
    (cokernel, DGModuleMap)
  Headline
    Cokernel of a DG module map as a DG quotient module of its target
  Usage
    Q = cokernel f
  Inputs
    f:DGModuleMap
      A morphism @ TT "M -> N" @ of DG modules over a common DG algebra.
  Outputs
    Q:DGQuotientModule
      The DG quotient module @ TT "N / image(f)" @ with the induced
      differential.
  Description
    Text
      Equivalently, this is @ TT "dgQuotientModule(target f, image f)" @.
      The @ TT "coker" @ spelling is an alias for @ TT "cokernel" @ and
      invokes the same method.
    Text
      A nontrivial example: over @ TT "R = k[x, y]/(x^2, y^2)" @,
      multiplication by @ TT "x" @ on the Koszul DG module
      @ TT "KM" @ has cokernel equal to @ TT "KM / x \\cdot KM" @,
      which at the natural level is @ TT "KM \\otimes_R R/(x)" @ --
      the base change of @ TT "KM" @ mod @ TT "x" @:
    Example
      R = ZZ/101[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      mx = dgModuleMap(KM, KM, x * id_(KM.natural))
      Q = cokernel mx
      isWellDefined Q
      ambient Q === KM
      presentation Q.natural
    Text
      The induced differential on @ TT "Q" @ is well-defined because
      @ TT "mx" @ commutes with the differential, and it can be
      realized as an ordinary chain complex via
      @ TO (toComplex, DGQuotientModule) @:
    Example
      C = toComplex Q
      apply(0..(maxDegree Q), n -> prune HH_n C)
    Text
      For a DG submodule inclusion @ TT "S \\hookrightarrow M" @, the
      cokernel is exactly the DG quotient module @ TT "M / S" @:
    Example
      Sfull = dgSubmodule(KM, id_(KM.natural));
      isZero cokernel (inclusion Sfull)
  SeeAlso
    "Image, kernel, and cokernel of DG module maps"
    (image, DGModuleMap)
    (kernel, DGModuleMap)
    DGQuotientModule
    (symbol /, DGModule, DGSubmodule)
///

doc ///
  Key
    "Homology of DG modules and DG module maps"
  Headline
    Computing H_n and H_* for DG modules, DG submodules, DG quotient modules, and DG module maps
  Description
    Text
      For a DG module @ TT "M" @ over a DG algebra @ TT "A" @, the package
      supports two parallel views of homology:
    Text
      @ TO (homology, ZZ, DGModule) @ returns a single degree
      @ TT "H_n(M)" @ as a module over the base ring @ TT "A.ring" @.
      This is the natural per-degree shape and matches the convention of
      @ TO (homology, ZZ, DGAlgebra) @.
    Text
      @ TO (homology, DGModule) @ (equivalently @ TO homologyModule @)
      returns the full graded object @ TT "H_*(M)" @ assembled as a
      module over the homology algebra @ TT "HH(A)" @, with the
      @ TT "HH(A)" @-action coming from multiplication by cycle classes.
    Text
      Maps @ TT "f : M -> N" @ of DG modules induce maps on homology at
      each level: @ TO (homology, DGModuleMap, ZZ) @ returns the induced
      map @ TT "H_n(M) -> H_n(N)" @ as a map of @ TT "A.ring" @-modules,
      and @ TO (homology, DGModuleMap) @ assembles those pieces into a
      map of @ TT "HH(A)" @-modules.
    Text
      For quotients, @ TO (homology, ZZ, DGQuotientModule) @ and
      @ TO (homology, DGQuotientModule) @ (equivalently
      @ TO (homologyModule, DGQuotientModule) @) work the same way,
      using @ TO toComplex @ to reduce to an
      underlying complex.  A @ TO DGSubmodule @ is a @ TO DGModule @,
      so its homology is computed by the DG module methods directly.
    Text
      To extract an explicit representative of a given homology class,
      use @ TO (homologyClass, DGModule, Vector) @; given a cycle in
      @ TT "M.natural" @ it returns the corresponding element of
      @ TT "prune homology(n, M)" @.
    Example
      R = QQ[x, y]/ideal(x^2, y^2)
      A = koszulComplexDGA R
      k = R^1 / ideal(x, y)
      M = minimalSemifreeResolution(A, k, EndDegree => 2)
      H0 = prune homology(0, M)
      numgens H0 == 1
      idM = identityDGModuleMap M
      h0 = homology(idM, 0)
      h0 == id_(source h0)
    Text
      The identity on @ TT "M" @ induces the identity on every
      @ TT "H_n(M)" @, a baseline sanity check that each of the pieces
      fits together.
  Subnodes
    (homology, ZZ, DGModule)
    (homology, DGModule)
    (homology, DGModuleMap, ZZ)
    (homology, ZZ, DGQuotientModule)
    (homology, DGQuotientModule)
    (homologyClass, DGModule, Vector)
  SeeAlso
    (homology, DGModuleMap)
    (homology, ZZ, DGAlgebra)
    (homology, DGAlgebra)
    homologyModule
    homologyClass
///

doc ///
  Key
    (homology, ZZ, DGModule)
  Headline
    The degree-n homology of a DG module as a module over the base ring
  Usage
    H = homology(n, M)
  Inputs
    n:ZZ
      A homological degree.
    M:DGModule
      A DG module over a DG algebra @ TT "A" @.
  Outputs
    H:Module
      The module @ TT "H_n(M) = ker(d_n) / image(d_{n+1})" @, presented
      as a module over @ TT "A.ring" @.
  Description
    Text
      This is the per-degree homology: the differential
      @ TT "d_n : M_n -> M_{n-1}" @ is built via
      @ TT "moduleDifferential" @, and @ TT "H_n(M)" @ is obtained by
      applying the built-in M2 @ TT "homology" @ method to the adjacent
      pair @ TT "(d_n, d_{n+1})" @.  The resulting module lives over
      @ TT "A.ring" @; use @ TO (prune, Module) @ to minimize the
      presentation.
    Text
      Because @ TO DGSubmodule @ is a subtype of @ TO DGModule @, this
      method also handles DG submodules directly.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      prune homology(0, KM)
      prune homology(1, KM) == 0
      prune homology(2, KM) == 0
    Text
      For the Koszul complex on a regular sequence, only
      @ TT "H_0 = R/(x,y,z)" @ is nonzero; this recovers the standard
      fact.
    Example
      R2 = ZZ/101[x]
      A2 = koszulComplexDGA R2
      M = freeDGModule(A2, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      numgens prune homology(0, M)
      numgens prune homology(1, M)
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homology, DGModule)
    (homology, DGModuleMap, ZZ)
    (homology, ZZ, DGQuotientModule)
    (homology, ZZ, DGAlgebra)
    moduleDifferential
///

doc ///
  Key
    (homology, DGModule)
    (homologyModule, DGModule)
  Headline
    The graded homology of a DG module as a module over HH(A)
  Usage
    HM = homology M
    HM = homologyModule M
  Inputs
    M:DGModule
      A DG module over a DG algebra @ TT "A" @.
  Outputs
    HM:Module
      The graded homology @ TT "H_*(M)" @ packaged as a module over the
      homology algebra @ TT "HH(A)" @.  The @ TT "HH(A)" @-action comes
      from multiplication by cycle classes of @ TT "A" @ on @ TT "M" @.
  Description
    Text
      The two names @ TT "homology M" @ and @ TT "homologyModule M" @
      produce the same object and are provided as aliases, matching the
      analogous convention for DG algebras in which
      @ TO (homology, DGAlgebra) @ returns @ TO homologyAlgebra @.
    Text
      For per-degree homology as an @ TT "A.ring" @-module, use
      @ TO (homology, ZZ, DGModule) @.
    Text
      A fast path is taken when @ TT "M" @ came from
      @ TO koszulComplexDGM @ and has vanishing generator differentials:
      in that case the result is computed directly from
      @ TT "M.module" @ via @ TO (homologyModule, DGAlgebra, Module) @.
      Otherwise the method builds the @ TT "HH(A)" @-module presentation
      degree by degree, using the action of each cycle class of
      @ TT "A" @ on @ TT "M" @.
    Example
      R = QQ[x, y]/ideal(x^2, y^2)
      A = koszulComplexDGA R
      k = R^1 / ideal(x, y)
      M = minimalSemifreeResolution(A, k, EndDegree => 2)
      HM = homology M
      ring HM === HH A
      HM' = homologyModule M
      HM == HM'
    Text
      The ring of the result is @ TT "HH(A)" @, the homology algebra of
      the underlying DG algebra.
  Caveat
    The general path currently requires @ TT "M.natural" @ to be a free
    @ TT "A.natural" @-module (or @ TT "M" @ to be a
    @ TO koszulComplexDGM @ with zero generator differentials).
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homology, ZZ, DGModule)
    (homology, DGAlgebra)
    homologyModule
    homologyAlgebra
///

doc ///
  Key
    (homology, DGModuleMap, ZZ)
  Headline
    The induced map on degree-n homology of a DG module map
  Usage
    h = homology(f, n)
  Inputs
    f:DGModuleMap
      A map @ TT "M -> N" @ of DG modules over a common DG algebra
      @ TT "A" @.
    n:ZZ
      A homological degree.
  Outputs
    h:Matrix
      The induced map @ TT "H_n(f) : H_n(M) -> H_n(N)" @, presented as
      a morphism of @ TT "A.ring" @-modules.
  Description
    Text
      Computed via @ TO inducedMap @ from the chain map
      @ TT "toComplexMap(f, n)" @ applied to the pair of per-degree
      homology modules.  Since a chain map sends cycles to cycles and
      boundaries to boundaries, the induced map is well defined.
    Text
      The identity DG module map induces the identity on each
      @ TT "H_n" @; the zero DG module map induces the zero map on each
      @ TT "H_n" @.  Composition respects homology:
      @ TT "homology(g * f, n) == homology(g, n) * homology(f, n)" @.
    Example
      R = QQ[x, y]/ideal(x^2, y^2)
      A = koszulComplexDGA R
      k = R^1 / ideal(x, y)
      M = minimalSemifreeResolution(A, k, EndDegree => 3)
      idM = identityDGModuleMap M
      zM = zeroDGModuleMap(M, M)
      h0id = homology(idM, 0)
      h0id == id_(source h0id)
      h0z = homology(zM, 0)
      h0z == map(target h0z, source h0z, 0)
    Text
      Multiplication by @ TT "x" @ on a minimal semifree resolution of
      @ TT "k = R/(x, y)" @ induces the zero map on @ TT "H_0 = k" @,
      because @ TT "x" @ kills @ TT "k" @.
    Example
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      xMap = dgModuleMap(M, M, apply(natGens, g -> x * g))
      h0x = homology(xMap, 0)
      h0x == map(target h0x, source h0x, 0)
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homology, DGModuleMap)
    (homology, ZZ, DGModule)
    toComplexMap
///

doc ///
  Key
    (homology, ZZ, DGQuotientModule)
  Headline
    The degree-n homology of a DG quotient module
  Usage
    H = homology(n, Q)
  Inputs
    n:ZZ
      A homological degree.
    Q:DGQuotientModule
      A DG quotient module @ TT "M / S" @ over a DG algebra @ TT "A" @.
  Outputs
    H:Module
      The module @ TT "H_n(Q)" @, presented as a module over
      @ TT "A.ring" @.
  Description
    Text
      Implemented via @ TT "HH_n (toComplex Q)" @.  Since the underlying
      natural module is a cokernel rather than a free module, the
      differentials of @ TT "Q" @ are expressed through
      @ TO toComplex @ and the standard M2
      @ TT "HH_n" @ is applied.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      S = dgSubmodule(M, (id_(M.natural))_{1})
      Q = M / S
      h0 = prune homology(0, Q)
      h1 = prune homology(1, Q)
      h2 = prune homology(2, Q)
      numgens h0 == 1
      numgens h1 == 1
      h2 == 0
    Text
      With @ TT "S" @ acyclic and @ TT "H_0(M) = R/x" @, the quotient
      @ TT "Q = M/S" @ has @ TT "H_0(Q) = R/x" @ and
      @ TT "H_1(Q) = R/x" @, both equal to the residue field
      @ TT "k = ZZ/101" @ (one generator each, torsion), and
      @ TT "H_2(Q) = 0" @.
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homology, DGQuotientModule)
    (homology, ZZ, DGModule)
    (toComplex, DGQuotientModule)
///

doc ///
  Key
    (homology, DGQuotientModule)
    (homologyModule, DGQuotientModule)
  Headline
    The graded homology of a DG quotient module
  Usage
    HQ = homology Q
    HQ = homologyModule Q
  Inputs
    Q:DGQuotientModule
      A DG quotient module @ TT "M / S" @ over a DG algebra @ TT "A" @.
  Outputs
    HQ:Module
      The graded homology @ TT "H_*(Q)" @ packaged as a module over
      @ TT "HH(A)" @.
  Description
    Text
      Implemented by taking @ TO toComplex @ and
      assembling the per-degree homologies
      @ TT "HH_i(toComplex Q)" @ into a direct sum, then tensoring each
      piece with @ TT "HH(A)" @ along @ TT "A.ring -> HH(A)" @.
      The two names @ TT "homology Q" @ and @ TT "homologyModule Q" @
      are aliases, matching @ TO (homology, DGModule) @.
    Text
      Because @ TT "Q.natural" @ is a cokernel rather than a free
      @ TT "A.natural" @-module, the general cycle-action construction
      used for @ TO (homologyModule, DGModule) @ does not apply; the
      result captures the R-module structure and the @ TT "HH(A)" @
      action coming from @ TT "A.ring -> HH(A)" @ only.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      S = dgSubmodule(M, (id_(M.natural))_{1})
      Q = M / S
      HQ = homology Q
      ring HQ === HH A
      HQ' = homologyModule Q
      HQ == HQ'
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homology, ZZ, DGQuotientModule)
    (homology, DGModule)
    (toComplex, DGQuotientModule)
    homologyModule
///

doc ///
  Key
    (homologyClass, DGModule, Vector)
  Headline
    The homology class of a cycle in a DG module
  Usage
    h = homologyClass(M, z)
  Inputs
    M:DGModule
      A free DG module over a DG algebra @ TT "A" @.
    z:Vector
      A cycle in @ TT "M.natural" @; that is, an element @ TT "z" @ of
      homological degree @ TT "d" @ with @ TT "diff(M, z) == 0" @.
  Outputs
    h:
      The corresponding element of @ TT "prune homology(d, M)" @.
      Boundaries go to zero; the zero vector returns
      @ TT "0_(prune homology(0, M))" @ by convention.
  Description
    Text
      This is the DG module analogue of
      @ TO (homologyClass, DGAlgebra, RingElement) @.  It expresses the
      cycle @ TT "z" @ in the basis of @ TT "prune homology(d, M)" @ by
      solving @ TT "zCol = pruneMat * hCoef + boundaryMat * t" @, where
      @ TT "pruneMat" @ is the pruning map of @ TT "H_d(M)" @ and
      @ TT "boundaryMat" @ is the degree-@ TT "d+1" @ differential.
    Text
      An input that is not a cycle raises an error.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      z = natGens#0
      H = prune homology(0, M)
      h = homologyClass(M, z)
      h != 0_H
    Text
      On a boundary, @ TT "homologyClass" @ returns zero.
    Example
      b = x * natGens#0
      hb = homologyClass(M, b)
      hb == 0_(prune homology(0, M))
  Caveat
    Requires @ TT "M" @ to be a free DG module.
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homologyClass, DGAlgebra, RingElement)
    (homology, ZZ, DGModule)
///

doc ///
  Key
    "Pruning DG modules, submodules, quotients, and maps"
  Headline
    Minimizing presentations of DG types via prune and minimalPresentation
  Description
    Text
      For each DG type @ TT "T" @ (DG algebra, DG module, DG submodule,
      DG quotient module, DG ideal, DG algebra map, DG module map), the
      package provides @ TT "prune" @ and its synonym
      @ TT "minimalPresentation" @.  The result is an object of the
      same type whose underlying presentation has had redundancy
      removed, where possible.
    Text
      Every call to @ TT "prune" @ caches a @ TT "pruningMap" @ in the
      @ TT "cache" @ table of the pruned output, encoding the canonical
      comparison map @ TT "(pruned) -> (original)" @.  For types where
      pruning does nothing (see below), the cached pruningMap is the
      identity; for types where it does something, the pruningMap is a
      well-defined @ TO DGModuleMap @ (or @ TO DGAlgebraMap @) whose
      source is the pruned object and whose target is the original.
      This mirrors the convention used by
      @ TT "prune" @ applied to @ TT "Complex" @ objects from the Complexes package for @ TT "Complex" @ objects.
    Text
      The two active cases are:
    Text
      @ TO (prune, DGSubmodule) @ trims the inclusion matrix to a
      minimal generating set of its column span.  When the inclusion
      matrix is already minimal, @ TT "prune" @ is the identity.
    Text
      @ TO (prune, DGQuotientModule) @ first prunes the relations
      submodule and then additionally prunes
      @ TT "toComplex Qp" @ at the complex level, caching the result so
      that @ TT "toComplex Qp" @ returns a minimally presented complex.
    Text
      The no-op cases, installed so that pruning always commutes with
      every constructor, are @ TO (prune, DGModule) @,
      @ TO (prune, DGAlgebra) @, @ TO (prune, DGAlgebraMap) @,
      @ TO (prune, DGModuleMap) @, and (already documented)
      @ TO (prune, DGIdeal) @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{1_Anat, x_Anat, y_Anat}})
      Sp = prune S
      numcols (inclusion S).natural
      numcols (inclusion Sp).natural
      image (inclusion S).natural == image (inclusion Sp).natural
    Text
      The image submodule of @ TT "M.natural" @ is unchanged; the
      presentation of @ TT "S" @ is reduced from three generators to
      one.
  Subnodes
    (prune, DGSubmodule)
    (prune, DGQuotientModule)
    (prune, DGModule)
    (prune, DGAlgebra)
    (prune, DGAlgebraMap)
    (prune, DGModuleMap)
  SeeAlso
    (prune, DGIdeal)
    (mingens, DGIdeal)
///

doc ///
  Key
    (prune, DGSubmodule)
    (minimalPresentation, DGSubmodule)
  Headline
    Trim a DG submodule to a minimal generating set of its inclusion
  Usage
    Sp = prune S
    Sp = minimalPresentation S
  Inputs
    S:DGSubmodule
  Outputs
    Sp:DGSubmodule
      A DG submodule of the same ambient DG module whose inclusion
      matrix has had @ TT "A.natural" @-linearly redundant columns
      removed.
  Description
    Text
      @ TT "prune S" @ computes
      @ TT "mingens image S.inclusion.natural" @ and builds a new
      DG submodule from that reduced inclusion matrix.  The
      @ TT "A.natural" @-span of the new inclusion matrix equals the
      original: @ TT "image Sp.inclusion.natural == image S.inclusion.natural" @.
    Text
      A @ TT "pruningMap" @ is cached in @ TT "Sp.cache" @ as a
      @ TO DGModuleMap @ from @ TT "Sp" @ to @ TT "S" @; when the
      inclusion matrix is already minimal this is the identity on
      @ TT "S" @ (and @ TT "prune S === S" @).
    Text
      Well-definedness (d-closure) and the underlying natural-level
      image are preserved by pruning.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{1_Anat, x_Anat, y_Anat}})
      Sp = prune S
      numcols (inclusion Sp).natural
      numcols (inclusion S).natural
      image (inclusion S).natural == image (inclusion Sp).natural
      isWellDefined Sp
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @ on
      DG submodules.
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGQuotientModule)
    (prune, DGModule)
    dgSubmodule
///

doc ///
  Key
    (prune, DGQuotientModule)
    (minimalPresentation, DGQuotientModule)
  Headline
    Minimize the presentation of a DG quotient module
  Usage
    Qp = prune Q
    Qp = minimalPresentation Q
  Inputs
    Q:DGQuotientModule
  Outputs
    Qp:DGQuotientModule
      A DG quotient module of the same ambient DG module whose
      relations submodule has been pruned, and whose
      @ TT "toComplex Qp" @ has additionally been pruned at the
      complex level.
  Description
    Text
      @ TT "prune Q" @ proceeds in two steps.  First, it prunes the
      relations submodule @ TT "Q.subDGModule" @ using
      @ TO (prune, DGSubmodule) @; this trims redundant columns from
      the inclusion matrix.  If the inclusion was already minimal,
      @ TT "Qp === Q" @.
    Text
      Second, it computes @ TT "prune (toComplex Qp)" @ at the
      complex level and caches the result in @ TT "Qp.cache" @ under
      the symbol @ TT "prunedComplex" @.  After this, calling
      @ TT "toComplex Qp" @ returns a minimally presented complex and
      pruning commutes with @ TT "toComplex" @: the complex-level
      prune of @ TT "toComplex Qp" @ is idempotent.
    Text
      A @ TT "pruningMap" @ from @ TT "Qp" @ to @ TT "Q" @ is cached
      in @ TT "Qp.cache" @ as a @ TO DGModuleMap @ induced by the
      identity on @ TT "M.natural" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{1_Anat, x_Anat, y_Anat}})
      Q = M / S
      Qp = prune Q
      image (inclusion Qp.subDGModule).natural == image (inclusion S).natural
    Text
      If the cokernel is secretly zero, pruning collapses
      @ TT "toComplex Qp" @ to the zero complex, matching
      @ TT "prune" @ applied to @ TT "Complex" @ objects from the Complexes package on @ TT "Complex" @.
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @ on
      DG quotient modules.
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGSubmodule)
    dgQuotientModule
    (symbol /, DGModule, DGSubmodule)
///

doc ///
  Key
    (prune, DGModule)
    (minimalPresentation, DGModule)
  Headline
    Pruning a DG module is the identity
  Usage
    Mp = prune M
    Mp = minimalPresentation M
  Inputs
    M:DGModule
  Outputs
    Mp:DGModule
      The same DG module @ TT "M" @.  A @ TT "pruningMap" @ equal to
      the identity DG module map on @ TT "M" @ is cached in
      @ TT "M.cache" @.
  Description
    Text
      DG modules as stored in this package (built from
      @ TO freeDGModule @ or @ TO koszulComplexDGM @) carry no
      auxiliary presentation data that @ TT "prune" @ could minimize;
      the method is installed as the identity so that calling code can
      use @ TT "prune" @ uniformly across DG types.
    Text
      Calling @ TT "prune M" @ has the side effect of caching an
      identity @ TO DGModuleMap @ under the symbol @ TT "pruningMap" @
      in @ TT "M.cache" @.
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      (prune M) === M
      (minimalPresentation M) === M
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGSubmodule)
    (prune, DGQuotientModule)
///

doc ///
  Key
    (prune, DGAlgebra)
    (minimalPresentation, DGAlgebra)
  Headline
    Pruning a DG algebra is the identity
  Usage
    Ap = prune A
    Ap = minimalPresentation A
  Inputs
    A:DGAlgebra
  Outputs
    Ap:DGAlgebra
      The same DG algebra @ TT "A" @.
  Description
    Text
      DG algebras as stored in this package carry a fixed underlying
      graded algebra and differential; there is no redundancy for
      @ TT "prune" @ to eliminate.  The method is installed as the
      identity so that pruning commutes uniformly with every
      constructor.
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      (prune A) === A
      (minimalPresentation A) === A
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGIdeal)
///

doc ///
  Key
    (prune, DGAlgebraMap)
    (minimalPresentation, DGAlgebraMap)
  Headline
    Pruning a DG algebra map is the identity
  Usage
    fp = prune f
    fp = minimalPresentation f
  Inputs
    f:DGAlgebraMap
  Outputs
    fp:DGAlgebraMap
      The same DG algebra map @ TT "f" @.
  Description
    Text
      Since @ TO (prune, DGAlgebra) @ is the identity, and a
      @ TO DGAlgebraMap @ is stored as a ring map between the
      underlying graded algebras of its source and target, there is
      nothing for @ TT "prune" @ to minimize.  The method is installed
      as the identity so that pruning commutes uniformly.
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      phi = identityDGAlgebraMap A
      (prune phi) === phi
      (minimalPresentation phi) === phi
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGAlgebra)
    DGAlgebraMap
///

doc ///
  Key
    (prune, DGModuleMap)
    (minimalPresentation, DGModuleMap)
  Headline
    Pruning a DG module map is the identity
  Usage
    fp = prune f
    fp = minimalPresentation f
  Inputs
    f:DGModuleMap
  Outputs
    fp:DGModuleMap
      The same DG module map @ TT "f" @.
  Description
    Text
      Since @ TO (prune, DGModule) @ is the identity, and a
      @ TO DGModuleMap @ is stored as an @ TT "A.natural" @-linear
      matrix between the naturals of its source and target, there is
      nothing for @ TT "prune" @ to minimize.  The method is installed
      as the identity so that pruning commutes uniformly.
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      idM = identityDGModuleMap M
      (prune idM) === idM
      (minimalPresentation idM) === idM
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGModule)
    DGModuleMap
///

doc ///
  Key
    "Well-definedness, acyclicity, and quasi-isomorphism"
  Headline
    Predicates that check DG-theoretic conditions on DG types
  Description
    Text
      The package exposes three families of predicates for checking
      conditions that distinguish DG objects from their underlying
      graded / module-theoretic data.
    Text
      @ TO isWellDefined @ — verifies that an object satisfies the DG
      axioms appropriate to its type.  For a @ TO DGAlgebra @ or
      @ TO DGModule @, the central check is that the stored
      differential squares to zero on every generator.  For a
      @ TO DGSubmodule @ or @ TO DGQuotientModule @, the check is
      d-closure of the defining submodule; for a @ TO DGAlgebraMap @
      or @ TO DGModuleMap @, the check is the chain-map condition
      @ TT "d \\circ f = f \\circ d" @ on every generator.
    Text
      @ TO isAcyclic @ — returns true when a DG algebra or DG module
      has @ TT "H_i = 0" @ for all @ TT "i >= 1" @ up to a finite
      bound.  For objects with infinite hom-degree the user must
      supply @ TO EndDegree @.
    Text
      @ TO isQuasiIsomorphism @ — returns true when a DG algebra map
      or DG module map induces an isomorphism on @ TT "H_*" @.  The
      underlying check is performed on @ TO toComplexMap @ and
      inherits the @ TT "Concentration" @ option from the Complexes
      package.
    Text
      All three predicates accept the option @ TO EndDegree @ where
      applicable; for finite-degree objects the default bound is
      @ TT "maxDegree" @ of the object and no option is required.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      isWellDefined A
      isAcyclic A
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{1_Anat}})
      Q = M / S
      isWellDefined S
      isWellDefined Q
      phi = identityDGAlgebraMap A
      isWellDefined phi
      isQuasiIsomorphism phi
    Text
      For a regular polynomial ring the Koszul DG algebra is acyclic
      and its identity map is a quasi-isomorphism.  Both
      @ TT "S" @ and @ TT "Q = M / S" @ are well-defined because
      @ TT "S" @ is d-closed.
  Subnodes
    (isWellDefined, DGAlgebra)
    (isWellDefined, DGModule)
    (isWellDefined, DGSubmodule)
    (isWellDefined, DGQuotientModule)
    (isWellDefined, DGModuleMap)
    (isAcyclic, DGModule)
    (isAcyclic, DGQuotientModule)
    (isQuasiIsomorphism, DGAlgebraMap)
    (annihilator, DGSubmodule)
    (annihilator, DGQuotientModule)
  SeeAlso
    (isWellDefined, DGIdeal)
    (isAcyclic, DGAlgebra)
    (isQuasiIsomorphism, DGModuleMap)
///

doc ///
  Key
    (isWellDefined, DGAlgebra)
  Headline
    Check that a DG algebra has correct structure and a differential that squares to zero
  Usage
    b = isWellDefined A
  Inputs
    A:DGAlgebra
  Outputs
    b:Boolean
      True when @ TT "A" @ has all expected structural keys with the
      correct types, the base ring of @ TT "A.natural" @ is compatible
      with @ TT "A.ring" @, the differential lowers homological
      degree by one on every generator, and @ TT "d^2 = 0" @ holds on
      every generator of @ TT "A.natural" @.
  Description
    Text
      Checks three conditions in order.  First, a structural
      validation: the DG algebra has keys @ TT "ring" @,
      @ TT "natural" @, @ TT "diff" @, and @ TT "Degrees" @ with the
      right types, and the length of @ TT "A.Degrees" @ equals the
      number of generators of @ TT "A.natural" @.  Second, every
      generator @ TT "g" @ of @ TT "A.natural" @ has
      @ TT "d(g)" @ of homological degree @ TT "|g| - 1" @.  Third,
      @ TT "d^2(g) = 0" @ on every generator — Leibniz then extends
      this to all of @ TT "A.natural" @.
    Text
      When @ TT "debugLevel > 0" @ the routine prints a diagnostic
      line for each failing check.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      isWellDefined A
    Text
      A DG algebra with @ TT "A.diff == {}" @ (no differential set)
      passes the check trivially — the second and third conditions
      apply only to generators with a stored differential.
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    DGAlgebra
    setDiff
    (isWellDefined, DGIdeal)
    (isAcyclic, DGAlgebra)
///

doc ///
  Key
    (isWellDefined, DGModule)
  Headline
    Check that a DG module has correct structure and that its differential squares to zero
  Usage
    b = isWellDefined M
  Inputs
    M:DGModule
  Outputs
    b:Boolean
      True when @ TT "M" @ has all expected structural keys, its base
      DG algebra @ TT "M.dgAlgebra" @ is itself well-defined, the
      ring of @ TT "M" @ matches that of its DG algebra, each stored
      differential is a module element of @ TT "M.natural" @, and
      @ TT "d_M^2 = 0" @ on every generator.
  Description
    Text
      Performs a structural check (required keys and list-length
      agreement @ TT "#M.diff == #M.Degrees" @), then verifies that
      @ TT "M.dgAlgebra" @ is a well-defined DG algebra, checks that
      each entry of @ TT "M.diff" @ is a vector or matrix over
      @ TT "M.natural" @, and finally calls the internal
      @ TT "isWellDefinedDifferential" @ routine to verify
      @ TT "d_M^2 = 0" @ up to the natural bound.
    Text
      Diagnostic messages are emitted when @ TT "debugLevel > 0" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      isWellDefined M
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    DGModule
    freeDGModule
    setDiff
    (isWellDefined, DGSubmodule)
    (isWellDefined, DGModuleMap)
///

doc ///
  Key
    (isWellDefined, DGSubmodule)
  Headline
    Check that a DG submodule is d-closed in its ambient DG module
  Usage
    b = isWellDefined S
  Inputs
    S:DGSubmodule
  Outputs
    b:Boolean
      True when @ TT "S" @ has all expected structural keys, its
      ambient is a well-defined DG module, and the column span of
      @ TT "S.inclusion.natural" @ in @ TT "S.ambient.natural" @ is
      stable under @ TT "d_{S.ambient}" @.
  Description
    Text
      After a structural key check, verifies that
      @ TT "S.ambient" @ is a well-defined DG module, then checks
      d-closure: for every column @ TT "v" @ of the inclusion matrix,
      @ TT "d_M(v)" @ lies in the column span of the inclusion
      matrix modulo further columns.  This is the one nontrivial
      condition that distinguishes a DG submodule from a general
      A.natural-submodule of @ TT "M.natural" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      S = dgSubmodule(M, (id_(M.natural))_{1})
      isWellDefined S
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    DGSubmodule
    dgSubmodule
    (isWellDefined, DGModule)
    (isWellDefined, DGQuotientModule)
///

doc ///
  Key
    (isWellDefined, DGQuotientModule)
  Headline
    Check that a DG quotient module has a well-defined differential
  Usage
    b = isWellDefined Q
  Inputs
    Q:DGQuotientModule
  Outputs
    b:Boolean
      True when @ TT "Q" @ has all expected structural keys, its
      projection map has the right source and target, and its
      relations submodule @ TT "Q.subDGModule" @ is itself a
      well-defined DG submodule of @ TT "Q.ambient" @.
  Description
    Text
      After a structural key-shape check, verifies that
      @ TT "Q.ambient" @ is a @ TO DGModule @, that
      @ TT "Q.subDGModule" @ is a @ TO DGSubmodule @ with ambient
      equal to @ TT "Q.ambient" @, and that @ TT "Q.projection" @ is
      a @ TO DGModuleMap @ from @ TT "Q.ambient" @ to @ TT "Q" @.
      The substantive check then delegates to
      @ TO (isWellDefined, DGSubmodule) @ on the relations
      submodule: the quotient is well-defined exactly when the
      relations are d-closed in the ambient.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      S = dgSubmodule(M, (id_(M.natural))_{1})
      Q = M / S
      isWellDefined Q
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    DGQuotientModule
    dgQuotientModule
    (isWellDefined, DGSubmodule)
    (symbol /, DGModule, DGSubmodule)
///

doc ///
  Key
    (isWellDefined, DGModuleMap)
  Headline
    Check that a DG module map has correct structure and is a chain map
  Usage
    b = isWellDefined f
  Inputs
    f:DGModuleMap
  Outputs
    b:Boolean
      True when @ TT "f" @ has all expected structural keys, source
      and target are DG modules (or DG quotient modules) over a
      common DG algebra, the natural matrix has the correct source
      and target, @ TT "f" @ preserves hom-degree, and
      @ TT "d_N(f(e_i)) = f(d_M(e_i))" @ on every natural generator
      @ TT "e_i" @ of the source.
  Description
    Text
      Six checks in order: structural key shape; source and target
      types are @ TO DGModule @ or @ TO DGQuotientModule @; source
      and target share the same DG algebra;
      @ TT "f.natural" @ is a module map with the correct source and
      target; the hom-degree of @ TT "f" @ is zero; and the chain-map
      condition holds on every generator of the source.
    Text
      Modeled on @ TT "isWellDefined ComplexMap" @ from the
      Complexes package.  Diagnostic messages are emitted when
      @ TT "debugLevel > 0" @.
    Example
      R = QQ[x, y]/ideal(x^2, y^2)
      A = koszulComplexDGA R
      M = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 2)
      idM = identityDGModuleMap M
      zM = zeroDGModuleMap(M, M)
      isWellDefined idM
      isWellDefined zM
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    DGModuleMap
    dgModuleMap
    identityDGModuleMap
    zeroDGModuleMap
    (isWellDefined, DGModule)
///

doc ///
  Key
    (isAcyclic, DGModule)
    [(isAcyclic, DGModule), EndDegree]
  Headline
    Determine whether a DG module has vanishing positive-degree homology
  Usage
    b = isAcyclic M
    b = isAcyclic(M, EndDegree => n)
  Inputs
    M:DGModule
    EndDegree => ZZ
      An upper bound for the range of degrees checked.  Required when
      @ TT "M" @ has infinite @ TO maxDegree @ (for example, when its
      DG algebra is an @ TO acyclicClosure @).  When omitted and
      @ TT "M" @ is finite-dimensional, @ TT "maxDegree M" @ is used.
  Outputs
    b:Boolean
      True when @ TT "H_i(M) = 0" @ for all @ TT "1 <= i <= n" @.
  Description
    Text
      Checks per-degree homology via @ TO (homology, ZZ, DGModule) @
      and @ TO prune @.  Degree zero is intentionally excluded:
      acyclicity concerns only positive-degree homology.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      isAcyclic KM
    Text
      For the Koszul DG module on a regular sequence, all
      positive-degree homology vanishes (@ TT "H_0 = R / (x, y, z)" @
      is the only nonzero piece).
    Example
      R' = QQ[x, y]/ideal(x^2, y^2)
      KM' = koszulComplexDGM R'^1
      not isAcyclic(KM', EndDegree => 3)
    Text
      Over a non-regular ring the Koszul DG module picks up nontrivial
      positive-degree homology.
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    (isAcyclic, DGAlgebra)
    (isAcyclic, DGQuotientModule)
    (homology, ZZ, DGModule)
    EndDegree
///

doc ///
  Key
    (isAcyclic, DGQuotientModule)
    [(isAcyclic, DGQuotientModule), EndDegree]
  Headline
    Determine whether a DG quotient module has vanishing positive-degree homology
  Usage
    b = isAcyclic Q
    b = isAcyclic(Q, EndDegree => n)
  Inputs
    Q:DGQuotientModule
    EndDegree => ZZ
      An upper bound for the range of degrees checked.  Required when
      @ TT "Q.ambient" @ has infinite @ TO maxDegree @.
  Outputs
    b:Boolean
      True when @ TT "H_i(Q) = 0" @ for all @ TT "1 <= i <= n" @.
  Description
    Text
      Semantics match @ TO (isAcyclic, DGModule) @: positive-degree
      homology only, computed via @ TO (homology, ZZ, DGQuotientModule) @
      and @ TO prune @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      Sfull = dgSubmodule(M, id_(M.natural))
      Q0 = M / Sfull
      isAcyclic Q0
    Text
      When the quotient is the zero DG module, @ TT "isAcyclic" @
      holds trivially.
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    (isAcyclic, DGModule)
    (homology, ZZ, DGQuotientModule)
    EndDegree
///

doc ///
  Key
    (isQuasiIsomorphism, DGAlgebraMap)
  Headline
    Determine whether a DG algebra map induces an isomorphism on homology
  Usage
    b = isQuasiIsomorphism phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    b:Boolean
      True when the induced chain map @ TT "toComplexMap phi" @ is a
      quasi-isomorphism on the underlying complexes.
  Description
    Text
      Computed by forwarding to @ TT "isQuasiIsomorphism(toComplexMap phi)" @
      from the Complexes package.  The underlying check inspects
      @ TT "H_n" @ of the induced chain map at every degree in the
      concentration of the source and target.  The
      @ TT "Concentration" @ option of the Complexes version is
      accepted and passed through.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      phi = identityDGAlgebraMap A
      isQuasiIsomorphism phi
    Text
      The identity DG algebra map is trivially a quasi-isomorphism.
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    (isQuasiIsomorphism, DGModuleMap)
    identityDGAlgebraMap
    toComplexMap
///

doc ///
  Key
    (annihilator, DGSubmodule)
  Headline
    The DG ideal of A annihilating a DG submodule
  Usage
    I = annihilator S
  Inputs
    S:DGSubmodule
  Outputs
    I:DGIdeal
      The DG ideal of @ TT "S.ambient.dgAlgebra" @ whose underlying
      ideal is the annihilator of @ TT "image S.inclusion.natural" @
      as a submodule of @ TT "S.ambient.natural" @.
  Description
    Text
      Computes @ TT "annihilator(image S.inclusion.natural)" @ at the
      @ TT "A.natural" @ level, then wraps the result as a DG ideal.
      The result is a DG ideal: if @ TT "a" @ annihilates @ TT "S" @
      and @ TT "s \\in S" @, then
      @ TT "d(a s) = d(a) s \\pm a d(s)" @ vanishes because both
      terms vanish (the second since @ TT "d(s) \\in S" @), so
      @ TT "d(a) s = 0" @ for every @ TT "s \\in S" @ and
      @ TT "d(a)" @ is again in the annihilator.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{x_Anat}})
      I = annihilator S
      isWellDefined I
    Text
      The annihilator of the zero submodule is the unit ideal.
    Example
      S0 = dgSubmodule(M, map(M.natural, (Anat)^0, 0))
      annihilator S0 == dgIdeal(A, {1_Anat})
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    (annihilator, DGQuotientModule)
    DGIdeal
    dgIdeal
    DGSubmodule
///

doc ///
  Key
    (annihilator, DGQuotientModule)
  Headline
    The DG ideal of A annihilating a DG quotient module
  Usage
    I = annihilator Q
  Inputs
    Q:DGQuotientModule
  Outputs
    I:DGIdeal
      The DG ideal of @ TT "Q.dgAlgebra" @ whose underlying ideal is
      the annihilator of @ TT "Q.natural" @ (a cokernel presentation)
      as an @ TT "A.natural" @-module.
  Description
    Text
      Computes @ TT "annihilator Q.natural" @ and wraps it as a DG
      ideal.  As with @ TO (annihilator, DGSubmodule) @ the result is
      automatically d-closed: the argument showing closure uses
      Leibniz together with the fact that @ TT "d" @ on the ambient
      descends to @ TT "d" @ on the quotient.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{x_Anat}})
      Q = M / S
      I = annihilator Q
      isWellDefined I
      isSubset(dgIdeal(A, {x_Anat}), I)
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    (annihilator, DGSubmodule)
    DGIdeal
    dgQuotientModule
    DGQuotientModule
///

doc ///
  Key
    "Semifree resolutions of DG modules"
  Headline
    Building semifree DG module resolutions via koszulComplexDGM, adjoinGenerators, killCycles, and semifreeResolution
  Description
    Text
      A @ TO DGModule @ @ TT "F" @ over a @ TO DGAlgebra @ @ TT "A" @ is
      @ EM "semifree" @ if its underlying graded @ TT "A.natural" @-module
      is free with a homogeneous basis.  Semifree DG modules play the
      role of free modules in ordinary homological algebra: every
      @ TT "A" @-module (or @ TT "A.ring" @-module lifted along the
      augmentation @ TT "A -> A.ring" @) admits a semifree resolution,
      unique up to DG module homotopy.
    Text
      The package provides four building blocks for assembling semifree
      resolutions by hand or automatically:
    Text
      @ TO koszulComplexDGM @ constructs the Koszul DG module
      @ TT "K^R \\otimes_R M" @ as a DG module over
      @ TT "koszulComplexDGA(ring M)" @.  On a regular ring this is
      already a free resolution of @ TT "M" @; over a singular ring it
      provides a starting point that needs further generators adjoined
      to kill higher homology.
    Text
      @ TO adjoinGenerators @ takes a free DG module @ TT "M" @ and a
      list of cycles in @ TT "M.natural" @ and returns a new free DG
      module with one additional generator per cycle, whose differential
      is that cycle.  This is the module-theoretic analog of
      @ TO adjoinVariables @ on a DG algebra.
    Text
      @ TO (killCycles, DGModule) @ scans for the smallest hom-degree
      @ TT "n" @ in the requested range at which @ TT "H_n(M)" @ is
      nonzero, then adjoins one new hom-degree-@ TT "(n+1)" @ generator
      per minimal homology class.  Iterating this from
      @ TT "StartDegree" @ up to @ TT "EndDegree" @ produces a DG module
      with vanishing homology in that range.
    Text
      @ TO semifreeResolution @ automates the full construction: build a
      free cover of @ TT "M" @, adjoin hom-degree-1 generators for the
      relations of @ TT "M" @, then iterate
      @ TO (killCycles, DGModule) @.  Its refined cousin
      @ TO minimalSemifreeResolution @ applies
      @ TO getBoundaryPreimage @ at each stage to subtract off
      boundaries before adjoining, producing a resolution that is
      minimal over @ TT "A" @ in the sense that every generator's
      differential lands in the augmentation ideal of
      @ TT "A.natural" @.  The predicate
      @ TO isMinimalSemifreeResolution @ tests this minimality
      condition.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 3)
      isMinimalSemifreeResolution Mdg
      apply(0..3, n -> prune homology(n, Mdg))
    Text
      Over a complete intersection of codimension two, the rank of
      @ TT "F_n" @ in the minimal semifree resolution of the residue
      field matches the @ TT "n" @-th Betti number @ TT "n+1" @, as
      expected from the Tate construction.
  Subnodes
    koszulComplexDGM
    adjoinGenerators
    (killCycles, DGModule)
    semifreeResolution
    [semifreeResolution, StartDegree]
    [semifreeResolution, EndDegree]
    minimalSemifreeResolution
    [minimalSemifreeResolution, StartDegree]
    [minimalSemifreeResolution, EndDegree]
    isMinimalSemifreeResolution
  SeeAlso
    acyclicClosure
    killCycles
    adjoinVariables
    getBoundaryPreimage
///

doc ///
  Key
    koszulComplexDGM
    (koszulComplexDGM, Module)
    (koszulComplexDGM, DGAlgebra, Module)
  Headline
    The Koszul complex of a module as a DG module
  Usage
    KM = koszulComplexDGM M
    KM = koszulComplexDGM(A, M)
  Inputs
    A:DGAlgebra
      A DG algebra over @ TT "R = ring M" @.  If omitted,
      @ TT "koszulComplexDGA(ring M)" @ is used.
    M:Module
      A module over a polynomial or quotient ring @ TT "R" @.
  Outputs
    KM:DGModule
      The Koszul DG module @ TT "K^R \\otimes_R M" @ viewed as a DG
      module over @ TT "A" @.
  Description
    Text
      The module generators of @ TT "KM" @ are one copy of each
      generator of @ TT "M" @, placed in hom-degree @ TT "0" @.  The
      differential vanishes on these generators, so the full
      differential of @ TT "KM" @ is determined by the Leibniz rule
      together with the differential on @ TT "A.natural" @.  In the
      one-argument form the DG algebra is built on demand via
      @ TT "koszulComplexDGA(ring M)" @; in the two-argument form the
      caller supplies @ TT "A" @, whose @ TT "ring" @ must equal
      @ TT "ring M" @.
    Text
      On a regular ring @ TT "R" @, @ TT "K^R" @ resolves the residue
      field @ TT "k" @, so @ TT "koszulComplexDGM M" @ resolves @ TT "M" @
      via @ TT "k \\otimes_R M" @-style homology.  In particular
      @ TT "koszulComplexDGM R^1" @ recovers the Koszul complex of
      @ TT "R" @ itself.
    Text
      Over a regular ring, the Koszul DG module on @ TT "R^1" @ is a
      free resolution of the residue field, so its higher homology
      vanishes:
    Example
      R = QQ[x, y, z]
      KM = koszulComplexDGM R^1
      apply(0..3, n -> prune homology(n, KM))
    Text
      Over a complete intersection, the higher homology of
      @ TT "koszulComplexDGM R^1" @ is no longer zero — it is the Koszul
      homology algebra of @ TT "R" @, which records the deviations of
      @ TT "R" @.  For the codimension-2 complete intersection
      @ TT "k[x,y]/(x^2, y^2)" @, @ TT "HH_i(KM)" @ has rank equal to the
      @ TT "i" @-th Betti number of the residue field:
    Example
      S = QQ[x, y] / ideal(x^2, y^2)
      KS = koszulComplexDGM S^1
      apply(0..3, n -> numgens prune homology(n, KS))
    Text
      Applying @ TO koszulComplexDGM @ to a nontrivial quotient module
      produces its Koszul resolution, now tensored against the given
      module.  Here we resolve @ TT "S / (x)" @ over @ TT "S = k[x, y]" @
      and read off its free ranks:
    Example
      S = QQ[x, y]
      KQ = koszulComplexDGM(S^1 / ideal(x))
      C = toComplex KQ
      apply(0..2, n -> rank C_n)
      apply(0..2, n -> prune homology(n, KQ))
    Text
      In the two-argument form the caller-supplied DG algebra must share
      the base ring of the module.  This is the usual way to build a
      Koszul DG module that lives over the @ TO DGAlgebra @ you will pass
      to @ TO semifreeResolution @ or @ TO minimalSemifreeResolution @:
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 3)
      apply(0..3, n -> rank (toComplex Mdg)_n)
  Caveat
    The two-argument form raises an error when
    @ TT "A.ring =!= ring M" @.
  SeeAlso
    "Semifree resolutions of DG modules"
    koszulComplexDGA
    DGModule
    freeDGModule
    semifreeResolution
    minimalSemifreeResolution
///

doc ///
  Key
    adjoinGenerators
    (adjoinGenerators, DGModule, List)
  Headline
    Adjoin new free generators to a DG module with prescribed differentials
  Usage
    Mnew = adjoinGenerators(M, cycleList)
  Inputs
    M:DGModule
      A free DG module (built by @ TO freeDGModule @ or an earlier call
      to @ TO adjoinGenerators @).
    cycleList:List
      A list of Vectors in @ TT "M.natural" @; each entry should be a
      cycle under the differential of @ TT "M" @, homogeneous with
      respect to the ambient multi-grading.
  Outputs
    Mnew:DGModule
      A free DG module whose generator list is @ TT "M.Degrees" @
      followed by one new generator per cycle in @ TT "cycleList" @,
      shifted up by one hom-degree, with differential equal to the
      corresponding cycle.
  Description
    Text
      This is the module-theoretic analog of @ TO adjoinVariables @ on a
      DG algebra.  For each @ TT "z" @ in @ TT "cycleList" @ of
      hom-degree @ TT "d" @, the output has a fresh generator in
      hom-degree @ TT "d + 1" @ whose differential is @ TT "z" @.  If
      @ TT "z" @ is indeed a cycle then the new generator witnesses its
      homology class as a boundary, so homology classes supported on
      @ TT "cycleList" @ are killed.
    Text
      Existing generator indices and their differentials are preserved
      verbatim: the first @ TT "#M.Degrees" @ generators of
      @ TT "Mnew" @ correspond to those of @ TT "M" @, so code that
      references @ TT "(M.natural)_i" @ for old indices @ TT "i" @
      continues to work when lifted to @ TT "Mnew" @.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      Mnew = adjoinGenerators(M, {x * natGens#0})
      #Mnew.Degrees
      first Mnew.Degrees#1
    Text
      The new generator sits in hom-degree @ TT "1" @ and its
      differential is the cycle @ TT "x * e_0" @ in @ TT "M.natural" @.
  Caveat
    Only supported when @ TT "M.natural" @ is a free
    @ TT "A.natural" @-module (i.e. @ TT "M" @ was built by
    @ TO freeDGModule @ or a previous @ TO adjoinGenerators @ call).
    Each entry of @ TT "cycleList" @ must be a cycle; otherwise the
    output differential will not satisfy @ TT "d^2 = 0" @.
  SeeAlso
    "Semifree resolutions of DG modules"
    freeDGModule
    (killCycles, DGModule)
    adjoinVariables
///

doc ///
  Key
    (killCycles, DGModule)
    [(killCycles, DGModule), StartDegree]
    [(killCycles, DGModule), EndDegree]
  Headline
    Adjoin free generators to kill the lowest nonvanishing homology of a DG module
  Usage
    Mnew = killCycles M
    Mnew = killCycles(M, StartDegree => s, EndDegree => e)
  Inputs
    M:DGModule
      A free DG module.
    StartDegree => ZZ
      The smallest hom-degree at which to look for nontrivial homology.
      Defaults to @ TT "1" @.
    EndDegree => ZZ
      The largest hom-degree at which to look; defaults to
      @ TT "StartDegree" @ (so by default a single degree is scanned).
  Outputs
    Mnew:DGModule
      Either @ TT "M" @ itself (if homology vanishes throughout the
      requested range) or a free DG module with additional hom-degree
      @ TT "n+1" @ generators, where @ TT "n" @ is the smallest degree
      in the range with @ TT "H_n(M) != 0" @; the new generators'
      differentials are representative cycles of a minimal generating
      set for @ TT "H_n(M)" @.
  Description
    Text
      The construction mirrors @ TO (killCycles, DGAlgebra) @ for DG
      modules.  Scanning from @ TT "StartDegree" @ upwards, the method
      computes @ TT "prune homology(n, M)" @ until it finds the first
      degree @ TT "n" @ with nontrivial homology.  It then lifts the
      minimal homology generators provided by the pruning map back to
      cycles in @ TT "M_n" @ and hands them to @ TO adjoinGenerators @,
      producing a new DG module in which those classes become
      boundaries.
    Text
      A single call kills homology at one degree.  Iterating with
      successive @ TT "StartDegree" @ values is how
      @ TO semifreeResolution @ and
      @ TO minimalSemifreeResolution @ build resolutions.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      M2 = adjoinGenerators(M, {x * natGens#0})
      prune homology(0, M2)
      M3 = killCycles(M2, StartDegree => 1, EndDegree => 2)
      prune homology(1, M3) == 0
    Text
      @ TT "M2" @ has @ TT "H_0 = R/(x) = k" @ and no higher homology
      to kill, so @ TT "M3" @ agrees with @ TT "M2" @ in this example.
      In settings where @ TT "H_1" @ is nontrivial, @ TT "killCycles" @
      adjoins one hom-degree-2 generator per minimal @ TT "H_1" @
      class.
  Caveat
    Raises an error when @ TT "StartDegree > EndDegree" @.  The input
    must be a free DG module for the underlying homology computation
    to extract representative cycles.
  SeeAlso
    "Semifree resolutions of DG modules"
    adjoinGenerators
    (killCycles, DGAlgebra)
    semifreeResolution
    minimalSemifreeResolution
    StartDegree
    EndDegree
///

doc ///
  Key
    semifreeResolution
    (semifreeResolution, DGAlgebra, Module)
    (semifreeResolution, Module)
  Headline
    Build a semifree DG module resolving a module over the base ring
  Usage
    F = semifreeResolution M
    F = semifreeResolution(A, M)
    F = semifreeResolution(A, M, EndDegree => n)
  Inputs
    A:DGAlgebra
      A DG algebra over @ TT "R = ring M" @.  If omitted,
      @ TT "koszulComplexDGA(ring M)" @ is used.
    M:Module
      A module over @ TT "A.ring" @.
    StartDegree => ZZ
      First hom-degree at which to start killing homology.  Defaults
      to @ TT "1" @.
    EndDegree => ZZ
      Last hom-degree at which to kill homology.  Defaults to
      @ TT "3" @.  Homology of @ TT "F" @ in the range
      @ TT "[StartDegree, EndDegree]" @ will vanish.
  Outputs
    F:DGModule
      A free DG module over @ TT "A" @ with
      @ TT "H_0(F) = M" @ and @ TT "H_n(F) = 0" @ for
      @ TT "StartDegree <= n <= EndDegree" @.
  Description
    Text
      Starts from a free DG module with one hom-degree-0 generator per
      generator of @ TT "M" @, adjoins hom-degree-1 generators whose
      differentials are the columns of @ TT "presentation M" @ lifted
      to @ TT "A.natural" @ (killing the presentation relations), and
      then iterates @ TO (killCycles, DGModule) @ from
      @ TT "StartDegree" @ through @ TT "EndDegree" @.
    Text
      This is the module-theoretic analog of
      @ TO acyclicClosure @ on a DG algebra.  The output is a semifree
      DG module but is not guaranteed to be minimal over @ TT "A" @:
      relation columns are adjoined even when they are already
      boundaries under the DG algebra differential.  For a minimal
      resolution, use @ TO minimalSemifreeResolution @.
    Example
      R = QQ[x]
      M = R^1 / ideal(x)
      Mdg = semifreeResolution(koszulComplexDGA R, M, EndDegree => 3)
      prune homology(0, Mdg)
      all(1..3, n -> prune homology(n, Mdg) == 0)
    Text
      Over a complete intersection, the resolution is infinite but the
      truncation up to any requested @ TT "EndDegree" @ is acyclic in
      positive hom-degrees up to that bound.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      Mdg = semifreeResolution(koszulComplexDGA R, k, EndDegree => 3)
      all(1..3, i -> prune homology(i, Mdg) == 0)
    Text
      The @ TT "Module" @-only form infers the DG algebra from
      @ TT "ring M" @:
    Example
      R = QQ[x]
      M = R^1 / ideal(x)
      Mdg = semifreeResolution(M, EndDegree => 2)
      Mdg.ring === R
      Mdg.dgAlgebra.ring === R
  Caveat
    The output is not minimal in general; use
    @ TO minimalSemifreeResolution @ when a minimal presentation is
    required.  The base ring of @ TT "M" @ must agree with
    @ TT "A.ring" @ in the two-argument form.
  SeeAlso
    "Semifree resolutions of DG modules"
    minimalSemifreeResolution
    (killCycles, DGModule)
    acyclicClosure
    koszulComplexDGM
///

doc ///
  Key
    [semifreeResolution, StartDegree]
  Headline
    Option to specify the degree at which semifreeResolution starts killing cycles
  Usage
    semifreeResolution(..., StartDegree => n)
  Description
    Text
      Controls the first hom-degree at which
      @ TO semifreeResolution @ attempts to kill homology.
      Hom-degrees strictly below @ TT "StartDegree" @ are populated
      only by the initial free cover and the adjoinment of
      presentation relations at hom-degree @ TT "1" @.  The default
      value is @ TT "1" @.
  SeeAlso
    semifreeResolution
    [semifreeResolution, EndDegree]
///

doc ///
  Key
    [semifreeResolution, EndDegree]
  Headline
    Option to specify the last degree at which semifreeResolution kills cycles
  Usage
    semifreeResolution(..., EndDegree => n)
  Description
    Text
      Controls the last hom-degree at which
      @ TO semifreeResolution @ iterates
      @ TO (killCycles, DGModule) @.  Homology of the output in the
      range @ TT "[StartDegree, EndDegree]" @ vanishes; higher degrees
      are not resolved.  The default value is @ TT "3" @.
  SeeAlso
    semifreeResolution
    [semifreeResolution, StartDegree]
///

doc ///
  Key
    minimalSemifreeResolution
    (minimalSemifreeResolution, DGAlgebra, Module)
    (minimalSemifreeResolution, Module)
  Headline
    Build a minimal semifree DG module resolution over a DG algebra
  Usage
    F = minimalSemifreeResolution M
    F = minimalSemifreeResolution(A, M)
    F = minimalSemifreeResolution(A, M, EndDegree => n)
  Inputs
    A:DGAlgebra
      A DG algebra over a (graded-)local ring @ TT "R = ring M" @.  If
      omitted, @ TT "koszulComplexDGA(ring M)" @ is used.
    M:Module
      A module over @ TT "A.ring" @.
    StartDegree => ZZ
      First hom-degree at which to start killing homology.  Defaults
      to @ TT "1" @.
    EndDegree => ZZ
      Last hom-degree at which to kill homology.  Defaults to
      @ TT "3" @.
  Outputs
    F:DGModule
      A semifree DG module resolving @ TT "M" @ over @ TT "A" @,
      minimal in the sense that every generator's differential lands
      in the augmentation ideal of @ TT "A.natural" @.
  Description
    Text
      Produces a semifree resolution whose generator count in each
      hom-degree is minimal among all semifree resolutions of
      @ TT "M" @ over @ TT "A" @.  Two ingredients make the output
      minimal:
    Text
      First, @ TT "M" @ is replaced by @ TT "prune M" @, giving a
      minimal presentation over the (graded-)local base ring.  The
      number of hom-degree-0 generators is therefore the minimal
      number of generators of @ TT "M" @ over @ TT "R" @.
    Text
      Second, each column of @ TT "presentation(prune M)" @ is tested
      with @ TO getBoundaryPreimage @ against the DG-algebra-induced
      differential.  If a relation is already a boundary, it is not
      adjoined; if only a residue is a non-boundary, the adjoined
      generator's differential is that residue rather than the
      original relation.  This is what makes the output minimal
      @ EM "over A" @, not merely over @ TT "R" @.
    Text
      The iterative @ TO (killCycles, DGModule) @ passes then use the
      canonical pruning map of @ TT "homology(n, F)" @ to pick a
      minimal generating set of representative cycles at each stage.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 3)
      isMinimalSemifreeResolution Mdg
      all(1..3, i -> prune homology(i, Mdg) == 0)
    Text
      Over the c.i. @ TT "R = k[x, y]/(x^2, y^2)" @ the minimal
      semifree resolution of @ TT "k" @ has @ TT "rank F_n = n+1" @,
      matching the Betti numbers of @ TT "k" @ over @ TT "R" @.
    Example
      apply(0..3, n -> numcols moduleDifferential(n, Mdg))
    Text
      As with @ TO semifreeResolution @, the @ TT "Module" @-only
      form infers @ TT "koszulComplexDGA(ring M)" @ as the DG
      algebra:
    Example
      R = QQ[x]
      M = R^1 / ideal(x)
      Mdg = minimalSemifreeResolution(M, EndDegree => 2)
      Mdg.ring === R
      isMinimalSemifreeResolution Mdg
    Text
      A more substantial example: take the Koszul DG algebra on a
      regular sequence cutting out a complete intersection, and
      resolve a module over the resulting DG algebra.  Here
      @ TT "A = koszulComplexDGA(I_*)" @ with
      @ TT "I = (x^2, y^2)" @ is a DG algebra over
      @ TT "Q = ZZ/101[x, y]" @ whose degree-zero homology is the
      complete intersection @ TT "Q/I" @, and
      @ TT "A" @ itself is the minimal DG algebra resolution of
      @ TT "Q/I" @ over @ TT "Q" @.  We resolve the
      @ TT "Q/I" @-module @ TT "M = Q/(x^2, x*y, y^2)" @ as a DG
      @ TT "A" @-module:
    Example
      Q = ZZ/101[x, y]
      I = ideal(x^2, y^2)
      A = koszulComplexDGA(I_*)
      M = Q^1 / ideal(x^2, x*y, y^2)
      Mdg = minimalSemifreeResolution(A, M, EndDegree => 5)
      isMinimalSemifreeResolution Mdg
      all(1..5, i -> prune homology(i, Mdg) == 0)
      apply(0..5, n -> numcols moduleDifferential(n, Mdg))
    Text
      @ BOLD "Matrix factorizations on hypersurfaces." @  Over a
      hypersurface @ TT "R = Q/(f)" @ every maximal Cohen-Macaulay
      module's free resolution is eventually @ EM "2-periodic" @
      with differentials forming a matrix factorization
      @ TT "(phi, psi)" @ of @ TT "f" @ (Eisenbud).  The minimal
      semifree resolution over @ TT "A = koszulComplexDGA(ideal f)" @
      exhibits this periodicity directly in its printed
      differentials.  Here is the classic example @ TT "f = x^5" @:
    Example
      Q = ZZ/101[x]
      A = koszulComplexDGA(ideal(x^5))
      Mdg = minimalSemifreeResolution(A, Q^1/ideal(x), EndDegree => 6)
      apply(1..6, n -> flatten entries moduleDifferential(n, Mdg))
    Text
      The 2x2 blocks alternate between an @ TT "x" @-pattern and an
      @ TT "x^4" @-pattern; their product is @ TT "x^5" @ times an
      identity matrix, which is exactly the matrix factorization
      @ TT "x * x^4 = x^5" @.  Similarly, for the quadric form
      @ TT "f = x^2 + y^2" @ the module
      @ TT "coker [[x, y], [y, -x]]" @ is already a matrix
      factorization of @ TT "f" @, so its minimal semifree
      resolution stabilizes in hom-degree one:
    Example
      Q = ZZ/101[x, y]
      A = koszulComplexDGA(ideal(x^2 + y^2))
      M = coker matrix{{x, y}, {y, -x}}
      Mdg = minimalSemifreeResolution(A, M, EndDegree => 6)
      apply(0..6, n -> numcols moduleDifferential(n, Mdg))
    Text
      For the three-variable quadric @ TT "f = x^2 + y^2 + z^2" @,
      resolving the residue field stabilizes at rank @ TT "8" @,
      reflecting the 8-dimensional Clifford algebra of a
      nondegenerate ternary quadratic form:
    Example
      Q = ZZ/101[x, y, z]
      A = koszulComplexDGA(ideal(x^2 + y^2 + z^2))
      Mdg = minimalSemifreeResolution(A, Q^1/ideal(x, y, z), EndDegree => 6)
      apply(0..6, n -> numcols moduleDifferential(n, Mdg))
  Caveat
    Minimality relies on @ TT "A.ring" @ being (graded-)local so that
    @ TT "prune" @ returns a minimal presentation.  For
    @ TT "A = koszulComplexDGA R" @ with @ TT "R" @ a standard graded
    quotient of a polynomial ring over a field, this is satisfied.
  SeeAlso
    "Semifree resolutions of DG modules"
    semifreeResolution
    isMinimalSemifreeResolution
    getBoundaryPreimage
    (killCycles, DGModule)
///

doc ///
  Key
    [minimalSemifreeResolution, StartDegree]
  Headline
    Option to specify the degree at which minimalSemifreeResolution starts killing cycles
  Usage
    minimalSemifreeResolution(..., StartDegree => n)
  Description
    Text
      Controls the first hom-degree at which
      @ TO minimalSemifreeResolution @ attempts to kill homology.
      The default is @ TT "1" @.
  SeeAlso
    minimalSemifreeResolution
    [minimalSemifreeResolution, EndDegree]
///

doc ///
  Key
    [minimalSemifreeResolution, EndDegree]
  Headline
    Option to specify the last degree at which minimalSemifreeResolution kills cycles
  Usage
    minimalSemifreeResolution(..., EndDegree => n)
  Description
    Text
      Controls the last hom-degree at which
      @ TO minimalSemifreeResolution @ iterates
      @ TO (killCycles, DGModule) @.  Homology of the output in the
      range @ TT "[StartDegree, EndDegree]" @ vanishes.  The default
      is @ TT "3" @.
  SeeAlso
    minimalSemifreeResolution
    [minimalSemifreeResolution, StartDegree]
///

doc ///
  Key
    isMinimalSemifreeResolution
    (isMinimalSemifreeResolution, DGModule)
  Headline
    Test whether a semifree DG module is minimal over its DG algebra
  Usage
    b = isMinimalSemifreeResolution M
  Inputs
    M:DGModule
      A free DG module (the intended use is on the output of
      @ TO semifreeResolution @ or
      @ TO minimalSemifreeResolution @).
  Outputs
    b:Boolean
      @ TT "true" @ when each generator's differential lies in the
      augmentation ideal of @ TT "A.natural" @, i.e. its reduction
      along @ TT "A -> k" @ (sending @ TT "T_i -> 0" @ and then mod
      @ TT "(vars R)" @) is zero.
  Description
    Text
      Minimality over a DG algebra @ TT "A" @ means: after base change
      along the augmentation @ TT "A -> k" @, every differential of
      @ TT "F" @ becomes the zero map.  Equivalently, each
      @ TT "M.diff#i" @ has all of its coefficients (in the free
      basis of @ TT "M.natural" @) lying in the augmentation ideal of
      @ TT "A.natural" @ generated by @ TT "(vars R)" @ and the
      positive-hom-degree generators @ TT "T_i" @.
    Text
      This predicate checks only the structural minimality condition;
      it does not verify acyclicity.  Use
      @ TO (isAcyclic, DGModule) @ with an @ TT "EndDegree" @ option
      for that.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 3)
      isMinimalSemifreeResolution Mdg
    Text
      A DG module built with a unit differential between free
      generators is not minimal:
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      Mbad = freeDGModule(A, {0, 1})
      natGens = apply(rank Mbad.natural, i -> (Mbad.natural)_i)
      setDiff(Mbad, {0, natGens#0})
      isMinimalSemifreeResolution Mbad
  Caveat
    Returns @ TT "false" @ on DG modules whose underlying
    @ TT "A.natural" @-module is not free (in that case the
    generator differentials are not the right data to test).
  SeeAlso
    "Semifree resolutions of DG modules"
    minimalSemifreeResolution
    semifreeResolution
    (isAcyclic, DGModule)
///

doc ///
  Key
    "Computing module differentials and visualizing DG modules"
  Headline
    Differential matrices, element-wise differentials, and visualization helpers for DG modules
  Description
    Text
      Once a @ TO DGModule @ @ TT "M" @ over a @ TO DGAlgebra @
      @ TT "A" @ is built, the package supports two levels of access
      to the differential.
    Text
      The @ EM "matrix-level" @ view is @ TO moduleDifferential @: the
      call @ TT "moduleDifferential(n, M)" @ returns the hom-degree-@ TT "n" @
      piece of @ TT "d_M" @ as a homogeneous matrix over
      @ TT "A.ring" @ whose source and target are the free
      @ TT "A.ring" @-modules on the respective monomial bases of
      @ TT "M_n" @ and @ TT "M_{n-1}" @.  The block-structured variant
      @ TO moduleBlockDiff @ partitions this matrix into one summand
      per @ TT "(i, v)" @ label, where @ TT "i" @ indexes a generator
      of @ TT "M.natural" @ and @ TT "v" @ is a chunk-degree vector
      on the variables of @ TT "A.natural" @.  The pretty-printer
      @ TO displayModuleBlockDiff @ renders this labeled block matrix
      with one label per row and column.
    Text
      The @ EM "element-level" @ view is @ TT "diff(M, v)" @: apply the
      differential directly to a homogeneous element
      @ TT "v" @ of @ TT "M.natural" @ via the Leibniz rule, returning
      a Vector in @ TT "M.natural" @ of hom-degree one less.  This is
      the entry point used internally by chain-map checks and by
      @ TO (homologyClass, DGModule, Vector) @.
    Text
      Two lightweight inspection helpers summarize the generator
      layout: @ TO generatorTable @ prints a one-row-per-generator
      table with hom-degree, external degree, and differential; and
      @ TO dgModuleSummary @ tabulates, for each hom-degree in a
      requested range, the number of freshly adjoined generators at
      that degree and the rank of @ TT "F_n" @ as an
      @ TT "A.ring" @-module.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 2)
      d2 = moduleDifferential(2, Mdg)
      d1 = moduleDifferential(1, Mdg)
      d1 * d2 == 0
    Text
      Over a complete intersection, the semifree resolution of the
      residue field has well-defined differentials matching
      @ TT "d^2 = 0" @ in every degree.
  Subnodes
    moduleDifferential
    (diff, DGModule, Vector)
    (diff, DGQuotientModule, Vector)
    generatorTable
    dgModuleSummary
    moduleBlockDiff
    displayModuleBlockDiff
  SeeAlso
    "Semifree resolutions of DG modules"
    (homology, ZZ, DGModule)
///

doc ///
  Key
    moduleDifferential
    (moduleDifferential, ZZ, DGModule)
  Headline
    The hom-degree-n differential of a DG module as a matrix over the base ring
  Usage
    dn = moduleDifferential(n, M)
  Inputs
    n:ZZ
      A hom-degree.
    M:DGModule
      A DG module over a DG algebra @ TT "A" @; must be free (as in
      @ TO freeDGModule @ output) or in @ TO koszulComplexDGM @
      form.
  Outputs
    dn:Matrix
      The component @ TT "d_n : F_n -> F_{n-1}" @ of the
      differential of @ TT "M" @, as a homogeneous matrix over
      @ TT "A.ring" @.  Columns are indexed by the pairs
      @ TT "(i, m)" @ of @ TT "moduleBasisPairs(M, n)" @; rows by
      the pairs of @ TT "moduleBasisPairs(M, n-1)" @.
  Description
    Text
      Two code paths are used internally:
    Text
      When @ TT "M" @ comes from @ TO koszulComplexDGM @ (so
      @ TT "M.module" @ is set) and every generator differential is
      zero, @ TT "d_n" @ is the tensor product
      @ TT "polyDifferential(n, A) \\otimes id_{M.module}" @.  This
      is the fast path.
    Text
      For general free DG modules, @ TT "d_n" @ is computed
      monomial-by-monomial via the Leibniz rule, extracting
      @ TT "A.ring" @-linear coefficients in the target basis.
      Results are cached per hom-degree in
      @ TT "M.cache.diffs" @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 2)
      d1 = moduleDifferential(1, Mdg)
      d2 = moduleDifferential(2, Mdg)
      d1 * d2 == 0
    Text
      Every column of @ TT "d_{n-1} d_n" @ is zero, which is
      the @ TT "d^2 = 0" @ condition.
  Caveat
    Raises an error on non-free DG modules (for example, the
    natural module of a @ TO DGQuotientModule @).  In that setting,
    compute the differential at the element level via
    @ TT "diff(Q, v)" @ or work with @ TO dgComplex @.
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    (diff, DGModule, Vector)
    moduleBlockDiff
    polyDifferential
    (homology, ZZ, DGModule)
///

doc ///
  Key
    (diff, DGModule, Vector)
  Headline
    Apply the DG module differential to an element
  Usage
    dv = diff(M, v)
  Inputs
    M:DGModule
      A DG module.
    v:Vector
      A homogeneous element of @ TT "M.natural" @.
  Outputs
    dv:Vector
      The image @ TT "d_M(v)" @ in @ TT "M.natural" @, computed via
      the Leibniz rule.
  Description
    Text
      Applies the Leibniz rule coordinate by coordinate.  If
      @ TT "v = sum_i f_i \\cdot e_i" @ where @ TT "e_i" @ are the
      free generators of @ TT "M.natural" @ and
      @ TT "f_i \\in A.natural" @, then
      @ TT "d_M(v) = sum_i (d_A(f_i) \\cdot e_i + (-1)^{|f_i|} f_i \\cdot d_M(e_i))" @
      where @ TT "|f_i|" @ is the hom-degree of @ TT "f_i" @.
    Text
      This element-level interface is the entry point for checking
      the cycle condition on a candidate homology class and for
      defining @ TT "DGModuleMap" @ objects without committing to the
      matrix-level bookkeeping of
      @ TO moduleDifferential @.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      diff(M, natGens#1)
      diff(M, natGens#0)
    Text
      Here @ TT "d(e_1) = x \\cdot e_0" @ and @ TT "d(e_0) = 0" @.
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    moduleDifferential
    (diff, DGQuotientModule, Vector)
    (diff, DGAlgebra, RingElement)
///

doc ///
  Key
    (diff, DGQuotientModule, Vector)
  Headline
    Apply the induced differential on a DG quotient module to an element
  Usage
    dv = diff(Q, v)
  Inputs
    Q:DGQuotientModule
      A DG quotient module @ TT "Q = M / S" @.
    v:Vector
      A homogeneous element of @ TT "Q.natural" @.
  Outputs
    dv:Vector
      The image @ TT "d_Q(v)" @ in @ TT "Q.natural" @.
  Description
    Text
      Because the inclusion of @ TT "S" @ into @ TT "M" @ is a DG
      submodule (its columns are d-closed in @ TT "M" @), the
      differential of @ TT "M" @ descends to @ TT "Q" @.  This method
      applies the Leibniz rule to a representative in
      @ TT "Q.natural" @ without going through a matrix-level
      differential, which is convenient because @ TT "Q.natural" @ is
      a cokernel rather than a free module.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      S = dgSubmodule(M, (id_(M.natural))_{1})
      Q = M / S
      projection Q
      -- apply d to a natural-module generator of Q
      diff(Q, (Q.natural)_0)
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    (diff, DGModule, Vector)
    DGQuotientModule
    dgQuotientModule
///

doc ///
  Key
    generatorTable
    (generatorTable, DGModule)
  Headline
    Display the generator list of a DG module with hom-degrees and differentials
  Usage
    t = generatorTable M
  Inputs
    M:DGModule
  Outputs
    t:Net
      A centered table with one row per generator.  Columns are the
      generator index @ TT "i" @, the hom-degree, the remaining
      external degree, and the differential
      @ TT "M.diff#i" @.
  Description
    Text
      A convenient overview of the generator structure of a
      semifree DG module.  Particularly useful for inspecting the
      output of @ TO minimalSemifreeResolution @ or
      @ TO semifreeResolution @, where each iteration of
      @ TO (killCycles, DGModule) @ appends further generators.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      instance(generatorTable M, Net)
    Text
      When @ TT "M" @ has no generators, the result is a single-row
      placeholder.
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    dgModuleSummary
    moduleBlockDiff
///

doc ///
  Key
    dgModuleSummary
    (dgModuleSummary, DGModule)
    (dgModuleSummary, DGModule, ZZ)
  Headline
    Tabulate hom-degree-wise generator counts and free-rank counts for a DG module
  Usage
    t = dgModuleSummary M
    t = dgModuleSummary(M, n)
  Inputs
    M:DGModule
      A free DG module (for example from @ TO freeDGModule @ or
      @ TO minimalSemifreeResolution @).
    n:ZZ
      Upper bound on the hom-degree range to display.  When omitted,
      @ TT "maxDegree M" @ is used; if that is @ TT "infinity" @ the
      method raises an error asking for an explicit bound.
  Outputs
    t:Net
      A centered table with one row per hom-degree in @ TT "0..n" @.
      Columns are the hom-degree @ TT "k" @, the number of
      generators of @ TT "M" @ placed at hom-degree @ TT "k" @ (i.e.
      adjoined in that degree), and the rank of @ TT "F_k" @ as an
      @ TT "A.ring" @-module.
  Description
    Text
      A one-call overview of how the generators of @ TT "M" @
      distribute across hom-degrees and how that translates into
      base-ring ranks.  Matches the data fed into
      @ TO moduleDifferential @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 3)
      instance(dgModuleSummary(Mdg, 3), Net)
  Caveat
    The one-argument form is only applicable when the underlying
    hom-grading of @ TT "M" @ is bounded (for example when
    @ TT "M" @ is a @ TO koszulComplexDGM @ output over a polynomial
    ring).  For arbitrary semifree DG modules, pass an explicit
    upper bound.
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    generatorTable
    moduleBlockDiff
    moduleDifferential
///

doc ///
  Key
    moduleBlockDiff
    (moduleBlockDiff, DGModule, ZZ)
  Headline
    The hom-degree-n differential of a DG module as a labeled block matrix
  Usage
    b = moduleBlockDiff(M, n)
  Inputs
    M:DGModule
      A DG module over a DG algebra @ TT "A" @; must be free.
    n:ZZ
      A hom-degree.
  Outputs
    b:Matrix
      The hom-degree-@ TT "n" @ differential of @ TT "M" @, assembled
      as a map between direct sums indexed by pairs
      @ TT "{i, v}" @ (generator index plus variable-chunk-degree
      vector).
  Description
    Text
      A block-structured refinement of @ TO moduleDifferential @.
      The source of the resulting map is a direct sum of small free
      @ TT "A.ring" @-modules, one per pair
      @ TT "{i, v}" @, where @ TT "i" @ is the generator index of
      @ TT "M.natural" @ and @ TT "v" @ is a tuple of exponents
      giving the chunk-degrees of @ TT "A.natural" @-monomials
      contributing to @ TT "F_n" @.  Composing with
      @ TT "matrix" @ recovers exactly
      @ TT "moduleDifferential(n, M)" @.
    Text
      Results are cached in
      @ TT "M.cache#\"moduleBlockDiffs\"" @ per @ TT "n" @, so
      repeated calls are cheap.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 3)
      b2 = moduleBlockDiff(Mdg, 2)
      matrix b2 == moduleDifferential(2, Mdg)
      -- Each source/target index is a {genIdx, chunkVec} pair.
      srcIdx = indices source b2
      all(srcIdx, l -> #l == 2 and instance(l#0, ZZ) and instance(l#1, List))
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    displayModuleBlockDiff
    moduleDifferential
///

doc ///
  Key
    displayModuleBlockDiff
    (displayModuleBlockDiff, DGModule, ZZ)
    (displayModuleBlockDiff, DGModule, List, List)
  Headline
    Pretty-print the labeled block matrix of a DG module differential
  Usage
    tab = displayModuleBlockDiff(M, n)
    entry = displayModuleBlockDiff(M, srcLbl, tgtLbl)
  Inputs
    M:DGModule
      A DG module; must be free.
    n:ZZ
      A hom-degree.
    srcLbl:List
      A pair @ TT "{i, v}" @ identifying a single source block (a
      generator index and a chunk-degree vector).
    tgtLbl:List
      A pair @ TT "{j, w}" @ identifying a single target block.
  Outputs
    tab:Net
      A centered table whose first row lists source labels and whose
      first column lists target labels; inner entries are the
      corresponding blocks of @ TT "moduleBlockDiff(M, n)" @.
    entry:Matrix
      In the three-argument form, returns the single block
      @ TT "b^[tgtLbl]_[srcLbl]" @ extracted from the labeled block
      matrix.
  Description
    Text
      The two-argument form lays out @ TO moduleBlockDiff @ as a
      labeled table, parallel to @ TT "displayBlockDiff" @ for DG
      algebras.  The three-argument form bypasses the table and
      returns a single block directly, which is handy for
      programmatic inspection.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 2)
      instance(displayModuleBlockDiff(Mdg, 2), Net)
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    moduleBlockDiff
    moduleDifferential
///

doc ///
  Key
    "Building DG algebras from existing DG algebras"
  Headline
    Base change, generator restriction, truncation, and targeted cycle-killing for DG algebras
  Description
    Text
      Given an existing @ TO DGAlgebra @ @ TT "A" @, the package
      supports several ways to derive new DG algebras from it.  All of
      these constructions preserve the graded-commutative and Leibniz
      structure by design; most perform safety checks before returning.
    Text
      @ TO baseChange @ moves a DG algebra along a ring map on its
      base: given @ TT "A" @ over @ TT "R" @ and a ring map
      @ TT "phi: R -> S" @ (or simply a target ring @ TT "S" @, in
      which case @ TT "map(S, R)" @ is used), @ TT "baseChange" @
      returns a DG algebra over @ TT "S" @ whose generator list and
      differentials are obtained by transporting scalars along
      @ TT "phi" @.
    Text
      @ TO subDGAlgebra @ produces a sub-DG algebra of @ TT "A" @
      generated by a chosen subset of the DG generators.  The subset
      must be d-closed: it is an error if the differential of a kept
      generator uses a dropped one.
      @ TO restrictDifferential @ is a synonym.
      @ TO truncateGenerators @ is a convenience wrapper:
      @ TT "truncateGenerators(A, n)" @ calls
      @ TT "subDGAlgebra" @ with the generators of hom-degree
      strictly greater than @ TT "n" @.
    Text
      @ TO killHomologyAtDegree @ targets a specific hom-degree
      @ TT "n" @: it computes minimal representatives of
      @ TT "H_n(A)" @ and adjoins variables in hom-degree
      @ TT "n+1" @ whose differentials are those representatives,
      producing a new DG algebra with @ TT "H_n = 0" @.
    Text
      All four constructions take optional arguments
      @ TO InitializeDegreeZeroHomology @ and
      @ TO InitializeComplex @ controlling how much of the
      downstream cache is eagerly populated by the internal call to
      @ TO setDiff @.
    Text
      Two low-level predicates round out the family:
      @ TO isValidDGAlgebra @ performs a structural-invariant check
      (keys and types), and
      @ TO isWellDefinedDifferential @ performs the semantic
      @ TT "d^2 = 0" @ check on every generator.  Both are called
      internally by @ TO (isWellDefined, DGAlgebra) @.
    Example
      R = QQ[x, y]
      S = R / ideal(x^2, y^2)
      A = koszulComplexDGA R
      B = baseChange(A, S)
      underlyingRing B === S
      isWellDefinedDifferential B
    Text
      Moving a Koszul DG algebra from a polynomial ring to a
      complete intersection factor preserves
      @ TT "d^2 = 0" @ because the differential expressions
      @ TT "d(T_i) = x_i" @ remain valid in @ TT "S" @.
  Subnodes
    baseChange
    subDGAlgebra
    restrictDifferential
    truncateGenerators
    killHomologyAtDegree
    isValidDGAlgebra
    isWellDefinedDifferential
  SeeAlso
    freeDGAlgebra
    koszulComplexDGA
    setDiff
    (isWellDefined, DGAlgebra)
    adjoinVariables
    killCycles
///

doc ///
  Key
    baseChange
    (baseChange, DGAlgebra, Ring)
    (baseChange, DGAlgebra, RingMap)
    [baseChange, InitializeDegreeZeroHomology]
    [baseChange, InitializeComplex]
  Headline
    Transport a DG algebra along a ring map on its base
  Usage
    B = baseChange(A, S)
    B = baseChange(A, phi)
  Inputs
    A:DGAlgebra
      A DG algebra over some ring @ TT "R" @.
    S:Ring
      A target ring.  Shorthand for @ TT "baseChange(A, map(S, R))" @.
    phi:RingMap
      A ring map with source @ TT "R = underlyingRing A" @; its target
      becomes the base ring of @ TT "B" @.
    InitializeDegreeZeroHomology => Boolean
      Whether to compute @ TT "H_0" @ of the result eagerly.  Default
      @ TT "true" @.
    InitializeComplex => Boolean
      Whether to build the underlying complex of @ TT "B" @
      eagerly.  Default @ TT "false" @.
  Outputs
    B:DGAlgebra
      A DG algebra over @ TT "target phi" @ with the same generator
      hom-degrees as @ TT "A" @, and whose differentials are the
      images of @ TT "A.diff" @ under the obvious extension of
      @ TT "phi" @ that sends each @ TT "A" @-generator to the
      corresponding @ TT "B" @-generator.
  Description
    Text
      Useful for lifting a Koszul DG algebra over a polynomial ring
      to one over a factor ring, or for specialization (via a ring
      map that kills some variables).
    Example
      R = QQ[x, y]
      A = koszulComplexDGA R
      S = R / ideal(x^2, y^2)
      B = baseChange(A, S)
      underlyingRing B === S
      isWellDefinedDifferential B
    Text
      An explicit ring map gives more flexibility:
    Example
      R = QQ[x, y]
      A = koszulComplexDGA R
      phi = map(R, R, {x, 0})
      Bphi = baseChange(A, phi)
      underlyingRing Bphi === R
  Caveat
    Raises an error if @ TT "source phi" @ is not equal to
    @ TT "underlyingRing A" @.
  SeeAlso
    "Building DG algebras from existing DG algebras"
    (symbol **, DGAlgebra, Ring)
    InitializeDegreeZeroHomology
    InitializeComplex
///

doc ///
  Key
    subDGAlgebra
    (subDGAlgebra, DGAlgebra, List)
    [subDGAlgebra, InitializeDegreeZeroHomology]
    [subDGAlgebra, InitializeComplex]
  Headline
    The sub-DG algebra generated by a chosen subset of DG generators
  Usage
    B = subDGAlgebra(A, keepIdx)
  Inputs
    A:DGAlgebra
    keepIdx:List
      A list of integers giving the indices (into
      @ TT "0 .. numgens A.natural - 1" @) of the DG generators to
      keep.  Duplicates and out-of-range indices raise an error.
    InitializeDegreeZeroHomology => Boolean
      Whether to compute @ TT "H_0" @ of the result eagerly.
      Default @ TT "true" @.
    InitializeComplex => Boolean
      Whether to build the underlying complex eagerly.  Default
      @ TT "false" @.
  Outputs
    B:DGAlgebra
      A DG algebra over @ TT "A.ring" @ whose DG generators are the
      @ TT "keepIdx" @-indexed generators of @ TT "A" @ in sorted
      order, with their differentials restricted accordingly.
  Description
    Text
      The subset @ TT "keepIdx" @ must be d-closed: if the
      differential of a kept generator uses a dropped generator,
      @ TT "subDGAlgebra" @ raises an error rather than silently
      returning a non-DG object.  This protects downstream code
      from accidentally producing inconsistent structures.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      B = subDGAlgebra(A, {0})
      numgens B.natural
    Text
      Dropping a generator whose image is referenced in a kept
      generator's differential is caught:
    Example
      R = QQ[x] / ideal(x^2)
      A = koszulComplexDGA R
      A' = adjoinVariables(A, {x * (gens A.natural)#0})
      try subDGAlgebra(A', {1}) else "error: kept differential uses dropped generator"
  Caveat
    Duplicate or out-of-range entries in @ TT "keepIdx" @ raise
    an error.  @ TT "keepIdx" @ is internally sorted, so the
    generator order of the output depends only on which indices
    appear.
  SeeAlso
    "Building DG algebras from existing DG algebras"
    restrictDifferential
    truncateGenerators
///

doc ///
  Key
    restrictDifferential
    (restrictDifferential, DGAlgebra, List)
    [restrictDifferential, InitializeDegreeZeroHomology]
    [restrictDifferential, InitializeComplex]
  Headline
    Synonym for subDGAlgebra, named to match the restricted-differential vocabulary
  Usage
    B = restrictDifferential(A, keepIdx)
  Inputs
    A:DGAlgebra
    keepIdx:List
      List of integers giving DG generator indices to keep.
    InitializeDegreeZeroHomology => Boolean
      Default @ TT "true" @.
    InitializeComplex => Boolean
      Default @ TT "false" @.
  Outputs
    B:DGAlgebra
      Identical to @ TT "subDGAlgebra(A, keepIdx)" @.
  Description
    Text
      Calls directly into @ TO subDGAlgebra @.  The two are
      interchangeable; @ TT "restrictDifferential" @ is provided so
      that calling code emphasizing the restricted-differential
      aspect reads more naturally.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      B = restrictDifferential(A, {0})
      numgens B.natural
  SeeAlso
    "Building DG algebras from existing DG algebras"
    subDGAlgebra
///

doc ///
  Key
    truncateGenerators
    (truncateGenerators, DGAlgebra, ZZ)
    [truncateGenerators, InitializeDegreeZeroHomology]
    [truncateGenerators, InitializeComplex]
  Headline
    Restrict a DG algebra to its generators of hom-degree strictly above a bound
  Usage
    B = truncateGenerators(A, n)
  Inputs
    A:DGAlgebra
    n:ZZ
      A hom-degree bound; generators of hom-degree @ TT "<= n" @ are
      dropped.
    InitializeDegreeZeroHomology => Boolean
      Default @ TT "true" @.
    InitializeComplex => Boolean
      Default @ TT "false" @.
  Outputs
    B:DGAlgebra
      The sub-DG algebra of @ TT "A" @ generated by the DG
      generators whose first-hom-degree component is strictly
      greater than @ TT "n" @.
  Description
    Text
      Computes @ TT "keepIdx" @ as
      @ TT "select(0 .. #A.Degrees - 1, i -> first A.Degrees#i > n)" @
      and delegates to @ TO subDGAlgebra @.  As with
      @ TO subDGAlgebra @, the subset must be d-closed; if the
      differential of a generator @ TT "T_i" @ of hom-degree
      @ TT "> n" @ uses a generator of hom-degree @ TT "<= n" @ that
      would be dropped, an error is raised.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      B = truncateGenerators(A, 0)
      numgens B.natural
    Text
      The Koszul algebra has all generators in hom-degree @ TT "1" @
      so truncating below degree @ TT "0" @ is a no-op in this
      case.
  SeeAlso
    "Building DG algebras from existing DG algebras"
    subDGAlgebra
///

doc ///
  Key
    killHomologyAtDegree
    (killHomologyAtDegree, DGAlgebra, ZZ)
  Headline
    Adjoin variables killing the homology of a DG algebra at a prescribed degree
  Usage
    B = killHomologyAtDegree(A, n)
  Inputs
    A:DGAlgebra
    n:ZZ
      The hom-degree at which to kill homology.
  Outputs
    B:DGAlgebra
      Either @ TT "A" @ itself (if @ TT "H_n(A) = 0" @) or a new DG
      algebra obtained from @ TT "A" @ by adjoining variables in
      hom-degree @ TT "n + 1" @ whose differentials are minimal
      representatives of @ TT "H_n(A)" @.
  Description
    Text
      Unlike @ TO killCycles @, which searches for the first
      nontrivial degree upward from @ TT "StartDegree" @, this
      method targets a specific degree.  It computes the minimal
      generators of @ TT "H_n(A)" @ using the pruning map of
      @ TT "prune homology(n, A)" @, lifts them to cycles in
      @ TT "A_n" @, and adjoins via @ TO adjoinVariables @.
    Example
      R = QQ[x, y]
      A = koszulComplexDGA R
      killHomologyAtDegree(A, 1) === A
    Text
      Over a regular ring, @ TT "H_1" @ vanishes already, so the
      method short-circuits.  Over a complete intersection, it
      adjoins new generators:
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      B = killHomologyAtDegree(A, 1)
      numgens B.natural > numgens A.natural
      prune homology(1, B)
  SeeAlso
    "Building DG algebras from existing DG algebras"
    killCycles
    adjoinVariables
    acyclicClosure
///

doc ///
  Key
    isValidDGAlgebra
    (isValidDGAlgebra, DGAlgebra)
  Headline
    Structural invariant check for a DG algebra
  Usage
    b = isValidDGAlgebra A
  Inputs
    A:DGAlgebra
  Outputs
    b:Boolean
      @ TT "true" @ when @ TT "A" @ has all required keys of the
      expected types (ring, natural algebra, degree list,
      differential list, cache table).
  Description
    Text
      A lightweight structural check, intended to guard
      downstream code against hand-assembled or corrupted
      @ TT "DGAlgebra" @ hash tables.  It does @ EM "not" @ check
      the @ TT "d^2 = 0" @ condition; for that, use
      @ TO isWellDefinedDifferential @ or the user-facing
      @ TO (isWellDefined, DGAlgebra) @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      isValidDGAlgebra A
  SeeAlso
    "Building DG algebras from existing DG algebras"
    isWellDefinedDifferential
    (isWellDefined, DGAlgebra)
///

doc ///
  Key
    isWellDefinedDifferential
    (isWellDefinedDifferential, DGAlgebra)
    (isWellDefinedDifferential, DGModule)
  Headline
    Semantic check that d^2 = 0 for a DG algebra or DG module
  Usage
    b = isWellDefinedDifferential A
    b = isWellDefinedDifferential M
  Inputs
    A:DGAlgebra
      A DG algebra.
    M:DGModule
      A DG module.
  Outputs
    b:Boolean
      @ TT "true" @ when the composition
      @ TT "d_{n-1} * d_n" @ is zero for every relevant
      @ TT "n" @.
  Description
    Text
      The @ EM "semantic" @ half of the well-definedness check: it
      verifies that the differential squares to zero on every
      generator.  The structural half
      (@ TO isValidDGAlgebra @, and the corresponding
      @ TT "isValidDGModule" @ for modules) is called first as a
      pre-condition.
    Text
      This is the primitive called internally by
      @ TO (isWellDefined, DGAlgebra) @ and the user-facing
      @ TO (isWellDefined, DGModule) @.  It is exported for
      low-level use when only the @ TT "d^2 = 0" @ half of
      well-definedness needs to be checked.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      isWellDefinedDifferential A
      M = freeDGModule(A, {0})
      isWellDefinedDifferential M
  SeeAlso
    "Building DG algebras from existing DG algebras"
    "Well-definedness, acyclicity, and quasi-isomorphism"
    isValidDGAlgebra
    (isWellDefined, DGAlgebra)
    (isWellDefined, DGModule)
///

doc ///
  Key
    "Low-level differential computations and validity checks"
  Headline
    Primitives behind DG algebra and DG module differentials
  Description
    Text
      The methods documented here implement the raw computations
      that higher-level DG algebra and DG module routines rely on.
      They split into three groups.

      The first group -- @ TO polyDifferential @ and
      @ TO polyDiffMonomial @ -- applies the Leibniz rule directly
      to elements of @ TT "A.natural" @ to produce the DG algebra
      differential.  @ TT "polyDifferential(n, A)" @ returns the
      per-degree matrix @ TT "A_n --> A_{n-1}" @ over the base ring
      and caches it inside @ TT "A" @; @ TT "polyDifferential(A, f)" @
      applies @ TT "d" @ to a single element by summing over its
      terms; @ TT "polyDiffMonomial(A, m)" @ is the monomial kernel
      that powers both.

      The second group -- @ TO dgComplex @ -- packages the per-degree
      differentials of a DG algebra or DG module as a
      @ TO2{Complex, "Complex"} @ of free modules over the base ring.
      The result is cached and invalidated by
      @ TO invalidateDGAlgebraCache @.

      The third group -- @ TO isValidDGModule @ and
      @ TO isDGSubmodule @ -- checks structural invariants.
      @ TT "isValidDGModule" @ parallels @ TO isValidDGAlgebra @ and
      confirms that the required keys are present with the expected
      types; @ TT "isDGSubmodule" @ tests whether a given inclusion
      matrix has @ TT "d" @-closed image without performing the
      @ TT "d" @-saturation that @ TO dgSubmodule @ would.

      Finally, @ TO2{(getBoundaryPreimage, DGModule, Vector), "getBoundaryPreimage"} @
      lifts boundaries through the module differential, returning a
      preimage when one exists and a residue otherwise.
  Subnodes
    polyDifferential
    polyDiffMonomial
    dgComplex
    isValidDGModule
    isDGSubmodule
    (getBoundaryPreimage, DGModule, Vector)
    (getBoundaryPreimage, DGModule, List)
  SeeAlso
    "Basic operations on DG Algebras"
    "Building DG modules, submodules, and quotients"
    "Well-definedness, acyclicity, and quasi-isomorphism"
///

doc ///
  Key
    polyDifferential
    (polyDifferential, ZZ, DGAlgebra)
    (polyDifferential, DGAlgebra, RingElement)
  Headline
    Matrix and element-level differentials of a DG algebra
  Usage
    d = polyDifferential(n, A)
    b = polyDifferential(A, f)
  Inputs
    n:ZZ
      A homological degree.
    A:DGAlgebra
    f:RingElement
      An element of @ TT "A.natural" @.
  Outputs
    d:Matrix
      The per-degree map @ TT "A_n --> A_{n-1}" @ over
      @ TT "A.ring" @ (first form).
    b:RingElement
      The image @ TT "d(f)" @ in @ TT "A.natural" @ (second form).
  Description
    Text
      The two-argument form @ TT "polyDifferential(n, A)" @ returns
      the matrix of the DG algebra differential in hom-degree
      @ TT "n" @, expressed in the monomial basis of @ TT "A_n" @
      and @ TT "A_{n-1}" @.  It is cached inside @ TT "A" @ and
      reused on subsequent calls; @ TO invalidateDGAlgebraCache @
      clears the cache.  Out-of-range degrees are handled
      uniformly: degree @ TT "0" @ gives the conventional zero map
      @ TT "R^1 --> R^0" @, and degrees past @ TT "maxDegree A" @
      give zero maps of the appropriate shape.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      d1 = polyDifferential(1, A)
      d2 = polyDifferential(2, A)
      d3 = polyDifferential(3, A)
      assert(d1 * d2 == 0)
      assert(d2 * d3 == 0)
    Text
      The zero-degree map is the conventional @ TT "R^1 --> R^0" @:
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      d0 = polyDifferential(0, A)
      assert(source d0 == R^1 and target d0 == R^0)
    Text
      The element form @ TT "polyDifferential(A, f)" @ applies
      @ TT "d" @ to an arbitrary element by summing over its
      monomial terms:
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      gs = gens A.natural
      polyDifferential(A, gs#0 * gs#1)
  SeeAlso
    "Low-level differential computations and validity checks"
    polyDiffMonomial
    (diff, DGAlgebra, RingElement)
    invalidateDGAlgebraCache
///

doc ///
  Key
    polyDiffMonomial
    (polyDiffMonomial, DGAlgebra, RingElement)
  Headline
    Leibniz rule on a single monomial of a DG algebra
  Usage
    b = polyDiffMonomial(A, m)
  Inputs
    A:DGAlgebra
    m:RingElement
      A monomial of @ TT "A.natural" @ (or the zero element).
  Outputs
    b:RingElement
      The image @ TT "d(m)" @ in @ TT "A.natural" @.
  Description
    Text
      @ TT "polyDiffMonomial" @ is the monomial kernel of the
      DG algebra differential.  It splits @ TT "m" @ into its
      coefficient and its support powers and then applies the
      graded Leibniz rule variable by variable, keeping track of
      the sign produced when @ TT "d" @ passes an odd-degree
      generator.  The routine handles the zero element without
      raising an exception, returning @ TT "0" @ in @ TT "A.natural" @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      assert(polyDiffMonomial(A, 0_(A.natural)) == 0_(A.natural))
      gs = gens A.natural
      polyDiffMonomial(A, gs#0 * gs#1)
    Text
      On a two-variable Koszul generator product, the Leibniz rule
      gives @ TT "d(T_1 T_2) = d(T_1) T_2 - T_1 d(T_2) = x T_2 - y T_1" @:
    Example
      r = polyDiffMonomial(A, gs#0 * gs#1)
      assert(r == x * gs#1 - y * gs#0)
  SeeAlso
    "Low-level differential computations and validity checks"
    polyDifferential
    (diff, DGAlgebra, RingElement)
///

doc ///
  Key
    dgComplex
    (dgComplex, DGAlgebra)
    (dgComplex, DGModule)
  Headline
    Package a DG algebra or DG module as a Complex of free base-ring modules
  Usage
    C = dgComplex A
    C = dgComplex M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    C:Complex
      A @ TO Complex @ of free @ TT "A.ring" @-modules whose
      @ TT "i" @-th module is @ TT "A_i" @ (resp.\ @ TT "M_i" @)
      and whose differential is @ TT "polyDifferential" @
      (resp.\ @ TO moduleDifferential @).
  Description
    Text
      @ TT "dgComplex" @ assembles the per-degree differentials of
      a DG algebra or DG module into a single
      @ TO2{Complex, "Complex"} @ object.  The result is cached on
      the input; calling @ TT "dgComplex" @ again returns exactly
      the same complex, and @ TO invalidateDGAlgebraCache @ clears
      the cache so that the next call rebuilds the complex.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      C1 = dgComplex A
      C2 = dgComplex A
      assert(C1 === C2)
      invalidateDGAlgebraCache A
      assert(not A.cache#?(symbol dgComplex))
      C3 = dgComplex A
      assert(instance(C3, Complex))
    Text
      When @ TT "maxDegree" @ is infinite -- for example on the
      acyclic closure of a non-regular ring -- @ TT "dgComplex" @
      truncates at a canonical finite bound so that a
      @ TO Complex @ is always produced.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = acyclicClosure(R, EndDegree => 2)
      assert(instance(dgComplex A, Complex))
    Text
      The DG module form is parallel: it builds the complex whose
      terms are @ TT "M_i" @ and whose differentials are the maps
      returned by @ TO moduleDifferential @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      KM = koszulComplexDGM R^1
      D1 = dgComplex KM
      D2 = dgComplex KM
      assert(D1 === D2)
  SeeAlso
    "Low-level differential computations and validity checks"
    polyDifferential
    moduleDifferential
    invalidateDGAlgebraCache
    toComplex
///

doc ///
  Key
    isValidDGModule
    (isValidDGModule, DGModule)
  Headline
    Structural-invariant check on a DG module
  Usage
    b = isValidDGModule M
  Inputs
    M:DGModule
  Outputs
    b:Boolean
      @ TT "true" @ if the stored @ TT "DGModule" @ has all
      required keys with the expected types; @ TT "false" @
      otherwise.
  Description
    Text
      @ TT "isValidDGModule" @ parallels @ TO isValidDGAlgebra @:
      it confirms that @ TT "M" @ carries the keys
      @ TT "dgAlgebra" @, @ TT "ring" @, @ TT "natural" @,
      @ TT "Degrees" @, and @ TT "diff" @ with the expected types,
      that the referenced @ TT "DGAlgebra" @ is itself structurally
      valid, and that the generator-degree list has the same length
      as the differential list.  It does not verify the semantic
      property @ TT "d_M^2 = 0" @; that is the job of
      @ TO isWellDefinedDifferential @, and the two together are
      what the user-facing @ TO (isWellDefined, DGModule) @ calls.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      KM = koszulComplexDGM R^1
      assert(isValidDGModule KM)
  SeeAlso
    "Low-level differential computations and validity checks"
    "Well-definedness, acyclicity, and quasi-isomorphism"
    isValidDGAlgebra
    isWellDefinedDifferential
    (isWellDefined, DGModule)
///

doc ///
  Key
    isDGSubmodule
    (isDGSubmodule, DGModule, Matrix)
  Headline
    Test whether the image of an inclusion matrix is d-closed
  Usage
    b = isDGSubmodule(M, incMat)
  Inputs
    M:DGModule
    incMat:Matrix
      A matrix whose target is @ TT "M.natural" @.
  Outputs
    b:Boolean
      @ TT "true" @ if the column span of @ TT "incMat" @ is
      closed under the differential of @ TT "M" @; @ TT "false" @
      otherwise.
  Description
    Text
      @ TT "isDGSubmodule" @ checks whether a matrix of candidate
      generators spans a DG submodule: it assembles
      @ TT "d_M" @ applied column-wise and then verifies that the
      result lies in the column span of @ TT "incMat" @ via a
      single @ TT "//" @ solve.  Unlike @ TO dgSubmodule @, it
      does not saturate: a submodule that is not @ TT "d" @-closed
      is reported as such rather than enlarged.  The empty matrix
      is always @ TT "d" @-closed.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      idM = identityDGModuleMap M
      imF = image idM
      assert(isDGSubmodule(target idM, (inclusion imF).natural))
      kerF = kernel idM
      assert(isDGSubmodule(source idM, (inclusion kerF).natural))
  SeeAlso
    "Low-level differential computations and validity checks"
    dgSubmodule
    (image, DGModuleMap)
    (kernel, DGModuleMap)
///

doc ///
  Key
    (getBoundaryPreimage, DGModule, Vector)
  Headline
    Lift a boundary in a DG module to a preimage under the differential
  Usage
    (lifted, myLift) = getBoundaryPreimage(M, b)
  Inputs
    M:DGModule
      A free DG module over a @ TO DGAlgebra @.
    b:Vector
      A homogeneous element of @ TT "M.natural" @.
  Outputs
    seq:Sequence
      A pair @ TT "(lifted, myLift)" @.  If @ TT "lifted" @ is
      @ TT "true" @, then @ TT "myLift" @ is an element with
      @ TT "d_M(myLift) = b" @; otherwise @ TT "myLift" @ is the
      residue @ TT "b - d_M(liftAttempt)" @, which records how far
      @ TT "b" @ fails to be a boundary.
  Description
    Text
      @ TT "getBoundaryPreimage" @ for DG modules is the module
      analogue of @ TO (getBoundaryPreimage, DGAlgebra, RingElement) @:
      it attempts to solve @ TT "d_M(x) = b" @ for @ TT "x" @, using
      the matrix @ TT "moduleDifferential(homDegree(b) + 1, M)" @
      and a single @ TT "//" @ solve.  Over a general ring such a
      solve may fail to find a preimage even when one exists, so a
      @ TT "true" @ answer is a guarantee but a @ TT "false" @
      answer only says "the particular preimage candidate we tried
      left a residue".  The returned residue is
      @ TT "b - d_M(liftAttempt)" @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      -- d(e_1) = x*e_0, so x*e_0 is a boundary with preimage e_1.
      b = x * natGens#0
      (ok, pre) = getBoundaryPreimage(M, b)
      assert(ok === true)
      assert(diff(M, pre) == b)
    Text
      On a non-boundary, the first coordinate is @ TT "false" @
      and the second coordinate records the residue:
    Example
      M2 = freeDGModule(A, {0})
      ng2 = apply(rank M2.natural, i -> (M2.natural)_i)
      (ok2, residue) = getBoundaryPreimage(M2, ng2#0)
      assert(ok2 === false)
      assert(residue != 0)
  SeeAlso
    "Low-level differential computations and validity checks"
    (getBoundaryPreimage, DGModule, List)
    getBoundaryPreimage
    moduleDifferential
    homologyClass
///

doc ///
  Key
    (getBoundaryPreimage, DGModule, List)
  Headline
    Lift a list of boundaries sharing a hom-degree through the module differential
  Usage
    (lifted, lifts) = getBoundaryPreimage(M, boundaryList)
  Inputs
    M:DGModule
      A free DG module over a @ TO DGAlgebra @.
    boundaryList:List
      A list of elements of @ TT "M.natural" @, each either
      @ TT "0" @ or homogeneous of a common hom-degree.
  Outputs
    seq:Sequence
      A pair @ TT "(lifted, lifts)" @.  If @ TT "lifted" @ is
      @ TT "true" @, then @ TT "lifts#i" @ satisfies
      @ TT "d_M(lifts#i) = boundaryList#i" @ for every @ TT "i" @;
      otherwise @ TT "lifts" @ is the list of per-entry residues.
  Description
    Text
      The list form of @ TO (getBoundaryPreimage, DGModule, Vector) @
      solves multiple preimage problems in a single
      @ TT "//" @ call.  Zero entries are permitted and contribute
      zero columns; all nonzero entries must share a hom-degree.
      When every boundary lifts, the returned list preserves the
      order of @ TT "boundaryList" @.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      b = x * natGens#0
      (ok, lifts) = getBoundaryPreimage(M, {b, 0_(M.natural)})
      assert(ok === true)
      assert(diff(M, lifts#0) == b)
      assert(diff(M, lifts#1) == 0)
  SeeAlso
    "Low-level differential computations and validity checks"
    (getBoundaryPreimage, DGModule, Vector)
    getBoundaryPreimage
    moduleDifferential
///

doc ///
  Key
    "Accessors and cache management"
  Headline
    Reading state out of a DG algebra or DG module and maintaining its caches
  Description
    Text
      A @ TO DGAlgebra @ or @ TO DGModule @ stores its defining
      data in a small collection of keys -- the base ring, the
      graded-commutative algebra, the generator degrees, and the
      list of generator differentials -- together with a cache of
      derived information (per-degree differential matrices,
      homology, the homology algebra, a @ TO Complex @
      presentation).  This page collects the two groups of
      utilities that manage that state.

      The first group -- @ TO underlyingRing @,
      @ TO underlyingAlgebra @, @ TO differential @, and
      @ TO generatorDegrees @ -- returns the four defining
      ingredients.  Because both DG algebras and DG modules carry
      the same names, code can remain agnostic about which one it
      is handling.

      The second group -- @ TO ensureDGAlgebraCaches @ and
      @ TO invalidateDGAlgebraCache @ -- maintains the cache
      sub-tables.  @ TT "ensureDGAlgebraCaches" @ is idempotent
      and guarantees that the standard cache keys are present
      with the expected types; @ TO invalidateDGAlgebraCache @
      wipes every cached computed value so that the next call
      recomputes from scratch.  Routines such as @ TO setDiff @
      that mutate the differential call
      @ TO invalidateDGAlgebraCache @ internally so that stale
      homology and stale differential matrices are never served.
  Subnodes
    underlyingRing
    underlyingAlgebra
    differential
    generatorDegrees
    ensureDGAlgebraCaches
    invalidateDGAlgebraCache
  SeeAlso
    "Basic operations on DG Algebras"
    "Building DG modules, submodules, and quotients"
    "Low-level differential computations and validity checks"
///

doc ///
  Key
    underlyingRing
    (underlyingRing, DGAlgebra)
    (underlyingRing, DGModule)
  Headline
    The commutative base ring of a DG algebra or DG module
  Usage
    S = underlyingRing A
    S = underlyingRing M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    S:Ring
      The commutative base ring @ TT "A.ring" @ or @ TT "M.ring" @
      over which the DG structure is defined.
  Description
    Text
      @ TT "underlyingRing" @ returns the base ring.  This is the
      ring @ TT "R" @ in the DG algebra @ TT "A = R\\langle T_1, T_2, \\ldots \\rangle" @
      or the DG module @ TT "M" @, and it is the ring over which
      every matrix returned by @ TO polyDifferential @ or
      @ TO moduleDifferential @ is built.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      assert(underlyingRing A === R)
    Example
      M = koszulComplexDGM R^1
      assert(underlyingRing M === R)
  SeeAlso
    "Accessors and cache management"
    underlyingAlgebra
    differential
    generatorDegrees
///

doc ///
  Key
    underlyingAlgebra
    (underlyingAlgebra, DGAlgebra)
    (underlyingAlgebra, DGModule)
  Headline
    The graded-commutative algebra carrying the DG structure
  Usage
    N = underlyingAlgebra A
    N = underlyingAlgebra M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    N:Ring
      The graded-commutative ring @ TT "A.natural" @
      (the DG algebra's own ring) or, for a DG module,
      the graded free module @ TT "M.natural" @
      (as a @ TO Module @ over @ TT "A.natural" @).
  Description
    Text
      For a @ TO DGAlgebra @, @ TT "underlyingAlgebra" @ returns the
      graded-commutative ring @ TT "A.natural" @ -- the ring whose
      elements one manipulates when writing polynomials in the DG
      generators.  For a @ TO DGModule @ it returns the underlying
      @ TT "A.natural" @-module @ TT "M.natural" @ on which the
      differential acts.  In both cases the result is what you
      typically @ TT "use" @ before referring to DG generators by
      name.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      assert(underlyingAlgebra A === A.natural)
      numgens underlyingAlgebra A
    Example
      M = koszulComplexDGM R^1
      assert(underlyingAlgebra M === M.natural)
  SeeAlso
    "Accessors and cache management"
    underlyingRing
    differential
    generatorDegrees
///

doc ///
  Key
    differential
    (differential, DGAlgebra)
    (differential, DGModule)
  Headline
    The list of generator differentials stored in a DG algebra or DG module
  Usage
    d = differential A
    d = differential M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    d:List
      The stored list of differentials: for a @ TO DGAlgebra @,
      one element of @ TT "A.natural" @ per DG generator; for a
      @ TO DGModule @, one element of @ TT "M.natural" @ per
      natural generator.
  Description
    Text
      @ TT "differential" @ returns the raw list of generator
      differentials that was installed by @ TO setDiff @.  The
      @ TT "i" @-th entry is the image of the @ TT "i" @-th DG
      generator under @ TT "d" @, and every other differential is
      computed from it by linearity and the graded Leibniz rule.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      differential A
      assert(differential A === A.diff)
    Example
      M = koszulComplexDGM R^1
      differential M
  SeeAlso
    "Accessors and cache management"
    setDiff
    polyDifferential
    moduleDifferential
///

doc ///
  Key
    generatorDegrees
    (generatorDegrees, DGAlgebra)
    (generatorDegrees, DGModule)
  Headline
    The hom-degrees (and optional internal degrees) of the DG generators
  Usage
    degs = generatorDegrees A
    degs = generatorDegrees M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    degs:List
      A list of degree vectors, one per DG generator (for a DG
      algebra) or one per natural generator (for a DG module).
  Description
    Text
      @ TT "generatorDegrees" @ returns the degree data that was
      supplied at construction.  Each entry is a list whose first
      coordinate is the homological degree and whose remaining
      coordinates record the internal (multi-)grading of the base
      ring.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      generatorDegrees A
    Example
      M = freeDGModule(A, {0, 1, 3})
      generatorDegrees M
  SeeAlso
    "Accessors and cache management"
    underlyingRing
    underlyingAlgebra
    differential
///

doc ///
  Key
    ensureDGAlgebraCaches
    (ensureDGAlgebraCaches, DGAlgebra)
    (ensureDGAlgebraCaches, DGModule)
  Headline
    Guarantee that the standard cache sub-tables are present
  Usage
    A = ensureDGAlgebraCaches A
    M = ensureDGAlgebraCaches M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    :{DGAlgebra,DGModule}
      The same object, with any missing standard cache sub-table
      created (the method mutates the input and returns it).
  Description
    Text
      @ TT "ensureDGAlgebraCaches" @ is idempotent: it creates the
      cache sub-tables that the rest of the package expects (the
      @ TT "homology" @, @ TT "homologyAlgebra" @, and
      @ TT "diffs" @ sub-tables for a DG algebra; the @ TT "diffs" @
      sub-table for a DG module), and leaves any that already exist
      untouched.  Code that reads from the cache can call this
      method once up front and thereafter assume the sub-tables are
      present with the right types, rather than defensively
      checking each access.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      ensureDGAlgebraCaches A
      assert(instance(A.cache, CacheTable))
      -- idempotent: calling twice leaves the cache intact
      ensureDGAlgebraCaches A
      assert(instance(A.cache, CacheTable))
    Example
      KM = koszulComplexDGM R^1
      ensureDGAlgebraCaches KM
      assert(instance(KM.cache, CacheTable))
  SeeAlso
    "Accessors and cache management"
    invalidateDGAlgebraCache
///

doc ///
  Key
    invalidateDGAlgebraCache
    (invalidateDGAlgebraCache, DGAlgebra)
    (invalidateDGAlgebraCache, DGModule)
  Headline
    Discard cached derived data so that it is recomputed from scratch
  Usage
    A = invalidateDGAlgebraCache A
    M = invalidateDGAlgebraCache M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    :{DGAlgebra,DGModule}
      The same object with every cached derived value cleared
      (the method mutates the input and returns it).
  Description
    Text
      @ TT "invalidateDGAlgebraCache" @ wipes every cache entry
      that records a value derived from the differential: per-degree
      differential matrices, per-degree homology, the homology
      algebra, and any @ TO Complex @ or block-diff summary cached
      by @ TO dgComplex @ or @ TO displayBlockDiff @.  The cache
      sub-tables themselves are recreated empty, so subsequent
      lookups recompute the values rather than returning stale
      results.  Routines that mutate the differential (most
      notably @ TO setDiff @) call this function internally.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      C1 = dgComplex A
      invalidateDGAlgebraCache A
      assert(not A.cache#?(symbol dgComplex))
      C2 = dgComplex A
      assert(instance(C2, Complex))
    Example
      KM = koszulComplexDGM R^1
      D1 = dgComplex KM
      invalidateDGAlgebraCache KM
      assert(not KM.cache#?(symbol dgComplex))
      D2 = dgComplex KM
      assert(instance(D2, Complex))
  SeeAlso
    "Accessors and cache management"
    ensureDGAlgebraCaches
    setDiff
    dgComplex
///

doc ///
  Key
    "DG module primitives and chain-complex export"
  Headline
    Installing differentials, reading degree data, and exporting to Complex
  Description
    Text
      This page collects the remaining DG module primitives
      (installing a differential, asking for the largest
      hom-degree, reading off a per-degree basis) together with
      the methods that export a DG algebra, DG module, DG module
      map, or DG ideal to the ordinary
      @ TO2{Complex, "Complex"} @ / @ TO2{ComplexMap, "ComplexMap"} @
      world.

      Installing a differential.  @ TO (setDiff, DGModule, List) @
      is the DG module analogue of @ TO (setDiff, DGAlgebra, List) @:
      it records the image of each natural generator under
      @ TT "d_M" @ and invalidates every cached derived value.

      Reading degree data.  @ TO (maxDegree, DGModule) @ and
      @ TO (maxDegree, DGQuotientModule) @ report the largest
      hom-degree present (or @ TT "infinity" @ if the underlying
      DG algebra has an even-degree generator).
      @ TO (getBasis, ZZ, DGModule) @ returns an
      @ TT "A.ring" @-basis of the hom-degree-@ TT "n" @ piece of
      @ TT "M.natural" @.

      Exporting to @ TO Complex @.  The @ TO toComplex @ family
      packages per-degree differentials as a plain
      @ TO Complex @ of free base-ring modules; the
      @ TO toComplexMap @ family packages per-degree induced maps
      as a @ TO ComplexMap @.  Together they let the chain-level
      results of the DG machinery be handed off to the rest of
      the @ TO2{Complexes, "Complexes"} @ package.

      Block-diagonal display.  @ TO displayBlockDiff @ prints the
      per-internal-multidegree block decomposition of a DG
      algebra differential at a given homological degree,
      convenient for inspecting the acyclic closure of a
      multigraded quotient.
  Subnodes
    (setDiff, DGModule, List)
    (maxDegree, DGModule)
    (maxDegree, DGQuotientModule)
    (getBasis, ZZ, DGModule)
    (toComplex, DGModule)
    (toComplex, DGModule, ZZ)
    (toComplex, DGQuotientModule)
    (toComplex, DGIdeal)
    (toComplexMap, DGModuleMap)
    (toComplexMap, DGModuleMap, ZZ)
    displayBlockDiff
  SeeAlso
    "Basic operations on DG Algebras"
    "Building DG modules, submodules, and quotients"
    "Low-level differential computations and validity checks"
///

doc ///
  Key
    (setDiff, DGModule, List)
  Headline
    Install the list of generator differentials on a free DG module
  Usage
    setDiff(M, diffList)
  Inputs
    M:DGModule
      A free DG module (from @ TO freeDGModule @).
    diffList:List
      One entry per natural generator of @ TT "M" @.  Each entry
      is a vector of @ TT "M.natural" @ (or @ TT "0" @, or a
      one-column matrix that will be folded into a vector).
  Outputs
    :DGModule
      The same @ TT "M" @, mutated to carry the new differentials.
  Description
    Text
      @ TT "setDiff" @ is the DG module analogue of
      @ TO (setDiff, DGAlgebra, List) @: it records the image of
      the @ TT "i" @-th natural generator under the differential in
      the @ TT "i" @-th slot of @ TT "M.diff" @, and clears every
      cached derived value so that later calls to
      @ TO moduleDifferential @ or @ TO homology @ will recompute
      from the newly installed data.  Only free DG modules (those
      produced by @ TO freeDGModule @) are accepted; the list
      length must match the number of natural generators.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x^2 * natGens#0})
      d1 = moduleDifferential(1, M)
      d2 = moduleDifferential(2, M)
      assert(d1 * d2 == 0)
  SeeAlso
    "DG module primitives and chain-complex export"
    freeDGModule
    (setDiff, DGAlgebra, List)
    moduleDifferential
    invalidateDGAlgebraCache
///

doc ///
  Key
    (maxDegree, DGModule)
  Headline
    Largest hom-degree present in a DG module
  Usage
    n = maxDegree M
  Inputs
    M:DGModule
  Outputs
    n:{ZZ,InfiniteNumber}
      @ TT "maxDegree A + max(first d | d in M.Degrees)" @ when
      @ TT "A" @ has finite max hom-degree; @ TT "infinity" @
      otherwise.
  Description
    Text
      @ TT "maxDegree" @ of a DG module combines the hom-degree
      bound of the ambient @ TO DGAlgebra @ with the largest
      hom-degree shift of a natural generator.  When the
      underlying DG algebra has an even-degree generator it has
      @ TT "infinity" @ as its max degree, and so does every DG
      module over it.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      KM = koszulComplexDGM R^1
      assert(maxDegree KM == maxDegree KM.dgAlgebra)
    Example
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 3})
      maxDegree M
  SeeAlso
    "DG module primitives and chain-complex export"
    maxDegree
    (maxDegree, DGQuotientModule)
    (maxDegree, DGAlgebra)
///

doc ///
  Key
    (maxDegree, DGQuotientModule)
  Headline
    Largest hom-degree present in a DG quotient module
  Usage
    n = maxDegree Q
  Inputs
    Q:DGQuotientModule
  Outputs
    n:{ZZ,InfiniteNumber}
      The @ TT "maxDegree" @ of the ambient DG module
      @ TT "Q.ambient" @.
  Description
    Text
      Passing a DG quotient module to @ TO maxDegree @ returns
      the max hom-degree of its ambient DG module.  Because
      taking a quotient cannot introduce elements of higher
      hom-degree than are already in the ambient, the ambient's
      bound is valid for the quotient.  The value is what
      @ TO (toComplex, DGQuotientModule) @ and
      @ TO (toComplexMap, DGModuleMap) @ use to decide where to
      truncate when no @ TO EndDegree @ is supplied.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      z = zeroDGModuleMap(M, M)
      Q = cokernel z
      assert(maxDegree Q == maxDegree M)
  SeeAlso
    "DG module primitives and chain-complex export"
    maxDegree
    (maxDegree, DGModule)
    (toComplex, DGQuotientModule)
///

doc ///
  Key
    (getBasis, ZZ, DGModule)
  Headline
    Basis of the hom-degree-n piece of a DG module
  Usage
    b = getBasis(n, M)
  Inputs
    n:ZZ
      A homological degree.
    M:DGModule
  Outputs
    b:Matrix
      A one-row matrix over @ TT "M.natural" @ whose entries are
      the basis elements of @ TT "F_n(M)" @, the hom-degree-@ TT "n" @
      piece of @ TT "M.natural" @ over @ TT "A.ring" @.
  Description
    Text
      @ TT "getBasis(n, M)" @ delegates to
      @ TO2{(basis, ZZ, Module), "basis"} @ on the underlying
      @ TT "A.natural" @-module @ TT "M.natural" @, restricting to
      the variables of @ TT "A.natural" @.  The result is the
      piece of @ TT "M" @ on which @ TT "moduleDifferential(n, M)" @
      acts as its source-side basis.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      KM = koszulComplexDGM R^1
      b = getBasis(1, KM)
      assert(instance(b, Matrix))
      assert(numcols b == 2)
  SeeAlso
    "DG module primitives and chain-complex export"
    getBasis
    (getBasis, ZZ, DGAlgebra)
    moduleDifferential
///

doc ///
  Key
    (toComplex, DGModule)
  Headline
    Export a DG module to a Complex of free base-ring modules
  Usage
    C = toComplex M
  Inputs
    M:DGModule
  Outputs
    C:Complex
      A @ TO Complex @ of free @ TT "A.ring" @-modules whose
      @ TT "i" @-th differential is @ TT "moduleDifferential(i+1, M)" @.
  Description
    Text
      @ TT "toComplex" @ packages the per-degree module
      differentials as an ordinary @ TO Complex @ so that the
      result can flow through the rest of the
      @ TO2{Complexes, "Complexes"} @ package.  The one-argument
      form uses @ TT "maxDegree M" @ as the truncation bound and
      errors out if that is @ TT "infinity" @; use
      @ TO (toComplex, DGModule, ZZ) @ to supply an explicit bound.
      The result is not cached by this method (use
      @ TO dgComplex @ for a cached variant).
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      KM = koszulComplexDGM R^1
      C = toComplex KM
      assert(instance(C, Complex))
      assert(C.dd_1 == moduleDifferential(1, KM))
      assert(C.dd_2 == moduleDifferential(2, KM))
  SeeAlso
    "DG module primitives and chain-complex export"
    toComplex
    (toComplex, DGModule, ZZ)
    (toComplex, DGAlgebra)
    dgComplex
    moduleDifferential
///

doc ///
  Key
    (toComplex, DGModule, ZZ)
  Headline
    Export a DG module to a Complex with an explicit hom-degree bound
  Usage
    C = toComplex(M, n)
  Inputs
    M:DGModule
    n:ZZ
      A hom-degree upper bound.
  Outputs
    C:Complex
      A @ TO Complex @ of free @ TT "A.ring" @-modules, truncated at
      hom-degree @ TT "n" @.
  Description
    Text
      The explicit-bound form of @ TO (toComplex, DGModule) @.
      Supplying a bound is required when the ambient DG algebra has
      infinite hom-degree (as for an @ TO acyclicClosure @), and
      convenient when a low bound suffices.
    Example
      R = QQ[x, y, z]
      KM = koszulComplexDGM R^1
      C = toComplex(KM, 2)
      assert(instance(C, Complex))
      assert(max C == 2)
  SeeAlso
    "DG module primitives and chain-complex export"
    toComplex
    (toComplex, DGModule)
    dgComplex
    moduleDifferential
///

doc ///
  Key
    (toComplex, DGQuotientModule)
  Headline
    Export a DG quotient module to a Complex
  Usage
    C = toComplex Q
  Inputs
    Q:DGQuotientModule
  Outputs
    C:Complex
      The cokernel of the chain map induced by the inclusion
      @ TT "Q.subDGModule.inclusion" @; equivalently, the
      per-degree quotient complex of the ambient.
  Description
    Text
      For a DG quotient module @ TT "Q = M / S" @,
      @ TT "toComplex Q" @ builds the cokernel at the
      @ TO Complex @ level: it exports the ambient DG module
      @ TT "M" @ via @ TO (toComplex, DGModule) @, exports the
      inclusion of @ TT "S" @ via @ TO (toComplexMap, DGModuleMap) @,
      and takes the cokernel of the resulting @ TO ComplexMap @.
      If @ TT "Q" @ was previously passed through
      @ TO (prune, DGQuotientModule) @ the cached pruned complex
      is returned instead.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      z = zeroDGModuleMap(M, M)
      Q = cokernel z
      CM = toComplex M
      CQ = toComplex Q
      assert(rank CM_0 == rank CQ_0)
      assert(rank CM_1 == rank CQ_1)
  SeeAlso
    "DG module primitives and chain-complex export"
    (toComplex, DGModule)
    (toComplexMap, DGModuleMap)
    (prune, DGQuotientModule)
///

doc ///
  Key
    (toComplex, DGIdeal)
  Headline
    Export a DG ideal to a Complex (not yet implemented)
  Usage
    C = toComplex I
  Inputs
    I:DGIdeal
  Outputs
    C:Complex
  Description
    Text
      @ TT "toComplex" @ on a @ TO DGIdeal @ is not yet
      implemented.  The natural definition views @ TT "I" @ as a
      DG submodule of the regular DG module
      @ TT "A.natural" @ over @ TT "A" @, but a canonical wrapper
      for the regular-representation view of @ TT "A" @ as a DG
      module over itself is not yet in the package, so the method
      raises an informative error pointing at the currently
      available alternatives.

      Use @ TT "toComplex(ambient I / I)" @ to obtain the
      @ TO Complex @ of the quotient DG algebra, or read the
      underlying data via @ TT "ideal I" @ and @ TT "gens I" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      T = (gens A.natural)#0
      I = dgIdeal(A, {T})
      errored = try (toComplex I; false) else true
      assert(errored)
  SeeAlso
    "DG module primitives and chain-complex export"
    "Operations on DG Ideals"
    DGIdeal
    dgIdeal
    (symbol /, DGAlgebra, DGIdeal)
///

doc ///
  Key
    (toComplexMap, DGModuleMap)
  Headline
    Export a DG module map to a ComplexMap
  Usage
    cm = toComplexMap f
  Inputs
    f:DGModuleMap
      A DG module map @ TT "M --> N" @.
  Outputs
    cm:ComplexMap
      The induced @ TO ComplexMap @
      @ TT "toComplex M --> toComplex N" @.
  Description
    Text
      @ TT "toComplexMap f" @ assembles the per-degree pieces of
      @ TT "f" @ into a @ TO ComplexMap @.  When either side has
      infinite hom-degree, an @ TO EndDegree @ bound must be
      supplied; the option @ TO AssertWellDefined @ (default
      @ TT "true" @) causes @ TT "isWellDefined f" @ to be checked
      up front.  When the target is a DG quotient module only the
      canonical projection is supported; the result is the induced
      quotient chain map.  Use
      @ TO (toComplexMap, DGModuleMap, ZZ) @ to extract a single
      per-degree component instead.
    Example
      R = QQ[a, b] / ideal(a^2, b^2)
      A = koszulComplexDGA R
      phi = dgAlgebraMap(A, A, matrix {gens A.natural})
      cm = toComplexMap phi
      assert(instance(cm, ComplexMap))
  SeeAlso
    "DG module primitives and chain-complex export"
    toComplexMap
    (toComplexMap, DGModuleMap, ZZ)
    (toComplexMap, DGAlgebraMap)
    (toComplex, DGModule)
    identityDGModuleMap
///

doc ///
  Key
    (toComplexMap, DGModuleMap, ZZ)
  Headline
    The per-hom-degree component of a DG module map as a matrix
  Usage
    m = toComplexMap(f, n)
  Inputs
    f:DGModuleMap
      A DG module map @ TT "M --> N" @.
    n:ZZ
      A homological degree.
  Outputs
    m:Matrix
      The hom-degree-@ TT "n" @ component
      @ TT "F_n(M) --> F_n(N)" @ as an @ TT "A.ring" @-linear
      matrix.
  Description
    Text
      Builds the @ TT "A.ring" @-linear matrix that @ TT "f" @
      induces on the hom-degree-@ TT "n" @ pieces of
      @ TT "M.natural" @ and @ TT "N.natural" @.  This is the
      per-degree primitive used by
      @ TO (toComplexMap, DGModuleMap) @ when assembling a full
      @ TO ComplexMap @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      Mdg = minimalSemifreeResolution(koszulComplexDGA R, R^1 / ideal(x, y), EndDegree => 3)
      idM = identityDGModuleMap Mdg
      all(0..3, n -> (
          cm := toComplexMap(idM, n);
          source cm == target cm and cm == id_(source cm)
          ))
  SeeAlso
    "DG module primitives and chain-complex export"
    toComplexMap
    (toComplexMap, DGModuleMap)
    (toComplex, DGModule)
    identityDGModuleMap
///

doc ///
  Key
    killCycles
    (killCycles,DGAlgebra)
  Headline
    Adjoin variables to kill non-bounding cycles in the lowest positive degree
  Usage
    B = killCycles A
  Inputs
    A:DGAlgebra
  Outputs
    B:DGAlgebra
      a new DG algebra with extra generators adjoined to make every cycle in the
      first positive degree where @ TT "HH_i A" @ is nonzero into a boundary
  Description
    Text
      This is the basic building block of Tate's acyclic closure construction.
      Given a DG algebra @ TT "A" @, @ TT "killCycles" @ locates the smallest positive
      homological degree @ TT "i" @ in which @ TT "HH_i A" @ is nonzero, picks a
      generating set of cycles, and adjoins new DG algebra generators in degree
      @ TT "i+1" @ mapping to those cycles.  The result is a DG algebra whose
      homology agrees with @ TT "A" @ below degree @ TT "i" @ and is killed in
      degree @ TT "i" @.  Iterating this procedure produces the acyclic closure.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3, b^3, c^3 - d^4}
      A = koszulComplexDGA R
      A.diff
      B = killCycles A
      B.diff
    Text
      The new generator @ TT "T_(2,1)" @ in @ TT "B" @ has differential equal to
      a chosen nonzero cycle representative in homological degree 1 of @ TT "A" @.
  SeeAlso
    DGAlgebra
    acyclicClosure
    adjoinVariables
///

doc ///
  Key
    adjoinVariables
    (adjoinVariables,DGAlgebra,List)
  Headline
    Extend a DG algebra by a user-chosen list of cycles to be killed
  Usage
    B = adjoinVariables(A, cycleList)
    B = adjoinVariables(A, cycleList, Variable => "U")
  Inputs
    A:DGAlgebra
    cycleList:List
      Homogeneous cycles of @ TT "A" @, i.e.\ elements @ TT "z" @ in
      @ TT "A.natural" @ with @ TT "(A.diff)(z) = 0" @ (no check is
      performed).  Each cycle must have the same multi-degree shape as
      @ TT "A.Degrees" @.
    Variable => String
      Base name for the newly adjoined generators.  Defaults to the base
      symbol already used by @ TT "A" @, or @ TT "\"T\"" @ if @ TT "A" @
      has no generators.
  Outputs
    B:DGAlgebra
      A semifree extension of @ TT "A" @ with one new generator per entry
      of @ TT "cycleList" @.  The new generator for a cycle @ TT "z" @ of
      hom-degree @ TT "n" @ has hom-degree @ TT "n + 1" @ and differential
      @ TT "z" @.
  Description
    Text
      Whereas @ TO killCycles @ chooses representatives of a basis of the
      first nonzero homology of @ TT "A" @ automatically,
      @ TT "adjoinVariables" @ lets the user specify exactly which cycles
      become boundaries.  This is the primitive used by
      @ TO acyclicClosure @, @ TO killCycles @, and @ TO minimalModel @,
      but can also be called directly to build DG algebra resolutions by
      hand.
    Text
      @ BOLD "Variable convention." @  The new generators extend the
      existing doubly-indexed naming scheme @ TT "base_(i, j)" @: for
      each adjoined cycle of hom-degree @ TT "n" @, a new generator named
      @ TT "base_(n+1, k)" @ is created, where @ TT "k" @ continues the
      1-indexed counter already in use at hom-degree @ TT "n + 1" @ in
      @ TT "A" @.  Existing generator names are preserved verbatim.
      Passing @ TT "Variable => \"U\"" @ switches the base symbol for
      @ EM "the new generators only" @:
    Example
      R = ZZ/101[a, b, c] / ideal(a^3, b^3, c^3)
      A = koszulComplexDGA R
      z = a^2 * (gens A.natural)_0
      polyDifferential(A, z) == 0
      B = adjoinVariables(A, {z})
      gens B.natural
      B' = adjoinVariables(A, {z}, Variable => "U")
      gens B'.natural
    Text
      Killing a single cycle lowers the size of the corresponding
      homology:
    Example
      apply(3, i -> numgens prune homology(i, A))
      apply(3, i -> numgens prune homology(i, B))
    Text
      Over a non-c.i.\ example with a non-regular relation, one can
      inspect the first homology, pick a representative, and kill just
      that class (leaving other @ TT "H_1" @ classes intact):
    Example
      S = ZZ/101[a, b, c, d] / ideal(a^3, b^3, c^3 - d^4)
      AS = koszulComplexDGA S
      prune homology(1, AS)
      BS = adjoinVariables(AS, {a^2 * (gens AS.natural)_0})
      prune homology(1, BS)
  Caveat
    The package does @ EM "not" @ verify that the supplied elements are
    actually cycles; passing a non-cycle produces a malformed DG algebra.
    Use @ TT "polyDifferential(A, z) == 0" @ to check before calling
    (the shorthand @ TT "(A.diff)(z)" @ applies @ TT "A.diff" @ as a
    ring map, which only agrees with the differential on monomials that
    contain at most one algebra generator and hence is @ EM "not" @ a
    valid cycle check for products).
  SeeAlso
    DGAlgebra
    killCycles
    acyclicClosure
    minimalModel
    setDiff
///

doc ///
  Key
    homologyAlgebra
    (homologyAlgebra,DGAlgebra)
  Headline
    Compute the homology algebra of a DGAlgebra.
  Usage
    HA = homologyAlgebra(A)
  Inputs
    A:DGAlgebra
  Outputs
    HA:Ring
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      A = koszulComplexDGA(R)
      apply(maxDegree A + 1, i -> numgens prune homology(i,A))
      HA = homologyAlgebra(A)
    Text
      Note that HA is a graded commutative polynomial ring (i.e. an exterior algebra) since R is a complete intersection.
    Example  
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4,a^3*b^3*c^3*d^3}
      A = koszulComplexDGA(R)
      apply(maxDegree A + 1, i -> numgens prune homology(i,A))
      HA = homologyAlgebra(A)
      numgens HA
      HA.cache.cycles
    Example
      Q = ZZ/101[x,y,z]
      I = ideal{y^3,z*x^2,y*(z^2+y*x),z^3+2*x*y*z,x*(z^2+y*x),z*y^2,x^3,z*(z^2+2*x*y)}
      R = Q/I
      A = koszulComplexDGA(R)
      apply(maxDegree A + 1, i -> numgens prune homology(i,A))
      HA = homologyAlgebra(A)
    Text
      One can check that HA has Poincare duality since R is Gorenstein.
    Text
      If your DGAlgebra has generators in even degrees, then one must specify the options GenDegreeLimit and RelDegreeLimit.
    Example
      R = ZZ/101[a,b,c,d]
      S = R/ideal{a^4,b^4,c^4,d^4}
      A = acyclicClosure(R,EndDegree=>3)
      B = A ** S
      HB = homologyAlgebra(B,GenDegreeLimit=>7,RelDegreeLimit=>14)
///

doc ///
  Key
    (homology,DGAlgebra)
  Headline
    The homology algebra of a DG algebra
  Usage
    HA = homology A
    HA = HH A
  Inputs
    A:DGAlgebra
  Outputs
    HA:Ring
      the homology algebra @ TT "HH_*(A)" @, as a graded-commutative ring
  Description
    Text
      Returns the homology algebra of @ TT "A" @, caching the result inside @ TT "A" @
      so subsequent calls are fast.  This is a convenience wrapper around
      @ TO homologyAlgebra @ that supports the @ TT "HH" @ shorthand.
    Example
      R = ZZ/101[a,b,c] / ideal(a^3, b^3, c^3, a^2*b^2*c^2)
      A = koszulComplexDGA R
      HA = HH A
      numgens HA
      apply(maxDegree A + 1, i -> numgens prune homology(i, A))
    Text
      For DG algebras with even-degree generators (such as acyclic closures of non-regular
      rings), call @ TO homologyAlgebra @ directly and supply
      @ TO GenDegreeLimit @ and @ TO RelDegreeLimit @ explicitly.
  SeeAlso
    homologyAlgebra
    zerothHomology
    (homology, ZZ, DGAlgebra)
///

doc ///
  Key
    torAlgebra
    (torAlgebra,Ring)
  Headline
    Computes the Tor algebra of a ring
  Usage
    torR = torAlgebra(R)
  Inputs
    R:Ring
  Outputs
    torR:Ring
  Description
    Example
      R = ZZ/101[a,b,c,d]
      TorR = torAlgebra(R)
      S = R/ideal{a^3,b^3,c^3,d^5}
      TorS = torAlgebra(S, GenDegreeLimit => 3)
    Text
      The above example calculates the Tor algebra of R and S up to degree 3, by default.  One can also specify the maximum degree
      to compute generators of the Tor algebra by specifying the GenDegreeLimit option.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3,a^2*b^2*c^3*d^2}
      TorR = torAlgebra(R,GenDegreeLimit=>5)
///

doc ///
  Key
    (torAlgebra,Ring,Ring)
  Headline
    Computes Tor_R(S,k) up to a specified generating and relating degree.
  Usage
    TorRS = torAlgebra(R,S,GenDegreeLimit=>m,RelDegreeLimit=>n)
  Inputs
    R:Ring
    S:Ring
  Outputs
    TorRS:Ring
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      M = coker matrix {{a^3*b^3*c^3*d^3}};
      S = R/ideal{a^3*b^3*c^3*d^3}
      HB = torAlgebra(R,S,GenDegreeLimit=>4,RelDegreeLimit=>8)
      numgens HB
      apply(5,i -> #(flatten entries getBasis(i,HB)))      
      Mres = freeResolution(M, LengthLimit=>8)
    Text
      Note that in this example, $Tor_*^R(S,k)$ has trivial multiplication, since the
      map from R to S is a Golod homomorphism by a theorem of Levin and Avramov.
///

doc ///
  Key
    maxDegree
    (maxDegree,DGAlgebra)
  Headline
    Computes the maximum homological degree of a DGAlgebra
  Usage
    mDegree = maxDegree(A)
  Inputs
    A:DGAlgebra
  Outputs
    mDegree:ZZ
      The maximum degree of the DGAlgebra A (this can be infinite).
  Description
    Text
      Note that if the DGAlgebra A has any generators of even degree, then maxDegree returns infinity.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
      A = koszulComplexDGA(R)
      B = acyclicClosure(A,EndDegree=>3)
      maxDegree(A)
      maxDegree(B)
///

doc ///
  Key
    isHomologyAlgebraTrivial
    (isHomologyAlgebraTrivial,DGAlgebra)
  Headline
    Determines if the homology algebra of a DGAlgebra is trivial
  Usage
    isTriv = isHomologyAlgebraTrivial(A) 
  Inputs
    A:DGAlgebra
  Outputs
    isTriv:Boolean
  Description
    Text
      This function computes the homology algebra of the DGAlgebra A and determines if the multiplication on H(A) is trivial.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      S = R/ideal{a^3*b^3*c^3*d^3}
      A = acyclicClosure(R,EndDegree=>3)
      B = A ** S
      isHomologyAlgebraTrivial(B,GenDegreeLimit=>6)
    Text
      The command returns true since R --> S is Golod.  Notice we also used the option GenDegreeLimit here.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      A = koszulComplexDGA(R)
      isHomologyAlgebraTrivial(A)
    Text
      The command returns false, since R is Gorenstein, and so HA has Poincare Duality, hence the multiplication
      is far from trivial.
///

doc ///
  Key
    isAcyclic
    (isAcyclic,DGAlgebra)
  Headline
    Determine whether a DG algebra has no higher homology
  Usage
    isAcyc = isAcyclic A
  Inputs
    A:DGAlgebra
  Outputs
    isAcyc:Boolean
      @ TT "true" @ if @ TT "HH_i A = 0" @ for all @ TT "i > 0" @, and @ TT "false" @ otherwise
  Description
    Text
      A DG algebra is acyclic (in the convention used by this package) if it has
      zero homology in all positive degrees, so that @ TT "A -> HH_0 A" @ is a
      quasi-isomorphism.  @ TT "isAcyclic" @ checks homology degree by degree up
      through @ TT "maxDegree A" @; for unbounded DG algebras an @ TO EndDegree @
      option bounds the search.
    Text
      The Koszul complex of a regular sequence is acyclic:
    Example
      Q = ZZ/101[a,b,c,d]
      I = ideal(a^4, b^4, c^4, d^4)
      isAcyclic(koszulComplexDGA I)
    Text
      But the Koszul complex of a ring with nontrivial relations in homology is not:
    Example
      R = ZZ/101[a,b,c,d] / ideal(a^4 + b^4 + c^4 + d^4)
      isAcyclic(koszulComplexDGA R)
    Text
      An acyclic closure built to sufficient degree is acyclic in the checked range:
    Example
      R = ZZ/101[a,b,c,d] / ideal(a^3, b^3, c^3, d^3)
      A = acyclicClosure(R, EndDegree => 3)
      isAcyclic(A, EndDegree => 3)
  SeeAlso
    DGAlgebra
    (homology, ZZ, DGAlgebra)
    acyclicClosure
///

doc ///
  Key
    isGolod
    (isGolod,Ring)
  Headline
    Determines if a ring is Golod
  Usage
    isGol = isGolod(R)
  Inputs
    R:Ring
  Outputs
    isGol:Boolean
  Description
    Text
      This function determines if the Koszul complex of a ring R admits a trivial Massey operation.  If one exists, then R is Golod.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4+b^4+c^4+d^4}
      isGolod(R)
    Text
      Hypersurfaces are Golod, but
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      isGolod(R)
    Text
      complete intersections of higher codimension are not.  Here is another example:
    Example
      Q = ZZ/101[a,b,c,d]
      R = Q/(ideal vars Q)^2
      isGolod(R)
    Text
      The above is a (CM) ring minimal of minimal multiplicity, hence Golod.  The next example was found
      by Lukas Katthan, and appears in his arXiv paper 1511.04883.  It is the first known example
      of an algebra that is not Golod, but whose Koszul complex has a trivial homology product.
    Example
      Q = ZZ/101[x_1,x_2,y_1,y_2,z,w]
      I = ideal {x_1*x_2^2,z^2*w,y_1*y_2^2,x_2^2*z*w,y_2^2*z^2,x_1*x_2*y_1*y_2,x_2^2*y_2^2*z,x_1*y_1*z}
      R = Q/I
      isHomologyAlgebraTrivial koszulComplexDGA R
      isGolod R
    Text
      Note that since the Koszul complex is zero in homological degree beyond the embedding dimension, there are only finitely
      many Massey products that one needs to check to verify that a ring is Golod.
///

doc ///
  Key
    isGolodHomomorphism
    (isGolodHomomorphism,QuotientRing)
  Headline
    Determines if the canonical map from the ambient ring is Golod
  Usage
    isGol = isGolodHomomorphism(R)
  Inputs
    R:QuotientRing
  Outputs
    isGol:Boolean
  Description
    Text
      This function determines if the canonical map from ambient R --> R is Golod.  It does this by computing an acyclic closure of
      ambient R (which is a @ TO DGAlgebra @), then tensors this with R, and determines if this DG Algebra has a trivial Massey operation
      up to a certain homological degree provided by the option GenDegreeLimit.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4+b^4+c^4+d^4}
      isGolodHomomorphism(R,GenDegreeLimit=>5)
    Text
      If R is a Golod ring, then ambient R $\rightarrow$ R is a Golod homomorphism. 
    Example
      Q = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      R = Q/ideal (a^3*b^3*c^3*d^3)
      isGolodHomomorphism(R,GenDegreeLimit=>5,TMOLimit=>3)
    Text
      The map from Q to R is Golod by a result of Avramov and Levin; we can only find the trivial Massey operations out to a given degree.
///

doc ///
  Key
    getGenerators
    (getGenerators,DGAlgebra)
  Headline
    Cycles whose classes generate the homology algebra of a DG algebra
  Usage
    cycleList = getGenerators A
  Inputs
    A:DGAlgebra
  Outputs
    cycleList:List
      a list of cycles in @ TT "A.natural" @ whose homology classes generate
      @ TT "HH_*(A)" @ as a graded-commutative algebra over @ TT "A.ring" @
  Description
    Text
      Walks up the homological degrees of @ TT "A" @, computing a basis of each
      homology module and selecting minimal new generators not already reachable
      by products of lower-degree cycles.  The returned list is exactly the
      generating set used internally by @ TO homologyAlgebra @ to build its
      presentation.
    Text
      This version of the function assumes all algebra generators of @ TT "A" @
      lie in odd homological degree.  When @ TT "A" @ has generators in even
      degree, a finite generating set need not exist, and the search depth must
      be bounded with @ TT "EndDegree" @ or @ TO GenDegreeLimit @.
    Example
      R = ZZ/101[a,b,c] / ideal(a^3, b^3, c^3, a^2*b^2*c^2)
      A = koszulComplexDGA R
      netList getGenerators A
    Text
      The generators above are the cycles used to present @ TT "homologyAlgebra A" @;
      their classes span @ TT "HH_i A" @ in each positive degree.
    Example
      apply(maxDegree A + 1, i -> numgens prune homology(i, A))
  SeeAlso
    DGAlgebra
    homologyAlgebra
    (homology, ZZ, DGAlgebra)
    homologyClass
///

doc ///
  Key
    deviations
    (deviations,Ring)
    (deviations,Complex)
    (deviations,RingElement,List)
  Headline
    Computes the deviations of the input ring, complex, or power series.
  Usage
    devTally = deviations(R)
  Inputs
    R:Ring
  Outputs
    devTally:Tally
  Description
    Text
      This command computes the deviations of a @ TO Ring @, a @ TO Complex @, or a power series in the form of a @ TO RingElement @.
      The deviations are the same as the degrees of the generators of the acyclic closure of R, or the degrees of the generators of the
      Tor algebra of R.  This function takes an option called Limit (default value 3) that specifies the largest deviation to compute.
    Example
      R = ZZ/101[a,b,c,d]/ideal {a^3,b^3,c^3,d^3}
      deviations(R)
      deviations(R,DegreeLimit=>4)
      S = R/ideal{a^2*b^2*c^2*d^2}
      deviations(S,DegreeLimit=>4)
      T = ZZ/101[a,b]/ideal {a^2-b^3}
      deviations(T,DegreeLimit=>4)
    Text
      Note that the deviations of T are not graded, since T is not graded.  When calling deviations on a Complex, the
      zeroth free module must be cyclic, and this is checked.  The same goes for the case
      of a RingElement.
    Example
      R = ZZ/101[a,b,c,d]/ideal {a^3,b^3,c^3,d^3}
      A = degreesRing R
      kRes = freeResolution(coker vars R, LengthLimit => 4)
      pSeries = poincareN kRes
      devA = deviations(R,DegreeLimit=>5)
      devB = deviations(kRes,DegreeLimit=>5)
      devC = deviations(pSeries,degrees R, DegreeLimit=>5)
      devA === devB and devB === devC
///

doc ///
  Key
    deviationsToPoincare
    (deviationsToPoincare,HashTable)
    [deviationsToPoincare,DegreeLimit]
  Headline
    Computes the power series corresponding to a set of deviations.
  Usage
    pSeries = deviationsToPoincare(devHash)
  Inputs
    devHash:HashTable
      HashTable of the same form as the output from @ TO deviations @
  Outputs
    pSeries:RingElement
  Description
    Text
      This command takes a HashTable of the same form output from @ TO deviations @ and produces the Poincare series corresponding to it.
      The (key,value) pairs must be of the form homologicalDegree=>number or (homologicalDegree,internalDegree)=>number.
      Because 
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      RDevs = deviations(R,DegreeLimit=>6)
      devPSeries = deviationsToPoincare(RDevs,DegreeLimit=>6)
      pSeries = poincareN (freeResolution(coker vars R, LengthLimit=>6))
      substitute(devPSeries,ring pSeries) == pSeries
///

doc ///
  Key
    findTrivialMasseyOperation
    findNaryTrivialMasseyOperation
    (findTrivialMasseyOperation,DGAlgebra)
    (findNaryTrivialMasseyOperation,DGAlgebra,List,HashTable,ZZ)
  Headline
    Finds a trivial Massey operation on a set of generators of H(A)
  Usage
    tmo = findTrivialMasseyOperation(A)
  Inputs
    A:DGAlgebra
  Outputs
    seq:Sequence
      A sequence seq whose first entry reports whether a trivial Massey operation has been found, and the second
      entry is a hash table with keys given by monomials in a generating set of the positive degree homology of
      A and values the element that bounds the Massey product corresponding to that monomial.
  Description
    Text
      This function the element that bounds all potentially nonzero Massey products (before taking homology class).
      The maximum degree of a generating cycle is specified in the option GenDegreeLimit, if needed.
    Text
      Golod rings are defined by being those rings whose Koszul complex K^R has a trivial Massey operation.
      Also, the existence of a trivial Massey operation on a DG algebra A forces the multiplication on H(A)
      to be trivial.  An example of a ring R such that H(K^R) has trivial multiplication, yet K^R does not admit
      a trivial Massey operation is unknown.  Such an example cannot be monomially defined, by a result of
      Jollenbeck and Berglund. 
    Text
      This is an example of a Golod ring.  It is Golod since it is the Stanley-Reisner ideal of a flag complex
      whose 1-skeleton is chordal [Jollenbeck-Berglund].
    Example
      Q = ZZ/101[x_1..x_6]
      I = ideal (x_3*x_5,x_4*x_5,x_1*x_6,x_3*x_6,x_4*x_6)
      R = Q/I
      A = koszulComplexDGA(R)
      isHomologyAlgebraTrivial(A,GenDegreeLimit=>3)
      cycleList = getGenerators(A)
      (hasTMO, tmoSoFar) = findTrivialMasseyOperation(A)
      assert(hasTMO)
    Text
      Below is an example of a Teter ring (Artinian Gorenstein ring modulo its socle), and the computation in Avramov and Levin's
      paper shows that H(A) does not have trivial multiplication, hence no trivial Massey operation can exist.
    Example
      Q = ZZ/101[x,y,z]
      I = ideal (x^3,y^3,z^3,x^2*y^2*z^2)
      R = Q/I
      A = koszulComplexDGA(R)
      isHomologyAlgebraTrivial(A)
      cycleList = getGenerators(A)
      assert(not first findTrivialMasseyOperation(A))
    Text
      The related function @ TO findNaryTrivialMasseyOperation @ find only the nth order trivial Massey operations.
///

doc ///
  Key
    masseyTripleProduct
    (masseyTripleProduct,DGAlgebra,RingElement,RingElement,RingElement)
  Headline
    Computes the Massey triple product of a set of cycles or homology classes
  Usage
    h = masseyTripleProduct(A,h1,h2,h3)
  Inputs
    A:DGAlgebra
    h1:RingElement
    h2:RingElement
    h3:RingElement
  Outputs
    h:RingElement
      The return value is either the homology class of the Massey triple product defined
      by the inputs or a cycle representing the homology class.
  Description
    Text
       These functions compute the Massey triple product of either three homology classes
       or three cycles that represent nonzero homology classes for which the Massey triple product
       is defined.
    Text
       For an example, we return to an example due to Lukas Katthan which was discussed in @ TO isGolod @.
       First, we define the algebra:
    Example
       Q = QQ[x_1,x_2,y_1,y_2,z]
       I = ideal (x_1*x_2^2,y_1*y_2^2,z^3,x_1*x_2*y_1*y_2,y_2^2*z^2,x_2^2*z^2,x_1*y_1*z,x_2^2*y_2^2*z)
       R = Q/I
       KR = koszulComplexDGA R
    Text
       The following are cycles:
    Example
       z1 = z^2*T_(1,5)
       z2 = y_2^2*T_(1,3)
       z3 = x_2^2*T_(1,1)
    Text
       and z1*z2, z2*z3 vanish in homology:
    Example
       (lifted12,lift12) = getBoundaryPreimage(KR,z1*z2)
       (lifted23,lift23) = getBoundaryPreimage(KR,z2*z3)
    Text
       Note that the first return value of @ TO getBoundaryPreimage @ indicates that the inputs
       are indeed boundaries, and the second value is the lift of the boundary along the differential.
    Text
       Given cycles z1,z2,z3 such that z1*z2 and z2*z3 are boundaries, 
       the Massey triple product of the homology classes represented by z1,z2 and z3 
       is the homology class of lift12*z3 + z1*lift23.  To see this, we compute and check:
    Example
       z123 = masseyTripleProduct(KR,z1,z2,z3)
       z123 == lift12*z3 + z1*lift23
    Text
       One may also compute Massey triple products directly on elements of the homology
       algebra itself, as is seen with the command masseyTripleProduct:
    Example
       H = HH(KR)
       h1 = homologyClass(KR,z1)
       h2 = homologyClass(KR,z2)
       h3 = homologyClass(KR,z3)
       h123 = masseyTripleProduct(KR,h1,h2,h3)
       h123 == homologyClass(KR,z123)
///

doc ///
  Key
    (masseyTripleProduct,DGAlgebra,ZZ,ZZ,ZZ)
  Headline
    Computes the matrix representing all triple Massey operations.
  Usage
    mat = masseyTripleProduct(A,l,m,n)
  Inputs
    A:DGAlgebra
    l:ZZ
    m:ZZ
    n:ZZ
  Outputs
    mat:Matrix
  Description
    Text
      Given a triple of homology classes h1,h2,h3, such that h1h2 = h2h3 = 0,
      the Massey triple product of h1,h2 and h3 may be defined as in
      @ TO masseyTripleProduct @.  This command computes a basis of the homology
      algebra of A in degrees l,m and n respectively, and expresses the triple
      Massey operation of each triple, provided it is defined.  If a triple product
      is not defined (i.e. if either h1h2 or h2h3 is not zero) then the triple
      product is reported as zero in the matrix.
    Text
      The following example appears in "On the Hopf algebra of a Local Ring" by Avramov
      as an example of a nonvanishing Massey operation which an algebra generator:
    Example
      Q = QQ[t_1,t_2,t_3,t_4]
      I = ideal (t_1^3,t_2^3,t_3^3-t_1*t_2^2,t_1^2*t_3^2,t_1*t_2*t_3^2,t_2^2*t_4,t_4^2)
      R = Q/I
      KR = koszulComplexDGA R
      H = HH(KR)
      masseys = masseyTripleProduct(KR,1,1,1);
      rank masseys
    Text
      As you can see, this command is useful to determine the number of linearly independent
      elements that arise as triple Massey products.
    Text
      For example, the following Massey triple product is nonvanishing and is an
      algebra generator:
    Example
      masseyTripleProduct(KR,X_2,X_4,X_1)
///

doc ///
  Key
    expandGeomSeries
    (expandGeomSeries,List,ZZ)
    (expandGeomSeries,RingElement,ZZ)
  Headline
    Expand a geometric series to a specified degree.
  Usage
    pSeries = expandGeomSeries(f,n)
  Inputs
    f:RingElement
      Ratio of the geometric series to be expanded.
    n:ZZ
      Degree which to expand the geometric series.
  Outputs
    pSeries:RingElement
      Power series representation of the geometric series.
  Description
    Text
      If the user supplies a list instead of a RingElement as the first argument, the return
      value is the product of all the each of the geometric series expanded to degree n obtained
      by calling expandGeomSeries on each element of the list.
    Example
      A = ZZ[S,T_0,T_1]
      f = expandGeomSeries(S^2*T_0^8,10)
      g = expandGeomSeries(S^4*T_1^15,10)
      h = expandGeomSeries({S^2*T_0^8,S^4*T_1^15},10)
      B = A/(first gens A)^11
      substitute(f*g,B) == h
///

doc ///
  Key
    torMap
    (torMap,RingMap)
    [torMap,GenDegreeLimit]
  Headline
    Compute the map of Tor algebras associated to a RingMap.
  Usage
    torPhi = torMap(phi)
  Inputs
    phi:RingMap
  Outputs
    torPhi:RingMap
  Description
    Text
      The functor Tor_R(M,N) is also functorial in the ring argument.  Therefore, a ring map phi from A to B induces an algebra map
      from the Tor algebra of A to the Tor algebra of B.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3,a^2*b^2*c^2}
      S = R/ideal{a*b^2*c^2,a^2*b*c^2,a^2*b^2*c}
      f = map(S,R)
      fTor = torMap(f,GenDegreeLimit=>3)
      matrix fTor
    Text
      In the following example, the map on Tor is surjective, which means that the ring homomorphism is large (Dress-Kramer).
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3,a*c,a*d,b*c,b*d}
      S = ZZ/101[a,b]/ideal{a^3,b^3}
      f = map(S,R,matrix{{a,b,0,0}})
      fTor = torMap(f,GenDegreeLimit=>4)
      matrix fTor      
///

doc ///
  Key
    (diff,DGAlgebra,RingElement)
  Headline
    Apply the differential of a DGAlgebra to a ring element
  Usage
    b = diff(A, a)
  Inputs
    A:DGAlgebra
    a:RingElement
      an element of @ TT "A.natural" @
  Outputs
    b:RingElement
      the image @ TT "d_A(a)" @
  Description
    Text
      Returns the image of @ TT "a" @ under the differential of @ TT "A" @.
      The element @ TT "a" @ must be a ring element of the underlying graded-commutative
      algebra @ TT "A.natural" @; products, sums, and polynomial expressions in the
      DG generators are all valid inputs.  The differential is applied by the Leibniz rule,
      so homogeneous components are processed independently.
    Example
      R = ZZ/101[x,y,z]
      A = koszulComplexDGA R
      diff(A, T_(1,1) * T_(1,2))
      diff(A, diff(A, T_(1,1) * T_(1,2) * T_(1,3)))
    Text
      When @ TT "a" @ is a cycle, the result is zero:
    Example
      z = x*T_(1,2) - y*T_(1,1)
      diff(A, z)
    Text
      To solve the equation @ TT "diff(A, b) == z" @ for a boundary @ TT "z" @,
      use @ TO getBoundaryPreimage @.
  SeeAlso
    DGAlgebra
    getBoundaryPreimage
    homologyClass
///

doc ///
  Key
    getBoundaryPreimage
    (getBoundaryPreimage,DGAlgebra,RingElement)
    (getBoundaryPreimage,DGAlgebra,List)
  Headline
    Attempt to find a preimage of a boundary under the differential of a DGAlgebra.
  Usage
    (lifted,myLift) = getBoundaryPreimage(A,z)
  Inputs
    A:DGAlgebra
    z:RingElement
  Outputs
    seq:Sequence
  Description
    Text
      The first element in the return value is a boolean value indicating whether the
      lift was possible.  If true, the second coordinate of the return value is the lift.
      If false, then the second coordinate of the return value is the reduction of the
      input modulo the image.
    Example
       Q = QQ[x_1,x_2,y_1,y_2,z]
       I = ideal (x_1*x_2^2,y_1*y_2^2,z^3,x_1*x_2*y_1*y_2,y_2^2*z^2,x_2^2*z^2,x_1*y_1*z,x_2^2*y_2^2*z)
       R = Q/I
       KR = koszulComplexDGA R
    Text
       The following are cycles:
    Example
       z1 = z^2*T_(1,5)
       z2 = y_2^2*T_(1,3)
       z3 = x_2^2*T_(1,1)
       {diff(KR,z1),diff(KR,z1),diff(KR,z1)}
    Text
       and z1*z2, z2*z3 vanish in homology:
    Example
       (lifted12,lift12) = getBoundaryPreimage(KR,z1*z2)
       (lifted23,lift23) = getBoundaryPreimage(KR,z2*z3)
    Text
       We can check that the differential of the lift is the supposed boundary:
    Example
       diff(KR,lift23) == z2*z3
///

doc ///
  Key
    homologyClass
    (homologyClass,DGAlgebra,RingElement)
  Headline
    Computes the element of the homology algebra corresponding to a cycle in a DGAlgebra.
  Usage
    h = homologyClass(A,z)
  Inputs
    A:DGAlgebra
    z:RingElement
  Outputs
    h:RingElement
  Description
    Text
      This function computes the element in the homology algebra of a cycle in a @ TO DGAlgebra @.
      In order to do this, the @ TO homologyAlgebra @ is retrieved (or computed, if it hasn't been
      already).
    Example
      Q = QQ[x,y,z]
      I = ideal (x^3,y^3,z^3)
      R = Q/I
      KR = koszulComplexDGA R
      z1 = x^2*T_(1,1)
      z2 = y^2*T_(1,2)
      H = HH(KR)
      homologyClass(KR,z1*z2)
///

doc ///
  Key
    zerothHomology
    (zerothHomology,DGAlgebra)
  Headline
    The zeroth homology of a DG algebra as a ring
  Usage
    HA0 = zerothHomology A
  Inputs
    A:DGAlgebra
  Outputs
    HA0:Ring
      @ TT "HH_0(A)" @, presented as a quotient of @ TT "A.ring" @
  Description
    Text
      Returns @ TT "HH_0(A)" @ as a ring, computed by taking @ TT "A.ring" @ modulo the image
      of the degree-one part of the differential.  For a Koszul complex
      @ TT "KR = koszulComplexDGA I" @ over @ TT "R" @, this recovers the quotient ring @ TT "R/I" @.
    Example
      Q = ZZ/101[x,y,z]
      I = ideal(x^2, y^2, z^2, x*y*z)
      KQ = koszulComplexDGA I
      H0 = zerothHomology KQ
      describe H0
    Text
      When @ TT "A" @ is an acyclic closure, @ TT "zerothHomology A" @ returns the quotient
      ring on which the DG algebra resolves the residue field.
    Example
      R = Q / I
      A = acyclicClosure(R, EndDegree => 2)
      zerothHomology A
  SeeAlso
    (homology, DGAlgebra)
    homologyAlgebra
///

doc ///
  Key
    getDegNModule
    (getDegNModule,ZZ,Ring,Ring)
  Headline
    Extract the degree-n strand of a graded algebra as a module over its degree-zero piece
  Usage
    MN = getDegNModule(N, R, A)
  Inputs
    N:ZZ
      the homological degree to extract
    R:Ring
      the degree-zero piece of @ TT "A" @, i.e.\ @ TT "A_0" @
    A:Ring
      a graded @ TT "R" @-algebra with @ TT "A_0 = R" @
  Outputs
    MN:Module
      the degree-@ TT "N" @ component of @ TT "A" @, presented as an @ TT "R" @-module
  Description
    Text
      Given a graded algebra @ TT "A" @ over @ TT "R" @ with @ TT "A_0 = R" @, the strand
      @ TT "A_N" @ is an @ TT "R" @-module.  This function returns that strand with a
      minimal presentation by monomials of internal degree @ TT "N" @ modulo the relations
      of @ TT "A" @.  It is most often applied to a homology algebra @ TT "HA = HH A" @
      together with its degree-zero subring @ TT "HA_0 = zerothHomology A" @.
    Example
      Q = ZZ/101[x,y,z]
      I = ideal(x^3, y^3, z^3)
      R = Q / I
      KR = koszulComplexDGA R
      HA = HH KR;
      H0 = zerothHomology KR
      M1 = getDegNModule(1, H0, HA)
      M2 = getDegNModule(2, H0, HA)
    Text
      The ranks of these strands are the Koszul Betti numbers of @ TT "R" @:
    Example
      apply(0..3, n -> numgens getDegNModule(n, H0, HA))
  SeeAlso
    zerothHomology
    homologyAlgebra
    homologyModule
///

doc ///
  Key
    DGAlgebraMap
  Headline
    The class of DG algebra maps
  Description
    Text
      A @ TT "DGAlgebraMap" @ represents a morphism of @ TO2{DGAlgebra, "DG algebras"} @.
      It carries four pieces of data: a @ TT "source" @ and @ TT "target" @ (each a
      @ TO DGAlgebra @), an underlying ring map @ TT "f.natural" @ on the graded-commutative
      algebras, and a degree-zero ring map @ TT "f.ringMap" @ between the underlying rings.
      A DG algebra map is a ring map that commutes with the differentials, so that
      @ TT "d_B \\circ f = f \\circ d_A" @ where @ TT "d_A" @ and @ TT "d_B" @ are the
      differentials of the source and target.
    Text
      There are two principal ways to construct a DG algebra map:
      @ TO dgAlgebraMap @ builds one from a matrix specifying where the DG generators go,
      and @ TO liftToDGMap @ lifts a ring map on the degree-zero part to a morphism of
      acyclic closures or Koszul complexes.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3, b^3, c^3}
      S = R / ideal{a^2*b^2*c^2}
      A = acyclicClosure(R, EndDegree => 3)
      B = acyclicClosure(S, EndDegree => 3)
      phi = liftToDGMap(B, A, map(S, R))
      source phi === A
      target phi === B
    Text
      Once constructed, a DG algebra map can be checked with @ TO (isWellDefined, DGAlgebraMap) @,
      converted to a @ TO ComplexMap @ via @ TO toComplexMap @, or pushed through homology
      with @ TO (homology, DGAlgebraMap) @.
  SeeAlso
    dgAlgebraMap
    liftToDGMap
    (isWellDefined, DGAlgebraMap)
    toComplexMap
    (homology, DGAlgebraMap)
    ringMap
///

doc ///
  Key
    ringMap
  Headline
    The degree-zero ring map underlying a DGAlgebraMap
  Usage
    phi.ringMap
  Description
    Text
      Every @ TO DGAlgebraMap @ @ TT "phi" @ from @ TT "A" @ to @ TT "B" @ includes a key
      @ TT "ringMap" @: a @ TO RingMap @ from @ TT "A.ring" @ to @ TT "B.ring" @ giving the
      restriction of @ TT "phi" @ to degree zero.  When the two DG algebras live over the
      same ring, @ TT "phi.ringMap" @ is the identity; in general it is the degree-zero
      ring map used to lift @ TT "phi" @.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3, b^3, c^3}
      S = R / ideal{a^2*b^2*c^2}
      A = acyclicClosure(R, EndDegree => 2)
      B = acyclicClosure(S, EndDegree => 2)
      phi = liftToDGMap(B, A, map(S, R))
      phi.ringMap
    Text
      The @ TT "ringMap" @ is the bridge used internally by @ TO toComplexMap @ to
      pushforward complexes between different rings.
  SeeAlso
    DGAlgebraMap
    liftToDGMap
    toComplexMap
///

doc ///
  Key
    dgAlgebraMap
    (isWellDefined,DGAlgebraMap)
    (dgAlgebraMap,DGAlgebra,DGAlgebra,Matrix)
  Headline
    Define a DG algebra map between DG algebras.
  Usage
    phi = dgAlgebraMap(B,A,M)
  Inputs
    A:DGAlgebra
       Source
    B:DGAlgebra
       Target
    M:Matrix
       A matrix representing where the generators of A should be mapped to (akin to ringMap)
  Outputs
    phi:DGAlgebraMap
  Description
    Example
      R = ZZ/101[a,b,c]/ideal{a^3+b^3+c^3,a*b*c}
      K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
      K2 = koszulComplexDGA(ideal {b,c},Variable=>"T")
      g = dgAlgebraMap(K1,K2,matrix{{Y_(1,2),Y_(1,3)}})
      isWellDefined g
    Text
      The function does not check that the DG algebra map is well defined, however.
    Example
      f = dgAlgebraMap(K2,K1,matrix{{0,T_(1,1),T_(1,2)}})
      isWellDefined f
///

doc ///
  Key
    toComplexMap
    (toComplexMap,DGAlgebraMap)
    (toComplexMap,DGAlgebraMap,ZZ)
    [toComplexMap,AssertWellDefined]
    [toComplexMap,EndDegree]
  Headline
    Construct the ComplexMap associated to a DGAlgebraMap
  Usage
    psi = toComplexMap phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    psi:ComplexMap
  Description
    Example
       R = ZZ/101[a,b,c]/ideal{a^3+b^3+c^3,a*b*c}
       K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
       K2 = koszulComplexDGA(ideal {b,c},Variable=>"T")
       g = dgAlgebraMap(K1,K2,matrix{{Y_(1,2),Y_(1,3)}})
       g' = toComplexMap g
    Text
      The option @ TO EndDegree @ must be specified if the source of phi has any algebra generators of even degree.  The option @ TO AssertWellDefined @
      is used if one wishes to assert that the result of this computation is indeed a chain map.  One can construct just the nth map in the
      chain map by providing the second @ TO ZZ @ parameter.
    Text
      This function also works when working over different rings, such as the case when the @ TO DGAlgebraMap @ is produced via
      @ TO liftToDGMap @ and in the next example.  In this case, the target module is produced via @ TO pushForward @.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      S = R/ideal{a^2*b^2*c^2}
      f = map(S,R)
      A = acyclicClosure(R,EndDegree=>3)
      B = acyclicClosure(S,EndDegree=>3)
      phi = liftToDGMap(B,A,f)
      toComplexMap(phi,EndDegree=>3)
///

doc ///
  Key
    liftToDGMap
    (liftToDGMap,DGAlgebra,DGAlgebra,RingMap)
    [liftToDGMap,EndDegree]
  Headline
    Lift a ring homomorphism in degree zero to a DG algebra morphism
  Usage
    phiTilde = liftToDGMap(B,A,phi)
  Inputs
    B:DGAlgebra
      Target
    A:DGAlgebra
      Source
    phi:RingMap
      Map from A in degree zero to B in degree zero
  Outputs
    phiTilde:DGAlgebraMap
      DGAlgebraMap lifting phi to a map of DGAlgebras.
  Description
    Text
      In order for phiTilde to be defined, phi of the image of the differential of A in degree 1 must lie in the image of the
      differential of B in degree 1.  At present, this condition is not checked.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      S = R/ideal{a^2*b^2*c^2}
      f = map(S,R)
      A = acyclicClosure(R,EndDegree=>3)
      B = acyclicClosure(S,EndDegree=>3)
      phi = liftToDGMap(B,A,f)
      toComplexMap(phi,EndDegree=>3)
///

doc ///
  Key
    (homology,DGAlgebraMap)
  Headline
    The induced ring map on homology algebras
  Usage
    homologyPhi = homology phi
    homologyPhi = HH phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    homologyPhi:RingMap
      the induced map @ TT "HH(source phi) -> HH(target phi)" @
  Description
    Text
      A @ TO DGAlgebraMap @ @ TT "phi: A -> B" @ is a chain map, so it descends to a ring
      map on homology algebras.  This function computes that induced map, returning a
      @ TO RingMap @ from @ TO homologyAlgebra @ of the source to @ TO homologyAlgebra @
      of the target.  The homology algebras are computed via @ TO homologyAlgebra @ if
      they have not been cached.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3 + b^3 + c^3, a*b*c}
      K1 = koszulComplexDGA(ideal vars R, Variable => "Y")
      K2 = koszulComplexDGA(ideal{b, c}, Variable => "T")
      g = dgAlgebraMap(K1, K2, matrix{{Y_(1,2), Y_(1,3)}})
      HHg = HH g
      source HHg === homologyAlgebra K1
      target HHg === homologyAlgebra K2
  SeeAlso
    DGAlgebraMap
    homologyAlgebra
    toComplexMap
///

-- moved this out of the documentation since it is not complete
--    (homology,DGAlgebraMap,ZZ)

--    Text
--      One can also supply the second argument (a ZZ) in order to obtain the map on homology in a specified degree.
--      (This is currently not available).

doc ///
  Key
    dgAlgebraMultMap
    (dgAlgebraMultMap,DGAlgebra,RingElement)
  Headline
    Returns the chain map corresponding to multiplication by a cycle.
  Usage
    phi = dgAlgebraMultMap(A,z)
  Inputs
    A:DGAlgebra
    z:RingElement
  Outputs
    phi:ComplexMap
  Description
    Text
      If A is a DGAlgebra, and z is a cycle of A, then left multiplication of A by z gives
      a chain map from A to A.  This command converts A to a complex using @ TO toComplex @,
      and constructs a @ TO ComplexMap @ that represents left multiplication by z.
      This command is used to determine the module structure that is computed in
      @ TO homologyModule @.
    Example
      R = QQ[x,y,z]/ideal{x^3,y^3,z^3}
      KR = koszulComplexDGA R
      z1 = x^2*T_(1,1)
      phi = dgAlgebraMultMap(KR,z1)
    Text
      As you can see, the degree of phi is the homological degree of z:
    Example
      degree phi == first degree z
    Text
      Care is also taken to ensure the resulting map is homogeneous if R and z are:
    Example
      isHomogeneous phi
    Text
      One may then view the action of multiplication by the homology class of z upon
      taking the induced map in homology:
    Example
      Hphi = prune HH(phi); (Hphi_0,Hphi_1,Hphi_2)
///

doc ///
  Key
    homologyModule
    (homologyModule,DGAlgebra,Module)
  Headline
    Compute the homology of a DGModule as a module over a DGAlgebra.
  Usage
    HM = homologyModule(A,M)
  Inputs
    A:DGAlgebra
    M:Module
  Outputs
    HM:Module
  Description
    Text
      Given a DGAlgebra A over a ring R, and an R-module M, A ** M carries the structure
      of a left DG module over A.  It follows that H(A ** M) is a module over H(A).
      Although DGModules have yet to be implemented as objects in Macaulay2 in their own right,
      the current infrastructure (with a little extra work) allows us to determine the module structure
      of this type of DG module as a module over the homology algebra of A.
    Text
      Currently, this code will only work on DGAlgebras that are finite over their ring
      of definition, such as Koszul complexes.  (Truncations of) module structures in case
      of non-finite DGAlgebras may be made available in a future update.
    Text
      For an example, we will compute the module structure of the Koszul homology of
      the canonical module over the Koszul homology algebra.
    Example
      Q = QQ[x,y,z,w]
      I = ideal (w^2, y*w+z*w, x*w, y*z+z^2, y^2+z*w, x*y+x*z, x^2+z*w)
      R = Q/I
      KR = koszulComplexDGA R
      cxKR = toComplex KR
      HKR = HH(KR)
    Text
      The following is the graded canonical module of R:
    Example
      degList = first entries vars Q / degree / first
      M = Ext^4(Q^1/I,Q^{-(sum degList)}) ** R
    Text
      We obtain the Koszul homology module using the following command:
    Example
      HKM = homologyModule(KR,M);
    Text
      One may notice the duality of HKR and HKM by considering their Hilbert series:
    Example
      hsHKR = value numerator reduceHilbert hilbertSeries HKR
      hsHKM = value numerator reduceHilbert hilbertSeries HKM
      AA = ring hsHKR
      e = numgens Q
      hsHKR == T_0^e*T_1^e*sub(hsHKM, {T_0 => T_0^(-1), T_1 => T_1^(-1)})
///

doc ///
  Key
    (source,DGAlgebraMap)
  Headline
    The source of a DG algebra map
  Usage
    A = source phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    A:DGAlgebra
      the source DG algebra of @ TT "phi" @
  Description
    Text
      Returns the @ TO DGAlgebra @ from which @ TT "phi" @ is defined.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3, b^3, c^3}
      S = R / ideal{a^2*b^2*c^2}
      A = acyclicClosure(R, EndDegree => 2)
      B = acyclicClosure(S, EndDegree => 2)
      phi = liftToDGMap(B, A, map(S, R))
      source phi === A
  SeeAlso
    DGAlgebraMap
    (target, DGAlgebraMap)
    liftToDGMap
///

doc ///
  Key
    (target,DGAlgebraMap)
  Headline
    The target of a DG algebra map
  Usage
    B = target phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    B:DGAlgebra
      the target DG algebra of @ TT "phi" @
  Description
    Text
      Returns the @ TO DGAlgebra @ into which @ TT "phi" @ maps.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3, b^3, c^3}
      S = R / ideal{a^2*b^2*c^2}
      A = acyclicClosure(R, EndDegree => 2)
      B = acyclicClosure(S, EndDegree => 2)
      phi = liftToDGMap(B, A, map(S, R))
      target phi === B
  SeeAlso
    DGAlgebraMap
    (source, DGAlgebraMap)
    liftToDGMap
///

doc ///
  Key
    AssertWellDefined
  Headline
    Option to verify that a constructed DG algebra map respects differentials
  Usage
    liftToDGMap(..., AssertWellDefined => true)
    toComplexMap(..., AssertWellDefined => true)
  Description
    Text
      When set to @ TT "true" @, the constructor checks that the output
      @ TO DGAlgebraMap @ or the resulting @ TO ComplexMap @ actually commutes with the
      differentials up to the working degree.  This is a @ TT "false" @ by default
      because the check can be expensive for large acyclic closures; set it to
      @ TT "true" @ when debugging a DG map that behaves unexpectedly.
  SeeAlso
    liftToDGMap
    toComplexMap
    dgAlgebraMap
///

doc ///
  Key
    StartDegree
  Headline
    Option to specify the first homological degree processed by an iterative DG construction
  Usage
    acyclicClosure(..., StartDegree => n)
    killCycles(..., StartDegree => n)
    getGenerators(..., StartDegree => n)
  Description
    Text
      Several routines in this package build DG algebras one homological degree at a
      time, starting from degree @ TT "StartDegree" @ and incrementing up to an
      appropriate endpoint.  Raising @ TT "StartDegree" @ can be useful when a partial
      acyclic closure has already been cached and you wish to continue the construction
      from where it left off.  The default value is @ TT "1" @.
  SeeAlso
    EndDegree
    acyclicClosure
    killCycles
    getGenerators
///

doc ///
  Key
    EndDegree
  Headline
    Option to specify the last homological degree processed by an iterative DG construction
  Usage
    acyclicClosure(..., EndDegree => n)
    killCycles(..., EndDegree => n)
    isAcyclic(..., EndDegree => n)
    toComplexMap(..., EndDegree => n)
  Description
    Text
      Bounds how far an iterative DG algebra computation should proceed.  In @ TO acyclicClosure @
      this limits the homological degree of new generators adjoined; in @ TO isAcyclic @
      it is the highest degree in which homology is checked; in @ TO toComplexMap @ it
      governs how many homological degrees of the resulting chain map are computed.
    Text
      The default value varies by function and is documented on each specific bracketed
      option node.  This option is required whenever a DG algebra has generators in
      even homological degree, since the algebra is otherwise unbounded.
  SeeAlso
    StartDegree
    acyclicClosure
    killCycles
    isAcyclic
    toComplexMap
///

doc ///
  Key
    GenDegreeLimit
  Headline
    Option to bound the homological degree searched for algebra generators
  Usage
    homologyAlgebra(..., GenDegreeLimit => n)
    torAlgebra(..., GenDegreeLimit => n)
    findTrivialMasseyOperation(..., GenDegreeLimit => n)
    isHomologyAlgebraTrivial(..., GenDegreeLimit => n)
    isGolodHomomorphism(..., GenDegreeLimit => n)
  Description
    Text
      Routines that assemble a homology algebra — such as @ TO homologyAlgebra @ and
      @ TO torAlgebra @ — must, in general, walk up all homological degrees to find
      algebra generators.  When the DG algebra has generators in even degree, that
      process is infinite; @ TT "GenDegreeLimit" @ caps the search.  The answer is a
      truncation of the full homology algebra below the specified degree, and is
      exact in the bounded case.
  SeeAlso
    RelDegreeLimit
    homologyAlgebra
    torAlgebra
    findTrivialMasseyOperation
    isHomologyAlgebraTrivial
    isGolodHomomorphism
///

doc ///
  Key
    RelDegreeLimit
  Headline
    Option to bound the homological degree searched for algebra relations
  Usage
    homologyAlgebra(..., RelDegreeLimit => n)
    torAlgebra(..., RelDegreeLimit => n)
  Description
    Text
      Once generators have been chosen (see @ TO GenDegreeLimit @), a homology algebra
      computation must determine relations among them.  @ TT "RelDegreeLimit" @ bounds
      the homological degree up to which those relations are searched.  Typical practice
      is to set @ TT "RelDegreeLimit" @ to roughly twice @ TT "GenDegreeLimit" @, since
      relations can be degree-sums of generator pairs.
  SeeAlso
    GenDegreeLimit
    homologyAlgebra
    torAlgebra
///

doc ///
  Key
    TMOLimit
  Headline
    Option to bound the arity of Massey operations searched for by findTrivialMasseyOperation
  Usage
    findTrivialMasseyOperation(..., TMOLimit => n)
    isGolodHomomorphism(..., TMOLimit => n)
  Description
    Text
      A trivial Massey operation on a DG algebra is a system of null-homotopies that
      simultaneously trivialize every Massey product.  @ TT "TMOLimit" @ caps the arity
      of Massey products considered: only @ TT "k" @-fold products with @ TT "k <= n" @
      are tested for bounding.  This makes the search finite even when the full system
      would be infinite.  In practice, @ TT "TMOLimit" @ values of @ TT "3" @ or @ TT "4" @
      suffice for small examples.
  SeeAlso
    findTrivialMasseyOperation
    isGolodHomomorphism
    isGolod
///

doc ///
  Key
    [isAcyclic,EndDegree]
  Headline
    Option to specify the degree to finish checking acyclicity
  Usage
    isAcyclic(...,EndDegree=>n)
///

doc ///
  Key
    [acyclicClosure,StartDegree]
  Headline
    Option to specify the degree to start computing the acyclic closure. 
  Usage
    acyclicClosure(...,StartDegree=>n)
  Description 
   Text
    The default value is 1.
///

doc ///
  Key
    [acyclicClosure,EndDegree]
  Headline
    Option to specify the degree to stop computing the acyclic closure
  Usage
    A =  acyclicClosure(B,EndDegree=>n)
  Inputs
    B: DGAlgebra
    n: ZZ
  Description
   Text
    All generators of the acyclic closure will be found up to homological degree n.
    Note that B can be an ordinary ring, a factor ring of a polynomial ring, treated as
    a DGAlgebra generated in homological degree 0, as in the following example:
    
    The default value of EndDegree is 3.
   Example
    R = ZZ/101[x,y,z,w]/(ideal"x3,y3,z3,x2yz")
    acyclicClosure R
    acyclicClosure(R,EndDegree => 3)
///

doc ///
  Key
    [getGenerators,StartDegree]
  Headline
    First homological degree in which to search for new algebra generators
  Usage
    getGenerators(..., StartDegree => n)
  Description
    Text
      Degrees below @ TT "n" @ are skipped when walking @ TO getGenerators @ up through
      the DG algebra.  Raising @ TT "StartDegree" @ is useful when earlier degrees have
      been inspected separately.  See @ TO StartDegree @.
  SeeAlso
    getGenerators
    StartDegree
    [getGenerators,DegreeLimit]
///

doc ///
  Key
    [getGenerators,DegreeLimit]
  Headline
    Last homological degree in which to search for algebra generators
  Usage
    getGenerators(..., DegreeLimit => n)
  Description
    Text
      Caps the homological degree of candidates considered by @ TO getGenerators @.
      When the underlying DG algebra has generators in even degree, this option
      is required to make the search finite.
  SeeAlso
    getGenerators
    [getGenerators,StartDegree]
///

doc ///
  Key
    [killCycles,StartDegree]
  Headline
    First homological degree from which killCycles begins searching for cycles
  Usage
    killCycles(..., StartDegree => n)
  Description
    Text
      @ TO killCycles @ searches upward from @ TT "StartDegree" @ for the first
      degree containing a non-bounding cycle, then adjoins generators to kill
      that cycle.  See @ TO StartDegree @.
  SeeAlso
    killCycles
    StartDegree
    [killCycles,EndDegree]
///

doc ///
  Key
    [killCycles,EndDegree]
  Headline
    Last homological degree processed by killCycles
  Usage
    killCycles(..., EndDegree => n)
  Description
    Text
      Bounds the search depth of @ TO killCycles @.  By default @ TT "EndDegree => -1" @
      tells @ TO killCycles @ to stop after processing the first degree with nontrivial
      homology.  See @ TO EndDegree @.
  SeeAlso
    killCycles
    EndDegree
    [killCycles,StartDegree]
///

doc ///
  Key
    [killCycles,Variable]
  Headline
    Option to name the DG algebra generators adjoined by killCycles
  Usage
    killCycles(..., Variable => "T")
  Description
    Text
      @ TO killCycles @ adjoins new DG generators that become boundaries of previously
      non-bounding cycles.  The @ TT "Variable" @ option specifies the base name used
      for these new generators, so that subsequent calls produce readable and
      non-colliding names.
  SeeAlso
    killCycles
    acyclicClosure
///

doc ///
  Key
    [adjoinVariables,Variable]
  Headline
    Option to name the DG algebra generators adjoined by adjoinVariables
  Usage
    adjoinVariables(..., Variable => "T")
  Description
    Text
      @ TO adjoinVariables @ adjoins a new DG generator for each cycle in its input list.
      The @ TT "Variable" @ option specifies the base name used for these new generators.
  SeeAlso
    adjoinVariables
    killCycles
///

doc ///
  Key
    [acyclicClosure,Variable]
  Headline
    Option to name the DG algebra generators adjoined during acyclic closure
  Usage
    acyclicClosure(..., Variable => "T")
  Description
    Text
      As @ TO acyclicClosure @ iteratively @ TO2{killCycles, "kills cycles"} @, it adjoins
      new DG algebra generators indexed by @ TT "T_(i, j)" @ (or the given base name).
      Use the @ TT "Variable" @ option to control that base name.
  SeeAlso
    acyclicClosure
    killCycles
    Variable
///

doc ///
  Key
    [getBasis,Limit]
  Headline
    Maximum number of basis elements to return from getBasis
  Usage
    getBasis(..., Limit => n)
  Description
    Text
      Caps the size of the returned basis, which is useful when only the first few
      elements are needed (for example, when enumerating cycles degree by degree).
  SeeAlso
    getBasis
///

doc ///
  Key
    [homologyAlgebra,GenDegreeLimit]
  Headline
    Maximum homological degree searched for homology-algebra generators
  Usage
    homologyAlgebra(..., GenDegreeLimit => n)
  Description
    Text
      Required when the DG algebra has generators in even degree; see @ TO GenDegreeLimit @.
  SeeAlso
    homologyAlgebra
    GenDegreeLimit
    [homologyAlgebra,RelDegreeLimit]
///

doc ///
  Key
    [homologyAlgebra,RelDegreeLimit]
  Headline
    Maximum homological degree searched for homology-algebra relations
  Usage
    homologyAlgebra(..., RelDegreeLimit => n)
  Description
    Text
      Bounds the degree up to which relations are computed; see @ TO RelDegreeLimit @.
  SeeAlso
    homologyAlgebra
    RelDegreeLimit
    [homologyAlgebra,GenDegreeLimit]
///

doc ///
  Key
    [torAlgebra,GenDegreeLimit]
  Headline
    Maximum homological degree searched for Tor-algebra generators
  Usage
    torAlgebra(..., GenDegreeLimit => n)
  Description
    Text
      See @ TO GenDegreeLimit @.
  SeeAlso
    torAlgebra
    GenDegreeLimit
    [torAlgebra,RelDegreeLimit]
///

doc ///
  Key
    [torAlgebra,RelDegreeLimit]
  Headline
    Maximum homological degree searched for Tor-algebra relations
  Usage
    torAlgebra(..., RelDegreeLimit => n)
  Description
    Text
      See @ TO RelDegreeLimit @.
  SeeAlso
    torAlgebra
    RelDegreeLimit
    [torAlgebra,GenDegreeLimit]
///

doc ///
  Key
    [findTrivialMasseyOperation,GenDegreeLimit]
  Headline
    Maximum homological degree of generators used in the trivial Massey operation search
  Usage
    findTrivialMasseyOperation(..., GenDegreeLimit => n)
  Description
    Text
      See @ TO GenDegreeLimit @.
  SeeAlso
    findTrivialMasseyOperation
    GenDegreeLimit
    TMOLimit
///

doc ///
  Key
    [findTrivialMasseyOperation,TMOLimit]
  Headline
    Maximum arity of Massey products searched for bounding
  Usage
    findTrivialMasseyOperation(..., TMOLimit => n)
  Description
    Text
      See @ TO TMOLimit @.
  SeeAlso
    findTrivialMasseyOperation
    TMOLimit
///

doc ///
  Key
    [isHomologyAlgebraTrivial,GenDegreeLimit]
  Headline
    Maximum homological degree of generators used by isHomologyAlgebraTrivial
  Usage
    isHomologyAlgebraTrivial(..., GenDegreeLimit => n)
  Description
    Text
      See @ TO GenDegreeLimit @.
  SeeAlso
    isHomologyAlgebraTrivial
    GenDegreeLimit
///

doc ///
  Key
    [isGolodHomomorphism,GenDegreeLimit]
  Headline
    Maximum homological degree of generators used by isGolodHomomorphism
  Usage
    isGolodHomomorphism(..., GenDegreeLimit => n)
  Description
    Text
      See @ TO GenDegreeLimit @.
  SeeAlso
    isGolodHomomorphism
    GenDegreeLimit
    TMOLimit
///

doc ///
  Key
    [deviations,DegreeLimit]
  Headline
    Maximum homological degree to which deviations are computed
  Usage
    deviations(..., DegreeLimit => n)
  Description
    Text
      Bounds the homological degree of generators reported by @ TO deviations @.
      The default value is @ TT "3" @.
  SeeAlso
    deviations
    deviationsToPoincare
///

doc ///
  Key
    [isGolodHomomorphism,TMOLimit]
  Headline
    Option to cap the arity of the trivial Massey operation used in isGolodHomomorphism
  Usage
    isGolodHomomorphism(...,TMOLimit=>n)
  Description
    Text
      Passes through to @ TO findTrivialMasseyOperation @: only products of
      up to @ TT "n" @ cycle representatives are considered when searching
      for a trivial Massey operation on the Koszul homology.  See
      @ TO TMOLimit @.
  SeeAlso
    isGolodHomomorphism
    TMOLimit
    findTrivialMasseyOperation
///

doc ///
  Key
    Verbosity
  Headline
    Option controlling the verbosity level of DGAlgebras computations
  Usage
    homologyAlgebra(..., Verbosity => n)
    getGenerators(..., Verbosity => n)
  Description
    Text
      Several iterative routines in this package accept a @ TT "Verbosity" @
      option whose value is a nonnegative integer.  Higher values print more
      diagnostic output about the steps being performed (cycle searches, relation
      searches, degree-by-degree progress, etc.).  The default value is @ TT "0" @,
      which suppresses all progress output.
  SeeAlso
    homologyAlgebra
    getGenerators
///

doc ///
  Key
    [homologyAlgebra,Verbosity]
  Headline
    Option to control progress output from homologyAlgebra
  Usage
    homologyAlgebra(...,Verbosity=>n)
  Description
    Text
      Larger values of @ TT "n" @ print progress messages about the
      generator and relation searches carried out by
      @ TO homologyAlgebra @.  The default is @ TT "0" @ (no output).
  SeeAlso
    homologyAlgebra
///

doc ///
  Key
    [getGenerators,Verbosity]
  Headline
    Option to control progress output from getGenerators
  Usage
    getGenerators(...,Verbosity=>n)
  Description
    Text
      Larger values of @ TT "n" @ print progress messages about the
      cycle enumeration and independence checks in @ TO getGenerators @.
      The default is @ TT "0" @ (no output).
  SeeAlso
    getGenerators
///

doc ///
  Key
    (net,DGAlgebra)
  Headline
    Short-form display of a DG algebra
  Usage
    net A
  Inputs
    A:DGAlgebra
  Outputs
    :Net
      a one-line summary of @ TT "A" @
  Description
    Text
      Produces the text Macaulay2 prints when a @ TO DGAlgebra @ appears at the top
      level.  The display names the underlying ring, the generators of @ TT "A.natural" @,
      and any differentials known so far, giving a compact identifier rather than a
      full data dump.  Use @ TT "peek A" @ or @ TT "displayBlockDiff" @ for a detailed
      view of the internal state.
    Example
      R = ZZ/101[x,y,z] / ideal(x^3, y^3, z^3)
      A = koszulComplexDGA R
      net A
      A
  SeeAlso
    DGAlgebra
    displayBlockDiff
///

doc ///
  Key
    (net,DGAlgebraMap)
  Headline
    Short-form display of a DG algebra map
  Usage
    net phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    :Net
      a short summary of @ TT "phi" @
  Description
    Text
      Produces the text Macaulay2 prints when a @ TO DGAlgebraMap @ is displayed at
      the top level.  The output lists the action of @ TT "phi" @ on the generators
      of the underlying algebra, using the information stored in @ TT "phi.natural" @.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3 + b^3 + c^3, a*b*c}
      K1 = koszulComplexDGA(ideal vars R, Variable => "Y")
      K2 = koszulComplexDGA(ideal{b, c}, Variable => "T")
      g = dgAlgebraMap(K1, K2, matrix{{Y_(1,2), Y_(1,3)}})
      net g
      g
  SeeAlso
    DGAlgebraMap
    dgAlgebraMap
///

doc///
 Key 
  displayBlockDiff
  (displayBlockDiff, DGAlgebra, ZZ)
  (displayBlockDiff, DGAlgebra, Array, Array)
  (displayBlockDiff, DGAlgebra, List, List)
  (displayBlockDiff, DGAlgebra, VisibleList)  
 Headline
  Shows natural decomposition of a map in the Tate resolution
 Usage
  displayBlockDiff (A, n)
  displayBlockDiff (A, LA, LA)
  displayBlockDiff (A, L, L)
  displayBlockDiff (A, V)
 Inputs
  A:DGAlgebra
   of the sort produced by @TO acyclicClosure@ or @TO killCycles@
  n:ZZ
  LA:Array
     of the form [L] where L is a list representing a multi-index in the complex
  L:List
   of indices as above
  V:VisibleList
   a list, array or sequence representing a (single) multi-index in the complex defined by A
 Description
  Text
   For example, consider the first five steps in the resolution of the residue field
   in the following example:
  Example
   R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
   G = betti freeResolution (coker vars R, LengthLimit => 5)
  Text
   It is a free graded-commutative divided power algebra on generator of each degree starting with 1.
   We can compute the beginning of the complex corresponding to the first 3 factors with as
  Example
   A = acyclicClosure(R,EndDegree => 2)
   F = toComplex(A, 5);
   betti F
   betti F
  Text
   Since we gave @TO acyclicClosure @ the EndDegree 2, the complex produced is exact up to
   step 2; that is betti F and betti G agree up to the column 3. There are 3 chunks of generators
   in A, of homological degrees 1,2,3. 
  
   From the betti table of F 
   we see the three generators of the exterior algebra K as the linear part of the resolution.
   The second strand, 4,12,12,4 is the tensor product of K with the 4 generators of A that have
   homological degree 2 and correspond to the 4 generators of I; thus they have internal degree 3.
   We also see that A has three generators in homological degree 3 and internal degree 5.
   K multiplied by these generators accounts for 3,9,9 of the 3rd strand, while the symmetric
   square of the 4 generators of homological degree 2,  account for 0,10,0 and the product of
   these with the generators of degree 1 account for the remaining 0,0,30. Finally the 12
   in the 4th row represents the product of the 4 generators of A in homological degree with
   the 3 in degree 3.
  
   The native display of the differentials of this complex does not distinguish these pieces,
   but displayBlockDiff allows one to look at them in various ways:
  Example
   F.dd_3
   displayBlockDiff(A,3)
  Text
   Here the triples of numbers represent the number of factors from the generators of each of the three chunks of
   variables: the first index gives the number from the Koszul complex, the second from the 4 variable in homological degree
   2 and the third the number from the 3 variables of homological degree 3. Thus the sum of the
   three indices is the homological degree. The sources of the blocks are listed on the top row, the targets
   are given by the columns.
   We can see the lists of indices of the source and target with
    Example
     indices source blockDiff(A,5)
     indices target blockDiff(A,5)
  Text
   We can extract one or several blocks from F.dd_3 as follows:
  Example
   R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
   A = acyclicClosure(R,EndDegree => 2)
  Text
   We can specify a lists of source and target degrees, either as lists representing an 
   index for th source and an index for the target:
  Example
   displayBlockDiff(A,  {0,2,3}, {0,4,0})
  Text
   or as a pair of arrays of such lists
  Example
   displayBlockDiff(A,  [{0,2,3}], [{1,0,3},{0,4,0}])
  Text
   or look at all the blocks with a given target summand with:
  Example
   displayBlockDiff(A,  (1,0,3) )
 SeeAlso
  blockDiff
  acyclicClosure
///

doc ///
 Key
  blockDiff
  (blockDiff, DGAlgebra, ZZ)
 Headline
  prepares a map for display
 Usage
  X = blockDiff(A,n)
 Inputs
  A: DGAlgebra
  n: ZZ
 Outputs
  X:Matrix
   map between labeled direct sums
 Description
  Text
   Use @TO displayBlockDiff@ to display the blocks in various ways.
 SeeAlso
  displayBlockDiff 
///
  
-------------------------------
--          Testing          --
-------------------------------
TEST ///
R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
A = acyclicClosure(R,EndDegree => 2)
X = blockDiff(A,3)
Y = blockDiff(A,2)
--reconstruct a composite block:
assert(0 == Y_[{2,0,0}]^[{1,0,0}] *X^[{2,0,0}]_[{1,2,0}] + Y_[{0,2,0}]^[{1,0,0}] *X^[{0,2,0}]_[{1,2,0}] )

///
TEST ///
-- test 0 : isHomogeneous, toComplex, maxDegree
R = ZZ/101[x,y,z]
A1 = freeDGAlgebra(R,{{1},{1},{1},{3}})
setDiff(A1,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
assert(not A1.isHomogeneous)
A1dd = toComplex(A1)
A1dd.dd

A2 = freeDGAlgebra(R,{{1,1},{1,1},{1,1},{3,3}})
setDiff(A2,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
assert(A2.isHomogeneous)
A2dd = toComplex(A2)
A2dd.dd

B1 = koszulComplexDGA(R)
assert(B1.isHomogeneous)
B1dd = toComplex(B1)
B1dd.dd

R = ZZ/101[x,y,z]
R2 = R/ideal {x^2-z^3}
B2 = koszulComplexDGA(R2)
assert(not B2.isHomogeneous)
B2dd = toComplex(B2)
B2dd.dd

R = QQ[x,y,z]
B = koszulComplexDGA(R)
toComplex(B)
degrees B.natural
A = freeDGAlgebra(R,{{1},{1},{1},{3}})
setDiff(A,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
Add = toComplex(A)
assert(apply(maxDegree(A)+1, i -> prune HH_i(Add)) == {coker vars R,0,0,coker vars R,0,0,0})
///

TEST ///
-- test 1 : differential tests
R = ZZ/101[x,y,z, Degrees => {2,2,3}]
kRes = freeResolution coker vars R
kRes.dd_3
A = koszulComplexDGA(R)
d3 = A.dd_3
d2 = A.dd_2
d1 = A.dd_1
assert(source d1 == target d2)
assert(source d2 == target d3)
assert(d1*d2 == 0)
assert(d2*d3 == 0)
S1 = R/ideal (x^3-z^2)
B1 = koszulComplexDGA(S1)
d3 = B1.dd_3
d2 = B1.dd_2
d1 = B1.dd_1
assert(source d1 == target d2)
assert(source d2 == target d3)
assert(d1*d2 == 0)
assert(d2*d3 == 0)
use R
S2 = R/ideal (x^4-z^2)
B2 = koszulComplexDGA(S2)
d3 = B2.dd_3
d2 = B2.dd_2
d1 = B2.dd_1
assert(source d1 == target d2)
assert(source d2 == target d3)
assert(d1*d2 == 0)
assert(d2*d3 == 0)
///

TEST ///
--- test 2 : homology, homologyAlgebra, HH_ZZ, HH
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
HH_2(A)
HH(A)
hh2 = prune HH_2(koszulR)
hh2' = prune HH_2(A)
assert(hh2 == hh2')
///

TEST ///
-- test 3 : torAlgebra, deviations
R1 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
TorR1 = torAlgebra(R1,GenDegreeLimit=>4)
devR1 = deviations(R1,DegreeLimit=>4)
use R1
M = coker matrix {{x^2*y^3*z^4}}
Mres = freeResolution(M, LengthLimit => 7)
R2 = QQ[x,y,z]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
time TorR1R2 = torAlgebra(R1,R2,GenDegreeLimit=>5,RelDegreeLimit=>10)
-- the multiplication is trivial, since the map R3 --> R4 is Golod
numgens TorR1R2
numgens ideal TorR1R2
apply(21, i -> #(flatten entries getBasis(i,TorR1R2)))
assert(sum oo - 1 == numgens TorR1R2)
///

TEST ///
-- test 4 : findEasyRelations
debug DGAlgebras
R1 = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
R2 = ZZ/32003[a,b,x,y,Degrees=>{1,1,2,2}]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
A1 = koszulComplexDGA(R1)
A2 = koszulComplexDGA(R2)
cycleList1 = getGenerators(A1,DegreeLimit=>4)
cycleList2 = getGenerators(A2,DegreeLimit=>4)
HAEasy1 = findEasyRelations(A1,cycleList1)
HAEasy2 = findEasyRelations(A2,cycleList2)
tally ((flatten entries basis HAEasy1) / degree)
pairs (tally ((flatten entries basis HAEasy1) / degree))
myList1 = {({4,8},1),({3,4},1),({3,5},6),({3,6},6),({3,7},4),({2,3},4),({2,4},11),({2,5},8),({2,6},4),({1,2},4),({1,3},4),({1,4},1),({0,0},1)}
myList2 = {({0},1),({1},9),({2},27),({3},17),({4},1)}
tally ((flatten entries basis HAEasy1) / degree)
tally myList1
assert(pairs tally((flatten entries basis HAEasy1) / degree) == myList1)
assert(pairs tally((flatten entries basis HAEasy2) / degree) == myList2)
///

TEST ///
-- test 5 : homology of a DGA whose H_0 is not a field
R = ZZ/32003[a,b]
I = ideal{a^6,b^6}
A = koszulComplexDGA(I)
HA = HH A
describe HA
use R
J = I + ideal {a^4*b^5,a^5*b^4}
B = koszulComplexDGA(J)
getGenerators(B)
apply(5, i -> numgens prune homology(i,B))
apply(5, i -> prune homology(i,B))
HB = HH B
HB2 = zerothHomology B
HB.cache.cycles
ideal HB
-- looks right...
getDegNModule(0,HB2,HB)
getDegNModule(1,HB2,HB)
getDegNModule(2,HB2,HB)
getDegNModule(3,HB2,HB)
getDegNModule(4,HB2,HB)

R = ZZ/32003[a,b,c]
I = (ideal vars R)^2
A = koszulComplexDGA(I)
apply(10, i -> prune homology(i,A))
time HA = HH A
HA2 = zerothHomology A
tally ((ideal HA)_* / degree / first)
select ((ideal HA)_*, f -> first degree f == 2)
-- looks right...
getDegNModule(0,HA2,HA)
getDegNModule(1,HA2,HA)
getDegNModule(2,HA2,HA)
getDegNModule(3,HA2,HA)
-- need to add asserts
///

TEST ///
-- test 6 : homologyAlgebra
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
time apply(5,i -> numgens prune homology(i,A))
-- ~2.15 seconds on mbp, with graded differentials
time HA = HH A
assert(numgens HA == 34)
assert(numgens ideal HA == 576)
assert(#(first degrees HA) == 2)

-- same example, but not graded because of the degree change.  The homologyAlgebra function
-- will then only return a graded algebra
R2 = ZZ/32003[a,b,x,y,Degrees=>{1,1,2,2}]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR2 = koszul vars R2
time apply(5,i -> numgens prune HH_i(koszulR2))
A2 = koszulComplexDGA(R2)
time apply(5,i -> numgens prune homology(i,A2))
-- ~2.85 seconds on mbp, with ungraded differentials
time HA2 = homologyAlgebra A2
assert(numgens HA2 == 34)
assert(numgens ideal HA2 == 576)
-- should only be singly graded
assert(#(first degrees HA2) == 1)
///

TEST ///
-- test 7 : acyclicClosure, isHomologyAlgebraTrivial, isGolod, isGolodHomomorphism
R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
M = coker matrix {{a^3*b^3*c^3*d^3}};
S = R/ideal{a^3*b^3*c^3*d^3}
time A = acyclicClosure(R,EndDegree=>6)
B = A ** S
assert(isHomologyAlgebraTrivial(B,GenDegreeLimit=>6))
assert(isGolodHomomorphism(S,GenDegreeLimit=>6,TMOLimit=>3))
-- returns true since R --> S is Golod
R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
A = koszulComplexDGA(R)
assert(not isHomologyAlgebraTrivial(A))
assert(not isGolod R)
-- false, since R is Gorenstein, and so HA has Poincare Duality
///

TEST ///
-- test 8 : DGAlgebra ** DGAlgebra - need to add in an assert
R = ZZ/101[a,b,c,d]
I = ideal(a,b)
J = ideal(c,d)
A = koszulComplexDGA(I)
B = koszulComplexDGA(J)
Cdd = toComplex(A ** B)
Cdd.dd
///

TEST ///
-- test 9 : isHomologyAlgebraTrivial, getGenerators, findTrivialMasseyOperation
Q = ZZ/101[x_1..x_6]
I = ideal (x_3*x_5,x_4*x_5,x_1*x_6,x_3*x_6,x_4*x_6)
R = Q/I
A = koszulComplexDGA(R)
isHomologyAlgebraTrivial(A,GenDegreeLimit=>3)
cycleList = getGenerators(A)
assert(first findTrivialMasseyOperation(A))

-- this is a Teter ring, and the computation in Avramov and Levin's paper shows
-- H(A) does not have trivial multiplication.
Q = ZZ/101[x,y,z]
I = ideal (x^3,y^3,z^3,x^2*y^2*z^2)
R = Q/I
A = koszulComplexDGA(R)
assert(not isHomologyAlgebraTrivial(A,GenDegreeLimit=>3))
cycleList = getGenerators(A)
prodList = apply(subsets(cycleList,2), l -> (first degree l#0 + first degree l#1,l#0*l#1));
assert(not first findTrivialMasseyOperation(A))
///

TEST ///
-- test 10 : isAcyclic
R = ZZ/101[a,b,c,d]
A = koszulComplexDGA(R)
B = koszulComplexDGA({a^4,b^4,c^4,d^4})
C = koszulComplexDGA((ideal vars R)^2)
assert(isAcyclic A)
assert(isAcyclic B)
assert(not isAcyclic C)
///

TEST ///
-- test 11 : isGolod and isHomologyAlgebraTrivial example
-- Interesting case due to Katthan.
Q = ZZ/101[x_1,x_2,y_1,y_2,z,w]
I = ideal {x_1*x_2^2,z^2*w,y_1*y_2^2,x_2^2*z*w,y_2^2*z^2,x_1*x_2*y_1*y_2,x_2^2*y_2^2*z,x_1*y_1*z}
R = Q/I
assert(isHomologyAlgebraTrivial koszulComplexDGA R == true)
assert(isGolod R == false)
///

TEST ///
--- test 12: isGolod and isHomologyAlgebraTrivial example again
--- This example is due to Roos
S = QQ[x,y,z,u]
I = ideal(u^3, x*y^2, (x+y)*z^2, x^2*u+z*u^2, y^2*u+x*z*u, y^2*z+y*z^2)
 -- you can see that the mult on the koszul homology will be trivial
betti (A = freeResolution I)
R = S/I
assert(isHomologyAlgebraTrivial koszulComplexDGA R == true)
assert(isGolod R == false)
///

TEST ///
-- test 13: freeDGAlgebra variable-naming convention and Variable option
R = QQ[x, y, z]
A = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
-- Default base symbol T, doubly-indexed by (hom-degree, counter).
assert(toString gens A.natural == "{T_(1,1), T_(1,2), T_(1,3), T_(3,1)}")
B = freeDGAlgebra(R, {{1}, {1}, {1}, {3}}, Variable => "U")
assert(toString gens B.natural == "{U_(1,1), U_(1,2), U_(1,3), U_(3,1)}")
-- List form: each symbol supplied verbatim.
C = freeDGAlgebra(R, {{1}, {1}, {1}}, Variable => {getSymbol "P", getSymbol "Q", getSymbol "RR"})
assert(toString gens C.natural == "{P, Q, RR}")
assert(numgens C.natural == 3)
-- Multigrade vs single-grade homogeneity.
D1 = freeDGAlgebra(R, {{1, 1}, {1, 1}, {1, 1}, {3, 3}})
setDiff(D1, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
assert(isHomogeneous D1)
D2 = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
setDiff(D2, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
assert(not isHomogeneous D2)
///

TEST ///
-- test 14: koszulComplexDGA naming convention and Variable option
R = QQ[a, b, c]
A = koszulComplexDGA R
assert(toString gens A.natural == "{T_(1,1), T_(1,2), T_(1,3)}")
AS = koszulComplexDGA(R, Variable => "S")
assert(toString gens AS.natural == "{S_(1,1), S_(1,2), S_(1,3)}")
-- Koszul complex on a regular sequence has acyclic higher homology.
assert(isAcyclic A)
-- Differentials on Koszul gens match the ring vars.
natGens = gens A.natural
assert((A.diff) natGens_0 == sub(a, A.natural))
assert((A.diff) natGens_1 == sub(b, A.natural))
assert((A.diff) natGens_2 == sub(c, A.natural))
-- (koszulComplexDGA, List) form: regular sequence in polynomial ring.
S = QQ[x, y, z]
B = koszulComplexDGA({x^2, y*z})
assert(numgens B.natural == 2)
-- (x^2, yz) is regular in QQ[x,y,z], so Koszul homology vanishes in deg > 0.
assert(prune HH_1 toComplex B == 0)
assert(prune HH_2 toComplex B == 0)
///

TEST ///
-- test 15: acyclicClosure Tate construction and variable convention
-- C.i. case: exactly three hom-degree-2 gens kill the three Koszul relation cycles.
R = ZZ/101[a, b, c] / ideal(a^3, b^3, c^3)
A = koszulComplexDGA R
B = acyclicClosure(A, EndDegree => 2)
assert(numgens B.natural == 6)
-- The Tate theorem: for a c.i., no further gens appear past hom-degree 2.
B2 = acyclicClosure(A, EndDegree => 4)
assert(numgens B2.natural == numgens B.natural)
-- Differentials on the newly adjoined generators are a_i^2 * T_(1,i).
(aa, bb, cc) = (sub(a, B.natural), sub(b, B.natural), sub(c, B.natural))
natGens = gens B.natural
-- First three gens are T_(1,*) with diff a,b,c.
assert((B.diff) natGens_0 == aa)
assert((B.diff) natGens_1 == bb)
assert((B.diff) natGens_2 == cc)
-- Next three gens are T_(2,*) with diff a_i^2 * T_(1,i).
assert((B.diff) natGens_3 == aa^2 * natGens_0)
assert((B.diff) natGens_4 == bb^2 * natGens_1)
assert((B.diff) natGens_5 == cc^2 * natGens_2)
-- Variable => ... renames only the new generators.
D = acyclicClosure(A, EndDegree => 2, Variable => "U")
assert(toString take(gens D.natural, 3) == "{T_(1,1), T_(1,2), T_(1,3)}")
assert(toString drop(gens D.natural, 3) == "{U_(2,1), U_(2,2), U_(2,3)}")
///

TEST ///
-- test 16: acyclicClosure deviation sequences
-- Non-c.i. has higher deviations; c.i. does not.
S = QQ[x, y, z] / ideal(x^2, y^2, z^2)
CS = acyclicClosure(S, EndDegree => 4)
homdegCounts = tally apply(CS.Degrees, d -> first d)
assert(homdegCounts#1 == 3)     -- three Koszul exterior gens
assert(homdegCounts#2 == 3)     -- three Tate polynomial gens
assert(not homdegCounts#?3)     -- no hom-deg 3 gens (c.i.)
assert(not homdegCounts#?4)     -- no hom-deg 4 gens (c.i.)
T = QQ[x, y] / ideal(x^2, x*y, y^2)
CT = acyclicClosure(T, EndDegree => 3)
homdegCountsT = tally apply(CT.Degrees, d -> first d)
assert(homdegCountsT#1 == 2)    -- two Koszul exterior gens
assert(homdegCountsT#2 == 3)    -- three relation-killers
assert(homdegCountsT#?3)        -- non-c.i. has hom-deg 3 deviation
///

TEST ///
-- test 17: adjoinVariables preserves existing names, cycleCheck via polyDifferential
R = ZZ/101[a, b, c] / ideal(a^3, b^3, c^3)
A = koszulComplexDGA R
z = a^2 * (gens A.natural)_0
assert(polyDifferential(A, z) == 0)
B = adjoinVariables(A, {z})
assert(numgens B.natural == numgens A.natural + 1)
-- First three gens of B preserve A's names; new gen continues the counter.
assert(toString take(gens B.natural, 3) == "{T_(1,1), T_(1,2), T_(1,3)}")
-- Variable => ... renames only the new generator.
C = adjoinVariables(A, {z}, Variable => "U")
assert(toString last gens C.natural == "U_(2,1)")
-- The added generator's differential is exactly the cycle z.
assert((B.diff) last gens B.natural == sub(z, B.natural))
///

TEST ///
-- test 18: setDiff ordering, validity via isWellDefined
R = ZZ/101[x, y, z]
A = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
setDiff(A, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
-- Differentials are in gen-order matching A.Degrees.
diffs = take(flatten entries matrix A.diff, numgens A.natural)
assert(diffs#0 == x)
assert(diffs#1 == y)
assert(diffs#2 == z)
assert(isWellDefined A)
///

TEST ///
-- test 19: freeDGModule construction, setDiff, and matrix factorization
-- 2-periodic min. free resolution of k = R/x over R = QQ[x]/(x^2).
R = QQ[x] / ideal(x^2)
A = koszulComplexDGA R
F = freeDGModule(A, toList(0..4))
natGens = apply(rank F.natural, i -> (F.natural)_i)
setDiff(F, {0, x * natGens#0, x * natGens#1, x * natGens#2, x * natGens#3})
-- Every consecutive pair of differentials composes to zero.
d1 = moduleDifferential(1, F)
d2 = moduleDifferential(2, F)
d3 = moduleDifferential(3, F)
d4 = moduleDifferential(4, F)
assert(d1 * d2 == 0)
assert(d2 * d3 == 0)
assert(d3 * d4 == 0)
assert(isWellDefined F)
///

TEST ///
-- test 20: koszulComplexDGM over regular ring vs complete intersection
-- Regular ring: higher Koszul-module homology vanishes.
S1 = QQ[x, y, z]
KM1 = koszulComplexDGM S1^1
C1 = toComplex KM1
assert(prune HH_1 C1 == 0)
assert(prune HH_2 C1 == 0)
assert(prune HH_3 C1 == 0)
-- Complete intersection: higher Koszul-module homology is nonzero.
S2 = QQ[x, y] / ideal(x^2, y^2)
KM2 = koszulComplexDGM S2^1
C2 = toComplex KM2
assert(prune HH_1 C2 != 0)
assert(prune HH_2 C2 != 0)
///

TEST ///
-- test 21: dgModuleMap mult-by-y on semifree resolution of residue field
-- Over R = k[x,y]/(x^2,y^2), multiplying by y annihilates the residue field,
-- so the induced chain map has zero action on all homology.
R = QQ[x, y] / ideal(x^2, y^2)
A = koszulComplexDGA R
Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 3)
natGens = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
fy = dgModuleMap(Mdg, Mdg, apply(natGens, g -> y * g))
assert(isWellDefined fy)
cmy = toComplexMap fy
assert(all(0..3, n -> prune HH_n cmy == 0))
-- Matrix form also works for the identity.
idmat = id_(Mdg.natural)
g = dgModuleMap(Mdg, Mdg, idmat)
assert(isWellDefined g)
assert(g == identityDGModuleMap Mdg)
///

TEST ///
-- test 22: identityDGModuleMap and zeroDGModuleMap composition laws
R = ZZ/101[x, y] / ideal(x^2, y^2)
A = koszulComplexDGA R
KM = koszulComplexDGM R^1
-- id_KM is the identity DGModuleMap; also compares equal to 1.
assert(id_KM == identityDGModuleMap KM)
assert(id_KM == 1)
-- Composition with mult-by-y on a semifree resolution.
Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 2)
natGens = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
fy = dgModuleMap(Mdg, Mdg, apply(natGens, v -> y * v))
idMdg = identityDGModuleMap Mdg
assert(idMdg * fy == fy)
assert(fy * idMdg == fy)
assert(idMdg * idMdg == idMdg)
-- Zero map: neutral for +, absorbing for *.
zM = zeroDGModuleMap(Mdg, Mdg)
assert(idMdg + zM == idMdg)
assert(zM * idMdg == zM)
assert(idMdg * zM == zM)
-- Cokernel of zero recovers the module (rank equality).
Q = cokernel zM
assert(rank Q.natural == rank Mdg.natural)
K = kernel zM
assert(instance(K, DGSubmodule))
///

TEST ///
-- test 23: dgSubmodule d-closure on a minimal semifree resolution
R = QQ[x, y] / ideal(x^2, y^2)
A = koszulComplexDGA R
Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 3)
-- Seeding with hom-degree-2 gens; d-closure pulls in hom-degree-4 gens.
pos2 = positions(Mdg.Degrees, d -> d#0 == 2)
seedHi = (id_(Mdg.natural))_pos2
Shi = dgSubmodule(Mdg, seedHi)
assert(isWellDefined Shi)
assert(maxDegree Shi == 4)
-- Higher-degree gens are pulled in by d-closure (dim > seed dim).
assert(rank (inclusion Shi).source > numcols seedHi)
///

TEST ///
-- test 24: dgIdeal d-saturation of a non-cycle seed
R = QQ[x, y]
A = koszulComplexDGA R
T1 = (A.natural)_0
T2 = (A.natural)_1
-- T_1 * T_2 is not a cycle: d(T_1*T_2) = -y*T_1 + x*T_2.
dz = polyDifferential(A, T1 * T2)
assert(dz != 0)
J = dgIdeal(A, {T1 * T2})
-- d-saturation should include both T_1 * T_2 and its differential.
mg = mingens J.natural
assert(numcols mg == 2)
-- Unit and zero edge-cases.
Z = dgIdeal(A, {})
assert(isZero Z)
U = dgIdeal(A, {1_(A.natural), x_(A.natural)})
assert(U.natural == ideal 1_(A.natural))
///

TEST ///
-- test 25: DGSubmodule / DGQuotientModule types and accessors
R = ZZ/101[x]
A = koszulComplexDGA R
M = freeDGModule(A, {0})
S = dgSubmodule(M, id_(M.natural))
assert(ambient S === M)
assert(source inclusion S === S)
assert(target inclusion S === M)
Z = dgSubmodule(M, {})
Q = M / Z
assert(instance(Q, DGQuotientModule))
assert(ambient Q === M)
assert(subDGModule Q === Z)
assert(source projection Q === M)
assert(target projection Q === Q)
-- freeDGModule with multigraded ring
Rg = ZZ/101[x, y, Degrees => {{1, 0}, {0, 1}}]
Ag = koszulComplexDGA Rg
Mg = freeDGModule(Ag, {{0, 0, 0}, {1, 0, 0}})
assert(#degrees Mg == 2)
-- dgQuotientModule explicit constructor
Qexp = dgQuotientModule(M, Z)
assert(instance(Qexp, DGQuotientModule))
assert(ambient Qexp === M)
assert(subDGModule Qexp === Z)
-- full-sub quotient is zero
Sfull = dgSubmodule(M, id_(M.natural))
assert(isZero (M / Sfull))
-- ambient DGIdeal
I = dgIdeal(A, {x_(A.natural)})
assert(ambient I === A)
///

TEST ///
-- test 26: module-like API on DG modules (numgens/rank/degrees/isHomogeneous/isFreeDGModule/isZero)
R = ZZ/101[x, y]
A = koszulComplexDGA R
Anat = A.natural
M = freeDGModule(A, {0, 1, 2})
assert(numgens M == 3)
assert(rank M == 3)
assert(degrees M == {{0,0},{1,0},{2,0}})
assert(isHomogeneous M)
assert(isFreeDGModule M)
assert(not isZero M)
-- Submodule accessors
M1 = freeDGModule(A, {0})
S = dgSubmodule(M1, matrix {{x_Anat, y_Anat}})
assert(numgens S == 2)
assert(isHomogeneous S)
Q = M1 / S
assert(numgens Q == numgens M1)
assert(isHomogeneous Q)
assert(super S === M1)
assert(super Q === M1)
assert(cover Q === M1)
assert(numcols relations Q == 2)
-- Zero module
M0 = freeDGModule(A, {})
assert(isZero M0)
assert(numgens M0 == 0)
Zsub = dgSubmodule(M1, map(M1.natural, Anat^0, 0))
assert(isZero Zsub)
Full = dgSubmodule(M1, id_(M1.natural))
assert(isZero (M1 / Full))
-- Free-after-tensor
MNK = (M ** R^2) ** R^2
assert(isFreeDGModule MNK)
///

TEST ///
-- test 27: DGSubmodule operations (+, intersect, ==, isSubset)
R = ZZ/101[x, y]
A = koszulComplexDGA R
Anat = A.natural
M = freeDGModule(A, {0})
S = dgSubmodule(M, matrix {{x_Anat}})
T = dgSubmodule(M, matrix {{y_Anat}})
ST = S + T
assert(isWellDefined ST)
assert(isSubset(S, ST) and isSubset(T, ST))
assert((S + S) == S)
-- intersect
T2 = dgSubmodule(M, matrix {{x_Anat * y_Anat}})
cap = intersect(S, T2)
assert(isWellDefined cap)
assert(isSubset(cap, S) and isSubset(cap, T2))
Zsub = dgSubmodule(M, map(M.natural, Anat^0, 0))
assert(isZero intersect(S, Zsub))
-- equality/inclusion
U = dgSubmodule(M, matrix {{x_Anat, y_Anat}})
assert(isSubset(S, U))
assert(not isSubset(U, S))
assert(S == S)
-- Different ambient => not equal
M' = freeDGModule(A, {0})
S' = dgSubmodule(M', matrix {{x_Anat}})
assert(not (S == S'))
///

TEST ///
-- test 28: image, kernel, cokernel of DGModuleMap
R = ZZ/101[x]
A = koszulComplexDGA R
M = freeDGModule(A, {0, 1})
natGens = apply(rank M.natural, i -> (M.natural)_i)
setDiff(M, {0, x * natGens#0})
idM = identityDGModuleMap M
zM = zeroDGModuleMap(M, M)
-- image
imId = image idM
imZ = image zM
assert(isWellDefined imId)
assert(isWellDefined imZ)
assert(numcols (inclusion imId).natural == rank M.natural)
assert(numcols (inclusion imZ).natural == 0)
-- image of inclusion recovers the submodule
Sfull = dgSubmodule(M, id_(M.natural))
assert((image (inclusion Sfull)) == Sfull)
-- kernel
kerId = kernel idM
kerZ = kernel zM
assert(isWellDefined kerId)
assert(isWellDefined kerZ)
assert(numcols (inclusion kerId).natural == 0)
assert(numcols (inclusion kerZ).natural == rank M.natural)
assert(isZero kernel (inclusion Sfull))
-- cokernel
assert(isZero cokernel idM)
Q = cokernel zM
assert(ambient Q === M)
assert(subDGModule Q == image zM)
assert(isZero cokernel (inclusion Sfull))
///

TEST ///
-- test 29: homology of DG modules and DG module maps
R = QQ[x, y]/ideal(x^2, y^2)
A = koszulComplexDGA R
k = R^1 / ideal(x, y)
M = minimalSemifreeResolution(A, k, EndDegree => 2)
H0 = prune homology(0, M)
assert(numgens H0 == 1)
idM = identityDGModuleMap M
h0 = homology(idM, 0)
assert(h0 == id_(source h0))
-- Regular ring: higher Koszul-module homology vanishes.
RR1 = QQ[x, y, z]
KM = koszulComplexDGM RR1^1
assert(numgens prune homology(0, KM) == 1)
assert(prune homology(1, KM) == 0)
assert(prune homology(2, KM) == 0)
-- homology functor on DG module
HM = homology M
assert(ring HM === HH A)
HM' = homologyModule M
assert(HM == HM')
-- homology of mult-by-x is zero on resolution of residue field
natGens = apply(rank M.natural, i -> (M.natural)_i)
xMap = dgModuleMap(M, M, apply(natGens, g -> x * g))
h0x = homology(xMap, 0)
assert(h0x == map(target h0x, source h0x, 0))
-- zero map induces zero homology
zM = zeroDGModuleMap(M, M)
h0z = homology(zM, 0)
assert(h0z == map(target h0z, source h0z, 0))
///

TEST ///
-- test 30: homology of DGQuotientModule and homologyClass
R = ZZ/101[x]
A = koszulComplexDGA R
M = freeDGModule(A, {0, 1})
natGens = apply(rank M.natural, i -> (M.natural)_i)
setDiff(M, {0, x * natGens#0})
S = dgSubmodule(M, (id_(M.natural))_{1})
Q = M / S
h0 = prune homology(0, Q)
h1 = prune homology(1, Q)
h2 = prune homology(2, Q)
assert(numgens h0 == 1)
assert(numgens h1 == 1)
assert(h2 == 0)
HQ = homology Q
assert(ring HQ === HH A)
HQ' = homologyModule Q
assert(HQ == HQ')
-- homologyClass: cycle is nonzero, boundary is zero
z = natGens#0
H = prune homology(0, M)
hc = homologyClass(M, z)
assert(hc != 0_H)
b = x * natGens#0
hb = homologyClass(M, b)
assert(hb == 0_(prune homology(0, M)))
///

TEST ///
-- test 31: prune / minimalPresentation
R = ZZ/101[x, y]
A = koszulComplexDGA R
Anat = A.natural
M = freeDGModule(A, {0})
-- Redundant seed pruned to one generator.
S = dgSubmodule(M, matrix {{1_Anat, x_Anat, y_Anat}})
Sp = prune S
assert(numcols (inclusion S).natural == 3)
assert(numcols (inclusion Sp).natural == 1)
assert(image (inclusion S).natural == image (inclusion Sp).natural)
assert(isWellDefined Sp)
Sp' = minimalPresentation S
assert(numcols (inclusion Sp').natural == 1)
-- prune on quotient
Q = M / S
Qp = prune Q
assert(image (inclusion Qp.subDGModule).natural == image (inclusion S).natural)
Qp' = minimalPresentation Q
assert(image (inclusion Qp'.subDGModule).natural == image (inclusion S).natural)
-- prune on DGModule / DGAlgebra / maps is identity
Mfree = freeDGModule(A, {0, 1})
assert((prune Mfree) === Mfree)
assert((minimalPresentation Mfree) === Mfree)
assert((prune A) === A)
assert((minimalPresentation A) === A)
phi = identityDGAlgebraMap A
assert((prune phi) === phi)
assert((minimalPresentation phi) === phi)
idMp = identityDGModuleMap Mfree
assert((prune idMp) === idMp)
assert((minimalPresentation idMp) === idMp)
///

TEST ///
-- test 32: predicates (isWellDefined, isAcyclic, isQuasiIsomorphism, annihilator)
R = QQ[x, y, z]
A = koszulComplexDGA R
assert(isWellDefined A)
assert(isAcyclic A)
-- Acyclic on a DG module (Koszul on regular ring)
KM = koszulComplexDGM R^1
assert(isAcyclic KM)
-- Non-acyclic on c.i.
Rci = QQ[x, y]/ideal(x^2, y^2)
KMci = koszulComplexDGM Rci^1
assert(not isAcyclic(KMci, EndDegree => 3))
-- Identity DGAlgebraMap is quasi-iso
phi = identityDGAlgebraMap A
assert(isWellDefined phi)
assert(isQuasiIsomorphism phi)
-- Submodule/quotient well-definedness
Anat = A.natural
Mfree = freeDGModule(A, {0})
Sub = dgSubmodule(Mfree, matrix {{1_Anat}})
Quo = Mfree / Sub
assert(isWellDefined Sub)
assert(isWellDefined Quo)
-- isWellDefined on DGModuleMap
Aci = koszulComplexDGA Rci
Mdg = minimalSemifreeResolution(Aci, Rci^1 / ideal(x, y), EndDegree => 2)
idM = identityDGModuleMap Mdg
zM = zeroDGModuleMap(Mdg, Mdg)
assert(isWellDefined idM)
assert(isWellDefined zM)
-- annihilator of submodule
A2 = koszulComplexDGA (ZZ/101[x, y])
A2nat = A2.natural
M2 = freeDGModule(A2, {0})
Sx = dgSubmodule(M2, matrix {{x_A2nat}})
Ian = annihilator Sx
assert(isWellDefined Ian)
S0 = dgSubmodule(M2, map(M2.natural, A2nat^0, 0))
assert(annihilator S0 == dgIdeal(A2, {1_A2nat}))
-- annihilator of quotient
Q2 = M2 / Sx
IQ = annihilator Q2
assert(isWellDefined IQ)
assert(isSubset(dgIdeal(A2, {x_A2nat}), IQ))
-- quotient acyclicity edge case
R0 = ZZ/101[x]
A0 = koszulComplexDGA R0
M0 = freeDGModule(A0, {0, 1})
natGens0 = apply(rank M0.natural, i -> (M0.natural)_i)
setDiff(M0, {0, x * natGens0#0})
Sfull0 = dgSubmodule(M0, id_(M0.natural))
assert(isAcyclic (M0 / Sfull0))
///

TEST ///
-- test 33: koszulComplexDGM, adjoinGenerators, killCycles
R = QQ[x, y]
A = koszulComplexDGA R
KM = koszulComplexDGM(A, R^1 / ideal(x))
assert(KM.dgAlgebra === A)
-- adjoinGenerators extends degree list
R2 = QQ[x]
A2 = koszulComplexDGA R2
M = freeDGModule(A2, {0})
natGens = apply(rank M.natural, i -> (M.natural)_i)
Mnew = adjoinGenerators(M, {x * natGens#0})
assert(#Mnew.Degrees == 2)
assert(first Mnew.Degrees#1 == 1)
-- killCycles on a DG module
M2k = adjoinGenerators(M, {x * natGens#0})
assert(prune homology(0, M2k) != 0)
M3 = killCycles(M2k, StartDegree => 1, EndDegree => 2)
assert(prune homology(1, M3) == 0)
///

TEST ///
-- test 34: semifreeResolution / minimalSemifreeResolution / isMinimalSemifreeResolution
R = QQ[x]
A = koszulComplexDGA R
M = R^1 / ideal(x)
Mdg = semifreeResolution(A, M, EndDegree => 3)
assert(prune homology(0, Mdg) != 0)
assert(all(1..3, n -> prune homology(n, Mdg) == 0))
-- 2-arg form w/ ambient ring
Mdg' = semifreeResolution(M, EndDegree => 2)
assert(Mdg'.ring === R)
assert(Mdg'.dgAlgebra.ring === R)
-- Minimal resolution over c.i.
Rci = QQ[x, y] / ideal(x^2, y^2)
kci = Rci^1 / ideal(x, y)
Aci = koszulComplexDGA Rci
Mmin = minimalSemifreeResolution(Aci, kci, EndDegree => 3)
assert(isMinimalSemifreeResolution Mmin)
assert(all(1..3, n -> prune homology(n, Mmin) == 0))
-- Ranks 1,2,3,4 at degrees 0,1,2,3
assert(toList apply(0..3, n -> numcols moduleDifferential(n, Mmin)) == {1, 2, 3, 4})
-- 2-arg minimal
Mm2 = minimalSemifreeResolution(M, EndDegree => 2)
assert(Mm2.ring === R)
assert(isMinimalSemifreeResolution Mm2)
-- isMinimalSemifreeResolution negative
Mbad = freeDGModule(A, {0, 1})
natGens = apply(rank Mbad.natural, i -> (Mbad.natural)_i)
setDiff(Mbad, {0, natGens#0})  -- diff(e_1) = e_0 alone, not minimal
assert(not isMinimalSemifreeResolution Mbad)
///

TEST ///
-- test 35: polyDifferential, polyDiffMonomial, dgComplex, isValidDGModule, isDGSubmodule
R = QQ[x, y, z]
A = koszulComplexDGA R
-- d^2 = 0 on Koszul resolution of regular ring
d1 = polyDifferential(1, A)
d2 = polyDifferential(2, A)
d3 = polyDifferential(3, A)
assert(d1 * d2 == 0)
assert(d2 * d3 == 0)
-- degree 0: R^1 -> R^0
Rci = QQ[x, y] / ideal(x^2, y^2)
Aci = koszulComplexDGA Rci
d0 = polyDifferential(0, Aci)
assert(source d0 == Rci^1 and target d0 == Rci^0)
-- element form and Leibniz
gsci = gens Aci.natural
rpd = polyDiffMonomial(Aci, 0_(Aci.natural))
assert(rpd == 0_(Aci.natural))
r2 = polyDiffMonomial(Aci, gsci#0 * gsci#1)
assert(r2 == x * gsci#1 - y * gsci#0)
-- dgComplex caching
C1 = dgComplex Aci
C2 = dgComplex Aci
assert(C1 === C2)
invalidateDGAlgebraCache Aci
assert(not Aci.cache#?(symbol dgComplex))
C3 = dgComplex Aci
assert(instance(C3, Complex))
-- dgComplex over infinite DGAlgebra (acyclicClosure)
Acc = acyclicClosure(Rci, EndDegree => 2)
assert(instance(dgComplex Acc, Complex))
-- dgComplex on DGModule (caching)
KMci = koszulComplexDGM Rci^1
D1 = dgComplex KMci
D2 = dgComplex KMci
assert(D1 === D2)
assert(isValidDGModule KMci)
-- isDGSubmodule: image and kernel of identityDGModuleMap are d-stable
R0 = ZZ/101[x]
A0 = koszulComplexDGA R0
Mdg = freeDGModule(A0, {0, 1})
natGens0 = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
setDiff(Mdg, {0, x * natGens0#0})
idM = identityDGModuleMap Mdg
imF = image idM
assert(isDGSubmodule(target idM, (inclusion imF).natural))
kerF = kernel idM
assert(isDGSubmodule(source idM, (inclusion kerF).natural))
///

TEST ///
-- test 36: getBoundaryPreimage
R = QQ[x, y] / ideal(x^2, y^2)
A = koszulComplexDGA R
M = freeDGModule(A, {0, 1})
natGens = apply(rank M.natural, i -> (M.natural)_i)
setDiff(M, {0, x * natGens#0})
b = x * natGens#0
(ok, pre) = getBoundaryPreimage(M, b)
assert(ok === true)
assert(diff(M, pre) == b)
-- non-boundary case
M2 = freeDGModule(A, {0})
ng2 = apply(rank M2.natural, i -> (M2.natural)_i)
(ok2, residue) = getBoundaryPreimage(M2, ng2#0)
assert(ok2 === false)
assert(residue != 0)
-- list form: zero + boundary
R3 = QQ[x]
A3 = koszulComplexDGA R3
M3 = freeDGModule(A3, {0, 1})
natGens3 = apply(rank M3.natural, i -> (M3.natural)_i)
setDiff(M3, {0, x * natGens3#0})
b3 = x * natGens3#0
(okL, lifts) = getBoundaryPreimage(M3, {b3, 0_(M3.natural)})
assert(okL === true)
assert(diff(M3, lifts#0) == b3)
assert(diff(M3, lifts#1) == 0)
///

TEST ///
-- test 37: moduleDifferential, diff(DGModule/DGQuotientModule, Vector), displays
R = QQ[x, y] / ideal(x^2, y^2)
k = R^1 / ideal(x, y)
A = koszulComplexDGA R
Mdg = minimalSemifreeResolution(A, k, EndDegree => 2)
d1 = moduleDifferential(1, Mdg)
d2 = moduleDifferential(2, Mdg)
assert(d1 * d2 == 0)
-- (diff, DGModule, Vector)
R2 = QQ[x]
A2 = koszulComplexDGA R2
M = freeDGModule(A2, {0, 1})
natGens = apply(rank M.natural, i -> (M.natural)_i)
setDiff(M, {0, x * natGens#0})
assert(diff(M, natGens#1) == x * natGens#0)
assert(diff(M, natGens#0) == 0_(M.natural))
-- (diff, DGQuotientModule, Vector)
R3 = ZZ/101[x]
A3 = koszulComplexDGA R3
M3 = freeDGModule(A3, {0, 1})
natGens3 = apply(rank M3.natural, i -> (M3.natural)_i)
setDiff(M3, {0, x * natGens3#0})
S = dgSubmodule(M3, (id_(M3.natural))_{1})
Q = M3 / S
assert(instance(projection Q, DGModuleMap))
assert(diff(Q, (Q.natural)_0) == 0_(Q.natural))
-- generatorTable, dgModuleSummary, displayModuleBlockDiff return a Net
assert(instance(generatorTable M, Net))
assert(instance(dgModuleSummary(Mdg, 3), Net))
-- moduleBlockDiff columns
Mdg3 = minimalSemifreeResolution(A, k, EndDegree => 3)
b2 = moduleBlockDiff(Mdg3, 2)
assert(matrix b2 == moduleDifferential(2, Mdg3))
srcIdx = indices source b2
assert(all(srcIdx, l -> #l == 2 and instance(l#0, ZZ) and instance(l#1, List)))
assert(instance(displayModuleBlockDiff(Mdg, 2), Net))
///

TEST ///
-- test 38: manipulation (baseChange / subDGAlgebra / restrictDifferential / truncateGenerators / killHomologyAtDegree)
R = QQ[x, y]
S = R / ideal(x^2, y^2)
A = koszulComplexDGA R
B = baseChange(A, S)
assert(underlyingRing B === S)
assert(isWellDefinedDifferential B)
-- RingMap form
phi = map(R, R, {x, 0})
Bphi = baseChange(A, phi)
assert(underlyingRing Bphi === R)
-- subDGAlgebra / restrictDifferential / truncateGenerators
Ac = koszulComplexDGA (QQ[x, y] / ideal(x^2, y^2))
Bsub = subDGAlgebra(Ac, {0})
assert(numgens Bsub.natural == 1)
Brd = restrictDifferential(Ac, {0})
assert(numgens Brd.natural == 1)
Btr = truncateGenerators(Ac, 0)
assert(numgens Btr.natural == numgens Ac.natural)
-- subDGAlgebra must receive a d-closed sub-list
Rx = QQ[x] / ideal(x^2)
Ax = koszulComplexDGA Rx
Ax' = adjoinVariables(Ax, {x * (gens Ax.natural)#0})
errored = try (subDGAlgebra(Ax', {1}); false) else true
assert(errored)
-- killHomologyAtDegree
Areg = koszulComplexDGA (QQ[x, y])
assert(killHomologyAtDegree(Areg, 1) === Areg)  -- no homology in deg 1
Bkill = killHomologyAtDegree(Ac, 1)
assert(numgens Bkill.natural > numgens Ac.natural)
assert(prune homology(1, Bkill) == 0)
-- isValidDGAlgebra / isWellDefinedDifferential
assert(isValidDGAlgebra Ac)
assert(isWellDefinedDifferential Ac)
Mfree = freeDGModule(Ac, {0})
assert(isWellDefinedDifferential Mfree)
///

TEST ///
-- test 39: accessors (underlyingRing, underlyingAlgebra, differential, generatorDegrees, caching)
R = QQ[x, y] / ideal(x^2, y^2)
A = koszulComplexDGA R
assert(underlyingRing A === R)
M = koszulComplexDGM R^1
assert(underlyingRing M === R)
assert(underlyingAlgebra A === A.natural)
assert(underlyingAlgebra M === M.natural)
assert(differential A === A.diff)
-- generatorDegrees returns whatever the DGA records
assert(generatorDegrees A == A.Degrees)
Mfree = freeDGModule(A, {0, 1, 3})
assert(generatorDegrees Mfree == Mfree.Degrees)
-- Caches
ensureDGAlgebraCaches A
assert(instance(A.cache, CacheTable))
ensureDGAlgebraCaches A
assert(instance(A.cache, CacheTable))
ensureDGAlgebraCaches M
assert(instance(M.cache, CacheTable))
-- invalidateDGAlgebraCache resets cache slots
C1 = dgComplex A
invalidateDGAlgebraCache A
assert(not A.cache#?(symbol dgComplex))
C2 = dgComplex A
assert(instance(C2, Complex))
D1 = dgComplex M
invalidateDGAlgebraCache M
assert(not M.cache#?(symbol dgComplex))
D2 = dgComplex M
assert(instance(D2, Complex))
///

TEST ///
-- test 40: extensive DGIdeal operations (+, *, ^, intersect, :, %, quotient, predicates)
R = ZZ/101[x, y]
A = koszulComplexDGA R
Anat = A.natural
I = dgIdeal(A, {x_Anat})
J = dgIdeal(A, {y_Anat})
assert isWellDefined I
assert isDGIdeal(A, I.natural)
-- +, *, ^, intersect, :
assert isWellDefined(I + J)
assert(I + J == J + I)
Zp = dgIdeal(A, {})
assert(Zp + I == I)
assert isWellDefined(I * J)
assert(I * J == J * I)
Uone = dgIdeal(A, {1_Anat})
assert(Uone * I == I)
I5 = dgIdeal(A, {x_Anat, y_Anat})
assert isWellDefined(I5^2)
assert isWellDefined(I5^3)
assert(I5^2 * I5 == I5^3)
I5zero = I5^0
assert(I5zero.natural == ideal 1_Anat)
assert isWellDefined(Zp^0)
assert isWellDefined(Zp^5)
assert isWellDefined intersect(I, J)
ZI = intersect(Zp, I)
assert(numgens ZI.natural == 0)
Ic = dgIdeal(A, {x_Anat * y_Anat})
Jc = dgIdeal(A, {y_Anat})
Kc = Ic : Jc
assert isWellDefined Kc
-- isSubset, ==, %
Ia = dgIdeal(A, {x_Anat})
Ja = dgIdeal(A, {x_Anat, y_Anat})
assert isSubset(Ia, Ja)
assert(Ia != Ja)
assert(Ia == Ia)
assert((x_Anat^2 % I) == 0)
assert((y_Anat^2 % I) == y_Anat^2)
-- Accessors
Im = dgIdeal(A, {x_Anat, x_Anat * y_Anat})
ignore = mingens Im
ignore = numgens Im
ignore = module Im
assert(numgens Im == numgens Im.natural)
assert(ring Im === R)
assert(ambient Im === A)
-- prune
Ipr = dgIdeal(A, {x_Anat, x_Anat * y_Anat, x_Anat^2})
Jpr = prune Ipr
assert isWellDefined Jpr
assert(Jpr.natural == Ipr.natural)
-- quotient A / I
Rq = ZZ/101[x]
Aq = koszulComplexDGA Rq
Tq = (Aq.natural)_0
Iq = dgIdeal(Aq, {Tq})
Bq = Aq / Iq
assert(class Bq === DGAlgebra)
assert isWellDefined Bq
-- predicates
Ih = dgIdeal(A, {x_Anat, y_Anat})
assert isHomogeneous Ih
assert not isZero Ih
Zh = dgIdeal(A, {})
assert isZero Zh
-- isDGIdeal positive/negative
assert isDGIdeal(A, ideal(x_Anat, y_Anat))
Tsym = Anat_0  -- T_{1,1}, with d(T) = x
assert not isDGIdeal(A, ideal(Tsym))
JT = dgIdeal(A, {Tsym})
assert isDGIdeal(A, JT.natural)
-- zero-seed isZero
Z' = dgIdeal(A, {0_Anat, 0_Anat})
assert isZero Z'
///

TEST ///
-- test 41: export layer (setDiff DGModule, maxDegree, getBasis, toComplex, toComplexMap)
R = QQ[x]
A = koszulComplexDGA R
M = freeDGModule(A, {0, 1})
natGens = apply(rank M.natural, i -> (M.natural)_i)
setDiff(M, {0, x^2 * natGens#0})
d1 = moduleDifferential(1, M)
d2 = moduleDifferential(2, M)
assert(d1 * d2 == 0)
-- maxDegree on DGModule matches its ambient DGAlgebra
Rci = QQ[x, y] / ideal(x^2, y^2)
KM = koszulComplexDGM Rci^1
assert(maxDegree KM == maxDegree KM.dgAlgebra)
-- maxDegree on DGQuotientModule matches ambient module
R3 = ZZ/101[x]
A3 = koszulComplexDGA R3
M3 = freeDGModule(A3, {0, 1})
zM3 = zeroDGModuleMap(M3, M3)
Q3 = cokernel zM3
assert(maxDegree Q3 == maxDegree M3)
-- getBasis on Koszul DG module
b = getBasis(1, KM)
assert(instance(b, Matrix))
assert(numcols b == 2)
-- toComplex on DGModule
C = toComplex KM
assert(instance(C, Complex))
assert(C.dd_1 == moduleDifferential(1, KM))
assert(C.dd_2 == moduleDifferential(2, KM))
-- toComplex on DGModule with degree cap
KMr = koszulComplexDGM (QQ[x, y, z])^1
Cr = toComplex(KMr, 2)
assert(instance(Cr, Complex))
assert(max Cr == 2)
-- toComplex on DGQuotientModule
CM = toComplex M3
CQ = toComplex Q3
assert(rank CM_0 == rank CQ_0)
assert(rank CM_1 == rank CQ_1)
-- toComplex on DGIdeal: errors (intended)
Aid = koszulComplexDGA (ZZ/101[x])
Tid = (gens Aid.natural)#0
Iid = dgIdeal(Aid, {Tid})
errored = try (toComplex Iid; false) else true
assert(errored)
-- toComplexMap on DGAlgebraMap
Rcu = QQ[u, v] / ideal(u^2, v^2)
Acu = koszulComplexDGA Rcu
phi = dgAlgebraMap(Acu, Acu, matrix {gens Acu.natural})
cm = toComplexMap phi
assert(instance(cm, ComplexMap))
-- toComplexMap on DGModuleMap identity at each degree
Rc2 = QQ[x, y] / ideal(x^2, y^2)
Mdg = minimalSemifreeResolution(koszulComplexDGA Rc2, Rc2^1 / ideal(x, y), EndDegree => 3)
idMdg = identityDGModuleMap Mdg
assert(all(0..3, n -> (
    cmn := toComplexMap(idMdg, n);
    source cmn == target cmn and cmn == id_(source cmn)
    )))
///

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
