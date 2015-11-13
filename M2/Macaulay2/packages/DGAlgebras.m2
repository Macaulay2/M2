-- -*- coding: utf-8 -*-
newPackage("DGAlgebras",
     Headline => "Data type for DG algebras",
     Version => "0.8",
     Date => "Sept 19, 2010",
     Authors => {
	  {Name => "Frank Moore",
	   HomePage => "http://www.math.cornell.edu/~frankmoore",
	   Email => "frankmoore@math.cornell.edu"}},
     DebuggingMode => false
     )

export {"DGAlgebra", "freeDGAlgebra", "setDiff", "natural", "cycles",
	"getBasis", "koszulComplexDGA", "acyclicClosure", "toComplex",
	"killCycles", "getGenerators", "adjoinVariables", "deviations", "zerothHomology",
        "homologyAlgebra", "torAlgebra", "maxDegree", "StartDegree", "EndDegree",
	"isHomologyAlgebraTrivial", "findTrivialMasseyOperation",
	"isGolod", "isGolodHomomorphism", "GenDegreeLimit", "RelDegreeLimit", "TMOLimit",
	"InitializeDegreeZeroHomology", "InitializeComplex", "isAcyclic", "getDegNModule"}

-- current bugs:

-- Still to document:
-- add some tutorials and nice examples

-- Questions for Mike:
-- is there a way to add options to a built-in method that does not have options?
--   See: toComplex, which I would rather be chainComplex
-- is there a way to present graded pieces of graded A-modules as modules over A_0

-- Other things to do before version 1
-- [user v1] Change toComplex to chainComplex as per conversation with Dan on the M2 Google group.
-- [functionality v1] Fix setDiff preparation preferences
-- [functionality v1] Allow custom naming of the variables in the DGA, using a list.
-- [functionality v1] Finish trivial Massey operations.  Test for strong Golod? (see Gulliksen-Levin for this?)
-- [functionality v1] Present a degree of the homology algebra as a module over H_0(A) *using the monomial basis* of HA as generators
--                    In fact, one should do the following:  Let R be a ring, A a f.g. graded R-algebra with A_0 = R, and M a f.g. graded A-module.
--                                                           Is it possible to compute a presentation of M_i as an R-module (graded, if R is)?
-- [functionality v1] DGAlgebraMap
-- [functionality v1] Lift semifree DGAs along quisms
-- [functionality v1] HH DGAlgebraMap
-- [functionality v1] isQuism DGAlgebraMap
-- [functionality v1] Resolvents (see pg 17 of the green book)
-- [functionality v1] torMap RingMap
-- [functionality v1] torDelta
-- [functionality v1] Golod/Levin/Avramov index? (see paper of Liana)
-- [functionality v1] Matric Massey products? (see Gulliksen-Levin)
-- [functionality v1] Computing Betti numbers using Massey products?

-- Other things to do before version 2
-- [functionality v2] Allow non-polynomial underlying algebras
--                    [--- In order to do this, the 'basis' command needs to be a bit more robust; things have changed in 1.4 but the package does not use the new basis yet. ---]
-- [functionality v2] A check that the algebra, together with the differential, is indeed a DG Algebra (up to a certain degree...)
-- [functionality v2] isSemiFree
-- [functionality v2] taylorResolutionDGA - Needs non-polynomial underlying algebra
-- [functionality v2] ekResolutionDGA - Needs non-polynomial underlying algebra
-- [functionality v2] Gorenstein pdim3 Pfaffians resolution (pg 15 of green book) -  needs non-polynomial underlying algebra
-- [functionality v2] DGIdeals
-- [functionality v2] DGModules
-- [functionality v2] DGModuleMap

-- Not sure if the below are possible
-- [functionality v?] incorporate divided powers somehow?
-- [functionality v?] torModules - is this possible? 
-- [functionality v?] Compute obstructions for the existence of a minimal DG Algebra resolution (see pg 31 of the green book) (req. torModules)

-----------------------------------------------------
-- Set DG algebra types and constructor functions. -- 
-----------------------------------------------------

-- protected symbols (I don't want to export these symbols, but I use them internally in the code at various places)
protect diffs
protect basisAlgebra

-- Defining the new type DGAlgebra
DGAlgebra = new Type of MutableHashTable
globalAssignment DGAlgebra
--DGAlgebraMap still in development
--DGAlgebraMap = new Type of RingMap
--globalAssignment DGAlgebraMap

-- Modify the standard output for a DGAlgebra
net DGAlgebra := A -> (
   local diffList;
   myOutput := {net "Ring => " | net A.ring};
   myOutput = myOutput | {net "Underlying algebra => " | net A.natural};
   if A.diff =!= {} then diffList = take(flatten entries matrix (A.diff),numgens A.natural);
   myOutput = myOutput | {net "Differential => " | net diffList};
   myOutput = myOutput | {net "isHomogeneous => " | net A.isHomogeneous};
   horizontalJoin flatten ("{", stack myOutput, "}")
)

freeDGAlgebra = method(TypicalValue => DGAlgebra)
freeDGAlgebra (Ring,List) := (R,degList) -> (
   -- Input:  A ring, a list of degrees of the variables, and a list that defines the differential
   -- Output:  A hash table of type DGAlgebra
   A := new MutableHashTable;
   T := getSymbol("T");
   A#(symbol ring) = R;
   varsList := toList (T_1..T_(#degList));
   A#(symbol diff) = {};
   if isHomogeneous R then (
      -- make sure the degree list has the right form.
      if #(first degList) != #(first degrees A.ring) + 1 then degList = apply(degList, i -> i | {0});
      A#(symbol natural) = (A.ring)[varsList, Degrees => degList, Join => false, SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
   )
   else (
      A#(symbol natural) = (A.ring)[varsList, Degrees => degList, SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
   );
   A#(symbol isHomogeneous) = false;
   A.natural.cache = new CacheTable;
   A.natural.cache#(symbol basisAlgebra) = (A.ring)[varsList, Join => false, MonomialOrder => GRevLex, Degrees => apply(degList, i -> {first i}), SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
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
   A.diff = map(A.natural,A.natural, substitute(matrix {diffList}, A.natural));
   A.isHomogeneous = isHomogeneous A.ring and checkIsHomogeneous(A);
   if opts.InitializeDegreeZeroHomology then (
      definingIdeal := ideal mingens (ideal A.ring + sub(ideal polyDifferential(1,A), ambient A.ring));
      if definingIdeal == ideal vars ambient A.ring then A#(symbol zerothHomology) = coefficientRing A.ring else A#(symbol zerothHomology) = (ambient A.ring)/definingIdeal;
   );
   if opts.InitializeComplex then A.dd = (toComplex(A,totalOddDegree(A)+1)).dd;
   A
)

checkIsHomogeneous = method()
checkIsHomogeneous DGAlgebra := A -> (
   gensList := gens A.natural;
   diffList := apply(gensList, f -> A.diff(f));
   homDegreeShift := {1} | (toList ((#(degree first gensList)-1):0));
   all(apply(#diffList, i -> degree gensList#i - homDegreeShift == degree diffList#i), i -> i)
)

getBasis = method(TypicalValue => Matrix, Options => {Limit => -1})
getBasis (ZZ,DGAlgebra) := opts -> (homDegree,A) -> getBasis(homDegree,A.natural, Limit => opts.Limit)

getBasis (ZZ,Ring) := opts -> (homDegree,R) -> (
   local retVal;
   myMap := map(R, R.cache.basisAlgebra);
   tempList := (flatten entries basis(homDegree, R.cache.basisAlgebra, Limit => opts.Limit)) / myMap;
   if tempList == {} then retVal = map((R)^1,(R)^0, 0) else
   (
      -- move this to an assert?
      -- tempList = reverse sort tempList;
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


koszulComplexDGA = method(TypicalValue => DGAlgebra)
koszulComplexDGA Ring := R -> (
   local A;
   if isHomogeneous R then (
      degList := apply(degrees R, i -> {1} | i);
      A = freeDGAlgebra(R, degList);
      use A.ring;
      setDiff(A, gens R);
   )
   else (
      A = freeDGAlgebra(R, toList ((numgens R):{1}));
      use A.ring;
      setDiff(A, gens R);
   );
   A
)

koszulComplexDGA Ideal := I -> (
   local A;
   if isHomogeneous I then (
      degList := apply(flatten entries gens I, i -> {1} | degree i); 
      A = freeDGAlgebra(ring I, degList);
      use A.ring;
      setDiff(A,I_*);
   )
   else
   (
      A = freeDGAlgebra(ring I, toList ((numgens I):{1}));
      use A.ring;
      setDiff(A, I_*);
   );
   A
)

koszulComplexDGA List := ringElts -> koszulComplexDGA(ideal ringElts);

toComplex = method(TypicalValue=>ChainComplex)
toComplex DGAlgebra := A -> (
   maxDeg := maxDegree A;
   if maxDeg == infinity then error "Must specify an upper degree bound if an even generator exists.";
   toComplex(A,maxDeg)
)

toComplex (DGAlgebra,ZZ) := (A,n) -> chainComplex(apply(n, i -> polyDifferential(i+1,A)))

killCycles = method(TypicalValue=>DGAlgebra,Options => {StartDegree => 1, EndDegree => -1})
killCycles DGAlgebra := opts -> A -> (
   -- for now, this will only work for DG algebras with H_0(A) = k
   retVal := 0;
   endDegree := 0;
   if opts.EndDegree == -1 then endDegree = opts.StartDegree;
   if opts.StartDegree > endDegree then error "Starting degree is not less than or equal to ending degree.";
   n := opts.StartDegree;
   foundHomology := false;
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
      retVal = adjoinVariables(A,cycleList);
   );
   retVal
)

adjoinVariables = method(TypicalValue=>DGAlgebra)
adjoinVariables (DGAlgebra, List) := (A,cycleList) -> (
   -- this function will add a new variable to make the elements of cycles boundaries in a new DG algebra (semifree over the input)
   local newDegreesList;
   tempDegree := {1} | toList ((#(degree first cycleList)-1):0);
   if A.isHomogeneous then
      newDegreesList = A.Degrees | apply(cycleList, z -> degree z + tempDegree)
   else
      newDegreesList = A.Degrees | apply(cycleList, z -> {first degree z + 1});
   B := freeDGAlgebra(A.ring,newDegreesList);
   newDiffList := apply(take(flatten entries matrix A.diff, numgens A.natural) | cycleList, f -> substitute(f, B.natural));
   setDiff(B,newDiffList);
   B
)

acyclicClosure = method(TypicalValue=>DGAlgebra,Options => {StartDegree => 1, EndDegree => -1})
acyclicClosure DGAlgebra := opts -> A -> (
  n := opts.StartDegree;
  endDegree := 3;
  if opts.EndDegree != -1 then endDegree = opts.EndDegree;
  while n <= endDegree do (
     A = killCycles(A,StartDegree => n);
     n = n + 1;
  );
  A
)

acyclicClosure Ring := opts -> R -> (
   K := koszulComplexDGA(R);
   acyclicClosure(K, opts)
)

polyDiffMonomial := (A,m) -> (
  -- uses the Leibniz rule to compute the differential of a traditional monomial
  dgSign := 1;
  justMon := first flatten entries first coefficients m;
  justCoeff := substitute(first flatten entries last coefficients m, ring m);
  monSupport := support justMon;
  monExponents := select(first exponents justMon, i -> i > 0);
  monSupportPowers := apply(#monSupport, i -> (monSupport#i)^(monExponents#i));
  allTerms := apply(#monSupport, i -> (product apply(i, j -> (-1)^((first degree monSupport#j)*(monExponents#j))))*
                                      (product take(monSupportPowers,i))*
  -- below are both versions of differential, for divided powers and without, should we want to use divided powers later.
  --                                  (A.diff(monSupport#i)*(monSupport#i)^((monExponents#i)-1))*
                                      (A.diff(monSupport#i)*(monExponents#i)*(monSupport#i)^((monExponents#i)-1))*
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
     else if n > mDegree + 1 then newDiffl = map((A.ring)^0,(A.ring)^0,0) else (
        diffList := matrix {apply(sourceList, m -> polyDiffMonomial(A,m))};
        coeffMatrix := substitute((coefficients(diffList, Monomials => targetList))#1, A.ring);
        newDiffl = map((A.ring)^(targetDegreeList), (A.ring)^(sourceDegreeList), coeffMatrix);
        A.cache.diffs#n = newDiffl;
     );
     newDiffl
  )
)

polyDifferential (DGAlgebra,RingElement) := (A,f) -> sum apply(terms f, m -> polyDiffMonomial(A,m))

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
   myColDegree := apply(colDegList_myCols, h -> -drop(h,1));
   tempMatrix = tempMatrix_myCols;
   myRowDegree = apply(rowDegList, h -> -drop(h,1));
   tempMatrix = map(R^myRowDegree, R^myColDegree,substitute(tempMatrix,R));
   prune coker tempMatrix
)

deviations = method(TypicalValue=>Tally,Options=>{DegreeLimit=>3})
deviations Ring := opts -> R -> tally degrees torAlgebra(R,GenDegreeLimit=>opts.DegreeLimit)

torAlgebra = method(TypicalValue=>Ring, Options=>{GenDegreeLimit=>infinity,RelDegreeLimit=>infinity})
torAlgebra Ring := opts -> R -> (
  -- since we are not yet implementing the Hopf structure, only the algebra structure, we need not
  -- actually use DGAlgebras to compute the Tor algebra.  We use the built in resolution function
  -- for the resolution of R/(ideal vars R) below since it is much faster.
  n := 3;
  if opts.GenDegreeLimit != infinity then n = opts.GenDegreeLimit;
  baseRing := coefficientRing R;
  kRes := res(coker vars R, LengthLimit => n);
  bettiNums := apply((length kRes)+1, i -> degrees source kRes.dd_i);
  local torSoFar;
  local cacheTorSoFar;
  local degreeList;
  local skewList;
  local numNewVars;
  local dimInCurDegree;
  local newDegreeList;
  X := getSymbol("X");
  if length kRes == 0 then baseRing else (
     currentDegree := 1;
     newVars := toList (X_1..X_(#(bettiNums#currentDegree)));
     if isHomogeneous R then degreeList = apply(bettiNums#currentDegree, i -> {currentDegree} | i) else degreeList = toList (#(bettiNums#currentDegree):{1});
     skewList = toList (0..#(bettiNums#currentDegree)-1);
     -- need to also define a cached version of the ring with only the homological grading in the homogeneous case
     torSoFar = baseRing[newVars,Degrees=>degreeList, SkewCommutative=>skewList];
     if not isHomogeneous R then cacheTorSoFar = torSoFar else cacheTorSoFar = baseRing[newVars, Degrees => apply(degreeList, i -> {first i}), SkewCommutative => skewList];
     torSoFar.cache = new CacheTable;
     torSoFar.cache#(symbol basisAlgebra) = cacheTorSoFar;
     currentDegree = currentDegree + 1;
     while currentDegree <= n do (
        -- this is the command that must change.  I think just doing a setminus from the basis list in the resln minus
	-- the basis list of the algebra should do the trick.
	if isHomogeneous R then (
           -- below we use a Tally object to find the new basis degrees we need to add in the homogeneous case
	   torSoFarTally := tally degrees source getBasis(currentDegree,torSoFar);
	   allDegreesTally := tally apply(bettiNums#currentDegree, i -> flatten {currentDegree,i});
	   newDegreeList = flatten apply(pairs (allDegreesTally - torSoFarTally), p -> toList (p#1:p#0));
	   numNewVars = #newDegreeList;  
	)
        else (
	   dimInCurDegree = hilbertFunction(currentDegree,torSoFar);
           numNewVars = #bettiNums#currentDegree - dimInCurDegree;
	   newDegreeList = toList (numNewVars:currentDegree);
	);
	-- the below check will only fail if R is a complete intersection, and currentDegree = 3 (or earlier, if R is regular)
	-- The numNewVars are the deviations of the ring R; these vanish rigidly by a theorem of Halperin.
	-- They are returned with the deviations command
	if numNewVars != 0 then (	 
           newVars = newVars | toList (X_((numgens torSoFar)+1)..X_((numgens torSoFar) + numNewVars));
           degreeList = degreeList | newDegreeList;
           if odd currentDegree then skewList = skewList | toList ((numgens torSoFar)..((numgens torSoFar) + numNewVars - 1));
           torSoFar = baseRing[newVars,Degrees=>degreeList, SkewCommutative=>skewList];
           if not isHomogeneous R then cacheTorSoFar = torSoFar else cacheTorSoFar = baseRing[newVars, Degrees => apply(degreeList, i -> {first i}), SkewCommutative => skewList];
	   torSoFar.cache = new CacheTable;
	   torSoFar.cache#(symbol basisAlgebra) = cacheTorSoFar;
           currentDegree = currentDegree + 1;
	)
        else currentDegree = n+1;
     );
     torSoFar
  )
)

torAlgebra (Ring,Ring) := opts -> (R,S) -> homologyAlgebra(acyclicClosure(R,EndDegree=>opts.GenDegreeLimit) ** S, opts)

representativeCycles := (n,A) -> (
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
  B := baseRing[varsList,MonomialOrder=>{numgens A.natural + numgens A.ring,#cycleList},Degrees=>degList, SkewCommutative=>skewList];
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

getGenerators = method(TypicalValue=>List, Options => {StartDegree => 1, DegreeLimit => -1})
getGenerators DGAlgebra := opts -> A -> (
  maxDeg := maxDegree A;
  if opts.DegreeLimit != -1 then maxDeg = opts.DegreeLimit;
  if maxDeg == infinity then error "Must specify maximum homological degree of generators.";
  n := opts.StartDegree;
  cycleList := {};
  while n <= maxDeg do (
     << "Computing generators in degree " << n << " : ";
     time newCycleList := findDegNGenerators(A,cycleList,n);
     cycleList = cycleList | newCycleList;
     n = n + 1;
  );
  cycleList
)

getRelations = method()
getRelations (DGAlgebra,Ring,List,ZZ) := (A,HA,cycleList,relDegreeLimit) -> (
   relList := (ideal HA)_*;
   n := 1;
   while n <= relDegreeLimit do (
      << "Computing relations in degree " << n << "  : ";
      time newRelList := findDegNRelations(A,HA,cycleList,n);
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

homologyAlgebra = method(TypicalValue=>Ring,Options=>{GenDegreeLimit=>infinity,RelDegreeLimit=>infinity})
homologyAlgebra DGAlgebra := opts -> A -> (
  local HA;
  if A.cache.homologyAlgebra#?GenDegreeLimit and A.cache.homologyAlgebra#GenDegreeLimit >= opts.GenDegreeLimit and
     A.cache.homologyAlgebra#?RelDegreeLimit and A.cache.homologyAlgebra#RelDegreeLimit >= opts.RelDegreeLimit then HA = A.cache.homologyAlgebra#homologyAlgebra else (
     maxDeg := maxDegree A;
  
     if maxDeg == infinity and (opts.GenDegreeLimit == infinity or opts.RelDegreeLimit == infinity) then
        error "Must supply upper degree bound on generators and relations if there is a DG algebra generator of even degree.";
     if opts.GenDegreeLimit != infinity then maxDeg = opts.GenDegreeLimit;
  
     n := maxDeg;
     while n <= maxDeg and prune homology(n,A) == 0 do n = n - 1;
     maxHomologyDegree := n + 1;
     if opts.RelDegreeLimit != infinity then maxHomologyDegree = opts.RelDegreeLimit;

     cycleList := getGenerators(A,DegreeLimit=>maxDeg);

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
        HA = getRelations(A,HA,cycleList,maxHomologyDegree);
        A.cache.homologyAlgebra#homologyAlgebra = HA;
	A.cache.homologyAlgebra#GenDegreeLimit = opts.GenDegreeLimit;
	A.cache.homologyAlgebra#RelDegreeLimit = opts.RelDegreeLimit;
     );
  );
  HA     
)

isHomologyAlgebraTrivial = method(TypicalValue=>Boolean,Options=>{GenDegreeLimit=>infinity})
isHomologyAlgebraTrivial DGAlgebra := opts -> A -> findTrivialMasseyOperation(A,opts) =!= null

isGolod = method(TypicalValue=>Boolean)
isGolod Ring := R -> isHomologyAlgebraTrivial(koszulComplexDGA(R))

isGolodHomomorphism = method(TypicalValue=>Boolean,Options=>{GenDegreeLimit=>infinity})
isGolodHomomorphism QuotientRing := opts -> R -> isHomologyAlgebraTrivial(acyclicClosure(ambient R, EndDegree=>opts.GenDegreeLimit) ** R, opts)

DGAlgebra ** Ring := (A,S) -> (
  B := freeDGAlgebra(S, A.Degrees);
  newDiff := apply(flatten entries matrix (A.diff), f -> substitute(f,B.natural));
  setDiff(B,newDiff);
  B
)

DGAlgebra ** DGAlgebra := (A,B) -> (
  if A.ring =!= B.ring then error "DGAlgebras must be defined over the same ring.";
  -- should I use a block ordering here since it is a tensor product?
  C := freeDGAlgebra(A.ring, A.Degrees | B.Degrees);
  newDiff := apply(take(flatten entries matrix (A.diff),numgens A.natural), f -> substitute(f,C.natural));
  newDiff = newDiff | apply(flatten entries matrix (B.diff), f -> substitute(f,C.natural));
  setDiff(C,newDiff);
  C
)

getBoundaryPreimage = method()
getBoundaryPreimage (DGAlgebra,List,ZZ) := (A,boundaryList,homDegree) -> (
   dnplus1 := polyDifferential(homDegree+1,A);
   Anbasis := flatten entries getBasis(homDegree,A);
   if Anbasis == {} then matrix {{0_(A.ring)}} else (
      boundaryVec := (coefficients(matrix{boundaryList}, Monomials => Anbasis))#1;
      degreeList := apply(degrees target boundaryVec, l -> -drop(l,1));
      boundaryVec = map((A.ring)^degreeList,(A.ring)^(rank source boundaryVec), sub(boundaryVec,A.ring));
      retVal := boundaryVec // dnplus1;
      -- if not all elements of the list are boundaries, then return null
      if (dnplus1 * retVal != boundaryVec) then retVal = null else retVal
   )
)

getBoundaryPreimage (DGAlgebra,RingElement) := (A,b) -> getBoundaryPreimage(A,{b}, first degree b)

findTrivialMasseyOperation = method(TypicalValue=>List, Options=>{GenDegreeLimit=>infinity,TMOLimit=>2})
findTrivialMasseyOperation DGAlgebra := opts -> A -> (
   maxDeg := maxDegree A;
   if maxDeg == infinity and opts.GenDegreeLimit == infinity then error "Must specify an upper bound on the generating degree";
   if opts.GenDegreeLimit != infinity then maxDeg = opts.GenDegreeLimit; 
   cycleList := getGenerators(A,DegreeLimit=>maxDeg);
   --- just do 2-fold TMOs for now
   prodList := apply(subsets(cycleList,2), l -> (first degree l#0 + first degree l#1,l#0*l#1));
   n := min (prodList / first);
   maxDegree := max (prodList / first);
   retVal := {};
   while n <= maxDegree do (
      boundaryList := select(prodList, z -> z#0 == n) / last;
      if boundaryList != {} then (
         tempVar := getBoundaryPreimage(A,boundaryList,n);
	 if (tempVar === null) then (
	    -- if we are in here, then no trivial Massey operation exists
            retVal = null;
	    n = maxDegree;
	 )
	 else retVal = retVal | {tempVar};
      )
      else retVal = retVal | {matrix{{0_(A.ring)}}};
      n = n + 1;
   );
   retVal
)

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
  Subnodes
    "Operations on DG Algebras"
    "The Koszul complex as a DG Algebra"
    "Acyclic closures"
    "Homology algebra of a DG Algebra"
///

doc ///
  Key
    "Operations on DG Algebras"
  Headline
    Outlines some basic operations on DG Algebras
  Description
    Text
      Text here.
///

doc ///
  Key
    "The Koszul complex as a DG Algebra"
  Headline
    an example
  Description
    Text
      Text here.
///

doc ///
  Key
    "Acyclic closures"
  Headline
    an example
  Description
    Text
      Text here.
///

doc ///
  Key
    "Homology algebra of a DG Algebra"
  Headline
    an example
  Description
    Text
      Text here.
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
    "Operations on DG Algebras"
///

doc ///
  Key
    freeDGAlgebra
  Headline
    Constructs a free (skew commutative) DGAlgebra
  Usage
    A = freeDGAlgebra(R,degreeList) 
///

doc ///
  Key
    (freeDGAlgebra,Ring,List)
  Headline
    Constructs a DGAlgebra
  Usage
    A = freeDGAlgebra(R,degreeList) 
  Inputs
    R:Ring 
      The ring over which the DGAlgebra is defined
    degreeList:List 
      A list of degrees of the algebra generators of R.
  Outputs
    A:DGAlgebra
  Description
    Text
      This function returns a @ TO DGAlgebra @ A whose underlying algebra is a graded commutative
      polynomial ring in a number of variables equal to the number of the degrees input.  The current version of this package
      does not handle algebras A whose underlying algebra is not a polynomial ring.
    Example
      R = ZZ/101[x,y,z]
      A = freeDGAlgebra(R,{{1},{1},{1},{3}})
      A.natural
      setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
    Text
      The resulting @ TO DGAlgebra @ will not be graded since the differential given does not respect the grading due to the degrees assigned in the definition.
    Example
      isHomogeneous(A)
      Add = toComplex A
      B = freeDGAlgebra(R,{{1,1},{1,1},{1,1},{3,3}})
      B.natural
      setDiff(B,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
    Text
      The result of the above declaration will be graded.
    Example
      isHomogeneous(B)
      Bdd = toComplex B
    Text  
      Note that the differential is not passed into the constructor.  The reason for this (at the moment)
      is that Macaulay2 does not know what ring the differentials are defined over until after the underlying
      algebra is constructed, so the differential is set later with setDiff.  Many DG algebras that one
      encounters in commutative algebra have been implemented, however, and do not need to be defined 'by hand'.
      For example, if one wants to work with the Koszul complex as a DG algebra, then one should see the command @ TO koszulComplexDGA @.
  Caveat
    There is currently a bug handling DG algebras that have no monomials in some degree, but some monomials in a later degree;
    for example if one replaces the 3 in the above example with a 5.
///

doc ///
  Key
    koszulComplexDGA
    (koszulComplexDGA,Ring)
  Headline
    Returns the Koszul complex as a DGAlgebra
  Usage
    A = koszulComplexDGA(R)
  Inputs
    R:Ring 
      Returns the Koszul complex on ideal vars R.
  Outputs
    A:DGAlgebra
  Description
    Text
      To construct the Koszul complex of a minimal set of generators as a @ TO DGAlgebra @ one uses
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      A = koszulComplexDGA(R)
      complexA = toComplex A
      complexA.dd
      ranks = apply(4, i -> numgens prune HH_i(complexA))
      ranks == apply(4, i -> numgens prune HH_i(koszul vars R))
    Text
      One can also compute the homology of A directly with @ TO (homology,ZZ,DGAlgebra) @.
///

doc ///
  Key
    (koszulComplexDGA,Ideal)
  Headline
    Returns the Koszul complex as a DGAlgebra
  Usage
    A = koszulComplexDGA(I)
  Inputs
    I:Ideal 
      An ideal of a ring R
  Outputs
    A:DGAlgebra
  Description
    Text
      To construct the Koszul complex on the set of generators of I as a @ TO DGAlgebra @ one uses
    Example
      R = ZZ/101[a,b,c]
      I = ideal{a^3,b^3,c^3,a^2*b^2*c^2}
      A = koszulComplexDGA(I)
      complexA = toComplex A
      complexA.dd
      ranks = apply(4, i -> numgens prune HH_i(complexA))
      ranks == apply(4, i -> numgens prune HH_i(koszul gens I))
    Text
      One can also compute the homology of A directly with @ TO (homology,ZZ,DGAlgebra) @.
///

doc ///
  Key
    (koszulComplexDGA,List)
  Headline
    Define the Koszul complex on a list of elements as a DGAlgebra
  Usage
    A = koszulComplexDGA(diffList)
  Inputs
    diffList:List
      A List of RingElements.  The resulting DGAlgebra will be defined over the ring of these elements.
  Outputs
    A:DGAlgebra
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
    Sets the differential of a DGAlgebra manually.
  Usage
    d = setDiff(A,diffList)
  Inputs
    A:DGAlgebra
    A:List 
  Outputs
    A:DGAlgebra
      The DGAlgebra with the differential now set.
  Description
    Example
      R = ZZ/101[x,y,z]
      A = freeDGAlgebra(R,{{1},{1},{1},{3}})
      setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
      Add = toComplex A
      Add.dd
    Text
      There are two options that are available for this function, and both are designed to bypass certain initializations
      that take place by default.
    Text
      The option InitializeComplex specifies whether or not to compute all differentials of
      the complex(up to the sum of the degrees of the odd degree generators) before returning from setDiff.  This is useful if
      your DGAlgebra has a large number of generators in odd degrees, and you are only interested in computing the homology
      in low degrees.  The default value of this option is true.
    Text
      The option InitializeDegreeZeroHomology specifies whether or not to define the quotient ring H_0(A).  This is used when
      computing HH(A) as a DGAlgebra.  This involves computing a Grobner basis of the image of the first differential of A,
      and as such, may want to be avoided if there are a large number of DGAlgebra generators in degree 1.  The default value of
      this options is true.
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
      setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
      isHomogeneous A
      B = freeDGAlgebra(R,{{1,1},{1,1},{1,1},{3,3}})
      setDiff(B,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
      isHomogeneous B
///

doc ///
  Key
    natural
  Headline
    The underlying algebra of a DGAlgebra.
  Usage
    Anat = A.natural
  Description
    Example
      R = ZZ/101[a,b,c,d]
      A = koszulComplexDGA(R)
      A.natural
///

doc ///
  Key
    cycles
  Headline
    Cycles chosen when computing the homology algebra of a DGAlgebra
  Usage
    A.cycles
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^4,c^5,d^6}
      A = koszulComplexDGA(R)
      apply(maxDegree A + 1, i -> numgens prune homology(i,A))
      HA = homologyAlgebra(A)
      numgens HA
      HA.cache.cycles
    Text
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
    Converts a DGAlgebra to a ChainComplex
  Usage
    C = toComplex A or C = toComplex(A,n)
  Inputs
    A:DGAlgebra
  Outputs
    C:ChainComplex
      The DG algebra A as a ChainComplex
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
    Converts a DGAlgebra to a ChainComplex
  Usage
    C = toComplex A or C = toComplex(A,n)
  Inputs
    A:DGAlgebra
    n:ZZ
  Outputs
    C:ChainComplex
      The DG algebra A as a ChainComplex
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
    Compute theae acyclic closure of a DGAlgebra.
  Usage
    B = acyclicClosure(A)
  Inputs
    A:DGAlgebra
  Outputs
    B:DGAlgebra
      The acyclic closure of the DG Algebra A up to homological degree provided in the EndDegree option (default value is 3).
  Description
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      A = koszulComplexDGA(R);
      B = acyclicClosure(A,EndDegree=>3)
      toComplex(B,8)
      B.diff
  SeeAlso
    (acyclicClosure,Ring)
///

doc ///
  Key
    (acyclicClosure,Ring)
  Headline
    Compute the acyclic closure of the residue field of a ring up to a certain degree
  Usage
    A = acyclicClosure(R)
  Inputs
    R:Ring
  Outputs
    A:DGAlgebra
      The acyclic closure of the ring R up to homological degree provided in the EndDegree option (default value is 3).
  Description
    Text
      This package always chooses the Koszul complex on a generating set for the maximal ideal as a starting
      point, and then computes from there, using the function @ TO (acyclicClosure,DGAlgebra) @.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^4-d^3}
      A = acyclicClosure(R,EndDegree=>3)
      A.diff
///

doc ///
  Key
    (symbol **, DGAlgebra, Ring)
  Headline
    Tensor product of a DGAlgebra and another ring.
  Usage
    B = A ** S
  Inputs
    A:DGAlgebra
    R:Ring
  Outputs
    B:DGAlgebra
  Description
    Text
      Tensor product of a DGAlgebra and another ring (typically a quotient of A.ring).
    Example
      R = ZZ/101[a,b,c,d]
      A = koszulComplexDGA(R)
      S = R/ideal{a^3,a*b*c}
      B = A ** S
      Bdd = toComplex B
      Bdd.dd
///

doc ///
  Key
    (symbol **, DGAlgebra, DGAlgebra)
  Headline
    Tensor product of a DGAlgebra and another ring.
  Usage
    C = A ** B
  Inputs
    A:DGAlgebra
    B:DGAlgebra
  Outputs
    C:DGAlgebra
  Description
    Text
      Tensor product of a pair of DGAlgebras.
    Example
      R = ZZ/101[a,b,c,d]
      A = koszulComplexDGA({a,b})
      B = koszulComplexDGA({c,d})
      C = A ** B
      Cdd = toComplex C
      Cdd.dd
  Caveat
    Currently, the tensor product function does not create a block order on the variables from A and B.
///

doc ///
  Key
    killCycles
    (killCycles,DGAlgebra)
  Headline
    Adjoins variables to make non-bounding cycles boundaries in the lowest positive degree with nontrivial homology.
  Usage
    B = killCycles(A)
  Inputs
    A:DGAlgebra
  Outputs
    B:DGAlgebra
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3-d^4}
      A = koszulComplexDGA(R)
      A.diff
      B = killCycles(A)
      B.diff
///

doc ///
  Key
    adjoinVariables
    (adjoinVariables,DGAlgebra,List)
  Headline
    Adjoins variables to make the specified cycles boundaries.
  Usage
    B = adjoinVariables(A,cycleList)
  Inputs
    A:DGAlgebra
    cycleList:List
  Outputs
    B:DGAlgebra
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3-d^4}
      A = koszulComplexDGA(R)
      A.diff
      prune homology(1,A)
      B = adjoinVariables(A,{a^2*T_1})
      B.diff
      prune homology(1,B)
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
    Compute the homology algebra of a DGAlgebra.
  Usage
    HA = homology(A)
  Inputs
    A:DGAlgebra
  Outputs
    HA:Ring
  SeeAlso
    (homologyAlgebra,DGAlgebra)
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
      TorS = torAlgebra(S)
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
      Mres = res(M, LengthLimit=>8)
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
    Determines if a DGAlgebra is acyclic.
  Usage
    isAcyc = isAcyclic(A)
  Inputs
    A:DGAlgebra
  Outputs
    isAcyc:Boolean
  Description
    Text
      This function determines if the DGAlgebra is acyclic.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4+b^4+c^4+d^4}
      isAcyclic(koszulComplexDGA R)
    Example
      Q = ZZ/101[a,b,c,d]
      I = ideal {a^4,b^4,c^4,d^4}
      isAcyclic(koszulComplexDGA I)
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
      The above is a (CM) ring minimal of minimal multiplicity, hence Golod.
  Caveat
    Currently, it does not try to find a full trivial Massey operation for the ring R, it just computes them to second order.
    Since there is not currently an example of a ring that is not Golod yet has trivial product on $H(K^R)$, this is ok for now.
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
      isGolodHomomorphism(R,GenDegreeLimit=>5)
    Text
      The map from Q to R is Golod by a result of Avramov and Levin.
  Caveat
    Currently, it does not try to find a full trivial Massey operation on acyclicClosure(Q) ** R, it just computes them to second order.
    Since there is not currently an example of a ring (or a homomorphism) that is not Golod yet has trivial product on its homotopy fiber,
    this is ok for now.
///

doc ///
  Key
    getGenerators
    (getGenerators,DGAlgebra)
  Headline
    Returns a list of cycles whose images generate HH(A) as an algebra
  Usage
    cycleList = getGenerators(A)
  Inputs
    A:DGAlgebra
  Outputs
    cycleList:List
  Description
    Text
      This version of the function should only be used if all algebra generators of A are in odd homological degree,
      provided in the EndDegree option.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3,a^2*b^2*c^2}
      A = koszulComplexDGA(R)
      netList getGenerators(A)
///

doc ///
  Key
    deviations
    (deviations,Ring)
  Headline
    Computes the deviations of the input ring
  Usage
    devTally = deviations(R)
  Inputs
    R:Ring
  Outputs
    devTally:Tally
  Description
    Text
      This command computes the deviations of the ring R.  The deviations are the same as the degrees of the generators of
      the acyclic closure of R, or the degrees of the generators of the Tor algebra of R.  This function takes an option
      called Limit (default value 3) that specifies the largest deviation to compute.
    Example
      R = ZZ/101[a,b,c,d]/ideal {a^3,b^3,c^3,d^3}
      deviations(R)
      deviations(R,DegreeLimit=>4)
      S = R/ideal{a^2*b^2*c^2*d^2}
      deviations(S,DegreeLimit=>4)
      T = ZZ/101[a,b]/ideal {a^2-b^3}
      deviations(T,DegreeLimit=>4)
    Text
      Note that the deviations of T are not graded, since T is not graded.
///

doc ///
  Key
    findTrivialMasseyOperation
    (findTrivialMasseyOperation,DGAlgebra)
  Headline
    Finds a trivial Massey operation on a set of generators of H(A)
  Usage
    tmo = findTrivialMasseyOperation(A)
  Inputs
    A:DGAlgebra
  Outputs
    tmo:List
      List of matrices whose columns span the image of the multiplication map, one for each homological degree.
  Description
    Text
      This function currently just finds the elements whose boundary give the product of every pair of cycles
      that are chosen as generators.  Eventually, all higher Massey operations will also be computed.  The maximum
      degree of a generating cycle is specified in the option GenDegreeLimit, if needed.
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
      tmo = findTrivialMasseyOperation(A)
      assert(tmo =!= null)
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
      assert(findTrivialMasseyOperation(A) === null)
///

doc ///
  Key
    zerothHomology
    (zerothHomology,DGAlgebra)
  Headline
    Compute the zeroth homology of the DGAlgebra A as a ring.
  Usage
    HA0 = zerothHomology A
  Inputs
    A:DGAlgebra
  Outputs
    HA0:Ring
      HH_0(A) as a ring.
  Description
    Text
      This is a stub.
///

doc ///
  Key
    getDegNModule
    (getDegNModule,ZZ,Ring,Ring)
  Headline
    Compute a presentation of M_i as an R-module
  Usage
    M' = getDegNModule(N,R,M)
    M' = getDegNModule(N,R,A)
  Inputs
    N:ZZ
    R:Ring
    M:Module
      M is a Module over a ring A, and A must be a graded R-algebra with A_0 = R
    A:Ring
      A must be a graded R-algebra, and A_0 = R
  Outputs
    M':Module
      M_N as an R = A_0-module
  Description
    Text
      This is a stub.
///

doc ///
  Key
    StartDegree
  Headline
    Option to specify the degree to start computing the acyclic closure and killing cycles
  Usage
    acyclicClosure(...,StartDegree=>n) or killCycles(...,StartDegree=>n)
///

doc ///
  Key
    EndDegree
  Headline
    Option to specify the degree to stop computing killing cycles and acyclic closure 
  Usage
    killCycles(...,StartDegree=>n)
///

doc ///
  Key
    GenDegreeLimit
  Headline
    Option to specify the maximum degree to look for generators
  Usage
    homologyAlgebra(...,GenDegreeLimit=>n)
///

doc ///
  Key
    RelDegreeLimit
  Headline
    Option to specify the maximum degree to look for relations
  Usage
    homologyAlgebra(...,RelDegreeLimit=>n)
///

doc ///
  Key
    TMOLimit
  Headline
    Option to specify the maximum arity of the trivial Massey operation
  Usage
    findTrivialMasseyOperation(...,TMOLimit=>n)
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
    Option to specify the degree to start computing the acyclic closure
  Usage
    acyclicClosure(...,StartDegree=>n)
///

doc ///
  Key
    [acyclicClosure,EndDegree]
  Headline
    Option to specify the degree to stop computing the acyclic closure
  Usage
    acyclicClosure(...,EndDegree=>n)
///

doc ///
  Key
    [getGenerators,StartDegree]
  Headline
    Option to specify the degree to start finding generators of HH(DGAlgebra)
  Usage
    getGenerators(...,StartDegree=>n)
///

doc ///
  Key
    [getGenerators,DegreeLimit]
  Headline
    Option to specify the degree to stop finding generators of HH(DGAlgebra)
  Usage
    getGenerators(...,DegreeLimit=>n)
///

doc ///
  Key
    [killCycles,StartDegree]
  Headline
    Option to specify the degree to start looking for cycles
  Usage
    killCycles(...,StartDegree=>n)
///

doc ///
  Key
    [killCycles,EndDegree]
  Headline
    Option to specify the degree to stop looking for cycles
  Usage
    killCycles(...,EndDegree=>n)
///

doc ///
  Key
    [getBasis,Limit]
  Headline
    Option to specify the maximum number of basis elements to return
  Usage
    getBasis(...,Limit=>n)
///

doc ///
  Key
    [homologyAlgebra,GenDegreeLimit]
  Headline
    Option to specify the maximum degree to look for generators
  Usage
    homologyAlgebra(...,GenDegreeLimit=>n)
///

doc ///
  Key
    [homologyAlgebra,RelDegreeLimit]
  Headline
    Option to specify the maximum degree to look for relations
  Usage
    homologyAlgebra(...,RelDegreeLimit=>n)
///

doc ///
  Key
    [torAlgebra,GenDegreeLimit]
  Headline
    Option to specify the maximum degree to look for generators
  Usage
    torAlgebra(...,GenDegreeLimit=>n)
///

doc ///
  Key
    [torAlgebra,RelDegreeLimit]
  Headline
    Option to specify the maximum degree to look for relations
  Usage
    torAlgebra(...,RelDegreeLimit=>n)
///

doc ///
  Key
    [findTrivialMasseyOperation,GenDegreeLimit]
  Headline
    Option to specify the maximum degree to look for generators
  Usage
    findTrivialMasseyOperation(...,GenDegreeLimit=>n)
///

doc ///
  Key
    [findTrivialMasseyOperation,TMOLimit]
  Headline
    Option to specify the maximum arity of a trivial Massey operation, if one exists.
  Usage
    findTrivialMasseyOperation(...,TMOLimit=>n)
///

doc ///
  Key
    [isHomologyAlgebraTrivial,GenDegreeLimit]
  Headline
    Option to specify the maximum degree to look for generators
  Usage
    isHomologyAlgebraTrivial(...,GenDegreeLimit=>n)
///

doc ///
  Key
    [isGolodHomomorphism,GenDegreeLimit]
  Headline
    Option to specify the maximum degree to look for generators
  Usage
    isGolodHomomorphism(...,GenDegreeLimit=>n)
///

doc ///
  Key
    [deviations,DegreeLimit]
  Headline
    Option to specify the maximum degree to look for generators when computing the deviations
  Usage
    deviations(...,DegreeLimit=>n)
///

doc ///
  Key
    (net,DGAlgebra)
  Headline
    Outputs the pertinent information about a DGAlgebra
  Usage
    net A
  Inputs
    A:DGAlgebra
///

-------------------------------
--          Testing          --
-------------------------------

TEST ///
-- test 0 : isHomogeneous, toComplex, maxDegree
R = ZZ/101[x,y,z]
A1 = freeDGAlgebra(R,{{1},{1},{1},{3}})
setDiff(A1,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
assert(not A1.isHomogeneous)
A1dd = toComplex(A1)
A1dd.dd

A2 = freeDGAlgebra(R,{{1,1},{1,1},{1,1},{3,3}})
setDiff(A2,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
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
setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
Add = toComplex(A)
assert(apply(maxDegree(A)+1, i -> prune HH_i(Add)) == {coker vars R,0,0,coker vars R,0,0,0})
///

TEST ///
-- test 1 : differential tests
R = ZZ/101[x,y,z, Degrees => {2,2,3}]
kRes = res coker vars R
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
Mres = res(M, LengthLimit => 7)
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
assert(isGolodHomomorphism(S,GenDegreeLimit=>6))
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
assert(findTrivialMasseyOperation(A) =!= null)

-- this is a Teter ring, and the computation in Avramov and Levin's paper shows
-- H(A) does not have trivial multiplication.
Q = ZZ/101[x,y,z]
I = ideal (x^3,y^3,z^3,x^2*y^2*z^2)
R = Q/I
A = koszulComplexDGA(R)
assert(not isHomologyAlgebraTrivial(A,GenDegreeLimit=>3))
cycleList = getGenerators(A)
prodList = apply(subsets(cycleList,2), l -> (first degree l#0 + first degree l#1,l#0*l#1));
assert(findTrivialMasseyOperation(A) === null)
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

end

-- How to install the package
uninstallPackage "DGAlgebras"
restart
installPackage "DGAlgebras"
viewHelp DGAlgebras
check "DGAlgebras"

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
Q = ZZ/101[x,y,z]
I = ideal{y^3,z*x^2,y*(z^2+y*x),z^3+2*x*y*z,x*(z^2+y*x),z*y^2,x^3,z*(z^2+2*x*y)}
R = Q/I
A = koszulComplexDGA(R)
time HA = homologyAlgebra(A)
reduceHilbert hilbertSeries HA

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
R = Q/I
A = koszulComplexDGA(R)
isHomologyAlgebraTrivial(A)

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
time kRes = res(coker vars R, LengthLimit => 20)

-- Homology
restart
loadPackage "DGAlgebras"
R3 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
A3 = acyclicClosure(R3,EndDegree=>1)
time apply(7, i -> time numgens prune homology(i,A3))
time kRes = res(coker vars R3, LengthLimit=> 18)
time apply(17, i -> time HH_i(kRes));

-- Tor algebras
restart
loadPackage "DGAlgebras"
R3 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
time TorR3 = torAlgebra(R3)
apply(16, i -> hilbertFunction(i,TorR3))
time res(coker vars R3, LengthLimit => 15)
R4 = QQ[x,y,z]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
TorR4 = torAlgebra(R4,GenDegreeLimit=>8)
apply(10, i -> hilbertFunction(i,TorR4))
res(coker vars R4, LengthLimit => 9)
TorR3R4 = torAlgebra(R3,R4,GenDegreeLimit=>4,RelDegreeLimit=>10)
reduceHilbert hilbertSeries TorR3R4
use R3
R4mod = coker matrix {{x^2*y^3*z^4}}
res(R4mod, LengthLimit => 6)

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
-- something is wrong with number of module generators/number of algebra generators
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



