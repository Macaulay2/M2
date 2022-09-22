-- -*- coding: utf-8 -*-
newPackage("DGAlgebras",
     Headline => "Data type for DG algebras",
     Version => "1.1.0",
     Date => "August 20, 2020",
     Authors => {
	  {Name => "Frank Moore",
	   HomePage => "http://www.math.wfu.edu/Faculty/Moore.html",
	   Email => "moorewf@wfu.edu"}},
     Keywords => {"Commutative Algebra"},
     DebuggingMode => false,
     PackageExports => {"IntegralClosure"}
     )

export {"DGAlgebra", "DGAlgebraMap", "dgAlgebraMap", "freeDGAlgebra", "setDiff", "natural", "cycles",
	"getBasis", "koszulComplexDGA", "acyclicClosure", "toComplex", "toComplexMap", "liftToDGMap",
        "killCycles", "getGenerators", "adjoinVariables", "deviations", "deviationsToPoincare", "expandGeomSeries", "zerothHomology",
        "torMap", "homologyAlgebra", "torAlgebra", "maxDegree", "StartDegree", "EndDegree", "ringMap",
	"isHomologyAlgebraTrivial", "findTrivialMasseyOperation", "findNaryTrivialMasseyOperation", "AssertWellDefined",
	"isGolod", "isGolodHomomorphism", "GenDegreeLimit", "RelDegreeLimit", "TMOLimit",
	"InitializeDegreeZeroHomology", "InitializeComplex", "isAcyclic", "getDegNModule",
	"masseyTripleProduct","getBoundaryPreimage","homologyClass",
	"homologyModule","dgAlgebraMultMap"
}

-- Questions:
-- is there a way to present graded pieces of graded A-modules as modules over A_0?
-- is there a way to make f act like a function for f a DGAlgebraMap?

-- Things to do before version 2
-- [user v1.5] Change toComplex to chainComplex as per conversation with Dan on the M2 Google group (9/14/2010)
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

-- Defining the new type DGAlgebra
DGAlgebra = new Type of MutableHashTable
globalAssignment DGAlgebra
--DGAlgebraMap still in development
DGAlgebraMap = new Type of MutableHashTable
globalAssignment DGAlgebraMap

-- this command is in the core, but we need it here.
spots  = C -> select(keys C, i -> class i === ZZ)

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
   T := getSymbol(opts.Variable);
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

setKoszulDiff = method(TypicalValue => DGAlgebra, Options => {InitializeDegreeZeroHomology => true, InitializeComplex => true})
setKoszulDiff (DGAlgebra,List) := opts -> (A,diffList) -> (
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
   diffList := apply(gensList, f -> A.diff(f));
   homDegreeShift := {1} | (toList ((#(degree first gensList)-1):0));
   all(#diffList, i -> degree gensList#i - homDegreeShift == degree diffList#i)
)

-- cache the basis of a DGAlgebra?
getBasis = method(TypicalValue => Matrix, Options => {Limit => -1})
getBasis (ZZ,DGAlgebra) := opts -> (homDegree,A) -> getBasis(homDegree,A.natural, Limit => opts.Limit)

getBasis (ZZ,Ring) := opts -> (homDegree,R) -> (
   local retVal;
   myMap := map(R, R.cache.basisAlgebra);
   tempList := (flatten entries basis(homDegree, R.cache.basisAlgebra, Limit => opts.Limit)) / myMap;
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
   phi := map(B.natural,A.natural,matrix {take(gens B.natural, numgens A.natural)});
   newDiffList := (take(flatten entries matrix A.diff, numgens A.natural) | cycleList) / phi;
   setDiff(B,newDiffList,InitializeComplex=>false);
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
   myColDegree := apply(colDegList_myCols, h -> -drop(h,1));
   tempMatrix = tempMatrix_myCols;
   myRowDegree = apply(rowDegList, h -> -drop(h,1));
   tempMatrix = map(R^myRowDegree, R^myColDegree,substitute(tempMatrix,R));
   prune coker tempMatrix
)

deviations = method(TypicalValue=>Tally,Options=>{DegreeLimit=>3})
deviations Ring := opts -> R -> (
  --tally degrees torAlgebra(R,GenDegreeLimit=>opts.DegreeLimit)
  kRes := res(coker vars R, LengthLimit => opts.DegreeLimit);
  deviations kRes
)

deviations ChainComplex := opts -> C -> (
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
  n := 3;
  if opts.GenDegreeLimit != infinity then n = opts.GenDegreeLimit;
  baseRing := coefficientRing R;
  kRes := res(coker vars R, LengthLimit => n);
  X := getSymbol("X");
  -- now build the degreeList and skewList out of the output from deviations
  RDevs := deviations(R,DegreeLimit=>opts.GenDegreeLimit);
  degreesList := sort flatten apply(pairs RDevs, p -> toList ((p#1):(flatten{p#0#0,p#0#1})));
  skewList := select(#degreesList, i -> odd first degreesList#i);
  torVars := toList(X_1..X_(#degreesList));
  baseRing[torVars,Degrees=>degreesList, SkewCommutative=>skewList]
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
  
     n := maxDeg;
     while n <= maxDeg and prune homology(n,A) == 0 do n = n - 1;
     maxHomologyDegree := n + 1;
     if opts.RelDegreeLimit != infinity then maxHomologyDegree = opts.RelDegreeLimit;

     cycleList := getGenerators(A,DegreeLimit=>maxDeg,Verbosity=>opts.Verbosity);

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
--- it returns the ChainComplexMap corresponding to left multiplication
--- by z
dgAlgebraMultMap = method()
dgAlgebraMultMap (DGAlgebra,RingElement) := (A,z) -> (
   R := A.ring;
   d := first degree z;
   cxA := toComplex A;
   zChainMap := map(cxA,cxA ** R^(-(drop(degree z,1))),
          i -> sub(last coefficients(z*getBasis(i,A),Monomials=>getBasis(i+d,A)), R),
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
   -- may add ability to provide maxdeg as an optional argument for toComplex later
   cxA := toComplex A;
   
   -- since z is a cycle, left multiplication by z is a chain map on A
   zChainMap := dgAlgebraMultMap(A,z);
   -- tensoring with M gives the action of z on A ** M
   zCMtensM := zChainMap ** M;
   -- determine the action of the homology class of z at the level of homology
   HM := HH(target zCMtensM);  
   -- prune the homology to ensure we have minimal generators
   pruneHM := prune HM;
   pruneZActionHM := prune HH(zCMtensM);
   -- ll for Loewy length
   ll := #(spots pruneHM);

   -- at this point we have all the multiplication tables, but we need to put them
   -- together to get a module structure.  Since HM is finite dimensional, this is not
   -- too bad.  The next part of the code combines all the actions into a large matrix
   -- representing the full multiplication table by z and uses this to construct a list of relations
   -- that represent the action of z, of the form ze_j = (z acting on e_j written in terms of basis).

   -- this next block of code constructs the full multiplication table
   -- for z on HM.  It constructs it as a block matrix, where the (i,j)th block
   -- is the left multiplication map M_i --> M_j.  Of course most of these are zero
   -- since z should be a cycle in a single homological degree.

   -- these commands ensure that the resulting matrix will have the right degrees
   -- over HA so that the result will be a graded module.
   degsHMTarget := flatten apply(spots pruneHM, p -> apply(degrees pruneHM#p, d -> {p} | d));
   degsHMSource := flatten apply(spots pruneHM, p -> apply(degrees pruneHM#p, d -> ({p} | d) + degree z));
   -- this function builds the blocks as mentioned above
   buildBlocks := (i,j) -> (
       if j + first degree z != i then map(pruneHM#i,pruneHM#j,0)
       else if not pruneZActionHM#?j then map(pruneHM#i,pruneHM#j,0)
       else pruneZActionHM#j
       );
   -- use map to build the matrix (using the matrix constructor for block matrices)
   matActOfZ := map(HA^(-degsHMTarget),HA^(-degsHMSource),tensor(map(HA,R),matrix table(ll,ll,buildBlocks)));
   -- now subtract from this z times an appropriately graded identity to indicate
   -- that this matrix is the result of z acting on HM.
   relsFromZAction := map(HA^(-degsHMTarget),HA^(-degsHMSource),h) - matActOfZ;
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
   (lifted,first myLift)
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

-- overload isWellDefined for DGAlgebraMap
isWellDefined DGAlgebraMap := f -> (
   A := source f;
   B := target f;
   all(apply(gens A.natural, x -> f.natural(A.diff(x)) == B.diff(f.natural(x))), identity)
)

toComplexMap = method(TypicalValue=>ChainComplexMap,Options=>{EndDegree=>-1,AssertWellDefined=>true})
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
   if R === S then (
      sourceList = flatten entries getBasis(n,A);
      targetList = flatten entries getBasis(n,B);
      coeffMatrix = substitute((coefficients(matrix {sourceList / f.natural}, Monomials => targetList))#1, B.ring);
      map(source bDiff, source aDiff, coeffMatrix)
   )
   else (
      sourceList = flatten entries getBasis(n,A);
      targetList = flatten entries getBasis(n,B);
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

pushForward(RingMap,ChainComplex) := opts -> (f,C) -> chainComplex apply(0..((length C)-1), i -> pushForward(f,C.dd_(i+1),opts))

-- The function below will return HH(f) as a module map over HH_0(A) (provided HH_0(B)
-- is a finite HH_0(A)-module). 
--homology (DGAlgebraMap,ZZ) := opts -> (f,n) -> (
--)

homology DGAlgebraMap := opts -> f -> (
   A := source f;
   B := target f;
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
    "Basic operations on DG Algebras"
    "The Koszul complex as a DG Algebra"
    "Basic operations on DG Algebra Maps"
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
      As a brief warning, the command @ TO poincareN @ which is used in @ TO deviations @ uses the symbols S and T internally, and may cause problems accessing such rings with the user interface.
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
      f = dgAlgebraMap(K2,K1,matrix{{0,T_1,T_2}})
    Text
      Once we define the DGAlgebraMap, it is a good idea to check to see if it indeed satisfies the Leibniz rule.  This can be checked by using
      isWellDefined.
    Example
      isWellDefined f
    Text
      Oops!  Let's try that again.
    Example
      g = dgAlgebraMap(K1,K2,matrix{{Y_2,Y_3}})
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
      Also, if one wishes to specify the name of the variables used, specify the Variable option; see the example in @ TO dgAlgebraMap @.
  Caveat
    There is currently a bug handling DG algebras that have no monomials in some degree, but some monomials in a later degree;
    for example if one replaces the 3 in the above example with a 5.
///

doc ///
  Key
    koszulComplexDGA
    (koszulComplexDGA,Ring)
    [koszulComplexDGA,Variable]
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
      One can also compute the homology of A directly with @ TO (homology,ZZ,DGAlgebra) @.  One may also specify
      the name of the variable using the Variable option.
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
    (deviations,ChainComplex)
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
      This command computes the deviations of a @ TO Ring @, a @ TO ChainComplex @, or a power series in the form of a @ TO RingElement @.
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
      Note that the deviations of T are not graded, since T is not graded.  When calling deviations on a ChainComplex, the
      zeroth free module must be cyclic, and this is checked.  The same goes for the case
      of a RingElement.
    Example
      R = ZZ/101[a,b,c,d]/ideal {a^3,b^3,c^3,d^3}
      A = degreesRing R
      kRes = res coker vars R
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
      pSeries = poincareN (res(coker vars R, LengthLimit=>6))
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
       z1 = z^2*T_5
       z2 = y_2^2*T_3
       z3 = x_2^2*T_1
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
    Computes the differential of a ring element in a DGAlgebra
  Usage
    b = diff(A,a)
  Inputs
    A:DGAlgebra
    a:RingElement
  Outputs
    b:RingElement
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
       z1 = z^2*T_5
       z2 = y_2^2*T_3
       z3 = x_2^2*T_1
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
      z1 = x^2*T_1
      z2 = y^2*T_2
      H = HH(KR)
      homologyClass(KR,z1*z2)
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
///

doc ///
  Key
    DGAlgebraMap
    ringMap
  Headline
    The class of all DG Algebra maps
  Description
    Text
      A common way to create a DGAlgebraMap is via @ TO liftToDGMap @.
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
      g = dgAlgebraMap(K1,K2,matrix{{Y_2,Y_3}})
      isWellDefined g
    Text
      The function does not check that the DG algebra map is well defined, however.
    Example
      f = dgAlgebraMap(K2,K1,matrix{{0,T_1,T_2}})
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
    Construct the ChainComplexMap associated to a DGAlgebraMap
  Usage
    psi = toComplexMap phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    psi:ChainComplexMap
  Description
    Example
       R = ZZ/101[a,b,c]/ideal{a^3+b^3+c^3,a*b*c}
       K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
       K2 = koszulComplexDGA(ideal {b,c},Variable=>"T")
       g = dgAlgebraMap(K1,K2,matrix{{Y_2,Y_3}})
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
    Computes the homomorphism in homology associated to a DGAlgebraMap.
  Usage
    homologyPhi = homology(phi,n)
  Inputs
    phi:DGAlgebraMap
  Outputs
    homologyPhi:RingMap
      The map on homology defined by phi.
  Description
    Example
      R = ZZ/101[a,b,c]/ideal{a^3+b^3+c^3,a*b*c}
      K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
      K2 = koszulComplexDGA(ideal {b,c},Variable=>"T")
      f = dgAlgebraMap(K2,K1,matrix{{0,T_1,T_2}})
      g = dgAlgebraMap(K1,K2,matrix{{Y_2,Y_3}})
      toComplexMap g
      HHg = HH g
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
    phi:ChainComplexMap
  Description
    Text
      If A is a DGAlgebra, and z is a cycle of A, then left multiplication of A by z gives
      a chain map from A to A.  This command converts A to a complex using @ TO toComplex @,
      and constructs a @ TO ChainComplexMap @ that represents left multiplication by z.
      This command is used to determine the module structure that is computed in
      @ TO homologyModule @.
    Example
      R = QQ[x,y,z]/ideal{x^3,y^3,z^3}
      KR = koszulComplexDGA R
      z1 = x^2*T_1
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
      Hphi = prune HH(phi); (Hphi#0,Hphi#1,Hphi#2)
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
    Outputs the source of a DGAlgebraMap
  Usage
    source phi
  Inputs
    phi:DGAlgebraMap
///

doc ///
  Key
    (target,DGAlgebraMap)
  Headline
    Outputs the target of a DGAlgebraMap
  Usage
    target phi
  Inputs
    phi:DGAlgebraMap
///

doc ///
  Key
    AssertWellDefined
  Headline
    Option to check whether the lifted map on DGAlgebras is well defined.
  Usage
    liftToDGMap(...,AssertWellDefined=>true)
///

doc ///
  Key
    StartDegree
  Headline
    Option to specify the degree to start computing the acyclic closure and killing cycles
  Usage
    acyclicClosure(...,StartDegree=>n)
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
    [isGolodHomomorphism,TMOLimit]
  Headline
    Option to specify the maximum degree to look for generators when computing the deviations
  Usage
    isGolodHomomorphism(...,DegreeLimit=>n)
///

doc ///
  Key
    [homologyAlgebra,Verbosity]
  Headline
    Option to specify the maximum degree to look for generators when computing the deviations
  Usage
    homologyAlgebra(...,DegreeLimit=>n)
///

doc ///
  Key
    [getGenerators,Verbosity]
  Headline
    Option to specify the maximum degree to look for generators when computing the deviations
  Usage
    getGenerators(...,Verbosity=>n)
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

doc ///
  Key
    (net,DGAlgebraMap)
  Headline
    Outputs the pertinent information about a DGAlgebraMap
  Usage
    net phi
  Inputs
    phi:DGAlgebraMap
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
betti (A = res I)
R = S/I
assert(isHomologyAlgebraTrivial koszulComplexDGA R == true)
assert(isGolod R == false)
///

end

-- How to install the package
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
res coker vars R2
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
kSRes = res(coker matrix {{x,y,z,w}}, LengthLimit=>5)
kSRes1push = pushForward(f,kSRes.dd_1)
kSRes2push = pushForward(f,kSRes.dd_2)
kSRes3push = pushForward(f,kSRes.dd_3)
kSRes1push*kSRes2push
kSRes2push*kSRes3push
prune homology(kSRes1push,kSRes2push)
prune homology(kSRes2push,kSRes3push)
-- pushforward the ChainComplex
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
-- pushForward (for ChainComplex; not yet written) for this to work.
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
res coker gens I
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
setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
isHomogeneous(A)
