-- -*- coding: utf-8 -*-
newPackage("DGAlgebras",
     Authors => {
	  {Name => "Frank Moore"}
	  },
     DebuggingMode => true,
     Headline => "Data type for DG Algebras",
     Version => "0.3"
     )

export {DGAlgebra, dgAlgebra, setDiff, natural, cycles,
        toComplex, koszulComplexDGA, acyclicClosure,
	killCycles, adjoinVariables, homology2,
        homologyAlgebra, torAlgebra, polyDifferential}

------------------------------------------------
-- Set DG algebra types and constructor functions. -- 
------------------------------------------------

DGAlgebra = new Type of MutableHashTable

-- this is the user friendly version of the DGAlgebra 'constructor'.  In the code below, we use makeDGAlgebra.
dgAlgebra = method()
dgAlgebra(Ring,List) := (R,degList) -> makeDGAlgebra(R, pack(degList,1))

makeDGAlgebra = method()     
makeDGAlgebra(Ring,List) := (R,degList) -> (
     -- Input:  A ring, a list of degrees of the variables, and a list that defines the differential
     -- Output:  A hash table of type DGAlgebra
     A := new MutableHashTable;
     A#(symbol ring) = R;
     varsList := toList (T_1..T_(#degList));
     A#(symbol diff) = {};
     if (isHomogeneous R) then
     (
        A#(symbol natural) = (A.ring)[varsList, Degrees => degList, Join => false, SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
	A#(symbol isHomogeneous) = true;
     )
     else
     (
	A#(symbol natural) = (A.ring)[varsList, Degrees => degList, SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
	A#(symbol isHomogeneous) = false;
     );
     A.natural.cache = new CacheTable;
     A.natural.cache#basisAlgebra = (A.ring)[varsList, Join => false, Degrees => apply(degList, i -> {first i}), SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
     use A.natural;
     A#(symbol Degrees) = degList;
     A#(symbol cache) = new CacheTable;
     A.cache#(symbol homology) = new MutableHashTable;
     A.cache#(symbol differentials) = new MutableHashTable;
     -- *should* verify that the differential is indeed of degree -1
     new DGAlgebra from A
)

TEST ///
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = ZZ/101[x,y,z]
A = dgAlgebra(R,{1,1,1,3})
A.natural
setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
Add = toComplex(A)
///

setDiff = method()
setDiff(DGAlgebra,List) := (A,diffList) -> (
   A.diff = map(A.natural,A.natural, substitute(matrix {diffList}, A.natural));
   A.diff
)

TEST ///
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = ZZ/101[x,y,z, Degrees => {2,2,3}]
A = koszulComplexDGA(R)
S1 = R/ideal (x^3-z^2)
B = koszulComplexDGA(S1)
use R
S2 = R/ideal (x^5-z^2)
C = koszulComplexDGA(S2)
degrees C.natural
test = getBasis(2,A, Limit => 2)
test = getBasis(3,A, Limit => 2)
test = getBasis(2,A)
test = getBasis(4,A)
degrees source test
///

getBasis = method(Options => {Limit => -1})
getBasis(ZZ,DGAlgebra) := opts -> (homDegree,A) -> getBasis(homDegree,A.natural, Limit => opts.Limit)

getBasis(ZZ,Ring) := opts -> (homDegree,R) -> (
   local retVal;
   myMap := map(R,R.cache.basisAlgebra);
   tempList := (flatten entries basis(homDegree, R.cache.basisAlgebra, Limit => opts.Limit)) / myMap;
   if (tempList == {}) then retVal = map((R)^1,(R)^0, 0) else
   (
      degList := apply(tempList, m -> -degree m);
      retVal = map(R^1, R^degList, matrix {tempList});
   );
   retVal
)

koszulComplexDGA = method()
koszulComplexDGA(Ring) := (R) -> (
   local A;
   if (isHomogeneous R) then (
      degList := apply(degrees R, i -> {1} | i);
      A = makeDGAlgebra(R, degList);
      use A.ring;
      setDiff(A, gens R);
   )
   else (
      A = makeDGAlgebra(R, toList ((numgens R):{1}));
      use A.ring;
      setDiff(A, gens R);
   );
   A
)

koszulComplexDGA(Ideal) := (I) -> (
   local A;
   if (isHomogeneous I) then (
      degList := apply(flatten entries gens I, i -> {1} | degree i); 
      A = makeDGAlgebra(ring I, degList);
      use A.ring;
      setDiff(A,I_*);
   )
   else
   (
      A = makeDGAlgebra(R, toList ((numgens I):{1}));
      use A.ring;
      setDiff(A, I_*);
   );
   A
)

koszulComplexDGA(List) := (ringElts) -> koszulComplexDGA(ideal ringElts);

TEST ///
restart
loadPackage "DGAlgebras"
R = ZZ/101[a,b,c]
I = ideal{a^3,b^3,c^3,a^2*b^2*c^2}
A = koszulComplexDGA(I)
Add = toComplex(A)
Add.dd
(koszul gens I).dd
///

taylorDGA = method()
taylorDGA(MonomialIdeal) := (I) -> (
   -- not implemented yet.
   A
)

toComplex = method()
toComplex(ZZ,DGAlgebra) := (N,A) -> chainComplex(apply(N, i -> polyDifferential(A,i+1)))

toComplex(DGAlgebra) := (A) -> (
   if (any(degrees (A.natural) / first, i -> even i)) then error "Must specify an upper degree bound if an even generator exists.";
   maxDegree := sum ((degrees A.natural) / first);
   chainComplex(apply(maxDegree, i -> polyDifferential(A,i+1)))
)

TEST ///
-- Test toComplex here.
///

killCycles = method(Options => {StartDegree => 1, EndDegree => -1})
killCycles(DGAlgebra) := opts -> (A) -> (
  -- for now, this will only work for DG algebras with H_0(A) = k
  retVal := 0;
  endDegree := 0;
  if (opts.EndDegree == -1) then endDegree = opts.StartDegree;
  if (opts.StartDegree > endDegree) then error "Starting degree is not less than or equal to ending degree.";
  n := opts.StartDegree;
  foundHomology := false;
  nthHomology := 0;
  while (n <= endDegree and not foundHomology) do ( nthHomology = prune homology2(n,A); if (nthHomology == 0) then n = n + 1 else foundHomology = true);
  -- at this point we have found a degree with nontrivial homology.
  -- we now add variables in one degree higher to make these cycles boundaries.
  if (not foundHomology) then retVal = A else
  (  
     homologyGenerators := entries transpose generators image (nthHomology.cache.pruningMap);
     basisList := flatten entries getBasis(n,A);
     cycleList := apply(homologyGenerators, gen -> sum apply(#gen, i -> gen#i*basisList#i));
     retVal = adjoinVariables(A,cycleList);
  );
  retVal
)

TEST ///
-- Test killCycles here.
///

adjoinVariables = method()
adjoinVariables(DGAlgebra, List) := (A,cycleList) -> (
  -- this function will add a new variable to make the elements of cycles boundaries in a new DG algebra (semifree over the input)
  newDegreesList := A.Degrees | apply(cycleList, z -> (first degree z) + 1);
  B := makeDGAlgebra(A.ring,newDegreesList);
  newDiffList := apply(take(flatten entries matrix A.diff, numgens A.natural) | cycleList, f -> substitute(f, B.natural));
  setDiff(B,newDiffList);
  B
)

acyclicClosure = method(Options => {StartDegree => 1})
acyclicClosure(Ring,ZZ) := opts -> (R, homologicalDegreeLimit) -> (
  K := koszulComplexDGA(R);
  acyclicClosure(K,homologicalDegreeLimit)
)

acyclicClosure(DGAlgebra,ZZ) := opts -> (A, homologicalDegreeLimit) -> (
  n := opts.StartDegree;
  while (n <= homologicalDegreeLimit) do (
     A = killCycles(A,StartDegree => n);
     n = n + 1;
  );
  A
)

TEST ///
-- Test acyclicClosure here.
///

polyDiffMonomial := (A,m) -> (
  -- uses the Leibniz rule to compute the differential of a traditional monomial
  dgSign := 1;
  monSupport := support m;
  monExponents := select(first exponents m, i -> i > 0);
  monSupportPowers := apply(#monSupport, i -> (monSupport#i)^(monExponents#i));
  firstDiffTerms := apply(#monSupport, i -> product take(monSupportPowers,i));
  lastDiffTerms := apply(#monSupport, i -> product drop(monSupportPowers,i+1));
  diffCoeffs := apply(#monSupport, i -> A.diff(monSupport#i)*(monExponents#i)*(monSupport#i)^((monExponents#i)-1));
  diffSigns := apply(#monSupport, l -> product apply(l, i -> (-1)^((first degree monSupport#i)*(monExponents#i))));
  allTerms := apply(#monSupport, i -> (diffSigns#i)*(firstDiffTerms#i)*(diffCoeffs#i)*(lastDiffTerms#i));
  sum allTerms
)

polyDifferential = method()
polyDifferential(DGAlgebra,ZZ) := (A,n) -> (
  if (A.cache.differentials#?n) then A.cache.differentials#n
  else if (n == 0) then map((A.ring)^0,(A.ring)^1,0)
  else (
     -- here, check to see if the ring A is graded with graded differential.  If so, then produce
     -- a graded map.  Otherwise, just treat things as ungraded (should be slower)
     -- TODO: add graded differential.
     local newDiffl;
     sourceList := getBasis(n,A);
     sourceDegreeList := apply(degrees source sourceList, l -> drop(l,1));
     sourceList = flatten entries sourceList;
     targetList := getBasis(n-1,A);
     targetDegreeList := apply(degrees source targetList, l -> drop(l,1));
     targetList = flatten entries targetList;
     diffList := matrix {apply(sourceList, m -> polyDiffMonomial(A,m))};
     coeffMatrix := substitute((coefficients(diffList, Monomials => targetList))#1, A.ring);
     degreeRank = #(first sourceDegreeList);
     if (degreeRank > 0) then (
        sourceDegreeList = apply(sourceDegreeList, l -> -l);
	targetDegreeList = apply(targetDegreeList, l -> -l);
        newDiffl = map((A.ring)^(targetDegreeList), (A.ring)^(sourceDegreeList), coeffMatrix);
     )
     else newDiffl = map((A.ring)^(#targetDegreeList), (A.ring)^(#sourceDegreeList), coeffMatrix);        
     A.cache.differentials#n = newDiffl;
     newDiffl
  )
)

TEST ///
-- test polyDifferential here.
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = ZZ/101[x,y,z, Degrees => {2,2,3}]
kRes = res coker vars R
kRes.dd_3
A = koszulComplexDGA(R)
d3 = polyDifferential(A,3)
d2 = polyDifferential(A,2)
d1 = polyDifferential(A,1)
assert(source d1 == target d2)
assert(source d2 == target d3)
S1 = R/ideal (x^3-z^2)
B1 = koszulComplexDGA(S1)
d3 = polyDifferential(B1,3)
d2 = polyDifferential(B1,2)
d1 = polyDifferential(B1,1)
assert(source d1 == target d2)
assert(source d2 == target d3)
use R
S2 = R/ideal (x^4-z^2)
B2 = koszulComplexDGA(S2)
d3 = polyDifferential(B2,3)
d2 = polyDifferential(B2,2)
d1 = polyDifferential(B2,1)
assert(source d1 == target d2)
assert(source d2 == target d3)
///

polyDifferential(DGAlgebra,RingElement) := (A,f) -> (
  sum apply(terms f, m -> polyDiffMonomial(A,m))
)

TEST ///
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
A = koszulComplexDGA(R)
A.diff
polyDifferential(A,sub(T_1*T_2*T_3, A.natural))
///

polyHomology := (n,A) -> (
  dn := 0;
  dnplus1 := 0;
  retVal := 0;
  if (A.cache.homology#?n) then retVal = A.cache.homology#n
  else if (#(flatten entries getBasis(n, A, Limit => 1)) != 0) then
  (
     if n == 0 then dn = map((A.ring)^0, (A.ring)^1, 0) else dn = polyDifferential(A,n);
     if (#(flatten entries getBasis(n+1, A, Limit => 1)) != 0) then
        dnplus1 = polyDifferential(A,n+1)
     else
        dnplus1 = map(source dn, (A.ring)^0, 0);
     retVal = homology(dn,dnplus1);
     A.cache.homology#n = retVal;
  )
  else
     retVal = (A.ring)^0;
  retVal
)

TEST ///
--- test homology2
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
hh2 = HH_2(koszulR)
-- make an assertion here.
///

-- note that this does not work for some reason (Dan explained it to me at one point but I can't remember.  I think it has
-- something to do with the fact that in the M2 scripting language, homology(sequence) hijacks all possible calls to homology.
homology(ZZ,DGAlgebra) := (n,A) -> polyHomology(n,A)

-- Temporary fix here for the moment
homology2 = method();
homology2(ZZ,DGAlgebra) := (n,A) -> polyHomology(n,A)

torAlgebra = method()
torAlgebra(Ring,ZZ) := (R,n) -> (
  -- since we are not yet implementing the Hopf structure, only the algebra structure, we need not
  -- actually use DGAlgebras to compute the Tor algebra.  We use the built in resolution function
  -- for the resolution of R/(ideal vars R) below since it is much faster.
  -- TODO: make the below return a bigraded algebra if R is homogeneous.
  baseRing := coefficientRing R;
  kRes := res(coker vars R, LengthLimit => n);
  bettiNums := apply((length kRes)+1, i -> rank source kRes.dd_i);
  local torSoFar;
  if (length kRes == 0) then baseRing else (
     currentDegree := 1;
     newVars := toList (X_1..X_(bettiNums#currentDegree));
     degreeList := toList ((bettiNums#currentDegree):1);
     skewList := toList (0..((bettiNums#currentDegree)-1));
     torSoFar = baseRing[newVars,Degrees=>degreeList, SkewCommutative=>skewList];
     currentDegree = currentDegree + 1;
     while(currentDegree <= n) do (
        dimInCurDegree := hilbertFunction(currentDegree,torSoFar);
        numNewVars := bettiNums#currentDegree - dimInCurDegree;
	-- the below check will only fail if R is a complete intersection, and currentDegree = 3.  The numNewVars are the
	-- deviations of the ring R; these vanish rigidly by a theorem of Halperin.
	if (numNewVars != 0) then (	 
           newVars = newVars | toList (X_((numgens torSoFar)+1)..X_((numgens torSoFar) + numNewVars));
           degreeList = degreeList | toList (numNewVars:currentDegree);
           if (odd currentDegree) then skewList = skewList | toList ((numgens torSoFar)..((numgens torSoFar) + numNewVars - 1));
           torSoFar = baseRing[newVars,Degrees=>degreeList, SkewCommutative=>skewList];
           currentDegree = currentDegree + 1;
	)
        else currentDegree = n+1;
     );
     torSoFar
  )
)

torAlgebra(Ring) := (R) -> torAlgebra(R,3)

torAlgebra(Ring,Ring,ZZ,ZZ) := (R,S,genDegree,relDegree) -> (
  -- S is an R-algebra
  acycClos := acyclicClosure(R,genDegree);
  acycClos' := acycClos ** S;
  homologyAlgebra(acycClos',genDegree,relDegree)
)

TEST ///
-- Test torAlgebra here.
///

representativeCycles = method()
representativeCycles(DGAlgebra,ZZ) := (A,n) -> (
  temp := transpose generators image ((prune homology2(n,A)).cache.pruningMap);
  homologyGenerators := entries temp;
  basisList := flatten entries getBasis(n,A);
  cycleList := apply(homologyGenerators, gen -> sum apply(#gen, i -> gen#i*basisList#i));
  cycleList
)

makeHomologyRing := (A, cycleList, relList) -> (
  --baseRing := A.ring/(ideal flatten entries polyDifferential(A,1));
  local HA;
  local degreesList;
  baseRing := coefficientRing A.ring;
  if (A.isHomogeneous) then degreesList = (cycleList / degree) else degreesList = pack((cycleList / degree) / first, 1);
  varList := toList (X_1..X_(#cycleList));
  polyRing := baseRing[varList, Degrees => degreesList, SkewCommutative => select(toList(0..(#degreesList-1)), i -> odd first degreesList#i)];
  polyRing' := baseRing[varList, Degrees => (degreesList / first), SkewCommutative => select(toList(0..(#degreesList-1)), i -> odd first degreesList#i)];
  if (relList == {}) then (
     HA = polyRing;
     HA.cache = new CacheTable;
     HA.cache#basisAlgebra = polyRing';
  )
  else (
     I := ideal relList;
     myMap := map(polyRing, ring I, gens polyRing);
     I = myMap(I);
     forceGB gens I;
     HA = polyRing/I;
     HA.cache = new CacheTable;
     --- set up the cached algebra for basis computations too
     myMap' := map(polyRing', polyRing, gens polyRing');
     I' = myMap'(I);
     if myMap'(leadTerm(gens I)) - leadTerm gens I' != 0 then error "Monomial Order changed.";
     forceGB gens I';
     HA.cache#basisAlgebra = polyRing'/I';
  );
  HA
)

TEST ///
I = ideal relList;
numgens I
Y = ring I;
forceGB gens I
time Y' = Y/I;
---
I1 = ideal select(I_*,f -> size f == 1);
forceGB gens I1
I2 = ideal select(I_*,f -> size f > 1)
numgens I1
numgens I2
Y1 = Y/I1;
time Y2 = Y1/sub(I2,Y1)
---
///

-- This code finds the relations that exist in the homology algebra that come from simply the relations that exist
-- in the ring, not including the ones that come because one must include the boundaries in determining the relations
findEasyRelations = method()
findEasyRelations(DGAlgebra,List) := (A, cycleList) -> (
  -- need to document this code!
  baseRing := coefficientRing A.ring;
  varsList := apply(gens A.ring | gens A.natural,f -> sub(f,A.natural)) | toList (X_1..X_(#cycleList));
  naturalGens := gens A.natural;
  skewList := apply(select(toList(0..#naturalGens-1), i -> odd first degree naturalGens#i), i -> i + #(gens A.ring));
  skewList = skewList | apply(select(toList(0..#cycleList-1), i -> odd first degree cycleList#i), i -> i+#(gens A.ring) + #(gens A.natural));
  degList := apply(#(gens A.ring) + #(gens A.natural), i -> degree varsList#i);
  degList = degList | apply(cycleList, i -> degree i);
  if (not A.isHomogeneous) then degList = pack(degList / first, 1);
  B := baseRing[varsList,MonomialOrder=>{#(gens A.ring)+#(gens A.natural),#cycleList},Degrees=>degList, SkewCommutative=>skewList];
  K := substitute(ideal A.ring, B) + ideal apply(#cycleList, i -> X_(i+1) - substitute(cycleList#i,B));
  assert(isHomogeneous K);
  easyRels := ideal selectInSubring(1,gens gb K);
  degList = apply(cycleList, i -> degree i);
  skewList = select(toList(0..#degList-1), i -> odd first degList#i);
  C := baseRing[X_1..X_(#cycleList), Degrees => degList, SkewCommutative=>skewList];
  makeHomologyRing(A,cycleList,(sub(easyRels,C))_*)
)

TEST ///
-- test findEasyRelations
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
R = ZZ/32003[a,b,x,y,Degrees=>{1,1,2,2}]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
A = koszulComplexDGA(R)
apply(5,i -> numgens prune homology2(i,A))
time cycleList = getGenerators(A,4)
time HAEasy = findEasyRelations(A,cycleList)
reduceHilbert hilbertSeries HAEasy
reduceHilbert hilbertSeries (HAEasy.cache.basisAlgebra)
///

getCycleProductMatrix = method()
getCycleProductMatrix(DGAlgebra,Ring,List,ZZ) := (A,HA,cycleList,N) -> (
  -- the input is the dga A, the homology algebra HA (so far), the list of cycle generators, and the degree.
  -- this version does use the knowledge of the homologyAlgebra so far to return the cycles products in a given degree.
  local retVal;
  local myMap;
  time monListHA := flatten entries getBasis(N,HA);
  time expListHA := flatten(monListHA / exponents);
  monListA := flatten entries getBasis(N,A);
  -- slow here
  time cycleProductList := apply(expListHA, xs -> product apply(#xs, i -> (cycleList#i)^(xs#i)));
  -- slow here
  << "-- cycleProduct --" << endl;
  time cycleProductMatrix := flatten apply(cycleProductList, z -> entries transpose (coefficients(z, Monomials => monListA))#1);
  -- make sure the degrees are correct...
  if (A.isHomogeneous) then
  (
     sourceDegreeList := apply((monListHA / degree), l -> -drop(l,1));
     targetDegreeList := apply((monListA / degree), l -> -drop(l,1));
     time cycleProductMatrix = map((A.ring)^targetDegreeList, (A.ring)^sourceDegreeList, sub(transpose matrix cycleProductMatrix, A.ring));
  )
  else
  (
     myMap = map(A.ring, A.natural);
     cycleProductMatrix = map((A.ring)^(#monListA), (A.ring)^(#monListHA), sub(transpose matrix cycleProductMatrix, A.ring));
  );
  (cycleProductMatrix,monListHA)
)

getCycleProductMatrix(DGAlgebra,List,ZZ) := (A,cycleList,N) -> (
  -- the input is the dga A, the list of cycle generators, and the degree.
  -- this function just assumes that HA is the free algebra on cycleList, and calls the method defined above
  -- the below function slowed down the computation significantly
  --HA := findEasyRelations(A,cycleList);
  HA := makeHomologyRing(A,cycleList,{});
  getCycleProductMatrix(A,HA,cycleList,N)
)

findDegNGenerators := (A,oldCycleList,N) -> (
  -- The goal of this function is to return the generators and relations in degree n.
  cycleList := {};
  relsList := {};
  varList := {};
  if (oldCycleList == {}) then (
     -- here, we know all the degree 1 elements are generators
     cycleList = representativeCycles(A,N);
  )
  else if (flatten entries getBasis(N,A, Limit => 1) == {}) then cycleList = {}
  else (
     nthHomology := homology2(N,A);
     if (prune nthHomology != 0)
     then (
	<< "---" << endl;
	time (cycleProductMatrix,monListHA) := getCycleProductMatrix(A,oldCycleList,N);
        << "---" << endl;
	if (monListHA != {}) then (
	   -- TODO: Document the below block of code.
	   newHomology := prune (nthHomology / (image cycleProductMatrix));
	   monListA := flatten entries getBasis(N,A);
           cycleList = apply(entries transpose gens image newHomology.cache.pruningMap, zList -> apply(#zList, i -> zList#i*monListA#i)) / sum;
        )
        else (
	   -- if we are here, then we need to add all of this degree as generators.
	   cycleList = representativeCycles(A,N);
	);
     ); 
  );
  cycleList
)

TEST ///
-- Homology algebra for the Koszul complex on a set of generators of the maximal ideal
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
time apply(5,i -> numgens prune homology2(i,A))
-- ~6.8 seconds on mbp, with graded differentials
time HA = homologyAlgebra(A)
reduceHilbert hilbertSeries HA
-- same example, but not graded because of the degree change.  The homologyAlgebra function
-- will then only return a graded algebra
R2 = ZZ/32003[a,b,x,y,Degrees=>{1,1,2,2}]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR2 = koszul vars R2
time apply(5,i -> numgens prune HH_i(koszulR2))
A2 = koszulComplexDGA(R2)
time apply(5,i -> numgens prune homology2(i,A2))
-- 7.1 seconds on mbp, with ungraded differentials
-- TODO: Fix the inHomogeneous case.  There is a problem in preprocessing
time HA2 = homologyAlgebra(A2)
-- should only be singly graded
reduceHilbert hilbertSeries HA2
///

TEST ///
--- Homology algebra for the Koszul complex on a set of generators of an ideal
--- should try to get this to work.
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]
I = ideal{a^3,b^3,x^3,y^3}
A = koszulComplexDGA(I)
-- incorrect at the moment
homologyAlgebra(A)
///

findDegNRelations := (A,HA,algGens,N) -> (
  -- this function tries to find the relations in degree N that involve the generators in the list algGens
  -- no checking is done to see if algGens are actually minimal generators at this point.
  local cycleProductList;
  local monListHA;
  retVal := {0_HA};
  -- check if DGA is zero in this degree. If so, just return back the monomials in the given degree
  if ((flatten entries getBasis(N, A, Limit => 1) != {}) or (#algGens == 0)) then (
     -- using HA, check if there are indeed any new relations in degree n
     nthHomology := homology2(N,A);
     pruneNthHomology := prune nthHomology;
     rankOfNthHomology := numgens pruneNthHomology;
     << "-----------------" << endl;
     << "--- Degree " << N << "  ---" << endl;
     << "-----------------" << endl;
     time rankOfAlgebraSoFar := #(flatten entries getBasis(N,HA));
     if (rankOfNthHomology != rankOfAlgebraSoFar) then
     (
       -- when in here, we know there is a relation in degree N.
       -- so take each monomial of the correct degree, build the cycle corresponding to that
       -- and define a map from the residue field to the homology class representing each cycle.
       -- then take the kernel, prune, and use cache.pruningMap to get the actual minimal generating
       -- set of the kernel.  Finally, reconstruct the elements from the monomials and viola!
       if (pruneNthHomology == 0) then (
          -- if we are here, all monomials in the HA of this degree are zero.
          retVal = flatten entries getBasis(N,HA);
       )
       else (
          (cycleProductMatrix,monListHA) = getCycleProductMatrix(A,HA,algGens,N);
          if (monListHA != {}) then (
             -- TODO: Document this code.
             baseRing := coker vars (A.ring);
	     multMap := map(coker relations nthHomology,(A.ring)^(rank source cycleProductMatrix),cycleProductMatrix);
             kerMultMap := gens ker multMap;
             coeffField := (A.ring)/ideal vars A.ring;
	     kerMultMap = compress sub(kerMultMap,coeffField);
	     kernelGens = entries transpose substitute(kerMultMap,coefficientRing A.ring);
	     time retVal = apply(kernelGens, z -> sum apply(#z, i -> (monListHA#i)*(z#i)));
          );
       );
     );
  )
  else (
     retVal = flatten entries getBasis(N,HA); 
  );
  retVal
)

TEST ///
degrees source multMap
///

getGreaterMonomials:= (R,N) -> (
  maxDegree := max (degrees R / first);
  flatten apply(maxDegree, i -> flatten entries getBasis(N+i,R))
)

getGenerators = method()
getGenerators(DGAlgebra,ZZ) := (A,genDegreeLimit) -> (
  n := 1;
  cycleList := {};
  while (n <= genDegreeLimit) do (
     << "Computing generator degree " << n << endl;
     newCycleList := findDegNGenerators(A,cycleList,n);
     cycleList = cycleList | newCycleList;
     n = n + 1;
  );
  cycleList
)

getRelations = method()
getRelations(DGAlgebra,Ring,List,ZZ) := (A,HA,cycleList,relDegreeLimit) -> (
   relList := (ideal HA)_*;
   n := 2;
   while (n <= relDegreeLimit) do (
      << "Computing relation degree " << n << endl;
      newRelList := findDegNRelations(A,HA,cycleList,n);
      if (relList == {}) then relList = newRelList
      else if (newRelList != {}) then (
         -- make sure newRelList and relList are in the same ring
         myMap = map(ring first relList, ring first newRelList, flatten entries vars ring first relList);
         relList = relList | (newRelList / myMap);
      );
      -- now reset HA using relList for the next iteration.
      HA = makeHomologyRing(A,cycleList,relList);
      n = n + 1;
   );
   -- put the cycles that the variables represent in the cache.
   HA.cache#cycles = cycleList;
   A.cache#homologyAlgebra = HA;
   HA
)

homologyAlgebra = method()
homologyAlgebra(DGAlgebra,ZZ,ZZ) := (A,genDegreeLimit,relDegreeLimit) -> (
  cycleList := {};
  relList := {};
  n := 1;
  local HA;
  local myMap;
  time cycleList = getGenerators(A,genDegreeLimit);

  if (cycleList == {}) then (
     -- put the cycles that the variables represent in the cache.
     HA = coefficientRing A.ring;
     HA.cache = new CacheTable;
     HA.cache#cycles = cycleList;
     A.cache#homologyAlgebra = HA;
  )
  else (
     time HA = findEasyRelations(A,cycleList);
     time HA = getRelations(A,HA,cycleList,relDegreeLimit);
  );
  HA
)

homologyAlgebra(DGAlgebra) := (A) -> (
  -- this is a routine that will compute the complete homology algebra
  -- if the DG Algebra is known to be finite rank free module over the base ring.
  cycleList := {};
  relList := {};
  n := 1;
  local HA;
  local myMap;
  
  -------------------------------------------
  -- Find the degree bounds needed for the usual homologyAlgebra function call
  degreesList := degrees A.natural / first;
  if (any(degreesList, i -> even i))
     then error "Must supply upper degree bound on generators and relations if there is a DG Algebra generator of even degree.";
  -- otherwise, all are odd, and we can compute the entire homology algebra
  maxDegree := sum degreesList;
  
  n = maxDegree;
  while (n <= maxDegree and prune homology2(n,A) == 0) do n = n - 1;
  maxHomologyDegree := n;
  -------------------------------------------
  
  HA = homologyAlgebra(A,maxDegree,maxHomologyDegree);
  relList = (ideal HA)_*;
  cycleList = HA.cache.cycles;
  
  HA
)

DGAlgebra ** Ring := (A,S) -> (
  B := makeDGAlgebra(S, A.Degrees);
  newDiff := apply(flatten entries matrix (A.diff), f -> substitute(f,B.natural));
  B.diff = map(B.natural,B.natural, newDiff);
  B
)

TEST ///
restart
loadPackage "DGAlgebras"
R = ZZ/32003[x,y,z]/ideal{x^3,y^4,z^5}
B = koszulComplexDGA(R)
HB = homologyAlgebra(B)
reduceHilbert hilbertSeries HB
<restart
loadPackage "DGAlgebras"
R = ZZ/32003[x,y,z,w]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
B = koszulComplexDGA(R)
HB = homologyAlgebra(B)
reduceHilbert hilbertSeries HB
HB = homologyAlgebra(B,5,15)
reduceHilbert hilbertSeries HB
numgens trim ideal HB
prune HH(koszul vars R)
///

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
      This package is used to define and manipulate DG Algebras.
///

doc ///
  Key
    DGAlgebra
  Headline
    The class of all DGAlgebras
  Description
    Text
      Common ways to create a DG algebra
      * @ TO (dgAlgebra, Ring, List) @
      * @ TO (setDiff,DGAlgebra,List) @
      * @ TO (koszulComplexDGA, Ring) @
      * @ TO (koszulComplexDGA, Ideal) @
      * @ TO (acyclicClosure, Ring, ZZ) @
      
      Information about a DG algebra
      * @ TO (homology2,ZZ,DGAlgebra) @
      * @ TO (homologyAlgebra,DGAlgebra) @
      * @ TO (homologyAlgebra,DGAlgebra,ZZ,ZZ) @
      
      Operations on DG algebras
      * @ TO (toComplex, ZZ, DGAlgebra) @
      * @ TO (killCycles, DGAlgebra) @
      * @ TO (adjoinVariables, DGAlgebra, List) @
      * @ TO (acyclicClosure, DGAlgebra, ZZ) @
///

--* @ TO (**, DGAlgebra, Ring) @
--* @ TO (**, DGAlgebra, DGAlgebra) @

doc ///
  Key
    dgAlgebra
  Headline
    Constructs a DGAlgebra
  Usage
    A = dgAlgebra(R,degreeList) 
///

doc ///
  Key
    (dgAlgebra,Ring,List)
  Headline
    Constructs a DGAlgebra
  Usage
    A = dgAlgebra(R,degreeList) 
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
      polynomial ring in variables of the degrees input.  The current version of this package
      does not handle algebras A whose underlying algebra is not a polynomial ring.
    Example
      R = ZZ/101[x,y,z]
      A = dgAlgebra(R,{1,1,1,3})
      A.natural
      setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
      Add = toComplex(A)
    Text  
      Note that the differential is not passed into the constructor.  The reason for this (at the moment)
      is that M2 does not know what ring the differentials are defined over until after the underlying
      algebra is constructed, so the differential is set later with setDiff.  Many DG Algebras that one
      encounters in commutative algebra have been implemented, however.  For example, if one wants to work
      with the Koszul complex as a DG Algebra, then one should see the command @ TO koszulComplexDGA @.
     
      There is currently a bug handling DG Algebras that have no monomials in some degree, but some monomials in a later degree;
      for example if one replaces the 3 in the above example with a 5.
///

doc ///
  Key
    koszulComplexDGA
  Headline
    Returns the Koszul complex as a DGAlgebra
  Usage
    A = koszulComplexDGA(R) or A = koszulComplexDGA(I)
  Inputs
    R:Ring 
      If just a ring is passed in, then it returns the Koszul complex on ideal vars R.
    I:Ideal 
      Returns the Koszul complex on gens R as a DGA.
  Outputs
    A:DGAlgebra
///

doc ///
  Key
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
      complexA = toComplex(A)
      complexA.dd
      ranks = apply(4, i -> numgens prune HH_i(complexA))
      ranks == apply(4, i -> numgens prune HH_i(koszul vars R))
    Text
      One can also compute the homology of A directly with @ TO (homology2,ZZ,DGAlgebra) @.
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
      complexA = toComplex(A)
      complexA.dd
      ranks = apply(4, i -> numgens prune HH_i(complexA))
      ranks == apply(4, i -> numgens prune HH_i(koszul gens I))
    Text
      One can also compute the homology of A directly with @ TO (homology2,ZZ,DGAlgebra) @.
///

doc ///
  Key
    (homology2,ZZ,DGAlgebra)
  Headline
    Computes the homology of a DG Algebra
  Usage
    H = homology2(n,A)
  Inputs
    n:ZZ
    A:DGAlgebra 
  Outputs
    H:Module
      The nth homology of A.
///

end

uninstallPackage "DGAlgebras"
restart
installPackage "DGAlgebras"
-- how to test a package
check "DGAlgebras"
viewHelp DGAlgebras

--Tutorial
-- Koszul Complex and homology algebras
restart
loadPackage "DGAlgebras"
R1 = ZZ/32003[x,y,z]
A1 = koszulComplexDGA(R1)
apply(4,i -> polyDifferential(A1,i))
HA1 = homologyAlgebra(A1)
describe oo
peek HA1.cache
R2 = R1/ideal{x^3,y^4,z^5}
A2 = koszulComplexDGA(R2)
HA2 = homologyAlgebra(A2)
describe oo
peek HA2.cache
reduceHilbert hilbertSeries HA2
apply(4,i -> numgens prune HH_i(koszul vars R2))
use R1
R3 = R1/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
A3 = koszulComplexDGA(R3)
HA3 = homologyAlgebra(A3)
describe oo
peek HA3.cache
reduceHilbert hilbertSeries HA3
apply(4,i -> numgens prune HH_i(koszul vars R3))

restart
loadPackage "DGAlgebras"
Q = ZZ/101[x,y,z]
I = ideal{y^3,z*x^2,y*(z^2+y*x),z^3+2*x*y*z,x*(z^2+y*x),z*y^2,x^3,z*(z^2+2*x*y)}
R = Q/I
dim R
ann ideal vars R
A = koszulComplexDGA(R)
HA = homologyAlgebra(A)
-- should check HA by hand since the homology algebra is still monomial.
reduceHilbert hilbertSeries HA
koszulR = koszul vars R
apply(4,i -> numgens prune HH_i(koszulR))
ann ideal vars HA

-- more complicated example
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
HA2 = homologyAlgebra(A2)
-- should check HA by hand since the homology algebra is still monomial.
reduceHilbert hilbertSeries HA2
koszulR2 = koszul vars R2
apply(4,i -> numgens prune HH_i(koszulR2))
ann ideal vars HA2

-- need to check this one (somehow!) it seems the multiplication on HA is trivial
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
HA = homologyAlgebra(A)
-- should check HA by hand since the homology algebra is still monomial.
reduceHilbert hilbertSeries HA
ann ideal vars HA
koszulR = koszul vars R
apply(4,i -> numgens prune HH_i(koszulR))

-- fiber product example
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^4,a*x,a*y,b*x,b*y}
apply((numgens R) + 1, i -> numgens prune HH_i(koszul vars R))
A = koszulComplexDGA(R)
-- 3.1 seconds on mac mini
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
ann ideal vars HB
-- now we trivially extend by a graded vector space, as well as its dual to get a new PD algebra, the
-- Koszul homology algebra of a connected sum (computed below)
reduceHilbert hilbertSeries HA
reduceHilbert hilbertSeries HB
peek HA.cache

-- connected sum example
-- goal: get this example to run quickly
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^4,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^3}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
-- 8.3 seconds on mbp 
time HA = homologyAlgebra(A)
socHA = ideal getBasis(4,HA)
HA.cache.cycles
reduceHilbert hilbertSeries HA
socHA = ideal getBasis(4,HA)
ann ideal vars HA
peek HA.cache

-- connected sum example
-- goal: get this example to run quickly
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
-- 2.3 seconds on mbp, with graded differentials
time HA = homologyAlgebra(A)
socHA = ideal getBasis(4,HA)
HA.cache.cycles
tally degrees HA
reduceHilbert hilbertSeries HA
socHA = ideal getBasis(4,HA)
ann ideal vars HA
peek HA.cache

-- connected sum example
-- goal: get this example to run quickly
restart
loadPackage "DGAlgebras"
R2 = ZZ/32003[a,b,x,y,z]/ideal{a^4,b^4,x^3,y^3,z^3,a*x,a*y,a*z,b*x,b*y,b*z,a^3*b^3-x^2*y^2*z^2}
A2 = koszulComplexDGA(R2)
time apply(6, i -> numgens prune homology2(i,A2))
koszulR2 = koszul vars R2
time apply(6,i -> numgens prune HH_i(koszulR2))
-- ~48 seconds on mbp
time HA2 = homologyAlgebra(A2)
tally degrees HA2
-- this won't finish?
reduceHilbert hilbertSeries HA2

-- connected sum example
-- goal: get this example to finish.
restart
loadPackage "DGAlgebras"
R2 = ZZ/32003[a,b,c,x,y,z]/ideal{a^3,b^3,c^3,x^3,y^3,z^3,a*x,a*y,a*z,b*x,b*y,b*z,c*x,c*y,c*z,a^2*b^2*c^2-x^2*y^2*z^2}
A2 = koszulComplexDGA(R2)
time apply(7, i -> numgens prune homology2(i,A2))
koszulR2 = koszul vars R2
time apply(7,i -> numgens prune HH_i(koszulR2))
HA2 = homologyAlgebra(A2)
reduceHilbert hilbertSeries HA2

-- Tate resolution, toComplex
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R3 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
A3 = acyclicClosure(R3,1)
time A3dd = toComplex(50,A3);
time kRes = res(coker vars R3, LengthLimit => 50);

-- Homology
restart
loadPackage "DGAlgebras"
R3 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
A3 = acyclicClosure(R3,1)
time apply(7, i -> time numgens prune homology2(i,A3))
time kRes = res(coker vars R3, LengthLimit=> 18)
time apply(17, i -> time HH_i(kRes));

-- Tor algebras
restart
loadPackage "DGAlgebras"
R3 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
TorR3 = torAlgebra(R3)
apply(16, i -> hilbertFunction(i,TorR3))
res(coker vars R3, LengthLimit => 15)
R4 = QQ[x,y,z]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
TorR4 = torAlgebra(R4,8)
apply(8, i -> hilbertFunction(i,TorR4))
res(coker vars R4, LengthLimit => 9)
TorR3R4 = torAlgebra(R3,R4,4,10)
reduceHilbert hilbertSeries TorR3R4
use R3
R4mod = coker matrix {{x^2*y^3*z^4}}
res(R4mod, LengthLimit => 6)

-- Acyclic closures
restart
loadPackage "DGAlgebras"
R3 = ZZ/32003[x,y]/ideal{x^3,y^4,x^2*y^3}
A3 = acyclicClosure(R3,3)
time apply(10, i -> time prune homology2(i,A3));
-- need to speed up the prune somehow.  Use the grading of the underlying ring?
HA3 = homologyAlgebra(A3,5,15)

R = ZZ/101[x,y,z]/ideal{x^2,y^3+z^3}
S = R[a,b,c]
flattenRing S

--- George example (not correct on the homology algebra part)
restart
loadPackage "DGAlgebras"
R = QQ[x_11,x_12,x_21,x_22,y_11,y_12,y_21,y_22]
A = dgAlgebra(R,{1,1,1})
setDiff(A,{x_12*y_21 - x_21*y_12,
	   x_21*y_11+x_22*y_21-x_11*y_21-x_21*y_22,
	   x_11*y_12+x_12*y_22-x_12*y_11-x_22*y_12})
homList = apply(5,i -> prune homology2(i,A))
homList_0
homologyAlgebra(A)
