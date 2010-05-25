-- -*- coding: utf-8 -*-
newPackage("DGAlgebras",
     Authors => {
	  {Name => "Frank Moore"}
	  },
     DebuggingMode => true,
     Headline => "Data type for DG algebras",
     Version => "0.61"
     )

export {DGAlgebra, dgAlgebra, setDiff, natural, cycles,
        getBasis, toComplex, koszulComplexDGA, acyclicClosure,
	killCycles, adjoinVariables, homology2, 
        homologyAlgebra, torAlgebra, maxDegree, StartDegree, EndDegree}

-- Still to document:
-- acyclicClosure(DGAlgebra,ZZ), acyclicClosure(Ring,ZZ), killCycles, killCycles(DGAlgebra, List), homologyAlgebra,
-- homologyAlgebra(DGAlgebra), homologyAlgebra(DGAlgebra, ZZ, ZZ), torAlgebra, torAlgebra(Ring), torAlgebra(Ring, ZZ),
-- torAlgebra(Ring,Ring,ZZ,ZZ), 

-- Other things to do:
-- Get HH_i(DGA) to work, as well as HH(DGA) return an algebra rather than a graded module
-- maxDegree of a DGA
-- taylorResolutionDGA
-- ekResolutionDGA - I don't think I can do this one yet since the underlying algebra is not a polynomial algebra
-- trivial Massey operations? strongGolod?
-- deviations
-- isHATrivial
-- Document the code
-- Help file entries (see above!)

-----------------------------------------------------
-- Set DG algebra types and constructor functions. -- 
-----------------------------------------------------

DGAlgebra = new Type of MutableHashTable

-- this is the user friendly version of the DGAlgebra 'constructor'.  In the code below, we use makeDGAlgebra.
-- may change this in the future if I can figure out how to determine if a given variable represents a list
-- of integers or not
dgAlgebra = method()
dgAlgebra(Ring,List) := (R,degList) -> makeDGAlgebra(R, degList)

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
        -- make sure the degree list has the right form.
	if #(first degList) != #(first degrees A.ring) + 1 then degList = apply(degList, i -> i | {0});
	A#(symbol natural) = (A.ring)[varsList, Degrees => degList, Join => false, SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
     )
     else
     (
	A#(symbol natural) = (A.ring)[varsList, Degrees => degList, SkewCommutative => select(toList(0..(#degList-1)), i -> odd first degList#i)];
     );
     A#(symbol isHomogeneous) = false;
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

setDiff = method()
setDiff(DGAlgebra,List) := (A,diffList) -> (
   A.diff = map(A.natural,A.natural, substitute(matrix {diffList}, A.natural));
   A.isHomogeneous = isHomogeneous A.ring and checkIsHomogeneous(A);
   A.diff
)

checkIsHomogeneous = method()
checkIsHomogeneous(DGAlgebra) := (A) -> (
   gensList := gens A.natural;
   diffList := apply(gensList, f -> A.diff(f));
   homDegreeShift := {1} | (toList ((#(degree first gensList)-1):0));
   fold(apply(#diffList, i -> degree gensList#i - homDegreeShift == degree diffList#i),  (i,j) -> i and j)
)

getBasis = method(Options => {Limit => -1})
getBasis(ZZ,DGAlgebra) := opts -> (homDegree,A) -> getBasis(homDegree,A.natural, Limit => opts.Limit)

getBasis(ZZ,Ring) := opts -> (homDegree,R) -> (
   local retVal;
   myMap := map(R, R.cache.basisAlgebra);
   tempList := (flatten entries basis(homDegree, R.cache.basisAlgebra, Limit => opts.Limit)) / myMap;
   if (tempList == {}) then retVal = map((R)^1,(R)^0, 0) else
   (
      degList := apply(tempList, m -> -degree m);
      retVal = map(R^1, R^degList, matrix {tempList});
   );
   retVal
)

isHomogeneous(DGAlgebra) := (A) -> A.isHomogeneous

maxDegree = method()
maxDegree(DGAlgebra) := (A) -> (
  degreesList := degrees A.natural / first;
  if (any(degreesList, i -> even i)) then infinity else sum degreesList
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
-- ask mike about ordering the basis using the specified term order in basis
debug DGAlgebras
R = ZZ/101[a,b,c]
I = ideal{a^3,b^3,c^3,a^2*b^2*c^2}
A = koszulComplexDGA(I)
getBasis(2,A.natural)
Add = toComplex(A)
Add.dd
(koszul gens I).dd
///

taylorResolutionDGA = method()
taylorResolutionDGA(MonomialIdeal) := (I) -> (
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
-- trying to fix the dgAlgebra definition problem
-- test 1
R = ZZ/101[x,y,z]
A1 = dgAlgebra(R,{{1},{1},{1},{3}})
setDiff(A1,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
A1.isHomogeneous
A1dd = toComplex(A1)
A1dd.dd
-- test 2
A2 = dgAlgebra(R,{{1,1},{1,1},{1,1},{3,3}})
setDiff(A2,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
A2.isHomogeneous
A2dd = toComplex(A2)
A2dd.dd
-- test 3
B1 = koszulComplexDGA(R)
B1.isHomogeneous
B1dd = toComplex(B1)
B1dd.dd
-- test 4
R = ZZ/101[x,y,z]
R2 = R/ideal {x^2-z^3}
B2 = koszulComplexDGA(R2)
B2.isHomogeneous
B2dd = toComplex(B2)
B2dd.dd
-- end tests

debug DGAlgebras
R = QQ[x,y,z]
B = koszulComplexDGA(R)
toComplex(B)
degrees B.natural
A = dgAlgebra(R,{{1},{1},{1},{3}})
setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
Add = toComplex(A)
prune HH(Add)
-- add HH(DGA)
-- prune HH(A)
-- add maxDegree
--assert(apply(maxDegree(A)+1, i -> prune HH_i(Add)) == {coker vars R,0,0,coker vars R,0,0,0})
assert(apply(7, i -> prune HH_i(Add)) == {coker vars R,0,0,coker vars R,0,0,0})
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
     homologyGenerators := entries transpose gens image (nthHomology.cache.pruningMap);
     basisList := flatten entries getBasis(n,A);
     cycleList := apply(homologyGenerators, gen -> sum apply(#gen, i -> gen#i*basisList#i));
     retVal = adjoinVariables(A,cycleList);
  );
  retVal
)

adjoinVariables = method()
adjoinVariables(DGAlgebra, List) := (A,cycleList) -> (
  -- this function will add a new variable to make the elements of cycles boundaries in a new DG algebra (semifree over the input)
  local newDegreesList;
  tempDegree := {1} | toList ((#(degree first cycleList)-1):0);
  if isHomogeneous(A) then
    newDegreesList = A.Degrees | apply(cycleList, z -> degree z + tempDegree)
  else
    newDegreesList = A.Degrees | apply(cycleList, z -> {first degree z + 1});
  B := makeDGAlgebra(A.ring,newDegreesList);
  newDiffList := apply(take(flatten entries matrix A.diff, numgens A.natural) | cycleList, f -> substitute(f, B.natural));
  setDiff(B,newDiffList);
  B
)

acyclicClosure = method(Options => {StartDegree => 1})
acyclicClosure(Ring,ZZ) := opts -> (R, homologicalDegreeLimit) -> (
  K := koszulComplexDGA(R);
  acyclicClosure(K,homologicalDegreeLimit, StartDegree=>opts.StartDegree)
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
     sourceList := getBasis(n,A);
     sourceDegreeList := apply(degrees source sourceList, l -> -drop(l,1));
     sourceList = flatten entries sourceList;
     targetList := getBasis(n-1,A);
     targetDegreeList := apply(degrees source targetList, l -> -drop(l,1));
     targetList = flatten entries targetList;
     diffList := matrix {apply(sourceList, m -> polyDiffMonomial(A,m))};
     coeffMatrix := substitute((coefficients(diffList, Monomials => targetList))#1, A.ring);
     newDiffl := map((A.ring)^(targetDegreeList), (A.ring)^(sourceDegreeList), coeffMatrix);
     A.cache.differentials#n = newDiffl;
     newDiffl
  )
)

TEST ///
-- test polyDifferential here.
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
debug DGAlgebras
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
hh2 = prune HH_2(koszulR)
-- make an assertion here.
hh2' = prune HH_2(toComplex(A))
assert(hh2 == hh2')
///

-- note that this does not work for some reason (Dan explained it to me at one point but I can't remember.  I think it has
-- something to do with the fact that in the M2 scripting language, homology(sequence) hijacks all possible calls to homology.
-- homology(ZZ,DGAlgebra) := (n,A) -> polyHomology(n,A)

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
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R3 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
use R3
M = coker matrix {{x^2*y^3*z^4}}
Mres = res(M, LengthLimit => 7)
R4 = QQ[x,y,z]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
time TorR3R4 = torAlgebra(R3,R4,7,21)
-- or course, the multiplication is trivial, since the map R3 --> R4 is Golod
numgens TorR3R4
numgens ideal TorR3R4
apply(21, i -> #(flatten entries getBasis(i,TorR3R4)))
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
  if (isHomogeneous A) then degreesList = (cycleList / degree) else degreesList = pack((cycleList / degree) / first, 1);
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
     --- set up the cached algebra for basis computations too
     myMap' := map(polyRing', polyRing, gens polyRing');
     I' = myMap'(I);
     if myMap'(leadTerm(gens I)) - leadTerm gens I' != 0 then error "Monomial order error.";
     forceGB gens I';
     HA.cache = new CacheTable;
     HA.cache#basisAlgebra = polyRing'/I';
  );
  HA
)

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
  if (not isHomogeneous A) then degList = pack(degList / first, 1);
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
debug DGAlgebras
R1 = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
R2 = ZZ/32003[a,b,x,y,Degrees=>{1,1,2,2}]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
A1 = koszulComplexDGA(R1)
A2 = koszulComplexDGA(R2)
cycleList1 = getGenerators(A1,4)
cycleList2 = getGenerators(A2,4)
HAEasy1 = findEasyRelations(A1,cycleList1)
HAEasy2 = findEasyRelations(A2,cycleList2)
myList1 = {({4,8},1),({3,4},1),({3,5},1),({3,6},1),({3,7},1),({2,3},1),({2,4},1),({2,5},1),({2,6},1),({1,2},1),({1,3},1),({1,4},1),({0,0},1)}
myList2 = {({0},1),({1},1),({2},1),({3},1),({4},1)}
assert(pairs(tally (flatten entries basis HAEasy1) / degree) == myList1)
assert(pairs(tally (flatten entries basis HAEasy2) / degree) == myList2)
///

getCycleProductMatrix = method()
getCycleProductMatrix(DGAlgebra,Ring,List,ZZ) := (A,HA,cycleList,N) -> (
  -- the input is the dga A, the homology algebra HA (so far), the list of cycle generators, and the degree.
  -- this version does use the knowledge of the homologyAlgebra so far to return the cycles products in a given degree.
  local retVal;
  local myMap;
  monListHA := flatten entries getBasis(N,HA);
  monListA := flatten entries getBasis(N,A);
  myMap = map(A.natural,HA,cycleList);
  -- this is needed when there is nothing in HA in this degree so far.
  if (monListHA != {}) then
  (
     (junk,cycleProductMatrix) := coefficients(myMap(matrix{monListHA}),Monomials=>monListA);
     -- make sure the degrees are correct if the ring is homogeneous
     if (isHomogeneous A) then
     (
        sourceDegreeList := apply((monListHA / degree), l -> -drop(l,1));
        targetDegreeList := apply((monListA / degree), l -> -drop(l,1));
        cycleProductMatrix = map((A.ring)^targetDegreeList, (A.ring)^sourceDegreeList, sub(cycleProductMatrix, A.ring));
     )
     else
     (
        myMap = map(A.ring, A.natural);
        cycleProductMatrix = map((A.ring)^(#monListA), (A.ring)^(#monListHA), sub(cycleProductMatrix, A.ring));
     );
     retVal = (cycleProductMatrix,monListHA);
  )
  else retVal = (map((A.ring)^(#monListA), (A.ring)^0,0), {});
  retVal
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
	(cycleProductMatrix,monListHA) := getCycleProductMatrix(A,oldCycleList,N);
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
debug DGAlgebras
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
time apply(5,i -> numgens prune homology2(i,A))
-- ~1.6 seconds on mbp, with graded differentials
time HA = homologyAlgebra(A)
assert(numgens HA == 34)
assert(numgens ideal HA == 581)
assert(#(support numerator reduceHilbert hilbertSeries HA) == 2)
-- same example, but not graded because of the degree change.  The homologyAlgebra function
-- will then only return a graded algebra
R2 = ZZ/32003[a,b,x,y,Degrees=>{1,1,2,2}]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR2 = koszul vars R2
time apply(5,i -> numgens prune HH_i(koszulR2))
A2 = koszulComplexDGA(R2)
time apply(5,i -> numgens prune homology2(i,A2))
-- ~2.3 seconds on mbp, with ungraded differentials
time HA2 = homologyAlgebra(A2)
assert(numgens HA2 == 34)
assert(numgens ideal HA2 == 581)
-- should only be singly graded
assert(#(support numerator reduceHilbert hilbertSeries HA2) == 1)
///

TEST ///
--- Homology algebra for the Koszul complex on a set of generators of an ideal
--- should try to get this to work.
--restart
--loadPackage "DGAlgebras"
--R = ZZ/32003[a,b,x,y]
--I = ideal{a^3,b^3,x^3,y^3}
--A = koszulComplexDGA(I)
-- incorrect at the moment
--homologyAlgebra(A)
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
     rankOfAlgebraSoFar := #(flatten entries getBasis(N,HA));
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
	     retVal = apply(kernelGens, z -> sum apply(#z, i -> (monListHA#i)*(z#i)));
          );
       );
     );
  )
  else (
     retVal = flatten entries getBasis(N,HA); 
  );
  retVal
)

getGreaterMonomials:= (R,N) -> (
  maxDegree := max (degrees R / first);
  flatten apply(maxDegree, i -> flatten entries getBasis(N+i,R))
)

getGenerators = method()
getGenerators(DGAlgebra,ZZ) := (A,genDegreeLimit) -> (
  n := 1;
  cycleList := {};
  while (n <= genDegreeLimit) do (
     << "Computing generators in degree " << n << " : ";
     time newCycleList := findDegNGenerators(A,cycleList,n);
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
      << "Computing relations in degree " << n << "  : ";
      time newRelList := findDegNRelations(A,HA,cycleList,n);
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
  cycleList = getGenerators(A,genDegreeLimit);

  if (cycleList == {}) then (
     -- put the cycles that the variables represent in the cache.
     HA = coefficientRing A.ring;
     HA.cache = new CacheTable;
     HA.cache#cycles = cycleList;
     A.cache#homologyAlgebra = HA;
  )
  else (
     << "Finding easy relations           : ";
     time HA = findEasyRelations(A,cycleList);
     HA = getRelations(A,HA,cycleList,relDegreeLimit);
  );
  HA
)

homologyAlgebra(DGAlgebra) := (A) -> (
  -- this is a routine that will compute the complete homology algebra
  -- if the DG algebra is known to be finite rank free module over the base ring.
  cycleList := {};
  relList := {};
  n := 1;
  local HA;
  local myMap;
  
  -------------------------------------------
  mDegree := maxDegree(A);
  if (mDegree == infinity) then error "Must supply upper degree bound on generators and relations if there is a DG algebra generator of even degree.";
  
  n = mDegree;
  while (n <= mDegree and prune homology2(n,A) == 0) do n = n - 1;
  maxHomologyDegree := n;
  -------------------------------------------
  
  HA = homologyAlgebra(A,mDegree,maxHomologyDegree);
  relList = (ideal HA)_*;
  cycleList = HA.cache.cycles;
  
  HA
)

DGAlgebra ** Ring := (A,S) -> (
  B := makeDGAlgebra(S, A.Degrees);
  newDiff := apply(flatten entries matrix (A.diff), f -> substitute(f,B.natural));
  setDiff(B,newDiff);
  B
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
      * @ TO (symbol **, DGAlgebra, Ring) @

///

--      * @ TO (symbol **, DGAlgebra, DGAlgebra) @

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
      polynomial ring in a number of variables equal to the number of the degrees input.  The current version of this package
      does not handle algebras A whose underlying algebra is not a polynomial ring.
    Example
      R = ZZ/101[x,y,z]
      A = dgAlgebra(R,{{1},{1},{1},{3}})
      A.natural
      setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
    Text
      The resulting DGA will not be graded since the differential given does not respect the grading due to the degrees assigned in the definition.
    Example
      isHomogeneous(A)
      Add = toComplex(A)
      B = dgAlgebra(R,{{1,1},{1,1},{1,1},{3,3}})
      B.natural
      setDiff(B,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
    Text
      The result of the above declaration will be graded.
    Example
      isHomogeneous(B)
      Bdd = toComplex(B)
    Text  
      Note that the differential is not passed into the constructor.  The reason for this (at the moment)
      is that Macaulay2 does not know what ring the differentials are defined over until after the underlying
      algebra is constructed, so the differential is set later with setDiff.  Many DG algebras that one
      encounters in commutative algebra have been implemented, however, and do not need to be defined 'by hand'.
      For example, if one wants to work with the Koszul complex as a DG algebra, then one should see the command @ TO koszulComplexDGA @.
     
      There is currently a bug handling DG algebras that have no monomials in some degree, but some monomials in a later degree;
      for example if one replaces the 3 in the above example with a 5.
///

doc ///
  Key
    koszulComplexDGA
  Headline
    Returns the Koszul complex as a DGAlgebra
  Usage
    A = koszulComplexDGA(R) or A = koszulComplexDGA(I)
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
    homology2
  Headline
    Computes the homology of a DG algebra as a module.
  Usage
    H = homology2(n,A)
///

doc ///
  Key
    (homology2,ZZ,DGAlgebra)
  Headline
    Computes the homology of a DG algebra as a module
  Usage
    H = homology2(n,A)
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
      apply(numgens R+1, i -> numgens prune homology2(i,A))
///

doc ///
  Key
    setDiff
  Headline
    Sets the differential of a DGAlgebra manually.
  Usage
    d = setDiff(A,diffList)
///

doc ///
  Key
    (setDiff,DGAlgebra,List)
  Headline
    Sets the differential of a DGAlgebra manually.
  Usage
    d = setDiff(A,diffList)
  Inputs
    A:DGAlgebra
    A:List 
  Outputs
    d:RingMap
      The differential of A.
  Description
    Example
      R = ZZ/101[x,y,z]
      A = dgAlgebra(R,{{1},{1},{1},{3}})
      setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
      Add = toComplex(A)
      Add.dd
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
      A = dgAlgebra(R,{{1},{1},{1},{3}})
      setDiff(A,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
      isHomogeneous(A)
      B = dgAlgebra(R,{{1,1},{1,1},{1,1},{3,3}})
      setDiff(B,{x,y,z,x*T_2*T_3-y*T_1*T_3+z*T_1*T_2})
      isHomogeneous(B)
///

doc ///
  Key
    natural
  Headline
    The underlying algebra of a DG algebra.
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
    Cycles chosen when computing the homology algebra of a DG algebra
  Usage
    A.cycles
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^4,c^5,d^6}
      A = koszulComplexDGA(R)
      apply(maxDegree A + 1, i -> numgens prune homology2(i,A))
      HA = homologyAlgebra(A)
      numgens HA
      HA.cache.cycles
    Text
///

doc ///
  Key
    getBasis
  Headline
    Get a basis for a particular homological degree of a DG algebra, or a multigraded ring.
  Usage
    getBasis(n,A) or getBasis(n,R)
///

doc ///
  Key
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
      This function is to allow for the retrieval of a basis of a particular homological degree of a DG algebra
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
  Headline
    Converts a DG Algebra to a ChainComplex
  Usage
    C = toComplex(A)
///

doc ///
  Key
    (toComplex,DGAlgebra)
  Headline
    Converts a DGAlgebra to a ChainComplex
  Usage
    C = toComplex(A)
  Inputs
    A:DGAlgebra
  Outputs
    C:ChainComplex
      The DG algebra A as a ChainComplex
  Description
    Example
      R = ZZ/101[x_1..x_10]
      A = koszulComplexDGA(R)
      C = toComplex(A)
    Text
      Warning:  The term order that the internal command koszul uses to order the monomials is not GRevLex, and so the differentials
      used in koszul and koszulComplexDGA will not match up exactly.  Also, this command will only execute if all of the variables
      of the DGAlgebra A are of odd homological degree.  Otherwise, you need to specify a homological upper bound, see
      @ TO (toComplex, ZZ, DGAlgebra) @.
///

doc ///
  Key
    (toComplex,ZZ,DGAlgebra)
  Headline
    Converts a DGAlgebra to a ChainComplex, up to a specified homological degree.
  Usage
    C = toComplex(n,A)
  Inputs
    n:ZZ
    A:DGAlgebra
  Outputs
    C:ChainComplex
      The DGAlgebra A as a ChainComplex, up to homological degree n.
  Description
    Text
      If your DGAlgebra has any generators in even homological degree, then one must
      use this version of the function, rather than @ TO (toComplex,DGAlgebra) @
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
      A = acyclicClosure(R,3)
    Text
      The above will be a resolution of the residue field over R, since R is a complete intersection.
    Example
      C = toComplex(10,A)
      apply(10, i -> prune HH_i(C))
///

doc ///
  Key
    acyclicClosure
  Headline
    Compute the acyclic closure of a DGAlgebra.
  Usage
    B = acyclicClosure(A,n)
///

doc ///
  Key
    (acyclicClosure,DGAlgebra,ZZ)
  Headline
    Compute theae acyclic closure of a DGAlgebra.
  Usage
    B = acyclicClosure(A,n)
  Inputs
    A:DGAlgebra
    n:ZZ
  Outputs
    B:DGAlgebra
      The acyclic closure of the DG Algebra A up to homological degree n.
  Description
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      A = koszulComplexDGA(R);
      B = acyclicClosure(A,3)
      toComplex(8,B)
      B.diff
///

doc ///
  Key
    (acyclicClosure,Ring,ZZ)
  Headline
    Compute the acyclic closure of the residue field of a ring up to a certain degree
  Usage
    A = acyclicClosure(R,3)
  Inputs
    R:Ring
    n:ZZ
  Outputs
    A:DGAlgebra
  Description
    Text
      This package always chooses the Koszul complex on a generating set for the maximal ideal as a starting
      point, and then computes from there, using the function @ TO (acyclicClosure,DGAlgebra,ZZ) @.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^4-d^3}
      A = acyclicClosure(R,3)
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
      toComplex(B)
///

doc ///
  Key
    killCycles
  Headline
    Adjoins variables to make non-bounding cycles boundaries in the lowest positive degree with nontrivial homology.
  Usage
    B = killCycles(A)
///

doc ///
  Key
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
  Headline
    Adjoins variables to kill specified cycles.
  Usage
    B = adjoinVariables(A,cycleList)
///

doc ///
  Key
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
      prune homology2(1,A)
      B = adjoinVariables(A,{a^2*T_1})
      B.diff
      prune homology2(1,B)
///

doc ///
  Key
    homologyAlgebra
  Headline
    Compute the homology algebra of a DGAlgebra.
  Usage
    HA = homologyAlgebra(A)
///

doc ///
  Key
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
      apply(maxDegree A + 1, i -> numgens prune homology2(i,A))
      HA = homologyAlgebra(A)
    Text
      Note that HA is a graded commutative polynomial ring (i.e. an exterior algebra) since R is a complete intersection.
    Example  
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4,a^3*b^3*c^3*d^3}
      A = koszulComplexDGA(R)
      apply(maxDegree A + 1, i -> numgens prune homology2(i,A))
      HA = homologyAlgebra(A)
      numgens HA
      HA.cache.cycles
    Example
      Q = ZZ/101[x,y,z]
      I = ideal{y^3,z*x^2,y*(z^2+y*x),z^3+2*x*y*z,x*(z^2+y*x),z*y^2,x^3,z*(z^2+2*x*y)}
      R = Q/I
      A = koszulComplexDGA(R)
      apply(maxDegree A + 1, i -> numgens prune homology2(i,A))
      HA = homologyAlgebra(A)
    Text
      One can check that HA has Poincare duality since R is Gorenstein.
///

doc ///
  Key
    (homologyAlgebra,DGAlgebra,ZZ,ZZ)
  Headline
    Compute the homology algebra of a DGAlgebra A up to certain generating degree and relation degree
  Usage
    HA = homologyAlgebra(A,genDegree,relDegree)
  Inputs
    A:DGAlgebra
    genDegree:ZZ
    relDegree:ZZ
  Outputs
    HA:Ring
  Description
    Example
      R = ZZ/101[a,b,c,d]
      S = R/ideal{a^4,b^4,c^4,d^4}
      A = acyclicClosure(R,3)
      B = A ** S
      HB = homologyAlgebra(B,7,14)
///

doc ///
  Key
    torAlgebra
  Headline
    Computes the Tor algebra of a surjection R --> S or of a ring R.
  Usage
    torAlgebra(R) or torAlgebra(R,S,7,14)
///

doc ///
  Key
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
///

doc ///
  Key
    (torAlgebra,Ring,ZZ)
  Headline
    Compute the Tor algebra of a ring up to a specified degree
  Usage
    TorR = torAlgebra(R,n)
  Inputs
    R:Ring
    n:ZZ
  Outputs
    TorR:Ring
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3,a^2*b^2*c^3*d^2}
      TorR = torAlgebra(R,5)
///

doc ///
  Key
    (torAlgebra,Ring,Ring,ZZ,ZZ)
  Headline
    Computes Tor_R(S,k) up to a specified generating and relating degree.
  Usage
    TorRS = torAlgebra(R,S,genDegree,relDegree)
  Inputs
    R:Ring
    S:Ring
    genDegree:ZZ
    relDegree:ZZ
  Outputs
    TorRS:Ring
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      M = coker matrix {{a^3*b^3*c^3*d^3}};
      S = R/ideal{a^3*b^3*c^3*d^3}
      HB = torAlgebra(R,S,4,8)
      numgens HB
      apply(5,i -> #(flatten entries getBasis(i,HB)))      
      Mres = res(M, LengthLimit => 8)
    Text
      Note that the Tor algebra has trivial multiplication, since the map from R to S is a Golod homomorphism by a theorem of Levin and Avramov.
///

doc ///
  Key
    maxDegree
  Headline
    Computes the maximum homological degree of a DGAlgebra
  Usage
    maxDegree(A)
///

doc ///
  Key
    (maxDegree,DGAlgebra)
  Headline
    Computes the maximum homological degree of a DGAlgebra
  Usage
    mDegree = maxDegree(A)
  Inputs
    A:DGAlgebra
  Outputs
    mDegree:ZZ
      the maximum degree of the DGAlgebra A.
  Description
    Text
      Note that if the DGAlgebra A has any generators of even degree, then maxDegree returns infinity.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
      A = koszulComplexDGA(R)
      B = acyclicClosure(A,3)
      maxDegree(A)
      maxDegree(B)
///

doc ///
  Key
    StartDegree
  Headline
    option to specify the degree to start computing the acyclic closure and killing cycles
  Usage
    acyclicClosure(...,StartDegree=>n) or killCycles(...,StartDegree=>n)
///

doc ///
  Key
    [acyclicClosure,StartDegree]
  Headline
    option to specify the degree to start computing the acyclic closure
  Usage
    acyclicClosure(...,StartDegree=>n)
///

doc ///
  Key
    [killCycles,StartDegree]
  Headline
    option to specify the degree to start looking for cycles
  Usage
    killCycles(...,StartDegree=>n)
///

doc ///
  Key
    [killCycles,EndDegree]
  Headline
    option to specify the degree to stop looking for cycles
  Usage
    killCycles(...,EndDegree=>n)
///

doc ///
  Key
    EndDegree
  Headline
    option to specify the degree to stop computing killing cycles
  Usage
    killCycles(...,StartDegree=>n)
///

end

uninstallPackage "DGAlgebras"
restart
installPackage "DGAlgebras"
check "DGAlgebras"
viewHelp DGAlgebras

--Tutorial (Include in a separate file?)
-- Koszul Complex and homology algebras
restart
loadPackage "DGAlgebras"
R1 = ZZ/32003[x,y,z]
A1 = koszulComplexDGA(R1)
apply(4,i -> polyDifferential(A1,i))
time HA1 = homologyAlgebra(A1)
describe HA1
peek HA1.cache
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
-- should check HA by hand since the homology algebra is still monomial.
reduceHilbert hilbertSeries HA

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
time HA2 = homologyAlgebra(A2)
-- should check HA by hand since the homology algebra is still monomial.
reduceHilbert hilbertSeries HA2

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
time HA = homologyAlgebra(A)
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
-- .86 seconds on mbp
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
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^4,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^3}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
-- 3.2 seconds on mbp 
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
-- 1.68 seconds on mbp, with graded differentials
time HA = homologyAlgebra(A)
reduceHilbert hilbertSeries HA

-- connected sum example
-- goal: get this example to run quickly
restart
loadPackage "DGAlgebras"
R2 = ZZ/32003[a,b,x,y,z]/ideal{a^4,b^4,x^3,y^3,z^3,a*x,a*y,a*z,b*x,b*y,b*z,a^3*b^3-x^2*y^2*z^2}
A2 = koszulComplexDGA(R2)
time apply(6, i -> numgens prune homology2(i,A2))
koszulR2 = koszul vars R2
time apply(6,i -> numgens prune HH_i(koszulR2))
-- 35.5 seconds on mbp
time HA2 = homologyAlgebra(A2)
numgens HA2
numgens ideal HA2
tally ((flatten entries basis HA2) / degree)
tally (((flatten entries basis HA2) / degree) / first)

-- connected sum example
-- goal: get this example to run quicker?
-- ~1430 seconds
restart
loadPackage "DGAlgebras"
gbTrace = 2
R2 = ZZ/32003[a,b,c,x,y,z]/ideal{a^3,b^3,c^3,x^3,y^3,z^3,a*x,a*y,a*z,b*x,b*y,b*z,c*x,c*y,c*z,a^2*b^2*c^2-x^2*y^2*z^2}
A2 = koszulComplexDGA(R2)
time apply(7, i -> numgens prune homology2(i,A2))
koszulR2 = koszul vars R2
time apply(7,i -> numgens prune HH_i(koszulR2))
time HA2 = homologyAlgebra(A2)
tally ((flatten entries basis HA2) / degree)
tally (((flatten entries basis HA2) / degree) / first)
-- 146 generators and 10662 relations!

-- Tate resolution, toComplex
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = QQ[x,y,z,w]/ideal{x^3,y^4,z^5}
A = acyclicClosure(R,1)
time Add = toComplex(20,A);
time kRes = res(coker vars R, LengthLimit => 20)

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
time TorR3 = torAlgebra(R3)
apply(16, i -> hilbertFunction(i,TorR3))
time res(coker vars R3, LengthLimit => 15)
R4 = QQ[x,y,z]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
TorR4 = torAlgebra(R4,8)
apply(10, i -> hilbertFunction(i,TorR4))
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
time A3 = acyclicClosure(R3,5)
time HA3 = homologyAlgebra(A3,6,12)
time apply(12, i -> #(flatten entries getBasis(i,HA3)))
-- need to check the mult structure from Lucho's book.

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
