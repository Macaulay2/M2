-- -*- coding: utf-8 -*-
newPackage("DGAlgebras",
     Authors => {
	  {Name => "Frank Moore"},
	  {Name => "Others"}
	  },
     DebuggingMode => true,
     Headline => "Data type for DG Algebras",
     Version => "0.1"
     )

export {DGAlgebra, dgAlgebra, setDiff, algebra, toComplex, koszulComplexDGA, acyclicClosure, killCycles, adjoinVariables,
        homologyAlgebra, torAlgebra, polyDifferential, polyHomology}

------------------------------------------------
-- Set DG algebra types and constructor functions. -- 
------------------------------------------------

DGAlgebra = new Type of MutableHashTable

dgAlgebra = method()
dgAlgebra(Ring,List) := (R,degList) -> (
     -- Input:  A ring, a list of degrees of the variables, and a list that defines the differential
     -- Output:  A hash table of type DGAlgebra
     A := new MutableHashTable;
     --A#(symbol ring) = newRing(R, Degrees => apply(degrees R, i -> 0));
     A#(symbol ring) = R;
     varsList := toList (T_1..T_(#degList));
     A#(symbol diff) = {};
     A#(symbol algebra) = (A.ring)[varsList, Degrees => degList, SkewCommutative => select(toList(0..(#degList-1)), i -> odd degList#i)];
     A#(symbol Degrees) = degList;
     A#(symbol cache) = new CacheTable;
     A.cache#(symbol homology) = new MutableHashTable;
     A.cache#(symbol differentials) = new MutableHashTable;
     -- should verify that the differential is indeed of degree -1
     new DGAlgebra from A
)

setDiff = method()
setDiff(DGAlgebra,List) := (A,diffList) -> (
   A.diff = map(A.algebra,A.algebra, substitute(matrix {diffList}, A.algebra));
   A.diff
)

koszulComplexDGA = method()
koszulComplexDGA(Ring) := (R) -> (
     A := dgAlgebra(R, toList ((numgens R):1));
     use A.ring;
     setDiff(A, gens R);
     A
)

koszulComplexDGA(Ideal) := (I) -> (
     A := dgAlgebra(ring I, toList ((numgens I):1));
     use A.ring;
     setDiff(A,I_*);
     A
)

taylorDGA = method()
taylorDGA(MonomialIdeal) := (I) -> (
   A
)

toComplex = method()
toComplex(ZZ,DGAlgebra) := (N,A) -> chainComplex(apply(N, i -> polyDifferential(A,i+1)))

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
  while (n <= endDegree and not foundHomology) do ( nthHomology = prune polyHomology(A,n); if (nthHomology == 0) then n = n + 1 else foundHomology = true);
  -- at this point we have found a degree with nontrivial homology.
  -- we now add variables in one degree higher to make these cycles boundaries.
  if (not foundHomology) then retVal = A else
  (  
     homologyGenerators := entries transpose generators image nthHomology.cache.pruningMap;
     basisList := flatten entries basis({n,0},A.algebra);
     cycleList := apply(homologyGenerators, gen -> sum apply(#gen, i -> gen#i*basisList#i));
     retVal = adjoinVariables(A,cycleList);
  );
  retVal
)

adjoinVariables = method()
adjoinVariables(DGAlgebra, List) := (A,cycleList) -> (
  -- this function will add a new variable to make the elements of cycles boundaries in a new DG algebra (semifree over the input)
  newDegreesList := flatten append(A.Degrees, apply(cycleList, z -> (first degree z) + 1));
  B := dgAlgebra(A.ring,newDegreesList);
  newDiffList := apply(flatten append(take(flatten entries matrix A.diff, numgens A.algebra), cycleList), f -> substitute(f, B.algebra));
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

polyDiffMonomial = method()
polyDiffMonomial(DGAlgebra,RingElement) := (A,m) -> (
  -- uses the Leibniz rule to compute the differential of a traditional monomial
  dgSign := 1;
  monSupport := support m;
  monExponents := select(first exponents m, i -> i > 0);
  monSupportPowers := apply(#monSupport, i -> (monSupport#i)^(monExponents#i));
  firstDiffTerms := apply(#monSupport, i -> product take(monSupportPowers,i));
  lastDiffTerms := apply(#monSupport, i -> product drop(monSupportPowers,i+1));
  --diffCoeffs := apply(monSupport, l -> A.diff(l)*l^(degree(l,m)-1));
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
     sourceList := flatten entries basis({n,0},A.algebra);
     targetList := flatten entries basis({n-1,0},A.algebra);
     diffList := matrix {apply(sourceList, m -> polyDiffMonomial(A,m))};
     coeffMatrix := substitute((coefficients(diffList, Monomials => targetList))#1, A.ring);
     newDiffl := map((A.ring)^(#targetList), (A.ring)^(#sourceList), coeffMatrix);
     A.cache.differentials#n = newDiffl;
     newDiffl
  )
)

polyDifferential(DGAlgebra,RingElement) := (A,f) -> (
  sum apply(terms f, m -> polyDiffMonomial(A,m))
)

polyHomology = method()
polyHomology(DGAlgebra,ZZ) := (A,n) -> (
  dn := 0;
  dnplus1 := 0;
  retVal := 0;
  if (A.cache.homology#?n) then retVal = A.cache.homology#n
  else if (#(flatten entries basis({n,0}, A.algebra, Limit => 1)) != 0) then
  (
     if n == 0 then dn = map((A.ring)^0, (A.ring)^1, 0) else dn = polyDifferential(A,n);
     if (#(flatten entries basis({n+1,0}, A.algebra, Limit => 1)) != 0) then
        dnplus1 = polyDifferential(A,n+1)
     else
        dnplus1 = map(source dn, (A.ring)^0, 0);
     retVal = homology(dn,dnplus1);
     A.cache.homology#n = retVal;
  )
  else
     retVal = R^0;
  retVal
)

torAlgebra = method()
torAlgebra(Ring,ZZ) := (R,n) -> (
  -- can do this much faster without computing acyclic closure, since ACs are minimal.  One need only check bases.
  acycClos := acyclicClosure(R,1);
  secondHomology := polyHomology(acycClos,2);
  if (prune secondHomology != 0) then acycClos = acyclicClosure(acycClos,n);
  (coefficientRing R)[gens (acycClos.algebra), Degrees => acycClos.Degrees, SkewCommutative => select(toList (0..(#acycClos.Degrees - 1)), i -> odd (acycClos.Degrees)#i)]
)

torAlgebra(Ring) := (R) -> torAlgebra(R,2)

torAlgebra(Ring,Ring,ZZ,ZZ) := (R,S,genDegree,relDegree) -> (
  -- S is an R-algebra
  acycClos := acyclicClosure(R,genDegree);
  acycClos' := acycClos ** S;
  homologyAlgebra(acycClos',genDegree,relDegree)
)

representativeCycles = method()
representativeCycles(DGAlgebra,ZZ) := (A,n) -> (
  homologyGenerators := entries transpose generators image (prune polyHomology(A,n)).cache.pruningMap;
  basisList := flatten entries basis({n,0},A.algebra);
  cycleList := apply(homologyGenerators, gen -> sum apply(#gen, i -> gen#i*basisList#i));
  cycleList
)

homologyAlgebra = method()
homologyAlgebra(DGAlgebra,ZZ,ZZ) := (A,genDegreeLimit,relDegreeLimit) -> (
  if (A.cache#?homologyAlgebra) then A.cache#homologyAlgebra
  else (
  cycleList := {};
  relList := {};
  n := 1;
  local HA;
  -- get the generators of the homology algebra
  while (n <= genDegreeLimit) do (cycleList = flatten append(cycleList, findDegNGenerators(A,cycleList,n)); n = n + 1;);
  if (cycleList == {}) then (
     HA = coefficientRing A.ring;
     -- put the cycles that the variables represent in the cache.
     HA.cache = new CacheTable;
     HA.cache#cycles = cycleList;
     A.cache#homologyAlgebra = HA;
  )
  else (
     polyRing := makeCycleRing(A,cycleList);
     n = 2;
     while (n <= relDegreeLimit) do (
        << "Relation degree " << n << endl;
	relList = flatten append(relList,findDegNRelations(A,polyRing,{cycleList,relList},n));
	n = n + 1;
     );
     defIdeal := trim ideal relList;
     HA = polyRing/defIdeal;
     -- put the cycles that the variables represent in the cache.
     HA.cache = new CacheTable;
     HA.cache#cycles = cycleList;
     A.cache#homologyAlgebra = HA;
  );
  HA )
)

homologyAlgebra(DGAlgebra) := (A) -> (
  -- this is a routine that will compute the complete homology algebra
  -- if the DG Algebra is known to be finite rank over the base ring.
  cycleList := {};
  relList := 0;
  n := 1;
  local HA;
  degreesList := degrees A.algebra / first;
  if (any(degreesList, i -> even i)) then error "Must supply upper degree bound on generators and relations if there is a DG Algebra generator of even degree.";
  -- otherwise, all are odd, and we can compute the entire homology algebra
  maxDegree := sum degreesList;
  while (n <= maxDegree) do (cycleList = flatten append(cycleList, findDegNGenerators(A,cycleList,n)); n = n + 1;);
  if (cycleList == {}) then (
     HA = coefficientRing A.ring;
     HA.cache = new CacheTable;
     HA.cache#cycles = cycleList;
     A.cache#homologyAlgebra = HA;
  )
  else (
     -- find the max homological degree with nonzero homology, and add all monomials that are above that degree
     polyRing := makeCycleRing(A,cycleList);
     n = maxDegree;
     while (n <= maxDegree and prune A.cache.homology#n == 0) do n = n - 1;
     maxHomologyDegree := n;
     relList = (trim ideal getGreaterMonomials(polyRing,maxHomologyDegree+1))_*;
     n = 2;
     while (n <= maxHomologyDegree) do (relList = flatten append(relList,findDegNRelations(A,polyRing,{cycleList,relList},n)); n = n + 1;);
     defIdeal := trim ideal relList;
     HA = polyRing/defIdeal;
     -- put the cycles that the variables represent in the cache.
     HA.cache = new CacheTable;
     HA.cache#cycles = cycleList;
     A.cache#homologyAlgebra = HA;
  );
  HA
)

getGreaterMonomials = method()
getGreaterMonomials(Ring,ZZ) := (R,N) -> (
  maxDegree := max (degrees R / first);
  flatten apply(maxDegree, i -> flatten entries basis(N+i,R))
)

makeCycleRing = method()
makeCycleRing(DGAlgebra,List) := (A, cycleList) -> (
  --baseRing := A.ring/(ideal flatten entries polyDifferential(A,1));
  baseRing := coefficientRing A.ring;
  degreesList := apply(cycleList, i -> first degree i);
  varList := toList (X_1..X_(#cycleList));
  baseRing[varList, Degrees => degreesList, SkewCommutative => select(toList(0..(#degreesList-1)), i -> odd degreesList#i)]
)

getCycleProductList = method()
getCycleProductList(DGAlgebra,List,ZZ) := (A,cycleList,N) -> (
  -- this function returns a list containing all monomials of degree N in the cycleList that was input, decomposed using the basis of monomials given by monListA
  HA := makeCycleRing(A,cycleList);
  getCycleProductList(A,HA,cycleList,N)
)

getCycleProductList(DGAlgebra,Ring,List,ZZ) := (A,HA,cycleList,N) -> (
  monListHA := flatten entries basis(N,HA);
  -- need to rewrite this, the lists are too big and building it is too slow.
  expListHA := flatten(monListHA / exponents);
  monListA := flatten entries basis ({N,0},A.algebra);
  cycleProductList := apply(expListHA, xs -> product apply(#xs, i -> (cycleList#i)^(xs#i)));
  (apply(cycleProductList, z -> (coefficients(z, Monomials => monListA))#1),monListHA)
)

findDegNGenerators = method()
findDegNGenerators(DGAlgebra,List,ZZ) := (A,oldCycleList,N) -> (
  -- I am assuming that the oldCycleList contains a (minimal?) set of algebra
  -- generators needed to generate the homology algebra up to degree n-1.
  -- the goal of this function is to return the generators and relations in degree n.
  cycleList := {};
  relsList := {};
  varList := {};
  if (oldCycleList == {}) then (
     -- here, we know all the degree 1 elements are generators
     cycleList = representativeCycles(A,N);
  )
  else if (flatten entries basis({N,0},A.algebra, Limit => 1) == {}) then cycleList = {}
  else (
     -- the below matrix contains all monomials of degree n in the cycleList that was input, put into a matrix using the basis of monomials given by monListA
     nthHomology := polyHomology(A,N);
     if (prune nthHomology != 0)
     then (
        cycleProductList := (getCycleProductList(A,oldCycleList,N))#0;
        if (cycleProductList != {})
	then (
           cycleProductMatrix := substitute(fold(cycleProductList, (i,j) -> i | j), A.ring);
           newHomology := prune (nthHomology / (image map (target gens nthHomology, source cycleProductMatrix, matrix entries cycleProductMatrix)));
           monListA := flatten entries basis ({N,0},A.algebra);
           newGenerators := apply(entries transpose gens image newHomology.cache.pruningMap, zList -> apply(#zList, i -> zList#i*monListA#i)) / sum;
           cycleList = newGenerators;
        )
        else (
	   -- if we are here, then we need to add all of this degree as generators.
	   cycleList = representativeCycles(A,N);
	);
     ); 
  );
  cycleList
)

findDegNRelations = method()
findDegNRelations(DGAlgebra,Ring,List,ZZ) := (A,polyRing,gensAndRels,N) -> (
  -- this function tries to find the relations in degree N that involve the generators in the list algGens
  -- no checking is done to see if algGens are actually minimal generators at this point.
  algGens := gensAndRels#0;
  algRels := gensAndRels#1;
  defIdeal := 0;
  local cycleProductList;
  local monListHA;
  if (algRels != {}) then defIdeal = ideal algRels else defIdeal = ideal 0_polyRing;
  retVal := {0_polyRing};
  -- check if the DGA is zero in this degree. If so, just return back the algRels.
  if ((flatten entries basis({N,0},A.algebra, Limit => 1) != {}) or (#algGens == 0)) then (
     ringSoFar := polyRing/defIdeal;
     nthHomology := polyHomology(A,N);
     -- using algRels, check if there are indeed any new relations in degree n
     pruneNthHomology := prune nthHomology;
     rankOfNthHomology := numgens pruneNthHomology;
     rankOfAlgebraSoFar := hilbertFunction(N,ringSoFar);
     if (rankOfNthHomology != rankOfAlgebraSoFar) then
     (
       -- when in here, we know there is a relation in degree N.
       -- so take each monomial of the correct degree, build the cycle corresponding to that
       -- and define a map from the residue field to the homology class representing each cycle.
       -- then take the kernel, prune, and use cache.pruningMap to get the actual minimal generating
       -- set of the kernel.  Finally, reconstruct the elements from the monomials and viola!
       if (pruneNthHomology == 0) then (
          -- if we are here, all monomials in the polyRing of this degree are zero.
          retVal = flatten entries basis (N,polyRing);
       )
       else (
          (cycleProductList,monListHA) = getCycleProductList(A,polyRing,algGens,N);
          if (cycleProductList != {}) then (
             -- if the homology is zero in this degree, then all monomials are also zero here.
             cycleProductMatrix := substitute(fold(cycleProductList, (i,j) -> i | j), A.ring);
             baseRing := coker vars (A.ring);
             myMatrix := cycleProductMatrix // (gens nthHomology);
             multMap := map(nthHomology,baseRing^(rank source cycleProductMatrix),myMatrix);
             kernelMultMap := prune ker multMap;       
             kernelGens := entries transpose substitute(gens image kernelMultMap.cache.pruningMap, coefficientRing A.ring);
             --retVal = flatten append(retVal / rightRingMap, apply(kernelGens, z -> sum apply(#z, i -> (monListHA#i)*(z#i))));
	     retVal = apply(kernelGens, z -> sum apply(#z, i -> (monListHA#i)*(z#i)));
          );
       );
     );
  )
  else (
     retVal = flatten entries basis (N,polyRing); 
  );
  retVal
)

DGAlgebra ** Ring := (A,S) -> (
  B := dgAlgebra(S, A.Degrees);
  newDiff := apply(flatten entries matrix (A.diff), f -> substitute(f,B.algebra));
  B.diff = map(B.algebra,B.algebra, newDiff);
  B
)

TEST ///
restart
loadPackage "DGAlgebras"
R = ZZ/32003[x,y,z]/ideal{x^3,y^4,z^5}
B = koszulComplexDGA(R)
homologyAlgebra(B,5,5)
R = ZZ/32003[x,y,z,w]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
B = koszulComplexDGA(R)
time HB = homologyAlgebra(B,5,5)
reduceHilbert hilbertSeries HB
time HB = homologyAlgebra(B,5,6)
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
    DGAlgebra
  Headline
    Data types and basic functions on graphs used in algebra and algebraic geometry. 
  Description
    Text
      This package is used to construct graphs. 
///

end

--Tutorial

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
time apply(7, i -> time numgens prune polyHomology(A3,i))
time kRes = res(coker vars R3, LengthLimit=> 18)
time apply(17, i -> time HH_i(kRes));

-- Tor algebras
restart
loadPackage "DGAlgebras"
R3 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
TorR3 = torAlgebra(R3)
apply(15, i -> hilbertFunction(i,TorR3))
res(coker vars R3, LengthLimit => 15)
R4 = QQ[x,y,z]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
TorR4 = torAlgebra(R4,5)
apply(8, i -> hilbertFunction(i,TorR4))
res(coker vars R4, LengthLimit => 8)
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
time apply(10, i -> time prune polyHomology(A3,i));
HA3 = homologyAlgebra(A3,5,15)
return; continue; listLocalSymbols

-- Koszul Complex and homology algebras
restart
loadPackage "DGAlgebras"
R1 = ZZ/32003[x,y,z]
A1 = koszulComplexDGA(R1)
apply(4,i -> polyDifferential(A1,i))
HA1 = homologyAlgebra(A1)
peek HA1.cache
describe oo
R2 = R1/ideal{x^3,y^4,z^5}
A2 = koszulComplexDGA(R2)
HA2 = homologyAlgebra(A2)
peek HA2.cache
describe oo
use R1
R3 = R1/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
A3 = koszulComplexDGA(R3)
HA3 = homologyAlgebra(A3)
describe oo
peek HA3.cache
betti prune HH(koszul vars R3)
reduceHilbert hilbertSeries HA3

break
