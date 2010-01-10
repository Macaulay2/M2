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

export {DGAlgebra, dgAlgebra, algebra, koszulComplexDGA, toComplex, acyclicClosure, killCycles, adjoinVariables,
        homologyAlgebra}

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
     A#(symbol vars) = toList (T_1..T_(#degList));
     A#(symbol diff) = {};
     A#(symbol algebra) = (A.ring)[A.vars, Degrees => degList, SkewCommutative => select(toList(0..(#degList-1)), i -> odd degList#i)];
     A#(symbol degreeList) = degList;
     A#(symbol cache) = new CacheTable;
     -- should verify that the differential is indeed of degree -1
     new DGAlgebra from A
)

koszulComplexDGA = method()
koszulComplexDGA(Ring) := (R) -> (
     A := dgAlgebra(R, toList ((numgens R):1));
     use A.ring;
     A.diff = gens R;
     A
)

koszulComplexDGA(Ideal) := (I) -> (
     A := dgAlgebra(ring I, toList ((numgens I):1));
     use A.ring;
     A.diff = I_*;
     A
)

taylorDGA = method()
taylorDGA(MonomialIdeal) := (I) -> (
   A
)

toComplex = method()
toComplex(DGAlgebra) := (A) -> (
   A
)

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
  newDegreesList := flatten append(A.degreeList,apply(cycleList, z -> (first degree z) + 1));
  newDiffList := flatten append(A.diff, cycleList);
  B := dgAlgebra(A.ring,newDegreesList);
  use B.ring;
  B.diff = apply(newDiffList, f -> substitute(f, B.algebra));
  B
)

acyclicClosure = method()
acyclicClosure(Ring,ZZ) := (R, homologicalDegreeLimit) -> (
  K := koszulComplexDGA(R);
  n := 1;
  A := K;
  while (n <= homologicalDegreeLimit) do (
     A = killCycles(A,StartDegree => n);
     n = n + 1;
  );
  A
)

polyDiffMonomial = method()
polyDiffMonomial(DGAlgebra,RingElement) := (A,m) -> (
  -- uses the Leibniz rule to compute the differential of a traditional monomial
  expList := flatten exponents m;
  dgSign := 1;
  diffList := apply(#expList, l -> if (expList#l != 0) then ( dgSign = sum apply(l, i -> (A.degreeList)#i*expList#i);
	                                                      prevTerm = l;
							      firstDegList := take(expList, l);
							      lastDegList := drop(expList, l+1);
							      firstTerm := 1;
							      lastTerm := 1;
							      if (l != 0) then firstTerm = product apply(#firstDegList, i -> (A.algebra_i)^(firstDegList#i));
      							      if (l != (#expList - 1)) then lastTerm = product apply(#lastDegList, i -> (A.algebra_(l+i+1))^(lastDegList#i));
							      (expList#l)*(-1)^dgSign*firstTerm*((A.diff)#l)*(A.algebra_l)^((expList#l)-1)*lastTerm)
							      --(-1)^dgSign*firstTerm*((A.diff)#l)*(A.algebra_l)^((expList#l)-1)*lastTerm)
					               else 0_(A.algebra) );
  sum diffList
)

polyDifferential = method()
polyDifferential(DGAlgebra,ZZ) := (A,n) -> (
  sourceList := flatten entries basis({n,0},A.algebra);
  targetList := flatten entries basis({n-1,0},A.algebra);
  myMatrix := substitute(fold(apply(sourceList, i -> (coefficients(polyDiffMonomial(A,i), Monomials => targetList))#1), (i,j) -> i | j), R);
  map((A.ring)^(#targetList), (A.ring)^(#sourceList), entries myMatrix)
)

polyDifferential(DGAlgebra,RingElement) := (A,f) -> (
  sum apply(terms f, m -> polyDiffMonomial(A,m))
)

polyHomology = method()
polyHomology(DGAlgebra,ZZ) := (A,n) -> (
  dn := 0;
  dnplus1 := 0;
  retVal := 0;
  if (#(flatten entries basis({n,0}, A.algebra, Limit => 1)) != 0) then
  (
     if n == 0 then dn = map(R^0, R^1, 0) else dn = polyDifferential(A,n);
     if (#(flatten entries basis({n+1,0}, A.algebra, Limit => 1)) != 0) then
        dnplus1 = polyDifferential(A,n+1)
     else
        dnplus1 = map(source dn, R^0, 0);
     retVal = homology(dn,dnplus1);
  )
  else
     retVal = R^0;
  retVal
)

representativeCycles = method()
representativeCycles(DGAlgebra,ZZ) := (A,n) -> (
  homologyGenerators := entries transpose generators image (prune polyHomology(A,n)).cache.pruningMap;
  basisList := flatten entries basis({n,0},A.algebra);
  cycleList := apply(homologyGenerators, gen -> sum apply(#gen, i -> gen#i*basisList#i));
  cycleList
)

--homology(DGAlgebra,ZZ) := (A,n) -> polyHomology(A,n)

homologyAlgebra = method()
homologyAlgebra(DGAlgebra, ZZ,ZZ) := (A,genDegreeLimit,relDegreeLimit) -> (
  cycleList := {};
  relList := {};
  n := 1;
  -- get the generators of the homology algebra
  while (n <= genDegreeLimit) do (cycleList = flatten append(cycleList, findDegNGenerators(A,cycleList,n)); n = n + 1;);
  n = 2;
  while (n <= relDegreeLimit) do (relList = flatten append(relList, findDegNRelations(A,cycleList,relList,n)); n = n + 1;);
  defIdeal := ideal relList;
  (ring first relList)/defIdeal
)

homologyAlgebra(DGAlgebra) := (A) -> (
  A
)

makeCycleRing = method()
makeCycleRing(DGAlgebra,List) := (A, cycleList) -> (
  --baseRing := A.ring/(ideal flatten entries polyDifferential(A,1));
  baseRing := coefficientRing (A.ring);
  degreesList := apply(cycleList, i -> first degree i);
  varList = toList (X_1..X_(#cycleList));
  baseRing[varList, Degrees => degreesList, SkewCommutative => select(toList(0..(#degreesList-1)), i -> odd degreesList#i)]
)

getCycleProductList = method()
getCycleProductList(DGAlgebra,List,ZZ) := (A,cycleList,N) -> (
  -- this function returns a list containing all monomials of degree N in the cycleList that was input, decomposed using the basis of monomials given by monListA
  subRing := makeCycleRing(A,cycleList);
  monListHA := flatten entries basis(N,subRing);
  expListHA := flatten(monListHA / exponents);
  monListA := flatten entries basis ({N,0},A.algebra);
  apply(apply(expListHA, xs -> product apply(#xs, i -> (cycleList#i)^(xs#i))), z -> (coefficients(z, Monomials => monListA))#1)
)

findDegNGenerators = method()
findDegNGenerators(DGAlgebra,List,ZZ) := (A,oldCycleList,N) -> (
  -- I am assuming that the oldCycleList contains a (minimal?) set of algebra
  -- generators needed to generate the homology algebra up to degree n-1.
  -- the goal of this function is to return the generators and relations in degree n.

  cycleList := {};
  relsList := {};
  varList := {};
  if (N == 1) then (
     -- here, we know all the degree 1 elements are generators
     cycleList = representativeCycles(A,1);
  )
  else  (
     -- the below matrix contains all monomials of degree n in the cycleList that was input, put into a matrix using the basis of monomials given by monListA
     cycleProductList := getCycleProductList(A,oldCycleList,N);
     if (cycleProductList != {}) then (
        cycleProductMatrix := substitute(fold(cycleProductList, (i,j) -> i | j), R);
        nthHomology := polyHomology(A,N);
        newHomology := prune (nthHomology / (image map (target gens nthHomology, source cycleProductMatrix, matrix entries cycleProductMatrix)));
        monListA := flatten entries basis ({N,0},A.algebra);
        newGenerators := apply(entries transpose gens image newHomology.cache.pruningMap, zList -> apply(#zList, i -> zList#i*monListA#i)) / sum;
        cycleList = newGenerators;
     ); 
  );
  cycleList
)

findDegNRelations = method()
findDegNRelations(DGAlgebra,List,List,ZZ) := (A,algGens,algRels,N) -> (
  -- this function tries to find the relations in degree N that involve the generators in the list algGens
  -- no checking is done to see if algGens are actually minimal generators at this point.
  retVal := 0;
  polyRing := {};
  defIdeal := 0;
  if (algRels != {}) then (
     polyRing = ring first algRels;
     defIdeal = ideal algRels;
  )   
  else (
     polyRing = makeCycleRing(A,algGens);
     defIdeal = ideal 0_polyRing;
  );
  -- initialize retVal
  if (algRels != {}) then retVal = algRels else retVal = {0_polyRing};
  ringSoFar := polyRing/defIdeal;
  -- using algRels, check if there are indeed any new relations in degree n
  nthHomology := polyHomology(A,N);
  rankOfNthHomology := numgens prune nthHomology;
  rankOfAlgebraSoFar := hilbertFunction(N,ringSoFar);
  if (rankOfNthHomology != rankOfAlgebraSoFar) then
  (
    -- when in here, we know there is a relation in degree N.
    -- so take each monomial of the correct degree, build the cycle corresponding to that
    -- and define a map from the residue field to the homology class representing each cycle.
    -- then take the kernel, prune, and use cache.pruningMap to get the actual minimal generating
    -- set of the kernel.  Finally, reconstruct the elements from the monomials and viola!
    cycleProductList := getCycleProductList(A,algGens,N);
    if (cycleProductList != {}) then (
       cycleProductMatrix := substitute(fold(cycleProductList, (i,j) -> i | j), R);
       baseRing := coker vars (A.ring);
       myMatrix := cycleProductMatrix // (gens nthHomology);
       multMap := map(nthHomology,baseRing^(rank source cycleProductMatrix),myMatrix);
       kernelMultMap := ker multMap;       
       error "err";
    );
  );
  retVal
)

TEST ///
restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = ZZ/32003[x,y,z]/ideal{x^3,y^4,z^5}
B = koszulComplexDGA(R)
homologyAlgebra(B,4,4)
R = ZZ/32003[x,y,z,w]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
B = koszulComplexDGA(R)
homologyAlgebra(B,4,4)
///

findDegNRelations(DGAlgebra,ZZ) := (A,N) -> (
  A
)

divPowDiffMonomial = method()
divPowDiffMonomial(DGAlgebra,RingElement) := (A,m) -> (
  -- uses the Leibniz rule to compute the differential of a divided powers monomial
  A  
)

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

restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = ZZ/32003[x,y,z]/ideal{x^3,y^4,z^5}
B = koszulComplexDGA(R)
homologyAlgebra(B,4,4)
R = ZZ/32003[x,y,z,w]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
B = koszulComplexDGA(R)
homologyAlgebra(B,4)

h2 = polyHomology(B,2)
newh2 = prune (h2 / (image map (target gens h2, R^1, matrix {{-x^2*y^3},{0},{0},{0},{0},{0}})))
newh2.cache.pruningMap

C = koszulComplexDGA(ideal {x^2,y^2,z^2})
B' = killCycles(B)
polyHomology(B',3)
D = acyclicClosure(R,2)

R = ZZ/101[a,b,c]
I = (ideal vars R)^2
S = R/I
res coker vars S
D = acyclicClosure(S,3)
numgens source basis({4,0},D.algebra)
polyHomology(D,0)
polyHomology(D,1)
polyHomology(D,2)
polyHomology(D,3)


break
