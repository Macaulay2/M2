-- -*- coding: utf-8 -*-
newPackage("DGAlgebras",
     Authors => {
	  {Name => "Frank Moore"}
	  },
     DebuggingMode => true,
     Headline => "Data type for DG Algebras",
     Version => "0.2"
     )

export {DGAlgebra, dgAlgebra, setDiff, natural, toComplex, koszulComplexDGA, acyclicClosure, killCycles, adjoinVariables,
        homologyAlgebra, torAlgebra, polyDifferential}

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
     A#(symbol natural) = (A.ring)[varsList, Degrees => degList, SkewCommutative => select(toList(0..(#degList-1)), i -> odd degList#i)];
     A#(symbol Degrees) = degList;
     A#(symbol cache) = new CacheTable;
     A.cache#(symbol homology) = new MutableHashTable;
     A.cache#(symbol differentials) = new MutableHashTable;
     -- should verify that the differential is indeed of degree -1
     new DGAlgebra from A
)

setDiff = method()
setDiff(DGAlgebra,List) := (A,diffList) -> (
   A.diff = map(A.natural,A.natural, substitute(matrix {diffList}, A.natural));
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

toComplex(DGAlgebra) := (A) -> (
   if (any(degrees (A.natural) / first, i -> even i)) then error "Must specify an upper degree bound if an even generator exists.";
   maxDegree := sum ((degrees A.natural) / first);
   chainComplex(apply(maxDegree, i -> polyDifferential(A,i+1)))
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
  while (n <= endDegree and not foundHomology) do ( nthHomology = prune homology(n,A); if (nthHomology == 0) then n = n + 1 else foundHomology = true);
  -- at this point we have found a degree with nontrivial homology.
  -- we now add variables in one degree higher to make these cycles boundaries.
  if (not foundHomology) then retVal = A else
  (  
     homologyGenerators := entries transpose generators image nthHomology.cache.pruningMap;
     basisList := flatten entries basis({n,0},A.natural);
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
  newDiffList := apply(flatten append(take(flatten entries matrix A.diff, numgens A.natural), cycleList), f -> substitute(f, B.natural));
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

polyDiffMonomial := (A,m) -> (
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
     sourceList := flatten entries basis({n,0},A.natural);
     targetList := flatten entries basis({n-1,0},A.natural);
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

polyHomology := (A,n) -> (
  dn := 0;
  dnplus1 := 0;
  retVal := 0;
  if (A.cache.homology#?n) then retVal = A.cache.homology#n
  else if (#(flatten entries basis({n,0}, A.natural, Limit => 1)) != 0) then
  (
     if n == 0 then dn = map((A.ring)^0, (A.ring)^1, 0) else dn = polyDifferential(A,n);
     if (#(flatten entries basis({n+1,0}, A.natural, Limit => 1)) != 0) then
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

homology(ZZ,DGAlgebra) := (n,A) -> polyHomology(n,A)

torAlgebra = method()
torAlgebra(Ring,ZZ) := (R,n) -> (
  -- since we are not yet implementing the Hopf structure, only the algebra structure, we need not
  -- actually use DGAlgebras to compute the Tor algebra.  We use the built in resolution function
  -- for the resolution of R/(ideal vars R) below since it is much faster.
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
	   newVars = flatten append(newVars, toList (X_((numgens torSoFar)+1)..X_((numgens torSoFar) + numNewVars)));
           degreeList = flatten append(degreeList, toList (numNewVars:currentDegree));
           if (odd currentDegree) then skewList = flatten append(skewList, toList ((numgens torSoFar)..((numgens torSoFar) + numNewVars - 1)));
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

representativeCycles = method()
representativeCycles(DGAlgebra,ZZ) := (A,n) -> (
  homologyGenerators := entries transpose generators image (prune homology(n,A)).cache.pruningMap;
  basisList := flatten entries basis({n,0},A.natural);
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
	relList = flatten append(relList,findDegNRelations(A,polyRing,cycleList,relList,n));
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
  degreesList := degrees A.natural / first;
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
     while (n <= maxHomologyDegree) do (relList = flatten append(relList,findDegNRelations(A,polyRing,cycleList,relList,n)); n = n + 1;);
     defIdeal := trim ideal relList;
     HA = polyRing/defIdeal;
     -- put the cycles that the variables represent in the cache.
     HA.cache = new CacheTable;
     HA.cache#cycles = cycleList;
     A.cache#homologyAlgebra = HA;
  );
  HA
)

getGreaterMonomials:= (R,N) -> (
  maxDegree := max (degrees R / first);
  flatten apply(maxDegree, i -> flatten entries basis(N+i,R))
)

makeCycleRing := (A, cycleList) -> (
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
  monListA := flatten entries basis ({N,0},A.natural);
  cycleProductList := apply(expListHA, xs -> product apply(#xs, i -> (cycleList#i)^(xs#i)));
  (apply(cycleProductList, z -> (coefficients(z, Monomials => monListA))#1),monListHA)
)

findDegNGenerators := (A,oldCycleList,N) -> (
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
  else if (flatten entries basis({N,0},A.natural, Limit => 1) == {}) then cycleList = {}
  else (
     -- the below matrix contains all monomials of degree n in the cycleList that was input, put into a matrix using the basis of monomials given by monListA
     nthHomology := homology(N,A);
     if (prune nthHomology != 0)
     then (
        cycleProductList := (getCycleProductList(A,oldCycleList,N))#0;
        if (cycleProductList != {})
	then (
           cycleProductMatrix := substitute(fold(cycleProductList, (i,j) -> i | j), A.ring);
           newHomology := prune (nthHomology / (image map (target gens nthHomology, source cycleProductMatrix, matrix entries cycleProductMatrix)));
           monListA := flatten entries basis ({N,0},A.natural);
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

--findDegNRelations = method()
findDegNRelations:= (A,polyRing,algGens,algRels,N) -> (
  -- this function tries to find the relations in degree N that involve the generators in the list algGens
  -- no checking is done to see if algGens are actually minimal generators at this point.
  defIdeal := 0;
  local cycleProductList;
  local monListHA;
  if (algRels != {}) then defIdeal = ideal algRels else defIdeal = ideal 0_polyRing;
  retVal := {0_polyRing};
  -- check if the DGA is zero in this degree. If so, just return back the algRels.
  if ((flatten entries basis({N,0},A.natural, Limit => 1) != {}) or (#algGens == 0)) then (
     ringSoFar := polyRing/defIdeal;
     nthHomology := homology(N,A);
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
  newDiff := apply(flatten entries matrix (A.diff), f -> substitute(f,B.natural));
  B.diff = map(B.natural,B.natural, newDiff);
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
      * @ TO (homology,ZZ,DGAlgebra) @
      * @ TO (homologyAlgebra,DGAlgebra) @
      * @ TO (homologyAlgebra,DGAlgebra,ZZ,ZZ) @
      
      Operations on DG algebras
      * @ TO DGAlgebra ** Ring @
      * @ TO DGAlgebra ** DGAlgebra @
      * @ TO (toComplex, DGAlgebra, ZZ) @
      * @ TO (killCycles, DGAlgebra, ZZ) @
      * @ TO (adjoinVariables, DGAlgebra, List) @
      * @ TO (acyclicClosure, DGAlgebra, ZZ) @
///

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
      One can also compute the homology of A directly with @ TO homology @.
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
      One can also compute the homology of A directly with @ TO homology @.
///

doc ///
  Key
    (homology,ZZ,DGAlgebra)
  Headline
    Computes the homology of a DG Algebra
  Usage
    H = homology(n,A)
  Inputs
    n:ZZ
    A:DGAlgebra 
  Outputs
    H:Module
      The nth homology of A.
///

doc ///
  Key
    (homology,ZZ,DGAlgebra)
  Headline
    Computes the homology of a DG Algebra
  Usage
    H = homology(n,A)
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
apply(4,i -> numgens prune HH_i(koszulR))
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

-- connected sum example
-- goal: get this example to run quickly.
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^4,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^3}
A = koszulComplexDGA(R)
HA = homologyAlgebra(A)
describe oo
peek HA.cache
reduceHilbert hilbertSeries HA
koszulR = koszul vars R
apply(5,i -> numgens prune HH_i(koszulR))

-- connected sum example
-- goal: get this example to finish.
R2 = ZZ/32003[a,b,c,x,y,z]/ideal{a^3,b^3,c^3,x^3,y^4,z^5,a*x,a*y,a*z,b*x,b*y,b*z,c*x,c*y,c*z,a^2*b^2*c^2-x^2*y^3*z^4}
A2 = koszulComplexDGA(R2)
HA2 = homologyAlgebra(A2)
describe oo
peek HA2.cache
reduceHilbert hilbertSeries HA2
koszulR2 = koszul vars R2
apply(7,i -> numgens prune HH_i(koszulR2))

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
time apply(7, i -> time numgens prune homology(i,A3))
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
time apply(10, i -> time prune homology(i,A3));
-- need to speed up the prune somehow.  Use the grading of the underlying ring?
HA3 = homologyAlgebra(A3,5,15)

break
