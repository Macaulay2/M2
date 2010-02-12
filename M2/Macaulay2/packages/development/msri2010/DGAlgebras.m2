-- -*- coding: utf-8 -*-
newPackage("DGAlgebras",
     Authors => {
	  {Name => "Frank Moore"}
	  },
     DebuggingMode => true,
     Headline => "Data type for DG Algebras",
     Version => "0.2"
     )

export {DGAlgebra, dgAlgebra, setDiff, natural, cycles,
        toComplex, koszulComplexDGA, acyclicClosure,
	killCycles, adjoinVariables, homology2,
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
  while (n <= endDegree and not foundHomology) do ( nthHomology = prune homology2(n,A); if (nthHomology == 0) then n = n + 1 else foundHomology = true);
  -- at this point we have found a degree with nontrivial homology.
  -- we now add variables in one degree higher to make these cycles boundaries.
  if (not foundHomology) then retVal = A else
  (  
     homologyGenerators := entries transpose generators image (nthHomology.cache.pruningMap);
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

polyHomology := (n,A) -> (
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

-- note that this does not work for some reason (Dan explained it to me at one point but I can't remember.  I think it has
-- something to do with the fact that homology2(sequence) hijacks all possible calls to homology.
homology(ZZ,DGAlgebra) := (n,A) -> polyHomology(n,A)

-- Temporary fix here for the moment
homology2 = method();
homology2(ZZ,DGAlgebra) := (n,A) -> polyHomology(n,A)

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
  homologyGenerators := entries transpose generators image ((prune homology2(n,A)).cache.pruningMap);
  basisList := flatten entries basis({n,0},A.natural);
  cycleList := apply(homologyGenerators, gen -> sum apply(#gen, i -> gen#i*basisList#i));
  cycleList
)

makeHomologyRing := (A, cycleList, relList) -> (
  --baseRing := A.ring/(ideal flatten entries polyDifferential(A,1));
  baseRing := coefficientRing A.ring;
  degreesList := apply(cycleList, i -> first degree i);
  varList := toList (X_1..X_(#cycleList));
  polyRing := baseRing[varList, Degrees => degreesList, SkewCommutative => select(toList(0..(#degreesList-1)), i -> odd degreesList#i)];
  if (relList == {}) then polyRing
  else (
     myMap := map(polyRing, ring first relList, flatten entries vars polyRing);
     relList = relList / myMap;
     polyRing/(trim ideal relList)
  )
)

getCycleProductList = method()
getCycleProductList(DGAlgebra,Ring,List,ZZ) := (A,HA,cycleList,N) -> (
  -- the input is the dga A, the homology algebra HA (so far), the list of cycle generators, and the degree.
  -- this version does use the knowledge of the homologyAlgebra so far to return the cycles products in a given degree.
  monListHA := flatten entries basis(N,HA);
  expListHA := flatten(monListHA / exponents);
  monListA := flatten entries basis ({N,0},A.natural);
  cycleProductList := apply(expListHA, xs -> product apply(#xs, i -> (cycleList#i)^(xs#i)));
  (apply(cycleProductList, z -> (coefficients(z, Monomials => monListA))#1),monListHA)
)

getCycleProductList(DGAlgebra,List,ZZ) := (A,cycleList,N) -> (
  -- the input is the dga A, the list of cycle generators, and the degree.
  -- this function just assumes that HA is the free algebra on cycleList, and calls the method defined above
  HA := makeHomologyRing(A,cycleList,{});
  getCycleProductList(A,HA,cycleList,N)
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
  else if (flatten entries basis({N,0},A.natural, Limit => 1) == {}) then cycleList = {}
  else (
     nthHomology := homology2(N,A);
     if (prune nthHomology != 0)
     then (
        cycleProductList := (getCycleProductList(A,oldCycleList,N))#0;
        if (cycleProductList != {}) then (
	   -- TODO: Document the below block of code.
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

findDegNRelations := (A,HA,algGens,N) -> (
  -- this function tries to find the relations in degree N that involve the generators in the list algGens
  -- no checking is done to see if algGens are actually minimal generators at this point.
  local cycleProductList;
  local monListHA;
  retVal := {0_HA};
  -- check if DGA is zero in this degree. If so, just return back the monomials in the given degree
  if ((flatten entries basis({N,0}, A.natural, Limit => 1) != {}) or (#algGens == 0)) then (
     ringSoFar := HA;
     -- using HA, check if there are indeed any new relations in degree n
     nthHomology := homology2(N,A);
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
          -- if we are here, all monomials in the HA of this degree are zero.
          retVal = flatten entries basis (N,HA);
       )
       else (
          (cycleProductList,monListHA) = getCycleProductList(A,HA,algGens,N);
          if (cycleProductList != {}) then (
             -- TODO: Carefully document this block of code, saying what each line does.  There is a lot going on here.
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
     retVal = flatten entries basis (N,HA); 
  );
  retVal
)

getGreaterMonomials:= (R,N) -> (
  maxDegree := max (degrees R / first);
  flatten apply(maxDegree, i -> flatten entries basis(N+i,R))
)

homologyAlgebra = method()
homologyAlgebra(DGAlgebra,ZZ,ZZ) := (A,genDegreeLimit,relDegreeLimit) -> (
  cycleList := {};
  relList := {};
  n := 1;
  local HA;
  local myMap;
  -- get the generators of the homology algebra
  while (n <= genDegreeLimit) do (
     << "Computing generator degree " << n << endl;
     newCycleList := findDegNGenerators(A,cycleList,n);
     cycleList = flatten append(cycleList, newCycleList);
     n = n + 1;
  );
  if (cycleList == {}) then (
     -- put the cycles that the variables represent in the cache.
     HA = coefficientRing A.ring;
     HA.cache = new CacheTable;
     HA.cache#cycles = cycleList;
     A.cache#homologyAlgebra = HA;
  )
  else (
     -- QUESTION: Is it better to get all the generators first, and then get the relations, or to do them incrementally?
     HA = makeHomologyRing(A,cycleList,{});
     n = 2;
     while (n <= relDegreeLimit) do (
        << "Computing relation degree " << n << endl;
	newRelList := findDegNRelations(A,HA,cycleList,n);
	if (relList == {}) then relList = newRelList
	else if (newRelList != {}) then (
	   -- make sure newRelList and relList are in the same ring
           myMap = map(ring first relList, ring first newRelList, flatten entries vars ring first relList);
	   relList = flatten append(relList, newRelList / myMap);
	);
	-- now reset HA using relList for the next iteration.
	HA = makeHomologyRing(A,cycleList,relList);
	n = n + 1;
     );
     -- put the cycles that the variables represent in the cache.
     HA.cache = new CacheTable;
     HA.cache#cycles = cycleList;
     A.cache#homologyAlgebra = HA;
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
  if (any(degreesList, i -> even i)) then error "Must supply upper degree bound on generators and relations if there is a DG Algebra generator of even degree.";
  -- otherwise, all are odd, and we can compute the entire homology algebra
  maxDegree := sum degreesList;
  
  n = maxDegree;
  while (n <= maxDegree and prune homology2(n,A) == 0) do n = n - 1;
  maxHomologyDegree := n;
  -------------------------------------------
  
  HA = homologyAlgebra(A,maxDegree,maxHomologyDegree);
  relList = (ideal HA)_*;
  cycleList = HA.cache.cycles;
  
  -------------------------------------------
  -- handle all the monomials that are outside the max homology degree that are zero for degree reasons.
  otherRelList := (trim ideal getGreaterMonomials(HA,maxHomologyDegree+1))_*;
  if (otherRelList != {}) then (
     myMap = map(ambient HA, ring first otherRelList, flatten entries vars ambient HA);
     relList = flatten append(relList, otherRelList / myMap);
     HA = makeHomologyRing(A,cycleList,relList);
     -- put the cycles that the variables represent in the cache.
     HA.cache = new CacheTable;
     HA.cache#cycles = cycleList;
     A.cache#homologyAlgebra = HA;
  );
  -- return the homology algebra
  HA
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
HB = homologyAlgebra(B)
reduceHilbert hilbertSeries HB
restart
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
-- 35 seconds on mac mini
time HA = homologyAlgebra(A)
reduceHilbert hilbertSeries HA
peek HA.cache

-- connected sum example
-- goal: get this example to run quickly.
restart
loadPackage "DGAlgebras"
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^4,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^3}
koszulR = koszul vars R
apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
-- 176 seconds on mac mini (!)
time HA = homologyAlgebra(A)
reduceHilbert hilbertSeries HA
peek HA.cache

-- connected sum example
-- goal: get this example to finish.
restart
loadPackage "DGAlgebras"
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
