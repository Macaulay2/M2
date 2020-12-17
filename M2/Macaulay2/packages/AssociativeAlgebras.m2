     newPackage(
        "AssociativeAlgebras",
        Version => "0.7", 
        Date => "10 June 2020",
        Authors => {{Name => "Frank Moore", 
                  Email => "", 
                  HomePage => ""},
              {Name => "Mike Stillman", 
                  Email => "", 
                  HomePage => ""}
              },
        Headline => "Non-commutative polynomial algebra",
        DebuggingMode => true
        )

-- MES + FM TODO 2020.07.29
--   1. Interreduction in GB code, as well as ensuring inhomogeneous
--      ideals are handled correctly. 
--   2. F4 matrices seem to be larger than they need to be.
--   3. F4 code should work on inhomogeneous ideals
--   4. Make tests more robust
--   5. Finish documentation for AssociativeAlgebras
--   6. Change basis to use hooks and use it instead of ncBasis
--   7. Same as 6. for NCGB and gb
--   8. isWellDefined and kernel (both are related to flattenRing)
--   9. Degree information in a ring tower is not kept -- do we want this?
-- Next steps:
--   1. Free Resolutions
--   2. GB over commutative rings (not just fields)
--   3. Anick resolution

-- M2 project/draft pull request for ncEngine
-- get examples all running
-- have nCAlgebra functionality brought over to ncEngine:
--  normalElements
--  modules (bi-modules)
-- major computations to be written yet:
--  Hilbert function (is needed)
--  inhomog GB's.
--  one-sides GB's in modules.
-- get unit tests working again.  Also tests in the package.
-- connect unit tests to autotools build

export {
    "freeAlgebra",
    "ncBasis",
    "NCGB", -- uugh: change name!
    "NCReduction2Sided",
    "FreeAlgebra", -- change this name too! FM (?)
    "FreeAlgebraQuotient", 
    "sequenceToVariableSymbols",
    "leftMultiplicationMap",
    "rightMultiplicationMap",
    "centralElements",
    "normalElements",
    "skewPolynomialRing",
    "threeDimSklyanin",
    "fourDimSklyanin",
    "oreIdeal",
    "oreExtension",
    "freeProduct",
    "qTensorProduct",
    "toNCRing",
    "isFreeAlgebraOrQuotient",
    "homogDual",
    "quadraticClosure",
    "oppositeRing",
    "isLeftRegular",
    "isRightRegular",
    "isCentral",
    "isNormal",
    "normalAutomorphism"
    }

-- FM: better way to do this?

-- symbols into hash table for ring
generatorSymbols = Core#"private dictionary"#"generatorSymbols"
generatorExpressions = Core#"private dictionary"#"generatorExpressions"
liftDegree = Core#"private dictionary"#"liftDegree"
promoteDegree = Core#"private dictionary"#"promoteDegree"
indexStrings = Core#"private dictionary"#"indexStrings"
indexSymbols = Core#"private dictionary"#"indexSymbols"

-- local functions in the commutative ring code from core that are needed
findSymbols = value Core#"private dictionary"#"findSymbols"
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary"
makepromoter = value Core#"private dictionary"#"makepromoter"
processWeights = value Core#"private dictionary"#"processWeights"
hasAttribute = value Core#"private dictionary"#"hasAttribute"
getAttribute = value Core#"private dictionary"#"getAttribute"
processDegrees = value Core#"private dictionary"#"processDegrees"
commonEngineRingInitializations = value Core#"private dictionary"#"commonEngineRingInitializations"

-- raw function calls to engine
raw = value Core#"private dictionary"#"raw"
rawPairs = value Core#"private dictionary"#"rawPairs"
rawQuotientRing = value Core#"private dictionary"#"rawQuotientRing"
rawGetTerms = value Core#"private dictionary"#"rawGetTerms"
rawSparseListFormMonomial = value Core#"private dictionary"#"rawSparseListFormMonomial"
rawNCFreeAlgebra = value Core#"private dictionary"#"rawNCFreeAlgebra"
rawNCBasis = value Core#"private dictionary"#"rawNCBasis"
rawNCReductionTwoSided = value Core#"private dictionary"#"rawNCReductionTwoSided"
rawNCGroebnerBasisTwoSided = value Core#"private dictionary"#"rawNCGroebnerBasisTwoSided"
RawRingElement = value Core#"private dictionary"#"RawRingElement"

-- we are having trouble with the following line, due to the fact
-- that RawRing is a class, but we would like to reference the symbol in the
-- local hash table.
RawRing = Core#"private dictionary"#"RawRing"

BUG = str -> ()

FreeAlgebra = new Type of EngineRing
FreeAlgebra.synonym = "free algebra"

FreeAlgebraQuotient = new Type of QuotientRing
FreeAlgebraQuotient.synonym = "quotient of a free algebra"

new FreeAlgebra from List := (EngineRing, inits) -> new EngineRing of RingElement from new HashTable from inits

getNameIfAny = (k) -> expression if hasAttribute(k,ReverseDictionary) then getAttribute(k,ReverseDictionary) else k

-- MES.  do we really need all 5 of these?
expression FreeAlgebra := R -> (
     if hasAttribute(R,ReverseDictionary) then return expression getAttribute(R,ReverseDictionary);
     k := last R.baseRings;
     (getNameIfAny k) (R#generatorSymbols)
     )
net FreeAlgebra := R -> (
     if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
     else net expression R)
describe FreeAlgebra := R -> (
     k := last R.baseRings;
     net ((getNameIfAny k) R#generatorSymbols)
     )
toExternalString FreeAlgebra := R -> (
    --toString describe R
     k := last R.baseRings;
     toString ((getNameIfAny k) R#generatorSymbols)
     )
toString FreeAlgebra := R -> (
    toString expression R
    -- 
    -- if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
    -- else toString expression R
    )

ambient FreeAlgebra := R -> R

combineWeightsAndDegree = (nvars, wtvecs, degvector) -> (
    -- nvars: ZZ
    -- wtvecs: List (of lists of ZZ's)
    -- degvector: List (of lists of ZZ's)
    -- wtvecs: should be a list lists of length nvars, consisting of all ints.
    -- return value: List of lists of (small) ZZ's
    wtvecs = processWeights(nvars, wtvecs);
    if nvars =!= #degvector then error "expected a degree for each variable";
    wtd := if #degvector#0 == 1 and all(degvector, d -> d#0 > 0) then flatten degvector
           else for i from 0 to nvars-1 list 1;
    append(wtvecs, wtd)
    )

checkHeft = (degs, heftvec) -> (
    all(degs, d -> #d === #heftvec) and
    all(degs, d -> sum apply(d,heftvec,times) > 0)
    )
-- TODO (11 Jan 2017 MS+FM), now (19 Dec 2017 MS+FM), and again Dec 2018...!
--   1. handle multigradings
--   2. handle different monomial orders
--     e.g. elimination order.
--   3. check on: ring map evaluations, promote, lift?
--   4. look at other top level rings (and do what with them?)
--   5. net, expression, toString...  DONE
--   6. check on listForm (what is the issue?)
--       problem #1: 'listForm RingElement' is making monomials into exponent vectors.
--       problem #2: we need a way of getting an element of R from a noncommutative monomial
--       issue #3: want a form where the monomials come out as lists of variables.
--   7. use, make it so we can create this ring without assigning the variables to global symbols
--   8. make sure multiplication is using a heap based approach.
--   9. need a good subword lookup table, for reduction wrt a GB.
-- TODO for PolynomialAlgebra.hpp,cpp:
--   - make monoid routines for the cryptic uses.
--   - figure out backInserter, i.e. conform to the standard c++ lib
--   - add in degree support
--   - add in e.g. leadMonomial, support.  
freeAlgebra = method()
freeAlgebra(Ring, List) := FreeAlgebra => (A, args)  -> (
   -- get the symbols associated to the list that is passed in, in case the variables have been used earlier.
   opts := new OptionTable from {Degrees=>null, DegreeRank=>null, Weights=>{}, Heft=>null};
   (opts,args) = override(opts,toSequence args);
   --- for some reason, override returns the elt itself if there is only one variable
   varList := if class args === Sequence then args else {args};
   if not (A.?Engine and A.Engine) then
       error "expected coefficient ring handled by the engine";
   varSymbols := findSymbols toSequence varList;
   if #varSymbols == 0 then error "Expected at least one variable.";
   (degs,degrk) := processDegrees( opts.Degrees, opts.DegreeRank, length varSymbols);
   -- now check correctness of weights, flatten them
   wtvecs := combineWeightsAndDegree(#varSymbols, opts.Weights, degs);
   heftvec := if opts.Heft =!= null then opts.Heft
       else splice {degrk: 1};
   -- check that we have a valid heft vector.
   if not checkHeft(degs, heftvec) then error "expected a valid Heft vector, which dots positively with each degree vector";
   rawR := rawNCFreeAlgebra(raw A, toSequence(varSymbols/toString), raw degreesRing degrk, flatten degs, flatten wtvecs, heftvec);
   R := new FreeAlgebra from {
       RawRing => rawR,
       generatorSymbols => varSymbols,
       --(symbol generatorExpressions) => hashTable apply(#varList, i -> (i,expression varList#i)),
       generatorExpressions => for v in varSymbols list if instance(v,Symbol) then v else expression v,
       (symbol generators) => {},
       (symbol degreesRing) => degreesRing degrk,
       (symbol degreeLength) => degrk,
       (symbol degrees) => degs,
       (symbol Heft) => heftvec,
       (symbol Weights) => wtvecs,
       (symbol isCommutative) => false,
       (symbol isHomogeneous) => true,
       (symbol CoefficientRing) => A,
       (symbol cache) => new CacheTable from {},
       (symbol baseRings) => append(A.baseRings,A)
       };
   newGens := for i from 0 to #varSymbols-1 list varSymbols#i <- new R from R#RawRing_i;
   R#promoteDegree = makepromoter degreeLength R;
   R#liftDegree = makepromoter degreeLength R;
   R.generators = newGens;
   commonEngineRingInitializations R;
   --- need to fix net of an RingElement coming from a NCPolynomial ring.
   processFactor := (k,v) -> if v =!= 1 then Power{R#generatorExpressions#k, v} else R#generatorExpressions#k;
   processFactors := facs -> (
	  if #facs  === 1
	  then processFactor facs#0
	  else new Product from apply(facs, processFactor));
   -- TODO:
   -- In order to get expression R to display monomials correctly, one must:
   -- 1. Create a new function which returns (coeff, a list of integers representing the monomial)
   -- 2. Use this function below, together with a modified version of the code above to
   --    create an expression of a monomial.
   expression R := f -> (
	       (
		    rawP := rawPairs(raw coefficientRing R, raw f);
		    -- apply the following function to the output of rawPairs
		    (coeffs,monoms) -> (
			 if #coeffs === 0
			 then expression 0
			 else sum(coeffs,monoms, (a,m) -> expression (if a == 1 then 1 else promote(a,A)) * expression (if m == {} then 1 else processFactors m))
			 )
		    ) (rawP#0, rawP#1 / rawSparseListFormMonomial)
        );
   net R := f -> net expression f;
   leadMonomial R := f -> (
       if f == 0 then return f;
       rawLT := rawGetTerms(numgens R, raw f, 0, 0);
       rawP := first last rawPairs(raw coefficientRing R, rawLT);
       product(apply(rawSparseListFormMonomial rawP, p -> R_(p#0)^(p#1)))
       );
   R
   );

-- WARNING, TODO!!  This breaks local rings
Ring List := Ring => (R,variables) -> (
    use freeAlgebra(R, variables)
    )

FreeAlgebra _ ZZ := (R, n) -> (R.generators)#n
coefficientRing FreeAlgebra := R -> last R.baseRings

degreesRing FreeAlgebra := PolynomialRing => R -> (
   if R#?(symbol degreesRing) then R#(symbol degreesRing)
   else error "no degreesRing for this ring"
   )

isWellDefined FreeAlgebra := Boolean => R -> (
    -- an internal check that R is defined properly.
    isbad := str -> (if debugLevel > 0 then << str; false);
    if # R.generators =!= numgens R then 
        return isbad "# R.generators =!= numgens R";
    if # R#generatorExpressions =!= numgens R then 
        return isbad "# R#generatorExpressions =!= numgens R";
    if # R#generatorSymbols =!= numgens R then 
        return isbad "# R#generatorSymbols =!= numgens R";
    if not all(R.generators, x -> class x === R) then 
        return isbad "generators are not all in the ring";
    if not all(R#generatorExpressions, x -> instance(x,Expression) or instance(x,Symbol)) then
        return isbad "generatorExpressions are not all expressions";
    if not all(R#generatorSymbols, x -> instance(x, Symbol) or instance(x, IndexedVariable)) then
        return isbad "generatorSymbols are not all symbols or indexed variables";
    if not instance(expression R, Expression) then
        return isbad "expression R should be an expression";
    if not instance(describe R, Net) then
        return isbad "'describe R' should be a Net";
    if not instance(net R, Net) then
        return isbad "'net R' should be a Net";
    if not instance(toString R, String) then
        return isbad "'toString R' should be a string";
    if not instance(toExternalString R, String) then
        return isbad "'toExternalString R' should be a string";
    true
    )
-- listForm

NCGB = method(Options => {Strategy=>0})
NCGB(Ideal, ZZ) := opts -> (I, maxdeg) -> (
    if not I.cache.?NCGB or I.cache.NCGB#0 < maxdeg then (
        tobecomputed := raw if I.cache.?NCGB then I.cache.NCGB#1 else gens I;
        gbI := map(ring I, rawNCGroebnerBasisTwoSided(tobecomputed, maxdeg, opts.Strategy));
        I.cache.NCGB = {maxdeg, gbI};
        );
    I.cache.NCGB#1
    )
NCGB Ideal := opts -> (I) -> (
    if I.cache.?NCGB then I.cache.NCGB#1
    else (
        maxdegI := first max(degrees source gens I); -- TODO: change once multidegrees are allowed.
        NCGB(I, 2*maxdegI, opts)
        )
    )
    

NCReduction2Sided = method()
NCReduction2Sided(Matrix, Matrix) := (M, I) -> (
    R := ring M;
    if R =!= ring I then error "expected same ring";
    map(R, rawNCReductionTwoSided(raw M, raw I))
    )
NCReduction2Sided(Matrix, Ideal) := (M, I) -> NCReduction2Sided(M, gens I)
NCReduction2Sided(RingElement, Ideal) := (F, I) -> (NCReduction2Sided(matrix{{F}}, gens I))_(0,0)

FreeAlgebra / Ideal := FreeAlgebraQuotient => (R,I) -> (
     if ring I =!= R then error "expected ideal of the same ring";
     if I == 0 then return R;
     A := R;
     while class A === QuotientRing do A = last A.baseRings;
     gensI := generators I;
     S := new FreeAlgebraQuotient from rawQuotientRing(raw R, raw NCGB I);
     S.cache = new CacheTable;
     S.ideal = I;
     S.baseRings = append(R.baseRings,R);
     commonEngineRingInitializations S;
     S.relations = gensI;
     S.isCommutative = false;
     S.isHomogeneous = isHomogeneous I;
     S.generators = apply(generators S, m -> promote(m,S));
     if R#?generatorSymbols then S#generatorSymbols = R#generatorSymbols;
     if R#?generatorExpressions then S#generatorExpressions = (
	  R#generatorExpressions
	  );
     if R#?indexStrings then S#indexStrings = applyValues(R#indexStrings, x -> promote(x,S));
     if R#?indexSymbols then S#indexSymbols = applyValues(R#indexSymbols, x -> promote(x,S));
     expression S := lookup(expression,R);
     S.use = x -> ( -- what is this for??
	  );
     S)

ncBasis = method(Options => {Limit => infinity})
ncBasis(InfiniteNumber,InfiniteNumber,Ring) := 
ncBasis(List,InfiniteNumber,Ring) := 
ncBasis(InfiniteNumber,List,Ring) := 
ncBasis(List,List,Ring) := opts -> (lo,hi,R) -> (
    if not instance(R, FreeAlgebra) and not instance(R, FreeAlgebraQuotient)
    then error "expected a free algebra or a free algebra quotient ring";
    neginfinity := -infinity;
    if lo === infinity then error "incongruous lower degree bound: infinity";
    if hi === -infinity then error "incongruous upper degree bound: -infinity";
    if lo === -infinity then lo = {};
    if hi === infinity then hi = {};
    if (#lo != 0 and #lo > degreeLength R) or 
       (#hi != 0 and #hi > degreeLength R) then
        error "expected length of degree bound not to exceed that of ring";
    if lo =!= hi and #lo > 1 then error "degree rank > 1 and degree bounds differ";
    if not all(lo, i -> instance(i,ZZ)) then error ("expected a list of integers: ", toString lo);
    if not all(hi, i -> instance(i,ZZ)) then error ("expected a list of integers: ", toString hi);
    limit := if opts.Limit === infinity then -1 else if instance(opts.Limit,ZZ) then opts.Limit else
       error "expected 'Limit' option to be an integer or infinity";
    if #hi == 1 and hi#0 < 0 then return map(R^1, R^0, {{}}); -- disallow use of "-1" meaning infinity, at top level.
    gbR := if #hi == 1 then NCGB(ideal R, hi#0) else NCGB(ideal R);
    result := map(ring gbR, rawNCBasis(raw gbR, lo, hi, limit));
    map(R^1,, promote(result, R))
    )

ncBasis(List,Ring) := opts -> (d,R) -> ncBasis(d,d,R,opts)
ncBasis(ZZ,Ring) := opts -> (d,R) -> ncBasis(d, d, R, opts)
ncBasis(ZZ,ZZ,Ring) := opts -> (lo,hi,R) -> ncBasis({lo}, {hi}, R, opts)
ncBasis(InfiniteNumber,ZZ,Ring) := opts -> (lo,hi,R) -> ncBasis(lo, {hi}, R, opts)
ncBasis(ZZ,InfiniteNumber,Ring) := opts -> (lo,hi,R) -> ncBasis({lo}, hi, R, opts)
ncBasis Ring := opts -> R -> ncBasis(-infinity, infinity, R, opts)

-------------------------
--- beginning to port over stuff from NCAlgebra

leftMultiplicationMap = method()
leftMultiplicationMap(RingElement,ZZ) := (f,n) -> (
   B := ring f;
   -- TODO: Make sure this is what we want in multigraded case
   m := sum degree f;
   if m === -infinity then m = 0;
   nBasis := flatten entries ncBasis(n,B);
   nmBasis := flatten entries ncBasis(n+m,B);
   leftMultiplicationMap(f,nBasis,nmBasis)
)

leftMultiplicationMap(RingElement,ZZ,ZZ) := (f,n,m) -> (
   B := ring f;
   nBasis := flatten entries ncBasis(n,B);
   mBasis := flatten entries ncBasis(m,B);
   leftMultiplicationMap(f,nBasis,mBasis)
)

leftMultiplicationMap(RingElement,List,List) := (f,fromBasis,toBasis) -> (
   local retVal;
   A := ring f;
   R := coefficientRing A;
   if not isHomogeneous f then error "Expected a homogeneous element.";
   if fromBasis == {} and toBasis == {} then (
      retVal = map(R^0,R^0,0);
      retVal
   )
   else if fromBasis == {} then (
      retVal = map(R^(#toBasis), R^0,0);
      retVal
   )
   else if toBasis == {} then (
      retVal = map(R^0,R^(#fromBasis),0);
      retVal
   )
   else (
      sub(last coefficients(matrix{apply(fromBasis, g -> f*g)},Monomials=>toBasis), R)
   )
)

rightMultiplicationMap = method()
rightMultiplicationMap(RingElement,ZZ) := (f,n) -> (
   B := ring f;
   m := sum degree f;
   if m === -infinity then m = 0;
   nBasis := flatten entries ncBasis(n,B);
   nmBasis := flatten entries ncBasis(n+m,B);
   rightMultiplicationMap(f,nBasis,nmBasis)
)

rightMultiplicationMap(RingElement,ZZ,ZZ) := (f,n,m) -> (   
   if f != 0 and degree f != m-n then error "Expected third argument to be the degree of f, if nonzero.";
   B := ring f;
   nBasis := flatten entries ncBasis(n,B);
   mBasis := flatten entries ncBasis(m,B);
   rightMultiplicationMap(f,nBasis,mBasis)
)

rightMultiplicationMap(RingElement,List,List) := (f,fromBasis,toBasis) -> (
   local retVal;
   A := ring f;
   R := coefficientRing A;
   if not isHomogeneous f then error "Expected a homogeneous element.";
   if fromBasis == {} and toBasis == {} then (
      retVal = map(R^0,R^0,0);
      retVal
   )
   else if fromBasis == {} then (
      retVal = map(R^(#toBasis), R^0,0);
      retVal
   )
   else if toBasis == {} then (
      retVal = map(R^0,R^(#fromBasis),0);
      retVal
   )
   else (
      sub(last coefficients(matrix{apply(fromBasis, g -> g*f)}, Monomials=>toBasis), R)
   )
)

isLeftRegular = method()
isLeftRegular (RingElement, ZZ) := (f,d) -> (
   A := ring f;
   if not isHomogeneous f then error "Expected a homogeneous element.";
   r := rank rightMultiplicationMap(f,d);
   s := #(flatten entries ncBasis(d,A));
   r == s
)

isRightRegular = method()
isRightRegular (RingElement, ZZ) := (f,d) -> (
   A := ring f;
   if not isHomogeneous f then error "Expected a homogeneous element.";
   r := rank leftMultiplicationMap(f,d);
   s := #(flatten entries ncBasis(d,A));
   r == s
)

centralElements = method()
centralElements(Ring,ZZ) := (B,n) -> (
   idB := map(B,B,gens B);
   normalElements(idB,n)
)

-- worker function for normalAutomorphism
findNormalComplement = method()
findNormalComplement (RingElement,RingElement) := (f,x) -> (
   B := ring f;
   kk := coefficientRing B;
   if B =!= ring x then error "Expected elements from the same ring.";
   if not isHomogeneous f or not isHomogeneous x then error "Expected homogeneous elements";
   n := first degree f;
   m := first degree x;  -- assumes singly graded!
   leftFCoeff := sub(last coefficients(f*x,Monomials=>flatten entries ncBasis(n+m,B)), kk);
   rightMultF := sub(rightMultiplicationMap(f,m), kk);
   factorMap := (leftFCoeff // rightMultF);
   if rightMultF * factorMap == leftFCoeff then
      first flatten entries (ncBasis(m,B) * factorMap)
   else
      null
)

normalAutomorphism = method()
normalAutomorphism RingElement := f -> (
   if not isFreeAlgebraOrQuotient(ring f) then error "Expected an element in a noncommutative ring.";
   if not isHomogeneous f then error "Expected a homogeneous element.";
   B := ring f;
   normalComplements := apply(gens B, x -> findNormalComplement(f,x));
   if any(normalComplements, f -> f === null) then error "Expected a normal element.";
   map(B, B, normalComplements)
)

isCentral = method()
isCentral RingElement := f -> (
   varsList := gens ring f;
   all(varsList, x -> (f*x - x*f) == 0)   
)

-*
-- I'm not sure we want this version until we can get % working correctly.
isCentral (RingElement, Ideal) := (f,I) -> (
   varsList := gens f.ring;
   all(varsList, x -> (f*x - x*f) % I == 0)
)
*-

-- FM: TODO This steps on isNormal in IntegralClosure, I think.
isNormal = method()
isNormal RingElement := f -> (
   if not isFreeAlgebraOrQuotient(ring f) then error "Expected an element in a noncommutative ring.";
   if not isHomogeneous f then error "Expected a homogeneous element.";
   all(gens ring f, x -> findNormalComplement(f,x) =!= null)
)

normalElements = method()
normalElements(FreeAlgebraQuotient, ZZ, Symbol, Symbol) := (R,n,x,y) -> (
   -- Inputs: An associate algebra R, a degree n, and two symbols to use for indexed variables.
   -- Outputs: (1) A list of normal monomials in degree n
   --          (2) the components of the variety of normal elements (excluding the normal monomials) 
   --              expressed as ideals in terms of coefficients of non-normal monomial basis elements
   -- 
   -- The variety also contains information about the normalizing automorphism. This information is not displayed.
   -- The user can obtain the normalizing automorphism of a normal element via the normalAutomorphism function
   kk := coefficientRing R;
   fromBasis := flatten entries ncBasis(n,R);
   toBasis := flatten entries ncBasis(n+1,R);
   pos := positions(fromBasis,m->isNormal(m));
   normalBasis := apply(pos, i-> fromBasis#i);
   nonNormalBasis := fromBasis-set normalBasis;
   -- print a list of normal monomials
      << "Normal monomials of degree " << n << ":" << endl;
      for i from 0 to ((length normalBasis) - 1) do (
        << normalBasis#i << endl;
      );
      if (length normalBasis)==0 then << "none" << endl;
   numGens := numgens R;
   leftMaps := apply(gens R, x->leftMultiplicationMap(x,nonNormalBasis,toBasis));
   rightMaps := apply(gens R, x->rightMultiplicationMap(x,nonNormalBasis,toBasis));
   -- make a polynomial ring with (fromDim) - (number of normal monomials)  + (numgens R)^2 variables
   -- need to hide variables from user
   xvars := apply(nonNormalBasis, i->x_i);
   yvars := table(numGens, numGens, (i,j) -> y_(i,j));
   -- is this what I want to do here?  or should I use monoid?
   cRing := kk[(flatten yvars) | xvars,MonomialOrder=>Eliminate numGens^2];
   xvars = xvars / value;
   yvars = applyTable(yvars,value);
   leftCoeff := apply(leftMaps, L-> L*transpose matrix {xvars});
   rightCoeff := apply(rightMaps, R-> R*transpose matrix {xvars});
   idealGens := flatten apply(numGens,g-> rightCoeff#(g-1) - sum(numGens,j->(yvars#g#j)*leftCoeff#(j-1)));
   I := ideal idealGens;
   -- the next line throws away information, including automorphism data
   << "Components of the normal variety, excluding normal monomials:" << endl;
   unique(select(apply(xvars, x-> (
                     J:=saturate(I,ideal(x));
                     if J!=cRing then 
                        selectInSubring(1, gens gb J)
                     else
                        0
                     )),
                  c->c!=0))
)

normalElements(RingMap,ZZ) := (phi,n) -> (
   if source phi =!= target phi then error "Expected an automorphism.";
   B := source phi;
   ringVars := gens B;
   diffMatrix := matrix apply(ringVars, x -> {leftMultiplicationMap(phi x,n) - rightMultiplicationMap(x,n)});
   diffMatrix = sub(diffMatrix, coefficientRing B);
   nBasis := ncBasis(n,B);
   kerDiff := ker diffMatrix;
   R := ring diffMatrix;
   if kerDiff == 0 then sub(matrix{{}},R) else nBasis * gens kerDiff
)

----------------------------------------
--- Noncommutative Ring Constructions
----------------------------------------

isFreeAlgebraOrQuotient = method()
isFreeAlgebraOrQuotient Ring := R -> (class R === FreeAlgebra) or (class R === FreeAlgebraQuotient)

isExterior = method()
isExterior Ring := A -> (
   sc := apply(subsets(gens A,2), s-> s_0*s_1+s_1*s_0);
   sq := apply(gens A, g->g^2);
   all(sc|sq, x-> x==0)
)

toNCRing = method()
toNCRing Ring := FreeAlgebraQuotient => R -> (
   isComm := isCommutative R;
   isExter := isExterior R;
   if not isComm and not isExter then error "Input ring must be either strictly (-1)-skew commutative or commutative.";
   --- generate the (skew)commutivity relations
   Q := coefficientRing R;
   A := Q ((gens R) / baseName | {Degrees=> degrees R});
   phi := map(A,ambient R,gens A);
   commRelations := apply(subsets(gens A,2), s-> s_0*s_1+(-1)^(if isComm then -1 else 0)*s_1*s_0);
   extRelations := if isExter then apply(gens A, s -> s^2) else {};
   --- here is the defining ideal of the commutative algebra, inside the tensor algebra
   I := ideal (commRelations | extRelations | ((flatten entries gens ideal R) / phi));
   commIgb := gb ideal R;
   maxDeg := (((flatten entries gens commIgb) / degree) | {{0}}) / sum // max;
   maxDeg = max(2,maxDeg);
   Igb := NCGB(I, 2*maxDeg);
   A/I
)

validSkewMatrix = method()
validSkewMatrix Matrix := M -> (
   rows := numgens source M;
   cols := numgens target M;
   if rows != cols then return false;
   invOffDiag := all(apply(rows, i -> all(apply(toList(i..cols-1), j -> isUnit M_i_j and isUnit M_j_i and (M_j_i)^(-1) == M_i_j),b->b==true)),c->c==true);
   oneOnDiag := all(rows, i -> M_i_i == 1);
   invOffDiag and oneOnDiag
)

skewPolynomialRing = method()
skewPolynomialRing (Ring,Matrix,List) := (R,skewMatrix,varList) -> (
   if not validSkewMatrix skewMatrix then
      error "Expected a matrix M such that M_ij = M_ji^(-1).";
   if ring skewMatrix =!= R then error "Expected skewing matrix over base ring.";
   A := R varList;
   gensA := gens A;
   I := ideal apply(subsets(numgens A, 2), p -> 
            (gensA_(p#0))*(gensA_(p#1)) - (skewMatrix_(p#0)_(p#1))*(gensA_(p#1))*(gensA_(p#0)));
   B := A/I;
   B
)

skewPolynomialRing (Ring,ZZ,List) := 
skewPolynomialRing (Ring,QQ,List) := 
skewPolynomialRing (Ring,RingElement,List) := (R,skewElt,varList) -> (
   -- if class skewElt =!= R then error "Expected ring element over base ring.";
   A := R varList;
   gensA := gens A;
   I := ideal apply(subsets(numgens A, 2), p ->
            (gensA_(p#0))*(gensA_(p#1)) - promote(skewElt,R)*(gensA_(p#1))*(gensA_(p#0)));
   B := A/I;
   B
)

threeDimSklyanin = method(Options => {DegreeLimit => 10})
threeDimSklyanin (Ring, List, List) := opts -> (R, params, varList) -> (
   if #params != 3 or #varList != 3 then error "Expected lists of length 3.";
   if instance(varList#0, R) or instance(varList#0,ZZ) or instance(varList#0,QQ) then
      error "Expected list of variables in third argument.";
   A := R varList;
   gensA := gens A;
   I := ideal {params#0*gensA#1*gensA#2+params#1*gensA#2*gensA#1+params#2*(gensA#0)^2,
       params#0*gensA#2*gensA#0+params#1*gensA#0*gensA#2+params#2*(gensA#1)^2,
       params#0*gensA#0*gensA#1+params#1*gensA#1*gensA#0+params#2*(gensA#2)^2};
   Igb := NCGB(I, opts.DegreeLimit);
   B := A/I;
   B
)
threeDimSklyanin (Ring, List) := opts -> (R, varList) -> (
   if char R =!= 0 then error "For random Sklyanin, QQ coefficients are required.";
   threeDimSklyanin(R,{random(QQ),random(QQ), random(QQ)}, varList, opts)
)

fourDimSklyanin = method(Options => {DegreeLimit => 10})
fourDimSklyanin (Ring, List, List) := opts -> (R, params, varList) -> (
   if #params != 3 or #varList != 4 then error "Expected three parameters and four variables.";
   if instance(varList#0, R) or instance(varList#0,ZZ) or instance(varList#0,QQ) then
      error "Expected list of variables in third argument.";
   A := R varList;
   gensA := gens A;
   varList = gens A;
   f1 := (varList#0*varList#1 - varList#1*varList#0) - params#0*(varList#2*varList#3 + varList#3*varList#2);
   f2 := (varList#0*varList#2 - varList#2*varList#0) - params#1*(varList#3*varList#1 + varList#1*varList#3);
   f3 := (varList#0*varList#3 - varList#3*varList#0) - params#2*(varList#1*varList#2 + varList#2*varList#1);
   g1 := (varList#0*varList#1 + varList#1*varList#0) - (varList#2*varList#3 - varList#3*varList#2);
   g2 := (varList#0*varList#2 + varList#2*varList#0) - (varList#3*varList#1 - varList#1*varList#3);
   g3 := (varList#0*varList#3 + varList#3*varList#0) - (varList#1*varList#2 - varList#2*varList#1);

   I := ideal {f1,f2,f3,g1,g2,g3};
   Igb := NCGB(I, opts.DegreeLimit);
   B := A/I;
   B
)
fourDimSklyanin (Ring, List) := opts -> (R, varList) -> (
   if char R != 0 and char R < 100 then error "For random Sklyanin, QQ coefficients or characteristic larger than 100 are required.";
   --- generate a generic four dimensional Sklyanin that is AS regular
   alpha := random R;
   while (alpha == 0) do alpha = random R;
   beta := random R;
   while (beta == 0 or (1 + alpha*beta) == 0) do beta = random R;
   gamma := (-alpha-beta)/(1 + alpha*beta);
   fourDimSklyanin(R,{alpha,beta,gamma}, varList, opts)
)

oreIdeal = method(Options => {Degree => 1})
oreIdeal (Ring,RingMap,RingMap,RingElement) := 
oreIdeal (Ring,RingMap,RingMap,Symbol) := opts -> (B,sigma,delta,X) -> (
   -- This version assumes that the derivation is zero on B
   -- Don't yet have multiple rings with the same variables names working yet.  Not sure how to
   -- get the symbol with the same name as the variable.
   X = baseName X;
   kk := coefficientRing B;
   varsList := ((gens B) / baseName) | {X};
   A := ambient B;
   C := kk (varsList | {Degrees => (degrees A | {opts.Degree})});
   fromBtoC := map(C,B,drop(gens C, -1));
   fromAtoC := map(C,A,drop(gens C, -1));
   X = value X;
   ideal (apply(flatten entries gens ideal B, f -> fromAtoC promote(f,A)) |
            apply(gens B, x -> X*(fromBtoC x) - (fromBtoC sigma x)*X - (fromBtoC delta x)))
)

oreIdeal (Ring,RingMap,Symbol) := 
oreIdeal (Ring,RingMap,RingElement) := opts -> (B,sigma,X) -> (
   zeroMap := map(B,B,toList ((numgens B):0));
   oreIdeal(B,sigma,zeroMap,X,opts)
)

oreExtension = method(Options => options oreIdeal)
oreExtension (Ring,RingMap,RingMap,Symbol) := 
oreExtension (Ring,RingMap,RingMap,RingElement) := opts -> (B,sigma,delta,X) -> (
   X = baseName X;
   I := oreIdeal(B,sigma,delta,X,opts);
   C := ring I;
   C/I
)

oreExtension (Ring,RingMap,Symbol) := 
oreExtension (Ring,RingMap,RingElement) := opts -> (B,sigma,X) -> (
   X = baseName X;
   I := oreIdeal(B,sigma,X,opts);
   C := ring I;
   C/I
)

freeProduct = method()
freeProduct (Ring,Ring) := (A,B) -> (
   R := coefficientRing A;
   if R =!= (coefficientRing B) then error "Input rings must have same coefficient ring.";
   gensA := gens A;
   gensB := gens B;
   newgens := gensA | gensB;     
   if #unique(newgens) != (#gensA + #gensB) then error "Input rings have a common generator.";

   I := flatten entries gens ideal A;
   J := flatten entries gens ideal B;
   
   A' := ambient A;
   B' := ambient B;
    
   --- bring over heft vectors, etc, of the factors as well? 
   C := R (newgens | {Degrees => (degrees A | degrees B)});
   gensAinC := take(gens C, #gensA);
   gensBinC := drop(gens C, #gensA);
   incA := map(C,A',gensAinC);
   incB := map(C,B',gensBinC);
   IinC := I / incA;
   JinC := J / incB;
   newIdealGens := select( (IinC | JinC), x -> x !=0);       
   if newIdealGens == {} then C 
   else C/(ideal newIdealGens)
)

qTensorProduct = method()
qTensorProduct (Ring, Ring, ZZ) :=
qTensorProduct (Ring, Ring, QQ) :=
qTensorProduct (Ring, Ring, RingElement) := (A,B,q) -> (
   -- this is the q-commuting tensor product of rings
   F := freeProduct(A,B);
   R := coefficientRing A;
   q = promote(q,R);
   gensAinF := take(gens F, #gens A);
   gensBinF := drop(gens F, #gens A);   
   -- create the commutation relations among generators of A and B
   K := flatten apply(gensAinF, g-> apply( gensBinF, h-> h*g-q*g*h));

   if class F === FreeAlgebra then F/(ideal K)
   else (
      I := gens ideal F;
      C := ambient F;
      newI := ideal select( (I | matrix {K}), g -> g!=0);
      -- can we be clever with not re-computing GB here?
      C/newI
   )
)

quadraticClosure = method()
quadraticClosure Ring := B -> quadraticClosure toNCRing B

quadraticClosure FreeAlgebra :=
quadraticClosure FreeAlgebraQuotient := B -> (
   I := ideal B;
   J := quadraticClosure(I);
   A := ambient B;
   A/J
)

quadraticClosure Ideal := I -> (
   J := select(flatten entries gens I, g -> (sum degree g)<=2);
   ideal J
)

homogDual = method()
homogDual Ring := B -> homogDual toNCRing B

homogDual FreeAlgebra :=
homogDual FreeAlgebraQuotient := B -> (
   I := ideal B;
   J := homogDual(I);
   A := ring J;
   A/J
)

homogDual Ideal := I -> (
   if class ring I =!= FreeAlgebra then
      error "Expected an ideal in the tensor algebra.";
   d:=degree first flatten entries gens I;
   if not all(flatten entries gens I, g->(degree g)==d)
      then error "Expected a pure ideal.";
   A := I.ring;
   bas := ncBasis(d,A);
   mat := transpose last coefficients(gens I,Monomials=>flatten entries bas);
   mat = sub(mat, coefficientRing A);
   dualGens := bas*(gens ker mat);
   if dualGens == 0 then ideal {0_A} else ideal flatten entries dualGens
)

-*  Move this to the engine, or leave at top level?
oppositeElement = method()
oppositeElement NCRingElement := f -> (
   new (ring f) from hashTable {
          (symbol ring, f.ring),
          (symbol cache, new CacheTable from {}),
          (symbol terms, hashTable apply(pairs f.terms, p -> (
               newMon := new NCMonomial from {(symbol monList) => reverse p#0#monList,
                                              (symbol ring) => ring f};
               (newMon,p#1))))}
)

oppositeRing = method()
oppositeRing NCRing := B -> (
   gensB := gens B;
   R := coefficientRing B;
   oppA := R gensB;
   if class B === NCPolynomialRing then return oppA;
   idealB := gens ideal B;
   phi := ncMap(oppA,ambient B, gens oppA);
   oppIdealB := idealB / oppositeElement / phi // ncIdeal;
   oppIdealBGB := ncGroebnerBasis(oppIdealB, InstallGB=>not B#BergmanRing);
   oppA / oppIdealB
)
*-

-*
-- Make this work eventually
NCRing ** NCRing := (A,B) -> (
   qTensorProduct(A,B,promote(1,coefficientRing A))
)
*-

BUG ///
--- things to get fixed:
1) basis rather than ncBasis
2) Check that the type "Ring" inputs above are actually
either FreeAlgebra or FreeAlgebraQuotient

--- bringing over ring constructions
restart
needsPackage "AssociativeAlgebras"
--needsPackage "NCAlgebra"
kk = ZZ/32003
A = kk[x,y]
R = kk{a,b,c}
promote(kk^3, R)
promote(kk^3, A)

-- XXX
restart
needsPackage "AssociativeAlgebras"
kk = ZZ/32003
R = kk{a,b,c}
I = ideal(2*a*b + 3*b*a + 5*c^2,
             2*b*c + 3*c*b + 5*a^2,
             2*c*a + 3*a*c + 5*b^2)
gbTrace=2
I = ideal I_*; time NCGB(I, 23, Strategy=>16); 

I = ideal I_*; time NCGB(I, 20, Strategy=>16); 
I = ideal I_*; time NCGB(I, 20, Strategy=>0);

--- this is the matrix in degree 3 for the above computation after column sort, if
--- one would like to manipulate it for checking purposes.
M = map (kk^21, kk^27, { (0,1) => 1, (0,3) => 3/2, (0,8) => 5/2,
	                  (1,0) => 1, (1,5) => 2/5, (1,7) => 3/5,
			  (2,2) => 1, (2,4) => 5/3, (2,6) => 2/3,
			  (3,1) => 1, (3,3) => 3/2, (3,8) => 5/2,
			  (4,3) => 1, (4,9) => 3/2, (4,24) => 5/2,
			  (5,8) => 1, (5,14) => 5/3, (5,20) => 2/3,
			  (6,0) => 1, (6,15) => 2/5, (6,21) => 3/5,
			  (7,5) => 1, (7,11) => 3/2, (7,26) => 5/2,
			  (8,7) => 1, (8,13) => 5/3, (8,19) => 2/3,
			  (9,2) => 1, (9,17) => 2/5, (9,23) => 3/5,
			  (10,4) => 1, (10,10) => 3/2, (10,25) => 5/2,
			  (11,6) => 1, (11,12) => 5/3, (11,18) => 2/3,
			  (12,9) => 1, (12,14) => 2/5, (12,16) => 3/5,
			  (13,20) => 1, (13,22) => 5/3, (13,24) => 2/3,
			  (14,11) => 1, (14,13) => 5/3, (14,15) => 2/3,
			  (15,19) => 1, (15,21) => 3/2, (15,26) => 5/2,
			  (16,10) => 1, (16,12) => 3/2, (16,17) => 5/2,
			  (17,18) => 1, (17,23) => 2/5, (17,25) => 3/5,
			  (18,1) => 1, (18,16) => 2/5, (18,22) => 3/5,
			  (19,0) => 1, (19,15) => 2/5, (19,21) => 3/5,
			  (20,2) => 1, (20,17) => 2/5, (20,23) => 3/5})
M = mutableMatrix M

I = ideal I_*; NCGB(I, 10, Strategy=>0);
--I = ideal I_*; NCGB(I, 10, Strategy=>1); -- crash
I = ideal I_*; NCGB(I, 10, Strategy=>2); -- works
--I = ideal I_*; NCGB(I, 10, Strategy=>3); -- infinite loop
I = ideal I_*; NCGB(I, 10, Strategy=>4); -- works
I = ideal I_*; NCGB(I, 10, Strategy=>5); -- works
I = ideal I_*; NCGB(I, 10, Strategy=>6); -- works -- hmm, doesn't seem to work well

restart
needsPackage "AssociativeAlgebras"
R = QQ{a,b,c, Weights=>{{1,0,0},{0,1,0},{0,0,1}}}
I = ideal {a^2 - 1, b^2 - 1, c^2 - 1, a*b*a - b*a*b, b*c*b - c*b*c, a*c - c*a}
NCGB(I,10)

restart
needsPackage "AssociativeAlgebras"
R = QQ{a,b,c,d}
J = ideal{ a*c - 1, b*d - 1, a^2 - b^3, a^3 - b^5, a*c - c*a, b*d - d*b }
J' = ideal flatten entries NCGB(J,50)

S = R/I
centralElements(S,3)
T = skewPolynomialRing(ZZ/32003,-1,{x,y,z})
T = threeDimSklyanin(QQ,{x,y,z}, DegreeLimit => 10)
time T = fourDimSklyanin(QQ,{x,y,z,w},DegreeLimit => 8)
T = fourDimSklyanin(QQ,{x,y,z,w},DegreeLimit => 4)
T = fourDimSklyanin(ZZ/32003,{x,y,z,w},DegreeLimit => 4)
T = fourDimSklyanin(ZZ/32003,{x,y,z,w})

restart
needsPackage "AssociativeAlgebras"
gbTrace = 2
kk = QQ
kk = ZZ/32003
R = kk{x,y,z,w}
I = ideal {x*y-y*x-7*z*w-7*w*z, 3*x*z-4*y*w-3*z*x-4*w*y, 31*x*w+25*y*z+25*z*y-31*w*x, x*y+y*x-z*w+w*z, x*z+y*w+z*x-w*y, x*w-y*z+z*y+w*x}
time Igb = NCGB(I, 11, Strategy=>16); -- 12.0 seconds (with std::vector<int>)
time Igb = NCGB(I, 10); -- 12.3 seconds (with std::vector<int>)


time Igb = NCGB(I, 20, Strategy=>16);
time Igb = NCGB(I, 10);
S = R/I
#(flatten entries ncBasis(8,S)) == binomial(8+3,3)
flatten entries Igb / degree

-- the following seems wrong
T = fourDimSklyanin(ZZ/32003,{x,y,z,w}, DegreeLimit => 20);
ideal T
--- playing with ore extensions
R = QQ {x,Degrees=>{1}}
f = map(R,R,{-x})
S = oreExtension(R,f,y,Degree=>{2})
g = map(S,S,{-x,-y})
T = oreExtension(S,g,z,Degree=>{3})
-- free products
R1 = QQ {x,Degrees=>{1}}
R2 = QQ {y,Degrees=>{2}}
S = freeProduct(R1,R2)
T = qTensorProduct(R1,R2,-1)
--- homog dual
R1 = QQ[x,y]
S1 = homogDual R1
ideal S1
R3 = QQ[x,y]/ideal{x^2,x*y,y^2}
S3 = homogDual R3
ideal S3
--- getting promote to work
kk = ZZ/32003
A = kk[x,y]
B = A{z,w, DegreeRank=>2}
promote(A^{1,2,3}, B) -- this is not an optimal situation.  We need to allow DegreeMap...
assert(ring promote(kk^3, B) === B) -- fails at the moment.
///

beginDocumentation()

BENCHMARK = method()
BENCHMARK String := (s) -> null

doc ///
Key
  AssociativeAlgebras
Headline
  non-commutative polynomial algebra
Description
  Text
    This code is in active development.
  Example
    R = QQ{a..c}
    R_0
    R_1
    R_2
Caveat
  Not yet fully functional.
SeeAlso
///

doc ///
   Key
      normalElements
      (normalElements, FreeAlgebraQuotient, ZZ, Symbol, Symbol)
   Headline
      Finds normal elements
   Usage
      normalElements(A,n,x,y)
   Inputs
      A : FreeAlgebraQuotient
      n : ZZ
      x : Symbol
      y : Symbol
   Outputs
      : List
   Description
      Text
         Let b_1,...,b_n be a monomial basis for a noncommutative Ring A in degree d. We assume A
	 is generated by elements a_1,...,a_k of degree 1. A homogeneous element r in A
	 is normal if a_i*r is in the span of the r*a_j for all i.
      Text
         Using the input symbols x and y, we define the "normal variety" to be the 
	 set of common solutions to the equations  
	 x_j*a_i*b_j = y_j1*b_j*a_1+...+y_jk*b_j*a_k 
	 for all i and j. Saturating the ideal at each x_i we extract polynomial equations
	 the x_i must satisfy for the element x_1*b_1+...+x_n*b_n to be normal in A.
      Text
         Before computing the normal variety, this method checks for normal monomials
	 in degree n. These are returned first to reduce the complexity of the problem.
	 Then the method computes the variety and returns its components. The equations
         the method returns are given in terms of the indexed variable x. The indices are
	 basis monomials in degree n.
      Text
         The following example is a 3-dimensional Sklyanin algebra.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 ncBasis(2,B)
	 normalElements(B,2,r,s)
      Text
         The normal elements in degree 2 are x^2, y^2 and z^2. The basis
	 calculation shows x^2 and y^2 are normal forms in B. The normalElements
	 method first checks all basis monomials using @ TO isNormal @. In this case
	 it finds x^2 and y^2 are normal and returns this information. However,  
	 z^2 is not a normal form expression. The normal form of z^2 is x*y+y*x. In 
	 the second phase of the calculation, the method returns generators of the
	 ideal describing the normal elements (excluding the normal monomials). We
	 see the coefficients of basis monomials y*z and x*z must be 0 and the 
	 coefficients of x*y and y*x must be equal. The last equation identifies
	 z^2 = x*y+y*x as a normal element of degree 2.
      Example
         normalElements(B,3,t,u) 
	 g = -y^3-x*y*z+y*x*z+x^3
	 isCentral g
      Text
         In degree 3, there are no normal monomials. The function returns several equations
	 which determine the only normal element of degree 3 (up to scaling) is the central
	 element g.
///

doc ///
   Key
      (normalElements, RingMap, ZZ)
   Headline
      Finds elements normalized by a ring map
   Usage
      normalElements(f,n)
   Inputs
      f : RingMap
      n : ZZ
          a homogeneous degree in which to search for normal elements
   Outputs
      : Matrix
   Description
      Text
         A normal element x in a non-commutative ring R determines an automorphism f of R by
	 a*x=x*f(a). Conversely, given a ring endomorphism, we may ask if any x
	 satisfy the above equation for all a. 
      Text
         Given a ring map f and a degree n, this method returns solutions to 
	 the equations a*x=x*f(a) for all generators a of R.
      Example
         B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
	 sigma = map(B,B,{y,z,w,x})
	 C = oreExtension(B,sigma,a)
	 sigmaC = map(C,C,{y,z,w,x,a})
	 normalElements(sigmaC,1)
         normalElements(sigmaC,2)
         normalElements(sigmaC * sigmaC,2)
         normalElements(sigmaC * sigmaC * sigmaC, 3)
///

-- Turn this into a test?
BUG ///
      Example
         D = threeDimSklyanin(QQ, {a,b,c}) 
         normalElements(id_D, 3)
         assert(numColumns oo == 1)
         assert(normalElements(id_D, 2) == 0)
///

doc ///
   Key
      "Basic operations on noncommutative algebras"
   Description
      Text 
         The AssociativeAlgebras package contains a number of methods for studying noncommutative
	 rings - primarily graded rings. The following three extended examples 
	 highlight the capabilites of the package. For a detailed account of the
	 Groebner basis calculations underlying nearly all of these methods, see
	 @ TO "Using the Bergman interface" @.
      Text
         Our first example concerns a three-dimensional Sklyanin algebra. This example is
	 a PI-ring. We define the ring as a quotient of the tensor algebra on three
	 generators by the two-sided ideal generated by the three elements listed.
      Example
         A = QQ{x,y,z}
      Text
         Users familiar with Macaulay2 will recognize the notation for a ring of
	 noncommutative polynomials is identical to that for local rings, see
	 @ TO (symbol SPACE, Ring, List) @. Thus the
	 previous line does not generate an error if you forget to load the NCAlgebra package. 
      Example
	 f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
	 B = A/ideal{f,g,h}
      Text
         It is known that this algebra has a unique (up to rescaling) central element 
	 of degree 3. We can verify this claim computationally using @ TO centralElements @
	 and check that the element is regular to a given degree. See @ TO isLeftRegular @.
      Example
         centralElements(B,3)
	 j = y^3+x*y*z-y*x*z-x^3
	 isCentral j
	 apply(5,i->isLeftRegular(j,i+1))
      Text
         In fact, we can see that j is (up to scaling) the only normal element of degree 3.
	 See the discussion above for interpreting the output of @ TO normalElements @.
      Example
         normalElements(B,3,n,o)
	 ncBasis(3,B)
      Text
         The user can create noncommutative rings in ways other than specifying a
	 presentation. For our second example, consider a skew polynomial ring on four
	 generators, where generators skew-commute (but are not nilpotent). See
	 @ TO skewPolynomialRing @ for more details.
      Example
         C = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
      Text
         Let us briefly note that the user can also define a skew polynomial ring 
	 with coefficients in a commutative ring.
      Example
         R = QQ[q]/ideal{q^4+q^3+q^2+q+1}
	 B = skewPolynomialRing(R,q,{x,y,z,w})
	 x*y == q*y*x         
      Text
         Returning to the main example, we can define a graded Ore extension of C 
	 by specifying an automorphism.
	 The function @ TO map @ is used to define a ring map. Note that ring maps 
	 are linear and multiplicative by definition but are not assumed to be  well-defined. 
      Example
         use C
         sigma = map(C,C,{y,z,w,x})
	   -- isWellDefined sigma
      Text
         We form the Ore extension of C by sigma. See @ TO oreExtension @.
      Example         
         D = oreExtension(C,sigma,a)
      Text
         The new generator a is normal and regular in D. Regularity (on the left or right)
	 can be checked one homogeneous degree at a time. See @ TO isLeftRegular @. 
	 Thus a determines a graded automorphism f:D->D via a*r=f(r)*a.
      Example
         isNormal a
	 apply(5,i-> isLeftRegular(a,i+1))
         sigmaD = normalAutomorphism a
      Text
	 Given an automorphism, one can check to see which elements it normalizes
	 in any given degree.
      Example       
         normalElements(sigmaD,1)
	 normalElements(sigmaD,2)
      Text
         One can check for the presence of normal elements more generally. In our
	 example, since a is normal, a^2 will also be normal. It is the only normal
	 monomial of degree 2. A complete description of the normal elements in a
	 given degree is given by @ TO normalElements @. 
      Example
         normalElements(D,2,P,Q)
      Text
         Each component of the "normal variety" is a set of polynomial equations which must
	 be satisfied by the coefficients of the monomial basis for an element expressed
	 in that basis to be normal. In this case, the basis of D in degree 2 is
      Example
         ncBasis(2,D)	 
      Text
         The output of normalElements tells us that in order for a degree 2 element of D
	 to be normal, it must be an expresison in powers of the generators. The coefficients
	 of these powers must satisfy the six equations listed.
      Example
         isNormal (x^2+z^2-y^2-w^2)	 
      Text
         In Macaulay2, the user can define a polynomial ring to be commutative or 
	 skew-commutative (the exterior algebra). The user can convert these rings (and
	 their quotients) to a type compatible with the NCAlgebra package using 
         @ TO toNCRing @.
      Example
         E' = QQ[x,y,z,w,SkewCommutative=>true]
	 E = toNCRing E'
	 f = map(E,C,gens E)
	 use C
	 f x^2       
	 use E
	 x^2 == 0
///

--- end documentation

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"

  restart
  check "AssociativeAlgebras"

*-
  --- generators test
  debug Core -- for generatorSymbols
  R = QQ{a,b,c}; assert(R#generatorSymbols == splice {vars(0,1,2)})
  assert isWellDefined R
    
  R = QQ{a,b,c}; assert(R#generatorSymbols == splice {vars(0,1,2)})
  assert isWellDefined R

  R = QQ{a,b, x_1..x_3, c, y_1..y_4}
  assert(numgens R == 10)
  debugLevel = 1
  isWellDefined R

  R = QQ{{a,b,c},{d,e}}; assert(R#generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{(a,b,c),{d,e}}; assert(R#generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{(a,b,c),(d,e)}; assert(R#generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{b..f}; assert(R#generatorSymbols == splice {vars(1,2,3,4,5)})
  R = QQ{a,b,c}; assert(R#generatorSymbols == splice {vars(0,1,2)})
  R = QQ{x_1..x_100, y_1..y_100}; assert(numgens R == 200)
  debugLevel = 1
  isWellDefined R
///

TEST ///
  -- toExternalString
  -- toString
  -- expression
  -- net
  -- describe
  R = QQ{a,b, x_1..x_3, c, y_1..y_4}
  isWellDefined R
  unstack describe R === {"QQ {a, b, x , x , x , c, y , y , y , y }", 
                            "           1   2   3      1   2   3   4"}
  assert(net R === "R")
  assert(net R == net expression R)
  assert((depth describe R, height describe R, width describe R) == (1,1,40))
  assert(toString R === "R")
  assert(toExternalString R === "QQ {a, b, x_1, x_2, x_3, c, y_1, y_2, y_3, y_4}")
///

TEST ///
  --- equality
  R = QQ{a,b,c}
  assert(a != b)
  assert(a == a)
  assert(b*a + a*b + b*a == 2*b*a + a*b)
  assert(R_0 == a)
  f = a^2*b*a^2*b+a^3*b+a^2*b*a+2*a^2*b+a^2+2*a+1
  g = (a*a*b+a+1)*(a*a*b+a+1)
  assert(f == g)
  assert(f - g == 0)
///

TEST ///
  -- printing tests
  R = QQ{a,b,c}
  f = a^2*b*a^2*b+a^3*b+a^2*b*a+2*a^2*b+a^2+2*a+1
  g = (a*a*b+a+1)*(a*a*b+a+1)
  assert(toExternalString(f - g) == "0")
  assert(f == g)
  assert(f-g == 0)
  assert(net f == net expression f)
  assert(toString f === "a^2*b*a^2*b+a^3*b+a^2*b*a+2*a^2*b+a^2+2*a+1")
  assert(toString f === toExternalString f)
-*
  for i from 0 to 10 list (elapsedTime size (h = g^i));
  for i from 0 to 8 list (elapsedTime size (h = g^i))
  g1 = g;
  for i from 0 to 7 do elapsedTime (h = g1*g; g1 = h; print size h) 
  
  g1 = g;  
  for i from 0 to 7 do elapsedTime (h = g*g1; g1 = h; print size h)   
  apply(11, i -> print size elapsedTime(h = g^i));
*-
///

///
i3 :   g = (a*a*b+a+1)*(a*a*b+a+1)

      2   2     3     2        2     2
o3 = a b*a b + a b + a b*a + 2a b + a  + 2a + 1
  
31
     -- 0.000237961 seconds elapsed
127
     -- 0.000494012 seconds elapsed
511
     -- 0.00139882 seconds elapsed
2047
     -- 0.00770198 seconds elapsed
8191
     -- 0.0549404 seconds elapsed
32767
     -- 0.188868 seconds elapsed
131071
     -- 0.583566 seconds elapsed
524287
     -- 2.66607 seconds elapsed
///

BENCHMARK ///
-- this takes currently about 2 GB, so can't be run as a test
  R = QQ{a,b,c,d}
  g = a+b+c+d
  elapsedTime for i from 0 to 11 list (elapsedTime size (h = g^i))
  g3 = g^3
  g5 = g^5
  g8 = g^8;
  assert(g3*g5 == g8)
///

TEST ///
  R = QQ{a,b,c,d}
  g1 = a^2-b*c+c^3
  g2 = a*b*a+c*d*b+3*a*b*c*d
  g3 = a*b+b*c+c*d+d*a-1
  h1 = g1*g2
  h2 = g2*g3
  assert((g1*g2)*g3 == g1*(g2*g3))
  --g1*g2*g3*g2*g1*g3*g1*g3*g2*g1*g2*g1*g2;
  h1 = g1*g2*g3*g2*g1*g3;
  h2 = g1*g3*g2*g1*g2*g1; -- *g2;

  --elapsedTime for i from 0 to 11 list (elapsedTime size (h = g^i))
  g3 = g1^3
  g5 = g1^5
  g8 = g1^8;
  assert(g3*g5 == g8)
///

TEST ///
  needsPackage "AssociativeAlgebras"
  R = QQ{a,b,c,d}
  g1 = a^2-b*c+c^3
  g2 = a*b*a+c*d*b+3*a*b*c*d
  g3 = a*b+b*c+c*d+d*a-1
  h1 = g1*g2*g3*g2*g1;
  h2 = g1*g3*g2*g1*g2;
  size(h1)
  size(h2)
  h3 = elapsedTime(h1*h2); -- memory usage: elapsedTime: 2.59 sec
  assert(size h3 == 164025)
///

BENCHMARK ///
  R = QQ{a,b,c,d}
  g1 = a^2-b*c+c^3
  g2 = a*b*a+c*d*b+3*a*b*c*d
  g3 = a*b+b*c+c*d+d*a-1
  h1 = g1*g2*g3*g2*g1*g2;
  h2 = g1*g3*g2*g1*g2;
  size(h1)
  size(h2)
  h3 = elapsedTime(h1*h2); -- memory usage: 670 MB elapsedTime: 10.2 sec
  assert(size h3 == (size h1) * (size h2))
  assert(size h3 == 492075)
///

BENCHMARK ///
-- This one uses too much memory (about 1.7 GB)
  R = QQ{a,b,c,d}
  g1 = a^2-b*c+c^3
  g2 = a*b*a+c*d*b+3*a*b*c*d
  g3 = a*b+b*c+c*d+d*a-1
  h1 = g1*g2*g3*g2*g1*g2;
  h2 = g1*g3*g2*g1*g2*g1;
  size(h1)
  size(h2)
  h3 = elapsedTime(h1*h2); -- memory usage: 2.4 GB elapsedTime:  90.4 sec
  assert(size h3 == (size h1) * (size h2))
  assert(size h3 == 1476225)
///

BENCHMARK ///
-- this examples uses 2.2 GB
  R = QQ{a,b,c,d,e,f,g}
  G = a+b+c+d+e+f+g
  elapsedTime for i from 0 to 8 list (elapsedTime size (H = G^i))
///

--- question: why is engine code slower for this computation than NCAlgebra?
-- apply(11, i -> time(h = g^i));  -- SIGSEGV?

TEST ///
  --- promote/lift tests
  R = QQ{a,b,c}
  3_R
  assert(promote(3,R) == 3_R)
  assert(promote(23423/324,R) == (23423/324)_R)
  
  debug Core
  A = ZZ/101[s,t]
  B = A{x,y,z}
  promote(s,B)
  f = (s + x + y)^2
  (coeff, monoms) = rawPairs(raw A, raw f)
  peek first monoms

  A = ZZ/101[t]/t^2
  B = A{x,y,z}
  promote(t,B)
  t_B
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  debug Core
  -- basic arithmetic
  A = ZZ/101[t]/t^2
  B = A{x,y,z}
  f = 0_A * x
  raw f 
  
  f = (t*x + t*y)^2
  assert(toString raw f == "0")
  (x + t*y)^2 == x^2 + t*x*y + t*y*x 
  
  f = (t*x + t*y)^2
  f = (t*x + t*y)*(t*x+t*y)
  assert(size f == 0)
///

TEST /// 
  R = QQ{b,c,d}
  f = 3*b^2*c*b + 2*b^4
  assert(size (b+c) == 2)
  terms f == {2*b^4, 3*b^2*c*b}
  assert(# terms f == 2)
  assert(sum terms f == f)
///

TEST /// 
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{b,c,d}
  assert instance(R, FreeAlgebra)

  f = 3*b^2*c*b + 2*b^4
  assert(leadTerm f == 2*b^4)
  assert(leadCoefficient f == 2)
  assert(degree f == {4})
  assert(someTerms(f,0,2) == f)
  assert(leadMonomial f == b^4)
  assert(isHomogeneous f)

  g = b*c*b-b
  assert not isHomogeneous g
  
  A = QQ[a]
  B = A{b,c,d}
  f = 3*(a^2-a-1)*b^2*c*b + 2*(a^3-a-1)*b^4
  g = (a+2*b+3*c)^3
  assert(leadCoefficient f == 2*(a^3-a-1))
  assert(leadTerm f == 2*(a^3-a-1)*b^4)
  assert(someTerms(g,2,3) == 12*b*c*b + 18*b*c^2 + 12*c*b^2)
  assert(size g == 15)

  A = frac(QQ[a])
  B = A{b,c,d}
  f = 3/(a^2-a-1)*b^2*c*b + 2/(a^3-a-1)*b^4
  assert(leadCoefficient f == 2/(a^3-a-1))
  assert(leadTerm f == 2/(a^3-a-1)*b^4)
///

BUG ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ[u,v]
  S = R{x,y,z}
  -- TODO: need to incorporate the degree information of base when creating such a ring.
  f = u*x*y^2 + v^2*x*y*x
  NCGB(ideal f, 5) -- BUG!
  isHomogeneous f -- BUG!
  
  f = c*m*w1 + lot
  g = d*n*w2 + lot
  
  -- overlap s-pair
  w = pos; w1 = po; w2 = os
  m = m1g
  n = n1g
  the s-pair corresponding to this overlap is:
  d*n1*f*s - c*m1*p*g

  -- same monomial lead term s-pair  
  f = f1*w + lot
  g = g1*w + lot  
///

TEST /// 
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{b,c,d, Degrees=>{2,3,4}}
  degree b
  degree c
  degree d
  assert(degree(b*d*c) == {9})
  assert isHomogeneous(b^2-d)
  assert not isHomogeneous(b^2-c)

  R = QQ{b,c,d, Degrees=>{{1,0},{0,1},{3,-4}}, Heft=>{2,1}}
  degree b
  degree c
  degree d
  assert(degree(b*d*c) == {4,-3})
  assert isHomogeneous(c^4*d-b^3)
  assert(degree(c^4*d-b^3) == {3,0})
  assert not isHomogeneous(b^2-c)
  
  F = b^3 + c^4*d
  assert(leadTerm F == c^4*d);  -- default order is heft-graded, then word length, then lexicographic
  I = ideal"b3-c2dc2"
  assert isHomogeneous I
  NCGB(I, 10)
///

TEST ///
  R = QQ{a,b,c,d}
  {b*c}
  e = {{b,c,d,b*c,c*b,b^2,a*c-1}}
  M = matrix for j from 1 to 10 list for i from 1 to 10 list a*b-i*a-j*b
  M_(1,1)
  B = matrix {{b}}
  C = matrix {{c}}
  assert(B*C - matrix {{c*b}} == 0)
  D = matrix {{b,c}}
  assert(D * transpose D - matrix {{b^2 + c^2}} == 0)
  assert(transpose D * D - matrix {{b^2,c*b},{b*c,c^2}} == 0)
///

TEST ///
  R = QQ{b,c,d}
  M = R^2
  B = matrix {{b}}
  C = matrix {{c}}
  assert(B*C - matrix {{c*b}} == 0)
  N = mutableMatrix(R,2,3);
  N = mutableMatrix(R,2,3)
  N = mutableMatrix(R,100,200);
  N_(1,1)
  D = matrix {{b,c}}
  assert(D * transpose D - matrix {{b^2 + c^2}} == 0)
  assert((transpose D * D) - matrix {{b^2,c*b},{b*c,c^2}} == 0)
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-

  A = QQ[s,t]
  R = QQ{b,c,d}
  F = map(R,A,{b*c,d*c})
  G = map(A,R,{s,t,s*t})
  assert(G b == s)
  assert(G 3 == 3)
  F s
  assert(F (s*t) == d*c*b*c) -- Do we want to allow this? F is not well-defined. kind of a BUG!!
          
  F1 = map(R,R,{c,b,d})
  F1 (b*c*d + b*b*d*c*d*b)

  use R  
  F2 = map(R,R,{c+b,c,d})  
  F2(b+c+d)
  g = 3*b*c + b*c*b -2* b*d*b
  assert(F2 g == 3 * (b+c)*c + (b+c)*c*(b+c) -2* (b+c)*d*(b+c))
  
  B = QQ[b,c,d]
  H1 = map(R,B)
  H2 = map(B,R)
  use R
  H2 (b*c)
  
  R = QQ{b,c,d}
  a1 = (3/4)_R
  lift(a1,QQ) -- ok
  a2 = 3_R
  lift(a2,ZZ) -- ok
  promote(3/4, R)  -- ok
  
  A = ZZ/32003[t]/t^2
  B = A{x,y,z}
  promote(t_A, B) == t_B
  promote(3, B)
  assert(coefficientRing B === A)
  lift(t_B, A)
  
  kk = QQ
  A = kk[a]
  B = A[b]
  C = B{c,d}
  assert(lift(a_C, B) == a_B)
  assert(lift(a_C, A) == a_A)
  assert(try (lift(a_C, kk); false) else true)
///

TEST ///
  RingMap @@ RingMap := (f,g) -> (
      if target g =!= source f then error "Expected composable maps.";
      map(target f, source g, apply(gens source g, x -> f g x))
      )

  R = QQ{b,c,d}
  F1 = map(R,R,{c,b,d})
  F2 = map(R,R,{c+b,c,d})  
  F3 = map(R,R,{b*d-1, c*c-c, b-d})
  G = F1 @@ F2
  G2 = F2 @@ F3
  use R  

  F1 (b*c*d + b*b*d*c*d*b)  
  F2 (b*c*d + b*b*d*c*d*b)  
  F3 (b*c*d + b*b*d*c*d*b)  
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b,c,d}
  M = matrix{{a*b*c-2*a*a*b*a}}
  assert(monomials M == matrix{{a^2*b*a, a*b*c}})
  coefficients M
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  A = QQ[a..d]
  M = matrix{{a,b},{c,d}}
  monomials(M*M*M)
  R = QQ{a,b,c,d}
  M = matrix{{a*b+b*a, a*b+c*d, a*a+b*a}}
  monomials M
  coefficients M

  M = matrix{{a,b},{b,d}}
  M3 = M*M*M
  M6 = M3 | M3
  assert(monomials M6 == monomials M3)
  mons = monomials M6
  (mon,cf) = coefficients M6
  assert(mon*cf == M6)
  assert(mons == monomials mons)

  M = matrix{{a,b},{b,3*d-1}}
  M3 = M*M*M
  M6 = M3 | M3
  assert(monomials M6 == monomials M3)
  mons = monomials M6
  (mon,cf) = coefficients M6
  assert(mon*cf == M6)
  assert(mons == monomials mons)
///

TEST ///
  -- noncommutative reduction test
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a..d}
  I = ideal(a*b*a-a*c*b)
  I2 = ideal(a*b*a-a*c*b, d*a*c*b)
  I3 = ideal(a*b - b*a, a*c - c*a, a*d - d*a, b*c - c*b, b*d - d*b, c*d - d*c)
  J = ideal(a*b*a)
  K = ideal(a*c*b)
  L = ideal(a*b*d*c*a*d*b*c*a*b*d*c*c*c*d*b*a)
  debug Core
  map(R, rawNCReductionTwoSided(raw gens I, raw gens I))
  map(R, rawNCReductionTwoSided(raw gens I, raw gens J))
  map(R, rawNCReductionTwoSided(raw gens I, raw gens K))
  map(R, rawNCReductionTwoSided(raw gens I2, raw gens K))

f = a*a-b*c-a
g = NCReduction2Sided(a*f-f*a, ideal(f))
g = -a*b*c+b*c*a
h = a*g + f*b*c
-- TODO: Fix this!
NCReduction2Sided(h, ideal(f,g)) -- never never land
///

TEST ///
  -- noncommutative reduction test
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b}
  I = ideal(a^2 - b^2)
  gbTrace=3
  debug Core
  NCGB(I, 387)
///

TEST ///
-- test of free algebra quotient rings
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b}
  I = ideal(a^2 - b^2)
  A = R/I

  NCGB(I, 1000) 
  J = gens ideal NCGB(I, 1000)
  A1 = R/I
  assert(A1 =!= A)
  assert(I.cache.NCGB#0 == 1000)

-- i16 : coefficients(a^3)
-- stdio:16:1:(3): error: expected polynomial ring

-- i17 : lift(a^3, R)
-- stdio:17:1:(3): error: cannot lift given ring element

-- basis(4, A) -- error: can't handle this kind of ring.  
  use R
  terms(a^3)
  f = a^3
  use A
  terms(a^3)
  assert(promote(f, A) == b^2*a)
  assert(a == A_0)
  assert(b == A_1)
  assert(5 == # unique for e in (0,0,0,0)..(1,1,1,1) list product for i in e list A_i)
  assert(6 == # unique for e in (0,0,0,0,0)..(1,1,1,1,1) list product for i in e list A_i)

  R = QQ{a,b,c}
  I = ideal"aba-bab, ac-ca, ab+ba"
  J = gens ideal NCGB(I, 10)  

  -- 'monomials' seems to be working:
  assert(monomials matrix"ab-ba,ab+ba" == matrix"ab,ba")
  assert(numcols monomials J == 131)

  -- coefficients works over free algebras
  coefficients(a^3)
  elapsedTime (monoms, cfs) = coefficients(J, Monomials => monomials J);
  assert(monoms * cfs == J)  
  elapsedTime (monoms, cfs) = coefficients(J);
  assert(monoms * cfs == J)  

  -- monomials, coefficients, over quotients of free algebras.
  A = R/I  

  -- 'monomials' seems to be working:
  assert(monomials matrix"ab-ba,ab+ba" == matrix"ba")
  coefficients(a^3)
  assert(sub(J, A) == 0)
  M = matrix"ab-ba,ac-ba,aca-bab-a3"
  monomials M 
  elapsedTime (monoms, cfs) = coefficients(M, Monomials => monomials M);
  assert(monoms * cfs == M)  
  elapsedTime (monoms, cfs) = coefficients M;
  assert(monoms * cfs == M)  

  sub(M, R) -- TODO: should lift monomials as in the commutative case
  map(R,A) -- gives the 0 map
  map(A,R) -- ok
  sub(M, vars R) -- ok
  
  lift(M,R)
  phi = map(R,A,vars R)
  phi M
  
-- TODO
-*
  . raw A -- display quotient elements, for debugging purposes.
  . coefficients, monomials DONE (might need some more refactoring in c++ code).
  . basis
  . random
  . terms DONE (changed makeTerm to use M2FreeAlgebraOrQuotient)  
*-
///

TEST ///
-- test of basis of a quotient ring
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b}
  I = ideal(a^2 - b^2)
  NCGB(I, 1000)
  A = R/I
  assert(numcols ncBasis({10}, {10}, A) == 11) 
  ncBasis({500},{500},A); -- Duplicate large block deallocation?
  elapsedTime assert(numcols ncBasis({1000},{1000},A) == 1001)

  S = QQ{u,v,Degrees=>{2,3}}
  I = ideal(u*v + v*u)
  T = S/I
  assert(ncBasis({15},{15},T) == matrix{{v*u^6, v^3*u^3, v^5}})
///

TEST ///
-*
-- XXX
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b,c}
  R = (ZZ/32003){a,b,c}
  I = ideal(2*a*b + 3*b*a + 5*c^2,
             2*b*c + 3*c*b + 5*a^2,
             2*c*a + 3*a*c + 5*b^2)
  elapsedTime NCGB(I, 4);

  elapsedTime NCGB(I, 10);
  A = R/I
  assert(numcols ncBasis(0,A) == 1)
  assert(numcols ncBasis(1,A) == 3)
  assert(numcols ncBasis(2,A) == 6)
  assert(numcols ncBasis(3,A) == 10)
  assert(numcols ncBasis(4,A) == 15)
  assert(numcols ncBasis(5,A) == 21)
  assert(numcols ncBasis(6,A) == 28)
  assert(numcols ncBasis(10,A) == 66)

  -*  
  -- Did these in order, in same session, right after defining I (reason for speedup: almost certainly skype)
  elapsedTime NCGB(I, 20); -- best time so far: Map.  5.9 sec, at home it is 4.2 sec (same computer)... 
    -- 27/12/2019, Mike MBP: now 2.7 sec

  elapsedTime NCGB(I, 21); -- 9.8 sec, 6.9 sec at home, same computer, Map.
    -- 27/12/2019, Mike MBP: 4.4 sec

  elapsedTime NCGB(I, 22); -- 16.23 sec, 11.7 sec at home, same computer, Map.
    -- 27/12/2019, Mike MBP: 7.3 sec

  elapsedTime NCGB(I, 23); 
    -- 27/12/2019, Mike MBP: 12.2 sec
  *-
///

///
  -- magma code
  kk := Rationals();
  kk := FiniteField(32003);
  F<a,b,c> := FreeAlgebra(kk,3);
  B := [2*a*b + 3*b*a + 5*c^2,
             2*b*c + 3*c*b + 5*a^2,
             2*c*a + 3*a*c + 5*b^2];
  I := ideal<F | B>;
  # GroebnerBasis(B,20);
  
  kk := Rationals();
  kk := FiniteField(32003);
  F<x,y,z,w> := FreeAlgebra(kk,4);
  B := [x*y-y*x-7*z*w-7*w*z, 3*x*z-4*y*w-3*z*x-4*w*y, 31*x*w+25*y*z+25*z*y-31*w*x, x*y+y*x-z*w+w*z, x*z+y*w+z*x-w*y, x*w-y*z+z*y+w*x];
  I := ideal<F | B>; 
  Igb := GroebnerBasis(B,10); 
///

TEST ///
-*
  restart
  debug needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b,c, Degrees=>{{1,0,0},{0,1,0},{0,0,1}}}
  assert(degree a == {1,0,0})
  assert(degree (a^2 + b^2 + c^2) == {2,2,2})
  assert not isHomogeneous (a^2 + b^2 + c^2)
  assert isHomogeneous a^2
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  -- note that variables in the base of a FreeAlgebra commute
  -- with the variables adjoined.  I.e. QQ{x}{y} is the same as QQ[x,y]
  R = QQ[x,y]/ideal{x^2,x*y,y^2}
  S = R{a,b}
  T = S{c,d}
  assert(a*c == c*a)
  assert(x*c == c*x)
  f = x*c + y*d
  assert(f^2 == 0)
  assert(x*f == 0)
  assert(numcols ncBasis(2,S) == 4)
  assert(numcols ncBasis(2,T) == 4)
  assert(numcols ncBasis(0,S) == 1)
  assert(ncBasis(-1,S) == 0)
  g = (a*c + b*d)^2
  assert(#(terms g) == 4)
///  

--- bugs 2/20/2020


TEST ///
-*
  restart
  debug needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b,c,t, Weights=>{{1,1,1,0}}}
  I = ideal {a*b - c*t, b*c - a*t, c*a - b*t, a*t - t*a, b*t - t*b, c*t - t*c}
  J2 = NCGB(I,2) 
  J3 = NCGB(I,3)
  J4 = NCGB(I,4)
  I2 = ideal J2 + ideal {a^2-c^2,b^2-c^2,c^2*b - t*a*c, a*c^2 - t*c*b, b*a^2-t*a*c, c^3 - t*b*a, c*b^2 - t*b*a}
  J4 = NCGB(I2,4)
  I3 = ideal J4_(toList(0..10)) + ideal {a*c*b - b*a*c, b*a*c - c*b*a}
  J4 = NCGB(I3,4)
  J5 = NCGB(I3,5)
  J6 = NCGB(I3,6)
  compress sub(J6, {t => 1}) -- looks like it is working :)
  
  R = QQ{a,b,Degrees=>{2,3}}
  assert(leadTerm (a+b) == b)  -- should be b
  assert(leadTerm (a^3 + b^2) == a^3)-- should be a^3 (which it is)

  R = QQ{a,b,Degrees=>{2,3}, Weights=>{{1,0},{0,1}}}
  -- The following two ring definitions are supposed to give errors.
  assert try (R = QQ{a,b,Degrees=>{2,3}, Weights=>{{1,0},{0,1,1}}}; false) else true
  assert try (R = QQ{a,b,Degrees=>{2,3}, Weights=>{{-1,0},{0,-1}}}; false) else true
///

TEST ///
-*
  restart
  debug needsPackage "AssociativeAlgebras"
*-
R = QQ{a,b,c,x,y, Degrees => {3,3,2,1,1}, Weights => {{0,0,0,1,1}} }
I = ideal{x*y - c, x*y*x-a, y*x*y-b}
isHomogeneous I
assert(degrees source gens I === {{2},{3},{3}})
M1 = gens I
J = NCGB(I,3) 
J = NCGB(I,20)
M2 = I.cache.NCGB#1
J1 = ideal (ideal M1)_*
J2 = ideal (ideal M2)_*
assert(NCGB(J1, 20) == NCGB(J2, 20)) -- note: NCGB J2 seems correct.

J = NCGB(I, 6)
assert isHomogeneous J
assert(NCReduction2Sided(x*y*x*y*x, ideal J) == c*a)
///

TEST /// 
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{b,c}
  I = ideal"bc"
  assert(NCGB(I, 10) == matrix{{b*c}})
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
R = QQ{x,y}
I = ideal {x^2-y^2}
S = R/I
gbS = NCGB(ideal S)
debug Core
rawNCBasis(raw gbS,{500},{500},-1);
rawNCBasis(raw gbS,{1000},{1000},-1);
///

TEST ///
-*
restart
needsPackage "AssociativeAlgebras"
*-
A = QQ[x,y]
R = A{b,c,d}
f = 3*x*y*b^2*c*b + 2*b^4
assert(leadMonomial f == b^4)
assert(ring leadCoefficient f === A)
assert(leadCoefficient f == 2_A)
assert(leadTerm f == 2*b^4)
g = f - 2*b^4
assert(leadMonomial g == b^2*c*b)
assert(ring leadCoefficient g === A)
assert(leadCoefficient g == 3*x*y)
assert(leadTerm g == 3*x*y*b^2*c*b)
assert(leadMonomial 0_R == 0_R)
///

-- Toying with adding FreeMonoids

FreeMonoid = new Type of Monoid

FreeMonoidElement = new Type of HashTable
FreeMonoidElement.synonym = "free monoid element"
new FreeMonoidElement from RawRingElement := (FreeMonoidElement, f) -> hashTable{ symbol RawRingElement => f }
raw FreeMonoidElement := x -> x.RawRingElement

freeMonoid = method()
freeMonoid FreeAlgebra := R -> (
    M := new FreeMonoid of FreeMonoidElement;
    M.vars = M.generators = apply(numgens R, i -> new M from raw R_i);
    M * M := (x,y) -> new M from x.RawRingElement * y.RawRingElement;
    M
)

end----------------------------------------------------------

-- making a new type of monoid for noncommutative algebras.
restart
debug Core
debug needsPackage "AssociativeAlgebras"
R = QQ{x,y}
M = freeMonoid R
M_0
M_1
M_1*M_0

restart
uninstallPackage "AssociativeAlgebras"
restart
needsPackage "AssociativeAlgebras"
installPackage "AssociativeAlgebras"
viewHelp "AssociativeAlgebras"
restart
check "AssociativeAlgebras"
  -- 21 Feb 2020: 2 tests fail (and all unit tests in NCGroebnerTest pass too):
  --   leadMonomial, and negative Weights in ring def.

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

-- XXX
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
ncBasis(2,B)
normalElements(B,2,r,s)
normalElements(B,3,t,u) 
g = -y^3-x*y*z+y*x*z+x^3
isCentral g

--- XXX
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
A = ambient B
use A
I = ideal B
x^3 % I
use B
phi = map(B,B,{y,z,x})
isWellDefined phi

--- XXX
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
A = ambient B
g = 2*(-y^3-x*y*z+y*x*z+x^3)
J = (ideal B) + ideal {g}
B' = A/J -- Factor of sklyanin
k = matrix {{x,y,z}}
BprimeToB = map(B,B',gens B) -- way to lift back from B' to B
M = BprimeToB rightKernelBergman rightKernelBergman k  -- second syzygy of k over B

-- Issues to talk to Mike about 6/23/2020.
-- Move these into the package once discussed.
TEST ///
-- 1. change NCGB to gb to get some things to work? (e.g. reduction via %, etc)
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
A = ambient B
use A
I = ideal B
x^3 % I -- fails
///
TEST ///
-- 2. Possible to change name from ncBasis to basis?
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
basis(3,B)
///
TEST ///
-- 3. Double curly braces for associative algebras?
restart
needsPackage "AssociativeAlgebras"
A = QQ{{x,y,z}}
assert(instance(A,FreeAlgebra))
B = QQ{x,y,z}
assert(instance(B,PolynomialRing))
///
TEST ///
-- 4. isWellDefined broken for maps of nc rings.
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
phi = map(B,B,{y,z,x})
isWellDefined phi
///
TEST ///
-- 5. kernel broken for maps of nc rings (but we can make a product order which will compute it).
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
phi = map(B,B,{y,z,x})
ker phi
///
TEST ///
-- 6. ** doesn't work right for nc rings.
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
C = B ** B
///
TEST ///
-- 7. "//" doesn't work for matrices over nc rings.  Need left/right ncgbs.
-- no test available just yet.
-- 8. Documentation!!! Continue to bring over working documentation nodes from NCAlgebra
-- 9. Opposite Ring and element don't work yet.
-- 10. Change toM2Ring to 'abelianization' or some such
///

-- Some things not bringing over yet:
-- 1. Hom_R(M,M) as a nc ring for R a commutative ring.  My code for computing
--    the defining ideal is all at top level and is slow.  It should be moved
--    into the engine.
-- 2. graded pieces of Hom_R(M,M) for M a graded `module' (note that (left/right/bi)
--    modules over an nc ring do not yet exist, only as presentation matrices.
--    current code is once again at top level and rather slow.

-- Still to do by Frank:
-- 1. Port over bergman interface for testing purposes
-- 2. Documentation nodes!
