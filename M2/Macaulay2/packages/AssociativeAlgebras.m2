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
        Headline => "Non-commutative algebra",
        DebuggingMode => true,
	AuxiliaryFiles => true
        )

--- FM: Functionality to still bring over from NCAlgebra:
---    1. basis of an ideal (this should be in the engine)
---       Also do this for left/right ideals when those arrive
---    2. Derivation option for maps (used in Ore extensions)

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
    "toFreeAlgebraQuotient",
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

--- debugging/benchmark tools
BUG = str -> ()
BENCHMARK = method()
BENCHMARK String := (s) -> null

-- we are having trouble with the following line, due to the fact
-- that RawRing is a class, but we would like to reference the symbol in the
-- local hash table.
RawRing = Core#"private dictionary"#"RawRing"

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

-- TODO: should make this work in the multigraded situations as well
leftMultiplicationMap = method()
leftMultiplicationMap(RingElement,ZZ) := (f,n) -> (
   B := ring f;
   -- is this what we want in the multigraded case?
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
      retVal = sub(last coefficients(matrix{apply(fromBasis, g -> f*g)},Monomials=>toBasis), R);
      retVal
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
      retVal = sub(last coefficients(matrix{apply(fromBasis, g -> g*f)}, Monomials=>toBasis), R);
      retVal
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
   n := first degree f;  -- assumes singly graded!
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
-- needs to be fixed
isNormal = method()
isNormal RingElement := f -> (
   if not isFreeAlgebraOrQuotient(ring f) then error "Expected an element in a noncommutative ring.";
   if not isHomogeneous f then error "Expected a homogeneous element.";
   all(gens ring f, x -> findNormalComplement(f,x) =!= null)
)

normalElements = method()
normalElements(FreeAlgebraQuotient, ZZ, Symbol) := Sequence => (R,n,x) -> (
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
   --   << "Normal monomials of degree " << n << ":" << endl;
   --   for i from 0 to ((length normalBasis) - 1) do (
   --     << normalBasis#i << endl;
   --   );
   --   if (length normalBasis)==0 then << "none" << endl;
   numGens := numgens R;
   leftMaps := apply(gens R, x->leftMultiplicationMap(x,nonNormalBasis,toBasis));
   rightMaps := apply(gens R, x->rightMultiplicationMap(x,nonNormalBasis,toBasis));
   -- make a polynomial ring with (fromDim) - (number of normal monomials)  + (numgens R)^2 variables
   -- need to hide variables from user
   y := getSymbol "yy";
   xvars := apply(nonNormalBasis, i->x_i);
   yvars := table(numGens, numGens, (i,j) -> y_(i,j));
   retRing := kk (monoid[xvars]);
   cRing := kk (monoid[(flatten yvars) | xvars,MonomialOrder=>Eliminate numGens^2]);
   phi := map(retRing, cRing, toList(numGens^2 : 0_retRing) | gens retRing);
   yvars = apply(numGens^2, i -> cRing_i);
   xvars = apply(#nonNormalBasis, i -> cRing_(numGens^2 + i));
   leftCoeff := matrix {apply(leftMaps, l -> l * transpose matrix {xvars})};
   rightCoeff := matrix {apply(rightMaps, r -> r * transpose matrix {xvars})};
   yAut := genericMatrix(cRing,numGens,numGens);
   I := ideal (rightCoeff - leftCoeff * yAut);
   -- the next line throws away information, including automorphism data
   retVal := unique(select(apply(xvars, x-> (
                           J:=saturate(I,ideal(x));
                           if J==cRing then 0 else selectInSubring(1, gens gb J)
                           )),c->c!=0));
   --- normalBasis are the elements of the basis that are normal
   --- and we place the answers in the ring without the y's in the second
   --- entry of the sequence
   (normalBasis,apply(retVal, J -> phi J))
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

toFreeAlgebraQuotient = method()
toFreeAlgebraQuotient Ring := FreeAlgebraQuotient => R -> (
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

-- This code will be useful once we can handle derivations properly.
-- as it stands, it will still run but not evaluate the derivation properly.
oreIdeal = method(Options => {Degree => 1})
oreIdeal (Ring,RingMap,RingMap,RingElement) := 
oreIdeal (Ring,RingMap,RingMap,Symbol) := opts -> (B,sigma,delta,X) -> (
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
-- TODO: do we want to return the commutative version of this?
--quadraticClosure Ring := B -> quadraticClosure toFreeAlgebraQuotient B

quadraticClosure FreeAlgebra :=
quadraticClosure FreeAlgebraQuotient := B -> (
   I := ideal B;
   J := quadraticClosure(I);
   A := ambient B;
   A/J
)

quadraticClosure Ideal := I -> (
   if not isHomogeneous I then error "Expected a homogeneous ideal.";
   -- TODO: is the sum below really what we want to do in the multigraded setting?
   J := select(flatten entries gens I, g -> (sum degree g)<=2);
   ideal J
)

homogDual = method()
-- TODO: do we want to return the commutative version of this?
-- homogDual Ring := B -> homogDual toFreeAlgebraQuotient B

homogDual FreeAlgebra :=
homogDual FreeAlgebraQuotient := B -> (
   I := ideal B;
   J := homogDual(I);
   A := ring J;
   A/J
)

homogDual Ideal := I -> (
   if class ring I =!= FreeAlgebra then
      error "Expected an ideal in a free algebra.";
   d:=degree first flatten entries gens I;
   if not isHomogeneous I then error "Expected a homogeneous ideal.";
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

--- load the tests
load "./AssociativeAlgebras/tests.m2"

--- load the documentation
beginDocumentation()
load "./AssociativeAlgebras/doc.m2"

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
