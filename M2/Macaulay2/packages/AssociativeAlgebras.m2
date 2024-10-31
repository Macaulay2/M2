newPackage(
    "AssociativeAlgebras",
    Version => "0.9", 
    Date => "23 Oct 2024",
    Authors => {{Name => "Frank Moore", 
	   Email => "moorewf@wfu.edu",
	   HomePage => "https://math.wfu.edu/moore"},
	{Name => "Mike Stillman",  
            Email => "mike@math.cornell.edu", 
            HomePage => "http://www.math.cornell.edu/~mike"}
        },
    Headline => "Noncommutative algebra",
    Keywords => {"Noncommutative Algebra"},
    PackageImports =>{"Complexes"},
    PackageExports =>{"IntegralClosure"},
    AuxiliaryFiles => true
    )

-- TODO for tbb update: (Dec 2021)
--   configure, cmake: need to set MTBB_VERSION to 2021, 2020, 0, etc.
--     for 0, maybe we need a configure option --disable-tbb.
--   testing=on: for cmake build, cannot compile tests at the moment
--      for both: we commented out tests from mathicgb, need to reinstate them.
--   run multithreaded tests for NCF4 and mgb to make sure they are actually getting speedups.
--   add in the arena stuff? Jay's pull request to mathicgb
--      (We would need to make the same changes for M2 usage, i.e. NCF4).
 
---- Warning!!  This package is a current work in progress.
---- The interface will change (even basic things like Groebner bases and the basis command)
---- and more functionality is expected to be added.

--- FM: Functionality to still bring over from NCAlgebra:
---    1. basis of an ideal (this should be in the engine)
---       Also do this for left/right ideals when those arrive
---    2. Derivation option for maps (used in Ore extensions)
---    3. 'Force' a GB as valid on the front end.
---    4. FIXED 'support' of a monomial doesn't work right
---    5. FreeProduct and Tensor Products not yet functional (flatten rings need work)

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
-- major computations to be written yet:
--  Hilbert function (is needed)
--  inhomog GB's.
--  one-sided GB's in modules.
--  modules (bi-modules)
-- get unit tests working again.  Also tests in the package.
-- connect unit tests to autotools build

export {
    "freeAlgebra",
    "ncBasis",
    "NCGB", -- uugh: change name!
    "NCReductionTwoSided",
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
    "freeProduct", -- not added yet
    "qTensorProduct", -- not added yet
    "toFreeAlgebraQuotient",
    "toCommRing",
    "isFreeAlgebraOrQuotient",
    "homogDual",
    "quadraticClosure",
    "oppositeRing",  
    "isLeftRegular",
    "isRightRegular",
    "isCentral",
    "normalAutomorphism",
    "ncHilbertSeries",
    "toRationalFunction",
    "UseVariables",
    "ncKernel",
    "ncGraphIdeal",
    "endomorphismRingIdeal",
    "extAlgebra",
    "leftQuadraticMatrix",
    "rightQuadraticMatrix",
    "pointScheme",
    "lineSchemeFourDim",
    "ncMatrixMult",
    "rightKernel",
    "Derivation",
    "derivation",
    "superpotential",
    "nakayamaAut",
    "isSuperpotential",
    "derivationQuotientIdeal",
    "derivationQuotient"
    -- "forceNCBasis"  -- not exported yet
    }

-- types from Core
exportFrom_Core {"FreeAlgebra", "FreeAlgebraQuotient"}

-- symbols into hash table for ring
importFrom_Core {"generatorSymbols","generatorExpressions",
    "liftDegree","promoteDegree","indexStrings","indexSymbols"}

-- local functions in the commutative ring code from core that are needed
importFrom_Core {"findSymbols","ReverseDictionary","makepromoter",
    "processWeights","hasAttribute","getAttribute",
    "processDegrees","commonEngineRingInitializations"}

-- raw functions for the engine interface
importFrom_Core{"raw","rawPairs","rawQuotientRing","rawGetTerms",
    "rawSparseListFormMonomial","rawNCFreeAlgebra",
    "rawNCBasis","rawNCReductionTwoSided","rawNCGroebnerBasisTwoSided",
    "RawRingElement","rawPromote"}

--- debugging/benchmark tools
BUG = str -> ()
DEVELOPMENT = str -> ()
BENCHMARK = method()
BENCHMARK String := (s) -> null

-- we are having trouble with the following line, due to the fact
-- that RawRing is a class, but we would like to reference the symbol in the
-- local hash table.
RawRing = Core#"private dictionary"#"RawRing"

FreeAlgebra.synonym = "free algebra"
FreeAlgebraQuotient.synonym = "quotient of a free algebra"

new FreeAlgebra from List := (EngineRing, inits) -> new EngineRing of RingElement from new HashTable from inits

getNameIfAny = (k) -> expression if hasAttribute(k,ReverseDictionary) then getAttribute(k,ReverseDictionary) else k

-- MES.  do we really need all 5 of these?
expression FreeAlgebra := R -> (
     if hasAttribute(R,ReverseDictionary) then return expression getAttribute(R,ReverseDictionary);
     k := last R.baseRings;
     (getNameIfAny k) new AngleBarList from (R#generatorSymbols)
     )
net FreeAlgebra := R -> (
     if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
     else net expression R)
describe FreeAlgebra := R -> (
     k := last R.baseRings;
     net (getNameIfAny k) | net (new AngleBarList from R#generatorSymbols)
     )
toExternalString FreeAlgebra := R -> (
    --toString describe R
     k := last R.baseRings;
     toString (getNameIfAny k) | toString(new AngleBarList from R#generatorSymbols)
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
freeAlgebra(Ring, BasicList) := FreeAlgebra => (A, args)  -> (
   -- get the symbols associated to the list that is passed in, in case the variables have been used earlier.
   opts := new OptionTable from {Degrees => null, DegreeRank => null, Weights => {}, Heft => null, UseVariables => false};
   (opts,args) = override(opts,toSequence args);
   --- for some reason, override returns the elt itself if there is only one variable
   varList := if class args === Sequence then args else {args};
   if not (A.?Engine and A.Engine) then
       error "expected coefficient ring handled by the engine";
   varSymbols := findSymbols toSequence varList;
   if #varSymbols == 0 then error "Expected at least one variable.";
   (degs,degrk,group) := processDegrees( opts.Degrees, opts.DegreeRank, null -* opts.DegreeGroup *-, length varSymbols);
   -- now check correctness of weights, flatten them
   wtvecs := combineWeightsAndDegree(#varSymbols, opts.Weights, degs);
   heftvec := if opts.Heft =!= null then opts.Heft
       else splice {degrk: 1};
   -- check that we have a valid heft vector.
   if not checkHeft(degs, heftvec) then error "expected a valid Heft vector, which dots positively with each degree vector";
   rawR := rawNCFreeAlgebra(raw A, toSequence(varSymbols/toString), raw degreesRing degrk, flatten degs, flatten wtvecs, heftvec);
   R := new FreeAlgebra from {
       RawRing => rawR,
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
   R.generators = for i from 0 to #varSymbols-1 list new R from R#RawRing_i;
   R.generatorSymbols     = varSymbols;
   R.generatorExpressions = apply(varSymbols, v -> if instance(v, Symbol) then v else expression v);
   R.index        = hashTable apply(R.generatorSymbols, 0 ..< #varSymbols, identity);
   R.indexSymbols = hashTable join(
       apply(if A.?indexSymbols then pairs A.indexSymbols else {},
	   (sym, x) -> sym => new R from rawPromote(raw R, raw x)),
       apply(R.generatorSymbols, R.generators, identity)
       );
   try R.indexStrings = applyKeys(R.indexSymbols, toString);
   R#promoteDegree = makepromoter degreeLength R;
   R#liftDegree = makepromoter degreeLength R;
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
   if opts#UseVariables then use R;
   R
   );

Ring AngleBarList := Ring => (R,variables) -> (
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

generators FreeAlgebra := opts -> R -> (
            if opts.CoefficientRing === null then R.generators
            else if opts.CoefficientRing === R then {}
            else join(R.generators, generators(coefficientRing R, opts) / (r -> promote(r,R))))

generators FreeAlgebraQuotient := opts -> (S) -> (
            if opts.CoefficientRing === S then {}
            else apply(generators(ambient S,opts), m -> promote(m,S)))
	
setNCGBStrategy := stratStr -> (
   if stratStr == "F4" then 16
   else if stratStr == "F4Parallel" then 48
   else if stratStr == "Naive" then 0
   else error "Unknown NCGB strategy provided."
)

NCGB = method(Options => {Strategy=>"F4Parallel"})
NCGB(Ideal, ZZ) := opts -> (I, maxdeg) -> (
    if I == 0 then return gens I;
    strat := opts#Strategy;
    if not I.cache.?NCGB or I.cache.NCGB#0 < maxdeg then (
	tobecomputed := raw if I.cache.?NCGB then I.cache.NCGB#1 else compress gens I;
	possField := ZZ/(char ultimate(coefficientRing, ring I));
	f4Allowed := (possField === (coefficientRing ring I)); -- or instance(coefficientRing ring I, GaloisField) or coefficientRing ring I === QQ;
	if not isHomogeneous I or (not f4Allowed and (strat == "F4" or strat == "F4Parallel")) then (
	   -- need to change to naive algorithm if I is not homogeneous at this point.
	   << "Warning:  F4 Algorithm not available over current coefficient ring or inhomogeneous ideal." << endl;
           -- if isHomogeneous I then strat = "F4" else strat = "Naive";
	   strat = "Naive";
	   << "Converting to " << strat << " algorithm." << endl;
	);
    	gbI := map(ring I, rawNCGroebnerBasisTwoSided(tobecomputed, maxdeg, setNCGBStrategy(strat)));
        I.cache.NCGB = {maxdeg, gbI};
        );
    I.cache.NCGB#1
    )
NCGB Ideal := opts -> (I) -> (
    if I == 0 then return gens I;
    if I.cache.?NCGB then I.cache.NCGB#1
    else (
        maxdegI := max((degrees source gens I) / sum); -- TODO: change once multidegrees are allowed.
        NCGB(I, 2*maxdegI, opts)
        )
    )

NCReductionTwoSided = method()
NCReductionTwoSided(Matrix, Matrix) := (M, I) -> (
    R := ring M;
    if R =!= ring I then error "expected same ring";
    map(R, rawNCReductionTwoSided(raw M, raw I))
    )
NCReductionTwoSided(Matrix, Ideal) := (M, I) -> NCReductionTwoSided(M, gens I)
NCReductionTwoSided(Matrix, List) := (M, L) -> NCReductionTwoSided(M, ideal L)
NCReductionTwoSided(RingElement, Ideal) := (F, I) -> (NCReductionTwoSided(matrix{{F}}, gens I))_(0,0)
NCReductionTwoSided(RingElement, Matrix) := (F, M) -> (NCReductionTwoSided(matrix{{F}}, M))_(0,0)
NCReductionTwoSided(RingElement, List) := (F,L) -> NCReductionTwoSided(F, ideal L)

FreeAlgebra / Ideal := FreeAlgebraQuotient => (R,I) -> (
    freeAlgebraQuotient(R,I,NCGB I)
)

freeAlgebraQuotient = method()
freeAlgebraQuotient (FreeAlgebra, Ideal, Matrix) := FreeAlgebraQuotient => (R,I,Igb) -> (
     -- this function uses the matrix passed in as Igb as the GB
     if ring I =!= R then error "expected ideal of the same ring";
     if I == 0 then return R;
     A := R;
     while class A === QuotientRing do A = last A.baseRings;
     gensI := generators I;
     -- changed default strategy to F4
     S := new FreeAlgebraQuotient from rawQuotientRing(raw R, raw Igb);
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
     if R#?index        then S#index = R#index;
     if R#?indexStrings then S#indexStrings = applyValues(R#indexStrings, x -> promote(x,S));
     if R#?indexSymbols then S#indexSymbols = applyValues(R#indexSymbols, x -> promote(x,S));
     expression S := lookup(expression,R);
     S.use = x -> ( -- what is this for??
	  );
     S
)

ncBasis = method(Options => {Strategy => "F4Parallel", Limit => infinity})
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
    gbR := if #hi == 1 then NCGB(ideal R, hi#0, Strategy => opts.Strategy) else NCGB(ideal R, Strategy => opts.Strategy);
    result := map(ring gbR, rawNCBasis(raw gbR, lo, hi, limit));
    map(R^1,, promote(result, R))
    )

ncBasis(List,Ring) := opts -> (d,R) -> ncBasis(d,d,R,opts)
ncBasis(ZZ,Ring) := opts -> (d,R) -> ncBasis(d, d, R, opts)
ncBasis(ZZ,ZZ,Ring) := opts -> (lo,hi,R) -> ncBasis({lo}, {hi}, R, opts)
ncBasis(InfiniteNumber,ZZ,Ring) := opts -> (lo,hi,R) -> ncBasis(lo, {hi}, R, opts)
ncBasis(ZZ,InfiniteNumber,Ring) := opts -> (lo,hi,R) -> ncBasis({lo}, hi, R, opts)
ncBasis Ring := opts -> R -> ncBasis(-infinity, infinity, R, opts)

forceNCBasis = method()
forceNCBasis (ZZ,Ring) := (d,R) -> (
   gbR := gens ideal R;
   L := ambient R;
   result := map(L, rawNCBasis(raw gbR, {d}, {d}, -1));
   map(R^1,,promote(result,R))
)
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
      retVal = sub(last coefficients(f*(matrix{fromBasis}),Monomials=>toBasis), R);
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
      retVal = sub(last coefficients((matrix{fromBasis})*f, Monomials=>toBasis), R);
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

-- FM: Brought this method in from IntegralClosure.  Is this the correct method?
--     All we need is the symbol, not the functionality.
-- isNormal = method()
isNormal RingElement := f -> (
   if not isFreeAlgebraOrQuotient(ring f) then error "Expected an element in a noncommutative ring.";
   if not isHomogeneous f then error "Expected a homogeneous element.";
   all(gens ring f, x -> findNormalComplement(f,x) =!= null)
)

normalElements = method()
normalElements(FreeAlgebraQuotient, ZZ, Symbol) := Sequence => (R,n,x) -> (
   -- Inputs: An associative algebra R, a degree n, and two symbols to use for indexed variables.
   -- Outputs: (1) A list of normal monomials in degree n
   --          (2) the components of the variety of normal elements (excluding the normal monomials) 
   --              expressed as ideals in terms of coefficients of non-normal monomial basis elements
   -- 
   -- The variety also contains information about the normalizing automorphism. This information is not displayed.
   -- The user can obtain the normalizing automorphism of a normal element via the normalAutomorphism function

   -- WARNING: This code currently is only valid for a singly graded algebra generated in degree one.  
   if degreeLength R != 1 or max flatten degrees R != 1 then error "Requires a singly graded algebra generated in degree one.";
   kk := coefficientRing R;
   fromBasis := flatten entries ncBasis(n,R);
   -- this line is the suspect line. In general, one must keep separate bases for each variable.
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
   --- generate the (skew)commutativity relations
   Q := coefficientRing R;
   gensA := new AngleBarList from ((gens R) / baseName | {Degrees=> degrees R});
   A := Q gensA;
   -- if only one generator and no relations, return the free algebra on one variable
   if numgens R == 1 and ideal R == 0 then return A;
   phi := map(A,ambient R,gens A);
   skewCommIndices := if R.?SkewCommutative then R.SkewCommutative else {};
   bothExter := (i,j) -> member(i,skewCommIndices) and member(j,skewCommIndices);
   commRelations := apply(subsets(numgens A,2), s -> A_(s_0)*A_(s_1)+(-1)^(if bothExter(s_0,s_1) then 0 else 1)*A_(s_1)*A_(s_0));
   extRelations := apply(skewCommIndices, i -> A_i^2);
   --- here is the defining ideal of the commutative algebra, inside the tensor algebra
   I := ideal (commRelations | extRelations | ((flatten entries gens ideal R) / phi));
   commIgb := gb ideal R;
   maxDeg := (((flatten entries gens commIgb) / degree) | {{0}}) / sum // max;
   maxDeg = max(2,maxDeg);
   Igb := NCGB(I, 2*maxDeg);
   A/I
)

ambientMap = method()
ambientMap RingMap := f -> (
   C := source f;
   ambC := ambient C;
   genCSymbols := (gens C) / baseName;
   map(target f, ambC, flatten entries matrix f)
)

toCommRing = method(Options => {SkewCommutative => false})
toCommRing FreeAlgebra := 
toCommRing FreeAlgebraQuotient := opts -> B -> (
   gensB := gens B;
   R := coefficientRing B;
   gensI := flatten entries gens ideal B;
   abB := R [ gens B , SkewCommutative => opts#SkewCommutative];
   if gensI == {} then
      abB
   else (
      phi := ambientMap map(abB,B,gens abB);
      abI := ideal flatten entries mingens ideal ((gensI) / phi);
      if abI == 0 then
         abB
      else
         abB/abI
   )
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
   A := freeAlgebra(R, varList);
   gensA := gens A;
   I := ideal apply(subsets(numgens A, 2), p -> 
            (gensA_(p#0))*(gensA_(p#1)) - (skewMatrix_(p#0)_(p#1))*(gensA_(p#1))*(gensA_(p#0)));
   -- this will be a GB for any coefficient ring, so we should tell the system that.
   I.cache.NCGB = {infinity,gens I};
   B := A/I;
   B
)

skewPolynomialRing (Ring,ZZ,List) := 
skewPolynomialRing (Ring,QQ,List) := 
skewPolynomialRing (Ring,RingElement,List) := (R,skewElt,varList) -> (
   -- if class skewElt =!= R then error "Expected ring element over base ring.";
   A := freeAlgebra(R, varList);
   gensA := gens A;
   I := ideal apply(subsets(numgens A, 2), p ->
            (gensA_(p#0))*(gensA_(p#1)) - promote(skewElt,R)*(gensA_(p#1))*(gensA_(p#0)));
   -- this will be a GB for any coefficient ring, so we should tell the system that.
   I.cache.NCGB = {infinity,gens I};
   B := A/I;
   B
)

threeDimSklyanin = method(Options => {DegreeLimit => 6})
threeDimSklyanin (Ring, List, List) := opts -> (R, params, varList) -> (
   if #params != 3 or #varList != 3 then error "Expected lists of length 3.";
   if instance(varList#0, R) or instance(varList#0,ZZ) or instance(varList#0,QQ) then
      error "Expected list of variables in third argument.";
   A := freeAlgebra(R, varList);
   gensA := gens A;
   I := ideal {params#0*gensA#1*gensA#2+params#1*gensA#2*gensA#1+params#2*(gensA#0)^2,
       params#0*gensA#2*gensA#0+params#1*gensA#0*gensA#2+params#2*(gensA#1)^2,
       params#0*gensA#0*gensA#1+params#1*gensA#1*gensA#0+params#2*(gensA#2)^2};
   Igb := NCGB(I, opts.DegreeLimit, Strategy=>if char R == 0 then "Naive" else "F4");
   B := A/I;
   B
)
threeDimSklyanin (Ring, List) := opts -> (R, varList) -> (
   if char R =!= 0 then error "For random Sklyanin, QQ coefficients are required.";
   threeDimSklyanin(R,{random(QQ),random(QQ), random(QQ)}, varList, opts)
)

fourDimSklyanin = method(Options => {DegreeLimit => 6})
fourDimSklyanin (Ring, List, List) := opts -> (R, params, varList) -> (
   if #params != 3 or #varList != 4 then error "Expected three parameters and four variables.";
   if instance(varList#0, R) or instance(varList#0,ZZ) or instance(varList#0,QQ) then
      error "Expected list of variables in third argument.";
   A := freeAlgebra(R, varList);
   gensA := gens A;
   varList = gens A;
   f1 := (varList#0*varList#1 - varList#1*varList#0) - params#0*(varList#2*varList#3 + varList#3*varList#2);
   f2 := (varList#0*varList#2 - varList#2*varList#0) - params#1*(varList#3*varList#1 + varList#1*varList#3);
   f3 := (varList#0*varList#3 - varList#3*varList#0) - params#2*(varList#1*varList#2 + varList#2*varList#1);
   g1 := (varList#0*varList#1 + varList#1*varList#0) - (varList#2*varList#3 - varList#3*varList#2);
   g2 := (varList#0*varList#2 + varList#2*varList#0) - (varList#3*varList#1 - varList#1*varList#3);
   g3 := (varList#0*varList#3 + varList#3*varList#0) - (varList#1*varList#2 - varList#2*varList#1);

   I := ideal {f1,f2,f3,g1,g2,g3};
   Igb := NCGB(I, opts.DegreeLimit, Strategy=>"F4");
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

--- Derivation functionality
Derivation = new Type of HashTable
derivation = method()
derivation (FreeAlgebra, List) :=
derivation (FreeAlgebraQuotient, List) := (A,output) -> derivation(A,output,map(A,A))

derivation (FreeAlgebra, List, RingMap) :=
derivation (FreeAlgebraQuotient, List, RingMap) := (A, output, twist) -> (
   -- currently, no error checking is performed
   der := new Derivation from {(symbol source) => A,
                               (symbol matrix) => matrix {output},
		               (symbol homomorphism) => twist,
			       (symbol generators) => hashTable apply(numgens A, i -> (A_i,output#i)),
			       ("imageCache") => new MutableHashTable from {}};                            
   der
)

Derivation ZZ := (der,n) -> 0

Derivation RingElement := (der,f) -> (
   if der#"imageCache"#?f then return der#"imageCache"#f;
   if der.generators#?f then return der.generators#f;
   varListF := toVariableList f;
   factoredTerms := apply(varListF, p -> (p#0, product take(p#1,#(p#1) // 2), product drop(p#1,#(p#1) // 2)));
   result := sum for term in factoredTerms list (
      leftMon := term#1;
      rightMon := term#2;
      coeff := term#0;
      monImage := (der leftMon)*rightMon + (der.homomorphism leftMon)*(der rightMon);
      coeff*monImage
   );
   if size f == 1 then der#"imageCache"#f = result;
   result  
)

-- This code will be useful once we can handle derivations properly.
-- as it stands, it will still run but not evaluate the derivation properly.
oreIdeal = method(Options => {Degree => 1})
oreIdeal (Ring,RingMap,Derivation,RingElement) := 
oreIdeal (Ring,RingMap,Derivation,Symbol) := opts -> (B,sigma,delta,X) -> (
   X = baseName X;
   kk := coefficientRing B;
   varsList := ((gens B) / baseName) | {X};
   A := ambient B;
   C := freeAlgebra(kk, (varsList | {Degrees => (degrees A | {opts.Degree})}));
   use C;
   fromBtoC := map(C,B,drop(gens C, -1));
   fromAtoC := map(C,A,drop(gens C, -1));
   X = value X;
   ideal (apply(flatten entries gens ideal B, f -> fromAtoC promote(f,A)) |
            apply(gens B, x -> X*(fromBtoC x) - (fromBtoC sigma x)*X - (fromBtoC delta x)))
)

oreIdeal (Ring,RingMap,Symbol) := 
oreIdeal (Ring,RingMap,RingElement) := opts -> (B,sigma,X) -> (
   zeroMap := derivation(B,toList ((numgens B):0));
   oreIdeal(B,sigma,zeroMap,X,opts)
)

oreExtension = method(Options => options oreIdeal)
oreExtension (Ring,RingMap,Derivation,Symbol) := 
oreExtension (Ring,RingMap,Derivation,RingElement) := opts -> (B,sigma,delta,X) -> (
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
   C := freeAlgebra(R, (newgens | {Degrees => (degrees A | degrees B)}));
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
   C := ambient F;
   R := coefficientRing A;
   q = promote(q,R);
   gensAinF := take(gens F, #gens A);
   gensBinF := drop(gens F, #gens A);   
   -- create the commutation relations among generators of A and B
   phi := map(C, F, gens C);
   K := ideal flatten apply(gensAinF, g-> apply( gensBinF, h-> phi (h*g-q*g*h)));

   if class F === FreeAlgebra then F/K else C/(ideal F + K)
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

--  Move this to the engine, or leave at top level?
oppositeElement = method()
oppositeElement RingElement := f -> (
   -- this is an internal routine that should be only called on elements
   -- in a free algebra
   if f == 0 then return f;
   A := ring f;
   (rawCoeff, rawMons) := rawPairs(raw coefficientRing ring f, raw f);
   sum for i from 0 to (#rawMons - 1) list (
      newMon := product(apply(reverse rawSparseListFormMonomial rawMons#i, p -> A_(p#0)^(p#1)));
      promote(rawCoeff#i,coefficientRing A) * newMon
   )
)

oppositeRing = method()
oppositeRing FreeAlgebra := 
oppositeRing FreeAlgebraQuotient := B -> (
   gensB := gens B;
   R := coefficientRing B;
   oppA := freeAlgebra(R, gensB);
   if class B === FreeAlgebra then return oppA;
   idealB := flatten entries gens ideal B;
   phi := map(oppA,ambient B, gens oppA);
   oppIdealB := idealB / oppositeElement / phi // ideal;
   -- TODO: Check that the opposite of a NCGB again an NCGB
   --   also, enable forcing of a GB on the front end
   -- oppIdealBGB = matrix {apply(flatten entries NCGB ideal B, oppositeElement)};
   oppA / oppIdealB
)

FreeAlgebra ** FreeAlgebra := 
FreeAlgebraQuotient ** FreeAlgebra :=
FreeAlgebra ** FreeAlgebraQuotient := 
FreeAlgebraQuotient ** FreeAlgebraQuotient := (A,B) -> (
   qTensorProduct(A,B,promote(1,coefficientRing A))
)

findRecurrence = method()
findRecurrence List := as -> (
   revas := reverse as;
   deg := 1;
   extra := 0;  -- used to build an overdetermined system to help rule out false recurrences.
   foundRecurrence := false;
   while (not foundRecurrence and 2*(deg+extra) < #revas) do (
      eqns := matrix apply(deg+extra, i -> revas_(toList((1+i)..(deg+i))));
      out := transpose matrix{revas_(toList(0..(deg+extra-1)))};
      soln := out // eqns;
      if eqns*soln != out then
         deg = deg + 1
      else
         foundRecurrence = true;
   );
   if foundRecurrence then soln else null
)

toRationalFunction = method()
toRationalFunction List := as -> (
   S := getSymbol "S";
   soln := findRecurrence(as);
   revas := reverse as;
   if (soln === null) then return null;
   deg := numgens target soln;
   A := ZZ(monoid[S]);
   fixLow := g -> g*(leadCoefficient last terms g)^(-1);
   B := frac A;
   C := degreesRing 1;
   phi := map(C,A,gens C);
   Q := 1 - first flatten entries ((matrix {apply(deg, i -> (B_0)^(i+1))})*soln);
   totalDeg := #as - 1;
   numDeg := totalDeg - deg;
   f := sum apply(numDeg + 1, i -> revas#(totalDeg - i)*(B_0)^i);

   guess1 := f + (B_0)^(numDeg + 1)*revas#(totalDeg - numDeg - 1)/Q;
   numGuess := sum select(terms numerator guess1, t -> sum degree t <= numDeg);
   guess2elt := numGuess / Q;
   num := phi fixLow numerator guess2elt;
   den := phi fixLow denominator guess2elt;
   guess2expr := expression num / expression factor den;
   (num, den, guess2expr)
)

ncHilbertSeries = method(Options => {Order => 20})
ncHilbertSeries FreeAlgebra := opts -> A -> (
   -- degreelimit is ignored here, since the formula for the tensor algebra
   -- is straightforward.
   S := degreesRing A;
   T := S_0;
   den := (expression 1_S) - expression (sum apply(gens A, x -> S_(degree x)));
   (expression 1_S) / den
)

--- working only for singly graded things at the moment.
ncHilbertSeries FreeAlgebraQuotient := opts -> B -> (
   degLim := opts#Order;
   hsB := apply(degLim + 1, i -> numgens source ncBasis(i,B));
   ratlReturn := toRationalFunction hsB;
   if ratlReturn === null then return sum apply(#hsB, i -> hsB#i*((degreesRing B)_0)^i);
   (num, den, expr) := ratlReturn;
   tempRing := ring num;
   phi := map(degreesRing B, tempRing, gens degreesRing B);
   expression phi num / expression factor phi den
)

-- support function for now
--ncSupport = method();
--ncSupport RingElement := f -> (
--   A := ring f;
--   (rawCoeff, rawMons) := rawPairs(raw coefficientRing ring f, raw f);
--   reverse sort unique flatten apply(rawMons, rm -> apply(rawSparseListFormMonomial rm, p -> A_(p#0)))
--)

ncKernel = method(Options=>{DegreeLimit => 10,Strategy=>"F4"});
ncKernel RingMap := opts -> phi -> (
   A := source phi;
   B := target phi;
   K := ncGraphIdeal phi;
   C := ring K;
   Kgb := NCGB(K, opts#DegreeLimit, Strategy => opts#Strategy);
   --- now select those elements of the GB that are only in the variables from ambA and place
   --- those elements in A.
   kerGens := select(flatten entries Kgb, f -> isSubset(support f, drop(gens C, numgens B)));
   backToA := map(A,C,toList((numgens B) : 0_A) | (gens A));
   kerGensA := select(kerGens / backToA, f -> f != 0_A);
   -- the ring map 'backToA' is messing up the coefficients.  It seems the denominators are
   -- disappearing if the coefficientRing is created via frac rather than toField.
   -- See line 801 in tests.m2
   if kerGensA == {} then ideal 0_A else ideal kerGensA
)

ncGraphIdeal = method()
ncGraphIdeal RingMap := phi -> (
   if not isFreeAlgebraOrQuotient source phi or not isFreeAlgebraOrQuotient target phi then
      error "Expected both source and target to be noncommutative at the present time.";
   -- TODO: isHomogeneous for ring maps needs to be fixed.
   -- if not isHomogeneous phi then
   --   error "Expected a homogeneous ring map.";
   A := source phi;
   ambA := ambient A;
   I := ideal A;
   B := target phi;
   ambB := ambient B;
   J := ideal B;
   kk := coefficientRing A;
   if not kk === coefficientRing B then
      error "Expected coefficient rings to be the same at the present time.";
   gensC := B#generatorSymbols | A#generatorSymbols;
   C := freeAlgebra(kk,gensC |
            {Degrees => (degrees B | degrees A)} | 
	    {Weights => toList(numgens B : 1) | (toList(numgens A : 0))} | 
	    {UseVariables=>false});
   psiA := map(C,ambA,drop(gens C, numgens B));
   psiB := map(C,ambB,take(gens C, numgens B));
   liftToAmbB := map(ambB,B,gens ambB);
   Igb := flatten entries if I.cache.?NCGB then last I.cache.NCGB else gens I;
   Jgb := flatten entries if J.cache.?NCGB then last J.cache.NCGB else gens J;
   K := (ideal (Igb / psiA)) + (ideal (Jgb / psiB)) + 
         ideal apply(numgens A, i -> (psiA ambA_i) - psiB liftToAmbB phi A_i);
   K
)

-------------------------------------------------------
--- Commands to compute endomorphism ring presentations
-------------------------------------------------------
--- This code is not very useful at the moment, since NCGBs with commutative coefficient
--- rings are not yet implemented

endomorphismRingIdeal = method()
endomorphismRingIdeal (Module,Symbol) := (M,X) -> (
   R := ring M;
   endM := End M;
   N := numgens endM;
   endGens := apply(N, i -> homomorphism endM_{i});
   endMVars := apply(N, i -> X_i);
   A := freeAlgebra(R,endMVars | {UseVariables=>false});
   gensA := ncBasis(1,A);
   deg2A := ncBasis(2,A);
   composites := apply(flatten entries deg2A, m -> evaluateAt(endGens, m)) / homomorphism';
   -- relations from multiplication table
   I := ideal(deg2A - (gensA)*(matrix entries matrix {composites}));
   -- now must identify the identity map.
   identM := homomorphism' id_M;
   ff := 1_A - first flatten entries (ncBasis(1,A)*(matrix entries identM));
   I = I + ideal ff;
   I.cache#"EndomorphismRingIdealGens" = endGens;
   I
)

--- converts a RingElement into a list of pairs (one element for each term)
--- and each pair has first coordinate the coefficient and the second a list
--- of variables used in the monomial.  The empty list indicates the constants
--- a substitution is also allowed.
toVariableList = method()
toVariableList RingElement := f -> (
   A := ring f;
   R := coefficientRing A;
   (rawCoeff, rawMons) := rawPairs(raw coefficientRing ring f, raw f);
   varPowers := apply(#rawMons, i -> (promote(rawCoeff#i,R), flatten apply(rawSparseListFormMonomial rawMons#i, p -> toList(p#1:A_(p#0)))));
   varPowers
)

-- function for evaluating a ring element at a list of other elements that may be multiplied
--    This can be either other ring elements, matrices, etc.
-- make this more robust?
-- this is basically sub, but we allow matrices as well
evaluateAt = method()
evaluateAt (List,RingElement) := (mats,f) -> (
   A := ring f;
   if (#mats != numgens A) then
      error "Expected the length of substitution list to match number of generators of ring.";
   if all(mats, m -> class m === Matrix) then (
      matMod := source first mats;
      if any(#mats, i -> matMod =!= source mats#i or matMod =!= target mats#i) then
         error "Expected a list of maps defined on the same module.";
   );
   varPowers := toVariableList f;
   toSub := hashTable apply(numgens A, i -> (A_i,mats#i));
   sum apply(varPowers, p -> p#0*(product apply(p#1, p -> toSub#p)))
)

-------------------------------------------------------------------------
---- Ext Algebra code (in the non-ci case)
-------------------------------------------------------------------------
-- this is a slightly more intelligent version of evaluateAt which remembers intermediate calculations
evaluateAtMonomial = method()
evaluateAtMonomial (RingElement, MutableHashTable) := (m, monHash) -> (
   if monHash#?m then return monHash#m;
   mVarList := last first toVariableList m;
   wordLen := #mVarList;
   leftMon := product take(mVarList,wordLen // 2);
   rightMon := product drop(mVarList,wordLen // 2);
   monHash#m = monHash#leftMon * monHash#rightMon;
   monHash#m
)

getExtGensAndRels = method(Options => {})
getExtGensAndRels (Module, ZZ, FreeAlgebra, MutableHashTable) :=
getExtGensAndRels (Module, ZZ, FreeAlgebraQuotient, MutableHashTable) := opts -> (k, d, B, monHash) -> (
   -- for each monomial of B in degree d, compute its value as a map from
   -- a resolution of k to itself.  Then convert all these back to elements of
   -- ext^d and determine any new generators that are required.  Return
   -- the elements required as a list of ComplexMaps.
   basisB := ncBasis(d,B);
   -- the following line is the main bottleneck in the algorithm:
   Extd := Ext^d(k,k);
   R := ring k;

   -- perform a quick check to see if any additional processing is necessary
   if (numgens Extd == #(flatten entries basisB)) then return ({},{});
 
   -- locate new generators and relations at this point
   images := matrix {apply(flatten entries basisB, m -> yonedaMap'(evaluateAtMonomial(m,monHash)))};
   newgens := mingens (Extd/image images);
   newMaps := if newgens == 0 then {} else (
      cxLen := length source monHash#(B_0);
      liftedGens := apply(numcols newgens, i -> map(Extd,R^1,newgens_{i}));
      apply(liftedGens, liftGen -> yonedaMap(liftGen,LengthLimit => cxLen))
   );
   kergens := gens ker sub(matrix entries images,coefficientRing ring images);
   newrels := flatten entries (basisB * kergens);
   phi := map(ambient B, B, gens ambient B);
   (newMaps,newrels / phi)
)

-- would like to do the following for modules (instead of just the residue field),
-- but NCGBs with coeffs in a commutative ring are not yet complete (similar to endomorphism rings)
extAlgebra = method(Options => {DegreeLimit => (5,10)})
extAlgebra (Ring, Symbol) := opts -> (R, xx) -> (
   if not isHomogeneous R or not isCommutative R then error "Expected a graded ring as input.";
   degLimit := first opts#DegreeLimit;
   gbLimit := last opts#DegreeLimit;
   if degLimit > gbLimit then
     error "Expected DegreeLimit <= GBDegreeLimit.";
   kk := coefficientRing R;
   if degLimit <= 0 then return kk;
   
   -- build the algebra generated in degree 1 first
   -- this should be the same as the Koszul dual, but we need
   -- the actual complex maps to compute products later
   k := coker vars R;
   Ext1 := basis Ext^1(k,k);
   degsExt1 := (degrees source Ext1) / (d -> -d);
   -- I really want this to be bigraded (at least, if R is graded) but ncBasis can't handle multigradings yet.
   A := freeAlgebra(kk,{xx_1..xx_(#degsExt1), Degrees => apply(degsExt1, d -> {1}) });
   -- construct the lifts of the maps out to degree relLimit + 2 to see if we can capture
   -- relations correctly.  This may need to be changed
   extMaps := new MutableHashTable from apply(numcols Ext1, i -> (A_i,yonedaMap(Ext1_{i},LengthLimit => degLimit))); 
   basisA2 := ncBasis(2,A);
   deg2prods := matrix {apply(flatten entries basisA2, m -> yonedaMap'(evaluateAtMonomial(m,extMaps)))};

   I1 := ideal compress (basisA2 * sub(gens ker (deg2prods),kk));
   I1gb := NCGB(I1,gbLimit);
   B := freeAlgebraQuotient(A,I1,I1gb);
   alpha := map(B,A,gens B);

   degsExt := degsExt1;
   -- update the hashtable
   extMaps = new MutableHashTable from apply(keys extMaps, m -> (alpha m, extMaps#m));
  
   for d from 2 to degLimit do (
      (newGens,newRels) := getExtGensAndRels(k, d, B, extMaps);
      -- enlarge ring by new generators
      -- will have to be more careful with multidegrees here in the future.  This degree
      -- is just homological degree
      degsExt = degsExt | apply(newGens, n -> {d});
      A' := freeAlgebra(kk, {xx_1..xx_(#degsExt), Degrees => degsExt});

      -- enlarge defining ideal by new relations
      phi := map(A',A,take(gens A', numgens A));
      psi := map(A',B,take(gens A', numgens B));
      -- recompute GB and update ring
      I := ideal B + ideal newRels;
      I' := phi I;
      I'gb := NCGB(I',gbLimit);
      B' := freeAlgebraQuotient(A',I',I'gb);
      alpha := map(B',B,take(gens B', numgens B));
      -- update generators hash table with new elements
      extMaps = new MutableHashTable from apply(keys extMaps, m -> (alpha m, extMaps#m));
      scan(numgens B', i -> if i >= numgens A then extMaps#(B'_i) = newGens#(numgens B' - i - 1));
      B = B';
      A = A';
   );
   -- only return the generator maps, not all monomials
   B.cache#"extMaps" = hashTable apply(gens B, v -> (v,extMaps#v));
   B
)

--------------------------------------------------------------------------
---- Point and line scheme code (in the sense of Artin-Tate-Van den Bergh)
--------------------------------------------------------------------------
leftQuadraticMatrix = method()
leftQuadraticMatrix Ideal := I -> (
   A := ring I;
   if class A =!= FreeAlgebra then 
      error "Expected an ideal in a free algebra.";
   gensI := gens I;
   splitRels := I_* / toVariableList;
   if (not isHomogeneous I and any(splitRels, p -> any(p#1, l -> #l != 2))) then
      error "Expected a homogeneous ideal with quadratic generators.";
   gensA := gens A;
   Id := id_(A^(numgens A));
   eHash := hashTable apply(numgens A, i -> (A_i, Id^{i}));
   myTable := apply(numgens I, i -> flatten entries sum apply(splitRels#i, t -> t#0*t#1#0*(eHash#(t#1#1))));
   matrix myTable
)
leftQuadraticMatrix List := I -> leftQuadraticMatrix ideal I

rightQuadraticMatrix = method()
rightQuadraticMatrix Ideal := I -> (
   -- almost the same as above, just the myTable is built differently at the end.
   A := ring I;
   if class A =!= FreeAlgebra then 
      error "Expected an ideal in a free algebra.";
   gensI := gens I;
   splitRels := I_* / toVariableList;
   if (not isHomogeneous I and any(splitRels, p -> any(p#1, l -> #l != 2))) then
      error "Expected a homogeneous ideal with quadratic generators.";
   gensA := gens A;
   Id := id_(A^(numgens A));
   eHash := hashTable apply(numgens A, i -> (A_i, Id^{i}));
   myTable := apply(numgens I, i -> flatten entries sum apply(splitRels#i, t -> t#0*t#1#1*(eHash#(t#1#0))));
   matrix transpose myTable
)
rightQuadraticMatrix List := I -> rightQuadraticMatrix ideal I

lineSchemeFourDim = method()
lineSchemeFourDim (FreeAlgebraQuotient, Symbol) := (B,M) -> (
   if not isHomogeneous B or
      numgens ideal B != 6 or
      numgens B != 4 then error "Expected an algebra on four generators and six relations.";
   Bd := homogDual B;
   J := ideal Bd;
   A := ring J;
   lqJ := leftQuadraticMatrix J;
   N := getSymbol "N";
   u := getSymbol "u";
   v := getSymbol "v";
   monS := monoid [M_(1,2),M_(1,3),M_(1,4),M_(2,3),M_(2,4),M_(3,4)];
   S := QQ monS;
   monR := monoid [u_1..u_4,v_1..v_4,N_(1,2),N_(1,3),N_(1,4),N_(2,3),N_(2,4),N_(3,4),MonomialOrder=>{8,6}];
   R := QQ monR;
   matching := hashTable {{1,2} => 0, {1,3} => 1, {1,4} => 2, {2,3} => 3, {2,4} => 4, {3,4} => 5};
   --- a bit of a hack, but oh well
   K := ideal apply(subsets({1,2,3,4},2), p -> R_(matching#p + 8) - (R_(p#0-1)*R_(p#1+3) - R_(p#1-1)*R_(p#0+3)));
   Kgb := gb K;
   psi := map(S,R,{0,0,0,0,0,0,0,0,S_5,-S_4,S_3,S_2,-S_1,S_0});
   alpha1 := map(R,A,{R_0,R_1,R_2,R_3});
   alpha2 := map(R,A,{R_4,R_5,R_6,R_7});
   bigMat := (matrix applyTable(entries lqJ, g -> alpha1 g)) | (matrix applyTable(entries lqJ, g -> alpha2 g));
   I := minors(8,bigMat);
   IN := (flatten entries gens I) / (f -> f % Kgb);
   IM := ideal (IN / psi) + ideal {S_0*S_5 - S_1*S_4 + S_2*S_3};
   IM
)

pointScheme = method()
pointScheme (FreeAlgebraQuotient, Symbol) := (B, y) -> (
    A := ambient B;
    n := numgens A;
    monR := monoid [y_1..y_n];
    R := (coefficientRing B) monR;
    phi := map(R,A,gens R);
    I := ideal B;
    Mleft := matrix applyTable(entries leftQuadraticMatrix I, g -> phi g);
    minors(n,Mleft)
)

ncMatrixMult = method()
ncMatrixMult (Matrix, Matrix) := (A,B) -> (
   if (numcols A != numrows B) then
       error "Maps not composable.";
   matrix table (numrows A, numcols B, (i,j) -> sum apply(numcols A, k -> A_(i,k)*B_(k,j)))
)

rightKernel = method(Options=>{DegreeLimit=>10,Strategy=>"F4"})
rightKernel Matrix := opts -> M -> (
   -- this function computes the right kernel of a map between free
   -- modules over a noncommutative ring.
   if not isHomogeneous M then error "Expected a homogeneous matrix.";
   if numgens degreesRing ring M != 1 then error "Expected a singly graded algebra.";
   col := getSymbol "col";
   row := getSymbol "row";
   B := ring M;
   A := ambient B;
   I := ideal B;
   liftM := lift(M,A);
   config := gens A | toList(row_0..row_(numrows M-1)) | toList(col_0..col_(numcols M-1));
   config = config | {Degrees=>(degrees A |
	                        apply(degrees target M, d -> d + {1}) |
	                        apply(degrees source M, d -> d + {1}))};
   AA := freeAlgebra(coefficientRing A, config);  -- doesn't 'use' variables
   gensAVars := apply(toList(0..(numgens A - 1)), i -> AA_i);
   rowVars := apply(toList(numgens A..(numgens A + numrows M - 1)), i -> AA_i);
   colVars := apply(toList((numgens A + numrows M)..(numgens AA-1)), i -> AA_i);
   phi := map(AA,A,apply(numgens A, i -> AA_i));
   psi := map(A,AA,gens A | toList(numrows M : 0) | toList(numcols M : 0));
   psiB := map(B,AA,gens B | toList(numrows M : 0) | toList(numcols M : 0));
   diagMat := diagonalMatrix rowVars;
   outputs := matrix {(entries transpose ncMatrixMult(diagMat,phi liftM)) / sum};
   inputs := matrix {colVars};
   IM := phi I + ideal (inputs - outputs);
   IMgb := NCGB(IM, opts#DegreeLimit, Strategy => opts#Strategy);
   kerGens := ideal select(flatten entries IMgb, f -> isSubset(support f,gensAVars | colVars) and not isSubset(support f, gensAVars));
   if kerGens == 0 then return map(source M, (ring M)^0,0);
   mkerGens := phi I + sum apply(gensAVars, v -> kerGens*v);
   mkerGensGB := NCGB(mkerGens,opts#DegreeLimit, Strategy => opts#Strategy);
   minKerGens := compress NCReductionTwoSided(gens kerGens,mkerGensGB);
   (minKerMons, minKerCoeffs) := coefficients minKerGens;
   linIndepKer := mingens image sub(minKerCoeffs,coefficientRing A);
   linIndepKerGens := flatten entries (minKerMons*linIndepKer);
   ident := id_(AA^(numcols M));
   identColHash := hashTable apply(numcols M, i -> (colVars_i,ident_{i}));
   tempKerMat := matrix {
     apply(flatten entries minKerMons, f -> (
         (coeff, monList) := first toVariableList f;
	 coeff*identColHash#(first monList)*(product drop(monList,1))))};
   targetDeg := (degrees source M) / (d -> -d);
   sourceDeg := (linIndepKerGens / degree) / (d -> -d+{1});
   result := map(B^targetDeg, B^sourceDeg, (psiB tempKerMat)*linIndepKer);
   result
)

------------------------------------------------------
--- superpotential code
------------------------------------------------------

superpotential = method(Options => {Strategy => "F4"})
superpotential FreeAlgebraQuotient := opts -> B -> (
   -- compute the relations of the Koszul dual out to degree d+e-1
   -- d = number of generators of the algebra
   -- e = relation degree
   A := ambient B;
   kk := coefficientRing A;
   e := first degree first (ideal B)_*;
   koszI := homogDual ideal B;
   d := numgens B;
   koszIgb := NCGB(koszI,d+e-1,Strategy => opts.Strategy);
   koszB := A/koszI;
   
   -- compute a basis of the Koszul dual
   allBasis := flatten apply(d+e, i -> flatten entries ncBasis(i,koszB,Strategy => opts.Strategy));
   
   -- find a generator of the 'top' degree of the Koszul dual
   topDeg := max apply(allBasis, m -> first degree m);
   topForm := ncBasis(topDeg,koszB,Strategy => opts.Strategy);
   if numcols topForm != 1 then error "Expected (m)-Koszul AS-regular algebra.";

   deg1Basis := vars A;
   -- phi is the projection map from A to koszB
   phi := map(koszB,A,gens koszB);
   
   -- basis of the entire tensor algebra in the 'correct degree'
   bigBasis := ncBasis(topDeg,A,Strategy => opts.Strategy);
   
   -- project the basis of the tensor algebra to the koszul dual to get a coefficient from the top form
   -- and take the sum of the basis with these coeffs as the superpotential
   first flatten entries (bigBasis * (transpose sub(last coefficients(phi bigBasis, Monomials=>topForm),kk)))
)

--- B = k[x,y,z] 
--- koszB = k<x,y,z>/(xy+yx,xz+zx,yz+zy,x^2,y^2,z^2)
--- xyz is a basis of the top form in koszB
--- A = k<x,y,z>
--- a basis of A in degree 3 is (27 terms)
--- xyz   xzy  yxz    yzx   zxy   zyx
--- xyz   -xyz -xyz   xyz   xyz   -xyz
--- this gives coeffs:
--- xyz - xzy - yxz + yzx + zxy - xyz

isSuperpotential = method()
isSuperpotential RingElement := f -> (
   X := matrix entries transpose splitElement f;
   if rank X != numcols X then return false;
   Y := matrix entries transpose splitElement cycleElement f;
   if rank Y != numcols Y then return false;
   P := (Y // X);
   return ((Y - X*P) == 0);
)

-- cycle from right to left
cycleElement = method()
cycleElement RingElement := f -> (
   varList := toVariableList f;
   monLen := #(last first varList);
   sum apply(varList, p -> p#0*((p#1)#(monLen-1))*product(drop(p#1,-1)))
)

splitElement = method()
splitElement RingElement := f -> (
   A := ring f;
   kk := coefficientRing A;
   d := first degree f;
   basis1 := flatten entries ncBasis(1,A);
   basisrest := flatten entries ncBasis(d-1,A);
   matrix apply(basis1, x -> apply(basisrest, m -> sub(first flatten entries last coefficients(f,Monomials=>matrix {{x*m}}),kk)))
)

nakayamaAut = method(Options => options superpotential)
nakayamaAut FreeAlgebraQuotient := opts -> B -> (
   sp := superpotential(B,opts);
   X := matrix entries transpose splitElement sp;
   Y := matrix entries transpose splitElement cycleElement sp;
   -- should multiply by (-1)^(gldim + 1) here, but this is expensive to compute and should be known by the user.
   -- the map P defined below should satisfy X = YP where Y is the matrix after cycling
   -- and X is the matrix representation of the original superpotential.
   P := (X // Y);
   -- if the algebra is quadratic, mult by (-1)^(word length sp + 1)
   -- if the algebra is cubic, mult by (-1)^(word length sp)
   f := first (ideal B)_*;
   degf := #(last first (toVariableList f));
   spWL := #(last first (toVariableList sp));
   P = (-1)^(if degf == 2 then spWL + 1 else spWL)*P^(-1);
   -- cycling from right to left gives the inverse of the Nakayama automorphism
   map(B,B,flatten entries (P*(transpose vars B)))
)

nakayamaAut RingElement := opts -> sp -> (
   A := ring sp;
   X := matrix entries transpose splitElement sp;
   Y := matrix entries transpose splitElement cycleElement sp;
   P := (Y // X);
   -- for the superpotential version, we assume that sp will give a quadratic algebra
   -- (this gives the right answer also in the only other known case
   -- of m-Koszul AS-regular algebra, namely m = 3 of gldim 3 on 2 generators
   spWL := #(last first (toVariableList sp));
   P = (-1)^(spWL + 1) * P^(-1);
   map(A,A,flatten entries (P*(transpose ncBasis(1,A))))
)

leftDiff = method()
leftDiff (RingElement, RingElement) := (w,v) -> (
   -- this function selects those monomials from w that start with the
   -- variable v, delete v from the monomial and keep the coefficient.
   if #(terms v) != 1 then error "Expected a monomial in second argument.";
   curElt := w;
   diffList := last first toVariableList v;
   for var in diffList do (
       curElt = sum for p in toVariableList curElt list (
                   if first p#1 == var then (p#0) * (product drop(p#1,1)) else 0
                );
       if curElt == 0 then return 0;
   );
   curElt
)

derivationQuotientIdeal = method()
derivationQuotientIdeal (RingElement, ZZ) := (w,n) -> (
   -- this method defines the derivation-quotient ideal
   -- of dubois-violette corresponding to w.  It is not verified
   -- that w is a superpotential.  n derivatives (deletions) are taken on the left.
   if not isHomogeneous w then error "Expected a homogeneous input.";
   A := ring w;
   baseA := coefficientRing A;
   d := first degree w;
   nBasis := flatten entries ncBasis(n,A);
   diffs := apply(nBasis, m -> leftDiff(w,m));
   -- The code below was to create a minimal set of generators of the defining ideal
   basisA := ncBasis(d-n,A);
   coeffs := sub(last coefficients(matrix{diffs}, Monomials => basisA),baseA);
   idealGens := ideal flatten entries (basisA*(mingens image coeffs));
   idealGens
)

derivationQuotient = method(Options => {DegreeLimit => 10, Strategy => "F4"})
derivationQuotient (RingElement, ZZ) := opts -> (w,n) -> (
   A := ring w;
   I := derivationQuotientIdeal(w,n);
   Igb := NCGB(I,opts#DegreeLimit, Strategy => opts#Strategy);
   B := A / I;
   B
)

--- load the tests
-- 21 Jan 2021: there are two failing tests, which have been modified to not fail:
-- one is commented out by using FAILINGTEST rather than TEST
-- one is a single line (it has a TODO header on the preceding line)
FAILINGTEST = (str) -> null
load "./AssociativeAlgebras/tests.m2"

--- load the documentation
beginDocumentation()
load "./AssociativeAlgebras/doc.m2"

end----------------------------------------------------------

restart
uninstallPackage "AssociativeAlgebras"
restart
needsPackage "AssociativeAlgebras"
installPackage "AssociativeAlgebras"
viewHelp "AssociativeAlgebras"
restart
check "AssociativeAlgebras"

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

-- making a new type of monoid for noncommutative algebras.
restart
debug needsPackage "AssociativeAlgebras"
R = QQ <| x,y |>
M = freeMonoid R
M_0
M_1
M_1*M_0
M_0*M_1

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
-- 3. isWellDefined broken for maps of nc rings.
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
phi = map(B,B,{y,z,x})
isWellDefined phi
///
TEST ///
-- 4. kernel broken for maps of nc rings (but we can make a product order which will compute it).
-- FIXED with ncKernel
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
phi = map(B,B,{y,z,x})
ker phi
///
TEST ///
-- 5. ** doesn't work right for nc rings.
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
C = B ** B
///
TEST ///
-- 6. "//" doesn't work for matrices over nc rings.  Need left/right ncgbs.
--    no test available just yet.
-- 7. Documentation!!! Continue to bring over working documentation nodes from NCAlgebra
-- 8. Opposite Ring and element don't work yet.
-- 9. Change toM2Ring to 'abelianization' or some such
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

/// --- FM: some toy examples for ncHilbertSeries code.

restart
needsPackage "AssociativeAlgebras"
R = QQ{x,y,Degrees=>{1,1}}
S = R/ideal{x^2*y - y*x^2,x*y^2 - y^2*x}
ncHilbertSeries(S, Order => 6)
ncHilbertSeries S

restart
needsPackage "AssociativeAlgebras"
R = QQ{x,y,Degrees=>{2,3}}
S = R/ideal{x^2*y - y*x^2,x*y^2 - y^2*x}
ncHilbertSeries(S,Order => 40)

restart
needsPackage "AssociativeAlgebras"
R = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
ncHilbertSeries R

restart
needsPackage "AssociativeAlgebras"
R = threeDimSklyanin(ZZ/32003,{2,3,5},{x,y,z})
ncHilbertSeries(R, Order => 15)

restart
needsPackage "AssociativeAlgebras"
R = fourDimSklyanin(ZZ/32003,{x,y,z,w})
ncHilbertSeries(R, Order => 10)

restart
needsPackage "AssociativeAlgebras"
R = QQ[x,y]/ideal(x^2,x*y)
kRes = res(coker vars R, LengthLimit => 10)
last toRationalFunction apply(11, i -> numgens source kRes.dd_i)

restart
needsPackage "AssociativeAlgebras"
R = QQ[x,y,Degrees=>{2,3}]
hilbertSeries R
ring value denominator hilbertSeries R === degreesRing R
-- I don't understand which degrees are allowed in the non-standard graded case...
toRationalFunction(apply(9, i -> numgens source basis(i, R)))
toRationalFunction(apply(10, i -> numgens source basis(i, R)))
toRationalFunction(apply(11, i -> numgens source basis(i, R)))
toRationalFunction(apply(12, i -> numgens source basis(i, R)))
toRationalFunction(apply(13, i -> numgens source basis(i, R)))
toRationalFunction(apply(14, i -> numgens source basis(i, R)))
toRationalFunction(apply(15, i -> numgens source basis(i, R)))
toRationalFunction(apply(16, i -> numgens source basis(i, R)))
toRationalFunction(apply(17, i -> numgens source basis(i, R)))
toRationalFunction(apply(18, i -> numgens source basis(i, R)))
toRationalFunction(apply(19, i -> numgens source basis(i, R)))
toRationalFunction(apply(20, i -> numgens source basis(i, R)))
///

/// -- working code for MES
  restart
  needsPackage "AssociativeAlgebras"
  A = QQ {x, y, z}
  gbTrace=2
  I = ideal(-x^2+y*z+z*y,x*z-y^2+z*x,x*y+y*x-z^2)  
  J = NCGB(I, 10, Strategy => "F4")
  J = NCGB(I, 22, Strategy => "F4")
///

/// -- notes from 12/21 meeting
-- 1. fix flattenring for towers
-- 2. teach f4 how to intelligently use previous reductions when building 
--    reducer rows.
-- 3. One option is to have the F4 matrix be only left and right multiplied by variables
--    from basis in one lower degree.  Should shrink matrices tremendously.
-- 4. Inhomogeneous GBs!
-- 5. Intelligently use if S/I is finite dimensional for operation (also allow for truncation of an algebra)

--- Additional notes (FM)
-- Would like to try altering our current version of F4 to the following.
-- ** Indicates new additions to the code.
-- ** 1. Suppose that f_1,...,f_r are the GB so far.  These are assumed to be
--    interreduced.  If the f_1,...,f_r are the inputs, then interreduce them all before processing the first degree.
--    This way, they are all of the form LM(f_i) + sum of reduced words.
--    2. Build the necessary s-pairs from the list f_1,...,f_r.
--    3. Track down all the necessary reducers required for reducing these s-pairs.  Note that since the f_i
--    are interreduced, this should be far less than we currently are creating for the F4 matrix.
-- ** 4. Add the reducers in to the matrix by multiplying the reduced row corresponding to the f_i used.
--    These won't be reduced fully, but at least they will not be 'too far' from something reduced.
--    It is possible we would want to reduce these before adding them in.
-- ** 5. Reduce the overlaps using the above reducers.  When a collection of new GB elements are found,
--    They should be interreduced to ensure (1) holds for the next iteration.
///

--- FM: Working on endomorphismRing code.  Basically rewrote what used to be in NCAlgebras
---     the GB for the resulting ideal is wrong since we do not yet have GBs of NC algebras defined over commutative rings
restart
debug needsPackage "AssociativeAlgebras"
R = QQ[x,y,z,w]/ideal(x*w - y*z)
kRes = res coker vars R
M = ker kRes.dd_5
I = endomorphismRingIdeal(M,X);
-- would be nice to get the quotient working correctly.
A = ring I
B = A/I
--- but...
B_0  -- should not be zero...
netList pack(10, select(I_*, f -> #(terms f) > 1))
--- check that the relations are correct
all(I_*, f -> evaluateAt(endGens,f) == 0)

restart
debug needsPackage "AssociativeAlgebras"
needsPackage "NCAlgebra"
R = QQ
M = R^5
I = endomorphismRingIdeal(M,X);
A = ring I
B = A/I
C = R apply(numgens A, i -> Y_i)
phi = map(B,C,{X_5*X_1} | apply(24, i -> B_(i+1)))
ncKernel phi

--- how could we minimize the generators and relations in a systematic way?
--- I have something like this in NCAlgebra, but not sure if it is the best way.
--- e.g. note that X_23*X_19 = X_18.  Therefore X_18 is not needed as a generator,
--- so we should replace all occurrences of X_18 with X_23*X_19.
--- other examples: X_23*X_14 = X_13, X_23*X_9 = X_8 etc.

--- check that the relations are correct
all(I_*, f -> evaluateAt(endGens,f) == 0)

---- point and line scheme calculations
restart
debug needsPackage "AssociativeAlgebras"
kk = ZZ/32003
R = kk{x,y,z,w}
I = ideal {x*y-y*x-7*z*w-7*w*z, 3*x*z-4*y*w-3*z*x-4*w*y, 31*x*w+25*y*z+25*z*y-31*w*x, x*y+y*x-z*w+w*z, x*z+y*w+z*x-w*y, x*w-y*z+z*y+w*x}
I = ideal I_*; elapsedTime Igb = NCGB(I, 7, Strategy=>"F4");
S = R/I
radical lineSchemeFourDim(S,M)

restart
debug needsPackage "AssociativeAlgebras"
R = QQ toList(x_1..x_4)
I = ideal {x_3^2 - x_1*x_2, x_4^2 - x_2*x_1, x_1*x_3 - x_2*x_4, x_3*x_1 - x_2*x_3, x_1*x_4 - x_4*x_2, x_4*x_1 - x_3*x_2}
Igb = NCGB(I, 14);
S = R/I
all(15, i -> #(flatten entries ncBasis(i, S)) == binomial(i + 3,3))
LL = lineSchemeFourDim(S,M)
primaryDecomposition radical LL
netList oo
PP = pointScheme(S,y)
primaryDecomposition radical PP
netList oo

restart
needsPackage "AssociativeAlgebras"
R = ZZ/32003 <|x_4,x_1,x_2,x_3|>
I = ideal {x_3^2 - x_1*x_2, x_4^2 - x_2*x_1, x_1*x_3 - x_2*x_4,
           x_3*x_1 - x_2*x_3, x_1*x_4 - x_4*x_2, x_4*x_1 - x_3*x_2}
S = R/I
PP = pointScheme(S,y)
netList minimalPrimes PP

PPR = ring Mleft
MleftSpecial = sub(Mleft, {PPR_0 => 1, PPR_1 => 2, PPR_2 => 0, PPR_3 => 0})
MrightSpecial = sub(Mright, {PPR_0 => 1, PPR_1 => 2, PPR_2 => -1, PPR_3 => -2})
C = first minimalPrimes minors(4,Mleft)
Mleft % C
ker (Mleft % C)
rank MrightSpecial

restart
needsPackage "AssociativeAlgebras"
S1 = skewPolynomialRing(QQ,(-1)_QQ,{x_1,x_2})
P = pointScheme(S1,a)
netList minimalPrimes P
S1 = skewPolynomialRing(QQ,(-1)_QQ,{x_1,x_2,x_3})
P = pointScheme(S1,a)
netList minimalPrimes P
S1 = skewPolynomialRing(QQ,(-1)_QQ,{x_1..x_4})
P = pointScheme(S1,a)
netList minimalPrimes P
S2 = threeDimSklyanin (QQ,{3,5,7},{x,y,z})
P = pointScheme(S2,a)
netList minimalPrimes P
minimalPrimes ideal singularLocus P
S3 = threeDimSklyanin (QQ,{1,1,-2},{x,y,z})
P = pointScheme(S3,a)
netList minimalPrimes P
minimalPrimes ideal singularLocus P

R = QQ[zz,a_1,a_2,a_3]
PP = sub(P,R) + ideal {zz^2 + zz + 1}
minPP = minimalPrimes PP
minPP / degree

x = baseName x
R = ZZ/32003 <|x_4,x_1,x_2,x_3|>
I = ideal {x_3^2 - x_1*x_2, x_4^2 - x_2*x_1, x_1*x_3 - x_2*x_4,
           x_3*x_1 - x_2*x_3, x_1*x_4 - x_4*x_2, x_4*x_1 - x_3*x_2}
S4 = R/I
P = pointScheme(S4,a)
A = ring P
dim (A/P)
reduceHilbert hilbertSeries (A/P)
netList minimalPrimes P

a = 1
b = 1
c = -2

(3*a*b*c)^3 - (a^3 + b^3 + c^3)^3

S = threeDimSklyanin (frac(QQ[a,b,c]),{a,b,c},{x,y,z}, DegreeLimit => 3)
P = pointScheme(S,X)

R = frac(QQ[a,b,c])[X_1,X_2,X_3]
phi = map(R,ambient S,gens R)
lQ = leftQuadraticMatrix ideal S
phi lQ
det phi lQ
P = pointScheme(S,X)
netList minimalPrimes P

restart
needsPackage "AssociativeAlgebras"
S = skewPolynomialRing(QQ,1_QQ,{x_1,x_2,x_3,x_4})
L = lineSchemeFourDim(S,M);
netList minimalPrimes L
S = skewPolynomialRing(QQ,(-1)_QQ,{x_1,x_2,x_3,x_4})
L = lineSchemeFourDim(S,M);
netList minimalPrimes L
R = QQ <|x_4,x_1,x_2,x_3|>
I = ideal {x_3^2 - x_1*x_2, x_4^2 - x_2*x_1, x_1*x_3 - x_2*x_4, x_3*x_1 - x_2*x_3, x_1*x_4 - x_4*x_2, x_4*x_1 - x_3*x_2}
Igb = NCGB(I, 10);
S = R/I
all(15, i -> #(flatten entries ncBasis(i, S)) == binomial(i + 3,3))
L = lineSchemeFourDim(S,M);
netList minimalPrimes L

DEVELOPMENT ///

ncEngine Pull Request todo (2/22/2021)

-- Make sure F4 works with new vectorArithmetic
Faster nonminimal resolution code working with VA
-- Double check that faster arithmetic is in aring-zzp
-- Remove vectorArithmetic2
-- (partly) Find memory leaks in NCF4 code (?)
Remove gausser files (in both f4 and res-f4) once they are no longer used

Own branch:
Move mathicgb to M2 repo rather than as a submodule for finer control

Following pull request:
Associative Algebras:
	Quotients of quotients
	basis and gb hooks?
	think about flattenRing
	Clean up AssociativeAlgebras code
	Other QoL improvements (?)
Rename some vectorArithmetic functions for easier use
Add delayed modulus vectorArithmetic class?

Future Work:
NCF4 for inhomogeneous ideals
Delayed modulus VA class?
Continue work on resolution code
Begin work on modules (incl. GBs for modules)
///
