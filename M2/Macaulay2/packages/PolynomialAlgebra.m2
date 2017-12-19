newPackage(
        "PolynomialAlgebra",
        Version => "0.1", 
        Date => "16 Feb 2016",
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

export {
    "NCPolynomialRing",
    "sequenceToVariableSymbols"
    }

debug Core

NCPolynomialRing = new Type of EngineRing
NCPolynomialRing.synonym = "noncommutative polynomial ring"

new NCPolynomialRing from List := (EngineRing, inits) -> new EngineRing of RingElement from new HashTable from inits

getNameIfAny = (k) -> expression if hasAttribute(k,ReverseDictionary) then getAttribute(k,ReverseDictionary) else k

-- MES.  do we really need all 5 of these?
expression NCPolynomialRing := R -> (
     if hasAttribute(R,ReverseDictionary) then return expression getAttribute(R,ReverseDictionary);
     k := last R.baseRings;
     (getNameIfAny k) (R.generatorSymbols)
     )
net NCPolynomialRing := R -> (
     if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
     else net expression R)
describe NCPolynomialRing := R -> (
     k := last R.baseRings;
     net ((getNameIfAny k) R.generatorSymbols)
     )
toExternalString NCPolynomialRing := R -> (
    --toString describe R
     k := last R.baseRings;
     toString ((getNameIfAny k) R.generatorSymbols)
     )
toString NCPolynomialRing := R -> (
    toString expression R
    -- 
    -- if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
    -- else toString expression R
    )

-- Currently, we are using findSymbols in m2/ofcm.m2 instead of this.
sequenceToVariableSymbols = args -> (
    variables := splice args;
    v := flatten toList apply(variables, x -> if class x === MutableList then toList x else x);
    for v0 in v list (
	    try baseName v0
	    else if instance(v0,String) and match("[[:alnum:]$]+",v0) then getSymbol v0
	    else error ("name " | v0 | " not usable as a variable")
       )
    )

-- MES: remove this older code:
-* getVariableSymbols = method()
getVariableSymbols List := variables -> (
    genSymbols := sequenceToVariableSymbols variables;

    << "symbols: " << genSymbols << endl;
    --newNCAlgebra(A, v)
    genSymbols
    )
*-

-- TODO (11 Jan 2017 MS+FM), now (19 Dec 2017 MS+FM)
--   1. handle multigradings
--   2. handle different monomial orders
--     e.g. elimination order.
--   3. check on: ring map evaluations, promote, lift?
--   4. look at other top level rings (and do what with them?)
--   5. net, expression, toString...  DONE
--   6. check on listForm (what is the issue?)
--   7. use, make it so we can create this ring without assigning the variables to global symbols
--   8. make sure multiplication is using a heap based approach.
-- TODO for PolynomialAlgebra.hpp,cpp:
--   - make sure that bringing in a zero element brings in a zero element!
--   - make monoid routines for the cryptic uses.
Ring List := (A, varList) -> (
   -- get the symbols associated to the list that is passed in, in case the variables have been used earlier.
   if not (A.?Engine and A.Engine) then
       error "expected coefficient ring handled by the engine";
   --varSymbols := sequenceToVariableSymbols toSequence varList;
   varSymbols := findSymbols toSequence varList;
   if #varSymbols == 0 then error "Expected at least one variable.";
   degreelen := 1;
   rawR := rawNCFreeAlgebra(raw A, toSequence(varSymbols/toString), raw degreesRing degreelen);
   R := new NCPolynomialRing from {
       (symbol RawRing) => rawR,
       (symbol generators) => {},
       (symbol generatorSymbols) => varSymbols,
       --(symbol generatorExpressions) => hashTable apply(#varList, i -> (i,expression varList#i)),
       (symbol generatorExpressions) => for v in varSymbols list expression v,
       (symbol degreesRing) => degreesRing degreelen,
       (symbol degreeLength) => degreelen,
       (symbol CoefficientRing) => A,
       (symbol cache) => new CacheTable from {},
       (symbol baseRings) => append(A.baseRings,A)
       };
   newGens := for i from 0 to #varSymbols-1 list varSymbols#i <- new R from R.RawRing_i;
   R.generators = newGens;
   commonEngineRingInitializations R;
   --- need to fix net of an RingElement coming from a NCPolynomial ring.
   processFactor := (k,v) -> if v =!= 1 then Power{R.generatorExpressions#k, v} else R.generatorExpressions#k;
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
   R
   );

NCPolynomialRing _ ZZ := (R, n) -> (R.generators)#n
coefficientRing NCPolynomialRing := R -> last R.baseRings

degreesRing NCPolynomialRing := PolynomialRing => R -> (
   if R#?(symbol degreesRing) then R#(symbol degreesRing)
   else error "no degreesRing for this ring"
   )

isWellDefined NCPolynomialRing := Boolean => R -> (
    -- an internal check that R is defined properly.
    isbad := str -> (if debugLevel > 0 then << str; false);
    if # R.generators =!= numgens R then 
        return isbad "# R.generators =!= numgens R";
    if # R.generatorExpressions =!= numgens R then 
        return isbad "# R.generatorExpressions =!= numgens R";
    if # R.generatorSymbols =!= numgens R then 
        return isbad "# R.generatorSymbols =!= numgens R";
    if not all(R.generators, x -> class x === R) then 
        return isbad "generators are not all in the ring";
    if not all(R.generatorExpressions, x -> instance(x,Expression)) then
        return isbad "generatorExpressions are not all expressions";
    if not all(R.generatorSymbols, x -> instance(x, Symbol) or instance(x, IndexedVariable)) then
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
beginDocumentation()

doc ///
Key
  PolynomialAlgebra
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
  Not yet functional
SeeAlso
///

TEST ///
-*
  restart
  needsPackage "PolynomialAlgebra"
*-
  --- generators test
  debug Core -- for generatorSymbols
  R = QQ{a,b,c}; assert(R.generatorSymbols == splice {vars(0,1,2)})
  assert isWellDefined R
    
  R = QQ{a,b,c}; assert(R.generatorSymbols == splice {vars(0,1,2)})
  assert isWellDefined R

  R = QQ{a,b, x_1..x_3, c, y_1..y_4}
  assert(numgens R == 10)
  debugLevel = 1
  isWellDefined R

  R = QQ{{a,b,c},{d,e}}; assert(R.generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{(a,b,c),{d,e}}; assert(R.generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{(a,b,c),(d,e)}; assert(R.generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{b..f}; assert(R.generatorSymbols == splice {vars(1,2,3,4,5)})
  R = QQ{a,b,c}; assert(R.generatorSymbols == splice {vars(0,1,2)})
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
  --- equality tests
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
  needsPackage "PolynomialAlgebra"
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
  pairs f -- WRONG
  leadTerm f -- fails
  leadCoefficient f -- fails
  assert(# terms f == 2)
  assert(sum terms f == f) -- WRONG (monomial is reversed...)
  assert(degree f == {4}) -- fails
  debug Core
  rawP = rawPairs(raw coefficientRing R, raw f)
  last rawP / rawSparseListFormMonomial
///

TEST ///
  R = QQ{b,c,d}
  M = R^2 -- works
  B = matrix {{b}}
  C = matrix {{c}}
  assert(B*C == matrix {{c*b}})
  N = mutableMatrix(R,2,3); -- ok
  N = mutableMatrix(R,2,3) -- SIGSEGV
  D = matrix {{b,c}}
  assert(D * transpose D == matrix {{b^2 + c^2}})
  assert(transpose D * D == matrix {{b^2,c*b},{b*c,c^2}})
///

end--

-- The following is the directory of my M2 build.
workingDir = ~/Macaulay2/M2-frank/M2/BUILD/frank/builds.tmp/darwin64-clang/M2

restart
needsPackage "PolynomialAlgebra"
installPackage "PolynomialAlgebra"
uninstallPackage "PolynomialAlgebra"
viewHelp "PolynomialAlgebra"
check "PolynomialAlgebra"

-- XXX
restart
needsPackage "PolynomialAlgebra"
debug Core
check PolynomialAlgebra

-- play with listForm
-- calls rawPairs, which calls IM2_RingElement_list_form in engine
-- each ring has its own "list_form(coeff ring, ring_elem)"

-- TODO: 1/13/17 MS+FM
-- 1. isEqual
-- 2. mutable matrices
-- 3. promote/lift
-- 4. pairs and rawPairs, etc
-- 5. leadTerm/Coefficient/Monomial
-- 6. terms
-- 7. degrees/weights of variables

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

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

restart
needsPackage "PolynomialAlgebra"
debug Core
A = QQ[x,y]
R = A{b,c,d}
f = 3*x*y*b^2*c*b + 2*b^4
rawP = rawPairs(raw coefficientRing R, raw f)
(rawP#0, rawP#1 / rawSparseListFormMonomial)
-- this code can be used, for example, to get the 'monomial part' of terms.
toList apply(last rawP / rawSparseListFormMonomial, t -> product(apply(t, p -> R_(p#0)^(p#1))))
--- this is how terms is computed.  rawTerm calls IM2_RingElement_term in the engine.
--- Q: Do we change this code to work for PolynomialAlgebra objects as well, or will they
---    get their own function?  (We added a separate function for these types of things in the past).
apply(rawP#0,rawP#1,(c,m) -> new R from rawTerm(raw R, c, m))
