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

sequenceToVariableSymbols = args -> (
    variables := splice args;
     v := flatten toList apply(variables, x->if class x === MutableList then toList x else x);
     v = for v0 in v list (
		    try baseName v0
		    else if instance(v0,String) and match("[[:alnum:]$]+",v0) then getSymbol v0
		    else error ("name " | v0 | " not usable as a variable")
        );
    v
    )

NCAlgebra = method()
NCAlgebra(Ring, Sequence) := (A, variables) -> (
    if not (A.?Engine and A.Engine) then
      error "expected coefficient ring handled by the engine";
    genSymbols := sequenceToVariableSymbols variables;
    if #genSymbols == 0 then error "Expected at least one variable.";
    << "symbols: " << genSymbols << endl;
    --newNCAlgebra(A, v)
    genSymbols
    )

-- TODO (11 Jan 2017 MS+FM)
--   1. handle multigradings
--   2. handle different monomial orders
--     e.g. elimination order.
--   3. check on: ring map evaluations, promote, lift?
--   4. look at other top level rings
--   5. net, expression, toString...
--   6. check on listForm.
--   7. use, ...
--   8. make sure multiplication is using a heap based approach.
Ring List := (A, varList) -> (
   -- get the symbols associated to the list that is passed in, in case the variables have been used earlier.
   varSymbols := NCAlgebra(A, toSequence varList);
   degreelen := 1;
   rawR := rawNCFreeAlgebra(raw A, toSequence(varSymbols/toString), raw degreesRing degreelen);
   R := new NCPolynomialRing from {
       (symbol RawRing) => rawR,
       (symbol generators) => {},
       (symbol generatorSymbols) => varSymbols,
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
   net R := f -> net raw f;
   R
   );
NCPolynomialRing _ ZZ := (R, n) -> (R.generators)#n
coefficientRing NCPolynomialRing := R -> last R.baseRings

degreesRing NCPolynomialRing := PolynomialRing => R -> (
   if R#?(symbol degreesRing) then R#(symbol degreesRing)
   else error "no degreesRing for this ring"
   )

-- toExternalString
-- toString
-- expression
-- net
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
  debug Core -- for generatorSymbols
  R = QQ{a,b,c}; assert(R.generatorSymbols == splice {vars(0,1,2)})
  R = QQ{a,b,c}; assert(R.generatorSymbols == splice {vars(0,1,2)})
  R = QQ{{a,b,c},{d,e}}; assert(R.generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{(a,b,c),{d,e}}; assert(R.generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{(a,b,c),(d,e)}; assert(R.generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{b..f}; assert(R.generatorSymbols == splice {vars(1,2,3,4,5)})
  R = QQ{a,b,c}; assert(R.generatorSymbols == splice {vars(0,1,2)})
  R = QQ{x_1..x_10}
  R = QQ{x_1..x_100, y_1..y_100}
  numgens R == 200
///
end

restart
installPackage "PolynomialAlgebra"
uninstallPackage "PolynomialAlgebra"
viewHelp "PolynomialAlgebra"

-- XXX
restart
needsPackage "PolynomialAlgebra"
debug Core
check PolynomialAlgebra

R = QQ{a,b,c}
gens R
R.RawRing
degreesRing R

3_R
promote(3,R) -- interesting, this is not calling promote?
(23423/324)_R
a+b
-a
a == b
a == a
b*a + a*b + b*a
(a*a*b+a+1)*(a*a*b+a+1) == a^2*b*a^2*b+a^3*b+a^2*b*a+2*a^2*b+a^2+2*a+1 -- ?
f = a^2*b*a^2*b+a^3*b+a^2*b*a+2*a^2*b+a^2+2*a+1
g = (a*a*b+a+1)*(a*a*b+a+1) - 1
f - g == 0
f == g -- isEqual bug?
R_0
R_1 + R_2
a == b
a*b
a == R_0
raw(a-b)
promote(2,R)
--- matrices!
M = matrix {{a,b}}
transpose M
M * transpose M
transpose M * M
matrix {{a}} * matrix {{b}}

-- trying to get pairs from the engine working so 'coefficients' will work.
f = 3*a^2*b*a + 2*a^4
pairs f -- weird return value
leadTerm f -- fails
leadCoefficient f -- fails
terms f -- fails
degree f -- wrong answer
rawPairs(raw coefficientRing R, raw f) -- folded incorrectly
first last oo
rawSparseListFormMonomial oo
M = R^2 -- works
N = mutableMatrix(R,2,3) -- SIGSEGV

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

