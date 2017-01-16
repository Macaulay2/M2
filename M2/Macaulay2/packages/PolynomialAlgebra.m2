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
    v := flatten toList apply(variables, x -> if class x === MutableList then toList x else x);
    for v0 in v list (
	    try baseName v0
	    else if instance(v0,String) and match("[[:alnum:]$]+",v0) then getSymbol v0
	    else error ("name " | v0 | " not usable as a variable")
       )
    )

{*getVariableSymbols = method()
getVariableSymbols List := variables -> (
    genSymbols := sequenceToVariableSymbols variables;

    << "symbols: " << genSymbols << endl;
    --newNCAlgebra(A, v)
    genSymbols
    )
*}

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
   if not (A.?Engine and A.Engine) then
       error "expected coefficient ring handled by the engine";
   varSymbols := sequenceToVariableSymbols toSequence varList;
   if #varSymbols == 0 then error "Expected at least one variable.";
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
   expression R := f -> (
	       (
		    -- apply the following function to the output of rawPairs
		    (coeffs,monoms) -> (
			 if #coeffs === 0
			 then expression 0
			 else sum(coeffs,monoms, (a,m) -> expression (if a == 1 then 1 else promote(a,A)) * expression (if m == 1 then 1 else m))
			 )
		    ) rawPairs(raw A, raw f)
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
  --- generators test
  debug Core -- for generatorSymbols
  R = QQ{a,b,c}; assert(R.generatorSymbols == splice {vars(0,1,2)})
  R = QQ{a,b,c}; assert(R.generatorSymbols == splice {vars(0,1,2)})
  R = QQ{{a,b,c},{d,e}}; assert(R.generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{(a,b,c),{d,e}}; assert(R.generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{(a,b,c),(d,e)}; assert(R.generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{b..f}; assert(R.generatorSymbols == splice {vars(1,2,3,4,5)})
  R = QQ{a,b,c}; assert(R.generatorSymbols == splice {vars(0,1,2)})
  R = QQ{x_1..x_100, y_1..y_100}; assert(numgens R == 200)
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
  f - g
  exprf = expression f
///

TEST ///
  --- promote/lift tests
  R = QQ{a,b,c}
  3_R
  assert(promote(3,R) == 3_R)
  assert(promote(23423/324,R) == (23423/324)_R)
  
  A = ZZ/101[s,t]
  B = A{x,y,z}
  promote(s,B)
  (s + x + y)^2


  A = ZZ/101[t]/t^2
  B = A{x,y,z}
  promote(t,B)
  t_B
  (t*x + t*y)^2 == 0
  (x + t*y)^2 == x^2 + t*x*y + t*y*x  -- should be equal, BUG
///

TEST ///
  R = QQ{b,c,d}
  f = 3*b^2*c*b + 2*b^4
  assert(size (b+c) == 2) -- WRONG
  pairs f -- fails
  leadTerm f -- fails
  leadCoefficient f -- fails
  terms f -- fails
  assert(degree f == {4})
  debug Core
  rawSparseListFormMonomial raw f
  rawPairs(raw coefficientRing R, raw f)
///

TEST ///
  R = QQ{b,c,d}
  M = R^2 -- works
  B = matrix {{b}}
  C = matrix {{c}}
  assert(B*C == matrix {{c*b}})
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

