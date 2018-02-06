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
       (symbol generatorExpressions) => for v in varSymbols list if instance(v,Symbol) then v else expression v,
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
    if not all(R.generatorExpressions, x -> instance(x,Expression) or instance(x,Symbol)) then
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

  restart
  check "PolynomialAlgebra"

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

TEST ///
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
  needsPackage "PolynomialAlgebra"
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

TEST ///
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

TEST ///
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

TEST ///
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
  -- XXX
  R = QQ{b,c,d}
  f = 3*b^2*c*b + 2*b^4
  assert(size (b+c) == 2)
  terms f == {2*b^4, 3*b^2*c*b}
  assert(# terms f == 2)
  assert(sum terms f == f)
///

TEST /// 
  R = QQ{b,c,d}
  f = 3*b^2*c*b + 2*b^4
  leadTerm f -- fails
  leadCoefficient f -- fails
  assert(degree f == {4}) -- fails
  someTerms(f,0,2) -- fails
///

TEST ///
  R = QQ{a,b,c,d}
  {b*c}
  e = {{b,c,d,b*c,c*b,b^2,a*c-1}}
  M = matrix for j from 1 to 10 list for i from 1 to 10 list a*b-i*a-j*b
  M_(1,1)
  B = matrix {{b}}
  C = matrix {{c}}
  assert(B*C == matrix {{c*b}})
  D = matrix {{b,c}}
  assert(D * transpose D == matrix {{b^2 + c^2}})
  assert(transpose D * D == matrix {{b^2,c*b},{b*c,c^2}})
///

TEST ///
  R = QQ{b,c,d}
  M = R^2 -- works
  B = matrix {{b}}
  C = matrix {{c}}
  assert(B*C == matrix {{c*b}})
  N = mutableMatrix(R,2,3);
  N = mutableMatrix(R,2,3)
  N = mutableMatrix(R,100,200);
  N_(1,1)
  D = matrix {{b,c}}
  assert(D * transpose D - matrix {{b^2 + c^2}} == 0)
  assert(transpose D * D == matrix {{b^2,c*b},{b*c,c^2}}) -- is this test wrong?
///

TEST ///
  restart
  needsPackage "PolynomialAlgebra"

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
///

TEST ///
-*
  restart
  needsPackage "PolynomialAlgebra"
*-
  R = QQ{a,b,c,d}
  M = matrix{{a*b*c-2*a*a*b*a}}
  assert(monomials M == matrix{{a^2*b*a, a*b*c}})
  coefficients M
///

TEST ///
-*
  restart
  needsPackage "PolynomialAlgebra"
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
