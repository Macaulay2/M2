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

Ring List := (A, varList) -> (
   -- get the symbols associated to the list that is passed in, in case the variables have been used earlier.
   varSymbols := NCAlgebra(A, toSequence varList);
   rawR := rawNCFreeAlgebra(raw A, toSequence(varSymbols/toString), raw degreesRing 1);
   R := new NCPolynomialRing from {
       (symbol RawRing) => rawR,
       (symbol generators) => {},
       (symbol generatorSymbols) => varSymbols,
       (symbol degreesRing) => degreesRing 1,
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


-- toExternalString
-- toString
-- expression
-- net

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
(a*a*b+a+1)*(a*a*b+a+1)
R_0
R_1 + R_2
a == b
a*b
a == R_0
raw(a-b)
promote(2,R)



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

