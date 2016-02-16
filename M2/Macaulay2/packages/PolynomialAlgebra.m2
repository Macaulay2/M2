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
NCAlgebra(EngineRing, Sequence) := (A, variables) -> (
    v := sequenceToVariableSymbols variables;
    << "symbols: " << v << endl;
    --newNCAlgebra(A, v)
    v
    )

Ring List := (R, varList) -> (
   -- get the symbols associated to the list that is passed in, in case the variables have been used earlier.
   varList = sequenceToVariableSymbols varList;
   if #varList == 0 then error "Expected at least one variable.";
   if #varList == 1 and class varList#0 === Sequence then varList = toList first varList;
   varList = varList / baseName;
   rawA := rawNCFreeAlgebra(raw R, toSequence(varList/toString), raw degreesRing 1);
   A := new NCPolynomialRing from {
       (symbol rawRing) => rawA,
       (symbol generators) => {},
       (symbol generatorSymbols) => varList,
       (symbol degreesRing) => degreesRing 1,
       (symbol CoefficientRing) => R,
       (symbol cache) => new CacheTable from {},
       (symbol baseRings) => {ZZ,R}
       };
   newGens := for i from 0 to #varList-1 list varList#i <- new A from A.rawRing_i;
   --A#(symbol generators) = newGens;
   A.generators = newGens;
   --- need to fix net of an RingElement coming from a NCPolynomial ring.
   net A := f -> net raw f;
   A);
NCPolynomialRing _ ZZ := (A, n) -> (A.generators)#n
coefficientRing NCPolynomialRing := A -> last A.baseRings


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
    R = QQ{symbol a, symbol b, symbol c}
    R_0
    R_1
    R_2
Caveat
  Not yet functional
SeeAlso
///

end

-- XXX
restart
needsPackage "PolynomialAlgebra"
R = QQ{a,b,c}
R = QQ{a,b,c}
installPackage "PolynomialAlgebra"
uninstallPackage "PolynomialAlgebra"
viewHelp "PolynomialAlgebra"

a+b
R_0
R_1 + R_2
a == b
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

