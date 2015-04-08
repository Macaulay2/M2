Steps to make NCalgebra arithmetic in the engine

. make a rawNCFreeAlgebra(coeff ring, NCmonoid?):
    NCAlgebra.m2 will call this function when making a ring
    in d/interface.dd: write a "hook" for the e dir routine
    in e/engine.h: write a declaration for this function
    in e/NCAlgebra.{hpp,cpp}, we include
      a type NCFreeAlgebra
      the class NCFreeAlgebra does the arithmetic.
        R->add(f,g) --> h ("raw pointers")

restart
debug Core    
kk = QQ
R = rawNCFreeAlgebra(raw kk, ("a","b","c"), raw degreesRing 1)
1_R
a = R_0
b = R_1
c = R_2
-a
a*b*a*b*b*a*a*a
a > b
a < b
a >= b
a <= b
a == b -- not sure why this is returning true
a*b*a*b*b*a*a*c > a*b*a*b*b*a*a*b
a*b*b*a*a*c > a*b*a*b*b*a*a*b
a*b*a*b*b*a*a*c > a*b*b*a*a*b
f = a+b+c
-- this thing takes up a lot of memory... 3^12 terms!
time(f*f*f*f*f*f*f*f*f*f*f*f);
time(f3 = f*f*f);
time(f3*f3*f3*f3);
g = a-b-c
f*g-g*f
f*f


restart
needsPackage "NCAlgebra"
R = QQ{a,b,c}
f = a+b+c
--elapsedTime time(f^12);
M = ncMatrix {{a,b},{c,a}}
elapsedTime(M^10);

restart
debug Core    
kk = QQ
R = rawNCFreeAlgebra(raw kk, ("a","b","c"), raw degreesRing 1)
1_R
a = R_0
b = R_1
c = R_2

matrix{{f}}
R^5
a * rawIdentity(R^5, 5)
rawMutableMatrix(R,4,4,true)
elems = toSequence flatten {{a,b,c},{b*a,c*a,a*c}}
M = rawMatrix1(R^2, 3, elems, 0)
N = rawDual M
M*N
oo * oo

elems = toSequence flatten {{a,b},{c,a}}
M = rawMatrix1(R^2, 2, elems, 0)
M*M*M
time (M*M*M*M*M*M*M*M*M*M);

M = rawMutableMatrix(R,4,4,false) -- crashes

-- Creating a new NCAlgebra Ring
restart
debug Core    
NCEngineRing = new Type of EngineRing
NCEngineRing.synonym = "NCEngineAlgebra"
NCRingElement = new Type of RingElement
new NCEngineRing from List := (NCEngineRing, inits) -> new NCEngineRing of NCRingElement from new HashTable from inits
Ring List := (R, varList) -> (
   -- get the symbols associated to the list that is passed in, in case the variables have been used earlier.
   if #varList == 0 then error "Expected at least one variable.";
   if #varList == 1 and class varList#0 === Sequence then varList = toList first varList;
   varList = varList / baseName;
   rawA := rawNCFreeAlgebra(raw R, toSequence(varList/toString), raw degreesRing 1);
   A := new NCEngineRing from {
       symbol rawRing => rawA,
       (symbol generators) => {},
       (symbol generatorSymbols) => varList,
       (symbol degreesRing) => degreesRing 1,
       (symbol CoefficientRing) => R,
       (symbol cache) => new CacheTable from {},
       (symbol baseRings) => {ZZ}
       };
   --newGens := apply(varList, v -> v <- new A from putInRing({v},A,1));
   newGens := for i from 0 to #varList-1 list varList#i <- new A from A.rawRing_i;
   A#(symbol generators) = newGens;
   A);
R = QQ {a,b,c}
R_0
gens R

kk = QQ
R = rawNCFreeAlgebra(raw kk, ("a","b","c"), raw degreesRing 1)
A = newNCEngineRing R;

