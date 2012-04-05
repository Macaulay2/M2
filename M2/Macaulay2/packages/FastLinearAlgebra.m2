-- -*- coding: utf-8 -*-
newPackage(
	"FastLinearAlgebra",
	AuxiliaryFiles => false,
    	Version => "0.1",
    	Date => "May 12, 2011",
	Authors => {
	     {Name => "Michael E. Stillman", 
		  Email => "mike@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"},
	     {Name => "Jakob Kroeker", 
		  Email => "Jakob Kröker <kroeker@uni-math.gwdg.de>", 
		  HomePage => "" }
	     },
    	Headline => "Fast linear algebra over finite fields: interface to ffpack",
        DebuggingMode => true
    	)


-- also: rank, det, solve
export {
   ARing,
   ZZp,
   powerMod,
   	
   testDivide,
    testAdd,
    testNegate,
    testSubtract,
    testMultiply,
    testAxioms,
    testGFArithmetic,
    constructGivaroField,
    constructZZpFFPACK,
    constructMacaulayGF,
    constructMacaulayZZp,
    testField,

     RightSide,
     nullSpace,
     invert,
     rowRankProfile,
     columnRankProfile,
     addMultipleTo,
     solveLinear,
     TransposeA,
     TransposeB,
     Alpha,
     Beta
--     ,
--     columnRankProfile,
--     rowRankProfile
     }

debug Core

savedZZpQuotients := new MutableHashTable

powerMod = method()
powerMod(ZZ,ZZ,ZZ) := (a,b,c) -> error "ha ha: not done yet!"

-- THIS FUNCTION IS NOT FUNCTIONAL!!
-- reason: we need a function: powerMod(a, b, c) is a^b mod c.
-- write powerMod
isGenerator = (n,P) -> (
     -- n is an integer in the range 2..P-1
     -- P is a prime number
     -- returns true if n is a generator of the group of units of ZZ/P
     if P === 2 then n == 1
     else (
       n >= 2 and n <= P-1 and (
	    all(factor (P-1), v -> 1 != powerMod(n, (P-1)//v#0, P))))
  )

ZZp = method(Options=> {ARing => true})
ZZp ZZ := opts -> (n) -> ZZp(ideal n, opts)
ZZp Ideal := opts -> (I) -> (
     gensI := generators I;
     if ring gensI =!= ZZ then error "expected an ideal of ZZ";
     n := gcd flatten entries gensI;
     if n < 0 then n = -n;
     if n === 0 then ZZ
     else if savedZZpQuotients#?n 
     then savedZZpQuotients#n
     else (
	  --if n > 32767 then error "large characteristics not implemented yet";
	  if n > 1 and not isPrime n
	  then error "ZZ/n not implemented yet for composite n";
	  S := new QuotientRing from 
	    if opts.ARing then rawARingGaloisField(n,1)  else rawZZp n;  -- rawARingZZp n
	  S.cache = new CacheTable;
	  S.isBasic = true;
	  S.ideal = I;
	  S.baseRings = {ZZ};
     	  commonEngineRingInitializations S;
	  S.relations = gensI;
	  S.isCommutative = true;
	  S.presentation = matrix{{n}};
	  S.order = S.char = n;
	  S.dim = 0;					    -- n != 0 and n!= 1
	  expression S := x -> expression raw x;
	  fraction(S,S) := S / S := (x,y) -> x//y;
	  S.frac = S;		  -- ZZ/n with n PRIME!
	  savedZZpQuotients#n = S;
	  --lift(S,QQ) := opts -> liftZZmodQQ;
	  S))


isPrimeField = method()
isPrimeField Ring := (R) -> (
     true
     )

rank MutableMatrix := (M) -> rawLinAlgRank raw M

determinant MutableMatrix := opts -> (M) -> (
     R := ring M;
     new R from rawLinAlgDeterminant raw M
     )

invert = method()
invert MutableMatrix := (A) -> (
     R := ring A;
     if numRows A =!= numColumns A then error "expected square matrix";
     map(R,rawLinAlgInvert(raw A))
     )

rowRankProfile = method()
rowRankProfile MutableMatrix := (A) -> rawLinAlgRankProfile(raw A, true)

columnRankProfile = method()
columnRankProfile MutableMatrix := (A) -> rawLinAlgRankProfile(raw A, false)

nullSpace = method(Options => {RightSide=>true})
nullSpace(MutableMatrix) := opts -> (M) -> (
     R := ring M;
     map(R, rawLinAlgNullSpace(raw M, opts.RightSide))
     )

solveLinear = method(Options => options nullSpace)
solveLinear(MutableMatrix, MutableMatrix) := opts -> (A,B) -> (
     -- solve A*X = B, or solve X*A = B
     R := ring A;
     if ring A =!= ring B then error "expected same base rings";
     map(R,rawLinAlgSolve(raw A,raw B,opts.RightSide))
     )

addMultipleTo = method(Options => {
	  TransposeA => false, 
	  TransposeB => false, 
	  Alpha => null, 
	  Beta => null})

addMultipleTo(MutableMatrix,MutableMatrix,MutableMatrix) := opts -> (C,A,B) -> (
     R := ring C;
     if ring A =!= ring C or ring B =!= ring C then 
       error "expected matrices over the same ring";
     a := if opts.Alpha === null then 1_R else opts.Alpha;
     b := if opts.Beta === null then 1_R else opts.Beta;
     rawLinAlgAddMultipleTo(raw C, raw A, raw B,
	  opts.TransposeA, 
	  opts.TransposeB, 
	  raw a, raw b);
     C)

MutableMatrix * MutableMatrix := (A,B) -> (
     C := mutableMatrix(ring A, numRows A, numColumns B, Dense=>true);
     addMultipleTo(C,A,B)
     )

fastRank= method();

fastRank MutableMatrix := (M) -> (
     R := ring M;
     if isPrimeField R and char R > 0 then (
	  new ZZ from rawFFPackRank raw M
	  )
     )

------------tests

testEqual = method();
testEqual (MutableHashTable, ZZ) := (fieldHashTable, nrtests)->
(
    --todo
    -- require random function.
)
testNegate = method();
testNegate (MutableHashTable, ZZ) := (fieldHashTable,nrtests)->
(
    galoisField := fieldHashTable#"field";
    fieldGen := fieldHashTable#"generator";
    cardinality := fieldHashTable#"cardinality";
    apply(nrtests,i->(  rnd:=random cardinality; 
                        assert( fieldGen^rnd + (- fieldGen^rnd) == 0_galoisField );
                    ) 
        );
)


--depends on correct testNegate.
testAdd = method();
testAdd (MutableHashTable, ZZ) := (fieldHashTable, nrtests )->
(
    galoisField := fieldHashTable#"field";
    fieldGen := galoisField_0;
    cardinality := fieldHashTable#"cardinality";
    apply( nrtests, i->( rnd1:=random cardinality; 
                         rnd2:=random cardinality; 
                         assert( fieldGen^rnd1 + fieldGen^rnd2 + (-(fieldGen)^rnd1) == fieldGen^rnd2);
                    ) 
        );
)


testSubtract = method();
testSubtract (MutableHashTable, ZZ) := (fieldHashTable, nrtests)->
(
   galoisField := fieldHashTable#"field";
  fieldGen := fieldHashTable#"generator";
    cardinality := fieldHashTable#"cardinality";
    apply( nrtests,i->( rnd1 := random cardinality;   
                        rnd2 := random cardinality; 
                        assert( fieldGen^rnd1 + fieldGen^rnd2 - fieldGen^rnd1 == fieldGen^rnd2);) 
        );
)

-- relies on correct pow and comparison
testMultiply = method();
testMultiply (MutableHashTable, ZZ) :=(fieldHashTable,nrtests)->
(
    galoisField := fieldHashTable#"field";
      fieldGen := fieldHashTable#"generator";
    Zero := 0_galoisField;
     cardinality := fieldHashTable#"cardinality";
    characteristic := fieldHashTable#"characteristic";
    apply( nrtests, i->( rnd1:=random cardinality;  assert( fieldGen^rnd1*Zero == Zero );) );
    --
    apply(nrtests, i->( rnd1:=random characteristic;
                        rnd2:=random characteristic; 
                        addresult := Zero; 
                        apply(rnd1,j->addresult = addresult + fieldGen^rnd2); 
                        assert(fieldGen^rnd2* rnd1_galoisField == addresult);) 
        );
)

-- relies on correct pow and comparison
testAxioms = method();
testAxioms (MutableHashTable, ZZ) := (fieldHashTable, nrtests)->
(
    galoisField := fieldHashTable#"field";
   fieldGen := fieldHashTable#"generator";
    cardinality := fieldHashTable#"cardinality";
    Zero := 0_galoisField;
    apply( nrtests, i->( rnd1:=random cardinality;  assert( fieldGen^rnd1*Zero == Zero );) );
    -- test commutative 
    apply( nrtests, i->(      rnd1:=random cardinality;
                            rnd2 := random cardinality; 
                            assert( fieldGen^rnd1 * fieldGen^rnd2 == fieldGen^rnd2 * fieldGen^rnd1);
                        assert( fieldGen^rnd1 + fieldGen^rnd2 == fieldGen^rnd2 + fieldGen^rnd1);
                    ) 
    );
    --
    -- test associative and distributive
    apply( nrtests, i->(      rnd1:=random cardinality;
                            rnd2 := random cardinality; 
                            rnd3 := random cardinality;
                            -- test associative
                            assert( (fieldGen^rnd1 * fieldGen^rnd2) * fieldGen^rnd3 == fieldGen^rnd2 * (fieldGen^rnd1 * fieldGen^rnd3) );
                            assert( (fieldGen^rnd1 + fieldGen^rnd2) + fieldGen^rnd3 == fieldGen^rnd2 + (fieldGen^rnd1 + fieldGen^rnd3) );
                            -- test distributive
                            assert( (fieldGen^rnd1 + fieldGen^rnd2) * fieldGen^rnd3 == fieldGen^rnd1 * fieldGen^rnd3  + fieldGen^rnd2 * fieldGen^rnd3 );
                    ) 
    );
)

--relies on correct multiplication and comparison
testDivide = method();
testDivide (MutableHashTable, ZZ) := ( fieldHashTable, nrtests )->
(
    galoisField := fieldHashTable#"field";
     fieldGen := fieldHashTable#"generator";
    cardinality := fieldHashTable#"cardinality";
    One := 1_galoisField;
    apply( nrtests, i->(    rnd1 := random cardinality;
                            assert(( One // fieldGen^rnd1) * fieldGen^rnd1 == One );
                       )
        );
    apply( cardinality, i->(  assert(( One // fieldGen^i )*fieldGen^i == One);) );
    --
    apply( nrtests, i->(    rnd1 := random cardinality; 
                            rnd2 := random cardinality; 
                            assert( (fieldGen^rnd2 // fieldGen^rnd1)*fieldGen^rnd1 == fieldGen^rnd2);
                    ) 
        );
)


testPower = method();
testPower (MutableHashTable) := (fieldHashTable)->
(
    galoisField := fieldHashTable#"field";
  fieldGen := fieldHashTable#"generator";
    cardinality := fieldHashTable#"cardinality";
    apply( cardinality-2 , i-> (print (fieldGen^(i+2)); assert( fieldGen^(i+2) != fieldGen)));
    
    assert( fieldGen^cardinality == fieldGen);
    One := 1_galoisField; 
    assert( fieldGen^(cardinality-1)   == One);
    assert( fieldGen^(-2) * (fieldGen^2) == One );
    assert( fieldGen^(-1) * fieldGen   == One );
    -- overflow test is not possible here because of big memory consumption...
    -- idea for overflow test: suppose elem= generator^k.  Then elem^m  is implemented as 
    --                         generator^(k*m mod cardinality). If k*m implementation overflows this would result in an error.
    --
    --B := rawARingGaloisField(30013,2);
    --c:= (B_0)^100040;
    --c^100040;
    --ZZP=ZZ/997;
)

constructGivaroField = method();
constructGivaroField (ZZ,ZZ) := (characteristic,dimension)->
(
    result := new MutableHashTable;
    --
    result#"field" = rawARingGaloisField(characteristic, dimension );
    result#"char" =  characteristic;
    result#"characteristic" =  characteristic;
    result#"dimension" =  dimension;
    result#"cardinality" =  characteristic^dimension;
    result#"generator" = (result#"field")_0;
    assert( result#"generator"!=0_(result#"field"));
    return result;
)
constructZZpFFPACK = method();
constructZZpFFPACK (ZZ,ZZ) := (characteristic,dimension)->
(
    assert(dimension == 1);
    result := new MutableHashTable;
    --
    result#"field" = ZZp (ideal characteristic);
    result#"char" =  characteristic;
    result#"characteristic" =  characteristic;
    result#"dimension" =  dimension;
    result#"cardinality" =  characteristic^dimension;

    tmp:=GF(characteristic,1);
    prim := (gens tmp)_0;
    result#"generator" = sub(prim, result#"field");
--    result#"generator"= (gens tmp)_0;
    assert( result#"generator"!=0);

    return result;
)

constructMacaulayGF = method();
constructMacaulayGF (ZZ,ZZ) := (characteristic,dimension)->
(
    result := new MutableHashTable;
    --
    result#"field" = GF(characteristic, dimension );
    result#"char" =  characteristic;
    result#"characteristic" =  characteristic;
    result#"dimension" =  dimension;
    result#"cardinality" =  characteristic^dimension;
    result#"generator" = (result#"field")_0;
    assert( result#"generator"!=0_(result#"field"));
    return result;
)
constructMacaulayZZp = method();
constructMacaulayZZp (ZZ,ZZ) := (characteristic,dimension)->
(
    assert(dimension == 1);
    result := new MutableHashTable;
    --
    result#"field" = ZZ/characteristic;
    result#"char" =  characteristic;
    result#"characteristic" =  characteristic;
    result#"dimension" =  dimension;
    result#"cardinality" =  characteristic^dimension;
    --todo:
    tmp:=GF(characteristic,1);
    result#"generator"= (gens tmp)_0;
    return result;
)

testField = method();
testField (MutableHashTable,ZZ) := (fieldHashTable,nrTests )->
(
    assert( fieldHashTable#"generator" != 0 );
    assert( fieldHashTable#"generator" != 1 );
    --testEqual(fieldHashTable, nrTests);
    testPower(fieldHashTable);
    testDivide(fieldHashTable, nrTests);
    testAdd(fieldHashTable,    nrTests);
    testNegate(fieldHashTable, nrTests);
    testSubtract(fieldHashTable, nrTests);
    testMultiply(fieldHashTable, nrTests);
    testAxioms(fieldHashTable, nrTests);
);

testGFArithmetic = method()
testGFArithmetic GaloisField := (F) -> (
     R := ambient F;
     Q := F.order;
     a := R_0;
     b := F_0;
     elemsR := prepend(0_R, for i from 1 to Q-1 list a^i);
     elemsF := prepend(0_F, for i from 1 to Q-1 list b^i);
     for i from 0 to Q-1 do assert(elemsR#i == lift(elemsF#i, R));
     -- negation
     for i from 0 to Q-1 do assert(- elemsR#i == lift(- elemsF#i, R));
     -- addition table
     for i from 0 to Q-1 do
  	  for j from 0 to Q-1 do
	       assert(elemsR#i + elemsR#j == lift(elemsF#i + elemsF#j, R));
     -- subtraction table
     for i from 0 to Q-1 do
  	  for j from 0 to Q-1 do
	       assert(elemsR#i - elemsR#j == lift(elemsF#i - elemsF#j, R));
     -- multiplication table
     for i from 0 to Q-1 do
  	  for j from 0 to Q-1 do
	       assert(elemsR#i * elemsR#j == lift(elemsF#i * elemsF#j, R));
     -- division table
     for i from 0 to Q-1 do
  	  for j from 1 to Q-1 do
	       assert(elemsR#i // elemsR#j == lift(elemsF#i // elemsF#j, R));
     -- powers table
     for i from 0 to Q-1 do 
  	  for j from -Q to Q-1 do (
	       if i == 0 and j <= 0 then continue;
	       assert(elemsR#i ^ j == lift(elemsF#i ^ j, R)));
     -- big powers table
     for i from 0 to Q-1 do
  	  for j from -Q to Q-1 do ( 
	       N := j + 894723897542398472389;
	       assert(elemsR#i ^ N == lift(elemsF#i ^ N, R)));
     )

beginDocumentation()


TEST ///
needsPackage "FastLinearAlgebra"
debug Core

 
nrTests = 100; -- may depend on cardinality, e.g. 1 % or so
fieldHashTable = constructGivaroField( 37,1 );
testField(fieldHashTable, nrTests);

nrTests = 100; -- may depend on cardinality, e.g. 1 % or so
fieldHashTable = constructGivaroField( 5,3 );
testField(fieldHashTable, nrTests);

nrTests = 100; -- may depend on cardinality, e.g. 1 % or so
fieldHashTable = constructGivaroField( 11,2 );
testField(fieldHashTable, nrTests);
fieldHashTable = constructGivaroField( 2,3 );
testField(fieldHashTable, nrTests);

fieldHashTable = constructMacaulayGF( 5,3 );
testField(fieldHashTable, nrTests);

fieldHashTable = constructMacaulayZZp(23,1);
testField(fieldHashTable, nrTests);

fieldHashTable = constructZZpFFPACK(23,1);
testField(fieldHashTable, nrTests);

 R = rawARingGaloisField(2,3)


///



TEST ///
 
restart
needsPackage "FastLinearAlgebra"
kk = ZZ/101
A = mutableMatrix random(kk^2, kk^2)
rank A

kk = GF(2^4, Strategy=>"New")
A = mutableMatrix random(kk^2, kk^2)
rank A
 
///

TEST ///
    kk = ZZp 101;
    --kk = ZZ/101
    A = random(kk^2, kk^4)
    B = random(kk^5, kk^2)
    M = mutableMatrix(B*A)
    N = nullSpace M
    assert((matrix M) * (matrix N) == 0)
    N = nullSpace(M, RightSide=>false)
    assert((matrix N) * (matrix M) == 0)
///

TEST ///
    kk = ZZ/101
    A = random(kk^23, kk^400)
    B = random(kk^500, kk^23)
    M = mutableMatrix(B*A);
    N = nullSpace M;
    assert(numRows N == numColumns M)
    assert(numColumns N == numColumns M - 23)
    assert((matrix M) * (matrix N) == 0)
    
    time N = nullSpace(M, RightSide=>false);
    assert(numRows N == numRows M - 23)
    assert(numColumns N == numRows M)
    assert((matrix N) * (matrix M) == 0)
///

TEST ///
    kk = ZZ/101
    A = random(kk^23, kk^1000);
    B = random(kk^1000, kk^23);
    M = mutableMatrix(B*A);
    N = nullSpace M;
    assert(numRows N == numColumns M)
    assert(numColumns N == numColumns M - 23)
    assert((matrix M) * (matrix N) == 0)
    
    time N = nullSpace(M, RightSide=>false);
    assert(numRows N == numRows M - 23)
    assert(numColumns N == numRows M)
    assert((matrix N) * (matrix M) == 0)
///

TEST ///
kk = ZZ/101
A = mutableMatrix random(kk^4, kk^4)
B = mutableMatrix random(kk^4, kk^1)
X = solveLinear(A,B)
(matrix A) * (matrix X) - matrix B
///

TEST ///
kk = ZZ/101
A = mutableMatrix random(kk^100, kk^100);
B = mutableMatrix random(kk^100, kk^5);
X = solveLinear(A,B) 
((matrix A) * (matrix X)) - matrix B
///

TEST ///
kk = ZZ/101
N = 10
  kk = ZZp 101
time A = mutableMatrix(kk, N, N, Dense=>true);
time fillMatrix A;
time B = invert A;
time C = A*B;
C == mutableIdentity(kk, N)

idN = mutableIdentity(kk, N, Dense=>true);
time X = solveLinear(A, idN);
assert(B == X)
///


TEST ///
kk = ZZ/101
A = mutableMatrix random(kk^3, kk^4);
B = mutableMatrix random(kk^4, kk^7);
A * B
(matrix A) * (matrix B) == matrix(A*B)
///

TEST ///
kk = ZZ/101
C = mutableMatrix random(kk^3, kk^7);
A = mutableMatrix random(kk^3, kk^4);
B = mutableMatrix random(kk^4, kk^7);

C0 = matrix C
addMultipleTo(C,A,B,Alpha=>3_kk, Beta=>-1_kk)
assert(-C0 + 3*(matrix A)*(matrix B) == matrix C)

C = mutableMatrix C0
addMultipleTo(C,mutableMatrix transpose matrix A, B,Alpha=>3_kk, Beta=>-1_kk, TransposeA=>true)
assert(-C0 + 3*(matrix A)*(matrix B) == matrix C)

C = mutableMatrix C0
addMultipleTo(C,A,mutableMatrix transpose matrix B,Alpha=>3_kk, Beta=>-1_kk, TransposeB=>true)
assert(-C0 + 3*(matrix A)*(matrix B) == matrix C)

C = mutableMatrix C0
addMultipleTo(C,mutableMatrix transpose matrix A,
     mutableMatrix transpose matrix B,
     Alpha=>3_kk, 
     Beta=>-1_kk, 
     TransposeB=>true, 
     TransposeA=>true)
assert(-C0 + 3*(matrix A)*(matrix B) == matrix C)

A * B
assert((matrix A) * (matrix B) == matrix(A*B))
///

TEST ///
kk = ZZ/101
A = mutableMatrix random(kk^300, kk^400);
B = mutableMatrix random(kk^400, kk^700);
time C1 = A * B;
time C2 = (matrix A) * (matrix B);
assert(matrix C1 == C2)
///

TEST ///
kk = ZZ/101
A = mutableMatrix random(kk^3, kk^4);
B = mutableMatrix random(kk^4, kk^7);
assert (try (B * A;false) else true)
time C1 = A * B;
time C2 = (matrix A) * (matrix B);
assert(matrix C1 == C2)
///


end

--load "ffpack-test.m2"
restart
loadPackage "FastLinearAlgebra"
kk = ZZ/101
N = 3

N = 4000
N = 100
M = mutableMatrix(kk, N, N, Dense=>true);
time fillMatrix M;
--viewHelp fillMatrix
M
time det M
time det matrix M
rank M
methods rank

A = random(kk^10, kk^31)
B = random(kk^45, kk^10)
C = B*A
D = mutableMatrix C
rank D
rank C

A = random(kk^10, kk^31)
B = random(kk^31, kk^10)
C = B*A
D = mutableMatrix C
det C
det D

restart
loadPackage "FastLinearAlgebra"
kk = ZZp (ideal 19)
M = mutableMatrix(kk, 10, 10)
for i from 0 to 9 do for j from 0 to 9 do M_(i,j) = random kk
M
rank M 

M_(0,3) = 2_kk
M
random(kk^3, kk^4) -- BUG
mutableMatrix oo
3_kk  -- printing is wrong here
matrix{{3_kk}}
matrix {for i from 0 to 30 list i_kk}
1_kk == 1
1_kk
0_kk
oo == ooo

matrix{{1_kk}}
R = kk[a..d]
basis(2,R) 
random(R^1, R^{-2})  
I = ideal random(R^1, R^{-2,-2,-2})  
gens gb I;

kk = ZZ/19
debug Core
rawDiscreteLog(raw 2_kk)
rawDiscreteLog(raw 3_kk)

TEST ///
-- Test that for GF(p,n) (fixed p, various n):
-- that the polynomials for different n are compatible:
-- if (p^m-1) | (p^n-1), then
--   if N = (p^n-1)//(p^m-1),
--   and Fn(x) = min poly of the generator x
--   and Fm(y) = min poly of the generator y
-- then y = x^N
-- 
-- Check this: eliminate(x,ideal(Fn(x), y-x^N)).  If we get Fm(y), great, if not, ERROR>

univariateCoefficients = method()
univariateCoefficients RingElement := (F) -> (
     R := ring F;
     n := first degree F;
     for i from 0 to n list lift(leadCoefficient part(i,F), ZZ)
     )
univariatePoly = method()
univariatePoly(List, RingElement) := (coeffs, x) -> (
     sum apply(#coeffs, i -> coeffs#i * x^i)
     )

checkPair = method()
checkPair(Ring,Ring,Ring) := (R,S,T) -> (
     -- R should be GF(p,m), S should be GF(p,n).
     -- T should be ZZ/p[x1,x2] (with some names of vars)
     -- if N = (p^n-1)/(p^m-1) is not an integer, then just return
     -- otherwise:
     -- 
     Qm := R.order;
     Qn := S.order;
     if (Qn-1) % (Qm-1) == 0
     then (
	  N := (Qn-1) / (Qm-1);
	  << "does divide, N=" << N << endl;
	  Fm := univariateCoefficients (ideal ambient R)_0;
	  Fn := univariateCoefficients (ideal ambient S)_0;
	  -- now check result:
	  ans := univariatePoly(Fm, T_1);
	  I := ideal(univariatePoly(Fn, T_0), T_0^N-T_1);
          GB := flatten entries selectInSubring(1, gens gb I);
     	  if #GB != 1 then << "error: wrong number of elements in GB!!" << endl;
	  if ideal ans != ideal GB then (
	       << "Fm = " << ans << endl;
	       << "Fn = " << univariatePoly(Fn, T_0) << endl;
	       << "minpoly = " << GB_0 << endl;
	       );
	  )
     )

R = ZZ/2[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 23 list GF(2,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = ZZ/3[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 15 list GF(3,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = ZZ/5[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 10 list GF(5,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = ZZ/7[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 8 list GF(7,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = ZZ/11[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 7 list GF(11,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = ZZ/13[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 6 list GF(13,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = ZZ/17[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 6 list GF(17,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = ZZ/19[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 5 list GF(19,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = ZZ/23[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 5 list GF(23,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = ZZ/29[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 5 list GF(29,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = ZZ/31[x,y, MonomialSize=>32, MonomialOrder=>Eliminate 1]
L = for i from 1 to 5 list GF(31,i)
L2 = subsets(L,2);
for p in L2 do checkPair(p_0, p_1, R)

R = GF(5,6)
kk = ZZ/5
phi = map(R,kk)
phi(1_kk)
phi(2_kk)
phi(3_kk)
phi(4_kk)

factor(5^6-1)
factor(5^2-1)
factor(5^3-1)
R6 = GF(5,6)
R3 = GF(5,3)
map(R6,R3)
a = R6_0
a
N = (5^6-1)//(5^3-1)
a^126
///


TEST ///
-- Testing Givaro GF
debug Core
R = rawARingGaloisField(2,3)
rawARingGFPolynomial R
a = R_0
rawARingGFCoefficients (a^3)

 L = lines get "~/src/M2-trunk/M2-jakob-jan2012/Macaulay2/packages/ConwayPolynomials/ConwayPolynomials.txt";
L#2 
L#6
kk = GF(2,7)
R = ambient kk
describe R

debug Core
R = rawARingGaloisField(2,7)
rawARingGFPolynomial R

k2 = rawARingGaloisField(2,2)
rawARingGFPolynomial k2  -- a^2+a+1
rawARingGFCoefficients (k2_0)

k3 = rawARingGaloisField(2,3)
rawARingGFPolynomial k3  -- 1+a^2+a^3
rawARingGFCoefficients (k3_0)

k4 = rawARingGaloisField(2,4)
rawARingGFPolynomial k4  -- a^4+a+1
rawARingGFCoefficients (k4_0)

k5 = rawARingGaloisField(2,5)
rawARingGFPolynomial k5  -- a^5+a^2+1
rawARingGFCoefficients (k5_0)

k6 = rawARingGaloisField(2,6)
rawARingGFPolynomial k6  -- a^6+a^4+a^3+a+1
rawARingGFCoefficients (k6_0)

R = ZZ/2[a,b]
I = ideal"a6+a4+a3+a+1,b-a21"
eliminate(a, I)

gf2 = GF(2,2)
(ideal ambient gf2)_0 -- a2+a+1

gf3 = GF(2,3)
(ideal ambient gf3)_0 -- a3+a+1

gf4 = GF(2,4)
(ideal ambient gf4)_0 -- a4+a+1

(map(gf4,gf2))(gf2_0) -- gf2_0 = a^2+a
R = ZZ/2[a,b]
I = ideal"a4+a+1,b-a5"
eliminate(a, I)

-- is there a way to lift from gf4 to gf2? Doesn't appear so...
-- is there a way to expand a matrix of gf4 elements to a matrix over ZZ/2 or gf2?

///

TEST ///

debug Core
R = rawARingGaloisField(2,7)
rawARingGFPolynomial R
gen= rawARingGFGenerator R
S = ZZ/2[x]/(x^7 + x^3 + 1)
T = rawARingGaloisFieldFromQuotient(raw S_0)
gen= rawARingGFGenerator T

S = GF(3,4)
R = ambient S
T = rawARingGaloisFieldFromQuotient(raw R_0)
gen= rawARingGFGenerator T

///


TEST /// -- Testing M2 ARing GF interface
--restart
--loadPackage "FastLinearAlgebra"
A = GF(2,4)
testGFArithmetic A

B = GF(2,4, Strategy=>"New")
testGFArithmetic B

C = GF(2,4, Strategy=>"Givaro")
testGFArithmetic C

D = GF(2,4, Strategy=>"CompleteGivaro")
testGFArithmetic D

///

TEST /// -- Testing M2 ARing GF interface
--restart
--loadPackage "FastLinearAlgebra"
A = GF(3,3)
testGFArithmetic A

B = GF(3,3, Strategy=>"New")
testGFArithmetic B

C = GF(3,3, Strategy=>"Givaro")
testGFArithmetic C

D = GF(3,3, Strategy=>"CompleteGivaro")
testGFArithmetic D

///

TEST /// -- XXXXXXXXXX -- of findGalois  (which will be placed into the engine?)
restart
loadPackage "FastLinearAlgebra"

R = ZZ/5[b]
G = (b^(5^4)-b) // (b^(5^2)-b)
Gfacs = factor  G  -- 150 factors
apply(Gfacs, fac -> (
	  f := fac#1;
	  S := R/f;
	  S.char = 5;
	  S.degree = 4;
	  findPrimitive S
	  ))

findGalois(5,5)
findGalois(5,5, Strategy=>"Blah")
findGalois(5,5, Strategy=>"Givaro")
findGalois(5,7, Strategy=>"Givaro")
findGalois(5,8, Strategy=>"Givaro")
findGalois(5,8)


findGalois(5,4)
findGalois(5,4)
findGalois(5,3)
findGalois(5,2)
findGalois(5,1)
findGalois(5,5^7, Variable=>getSymbol "c")

debug Core
rawARingGaloisField(5,2)
rawARingGaloisField(5,3)
kk = rawARingGaloisField(5,4)
rawARingGFPolynomial kk
rawARingGaloisField(5,5)
rawARingGaloisField(5,6)
rawARingGaloisField(5,7)

rawARingGaloisField(5,8)


A = GF(2,3)
B = GF(2,3,Strategy=>"Givaro")  -- fails
GF(2,3,Strategy=>"CompleteGivaro")  -- fails


A = GF(2,3,Strategy=>"New")  -- fails, needs 'promote'

debug Core
B = ambient A
B_0
rawARingGaloisFieldFromQuotient(raw B_0)
///

TEST /// 
-- Test of rank
-- JAKOB: can you look at the FFPACK rank call in e/dmat.cpp, and see what is wrong?
restart

mat = {{1, -8, -8, 8, -5, 7, -5, 1}, 
     {9, 3, -7, -3, -5, 2, 6, -7}, 
     {5, -7, -9, -2, 0, 8, -2, 4}, 
     {4, 0, -5, 4, -4, 4, -2, 5}, 
     {3, 0, -1, 5, 8, -6, 4, 4}, 
     {-7, 8, -7, -2, 9, 4, 4, 5}, 
     {0, 9, -6, -3, 7, -3, -4, -5}, 
     {-8, 1, 6, -2, 5, 6, -9, 7}, 
     {-5, -5, 0, -6, -2, -5, -5, -6}}
loadPackage "FastLinearAlgebra"
debug Core
debug FastLinearAlgebra
kk = ZZp (ideal 19)

M = matrix(kk, mat)
mm = mutableMatrix M

rawFFPackRank raw mm
fastRank mm

kk= ZZ/19
M = matrix(kk, mat)
mm = mutableMatrix M
rawFFPackRank raw mm
fastRank mm
assert(5 == rank mm)  -- WRONG!!

R = ZZ/19
MR = matrix(R, mat)
assert(5 == rank MR)
syz MR
///

/// -- rawFFLU, is this active??
kk = QQ
M = random(QQ^5, QQ^3)
debug Core
mm = mutableMatrix M
rawFFLU raw mm  -- doesn't look fraction free!!

kk = ZZ/101
A = mutableMatrix random(kk^10, kk^15)
B = mutableMatrix random(kk^10, kk^1)
x = mutableMatrix(kk, 15, 1)
debug Core
rawSolve(raw A, raw B, raw x)
x1 = matrix x
(matrix A)*x1 == matrix B

time A = random(kk^300, kk^300);
time (A*A);
time (A+A);

restart
-- XXXXXXXXXXXXXX
restart
debug loadPackage "FastLinearAlgebra"
--kk = ZZ/101
kk = ZZp 101
kk = GF(2,4,Strategy=>"Givaro")
random(kk^3, kk^4)
N = 5
m = mutableMatrix(kk, N, N)
m = mutableMatrix random(kk^100, kk^100);
fastRank m
time for i from 0 to N-1 do for j from 0 to N-1 do m_(i,j) = random kk
time M = matrix m;
time(ans1 = m * m);
time(ans2 = M * M);
assert(ans2 == matrix ans1)

kk = GF(2,4,Strategy=>"Givaro")
M = mutableMatrix random(kk^3, kk^4)
debug FastLinearAlgebra
fastRank M

restart
debug Core
debug loadPackage "FastLinearAlgebra"


--kk = ZZp 1049599
kk = ZZp 101
N = 5
M = random(kk^N, kk^N);
time det M
m = mutableMatrix M;
time determinant m
time rank m
(matrix invert m) * M

M1 = M_{0} | M_{1} | M_{0} + 2*M_{1} | M_{2} | M_{2} | M_{3}
m1 = mutableMatrix M1
rowRankProfile m1 -- WRONG
columnRankProfile m1 -- WRONG
m2 = mutableMatrix transpose M1
rowRankProfile m2 -- WRONG
columnRankProfile m2 -- WRONG

M = matrix(kk, {{1,1,1,0}, {0,1,0,0}})
m = mutableMatrix M
nullSpace m
m2 = mutableMatrix transpose M
nullSpace(m2, RightSide=>false)
B = matrix(kk, {{1,0},{0,1}})
b = mutableMatrix B
solveLinear(m, b)

M = matrix(kk, {{1,1,1,0}, {0,1,0,0}})
m = mutableMatrix M
nullSpace m
m2 = mutableMatrix transpose M
nullSpace(m2, RightSide=>false)
B = matrix(kk, {{0},{1} })
b = mutableMatrix B
x = solveLinear(m, b)
(matrix m )*(matrix x)

M = random(kk^10, kk^15)
m = mutableMatrix M
kerm = matrix nullSpace m
assert(M * kerm == 0)
assert(rank kerm == 5)
m2 = mutableMatrix transpose M
kerm2 = matrix nullSpace(m2, RightSide=>false)
assert(kerm2 * (transpose M) == 0)
assert(rank kerm2 == 5)

kk = GF(3,4,Strategy=>"Givaro")
rawARingGFGenerator raw kk
N = 10
N = 4
--M = random(kk^N, kk^N);
--M= matrix {{-a^3+a^2-a-1, -a^3-a^2-a-1, a-1, a-1}, {-a^2-1, a^3-a^2-a, a^3+a^2, -a^3-a^2+a+1}, {a^3-a^2+1, -a^2-a-1, -a^3-a^2+a, a^3-a^2-a+1}, {0, a^2-a-1, -a^2-1, -a^3+a-1}}
M=matrix {{a^3+a^2, -a^3+a^2+a-1, a^3-a^2+a, -a^2-a}, {a^3-a-1, a^3-a^2-a-1, -a^3-a^2-a, a^3-a^2-1}, {a^2+a-1, a^2+1, -a^3-a^2, a^2+a+1}, {a^3+a^2-a-1, a^3+a^2-1, -a^2-a, a^3-a^2+a}}
time det M
m = mutableMatrix M;
time determinant m  -- WRONG
rank m -- WRONG
rank M
time minv = invert m; -- NOT IMPLEMENTED for GF
(matrix minv) * M

M1 = M_{0} | M_{1} | M_{0} + 2*M_{1} | M_{2} | M_{2} | M_{3}
rowRankProfile mutableMatrix M1 -- NOT IMPLEMENTED for GF
columnRankProfile mutableMatrix M1 -- NOT IMPLEMENTED for GF

///
-- TODO:
--   top level M2 package (this file): calls the rawFFPack routines
--   interface.dd:  glue functions to call engine functions
--   engine.h: put in headers for new functions
--   x-mutablemat.cpp: 2 options:  (1) as we do now, just copy to/from ffpack matrices to do computation
--     (2) make a new MutableMatrix type in M2 engine
--   for new MutableMatrix type:
--      (a) subclass MutableMat<coeffRing, ...>
--      (b) include in MutableMatrix: the functions we want.
--      (c) write the matrix class.
-- Also, want GF(q).  Current problem: givaro didn't even compile.

-- invert2 DONE
-- test that with solve DONE
-- multiplication DONE
-- rank profiles (column and row) MOSTLY DONE: need to fix ffpack, test it
-- LU decomposition
-- submatrix + stride
-- row and col echelon forms
-- incorporate givaro types for all of these operations

-- after that:
--  make a new mutable matrix data type
--  

-- HW:
--  Mike:
--    think about the interface for ffpack matrices, so we do not need to copy matrices each operation
--  Jakob:
--    check with ffpack people about: row rank profile.  DONE

-- HW after 24 May 2011:
--   FIXED givaro seems to crash M2 when linked in (Jakob and Mike will both look at this).  Still does! FIXED
--   linbox: try to compile it in (maybe after givaro is working) FIXED: but need to patch M2
--   need: how to use givaro in the same way as for fflas-ffpack. (Where is doc and test code for this?  Jakob will send to Mike?)
--     converting elements?
--     what exactly is the field?  ZZ/p[a]/(g(a)).  What is g?
--   need: new snapshots of givaro and fflas-ffpack.  Dan does this? DONE
--   need: ask them about license?  Check with Dan.  ffpack: LGPL license.  givaro: LGPL? DONE: seems OK
--   ask Dan: look at the givaro license. DONE
--   Mike: in my GF ring code, allow givaro to decide the polynomial, and/or tell givaro what the poly is.
--    learn givaro myself before our next meeting.
--    REALLY start thinking about design for placing these in as M2 types.
--   Jakob: play with givaro
--
-- HW after 15 June 2011:
--   Jakob needs to check valgrind on his givaro-mpir mix
--   Jakob: will check to see if linbox has a "better" solution for small vs large GF fields
--   Mike: will setup GF so that in the presence of givaro, the polynomial used
--     is chosen by givaro.
--     Questions/issues: 1. front end chooses the polynomial currently
--        2. We need to consider two cases: when the poly rep is via exponents vs via polynomials.
--         (see the wiki for how to check this)
--        3. Change the GF class to setup a givaro field once and for all.
--   
-- Plan made 30 June 2011, before Mike goes to Budapest:
--   A. class structure (rings and matrix types)
--   B. make sure top level interface to lin algebra routines is what we want
--   C. Connect ffpack, givaro, linbox to this interface (in B).
--   D. Want possibly other functions from linbox, or elsewhere
--   E. think about connecting Fgb of Faugere (but it is not open source)
--   Milestones:
--    1. Do (B): Top level interface and documentation for these functions.  For fast linear algebra functions
--       what is needed: FastLinearAlgebra package: have to document the functions.
--       thid package should also document: rank, det, ...  but it can't quite do that.  So we will need to change the
--       doc in M2 itself.  Need examples and tests.  Do this in July (in particular: the week of July 18).
--    2. Do (A). The current implementation of rings: each ring type will have a type from (A) as a member field
--      5 days of work?  Jakob: look at e/ring-test.hpp before August. 
--      Do this in August.  Finish this in August.  
--      Also do (or start)  (3), (4) in August.
--    3. Connect linear algebra over ZZ/p  (fflas-ffpack) (adding each ring type: should be easy code, but tests should be written, in FastLinearAlgebra)
--    4. Connect linear algebra over GF(q)  (givaro, or maybe linbox)
--    5. Connect linear algebra over ZZ or QQ (linbox).  This includes LLL, Smith normal form, and perhaps other normal forms.
--      finish (5) in September?
--    6. Want these linear algebra routines (det, solving, LU, ...) for approximate fields: RRR, CCC (i.e. arbitrary precision real and complex).
--       Other possible approx fields: interval, ball arithmetic. (use mathemagix?  Date for this one is unspecified).
--
--   Field  <-- what we have been using
--   FieldEnvelope <-- wrapper for Field for doing virtual function calls, also a template (takes a Field)
--   FieldAbstract <-- base for all of the FieldEnvelope's
--   FieldExample <-- example Field
--   Mike's plan: create a similar class structure
--    also: EngineTests package
--   matrices:
--     we are interested in the following routines:
--       rank
--       det
--       LU decomposition
--       solve AX=B, or XA = B
--       A*B
--         template<typename Ring, template Mat1, template Mat2> multiplyMatrices(const Mat1<Ring> &, const Mat2<Ring> &);
--         (Mat1, Mat2 could be DenseMatrix or SparseMatrix).
--       A += B*C
--       Smith normal form
--       maybe others
--     dmat.hpp: implement dense matrices over coeff rings.  These are templates.
--     matrix.hpp: use mutable matrices as the base. (these are immutable, graded matrices)
--   overall:
--    C++ organization of classes (like above)
--    top level interface to these functions (the user writing m2 code)
--      rank, det, A*B, solving interfaces should not need to change.
--      a few functions need to be added.
--
-- TODO notes made 14 Oct 2011.
--   - Jakob: doxygen: to do tags, make help, place documentation directory "elsewhere", handle options for generating class info DONE
--   - Mike: recompile all stuff, and retest everything -- we recall some test not working. CONTINUAL
--   - goal: have fast linear algebra over finite fields, and possibly other fields too.
--      problem: connect this seamlessly into M2
--   - we will add in calls that operate on mutables matrices.
--   -- design#1: all these interface files will be in Mutable matrices, and in DMat<CR>, SMat<CR>, ...
--   -- ring work:
--      (a) ZZ/p: M2 version, and the ffpack version
--      (b) GF: M2 version, givaro version
--   -- after this: incorporate more rings for these operations: ZZ, QQ, ...
--   -- Mike: add an include file to each cpp file: intent: ASSERT macro. TODO
--   -- SO:
--     (1) x-mutablemat.cpp: need the interface routines to front end (rank,det, ...)
--     (2) also: add in the corresponding interface routines in the d directory
--     (3) MutableMatrix, DMat, SMat: define class methods for these linear algebra routines
--          note: implementation type of DMat, SMat will use ARing data structures, e.g. ARingGF::ElementType
--          then: if entries are required, or other M2 matrices are required, these values will be translated to 
--           "ringelem" types.  (Later: maybe the "ringelem" type will be exactly these ElementType's.)
--          note: this will mean that we do not need to translate matrices before calling the appropriate functions
--            in ffpack/lapack/givaro/etc.
--     (4) write tests for these functions, as well as arithmetic, in the FastLinearAlgebra, EngineTests packages.
--     (5) document exactly what is required for ring types (e.g. what classes/types/methods are required), in order
--         to instantiate them with DMat, SMat.
--
-- TODO notes 19 Jan 2012
--    a. get latest changes of Jakob working (Givaro problem Jakob DONE)
--    b. merge in trunk changes (Mike DONE)
--    c. DONE possibly: merge out changes back to the trunk, delete the branch, and create a new branch. (ask Dan for read/write privs for Jakob on new branch)
--    d. put x-mutablemat routines (fast linear algebra) into dmat.  Organize dispatch in x-mutablemat.
--    e. tests: for matrices/elements for all of our new ring types (ffpack ZZ/p, givaro GF, ...)
--    f. have toplevel GF have an option to call new GF code, same with ZZ/p.
--       issues: at least have a toplevel M2 function which makes new GF code "first class".
--    g. make sure that new GF and ZZ/p code allow to change rings (ring map code, promote, lift)  (e.g. GF(p^2) --> GF(p^4))
--    h. TODO: Mike: what is the global picture in the e directory, especially rings.
--       Mike: document this.  Is it @brief?  Put this into ring.hpp (seems like a good spot).
--         Ring
--         CoefficientRing
--         ARing
--    i. we have ffpack/givaro/linbox field types.
--       it would be nice:  ConcreteFromLinbox<FieldType> this will be usable in M2 as an ARing type.
--                          ConcreteRing<ARingType> This is a "Ring" in M2.
--                          ConcretePolynomialRing<APolyRingType>
--                          ConcreteField<AFieldType>
--
-- TODO generated 26 Jan 2012
--  Getting ZZ/p and GF ffpack and givaro and M2 routines all working at top level, and with dense/sparse matrices
--    a. bugs in ffpack ZZ/p: basis(2,R) fails (R = polyring over ZZ/p).  Fix this?  MIKE
--    b. DONE GF givaro: routine to get the defining polynomial coeffs (see M2_makearrayint in monordering.c). JAKOB
---   b1. DONE M2 GF in the same framework (as ConcreteRing). MIKE
--    c. promote/lift/eval beween these rings MIKE (or both of us, after a,b,d,e are done).
--    d. DONE top level M2 function to create GF from Givaro. (needs (b).  MIKE).
--       this needs: the polynomial
--       DONE another routine: create givaro GF ring using a specific poly (M2_arrayint as input) (JAKOB)
--    e. make a function called primitiveGenerator(ZZ/p), or primitiveGenerator(GF). (BOTH: Jakob for givaro and ffpack, Mike for M2)
--       top level: each finite field should have routines: char, order, vdim??, generator. (MIKE)
--       char kk, kk.order (order), kk.degree (dimension over ZZ/p), kk_0 is the generator
--    f. more testing! (JAKOB, mike too)
--
--
-- TODO generated 9 Feb 2012  (for next meeting, 20 Feb 2012)  ALSO for meeting on 1 Mar 2012.
--    a. Jakob:
--        finish GF and ffpack ZZ/p code, and test it.
--        make sure that primitive element in GF code is either the variable, or we can obtain it. ALMOST DONE
--        test these!
--    b. Mike:
--        get mutable matrices so they work with these new ZZ/p and GF rings.
--        test these!
--        talk to Dan: make sure that creation of rings at top level is the way it should be done.
--    this will put us into place to write the linear algebra routines for these fields.
--
-- TODO generated 20. Feb 2012
--      Jakob: ask givaro authors if there is a reason for their choice of the generator
--      can we set the primitive element ourself ?
--      inside of aring-gf code: check that promote, lift, and eval are "correct".
--      Mike:
--        Problem: suppose phi : K = GF(p^n) --> R, for some ring R.
--                 phi.map(0) is the image of either (1) x = K_0, or (2) the generator of the multiplicative group.  Which is it?
--                 in m2/ringmap.m2 it seems that the value we are given in phi.map(0) is the image of the 
--                   primitive element, so the current code is correct, IF IF IF the primitive element that M2 has matches the
--                   implementation primitive element.
--                 SEEMS OK in ARingGFM2 code, but we need to make sure it is OK in ARingGF code.  Then: fix logic in m2/galois.m2.
-- TODO made 1 Mar 2012
--      Jakob: ask givaro authors if there is a reason for their choice of the generator
--        can we set the primitive element ourself ? DONE, need to Check 
--        (2)Does givaro, ffpack, linbox have initialized variables which occur before main()?
--        (3) make sure all givaro, ffpack uses are enclosed by #ifdef... DONE (Jakob)
--      Mike:
--        get M2 on our branch compiled: gcc 4.2.1: with givaro,  and also without givaro.  Merge in the changes from the trunk
--          same with gcc 4.6.2.
--      Jakob:
--        finish GF and ffpack ZZ/p code, and test it.
--        make sure that primitive element in GF code is either the variable, or we can obtain it. ALMOST DONE
--        test these!
--      Mike:
--        get mutable matrices so they work with these new ZZ/p and GF rings.
--        test these!
--         a. possibly make zero_matrix a member function for Ring.
--         b. MutableMat functions do not create new matrices (except for zero_matrix), so we have
--            functions like:  X += Y, X += Y*Z, submatrix of Y into X.
--         c. MutableMat matrices have two ring pointers: one is a generic "const Ring*", the other depends on the template.
--            I don't really like having two ring pointers in our objects, but it makes creation of new rings simpler.
--            But maybe this is not necessary.  Need to think about it.
--         d. (Jakob and Mike): finally get to write the linear algebra routines.
--
-- TODO 22 Mar 2012
--     Jakob: why is the bound for modulus in ffpack about 67 million?  is it really? - yes. (<2^26)
--            compute the generator in a faster manner, perhaps change interface to rawARing... to take a generator.
--            debug GF determinant.  Seems not to be working.
--            benchmarks should also appear as test and fail on specified hardware if the runtime increased significantly #
--            random generator from givaro seems to be not that good - replace with a better one?
--     Mike: write powerMod
--     in FastLinearAlgebra.m2: add in benchmarks for rank, determinant, ...
--     in EngineTests.m2: test rank, determinant, ...
--     Mike: get rank, determinant working with other ring types
--
-- 29 Mar 2012
--     interface.dd: rank needs to give an error if -1 is returned.
--     solve: throw an error if error occurs.
--     x-mutablemat: catch errors, somehow alert front end.
--     invert: maybe have the DMat and SMat routines throw an error.
--         possible errors: not invertible.
--                          not implemented for this ring/matrix type
--                          not a square matrix
----------------------------------------------
-- not discussed yet:
--    a. bugs in ffpack ZZ/p: basis(2,R) fails (R = polyring over ZZ/p).  Fix this?  MIKE 
--        => i suggest we wil use M4RI for basis(2,R) op: we (mostly Mike) did setup this itsy bitsy template interface to incorporate other field implementations
--           and therefore we should also use it extensively ;-)

R = GF(2,7)
ambient R
S = ZZ/2[x]/(x^7+x+1)
A = GF(S, PrimitiveElement => x^2)
B = ZZ/2[y]
phi = map(B,A,{y})
phi(x) -- y^128 !! Oh no!!  But mathematically this is OK.
phi(x^2) 
phi(x^7)
phi = map(S,A,{S_0})

steps:
1. find f (but with Givaro: this also produces a raw ring
2. find primitive element
3. construct GF

findGaloisField(p,n)
  find quotient poly f (using Conway Polynomials)
  form R = kk[a]/f
  find primitive element x
  return x

findGaloisField(p,n,Givaro=>true)
findGaloisField(kka,n,Givaro=>true)
  construct R0 = rawARingGF
  get quotient poly as array
  make a poly in one variable out of it (either in kka = kk[a], or in ZZ/p[a])
  form R = kk[a]/f
  find primitive element x
  in R, set R#"RawGivaroRing" = R0
  return x

findGaloisField(kk[a], n, Givaro=>true/false)


GF(p,n,Strategy=>"Givaro")

-- checks to make sure size is OK, if not: error.
-- first construct a raw Givaro field.
-- then create a poly ring S in one var matching this.
-- then call GF(S, rawGFRing)

GF(R, Strategy=>"Givaro")
-- calls Givaro with a given poly already
GF(R, Strategy=>"New")


Overall logic:
Strategy=>null:  current method

Strategy=>"New": 
  case 1: GF(p,n):  (small rep) as currently done, EXCEPT: call different rawGaloisField routine.
  case 2: GF a_R: (small rep) as currently done, EXCEPT: call different rawGaloisField routine.

Strategy=>"Givaro":
  case 1: GF a_R:
    if size is too big, error
    do current GF code, except: use different raw routine.

  case 2: (p,n) or p^n:
    if size is too big, error
    make rawARingGF?  (Alternate: ?)
    find f that they use
    make ZZ/p[a]/(f) ring
    have a new version of GF which takes an already constructed raw ring
     but otherwise does exactly what is done now.


possible structure for mutable matrices:
class DMat<RingType>  -- just like now
class SMat<RingType>  -- just like now
class MutableMatrix  (virtual calls to everything) -- just like now
  class ConcreteMutableMatrix<MatType>

question: how to create a new matrix, given a ring?
answer#1: have, inside the Ring hierarchy.  This seems to be the cleanest solution
  virtual MutableMatrix* mutableZeroMatrix(size_t nrows, size_t ncols, bool dense)
answer#2:
  MutableMatrix has a virtual call:
  virtual MutableMatrix * mutableZeroMatrix(size_t nrows, size_t ncols)
  this could be called via... THIS DOESN'T WORK SO WELL!!
  MatrixFactory: abstract class
    ConcreteMatrixFactory<RingType>::mutableZeroMatrix


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=FastLinearAlgebra all check-FastLinearAlgebra RemakeAllDocumentation=true RerunExamples=true RemakePackages=true"
-- End:
