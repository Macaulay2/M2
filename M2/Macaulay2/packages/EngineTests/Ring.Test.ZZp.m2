export {
    "testGFpromote",
    "testGFGenerator"
    }

debug Core
testGFGenerator = (R) -> (
    -- R should be ZZ/p or a GF.
    gen := try(
            new R from rawMultiplicativeGenerator raw R
            ) else null;
    if gen =!= null then (
        nelems := R.order;
        facs := (nelems-1)//factor//toList/toList/first;
        for s in facs do (assert(gen^((nelems-1)//s) != 1));
        assert(gen^(nelems-1) == 1);
        )
    else
        << "no generator for " << describe R << endl;
    )

minpoly = (f) -> (
    -- under construction
    R := ring f;
    A := ambient R;
    d := R.degree;
    L := matrix{for i from 0 to d list lift(f^i, A)};
    (monoms, cfs) := coefficients L;
    (monoms, cfs)
    )

isGen = (f) -> (
    R := ring f;
    nelems := R.order;
    facs := (nelems-1)//factor//toList/toList/first;
    for s in facs do if f^((nelems-1)//s) == 1 then return false;
    true
    )

debug Core
testGeneratorNotPrimitive = () -> (
    -- let's make a GF whose primitive element is not the generator
    -- under construction...
    )
TEST ///
    debug needsPackage "EngineTests"
    debug Core
    R = ZZ/5[a];
    rawConwayPolynomial(5,4,false)
    kk = GF(5,4);
    isGen (a^2);
    minpoly(a^2)
    for i from 1 to 5^4-1 list if not isGen(a^i) then i else continue
///

fieldsGFFlintBig1 = {
    ///GF(2,2, Strategy=>"FlintBig")///,
    ///GF(2,3, Strategy=>"FlintBig")///,
    ///GF(2,11, Strategy=>"FlintBig")///,
    ///GF(2,12, Strategy=>"FlintBig")///,
    ///GF(2,13, Strategy=>"FlintBig")///,
    ///GF(2,20, Strategy=>"FlintBig")///,
    ///GF(2,30, Strategy=>"FlintBig")///,
    ///GF(3,2, Strategy=>"FlintBig")///,
    ///GF(3,3, Strategy=>"FlintBig")///,
    ///GF(3,4, Strategy=>"FlintBig")///,
    ///GF(3,5, Strategy=>"FlintBig")///,
    ///GF(3,6, Strategy=>"FlintBig")///,
    ///GF(3,7, Strategy=>"FlintBig")///,
    ///GF(3,8, Strategy=>"FlintBig")///,
    ///GF(3,13, Strategy=>"FlintBig")///,
    ///GF(3,20, Strategy=>"FlintBig")///,
    ///GF(23,7, Strategy=>"FlintBig")///
    }

TEST ///
    debug EngineTests
    debug Core
-- MES: fix this test: it crashes on ubuntu (I think).    
--    runTests(finitefields, {"testGFGenerator"}, set{})
--    runTests(fieldsGFFlint, {"testGFGenerator"}, set{})
--    runTests(fieldsGFFlintBig1, {"testGFGenerator"}, set{})

///

testFiniteField = (R, charac) -> (
    assert(rawCharacteristic R === charac)
    )

allElements = (p,d,A) -> (
    if d == 0 then for i from 0 to p-1 list i_A
    else (
        elems := allElements(p,d-1,A);
        flatten for f in elems list for i from 0 to p-1 list (f + i*A_0^d)
        )
    )

testGF1 = (p,d,kk) -> (
   A := ambient kk;
   gen := rawMultiplicativeGenerator raw kk;
   facs := (p^d-1)//factor//toList/toList/first;
   for a in facs do assert(gen^((p^d-1)//a) != 1);
   --rawARingGFPolynomial raw kk;
   --rawARingGFCoefficients raw (kk_0^5);
   time elems := allElements(p,d-1,A); -- creating them over the finite field would be faster...
   << "fraction of unique hash vals: " << ((elems/(f -> hash raw f)//unique//length) / (#elems * 1.0)) << endl;
   time elems1 := elems/(f -> promote(f,kk));
   time elems2 := elems1/(f -> lift(f,A)); -- this one is slow for 2^13
   time elems3 := elems2/(f -> promote(f,kk));
   time assert(elems3 == elems1);
   time assert(elems2 == elems);
   time assert(# unique elems == p^d); -- this one is very slow for 2^13
   time assert(# unique elems1 == p^d);
   time m1 := promote(matrix{elems}, kk);
   time m2 := lift(m1, A);
   m1
   )

testGFpromote = (p,d,strategy) -> (
   kk := GF(p^d, Strategy=>strategy);
   testGF1(p,d,kk)
   )
TEST ///
  -- the following should all give errors.
  assert try (ZZpFlint 1; false) else true
  assert try (ZZpFFPACK 1; false) else true
  assert try (ZZp(2, Strategy=>null); false) else true
  assert try (rawARingZZp 1; false) else true -- Strategy => "ARing"
  assert try (rawZZp      1; false) else true -- Strategy => "Old"
///


TEST ///
  -- Factorization over these finite fields
  debug Core
  R = ZZp(101, Strategy=>"Aring")
  S = R[x]
  F = (x-3)^3*(x^2+x+1)
  factor F 

  R = ZZp(101)  
  S = R[x]  -- display here is messed up
  F = (x-3)^3*(x^2+x+1)
  factor F

  R = ZZ/101
  S = R[x]
  F = (x-3)^3*(x^2+x+1)
  factor F

  R = ZZp(101, Strategy=>"Flint")
  S = R[x]
  F = (x-3)^3*(x^2+x+1)
  factor F  

  if hasFFPACK then (
      R = ZZp(101, Strategy=>"Ffpack");
      S = R[x];
      F = (x-3)^3*(x^2+x+1);
      factor F
      );

  R = ZZp(65537, Strategy=>"Flint")
  S = R[x]
  F = (x-3)^3*(x^2+x+1)
  factor F  

  R = ZZp(536870909, Strategy=>"Flint")  -- max prime that factory can handle is 2^29-3.
  S = R[x]
  F = (x-3)^3*(x^2+x+1)
  factor F  

  if hasFFPACK then (
      R = ZZp(33554393, Strategy=>"Ffpack"); -- max prime that ffpack can handle is 2^25 - 39
      S = R[x];
      F = (x-3)^3*(x^2+x+1);
      factor F  
      )
///

TEST ///
  -- Switch between these rings.
  debug Core -- for ZZp
  R1 = ZZ/101
  M1 = matrix(R1, {{0..103}})
  R2 = ZZp(101, Strategy=>"Aring")
  R3 = ZZp(101)  
  R4 = ZZp(101, Strategy=>"Flint")
  Rs = {R1,R2,R3,R4}
  if hasFFPACK then (
      R5 = ZZp(101, Strategy=>"Ffpack");
      Rs = append(Rs, R5);
      );
      
  fs = apply(subsets(Rs,2), rs -> {map(rs#0, rs#1), map(rs#1, rs#0)})
  for rs in fs do (
      f := rs#0;
      g := rs#1;
      M1 = matrix(source f, {{0..103}});
      M2 = matrix(target f, {{0..103}});
      N1 = mutableMatrix M1;
      N2 = mutableMatrix M2;
      assert(f M1 == M2);
      assert(g M2 == M1);
      --assert(f N1 == N2); -- these are not defined yet!
      --assert(g N2 == N1); -- these are not defined yet!
      )
  -- now try it for some larger sizes
  P = 2^25-39 -- largest ffpack prime
  if hasFFPACK then (
      S1 = ZZp(P, Strategy=>"Flint");
      S2 = ZZp(P, Strategy=>"Ffpack");
      (f,g) = (map(S1,S2), map(S2,S1));
      M1 = matrix(S1, {{-100..100, 2^25..2^25 + 10000}});
      M2 = matrix(S2, {{-100..100, 2^25..2^25 + 10000}});
      assert(f M2 == M1);
      assert(f M2 == M1);
      );
///

TEST ///
  debug Core
  rawConwayPolynomial(5,2,false) == {2,4,1}
  rawConwayPolynomial(5,7,false) == {3, 3, 0, 0, 0, 0, 0, 1}
  rawConwayPolynomial(5,11,false) == {3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1}
  rawConwayPolynomial(5,34,false) == {}

  char5s = {{2, {2, 4, 1}}, 
      {3, {3, 3, 0, 1}}, 
      {4, {2, 4, 4, 0, 1}}, 
      {5, {3, 4, 0, 0, 0, 1}}, 
      {6, {2, 0, 1, 4, 1, 0, 1}}, 
      {7, {3, 3, 0, 0, 0, 0, 0, 1}}, 
      {8, {2, 4, 3, 0, 1, 0, 0, 0, 1}}, 
      {9, {3, 1, 0, 2, 0, 0, 0, 0, 0, 1}}, 
      {10, {2, 1, 4, 2, 3, 3, 0, 0, 0, 0, 1}}}  
  ans := for i from 2 to 10 list {i, rawConwayPolynomial(5,i,false)};
  assert(ans == char5s);
///

///
  -- This is more of a benchmark code, than a test...
  debug Core
  R1 = ZZ/101
  R2 = ZZp(101, Strategy=>"Aring")
  R3 = ZZp(101)  
  R4 = ZZp(101, Strategy=>"Flint")
  R5 = ZZp(101, Strategy=>"Ffpack")

  S1 = R1[vars(0..4)]  
  S2 = R2 (monoid S1)
  S3 = R3 (monoid S1)
  S4 = R4 (monoid S1)
  S5 = R5 (monoid S1)
  
  I = ideal random(S1^1, S1^{-5,-5,-5,-7});
  I = ideal I_*;
  time gens gb I;
  
  I2 = (map(S2,S1)) I;
  time gens gb I2;

  I3 = (map(S3,S1)) I;
  time gens gb I3;

  I4 = (map(S4,S1)) I;
  time gens gb I4;

  I5 = (map(S5,S1)) I;
  time gens gb I5;

  -- how about mutable matrix multiplication?
  Ma = mutableMatrix(R1, 1000, 1000); fillMatrix Ma;
  Mb = mutableMatrix(R1, 1000, 1000); fillMatrix Mb;
  
  time Mc = Ma*Mb;

  Ma2 = mutableMatrix sub(matrix Ma, R2);
  Mb2 = mutableMatrix sub(matrix Mb, R2);
  time Mc2 = Ma2*Mb2;  

  assert(matrix Mc == sub(matrix Mc2, R1))  
  
  Ma4 = mutableMatrix sub(matrix Ma, R4);
  Mb4 = mutableMatrix sub(matrix Mb, R4);
  time Mc4 = Ma4*Mb4;   -- MUCH faster
  assert(matrix Mc == sub(matrix Mc4, R1))  

  Ma5 = mutableMatrix sub(matrix Ma, R5);
  Mb5 = mutableMatrix sub(matrix Mb, R5);
  time Mc5 = Ma5*Mb5;   -- MUCH faster and it is faster than flint
  assert(matrix Mc == sub(matrix Mc5, R1))  

  time det Ma;
  time det Ma4;
  time det Ma5;
  
  time LUdecomposition Ma;
  time LUdecomposition Ma2;
  time LUdecomposition Ma4;
  time LUdecomposition Ma5;
///


TEST ///
  kk = GF 9
  A = kk[x]
  factor(x^9-x)
  factor(x^2-a)
  factor(x^3-x)
  factor(x^4-a)
  F = x^81-x
  facs = factor F
  facs = facs//toList/toList
  assert(#facs == 45)
  assert(all(facs, f -> last f == 1))
  assert(product(facs/first) == F)
  F1 = x^2 - a*x + 1
  F2 = x^2+x-a-1
  G = F1*F2
  facs = factor G
  assert(facs//toList/toList/first//set  === set{F1,F2})
  
  -- test gcd over GF
  F1 = (x-a)^3*(x-a^2)
  F2 = (x-a)*(x-a^3)
  assert(gcd(F1,F2) == x-a)
///  

TEST ///
  debug Core
  kk = GF(9, Strategy=>"New")
  A = kk[x]
  factor(x^9-x)
  factor(x^2-a)
  factor(x^3-x)
  factor(x^4-a)
  F = x^81-x
  facs = factor F
  facs = facs//toList/toList
  assert(#facs == 45)
  assert(all(facs, f -> last f == 1))
  assert(product(facs/first) == F)
  F1 = x^2 - a*x + 1
  F2 = x^2+x-a-1
  G = F1*F2
  facs = factor G
  assert(facs//toList/toList/first//set  === set{F1,F2})
  
  -- test gcd over GF
  F1 = (x-a)^3*(x-a^2)
  F2 = (x-a)*(x-a^3)
  assert(gcd(F1,F2) == x-a)
///  

TEST ///
  -- test of lift/promote for all galois fields which use tables
  -- testGFpromote enumerates all elements, so only use on smaller size...
  topval = 10;  -- 13, although better, makes this too long
  for i from 1 to topval do (<< i << " "; testGFpromote(2,i,"New"))
  for i from 1 to topval do (<< i << " "; testGFpromote(2,i,null))
  for i from 2 to topval do (<< i << " "; testGFpromote(2,i,"FlintBig"))  
  for i from 1 to topval do (<< i << " "; testGFpromote(2,i,"Flint"))  

  topval = 6 -- 8 would be better
  time for i from 1 to topval do (<< i << " "; testGFpromote(3,i,"New"))
  time for i from 1 to topval do (<< i << " " << flush; testGFpromote(3,i,null))  
  time for i from 2 to topval do (<< i << " " << flush; testGFpromote(3,i,"FlintBig"))  
  time for i from 1 to topval do (<< i << " " << flush; testGFpromote(3,i,"Flint"))  
  
  time for i from 1 to 5 do (<< i << " " << flush; testGFpromote(5,i,"New"))
  time for i from 1 to 5 do (<< i << " " << flush; testGFpromote(5,i,null))  
  time for i from 2 to 5 do (<< i << " " << flush; testGFpromote(5,i,"FlintBig"))  
  
  --time for i from 1 to 4 do (<< i << " " << flush; testGFpromote(7,i,"New"))
  --time for i from 1 to 4 do (<< i << " " << flush; testGFpromote(7,i,null))  
  time for i from 2 to 4 do (<< i << " " << flush; testGFpromote(7,i,"FlintBig"))  
  
  time for i from 1 to 3 do (<< i << " " << flush; testGFpromote(11,i,"New"))
  time for i from 1 to 3 do (<< i << " " << flush; testGFpromote(11,i,null))  
  time for i from 2 to 3 do (<< i << " " << flush; testGFpromote(11,i,"FlintBig"))  
  
  --time for i from 1 to 3 do (<< i << " " << flush; testGFpromote(13,i,"New"))
  --time for i from 1 to 3 do (<< i << " " << flush; testGFpromote(13,i,null))  
  time for i from 2 to 3 do (<< i << " " << flush; testGFpromote(13,i,"FlintBig"))  

  --time for i from 1 to 3 do (<< i << " " << flush; testGFpromote(17,i,"New"))
  --time for i from 1 to 3 do (<< i << " " << flush; testGFpromote(17,i,null))  
  time for i from 2 to 3 do (<< i << " " << flush; testGFpromote(17,i,"FlintBig"))  
  
  --time for i from 1 to 3 do (<< i << " " << flush; testGFpromote(19,i,"New"))
  --time for i from 1 to 3 do (<< i << " " << flush; testGFpromote(19,i,null))  
  time for i from 2 to 3 do (<< i << " " << flush; testGFpromote(19,i,"FlintBig"))  
  
  for p from 80 to 100 do if isPrime p then (
      << "doing " << p << endl;
      time for i from 1 to 2 do (<< i << " " << flush; testGFpromote(p,i,"New"));
      time for i from 1 to 2 do (<< i << " " << flush; testGFpromote(p,i,null))  ;
      time for i from 2 to 2 do (<< i << " " << flush; testGFpromote(p,i,"FlintBig"));
      )
///

TEST ///
 -- maps GF(p^2)[x,y] --> GF(p^4)[x,y]
 restart
 R4 = GF(2,4, Strategy=>"Flint")
 R2 = GF(2,2, Strategy=>"Flint")
 A4 = ambient R4
 A2 = ambient R2
 ideal A4
 ideal A2
 B = ZZ/2[x]
 f = x^2+x+1
 sub(f, x=>x^2)
 C4 = R4[x,y]
 C2 = R2[x,y]
 use C4
 phi = map(C4,C2,{x,y,R4_0^2})
 use C2
 G = a*x + (a-1)*y^3 + x^2 + a^3
 use C4
 use coefficientRing C4
 assert(phi G == (a+1)*y^3 + x^2 + a^2*x+1)
///

TEST ///
 -- maps GF(p^2)[x,y] --> GF(p^4)[x,y]
 restart
 R4 = GF(2,4, Strategy=>"FlintBig", Variable=>b)
 R2 = GF(2,2, Strategy=>"FlintBig", Variable=>a)
 A4 = ambient R4
 A2 = ambient R2
 ideal A4
 ideal A2
 B = ZZ/2[x]
 f = x^2+x+1
 sub(f, x=>x^5)
 C4 = R4[x,y]
 C2 = R2[x,y]
 use C4
 phi = map(C4,C2,{x,y,R4_0^5})
 phi2 = map(C4,C2,{x,y,R4_0^2 + R4_0})
 use C2
 G = a*x + (a-1)*y^3 + x^2 + a^3
 use C4
 use coefficientRing C4
 assert(phi G == b^5*x + (b^5-1)*y^3 + x^2 + b^15)
 assert(phi2 G == b^5*x + (b^5-1)*y^3 + x^2 + b^15)
 mutableMatrix{{G}}
 -- phi oo -- not defined yet!!
///

TEST ///
  kk = GF 9
  A = kk[x]
  factor(x^9-x)
  factor(x^2-a)
  factor(x^3-x)
  factor(x^4-a)
  F = x^81-x
  facs = factor F
  facs = facs//toList/toList
  assert(#facs == 45)
  assert(all(facs, f -> last f == 1))
  assert(product(facs/first) == F)
  F1 = x^2 - a*x + 1
  F2 = x^2+x-a-1
  G = F1*F2
  facs = factor G
  assert(facs//toList/toList/first//set  === set{F1,F2})
  
  -- test gcd over GF
  F1 = (x-a)^3*(x-a^2)
  F2 = (x-a)*(x-a^3)
  assert(gcd(F1,F2) == x-a)
///  
  
///
  B = ZZ/5[x,y]
  I = ideal(x^3-x-1, y^2-y-1)
  H = resultant(I_1, sub(I_0, {x => x+y}), y)
  factor H

  debug Core  
  kk = ZZ/3
  R = kk[x,y,a]
  rawFactor(raw(x^80-y^80), raw(a^2-a-1))
  rawFactor(raw(x^80-1), raw(a^2-a-1))
  rawFactor(raw(x^9-x), raw(a^2-a-1))

  debug Core  
  kk = QQ
  R = kk[x,y,a]
  rawFactor(raw(x^2-3*y^2), raw(a^2-3))
  rawFactor(raw(x^80-1), raw(a^2-a-1))
  rawFactor(raw(x^9-x), raw(a^2-a-1))

  debug Core
  kk = ZZ/3
  R = kk[x,y,a]
  F = a^2-a-1
  F1 = (x-a)^3*(x-a^2) % F
  F2 = (x-a)*(x-a^3) % F
  new R from rawGCD(raw F1,raw F2,raw F)
///

if hasFFPACK then
///
  restart
  kk = GF(101,5)
  kk = GF(101,5,Strategy=>"FFPACK")
  R = kk[x]
  R1 = ZZ/101[x,a]
  facs = first rawFactor(raw(x^5-a^5), raw(a^5+2*a-2))
  assert(#facs == 5)
  x^5-a^5 == new R1 from times facs 
  factor(x-a)
  kk
  debug Core
  raw kk
///

///
  -- test of primitive elements vs generators of the poly ring
  kk = GF(2,4)
  ambient kk
  elems = for i from 0 to 15 list kk_0^i
  assert(# unique elems == 15)
  -- what is the order of each of these elements?
  ord = (a) -> (if a == 1 then 0 else for i from 1 do if a^i == 1 then return i)
  elems/ord
  select(elems, f -> f != 1 and ord f != 15)
  f = a^3  
  f^2
  f^3
  f^4 + f^3 + f^2 + f + 1
  ZZ/2[x]
  F = x^4 + x^3 + x^2 + x + 1
  A = (ring F)/F
  kk = GF(A, Strategy=>"New")
  kk_0
  kk_0^5 -- order 5
  testGF1(2,4,kk)
  kk = GF A
///

if hasFFPACK then
///
  kk = GF(9, Strategy=>"New")
  testGF1(3,2,kk)
///

///  
  restart
  debug loadPackage "EngineTests"
  kk = GF(9, Strategy=>"FlintBig")
  testGF1(3,2,kk)
///

///
  -- how good are the random numbers:
  debug loadPackage "EngineTests"
  kk = GF(9, Strategy=>"FlintBig")
  m = mutableMatrix(kk, 100, 100);
  fillMatrix m;
  L = flatten entries m;
  tally L
  tally for i from 1 to 10000 list random kk -- much better random behavior...
///

///
  -- how good are the random numbers.  Let's try it for 2^16
  debug needsPackage "EngineTests"
  kk = GF(2, 16, Strategy=>"FlintBig")
  m = mutableMatrix(kk, 100, 100);
  fillMatrix m;
  L = flatten entries m;
  tally L;
  tally values tally for i from 1 to 10000 list random kk -- much better random behavior...
///

///
  R = ZZ/101[a..d]
  time L = for i from 1 to 100000 list random(2,R);  
  time L = for i from 1 to  50000 list random(2,R);  
  debug Core
  L/(x -> hash raw x)//unique//length
  length unique L -- *really* slow on 1.6.  Instantaneous (.05 sec) on linalg branch, 23 May 2014.
  incr1 = 463633
  incr2 = 7858565
  seed1 = 103
  seed2 = 347654
  hash raw a
  hash raw (a^2) - hash raw a
  
  restart
  R = ZZ/101[a..d]
  a
  -a
  -o2
  f = a+b
  f-b
  -f   -- 3 (double) calls to hash
  new HashTable from {a=>b}
  hash oo
  {a,b}
  {a+b}
  hash raw first oo

  L = flatten entries basis(0,7,R)    
  L/(f -> hash raw f)
  oo//unique//length
  #L
  partition(f -> hash raw f, L)
///

