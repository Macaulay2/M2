-- -*- coding: utf-8 -*-

export  { 
   testPromote,
   testPromoteLift,
   testRingMapEval
}

testPromote = () -> (
    -- test basic promote routines
    -- Part1: source: RR_53
    -- Part2: source: RR_100
    -- Part3: source: CC_53
    -- Part4: source: CC_100
    R1 := RR_53;
    C1 := CC_53;
    R := RR_100;
    S := RR_200;
    T := CC_100;
    U := CC_200;
    ---- Part1: promote from R1 ----
      m := matrix{{1.234 _ R1}};
      m1 := promote(m,C1);
      assert(lift(m1,R1) == m); -- NOT YET
      m1 = promote(m,R);
      assert(promote(m1,R1) == m);
      m1 = promote(m,S);
      assert(promote(m1,R1) == m);
      m1 = promote(m,T);
      assert(lift(m1,R1) == m); -- NOT YET
      m1 = promote(m,R1);
      assert(m1 == m);
    ---- Part 2: promote from R ----
      m = matrix{{1.234 _ R}};
      m1 = promote(m,C1);
      assert(lift(m1,R) == m); -- NOT YET
      m1 = promote(m,R);
      assert(promote(m1,R) == m);
      m1 = promote(m,S);
      assert(promote(m1,R) == m);
      m1 = promote(m,T);
      assert(lift(m1,R) == m); -- NOT YET
      m1 = promote(m,R1);
      assert(promote(m1,R) == m);
    ---- Part 3: promote from T ----
      m = matrix{{1.234 _ T}};
      m1 = promote(m,C1);
      assert(promote(m1,T) == m);
      m1 = promote(m,T);
      assert(m1 == m);
      m1 = promote(m,U);
      assert(promote(m1,T) == m);
      assert(try (promote(m,R); false) else true);
      assert(try (promote(m,S); false) else true);
      assert(try (promote(m,R1); false) else true);
    ---- Part 4: promote from C1 ----
      m = matrix{{1.234 _ C1}};
      m1 = promote(m,C1);
      assert(promote(m1,C1) == m);
      m1 = promote(m,T);
      assert(promote(m1,C1) == m);
      m1 = promote(m,U);
      assert(promote(m1,C1) == m);
      assert(try (promote(m,R); false) else true);
      assert(try (promote(m,S); false) else true);
      assert(try (promote(m,R1); false) else true);
    )

TEST ///
    testPromote()
///

testPromoteLift = () -> (
    R53 := RR_53;
    C53 := CC_53;
    R := RR_100;
    S := RR_200;
    T := CC_100;
    U := CC_200;
    m := matrix{{1.234 _ R}};
    mS := promote(m, S);
    mT := promote(m, T);
    mU := promote(m,U);
    mR53 := promote(m,R53);
    mC53 := promote(m,C53);
    n := mS;
    assert(m == promote(n, R));
    assert(mS == promote(n, S));
    assert(mT == promote(n, T));
    assert(mU == promote(n,U));
    assert(mR53 == promote(n,R53));
    assert(mC53 == promote(n,C53));
    assert(m == lift(mS, R)); -- fails in 1.6
    assert(m == lift(mT,R));
    m1 := matrix{{1+ii}};
    assert(try (lift(m1,RR_53); false) else true); -- should fail (as it does).
    x := (5.2)_R53;
    sub(x, R53); -- CRASH
    phi := map(R53, R53, {});
    phi (4.2); -- CRASH
    -- these work over infinite precision
    x = (5.2)_R;
    sub(x, R); -- ok
    phi = map(RR_300, RR_200, {});
    phi (4.2p200); -- ok
    -- try polynomial rings over these
    A := RR_100[getSymbol"x", getSymbol"y"];
    f := map(A, RR_100, {});
    f (1.2p100);
    sub(ideal x_A, A);
    -- try polynomial rings over  RR_53
    A = RR_53[getSymbol"x", getSymbol"y"];
    f = map(A, RR_53, {});
    f (1.2);
    sub(ideal x_A, A);
    )

TEST ///
 debug EngineTests
 testPromoteLift()
///

testRingMapEval = (R,C) -> (
    x := getSymbol "x";
    PC := C[x];
    x = PC_0;
    F := matrix{{x^3}};
    P := matrix{{0.1_R}};
    (map(C,PC,P)) F;
    map(R,PC); -- works, but should not
    (map(R,PC,P)) F -- crashes
    )

--TEST 
///
  debug EngineTests
  R = RR; C = CC;
  testRingMapEval(RR,CC)
  testRingMapEval(RR_100,CC_100)
///

