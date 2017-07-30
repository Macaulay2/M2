-- -*- coding: utf-8 -*-

export  { 
   "testPromote",
   "testPromoteLift",
   "testRingMapEval"
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
    sub(x, R53); 
    phi := map(R53, R53, {});
    phi (4.2); 
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

testPromoteLiftQQ = () -> (
    -- promote/lift from ZZ to QQ
    -- promote/lift from QQ to RR_*, CC_*
    -- others should fail?
    -- test these in matrices/mutable matrices, as engine code isn't called for QQ, RR_*, CC_* on elements
    tab := {{1, -3, 0, 2387147821647821647812641278461782461}};
    m := matrix(ZZ, tab);
    assert(promote(m, QQ) == matrix(QQ, tab));
    assert(lift(promote(m,QQ), ZZ) == m);
    mRR53 := promote(m, RR_53);
    mRR1000 := promote(m, RR_1000);
    lift(mRR53, ZZ); -- CRASH!  (not implemented in lift)
    lift(mRR1000, ZZ); -- CRASH!  (not implemented in lift)
    lift(mRR53, QQ); -- WRONG: cannot lift given matrix (want to use method of continued fractions, I think, as in front end).
    lift(mRR1000, QQ); -- WRONG: cannot lift given matrix
    m1 := promote(mRR53, RR_100);
    m2 := lift(mRR53, RR_100);
    assert(m1 == m2); -- promote and lift are really the same thing between different RR's
    assert(mRR53_(0,3) == 2387147821647821647812641278461782461.0p53);
    assert(mRR1000_(0,3) == 2387147821647821647812641278461782461.0p1000);
    m3 := promote(mRR53, CC_100);
    assert(mRR53 == lift(m3, RR_53));
    mQQ := matrix(QQ, {{1/2, 1124124/54698534, -12/214718971289741987}});
    m1 = promote(mQQ, RR_53);
    lift(m1, QQ); -- not yet
    m2 = promote(m1, RR_100);
    m3 = promote(mQQ, RR_100);
    assert not(m2 == m3); -- m2 is lifted from lower number of bits, so we expect these to be different
    m4 := promote(m3, RR_53);
    assert (norm(m1 - m4) < 2e-16); -- I guess this is close enough, for 2^-53 = 1.11022302462516e-16
    )

TEST ///
 debug EngineTests
 testPromoteLift()
///

-*
liftit = (r,s) -> (
    if r == 0 then return 0/1;
    r' := r;
    p := precision r;
    p2 := 2^p;
    m := mutableIdentity(ZZ,2);
    i := 0;
    while i < s do (
        << "-- top of loop --" << endl;
        a := round r';
        columnSwap(m,0,1);
        << m << endl << endl;
        columnAdd(m,0,a,1);
        << m << endl << endl;
        r' = r' - a;
        n := m_(0,0);
        d := m_(1,0);
        q := n / d;
        << "r' = " << r' << endl;
        if r === numeric(p,q) then return q;
        if r' == 0 or abs(n*d) > p2 then return promote(r,QQ);
        r' = 1/r' ;
        i = i+1;
        );
    )

continuedFraction = (r, nsteps) -> (
    r' := r;
    p := precision r;
    m := mutableIdentity(ZZ,2);
    i := 0;
    while i < nsteps list (
        << "-- top of loop --" << endl;
        a := floor r';
        columnSwap(m,0,1);
        << m << endl << endl;
        columnAdd(m,0,a,1);
        << m << endl << endl;
        r' = r' - a;
        n := m_(0,0);
        d := m_(1,0);
        q := n / d;
        << "r' = " << r' << endl;
        r' = 1/r' ;
        i = i+1;
        a
        )
    )
a = pi * 1.0p100
liftit(3.14159p53)
sqrt(2p100)
liftit(oo, 3)
continuedFraction(sqrt(2p100), 100)
continuedFraction(pi*1.0p100, 10)
3 + 1/7
3 + 1/(7 + 1/(15+1/(1+1/(292+1/1))))
*-

