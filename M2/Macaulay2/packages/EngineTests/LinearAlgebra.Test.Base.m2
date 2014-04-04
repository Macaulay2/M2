-- -*- coding: utf-8 -*-


export { jordanForm,
    maxFLINTPrime,
    maxFFPACKPrime,
    testDeterminant,
    testMult,
    testInverse,
    testNullspace,
    testRank,
    testRankProfile,
    testSolve,
    testLinearAlgebra,
    hasFlint,
    hasFFPACK,
    testLinearAlgebraSet,
    testLUBoundaryCases,
    testHasEngineLinearAlgebra
    }



debug Core;
-----------------------------------------------------------------
-- Test of MutableMatrix linear algebra over fields -------------
-----------------------------------------------------------------
maxFLINTPrime = 18446744073709551521
maxFFPACKPrime = 33554393

hasFFPACK = try (ZZp(101, Strategy=>"FFPACK"); true) else false;
hasFlint = try (ZZp(101, Strategy=>"FLINT"); true) else false;


needsPackage "FastLinearAlgebra"
debug FastLinearAlgebra

load "EngineTests/LinearAlgebra.Test.LU.Base.m2"


jordanBlock = (R, diag, n) -> (
    m := mutableMatrix(R,n,n);
    for i from 0 to n-1 do m_(i,i) = diag;
    for i from 0 to n-2 do m_(i,i+1) = 1;
    m
    )
jordanForm = method()
jordanForm(Ring, List) := (R, L) -> (
    -- L is a list of (eigenvalue, size)
    directSum apply(L, x -> matrix jordanBlock(R, x#0, x#1))
    )

changeOfBasis = method()
changeOfBasis(Ring, ZZ) := (R,n) -> (
    -- returns a pair of matrices S, S^-1
    S := random(R^n, R^n);
    S1 := mutableMatrix S;
    Sinv := inverse S1;
    (S1, Sinv)
    )


-- the following function finds an n x n matrix S, and its inverse, S^-1 
-- over the ring kk.
-- The ring kk should be a field, for which det, rawLinAlgInverse are defined
randomFullRank = (R,n) -> (
    S := mutableMatrix(R,n,n);
    for i from 0 to n-1 do for j from 0 to n-1 do S_(i,j) = random 100;
    --fillMatrix S;
    while det S == 0 do (
        S = mutableMatrix(R,n,n);
        fillMatrix S;
        );
    (S, inverse S)
    --Sinv := map(R, rawLinAlgInverse raw S);
    --(S, Sinv)
    )

randomJordanForm = (R, L) -> (
    M := mutableMatrix jordanForm(R,L);
    (S,Sinv) := randomFullRank(R, numColumns M);
    S * M * Sinv
    )

testLinAlgZZpFFPACK = () -> (
    kk := ZZp(101, Strategy=>"FFPACK");
    S := mutableMatrix(kk,3,3);
    fillMatrix S;
    D := mutableMatrix jordanForm(kk, splice{{1,3},4:{0,1},{2,3}});
    M := S * D;
    
    )

testRankBIG = (R) -> (
     << "testrank (TODO:finish it)..." << endl;
     m1 := random(R^5, R^11);
     m2 := random(R^11, R^6);
     m := mutableMatrix(m1 * m2);
     assert(5 == rank m); -- this can fail every now and then.
     N := 300;
     M := 200;
     m3 := mutableMatrix(ZZ, N, M);
     m4 := mutableMatrix(ZZ, M, N);
     fillMatrix m3;
     fillMatrix m4;
     m3 = time mutableMatrix sub(matrix m3, QQ);
     m4 = time mutableMatrix sub(matrix m4, QQ);
     time (m3*m4); -- MUCH faster than next line!
     time m5 := mutableMatrix((matrix m3) * (matrix m4));
     assert(m3*m4 == m5);
     debug Core;
     time rawLinAlgDeterminant raw m5;
     time det m5; -- doesn't work yet
     time rank m5; -- crashes: duplicate large block deallocation
     -- test of inverse:
     m1 = mutableMatrix(QQ, 4, 4);
     fillMatrix m1 ;
     inverse m1; -- doesn't work yet
     m2 = map(QQ, rawLinAlgInverse raw m1);
     m1*m2 == mutableIdentity(QQ, 4);
     )

///
-- simpler bug
     debug Core;
     R := ZZFlint;
     N = 100
     N = 70
     N = 50
     N = 20
     tally for i from 1 to 10000 list (
       m = mutableMatrix(R, N, N);
       fillMatrix m;
       rank m
       )
///

testRankFailing = () -> (
     debug Core;
     R := ZZFlint;
     (N,M) := (100,70);
     --(N,M) := (500,372);
     m3 := mutableMatrix(R, N, M);
     m4 := mutableMatrix(R, M, N);
     fillMatrix m3;
     fillMatrix m4;
     m5 := m3*m4;
     assert(0 == det m5);
     rank m5;  -- crash for R==ZZFlint
     m6 := m4*m3;
     rank m3;
     rank m4;
     rank m6 ;-- crash for R==ZZFlint
     rawLinAlgDeterminant raw m5;
     rawLinAlgRank raw m5; -- 
     )
     


benchMult = (R,N) -> (
    A := mutableMatrix(R,N,N);
    B := mutableMatrix(R,N,N);
    fillMatrix A;
    fillMatrix B;
    time A1 := matrix A;
    time B1 := matrix B;
    C := time A*B;
    C1 := time A1*B1;
    C1 = mutableMatrix C1;
    if C != C1 then << "ERROR: matrix mult is WRONG" << endl;
    (A,B,C,C1)
    )
///
  benchMult(ZZ,3)
  benchMult(ZZ,100);
  benchMult(ZZ,300);

  benchMult(ZZ/101,300);

  debug Core
  R = ZZp(101, Strategy=>"FLINT")
  benchMult(R,300); -- fails, due to row/col major encoding (I think).

  debug Core
  R = ZZp(101, Strategy=>"FFPACK")
  benchMult(R,10);
  oo#2 == oo#3

  benchMult(R,600);
  benchMult(RR_100,100);  -- WRONG
  (A,B,C,C1) = benchMult(RR_53,10);  -- some error.  Which is better?
  clean(1.0*10^-13, C-C1)
  clean(1.0*10^-13, matrix(C-C1))
  (A,B,C,C1) = benchMult(RR_53,100);
  flatten entries (C-C1);
  oo/abs//max
  (A,B,C,C1) = benchMult(RR_200,100);
  flatten entries (C-C1);
  oo/abs//max

  (A,B,C,C1) = benchMult(RR_200,300);
  flatten entries (C-C1);
  oo/abs//max

  (A,B,C,C1) = benchMult(RR_53,300);

  debug Core
  R2 = ZZp(33554393, Strategy=>"FLINT");
  benchMult(R2, 400); -- the multiply routine is the flint one: but it is multiplying the wrong order.
    -- This is caused by row/column major order problem in my (MES) code.
///

testDeterminant2x2 = (R) -> (
    for i from 1 to 10 do (
        M := mutableMatrix(R, 2, 2);
        fillMatrix M;
        assert(determinant M == M_(0,0) * M_(1,1) - M_(0,1) * M_(1,0));
        );
    )
testDeterminant3x3 = (R) -> (
    for i from 1 to 10 do (
        M := mutableMatrix(R, 3, 3);
        fillMatrix M;
        ans := M_(0,0) * M_(1,1) * M_(2,2) +
           M_(1,0) * M_(2,1) * M_(0,2) +
           M_(2,0) * M_(0,1) * M_(1,2) -
           M_(2,0) * M_(1,1) * M_(0,2) -
           M_(1,0) * M_(0,1) * M_(2,2) -
           M_(0,0) * M_(2,1) * M_(1,2);
        assert(determinant M == ans)
        );
    )
testDeterminantViaMult = (R) -> (
    for i from 1 to 10 do (
        M := mutableMatrix jordanForm(R, splice{{i-1,3},{i+13,2},7:{i,1}});
        assert((i-1)^3 * (i+13)^2 * i^7 == det M);
        S := mutableMatrix(R, numRows M, numColumns M);
        fillMatrix(S, Density=>.3);
        assert(ring det S === R);
        assert(det(S*M*S) == det(S)^2 * det(M));
        );
    )
testDeterminant = (R) -> (
    testDeterminant2x2 R;
    testDeterminant3x3 R;
    testDeterminantViaMult R;
    )

testMultSimple = (R) -> (
    E := {{1_R, 2_R, 3_R}, {7_R, 0_R, 0_R}};
    m1 := mutableMatrix E;
    m2 := mutableMatrix transpose E;
    assert(matrix(m1*m2) == (matrix m1) * (matrix m2));
    assert(matrix(m2*m1) == (matrix m2) * (matrix m1));
    m3 := mutableMatrix(R,0,3);
    m4 := mutableMatrix(R,3,4);
    m5 := m3*m4;
    assert(numRows m5 == 0);
    assert(numColumns m5 == 4);
    )
testMultrs = (R,r,s) -> (
    for i from 1 to 10 do (
        M := mutableMatrix(R, r,s);
        N := mutableMatrix(R, s,r);
        fillMatrix M;
        fillMatrix N;
        MN := M*N;
        NM := N*M;
        assert(MN == mutableMatrix((matrix M) * (matrix N)));
        assert(NM == mutableMatrix((matrix N) * (matrix M)));
        );
    )
testMultAddSub = (R1) -> (
    A := mutableMatrix(R1,3,6);
    B := mutableMatrix(R1,6,5);
    C := mutableMatrix(R1,3,5);
    fillMatrix A; fillMatrix B; fillMatrix C;
    C1 := matrix C;
    rawLinAlgAddMult(raw C,raw A,raw B);
    assert(C1 + (matrix A) * (matrix B) == matrix C);
    C = mutableMatrix C1;
    rawLinAlgSubMult(raw C,raw A,raw B);
    assert(C1 - (matrix A) * (matrix B) == matrix C);
    )
testMult = (R) -> (
  testMultSimple R;
  testMultrs(R,2,3);
  testMultrs(R,5,10);
  testMultrs(R,10,3);
  testMultAddSub R;
  )

testInverse = (R) -> (
    m1 := mutableMatrix(R,0,0);
    --assert(inverse m1 == 0);
    m2 := mutableMatrix(R,1,1);
    --inverse m2 -- gives an unknown engine error
    for i from 2 to 20 do (
        for j from 1 to 5 do (
            (S,Sinv) := randomFullRank(R,i);
            assert(S*Sinv == mutableIdentity(R,i));
            assert(Sinv*S == mutableIdentity(R,i));
        ))
    )

randomMatrixWithKernel = (R, ncols, rk) -> (
    -- returns a pair (M, X), such that M*X == 0, and the
    -- columns of X are a basis for ker M.
    MAXN := if char R > 0 then char R - 1 else 50;
    L1 := for i from 0 to rk-1 list {(1 + random MAXN)_R, 1};
    L2 := splice {(ncols-rk):{0,1}};
    L := join(L1,L2);
    A := mutableMatrix jordanForm(R, L);
    (S,Sinv) := randomFullRank(R, ncols);
    M := Sinv*A*S;
    X := mutableMatrix(R,ncols,ncols-rk);
    for i from 0 to ncols-rk-1 do X_(rk+i,i) = 1;
    (M, Sinv*X)
    )
testNullspace1 = (R) -> (
    -- we expect that R has the usual linear algebra routines defined
    (S,Sinv) := randomFullRank(R,10);
    D := mutableMatrix jordanForm(R, splice{{1,3},4:{0,1},{2,3}});
    M := S * D * Sinv;
    r := rank M;
    assert(r == rank D);
    assert(det M == det D);
    V := nullSpace M;
    assert(M * V == 0);
    assert(numColumns M == r + numColumns V)
    )
testNullspace = (R) -> (
    E := map(R^2, R^3, {{1, 4, 5}, {2, 3, 6}});
    M := mutableMatrix E;
    X := nullSpace M;
    assert(M*X == 0);
    assert(numColumns X == 1);
 
    testNullspace1 R;
        
    -- Now: create matrices with known null space, and check them
    for j from 1 to 15 do (
    for i from 0 to j do (
      (M,X1) := randomMatrixWithKernel(R, j, i);
      X2 := nullSpace M;
      Xall := mutableMatrix((matrix X1) | (matrix X2));
      assert(M*X2 == 0);
      assert(numColumns X2 == j-i);
      assert(rank Xall == j-i);
      ))
    )

testRank = (R) -> (
    -- make a random square matrix, with known rank.  Compute rank.
    M := mutableMatrix(R,0,0);
    assert(rank M == 0);
    for i from 1 to 20 do (
        for j from 0 to i do (
            (M,X) := randomMatrixWithKernel(R, i, j);
            assert(rank M == j);
        ))
    )

testRankProfile = (R) -> (
    -- WRITE ME WRITE ME
    M1 := mutableMatrix matrix(R, {{2, 16, 29}, {-18, 24, 12}, {-41, 7, -31}});
    rowRankProfile M1
    );

testSolveSimple = (R) -> (
    M := (if char R == 5 
         then mutableMatrix map(R^2, R^2, {{1, 4}, {2, 2312}})
         else mutableMatrix map(R^2, R^2, {{1, 4}, {2, 3}}));
    B := mutableMatrix map(R^2, R^1, {{3}, {7}});
    -- square M, B has one col
    X := solve(M,B);
    assert(M*X-B == 0);
    -- square M, B has 3 cols
    B2 := mutableMatrix map(R^2, R^3, {{1,0,3},{0,1,7}});
    X = solve(M,B2);
    assert(M*X-B2 == 0);
    -- M is 2x3, B is 2x1
    M = mutableMatrix map(R^2, R^3, {{1,4,7},{1,8,9}});
    B = mutableMatrix map(R^2, R^1, {{1},{3}});
    X = solve(M,B);
    assert(M*X-B == 0);
    -- M is 2x3, B is 2x1
    M = mutableMatrix transpose map(R^2, R^3, {{1,4,7},{1,8,9}});
    B = mutableMatrix map(R^3, R^1, {{5},{32},{41}});
    X = solve(M,B);
    assert(M*X-B == 0);
    )
testSolve = (R) -> (
    testSolveSimple R;
    return;
    -- now for more complicated examples
    -- FAILING TEST: crashes
    debug Core;
    N := 90;
    M := mutableMatrix(R, N, N);
    fillMatrix M;
    B := mutableMatrix(R, N, 5);
    fillMatrix B;
    XRaw := time rawLinAlgSolve(raw M, raw B, true);
    X := solve(M, B);
    assert( raw X == XRaw);
    assert(M*X-B == 0);
    )

testLinearAlgebra = (R) -> (
    -- ONLY TESTS DENSE MATRIX CASE
    testLUBoundaryCases R;
    testDeterminant R;
    testMult R;
    testInverse R;
    testNullspace R;
    testRank R;
    testRankProfile R; -- NOT WRITTEN YET
    testSolve R; -- NOT DONE
    )
    
testLinearAlgebraSet = (rng) -> (
  debug Core;
  testDeterminant rng;
  testMult        rng;
  testInverse     rng;
  testRank        rng;
  testNullspace   rng;
  testSolve       rng;
)

testHasEngineLinearAlgebra = () -> (
    -- Note: hasEngineLinearAlgebra is only used for matrices, not mutable matrices
    -- EXCEPT currently: solve.
    assert hasEngineLinearAlgebra RR_100;
    assert hasEngineLinearAlgebra RR_53;
    assert hasEngineLinearAlgebra CC_100;
    assert hasEngineLinearAlgebra CC_53;
    debug Core;
    -- ZZ linear algebra needs to take into account that solutions are over ZZ
    assert not hasEngineLinearAlgebra ZZ;
    -- QQ should have linear algebra set
    assert hasEngineLinearAlgebra QQ; -- when does this get set?
    if hasFlint then (
        assert hasEngineLinearAlgebra (ZZp(101, Strategy=>"FLINT"));
        assert hasEngineLinearAlgebra ZZFlint;
        assert hasEngineLinearAlgebra QQFlint;
        );
    if hasFFPACK then (
        assert hasEngineLinearAlgebra (ZZp(101, Strategy=>"FFPACK"));
        );
    -- The following do NOT have engine linear algebra yet, but they should:
      assert not hasEngineLinearAlgebra (GF(25));
      assert not hasEngineLinearAlgebra (GF(25, Strategy=>"New"));
      assert not hasEngineLinearAlgebra (GF(25, Strategy=>"CompleteGivaro"));
      assert not hasEngineLinearAlgebra (GF(25, Strategy=>"Givaro"));
      --hasEngineLinearAlgebra (GF(25, Strategy=>"FLINT")) -- this ring doesn't yet exist!
    )

TEST ///
    testHasEngineLinearAlgebra()
///