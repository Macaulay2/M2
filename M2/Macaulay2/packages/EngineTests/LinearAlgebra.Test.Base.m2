-- -*- coding: utf-8 -*-


export { "jordanForm",
    "maxFLINTPrime",
    "maxFFPACKPrime",
    "testDeterminant",
    "testMult",
    "testInverse",
    "testNullspace",
    "testRank",
    "testRankProfile",
    "testSolve",
    "testLinearAlgebra",
    "testLinearAlgebraSet",
    "testLUBoundaryCases",
    "testHasEngineLinearAlgebra"
    }

needs "EngineTests/LinearAlgebra.Test.LU.Base.m2"

debug Core;
-----------------------------------------------------------------
-- Test of MutableMatrix linear algebra over fields -------------
-----------------------------------------------------------------
maxFLINTPrime = 4294967291		    -- the largest prime less than 2^32
maxFFPACKPrime = 33554393

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
    kk := ZZp(101, Strategy=>"Ffpack");
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
  R = ZZp(101, Strategy=>"Flint")
  benchMult(R,300); -- fails, due to row/col major encoding (I think).

  debug Core
  R = ZZp(101, Strategy=>"Ffpack")
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
  R2 = ZZp(33554393, Strategy=>"Flint");
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
    for i from 1 to 20 do (
        (S,Sinv) := randomFullRank(R,i);
        assert(S*Sinv == mutableIdentity(R,i));
        assert(Sinv*S == mutableIdentity(R,i));
        )
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
    L := {1,2,4,7,12, 18};
    for j in L do (
    for i in L do if i <= j then (
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
    L := {1,2,4,8,15,20};
    for i in L do
        for j in L do if j <= i then (
            (M,X) := randomMatrixWithKernel(R, i, j);
            assert(rank M == j);
        ))

makeMatrixWithColumnRankProfile = (R, nrows, ncols, prof) -> (
    -- prof is a list of column indices (so each entry should be in range [0,ncols-1])
    A := first randomFullRank(R, nrows);
    B := mutableMatrix(R, nrows, ncols);
    -- now set B with random values
    for p from 0 to #prof-1 do B_(p,prof#p) = 1_R;
    rk := 1;
    prof = append(prof, ncols);
    for i from 1 to #prof - 1 do (
        -- set all entries in rows 0..rk in columns prof#(i-1) to prof#i-1 to random values
        for r from 0 to rk-1 do for c from prof#(i-1)+1 to prof#i - 1 do B_(r,c) = random R;
        rk = rk+1;
        );
    A*B
    --(A,B)
    )

testRankProfile = (R) -> (
    -- base cases:
    A := makeMatrixWithColumnRankProfile(R, 3, 7, {});
    assert(columnRankProfile A == {});
    assert(rowRankProfile A == {});
    A = makeMatrixWithColumnRankProfile(R, 0, 0, {});
    assert(columnRankProfile A == {});
    assert(rowRankProfile A == {});
    A = makeMatrixWithColumnRankProfile(R, 0, 4, {});
    assert(columnRankProfile A == {});
    assert(rowRankProfile A == {}); 
    A = makeMatrixWithColumnRankProfile(R, 4, 0, {});
    assert(columnRankProfile A == {});
    assert(rowRankProfile A == {});
    -- 3 x 7 matrices of rank 2
    for p in subsets(7,2) do (
        A = makeMatrixWithColumnRankProfile(R, 3, 7, p);
        assert(columnRankProfile A == p);
        );
    -- 3 x 7 matrices of rank 3
    for p in subsets(7,3) do (
        A = makeMatrixWithColumnRankProfile(R, 3, 7, p);
        assert(columnRankProfile A == p);
        B := transpose A;
        assert(rowRankProfile B == p);
        );
    );

testRREF = (R) -> (
    -- create a matrix, take RREF
    -- check that it is in rref form
    -- now check that ranks are the same
    -- and check that by combining rows, rref, then taking non-zero elements, 
    -- we get the same as the non-zero elements of the rref.
    )

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

testSolveNotFullRank = (R) -> (
    L := {{2, 3, 10, 11, 12}, {0, 6, 8, 9, 11}, {4, 5, 9, 10, 12}, 
        {0, 1, 2, 3, 9}, {0, 1, 5, 10, 11}, {0, 2, 6, 8, 12}, 
        {4, 5, 8, 10, 12}, {2, 3, 5, 6, 12}, {0, 1, 4, 6, 10}, 
        {1, 4, 6, 8, 9}};
    for i from 5 to 8 do
        for p in L do (
            m := makeMatrixWithColumnRankProfile(R, i, 13, p);
            x := mutableMatrix(R,numColumns m,2); fillMatrix x;
            b := m*x;
            X := solve(m,b);
            assert(m*X-b == 0);
            -- check one that isn't in there either
            c := mutableMatrix(R,numRows m,1); fillMatrix c;
            X = solve(m,c);
            if X =!= null then assert(m*X == c); -- really, we expect it to be null
            )
     )


testSolve = (R) -> (
    --testSolveSimple R;
    for i in {3,4,5,6,7,8,9,10} do (
        A := first randomFullRank(R, i);
        B := mutableMatrix(R, i, 3); fillMatrix B;
        X := solve(A,B);
        assert(X =!= null or rank A < i);
        if X =!= null then
            assert(A*X-B == 0);
        );
    testSolveNotFullRank R;    
    )

testSolveInvertible0 = (R) -> (
    -- first we need to see if the 3 types of output can happen:
    m := mutableMatrix(R,2,2);
    b := mutableMatrix(R,2,1);
    m_(0,1) = 4_R;
    b_(1,0) = 2_R;
    if rank m != numRows m then 
        assert(solve(m,b, Invertible=>true) === null); -- should return null if m not invertible
    b = mutableMatrix(R,3,1);
    assert(try (rawLinAlgSolveInvertible(raw m, raw b); false) else true);
    assert(try (solve(m, b, Invertible=>true); false) else true); -- should raise an error since sizes don't match
    m_(1,0) = 2_R;
    b = mutableMatrix(R,2,1);
    b_(0,0) = 5_R;
    if rank m == numRows m then (
        minv := inverse m;
        assert(solve(m, b, Invertible=>true) == minv*b);
        );
    )

testSolveInvertible = (R) -> (
    testSolveInvertible0 R;
    -- TODO: write some more serious tests
    )

testLinearAlgebra = (R) -> (
    -- ONLY TESTS DENSE MATRIX CASE
    time testLUBoundaryCases R;
    time testDeterminant R;
    time testMult R;
    time testInverse R;
    time testNullspace R;
    time testRank R;
    time testRankProfile R;
    time testSolve R; -- NOT DONE
    time testSolveInvertible R; -- NOT DONE
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

    assert hasEngineLinearAlgebra (ZZp(101, Strategy=>"Flint"));
    assert hasEngineLinearAlgebra ZZFlint;
    assert hasEngineLinearAlgebra QQFlint;

    if hasFFPACK then (
        assert hasEngineLinearAlgebra (ZZp(101, Strategy=>"Ffpack"));
        );

    -- The following do NOT have engine linear algebra yet, but they should:
    assert not hasEngineLinearAlgebra (GF(25));
    assert not hasEngineLinearAlgebra (GF(25, Strategy=>"New"));
    assert not hasEngineLinearAlgebra (GF(25, Strategy=>"Flint"))
    )

TEST ///
    testHasEngineLinearAlgebra()
///
