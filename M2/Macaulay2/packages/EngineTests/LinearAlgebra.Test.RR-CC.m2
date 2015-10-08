hilbertMatrix = (R, nrows) -> (
    M := mutableMatrix(R, nrows, nrows);
    for i from 0 to nrows-1 do for j from 0 to nrows-1 do M_(i,j) = 1/(i+j+1);
    M
    )

-- see wiki: condition number
-- Ax = b
-- we need euclidean norm of x and b,
-- and the condition number of A.
-- (relative error on x) < condition(A) * (relative error on b)
-- and (relative error of x) = norm(x-xans)/norm(x)
norm2 = (M) -> sqrt sum apply(flatten entries M, x -> abs(x)^2)

testSolveApprox = (R,M,B) -> (
    -- M and B are matrices over QQ
    -- R is a RR or CC ring
    X1 := solve(M,B);
    MR := mutableMatrix promote(matrix M, R);
    BR := mutableMatrix promote(matrix B,R);
    Xans := mutableMatrix promote(matrix X1, R);
    -- Condition number and estimate of error
    -- WARNING: the check on the error is rough, and can be wrong on some input
    Sigma := flatten entries first SVD MR;
    condM := (first Sigma)/(last Sigma);
    -- now solve over R
    XR := solve(MR,BR);
    relX := norm2(XR-Xans)/norm2(XR);
    relB := sqrt(1.0 * numRows(B)) * (2^(-precision R));
    << "error relX = " << relX << " and expected max error is: " << (condM * relB) << endl;
    assert(relX < condM * relB);
    )


makeQQi = (R, nrows, ncols) -> (
    -- R = QQ[i]/(i^2+1)
    random(R^nrows, R^ncols) + R_0 * random(R^nrows, R^ncols))

testSolveApproxCC = (R,M,B) -> (
    -- M and B are matrices over QQ[i]/(i^2+1)
    -- R is a CC ring
    X1 := B // M;
    phi := map(R, ring M, {ii_R});
    MR := mutableMatrix phi M;
    BR := mutableMatrix phi B;
    Xans := mutableMatrix phi X1;
    -- Condition number and estimate of error
    -- WARNING: the check on the error is rough, and can be wrong on some input
    Sigma := flatten entries first SVD MR;
    condM := (first Sigma)/(last Sigma);
    -- now solve over R
    XR := solve(MR,BR);
    relX := norm2(XR-Xans)/norm2(XR);
    relB := sqrt(1.0 * numRows(B)) * (2^(-precision R));
    << "error relX = " << relX << " and expected max error is: " << (condM * relB) << endl;
    assert(relX < condM * relB);
    )


testSolveOverRRR = () -> (
    R := RR_100;
    -- First, a random matrix over QQ
    M := mutableMatrix(QQ,10,10);    fillMatrix M;
    B := mutableMatrix(QQ,10,1);     fillMatrix B;
    testSolveApprox(R,M,B);
    testSolveApprox(RR_300,M,B);
    testSolveApprox(RR_53,M,B);
    testSolveApprox(CC_53,M,B);
    testSolveApprox(CC_100,M,B);
    testSolveApprox(CC_200,M,B);
    testSolveApprox(CC_1000,M,B);
    -- 5 by 5 Hilbert
    H := hilbertMatrix(QQ, 5);
    B := mutableMatrix(QQ, 5, 1);
    B_(0,0) = 1;
    testSolveApprox(RR_53, H, B);
    -- 6 by 6 Hilbert
    H := hilbertMatrix(QQ, 6);
    B := mutableMatrix(QQ, 6, 1);
    B_(0,0) = 1;
    testSolveApprox(RR_200, H, B);
    )

testSolveOverCCC = () -> (
    R := CC_100;
    A := QQ[i]/(i^2+1);
    M := makeQQi(A,10,10);
    B := makeQQi(A,10,1);
    testSolveApproxCC(R,M,B);
    testSolveApproxCC(CC_300,M,B);
    testSolveApproxCC(CC_53,M,B);
    testSolveApproxCC(CC_100,M,B);
    testSolveApproxCC(CC_200,M,B);
    testSolveApproxCC(CC_1000,M,B);
    )

TEST ///
    for i from 0 to 100 do testSolveOverRRR()
///

TEST ///
  R = RR_53
  A = mutableMatrix(R,10,4); fillMatrix A
  B = mutableMatrix(R,10,1); fillMatrix B
  X = mutableMatrix(R,4,1); fillMatrix X
  solve(A,A*X)
  
  A = mutableMatrix(RR_200,10,4);
  B = mutableMatrix(RR_100,10,1);
  solve(A,B) == 0
///

///
R = RR_53
a1 = mutableMatrix(R, 10, 20); fillMatrix a1
a2 =  mutableMatrix(R, 20, 10); fillMatrix a2
A = a2*a1
b = mutableMatrix(R, 20, 1); fillMatrix b;
x = solve(A,b)
A*x-b 
///

