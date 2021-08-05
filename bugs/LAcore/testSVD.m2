testSVD = (n,F,doDivideConquer) -> (
    M := matrix fillMatrix mutableMatrix(F,n,n);
    elapsedTime (S,U,Vt) := SVD(M, DivideConquer => doDivideConquer);
    norm(M - U*diagonalMatrix S*Vt)
    )
end--

restart
load "testSVD.m2"

testSVD(10, CC_100, false)
 -- 0.0836859 seconds elapsed
    -- 4.18691223191206101368034303402e-29
testSVD(10, CC_100, true)
 -- 0.0771507 seconds elapsed
    -- 4.26641766447494442836279415274e-29
testSVD(100, CC_100, false)
 -- 129.978 seconds elapsed
    -- 4.21716068129203061644930260782e-28
testSVD(100, CC_100, true)
 -- 10.4747 seconds elapsed
    -- 7.13341404427575308113160829207e-29
testSVD(100,CC_53, false)
 -- 0.0917416 seconds elapsed
    -- 5.7445325344286e-15
testSVD(100,CC_53, true)
 -- 0.0576426 seconds elapsed
    -- 4.34266116746861e-15
testSVD(500,CC_53, false)
 -- 1.40274 seconds elapsed
    -- 3.67884423497353e-14
testSVD(500,CC_53, true)
 -- 0.422249 seconds elapsed
    -- 3.28642893246205e-14