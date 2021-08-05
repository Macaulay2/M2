testSolve = (n,F) -> (
    M := matrix fillMatrix mutableMatrix(F,n,n);
    b := matrix fillMatrix mutableMatrix(F,n,1);
    elapsedTime x := solve(M, b, ClosestFit => true);
    norm(b - M*x)
    )
end--

restart
load "testSolve.m2"

testSolve(10, CC_100)
 -- 0.0811792 seconds elapsed
    -- 8.38570644846520377471431308894e-30
testSolve(100, CC_100)
 -- 9.53715 seconds elapsed
    -- 2.58867804138806926438240058111e-28
testSolve(100,CC_53)
 -- 0.0318108 seconds elapsed
    -- 2.64263982371124e-14
testSolve(500,CC_53)
 -- 0.896753 seconds elapsed
    -- 4.30006541585593e-14