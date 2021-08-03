testEigenvectors = (n,F) -> (
    M := mutableMatrix(F,n,n);
    fillMatrix(M);
    elapsedTime (E,V) := eigenvectors M;
    norm(M*V - V*mutableMatrix diagonalMatrix E)
    )
end
restart
load "test.m2"
testEigenvectors(10,CC_100)
testEigenvectors(100,CC_100)

testEigenvectors(100,CC_53)
 -- 0.0512446 seconds elapsed
--  o7 = 1.43214466921978e-14
-- LAPACK:  -- 0.471286 seconds elapsed

testEigenvectors(500,CC_53)
 -- 9.61811 seconds elapsed
 -- o10 = 2.5498610904972e-13
 -- LAPACK: seg fault