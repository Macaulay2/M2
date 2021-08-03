testSVD = (n,F) -> (
    M := mutableMatrix(F,n,n);
    fillMatrix(M);
    elapsedTime (S,U,Vt) := SVD M;
    norm(M - U*mutableMatrix diagonalMatrix S*Vt)
    )
end
restart
load "test.m2"
testSVD(10,CC_100)
testSVD(100,CC_100)

testSVD(100,CC_53)

testSVD(500,CC_53)