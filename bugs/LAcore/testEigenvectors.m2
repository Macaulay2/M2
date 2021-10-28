conjugate Matrix := Matrix => M -> matrix table(numrows M, numcols M, (i,j) -> conjugate M_(i,j))

testEigenvalues = (n,F,isHermitian) -> (
    M := matrix fillMatrix mutableMatrix(F,n,n);
    if isHermitian then M = M + conjugate transpose M;
    elapsedTime E := eigenvalues(M, Hermitian => isHermitian);
    norm matrix{apply(E, e -> det(M - id_(F^n)*e))}
    )

testEigenvectors = (n,F,isHermitian) -> (
    M := matrix fillMatrix mutableMatrix(F,n,n);
    if isHermitian then M = M + conjugate transpose M;
    elapsedTime (E,V) := eigenvectors(M, Hermitian => isHermitian);
    norm(M*V - V*diagonalMatrix E)
    )
end--

restart
load "testEigenvectors.m2"

testEigenvalues(10,CC_100, false)
 -- 0.0300995 seconds elapsed
    -- 7.5897592092500107469917360173e-21
testEigenvalues(10,CC_100, true)
 -- 0.0049021 seconds elapsed
    -- 1.9713247129867907428533091422e-20
testEigenvalues(10,CC_500, false)
 -- 0.0409078 seconds elapsed
    -- 1.39280593400981052652588339458119110841201051959083300602716189728569671590177320071601097969295165428154501503806003629832488221985523415157968449864e-141
testEigenvalues(10,CC_500, true)
 -- 0.0047787 seconds elapsed
    -- 2.37314453273467319706200409330257482872831526251611480857988430382101293847622376528786753379038840631218259974238291004495278506765850906844924935543e-140
testEigenvalues(100,CC_100, false)
 -- 20.5304 seconds elapsed
    -- 1.62365166256065346336123686143e155
testEigenvalues(100,CC_100, true)
 -- 1.11957 seconds elapsed
    -- 6.14419188544453999204646582443e170
testEigenvalues(100,CC_53, false)
 -- 0.0777056 seconds elapsed
    -- infinity
testEigenvalues(100,CC_53, true)
 -- 0.0155782 seconds elapsed
    -- infinity
testEigenvalues(500,CC_53, false)
 -- 1.16855 seconds elapsed
    -- 0
testEigenvalues(500,CC_53, true)
 -- 0.0769585 seconds elapsed
    -- 0
    
testEigenvectors(10,CC_100, false)
 -- 0.0397692 seconds elapsed
    -- 2.46448025173513498498555894298e-29
testEigenvectors(10,CC_100, true)
 -- 0.008271 seconds elapsed
    -- 5.36629298798678619354468946955e-29
testEigenvectors(100,CC_100, false)
 -- 34.0853 seconds elapsed
    -- 2.61806759799042926123801833026e-28
testEigenvectors(100,CC_100, true)
 -- 6.47257 seconds elapsed
    -- 1.26451874839045744567789405633e-28
testEigenvectors(100,CC_53, false)
 -- 0.0512446 seconds elapsed
    -- 1.43214466921978e-14
 -- LAPACK: 0.471286 seconds elapsed
testEigenvectors(100,CC_53, true)
 -- LAPACK: 0.0336566 seconds elapsed
testEigenvectors(500,CC_53, false)
 -- 9.61811 seconds elapsed
    -- 2.5498610904972e-13
 -- LAPACK: seg fault
 testEigenvectors(500,CC_53, true)
 -- LAPACK: 0.711708 seconds elapsed
    -- 29.4428352754329
