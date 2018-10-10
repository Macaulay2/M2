needs "test-mem-leaks.m2"
debug needsPackage "NumericalAlgebraicGeometry"
debug SLPexpressions

n = 2; d = 2;
R=QQ[x_0..x_(n-1)]
eps = 1/10^20
T = apply(n, i->if i==0 then x_i^d-eps^d else (x_i-i)^d-eps^(d-1)*x_i)
(S,solsS) = totalDegreeStartSystem T
H = segmentHomotopy(S,T,gamma=>1+pi*ii);
M = H#"X" | matrix{{H#"T"}};
I = H#"H";


TESTmakeSLProgram = () -> (
    makeSLProgram(I,M);
    )

TESTmakeEvaluator = () -> (
    makeEvaluator(I,M);
    )

TESTsolve = () -> (
    solveSystem T;
    )

TESTsegmentHomotopy = () -> (
    segmentHomotopy(S,T,gamma=>1+pi*ii);
    )

TESTtrackHomotopy = () -> (
    -*
    sols = trackHomotopy(H,solsS,tStepMin=>minimalStepSize 53,CorrectorTolerance=>1e-15,Precision=>infinity,EndZoneFactor=>0)
    assert((first sols).NumberOfSteps == 101)

sols = trackHomotopy(H,solsS, CorrectorTolerance=>1e-15,Precision=>53,EndZoneFactor=>0)
peek sols 

sols = trackHomotopy(H,solsS, CorrectorTolerance=>1e-15,Precision=>100,EndZoneFactor=>0)
peek sols 

sols = trackHomotopy(H,solsS, CorrectorTolerance=>1e-15,Precision=>1000,EndZoneFactor=>0)
peek sols 
*- 
)

end--

restart
needs "NAG-leaks.m2"
errorDepth = 0

testF(1000,TESTmakeSLProgram)
testF(10000,TESTmakeEvaluator)
testF(100,TESTsegmentHomotopy)
testF(1000,TESTsolve)
