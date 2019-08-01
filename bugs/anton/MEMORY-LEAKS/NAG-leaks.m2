needs "test-mem-leaks.m2"
debug needsPackage "NumericalAlgebraicGeometry"
debug SLPexpressions
debug Core

errorDepth = 0
n = 2; d = 2;
R=QQ[x_0..x_(n-1)]
eps = 1/10^20
T = apply(n, i->if i==0 then x_i^d-eps^d else (x_i-i)^d-eps^(d-1)*x_i)
(S,solsS) = totalDegreeStartSystem T
H = segmentHomotopy(S,T,gamma=>1+pi*ii);
polyS = polySystem S;
polyT = polySystem T;
M = H#"X" | matrix{{H#"T"}};
I = H#"H";
gammaValue = random CC

TESTrawSLProgram = () -> (
    rawSLProgram(1);
    )

TESTmakeSLProgram = () -> (
    slp := makeSLProgram(I,M);
    scan(flatten entries M, g->removeSLPfromCache(slp,g));    
    scan(flatten entries I, g->removeSLPfromCache(slp,g));    
    )

TESTmakeEvaluator = () -> (
    makeEvaluator(I,M);
    )

TESTgateMatrix = () -> (
    t := local t;
    tt := inputGate [t];
    gammaValue*(1-tt)*gateMatrix polyS + tt*gateMatrix polyT;
    )

TESTsegmentHomotopy = () -> (
    segmentHomotopy(S,T,gamma=>1+pi*ii);
    )

TESTsolve = () -> (
    solveSystem T;
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

needsPackage "MonodromySolver"
p = point {{1.79463+.302691*ii, -.379269+1.29466*ii, 2.49917+.526336*ii, 2.28917-1.3737*ii, -1.78834+.847366*ii}}
q = point {{1.79463+.302691*ii, 2.49917+.526336*ii, -.379269+1.29466*ii, 2.28917-1.3737*ii, -1.78834+.847366*ii}}

TESTpointArray = () -> (
    pointArray {p,q};
    )

end--

restart
needs "NAG-leaks.m2"

testF(1000000,TESTrawSLProgram) 
-- does not leak

testF(100000,TESTmakeSLProgram) 
-- does not leak

testF(100000,TESTmakeEvaluator)
-- elapsed time = 19.4445
-- leaks 6.06208 bytes, takes .194445 ms. (per call)

testF(1000000,TESTgateMatrix)
-- elapsed time = 29.2733
leaks 0 bytes, takes .0292733 ms. (per call)

testF(10000,TESTsegmentHomotopy)
-- elapsed time = 52.4079
-- leaks 4.096 bytes, takes 5.24079 ms. (per call)

testF(10000,TESTsolve)
-- elapsed time = 84.1909
-- leaks 7161.45 bytes, takes 8.41909 ms. (per call)

testF(100000,TESTpointArray)
-- elapsed time = 79.8302
-- leaks 874.619 bytes, takes .798302 ms. (per call)
