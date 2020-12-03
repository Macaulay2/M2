needs "test-mem-leaks.m2"
debug needsPackage "NumericalAlgebraicGeometry"
debug SLPexpressions
debug Core

setRandomSeed "ab"

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
M0 = random(CC^1, CC^(n+1));

tt = inputGate [symbol t];
Huncompressed = gammaValue*(1-tt)*gateMatrix polyS + tt*gateMatrix polyT;
        
TESTmakeSLProgram = () -> (
    slp := makeSLProgram(M, I);
    )

TESTrawSLEvaluator = () -> (
    slp := makeSLProgram(M, I);
    evaluate(slp, M0);
    )

TESTgateMatrix = () -> (
    t := local t;
    tt := inputGate [t];
    gammaValue*(1-tt)*gateMatrix polyS + tt*gateMatrix polyT;
    )

TESTsegmentHomotopy = () -> (
    segmentHomotopy(S,T,gamma=>1+pi*ii);
    )

TESTsegmentHomotopy1 = () -> (
    segmentHomotopy(S,T);
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

TESTmutableMatrix = () -> (
    mutableMatrix(RR, 3, 5);
    )
needsPackage "MonodromySolver"
debug Core
p = point {{1.79463+.302691*ii, -.379269+1.29466*ii, 2.49917+.526336*ii, 2.28917-1.3737*ii, -1.78834+.847366*ii}}
q = point {{1.79463+.302691*ii, 2.49917+.526336*ii, -.379269+1.29466*ii, 2.28917-1.3737*ii, -1.78834+.847366*ii}}
mp = raw transpose mutableMatrix matrix p;

TESTpointArray = () -> (
    PA := rawPointArray(1e-4,2*#coordinates p);
    rawPointArrayLookupOrAppend(PA,mp,0);
    )

end--

restart
needs "NAG-leaks.m2"
testF(10000000, TESTmutableMatrix)
  -- M2 eigen branch 5 June 2019 (no leak)
  --   elapsed time = 56.209
  --   leaks .0974848 bytes, takes .0056209 ms. (per call)
  -- M2 master branch 5 June 2019 (no leak)
  --   elapsed time = 60.8538
  --   leaks -.936346 bytes, takes .00608538 ms. (per call)
  
testF(3*100000,TESTmakeSLProgram) 
  -- does not leak
  -- M2 eigen branch 5 June 2019: no leak
  --   elapsed time = 22.0832
  --   leaks 1.36533 bytes, takes .0736105 ms. (per call)
  -- M2 master branch 5 June 2019 (no leak) mildly faster?
  --   elapsed time = 20.4895
  --   leaks 1.01035 bytes, takes .0682984 ms. (per call)

testF(100000,TESTrawSLEvaluator) 
  -- M2 eigen branch 5 June 2019
  --   elapsed time = 24.872
  --   leaks 3.31776 bytes, takes .24872 ms. (per call)
  -- M2 master branch 5 June 2019
  --   elapsed time = 23.485
  --   leaks 3.35872 bytes, takes .23485 ms. (per call)

testF(1000000,TESTgateMatrix)
  -- M2 eigen branch 5 June 2019 (no leak)
  --   elapsed time = 20.6455
  --   leaks .344064 bytes, takes .0206455 ms. (per call)
  -- M2 master branch 5 June 2019 (no leak)
  --   elapsed time = 19.5357
  --   leaks .335872 bytes, takes .0195357 ms. (per call)

testF(2*10000,TESTsegmentHomotopy) -- LEAK (FIX) 
-- elapsed time = 52.4079
-- leaks 4.096 bytes, takes 5.24079 ms. (per call)
  -- M2 eigen branch 5 June 2019: SMALL LEAK (I think)
  --   elapsed time = 28.4519
  --   leaks 208.486 bytes, takes 2.84519 ms. (per call)
  -- M2 master branch 5 June 2019 (small leak?) MUCH SLOWER...
  --   elapsed time = 55.3778
  --   leaks 16.5888 bytes, takes 2.76889 ms. (per call)
testF(2*10000,TESTsegmentHomotopy1) -- LEAK (FIX) 
-- LEEKS MORE than the one above

testF(10000,TESTsolve) -- LEAK (FIX)
-- elapsed time = 84.1909
-- leaks 7161.45 bytes, takes 8.41909 ms. (per call) TIME IS SLOWER!
  -- M2 eigen branch 5 June 2019: LEAK
  --   elapsed time = 136.331
  --   leaks 461.21 bytes, takes 13.6331 ms. (per call)
  -- M2 master branch 5 June 2019 (BIG LEAK)
  --   elapsed time = 142.476
  --   leaks 15222.4 bytes, takes 14.2476 ms. (per call)

testF(10000000,TESTpointArray)
-- elapsed time = 79.8302
-- leaks 874.619 bytes, takes .798302 ms. (per call)
  -- M2 eigen branch 5 June 2019: NO leak
  --   elapsed time = 31.3268
  --   leaks .497254 bytes, takes .00313268 ms. (per call)
  -- M2 master branch 5 June 2019 (BIG LEAK)
  --   elapsed time = 25.9994
  --   leaks 555.488 bytes, takes .00259994 ms. (per call)
