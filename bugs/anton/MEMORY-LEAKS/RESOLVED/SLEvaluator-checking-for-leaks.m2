restart
needs "../test-mem-leaks.m2"
debug Core
debug needsPackage "SLPexpressions"

-- check SLPexpressions

X = inputGate symbol X
C = inputGate symbol C
XpC = X+C
XXC = productGate{X,X,C}
detXCCX = detGate{{X,C},{C,X}}
XoC = X/C
s = makeSLProgram({C,X},{XXC,detXCCX,XoC,XpC+XoC}) 

(consts,indets):=(positionsOfInputGates({C},s), positionsOfInputGates({X},s))
R = CC_1000
f = () -> (
    eCC := rawSLEvaluator(s,consts,indets,raw mutableMatrix{{3_R}});
    outM := mutableMatrix(R,1,4);
    inM := mutableMatrix{{7_R}};
    rawSLEvaluatorEvaluate(eCC, raw inM, raw outM);
    -- assert (abs(last flatten entries outM - 37/3) < 2^(-999))
    ) 
testF(100000, f)

