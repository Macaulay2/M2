restart
needsPackage "SLPexpressions"
debug SLPexpressions
X = inputGate symbol X
C = inputGate symbol C
XpC = X+C
XXC = productGate{X,X,C}
detXCCX = detGate{{X,C},{C,X}}
XoC = X/C
s = makeSLProgram({C,X},{XXC,detXCCX,XoC,XpC+XoC}) 

support(X*X)
support(detXCCX + X)
support gateMatrix{{detXCCX,X}}

assert(set support(C*X) === set support(X*C))

constants(X*(1*X+2))
constants(3*detXCCX + 2*X)
constants gateMatrix{{detXCCX,12*X}}

depth(X*X)
depth(detXCCX + X)
depth gateMatrix{{detXCCX,X}}
assert (depth {X+((detXCCX+X)*X)/C}==5)


--!!!!
debug Core
(consts,indets):=(positionsOfInputGates({C},s), positionsOfInputGates({X},s))
eQQ = rawSLEvaluator(s,consts,indets,raw mutableMatrix{{3_QQ}})
output = mutableMatrix(QQ,1,4)
rawSLEvaluatorEvaluate(eQQ, raw mutableMatrix{{7_QQ}}, raw output) 
output

eCC = rawSLEvaluator(s,consts,indets,raw mutableMatrix{{3_CC}})
output = mutableMatrix(CC,1,4)
rawSLEvaluatorEvaluate(eCC, raw mutableMatrix{{7_CC}}, raw output) 
output

R = CC_1000
eCC = rawSLEvaluator(s,consts,indets,raw mutableMatrix{{3_R}})
output = mutableMatrix(R,1,4)
rawSLEvaluatorEvaluate(eCC, raw mutableMatrix{{7_R}}, raw output) 

assert (abs(last flatten entries output - 37/3) < 2^(-999))
