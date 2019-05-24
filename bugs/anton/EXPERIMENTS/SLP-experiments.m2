restart
needsPackage "SLPexpressions"
check SLPexpressions
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


debug Core
(consts,indets):=(s#"constant positions",s#"input positions")
assert(#consts == 0)
(newConsts,newIndets):=(take(indets,1),drop(indets,1))
eQQ = rawSLEvaluator(s#RawSLProgram,newConsts,newIndets,raw mutableMatrix{{3_QQ}}) -- set C=3_QQ
output = mutableMatrix(QQ,1,4)
rawSLEvaluatorEvaluate(eQQ, raw mutableMatrix{{7_QQ}}, raw output) 
output
eCC = rawSLEvaluator(s#RawSLProgram,newConsts,newIndets,raw mutableMatrix{{3_CC}})
output = mutableMatrix(CC,1,4)
rawSLEvaluatorEvaluate(eCC, raw mutableMatrix{{7_CC}}, raw output) 
output
R = CC_1000
eCC = rawSLEvaluator(s#RawSLProgram,newConsts,newIndets,raw mutableMatrix{{3_R}})
rawM = mutableMatrix(R,1,4)
rawSLEvaluatorEvaluate(eCC, raw mutableMatrix{{7_R}}, raw rawM) 
assert (abs(last flatten entries rawM - 37/3) < 2^(-999))


-- homotopy
restart
debug needsPackage "NumericalAlgebraicGeometry"
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T
F = {X*X-1, Y*Y*Y-1}
G = {X*X+Y*Y-1, X*X*X+Y*Y*Y-1}
H = (1 - T) * F + T * G
gV = matrix{{X,Y}}
gH = transpose matrix {H}
HS = gateHomotopy(gH,gV,T)
peek HS#"EHx" -- 5
support HS#"Hx" -- 3
debug SLPexpressions
inp = mutableMatrix {{1.,2.,1+ii}}
out = mutableMatrix(CC_53, 1, 2)
evaluate(HS#"EH", inp, out)
out = mutableMatrix(CC_53, 1, 4)
evaluate(HS#"EHx", inp, out)
out


-*
s = first trackHomotopy(HS,{matrix{{1_CC},{1}}},Software=>M2)
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-4)
*-
s = first trackHomotopy(HS,{matrix{{1_CC},{1}}},Software=>M2engine)
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-4)
