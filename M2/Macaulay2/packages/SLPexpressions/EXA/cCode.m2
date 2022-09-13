restart
debug needsPackage "NumericalAlgebraicGeometry"
NumericalAlgebraicGeometry#"exported symbols"
R = CC[x,y,z]
F = for i to 2 list random(2,R)-1 
(G,solsG) = totalDegreeStartSystem F
H = segmentHomotopy(F,G)
keys H

inputs = H#"X" | matrix{{H#"T"}}
numColumns inputs
printAsSLP H#"H"

cCode (H#"H", inputs)
cCode (H#"Ht", inputs)
cCode (H#"Hx", inputs)
cCode (H#"Hx"|H#"Ht", inputs)

----------------------------------
-- makeCompiledSLProgram
restart
errorDepth = 0
needsPackage "SLPexpressions"
declareVariable X; declareVariable C;
XpC = X+C
XXC = productGate{X,X,C}
XoC = X/C
slp = makeCompiledSLProgram(matrix{{C,X}},matrix{{XXC,XoC,XpC+2}})
inp = mutableMatrix{{1.2,-1}}
out = mutableMatrix(ring inp,1,3)
debug SLPexpressions
compiledE = rawCompiledSLEvaluatorK(slp,ring inp); -- SIGSEGV when prints
evaluate(slp,inp,out)
a
out
