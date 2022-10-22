----------------------------------
-- makeCompiledSLProgram
restart
errorDepth = 0
needsPackage "SLPexpressions"
declareVariable X; declareVariable C;
XpC = X+C
XXC = productGate{X,X,C}
XoC = X/C
slpI = makeInterpretedSLProgram(matrix{{C,X}},matrix{{XXC,XoC,XpC+2}})
slpC = makeCompiledSLProgram(matrix{{C,X}},matrix{{XXC,XoC,XpC+2}})
inpRR = mutableMatrix{{1.2,-1}}
inpCC = mutableMatrix{{1.2,-ii}}

compareItoC = (slpI,slpC,inp) -> (
    out := mutableMatrix(ring inp,1,slpI#"number of outputs");
    evaluate(slpI,inp,out);
    resI := out;
    evaluate(slpC,inp,out);
    resC := out;
    print(resI,resC);
    assert(resI - resC == 0)
    )

compareItoC (slpI,slpC,inpCC)
compareItoC (slpI,slpC,inpRR)

-* -- test rawSLEvaluatorK
debug SLPexpressions
interpretedE = rawSLEvaluatorK(slp',ring inp)
compiledE = rawSLEvaluatorK(slp,ring inp)
*-

restart
needsPackage "MonodromySolver"

setRandomSeed 0;
declareVariable \ {A,B,C,D,X,Y};
F = gateSystem(matrix{{A,B,C,D}},matrix{{X,Y}},matrix{{A*(X-1)^2-B}, {C*(Y+2)^2+D}});
p0 = point{{1,1,1,1}};
sols = solveFamily(p0, F, NumberOfNodes=>3);
apply(#sols, i->norm(evaluate(F, p0, sols#i)))

