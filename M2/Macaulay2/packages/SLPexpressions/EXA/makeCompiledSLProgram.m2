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
needsPackage "NumericalAlgebraicGeometry"
setTryJustInTimeCompilation true
setRandomSeed 0;
for i to 9 do (
    declareVariable \ {a_i,X_i,Y_i};
    F = gateSystem(matrix{{a_i}},matrix{{X_i,Y_i}},matrix{{a_i*(X_i-1)^2-1}, {(Y_i+2)^2-a_i}});
    PH = parametricSegmentHomotopy F;
    AB = mutableMatrix{{1},{2+ii}};
    GH_i = specialize(PH, AB);
    for j to i do ( 
    	sol := first trackHomotopy(GH_j,{point{{2,-1_CC}}});
    	assert(norm evaluateH(GH_i,transpose matrix sol,1) < 0.0001);
    	)
    )

restart
-- system linear in parameters
needsPackage "MonodromySolver"
setTryJustInTimeCompilation true
setRandomSeed 0;
declareVariable \ {A,B,C,D,X,Y};
F = gateSystem(matrix{{A,B,C,D}},matrix{{X,Y}},matrix{{A*(X-1)^2-B}, {C*(Y+2)^2+D}});
p0 = point{{1,1,1,1}};
-*
sols = solveFamily(p0, F, NumberOfNodes=>2);
apply(length sols, i->norm(evaluate(F, p0, sols#i)))
*-
monodromySolve(F, NumberOfNodes=>2);

-- system NONlinear in parameters
restart
debug needsPackage "MonodromySolver"
setTryJustInTimeCompilation true
setRandomSeed 0;
declareVariable \ {A,B,C,D,X,Y};
F = gateSystem(matrix{{A,B,C,D}},matrix{{X,Y}},matrix{{A*(X-1)^2-B^2}, {C*(Y+2)^2+D}});
errorDepth = 0 
(p0,x0) = newtonHomotopy F
(Hnode,nPaths) = monodromySolve(F, p0, {x0}, NumberOfNodes=>4);
sols = points Hnode.PartialSols
apply(length sols, i->norm(evaluate(F, Hnode.BasePoint, sols#i)))
