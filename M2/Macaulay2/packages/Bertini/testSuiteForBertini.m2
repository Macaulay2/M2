help   "bertiniZeroDimSolve"--check
help  "bertiniParameterHomotopy",---example in summer Demo just need todocument
help  "bertiniPosDimSolve" --check
help  "bertiniSample"--check
help  "bertiniTrackHomotopy"
--help  "bertiniSolve"
help   "bertiniComponentMemberTest"--something seems off
help  "bertiniRefineSols"--check




----refined solutions
R=QQ[x,y]
f1=x^3-1
f2=y^3-1
L={f1, f2}
pts=bertiniZeroDimSolve(L)
rdf="/Users/MacMath/Desktop/tmp/M2-20172-0/5/raw_data" 
bertiniRefineSols(L,RawData=>rdf,StartSolutions=>{{1,
-.5+.866025*ii}},digits=>100)




