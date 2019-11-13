needsPackage "SLPexpressions"
help cCode

uninstallPackage "SLPexpressions"
restart
installPackage("SLPexpressions",FileName=>"../SLPexpressions.m2",
    RerunExamples=>true,RunExamples=>true,
    RemakeAllDocumentation=>true,MakeDocumentation=>true,
    IgnoreExampleErrors=>true)


X = inputGate symbol X
C = inputGate symbol C
XpC = X+C+2
XXC = productGate{X,X,C}
detXCCX = detGate{{X,C},{C,X}}
XoC = X/C
cCode (matrix{{XXC,detXCCX,0},{XoC,1,2}},matrix{{X,C}})


uninstallPackage "MonodromySolver"
restart
installPackage("MonodromySolver",FileName=>"../MonodromySolver.m2",
    RerunExamples=>true,RunExamples=>true,
    RemakeAllDocumentation=>true,MakeDocumentation=>true,
    IgnoreExampleErrors=>true)
help seedTest

viewHelp MonodromySolver

check MonodromySolver
