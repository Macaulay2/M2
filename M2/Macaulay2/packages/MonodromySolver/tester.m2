uninstallPackage "MonodromySolver"
restart
installPackage("MonodromySolver",FileName=>"~/M2fork/M2/M2/Macaulay2/packages/MonodromySolver.M2",
    RerunExamples=>true,RunExamples=>true,
    RemakeAllDocumentation=>true,MakeDocumentation=>true,
    IgnoreExampleErrors=>true)
help seedTest

viewHelp MonodromySolver

check MonodromySolver
