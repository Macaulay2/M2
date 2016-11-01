-- before release
uninstallAllPackages()

restart
check "NAGtypes"
installPackage("NAGtypes", RerunExamples=>true)

restart
check "SLPexpressions"
installPackage("SLPexpressions", RerunExamples=>true)

restart
check "NumericalAlgebraicGeometry"
installPackage("NumericalAlgebraicGeometry", RerunExamples=>true)

restart
check "NAGtools"
installPackage("NAGtools", RerunExamples=>true)

restart
check "MonodromySolver"
restart -- does not work without a restart... why?
installPackage("MonodromySolver", RerunExamples=>true)

{* check debugging mode, version, date...

grep -nH -e "DebuggingMode => false" NAGtypes.m2 SLPexpressions.m2 NumericalAlgebraicGeometry.m2 NAGtools.m2 NumericalSchubertCalculus.m2 MonodromySolver.m2

grep -nH -e "Version" NAGtypes.m2 SLPexpressions.m2 NumericalAlgebraicGeometry.m2 NAGtools.m2 NumericalSchubertCalculus.m2 MonodromySolver.m2

grep -nH -e "Date" NAGtypes.m2 SLPexpressions.m2 NumericalAlgebraicGeometry.m2 NAGtools.m2 NumericalSchubertCalculus.m2 MonodromySolver.m2

*}

{* undocumented 

grep "no documentation" undocumented.txt | awk 'NF>1{print $NF ","}'

*}