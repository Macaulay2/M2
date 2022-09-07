restart
loadPackage "SubalgebraBases";
R = QQ[x_1..x_3];
f = x_1^2 + x_2^2 + x_3^2;
A = subring({x_1+x_2+x_3, x_1*x_2 + x_1*x_3 + x_2*x_3, x_1*x_2*x_3}, GeneratorSymbol => s)
f // A -- SAGBI version?
check "SubalgebraBases"
restart
loadPackage("SubalgebraBases",FileName=>"./SubalgebraBases.m2",DebuggingMode=>true)
check SubalgebraBases
check "SubalgebraBases"
check(1, "SubalgebraBases")
subductionQuotient(f, A)
peek A.cache
A = subring {x_1+x_2+x_3, x_1*x_2 + x_1*x_3 + x_2*x_3, x_1*x_2*x_3}

sagbi A
f % A
methods symbol //
(f // A, f % A)
