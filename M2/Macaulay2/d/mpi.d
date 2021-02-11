use util;
use struct;

header "
extern int MPInumberOfProcesses();
";
export numberOfProcesses(e:Expr):Expr := toExpr(Ccode(int, "MPInumberOfProcesses()"));
setupfun("numberOfProcesses",numberOfProcesses);
