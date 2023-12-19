use parse;
use expr;
use common;

toExpr(x:atomicInteger):Expr := Expr(atomicIntegerCell(x));

newAtomicInteger(e:Expr):Expr := (
    when e
    is n:ZZcell do (
	if isInt(n)
	then (
	    r:atomicInteger := Ccode(atomicInteger, toInt(n));
	    toExpr(r))
	else WrongArgSmallInteger())
    else WrongArgZZ());
setupfun("atomicInteger", newAtomicInteger);

atomicFetchAdd(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2 then (
	    when a.0
	    is x:atomicIntegerCell do (
		when a.1
		is y:ZZcell do (
		    if isInt(y) then (
			Ccode(void,
			    "atomic_fetch_add(&", x.v, ", ", toInt(y), ")");
			toExpr(x.v))
		    else WrongArgSmallInteger(2))
		else WrongArgZZ(2))
	    else WrongArg(1, "an atomic integer"))
	else WrongNumArgs(2))
    else WrongNumArgs(2));
setupfun("atomicFetchAdd", atomicFetchAdd);
