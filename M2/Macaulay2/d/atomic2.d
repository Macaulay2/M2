use atomic;
use parse;
use expr;
use common;
use hashtables;

header "#include \"M2mem.h\"";

toExpr(x:atomicField):Expr := (
    y := atomicIntCell(x, hash_t(0));
    y.hash = hashFromAddress(Expr(y));
    Expr(y));
WrongArgAtomicInt(n:int):Expr := WrongArg(n, "an atomic integer");

atomicInit(n:int):Expr := (
    ptr := Ccode(voidPointer, "getmem_atomic(sizeof(struct atomic_field))");
    x := Ccode(atomicField, "*(struct atomic_field *)", ptr);
    Ccode(void, "atomic_init(&(", x, ").field, ", n, ")");
    toExpr(x));

atomicInit(e:Expr):Expr := (
    when e
    is x:HashTable do atomicInit(0)
    is a:Sequence do (
	if length(a) == 2
	then (
	    when a.0
	    is HashTable do (
		when a.1
		is x:ZZcell do (
		    if isInt(x) then atomicInit(toInt(x))
		    else WrongArgSmallInteger(2))
		else WrongArgZZ(2))
	    else WrongArg(1, "a hash table"))
	else WrongNumArgs(1, 2))
    else WrongNumArgs(1, 2));
installMethod(NewS, atomicIntClass, atomicInit);
installMethod(NewFromS, atomicIntClass, ZZClass, atomicInit);

atomicStore(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2
	then (
	    when a.0
	    is x:atomicIntCell do (
		when a.1
		is y:ZZcell do (
		    if isInt(y) then (
			store(x.v, toInt(y));
			nullE)
		    else WrongArgSmallInteger(2))
		else WrongArgZZ(2))
	    else WrongArgAtomicInt(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));
setupfun("atomicStore", atomicStore);

atomicLoad(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2
	then (
	    when a.0
	    is HashTable do (
		when a.1
		is x:atomicIntCell do toExpr(load(x.v))
		else WrongArgAtomicInt(2))
	    else WrongArgHashTable(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));
installMethod(NewFromS, ZZClass, atomicIntClass, atomicLoad);

atomicFetchAdd(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2
	then (
	    when a.0
	    is x:atomicIntCell do (
		when a.1
		is y:ZZcell do (
		    if isInt(y)
		    then toExpr(Ccode(int, "atomic_fetch_add(&(", x.v,
			    ").field, ", toInt(y), ")"))
		    else WrongArgSmallInteger(2))
		else WrongArgZZ(2))
	    else WrongArgAtomicInt(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));

atomicFetchSub(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2
	then (
	    when a.0
	    is x:atomicIntCell do (
		when a.1
		is y:ZZcell do (
		    if isInt(y)
		    then toExpr(Ccode(int, "atomic_fetch_sub(&(", x.v,
			    ").field, ", toInt(y), ")"))
		    else WrongArgSmallInteger(2))
		else WrongArgZZ(2))
	    else WrongArgAtomicInt(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));

atomicFetchOr(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2
	then (
	    when a.0
	    is x:atomicIntCell do (
		when a.1
		is y:ZZcell do (
		    if isInt(y)
		    then toExpr(Ccode(int, "atomic_fetch_or(&(", x.v,
			    ").field, ", toInt(y), ")"))
		    else WrongArgSmallInteger(2))
		else WrongArgZZ(2))
	    else WrongArgAtomicInt(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));

atomicFetchXor(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2
	then (
	    when a.0
	    is x:atomicIntCell do (
		when a.1
		is y:ZZcell do (
		    if isInt(y)
		    then toExpr(Ccode(int, "atomic_fetch_xor(&(", x.v,
			    ").field, ", toInt(y), ")"))
		    else WrongArgSmallInteger(2))
		else WrongArgZZ(2))
	    else WrongArgAtomicInt(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));

atomicFetchAnd(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2
	then (
	    when a.0
	    is x:atomicIntCell do (
		when a.1
		is y:ZZcell do (
		    if isInt(y)
		    then toExpr(Ccode(int, "atomic_fetch_and(&(", x.v,
			    ").field, ", toInt(y), ")"))
		    else WrongArgSmallInteger(2))
		else WrongArgZZ(2))
	    else WrongArgAtomicInt(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));

foreach oper in augmentedAssignmentOperators do (
    if oper.symbol.word.name === "+="
    then installMethod(oper, atomicIntClass, atomicFetchAdd);
    if oper.symbol.word.name === "-="
    then installMethod(oper, atomicIntClass, atomicFetchSub);
    if oper.symbol.word.name === "|="
    then installMethod(oper, atomicIntClass, atomicFetchOr);
    if oper.symbol.word.name === "^^="
    then installMethod(oper, atomicIntClass, atomicFetchXor);
    if oper.symbol.word.name === "&="
    then installMethod(oper, atomicIntClass, atomicFetchAnd));

atomicExchange(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2
	then (
	    when a.0
	    is x:atomicIntCell do (
		when a.1
		is y:ZZcell do (
		    if isInt(y)
		    then toExpr(Ccode(int, "atomic_exchange(&(", x.v,
			    ").field, ", toInt(y), ")"))
		    else WrongArgSmallInteger(2))
		else WrongArgZZ(2))
	    else WrongArgAtomicInt(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));
setupfun("atomicExchange", atomicExchange);

atomicCompareExchange(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 3
	then (
	    when a.0
	    is x:atomicIntCell do (
		when a.1
		is y:ZZcell do (
		    if isInt(y) then (
			when a.2
			is z:ZZcell do (
			    if isInt(z)
			    then (
				yint := toInt(y);
				toExpr(Ccode(bool,
					"atomic_compare_exchange_strong(&(",
					x.v, ").field, &", yint, ", ",
					toInt(z), ")")))
			    else WrongArgSmallInteger(3))
			else WrongArgZZ(3))
		    else WrongArgSmallInteger(2))
		else WrongArgZZ(2))
	    else WrongArgAtomicInt(1))
	else WrongNumArgs(3))
    else WrongNumArgs(3));
setupfun("atomicCompareExchange", atomicCompareExchange);
