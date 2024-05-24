needs "classes.m2"
needs "methods.m2"

-- originally defined (as null) in evaluate.d
iterator = method(Dispatch => Thing)
next = method()

Iterator = new SelfInitializingType of FunctionClosure
Iterator.synonym = "iterator"

iterator Iterator := identity
next Iterator := iter -> iter()

expression Iterator := iter -> if hasAttribute(iter,ReverseDictionary) then expression getAttribute(iter,ReverseDictionary) else (
    x := if not (first frames iter)#?0 then () else first first frames iter;
    FunctionApplication{iterator,
	(if instance(x, String) then format else expression) x}
    )
net Iterator := net @@ expression
toString Iterator := toString @@ expression

iterator VisibleList :=
iterator String      := x -> Iterator (
    i := 0;
    () -> (
	if i >= #x then StopIteration
	else (
	    r := x#i;
	    i += 1;
	    r)))

toList Thing := x -> for y in x list y

-- called by map(Expr,Expr) in actors3.d
applyIterator = (iter, f) -> Iterator (
    () -> (
	x := next iter;
	if x === StopIteration then StopIteration
	else f x))

select(Thing, Function) := Iterator => {} >> o -> (X, f) -> (
    if lookup(iterator, class X) === null
    then error "expected argument 1 to be an iterable object";
    iter := iterator X;
    Iterator (
	() -> while true do (
	    x := next iter;
	    if x === StopIteration then return StopIteration;
	    y := f x;
	    if not instance(y, Boolean)
	    then error("select: expected predicate to yield true or false");
	    if y then return x)))

joinIterators = a -> (
    n := #a;
    iters := iterator \ a;
    i := 0;
    Iterator(
	() -> (
	    if i >= n then StopIteration
	    else (
		while (
		    r := next iters#i;
		    r === StopIteration)
		do (
		    i += 1;
		    if i >= n then return StopIteration);
		r))))

Iterator | Iterator := (x, y) -> joinIterators(x, y)
