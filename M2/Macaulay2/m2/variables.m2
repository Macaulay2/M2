--		Copyright 1993-1999 by Daniel R. Grayson

needs "expressions.m2"
needs "methods.m2"

-- indexed variables

IndexedVariable = new Type of BasicList
IndexedVariable.synonym = "indexed variable"
expressionValue IndexedVariable := value				    -- do we really want this?
IndexedVariableTable = new Type of MutableHashTable
IndexedVariableTable.synonym = "indexed variable table"
protect symbol$						    -- not exported, to avoid interference with the user
new IndexedVariableTable from Symbol := (IndexedVariableTable,X) -> (
     x := new IndexedVariableTable;
     x#symbol$ = X;
     x)
checkValue = x -> if x#?symbol$ then (
     X := x#symbol$;
     if value X =!= x then (
     	  if value X =!= X then warningMessage("clearing value of symbol ", toString X, " to allow access to subscripted variables based on it");
     	  X <- x;
	  )
     )
IndexedVariableTable _ Thing := (x,i) -> (
     if x#?i then x#i
     else if x#?symbol$ then new IndexedVariable from {x#symbol$,i}
     else error "attempted to make new indexed variable from indexed variable table associated with no symbol")
IndexedVariableTable _ Thing  = (x,i,e) -> (checkValue x; x#i = e)
IndexedVariableTable.GlobalAssignHook = (X,x) -> (
     globalAssignFunction(X,x);
     x#symbol$ ??= X;
     )
IndexedVariableTable.GlobalReleaseHook = (X,x) -> (
     globalReleaseFunction(X,x);
     if x#?symbol$ and x#symbol$ === X then remove(x,symbol$);
     )
expression IndexedVariable := x -> (expression x#0) _ (expression x#1)
net IndexedVariable := v -> net expression v
toString IndexedVariable := v -> toString expression v
texMath IndexedVariable := v -> texMath expression v
expression IndexedVariableTable := x -> if x#?symbol$ then expression x#symbol$ else expression "-*an indexed variable table*-"
net IndexedVariableTable := net @@ expression
toString IndexedVariableTable := toString @@ expression
texMath IndexedVariableTable := x -> texMath expression x

IndexedVariable ? IndexedVariable := (x,y) -> toSequence x ? toSequence y
Symbol ? IndexedVariable := (x,y) -> if x === (y#0) then symbol > else x ? (y#0)
Symbol _ Thing := (X,i) -> new IndexedVariable from {X,i}
value IndexedVariableTable := x -> x
value IndexedVariable := v -> (
     (x,i) := toSequence v;
     if not instance(x,Symbol) then return v;		    -- an error
     x' := value x;
     if x' === x or not instance(x',IndexedVariableTable) then return v;
     if x'#?i then x'#i else v)
Symbol _ Thing = (x,i,e) -> (
     x' := value x;
     if not instance(x',IndexedVariableTable) then x' = new IndexedVariableTable from x;
     x'_i = e)
installMethod(symbol <-, IndexedVariable, (xi,e) -> ((x,i) -> x_i = e) toSequence xi)

installMethod(symbol <-, Sequence, (x,y) -> (
	  if not instance(y,Sequence) then error "expected a sequence of values";
	  if #x =!= #y then error ("expected ", toString (#x), " values, but encountered ", toString (#y));
	  scan(x,y,(i,j) -> i <- j);
	  y))

IndexedVariable .. IndexedVariable := Sequence => (v,w) -> apply(toSequence v .. toSequence w, xi -> new IndexedVariable from xi)
IndexedVariable ..< IndexedVariable := Sequence => (v,w) -> apply(toSequence v ..< toSequence w, xi -> new IndexedVariable from xi)

-- baseName
baseName Thing := R -> (
    if not hasAttribute(R, ReverseDictionary) then error "baseName: no base name available";
    x := getAttribute(R, ReverseDictionary);
    if isMutable x then x else error("baseName: base name ", toString x,
	" is not mutable, hence not available for use as a variable"))
baseName Symbol :=
baseName IndexedVariable := identity
baseName IndexedVariableTable := x -> if x#?symbol$ then x#symbol$ else error "indexed variable table not associated to a symbol"
baseName Subscript := x -> new IndexedVariable from { baseName x#0 , x#1 }
baseName Holder := x -> baseName x#0

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
