--		Copyright 1993-1999 by Daniel R. Grayson

-- sequences of variables

Sequence .. Sequence := Sequence => (v,w) -> (
     n := #v;
     if n =!= #w then error "expected sequences of equal length";
     if n === 0 
     then 1 : v
     else if n === 1 
     then apply(first v .. first w, t -> 1:t)
     else splice table(first v .. first w, drop(v,1) .. drop(w,1), prepend))

List .. List := Sequence => (v,w) -> apply(toSequence v .. toSequence w, toList)

-- indexed variables

IndexedVariable = new Type of BasicList
IndexedVariable.synonym = "indexed variable"
value' IndexedVariable := value				    -- do we really want this?
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
     	  if value X =!= X then stderr << "--warning: clearing value of symbol " << X << " to allow access to subscripted variables based on it" << endl << flush;
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
     if not x#?symbol$ then x#symbol$ = X;
     )
IndexedVariableTable.GlobalReleaseHook = (X,x) -> (
     globalReleaseFunction(X,x);
     if x#?symbol$ and x#symbol$ === X then remove(x,symbol$);
     )
Ring _ IndexedVariable := (x,s) -> x.indexSymbols#s
expression IndexedVariable := x -> (expression x#0) _ (expression x#1)
net IndexedVariable := v -> net expression v
toString IndexedVariable := v -> toString expression v
expression IndexedVariableTable := x -> hold x
net IndexedVariableTable :=
toString IndexedVariableTable := x -> if x#?symbol$ then toString x#symbol$ else "{*an indexed variable table*}"
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

IndexedVariable .. IndexedVariable := Sequence => (v,w) -> (
     if v#0 =!= w#0 then error("unmatching base names in ", toString v, " .. ", toString w);
     apply(v#1 .. w#1, s -> v#0_s))	  

baseName IndexedVariable := identity
baseName IndexedVariableTable := x -> if x#?symbol$ then x#symbol$ else error "indexed variable table no associated to a symbol"
baseName Subscript := x -> new IndexedVariable from { baseName x#0 , x#1 }
baseName Holder := x -> baseName x#0

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
