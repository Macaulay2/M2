--		Copyright 1993-1999 by Daniel R. Grayson

IndexedVariableTable = new Type of MutableHashTable
IndexedVariableTable.synonym = "indexed variable table"
expression IndexedVariableTable := x -> hold x.Symbol
precedence IndexedVariableTable := x -> 70
baseName IndexedVariableTable := x -> (
     if x.?name then x.name
     else error "IndexedVarableTable with no name"
     )

IndexedVariable = new Type of BasicList
IndexedVariable.synonym = "indexed variable"
toString IndexedVariable := v -> (
     x := v#0;
     i := v#1;
     concatenate(toString x.Symbol,"_",toString i)
     )
net IndexedVariable := v -> (
     x := v#0;
     i := v#1;
     net new Subscript from { x.name, i}
     )
vars IndexedVariable := x -> {x}
IndexedVariable ? IndexedVariable := (x,y) -> (
     if x#0 === y#0 then y#1 ? x#1
     else x#0 ? y#0)
Symbol ? IndexedVariable := (x,y) -> (
     if x === (y#0).name then symbol > else x ? (y#0).name
     )

IndexedVariableTable _ Thing := IndexedVariable => (x,i) -> (
     if x#?i then x#i
     else new IndexedVariable from {x,i}
     )
Symbol _ Thing := IndexedVariable => (v,i) -> (
     if class value v === IndexedVariableTable
     then new IndexedVariable from {value v,i}
     else (
     	  v <- x := new IndexedVariableTable from {
	       symbol name => toString v,
	       symbol Symbol => v
	       };
	  if not ReverseDictionary#?x then ReverseDictionary#x = v;
     	  new IndexedVariable from {x,i}))

Sequence .. Sequence := Sequence => (v,w) -> (
     n := #v;
     if n =!= #w then error "expected sequences of equal length";
     if n === 0 
     then singleton v
     else if n === 1 
     then apply(first v .. first w, singleton)
     else splice table(first v .. first w, drop(v,1) .. drop(w,1), prepend))

List .. List := Sequence => (v,w) -> apply(toSequence v .. toSequence w, toList)

IndexedVariable .. IndexedVariable := Sequence => (v,w) -> (
     x := v#0;
     if x =!= w#0 
     then error("unmatching base names in ", toString v#0,"_",toString v#1, " .. ", toString w#0,"_",toString w#1);
     toSequence apply(v#1 .. w#1, s -> x_s))	  

expression IndexedVariable := x -> new Subscript from { expression x#0, expression x#1 }

baseName IndexedVariable := identity

assign(IndexedVariable,Thing) := (x,val) -> (x#0)#(x#1) = val
assign(Symbol,Thing) := (x,val) -> x <- val



-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
