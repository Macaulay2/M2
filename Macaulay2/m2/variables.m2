--		Copyright 1994 by Daniel R. Grayson

IndexedVariableTable = new Type of MutableHashTable
expression IndexedVariableTable := x -> hold x.symbol
precedence IndexedVariableTable := x -> 70
baseName IndexedVariableTable := x -> (
     if x.?name then x.name
     else error "IndexedVarableTable with no name"
     )

IndexedVariable = new Type of BasicList
name IndexedVariable := v -> (
     x := v#0;
     i := v#1;
     -- if x#?i then concatenate(name x.symbol,"_",name i) else 
     concatenate(string x.symbol,"_",name i)
     )
net IndexedVariable := v -> (
     x := v#0;
     i := v#1;
     net new Subscript from { expression x, expression i}
     )
vars IndexedVariable := x -> {x}
IndexedVariable ? IndexedVariable := (x,y) -> (
     if x#0 === y#0 then y#1 ? x#1
     else x#0 ? y#0)
Symbol ? IndexedVariable := (x,y) -> (
     if x === (y#0).name then quote > else x ? (y#0).name
     )

IndexedVariableTable _ Thing := (x,i) -> (
     if x#?i then x#i
     else new IndexedVariable from {x,i}
     )
Symbol _ Thing := (v,i) -> (
     if class value v === IndexedVariableTable
     then new IndexedVariable from {value v,i}
     else (
     	  v <- x := new IndexedVariableTable;
     	  x.name = string v;
	  x.symbol = v;
     	  new IndexedVariable from {x,i}
	  )
     )

Sequence .. Sequence := (v,w) -> (
     n := #v;
     if n =!= #w then error "expected sequences of equal length";
     if n === 0 
     then singleton v
     else if n === 1 
     then apply(first v .. first w, singleton)
     else splice table(first v .. first w, drop(v,1) .. drop(w,1), prepend))

List .. List := (v,w) -> apply(toSequence v .. toSequence w, toList)

IndexedVariable .. IndexedVariable := (v,w) -> (
     x := v#0;
     if x =!= w#0 
     then error ("unmatching base names in ",
	  name v#0,"_",name v#1,
	  " .. ",
	  name w#0,"_",name w#1);
     toSequence apply(v#1 .. w#1, s -> x_s))	  

expression IndexedVariable := x -> new Subscript from { expression x#0, expression x#1 }

baseName IndexedVariable := identity

assign(IndexedVariable,Thing) := (x,val) -> (x#0)#(x#1) = val
assign(Symbol,Thing) := (x,val) -> x <- val


