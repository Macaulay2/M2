--		Copyright 1993-1999 by Daniel R. Grayson

valueTables = new MutableHashTable
getValueTable = x -> if valueTables#?x then valueTables#x else valueTables#x = new MutableHashTable
installAssignmentMethod(symbol "_",Symbol,Thing, (sym,i,val) -> (getValueTable sym)#i=val)

IndexedVariable = new Type of BasicList
IndexedVariable.synonym = "indexed variable"
Symbol _ Thing := (x,i) -> if valueTables#?x and valueTables#x#?i then valueTables#x#i else new IndexedVariable from {x,i}
installMethod(symbol <-, IndexedVariable, (xi,y) -> (getValueTable xi#0)#(xi#1) = y)
net IndexedVariable := v -> net new Subscript from { v#0, v#1}
toString IndexedVariable := v -> concatenate(toString v#0,"_",toString v#1)
vars IndexedVariable := x -> {x}
IndexedVariable ? IndexedVariable := (x,y) -> toSequence x ? toSequence y
Symbol ? IndexedVariable := (x,y) -> if x === (y#0) then symbol > else x ? (y#0)

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
     if v#0 =!= w#0 then error("unmatching base names in ", toString v, " .. ", toString w);
     apply(v#1 .. w#1, s -> v#0_s))	  

expression IndexedVariable := x -> new Subscript from { expression x#0, expression x#1 }
baseName IndexedVariable := identity

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
