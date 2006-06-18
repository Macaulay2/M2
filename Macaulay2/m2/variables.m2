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

expression IndexedVariable := x -> new Subscript from { expression x#0, expression x#1 }
net IndexedVariable := v -> net expression v
toString IndexedVariable := v -> toString expression v
IndexedVariable ? IndexedVariable := (x,y) -> toSequence x ? toSequence y
Symbol ? IndexedVariable := (x,y) -> if x === (y#0) then symbol > else x ? (y#0)

valueTables = new MutableHashTable
valueTable = x -> if valueTables#?x then valueTables#x else valueTables#x = new MutableHashTable
methodTable = new MutableHashTable
Symbol _ Thing := (x,i) -> ( 
     t := valueTable x; 
     if t#?i then t#i 
     else if methodTable#?x then methodTable#x(x,i) else new IndexedVariable from {x,i} )
value IndexedVariable := v -> (
     x := v#0;
     i := v#1;
     t := valueTable x;
     if t#?i then t#i else v)

installMethod(symbol <-, IndexedVariable, 
     (xi,y) -> (
	  (valueTable xi#0)#(xi#1) = y;
	  xi#0 <- xi#0;
	  y))
installAssignmentMethod(symbol "_",Symbol,Thing, (sym,i,val) -> (valueTable sym)#i=val)

IndexedVariable .. IndexedVariable := Sequence => (v,w) -> (
     if v#0 =!= w#0 then error("unmatching base names in ", toString v, " .. ", toString w);
     apply(v#1 .. w#1, s -> v#0_s))	  

baseName IndexedVariable := identity
baseName Subscript := x -> new IndexedVariable from { baseName x#0 , x#1 }
baseName Holder := x -> baseName x#0

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
