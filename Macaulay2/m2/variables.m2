--		Copyright 1994 by Daniel R. Grayson

IndexedVariableTable = new Type of MutableHashTable
expression IndexedVariableTable := hold
precedence IndexedVariableTable := x -> 70
baseName IndexedVariableTable := x -> (
     if x.?name then x.name
     else error "IndexedVarableTable with no name"
     )
document { quote IndexedVariableTable,
     TT "IndexedVariableTable", " -- the class of those hash tables which
     are used to hold the values of those indexed variables sharing a given
     base name.",
     PARA,
     EXAMPLE "t_0",
     EXAMPLE "scan(3, i -> t#i = i^2)",
     EXAMPLE "t",
     EXAMPLE "peek t",
     SEEALSO "IndexedVariable"
     }

IndexedVariable = new Type of BasicList
name IndexedVariable := x -> concatenate(name x#0,"_",name x#1)
expression IndexedVariable := x -> new Subscript from {
     expression x#0,expression x#1
     }
net IndexedVariable := x -> net new Subscript from {x#0,x#1}
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
     if class value v === IndexedVariableTable and (value v).name === v
     then (value v)_i
     else (
     	  v <- x := new IndexedVariableTable;
     	  x.name = v;
     	  new IndexedVariable from {x,i}
	  )
     )

Sequence .. Sequence := (v,w) -> (
     n := #v;
     if n =!= #w then error "expected sequences of equal length";
     if n === 0 
     then seq v
     else if n === 1 
     then apply(first v .. first w, seq)
     else splice table(first v .. first w, drop(v,1) .. drop(w,1), prepend))

TEST "
assert (
     (0,0)..(2,3) == 
     ((0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3))
     )
"

List .. List := (v,w) -> apply(toSequence v .. toSequence w, toList)

IndexedVariable .. IndexedVariable := (v,w) -> (
     x := v#0;
     if x =!= w#0 
     then error ("unmatching base names in ",
	  name v#0,"_",name v#1,
	  " .. ",
	  name w#0,"_",name w#1);
     toSequence apply(v#1 .. w#1, s -> x_s))	  

expression IndexedVariable := (
     x -> new Subscript from { expression x#0, expression x#1 }
     )

baseName IndexedVariable := identity

assign(IndexedVariable,Thing) := (x,val) -> (x#0)#(x#1) = val
assign(Symbol,Thing) := (x,val) -> x <- val

document { quote assign,
     TT "assign(x,v)", " -- assigns v as the value of x.",
     PARA,
     "If the value of x is a symbol or indexed variable, then it
     can be assigned the value v with ",
     PRE "          assign(x,v)",
     "When the value of x is an indexed variable y_i then what happens
     is that the i-th element of the list y is replaced by v.",
     PARA,
     "Differs from x=v in that here x is evaluated.",
     PARA,
     "Note: it would be better if we could arrange for ",
     PRE "          x <- v",
     "to work with indexed variables.  See ", TO "<-", "."
     }

document { quote IndexedVariable,
     TT "IndexedVariable", " -- the class of all indexed variables.",
     PARA,
     "Indexed variables provide the possibility of producing 
     polynomial rings ", TT "R[x_0, x_1, ..., x_(n-1)]", " in n variables,
     where n is not known in advance.  If ", TT "x", " is an symbol,
     and i is an integer, then ", TT "x_i", " produces an indexed variable.
     (What actually happens is a hash table been assigned to the
     as the value of the symbol ", TT "x", ".
     After this has been done, an assignment ", TT "x#i=v", " will assign a 
     value to it.  A new sequence of indexed variables of
     length n assigned to the symbol ", TT "x", " can be produced with ",
     TT "x_1 .. x_n", " and that sequence can be used in constructing
     a polynomial ring.",
     EXAMPLE "ZZ/101[t_0 .. t_4]",
     EXAMPLE "(t_0 -  2*t_1)^3",
     SEEALSO "IndexedVariableTable"
     }

