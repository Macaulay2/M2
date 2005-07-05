-- Copyright 1994 by Daniel R. Grayson

toString Tally := x -> (
     "new Tally from {"
     | demark(", ", apply(pairs x, (v,i) -> toString v | " => " | toString i))
     | "}"
     )

net Tally := t -> peek t

Tally _ Thing := (a,b) -> if a#?b then a#b else 0

Tally ** Tally := Tally => (x,y) -> combine(x,y,identity,times,)

Tally ? Tally := (x,y) -> (
     w := values (x-y); -- warning: if x and y are both sets, then x-y isn't as expected here
     if #w === 0 then symbol ==
     else if all(w,i -> i>0) then symbol >
     else if all(w,i -> i<0) then symbol <
     else incomparable)
Set ? Set := (x,y) -> (new Tally from x) ? (new Tally from y)

Tally + Tally := Tally => (x,y) -> merge(x,y,plus)
Tally - Tally := Tally => (x,y) -> select(merge(x,applyValues(y,minus),plus),i -> i =!= 0)
     
sum(Tally) := (w) -> sum(pairs w, (k,v) -> v * k)
product(Tally) := (w) -> product(pairs w, (k,v) -> k^v)

new Set from List := Set => (X,x) -> set x

net Set := x -> net class x | " " | net keys x
toString Set := x -> (
     -- unpleasant hack
     if class x === Set
     then "set " | toString keys x
     else "new " | toString class x | " from " | toString keys x
     )
Set + Set := Set => (x,y) -> merge(x,y,(i,j)->i)
Set ++ Set := Set => (x,y) -> applyKeys(x,i->(0,i)) + applyKeys(y,j->(1,j))
Set ** Set := Set => (x,y) -> combine(x,y,identity,(i,j)->i,)
Set == Set := Boolean => (x,y) -> x === y
special := symbol special
Set * Set := Set => (x,y) -> (
     if # x < # y 
     then set select(keys x, k -> y#?k)
     else set select(keys y, k -> x#?k)
     )
Set - Set := Set => (x,y) -> applyPairs(x, (i,v) -> if not y#?i then (i,v))
List - Set := List => (x,y) -> select(x, i -> not y#?i)
Set - List := Set => (x,y) -> x - set y
sum Set := s -> sum toList s
product Set := s -> product toList s

unique = method(SingleArgumentDispatch => true, TypicalValue => List)
unique Sequence := x -> unique toList x
unique List := x -> (
     -- old faster way: keys set x
     -- new way preserves order:
     seen := new MutableHashTable;
     select(x, i -> if seen#?i then false else seen#i = true))

Thing in Set := member(Thing,Set) := Boolean => (a,s) -> s#?a

isSubset(Set,Set) := Boolean => (S,T) -> all(S, (k,v) -> T#?k)

isSubset(VisibleList,Set) := Boolean => (S,T) -> all(S, x -> T#?x)
isSubset(VisibleList,VisibleList) := Boolean => (S,T) -> isSubset(S,set T)
isSubset(Set,VisibleList) := Boolean => (S,T) -> isSubset(S,set T)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
