-- Copyright 1994 by Daniel R. Grayson

Tally.name = "Tally"
toString Tally := x -> (
     "new Tally from {"
     | demark(", ", apply(pairs x, (v,i) -> toString v | " => " | toString i))
     | "}"
     )

net Tally := t -> peek t

Tally _ Thing := (a,b) -> if a#?b then a#b else 0

Tally ** Tally := Tally => (x,y) -> combine(x,y,identity,times,)

Tally ? Tally := (x,y) -> (
     w := values (x-y);
     if #w === 0 then symbol ==
     else if all(w,i -> i>0) then symbol >
     else if all(w,i -> i<0) then symbol <
     else incomparable)

Tally + Tally := Tally => (x,y) -> merge(x,y,plus)

singleton := tally {0}

Tally - Tally := Tally => (x,y) -> select(merge(x,apply(y,minus),plus),i -> i =!= 0)
     
sum(Tally) := (w) -> sum(pairs w, (k,v) -> v * k)
product(Tally) := (w) -> product(pairs w, (k,v) -> k^v)

Set.name = "Set"

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
special := symbol special
Set * Set := Set => (x,y) -> (
     if # x < # y 
     then set select(keys x, k -> y#?k)
     else set select(keys y, k -> x#?k)
     )
Set - Set := Set => (x,y) -> applyPairs(x, (i,v) -> if not y#?i then (i,v))
sum Set := s -> sum toList s
product Set := s -> product toList s

unique = x -> keys set x
typicalValues#unique = List

member(Thing,Set) := Boolean => (a,s) -> s#?a

isSubset(Set,Set) := Boolean => (S,T) -> all(S, (k,v) -> T#?k)

isSubset(Sequence,Set) :=
isSubset(List    ,Set) := Boolean => (S,T) -> all(S, x -> T#?x)

isSubset(Sequence,List    ) := 
isSubset(List    ,List    ) := 
isSubset(Sequence,Sequence) := 
isSubset(List    ,Sequence) := Boolean => (S,T) -> isSubset(S,set T)

isSubset(Set,List    ) := 
isSubset(Set,Sequence) := Boolean => (S,T) -> isSubset(S,set T)
