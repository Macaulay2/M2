-- Copyright 1994 by Daniel R. Grayson

Tally.name = "Tally"
toString Tally := x -> (
     "new Tally from {"
     | demark(", ", apply(pairs x, (v,i) -> toString v | " => " | toString i))
     | "}"
     )

net Tally := t -> peek t

Tally _ Thing := (a,b) -> if a#?b then a#b else 0

Tally ** Tally := (x,y) -> combine(x,y,identity,times,)

Tally ? Tally := (x,y) -> (
     w := values (x-y);
     if #w === 0 then quote ==
     else if all(w,i -> i>0) then quote >
     else if all(w,i -> i<0) then quote <
     else incomparable)

Tally + Tally := (x,y) -> merge(x,y,plus)

singleton := tally {0}

Tally - Tally := (x,y) -> select(merge(x,applyPairs(y,(k,v)->(k,-v)),plus),i -> i =!= 0)
     
sum(Tally) := (w) -> sum(pairs w, (k,v) -> v * k)
product(Tally) := (w) -> product(pairs w, (k,v) -> k^v)

Set.name = "Set"

new Set from List := (X,x) -> set x

set Set := x -> x
net Set := x -> net class x | " " | net keys x
toString Set := x -> (
     -- unpleasant hack
     if class x === Set
     then "set " | toString keys x
     else "new " | toString class x | " from " | toString keys x
     )
Set + Set := (x,y) -> merge(x,y,(i,j)->i)
Set ++ Set := (x,y) -> applyKeys(x,i->(0,i)) + applyKeys(y,j->(1,j))
Set ** Set := (x,y) -> combine(x,y,identity,(i,j)->i,)
special := quote special
Set * Set := (x,y) -> (
     if # x < # y 
     then set select(keys x, k -> y#?k)
     else set select(keys y, k -> x#?k)
     )
Set - Set := (x,y) -> applyPairs(x, (i,v) -> if not y#?i then (i,v))
sum Set := s -> sum toList s
product Set := s -> product toList s
unique = x -> keys set x

member(Thing,Set) := (a,s) -> s#?a

isSubset(Set,Set) := (S,T) -> all(S, (k,v) -> T#?k)

isSubset(Sequence,Set) := isSubset(List,Set) := (S,T) -> all(S, x -> T#?x)
isSubset(Sequence,List) := isSubset(List,List) := 
isSubset(Sequence,Sequence) := isSubset(List,Sequence) := (S,T) -> isSubset(S,set T)
isSubset(Set,List) := isSubset(Set,Sequence) := (S,T) -> isSubset(S,set T)
