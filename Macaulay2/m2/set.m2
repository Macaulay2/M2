-- Copyright 1994 by Daniel R. Grayson

Set.synonym = "set"
Tally.synonym = "tally"

elements = method()
elements Set := x -> keys x
elements Tally := x -> splice apply(pairs x, (k,v) -> v:k)

toString Tally := x -> concatenate( "new ", toString class x, " from {", demark(", ", apply(pairs x, (v,i) -> (toString v, " => ", toString i))), "}" )

net Tally := t -> peek t

Tally _ Thing := (a,b) -> if a#?b then a#b else 0

Tally ** Tally := Tally => (x,y) -> combine(x,y,identity,times,)

Tally ^** ZZ := Tally => (x,n) -> (
     if n < 0 then error "expected non-negative integer";
     if n == 0 then return if class x === Set then set {()} else tally {()},
     if n == 1 then return x;
     y := x ** x;
     scan(n-2, i -> y = combine(y,x,append,times,));
     y)

Tally ? Tally := (x,y) -> (
     w := values select(merge(x,applyValues(y,minus),plus),i -> i =!= 0);
     if #w === 0 then symbol ==
     else if all(w,i -> i>0) then symbol >
     else if all(w,i -> i<0) then symbol <
     else incomparable)

Tally + Tally := Tally => (x,y) -> merge(x,y,plus)
Tally - Tally := Tally => (x,y) -> select(merge(x,applyValues(y,minus),plus),i -> i > 0)

VirtualTally = new Type of Tally
VirtualTally.synonym = "virtual tally"
VirtualTally - VirtualTally := VirtualTally => (x,y) -> select(merge(x,applyValues(y,minus),plus),i -> i != 0)
     
sum(Tally) := (w) -> sum(pairs w, (k,v) -> v * k)
product(Tally) := (w) -> product(pairs w, (k,v) -> k^v)

new Set from List := Set => (X,x) -> set x

net Set := x -> "set " | net keys x
toString Set := x -> (
     -- unpleasant hack
     if class x === Set
     then "set " | toString keys x
     else "new " | toString class x | " from " | toString keys x
     )
Set + Set := Set => (x,y) -> merge(x,y,(i,j)->i)
-- Set ++ Set := Set => (x,y) -> applyKeys(x,i->(0,i)) + applyKeys(y,j->(1,j))
Set ** Set := Set => (x,y) -> combine(x,y,identity,(i,j)->i,)
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

unique = method(Dispatch => Thing, TypicalValue => List)
unique Sequence := x -> unique toList x
unique List := x -> (
     -- old faster way: keys set x
     -- new way preserves order:
     seen := new MutableHashTable;
     select(x, i -> if seen#?i then false else seen#i = true))

-- we've been waiting to do this:
binaryOperators = unique binaryOperators
prefixOperators = unique prefixOperators
postfixOperators = unique postfixOperators
flexibleOperators = unique flexibleOperators
fixedOperators = unique fixedOperators
allOperators = unique allOperators

isSubset(Set,Set) := Boolean => (S,T) -> all(S, (k,v) -> T#?k)

isSubset(VisibleList,Set) := Boolean => (S,T) -> all(S, x -> T#?x)
isSubset(VisibleList,VisibleList) := Boolean => (S,T) -> isSubset(S,set T)
isSubset(Set,VisibleList) := Boolean => (S,T) -> isSubset(S,set T)

member(Thing,Set) := Boolean => (a,s) -> s#?a

Tally / Command  := 
Tally / Function := Tally => (x,f) -> applyKeys(x,f)

Command  \ Tally := 
Function \ Tally := Tally => (f,x) -> applyKeys(x,f)

permutations = method()
permutations VisibleList := VisibleList => x -> if #x <= 1 then {x} else flatten apply(#x, i -> apply(permutations drop(x,{i,i}), t -> prepend(x#i,t)))
permutations ZZ := List => n -> permutations toList (0 .. n-1)

partition = method()
partition(Function,Tally) := (f,s) -> (
     p := new MutableHashTable;
     scanPairs(s, (x,n) -> ( y := f x; if p#?y then if p#y#?x then p#y#x = p#y#x + n else p#y#x = n else (p#y = new MutableHashTable)#x = n; ));
     applyValues(new HashTable from p, px -> new class s from px))
partition(Function,VisibleList) := (f,s) -> (
     p := new MutableHashTable;
     scan(s, x -> ( y := f x; if p#?y then p#y = (p#y,x) else p#y = 1:x; ));
     applyValues(new HashTable from p, px -> new class s from splice px))

-----------------------------------------------------------------------------
-- a first use of sets:

protect Flexible
protect Binary
protect Prefix
protect Postfix
operatorAttributes = new MutableHashTable from apply(allOperators, op -> op => new MutableHashTable)
scan((
	  (binaryOperators,Binary),
	  (prefixOperators,Prefix),
	  (postfixOperators,Postfix)
	  ),
     (li,at) -> scan(li, op -> operatorAttributes#op#at = new MutableHashTable))
scan((
	  (flexibleBinaryOperators,Binary,Flexible),
	  (flexiblePrefixOperators,Prefix,Flexible),
	  (flexiblePostfixOperators,Postfix,Flexible)
	  ),
     (li,at,fl) -> scan(li, op -> operatorAttributes#op#at#fl = 1))
operatorAttributes = hashTable apply(pairs operatorAttributes, (op,ats) -> (op, hashTable apply(pairs ats, (at,fls) -> (at, set keys fls))))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
