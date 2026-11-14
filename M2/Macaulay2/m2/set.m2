-- Copyright 1994 by Daniel R. Grayson

needs "methods.m2"
needs "shared.m2" -- for union

-----------------------------------------------------------------------------
-- Tally and VirtualTally type declarations and basic constructors
-----------------------------------------------------------------------------

VirtualTally.synonym = "virtual tally"
Tally.synonym = "tally"

-- constructors, defined in d/sets.dd
tally String      :=
tally VisibleList := Tally => tally

elements = method()
elements Tally := toList Tally := x -> splice apply(pairs x, (k,v) -> v:k)

toString VirtualTally := x -> concatenate( "new ", toString class x, " from {", demark(", ", sort apply(pairs x, (v,i) -> (toString v, " => ", toString i))), "}" )

net VirtualTally := t -> peek t

VirtualTally _ Thing := (a,b) -> if a#?b then a#b else 0

VirtualTally ** VirtualTally := VirtualTally => (x,y) -> combine(x,y,identity,times,)
--VirtualTally^** ZZ           := VirtualTally => (x, n) -> BinaryPowerMethod(x, n, (a, b) -> a ** b, x -> new class x from {},
--    x -> error "VirtualTally ^** ZZ: expected non-negative integer")
-- TODO: this is different than the above and some tests rely on it.
VirtualTally ^** ZZ := VirtualTally => (x,n) -> (
     if n < 0 then error "expected non-negative integer";
     if n == 0 then return new class x from {()};
     if n == 1 then return applyKeys(x,i -> 1:i);
     y := x ** x;
     scan(n-2, i -> y = y ** x);
     y)

continueIfZero = n -> if n == 0 then continue else n
continueIfNonPositive = n -> if n <= 0 then continue else n

- VirtualTally := y -> applyValues(y,minus)
VirtualTally + VirtualTally := VirtualTally => (x,y) -> merge(x,y,continueIfZero @@ plus)
VirtualTally - VirtualTally := VirtualTally => (x,y) -> x + -y

- Tally := x -> new class x from {};
Tally + Tally := Tally => (x,y) -> merge(x,y,plus) -- no need to check for zero
Tally - Tally := Tally => (x,y) -> select(merge(x,applyValues(y,minus),plus),i -> i > 0) -- sadly, can't use continueIfNonPositive

VirtualTally ? VirtualTally := (x,y) -> (
    flag:=symbol ==;
    for k in keys x|keys y do
    if ((cmp := x_k?y_k) =!= symbol ==) and flag =!= cmp and first(flag =!= symbol ==,flag = cmp) then return incomparable;
    flag
    )

zeroVirtualTally := new VirtualTally from {}
toVirtualTally := i -> if i === 0 then zeroVirtualTally else error "comparison of a virtual tally with a nonzero integer"
VirtualTally == ZZ := (x,i) -> x == toVirtualTally i
ZZ == VirtualTally := (i,x) -> toVirtualTally i == x
VirtualTally ? ZZ := (x,i) -> x ? toVirtualTally i
ZZ ? VirtualTally := (i,x) -> toVirtualTally i ? x
VirtualTally == VirtualTally := (x,y) -> (x ? y) === symbol ==

-*
zeroTally := new Tally from {}
toTally := i -> if i === 0 then zeroTally else error "comparison of a tally with a nonzero integer"
Tally == ZZ := (x,i) -> x == toTally i
ZZ == Tally := (i,x) -> toTally i == x
Tally ? ZZ := (x,i) -> x ? toTally i
ZZ ? Tally := (i,x) -> toTally i ? x
*-

Number * VirtualTally := (i,v) -> if i==0 then new class v from {} else applyValues(v,y->y*i)
Number * Tally := (i,v) -> if i<=0 then new class v from {} else applyValues(v,y->y*i)
     
sum VirtualTally := (w) -> sum(pairs w, (k,v) -> v * k)
product VirtualTally := (w) -> product(pairs w, (k,v) -> k^v)

-----------------------------------------------------------------------------
-- Set type declarations and basic constructors
-----------------------------------------------------------------------------

Set.synonym = "set"

-- constructors, both compiled functions defined in d/sets.dd
set' = set
set = method(TypicalValue => Set, Dispatch => Thing)
set Set         := identity
set HashTable   :=
set VisibleList := Set => set'
new Set from Set         := Set => (Set, S) -> set' S
new Set from HashTable   :=
new Set from VisibleList := Set => (Set, X) -> set' X

-- set operations
elements Set := List => keys
union()         := () -> set {}
union Set       := identity
union(Set, Set) := Set + Set := Set => (x,y) -> merge(x,y,(i,j)->i)

-- Set ++ Set := Set => (x,y) -> applyKeys(x,i->(0,i)) + applyKeys(y,j->(1,j))
Set ** Set := Set => (x,y) -> combine(x,y,identity,(i,j)->i,)

Set * Set := Set => (x,y) -> (
     if # x < # y 
     then set select(keys x, k -> y#?k)
     else set select(keys y, k -> x#?k)
     )
intersect Set       := Set => {} >> o -> identity
intersect(Set, Set) := Set => {} >> o -> (x,y) -> x*y

Set - Set := Set => (x,y) -> applyPairs(x, (i,v) -> if not y#?i then (i,v))
VisibleList - Set := List => (x,y) -> select(x, i -> not y#?i)
Set - VisibleList := Set => (x,y) -> x - set y

--
sum Set := s -> sum toList s
product Set := s -> product toList s

-----------------------------------------------------------------------------
-- Methods that use sets
-----------------------------------------------------------------------------

unique = method(Dispatch => Thing, TypicalValue => List)
unique VisibleList := x -> (
     -- old faster way: keys set x
     -- new way preserves order:
     seen := new MutableHashTable;
     select(x, i -> if seen#?i then false else seen#i = true))

repeats = L -> #L - #unique L

isSubset(Set,Set) := Boolean => (S,T) -> all(S, (k,v) -> T#?k)

isSubset(VisibleList,Set) := Boolean => (S,T) -> all(S, x -> T#?x)
isSubset(VisibleList,VisibleList) := Boolean => (S,T) -> isSubset(S,set T)
isSubset(Set,VisibleList) := Boolean => (S,T) -> isSubset(S,set T)

isMember(Thing,Set) := Boolean => (a,s) -> s#?a

VirtualTally / Command  :=
VirtualTally / Function := VirtualTally => (x,f) -> applyKeys(x,f,plus)

Command  \ VirtualTally :=
Function \ VirtualTally := VirtualTally => (f,x) -> applyKeys(x,f,plus)

Set / Command  :=
Set / Function := Set => (x,f) -> applyKeys(x,f,(i,j)->1)

Command  \ Set :=
Function \ Set := Set => (f,x) -> applyKeys(x,f,(i,j)->1)


permutations = method()
permutations VisibleList := VisibleList => x -> if #x <= 1 then {x} else flatten apply(#x, i -> apply(permutations drop(x,{i,i}), t -> prepend(x#i,t)))
permutations ZZ := List => n -> permutations toList (0 .. n-1)

inversePermutation = method()
inversePermutation VisibleList := VisibleList => v -> (
    w := new MutableList from #v:null; scan(#v, i -> w#(v#i)=i); toList w )

uniquePermutations = method()
uniquePermutations VisibleList := VisibleList => x -> if #x <= 1 then {x} else (
    l := new MutableHashTable;
    flatten apply(#x,i -> if l#?(x#i) then {} else (
            l#(x#i)=1;
            apply(uniquePermutations drop(x,{i,i}), t -> prepend(x#i,t)))
    ))
uniquePermutations ZZ := permutations

partition = method()
partition(VirtualTally,Function,VisibleList, Function) := HashTable => (t,f,ks, g) -> (
    p := new MutableHashTable from for k in ks list (k, new MutableHashTable);
    scanPairs(t, (x, n) -> (
        y := f x;
        v := g x;
        (p#y ??= new MutableHashTable)#v ??= 0;
        p#y#v += n;
    ));
    applyValues(new HashTable from p, px -> new class t from px)
)
partition(VirtualTally,Function,VisibleList) := HashTable => (t,f,ks) -> partition(t, f, ks, identity)
partition(VirtualTally,Function) := HashTable => (t, f) -> partition(t, f, {}, identity)
partition(VirtualTally, Function, Function) := HashTable => (t, f, g) -> partition(t, f, {}, g)

-- partition-function-first interface supported
partition(Function,VirtualTally) := HashTable => (f,t) -> partition(t,f,{}, identity)
partition(Function,VirtualTally,Function) := HashTable => (f,t,g) -> partition(t,f,{}, g)
partition(Function,VirtualTally,VisibleList) := HashTable => (f,t,ks) -> partition(t, f, ks, identity)
partition(Function, VirtualTally,VisibleList, Function) := HashTable => (f,t,ks, g) -> partition(t, f, ks, g)

-- partition values of xs according to their values under f
--  ensure that all of the elements of ks also appear as keys in the result
--  apply g to each value in the resulting partition
partition(VisibleList, Function, VisibleList, Function) := HashTable => (xs, f, ks, g) -> (
    -- initialize with ks to guarantee that these keys appear
    p := new MutableHashTable from for k in ks list (k, new MutableHashTable);
    -- scan over xs and update p as we go
    scan(xs, x -> (y := f x; vs := p#y ??= new MutableHashTable; vs#(#vs) = g x));
    -- repack everything into the same type of data structure as the input
    new HashTable from apply(pairs p, (k, v) -> (k, new class xs from values v))
)
partition(VisibleList, Function) := HashTable => (xs, f) -> partition(xs, f, {}, identity)
partition(VisibleList, Function, Function) := HashTable => (xs, f, g) -> partition(xs, f, {}, g)
partition(VisibleList, Function, VisibleList) := HashTable => (xs, f, ks) -> partition(xs, f, ks, identity)

-- partition-function-first interface supported
partition(Function, VisibleList, VisibleList, Function) := HashTable => (f, xs, ks, g) -> partition(xs, f, ks, g)
partition(Function, VisibleList) := HashTable => (f, xs) -> partition(xs, f, {}, identity)
partition(Function, VisibleList, Function) := HashTable => (f, xs, g) -> partition(xs, f, {}, g)
partition(Function, VisibleList, VisibleList) := HashTable => (f, xs, ks) -> partition(xs, f, ks, identity)
-----------------------------------------------------------------------------
-- a first use of sets:

-- TODO: move these somewhere more appropriate
-- we've been waiting to do this:
binaryOperators = unique toList binaryOperators
prefixOperators = unique toList prefixOperators
postfixOperators = unique toList postfixOperators
flexibleOperators = unique toList flexibleOperators
fixedOperators = unique toList fixedOperators
allOperators = unique toList allOperators

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
	  (flexiblePostfixOperators,Postfix,Flexible),
	  (augmentedAssignmentOperators,Binary,Flexible)
	  ),
     (li,at,fl) -> scan(li, op -> operatorAttributes#op#at#fl = 1))
operatorAttributes = hashTable apply(pairs operatorAttributes, (op,ats) -> (op, hashTable apply(pairs ats, (at,fls) -> (at, set keys fls))))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
