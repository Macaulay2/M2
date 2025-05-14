--		Copyright 1993-2002 by Daniel R. Grayson

needs "set.m2"
needs "methods.m2"

    Sequence.synonym = "sequence"
       Array.synonym = "array"
        List.synonym = "list"
   BasicList.synonym = "basic list"
 VisibleList.synonym = "visible list"
 MutableList.synonym = "mutable list"
AngleBarList.synonym = "angle bar list"

List ? List := (s,t) -> if class s === class t then toSequence s ? toSequence t else (class s) ? (class t)
Option ? Option := (s,t) -> toSequence s ? toSequence t

VisibleList _ ZZ := (s,i) -> s#i
String _ ZZ := String => (s,i) -> s#i
String _ Sequence := String => (s,p) -> substring(p,s)

List | List  := List => join
Array | Array  := Array => join
Sequence | Sequence := Sequence => join

List + List  := List => (v,w) -> apply(v,w,plus)
     - List  := List => v -> apply(v,minus)
List - List  := List => (v,w) -> apply(v,w,difference)
List * Thing := List => (v,a) -> apply(v,x->x * a)
Thing * List := List => (a,v) -> apply(v,x->a * x)
List ** List := List => (X,Y) -> flatten for x in X list apply(Y, y -> (x, y))

List /  RingElement := List /  Number := List => (v,b) -> apply(v,x->x /  b)
List // RingElement := List // Number := List => (v,b) -> apply(v,x->x // b)
List  % RingElement := List  % Number := List => (v,b) -> apply(v,x->x  % b)

VisibleList _ List := VisibleList => (x,y) -> apply(splice y, i -> x#i)

Sequence .. Sequence := Sequence => (v,w) -> (
     n := #v;
     if n =!= #w then error "expected sequences of equal length";
     if n === 0 
     then 1 : v
     else if n === 1 
     then apply(first v .. first w, t -> 1:t)
     else splice table(first v .. first w, drop(v,1) .. drop(w,1), prepend))
Sequence ..< Sequence := Sequence => (v,w) -> (
     n := #v;
     if n =!= #w then error "expected sequences of equal length";
     if n === 0 
     then 1 : v
     else if n === 1 
     then apply(first v ..< first w, t -> 1:t)
     else splice table(first v ..< first w, drop(v,1) ..< drop(w,1), prepend))

chk := (v,w) -> (
     if #v =!= #w then error "expected lists of equal length";
     if class v =!= class w then error "expected lists of the same class";
     )
List .. List := Sequence => (v,w) -> (
     chk(v,w);
     L := class v;
     apply(toSequence v .. toSequence w, x -> new L from x))
List ..< List := Sequence => (v,w) -> (
     chk(v,w);
     L := class v;
     apply(toSequence v ..< toSequence w, x -> new L from x))

String .. String := Sequence => (s,t) -> (
     s = toSequence utf8 s;
     t = toSequence utf8 t;
     if #s =!= #t then error "expected strings of equal length (in utf8 encoding)";
     utf8 \ ( s .. t ))
String ..< String := Sequence => (s,t) -> (
     s = toSequence utf8 s;
     t = toSequence utf8 t;
     if #s =!= #t then error "expected strings of equal length (in utf8 encoding)";
     utf8 \ ( s ..< t ))

-----------------------------------------------------------------------------
-- positions, position, minPosition, maxPosition, number
-----------------------------------------------------------------------------

positions = method(TypicalValue => List)
positions(MutableList, Function) :=
positions(VisibleList, Function) := (v, f) -> for i from 0 to #v-1 list if f v#i then i else continue

position = method(TypicalValue => ZZ, Options => {Reverse => false})
position(ZZ,          Function) := o -> (n, f) -> position(0..n-1, f, o)
position(VisibleList, Function) := o -> (v, f) -> (
    if o.Reverse
    then (for i to #v-1 do if f v#(-i-1) then return #v-i-1)
    else (for i to #v-1 do if f v#i      then return i))
position(VisibleList, VisibleList, Function) := o -> (v, w, f) -> (
    if #v != #w then error "expected lists of the same length";
    if o.Reverse
    then (for i to #v-1 do if f(v#(-i-1), w#(-i-1)) then return #v-i-1)
    else (for i to #v-1 do if f(v#i,      w#i)      then return i))

maxPosition = method(Dispatch => Thing)
minPosition = method(Dispatch => Thing)

maxPosition BasicList := ZZ => x -> (
     if # x === 0 then error "expected a nonempty list" 
     else (
     	  m := x#0; 
	  pos := 0;
	  scan(1 .. # x-1, i -> if x#i>m then (m=x#i;pos=i));
	  pos))

minPosition BasicList := ZZ => x -> (
     if # x === 0 then error "expected a nonempty list" 
     else (
     	  m := x#0; 
	  pos := 0;
	  scan(1 .. # x-1, i -> if x#i<m then (m=x#i;pos=i));
	  pos))

delete = method()
delete(Thing, BasicList) := (x, v) -> select(v, i -> i =!= x)

number = x -> # select x

-----------------------------------------------------------------------------
-- any, all, same, uniform, isMember
-----------------------------------------------------------------------------

-- method defined in actors4.d
any(ZZ,                   Function) :=
any(HashTable,            Function) :=
any(BasicList,            Function) :=
any(BasicList, BasicList, Function) := Boolean => any

all = method(TypicalValue => Boolean)
all(ZZ,                   Function) := -- uses 0 .. n-1
all(HashTable,            Function) := -- uses pairs h
all(BasicList,            Function) := (x,    p) -> not any(x,    i  -> not p i)
all(BasicList, BasicList, Function) := (x, y, p) -> not any(x, y, ij -> not p ij)
-- TODO: implement this for 'any'
all BasicList := L -> not any(L, x -> not x)

-- TODO: how to get this to work? When fixed, replace "same apply" with "same"
--same = method(TypicalValue => Boolean)
--same BasicList            :=  L     -> same(L, identity)
--same(BasicList, Function) := (L, f) -> #L <= 1 or (t := f L#0; not any(L, l -> t =!= f l))
same = L -> #L <= 1 or (t := L#0; not any(L, l -> t =!= l))

uniform = L -> same apply(L, class)

isMember(Thing,VisibleList) := Boolean => (c,x) -> any(x, i -> c===i)

-----------------------------------------------------------------------------

sum List := x -> plus toSequence x

sum(ZZ,Function) := (n,f) -> (
     if n <= 0 then 0
     else (
	  s := f 0;
	  for i from 1 to n-1 do s = s + f i;
	  s))

sum(VisibleList,Function) := (x,f) -> (
     n := #x;
     if n === 0 then 0
     else (
	  s := f x#0;
	  for i from 1 to n-1 do s = s + f x#i;
	  s))

sum(VisibleList, VisibleList, Function) := (x,y,f) -> (
     n := #x;
     if n =!= #y then error "expected lists of the same length";
     if n === 0 then 0
     else (
	  s := f(x#0,y#0);
	  for i from 1 to n-1 do s = s + f(x#i,y#i);
	  s))

product List := x -> times toSequence x

product(ZZ,Function) := (n,f) -> (
     if n <= 0 then 1
     else (
	  s := f 0;
	  for i from 1 to n-1 do s = s * f i;
	  s))

product(VisibleList,Function) := (x,f) -> (
     n := #x;
     if n === 0 then 1
     else (
	  s := f x#0;
	  for i from 1 to n-1 do s = s * f x#i;
	  s))

product(VisibleList, VisibleList, Function) := (x,y,f) -> (
     n := #x;
     if n =!= #y then error "expected lists of the same length";
     if n === 0 then 1
     else (
	  s := f(x#0,y#0);
	  for i from 1 to n-1 do s = s * f(x#i,y#i);
	  s))

rotate = method()
rotate(ZZ,VisibleList) := (n,s) -> (
     if #s == 0 then s
     else (
	  n = n % #s;
	  join(drop(s,n),take(s,n))))

-- sort should not accept sequences because sort is now a function with options!
sort List :=  opts -> internalsort
rsort List := opts -> internalrsort

apply({sort, rsort}, sort ->
    sort(List, Function) := opts -> (L, f) -> (
	H := hashTable(join, apply(L, l -> f(l) => {l}));
	flatten apply(sort keys H, k -> H#k)))

List << List := (A, B) -> all(min(#A, #B), i -> A#i <= B#i)

-- we've been waiting to do this:
binaryOperators = sort binaryOperators
prefixOperators = sort prefixOperators
postfixOperators = sort postfixOperators
flexibleOperators = sort flexibleOperators
fixedOperators = sort fixedOperators
allOperators = sort allOperators

random List := opts -> s -> (
     n := #s;
     if n <= 1 then return s;
     s = new MutableList from s;
     for i from 1 to n-1 do (
	  j := random (i+1);
	  t := s#i ; s#i = s#j ; s#j = t;
	  );
     new List from s)

randomSubset = method()
-- Knuth Algorithm S, Art of Computer Programming, Section 3.4.2
randomSubset(ZZ, ZZ) := (N, n) -> (
    if n < 0 or n > N then error("expected an integer between 0 and ", N);
    t := 0;
    apply(n, m -> (
	    while (N - t) * rawRandomRRUniform defaultPrecision >= n - m
	    do t += 1;
	    first (t, t += 1))))
randomSubset ZZ := N -> (
    if N < 0 then error "expected a nonnegative integer";
    r := random 2^N;
    for i to N - 1 list if r & 2^i != 0 then i else continue)
randomSubset(VisibleList, ZZ) := (x, n) -> x_(randomSubset(#x, n))
randomSubset VisibleList := x -> x_(randomSubset(#x))
randomSubset(Set, ZZ) := (x, n) -> set randomSubset(toList x, n)
randomSubset Set := x -> set randomSubset toList x

-----------------------------------------------------------------------------
-- sublists
-----------------------------------------------------------------------------
sublists = method()
sublists(VisibleList, Function, Function, Function) := (x,f,g,h) -> (
     -- x is a list with elements i
     -- apply g to the nonempty sublists of consecutive elements between the ones for which f i is true
     -- apply h to those i for which f i is false
     -- return the results in the same order
     p := positions(toSequence x, i -> not f i);
     s := mingle( apply( prepend(-1,p), append(p,#x), identity), apply(p,i->(i,i)) );
     s = select(s, (i,j) -> j != i+1);
     s = apply(s, (i,j) -> if i === j then h x#i else g take(x,{i+1,j-1}));
     s )
sublists(VisibleList, Function) := (x,f) -> sublists(x,f,identity,identity)
sublists(VisibleList, Function, Function) := (x,f,g) -> sublists(x,f,g,identity)
sublists(VisibleList, Function, Nothing) := (x,f,g) -> sublists(x,f,identity,identity)
sublists(VisibleList, Function, Function, Nothing) := (x,f,g,h) -> sublists(x,f,g,identity)
sublists(VisibleList, Function, Nothing, Function) := (x,f,g,h) -> sublists(x,f,identity,h)
sublists(VisibleList, Function, Nothing, Nothing) := (x,f,g,h) -> sublists(x,f,identity,identity)
-----------------------------------------------------------------------------
commonest = method(Dispatch => Thing, TypicalValue => List)
commonest VisibleList := x -> commonest tally x
commonest Set := keys
commonest Tally := t -> (
     t = hashTable(join, apply(pairs t, (k,v) -> (v,{k})));
     if #t === 0 then {} else t#(max keys t))

-- suggested by Allen Knutson:
insert = method()
insert(ZZ,Thing,BasicList) := BasicList => (i,x,s) -> (
     j := i;
     if j < 0 then j = j + #s + 1;
     if j < 0 or j > #s then error("insert: index ", toString i, " out of bounds: 0..", toString length s);
     join(take(s,{0,j-1}),{x},take(s,{j,#s-1})))
switch = method()
switch(ZZ,ZZ,BasicList) := BasicList => (i,j,s) -> (
     t := new MutableList from s;
     t#i = s#j;
     t#j = s#i;
     new class s from t)
--

replace(ZZ,Thing,BasicList) := BasicList => {} >> o -> (i,x,s) -> (
     j := i;
     if j < 0 then j = j + #s;
     if j < 0 or j >= #s then error("replace: index ", toString i, " out of bounds: 0..", toString (length s - 1));
     join(take(s,{0,j-1}),{x},take(s,{j+1,#s-1})))

isSorted = method(Dispatch => Thing)
isSorted VisibleList := s -> all(#s-1, i -> s#i <= s#(i+1))

deepApply' = (L, f, g) -> flatten if g L then toList apply(L, e -> deepApply'(e, f, g)) else toList{f L}
deepApply  = (L, f) ->  deepApply'(L, f, e -> instance(e, BasicList))
deepScan   = (L, f) -> (deepApply'(L, f, e -> instance(e, BasicList));) -- not memory efficient

deepSelect = method()
deepSelect(BasicList, Type)     := (L, T) -> deepSelect(L, e -> instance(e, T))
deepSelect(BasicList, Function) := (L, f) -> nonnull deepApply'(L,
    e -> if f e then e, e -> instance(e, BasicList) and not f e)

-----------------------------------------------------------------------------
-- Tables (nested lists)
-----------------------------------------------------------------------------

isTable = L -> instance(L, List) and all(L, row -> instance(row, List)) and same apply(L, length)

table = (rows, cols, f) -> apply(rows, r -> apply(cols, c -> f(r, c)))

subtable = (rows, cols, a) -> table(rows, cols, (r, c) -> a_r_c)

applyTable = (m,f) -> apply(m, v -> apply(v,f))

transpose List := List => L -> if isTable L then pack(#L, mingle L) else error "transpose expected a table"

pack' = pack -- defined in d/actors4.d
pack = method()
pack(ZZ, String)    :=
pack(ZZ, BasicList) := List => pack'
-- TODO: deprecate these versions
pack(String,    ZZ) :=
pack(BasicList, ZZ) := List => (L, n) -> pack'(n, L)

-----------------------------------------------------------------------------

parallelApplyRaw = (L, f) ->
     -- 'reverse's to minimize thread switching in 'taskResult's:
     reverse (taskResult \ reverse apply(L, e -> schedule(f, e)));
parallelApply = method(Options => {Strategy => null})
parallelApply(BasicList, Function) := o -> (L, f) -> (
     if o.Strategy === "raw" then return parallelApplyRaw(L, f);
     n := #L;
     numThreads := min(n + 1, maxAllowableThreads);
     oldAllowableThreads := allowableThreads;
     if allowableThreads < numThreads then allowableThreads = numThreads;
     numChunks := 3 * numThreads;
     res := if n <= numChunks then toList parallelApplyRaw(L, f) else
	  flatten parallelApplyRaw(pack(L, ceiling(n / numChunks)), chunk -> apply(chunk, f));
     allowableThreads = oldAllowableThreads;
     res);


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
