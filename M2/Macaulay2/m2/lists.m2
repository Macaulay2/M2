--		Copyright 1993-2002 by Daniel R. Grayson

List#"major documentation node" = true

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
Thing * List := List => (a,v) -> apply(v,x->a * x)

List / Thing := List => (v,b) -> apply(v,x->x / b)	    -- slight conflict with List / Function!
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

number = x -> # select x

all = method(TypicalValue => Boolean)
all(ZZ,Function) := all(HashTable,Function) := all(BasicList,Function) := (x,p) -> not any(x, i -> not p i)
all(BasicList,BasicList,Function) := (x,y,p) -> not any(apply(x,y,identity), ij -> not p ij)

same = v -> #v <= 1 or (
     w := v#0;
     for i from 1 to #v-1 do if w =!= v#i then return false;
     true)

sameresult = (f,v) -> #v <= 1 or (
     w := f v#0;
     for i from 1 to #v-1 do if w =!= f v#i then return false;
     true)

member(Thing,VisibleList) := Boolean => (c,x) -> any(x, i -> c===i)

sum List := x -> plus toSequence x

sum(ZZ,Function) := (n,f) -> (
     if n === 0 then 0
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
     if n === 0 then 1
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
insert(ZZ,Thing,VisibleList) := VisibleList => (i,x,s) -> (
     j := i;
     if j < 0 then j = j + #s + 1;
     if j < 0 or j > #s then error("insert: index ", toString i, " out of bounds: 0..", toString length s);
     join(take(s,{0,j-1}),{x},take(s,{j,#s-1})))
switch = method()
switch(ZZ,ZZ,VisibleList) := VisibleList => (i,j,s) -> (
     t := new MutableList from s;
     t#i = s#j;
     t#j = s#i;
     new class s from t)
--

replace(ZZ,Thing,VisibleList) := VisibleList => {} >> o -> (i,x,s) -> (
     j := i;
     if j < 0 then j = j + #s;
     if j < 0 or j >= #s then error("replace: index ", toString i, " out of bounds: 0..", toString (length s - 1));
     join(take(s,{0,j-1}),{x},take(s,{j+1,#s-1})))

isSorted = method(Dispatch => Thing)
isSorted VisibleList := s -> all(#s-1, i -> s#i <= s#(i+1))

deepApply' = (L, f, g) -> flatten if g L then toList apply(L, e -> deepApply'(e, f, g)) else toList{f L}
deepApply  = (L, f) ->  deepApply'(L, f, e -> instance(e, BasicList))
deepScan   = (L, f) -> (deepApply'(L, f, e -> instance(e, BasicList));) -- not memory efficient

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
