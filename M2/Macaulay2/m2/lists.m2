--		Copyright 1993-2002 by Daniel R. Grayson

List#"major documentation node" = true

VisibleList _ ZZ := (s,i) -> s#i
String _ ZZ := String => (s,i) -> s#i
String _ Sequence := String => (s,i) -> ((j,k) -> substring(j,k,s)) i

List | List  := List => join
Array | Array  := Array => join
Sequence | Sequence := Sequence => join

List + List  := List => (v,w) -> apply(v,w,plus)
     - List  := List => v -> apply(v,minus)
List - List  := List => (v,w) -> apply(v,w,difference)
Thing * List := List => (a,v) -> apply(v,x->a * x)

List / Thing := List => (v,b) -> apply(v,x->x / b)	    -- slight conflict with List / Function!

VisibleList _ List := VisibleList => (x,y) -> apply(splice y, i -> x#i)

maxPosition = method(SingleArgumentDispatch => true)
minPosition = method(SingleArgumentDispatch => true)

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

all = method()
all(HashTable,Function) := all(BasicList,Function) := Boolean => (x,p) -> not any(x, i -> not p i)

same = v -> (
     -- this could be compiled for speed
     if # v === 0
     then true
     else (
	  w := v#0;
	  all(1 .. # v - 1, i -> w === v#i)
	  )
     )

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
rotate(VisibleList,ZZ) := (s,n) -> (
     if #s == 0 then s
     else (
	  n = n % #s;
	  join(drop(s,n),take(s,n))))

sort List :=  sort Sequence := opts -> internalsort
rsort List := rsort Sequence := opts -> internalrsort

-- we've been waiting to do this:
binaryOperators = sort binaryOperators
prefixOperators = sort prefixOperators
postfixOperators = sort postfixOperators
flexibleOperators = sort flexibleOperators
fixedOperators = sort fixedOperators
allOperators = sort allOperators

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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
