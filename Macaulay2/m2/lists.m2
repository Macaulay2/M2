--		Copyright 1993-2002 by Daniel R. Grayson

Sequence _ ZZ := List _ ZZ := (s,i) -> s#i
String _ ZZ := String => (s,i) -> s#i
String _ Sequence := String => (s,i) -> ((j,k) -> substring(j,k,s)) i

List | List  := List => join
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

sum(List) := x -> plus toSequence x

sum(List,Function) := 
sum(Sequence,Function) := 
sum(ZZ,Function) := (n,f) -> (
     s := 0;
     g := x -> (
	  s = f x; 
	  g = x -> s = s + f x);
     scan(n,x -> g x);
     s);

sum(VisibleList, VisibleList, Function) := (v,w,f) -> sum(apply(v,w,identity),f)

product List := x -> times toSequence x

product(List,Function) := 
product(Sequence,Function) := 
product(ZZ,Function) := (n,f) -> (
     s := 1;
     g := x -> (
	  s = f x; 
	  g = x -> s = s * f x);
     scan(n,x -> g x);
     s);

product(VisibleList, VisibleList, Function) := (v,w,f) -> product(apply(v,w,identity),f)

rotate = method()
rotate(VisibleList,ZZ) := (s,n) -> (
     if #s == 0 then s
     else (
	  n = n % #s;
	  join(drop(s,n),take(s,n))))

 sort List :=  sort Sequence := opts -> internalsort
rsort List := rsort Sequence := opts -> internalrsort

-----------------------------------------------------------------------------
-- sublists
-----------------------------------------------------------------------------
sublists = (x,f,g,h) -> (
     -- x is a list with elements i
     -- apply g to those i for which f i is true
     -- apply h to the sublists, possibly empty, including those at the beginning and end, of elements between the ones for which f i is true
     -- return the results in the same order
     p := positions(toSequence x, f);
     mingle(
	  apply( prepend(-1,p), append(p,#x), (i,j) -> h take(x,{i+1,j-1})),
	  apply( p, i -> g x#i)))
-----------------------------------------------------------------------------

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
