--		Copyright 1993-1999 by Daniel R. Grayson

-- singleton = x -> toSequence {x}
typicalValues#singleton = Sequence

List _ ZZ    := (x,i) -> x#i
ggPush List  := v -> (ggINTARRAY, gg v)
List | List  := List => join

List + List  := List => (v,w) -> apply(v,w,plus)
     - List  := List => v -> apply(v,minus)
List - List  := List => (v,w) -> apply(v,w,difference)
Thing * List := List => (a,v) -> apply(v,x->a * x)

List / Thing := List => (v,b) -> apply(v,x->x / b)	    -- slight conflict with List / Function!

Sequence _ List := Sequence => (x,y) -> apply(splice y, i -> x#i)
    List _ List := List     => (x,y) -> apply(splice y, i -> x#i)

maxPosition = x -> (
     if # x === 0 then error "expected a nonempty list" 
     else (
     	  m := x#0; 
	  pos := 0;
	  scan(1 .. # x-1, i -> if x#i>m then (m=x#i;pos=i));
	  pos))


minPosition = x -> (
     if # x === 0 then error "expected a nonempty list" 
     else (
     	  m := x#0; 
	  pos := 0;
	  scan(1 .. # x-1, i -> if x#i>m then (m=x#i;pos=i));
	  pos))


number = x -> # select x

all = (x,p) -> not any(x, i -> not p i)

same = v -> (
     -- this could be compiled for speed
     if # v === 0
     then true
     else (
	  w := v#0;
	  all(1 .. # v - 1, i -> w === v#i)
	  )
     )

member(Thing,Sequence) := member(Thing,List) := Boolean => (c,x) -> any(x, i -> c===i)

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

sum(Sequence, List, Function) :=
sum(List, Sequence, Function) :=
sum(Sequence, Sequence, Function) :=
sum(List, List, Function) := (v,w,f) -> sum(apply(v,w,identity),f)

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

product(Sequence, List, Function) :=
product(List, Sequence, Function) :=
product(Sequence, Sequence, Function) :=
product(List, List, Function) := (v,w,f) -> product(apply(v,w,identity),f)

