--		Copyright 1994 by Daniel R. Grayson

accumulate = method()
accumulate(Function,Thing,List) := List => (f,x,v) -> apply(v, y -> x = f(x,y))
accumulate(List,Thing,Function) := List => (v,x,f) -> reverse apply(reverse v, w -> x = f(w,x))
accumulate(Function,List) := List => (f,v) -> (
     if #v === 0 then error "expected a nonempty list";
     accumulate(f,v#0,drop(v,1)))
accumulate(List,Function) := List => (v,f) -> (
     if #v === 0 then error "expected a nonempty list";
     accumulate(drop(v,-1),v#-1,f))     

fold = method()
fold(Function,Thing,List) := List => (f,x,v) -> (scan(v, y -> x = f(x,y)); x)
fold(List,Thing,Function) := List => (v,x,f) -> (scan(reverse v, w -> x = f(w,x)); x)
fold(Function,List) := List => (f,v) -> (
     if # v === 0 then error "expected a nonempty list";
     fold(f,v#0,drop(v,1)))
fold(List,Function) := List => (v,f) -> (
     if #v === 0 then error "expected a nonempty list";
     fold(drop(v,-1),v#-1,f))     

demark = (s,v) -> concatenate between(s,v)

