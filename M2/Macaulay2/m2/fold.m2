--		Copyright 1993-1999 by Daniel R. Grayson

needs "methods.m2"

accumulate = method()
accumulate(Function,Thing,VisibleList) := VisibleList => (f,x,v) -> apply(v, y -> x = f(x,y))
accumulate(VisibleList,Thing,Function) := VisibleList => (v,x,f) -> reverse apply(reverse v, w -> x = f(w,x))
accumulate(Function,VisibleList) := VisibleList => (f,v) -> (
     if #v === 0 then error "expected a nonempty list";
     accumulate(f,v#0,drop(v,1)))
accumulate(VisibleList,Function) := VisibleList => (v,f) -> (
     if #v === 0 then error "expected a nonempty list";
     accumulate(drop(v,-1),v#-1,f))     

fold = method()
fold(Function,Thing,VisibleList) := VisibleList => (f,x,v) -> (scan(v, y -> x = f(x,y)); x)
fold(VisibleList,Thing,Function) := VisibleList => (v,x,f) -> (scan(reverse v, w -> x = f(w,x)); x)
fold(Function,VisibleList) := VisibleList => (f,v) -> (
     if # v === 0 then error "expected a nonempty list";
     fold(f,v#0,drop(v,1)))
fold(VisibleList,Function) := VisibleList => (v,f) -> (
     if #v === 0 then error "expected a nonempty list";
     fold(drop(v,-1),v#-1,f))     

demark = (s,v) -> concatenate between(s,v)


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
