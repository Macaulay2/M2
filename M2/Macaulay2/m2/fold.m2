--		Copyright 1993-1999 by Daniel R. Grayson

-- TODO: implement a copy-free accumulate and fold without reverse and drop in the interpreter
-- One solution is to use iterators; see https://github.com/Macaulay2/M2/issues/1904

needs "methods.m2"

accumulate = method()
accumulate(Function, Thing, VisibleList) := VisibleList =>
accumulate(Function, Thing, Thing)       := Iterator    => (f, x, v) -> (
    apply(v, y -> x = f(x, y)))
accumulate(VisibleList,Thing,Function) := VisibleList => (v,x,f) -> reverse apply(reverse v, w -> x = f(w,x))
accumulate(Function,VisibleList) := VisibleList => (f,v) -> (
     if #v === 0 then error "expected a nonempty list";
     accumulate(f,v#0,drop(v,1)))
accumulate(VisibleList,Function) := VisibleList => (v,f) -> (
     if #v === 0 then error "expected a nonempty list";
     accumulate(drop(v,-1),v#-1,f))
accumulate(Function, Thing) := Iterator => (f, v) -> (
    iter := iterator v;
    x := next iter;
    if x === StopIteration then error "expected a nonempty iterator"
    else accumulate(f, x, iter))

fold = method()
fold(Function,Thing,Thing) := foldL
fold(VisibleList,Thing,Function) := (v,x,f) -> (scan(reverse v, w -> x = f(w,x)); x)
fold(Function,VisibleList) := (f,v) -> (
     if # v === 0 then error "expected a nonempty list";
     fold(f,v#0,drop(v,1)))
fold(VisibleList,Function) := (v,f) -> (
     if #v === 0 then error "expected a nonempty list";
     fold(drop(v,-1),v#-1,f))
fold(Function, Thing) := (f, v) -> (
    iter := iterator v;
    x := next iter;
    if x === StopIteration then error "expected a nonempty iterator"
    else fold(f, x, iter))

demark = (s,v) -> concatenate between(s,v)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
