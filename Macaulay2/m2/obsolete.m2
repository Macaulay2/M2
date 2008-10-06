--		Copyright 1997-2002 by Daniel R. Grayson

undocumented mutableZero
mutableZero = X -> error "mutableZero' has been replaced by mutableMatrix"

undocumented unlist
unlist = X -> error "'unlist' has been replaced by toSequence"

undocumented evaluate
evaluate = X -> error "'evaluate' has been replaced by 'value'"

undocumented seq
seq = X -> error "'seq x' has been replaced by 'singleton x', which has been replaced by '1:x'"

undocumented verticalJoin
verticalJoin = X -> error "'verticalJoin' has been replaced by 'stack'"

undocumented netRows
netRows = X -> error "'netRows' has been replaced by 'unstack'"

-- name = X -> error "'name' has been replaced by 'toString'"

undocumented quote
quote = X -> error "'quote' has been replaced by 'symbol'"

undocumented Numeric
Numeric = X -> error "'Numeric' has been replaced by 'numeric'"

undocumented submodule
submodule = X -> error "'submodule' has been removed"

undocumented monomialCurve
monomialCurve = X -> error "'monomialCurve' has been replaced by 'monomialCurveIdeal'"

undocumented assign
assign = X -> error "'assign' has been replaced by '<-'"

undocumented minprimes
minprimes = X -> error "'minprimes' has been replaced by 'independentSets'"

--map(Module) := Matrix => options -> (M) -> error "method for 'map(Module)' has been removed: use 'map(M,M,1)' instead"
--undocumented(map,Module)
map(Ideal) := Matrix => options -> (I) -> error "method for 'map(Ideal)' has been removed: use 'map(module I,module I, 1)' instead"
undocumented(map,Ideal)

map(Ideal,Ideal) := Matrix => options -> (I,J) -> error "method for 'map(Ideal,Ideal)' has been removed: use 'map(module I,module J)' instead"
undocumented(map,Ideal,Ideal)

map(Module,Matrix) := options -> (M,f) -> error "method for 'map(Module,Matrix)' has been replaced: use 'map(M,,f)' instead";
undocumented(map,Module,Matrix)

map(Module,ZZ) := map(Module,RingElement) := options -> (M,r) -> error "method for 'map(Module,RingElement)' has been removed: use 'map(M,M,r)' instead"
undocumented(map,Module,ZZ)
undocumented(map,Module,RingElement)

RingElement _ ZZ := x -> error "method for 'RingElement _ ZZ' has been replaced: use 'part(n,f)' instead"
RingElement _ List := x -> error "method for 'RingElement _ List' has been replaced: use 'part(d,f)' instead"
undocumented(symbol _,RingElement,ZZ)
undocumented(symbol _,RingElement,List)

diff(RingElement)    := f -> error "method for 'diff(RingElement)' has been removed: use 'diff(vars ring f, f)' instead"
diff(Matrix)         := f -> error "method for 'diff(Matrix)' has been removed: use 'diff(vars ring f, f)' instead"
undocumented(diff,RingElement)
undocumented(diff,Matrix)

map(Module,Module) := opts -> (M,N) -> error "method for 'map(Module,Module)' has been removed: use 'inducedMap' instead"
undocumented(map,Module,Module)

--monomialIdeal Ring := MonomialIdeal => R -> monomialIdeal {0_R}
monomialIdeal Ring := MonomialIdeal => R -> error "method for 'monomialIdeal(R)' has been removed: use 'monomialIdeal(0_R)' instead"
undocumented(monomialIdeal,Ring)

undocumented elapsedTime
elapsedTime = x -> error "function 'elapsedTime' has been renamed to 'cpuTime'"

undocumented pushForward1
pushForward1 = x -> error "function 'pushForward1(f,M)' has been replaced by 'relations coimage map(M,f)'"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
