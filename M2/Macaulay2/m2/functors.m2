-- TODO: figure out how to load this file later

needs "code.m2" -- for methods
needs "expressions.m2"
needs "methods.m2"

protect argument
protect subscript
protect superscript

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

wrongDomain := (G, op, X) -> error("no method for ", toString G, toString op, toString X)

-----------------------------------------------------------------------------
-- helpers for functors
-----------------------------------------------------------------------------

-- flatten the arguments given to a functor
functorArgs = method()
functorArgs(Thing,        Sequence) := (i,    args) -> prepend(i, args)
functorArgs(Thing, Thing, Sequence) := (i, j, args) -> prepend(i, prepend(j, args))
functorArgs(Thing, Thing, Thing)    :=
functorArgs(Thing, Thing)           := identity

-- check if a function can be applied to the inputs
applyFunctor = (key, G, op, X) -> (
    if (F := lookup key) =!= null then F X else wrongDomain(G, op, X))

-- TODO: retire this
applyFunctor' = (key, desc, X) -> (
    if (F := lookup key) =!= null then F X
    else error("no method for ", desc, " applied to ", X))

-----------------------------------------------------------------------------
-- Functor type declarations
-----------------------------------------------------------------------------

Functor = new Type of MutableHashTable
Functor.synonym = "functor"
globalAssignment Functor

-----------------------------------------------------------------------------
-- Main methods
-----------------------------------------------------------------------------
-- TODO: domain and codomain

Functor           Thing := (G, X) -> if G#?argument    then G#argument X    else wrongDomain(G, " ", X)

-----------------------------------------------------------------------------
-- printing and introspection

expression Functor := F -> if F.?expression then F.expression else (lookup(expression, Type)) F
describe   Functor := F -> if F.?describe   then F.describe   else (lookup(describe,   Type)) F
net        Functor := F -> if F.?net        then F.net        else (lookup(net,        Type)) F
toString   Functor := F -> if F.?toString   then F.toString   else (lookup(toString,   Type)) F
toExternalString Functor := toString @@ describe
precedence Functor := x -> 70

-- TODO: use codeHelpers to get code HH to work
-- TODO: get methods OO to work
-- TODO: improve this for RingMap_*
methods' := lookup(methods, Symbol)
methods Functor := F -> (
    if F === HH then join(methods homology, methods cohomology) else methods' F)
-- TODO: perhaps give info about argument, subscript, superscript?
methodOptions Functor := F -> null

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
