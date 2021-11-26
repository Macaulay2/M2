--		Copyright 1995 by Daniel R. Grayson
-- TODO: figure out how to load this file later

needs "code.m2" -- for methods
needs "expressions.m2"
needs "methods.m2"

protect argument
protect subscript
protect superscript

-----------------------------------------------------------------------------
-- helpers for functors
-----------------------------------------------------------------------------

-- what does this do?
args := method()
args(Thing,        Sequence) := (i,    args) -> prepend(i, args)
args(Thing, Thing, Sequence) := (i, j, args) -> prepend(i, prepend(j, args))
args(Thing, Thing, Thing)    :=
args(Thing, Thing)           := identity

wrongDomain := (G, op, X) -> error("no method for ", toString G, toString op, toString X)

applyFunctor = (key, X) -> (
    if (F := lookup key) =!= null then F X
    -- TODO: expand this error message
    else error("no method for functor ", toString key#1, " applied to ", X))

-----------------------------------------------------------------------------
-- Functor and ScriptedFunctor type declarations
-----------------------------------------------------------------------------

Functor = new Type of MutableHashTable
Functor.synonym = "functor"
globalAssignment Functor

ScriptedFunctor = new Type of Functor
ScriptedFunctor.synonym = "scripted functor"

-----------------------------------------------------------------------------
-- Main methods
-----------------------------------------------------------------------------
-- TODO: domain and codomain

Functor           Thing := (G, X) -> if G#?argument    then G#argument X    else wrongDomain(G, " ", X)
ScriptedFunctor _ Thing := (G, i) -> if G#?subscript   then G#subscript i   else wrongDomain(G, symbol _, i)
ScriptedFunctor ^ Thing := (G, i) -> if G#?superscript then G#superscript i else wrongDomain(G, symbol ^, i)

-----------------------------------------------------------------------------

net        Functor := lookup(net, Type)
toString   Functor := lookup(toString, Type)
expression Functor := F -> new Holder from { F }
precedence Functor := x -> 70

-- TODO: use codeHelpers to get code HH to work
-- TODO: get methods OO to work
methods' := lookup(methods, Symbol)
methods Functor := F -> (
    if F === HH then join(methods homology, methods cohomology) else methods' F)
-- TODO: perhaps give info about argument, subscript, superscript?
methodOptions Functor := F -> null

-----------------------------------------------------------------------------
-- id: the identity morphism
-----------------------------------------------------------------------------

id = new ScriptedFunctor from {
    subscript => X -> applyFunctor((id, class X), X),
    }

-----------------------------------------------------------------------------
-- HH: homology and cohomology
-----------------------------------------------------------------------------

-- TODO: change to Options => true?
  homology = method(Options => {})
cohomology = method(Options => {Degree => 0}) -- for local cohomology and sheaf cohomology

  homology(ZZ, Sequence) := opts -> (i, X) ->   homology(prepend(i, X), opts)
cohomology(ZZ, Sequence) := opts -> (i, X) -> cohomology(prepend(i, X), opts)

HH = new ScriptedFunctor from {
    subscript => (
	i -> new ScriptedFunctor from {
	    -- HH_i^j X -> cohomology(i, j, X)
	    superscript => j -> new Functor from { argument => X -> cohomology args(i, j, X) },
	    -- HH_i X -> homology(i, X)
	    argument => X -> homology args(i, X)
	    }
	),
    superscript => (
	j -> new ScriptedFunctor from {
	    -- HH^j_i X -> homology(j, i, X)
	    subscript => i -> new Functor from { argument => X -> homology args(j, i, X) },
	    -- HH^j X -> cohomology(j, X)
	    argument => X -> cohomology args(j, X)
	    }
	),
    -- HH X -> homology X
    argument => X -> homology X
    }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
