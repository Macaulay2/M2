-----------------------------------------------------------------------------
-- Copyright 2021 Mahrud Sayrafi
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program.  If not, see <https://www.gnu.org/licenses/>.
-----------------------------------------------------------------------------
-*
  A Computation object is a container for the partial results of a computation.

  Given that multiple simultaneous computations can be cached at the same time
  (e.g. saturation with respect to different ideals), a Context object is
  necessary to distinguish computations.

  A computation cached in object X is stored in the CacheTable X.cache as:
    Context{ ... } => Computation{ Result, ... }
*-

needs "classes.m2"
needs "methods.m2"

ComputationDebugLevel := new MutableHashTable from {}
ComputationCacheStats := new MutableHashTable from {}

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Computation type declarations and basic constructors
-----------------------------------------------------------------------------

-- context for computation objects
Context = new SelfInitializingType of BasicList
Context.synonym = "computation context"

-- TODO: support options?
new Context from HashTable := (C, T) -> new C from {}

-- container for computation results
Computation = new Type of MutableHashTable
Computation.synonym = "computation"

new Computation from HashTable := (C, T) -> new C from { Result => null }

-----------------------------------------------------------------------------
-- fetchComputation
-----------------------------------------------------------------------------

-- TODO: not very happy with the inputs ... is there a better way?
-- if there is a compatible computation stored in T.cache,
-- returns the computation container, otherwise creates the entry:
--   Context => Computation
-- TODO: how should one look for other compatible contexts as well?
fetchComputation = method(Options => true)
fetchComputation(Type, HashTable,            Context) := Computation => true >> opts -> (C, T,    context) -> fetchComputation(C, T, T, context)
fetchComputation(Type, HashTable, Sequence,  Context) :=
fetchComputation(Type, HashTable, HashTable, Context) := Computation => true >> opts -> (C, T, X, context) -> T.cache#context ??= new C from X

-----------------------------------------------------------------------------
-- isComputationDone
-----------------------------------------------------------------------------

-- determines whether further computation is necessary
isComputationDone = method(TypicalValue => Boolean, Options => true)
-- the default method only checks whether Result is non-null
isComputationDone Computation := Boolean => true >> opts -> container -> container.Result =!= null
-- TODO: use isReady?

-----------------------------------------------------------------------------
-- updateComputation
-----------------------------------------------------------------------------

-- update the computation with the given result
updateComputation = method(Options => true)
updateComputation(Computation, Thing)   := true >> opts -> (container, result) -> container.Result = result
updateComputation(Computation, Nothing) := true >> opts -> (container, result) -> null

-----------------------------------------------------------------------------
-- adjustComputation
-----------------------------------------------------------------------------

-- adjust and return the cached result as appropriate based on opts
adjustComputation = method(Options => true)
adjustComputation Computation := true >> opts -> container -> container.Result

-----------------------------------------------------------------------------
-- cacheComputation
-----------------------------------------------------------------------------

-- perform the computation, or return the cached result
cacheComputation = method(Options => true)
-- the default method only sets the Result in the container
-- Note: this function takes advantage of FunctionClosures by modifying the container
cacheComputation Computation := Function => true >> opts -> container -> (
    algorithm -> (
	if isComputationDone(opts, container) then ( cacheHit container;
	    adjustComputation(opts, container))
	else updateComputation(opts, container, algorithm(opts, container))))

-----------------------------------------------------------------------------
-- Introspective tools for Computations
-----------------------------------------------------------------------------

-- toggle printing debug information or collecting data about cache hits
debug  Computation :=      C -> ComputationDebugLevel#(hash C) = ComputationDebugLevel#(hash C) ^^ 1
-- print the status of the computation, or cache hit statistics
status Computation := o -> C -> ComputationCacheStats#(hash C)

--
cacheHit = method()
cacheHit Computation := ZZ => C -> if debugLevel > 0 then (
    if not ComputationDebugLevel#?(T := hash C)
    then ComputationDebugLevel#T = ComputationCacheStats#T = 0;
    if ComputationDebugLevel#T & 1 > 0 then printerr("Cache hit on a ", synonym class C, "!");
    ComputationCacheStats#T = ComputationCacheStats#T + 1)
