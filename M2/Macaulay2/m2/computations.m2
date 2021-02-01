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
fetchComputation = method(Options => true)
fetchComputation(Type, HashTable, Context) := Computation => true >> opts -> (C, T, context) -> (
    -- TODO: look for other compatible contexts as well
    -- TODO: use https://github.com/Macaulay2/M2/issues/1596 when it is implemented
    if T.cache#?context then T.cache#context
    else T.cache#context = new C from T)

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
-- cacheComputation
-----------------------------------------------------------------------------

-- perform the computation, or return the cached result
cacheComputation = method(TypicalValue => CacheFunction, Options => true)
-- the default method only sets the Result in the container
-- Note: this function takes advantage of FunctionClosures by modifying the container
cacheComputation Computation := CacheFunction => true >> opts -> container -> new CacheFunction from (
    algorithm -> (
	if isComputationDone(opts, container) then ( cacheHit container; container.Result )
	else updateComputation(opts, container, algorithm(opts, container))))

-----------------------------------------------------------------------------
-- Introspective tools for Computations
-----------------------------------------------------------------------------

-- enable printing debug information or collecting data about cache hits
debug  Computation :=      C -> null
-- print the status of the computation, or cache hit statistics
status Computation := o -> C -> null

--
cacheHit = method()
cacheHit Computation := ZZ => C -> if debugLevel > 0 then (
    -- TODO: should we count the cache on C or T?
    if not ComputationDebugLevel#?(T := class C)
    then ComputationDebugLevel#T = ComputationCacheStats#T = 0;
    if ComputationDebugLevel#T > 0 then printerr("Cache hit on a ", synonym T, "! 🎉");
    ComputationCacheStats#T = ComputationCacheStats#T + 1)
