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

-- container for computation results
Computation = new Type of MutableHashTable
Computation.synonym = "computation"

-- TODO: this might not be the best syntax, since the object is not always "new"
-- if there is a compatible computation stored in T.cache,
-- returns the computation container, otherwise creates the entry:
--   Context{} => Computation{ Result }
new Computation from HashTable := (C, T) -> (
    -- TODO: use new Computation from (HashTable, ...) to also give the Context, perhaps
    context := Context{};
    -- TODO: use https://github.com/Macaulay2/M2/issues/1596 when it is implemented
    if T.cache#?context then T.cache#context else T.cache#context = new Computation from { Result => null })

-----------------------------------------------------------------------------
-- isComputationDone
-----------------------------------------------------------------------------

-- determines whether further computation is necessary
isComputationDone = method(TypicalValue => Boolean, Options => true)
-- the default method only checks whether Result is non-null
isComputationDone Computation := Boolean => {} >> opts -> container -> container.Result =!= null

-----------------------------------------------------------------------------
-- updateComputation
-----------------------------------------------------------------------------

updateComputation = method(Options => true)
updateComputation(Computation, Thing)   := {} >> opts -> (container, result) -> container.Result = result
updateComputation(Computation, Nothing) := {} >> opts -> (container, result) -> null

-----------------------------------------------------------------------------
-- cacheComputation
-----------------------------------------------------------------------------

-- perform the computation, or return the cached result
cacheComputation = method(TypicalValue => CacheFunction, Options => true)
-- the default method only sets the Result in the container
-- Note: this function takes advantage of FunctionClosures by modifying the container
cacheComputation Computation := CacheFunction => {} >> opts -> container -> new CacheFunction from (
    algorithm -> (
	if isComputationDone(opts, container) then ( cacheHit container; container.Result )
	else updateComputation(opts, container, algorithm(opts, container))))

-----------------------------------------------------------------------------
-- Introspective tools for Computations
-----------------------------------------------------------------------------

debug  Computation :=      C -> null
status Computation := o -> C -> null

--
cacheHit = method()
cacheHit Computation := ZZ => C -> if debugLevel > 0 then (
    -- TODO: should we count the cache on C or T?
    if not ComputationDebugLevel#?(T := class C)
    then ComputationDebugLevel#T = ComputationCacheStats#T = 0;
    if ComputationDebugLevel#T > 0 then printerr("Cache hit on a ", synonym T, "! 🎉");
    ComputationCacheStats#T = ComputationCacheStats#T + 1)
