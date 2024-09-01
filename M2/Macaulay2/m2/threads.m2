-- Copyright 2024 by Mahrud Sayrafi

-- TODO: unreachable tasks should be cancelled

needs "methods.m2"

-- TODO: move these to the interpreter
async = method(Dispatch => Thing)
async Function := Function => f -> x -> schedule(f, x)

-- TODO: what other data structures can hold tasks?
await = method(Dispatch => Thing)
await Task      := Thing     => await @@ taskResult
await Thing     := Thing     => identity
await BasicList := BasicList => L -> apply(L, await)
-- TODO: should this also replace the keys and handle conflicts?
await HashTable        := HashTable => H -> applyValues(H, await)
-- TODO: applyValues should take a mutable hash table, and perhaps modify it in place?
await MutableHashTable := MutableHashTable => H -> ( scan(keys H, k -> H#k = await H#k); H )
