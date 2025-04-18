-----------------------------------------------------------------------------
-- AtomicInt
-----------------------------------------------------------------------------

AtomicInt.synonym = "atomic integer"

scan({symbol +=, symbol -=, symbol &=, symbol |=, symbol ^^=},
    op -> typicalValues#(op, AtomicInt) = ZZ)

store = method()
store(AtomicInt, ZZ) := atomicStore

exchange = method()
exchange(AtomicInt, ZZ) := atomicExchange

compareExchange = method()
compareExchange(AtomicInt, ZZ, ZZ) := atomicCompareExchange

-----------------------------------------------------------------------------
-- Mutex
-----------------------------------------------------------------------------

Mutex.synonym = "mutex"
globalAssignment Mutex
net Mutex := x -> toString (
    if hasAttribute(x, ReverseDictionary)
    then getAttribute(x, ReverseDictionary)
    else x)

lock = method()
lock Mutex := lock0

tryLock = method()
tryLock Mutex := tryLock0

unlock = method()
unlock Mutex := unlock0

-----------------------------------------------------------------------------

parallelApplyRaw = (L, f) ->
     -- 'reverse's to minimize thread switching in 'taskResult's:
     reverse (taskResult \ reverse apply(L, e -> schedule(f, e)));
parallelApply = method(Options => {Strategy => null})
parallelApply(BasicList, Function) := o -> (L, f) -> (
     if o.Strategy === "raw" then return parallelApplyRaw(L, f);
     n := #L;
     numThreads := min(n + 1, maxAllowableThreads);
     oldAllowableThreads := allowableThreads;
     if allowableThreads < numThreads then allowableThreads = numThreads;
     numChunks := 3 * numThreads;
     res := if n <= numChunks then toList parallelApplyRaw(L, f) else
	  flatten parallelApplyRaw(pack(L, ceiling(n / numChunks)), chunk -> apply(chunk, f));
     allowableThreads = oldAllowableThreads;
     res);
