centerRing = method(Options => {Strategy => null})
centerRing(Ring) := RingMap => opts -> (R) -> R.cache.centerRing ??= (
    z := runHooks((centerRing, Ring), (opts, R), Strategy => opts.Strategy);
    if z =!= null then z else error "centerRing: no strategy for computing center of the given ring"
)

addHook((centerRing, Ring), Strategy => "isCommutative", (opts, R) -> if isCommutative R then map(R, R) else null)