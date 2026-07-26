center = method(Options => {Strategy => null})
center(Ring) := RingMap => opts -> (R) -> R.cache.center ??= (
    z := runHooks((center, Ring), (opts, R), Strategy => opts.Strategy);
    if z =!= null then z else error "center: no strategy for computing center of the given ring"
)

addHook((center, Ring), Strategy => "isCommutative", (opts, R) -> if isCommutative R then map(R, R) else null)