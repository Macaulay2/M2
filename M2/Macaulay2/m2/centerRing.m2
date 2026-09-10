-*
computes the center of a ring R and returns the map from center(R) -> R

maybe this should just be called "center" but the term is ubiquitous across
mathematics and appears in several different. trying to merge them all into a
single method runs into the awkwardness of method options leaking out to all
the different overrides.
*-

centerRing = method(Options => {Strategy => null})
centerRing(Ring) := RingMap => opts -> (R) -> R.cache.centerRing ??= (
    z := runHooks((centerRing, Ring), (opts, R), Strategy => opts.Strategy);
    if z =!= null then z else error "centerRing: no strategy for computing center of the given ring"
)

addHook((centerRing, Ring), Strategy => "isCommutative", (opts, R) -> if isCommutative R then map(R, R) else null)
addHook((centerRing, Ring), Strategy => "isSkewAffineRing", (opts, R) -> (
    -- David Eisenbud observed this strategy for computing the center of a skew polynomial ring
    if not isPolynomialRing R then return null;
    if not isSkewAffineRing R then return null;

    (R', phi) := flattenRing R;
    -- get the natural prime coefficient ring
    kk := coefficientRing R';

    x := symbol x; -- for pairs of skew variables
    y := symbol y; -- for commuting variables
    skews := R'.SkewCommutative;
    n := #skews;
    xvars := flatten for i from 0 to n - 1 list
        for j from i + 1 to n - 1 list
            x_{i, j};
    ximages := flatten for i from 0 to n - 1 list
        for j from i + 1 to n - 1 list
            R'_(skews_i) * R'_(skews_j);

    commuters := toList (0..numgens R' - 1) - set skews;
    yvars := toList(y_0..y_(#commuters - 1));
    yimages := apply(commuters, i -> R'_i);
    S := kk[xvars, yvars, Degrees => apply(ximages| yimages, degree)];

    -- hack to lazy-load PushForward package only when this hook needs it
    PushForward := needsPackage "PushForward";
    pushFwd := value PushForward.Dictionary#"pushFwd";

    -- the ideal defining the center is the annihilator of R^1 as a module over the polynomial ring S
    K := annihilator pushFwd(map(R', S, ximages | yimages), R'^1);
    Z := S/K;
    Z.formation = FunctionApplication {centerRing, R};

    map(R, Z, phi^-1 matrix {ximages | yimages})
))