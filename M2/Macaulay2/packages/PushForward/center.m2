-- help: what is the right way to add a strategy to a core method that relies on functionality in a package?
addHook((center, Ring), Strategy => "isSkewAffineRing", (opts, R) -> (
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

    Z' := kk[xvars, yvars, Degrees => apply(ximages| yimages, degree)];
    -- the ideal defining the center is the annihilator of R^1 as a module over the polynomial ring Z'
    K := ann pushFwd(map(R', Z', ximages | yimages), R'^1);
    Z := Z'/K;
    Z.formation = FunctionApplication {center, R};

    map(R, Z, phi^-1 matrix {ximages | yimages})
))