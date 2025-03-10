--debug needsPackage "Dmodules"

-- Orders
-- Weighted lexicographic order x_1 > .. > x_n > dx_1 > .. > dx_n (Not an elimination order)
WeightThenLexicographicOrder = w -> join( { Weights => w },
    apply(entries id_(ZZ^(#w)), w' -> Weights => w'))

-- Weighted lexicographic elimination order dx_1 > .. > dx_n > x_1 > .. > x_n
WeightThenEliminationOrder = w -> join( { Weights => w },
    apply(flatten reverse pack_(#w//2) entries id_(ZZ^(#w)), w' -> Weights => w'))
--D = QQ[x,y,dx,dy, WeylAlgebra => {x=>dx, y=>dy}, MonomialOrder => WeightThenEliminationOrder {0,0,1,1}]
--D = QQ[x,y,dx,dy, WeylAlgebra => {x=>dx, y=>dy}, MonomialOrder => WeightThenLexicographicOrder {0,0,1,1}]
--1 + dx + dy + dx^2 + dx*dy + dy^2 + x*(1 + dx + dy + dx^2 + dx*dy + dy^2)

-- Weyl Algebra with monomial order given by the weighted lexicographic elimination order
makeWeylAlgebra(PolynomialRing, List) := opts -> (R, w) -> (
    coordVars := gens R;
    diffVars := apply(coordVars, i -> value("symbol d" | toString(i)) );
    allVars := join(coordVars, diffVars);
    W := (coefficientRing R)(monoid [allVars,
	    WeylAlgebra   => apply(coordVars, diffVars, (x,dx) -> x => dx),
	    MonomialOrder => WeightThenEliminationOrder w ]);
    if opts.SetVariables then use W;
    W)

-- Weyl Algebra with non-weighted lexicographic elimination order
makeWeylAlgebra(PolynomialRing) := opts -> R -> (
    n := length(gens R);
    w := for i from 1 to 2*n list 0;
    W := makeWeylAlgebra(R, w);
    W)

-- Fraction field K(x) of a Weyl algebra K[x,dx]/(...)
fractionField = memoize(D -> if class(coefficientRing(D)) === FractionField then (
                    frac(coefficientRing(coefficientRing(D))[(gens coefficientRing D) | (drop(gens D, - (numgens D)//2))])
                )
                else frac extractVarsAlgebra(D)
);


-- Graded associative ring of the rational Weyl algebra
-- Used for bookkeeping elements in R
rationalWeylAlgebra = memoize((D) -> (
    createDpairs D;
    w := (((options(D)).MonomialOrder)#1)#1;
    R := fractionField(D);
    (R)(monoid[D.dpairVars#1,
	    MonomialOrder => WeightThenLexicographicOrder last pack_(#w//2) w ]))
)
--            MonomialOrder => { Weights => w } ]))

-- reduce the lead term in rational Weyl algebra R

reduceOneStep = method()
reduceOneStep(RingElement, RingElement) := (f, g) -> (
    if f == 0 then return f;
    D := ring g;
    w := (((options(D)).MonomialOrder)#1)#1;
    n := numgens D // 2;
    F := fractionField D;
    R := rationalWeylAlgebra(D);
    if R =!= ring f then f = sub(f, R);
    f0 := leadTerm(f);
    g0 := leadTerm(g);
    --fexp := unique exponents f0;
    --gexp := unique apply(exponents g0, e -> last pack_n e);
    fexp := (exponents f0)#0;
    gexp := (last pack_n (exponents g0)#0);
    -- as long as we refine with elimination lexicographic order, we don't need generic weights
    -- if #fexp > 1 or #gexp > 1 then error "expected generic weight order";
    -- RECURSION: if g0 does not divide f0, no reduction is necessary
    if not (gexp << fexp) then (return f0 + reduceOneStep(f-f0, g));
    -- compare weights of leading monomials
    -- scalar product of exponent vector with the weight of the d's
    fwt := sum(fexp, last pack_n w, times);
    gwt := sum(gexp, last pack_n w, times);
    if fwt < gwt then return f;
    -- find the coefficient to divide by
    fcoef := lift(f0 // R_(fexp), F);
    gcoef := lift(sub(g0, R) // R_(gexp), F);
    -- this must be in D to perform the derivative
    ddexp := fexp - gexp;
    ddmon := D_(toList(n:0) | ddexp);
    -- exponents on the x's are zeroes and exponents of the d's are ddexp
    -- recurse to normalize the lower order terms
--     -- FIXME: this should work, but doesn't:
--     -- f - fcoef / gcoef * sub(ddmon * g, R)
--     --    print netList {f, g, sub(ddmon * g, R), f % sub(ddmon * g, R)};
--     --    error 0;
    f % sub(ddmon * g, R)
    -- eliminates leading term of f and restarts with the remainder
    )

normalForm = method()
-- f in D, g in D, SST page 7
normalForm(RingElement, RingElement) := (f, g) -> (
    if f == 0 then return f;
    D := ring g;
    w := (((options(D)).MonomialOrder)#1)#1;
    n := numgens D // 2;
    F := fractionField D;
    R := rationalWeylAlgebra(D);
    if R =!= ring f then f = sub(f, R);
    f0 := leadTerm(f);
    g0 := leadTerm(g);
    --fexp := unique exponents f0;
    --gexp := unique apply(exponents g0, e -> last pack_n e);
    fexp := (exponents f0)#0;
    gexp := (last pack_n (exponents g0)#0);
    -- as long as we refine with elimination lexicographic order, we don't need generic weights
    -- if #fexp > 1 or #gexp > 1 then error "expected generic weight order";
    -- RECURSION: if g0 does not divide f0, no reduction is necessary
    if not (gexp << fexp) then (return f0 + normalForm(f-f0, g));
    -- compare weights of leading monomials
    -- scalar product of exponent vector with the weight of the d's
    fwt := sum(fexp, last pack_n w, times);
    gwt := sum(gexp, last pack_n w, times);
    if fwt < gwt then return f;
    -- find the coefficient to divide by
    fcoef := lift(f0 // R_(fexp), F);
    gcoef := lift(sub(g0, R) // R_(gexp), F);
    -- this must be in D to perform the derivative
    ddexp := fexp - gexp;
    ddmon := D_(toList(n:0) | ddexp);
    -- exponents on the x's are zeroes and exponents of the d's are ddexp
    -- recurse to normalize the lower order terms
--     -- FIXME: this should work, but doesn't:
--     -- f - fcoef / gcoef * sub(ddmon * g, R)
--     --    print netList {f, g, sub(ddmon * g, R), f % sub(ddmon * g, R)};
--     --    error 0;
    normalForm(f % sub(ddmon * g, R), g)
    -- eliminates leading term of f and restarts with the remainder
    )

-- use this for lifting from R to D
-- c.f. https://github.com/Macaulay2/M2/issues/3613
sub' = (g, D) -> if instance(g, D) then g else sum(listForm g,
    (e, c) -> sub(c, D) * sub((ring g)_e, D))

clearDenominators = (G, D) -> apply(G, g -> if instance(g, D) then g else sub'(g * lcm(denominator \ last \ listForm g), D))

normalForm(RingElement, List) := (f, G) -> (
    D := ring(G#0);
    w := (((options(D)).MonomialOrder)#1)#1;
    G = clearDenominators(G, D);
    useRecursiveVersion := false;
    -- iterated version:
    if useRecursiveVersion then (
        scan(G, g -> f = normalForm(f, g));
    ) else (
        haschanged := true;
        -- iterate as long as going through G does not give any change
        while haschanged do(
            fstart := sub(f, rationalWeylAlgebra(D));
            scan(G, g -> f = reduceOneStep(f, g));
            haschanged = not(fstart == f);
        );
    );
    f)
end--

--------------------------------------------------------

restart
needs "reduce.m2"


-----------------------------------------------------------------
-- EXAMPLE SECTION --
-----------------------------------------------------------------

-----------------------------------------------------------------
-- Example: Test construction of Weyl algebra with weighted Lex.
w = {0,0,1,1}
D = makeWeylAlgebra(QQ[x,y],w)

f = dx^2
--f = ((x+y)*dx)
g = x*dx+1

-- Reduce completely by g.
normalForm(f, g)          -- Output:  2 / x^2

-- Check leadterm:
leadTerm inw(x*dx+y*dy,w)       -- Output: x*dx
--------------------------------------------------------
--------------------------------------------------------





--------------------------------------------------------
-- OLD CODE TO CHECK IMPLEMENTATION --------------------
--------------------------------------------------------

R = rationalWeylAlgebra(D)
f2 = (x_R)^(-1)*dx_R + (y_R)^(-1)*dy_R
f = (x_R)^(-4)*dx_R^2 + (y_R)^(-1)*dy_R
use D
g = (x+y)*dy
h = (x_R+y_R)^(-1)*dx_R^2+x_R^(-1)*dx_R*dy_R
h0 = inw(h,wR)
terms(h0)
use D

-- not correct definition of Weyl Algebra
D = QQ[x,y,dx,dy, WeylAlgebra =>{x=>dx,y=>dy},MonomialOrder=>{Weights=>{0,0,2,1}, RevLex}, Global => false]
I = ideal(x*dx^2-y*dy^2+dx-dy,x*dx+y*dy+1)
G = gb I
leadTerm(I)
normalForm(D, {0,0,1,2},dx_R,flatten entries gens G)
normalForm(D, {0,0,2,1},dy_R^2,flatten entries gens G)
assert ((x_R)^(-4)*dx_R^2 == normalForm(D, {0,0,1,1},f,g))
assert((x_R)^(-4)*dx_R^2 == normalForm(D, {0,0,5,1},f,g))
assert(y_R^(-1)*dy_R == normalForm(D, {0,0,4,17},(x_R)^(-4)*dx_R^2+y_R^(-1)*dy_R,7*dx))
assert(-dy_R == normalForm(D, {0,0,5,4},(x_R)^(-4)*dx_R^5+y_R^(-1)*dx_R^3-dy_R,7*dx))
assert(0 == normalForm(D, {0,0,4,17},(x_R)^(-4)*dx_R^2+y_R^(-1)*dy_R,{7*dx,dy}))
assert((x_R)^(-4)*dx_R^2+y_R^(-1)*dy_R == normalForm(D, {0,0,1,1},(x_R)^(-4)*dx_R^2+y_R^(-1)*dy_R,{x^2*dx*dy-5*x*dx+8,x*dx^2*dy^3-dx*dy^2}))
f = 2*x_R^(-2)
w = {0,0,2,1}
g = (flatten entries gens G)#1

----------------------------------------------------------------------------------------------------------------
