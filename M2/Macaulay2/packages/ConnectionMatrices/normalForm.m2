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
makeWeylAlgebra(PolynomialRing, List) := opts -> (R, v) -> (
    n := numgens R;
    w := if #v == n   then toList(#v:0) | v
    else if #v == 2*n then v
    else error("expected a weight of length ", n, " or ", 2*n);
    coordVars := gens R;
    diffVars := apply(coordVars, i -> value("symbol d" | toString(i)) );
    allVars := join(coordVars, diffVars);
    W := (coefficientRing R)(monoid [allVars,
	    WeylAlgebra   => apply(coordVars, diffVars, (x,dx) -> x => dx),
	    MonomialOrder => WeightThenEliminationOrder w ]);
    if opts.SetVariables then use W;
    W)

-- This causes a crash
-- Weyl Algebra with non-weighted lexicographic elimination order
makeWeylAlgebra(PolynomialRing) := opts -> R -> (
    n := length(gens R);
    v := (for i from 1 to n list 0);
    W := makeWeylAlgebra(R, v);
    W)

-- TODO: fix in Core
-- R = frac(QQ[x])[y]; baseName x_R
baseName' = x -> try baseName x else baseName' lift(x, baseRing ring x)

flattenBaseMonoid = (K, D) -> (
    -- given QQ and frac(QQ[e])[a,b,c][x,y,dx,dy]
    -- extract monoid[x,y,a,b,c,e]
    createDpairs D; -- e.g. {{x, y}, {dx, dy}, {}}
    -- extract generators all the way to K, remove differentials
    L := generators(D, CoefficientRing => K) - set D.dpairVars#1;
    monoid[Variables => L / baseName'])

-- Fraction field K(x) of a Weyl algebra K[x,dx]/(...)
-- TODO: implement this as frac(D) for Weyl algebras
baseFractionField = method()
baseFractionField FractionField  := FractionField => identity
baseFractionField PolynomialRing := FractionField => D -> D.baseFractionField ??= (
    -- given frac(QQ[e])[a,b,c][x,y,dx,dy]
    -- extract frac(QQ[x,y,a,b,c,e])
    if not isWeylAlgebra D then return frac D;
    -- find the ultimate coefficient ring (in this case QQ)
    K := ultimate(coefficientRing, D);
    F := frac K flattenBaseMonoid_K D;
    F.dpairVars = D.dpairVars;
    -- TODO: think of a better name
    F#"OriginalWeylAlgebra" = D;
    F)

-- Infers the WeylAlgebra from the fraction field
-- TODO: Make this more standard
-- Warning: this may forget the weight order!
inferWeylAlgebra = F -> (
    if instance(F, FractionField)  then (
	try F#"OriginalWeylAlgebra" else makeWeylAlgebra baseRing F) else
    if instance(F, PolynomialRing) then makeWeylAlgebra F
    else error "can't infer a Weyl algebra from ring that is neither a fraction field nor a polynomial ring."
)

-- Graded associative ring of the rational Weyl algebra
-- Used for bookkeeping elements in R
rationalWeylAlgebra = memoize((D) -> (
    createDpairs D;
    w := (((options(D)).MonomialOrder)#1)#1;
    R := baseFractionField(D);
    (R)(monoid[D.dpairVars#1,
	    MonomialOrder => WeightThenLexicographicOrder last pack_(#w//2) w ]))
)

-- reduce the lead term in rational Weyl algebra R
reduceOneStep = method()
reduceOneStep(RingElement, RingElement) := (f, g) -> (
    if f == 0 then return f;
    D := ring g;
    w := (((options(D)).MonomialOrder)#1)#1;
    n := numgens D // 2;
    F := baseFractionField D;
    R := rationalWeylAlgebra(D);
    if R =!= ring f then f = sub(f, R);
    f0 := leadTerm(f);
    g0 := leadTerm(sub(g,R));
    --fexp := unique exponents f0;
    --gexp := unique apply(exponents g0, e -> last pack_n e);
    fexp := (exponents f0)#0;
    gexp := (last pack_n (exponents g0)#0);
    -- as long as we refine with elimination lexicographic order, we don't need generic weights
    -- if #fexp > 1 or #gexp > 1 then error "expected generic weight order";
    if not (gexp << fexp) then return f;
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
    -- Computing S-pair
    f - fcoef / gcoef * sub(ddmon * g, R)
    )

-- Normal form with respect to single element g.
normalForm = method()
-- f in D, g in D, SST page 7
normalForm(RingElement, RingElement) := (f, g) -> (
    if f == 0 then return f;
    D := ring g;
    w := (((options(D)).MonomialOrder)#1)#1;
    n := numgens D // 2;
    F := baseFractionField D;
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

-- Normal Form w.r.t. a Groebner Basis
normalForm(RingElement, List) := (f, G) -> (
    if f == 0 then return f;
    D := ring(G#0);
    w := (((options(D)).MonomialOrder)#1)#1;
    G = clearDenominators(G, D);
    haschanged := true;
    -- iterate as long as going through G does not give any change
    while haschanged do(
        fstart := sub(f, rationalWeylAlgebra(D));
        scan(G, g -> f = reduceOneStep(f, g));
        haschanged = not(fstart == f);
        );
    f = leadTerm(f) + normalForm(f-leadTerm(f), G);
    f)

end--
restart

needs "reduce.m2"
-- Examples for testing with connection matrices
-- Example 1.3: w = (0,0,2,1)
D = makeWeylAlgebra(QQ[x,y], w = {0,0,2,1});
I = ideal(x*dx^2-y*dy^2+dx-dy,x*dx+y*dy+1)
leadTerm(I)
R = rationalWeylAlgebra D
-- Gröbner basis of D_2I
G = gb I
-- P1: Worked before
-- first row P1 -- EQUAL
normalForm(dx_R,flatten entries gens G)
-- second row P1 -- EQUAL
normalForm(dx_R*dy_R,flatten entries gens G)

-- P2: WORKS NOW
-- first row P2 -- EQUAL
normalForm(dy_R,flatten entries gens G)
-- second row P2 -- EQUAL
normalForm(dy_R^2,flatten entries gens G)


-- Example 1.3: w = (0,0,1,2)
D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
I = ideal(x*dx^2-y*dy^2+dx-dy,x*dx+y*dy+1)
leadTerm(I)
R = rationalWeylAlgebra D
-- Gröbner basis of D_2I
G = gb I
-- P1:
-- first row P1 -- EQUAL
normalForm(dx_R,flatten entries gens G)
-- second row P1 -- EQUAL
normalForm(dx_R^2,flatten entries gens G)

-- P2:
-- first row P2 -- EQUAL
normalForm(dy_R,flatten entries gens G)
-- second row P2 -- EQUAL
normalForm(dx_R*dy_R,flatten entries gens G)

-- Example 1.4: w = (0,0,2,1)
D = makeWeylAlgebra(QQ[x,y], w = {0,0,2,1});
I = ideal(x*dx^2-y*dy^2+2*dx-2*dy,x*dx+y*dy+1)
leadTerm(I)
R = rationalWeylAlgebra D
-- Gröbner basis of D_2I
G = gb I
-- P1:
-- first row P1 -- EQUAL
normalForm(dx_R,flatten entries gens G)
-- second row P1 -- EQUAL
normalForm(dx_R*dy_R,flatten entries gens G)
-- P2:
-- first row P2 -- EQUAL
normalForm(dy_R,flatten entries gens G)
-- second row P2 -- EQUAL
normalForm(dy_R^2,flatten entries gens G)

-- w=(0,0,1,2)
D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
I = ideal(x*dx^2-y*dy^2+2*dx-2*dy,x*dx+y*dy+1)
leadTerm(I)
R = rationalWeylAlgebra D
-- Gröbner basis of D_2I
G = gb I
-- P1:
-- first row P1 -- EQUAL
normalForm(dx_R,flatten entries gens G)
-- second row P1 -- EQUAL
normalForm(dx_R^2,flatten entries gens G)
-- P2:
-- first row P2 -- EQUAL
normalForm(dy_R,flatten entries gens G)
-- second row P2 -- EQUAL
normalForm(dx_R*dy_R,flatten entries gens G)
