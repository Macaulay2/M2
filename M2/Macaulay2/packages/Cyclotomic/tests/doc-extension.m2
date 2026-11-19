-- doc-extension.m2 -- independent tests from the documentation alone.
-- cyclotomicExtension(R, N): the polynomial ring with the monoid of R
-- (variables, degrees, monomial order) over cyclotomicField(N, K), K the
-- coefficient field of R; R itself if K contains the N-th roots of unity.
-- The ring is made once for every R and field; cyclotomicInclusion(R, T)
-- is the inclusion.

sameMonoid = (R, T) -> (
    assert(numgens T == numgens R);
    assert(apply(gens T, toString) == apply(gens R, toString));
    assert(degrees T == degrees R);
    assert((options T).MonomialOrder === (options R).MonomialOrder);
    assert((options T).Degrees === (options R).Degrees);
    -- the monomial order really is the same: leading terms agree
    phi := cyclotomicInclusion(R, T);
    m := gens R;
    polys := {sum m, product m + sum(m, v -> v^2), (sum m)^3 - product m,
        (first m) * (last m)^2 + (last m) * (first m)^2 + (first m)^3};
    for f in polys do (
        assert(leadMonomial phi f == phi leadMonomial f);
        assert(phi(f) - phi(leadTerm f) == phi(f - leadTerm f));
        assert(degree phi f == degree f));
    -- sorting of monomials agrees
    -- all monomials of total exponent at most 3
    exps := {{}};
    for v in m do exps = flatten apply(exps, e -> apply(4, i -> append(e, i)));
    exps = select(exps, e -> sum e <= 3);
    mons := apply(exps, e -> product(#m, i -> (m#i)^(e#i)));
    assert(apply(sort mons, f -> phi f) == sort apply(mons, f -> phi f));
    assert(apply(rsort mons, f -> phi f) == rsort apply(mons, f -> phi f));
    phi);

checkExtension = (R, N, expectedField) -> (
    T := cyclotomicExtension(R, N);
    assert(cyclotomicExtension(R, N) === T);
    K := coefficientRing R;
    if expectedField === K then (assert(T === R); return T);
    assert(coefficientRing T === expectedField);
    sameMonoid(R, T);
    T);

orders = {
    {},
    {MonomialOrder => Lex},
    {MonomialOrder => GLex},
    {MonomialOrder => GRevLex},
    {MonomialOrder => {Weights => {1, 2, 3}}},
    {MonomialOrder => {Lex => 1, GRevLex => 2}},
    {MonomialOrder => {GRevLex => 2, Lex => 1}},
    {MonomialOrder => Eliminate 1},
    {Degrees => {2, 3, 5}},
    {Degrees => {2, 3, 5}, MonomialOrder => Lex},
    {Degrees => {{1, 0}, {0, 1}, {1, 1}}},
    {Degrees => {{1, 0}, {0, 1}, {1, 1}}, MonomialOrder => GLex}};
-- the polynomial ring K[x, y, z, opts]
mk = (K, opts) -> K (new Array from ({symbol x, symbol y, symbol z} | opts));
for opts in orders do (
    R := mk(QQ, opts);
    checkExtension(R, 3, cyclotomicField 3);
    checkExtension(R, 4, cyclotomicField 4);
    checkExtension(R, 2, QQ);
    checkExtension(R, 1, QQ);
    -- the same field: the same ring
    assert(cyclotomicExtension(R, 3) === cyclotomicExtension(R, 6));
    assert(cyclotomicExtension(R, 5) === cyclotomicExtension(R, 10));
    -- over a cyclotomic field
    S := mk(cyclotomicField 3, opts);
    checkExtension(S, 4, cyclotomicField 12);
    checkExtension(S, 6, cyclotomicField 3);
    checkExtension(S, 3, cyclotomicField 3);
    assert(cyclotomicExtension(S, 4) === cyclotomicExtension(S, 12));
    -- over ZZ/p and Galois fields
    P := mk(ZZ/5, opts);
    checkExtension(P, 4, ZZ/5);
    T := checkExtension(P, 3, cyclotomicField(3, ZZ/5));
    assert(cyclotomicExtension(P, 8) === T and cyclotomicExtension(P, 24) === T);
    G := mk(GF 9, opts);
    checkExtension(G, 5, cyclotomicField(5, coefficientRing G));
    checkExtension(G, 8, coefficientRing G);
    N := mk(GF(ZZ/3[c]/(c^2+1)), opts);
    checkExtension(N, 5, cyclotomicField(5, coefficientRing N));
    checkExtension(N, 4, coefficientRing N);
    );

-- indexed variables and many variables
R = QQ[x_1 .. x_6, MonomialOrder => Lex];
T = checkExtension(R, 5, cyclotomicField 5);
assert(toString T_5 == "x_6");
R = (ZZ/2)[a, b, c, d, Degrees => {1, 2, 3, 4}];
T = checkExtension(R, 7, cyclotomicField(7, ZZ/2));
assert(T =!= R);
-- one variable
R = QQ[t];
T = checkExtension(R, 12, cyclotomicField 12);
-- the coefficients of the image
F = cyclotomicField 3; S = F[u, v];
T = cyclotomicExtension(S, 4);
phi = cyclotomicInclusion(S, T);
assert(phi(F_0 * S_0) == cyclotomicRoot(cyclotomicField 12, 1/3) * T_0);
-- the documented example
R = QQ[x, y, Degrees => {2, 3}, MonomialOrder => Lex];
T = cyclotomicExtension(R, 3);
assert(degrees T == {{2}, {3}});
assert(cyclotomicExtension(R, 3) === T);
assert(cyclotomicExtension(R, 2) === R);
