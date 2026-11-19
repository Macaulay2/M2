-- -*- coding: utf-8 -*-
--  cyclotomic.m2
--
--  Copyright (C) 2009-12, 2026 Thomas Kahle <thomas-kahle@gmx.de>
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or (at
--  your option) any later version.
--
--  This program is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with this program; if not, see <https://www.gnu.org/licenses/>.
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newPackage(
    "Cyclotomic",
    Version => "2.0",
    Date => "October 2026",
    Authors => {{Name => "Thomas Kahle",
            Email => "thomas.kahle@ovgu.de",
            HomePage => "https://thomas-kahle.de/"}},
    Keywords => {"Algebraic Number Theory"},
    Headline => "cyclotomic fields and roots of unity",
    PackageImports => {"ConwayPolynomials"},
    AuxiliaryFiles => true
    )

export {
    -- the functions of version 1.0
    "cyclotomicField",
    "cyclotomicPoly",
    "findRootPower",
    "joinCyclotomic",
    -- roots of unity
    "cyclotomicRoot",
    "rootOfUnityGenerator",
    "rootOfUnityExponent",
    "rootOfUnityOrder",
    "discreteLog",
    -- fields and rings
    "cyclotomicInclusion",
    "cyclotomicExtension",
    "galoisAutomorphism"
    }

------------------------------------------------------------------------------
-- Conventions
--
-- e(q) = exp(2 pi i q) for q in QQ.  The generator ww_n of cyclotomicField n
-- is e(1/n).  Every field K that the package handles has a finite cyclic
-- group of roots of unity (in characteristic p: of order prime to p) with a
-- fixed generator, rootOfUnityGenerator K, which is e(1/w) for w the order
-- of the group:
--   QQ                      w = 2,          generator -1
--   cyclotomicField n       w = n (n even), generator ww_n
--                           w = 2n (n odd), generator -ww_n^((n+1)/2)
--   ZZ/p, GF(p^n)           w = p^n - 1,    generator gamma, a root of the
--                           Conway polynomial of degree n (the least
--                           primitive root modulo p for ZZ/p, the variable
--                           of GF(p, n), found by factoring for a Galois
--                           field defined by another polynomial)
-- In characteristic p the choice of gamma is what e(q) means:
-- e(k/w) = gamma^k.  Conway polynomials make these choices compatible with
-- the inclusions GF(p^n) -> GF(p^m), gamma_n -> gamma_m^((p^m-1)/(p^n-1)).
------------------------------------------------------------------------------

ww := getSymbol "ww"

------------------------------------------------------------------------------
-- Cyclotomic fields and polynomials
------------------------------------------------------------------------------

-- The coefficients (lowest degree first) of the n-th cyclotomic polynomial,
-- by Moebius inversion: Phi_n is the product of (x^d - 1)^mu(n/d) over the
-- divisors d of n.  The factors with mu = 1 are multiplied first; then every
-- division by an x^d - 1 is exact and is the recurrence q_j = q_(j-d) - c_j.
-- For n not squarefree Phi_n(x) = Phi_r(x^(n/r)) with r the radical of n.
-- The coefficients are integers, so Phi_n(v) is defined over every
-- coefficient ring of v.
divisorsOf = n -> sort fold((ds, f) -> flatten apply(ds, d -> apply(f#1 + 1, e -> d * f#0^e)), {1}, toList factor n)
cyclotomicCoefficients = memoize (n -> (
    primes := apply(select(toList factor n, f -> f#0 > 1), f -> f#0);
    r := product(primes, identity);
    if r != n then (
        -- spread the coefficients of Phi_r: Phi_n(x) = Phi_r(x^(n/r))
        s := n // r;
        cr := cyclotomicCoefficients r;
        return flatten apply(#cr, j -> if j == #cr - 1 then {cr#j} else {cr#j} | toList(s - 1 : 0)));
    mu := d -> (-1)^(#select(primes, q -> (n // d) % q == 0));
    ds := divisorsOf n;
    c := {1};
    scan(select(ds, d -> mu d == 1), d -> (z := toList(d : 0); c = (z | c) - (c | z)));
    scan(select(ds, d -> mu d == -1), d -> (
            m := #c - d;
            q := new MutableList from toList(m : 0);
            for j from 0 to m - 1 do q#j = (if j >= d then q#(j - d) else 0) - c#j;
            c = toList q));
    c))

cyclotomicPoly = (i,v) -> (
     -- returns the i-th cyclotomic polynomial in variable v.
     -- v must be a variable a priori
     v = value v;
     if i <= 0 then error "the input should be > 0.";
     c := cyclotomicCoefficients i;
     sum(#c, j -> if c#j == 0 then 0_(ring v) else c#j * v^j))

-- the order n of the fields made by cyclotomicField n, recorded when they are
-- made
cyclotomicFieldOrder = new MutableHashTable

-- One field per order n (memoized as in version 1.0).
cyclotomicFieldOfOrder = memoize (n -> (
     Q := QQ(monoid [ww_n]);
     F := toField ((Q) / cyclotomicPoly (n, Q_0));
     cyclotomicFieldOrder#F = n;
     F))

-- n with n = 2 mod 4 replaced by n/2: the order under which the package makes
-- the cyclotomic field of order n and 2n (they are the same field)
canonicalOrder = n -> if n % 4 == 2 then n // 2 else n

------------------------------------------------------------------------------
-- Finite fields
------------------------------------------------------------------------------

-- the least primitive root modulo the prime p
leastPrimitiveRoot = p -> (
    if p == 2 then return 1;
    qs := apply(toList factor(p - 1), f -> f#0);
    r := 2;
    while any(qs, q -> powermod(r, (p - 1) // q, p) == 1) do r = r + 1;
    r)

-- Is the Galois field F defined by its Conway polynomial?
isConwayField = F -> (
    cp := conwayPolynomial(char F, F.degree);
    if cp === null then return false;
    f := (ideal ambient F)_0;
    (map(ring cp, ring f, {(ring cp)_0})) f == cp)

-- A root in the Galois field F of the Conway polynomial of its degree, found
-- by factoring the polynomial over F.  Conway polynomials are primitive, so
-- the root generates the multiplicative group of F.  Of the roots the one
-- that factor lists first is taken.
conwayRoot = F -> (
    (p, n) := (char F, F.degree);
    cp := conwayPolynomial(p, n);
    if cp === null then error("no Conway polynomial for GF(" | toString p | "," | toString n | ") is known: the generator of the field cannot be fixed");
    S := F(monoid [Variables => 1]);
    z := S_0;
    linear := select(toList factor ((map(S, ring cp, {z})) cp), fa -> first degree fa#0 == 1);
    if #linear == 0 then error "conwayRoot: internal error, the Conway polynomial has no root in the field";
    g := (first linear)#0;
    lift(z - g / leadCoefficient g, F))

-- (p, n, gamma) for a finite field F = ZZ/p or a Galois field GF(p^n): the
-- characteristic, the degree over ZZ/p and the generator gamma; null for any
-- other ring.  (F.PrimitiveElement is not used: it lives in the ambient
-- quotient ring, and for some Galois fields it is F_0 whether or not that is
-- primitive.)
finiteFieldCache = new MutableHashTable
finiteFieldData = F -> (
    if finiteFieldCache#?F then return finiteFieldCache#F;
    p := char F;
    finiteFieldCache#F = (
        if p == 0 then null
        else if instance(F, GaloisField) then (p, F.degree, if isConwayField F then F_0 else conwayRoot F)
        else if isFinitePrimeField F then (p, 1, (leastPrimitiveRoot p)_F)
        else (
            -- a field toField(ZZ/p[c]/(f)): through the Galois field
            -- GF(ZZ/p[c]/(f)) of the same presentation, identified by c -> c
            G := galoisFieldOfPresentation F;
            if G === null then null
            else (
                (p', n, gammaG) := finiteFieldData G;
                (p, n, (map(F, G, {F_0})) gammaG)))))

-- For a field F = toField(ZZ/p[c]/(f)) that is not a GaloisField: the
-- GaloisField GF(ZZ/p[c]/(f)) of the same presentation; null for any other
-- ring.  (factor does not work over F itself, so the Conway root is found in
-- the Galois field and moved back by c -> c.)
galoisFieldOfPresentation = F -> (
    if char F == 0 or instance(F, GaloisField) or isFinitePrimeField F then return null;
    if not (isField F and F.?baseRings and #F.baseRings > 0) then return null;
    A := last F.baseRings;
    if not (instance(A, QuotientRing) and isFinitePrimeField coefficientRing A
            and numgens A == 1 and numgens ideal A == 1) then return null;
    try GF A else null)

notFiniteMessage = "in positive characteristic the field must be finite, ZZ/p or a Galois field made by GF"

-- the multiplicative order of p modulo N
multiplicativeOrder = (p, N) -> (
    if N == 1 then return 1;
    if gcd(p, N) != 1 then error "multiplicativeOrder: p and N are not coprime";
    m := 1;
    q := p % N;
    while q != 1 do (q = (q * p) % N; m = m + 1);
    m)

-- The Galois field GF(p, m) made from its Conway polynomial, with generator
-- ww_(p^m - 1); one ring per (p, m).
galoisFieldOfDegree = memoize ((p, m) -> (
    if conwayPolynomial(p, m) === null then error("no Conway polynomial for GF(" | toString p | "," | toString m | ") is known: the field cannot be made");
    F := GF(p, m, Variable => ww_(p^m - 1));
    if not isConwayField F then error("no Conway polynomial for GF(" | toString p | "," | toString m | ") is known: the field cannot be made");
    F))

-- The baby-step tables of the Pohlig-Hellman algorithm for the field F.  For
-- every prime q that divides the group order N = p^n - 1 with multiplicity e
-- the tuple (q, e, gamma^(N/q^e), g_q = gamma^(N/q), m, table, g_q^(-m)) with
-- m = ceiling of the square root of q and the table g_q^j => j for j < m.
-- The tables do not depend on the element whose logarithm is taken, so they
-- are made once per field.  A prime factor above discreteLogLimit would need
-- a table of more than 2^19 elements and as many giant steps per logarithm.
discreteLogLimit = 2^38
discreteLogCache = new MutableHashTable
discreteLogData = F -> (
    if discreteLogCache#?F then return discreteLogCache#F;
    (p, n, gamma) := finiteFieldData F;
    N := p^n - 1;
    discreteLogCache#F = apply(toList factor N, fa -> (
            (q, e) := (fa#0, fa#1);
            if q > discreteLogLimit then error("the discrete logarithm in a field with " | toString N
                | " nonzero elements is out of reach: the prime factor " | toString q | " of this number is larger than 2^38");
            gq := gamma^(N // q);
            m := floor sqrt(q * 1.0);
            while m * m < q do m = m + 1;
            while (m - 1) * (m - 1) >= q do m = m - 1;
            table := new MutableHashTable;
            x := 1_F;
            for j from 0 to m - 1 do (table#x = j; x = x * gq);
            (q, e, gamma^(N // q^e), gq, m, table, x^(-1)))))

-- the solution of x = r_i modulo m_i for a list of pairs (r_i, m_i) with
-- pairwise coprime moduli, in the range 0 .. prod m_i - 1
chineseRemainder = pairs -> (
    (a, m) := (0, 1);
    scan(pairs, pair -> (
            (b, n) := pair;
            (g, u, v) := toSequence gcdCoefficients(m, n);   -- u m + v n = 1
            a = (a * v * n + b * u * m) % (m * n);
            m = m * n));
    a)

------------------------------------------------------------------------------
-- Fields: recognition, order of the group of roots of unity, generator
------------------------------------------------------------------------------

-- The order n of a field F made by cyclotomicField n, or recognised as
-- QQ[w]/Phi_n with w = e(1/n); 1 for QQ; null otherwise.  A generator of a
-- number field of degree d that is a root of unity has order at most 2 d^2,
-- which bounds the search.
cyclotomicOrderOf = F -> (
    if F === QQ then return 1;
    if cyclotomicFieldOrder#?F then return cyclotomicFieldOrder#F;
    if F.?cache and F.cache#?(symbol cyclotomicOrderOf) then return F.cache#(symbol cyclotomicOrderOf);
    n := null;
    if char F == 0 and F.?baseRings then (
        A := last F.baseRings;
        if instance(A, QuotientRing) and coefficientRing A === QQ and numgens A == 1 and numgens ideal A == 1 then (
            f := (ideal A)_0;
            d := first degree f;
            w := F_0;
            p := w;
            for e from 1 to 2 * d^2 do (
                if p == 1 then (n = e; break);
                p = p * w);
            -- the field is QQ(zeta_n) presented by Phi_n only if the degree is phi(n)
            if n =!= null and d != #select(toList(1 .. n), j -> gcd(j, n) == 1) then n = null));
    if F.?cache then F.cache#(symbol cyclotomicOrderOf) = n;
    n)

-- The order w of the group of roots of unity of the field K, or null if the
-- package does not handle K.
rootOfUnityGroupOrder = K -> (
    if char K > 0 then (
        data := finiteFieldData K;
        if data === null then null else data#0^(data#1) - 1)
    else (
        n := cyclotomicOrderOf K;
        if n === null then null else if n % 2 == 0 then n else 2 * n))

checkField = K -> if rootOfUnityGroupOrder K === null then error(
    if char K > 0 then notFiniteMessage
    else "expected QQ or a cyclotomic field made by cyclotomicField")

------------------------------------------------------------------------------
-- Roots of unity
------------------------------------------------------------------------------

rootOfUnityGenerator = method()
rootOfUnityGenerator Ring := RingElement => K -> (
    checkField K;
    if char K > 0 then return (finiteFieldData K)#2;
    n := cyclotomicOrderOf K;
    if n == 1 then return -1_K;
    w := K_0;
    if n % 2 == 0 then w else -w^((n + 1) // 2))

-- We would have liked to call this function rootOfUnity, but that name is
-- taken: MatrixFactorizations exports the symbol rootOfUnity and uses it as a
-- key (S.rootOfUnity).  Renamed to cyclotomicRoot on 2026-11-19.
cyclotomicRoot = method()
cyclotomicRoot(Ring, QQ) := RingElement => (K, q) -> (
    checkField K;
    w := rootOfUnityGroupOrder K;
    k := q * w;
    if denominator k != 1 then error("the field does not contain e(" | toString q | "), a root of unity of order " | toString denominator(q/1));
    (rootOfUnityGenerator K)^(numerator k % w))
cyclotomicRoot(Ring, ZZ) := RingElement => (K, q) -> cyclotomicRoot(K, q/1)
cyclotomicRoot QQ := RingElement => q -> (
    N := canonicalOrder denominator q;
    cyclotomicRoot(if N == 1 then QQ else cyclotomicFieldOfOrder N, q))
cyclotomicRoot ZZ := RingElement => q -> cyclotomicRoot(q/1)

-- the discrete logarithm k of x, x = gamma^k, in a finite field (Pohlig-
-- Hellman: the logarithm modulo q^e for every prime power in N = p^n - 1 is
-- found digit by digit in base q, each digit by one baby-step giant-step
-- search in the subgroup of order q; the Chinese remainder theorem combines
-- them)
discreteLog = method()
discreteLog RingElement := ZZ => x -> (
    F := ring x;
    data := finiteFieldData F;
    if data === null then error("discreteLog: " | notFiniteMessage);
    if x == 0 then error "discreteLog: zero has no logarithm";
    (p, n, gamma) := data;
    N := p^n - 1;
    if x == 1 then return 0;
    chineseRemainder apply(discreteLogData F, part -> (
            (q, e, g, gq, m, table, giant) := part;
            qe := q^e;
            h := x^(N // qe);   -- in the subgroup of order q^e generated by g
            k := 0;
            for j from 0 to e - 1 do (
                -- the j-th digit d: (h g^(-k))^(q^(e-1-j)) = gq^d
                t := (h * g^(-k))^(q^(e - 1 - j));
                d := null;
                for i from 0 to m do (
                    if table#?t then (d = i * m + table#t; break);
                    t = t * giant);
                if d === null then error "discreteLog: internal error, no logarithm found";
                k = k + d * q^j);
            (k, qe))))

rootOfUnityExponent = method()
rootOfUnityExponent Number := QQ => x -> if x == 1 then 0/1 else if x == -1 then 1/2 else null
rootOfUnityExponent RingElement := QQ => x -> (
    K := ring x;
    if K === ZZ then K = QQ;
    x = promote(x, K);
    if x == 1 then return 0/1;
    if x == -1 and char K != 2 then return 1/2;
    if K === QQ then return null;
    if char K > 0 then (
        data := finiteFieldData K;
        if data === null then error("rootOfUnityExponent: " | notFiniteMessage);
        if x == 0 then return null;
        return discreteLog x / (data#0^(data#1) - 1));
    checkField K;
    w := rootOfUnityGroupOrder K;
    g := rootOfUnityGenerator K;
    p := 1_K;
    for k from 0 to w - 1 do (
        if x == p then return k / w;
        p = p * g);
    null)

rootOfUnityOrder = method()
-- integers and rational numbers: the roots of unity of QQ are 1 and -1
rootOfUnityOrder Number := ZZ => x -> if x == 1 then 1 else if x == -1 then 2 else null
rootOfUnityOrder RingElement := ZZ => x -> (
    K := ring x;
    if K === ZZ then K = QQ;
    x = promote(x, K);
    if x == 0 then return null;
    if x == 1 then return 1;
    w := if char K > 0 or cyclotomicOrderOf K =!= null then rootOfUnityGroupOrder K;
    if w =!= null then (
        -- the roots of unity of K are exactly the w-th roots of unity
        if x^w != 1 then return null;
        o := w;
        scan(toList factor w, fa -> while o % fa#0 == 0 and x^(o // fa#0) == 1 do o = o // fa#0);
        return o);
    if char K > 0 then error("rootOfUnityOrder: " | notFiniteMessage);
    -- another number field QQ[a]/(f): a root of unity of degree d over QQ
    -- has order at most 2 d^2
    A := if instance(K, QuotientRing) then K else if K.?baseRings and #K.baseRings > 0 then last K.baseRings;
    if not (instance(A, QuotientRing) and coefficientRing A === QQ and numgens A == 1 and numgens ideal A == 1) then
        error "rootOfUnityOrder: expected an element of QQ, of a number field QQ[a]/(f), or of a finite field";
    d := first degree (ideal A)_0;
    p := x;
    for e from 1 to 2 * d^2 do (
        if p == 1 then return e;
        p = p * x);
    null)

------------------------------------------------------------------------------
-- Fields with roots of unity, inclusions, extensions
------------------------------------------------------------------------------

cyclotomicField = method()
cyclotomicField ZZ := Ring => n -> (
    if n <= 0 then error "cyclotomicField: expected a positive integer";
    cyclotomicFieldOfOrder n)
cyclotomicField(ZZ, Ring) := Ring => (N, K) -> (
    if N <= 0 then error "cyclotomicField: expected a positive integer";
    checkField K;
    if char K > 0 then (
        (p, n, gamma) := finiteFieldData K;
        if N % p == 0 then error "cyclotomicField: there are no roots of unity of an order divisible by the characteristic";
        m := lcm(n, multiplicativeOrder(p, N));
        if m == n then K else galoisFieldOfDegree(p, m))
    else (
        M := canonicalOrder lcm(cyclotomicOrderOf K, N);
        if M == canonicalOrder cyclotomicOrderOf K then K
        else cyclotomicFieldOfOrder M))

-- the image of the generator of the field K in the field L containing it
generatorImage = (K, L) -> (
    if char K > 0 then (
        (p, n, gamma) := finiteFieldData K;
        (p', m, gamma') := finiteFieldData L;
        if p != p' or m % n != 0 then error "cyclotomicInclusion: the second field does not contain the first";
        if isFinitePrimeField K then return null;
        -- K_0 = gamma^k goes to the k-th power of gamma' ^ ((p^m - 1)/(p^n - 1))
        if L === K then return L_0;
        -- gamma' is a root of the Conway polynomial of degree m (also for a
        -- field L given by another polynomial), so this is a root of the
        -- Conway polynomial of degree n
        image := gamma'^((p^m - 1) // (p^n - 1));
        image^(if K_0 == gamma then 1 else discreteLog K_0))
    else (
        if K === QQ then return null;
        a := cyclotomicOrderOf K;
        b := cyclotomicOrderOf L;
        if b === null or lcm(rootOfUnityGroupOrder K, rootOfUnityGroupOrder L) != rootOfUnityGroupOrder L then
            error "cyclotomicInclusion: the second field does not contain the first";
        cyclotomicRoot(L, 1/a)))

cyclotomicInclusion = method()
cyclotomicInclusion(Ring, Ring) := RingMap => (A, B) -> (
    if char A != char B then error "cyclotomicInclusion: the rings have different characteristics";
    if isField A and isField B then (
        checkField A; checkField B;
        image := generatorImage(A, B);
        return if image === null then map(B, A) else map(B, A, {image}));
    -- polynomial rings: the variables in order, the coefficients by the
    -- inclusion of the coefficient fields; cached in A
    key := (symbol cyclotomicInclusion, B);
    if A.?cache and A.cache#?key then return A.cache#key;
    (K, L) := (coefficientRing A, coefficientRing B);
    checkField K; checkField L;
    if numgens A != numgens B then error "cyclotomicInclusion: the rings have different numbers of variables";
    image' := generatorImage(K, L);
    phi := map(B, A, gens B | (if image' === null then {} else {image'}));
    if A.?cache then A.cache#key = phi;
    phi)

cyclotomicExtension = method()
cyclotomicExtension(Ring, ZZ) := Ring => (R, N) -> (
    K := coefficientRing R;
    F := cyclotomicField(N, K);
    if F === K then return R;
    if not R.?cache then R.cache = new CacheTable;
    key := (symbol cyclotomicExtension, F);
    if not R.cache#?key then R.cache#key = F(monoid R);
    R.cache#key)

galoisAutomorphism = method()
galoisAutomorphism(Ring, ZZ) := RingMap => (K, k) -> (
    checkField K;
    if char K > 0 then (
        (p, n, gamma) := finiteFieldData K;
        if isFinitePrimeField K then return map(K, K);
        -- the k-th power of the Frobenius x -> x^p
        return map(K, K, {K_0^(p^(k % n))}));
    a := cyclotomicOrderOf K;
    if a == 1 then return map(K, K);
    if gcd(k, a) != 1 then error "galoisAutomorphism: the exponent must be prime to the order of the field";
    map(K, K, {K_0^(k % a)}))

------------------------------------------------------------------------------
-- findRootPower and joinCyclotomic
------------------------------------------------------------------------------

-- The order of the generator of the coefficient field of R (or of R itself
-- if R is a field), or 2 for QQ and for orders 1 and 2; an error if the
-- generator is not a root of unity.
findRootPower = R -> (
    F := if isField R then R else coefficientRing R;
    if F === QQ then return 2;
    if not F.?baseRings or #gens last F.baseRings == 0 then return 2;
    if #gens last F.baseRings > 1 then error "The coefficient field has more than one generator";
    r := rootOfUnityOrder F_0;
    if r === null then error "findRootPower: the generator of the coefficient field is not a root of unity";
    if r < 3 then 2 else r)

-- Joins ideals in polynomial rings over QQ and cyclotomic fields (or over
-- finite fields of one characteristic) with the same variables: maps them
-- into one ring with the monoid of the first ring, over the cyclotomic
-- field of the lcm of the orders (as in version 1.0), or over the Galois
-- field of the lcm of the degrees.
joinCyclotomic = li -> (
    if #li == 0 then return li;
    rings := apply(li, ring);
    R := first rings;
    names := apply(gens R, toString);
    scan(rings, S -> if apply(gens S, toString) != names then error "joinCyclotomic: the rings have different variables");
    fields := apply(rings, coefficientRing);
    p := char R;
    if any(fields, K -> char K != p) then error "joinCyclotomic: the coefficient fields have different characteristics";
    F := if p > 0 then (
            ffData := apply(fields, K -> (d := finiteFieldData K; if d === null then error("joinCyclotomic: " | notFiniteMessage); d));
            m := lcm apply(ffData, d -> d#1);
            if all(fields, K -> K === first fields) then first fields
            else if m == 1 then first fields
            else galoisFieldOfDegree(p, m))
        else (
            leastcm := lcm for K in fields list (l := findRootPower K; if l > 2 then l else 1);
            if leastcm < 3 then QQ else cyclotomicFieldOfOrder leastcm);
    if not R.?cache then R.cache = new CacheTable;
    key := (symbol joinCyclotomic, F);
    S := if coefficientRing R === F then R else (
        if not R.cache#?key then R.cache#key = F(monoid R);
        R.cache#key);
    apply(li, I -> (cyclotomicInclusion(ring I, S)) I))

-- End of source code ---

beginDocumentation()

doc ///
    Key
        Cyclotomic
    Headline
        cyclotomic fields and roots of unity
    Description
        Text
            This package constructs cyclotomic fields over @TO QQ@ and the fields with roots of unity over
            finite fields, computes with roots of unity in them, and moves ideals between polynomial rings
            over such fields.  It is used by the package @TO "Binomials::Binomials"@ to construct the
            extensions of the coefficient field that binomial primary decomposition needs.

            {\bf Conventions.}  For a rational number $q$ let $e(q) = \exp(2\pi i q)$.  The generator
            {\tt ww_n} of @TO cyclotomicField@ $n$ is $e(1/n)$.  Every field $K$ the package handles has a
            finite cyclic group of roots of unity; @TO rootOfUnityGenerator@ $K$ is its generator
            $e(1/w)$, where $w$ is its order: $w = 2$ for $\mathbb{Q}$, $w = n$ for cyclotomicField $n$ with
            $n$ even, $w = 2n$ for $n$ odd.  In characteristic $p$, for $K = \mathbb{Z}/p$ or a Galois field
            $GF(p^n)$, the group is the multiplicative group of order $w = p^n - 1$, and its generator
            $\gamma$ is a root of the Conway polynomial of degree $n$: the least primitive root modulo $p$,
            the variable of {\tt GF(p,n)}, or, for a Galois field defined by another polynomial, a root of
            the Conway polynomial found by factoring.  A field {\tt toField(ZZ/p[c]/(f))} with $f$
            irreducible is handled like {\tt GF(ZZ/p[c]/(f))}, the Galois field of the same presentation.
            In characteristic $p$ this choice is what $e(q)$ means: $e(k/w) = \gamma^k$.  Conway
            polynomials make these choices compatible with the inclusions $GF(p^n) \subset GF(p^m)$.
        Example
            cyclotomicRoot(1/6)
            ring oo
            F = cyclotomicField(8, ZZ/5)
            r = cyclotomicRoot(F, 1/8)
            r^8, r^4
            discreteLog r
        Text
            Version 2.0 (2026) keeps the four functions of version 1.0 (@TO cyclotomicField@,
            @TO cyclotomicPoly@, @TO findRootPower@, @TO joinCyclotomic@) and fixes their bugs:
            @TO findRootPower@ did not stop for fields that are not cyclotomic, and @TO joinCyclotomic@
            forgot the monoid of the input rings, made a new ring on every call and mapped rings with
            different variables by position.  The other functions are new.
    Subnodes
        cyclotomicField
        cyclotomicPoly
        cyclotomicRoot
        rootOfUnityGenerator
        rootOfUnityExponent
        rootOfUnityOrder
        discreteLog
        cyclotomicInclusion
        cyclotomicExtension
        galoisAutomorphism
        findRootPower
        joinCyclotomic
///

doc ///
    Key
        cyclotomicField
        (cyclotomicField, ZZ)
        (cyclotomicField, ZZ, Ring)
    Headline
        cyclotomic fields, and fields with roots of unity
    Usage
        F = cyclotomicField n
        F = cyclotomicField(N, K)
    Inputs
        n:ZZ
            positive
        N:ZZ
            positive; in characteristic $p$ not divisible by $p$
        K:Ring
            @TO QQ@, a field made by {\tt cyclotomicField}, {\tt ZZ/p}, or a Galois field
    Outputs
        F:Ring
            {\tt cyclotomicField n} is $\mathbb{Q}(\zeta_n)$, with generator {\tt ww_n} $= e(1/n)$;
            {\tt cyclotomicField(N, K)} is the field generated by $K$ and the $N$-th roots of unity
    Description
        Text
            {\tt cyclotomicField n} is $\mathbb{Q}[ww_n]/\Phi_n$, made once for every $n$.  For odd $n$ the
            fields of order $n$ and $2n$ are equal, but {\tt cyclotomicField n} and
            {\tt cyclotomicField(2n)} are different rings, with generators $e(1/n)$ and $e(1/(2n))$.
        Example
            S = cyclotomicField 5
            r = S_0
            (r^4, r^5)
            cyclotomicField 5 === S
        Text
            {\tt cyclotomicField(N, K)} chooses one ring for every field.  In characteristic 0 it is
            $K$ if $K$ contains the $N$-th roots of unity, and otherwise {\tt cyclotomicField M} for the
            smallest possible order $M$, with $M = 2 \bmod 4$ replaced by $M/2$.  In characteristic $p$
            it is $K$ if $K$ contains them, and otherwise the Galois field $GF(p, m)$ made from its Conway
            polynomial, with generator {\tt ww_(p^m-1)}, where $m$ is the least common multiple of the
            degree of $K$ and the order of $p$ modulo $N$.
        Example
            cyclotomicField(6, QQ) === cyclotomicField 3
            cyclotomicField(4, cyclotomicField 3)
            cyclotomicField(3, ZZ/5)
            cyclotomicField(5, GF 9)
    Caveat
        In characteristic $p$ the result needs a Conway polynomial of degree $m$; for large $m$ none
        may be known.
    SeeAlso
        cyclotomicRoot
        cyclotomicExtension
        cyclotomicPoly
///

doc ///
    Key
        cyclotomicPoly
    Headline
        cyclotomic polynomials
    Usage
        f = cyclotomicPoly(n, v)
    Inputs
        n:ZZ
            positive
        v:RingElement
            a variable of a polynomial ring over any coefficient ring
    Outputs
        f:RingElement
            the $n$-th cyclotomic polynomial $\Phi_n(v)$, the minimal polynomial of $e(1/n)$ over
            @TO QQ@, with its integer coefficients taken in the coefficient ring
    Description
        Example
            R = QQ[w]
            cyclotomicPoly(6, w)
            cyclotomicPoly(105, w)
            S = (ZZ/7)[x,y];
            cyclotomicPoly(6, y)
        Text
            It is computed by Moebius inversion, $\Phi_n = \prod_{d | n} (x^d - 1)^{\mu(n/d)}$, on the
            coefficients: for $n = 4620$ in 0.06 seconds (version 1.0: 4.5 seconds).
    SeeAlso
        cyclotomicField
///

doc ///
    Key
        cyclotomicRoot
        (cyclotomicRoot, QQ)
        (cyclotomicRoot, ZZ)
        (cyclotomicRoot, Ring, QQ)
        (cyclotomicRoot, Ring, ZZ)
    Headline
        the root of unity e(q)
    Usage
        x = cyclotomicRoot q
        x = cyclotomicRoot(K, q)
    Inputs
        q:QQ
        K:Ring
            @TO QQ@, a cyclotomic field, {\tt ZZ/p} or a Galois field
    Outputs
        x:RingElement
            $e(q) = \exp(2\pi i q)$, in $K$, or without $K$ in {\tt cyclotomicField(N, QQ)} for the
            denominator $N$ of $q$
    Description
        Text
            It is an error if $K$ does not contain $e(q)$.  In characteristic $p$, $e(q)$ means
            $\gamma^{q (|K| - 1)}$ for the generator $\gamma$ = @TO rootOfUnityGenerator@ $K$.
        Example
            cyclotomicRoot(1/3)
            cyclotomicRoot(cyclotomicField 12, 1/3)
            cyclotomicRoot(1/2)
            K = GF 25;
            x = cyclotomicRoot(K, 1/3)
            x^3
    SeeAlso
        rootOfUnityExponent
        rootOfUnityOrder
        rootOfUnityGenerator
///

doc ///
    Key
        rootOfUnityGenerator
        (rootOfUnityGenerator, Ring)
    Headline
        the generator of the group of roots of unity of a field
    Usage
        g = rootOfUnityGenerator K
    Inputs
        K:Ring
            @TO QQ@, a cyclotomic field, {\tt ZZ/p} or a Galois field
    Outputs
        g:RingElement
            the generator $e(1/w)$ of the cyclic group of roots of unity of $K$, of order $w$
    Description
        Text
            See @TO Cyclotomic@ for the conventions.  For a cyclotomic field of odd order $n$ the group
            has order $2n$ and the generator is $-ww_n^{(n+1)/2}$.
        Example
            rootOfUnityGenerator QQ
            rootOfUnityGenerator cyclotomicField 3
            rootOfUnityGenerator cyclotomicField 4
            rootOfUnityGenerator(ZZ/7)
            rootOfUnityGenerator GF(ZZ/3[c]/(c^2+1))
    SeeAlso
        cyclotomicRoot
        discreteLog
///

doc ///
    Key
        rootOfUnityExponent
        (rootOfUnityExponent, RingElement)
        (rootOfUnityExponent, Number)
    Headline
        the exponent q of a root of unity e(q)
    Usage
        q = rootOfUnityExponent x
    Inputs
        x:RingElement
            in @TO QQ@, a cyclotomic field, {\tt ZZ/p} or a Galois field
    Outputs
        q:QQ
            the rational number $q$ with $0 \le q < 1$ and $x = e(q)$, or @TO null@ if $x$ is not a root
            of unity
    Description
        Example
            F = cyclotomicField 3;
            rootOfUnityExponent F_0
            rootOfUnityExponent(-F_0)
            rootOfUnityExponent(2*F_0)
            rootOfUnityExponent (6_(ZZ/7))
    SeeAlso
        cyclotomicRoot
        rootOfUnityOrder
        discreteLog
///

doc ///
    Key
        rootOfUnityOrder
        (rootOfUnityOrder, RingElement)
        (rootOfUnityOrder, Number)
    Headline
        the order of a root of unity
    Usage
        n = rootOfUnityOrder x
    Inputs
        x:RingElement
            in @TO QQ@, a number field $\mathbb{Q}[a]/(f)$ (with or without @TO toField@), {\tt ZZ/p}
            or a Galois field
    Outputs
        n:ZZ
            the least $n > 0$ with $x^n = 1$, or @TO null@ if there is none
    Description
        Text
            In a cyclotomic field and in a finite field the order is found from the factorization of
            the order of the group of roots of unity.  In another number field of degree $d$ a root of
            unity has order at most $2d^2$, which bounds the search; the function always stops.
        Example
            F = cyclotomicField 3;
            rootOfUnityOrder F_0, rootOfUnityOrder(-F_0), rootOfUnityOrder(2*F_0)
            K = toField(QQ[a]/(a^2+1));
            rootOfUnityOrder a
            rootOfUnityOrder (2_(ZZ/7))
    SeeAlso
        rootOfUnityExponent
        findRootPower
///

doc ///
    Key
        discreteLog
        (discreteLog, RingElement)
    Headline
        discrete logarithms in finite fields
    Usage
        k = discreteLog x
    Inputs
        x:RingElement
            a nonzero element of {\tt ZZ/p} or of a Galois field $K$
    Outputs
        k:ZZ
            the $k$ with $0 \le k < |K| - 1$ and $x = \gamma^k$, for $\gamma$ = @TO rootOfUnityGenerator@ $K$
    Description
        Text
            The algorithm of Pohlig and Hellman: the logarithm modulo every prime power $q^e$ in
            $|K| - 1$ is found digit by digit in base $q$, every digit by a baby-step giant-step search in
            the subgroup of order $q$, and the Chinese remainder theorem combines them.  The tables of the
            baby steps are made once per field.
        Example
            K = ZZ/32003;
            k = discreteLog 12345_K
            (rootOfUnityGenerator K)^k
            L = GF(2^20);
            discreteLog (L_0^1000 + 1)
    Caveat
        A prime factor of $|K| - 1$ above $2^{38}$ is out of reach and raises an error.
    SeeAlso
        rootOfUnityGenerator
        rootOfUnityExponent
///

doc ///
    Key
        cyclotomicInclusion
        (cyclotomicInclusion, Ring, Ring)
    Headline
        inclusions of fields with roots of unity, and of polynomial rings over them
    Usage
        phi = cyclotomicInclusion(A, B)
    Inputs
        A:Ring
            a field, or a polynomial ring over one
        B:Ring
            a field that contains $A$, or a polynomial ring over one with as many variables
    Outputs
        phi:RingMap
            the inclusion $A \to B$ that is compatible with the roots of unity: $e(q)$ goes to $e(q)$;
            for polynomial rings the variables go to the variables in order
    Description
        Example
            phi = cyclotomicInclusion(cyclotomicField 3, cyclotomicField 12)
            phi (cyclotomicField 3)_0 == cyclotomicRoot(cyclotomicField 12, 1/3)
            K = GF(ZZ/3[c]/(c^2+1));
            psi = cyclotomicInclusion(K, cyclotomicField(5, K));
            psi K_0
            (psi K_0)^2
            R = QQ[x,y]; T = cyclotomicExtension(R, 3);
            cyclotomicInclusion(R, T)
        Text
            A Galois field not defined by its Conway polynomial is embedded by sending its variable,
            which is $\gamma^k$ with $k$ its @TO discreteLog@, to the $k$-th power of the image of $\gamma$.
    SeeAlso
        cyclotomicExtension
        cyclotomicField
///

doc ///
    Key
        cyclotomicExtension
        (cyclotomicExtension, Ring, ZZ)
    Headline
        a polynomial ring over the extension by roots of unity
    Usage
        T = cyclotomicExtension(R, N)
    Inputs
        R:Ring
            a polynomial ring over @TO QQ@, a cyclotomic field, {\tt ZZ/p} or a Galois field $K$
        N:ZZ
            positive; in characteristic $p$ not divisible by $p$
    Outputs
        T:Ring
            the polynomial ring with the monoid of $R$ (variables, degrees, monomial order) over
            {\tt cyclotomicField(N, K)}; $R$ itself if $K$ contains the $N$-th roots of unity
    Description
        Text
            The ring is made once for every $R$ and field.  @TO cyclotomicInclusion@ $(R, T)$ is the
            inclusion.
        Example
            R = QQ[x,y, Degrees => {2,3}, MonomialOrder => Lex];
            T = cyclotomicExtension(R, 3)
            degrees T
            cyclotomicExtension(R, 3) === T
            cyclotomicExtension(R, 2) === R
    SeeAlso
        cyclotomicInclusion
        joinCyclotomic
///

doc ///
    Key
        galoisAutomorphism
        (galoisAutomorphism, Ring, ZZ)
    Headline
        automorphisms of fields with roots of unity
    Usage
        sigma = galoisAutomorphism(K, k)
    Inputs
        K:Ring
            a cyclotomic field, or {\tt ZZ/p} or a Galois field
        k:ZZ
            for a cyclotomic field of order $n$ prime to $n$
    Outputs
        sigma:RingMap
            for {\tt cyclotomicField n} the automorphism $\zeta_n \mapsto \zeta_n^k$; for a finite field of
            characteristic $p$ the $k$-th power of the Frobenius $x \mapsto x^p$
    Description
        Example
            F = cyclotomicField 5;
            sigma = galoisAutomorphism(F, 2)
            sigma F_0
            K = GF 25;
            (galoisAutomorphism(K, 1)) K_0 == K_0^5
    SeeAlso
        cyclotomicField
///

doc ///
    Key
        findRootPower
    Headline
        the order of the root of unity that generates a cyclotomic coefficient field
    Usage
        i = findRootPower R
    Inputs
        R:Ring
            a polynomial ring over a cyclotomic field, or the field itself
    Outputs
        i:ZZ
            the order of the generator of the field, or 2 if the field is @TO QQ@ (also for the orders 1
            and 2)
    Description
        Example
            S = cyclotomicField 5;
            findRootPower(S[x,y])
            findRootPower(QQ[x])
        Text
            Version 1.0 multiplied the generator with itself until 1 appeared, which did not stop for
            fields such as $\mathbb{Q}(\sqrt 2)$.  Now the order comes from @TO rootOfUnityOrder@, and it
            is an error if the generator is not a root of unity.
    SeeAlso
        rootOfUnityOrder
        cyclotomicField
///

doc ///
    Key
        joinCyclotomic
    Headline
        join ideals in polynomial rings over different cyclotomic fields
    Usage
        L = joinCyclotomic l
    Inputs
        l:List
            of ideals in polynomial rings with the same variables, over @TO QQ@ and cyclotomic fields, or
            over finite fields of one characteristic
    Outputs
        L:List
            the ideals in one polynomial ring, over the cyclotomic field whose order is the least common
            multiple of the orders of the coefficient fields, or over the Galois field whose degree is the
            least common multiple of the degrees
    Description
        Text
            The common ring has the monoid of the ring of the first ideal (its variables, degrees and
            monomial order) and is made once for every such ring and field.  Version 1.0 forgot the
            degrees and the monomial order, made a new ring on every call, and mapped rings with different
            variables by position; now that is an error.
        Example
            F = cyclotomicField 3; G = cyclotomicField 4;
            R = F[t]; I = ideal (t-F_0^2)
            S = G[t]; J = ideal (t^2-G_0)
            joinCyclotomic {I,J}
    SeeAlso
        cyclotomicInclusion
        cyclotomicExtension
///

TEST ///
F = cyclotomicField 3
G = cyclotomicField 4
R = F[t]
I = ideal (t-F_0^2)
S = G[t]
J = ideal (t^2-G_0)
assert (findRootPower coefficientRing ring (joinCyclotomic {I,J})#0 == 12)
///

TEST ///
-- https://github.com/Macaulay2/M2/issues/487
KK = QQ[r]/cyclotomicPoly(3,r);
S = KK[x_0..x_11];
A = matrix{
    {x_0 + r*x_1+r^2*x_2,0,0},
    {0,r*(x_0 + r*x_1+r^2*x_2),0},
    {0,0,r^2*(x_0 + r*x_1+r^2*x_2)}}
B = genericMatrix(S,x_3,3,3)
M = matrix{{A,B,0*id_(S^3)},{0*id_(S^3),r*A,r*B},{r^2*B,0*id_(S^3),r^2*A}}
det M -- used to segfault
///

end
