-- R = QQ[b_1 .. b_m, a_1 .. a_n, Degrees => {1 .. m, 1 .. n}, MonomialOrder => {m,n} ];
-- S = R[x];
-- f = x^n + sum(1 .. n, i -> a_i * x^(n-i))
-- g = x^m + sum(1 .. m, i -> b_i * x^(m-i))
-- I = ideal apply(flatten entries last coefficients(f % g),r -> lift(r,R))
-- isHomogeneous I
-- gI = gens gb I
-- leadTerm gI

compactMatrixForm = false
n = 5;
m = 2;
R = QQ[¦Â_1 .. ¦Â_m, a_1 .. a_n, Degrees => {m:1, 1 .. n}, MonomialOrder => {m:1,n} ];
isSymmetric = method();
isSymmetric R := r -> all (0 .. m-2, i -> r == sub( r, matrix { switch(i,i+1,gens R) } ));
S = R[x];
f = x^n + sum(1 .. n, i -> a_i * x^(n-i))
g = product(1 .. m, i -> x - ¦Â_i)
I = ideal apply(flatten entries last coefficients(f % g),r -> lift(r,R))
isHomogeneous I
gI = gens gb I;
transpose gI
leadTerm gI
T = R/I
isSymmetric T := t -> isSymmetric lift(t,R);
isSymmetric ¦Â_1
isSymmetric sum (1 .. m, i -> ¦Â_i)
isSymmetric sum (1 .. m, i -> ¦Â_i^3)
isSymmetric sum (1 .. m, i -> ¦Â_i^5)
b = apply(flatten entries last coefficients g, r -> promote(lift(r,R),T));
netList toList apply(1 .. 8, j -> toList apply(0 .. j, i -> isSymmetric(b_1^i*b_2^(j-i))))
