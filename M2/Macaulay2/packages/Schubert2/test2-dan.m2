--*- coding: utf-8 -*-

base(3, Bundle => (E,4,e))
chern E
chern schur_{1,1,1,1} E
chern exteriorPower_4 E
assert( oo === ooo )

chern schur_{3,1} E
chern schur_{2,2} E
chern schur_{2,1,1} E

chern schur_{4} E
chern symmetricPower_4 E
assert( oo === ooo )

chern schur_{1,1,1,1,1} E
chern exteriorPower_5 E
assert( oo === ooo )

base(3, Bundle => (E,3,e),Bundle => (F,3,f))
P = exteriorPower_4 (E+F)
Q = exteriorPower_4 E + (exteriorPower_3 E) * F + (exteriorPower_2 E) * (exteriorPower_2 F) + E * (exteriorPower_3 F) + exteriorPower_4 F
assert( P === Q )

clearAll

compactMatrixForm = false
n = 5;
m = 2;
S = QQ[b_1 .. b_m, a_1 .. a_n, Degrees => {1 .. m, 1 .. n}, MonomialOrder => {m,n} ];
S[x];
f = x^n + sum(1 .. n, i -> a_i * x^(n-i))
g = x^m + sum(1 .. m, i -> b_i * x^(m-i))
I = ideal apply(flatten entries last coefficients(f % g),r -> lift(r,S))
isHomogeneous I
gI = gens gb I
leadTerm gI

clearAll

isSymmetric = method();
compactMatrixForm = false
n = 5;
m = 2;
R = QQ[β_1 .. β_m, a_1 .. a_n, Degrees => {m:1, 1 .. n}, MonomialOrder => {m:1,n} ];
isSymmetric R := r -> all (0 .. m-2, i -> r == sub( r, matrix { switch(i,i+1,gens R) } ));
S = R[x];
f = x^n + sum(1 .. n, i -> a_i * x^(n-i))
g = product(1 .. m, i -> x - β_i)
I = ideal apply(flatten entries last coefficients(f % g),r -> lift(r,R))
isHomogeneous I
gI = gens gb I;
transpose gI
leadTerm gI
T = R/I
isSymmetric T := t -> isSymmetric lift(t,R);
b = apply(flatten entries last coefficients g, r -> promote(lift(r,R),T));
netList toList apply(1 .. 8, j -> toList apply(0 .. j, i -> isSymmetric(b_1^i*b_2^(j-i))))

clearAll

compactMatrixForm = false
r = 2;
n = 2*r;
m = 2;
S = QQ[b_1 .. b_m, a_1 .. a_r, Degrees => {1 .. m, apply(1 .. r, i -> 2*i)}, MonomialOrder => {m,r} ];
T = S[x];
f = x^n + sum(1 .. r, i -> a_i * x^(n-2*i))
g = x^m + sum(1 .. m, i -> b_i * x^(m-i))
* T := h -> sub(h,{x=>-x});
J = ideal apply(flatten entries last coefficients(f % (g * *g)),r -> lift(r,S))
isHomogeneous J
gI = gens gb J;
leadTerm gI

clearAll

isSymmetric = method();
compactMatrixForm = false
r = 2;
n = 2*r;
m = 2;
R = QQ[β_1 .. β_m, a_1 .. a_r, Degrees => {m:1, apply(1 .. r, i -> 2*i)}, MonomialOrder => {Lex=>m,r} ];
isSymmetric R := r -> all (0 .. m-2, i -> r == sub( r, matrix { switch(i,i+1,gens R) } ));
T = R[x];
* T := h -> sub(h,{x=>-x});
f = x^n + sum(1 .. r, i -> a_i * x^(n-2*i))
g = product(1 .. m, i -> x - β_i)
I = ideal apply(flatten entries last coefficients(f % (g * *g)),r -> lift(r,R))
isHomogeneous I
gI = gens gb I;
transpose gI
leadTerm gI
U = R/I
isSymmetric U := t -> isSymmetric lift(t,R);
b = apply(flatten entries last coefficients (g), r -> promote(lift(r,R),U));
netList toList apply(0 .. 12, j -> toList prepend(j, apply(0 .. j, i -> if isSymmetric(b_1^i*b_2^(j-i)) then ".." else "NO")))

clearAll

compactMatrixForm = false
cyclic = n -> (
     x := local x;
     g := gens QQ(monoid [x_1 .. x_n, MonomialOrder => Lex]);
     ideal append(apply(1 .. n-1, i -> sum(1 .. n, j -> product(j-i .. j-1, k -> g_k))), product g - 1))
I = cyclic 6
transpose leadTerm gens gb I

--       |           48          |
-- o13 = |          x            |
--       |           6           |
--       |                       |
--       |                 2 12  |
--       |   1387545279120x x    |
--       |                 5 6   |
--       |                       |
--       |                  3 6  |
--       |   25438330117200x x   |
--       |                  5 6  |
--       |                       |
--       |                    6  |
--       |   1322793166094400x   |
--       |                    5  |
--       |                       |
--       |                     6 |
--       | 11905138494849600x x  |
--       |                   4 6 |
--       |                       |
--       |  5952569247424800x x  |
--       |                   4 5 |
--       |                       |
--       |                    3  |
--       |   1984189749141600x   |
--       |                    4  |
--       |                       |
--       |                    6  |
--       |   72152354514240x x   |
--       |                  3 6  |
--       |                       |
--       |  7936758996566400x x  |
--       |                   3 5 |
--       |                       |
--       | 23810276989699200x x  |
--       |                   3 4 |
--       |                       |
--       |                  3    |
--       |     801692827936x     |
--       |                  3    |
--       |                       |
--       |                     6 |
--       | 11905138494849600x x  |
--       |                   2 6 |
--       |                       |
--       | 11905138494849600x x  |
--       |                   2 5 |
--       |                       |
--       |  7936758996566400x x  |
--       |                   2 4 |
--       |                       |
--       | 23810276989699200x x  |
--       |                   2 3 |
--       |                       |
--       |                    2  |
--       |   3968379498283200x   |
--       |                    2  |
--       |                       |
--       |           x           |
--       |            1          |


-- test that github issue #2051 remains fixed
m=3
k=3
n=5
Gr=flagBundle({k,n-k})
B=bundles Gr
F=(dual det first B)^m
toBlow=sectionZeroLocus(F)
bl=blowup (toBlow.StructureMap)
for i from 0 to dim first bl list chi(exteriorPower(i,cotangentBundle(first bl)))
assert Equation{{1, -2, 4, -4, 4, -2, 1}, oo}
