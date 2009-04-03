--*- coding: utf-8 -*-

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

--

compactMatrixForm = false
n = 5;
m = 2;
R = QQ[β_1 .. β_m, a_1 .. a_n, Degrees => {m:1, 1 .. n}, MonomialOrder => {m:1,n} ];
isSymmetric = method();
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
isSymmetric β_1
isSymmetric sum (1 .. m, i -> β_i)
isSymmetric sum (1 .. m, i -> β_i^3)
isSymmetric sum (1 .. m, i -> β_i^5)
b = apply(flatten entries last coefficients g, r -> promote(lift(r,R),T));
netList toList apply(1 .. 8, j -> toList apply(0 .. j, i -> isSymmetric(b_1^i*b_2^(j-i))))

--


compactMatrixForm = false
r = 6;
n = 2*r;
m = 3;
S = QQ[b_1 .. b_m, a_1 .. a_r, Degrees => {1 .. m, apply(1 .. r, i -> 2*i)}, MonomialOrder => {m,r} ];
T = S[x];
f = x^n + sum(1 .. r, i -> a_i * x^(n-2*i)))
g = x^m + sum(1 .. m, i -> b_i * x^(m-i))
* T := h -> sub(h,{x=>-x});
J = ideal apply(flatten entries last coefficients(f % (g * *g)),r -> lift(r,S))
isHomogeneous J
gI = gens gb J
leadTerm gI

--

compactMatrixForm = false
r = 6;
n = 2*r;
m = 3;
R = QQ[β_1 .. β_m, a_1 .. a_r, Degrees => {m:1, apply(1 .. r, i -> 2*i)}, MonomialOrder => {Lex=>m,r} ];
isSymmetric = method();
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
T = R/I
isSymmetric T := t -> isSymmetric lift(t,R);
isSymmetric β_1
isSymmetric sum (1 .. m, i -> β_i)
isSymmetric sum (1 .. m, i -> β_i^3)
isSymmetric sum (1 .. m, i -> β_i^5)
b = apply(flatten entries last coefficients g, r -> promote(lift(r,R),T));
netList toList apply(1 .. 10, j -> toList prepend(j, apply(0 .. j, i -> isSymmetric(b_1^i*b_2^(j-i)))))


--

cyclic = n -> (
     R := QQ(monoid [x_1 .. x_n]);
     
