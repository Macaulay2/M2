M = matrix {{1},{0}}
N = matrix {{0},{1}}
assert(M // N == 0)

M = sub(matrix {{1},{0}}, QQ)
N = sub(matrix {{0},{1}}, QQ)
assert(M // N == 0)

M = sub(matrix {{1},{0}}, ZZ/32003)
N = sub(matrix {{0},{1}}, ZZ/32003)
assert(M // N == 0)

K = GF(32003^2)
M = sub(matrix {{1},{0}}, K)
N = sub(matrix {{0},{1}}, K)
assert(M // N == 0)

-- Example from Mike Stillman for factoring maps of non-free modules
-- see https://github.com/Macaulay2/M2/pull/3567#pullrequestreview-2638871494
S = ZZ/101[a..d]/(a^2+b^2+c^2+d^2)
F = res(coker vars S, LengthLimit => 10)
A = coker F.dd_4
B = coker F.dd_3
C = coker F.dd_5
HAB = Hom(A,B)
basis(0, HAB)
f = homomorphism random HAB
HBC = Hom(B,C)
basis(3, HBC)
gd = homomorphism random(3, HBC)
g = map(target gd ** S^{3}, source gd, gd)
assert(degree g == {0} and isHomogeneous g)
h = g*f
-- So: f : A --> B
--     g : B --> C ** S^{3}
--     h : A --> C ** S^{3}
assert(g * (h//g) == h)
assert(h // g == f) 
assert(f \\ h == g)
assert((f\\h) * f == h)
assert(g // h == 0) -- does it always happen that this is zero if lift can't occur?  Probably not
assert(h \\ f == 0)


-- tests which show that the "Reflexive" strategy doesn't always work
S = QQ[x_0,x_1,x_2] --/ sum(3, i -> x_i^3)
a = x_0 + x_1
b = x_0^2-x_0*x_1+x_1^2
A = matrix {{x_2, a}, {b, -x_2^2}}
B = matrix {{x_2^2, a}, {b, -x_2}}

f = map(S^2 / a, S^2 / a, A * B)
f' = map(S^2 / a, S^2, A * B)
g = map(S^2 / a, S^2, A)
h = map(S^2 / a, S^2 / a, B)
assert all({f', f, g, h}, isWellDefined)

-- f does not factor through g without a remainder
assert not isSubset(image homomorphism' f, image Hom(source f, g))

-- here is the example
q = f // g
assert(isWellDefined q and q == 0)

q = quotient(f, g, Strategy => Default)
assert(isWellDefined q and q == 0)

-- the Reflexive strategy is not applicable here
assert try ( quotient(f, g, Strategy => "Reflexive"); false ) else true

-- but f' does (left) factor through g
assert isSubset(image homomorphism' f, image Hom(g, target f))

q = g \\ f'
assert isWellDefined q
assert(f' == q * g)
assert(h == q)

q = quotient'(f', g, Strategy => Default)
assert isWellDefined q
assert(f' == q * g)
assert(h == q)

assert try ( quotient'(f', g, Strategy => "Reflexive"); false ) else true


-- this is an example where the improved "Reflexive" strategy works
f = homomorphism random Hom(image(A | B), S^2)
g = homomorphism random Hom(image(B | A), S^2)
h = homomorphism random Hom(image(A | B), image(B | A))
f = g * h
assert all({f, g}, isWellDefined)

-- f factors through g without a remainder
assert isSubset(image homomorphism' f, image Hom(source f, g))

-- here is the example
q = f // g
assert(isWellDefined q and f == g * q)

q = quotient(f, g, Strategy => Default)
assert(isWellDefined q and f == g * q)


-- this is an example where the improved "Reflexive" strategy works
R = ZZ/2[x,y,z,w]
g = matrix {{x, x+y+z}, {x+y+z, z}}
f = g * matrix {{y^2}, {z^2}}
f = inducedMap(target f / (x+y+z), source f / (x+y+z), f)
g = inducedMap(target g / (x+y+z), source g, g)
isWellDefined f
isWellDefined g

-- f does not factor through g without a remainder
assert not isSubset(image homomorphism' f, image Hom(source f, g))

q = quotient(f, g, Strategy => Default)
assert(isWellDefined q and q == 0)

assert try ( quotient(f, g, Strategy => "Reflexive"); false ) else true

----
clearAll
B = QQ[a..d]
f = prune map(B^1, image(map(B^{{-2}, 3:{-3}}, B^{{-3}, 3:{-4}, {-3}, 3:{-4}, {-3}, 3:{-4}, {-3}, 3:{-4}}, {{a, 0, 0, 0, b, 0, 0, 0, c, 0, 0, 0, d, 0, 0, 0}, {0, a, 0, 0, 0, b, 0, 0, 0, c, 0, 0, 0, d, 0, 0}, {0, 0, a, 0, 0, 0, b, 0, 0, 0, c, 0, 0, 0, d, 0}, {0, 0, 0, a, 0, 0, 0, b, 0, 0, 0, c, 0, 0, 0, d}})),{{a*b*c-a^2*d, a*b^3-a^3*c, a^2*c^2-a*b^2*d, a*c^3-a*b*d^2, b^2*c-a*b*d, b^4-a^2*b*c, a*b*c^2-b^3*d, b*c^3-b^2*d^2, b*c^2-a*c*d, b^3*c-a^2*c^2, a*c^3-b^2*c*d, c^4-b*c*d^2, b*c*d-a*d^2, b^3*d-a^2*c*d, a*c^2*d-b^2*d^2, c^3*d-b*d^3}})
g = prune map(B^1, image(map(B^1, B^{4:{-1}}, {{a, b, c, d}})), {{a, b, c, d}})
assert isSubset(image homomorphism' f, image Hom(source f, g))
elapsedTime assert(f == g * quotient(f, g, Strategy => Default))
elapsedTime assert(f == g * quotient(f, g, Strategy => "Reflexive"))

----
S = QQ[x]
f = random(S^1/x, S^1/x^3)
g = random(S^1/x, S^1/x^2)
elapsedTime assert(f == g * quotient(f, g, Strategy => Default))
elapsedTime assert(f == g * quotient(f, g, Strategy => "Reflexive"))

----
S = QQ[x,y];
M = coker matrix {{x,y},{-y,x}};
g = random(M, M)
f = g * random(M, M)
q = quotient(f, g, Strategy => "Reflexive")
assert(isWellDefined q and f == g * q)

----
S = QQ[x,y,z]
M = S^{-2}
N = S^{-3}
P = truncate(3, S^1)
h = homomorphism random(3, Hom(P, N))
g = homomorphism random(0, Hom(N, M))
f = g * h
assert all({f, g, h}, isWellDefined)
q = quotient(f, g, Strategy => "Reflexive")
assert(isWellDefined q and f == g * q)
