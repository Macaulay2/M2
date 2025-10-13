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
