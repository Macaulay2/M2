S = ZZ/101 [a, b, c, d]
B = matrix {{0}, {0}, {0}, {0}, {0}, {0}, {a}, {0}, {0}, {0}, {c}, {0}}
B0 = submatrix(B,{6..11},)  -- BUG!!
B = submatrix(B,{6..11},{0})
assert(rank source B0 == 1)
assert(rank target B0 == 6)
assert(rank source B == 1)
assert(rank target B == 6)
assert(B == B0)

-- c.f. https://groups.google.com/g/macaulay2/c/P7t43OPwO0E/m/TStzslnIAAAJ
S = ZZ/101[a,b,c,d]
M = matrix"a,b;c,d"
assert(M_{0,1,0,1} == M | M) -- produces the matrix with two columns equal to the first column of M, as it should
assert(M^{0,1,0,1} == M || M) -- produces a matrix with a row of zeros followed by the first row of M -- wrong!

R = S[x]
N = coker matrix{{x}}
m = map(N++R^1, N++R^1, sub(M, R))
assert(m^{0} == map(N, N++R^1, {{a, b}}))
assert(m^{1} == map(R^1, N++R^1, {{c, d}}))
assert(m_{0} == map(N++R^1, N, {{a}, {c}}))
assert(m_{1} == map(N++R^1, R^1, {{b}, {d}}))

R = QQ[a..d];
M = image vars R ++ coker vars R
assert(M^{2} == map(R^{-1}, M, {{0, 0, 1, 0, 0}}))
assert(M_{2} == map(M, R^{-1}, {{0}, {0}, {1}, {0}, {0}}))

R = QQ[x,y,z]
m = map(image map(R^2, , {{x,y,0}, {0,0,z}}), , {{x},{2*y},{3*z}})
assert(m^{2,0} == map(image map(R^2, , {{0,x},{z,0}}), , {{3*z},{x}}))

M = coker m
n = id_M
assert all(numgens M, i -> same { target n_{i}, source n^{i}, M })
assert all(numgens M, i -> same { target n^{i}, source n_{i}, image M_{i} })

--
R = ZZ[x_1..x_12,y]
f = genericMatrix(R,3,4)
assert(source (f_{1,2}) == R^{-1,-1})
assert(target (f_{1,2}) == target f)
M1 = (target f)/(y * target f)
M2 = (source f)/(y * source f)
g = map(target f,M2,f)
h = map(M1,M2,f)
k = submatrix(g, {1})
assert(target k === target g)
l = submatrix(h, {1})
assert(target l === target h)
assert(source l == coker map(R^{-1}, , {{0, y, 0, 0}}))
m = submatrix(h, {1,2},{2,3})
assert(target m == coker map(R^2, , {{0, y, 0}, {0, 0, y}}))
assert(source m == coker map(R^{2:-1}, , {{0, 0, y, 0}, {0, 0, 0, y}}))
n = submatrix(h, {1,2}, )
assert(target n == coker map(R^2, , {{0, 0, y, 0}, {0, 0, 0, y}}))
assert(source n === source h)

-- test of submatrixByDegrees
R = QQ[a..d]
I = ideal"a2b-c3,abc-d3,ac2-bd2-cd2,abcd-c4"
C = res I
submatrixByDegrees(C.dd_2, (3,3),(6,6))
submatrixByDegrees(C.dd_2, ({3},{3}),({6},{6}))
submatrixByDegrees(C.dd_2, ({4},{4}),({},{}))
submatrixByDegrees(C.dd_2, ({3},{3}),({7},{7}))
F = source C.dd_2
-- rawSelectByDegrees(raw F, {-4}, {-3})
-- rawSelectByDegrees(raw F, {}, {8})

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test submatrix.out"
-- End:
