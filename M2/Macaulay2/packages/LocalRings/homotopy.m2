needsPackage("LocalRings", FileName => "../LocalRings.m2")
load "mike-linkage.m2"
printWidth = 0

S = kk[x,y,z]
M0 = ideal((gens ideal"z2,x3,y4,xy2,x3y") * random(S^5, S^4))
--M0 = ideal"-2520y4-926x3+12912x2y+880xy2+6416z2,8468y4-4548x3-5320x2y+4454xy2+4894z2,-641y4-3539x3+12531x2y+9104xy2+9082z2,9760y4+7853x3-85x2y-8513xy2+4256z2"
assert not isHomogeneous M0
res (I = generalLink M0)
--tally for i to 100 list betti res (I = generalLink M0)

R = S_(ideal vars S)
J = I ** R

J = ideal map(R^1,R^{{-3}, {-3}, {-3}},{{x^2*y-12373*x^2-8521*y^2, x*y^2+5019*x*y+3216*y^2-13233*z^2+3723*x, 13424*x^2*y+936*x*y^2+10667*y*z^2+14913*x^2-8521*x*y-15541*y^2-12289*x}})
J = ideal map(R^1,R^{{-3}, {-3}, {-3}}, {{
	    x*y^2-2521*x*y+3380*y^2-7314*z^2-5316*x,
	    x^2*y-3016*x^2-3793*y^2,
	    -5866*x^2*y-492*x*y^2-12514*y*z^2-12634*x^2-3793*x*y+4895*y^2-10918*x }})
F' = chainComplex { gens J, syz gens J, syz syz gens J }
F = res J
F.dd

f0 = J_1
phi0 = map(F_0, F_0, f0 * id_(F_0))
s0 = phi0 // F.dd_1
assert(F.dd_1 * s0 == phi0)

phi1 = map(F_1, F_1, f0 * id_(F_1))
s1 = (phi1 - s0 * F.dd_1) // F.dd_2
assert(F.dd_2 * s1 == phi1 - s0 * F.dd_1)

phi2 = map(F_2, F_2, f0 * id_(F_2))
s2 = (phi2 - s1 * F.dd_2) // F.dd_3
assert(F.dd_3 * s2 == phi2 - s1 * F.dd_2)

phi3 = map(F_3, F_3, f0 * id_(F_3))
s3 = (phi3 - s2 * F.dd_3) // F.dd_4
assert(F.dd_4 * s3 == phi3 - s2 * F.dd_3)

L = {s0, s1, s2, s3}
phi = map(F, F, i -> L#i, Degree => 1)
phi = map(F[1], F, i -> L#i)

needsPackage "Complexes"
F' = complex F
map(F', F', i -> L#i, Degree => 1)
map(F'[1], F', i -> L#i)
phi' = map(F'[1], F', L)
phi' = map(F', F', L, Degree => 1)
-- FIXME
isCommutative phi'
errorDepth=2
-- FIXME
-- isNullHomotopic phi'


end--
restart
errorDepth=2
needs "homotopy.m2"

assert(F.dd_2 * s1 == phi1 - s0 * F.dd_1) -- FIXED

-- tracking the problem
A = F.dd_2 * s1
F'.dd_2 * L#1
B = s0 * F.dd_1
A + B
C = (A + B)_{1}

s1_0
F.dd_2 * s1_0 + B_0
F.dd_2^{0} * s1_0
F.dd_2^{1} * s1_0
F.dd_2^{2} * s1_0

-- here is the issue
s1_1
F.dd_2 * s1_1 + B_1
F.dd_2^{0} * s1_1
F.dd_2^{1} * s1_1
F.dd_2^{2} * s1_1

errorDepth = 1
quotient(phi1 - s0 * F.dd_1, F.dd_2)

f = phi1 - s0 * F.dd_1
g = F.dd_2

-- Here is the cause of the first problem: this matrix is not column reduced
G = syz(f | g)
G_1 - (14120_R^-1*14120) * G_1
G_2 - (14120_R^-1*7556) * G_1

G0 = mutableMatrix (syz liftUp(f | g) ** R)
G = G0

i = 1

C = f_{i}
n = scan(numColumns G, j -> if isUnit G_(i,j) then break j)
u = -G_(i, n)^-1
C = u * matrix submatrix(G, {numColumns f..numRows G-1}, {n})
scan(n+1 .. numColumns G-1, j -> columnAdd(G, j, u * G_(i,j), n))
-- TODO: is this step necessary? simplify above and remove this
G = submatrix(G, ,{0..n-1, n+1..numColumns G-1})
C_0

F.dd_2 * C_0

E = syz(f_{1} | g)
E = syz(f_{1}^{0} | g^{0})
E' = (14120*y+10073)_R^-1 * E_1^{1..3}
g * E' + B_1

E = syz(f_{2} | g)
E = syz(f_{2}^{0} | g^{0})
E' = -(11679*y+11439)_R^-1 * E_1^{1..3}
g * E' + B_2


-- old, bad value
s' = -9184_R^-1 * vector{0,0,1_R}
g * s' + B_1
