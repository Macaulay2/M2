-- map accepts ring homomorphisms, including ones only defined on a subring
f = map(ZZ/5, QQ)
assert(f(1/2) == 3)
assert(try f(1/5) then false else true)
f = map(ZZ, QQ)
assert(f(4/2) == 2)
assert(try f(1/2) then false else true)
assert((map(QQ, RR_53)) 0.5 == 1/2)
R = QQ[x]
S = ZZ/5[x]
assert((map(S, R)) (R_0/2) == 3*S_0)

-- every ring maps to the zero ring
Z = (ZZ/5)[x]/1
map(Z, Z)
f = map(Z, ZZ/7)
assert(f 1 == 0)
assert(dim Z == -1)

-- when no subring of the source maps to the target, map lifts coefficients
-- to integers between -p/2 and p/2, which isn't a ring homomorphism
f = map(ZZ, ZZ/5)
assert(f 4 == -1)
assert(f 2 + f 2 != f 4)
assert((map(ZZ/7, ZZ/5)) 4_(ZZ/5) == -1)
V = (ZZ/5)[z]
W = ZZ[z]
assert((map(W, V, {V_0 => W_0})) (4*V_0) == -W_0)

-- substitute behaves the same way
assert(sub(4_(ZZ/5), ZZ) == lift(4_(ZZ/5), ZZ))
assert(ring sub(4_(ZZ/5), ZZ/7) === ZZ/7)
assert(sub(matrix {{1/2}}, ZZ/5) == matrix(ZZ/5, {{3}}))
T = ZZ[y]
U = (ZZ/5)[y]
assert(sub(U_0^2, T) == T_0^2)
