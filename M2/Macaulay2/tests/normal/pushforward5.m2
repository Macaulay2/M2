kk = ZZ/101

-- source and target have different coefficient rings
R = kk[a]
S = R[b, Join => false] / {a^2, b^2}
T = kk[t]
f = map(S, T, {a})
M = pushForward(f, S^1)
assert(t^2 * M == 0)

-- map from coefficient ring is not an inclusion
R = kk[a]
S = R[b]/{a^2, b^2}
f = map(S, R)
M = pushForward(f, S^1)
assert(a^2 * M == 0)
