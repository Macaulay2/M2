-- c.f. https://github.com/Macaulay2/M2/issues/3790
S = QQ[x]
f = random(S^1, S^1/x)
assert isWellDefined f
assert(target f === S^1)
assert(source f === S^1/x)

f = random(S^1/x, S^1)
assert isWellDefined f
assert(target f === S^1/x)
assert(source f === S^1)
