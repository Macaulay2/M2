S = QQ[x]
M = image matrix{{x}}
R = S/x
assert(tensor(map(R, S), M) != 0)
assert(isFreeModule tensor(map(R, S), M))
