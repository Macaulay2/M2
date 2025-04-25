-- Simple check for linearTransform
TEST ///
F = normalFan hypercube 3
A = matrix {{1,0,1},{0,1,1}}
B = matrix {{1,0,1},{0,1,1},{0,0,1}}
FA = linearTransform(F, A)
FB = linearTransform(F, B)
assert(not isWellDefined FA)
assert(isWellDefined FB)
MC = maxCones F
R = rays F
R = B*R
Fcheck = fan(R, MC)
assert(FB == Fcheck)
///

TEST ///
A = transpose matrix{{1,0},{-1,0}}
L = matrix {{0},{1}}
F = fan(A, L, {{0},{1}})
assert(isWellDefined F)
M = matrix {{1,0}}
FF = linearTransform(F,M)
assert(isWellDefined FF)
assert(isPure FF)
assert(isSimplicial FF)
assert(dim FF == 1)
assert(isPointed FF)
///
