TEST ///
M=matrix {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{1,-1,-1}}
L={{0, 1, 2, 3}, {0, 4}}
P=polyhedralComplex(transpose M,L)
assert(not isPure P)
assert(dim P == 2)
assert(isSimplicial P)
///
