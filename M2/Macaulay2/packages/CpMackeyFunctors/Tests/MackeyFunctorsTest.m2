TEST ///
assertLevel = 0;
-- Test if the Zero Mackey functor can be constructed and is well-defined
R:=matrix({});
C:=R;
T:=R;
M:=makeCpMackeyFunctor(7,R,T,C)
assert(isWellDefined M);

-- Test if constant F_2 is a C_2-Mackey functor
U:=cokernel(matrix({{2}}));
R:=inducedMap(U,U);
T:=inducedMap(U,U,matrix({{0}}));
C:=inducedMap(U,U);
M:=makeCpMackeyFunctor(2,R,T,C);
assert(isWellDefined M);

-- Test if F_4 Galois is a C_2-Mackey functor
U:=cokernel(matrix({{2,0},{0,2}}));
C:=inducedMap(U,U,matrix({{1,1},{0,1}}));
F:=kernel(C - id_U);
R:=inducedMap(U,F,matrix({{1,0},{0,1}}));
T:=inducedMap(F,U,matrix({{0,1},{0,0}}));
M:=makeCpMackeyFunctor(2,R,T,C);
assert(isWellDefined M);
-- Make sure not well defined if we swap R and T
M:=makeCpMackeyFunctor(2,T,R,C);
assert(not isWellDefined M);

-- Test if FP of C_7 with *2 is a C_3-Mackey functor
U:=cokernel(matrix({{7}}));
C:=inducedMap(U,U,matrix({{2}}));
F:=kernel(C - id_U);
R:=inducedMap(U,F,matrix({{1}}));
T:=inducedMap(F,U,matrix({{0}}));
M:=makeCpMackeyFunctor(3,R,T,C);
assert(isWellDefined M);
assert(M.Underlying == U);
assert(M.Fixed == F);
assert(M.Res == R);
assert(M.Trans == T);
assert(M.Conj == C);

assert(not isCohomological makeBurnsideMackeyFunctor 7);
assert(isCohomological makeFixedPointMackeyFunctor(2,id_(ZZ^1)));
assert(isCohomological makeUnderlyingFreeMackeyFunctor 5);

///