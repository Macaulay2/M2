checkLU = method()
checkLU(List,Matrix,Matrix) := (P,L,U) -> (
     R := ring L;
     Q = id_(R^#P) _ P;
     --Q = mutableMatrix(R, numrows L, numrows L);
     --for i from 0 to numrows L - 1 do Q_(i,P_i) = 1_R;
     --Q = matrix Q;
     Q*L*U)
checkLU Matrix := (M) -> assert (checkLU time LUdecomposition M == M)

permutationMatrix = (p) -> id_(ZZ^#p)^p
M = matrix {{1.0, 3.0, 4.0, 5.0},{2.0, 3.0, 0.0, 1.0}}
M = mutableMatrix(M, Dense=>true)
(P,L,U) = LUdecomposition M
assert(0 == permutationMatrix P * matrix L * matrix U - matrix M)

-- over a prime finite field
kk = ZZ/101
M = random(kk^20, kk^30)
(P,L,U) = LUdecomposition M
assert(0 == permutationMatrix P * matrix L * matrix U - matrix M)

-- over a nonprime finite field.  Not available currently!!
{*
kk = GF(8)
M = random(kk^20, kk^30)
(P,L,U) = LUdecomposition M
assert(0 == permutationMatrix P * matrix L * matrix U - matrix M)
*}

-- over QQ.  Not available currently!
{*
kk = QQ
M = random(kk^20, kk^30)
(P,L,U) = LUdecomposition M
assert(0 == permutationMatrix P * matrix L * matrix U - matrix M)
*}


kk = ZZ/32003
M = mutableMatrix(kk,3,3)
fillMatrix M
(P,L,U) = LUdecomposition M
L*U
M
kk = ZZ/32003
M = mutableMatrix(kk,2,2)
fillMatrix M
(P,L,U) = LUdecomposition M
L*U

kk = ZZ/32003
M = matrix(kk, {{3,2},{3,5}})
(P,L,U) = LUdecomposition M
M == L*U

kk = ZZ/32003
M = matrix(kk, {{3,2,1},{3,5,4},{1,1,7}})
(P,L,U) = LUdecomposition M
M == L*U

kk = ZZ/32003
M1 = matrix(kk, {{2,3,1,0,0,1},
            {0,0,1,2,3,1},
            {0,0,0,0,1,1},
            {0,0,0,0,0,0}})
S = random(kk^4, kk^4)
S = matrix(kk, {{1,0,0,0},
                {1,1,0,0},
                {1,1,1,0},
                {1,1,1,1}})
M = S * M1
(P,L,U) = LUdecomposition M
M == L*U

S = matrix(kk, {{1,0,0,0},
                {1,0,0,1},
                {1,1,0,0},
                {1,1,1,0}})
M = S * M1
(P,L,U) = LUdecomposition M
M == L*U
L*U
M
M == checkLU LUdecomposition M
