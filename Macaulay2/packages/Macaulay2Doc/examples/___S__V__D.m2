M = map(RR^3, RR^5, (i,j) -> (i+1)^j * 1.0)
(S,U,V) = SVD(M)
(transpose U) * M * (transpose V)
U^-1 == transpose U
(S1,U1,V1) = SVD(M, DivideConquer => true)
S1 == S, U1==U, V1==V
