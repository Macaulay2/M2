M = matrix{{1,2,3},{1,34,45},{2213,1123,6543},{0,0,0}}
(D,P,Q) = smithNormalForm M
D == P * M * Q
(D,P) = smithNormalForm(M, ChangeMatrix=>{true,false})
D = smithNormalForm(M, ChangeMatrix=>{false,false}, KeepZeroes=>true)
prune coker M
S = ZZ/101[t]
D = diagonalMatrix{t^2+1, (t^2+1)^2, (t^2+1)^3, (t^2+1)^5}
P = random(S^4, S^4)
Q = random(S^4, S^4)
M = P*D*Q
(D1,P1,Q1) = smithNormalForm M;
D1 - P1*M*Q1 == 0
prune coker M
