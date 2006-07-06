kk = ZZ/101;
A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
A = mutableMatrix A
(P,L,U) = LU A
matrix L * matrix U == matrix A
kk = RR
A = matrix"1,2,3,4,5,6;1,3,6,12,13,16;19,7,11,47,48,21" ** kk
A = mutableMatrix(A, Dense=>true)
(P,L,U) = LU A
Q = mutableZero(kk, numrows A, numrows A)
for i from 0 to numrows A - 1 do Q_(i,P_i) = 1.0
matrix Q * matrix L * matrix U == matrix A
kk = CC
A = mutableZero(kk,1000,900, Dense=>true)
for i from 1 to 10000 do A_(random 1000, random 900) = random 1.0 + ii * random 1.0;
time (P,L,U) = LU A;
