kk = ZZ/101;
A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
b = matrix"1;1;1" ** kk
x = solve(A,b)
A*x-b
kk = RR
A = matrix"1,2,3;1,3,6;19,7,11" ** kk
b = matrix"1;1;1" ** kk
x = solve(A,b)
A*x-b
kk = CC
A = mutableZero(kk,1000,1000, Dense=>true)
b = mutableZero(kk,1000,2, Dense=>true)
for i from 1 to 10000 do A_(random 1000, random 1000) = random 1.0 + ii * random 1.0;
for i from 0 to 999 do b_(i,0) = random 1.0 + ii * random 1.0;
for i from 0 to 999 do b_(i,1) = random 1.0 + ii * random 1.0;          
time x = solve(A,b);
C = (matrix A)*(matrix x)-matrix b;
C_(123,0)
C_(567,1)
kk = RR
A = random(kk^5, kk^5)
I = id_(target A)
x = solve(A,I)
A*x - I
x*A - I
invA = I // A
x == invA
x - invA
