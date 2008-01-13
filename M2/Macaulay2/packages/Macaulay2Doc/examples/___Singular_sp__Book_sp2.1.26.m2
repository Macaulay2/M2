A = QQ[x,y,z];
M = cokernel matrix(A, {{1,2,3},{4,5,6},{7,8,9}})
N = cokernel matrix{{x,y},{z,0}}
H = Hom(M,N)
f = homomorphism H_{0}
target f === N
source f === M
matrix f
