d = 3;
needsPackage "Cyclotomic";
KK = QQ[r]/cyclotomicPoly(d,r);
S = KK[x_0..x_11];
A = matrix{
       {x_0 + r*x_1+r^2*x_2,0,0},
       {0,r*(x_0 + r*x_1+r^2*x_2),0},
       {0,0,r^2*(x_0 + r*x_1+r^2*x_2)}}
det A
B = genericMatrix(S,x_3,3,3)
M = matrix{{A,B,0*id_(S^3)},{0*id_(S^3),r*A,r*B},{r^2*B,0*id_(S^3),r^2*A}}
det(M, Strategy=>Bareiss) -- crashes
det(M, Strategy=>Cofactor) -- OK

