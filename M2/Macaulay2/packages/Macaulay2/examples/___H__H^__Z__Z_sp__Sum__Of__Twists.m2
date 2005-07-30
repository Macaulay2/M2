X = Proj(QQ[x_0..x_2])
HH^0(OO_X^1(>=0))
HH^1(OO_X^1(>=0))
HH^2(OO_X^1(>=-3)) -- this should change to * once implemented
TruncDual = HH^2(OO_X^1(>=-4))
hilbertFunction(-4, TruncDual)
hilbertFunction(-3, TruncDual)
R = QQ[x_0..x_4];
a = {1,0,0,0,0}
b = {0,1,0,0,1}
c = {0,0,1,1,0}
M1 = matrix table(5,5, (i,j)-> x_((i+j)%5)*a_((i-j)%5))
M2 = matrix table(5,5, (i,j)-> x_((i+j)%5)*b_((i-j)%5))
M3 = matrix table(5,5, (i,j)-> x_((i+j)%5)*c_((i-j)%5))
M = M1 | M2 | M3;
betti (C=res coker M)
N = transpose submatrix(C.dd_3,{10..28},{2..36});
betti (D=res coker N)
Pfour = Proj(R)
HorrocksMumford = sheaf(coker D.dd_3);
T = HH^1(HorrocksMumford(>=-1))
apply(-1..2, i-> hilbertFunction(i,T))
