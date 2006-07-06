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
HH^0(HorrocksMumford(1))
HH^0(HorrocksMumford(2))
eulers(HorrocksMumford(2))
