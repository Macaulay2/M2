A = QQ[x,y,z];
I1 = ideal(x,y);
I2 = ideal(y^2,z);
I1 : I2
quotient(I1,I2)
J1 = intersect(I1,ideal(I2_0))
J2 = intersect(I1,ideal(I2_1))
K1 = ideal(J1_0//I2_0)
K2 = ideal(J2_0//I2_1, J2_1//I2_1)
intersect(K1,K2)
