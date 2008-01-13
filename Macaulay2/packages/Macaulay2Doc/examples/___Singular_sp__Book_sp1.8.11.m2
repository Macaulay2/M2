A = QQ[x,y,z];
I1 = ideal(x,y);
I2 = ideal(y^2,z);
intersect(I1,I2)
B = QQ[t,x,y,z];
I1 = substitute(I1,B);
I2 = substitute(I2,B);
J = t*I1 + (1-t)*I2
loadPackage "Elimination";
eliminate(J,t)
