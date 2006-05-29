A = QQ[x,y,z];
I1 = ideal(x^5*z^3, x*y*z, y*z^4);
saturate(I1,z)
J = I1:z
k = 0;
while not isSubset(J,I1) do (
   k = k+1;
   I1 = J;
   J = I1 : z;
   );
J
k
