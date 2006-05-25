R = QQ[x,y,z];
radical ideal(z^4+2*z^2+1)
I = ideal"xyz,x2,y4+y5"
radical I
k = 0;
while (y^2+y)^k % I != 0 do k = k+1;
k
