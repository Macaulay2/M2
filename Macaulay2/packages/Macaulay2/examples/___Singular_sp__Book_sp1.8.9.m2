A = QQ[x,y,z];
I = ideal"x5,xy3,y7,z3+xyz";
f = x+y+z;
B = A[t];
J = substitute(I,B) + ideal(f*t-1)
1 % J 
radical I
