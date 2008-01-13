R = ZZ/101[x,y,z];
ring x
M = matrix {{2*x, x+y},{y^3, z*y}};
ring M
S = QQ[x,y,z];
ring x
I = ideal (x*y, y*z);
ring I
