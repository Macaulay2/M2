R = QQ[x,y,z];
I = monomialIdeal(x*y^2, x^2*z, y^2*z)
ideal I
I * ideal I
I + ideal(x*y+y*z)
