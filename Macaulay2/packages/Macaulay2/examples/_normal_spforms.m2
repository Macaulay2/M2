R = QQ[x,y,z,a,b,c,MonomialOrder=>Eliminate 3];
I = ideal(a-(x+y+z), b-(x*y+x*z+y*z), c-x*y*z)
f = x^3+y^3+z^3
f % I
