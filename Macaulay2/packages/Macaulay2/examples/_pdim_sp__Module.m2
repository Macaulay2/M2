R = QQ[x,y,z];
I = ideal(x^2, x*y, y*z);
M = R^1/I
res M
pdim M
res(module I)
pdim(module I)
