R = ZZ/101[w,x,y,z];
ideal{x^2-w*y, x*y-w*z, x*z-y^2}
ideal(y^2-x*z,x^2*y-z^2,x^3-y*z)
E = ZZ/2[x,y, SkewCommutative => true];
ideal(x^2,x*y)
W = QQ[x,dx, WeylAlgebra => {x => dx}];
ideal(dx*x+x*dx)
I = ideal(12,18)
mingens I
