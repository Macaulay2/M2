R = QQ[w,x,y,z];
X = Spec(R/(y^2-x*z,x^2*y-z^2,x^3-y*z))
ideal X
ring X
Y = Proj(R/(x^2-w*y, x*y-w*z, x*z-y^2))
ideal Y
