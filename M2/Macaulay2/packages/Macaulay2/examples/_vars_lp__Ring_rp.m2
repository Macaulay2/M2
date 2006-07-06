S = QQ[w,x,y,z];
vars S
ideal vars S
coker vars S
res coker vars S
R = S/(x^2-w*y, x*y-w*z, x*z-y^2);
vars R
use S;
Q = S/(x^2-w*y, z);
vars S
