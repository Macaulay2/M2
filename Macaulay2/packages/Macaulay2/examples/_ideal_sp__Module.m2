R = QQ[w,x,y,z];
f = map(R^1,R^3, matrix{{x^2-w*y, x*y-w*z, x*z-y^2}})
image f
ideal image f
