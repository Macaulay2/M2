R = ZZ/7[w,x,y,z];
f = vars R
ideal f
g = matrix{{x^2-w*y, x*y-w*z, x*z-y^2},{y^2-x*z,x^2*y-z^2,x^3-y*z}}
ideal g
