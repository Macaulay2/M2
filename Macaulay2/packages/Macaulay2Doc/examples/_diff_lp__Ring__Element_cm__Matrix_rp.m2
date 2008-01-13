R = QQ[x,y,z];
f = matrix{{x^2-y*z, x*y*z + z^4}, {x-1, 2*y^2+z^2-1}}
diff(x,f)
diff(x^2-y*z,f)
