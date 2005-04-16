R = QQ[a,b,c][x,y,z]
m = transpose vars R
n = matrix{{x^2-a*y^3, x^3-z^2*y, x*y-b, x*z-c}}
diff(m,n)
