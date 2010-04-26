R = QQ[x,y,z,Degrees=>{0,0,1}]
F = x^2+y^3 + z^4 + x^2*y^2*z^6
assert(degree_x F == 2)
assert(degree_y F == 3)
assert(degree_z F == 6)
