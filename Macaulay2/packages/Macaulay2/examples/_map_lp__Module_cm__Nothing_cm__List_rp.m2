R = ZZ/101[x,y,z]
p = map(R^2,,{{x^2,0,3},{0,y^2,5}})
isHomogeneous p
p = matrix {{x^2,0,3},{0,y^2,5}}
R = ZZ/101[x,y]
f = map(R^2,,{{x^2,y^2},{x*y,0}})
degrees source f
isHomogeneous f
