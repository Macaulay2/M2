R = ZZ/101[x,y,z];
p = map(R^2,3,{{x^2,0,3},{0,y^2,5}})
isHomogeneous p
p = map(R^2,3,{(0,0) => x+y, (1,1) => x^2, (0,2) => x-1, (0,0) => x-y})
