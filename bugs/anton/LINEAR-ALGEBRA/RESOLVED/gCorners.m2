restart
needsPackage "NoetherianOperators"
RQQ = QQ[x,y,z]
I = intersect(ideal(x,z),ideal(x^2-y^2,y+z),ideal(x^2-z^2,x+2*y)) -- line arrangement
I = intersect(ideal ((x-1)*y), I) -- add a plane
RCC = CC[x,y,z]; 
gCs = gCorners (origin RQQ, I) -- works 
gCs = gCorners (origin RCC, sub(I,RCC)) -- SIGSEGV
sCorners gCs
