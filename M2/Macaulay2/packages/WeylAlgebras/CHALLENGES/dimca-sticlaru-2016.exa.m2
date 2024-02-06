-- these do not compute in 1 hour
needsPackage "Dmodules"
QQ[x,y,z]
f = (x^2+y^2)^3 + (y^3+z^3)^2 -- Zariski sextic
f = (x^2+y^2)^4 + (y^4+z^4)^2
globalBFunction f

