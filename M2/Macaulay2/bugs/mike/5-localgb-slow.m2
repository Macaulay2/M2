-- Yanked from test/engine/raw-localgb.m2
--------------------------
-- singular example #18 --
--------------------------
needs "raw-util.m2"

mo = rawMonomialOrdering{Weights=>4:-1, GRevLex=>4:1}
R = polyring3(rawZZp 101, (symbol t, symbol x, symbol y, symbol z), mo, 4:1)
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7
H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5
M = mat{{F,G,H}}
gbM = rawGB(M,false,0,{1,1,1,1},false,0,0,0)
gbTrace = 3
rawStartComputation gbM -- NOT CORRECT YET, I THINK.
m = rawGBGetMatrix gbM
