-------------------------
-- testing of local GB --
-------------------------
needs "raw-util.m2"

mo = rawMonomialOrdering{Weights=>4:-1, GRevLex=>4:1}
R = polyring3(rawZZp 101, (symbol a, symbol b, symbol c, symbol d), mo, 4:1)
a
rawRing a
F = a-b*a^2-a^3
G = b-a*b-c^3
M = mat{{F,G}}
gbM = rawGB(M,false,0,{1,1,1,1},false,0,0,0,10)
gbTrace = 10
rawStartComputation gbM
m = rawGBGetMatrix gbM

--------------------
-- simple example --
--------------------
needs "raw-util.m2"
mo = rawMonomialOrdering{Weights=>4:-1, GRevLex=>4:1}
R = polyring3(rawZZp 101, (symbol a, symbol b, symbol c, symbol d), mo, 4:1)
F = a-a^2
G = a^3
M = mat{{F,G}}
gbM = rawGB(M,false,0,{1,1,1,1},false,0,0,0,10)
gbTrace = 1024
rawStartComputation gbM
m = rawGBGetMatrix gbM
assert(m == mat{{F}})

--------------------
-- simple example --
--------------------
needs "raw-util.m2"
mo = rawMonomialOrdering{Weights=>4:-1, GRevLex=>4:1}
R = polyring3(rawZZp 101, (symbol a, symbol b, symbol c, symbol d), mo, 4:1)
F = a*b - a^3*b
G = a*c - b^3
M = mat{{F,G}}
gbM = rawGB(M,false,0,{1,1,1,1},false,0,0,0,10)
gbTrace = 1024
rawStartComputation gbM
m = rawGBGetMatrix gbM

--------------------------
-- singular example #18 --
--------------------------
needs "raw-util.m2"

mo = rawMonomialOrdering{GRevLex=>5:1}
R = polyring3(rawZZp 101, (symbol t, symbol x, symbol y, symbol z, symbol h), mo, 5:1)
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7
H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5
M = mat{{F,G,H}}
Mh = rawHomogenize(M,4,(1,1,1,1,1))
gbMh = rawGB(Mh,false,0,{1,1,1,1,1},false,0,0,0,10)
gbTrace = 3
rawStartComputation gbMh
m = rawGBGetMatrix gbMh


