clearAll()
load "D-modules.m2"
Dtrace 1
pInfo(1, "testing localCohom...")

W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]
I = ideal (X*(Y-Z), X*Y*Z)
M = ideal (dX, dY, dZ)

time h = localCohom I
time h = localCohom (I, W^1/M, Strategy=>Walther)
time h = localCohom (I, Strategy=>Walther, LocStrategy=>OaTaWa)
time h = localCohom (I, Strategy=>Walther, LocStrategy=>Oaku)
time h = localCohom (I, Strategy=>OaTa)
pruneLocalCohom h
---------------------------------------------------------------
W = QQ[X, dX, Y, dY, WeylAlgebra=>{X=>dX, Y=>dY}]
I = ideal (X^2+Y^2, X*Y)
M = ideal (dX, dY)

time h = localCohom I
time h = localCohom (I, W^1/M, Strategy=>Walther)
time h = localCohom (I, Strategy=>Walther, LocStrategy=>OTW)
time h = localCohom (I, Strategy=>Walther, LocStrategy=>Oaku)
time h = localCohom (I, Strategy=>OaTa)
pruneLocalCohom h
---------------------------------------------------------------
W = QQ[x, dx, WeylAlgebra=>{x=>dx}]
I = ideal {x, x^2, x^2+x, x^3, x^4+2*x}
M = W^1 / ideal dx 
time h = localCohom (I, M, Strategy=>Walther, LocStrategy=>Oaku)
time hh = localCohom (ideal x)  
h = pruneLocalCohom h
hh = pruneLocalCohom hh
assert all(toList h, i-> not hh#?i or hh#i == h#i)







