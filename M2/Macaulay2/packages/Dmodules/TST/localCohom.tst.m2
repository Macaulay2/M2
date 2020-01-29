-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needsPackage "Dmodules"
Dtrace 1
pInfo(1, "testing localCohom...")

x = symbol x; dx = symbol dx;
y = symbol y; dy = symbol dy;
z = symbol z; dz = symbol dz;
 
W = QQ[x, dx, y, dy, z, dz, WeylAlgebra=>{x=>dx, y=>dy, z=>dz}]
I = ideal (x*(y-z), x*y*z)
J = ideal (dx, dy, dz)

time h = localCohom I
time h = localCohom (I, W^1/J, Strategy=>Walther)
time h = localCohom (I, Strategy=>Walther, LocStrategy=>OaTaWa)
time h = localCohom (I, Strategy=>Walther, LocStrategy=>Oaku)
time h = localCohom (I, Strategy=>OaTa)
pruneLocalCohom h
---------------------------------------------------------------
W = QQ[x, dx, y, dy, WeylAlgebra=>{x=>dx, y=>dy}];
I = ideal (x^2+y^2, x*y);
J = ideal (dx, dy);
K = ideal(x^3,y^3);

time h = localCohom I
time h = localCohom (I, W^1/J, Strategy=>Walther)
time h = localCohom (I, Strategy=>Walther, LocStrategy=>OTW)
time h = localCohom (I, Strategy=>Walther, LocStrategy=>Oaku)
time h = localCohom (I, Strategy=>OaTa)
pruneLocalCohom h

m = ideal(x,y);
L = pruneLocalCohom localCohom(m);
assert (rank L#0 == 0);
assert (rank L#1 == 0);
assert (rank L#2 == 1);
Mat = (pruneLocalCohom localCohom(2,I))#2;
assert (sub((minimalPrimes charIdeal coker Mat)_0,W)==ideal(x,y));
L' = localCohom (m, W^1/K, Strategy=>OaTa);
assert (L'#0==W^1/ideal(x^3,y^3));


---localCohom(m,W^1/K) gives an error.  These strategies need rechecking and correcting.

---------------------------------------------------------------
x = symbol x; dx = symbol dx; 
W = QQ[x, dx, WeylAlgebra=>{x=>dx}]
I = ideal {x, x^2, x^2+x, x^3, x^4+2*x}
M = W^1 / ideal dx 
time h = localCohom (I, M, Strategy=>Walther, LocStrategy=>Oaku)
time h' = localCohom (ideal x)  
h = pruneLocalCohom h
h' = pruneLocalCohom h'
assert all(keys h, i-> not h'#?i or h'#i == h#i)