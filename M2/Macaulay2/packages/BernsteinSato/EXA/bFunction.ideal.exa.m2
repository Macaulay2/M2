loadPackage ("Dmodules", FileName => "../../Dmodules.m2")

-------------------------EXAMPLES for bFunction

--Example of non-specializable
R = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
I = ideal Dx	 
bFunction(I, {1,1})

--Example 1 (GD_of_HDE Example 5.2.10)
R = QQ[x, y, dx, dy, WeylAlgebra => {x=>dx, y=>dy}]
I = ideal(x, dy-1)     	    	 
time bFunction(I, {1,0})

--Example 1b (GD_of_HDE Example 5.2.10)
R = frac(QQ[p]/ ideal p)[x, y, dx, dy, WeylAlgebra => {x=>dx, y=>dy}]
I = ideal(x, dy-1)     	    	 
time bFunction(I, {1,0})

--Example 2 (Algs_for_D_mods Example 5.5)
A = matrix{{0,0,1,1},{1,0,1,0},{0,1,0,1}}
b = {15,100,40}
I = gkz(A,b)
transpose gens I
w = {1,1,1,0}
time(apply(20, i->print bFunction(I, w)))
time(apply(20, i->print bFunction(I, w, Homogenization => false)))
bf = bFunction(I, w)
-- Expected answer
bf == (s:=(ring bf)_0; (s-b#1)*(s+b#0-b#1-b#2))

--Example 2a (Algs_for_D_mods Example 5.5)
A = matrix{{0,0,1,1},{1,0,1,0},{0,1,0,1}}
b = {15,100,40}
I = gkz(A,b)
transpose gens I
w = {1,1,1,1}
bf = bFunction(I,w)
-- Expected answer
bf == (s:=(ring bf)_0; s-b#1-b#2)

--Example 3 (Algs_for_D_mods Example 4.7) -- !!! Doesn't finish !!!
W = QQ[t_1,t_2,x,y,z, d_1,d_2,dx,dy,dz,
     WeylAlgebra => {t_1=>d_1, t_2=>d_2, x=>dx, y=>dy, z=>dz}]
I = ideal(t_1-x^3+y^2,
     t_2-y^3+z^2,
     dx+3*x^2*d_1,
     dy-2*y*d_1+3*y^2*d_2,
     dz-2*z*d_2)
w = {1,1,0,0,0}
bFunction(I, w)

-- log canonical threshold
restart; 
loadPackage "Dmodules";
QQ[x_1..x_6];
time lct minors(2, matrix{{x_1, x_2, x_3}, {x_4, x_5, x_6}}) 
QQ[x_1..x_3];
time lct ideal (x_2^2-x_1*x_3, x_1^3-x_3^2);
QQ[x_1..x_3];
time lct ideal (x_1^3-x_2^2, x_2^3-x_3^2);
QQ[x_1..x_3];
time lct ideal (x_1^4-x_2^3, x_3^2-x_1*x_2^2); -- does not finish

-- rlct
restart
loadPackage ("Dmodules", FileName => "../../Dmodules.m2")
debug Dmodules
makeWA

QQ[x,y]
f = x^2+y^3
sort bFunctionRoots globalBFunction f
sort bFunctionRoots localBFunction(f, ideal gens ring f)
rlct f

QQ[x,y,z]
f = (x^2+y^2+z^2)^2+x^6
isFsLocallyIntegrable(f,-17/24)
isFsLocallyIntegrable(f,-19/24)
