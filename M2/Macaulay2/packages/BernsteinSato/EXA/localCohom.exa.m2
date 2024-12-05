restart
load "Dloadfile.m2"

-- set your favorite strategy
Str = Walther
LocStr = Oaku
-- Example (Harry)
setInfoLevel 666
W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]
I = ideal (X*(Y-Z), X*Y*Z)
M = ideal (dX, dY, dZ)
time h = localCohom(I, Strategy=>Walther)
pruneLocalCohom h 
time h = localCohom(2, I)
pruneLocalCohom h 

-- Example (Anton)
W = QQ[X, dX, Y, dY, WeylAlgebra=>{X=>dX, Y=>dY}]
I = ideal (X^2+Y^2, X*Y)
time localCohom(I, Strategy=>Str)

-- OT "Algs for D-mods" (1998), Example 7.5
W = QQ[x,y,z,dx,dy,dz, WeylAlgebra=>{x=>dx,y=>dy,z=>dz}]
R = cokernel matrix{{dx, dy, z^3*dz+z}}
I = ideal (x*z, y*z)
localCohom(I, R, Strategy=>Str) 

--------------------------------------------------------------------------------
restart
load "Dloadfile.m2"
-- b^2-ac, c^2-bd, ad-bc. 
W =  QQ[x_1..x_4, dx_1..dx_4, WeylAlgebra => toList (1..4) / (i -> x_i => dx_i)]
I = minors(2, matrix{{x_1, x_2, x_3}, {x_2, x_3, x_4}})
r = localCohom (I, W^1 / ideal {dx_1, dx_2, dx_3, dx_4}, 
     Strategy=>Walther, LocStrategy=>Oaku); 

 
restart
load "Dloadfile.m2"
-- Example (!!! Unable to compute !!!)
W = QQ[x_1..x_6, dx_1..dx_6, WeylAlgebra => toList (1..6) / (i -> x_i => dx_i)]
I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, x_5, x_6}}) 
h = localCohom ({0,1,3}, I, W^1 / ideal {dx_1, dx_2, dx_3, dx_4, dx_5, dx_6}, 
     Strategy=>Walther, LocStrategy=>Oaku); 
pruneLocalCohom oo
      

-----------------------------------------------------------------------------
-- EXAMPLES for the local cohomology paper
-----------------------------------------------------------------------------
restart
load "Dloadfile.m2"

W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}];
I = ideal (X*(Y-Z), X*Y*Z);
time h = localCohom(I, Strategy=>Walther)
pruneLocalCohom h 
time localCohom(I, Strategy=>OaTa)


M = cokernel matrix{{dX, dY, Z^3*dZ+Z}};
I = ideal (X*Z, Y*Z);
time h = localCohom(I, M, Strategy=>Walther, LocStrategy=>Oaku) 
pruneLocalCohom h
time localCohom(I, M, Strategy=>OaTa)


W = QQ[X, dX, Y, dY, WeylAlgebra=>{X=>dX, Y=>dY}]
I = ideal (X^2+Y^2, X*Y)
time h = localCohom(I, Strategy=>Walther)
pruneLocalCohom h 
time localCohom(I, Strategy=>OaTa)


-- twisted cubic
W =  QQ[x_1..x_4, dx_1..dx_4, 
     WeylAlgebra => toList (1..4) / (i -> x_i => dx_i)];
I = minors(2, matrix{{x_1, x_2, x_3}, {x_2, x_3, x_4}});
time h = localCohom ({0,1,3}, I, W^1 / ideal {dx_1, dx_2, dx_3, dx_4}, 
     Strategy=>Walther, LocStrategy=>Oaku); 
pruneLocalCohom h
time localCohom (2, I, W^1 / ideal {dx_1, dx_2, dx_3, dx_4}, 
     Strategy=>Walther, LocStrategy=>Oaku); 
time h = localCohom ({0,1,3}, I, W^1 / ideal {dx_1, dx_2, dx_3, dx_4}, 
     Strategy=>OaTa); 


-- 2x2 minor
W = QQ[x_1..x_6, dx_1..dx_6, 
     WeylAlgebra => toList (1..6) / (i -> x_i => dx_i)];
I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, x_5, x_6}}); 
time h = localCohom ({0,1,3}, I, 
     W^1 / ideal {dx_1, dx_2, dx_3, dx_4, dx_5, dx_6}, 
     Strategy=>Walther, LocStrategy=>Oaku); 
pruneLocalCohom h
time h = localCohom (2, I, 
     W^1 / ideal {dx_1, dx_2, dx_3, dx_4, dx_5, dx_6}, 
     Strategy=>Walther, LocStrategy=>Oaku); 
 
-----------------------------------------------------------------
-- Simple examples 
restart
load "Dloadfile.m2"
W = QQ[x_1..x_6, dx_1..dx_6, 
     WeylAlgebra => toList (1..6) / (i -> x_i => dx_i)];
M = W^10/ideal{dx_1^3, dx_2, dx_3, dx_4, dx_5, dx_6}
M = makeCyclic presentation M
M = makeCyclic(matrix{{dx_1^2, dx_2^2, dx_3^2, dx_4^2, dx_5^2, dx_6^2}})
M = makeCyclic(matrix{{dx_1, dx_2, dx_3, dx_4, dx_5, dx_6}|toList(12:0),
	  toList(6:0)|{dx_1, dx_2, dx_3, dx_4, dx_5, dx_6}|toList(6:0),
	  toList(12:0)|{dx_1^2, dx_2^2, dx_3^2, dx_4^2, dx_5^2, dx_6^2}}
	  )
time h = localCohom (ideal{x_1,x_2,x_3}, W^1/M.AnnG, 
     Strategy=>Walther, LocStrategy=>Oaku); 
pruneLocalCohom h

time h = localCohom (ideal{x_1,x_2,x_3}, W^1/M.AnnG, 
     Strategy=>OaTa); 
time h = localCohom (ideal{x_1,x_2,x_3}, M, 
     Strategy=>OaTa); 
pruneLocalCohom h

---------------------------------------------------
-- Foolproof check 
---------------------------------------------------
W = QQ[X, dX, Y, dY, WeylAlgebra=>{X=>dX, Y=>dY}]
I = ideal (X^2+Y^2, X*Y)
M = cokernel dX
Dtrace 1
time localCohom(I,M)













