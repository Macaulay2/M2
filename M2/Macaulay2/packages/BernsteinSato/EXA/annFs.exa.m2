restart
loadPackage "Dmodules"

--------------------------------------- EXAMPLES for AnnFs
--Example 5.3.7 (p216)
QQ[x,dx, WeylAlgebra=>{x=>dx}]
f = x
AnnFs f 

--Example 5.3.9 (p217)
QQ[x_1..x_4, z, d_1..d_4, Dz, WeylAlgebra => ( toList(1..4) / (i -> (x_i => d_i)) | {z => Dz} ) ]
f = x_1 + x_2 * z + x_3 * z^2 + x_4 * z^3

<< "AnnFs output = " <<  
--flatten entries autoReduce gens gb matrix 
(I1 = AnnFs f)
<<endl;

--------------------------------------- EXAMPLES for AnnIFs
W = QQ[x,dx, WeylAlgebra=>{x=>dx}]
AnnIFs(ideal dx, x^2) 

--I2 = AnnIFs(ideal(d_2,d_3,d_4), f)
--I3 = AnnIFs(ideal(x_1*x_2,x_3,x_4), f)

--------------------------------------- EXAMPLES for AnnFs or several polys
restart
loadPackage "Dmodules"

W = makeWA ( QQ[x_1..x_6] ) 
I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, x_5, x_6}}) 
w = toList (6:0) | { 1,1,1 } 

W = makeWA ( QQ[x_1..x_3] ) 
I = ideal (x_2^2-x_1*x_3, x_1^3-x_3^2)
w = toList (3:0) | { 1,1 } 

W = makeWA ( QQ[x_1..x_3] ) 
I = ideal (x_1^3-x_2^2, x_2^3-x_3^2)
w = toList (3:0) | { 1,1 } 

W = makeWA ( QQ[x_1..x_3] ) 
I = ideal (x_1^4-x_2^3, x_3^2-x_1*x_2^2)
w = toList (3:0) | { 1,1 } 

M = AnnFs I_*
time b = bFunction(M, w)
S = ring b;
r = numgens I
factorBFunction sub(b, {S_0=>-S_0-r})  
