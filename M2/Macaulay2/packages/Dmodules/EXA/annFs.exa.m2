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
restart
load "Dloadfile.m2"
W = QQ[x,dx, WeylAlgebra=>{x=>dx}]
AnnIFs(ideal dx, x^2) 

