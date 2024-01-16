--EXAMPLE 1-------------------------------
n = 3
R = QQ[x_1..x_n,D_1..D_n, WeylAlgebra=>(apply(n,i->x_(i+1)=>D_(i+1)))] 
a = D_1; b = D_3; c = D_2;
a = D_1+x_2; b = D_3^2+x_2*D_3+x_3; c = D_2^2;
stafford ideal (a,b,c)
ideal(a,b,c)==oo
--EXAMPLE 2-------------------------------
n = 3
R = ZZ/101[x_1..x_n,D_1..D_n, WeylAlgebra=>(apply(n,i->x_(i+1)=>D_(i+1)))] 
a = D_1; b = D_3; c = D_2;
a = D_1+x_2; b = D_3^2+x_2*D_3+x_3; c = D_2^2;
stafford ideal (a,b,c)
ideal(a,b,c)==oo
--EXAMPLE 3-------------------------------
n = 3
R = QQ[x_1..x_n,D_1..D_n, WeylAlgebra=>(apply(n,i->x_(i+1)=>D_(i+1)))] 
a = D_1+x_3; b = D_2+x_2; c = D_3+x_1;
d = x_2+D_2
a = d*(D_1+x_3); b = (D_1+x_3)*(D_3+x_1); c = d*(D_3+x_1);
stafford ideal (a,b,c)
ideal(a,b,c)==oo
--EXAMPLE 4-------------------------------
n = 4
R = QQ[x_1..x_n,D_1..D_n, WeylAlgebra=>(apply(n,i->x_(i+1)=>D_(i+1)))] 
a = D_1; b = D_2; c = D_3; d = D_4;
stafford ideal (a,b,c,d)
ideal(a,b,c,d)==oo
