restart
load "Dloadfile.m2"
Dtrace 4

------------------------- EXAMPLES for Drestriction --------------------------------

-- Example 0: Resolution of the polynomial ring
n = 5
W = QQ[z_1..z_n, Dz_1..Dz_n, WeylAlgebra => 
     apply(toList(1..n), i -> z_i => Dz_i)]
M = W^1/ideal(Dz_1..Dz_n)

Dres(M)
Dres(M, {-1,-2,-3,-4,-5,1,2,3,4,5})


-- Example 1: Resolution of an Appell F1 system
I = AppellF1({1,2,3,4}, Vars => Local)
W = ring I

Dres(I)
Dres(I, {-1,-1,1,1})
Dres(I, {-1,-1,1,1}, Strategy => Vhomogenize)
Dres(I, {-1,-2,1,2})
Dres(I, {-1,-2,1,2}, Strategy => Vhomogenize)


-- Example 2: Resolution of a GKZ system
I = gkz(matrix{{1,1,1,1},{0,1,3,4}}, {5,2})
W = ring I

Dres(I)
Dres(I, {-1,-1,-1,-1,1,1,1,1})

