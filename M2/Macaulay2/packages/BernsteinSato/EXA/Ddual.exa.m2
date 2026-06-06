path = join(path, {"../"})
load "Dloadfile.m2"
Dtrace 4

I = AppellF1({1,3,4,-1}, Vars => Local)
Ddual I

A = matrix{{1,1,1},{0,1,2}}
b = {8/3,9/17}
I = gkz(A,b,Vars=>Local)
Ddual I

J = substitute(gkz(A,{1,2},Vars=>Local), ring I)
M = directSum(cokernel gens I, cokernel gens J)
Ddual M

-- non-holonomic!
R = QQ[x,y,dx,dy, WeylAlgebra=>{x=>dx,y=>dy}]
Ddual ideal dx
