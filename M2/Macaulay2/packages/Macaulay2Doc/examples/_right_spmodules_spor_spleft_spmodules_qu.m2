R = QQ[x,dx,WeylAlgebra=>{x=>dx}]
x*dx
dx*x
M = R^2
v = M_0
dx*v
x*(dx*v)
(x*dx)*v
x*(dx*v) == (x*dx)*v
f = dx * id_M
f*(x*v)
x*(f*v)
f*(x*v) == x*(f*v)
g = x * id_M
f*g
f*g == (x*dx) * id_M
(dx * id_M)*(x * id_M) == (x*dx) * id_M
x * ( (dx * id_M) * v )
(x *  (dx * id_M) ) * v
(x *  (dx * id_M) ) * v == x * ( (dx * id_M) * v )      
x * ( id_M * ( dx * id_M ) )
(x * id_M) * ( dx * id_M )
x * ( id_M * ( dx * id_M ) ) == (x * id_M) * ( dx * id_M )
