path = join(path, {"../"})
load "Dloadfile.m2"
Dtrace 4

-- Example 1
n = 2
R = QQ[u_1..u_n,v_1..v_n]
I = ideal( sum(toList(1..n), i -> u_i*u_i),
     sum(toList(1..n), i -> v_i*v_i),
     sum(toList(1..n), i -> u_i*v_i))
k = 2
m = diffOps(I,k)
transpose putWeylAlgebra m

-- Example 2
R = QQ[x,y,z]
I = ideal(x+y^3+z^2)
m = diffOps(I, 3)
transpose putWeylAlgebra m
