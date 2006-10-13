needsPackage "Dmodules"

-- example 1: annihilator of a rational function 1/f
W = QQ[x,t,Dx,Dt, WeylAlgebra => {x=>Dx, t=>Dt}]
f = t-t^2-x
I = ideal(Dx*f, Dt*f)
ClI = WeylClosure(I)
ClI == I
ClI == I + ideal (-2*t*Dx^2*Dt^3 + Dx^2*Dt^3 + Dx*Dt^4 - 6*Dx^2*Dt^2)

-- example 2: annihilator of e^{1/f^2}
W = QQ[x,y,z,Dx,Dy,Dz, WeylAlgebra => {x=>Dx, y=>Dy, z=>Dz}]
f = (x^3-y^2*z^2)
I = ideal(f^2*Dx+3*x^2, f^2*Dy-2*y*z^2, f^2*Dz-2*y^2*z)
ClI = WeylClosure I
ClI == I
ClI == I + ideal(y*Dy-z*Dz, y^2*z^3*Dz-(2/3)*x^4*Dx-2*x^3*z*Dz-2)

-- example 3: gkz system
A = matrix{{1,1,1},{0,1,2}};
I = gkz(A, {0,0})

I = gkz(A, {-1,-2});
W = ring I
F0 = map(W^1/I, W^1, matrix{{1_W}})
F1 = DlocalizeMap(I, x_2^2-4*x_1*x_3)
F2 = DlocalizeMap(target F1, x_1)
F3 = DlocalizeMap(target F2, x_3)
ClI = ideal kernel (F3*F2*F1*F0)

ClI == I
ClI == I + ideal(x_2^2*D_1^2-4*x_3^2*D_1*D_3-6*x_3*D_1)

L = (x_2^2*D_1^2-4*x_3^2*D_1*D_3-6*x_3*D_1)
(x_1*L) % I 
(x_1*L) // (gens I) -- expressing the extra generator
