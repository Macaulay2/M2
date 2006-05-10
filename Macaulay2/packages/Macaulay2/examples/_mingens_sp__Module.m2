R = QQ[a..f]
I = ideal(a,b,c) * ideal(a,b,c)
mingens I
J = ideal(a-1, b-2, c-3)
I = J*J
mingens I
M = matrix{{a^2*b*c-d*e*f,a^3*c-d^2*f,a*d*f-b*c*e-1}}
I = kernel M
J = image mingens I
I == J
trim I
