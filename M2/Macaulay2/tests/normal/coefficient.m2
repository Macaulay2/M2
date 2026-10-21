R = GF(25,Variable=>a)[x,y,z]
f = ((a+1)*x + a*y + a^2*z)^2
assert Equation(coefficient(x^2, f), -2*a - 1)
assert Equation(coefficient(x*y, f), -a + 1)
assert Equation(coefficient(y^2, f), a - 2)
assert Equation(coefficient(x*z, f), 2)
assert Equation(coefficient(y*z, f), -2*a + 1)
assert Equation(coefficient(z^2, f), 2*a + 2)

S = R[r,s,t]
assert Equation(coefficient(r, a*x*(r + a*s)), a*x)

-- issue #4452 (used to return 1)
assert try coefficient(x, r) then false else true
