-- Easy GB computations

R = ZZ/101[x,y,z]
m = matrix{{x,y,z}}
assert(gens gb m == matrix{{z,y,x}})
assert(0 == x % m)

syz m
assert(syz m - matrix {{0, -(z), -(y)}, {-(z), 0, x}, {y, x, 0}} == 0)
syz2 = syz syz m
syz3 = syz syz syz m
assert((syz m) * syz2 == 0)
assert(syz3 == 0)

-------------------
-- second example -
-------------------
R = ZZ/101[symbol x, symbol y, symbol z]
I = ideal(x,y,z^2,x*z+z^2)
assert(gens gb I == matrix {{y, x, z^2}})
mingens I

-------------------
R = ZZ/101[symbol x, symbol y, symbol z]
I = ideal(x*y-y^2, x^2)
gens gb I
mingens I
syz gens I
syz gens gb I
assert(gens gb I == matrix{{x*y-y^2, x^2, y^3}})
y^3 // (gens I)
first first entries((gens I) * (y^3 // (gens I))) == y^3
assert((x*y) % I == y^2)
assert(status gb I == "status: done; S-pairs encountered up to degree 3")
peek gb I
-------------------
R = ZZ/101[symbol x, symbol y, symbol z]
I = ideal(x*y-y^2, x^2, y^4)
gens gb I
assert(gens gb I == matrix{{x*y-y^2, x^2, y^3}})
assert(mingens I == matrix{{x*y-y^2, x^2}})
-------------------
R = ZZ/101[symbol a .. symbol f]
I = ideal((5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10)
gbTrace = 3
time gens gb I;
-------------------
R = ZZ/101[symbol x, symbol y, symbol z]
I = ideal(x*y-1, x^2-x, x^3-z-1)
assert(gens gb I == matrix{{z,y-1,x-1}})
-------------------
R = ZZ/101[symbol x, symbol y, symbol z, MonomialOrder=>Lex]
I = ideal(x*y-1, x^2-x, x^3-z-1)
assert(gens gb I == matrix{{z,y-1,x-1}})
-------------------
