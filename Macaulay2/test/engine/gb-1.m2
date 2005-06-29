-- Easy GB computations

gbi = (m) -> gb(m, Algorithm=>Homogeneous)
gbi = (m) -> gb(m, Algorithm=>Inhomogeneous)

gbi = (m) -> gb(m, Algorithm=>Sugarless)
syzi = (m) -> syz(m, Algorithm=>Sugarless)

--gbi = (m) -> gb(m, Algorithm=>Faugere)

R = ZZ/101[x,y,z]
m = matrix{{x,y,z}}
assert(gens gbi  m == matrix{{z,y,x}})
assert(0 == x % m)

syzi m
assert(syzi m - matrix {{0, -(z), -(y)}, {-(z), 0, x}, {y, x, 0}} == 0)
syz2 = syzi syzi m
syz3 = syzi syzi syzi m
assert((syzi m) * syz2 == 0)
assert(syz3 == 0)

-------------------
-- second example -
-------------------
R = ZZ/101[symbol x, symbol y, symbol z]
I = ideal(x,y,z^2,x*z+z^2)
assert(gens gbi I == matrix {{y, x, z^2}})
mingens I

-------------------
R = ZZ/101[symbol x, symbol y, symbol z]
I = ideal(x*y-y^2, x^2)
gens gbi I
mingens I
syzi gens I
syzi gens gbi I
assert(gens gbi I == matrix{{x*y-y^2, x^2, y^3}})
y^3 // (gens I)
first first entries((gens I) * (y^3 // (gens I))) == y^3
assert((x*y) % I == y^2)
assert(status gbi I == "status: done; S-pairs encountered up to degree 3")
peek gbi I
-------------------
R = ZZ/101[symbol x, symbol y, symbol z]
I = ideal(x*y-y^2, x^2, y^4)
gens gbi I
assert(gens gbi I == matrix{{x*y-y^2, x^2, y^3}})
assert(mingens I == matrix{{x*y-y^2, x^2}})
-------------------
R = ZZ/101[symbol a .. symbol f]
I = ideal((5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10)
gbTrace = 3
time gens gbi I;
time gbi I;
-------------------
R = ZZ/101[symbol x, symbol y, symbol z]
I = ideal(x*y-1, x^2-x, x^3-z-1)
assert(gens gbi I == matrix{{z,y-1,x-1}})
-------------------
R = ZZ/101[symbol x, symbol y, symbol z, MonomialOrder=>Lex]
I = ideal(x*y-1, x^2-x, x^3-z-1)
assert(gens gbi I == matrix{{z,y-1,x-1}})
-------------------
R = ZZ/101[symbol a .. symbol f]
I = ideal((5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10-1)
gbTrace = 3
time gens gbi I;
time gbi I;
