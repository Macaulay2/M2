-- test 'terms'
QQ[a,b][x,y]
assert( (terms (1+a+b)^3) === {a^3, 3*a^2*b, 3*a*b^2, b^3, 3*a^2, 6*a*b, 3*b^2, 3*a, 3*b, 1+a-a} )
assert( (terms (1+a+x)^3) === {x^3, (3*a+3)*x^2, (3*a^2+6*a+3)*x, a^3+3*a^2+3*a+1+x-x} )
try terms(ZZ,x) -- used to crash


-- another terms test, fixed 1/5/09
kk = QQ[a]/(a^2+1)
K = toField kk
R = K[x, y]
G = a*x^28+x^28 + a*x^2*y^5
assert(terms G === {(a+1)*x^28, a*x^2*y^5})

-- previously, rawTerm returned 0 and then we'd segfault creating the ideal
debug Core
R = QQ[x,y]
assert try rawTerm(raw R, raw 1_(ZZ/2), rawMakeMonomial {(0,1)}) then false else true
ideal vars R

---------------
-- Ring_List --
---------------

R = QQ[x,y,z]
assert Equation(R_{2,0,3}, x^2*z^3)
assert Equation(R_{0,0,0}, 1)
assert Equation(R_{}, 1)
assert Equation(R_{2}, x^2)
assert try R_{1,1,1,1} then false else true -- too many exponents
assert try R_{2,-1,3} then false else true  -- negative, but not Laurent

R = QQ[x,y, Inverses => true, MonomialOrder => Lex]
assert Equation(R_{-2,3}, x^(-2)*y^3)
assert Equation(R_{-2,-3}, x^(-2)*y^(-3))
assert Equation(R_{-2,0}, x^(-2))
assert Equation(R_{3,2}, x^3*y^2)

R = QQ[x,y]/ideal(x^2-y)
assert Equation(R_{3,1}, x*y^2)
R = QQ[x,dx, WeylAlgebra => {x => dx}]
assert Equation(R_{1,2}, x*dx^2)
R = QQ[x,y,z, SkewCommutative => true]
assert Equation(R_{1,0,1}, x*z)
-- the square of a skew commutative variable is zero
assert Equation(R_{2,0,0}, 0)
assert Equation(R_{1,0,2}, 0)
assert Equation(R_{0,3,0}, 0)
S = R/ideal(x*y)
assert Equation(S_{1,0,1}, x*z)
assert Equation(S_{0,0,2}, 0)
R = QQ[x,y,a,b, SkewCommutative => {2,3}]
-- only the skew commutative variables square to zero
assert Equation(R_{2,0,0,0}, x^2)
assert Equation(R_{0,0,2,0}, 0)

R = QQ[a][x,y]
assert Equation(R_{2,1}, x^2*y)
assert Equation(R_{1,1,1}, a*x*y)
assert Equation(R_{0,0,1}, a)
assert try R_{1,1,1,1} then false else true
R = QQ[a,b][c][x,y]
assert Equation(R_{1,1}, x*y)
assert Equation(R_{1,1,1,1,1}, a*b*c*x*y)

F = frac(QQ[x,y])
assert Equation(F_{1,2}, x*y^2)
assert Equation(F_{-1,2}, y^2/x)
assert Equation(F_{-1,-1}, 1/(x*y))
assert Equation(F_{0,-3}, 1/y^3)
assert Equation(F_{}, 1)
F = frac(QQ[x,y]/ideal(x^2-y-1))
assert Equation(F_{1,1}, x*y)
assert Equation(F_{-2,-2}, 1/(y^3+y^2))
F = frac((QQ[a])[x,y])
assert Equation(F_{1,1,1}, a*x*y)

F = GF 8
assert Equation(F_{2}, a^2)
assert Equation(F_{-1}, a^2+1)
assert Equation(F_{0}, 1)
assert Equation(F_{}, 1)
assert try F_{1,1} then false else true
F = GF 9
assert Equation(F_{2}, a+1)
assert Equation(F_{3}, -a+1)
F = GF 2^20
assert Equation(F_{3}, a^3)

R = (ZZ/101)[x,y, Constants => true]
assert Equation(R_{2,3}, x^2*y^3)
assert Equation(R_{1}, x)
assert Equation(R_{5,0}, x^5)
assert Equation(R_{0,7}, y^7)
assert Equation(R_{0,0}, 1)
assert Equation(R_{}, 1)
assert try R_{1,1,1} then false else true
assert try R_{-1,2} then false else true
R = (ZZ/7)[x,y,z, Constants => true]
assert Equation(R_{1,2,3}, x*y^2*z^3)
assert Equation(R_{2,0,0}, x^2)

assert Equation(ZZ_{}, 1)
assert Equation(QQ_{}, 1)
assert Equation((ZZ/7)_{}, 1)
assert Equation(RR_{}, 1)
assert try (ZZ/7)_{1} then false else true

M = monoid[x,y,z]
assert Equation(M_{2,0,3}, x^2*z^3)
assert Equation(M_{}, 1)
assert try M_{1,1,1,1} then false else true
assert Equation(M_{2,1,0} / M_{0,1,0}, x^2)

-- Ring _ List always passes a coefficient of 1 and screens the exponent
-- vector first, so call rawTerm directly to reach the engine's own checks.

-- coefficients other than 1
R = QQ[x,y]
m = rawMakeMonomial {(0,2)}
assert Equation(new R from rawTerm(raw R, raw(3/2), m), 3/2*x^2)
F = GF 9
m = rawMakeMonomial {(0,1)}
assert Equation(new F from rawTerm(raw F, raw 2_(ZZ/3), m), 2*a)
R = (ZZ/101)[x,y, Constants => true]
m = rawMakeMonomial {(0,2)}
assert Equation(new R from rawTerm(raw R, raw 5_(ZZ/101), m), 5*x^2)

-- the coefficient must come from the ring's own coefficient ring
R = QQ[x,y]
m = rawMakeMonomial {(0,1)}
assert try rawTerm(raw R, raw 1_(ZZ/2), m) then false else true
F = GF 8
assert try rawTerm(raw F, raw 1_(ZZ/3), m) then false else true  -- wrong char
assert try rawTerm(raw F, raw 1_QQ, m) then false else true
assert try rawTerm(raw F, raw 1_F, m) then false else true  -- not a prime field
R = (ZZ/101)[x,y, Constants => true]
assert try rawTerm(raw R, raw 1_(ZZ/7), m) then false else true
assert try rawTerm(raw R, raw 1_QQ, m) then false else true

-- an exponent vector naming a variable the ring does not have
R = QQ[x,y,z]
m = rawMakeMonomial {(3,1)}
assert try rawTerm(raw R, raw 1_QQ, m) then false else true
R = (ZZ/101)[x,y, Constants => true]
m = rawMakeMonomial {(2,1)}
assert try rawTerm(raw R, raw 1_(ZZ/101), m) then false else true

-- a tower takes the coefficient ring's variables in the coefficient
R = QQ[a][x,y]
m = rawMakeMonomial {(0,1),(1,1)}
assert Equation(new R from rawTerm(raw R, raw(a^4), m), a^4*x*y)

-- every Galois field representation agrees
m = rawMakeMonomial {(0,1)}
scan({"Flint", "FlintBig", "New", "Old"}, s -> (
    F := GF(9, Strategy => s);
    assert Equation(new F from rawTerm(raw F, raw 2_(ZZ/3), m), 2*F_0)))

---------------
-- List_Ring --
---------------

R = QQ[x,y,z]
assert Equation({({2,0,3}, 1/2)}_R, 1/2*x^2*z^3)
assert Equation({({0,0,0}, 3/4)}_R, 3/4)
assert Equation({({2}, 1)}_R, x^2)  -- short exponent vectors pad with zeros
assert Equation({({}, 5)}_R, 5)
assert Equation({}_R, 0)            -- the empty sum, not the empty product
assert(ring {}_R === R)

-- coefficients are promoted
assert Equation({({1,0,0}, 3)}_R, 3*x)

-- the terms need not be sorted, repeated monomials are collected
assert Equation({({0,1,0}, 1), ({2,0,0}, 1)}_R, x^2+y)
assert Equation({({1,0,0}, 1), ({1,0,0}, 2)}_R, 3*x)
assert Equation({({1,0,0}, 1), ({1,0,0}, -1)}_R, 0)
assert Equation({({1,0,0}, 0)}_R, 0)

f = x^2*z^3 - 1/2*y + 7
assert Equation((listForm f)_R, f)

assert try {{1,1,1}}_R then false else true       -- pairs, not lists
assert try {({1,1,1}, 1, 1)}_R then false else true
assert try {(1, 2)}_R then false else true        -- exponents must be a list
assert try {({1/2,0,0}, 1)}_R then false else true
assert try {({1,1,1,1}, 1)}_R then false else true -- too many exponents
assert try {({-1,0,0}, 1)}_R then false else true  -- negative, not Laurent
assert try {({1,0,0}, 1_(ZZ/2))}_R then false else true -- no promotion

R = ZZ/101[x,y]
assert Equation({({1,1}, 3)}_R, 3*x*y)
f = 3*x*y + 5
assert Equation((listForm f)_R, f)

R = QQ[x,y, Inverses => true, MonomialOrder => Lex]
assert Equation({({-2,3}, 1)}_R, x^(-2)*y^3)
assert Equation({({-2,-3}, 1/2)}_R, 1/2*x^(-2)*y^(-3))
f = x^(-2)*y^3 + x*y^(-1)
assert Equation((listForm f)_R, f)

R = QQ[x,y]/ideal(x^2-y)
assert Equation({({3,1}, 1)}_R, x*y^2)  -- reduced modulo the ideal
f = x*y^2 + 1
assert Equation((listForm f)_R, f)

R = QQ[x,dx, WeylAlgebra => {x => dx}]
assert Equation({({1,2}, 1)}_R, x*dx^2)
f = x*dx^2 + dx
assert Equation((listForm f)_R, f)

R = QQ[x,y,z, SkewCommutative => true]
assert Equation({({1,0,1}, 1)}_R, x*z)
f = x*z - y*z
assert Equation((listForm f)_R, f)
assert Equation({({2,0,0}, 1)}_R, 0)
assert Equation({({2,0,0}, 1), ({1,0,1}, 1)}_R, x*z)

R = QQ[a][x,y]
assert Equation({({1,1}, 1)}_R, x*y)
assert Equation({({1,1}, a^3)}_R, a^3*x*y)
f = (a^2+1)*x*y + a
assert Equation((listForm f)_R, f)

F = GF 9
R = F[x,y]
assert Equation({({1,0}, F_0)}_R, F_0*x)
f = F_0*x + 1
assert Equation((listForm f)_R, f)

R = frac(QQ[x,y])
assert Equation({({1,-2}, 1)}_R, x/y^2)
assert Equation({({1,0}, 1), ({0,-1}, 1)}_R, x + 1/y)

R = (ZZ/101)[x,y, Constants => true]
assert Equation({({2,3}, 1)}_R, x^2*y^3)
assert Equation({({2,3}, 1), ({1,0}, 5)}_R, x^2*y^3 + 5*x)

-- List _ RingFamily
assert Equation({}_RR, 0)
assert(ring {}_RR === RR_53)
