loadPackage "PowerSeries"
R = QQ[x]

-- We can create a series from a rational function:
series(3/(1-x))             --method is (series, RingElement)

-- We can create a series using a generating function:
series(x,i->i^2)            --method is (series, RingElement, Function) 

-- We can create a series given a function that computes successive
-- polynomial approximations:
f = i -> sum(i,j-> j*(x)^j);
series(f)                   --method is (series, Function) 

-- We can create a series by manually typing in some terms of it to a
-- given precision:
series(20,1+x+x^2+x^3+x^10) --method is (series, ZZ, RingElement) 

-- Note this is not the same as casting the polynomial as a series
series(1+x+x^2+x^3+x^10) --method is (series, ZZ, RingElement) 

-- We can create a series to any given precision using the "Degree"
-- option:
S = series(1/(1-x),Degree => 3) --(series, RingElement)

-- The key is that series created in most ways can have their
-- precision increased or decreased at will:
s1 = setDegree(7,S)
s2 = setDegree(2,s1)

-- and old precision calculations are cached when precision is
-- artificially decreased:
peek s2

-- Series can be added, multiplied, subtracted, and negated:
S = s1 + s2
-S
x*S
7+S
S = s1*s2
setDegree(11,S)
-- recall the initial precision of s1 and s2 was lower.


-- Precision does reasonable things on addition:
s1 = series(3,1+x+x^2)
s2 = series(1/(1+x))
S = s1 + s2
setDegree(10,S)


-- Note that we can turn these into polynomials

toPolynomial(s2)
toPolynomial(2,s2)
toPolynomial(10,s2)

-- inverses of Series.
F = series(2-x)
G = inverse F
peek G
setDegree(4,G)

S = series (1/(1+x+x^2))
T = inverse S
timing setDegree(2000,T)


S = series (3,1+x+x^2)
T = inverse S
timing setDegree(2000,T)

-- Equality of Series.
-- By analogy with floating-point numbers.
1.
1p1
1p1 == 1.00000000000000070
1p1 == 1.00000000000000007

A1 = series(1,1+x)
A2 = series(6,1+x)
B = series(1+x+x^2+x^3)
A1 == B
A2 == B
