restart
loadPackage "Series"

ZZ[x]

-- We can create a series from a rational function:
timing s1 = series(1/(1-x)^700, Degree =>200) --(series, RingElement)
timing s1 = series(1/(1-x)^700,Degree =>300) --(series, RingElement)

s1.polynomial
peek s1
s1#((keys s1)_3)
-- We can create a series using a generating function:
s2 = series(x,i->i^2) --(series, RingElement, Function) 

-- We can create a series by manually typing in some terms of it:
s3 = series(20,1+x+x^2+x^3+x^10) --(series, ZZ, RingElement) 

-- We can create a series to any given precision using the "Degree" option:
s4 = series(1/(1-x),Degree=>8) --(series, RingElement)

-- The key is that series created in most ways can have their precision increased or decreased at will:
s5 = setDegree(10,s1)
s6 = setDegree(2,s5)

-- and old precision calculations are cached when precision is artificially decreased:
peek s6

-- Series can be added, multiplied, subtracted, and negated:
S = s1 + s2
-S

prod = S*S
prod = setDegree(12,prod)
prod = setDegree(1,prod)
peek prod

-- Precision does reasonable things on addition:
s1
prod
s1+prod
peek oo
