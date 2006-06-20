-- when is a/b, a%b defined.
-- Is it correctly done too?

A = ZZ/101[x,y,z]
F= (x^2-y)*(y-z)
G = x^2-y
F // G
debug Macaulay2Core
raw F
(raw F) // (raw G)  -- done correctly
(raw 5) // (raw 3)

(raw F) % (raw G)  -- done correctly
(raw 5) % (raw 3)  -- error, as perhaps it should be??

B = A/(x^2-x-1)
1_B // x
raw 1_B // raw x
F = substitute(F,B)

(1_B).RawRingElement // x.RawRingElement

-- The following should be changed.  The try dhould be removed, the last line should be removed.
--o29 = -- code for method: (ZZ/101 [x, y, z])/(x^2-x-1) // (ZZ/101 [x, y, z])/(x^2-x-1)
      -- /capybara/share/Macaulay2/Macaulay2Core/m2/enginering.m2:214-216
--                if R === S then (x,y) -> (
--                     try new R from x.RawRingElement // y.RawRingElement
--                     else first first entries (matrix {{x}} // y))


restart
A = QQ[x]
(x-1) // x

A = ZZ[x]
(x-1) // x

F = poly"x6-6x5+15x4-20x3+15x2-6x+1"
G = poly"x10+5x9+15x8+30x7+45x6+51x5+45x4+30x3+15x2+5x+1"
F//G  -- OK
debug Macaulay2Core
(raw F) // (raw G) -- fails
