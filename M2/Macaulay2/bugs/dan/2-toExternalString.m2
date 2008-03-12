fix this to output (,,) instead:

    i3 : toExternalString (,,)

    o3 = (null,null,null)

=============================================================================

A = ZZ[a,b]

E = frac A
toExternalString E
toExternalString(E^4)

use A
B = A/(a^2-1,b^2-1)
toExternalString ambient B
toExternalString (B^3)

C = B[x,y]/(a*x+b*y)
D = frac C
toExternalString D

toExternalString (C^4)

-- and this one is worthless:

    i13 : QQ[x]

    o13 = QQ [x]

    o13 : PolynomialRing

    i14 : map(oo^1,,{{x}})

    o14 = | x |

			 1              1
    o14 : Matrix (QQ [x])  <--- (QQ [x])

    i15 : toExternalString oo

    o15 = map(QQ [x]^{{0}}, QQ [x]^{{-1}}, {{x}})
