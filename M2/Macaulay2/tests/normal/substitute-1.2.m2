p = 13;
B = ZZ/p;
C = B[a];
D = C/ideal(a^4-a^3+a^2-a+1);
F = GF D;
E = ambient F;
G = F[z];
netList {{B,describe B},{C,describe C},{D,describe D},{E,describe E},{F,describe F},{G,describe G}}
--        +-+-----------------------------------------------------------------------------------------+
--        | |ZZ                                                                                       |
-- oo30 = |B|--                                                                                       |
--        | |13                                                                                       |
--        +-+-----------------------------------------------------------------------------------------+
--        |C|B[a, Degrees => {1}, Heft => {1}, MonomialOrder => {MonomialSize => 32}, DegreeRank => 1]|
--        | |                                                   {GRevLex => {1}    }                  |
--        | |                                                   {Position => Up    }                  |
--        +-+-----------------------------------------------------------------------------------------+
--        | |          C                                                                              |
--        |D|--------------------                                                                     |
--        | | 4    3    2                                                                             |
--        | |a  - a  + a  - a + 1                                                                     |
--        +-+-----------------------------------------------------------------------------------------+
--        | |        B[a]                                                                             |
--        |E|--------------------                                                                     |
--        | | 4    3    2                                                                             |
--        | |a  - a  + a  - a + 1                                                                     |
--        +-+-----------------------------------------------------------------------------------------+
--        |F|GF 28561                                                                                 |
--        +-+-----------------------------------------------------------------------------------------+
--        |G|F[z, Degrees => {1}, Heft => {1}, MonomialOrder => {MonomialSize => 32}, DegreeRank => 1]|
--        | |                                                   {GRevLex => {1}    }                  |
--        | |                                                   {Position => Up    }                  |
--        +-+-----------------------------------------------------------------------------------------+

-- i8 : F.PrimitiveElement
--        3     2
-- o8 = 6a  + 5a  + 6a - 2

promote(D_0,E)
promote(E_0,F)
promote(D_0,F)

use F
e = map(F,G,{a})					    -- this failed in 1.2 (no method for applying promote), but it worked in 1.1
e a_G							    -- this must be related to the difference between the generator 'a' and the primitive element
assert( a == e a_G )					    -- fails in 1.3, didn't get to here in 1.2, but it worked in 1.1

use D
promote(a,F)						    -- fixed
z+a
p = F.PrimitiveElement

use F
f = 1/5*z^2+a
errorDepth = 0
y = substitute(f,{z=>a})				      -- at least fix this one!
assert( y == 1/5*a^2+a )

-- similar test with internal GF
p = 13;
F = GF 13^3;
G = F[z];
use F
e = map(F,G,{a})
e a_G
assert( a == e a_G )					    -- this works!
