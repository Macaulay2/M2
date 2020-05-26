-- tests for rawFactorOverTower
-- this routine was added in 6 Feb 2013.  But it is very buggy so far.

end

restart
debug Core
R = QQ[a,b,c,MonomialOrder=>Lex]
T = matrix"b2-c"
F = a^2-c
(facs,exps) = rawFactorOverTower(raw F, raw T)
for f in facs list new R from f
product oo


restart
debug Core
R = QQ[a,b,MonomialOrder=>Lex]
T = matrix"b2-2"
F = a^2-2
(facs,exps) = rawFactorOverTower(raw F, raw T)
for f in facs list new R from f
(product oo) % ideal T

restart
debug Core
R = QQ[a,b,c,MonomialOrder=>Lex]
T = matrix"c2-2,b2-b-c"
F = a^2-2
(facs,exps) = rawFactorOverTower(raw F, raw T)
for f in facs list new R from f
(product oo) % ideal T

restart
debug Core
R = QQ[g_2, g_4, r, g_1, e_4, MonomialOrder=>Lex]
T = matrix{{r^2-3,g_4^4+((4*e_4^2+3*g_1^2)/3)*g_4^2+(4*e_4^4+18*e_4^2*g_1^2)/9}}
F = g_2^2+(9/8)*g_4^2+(2*e_4^2+9*g_1^2)/8
(facs,exps) = rawFactorOverTower(raw F, raw T)  -- BUG: F should factor!!
for f in facs list new R from f
(product oo) % ideal T

restart
debug Core
R = QQ[g_2, g_4, r, g_1, e_4, MonomialOrder=>Lex]
T = matrix{{g_4^4+((4*e_4^2+3*g_1^2)/3)*g_4^2+(4*e_4^4+18*e_4^2*g_1^2)/9}}
F = g_2^2+(9/8)*g_4^2+(2*e_4^2+9*g_1^2)/8
(facs,exps) = rawFactorOverTower(raw F, raw T)  -- BUG: F should factor!!
for f in facs list new R from f
(product oo) % ideal T

restart
debug Core
R = ZZ/32003[g_2, g_4, r, g_1, e_4, MonomialOrder=>Lex]
T = matrix{{g_4^4+((4*e_4^2+3*g_1^2)/3)*g_4^2+(4*e_4^4+18*e_4^2*g_1^2)/9}}
F = g_2^2+(9/8)*g_4^2+(2*e_4^2+9*g_1^2)/8
(facs,exps) = rawFactorOverTower(raw F, raw T)  -- BUG: F should factor!!
for f in facs list new R from f
G = (product oo) % ideal T

time facs = towerFactor(T, L_2)
G // gens (ideal(F, T_(0,0)))
