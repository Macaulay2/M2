debug Core
R = QQ[a..d,y1,y2,y3,MonomialOrder=>{4,3}]
J = ideal(y1-a^2,y2-a*b,y3-b^2)
M = matrix{{a^3*b}}
F = map(R,R,{a,b,c,d,a^2+b^2,a*b,b^2+c^2-d^2})
assert(rawSubduction(rawMonoidNumberOfBlocks raw monoid R, raw M, raw F, raw gb J) == raw matrix"abc2-abd2")
