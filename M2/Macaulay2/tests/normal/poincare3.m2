R = QQ[x,y,z]
M1 = module(ideal(x,y,z))
hf = poincare M1
T = (ring hf)_0
assert(hf == poly"3T-3T2+T3")
assert(hf == poincare res M1)

M2 = minimalPresentation M1
assert(hf == poincare M1)
assert(hf == poincare M2)

R2 = ZZ/101[x0,x1,x2,Degrees=>{1,3,5}]
M3 = module(ideal(x0,x1))
pres3 = presentation M3
hf3 = poincare M3
U = (ring hf3)_0
assert(hf3 == poly"U+U3-U4")
