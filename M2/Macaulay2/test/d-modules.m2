R = QQ[x,y]
load "D-modules.m2"
A = deRhamAll(x^2+y^3)
assert A.?TransferCycles
B = deRhamAll(x^2+y^2)
assert B.?TransferCycles				    -- seems to fail for homgeneous polynomials
