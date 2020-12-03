restart
loadPackage "NumericalHilbert"
R = CC[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
R = QQ[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
R = (ZZ/101)[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
M = matrix {{x^2-x*y^2,x^3}}
M = matrix {{x*y}}
M = matrix {{x^9 - y}}
dualInfo(M,Truncate=>8)
standardBasis(M)
dualHilbert(M,Truncate=>25)
dualBasis(M)
dualInfo(M)
dualInfo(M, Strategy=>DZ)
dualInfo(M,Point=>{0.01,0.01})

restart
loadPackage "NumericalHilbert"
R = CC[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
V = matrix{{x,y}}
W = matrix{{x,y+x,y-x}}
V == W
image(W#"generators")
kernel(W#"generators")

C = last coefficients(V|W)
CV = C_(toList(0..numcols V -1))
CW = C_(toList(numcols V..numcols C-1))
image CV == image CW

restart
loadPackage "NumericalHilbert"
debug NumericalHilbert
R = CC[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
M = matrix{{x-y^2}}
M = matrix{{x^3-y^2}}
M = matrix{{(x^3-y^2)^2, (x+y)*(x^3-y^2)}}
E3 = eliminatingDual(M,3,{R_0})
E2 = eliminatingDual(M,2,{R_0})
E2' = colonDual(E3,{R_0})
dualCompare(E2,E2')
