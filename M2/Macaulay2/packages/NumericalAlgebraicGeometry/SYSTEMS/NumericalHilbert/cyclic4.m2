restart
loadPackage "NumericalHilbert"

R = CC[x_1..x_4]
M = matrix{{x_1 + x_2 + x_3 + x_4, x_1*x_2 + x_2*x_3 + x_3*x_4 + x_4*x_1, x_2*x_3*x_4 + x_1*x_3*x_4 + x_1*x_2*x_4 + x_1*x_2*x_3, x_1*x_2*x_3*x_4 - 1}}
P = point matrix {{-1,1,1,-1}}
P = point matrix {{-1.0-.53734e-17*ii, 1.0-.20045e-16*ii, 1.0+.89149e-17*ii, -1.0+.18026e-17*ii}}
P = point matrix {{-1.0-.53734e-17, 1.0-.20045e-16, 1.0+.89149e-17, -1.0+.18026e-17}}
gCorners(P,M)
E3 = eliminatingDual(M,3,{R_0},Point=>P)
E2 = eliminatingDual(M,2,{R_0},Point=>P)
dualCompare(E2, colonDual(E3,{R_0}))

restart
loadPackage "NumericalHilbert"
R = CC[x,y, MonomialOrder=>{Weights=>2:-1}, Global=>false]
M = matrix{{x^2,x*y*ii}}
dualInfo(M,Strategy=>BM)
