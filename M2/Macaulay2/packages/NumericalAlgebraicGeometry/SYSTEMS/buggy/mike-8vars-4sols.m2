needsPackage "NumericalAlgebraicGeometry"
FF = CC_53
FF = QQ
R1 = FF[c_1, c_2, c_3, c_4, s_1, s_2, s_3, s_4]
P1 = {16*s_4^4-20*s_4^2+5, -4*s_4^3+s_3+3*s_4, 4*s_4^3+s_2-3*s_4, s_1+s_4, -4*s_4^2+2*c_4+3, 2*s_4^2+c_3-1, 2*s_4^2+c_2-1, -4*s_4^2+2*c_1+3}
elapsedTime sols = solveSystem(P1,Software=>M2engine) -- this is default
elapsedTime sols = solveSystem(P1,Software=>M2engine, PostProcess=>false) 

elapsedTime sols = solveSystem(P1,Software=>M2engine, CorrectorTolerance => 1e-10, PostProcess=>false) 

elapsedTime sols = solveSystem(P1,Software=>M2) -- this is slow prototype code in top-level M2
sols = select(sols,s->status s== Regular)
#sols
assert all(sols,s->norm sub(matrix{P1}, matrix s)<0.001)
solsM = solutionsWithMultiplicity sols
#solsM -- this is 4
VerticalList solsM

sols = solveSystem(P1,Software=>BERTINI)
#sols
# solutionsWithMultiplicity sols


Rh = FF[gens R1, h]
Ph = homogenize(sub(ideal P1,Rh),h)
decompose Ph
degree Ph
P1 / degree
