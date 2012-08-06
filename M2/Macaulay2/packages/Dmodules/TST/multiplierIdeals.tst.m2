QQ[x_1..x_3]
I = ideal {x_1^3-x_2^2, x_2^3-x_3^2}; --Shibuta Ex 5.6
(jumps,mI) = jumpingCoefficients I
assert(jumps == {4/3, 29/18, 31/18, 11/6, 35/18})
I1 = multiplierIdeal(I, 35/18, Strategy=>ViaLinearAlgebra, DegreeLimit=>10)
I2 = multiplierIdeal(I, 35/18, Strategy=>ViaColonIdeal)
I3 = multiplierIdeal(I, 35/18, Strategy=>ViaElimination)
R1 = ring I1;
assert(sub(I2,R1)==I1 and sub(I3,R1)==I1)
multiplierIdeal(I,2)