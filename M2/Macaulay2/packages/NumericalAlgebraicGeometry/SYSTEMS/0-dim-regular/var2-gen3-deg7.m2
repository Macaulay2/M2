-- Anna Seigal, July 2016
needsPackage "NumericalAlgebraicGeometry"
QQ[p,q]
I = ideal(
    2968*p*q^2+210*q^3-960*p^2-3780*p*q+196*q^2+2296*p-350*q-247,
    3205440*p^2*q+96418*q^3+972000*p^2+2767674*p*q+269010*q^2-4728780*p+118048*q-42075,
    43960320*p^3-5740182*q^3-116935488*p^2-153672876*p*q-19751116*q^2+214271592*p-16860102*q-2859727
    )
dim I
degree I
-*
solveSystem(I_*,PostProcess=>false) -- SEG!!!
solveSystem(I_*,Precision=>infinity,PostProcess=>false) --SEG!!!
*-

J = ideal(I_0,I_1)
degree J
dim J
solveSystem(J_*,PostProcess=>false)
solveSystem(J_*,Precision=>infinity,PostProcess=>false)

end

debug needsPackage "NumericalAlgebraicGeometry"
R = CC_1000[p,q]
I = ideal(
    2968*p*q^2+210*q^3-960*p^2-3780*p*q+196*q^2+2296*p-350*q-247,
    3205440*p^2*q+96418*q^3+972000*p^2+2767674*p*q+269010*q^2-4728780*p+118048*q-42075,
    43960320*p^3-5740182*q^3-116935488*p^2-153672876*p*q-19751116*q^2+214271592*p-16860102*q-2859727
    )
F = I_*
F = normalize polySystem F
sols = solveSystem(F,PostProcess=>false) -- {}
solveSystem(F,Precision=>infinity,PostProcess=>false) -- {}
F' = generalEquations(numgens R, F)
F' = normalize polySystem F_{0,1}
solveSystem(F',PostProcess=>false) -- all fail
result = solveSystem(F',Precision=>infinity,PostProcess=>false) 
resultB = solveSystem(F',Software=>BERTINI)
apply(sols, s->evaluate(F.PolyMap,s))
 polySystem F'
conditionNumber(F',coordinates first result) 
end

restart
load "var2-gen3-deg7.m2"
