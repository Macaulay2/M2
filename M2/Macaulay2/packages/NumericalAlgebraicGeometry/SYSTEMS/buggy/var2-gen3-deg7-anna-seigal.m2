-- Anna Seigal, July 2016
restart
needsPackage "NumericalAlgebraicGeometry"
QQ[p,q]
I = ideal(
    2968*p*q^2+210*q^3-960*p^2-3780*p*q+196*q^2+2296*p-350*q-247,
    3205440*p^2*q+96418*q^3+972000*p^2+2767674*p*q+269010*q^2-4728780*p+118048*q-42075,
    43960320*p^3-5740182*q^3-116935488*p^2-153672876*p*q-19751116*q^2+214271592*p-16860102*q-2859727
    )
elapsedTime sols = solveSystem I_*
elapsedTime solsB = solveSystem(I_*,Software=>BERTINI)

--TO FIX: need to normalize equations... which new blackbox does _not_ do
debug NumericalAlgebraicGeometry -- fetch BombieriWeylNormSquared
debug NAGtypes -- fetch toCCpolynomials
F = apply(toCCpolynomials(I_*,53), f->f/(sqrt BombieriWeylNormSquared f)) 

elapsedTime sols = solveSystem F 

areEqual(sortSolutions sols, sortSolutions solsB) 
