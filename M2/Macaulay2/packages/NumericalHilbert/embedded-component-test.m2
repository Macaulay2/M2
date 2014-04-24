restart
needsPackage "NumericalHilbert"
needsPackage "NumericalAlgebraicGeometry"
needs "old-deflation.m2"


-- CYCLIC4: all discovered components are embedded
RQQ = QQ[x_1..x_4]
M = matrix{{x_1 + x_2 + x_3 + x_4, x_1*x_2 + x_2*x_3 + x_3*x_4 + x_4*x_1, x_2*x_3*x_4 + x_1*x_3*x_4 + x_1*x_2*x_4 + x_1*x_2*x_3, x_1*x_2*x_3*x_4 - 1}}
M = M_{0,1,2}
M = M_{0,1}

-- NPD2.8: pseudo-component at the origin
RQQ = QQ[x_1..x_3]
M = matrix{{x_1^2,x_1*x_2*x_3}}

-- NPD3.10: all components are embedded
RQQ = QQ[x_1..x_3]
M = matrix{{x_1^2,x_1*x_2^2*x_3,x_1*x_2*x_3^3}}

-- primary components
pri = primaryDecomposition ideal M
pri/radical
pri/dim  
pri/degree

-- visible components at d = 0
decompose ideal M
numericalIrreducibleDecomposition ideal M

-- projection function
project = I -> (
    extra'vars := set gens ring I - set flatten entries sub(vars RQQ, ring I);
    sub(eliminate(toList extra'vars,I),RQQ)
    )

-- visible components at
d = 1
D1 = dIdeal(ideal M, d)
lifted1 = decompose D1
lifted1/dim
lifted1/degree
numericalIrreducibleDecomposition D1
projected1 = lifted1/project
projected1 / dim


-- visible components at
d = 2
D2 = dIdeal(ideal M, d)
lifted2= decompose D2
lifted2/dim
lifted2/degree
numericalIrreducibleDecomposition D2 -- does not work for NPD2.8
projected2 = lifted2/project
projected2 / dim

-- visible components at
d = 3
lifted3 = decompose dIdeal(ideal M, d)
projected3 = lifted3/project
projected3 / dim
