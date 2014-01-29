restart
loadPackage "NumericalHilbert"
load "old-deflation.m2"


-- CYCLIC4: all discovered components are embedded
RQQ = QQ[x_1..x_4]
M = matrix{{x_1 + x_2 + x_3 + x_4, x_1*x_2 + x_2*x_3 + x_3*x_4 + x_4*x_1, x_2*x_3*x_4 + x_1*x_3*x_4 + x_1*x_2*x_4 + x_1*x_2*x_3, x_1*x_2*x_3*x_4 - 1}}

-- NPD2.8: pseudo-component at the origin
RQQ = QQ[x_1..x_3]
M = matrix{{x_1^2,x_1*x_2*x_3}}

-- NPD3.10: all components are embedded
RQQ = QQ[x_1..x_3]
M = matrix{{x_1^2,x_1*x_2^2*x_3,x_1*x_2*x_3^3}}

-- orimary components
pri = primaryDecomposition ideal M
pri/radical
pri/dim  

-- visible components at d = 0
decompose ideal M

-- projection function
project = I -> (
    extra'vars := set gens ring I - set flatten entries sub(vars RQQ, ring I);
    sub(eliminate(toList extra'vars,I),RQQ)
    )

-- visible components at
d = 1
lifted1 = decompose dIdeal(ideal M, d)
projected1 = lifted1/project
projected1 / dim

-- visible components at
d = 2
lifted2= decompose dIdeal(ideal M, d)
projected2 = lifted2/project
projected2 / dim

-- visible components at
d = 3
lifted3 = decompose dIdeal(ideal M, d)
projected3 = lifted3/project
projected3 / dim
