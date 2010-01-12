restart
debug loadPackage "Dmodules"

--R = QQ[x,y];
--a = ideal {x*y*(x+y)*(x+2*y)}; 

R = QQ[x_1..x_3];
a = ideal {x_2^2-x_1*x_3, x_1^3-x_3^2}; 
a = ideal {x_1^3-x_2^2, x_2^3-x_3^2};
a = ideal {x_1^4-x_2^3, x_3^2-x_1*x_2^2};

aS = analyticSpread a

-- candidates for jumping numbers below the analytic spread
lowJumps = select(sort(bFunctionRoots generalB(a_*, 1_R) / minus), c->c<aS)
mI = multiplierIdeal(a,lowJumps)
all(#mI, i->all((mI#i)_*,g->isInMultiplierIdeal(g,a,lowJumps#i)))
