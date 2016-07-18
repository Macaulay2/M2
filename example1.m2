restart
needs "../code/solveViaMonodromy.m2"
needs "cyclic.m2"
plugin'c0 = map(R,AR,vars R | matrix c0) -- the actual polynomial system we solve
apply(polysP,p->plugin'c0 p) 
stop = (n,L)->n>1
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}
elapsedTime sols = flowerStrategy(SP,c0,{pre0},StoppingCriterion=>stop);





