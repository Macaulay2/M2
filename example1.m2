restart
needs "../code/solveViaMonodromy.m2"
needs "cyclic.m2"
plugin'c0 = map(R,AR,vars R | matrix c0) -- the actual polynomial system we solve
apply(polysP,p->plugin'c0 p) 
stop = (n,L)->n>10
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}

-- two vertex

elapsedTime sols = twoNodes(SP,c0,{pre0},3,StoppingCriterion=>stop)

G = first sols
E = G.Edges
for e in E do (
    print peek e.Correspondence12;
    print peek e.Correspondence21;
    )

-- other strategies
elapsedTime sols = solveViaMonodromy(SP,c0,{pre0},StoppingCriterion=>stop)
elapsedTime sols = flowerStrategy(SP,c0,{pre0},StoppingCriterion=>stop);
sols




