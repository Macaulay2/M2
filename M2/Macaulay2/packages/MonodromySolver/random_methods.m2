-- this file is probably deprecated: solveViaMonodromy depends on randomWeights, which is essentially a rewrite of rp1d below

-- computes a random projection p:R^n->R^1
rp1d = n -> matrix(RR, {drop(flatten apply(ceiling(n/2),i-> (
	    us := apply(2, i -> random(sub(0,RR), sub(1,RR)));
	    us = {sqrt(-2* log first us), 2*pi* last us};
	    {first us * cos last us, first us * sin last us})), sub(mod(n,2),ZZ))})

-- samples from a Poisson(x) distribution (use for x<= 30)
pois = x -> (
    (L,k,p) := (exp(-x), 0, 1);
    while p > L do (
	k = k+1;
	u := random(sub(0,RR), sub(1,RR));
	p = p * u;
	);
    k-1
    )

