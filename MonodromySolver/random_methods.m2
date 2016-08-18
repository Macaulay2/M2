-- export{"randomWeights"}
-- one idea for a better point array (untested)
-- other ideas: use image of random projection as key in a hash table (this would be simpler, but how do we mainain 
-- sorted order in key list?), balanced BSTs (not as simple, not sure if hese get used in practice)

needsPackage "NumericalAlgebraicGeometry"

-- computes a random projection p:R^n->R^1
rp1d = n -> matrix(RR, {drop(flatten apply(ceiling(n/2),i-> (
	    us := apply(2, i -> random(sub(0,RR), sub(1,RR)));
	    us = {sqrt(-2* log first us), 2*pi* last us};
	    {first us * cos last us, first us * sin last us})), sub(mod(n,2),ZZ))})

-- randm complex vector
randomWeights = n -> matrix(CC, {apply(n,i-> (
	    us := apply(2, i -> random(sub(0,RR), sub(1,RR)));
	    us = {sqrt(-2* log first us), 2*pi* last us};
	    first us * cos last us + ii * first us * sin last us))})

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

