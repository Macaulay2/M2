-- -*- coding: utf-8 -*-
-- Helper functions for ToricTopology

projectiveSpace = (n, base) -> (
	I := id_(base^n);
	ones := matrix(apply(n, i -> {-1}));
	R := base[vars(0..n)];
	K := simplicialComplex monomialIdeal {product gens R};
	(K,I|ones)
)

listMinors = (sc, lambda) -> (
	apply(facets(sc), f -> determinant(submatrix(lambda, indices(f))))
)

-- method to compute the subcomplex of sc, restricted to variables indexed by
-- the subset V
-- if V is empty, the empty complex {1} is returned
subComplex = (sc, V) -> (
	if isEmpty V then
		return simplicialComplex {1_(ring sc)};
	varlist := (entries(vars(ring sc)))_0;
	mV := sub(varlist_(V_0-1),ring sc);
	scan(drop(V,1),i->mV=mV*sub(varlist_(i-1),ring(sc)));
	candidates := {};
	for k in (0..(length V)) do (
		candidates = join(candidates, faces(k,sc));
	);
	k:=0;
	lis := {};
	while k!= length(candidates) do (
		if (denominator(sub(mV, ring sc)/(candidates_k)))==1 then (
			lis=append(lis,candidates_k);
			candidates=drop(candidates,{k,k});
			k=k-1;
		);
		k=k+1;
	);
	simplicialComplex(lis)
);

-- given a char matrix lambda (n rows, m cols) and a subset I={i_1, .., i_n} of [n]
-- returns the support of lambda_I = lambda_{i_1} + ... + lambda_{i_n}
charSupport = (lambda, I) -> (
	cI := {};
	m := numgens(source(lambda));
	n := numgens(target(lambda));
	cI=apply(m, i -> 0);
	scan(I, i -> cI = entries((transpose lambda)_(i-1)) + cI);
	fincI := apply(cI, i -> sub(i,ZZ/2));
	toList select(1..m, i -> fincI_(i-1) != 0)
);


simplicialIntToMon = (sc) -> (
	p := max( flatten( sc ) );
	R := ZZ[vars(0..p-1)];
	e := apply(p, i -> 0);
	lis := {};
	for i in (0..length(sc)-1) do (
		lis = append(lis,new MutableList from e);
		for j in sc#i do (
			lis#i#(j-1)=1;
		);
	);
	lismon := apply(lis, i -> R_(toList(i)));
	simplicialComplex(lismon)
);

-- returns the characteristic matrix for the Hessenberg variety sitting on the dual of the n-dimensional permutahedron
lambdaHessenberg = (n) -> (
	-- finds the char matrix
	col1s := {};
	lambdasimplex := id_((ZZ/2)^n)|(transpose (matrix {apply(n,i->1)} ));
	columns := new MutableHashTable;
	i :=0;
	for maxl in subsets(n+1,n) do (
		columns#maxl = lambdasimplex_{i};
		i=i+1;
	);

	vertices := drop(drop(subsets(n+1),1),-1);
	for vert in vertices do (
		if not member(vert, subsets(n+1,n)) then (
			supersets := {};
			scan(subsets(n+1,n), i -> (if (not(i==vert) and isSubset(set(vert),set(i))) then
				supersets= append(supersets,i) ) );
			col := 0;
			scan(supersets, i -> col = col+ columns#i);
			columns#vert = col;
		);
	);

	--finally computes the char matrix
	lambda := columns#(vertices#0);
	for i in 1..(length(vertices)-1) do (
		lambda = lambda | columns#(vertices#i);
	);
	lambda
);

permsimplices = (lis) -> (
	resl :={};
	for fac in lis do (
		if length(last(fac))==1 then return lis
		else (
			tmplis := {};
			for sub in subsets(last(fac),length(last(fac))-1) do (
				tmplis = append(tmplis, append(fac, sub));
			);
			resl = join(resl, tmplis);
		);
	);
	return permsimplices(resl);
)

-- returns the simplicial complex dual to the n-dimensional permutahedron
permutahedronDual = (n) -> (
	vertices := drop(drop(subsets(n+1),1),-1);
	hashgen := {};
	for i in 1..length(vertices) do (
		hashgen = append(hashgen, {vertices#(i-1),i});
	);
	vhash := hashTable(hashgen);

	psimplices := {};
	for i in 0..n do (
		psimplices = append(psimplices,{(subsets(n+1,n))#i});
	);
	simplices := {};
	for permsimplex in permsimplices(psimplices) do (
		simplex := {};
		for sub in permsimplex do (
			simplex = append(simplex, vhash#sub);
		);
		simplices = append(simplices, simplex);
	);
	simplicialIntToMon(simplices)
)
