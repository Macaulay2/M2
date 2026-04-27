-- from deGraafMainAlgorithm.v6.m2
-- Version 5 is a little faster than version 4,
-- but Gap can do so10, lambda = 30010 almost instantly
--


---------------------------------------------------------------
---------------------------------------------------------------
-- 1. Additional top level functions for noncommutative algebra
---------------------------------------------------------------
---------------------------------------------------------------

-- Hopefully someday everything in this section can be
-- implemented in the engine instead of at top level

ncLeadTerm = (f) -> (
    first terms f
);

ncLeadMonomial = (f) -> (
    (first coefficients(f))_(0,0)
);

ncLeadCoefficient = (f) -> (
    (last coefficients(f))_(0,0)
);


-- Implement formula from de Graaf, page 94
ncSpairWeight = memoize((g1,g2,PhiPlus) -> (
    n:=first exponents(g1);
    m:=first exponents(g2);
    eb:=apply(#n, i -> max {m_i-n_i,0});
    sum apply(#PhiPlus, i -> (n_i+eb_i)*PhiPlus_i)
));


-- Implement formula from de Graaf, page 94
ncSpair = (g1,g2) -> (
    R:=ring(g1);
    n:=first exponents(g1);
    m:=first exponents(g2);
    a:=product apply(#n, i -> R_i^(max {n_i-m_i,0}));
    b:=product apply(#n, i -> R_i^(max {m_i-n_i,0}));
    gamma:=ncLeadCoefficient(b*g1);
    delta:=ncLeadCoefficient(a*g2);
    delta*b*g1-gamma*a*g2
);


ncCoefficient = (m,f) -> (
    --CR:=coefficientRing ring f;
    if f==0 then return f;
    L:=coefficients(f);
    fmonomials:=flatten entries(L_0);
    --fcoefficients:=apply(flatten entries(L_1), c -> lift(c,CR));
    fcoefficients:=flatten entries(L_1);
    for i from 0 to #fmonomials-1 do (
        if fmonomials_i==m then return (fcoefficients_i)
    );
    0_(ring f)
);

-- Using the definition from de Graaf, page 94
ncDivides = (a,b) -> (
    ea:=first exponents(a);
    eb:=first exponents(b);
    all(#ea, i -> ea_i <= eb_i)
);


-- Use this function when LM(g) ncDivides a monomial a in f
ncReduce = (a,f,g) -> (
    ea:=first exponents(a);
    eg:=first exponents(g);
    R:=ring(a);
    c:=product apply(#ea, i -> R_i^(ea_i-eg_i));
    gamma:=ncCoefficient(a,f);
    --CR:=coefficientRing ring f;
    delta:=ncLeadCoefficient(c*g);
    f-gamma*(delta^-1)*c*g
);


reduceHighestPossibleMonomial = (f0, G) -> (
    f:=f0;
    monsf:=flatten entries first coefficients(f);
    for i from 0 to #G-1 do (
      for j from 0 to #monsf-1 do (
	if ncDivides(G_i_0,monsf_j) then (
	    return ncReduce(monsf_j,f,G_i_2)
	)
      )
    );
);

-- Can improve this in several ways
-- 1. Only test monomials that might divide m
-- 2. Only reduce each Spair once
-- 3. Only reduce Spairs that might generate elements of the correct weight
reduceByG = memoize((f0,G) -> (
    f:=f0;
    monsf:=flatten entries first coefficients(f);
    while any(monsf, m -> any(G, g -> ncDivides(g_0,m))) do (
	f = reduceHighestPossibleMonomial(f,G);
	monsf=flatten entries first coefficients(f);
    );
    f
));



---------------------------------------------------------------
---------------------------------------------------------------
-- 2. Additional functions needed for de Graaf's algorithm 
---------------------------------------------------------------
---------------------------------------------------------------

-- I moved these to lieAlgebraBasis.m2




---------------------------------------------------------------
---------------------------------------------------------------
-- 3. de Graaf's algorithm for Blambda, Glambda
---------------------------------------------------------------
---------------------------------------------------------------


deGraafBases = method(
    TypicalValue => Sequence
)


-- Idea: make G a list of pairs
deGraafBases(List,LieAlgebra) := (lambda,g) -> (
    -- Setup
    LAB := lieAlgebraBasis(g);
    V := irreducibleLieAlgebraModule(lambda,g);
    Delta := simpleRoots(g);
    PhiPlus:=reverse positiveRoots(g);
    UNminus:=uNminus(g);
    -- Step 1: Create the extended weight diagram D
    -- and initialize G and B
    WD := weightDiagram(V);
    K := keys WD;
    Dlist := extendedWeightDiagram(V);
    D := partition(mu -> level(lambda-mu,g#"RootSystemType",g#"LieAlgebraRank"), Dlist);
    maxlev := max(keys D);
    print concatenate("max-lev=",toString(maxlev))<<endl;
    G := {};
    B := {1_UNminus};
    Bweights := {lambda};
    -- Step 2
    Mmu:={};
    alreadyReduced:={};
    h:=0;
    for i from 1 to maxlev do (
        for mu in D#i do (
            -- Step 2a: Construct Mmu
    	    Mmu={};
            for j from 0 to #PhiPlus-1 do (
      	        if not member(mu+PhiPlus_j,Dlist) then continue;
      	        Mmu=append(Mmu,apply(select(#B, k -> Bweights_k==mu+PhiPlus_j),k -> star(j,B_k)))
            );
            Mmu = sort unique flatten Mmu;
            Mmu = select(Mmu, a -> not any(G, g -> ncDivides(g_0,a)));
            -- Step 2b:
            if not member(mu,K) then (
                G = join(G,apply(Mmu, i -> {ncLeadMonomial(i),mu,i}))
            ) else (
               -- Step 2c:
                while #Mmu > (WD#mu) do (
      	            for g1 in G do (
    		        if #Mmu <= (WD#mu) then break;
      	                for g2 in G do (
      		            -- Step 2c1
    			    if #Mmu <= (WD#mu) then break;
    			    if g1==g2 then continue;
			    if lambda-ncSpairWeight(g1_0,g2_0,PhiPlus)!=mu then continue;
			    if member({g1_0,g2_0},alreadyReduced) or member({g2_0,g1_0},alreadyReduced) then continue;
      		            h = reduceByG(ncSpair(g1_2,g2_2),G);
			    alreadyReduced = append(alreadyReduced,{g1_0,g2_0});
			    -- Step 2c2
      		            if h!=0 then (
				lmh:=ncLeadMonomial(h);
                                Mmu = delete(lmh,Mmu);
    			        h = ((ncLeadCoefficient(h))^-1)*h;
    			        G = sort append(G,{lmh,mu,h});
      		            );
      	                )
      	            )
    	        );
                -- Step 2d:
                B = join(B,Mmu);
                Bweights = join(Bweights, apply(#Mmu, i -> mu))
    	    );
        );
        print concatenate("Finished level ",toString(i),". {#G,#B}=",toString({#G,#B}) ) << endl;
    );
    (G,B)
);





---------------------------------------------------------------
---------------------------------------------------------------
-- 4. Construct the representation
---------------------------------------------------------------
---------------------------------------------------------------

deGraafRepresentation = method(
    TypicalValue => LieAlgebraRepresentation
)

deGraafRepresentation(List,LieAlgebra) := (lambda,g) -> (
    LAB := lieAlgebraBasis(g);
    V:=irreducibleLieAlgebraModule(lambda,g);
    (U,sigma,sigmainverse):=universalEnvelopingAlgebra(g);
    (Ilambda,BV):=deGraafBases(lambda,g);
    --Ilambda:=apply(Glambda, i -> i_1);
    UNminus:=ring(Ilambda_0_0);
    m:=g#"LieAlgebraRank";
    l:=numgens UNminus;
    fx:=apply(l,i -> 0);
    -- Compute the values lambda(h_i)
    -- and in the term order for U they appear in decreasing order
    -- Old version: using the basis H_i of h
    -- M:=inverse(LAB#"FundamentalDominantWeightValues");
    -- fh:=reverse flatten entries(M*(transpose matrix {lambda}));
    -- New version: using the basis H_(alpha_i) of h
    -- Then lambda(h_i) = omega_i
    fh := reverse lambda;
    fy:=gens UNminus;
    f:=map(UNminus,U,flatten {fx,fh,fy});
    finv:=map(U,UNminus,apply(l, i -> U_(m+l+i)));
    BVlift:=apply(BV, t -> finv(t));
    BVtoZ := new HashTable from apply(#BV, i -> {BV_i,i});
    mons:={};
    c:={};
    redf:=0;
    Lk:={};
    L:=for k from 0 to (dim(g)-1) list (
	print concatenate("Compute rho(B_",toString(k),")") << endl;
        Lk=for i from 0 to #BV-1 list (
	    redf=reduceByG(f((U_(sigmainverse_k))*BVlift_i),Ilambda);
	    if redf==0 then continue;
            (mons,c)=coefficients redf;
	    -- To avoid using lift and the alert it generates, we use value toString entries instead
            apply(numColumns mons, t -> (BVtoZ#(mons_(0,t)),i)=>value toString(c_(t,0)))
        );
        map(QQ^(#BV),QQ^(#BV),flatten Lk)
    );
    if not isLieAlgebraRepresentation(LAB,L) then error "The list of matrices computed does not define a representation";
    lieAlgebraRepresentation(V,LAB,L)
);



