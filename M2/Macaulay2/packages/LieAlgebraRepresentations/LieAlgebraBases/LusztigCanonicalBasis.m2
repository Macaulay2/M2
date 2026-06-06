-- Goal: implement Lusztig's canonical basis of g
-- using the formulas from Geck and Lang,
-- "Canonical structure constants for simple Lie algebras", arXiv:2404.07652v1




-- Use the Dynkin diagram numbering in LieAlgebraRepresentations
-- Check: does this match Bourbaki?

epsfunction = (g) -> (
    if not isSimple(g) then error "Not implemented yet";
    if member(g#"RootSystemType",{"A","B","C","F","G"}) then return i -> (-1)^i;
    m:=g#"LieAlgebraRank";
    if g#"RootSystemType"=="D" then (
        return i -> (if i<=m-3 then (-1)^i else (-1)^(m-2))
    );
    if g#"RootSystemType"=="E" then (
        return i -> (if member(i+1,{1,4,6,8}) then 1 else -1)
    );    
);


allRoots = (g) -> (
    PhiPlus:=positiveRoots(g);
    join(PhiPlus,apply(PhiPlus, v -> -v))
);



p = (alpha,beta,g) -> (
    Phi:=allRoots(g);
    if alpha==beta or alpha==-beta then return "Does not exist";
    i:=0;
    while member(beta+(i+1)*alpha, Phi) do i=i+1;
    i
);



q = (alpha,beta,g) -> (
    Phi:=allRoots(g);
    if alpha==beta or alpha==-beta then return "Does not exist";
    i:=0;
    while member(beta-(i+1)*alpha, Phi) do i=i+1;
    i
);



sgn = (alpha,g) -> (
    if member(alpha,positiveRoots(g)) then 1 else -1
)



writeInDelta = (w,g) -> (
    M:=matrix apply(simpleRoots(g), a -> 1/1*a);
    v:=(inverse transpose M)*(transpose matrix {w});
    apply(flatten entries v, i -> lift(i,ZZ))
);



writeInDeltaCheck = (w,g) -> (
    M:=matrix apply(simpleRoots(g), a -> (2/killingForm(g,a,a))*a);
    v:=(inverse transpose M)*(transpose matrix {(2/killingForm(g,w,w))*w});
    flatten entries v
);



-- Implement formula from [GL] Definition 3.5
simplyLacedEtaHat = (alpha,beta,g) -> (
    eta:=sgn(alpha,g)*sgn(beta,g)*sgn(alpha+beta,g);
    n:=writeInDelta(alpha,g);
    m:=writeInDelta(beta,g);
    A:=cartanMatrix(g);
    Delta:=simpleRoots(g);
    eps:=epsfunction(g);
    eta*product(apply(#Delta, i -> product apply(#Delta, j -> (eps(i))^(A_(i,j)*n_i*m_j))))
);



etaHat = (alpha,beta,gtilde) -> (
    if member(gtilde#"RootSystemType",{"A","D","E"}) then return simplyLacedEtaHat(alpha,beta,gtilde);
    FD:=gtilde.cache#"FoldingData";
    g:=FD_0;
    FD=FD_1;
    alphalift:=first(FD#alpha);
    i:=0;
    betalift:=(FD#beta)_i;
    Phi:=allRoots(g);
    while not member(alphalift+betalift,Phi) do (
        i=i+1;
	betalift=(FD#beta)_i;
    );
    simplyLacedEtaHat(alphalift,betalift,g)
);



-- Compare to level in lieAlgebraModules.m2
ht = (w,g) -> (
    M:=matrix apply(simpleRoots(g), a -> 1/1*a);
    v:=(inverse transpose M)*(transpose matrix {w});
    lift(sum flatten entries v,ZZ)
);


-- First, compute brackets on the basis elements
brLusztigVar = memoize((x,y,g,LusztigRtoZ,weight,E) -> (
    if x==0 or y==0 then return 0;
    -- Geck and Lang's convention for the Cartan matrix is the transpose of
    -- the convention used in LieAlgebraRepresentations
    a:=transpose cartanMatrix(g);
    m:=g#"LieAlgebraRank";
    Phi:=allRoots(g);
    R:=ring(x);
    n:={};
    i:=0;
    -- Case 1. Both x and y are in h
    -- Then they commute
    if LusztigRtoZ#x < m and LusztigRtoZ#y < m then return 0_R;   
    -- Case 2. x is in h, and y is not
    -- Write x = h_i, y = e_alpha = sum n_j alpha_j
    -- Then [h_i,e_alpha] = alpha(h_i) e_alpha
    --                    = (sum n_j a_ij) e_alpha
    if LusztigRtoZ#x < m and LusztigRtoZ#y >= m then (
       n = writeInDelta(weight(y),g);
       i = LusztigRtoZ#x;
       return (sum apply(m, j -> n_j*a_(i,j)))*y
    );   
    -- Case 3. x is not in h, y is in h
    -- Flip and use Case 2.
    if LusztigRtoZ#x >= m and LusztigRtoZ#y < m then (
        return -brLusztigVar(y,x,g,LusztigRtoZ,weight,E)
    ); 
    -- Case 4. Both x and y are not in h
    -- x = e_alpha, y = e_beta
    alpha:=weight(x);
    beta:=weight(y);
    ---- Case 4a. alpha+beta = 0
    if alpha==-beta then (
	--print toString "Case 4a" << endl;
	n = writeInDeltaCheck(alpha,g);
	return (-1)^(ht(alpha,g))*(sum apply(m, i -> n_i*R_i))
    );
    ---- Case 4b1. alpha+beta != 0, alpha + beta is not in Phi
    if not member(alpha+beta,Phi) then return 0_R;
    ---- Case 4b2. alpha+beta != 0, alpha + beta is in Phi
    ---- Note: q_(a,b)=0 if g simply laced and a+b in Phi, and then this factor is 1 in the formula below
    etaHat(alpha,beta,g)*(q(alpha,beta,g)+1)*(E#(alpha+beta))
));

-- Now extend the bracket using bilinearity

brLusztig = (v,w,g,LusztigRtoZ,weight,E) -> (
    T1:=terms v;
    T2:=terms w;
    sum flatten apply(T1, t1 -> apply(T2, t2 -> leadCoefficient(t1)*leadCoefficient(t2)*brLusztigVar(leadMonomial(t1),leadMonomial(t2),g,LusztigRtoZ,weight,E))) 
);



lusztigBasis = (g) -> (
    if member(g#"RootSystemType",{"B","C","F","G"}) then (
        unfg:=unfoldingLieAlgebra(g);
        NodeOrbits:=nodeOrbits(g);
        g.cache#"FoldingData" = (unfg,partition(x -> alphatilde(x,NodeOrbits), allRoots(unfg)));
    );
    m := g#"LieAlgebraRank";
    Delta:=simpleRoots(g);
    PhiPlus:=positiveRoots(g);
    Phi:=allRoots(g);
    eps:=epsfunction(g);
    h:=getSymbol "h";
    e:=getSymbol "e";
    R:=QQ[join(apply(Delta, a -> h_a),apply(Phi, a -> e_a))];
    LusztigRtoZ:=new HashTable from apply(numgens R, i -> {R_i,i});
    weight := (x) -> (if LusztigRtoZ#x < m then apply(m, i -> 0) else Phi_(LusztigRtoZ#x-m));
    E := new HashTable from apply(#Phi, i -> {Phi_i,R_(i+m)});
    writeInLusztigBasis := (f) -> apply(gens R, x -> coefficient(x,f));
    ad := X -> transpose matrix apply(gens R, Y -> writeInLusztigBasis brLusztigVar(X,Y,g,LusztigRtoZ,weight,E));
    L := apply(gens R, X -> ad X);
    kappa := matrix apply(L, i-> apply(L, j -> trace(i*j)));
    cs := casimirScalar irreducibleLieAlgebraModule(highestRoot(g),g);
    cstar := entries transpose(cs*(inverse kappa));
    B:=gens R;
    Bstar := apply(#B, i -> sum apply(#B, j -> ((cstar_i)_j*B_j)));
    new LieAlgebraBasis from {
	"LieAlgebra"=>g,
        "BasisElements"=>gens R,
	"Bracket"=> (A,B) -> brLusztig(A,B,g,LusztigRtoZ,weight,E),
	--"DualBasis"=> slnDualBasis(n,B),
	"DualBasis"=> Bstar,
        "Weights"=> apply(gens R, x -> weight x),
	"Labels" => apply(gens R, x -> toString(x)),
	"RaisingOperatorIndices"=>apply(#PhiPlus, i -> m+i),
	"LoweringOperatorIndices"=>apply(#PhiPlus, i -> m+i+#PhiPlus),
	"WriteInBasis"=>writeInLusztigBasis
    }
);


-- From [GL] Table 2
unfoldingLieAlgebra = (gtilde) -> (
    mtilde:=gtilde#"LieAlgebraRank";
    if gtilde#"RootSystemType"=="B" then return simpleLieAlgebra("D",mtilde+1);
    if gtilde#"RootSystemType"=="C" then return simpleLieAlgebra("A",2*mtilde-1);
    if gtilde#"RootSystemType"=="F" then return simpleLieAlgebra("E",6);
    if gtilde#"RootSystemType"=="G" then return simpleLieAlgebra("D",4);
);

-- Adapted from [GL] Table 2
---- Start indexing at 0 rather than 1
---- Match Dynkin diagram node numbering used in LieAlgebraRepresentations
nodeOrbits = (gtilde) -> (
    mtilde:=gtilde#"LieAlgebraRank";
    if gtilde#"RootSystemType"=="B" then return append(apply(mtilde-1, i -> {i}),{mtilde-1,mtilde});
    if gtilde#"RootSystemType"=="C" then return append(apply(mtilde-1, i -> {i,2*mtilde-2-i}),{mtilde-1});
    if gtilde#"RootSystemType"=="F" then return {{1},{3},{2,4},{0,5}};
    if gtilde#"RootSystemType"=="G" then return {{0,2,3},{1}};
);
--assert(nodeOrbits(simpleLieAlgebra("B",3))== {{0}, {1}, {2,3}})
--assert(nodeOrbits(simpleLieAlgebra("C",3))=={{0, 4}, {1, 3}, {2}})



brhiealpha = (i,alpha,NodeOrbits) -> (
    w:=first select(NodeOrbits, j -> member(i,j));
    sum apply(w, i -> alpha_i)
);

alphatilde = (alpha,NodeOrbits) -> (apply(#NodeOrbits, j -> brhiealpha(first(NodeOrbits_j),alpha,NodeOrbits)))

cartanMatrixOfFolding = (gtilde) -> (
    g:=unfoldingLieAlgebra(gtilde);
    Phi:=allRoots(g);
    Deltatilde:=simpleRoots(gtilde);
    NodeOrbits:=nodeOrbits(gtilde);
    L:=apply(Deltatilde, i -> first select(Phi, w -> alphatilde(w,NodeOrbits)==i));
    mtilde:=gtilde#"LieAlgebraRank";
    sigma:=apply(NodeOrbits, i -> first i);
    matrix apply(mtilde, i -> apply(mtilde, j -> brhiealpha(sigma_j,L_i,NodeOrbits)))
);




