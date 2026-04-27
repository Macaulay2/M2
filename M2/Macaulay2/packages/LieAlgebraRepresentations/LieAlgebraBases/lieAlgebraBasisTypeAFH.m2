
--Eijm = (i0,j0,m) -> ( matrix apply(m, i -> apply(m, j -> if i==i0 and j==j0 then 1/1 else 0/1)) );
Hin = (i,n) -> ( Eijm(i,i,n) - Eijm(i+1,i+1,n));


-- Want to change between Dynkin basis of the weight lattice and L_i basis
-- Use the formula omega_j = L_1+...+L_j from [FH, Section 15]
-- Very similar to the formula for type C in [FH, Section 17.2]
DtoLMatrixTypeA = memoize((n) -> (
    transpose matrix apply(n, j -> apply(n, i -> if i<=j then 1 else 0/1))    
));



DtoLTypeA = (v) -> (
    M:=DtoLMatrixTypeA(#v);
    flatten entries(M*(transpose matrix {v}))
);



LtoDTypeA = (v) -> (
    M:=DtoLMatrixTypeA(#v);
    w:=flatten entries(M^-1*(transpose matrix {v}));
    apply(w, i -> lift(i,ZZ))
);




LiminusLjTypeA = (i,j,n) -> (
    ei:={};
    if i==n-1 then ei = apply(n-1, k -> -1/1) else ei = apply(n-1, k -> if k==i then 1 else 0/1);
    ej:={};
    if j==n-1 then ej = apply(n-1, k -> -1/1) else ej = apply(n-1, k -> if k==j then 1 else 0/1);
    LtoDTypeA(ei-ej)
);



unorderedslnBasisWeights = (n) -> (
    Hbasis := apply(n-1, i -> apply(n-1, i -> 0));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then LiminusLjTypeA(i,j,n) )));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if j<i then LiminusLjTypeA(i,j,n) ))); 
    flatten {Hbasis, Xbasis, Ybasis}
);



slnBasisWeights = (n) -> (
    PhiPlus:=positiveRoots("A",n-1);
    l:=#PhiPlus;
    flatten {apply(n-1, i -> apply(n-1, j -> 0)), PhiPlus, apply(PhiPlus, v -> -v)}
);



-- Permutation to take the basis in the order we originally programmed
-- to the lex level order by positive roots
slnPermutation = memoize((n) -> (
    unorderedBasisWeights:=unorderedslnBasisWeights(n);
    Hperm:=apply(n-1, i -> i);
    PhiPlus:=positiveRoots("A",n-1);
    positiveRootPerm:=apply(#PhiPlus, i -> first delete(null,apply(#unorderedBasisWeights, j -> if unorderedBasisWeights_j==PhiPlus_i then j)));
    negativeRootPerm:=apply(#PhiPlus, i -> first delete(null,apply(#unorderedBasisWeights, j -> if unorderedBasisWeights_j==-(PhiPlus_i) then j)));
    flatten {Hperm,positiveRootPerm,negativeRootPerm}
));



slnBasisElements = (n) -> (
    -- Create the basis elements    
    Hbasis := apply(n-1, i -> Hin(i,n));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then Eijm(i,j,n))));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if j<i then Eijm(i,j,n))));    
    unorderedBasis:=flatten {Hbasis, Xbasis, Ybasis};
    -- Put them in the order determined by the positive roots
    sigma:=slnPermutation(n);
    apply(sigma, i -> unorderedBasis_i)
);



slnDualBasis = (n,B) -> (
    -- Create the basis elements    
    Hcoeffs := entries(inverse(1/1*cartanMatrix("A",n-1)));
    Hdual := apply(n-1, i -> sum apply(n-1, j -> (Hcoeffs_i_j)*(B_j)));
    Hlabels := apply(n-1, i -> (i,i));
    Xdual := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then Eijm(j,i,n))));   
    Ydual := flatten apply(n, i -> delete(null,apply(n, j -> if j<i then Eijm(j,i,n))));    
    unorderedDualBasis:=flatten {Hdual, Xdual, Ydual};
    -- Put them in the order determined by the positive roots       
    sigma:=slnPermutation(n);
    apply(sigma, i -> unorderedDualBasis_i)
);



slnBasisLabels = (n) -> (
    -- Create the basis elements     
    Hbasis := apply(n-1, i -> "H_"|toString(i+1));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then "E_"|toString(i+1,j+1) )));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if j<i then "E_"|toString(i+1,j+1) ))); 
    unorderedLabels:=flatten {Hbasis, Xbasis, Ybasis};
    -- Put them in the order determined by the positive roots
    sigma:=slnPermutation(n);
    apply(sigma, i -> unorderedLabels_i)
);



slnBasisSubscripts = (n) -> (
    -- Create the basis elements     
    Hbasis := apply(n-1, i -> "H_"|toString(i+1));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then (i,j) )));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if j<i then (i,j) ))); 
    unorderedSubscripts:=flatten {Hbasis, Xbasis, Ybasis};
    -- Put them in the order determined by the positive roots
    sigma:=slnPermutation(n);
    allSubscripts:=apply(sigma, i -> unorderedSubscripts_i);
    l:=#Xbasis;
    apply(l, i -> allSubscripts_(n-1+i))  
);




slnRaisingOperatorIndices = (n) -> (
    h:=lift(n*(n-1)/2,ZZ);
    apply(h, i -> (n-1)+i)
);



slnLoweringOperatorIndices = (n) -> (
    h:=lift(n*(n-1)/2,ZZ);
    apply(h, i -> (n-1)+h+i)
);



writeInslnBasis = (M) -> (
    n:=numRows M;
    Hcoeffs:=apply(n-1, i -> sum apply(i+1, j -> M_(j,j)));
    Xcoeffs := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then M_(i,j))));   
    Ycoeffs := flatten apply(n, i -> delete(null,apply(n, j -> if j<i then M_(i,j))));    
    unorderedCoefficients:=flatten {Hcoeffs,Xcoeffs,Ycoeffs};
    -- Put them in the order determined by the positive roots
    sigma:=slnPermutation(n);
    apply(sigma, i -> unorderedCoefficients_i)
);

br = (A,B) -> A*B-B*A


-- Lie algebra
-- Basis
-- Dual basis
-- Weights
-- Labels
-- RaisingOperatorIndices
-- LoweringOperatorIndices
-- WriteInBasis

slnBasisFH = (n) -> (
    B:=slnBasisElements(n);
    writeInBasis := writeInslnBasis;
    br := (A,B) -> A*B-B*A;
    ad := X -> transpose matrix apply(B, Y -> writeInBasis br(X,Y));
    L := apply(B, X -> ad X);
    kappa := matrix apply(L, i-> apply(L, j -> trace(i*j)));
    sln:=simpleLieAlgebra("A",n-1);
    cs := casimirScalar irreducibleLieAlgebraModule(highestRoot(sln),sln);
    cstar := entries transpose(cs*(inverse kappa));
    Bstar := apply(#B, i -> sum apply(#B, j -> ((cstar_i)_j*B_j)));
    new LieAlgebraBasis from {
	"LieAlgebra"=>simpleLieAlgebra("A",n-1),
        "BasisElements"=>B,
	"Bracket"=>br,
	--"DualBasis"=> slnDualBasis(n,B),
	"DualBasis"=> Bstar,
        "Weights"=>slnBasisWeights(n),
	"Labels"=>slnBasisLabels(n),
	"RaisingOperatorIndices"=>slnRaisingOperatorIndices(n),
	"LoweringOperatorIndices"=>slnLoweringOperatorIndices(n),
	"WriteInBasis"=>writeInslnBasis    }
);

