

-- See Fulton and Harris, Section 18.1, especially p. 270
-- Note: Yijn and Zijn are not the same as type C--the sign in the middle changes

typeDHin = (i,n) -> ( Eijm(i,i,2*n) - Eijm(n+i,n+i,2*n));
typeDXijn = (i,j,n) -> ( Eijm(i,j,2*n)-Eijm(n+j,n+i,2*n));
typeDYijn = (i,j,n) -> ( Eijm(i,n+j,2*n)-Eijm(j,n+i,2*n));
typeDZijn = (i,j,n) -> ( Eijm(n+i,j,2*n)-Eijm(n+j,i,2*n));



-- Want to change between Dynkin basis of the weight lattice and L_i basis
-- Use the formula for type D in [FH, ??]

DtoLMatrixTypeD = memoize((n) -> (
    M:=apply(n-2, i -> apply(n, j -> if j<i then 0 else if j<n-2 then 1 else 1/2));    
    M=append(M, apply(n, j -> if j<n-2 then 0 else 1/2));
    M=append(M, apply(n, j -> if j<n-2 then 0 else if j==n-2 then -1/2 else 1/2));
    matrix M
));


DtoLTypeD = (v) -> (
    M:=DtoLMatrixTypeD(#v);
    flatten entries(M*(transpose matrix {v}))
);


LtoDTypeD = (v) -> (
    M:=DtoLMatrixTypeD(#v);
    w:=flatten entries(M^-1*(transpose matrix {v}));
    apply(w, i -> lift(i,ZZ))
);



unorderedso2nBasisWeights = (n) -> (
    -- First make the weights in the Li basis
    -- Cartan subalgebra: weight 0
    Hweights := apply(n, i -> apply(n, i -> 0));
    -- Xij has weight Li-Lj
    Xweights := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then apply(n, k -> if k==i then 1 else if k==j then -1 else 0/1) )));
    -- Yij has weight Li+Lj
    Yweights := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then apply(n, k -> if k==i then 1 else if k==j then 1 else 0/1) )));
    -- Zij has weight -Li-Lj
    Zweights := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then apply(n, k -> if k==i then -1 else if k==j then -1 else 0/1))));
    Lweights:= flatten {Hweights, Xweights, Yweights, Zweights};
    apply(Lweights, v -> LtoDTypeD v)
);



so2nBasisWeights = (n) -> (
    PhiPlus:=positiveRoots("D",n);
    l:=#PhiPlus;
    flatten {apply(n, i -> apply(n, j -> 0)), PhiPlus, apply(PhiPlus, v -> -v)}
);


-- Permutation to take the basis in the order we originally programmed
-- to the lex level order by positive roots
so2nPermutation = memoize((n) -> (
    unorderedBasisWeights:=unorderedso2nBasisWeights(n);
    Hperm:=apply(n, i -> i);
    PhiPlus:=positiveRoots("D",n);
    positiveRootPerm:=apply(#PhiPlus, i -> first delete(null,apply(#unorderedBasisWeights, j -> if unorderedBasisWeights_j==PhiPlus_i then j)));
    negativeRootPerm:=apply(#PhiPlus, i -> first delete(null,apply(#unorderedBasisWeights, j -> if unorderedBasisWeights_j==-(PhiPlus_i) then j)));
    flatten {Hperm,positiveRootPerm,negativeRootPerm}
));



so2nBasisElements = (n) -> (
    -- Create the basis elements
    Hbasis := apply(n-1, i -> typeDHin(i,n)-typeDHin(i+1,n));
    Hbasis = append(Hbasis, typeDHin(n-2,n)+typeDHin(n-1,n));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then typeDXijn(i,j,n))));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then typeDYijn(i,j,n)))); 
    Zbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then typeDZijn(j,i,n)))); 
    unorderedBasis:=flatten {Hbasis, Xbasis, Ybasis, Zbasis};
    -- Put them in the order of the positive roots
    sigma:=so2nPermutation(n);
    apply(sigma, i -> unorderedBasis_i)
);



so2nDualBasis = (n) -> (
    -- Create the basis elements
    Hbasis := apply(n, i -> typeDHin(i,n));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then typeDXijn(j,i,n))));   
    Zbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then typeDZijn(j,i,n)))); 
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then typeDYijn(i,j,n))));
    unorderedDualBasis:=flatten {Hbasis, Xbasis, Zbasis, Ybasis};
    -- Put them in the order of the positive roots
    sigma:=so2nPermutation(n);
    apply(sigma, i -> unorderedDualBasis_i)
);



so2nBasisLabels = (n) -> (
    -- Create the basis elements
    Hbasis := apply(n, i -> "H_"|toString(i));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then "X_"|toString(i,j) )));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then "Y_"|toString(i,j) ))); 
    Zbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then "Z_"|toString(j,i) ))); 
    unorderedLabels:=flatten {Hbasis, Xbasis, Ybasis, Zbasis};
    -- Put them in the order of the positive roots
    sigma:=so2nPermutation(n);
    apply(sigma, i -> unorderedLabels_i)
);



so2nRaisingOperatorIndices = (n) -> (
    l:=#(positiveRoots("D",n));
    apply(l, i -> n+i)
);



so2nLoweringOperatorIndices = (n) -> (
    l:=#(positiveRoots("D",n));
    apply(l, i -> n+l+i)
);


writeInso2nBasis = (M) -> (
    n:=lift(numrows(M)/2,ZZ);
    Hcoeffs:= apply(n-2, i -> sum apply(i+1, j -> M_(j,j)));
    b:=1/2*(sum apply(n, j -> M_(j,j)));
    a:=sum apply(n-1, j -> M_(j,j))-b;
    Hcoeffs = join(Hcoeffs,{a,b});
    Xcoeffs:= flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then M_(i,j)))); 
    Ycoeffs := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then M_(i,n+j)))); 
    Zcoeffs := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then M_(n+j,i))));
    unorderedCoefficients:= flatten {Hcoeffs, Xcoeffs, Ycoeffs, Zcoeffs};
    -- Put them in the order of the positive roots
    sigma:=so2nPermutation(n);
    apply(sigma, i -> unorderedCoefficients_i)
);



-- Lie algebra
-- Basis
-- Dual basis
-- Weights
-- Labels
-- RaisingOperatorIndices
-- LoweringOperatorIndices
-- WriteInBasis

so2nBasisFH = (n) -> (
    B:=so2nBasisElements(n);
    writeInBasis := writeInso2nBasis;
    br := (A,B) -> A*B-B*A;
    ad := X -> transpose matrix apply(B, Y -> writeInBasis br(X,Y));
    L := apply(B, X -> ad X);
    kappa := matrix apply(L, i-> apply(L, j -> trace(i*j)));
    so2n:=simpleLieAlgebra("D",n);
    cs := casimirScalar irreducibleLieAlgebraModule(highestRoot(so2n),so2n);
    cstar := entries transpose(cs*(inverse kappa));
    Bstar := apply(#B, i -> sum apply(#B, j -> ((cstar_i)_j*B_j)));
    new LieAlgebraBasis from {
	"LieAlgebra"=>simpleLieAlgebra("D",n),
        "BasisElements"=>B,
	"Bracket"=> (A,B) -> A*B-B*A,
	--"DualBasis"=> so2nDualBasis(n),
	"DualBasis"=> Bstar,
        "Weights"=>so2nBasisWeights(n),
	"Labels"=>so2nBasisLabels(n),
	"RaisingOperatorIndices"=>so2nRaisingOperatorIndices(n),
	"LoweringOperatorIndices"=>so2nLoweringOperatorIndices(n),
	"WriteInBasis"=>writeInso2nBasis
    }
);




