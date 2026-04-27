-- See Fulton and Harris, Section 16.1, especially p. 240


typeCHin = (i,n) -> ( Eijm(i,i,2*n) - Eijm(n+i,n+i,2*n));
typeCXijn = (i,j,n) -> ( Eijm(i,j,2*n)-Eijm(n+j,n+i,2*n));
typeCYijn = (i,j,n) -> ( Eijm(i,n+j,2*n)+Eijm(j,n+i,2*n));
typeCZijn = (i,j,n) -> ( Eijm(n+i,j,2*n)+Eijm(n+j,i,2*n));



-- Want to change between Dynkin basis of the weight lattice and L_i basis
-- Use the formula for type C in [FH, Section 17.2], p. 259
DtoLMatrixTypeC = memoize((n) -> (
    transpose matrix apply(n, j -> apply(n, i -> if i<=j then 1 else 0/1))    
));


DtoLTypeC = (v) -> (
    M:=DtoLMatrixTypeC(#v);
    flatten entries(M*(transpose matrix {v}))
);


LtoDTypeC = (v) -> (
    M:=DtoLMatrixTypeC(#v);
    w:=flatten entries(M^-1*(transpose matrix {v}));
    apply(w, i -> lift(i,ZZ))
);


unorderedsp2nBasisWeights = (n) -> (
    -- First make the weights in the Li basis
    -- Cartan subalgebra: weight 0
    Hweights := apply(n, i -> apply(n, i -> 0));
    -- Xij has weight Li-Lj
    Xweights := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then apply(n, k -> if k==i then 1 else if k==j then -1 else 0/1) )));
    -- Yij has weight Li+Lj
    Yweights := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then apply(n, k -> if k==i then 1 else if k==j then 1 else 0/1) )));
    -- Zij has weight -Li-Lj
    Zweights := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then apply(n, k -> if k==i then -1 else if k==j then -1 else 0/1))));
    -- Uij has weight 2Li
    Uweights := apply(n, i -> apply(n, k -> if k==i then 2 else 0/1));
    -- Vij has weight -2Li
    Vweights:= apply(n, i -> apply(n, k -> if k==i then -2 else 0/1));
    Lweights:=flatten {Hweights, Xweights, Yweights,Zweights,Uweights,Vweights};
    apply(Lweights, v -> LtoDTypeC v)
);



sp2nBasisWeights = (n) -> (
    PhiPlus:=positiveRoots("C",n);
    l:=#PhiPlus;
    flatten {apply(n, i -> apply(n, j -> 0)), PhiPlus, apply(PhiPlus, v -> -v)}
);



-- Permutation to take the basis in the order we originally programmed
-- to the lex level order by positive roots
sp2nPermutation = memoize((n) -> (
    unorderedBasisWeights:=unorderedsp2nBasisWeights(n);
    Hperm:=apply(n, i -> i);
    PhiPlus:=positiveRoots("C",n);
    positiveRootPerm:=apply(#PhiPlus, i -> first delete(null,apply(#unorderedBasisWeights, j -> if unorderedBasisWeights_j==PhiPlus_i then j)));
    negativeRootPerm:=apply(#PhiPlus, i -> first delete(null,apply(#unorderedBasisWeights, j -> if unorderedBasisWeights_j==-(PhiPlus_i) then j)));
    sigma:=flatten {Hperm,positiveRootPerm,negativeRootPerm};
    sigma
));




sp2nBasisElements = (n) -> (
    Hbasis := apply(n-1, i -> typeCHin(i,n)-typeCHin(i+1,n));
    Hbasis = append(Hbasis, typeCHin(n-1,n));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then typeCXijn(i,j,n))));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then typeCYijn(i,j,n)))); 
    Zbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then typeCZijn(i,j,n))));
    Ubasis := apply(n, i -> Eijm(i,n+i,2*n));
    Vbasis := apply(n, i -> Eijm(n+i,i,2*n)); 
    unorderedBasis:=flatten {Hbasis, Xbasis, Ybasis, Zbasis,Ubasis,Vbasis};
    -- Put them in the order of the positive roots
    sigma:=sp2nPermutation(n);
    apply(sigma, i -> unorderedBasis_i)
);



sp2nDualBasis = (n) -> (
    B:={};
    Hbasis := apply(n, i -> 1/2*typeCHin(i,n));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then 1/2*typeCXijn(j,i,n))));   
    Zbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then 1/2*typeCZijn(i,j,n)))); 
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then 1/2*typeCYijn(i,j,n))));
    Vbasis := apply(n, i -> Eijm(n+i,i,2*n));
    Ubasis := apply(n, i -> Eijm(i,n+i,2*n));
    unorderedDualBasis:=flatten {Hbasis, Xbasis, Zbasis, Ybasis, Vbasis,Ubasis};
    -- Put them in the order determined by the positive roots
    sigma:=sp2nPermutation(n);
    apply(sigma, i -> unorderedDualBasis_i)
);



sp2nBasisLabels = (n) -> (
    B:={};
    Hbasis := apply(n, i -> "H_"|toString(i));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then "X_"|toString(i,j) )));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then "Y_"|toString(i,j) ))); 
    Zbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then "Z_"|toString(i,j) )));
    Ubasis := apply(n, i -> "U_"|toString(i));
    Vbasis := apply(n, i -> "V_"|toString(i));
    unorderedBasisLabels:=flatten {Hbasis, Xbasis, Ybasis, Zbasis,Ubasis,Vbasis};
    -- Put them in the order determined by the positive roots
    sigma:=sp2nPermutation(n);
    apply(sigma, i -> unorderedBasisLabels_i)
);



sp2nRaisingOperatorIndices = (n) -> (
    l:=#(positiveRoots("C",n));
    apply(l, i -> n+i)
);


sp2nLoweringOperatorIndices = (n) -> (
    l:=#(positiveRoots("C",n));
    apply(l, i -> n+l+i)
);


writeInsp2nBasis = (M) -> (
    n:=lift(numrows(M)/2,ZZ);
    Hcoeffs:= apply(n, i -> sum apply(i+1, j -> M_(j,j)));
    Xcoeffs:= flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then M_(i,j)))); 
    Ycoeffs := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then M_(i,n+j)))); 
    Zcoeffs := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then M_(n+j,i))));
    Ucoeffs := apply(n, i -> M_(i,n+i));
    Vcoeffs := apply(n, i -> M_(n+i,i));    
    unorderedBasisCoefficients:=  flatten {Hcoeffs, Xcoeffs, Ycoeffs, Zcoeffs,Ucoeffs,Vcoeffs};
    -- Put them in the order determined by the positive roots
    sigma:=sp2nPermutation(n);
    apply(sigma, i -> unorderedBasisCoefficients_i)
);



-- Lie algebra
-- Basis
-- Dual basis
-- Weights
-- Labels
-- RaisingOperatorIndices
-- LoweringOperatorIndices
-- WriteInBasis

sp2nBasisFH = (n) -> (
    B:=sp2nBasisElements(n);
    writeInBasis := writeInsp2nBasis;
    br := (A,B) -> A*B-B*A;
    ad := X -> transpose matrix apply(B, Y -> writeInBasis br(X,Y));
    L := apply(B, X -> ad X);
    kappa := matrix apply(L, i-> apply(L, j -> trace(i*j)));
    sp2n:=simpleLieAlgebra("C",n);
    cs := casimirScalar irreducibleLieAlgebraModule(highestRoot(sp2n),sp2n);
    cstar := entries transpose(cs*(inverse kappa));
    Bstar := apply(#B, i -> sum apply(#B, j -> ((cstar_i)_j*B_j)));
    new LieAlgebraBasis from {
	"LieAlgebra"=>simpleLieAlgebra("C",n),
        "BasisElements"=>B,
	"Bracket"=> (A,B) -> A*B-B*A,
	--"DualBasis"=> sp2nDualBasis(n),
	"DualBasis"=>Bstar,
        "Weights"=>sp2nBasisWeights(n),
	"Labels"=>sp2nBasisLabels(n),
	"RaisingOperatorIndices"=>sp2nRaisingOperatorIndices(n),
	"LoweringOperatorIndices"=>sp2nLoweringOperatorIndices(n),
	"WriteInBasis"=>writeInsp2nBasis
    }
);


