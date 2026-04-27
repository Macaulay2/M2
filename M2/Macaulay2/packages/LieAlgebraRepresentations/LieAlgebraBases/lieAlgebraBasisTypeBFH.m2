-- Type B so(2n+1) Lie algebras
-- written by Naufil Sakran, Dinesh Limbu, Rohan Joshi

-- See Fulton and Harris, Section 18.1, especially p. 270

--Eijm = (i0,j0,m) -> ( matrix apply(m, i -> apply(m, j -> if i==i0 and j==j0 then 1/1 else 0/1)) );
--Hin = (i,n) -> ( Eijm(i,i,n) - Eijm(i+1,i+1,n));

typeBHin = (i,n) -> ( Eijm(i,i,2*n+1) - Eijm(n+i,n+i,2*n+1));
typeBXijn = (i,j,n) -> ( Eijm(i,j,2*n+1) - Eijm(n+j,n+i,2*n+1));
typeBYijn = (i,j,n) -> ( Eijm(i,n+j,2*n+1) - Eijm(j,n+i,2*n+1));
typeBZijn = (i,j,n) -> ( Eijm(n+i,j,2*n+1) - Eijm(n+j,i,2*n+1));
typeBUin = (i,n) -> ( Eijm(i,2*n,2*n+1) - Eijm(2*n,n+i,2*n+1));
typeBVin = (i,n) -> ( Eijm(n+i,2*n,2*n+1) - Eijm(2*n,i,2*n+1));


------------------------------------------------


DtoLMatrixTypeB = memoize((n) -> (
    M:=apply(n, i -> apply(n, j -> if j<i then 0 else if j<n-1 then 1 else 1/2));    
    matrix M
));



DtoLTypeB = (v) -> (
    M:=DtoLMatrixTypeB(#v);
    flatten entries(M*(transpose matrix {v}))
);



LtoDTypeB = (v) -> (
    M:=DtoLMatrixTypeB(#v);
    w:=flatten entries(M^-1*(transpose matrix {v}));
    apply(w, i -> lift(i,ZZ))
);



unorderedso2n1BasisWeights = (n) -> (
    -- First make the weights in the Li basis
    -- Cartan subalgebra: weight 0
    Hweights := apply(n, i -> apply(n, i -> 0));
    -- Xij has weight Li-Lj
    Xweights := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then apply(n, k -> if k==i then 1 else if k==j then -1 else 0/1) )));
    -- Yij has weight Li+Lj
    Yweights := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then apply(n, k -> if k==i then 1 else if k==j then 1 else 0/1) )));
    -- Zij has weight -Li-Lj
    Zweights := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then apply(n, k -> if k==i then -1 else if k==j then -1 else 0/1))));
    -- Ui has weight Li
    Uweights := apply(n, i -> apply(n, k -> if k==i then 1 else 0/1));
    -- Vi has weight -Li
    Vweights := apply(n, i -> apply(n, k -> if k==i then -1 else 0/1));
    Lweights:= flatten {Hweights, Xweights, Yweights, Zweights, Uweights, Vweights};
    apply(Lweights, v -> LtoDTypeB v)
);


so2n1BasisWeights = (n) -> (
    PhiPlus:=positiveRoots("B",n);
    l:=#PhiPlus;
    flatten {apply(n, i -> apply(n, j -> 0)), PhiPlus, apply(PhiPlus, v -> -v)}
);


-- Permutation to take the basis in the order we originally programmed
-- to the lex level order by positive roots
so2n1Permutation = memoize((n) -> (
    unorderedBasisWeights:=unorderedso2n1BasisWeights(n);
    Hperm:=apply(n, i -> i);
    PhiPlus:=positiveRoots("B",n);
    positiveRootPerm:=apply(#PhiPlus, i -> first delete(null,apply(#unorderedBasisWeights, j -> if unorderedBasisWeights_j==PhiPlus_i then j)));
    negativeRootPerm:=apply(#PhiPlus, i -> first delete(null,apply(#unorderedBasisWeights, j -> if unorderedBasisWeights_j==-(PhiPlus_i) then j)));
    flatten {Hperm,positiveRootPerm,negativeRootPerm}
));




so2n1BasisElements = (n) -> (
    -- Create the basis elements
    Hbasis := apply(n-1, i -> typeBHin(i,n)-typeBHin(i+1,n));
    Hbasis = append(Hbasis,2*typeBHin(n-1,n));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then typeBXijn(i,j,n))));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then typeBYijn(i,j,n)))); 
    Zbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then typeBZijn(j,i,n))));
    Ubasis := flatten apply(n, i -> typeBUin(i,n));
    Vbasis := flatten apply(n, i -> typeBVin(i,n));
    unorderedBasis:=flatten {Hbasis, Xbasis, Ybasis, Zbasis, Ubasis, Vbasis};
    -- Put them in the order of the positive roots
    sigma:=so2n1Permutation(n);
    apply(sigma, i -> unorderedBasis_i)
);


-*
so2n1DualBasis = (n) -> (
    -- Create the basis elements
    Hbasis := apply(n-1, i -> typeBHin(i,n)-typeBHin(i+1,n));
    Hbasis = append(Hbasis,typeBHin(n-1,n));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then typeBXijn(j,i,n))));   
    Zbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then typeBZijn(j,i,n)))); 
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then typeBYijn(i,j,n))));
    Vbasis := flatten apply(n, i -> -typeBVin(i,n));
    Ubasis := flatten apply(n, i -> -typeBUin(i,n));
    unorderedDualBasis:=flatten {Hbasis, Xbasis, Zbasis, Ybasis, Vbasis, Ubasis};
    -- Put them in the order determined by the positive roots
    sigma:=so2n1Permutation(n);
    apply(sigma, i -> unorderedDualBasis_i)
);
*-


so2n1BasisLabels = (n) -> (
    -- Create the labels
    Hbasis := apply(n, i -> "H_a_"|toString(i));
    Xbasis := flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then "X_"|toString(i+1,j+1) )));   
    Ybasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then "Y_"|toString(i+1,j+1) ))); 
    Zbasis := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then "Z_"|toString(j+1,i+1) ))); 
    Ubasis := apply(n, i -> "U_"|toString(i+1) ); 
    Vbasis := apply(n, i -> "V_"|toString(i+1) ); 
    unorderedBasisLabels:=flatten {Hbasis, Xbasis, Ybasis, Zbasis, Ubasis, Vbasis};
    -- Put them in the order determined by the positive roots
    sigma:=so2n1Permutation(n);
    apply(sigma, i -> unorderedBasisLabels_i)
);



so2n1RaisingOperatorIndices = (n) -> (
    l:=#(positiveRoots("B",n));
    apply(l, i -> n+i)
);



so2n1LoweringOperatorIndices = (n) -> (
    l:=#(positiveRoots("B",n));
    apply(l, i -> n+l+i)
);


--- writeInBasis

-- writeInso2n1Basis
writeInso2n1Basis = (M) -> (
    -- Get the coefficients in the original order
    n:=lift((numrows(M)-1)/2,ZZ);
    Hcoeffs:= apply(n-1, i -> sum apply(i+1, j -> M_(j,j)));
    Hcoeffs= append(Hcoeffs, 1/2*(M_(n-1,n-1)+last Hcoeffs));
    Xcoeffs:= flatten apply(n, i -> delete(null,apply(n, j -> if j!=i then M_(i,j)))); 
    Ycoeffs := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then M_(i,n+j)))); 
    Zcoeffs := flatten apply(n, i -> delete(null,apply(n, j -> if i<j then M_(n+j,i))));
    Ucoeffs := apply(n, i -> M_(i,2*n));
    Vcoeffs := apply(n, i -> M_(n+i,2*n));    
    unorderedBasisCoefficients:= flatten {Hcoeffs, Xcoeffs, Ycoeffs, Zcoeffs, Ucoeffs, Vcoeffs};
    -- Put them in the order determined by the positive roots
    sigma:=so2n1Permutation(n);
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

so2n1BasisFH = (n) -> (
    B:=so2n1BasisElements(n);
    writeInBasis := writeInso2n1Basis;
    br := (A,B) -> A*B-B*A;
    ad := X -> transpose matrix apply(B, Y -> writeInBasis br(X,Y));
    L := apply(B, X -> ad X);
    kappa := matrix apply(L, i-> apply(L, j -> trace(i*j)));
    so2n1:=simpleLieAlgebra("B",n);
    cs := casimirScalar irreducibleLieAlgebraModule(highestRoot(so2n1),so2n1);
    cstar := entries transpose(cs*(inverse kappa));
    Bstar := apply(#B, i -> sum apply(#B, j -> ((cstar_i)_j*B_j)));
    new LieAlgebraBasis from {
	"LieAlgebra"=>simpleLieAlgebra("B",n),
        "BasisElements"=>B,
	"Bracket"=> (A,B) -> A*B-B*A,
	"DualBasis"=> Bstar,
        "Weights"=>so2n1BasisWeights(n),
	"Labels"=>so2n1BasisLabels(n),
	"RaisingOperatorIndices"=>so2n1RaisingOperatorIndices(n),
	"LoweringOperatorIndices"=>so2n1LoweringOperatorIndices(n),
	"WriteInBasis"=>writeInso2n1Basis
    }
);
