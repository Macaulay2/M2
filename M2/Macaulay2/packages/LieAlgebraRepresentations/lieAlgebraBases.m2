needs "./LieAlgebraBases/LusztigCanonicalBasis.m2"
needs "./LieAlgebraBases/lieAlgebraBasisTypeAFH.m2"
needs "./LieAlgebraBases/lieAlgebraBasisTypeBFH.m2"
needs "./LieAlgebraBases/lieAlgebraBasisTypeCFH.m2"
needs "./LieAlgebraBases/lieAlgebraBasisTypeDFH.m2"
needs "./LieAlgebraBases/lieAlgebraBasisTypeGFH.m2"

LieAlgebraBasis = new Type of HashTable  
-- Keys:
-- LieAlgebra
-- BasisElements
-- DualBasis
-- Weights
-- Labels
-- RaisingOperatorIndices
-- LoweringOperatorIndices
-- WriteInBasis
-- FundamentalDominantWeightValues -- need this if using a basis of h that is not {H_(alpha_i)}

net(LieAlgebraBasis) := LAB -> net "Enhanced basis of"expression(LAB#"LieAlgebra")

isDiagonal = (M) -> (
    for i from 0 to numrows(M)-1 do (
	for j from 0 to numColumns(M)-1 do (
	    if i!=j and M_(i,j)!=0 then return false
	)
    );
    true
);



checkLieAlgebraBasis = (LAB) -> (
    -- Check the dimension
    B:=LAB#"BasisElements";
    g:=LAB#"LieAlgebra";
    m:=g#"LieAlgebraRank";
    if #B != dim(g) then (
	return (false,"The basis does not have the correct number of elements")
    );
    -- Check the Jacobi identity
    br := LAB#"Bracket";    
    for i from 0 to #B-1 do (
        for j from 0 to #B-1 do (
            for k from 0 to #B-1 do (
                if br(B_i,br(B_j,B_k))+br(B_j,br(B_k,B_i))+br(B_k,br(B_i,B_j))!=0 then (
		    return (false,concatenate("The Jacobi identity is not satisfied on basis elements ",toString({i,j,k})))
		 )
	     )
	 )
     );
    -- Check the writeInBasis function
    c := apply(#(LAB#"BasisElements"), i -> random(-1000,1000));
    M := sum(#c, i -> c_i*B_i);
    writeInBasis := LAB#"WriteInBasis";
    v := writeInBasis(M);
    if v!=c then (
	return (false,"WriteInBasis function failed")
    );
    -- Check the weights of the adjoint representation
    ad := X -> transpose matrix apply(B, Y -> writeInBasis br(X,Y));
    L := apply(B, X -> ad X);
    if not all(m, i -> isDiagonal(L_i)) then (
	return (false,"Not all basis elements are eigenvectors")
    );
    L1:=apply(#B, i -> apply(m, j -> (L_j)_(i,i)));
    repWts:=apply(L1, v -> apply(v, i -> lift(i,ZZ)));
    WD := weightDiagram irreducibleLieAlgebraModule(highestRoot(g),g);
    if sort(pairs(WD))!=sort(pairs(tally(repWts))) then (
	return (false,"The set of weights is incorrect")
    );
    if repWts != LAB#"Weights" then (
        return (false,"The weights are incorrect")
    );
    -- Check that the Killing form is nondegenerate
    kappa := matrix apply(L, i-> apply(L, j -> trace(i*j)));
    if rank kappa != #B then (
	return (false,"The Killing form is degenerate")
    );
    -- Check the dual basis
    Lstar := apply(LAB#"DualBasis", X -> ad X);
    cs:=casimirScalar irreducibleLieAlgebraModule(highestRoot(g),g);
    if matrix apply(L, i-> apply(Lstar, j -> trace(i*j)))!=matrix apply(#L, i -> apply(#L, j -> if i==j then cs/1 else 0/1)) then (
	return (false,"The dual basis is incorrect")
    );	
    -- Check the Cartan matrix
    CM := cartanMatrix(g);
    if CM != matrix apply(m, k -> (LAB#"Weights")_(m+k)) then (
	return (false, "The Cartan matrix is incorrect")
    );
    (true,"")
)


-- Available methods:
-- "Lusztig" -- the Lusztig canonical basis as described by Geck-Lang
-- "FH" -- the basis described by Fulton Harris with a Chevalley basis on the Cartan subalgebra \mathfrak{h}
-- "FH2" -- the basis described by Fulton Harris for each $\mathfrak{g}_{\alpha}$ along with a naive basis of the Cartan subalgebra \mathfrak{h}

lieAlgebraBasis = method(
    Options=>{"Check"=>true,"Method"=>"FH"},
    TypicalValue => LieAlgebraBasis
    )

lieAlgebraBasis(String,ZZ) := o -> (type,m) -> (
    LAB:={};
    if o#"Method"=="Lusztig" then LAB=lusztigBasis(simpleLieAlgebra(type,m));    
    if type=="A" then LAB=slnBasisFH(m+1);
    if type=="B" then LAB=so2n1BasisFH(m);
    if type=="C" then LAB=sp2nBasisFH(m);
    if type=="D" then LAB=so2nBasisFH(m);
    if type=="G" and m==2 then LAB=g2BasisFH();
    if o#"Check" then (
	(b,errorString):=checkLieAlgebraBasis(LAB);
	if not b then error errorString;
    );
    LAB
);

Eijm = (i0,j0,m) -> ( matrix apply(m, i -> apply(m, j -> if i==i0 and j==j0 then 1/1 else 0/1)) );

lieAlgebraBasis(LieAlgebra) := o -> (g) -> (
    if not isSimple(g) then error "Lie algebra bases are only implemented for simple Lie algebras so far" << endl;
    LAB:={};
    if o#"Method"=="Lusztig" and isSimple(g) then LAB = lusztigBasis(g);
    if o#"Method"=="FH" and isSimple(g) and g#"RootSystemType"=="A" then LAB = slnBasisFH(g#"LieAlgebraRank"+1);
    if o#"Method"=="FH" and isSimple(g) and g#"RootSystemType"=="B" then LAB = so2n1BasisFH(g#"LieAlgebraRank");
    if o#"Method"=="FH" and isSimple(g) and g#"RootSystemType"=="C" then LAB = sp2nBasisFH(g#"LieAlgebraRank");
    if o#"Method"=="FH" and isSimple(g) and g#"RootSystemType"=="D" then LAB = so2nBasisFH(g#"LieAlgebraRank");
    if o#"Method"=="FH" and g==simpleLieAlgebra("G",2) then LAB = g2BasisFH();
    if o#"Check" then (
	(b,errorString):=checkLieAlgebraBasis(LAB);
	if not b then error errorString << endl;
    );
    LAB    
);


makeDualBasisFunction = (LAB) -> (
    w:=LAB#"WriteInBasis";
    dualBasisCoefficients:=apply(LAB#"DualBasis", M -> w(M));
    B -> apply(dualBasisCoefficients, c -> sum apply(#c, i -> c_i*B_i))
);


-- level moved to lieAlgebraModules.m2


-- Implement fromula from de Graaf, page 98
star = (j, a) -> (
    R:=ring(a);
    ea:=first exponents(a);
    ea = apply(#ea, i -> if i!=j then ea_i else ea_i+1);
    product reverse apply(#ea, i -> R_i^(ea_i))
);


extendedWeightDiagram = (V) -> (
    WD := weightDiagram(V);
    K := keys(WD);
    PhiPlus := positiveRoots(V#"LieAlgebra");
    D := K;
    for i from 0 to #PhiPlus-1 do (
        for j from 0 to #K-1 do (
            if not member(K_j-PhiPlus_i,D) then D = append(D,K_j-PhiPlus_i)
        )
    );
    D
);



-- U(g)
-- Order the basis vectors as x,h,y
-- Within each group, put in order reverse lex level

universalEnvelopingAlgebra = method(
    TypicalValue => FreeAlgebraQuotient
    )

universalEnvelopingAlgebra(LieAlgebraBasis) := (LAB) -> (
    -- Set up the Lie algebra basis
    g := LAB#"LieAlgebra";
    B := LAB#"BasisElements";
    br := LAB#"Bracket";
    writeInBasis := LAB#"WriteInBasis";
    Bweights:=LAB#"Weights";
    -- Create U(g) in the original basis
    BB:=getSymbol "BB";
    R1 := QQ<|apply(#B, i -> BB_i)|>;
    v:={};
    r:=0;
    bracketRelations:=flatten for i from 0 to #B-1 list (
        for j from 0 to #B-1 list (
            v = writeInBasis(br(B_i,B_j));
            r = R1_i*R1_j-R1_j*R1_i - (sum apply(#B, k -> v_k*R1_k));
            r
        )
    );
    -- Now set up the free algebra
    PhiPlus := positiveRoots(g);
    x:=getSymbol "x";
    h:=getSymbol "h";
    y:=getSymbol "y";
    varListx := reverse apply(#PhiPlus, i -> x_(i+1));
    varListh := reverse apply(g#"LieAlgebraRank", i -> h_(i+1));
    varListy := reverse apply(#PhiPlus, i -> y_(i+1));
    R2 := QQ<|flatten {varListx,varListh,varListy}|>;
    -- Get the map
    WtToZZ:=new HashTable from apply(#B, i -> (Bweights_i,i));
    posRootMap:=reverse apply(PhiPlus, w -> WtToZZ#w);
    cartanMap:=reverse apply(g#"LieAlgebraRank", i -> i);
    negRootMap:=reverse apply(PhiPlus, w -> WtToZZ#(-w));
    sigma:=join(posRootMap,cartanMap,negRootMap);
    sigmainverse := apply(sort apply(#sigma, i -> {sigma_i,i}), p -> p_1);
    f12:=map(R2,R1,apply(#sigma, i -> R2_(sigmainverse_i)));
    (R2/f12(ideal bracketRelations),sigma,sigmainverse)
);

universalEnvelopingAlgebra(LieAlgebra) := (g) -> (
    universalEnvelopingAlgebra(lieAlgebraBasis(g))
);


uNminus = method(
    TypicalValue => FreeAlgebraQuotient
    )

uNminus(LieAlgebraBasis) := (LAB) -> (
    -- Set up the Lie algebra basis
    g := LAB#"LieAlgebra";
    B := LAB#"BasisElements";
    br := LAB#"Bracket";
    writeInBasis := LAB#"WriteInBasis";
    Bweights:=LAB#"Weights";
    -- Get the bracket relations with respect to the original basis
    BB:=getSymbol "BB";
    R1 := QQ<|apply(#B, i -> BB_i)|>;
    LOI:=LAB#"LoweringOperatorIndices";
    v:={};
    r:=0;
    uNminusbracketRelations := flatten for i from 0 to #B-1 list (
        for j from 0 to #B-1 list (
	    if not(member(i,LOI) and member(j,LOI)) then continue;
            v = writeInBasis(br(B_i,B_j));
            r = R1_i*R1_j-R1_j*R1_i - (sum apply(#B, k -> v_k*R1_k));
            r
        )
    );
    -- Now set up the free algebra
    PhiPlus := positiveRoots(g);
    Y:=getSymbol "Y";
    varListy := reverse apply(#PhiPlus, i -> Y_(i+1));
    R2 := QQ<|varListy|>;
    -- Get the map
    WtToZZ:=new HashTable from apply(#B, i -> (Bweights_i,i));
    posRootMap:=reverse apply(PhiPlus, w -> WtToZZ#w);
    cartanMap:=reverse apply(g#"LieAlgebraRank", i -> i);
    negRootMap:=reverse apply(PhiPlus, w -> WtToZZ#(-w));
    sigma:=join(posRootMap,cartanMap,negRootMap);
    sigmainverse := apply(sort apply(#sigma, i -> {sigma_i,i}), p -> p_1);
    n:=#PhiPlus+g#"LieAlgebraRank";
    f12:=map(R2,R1,apply(#sigma, i -> if sigmainverse_i <n then 0 else R2_(sigmainverse_i-n)));
    R2/f12(ideal uNminusbracketRelations)
);



uNminus(LieAlgebra) := (g) -> (
    uNminus(lieAlgebraBasis(g))
);
