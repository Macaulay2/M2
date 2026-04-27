---------------------------------
-- Define the LieAlgebraModuleElement type
-- for linear combinations of VLB elements
---------------------------------


LieAlgebraModuleElement = new Type of HashTable
-- The keys are:
-- LieAlgebraModule
-- Terms, a hashtable
---- The keys are VLB patterns
---- The values are the coefficients 


lieAlgebraModuleElement = (V,L) -> (
    new LieAlgebraModuleElement from {"LieAlgebraModule"=>V,"Terms"=>L} 
);


zeroElement = (V) -> (
    lieAlgebraModuleElement(V,{})    
);


simplify = (L) -> (
    P:=pairs partition(first,L);
    P=apply(P, p -> {p_0,sum apply(p_1, t -> t_1)});
    select(P, p -> p_1 != 0_(ring(p_1)))
);


LieAlgebraModuleElement + LieAlgebraModuleElement := (F1,F2) -> (
    V1:=F1#"LieAlgebraModule";
    V2:=F2#"LieAlgebraModule";
    if V1=!=V2 then error "Not elements of the same module";
    L:=join(F1#"Terms",F2#"Terms");
    lieAlgebraModuleElement(V1, simplify(L))
);


Number * LieAlgebraModuleElement := (c,f) -> (
    lieAlgebraModuleElement(f#"LieAlgebraModule", apply(f#"Terms", p -> {p_0,c*p_1}))
);

- LieAlgebraModuleElement := (F1) -> (
    (-1)*F1
);

LieAlgebraModuleElement - LieAlgebraModuleElement := (F1,F2) -> (
    F1 + (-1)*F2
);

LieAlgebraModuleElement == LieAlgebraModuleElement := (F1,F2) -> (
    F1- F2 === zeroElement(F1#"LieAlgebraModule")
);



---------------------------------
-- 6. VLB basis in words
---------------------------------

matrixFromColumns = L -> (
    M:=L_0;
    for i from 1 to #L-1 do (
        M = M|(L_i)
    );
    M
);
-*
B = {matrix({{1},{3}}),matrix({{2},{4}})}
matrixFromColumns(B)
*-

testAgainstPartialBasis = (v,B) -> (
    if #B==0 then return {v};
    MB:=matrixFromColumns B;
    MBv:=MB | v;
    if rank(MBv)==#B then return B else return append(B,v)
);

-*
B = {matrix({{1},{3},{0/1}}),matrix({{2},{4},{0/1}})}
testAgainstPartialBasis(matrix {{1},{1},{0/1}},B)
testAgainstPartialBasis(matrix {{0},{0},{1/1}},B)
*-

-*
dynkinToPartition = (v) -> (
    n:=#v;
    append(apply(n, i -> sum apply(n-i, j -> v_(n-1-j))),0)
);
*-

wt = (v,Vlambdaweights) -> (
    e:=flatten entries v;
    i:=first select(#e, j -> e_j!=0);
    Vlambdaweights_i
)


applyLOWord = (w,v,LoweringOperators) -> (
    if w=={} then return v;
    x:=reverse(w);
    u:=v;
    for i from 0 to #x-1 do (
	u = (LoweringOperators_(x_i))*u
    );
    u    
);
-*
-- Test some examples
applyLOWord({},v1,LoweringOperators1)
applyLOWord({0},v1,LoweringOperators1)
applyLOWord({1,0},v1,LoweringOperators1)
*-




basisWordsFromMatrixGenerators = method(
    TypicalValue=>List
);

basisWordsFromMatrixGenerators(LieAlgebraRepresentation) := (rho) -> (
    V:=rho#"Module";
    LAB:=rho#"Basis";
    rhoB:=rho#"RepresentationMatrices";
    Vlambdaweights :=representationWeights(rho);
    WD := weightDiagram(V);
    wts:=keys(V#"DecompositionIntoIrreducibles");
    mults:= values(V#"DecompositionIntoIrreducibles");
    if #wts!=1 then error "V must have exactly one highest weight";
    if mults!={1} then error "V is not irreducible";
    lambdaDynkin:=first wts;
    LoweringOperators1:=apply(LAB#"LoweringOperatorIndices", i -> rhoB_i);
    v1 := weightMuHighestWeightVectorsInW(lambdaDynkin,rho);
    Words:=new MutableHashTable from {};
    WordsAndVectorsByWeight1 := new MutableHashTable from apply(keys(WD), k -> k=>{});
    BasesByWeight1 := new MutableHashTable from apply(keys(WD), k -> k=>{});
    Words#0 = {{}};
    WordsAndVectorsByWeight1#lambdaDynkin = {{{},v1}};
    BasesByWeight1#lambdaDynkin = {v1};
    -- Do words of length 1 separately before starting the loop
    WordsOfLengthl := {};
    Yv:={};
    wtYv:={};
    A:={};
    B:={};
    for i from 0 to #LoweringOperators1-1 do (
        Yv = applyLOWord({i},v1,LoweringOperators1);
        if Yv==0 then continue;
        wtYv = wt(Yv,Vlambdaweights);
        A = BasesByWeight1#wtYv;
        B = testAgainstPartialBasis(Yv,A);
        if #B == #A then continue;
        WordsAndVectorsByWeight1#wtYv = append(WordsAndVectorsByWeight1#wtYv,{{i},Yv});
        BasesByWeight1#wtYv = B;
        WordsOfLengthl = append(WordsOfLengthl,{i});
    );
    Words#1 = WordsOfLengthl;
    -- Now loop to find the rest
    l:=1;
    print concatenate("Length ",toString(l)," complete. ",toString(#WordsOfLengthl)," new words found") << endl;
    while sum apply(keys(Words), i -> #(Words#i)) < sum(values(WD)) do (
      l = l+1;
      WordsOfLengthl = {};
      for i from 0 to #(Words#(l-1))-1 do (
        for j from 0 to first((Words#(l-1))_i) do (  
          Yv = applyLOWord(prepend(j,(Words#(l-1))_i),v1,LoweringOperators1);
          if Yv==0 then continue;
          wtYv = wt(Yv,Vlambdaweights);
          A = BasesByWeight1#wtYv;
          B = testAgainstPartialBasis(Yv,A);
          if #B == #A then continue;
          WordsAndVectorsByWeight1#wtYv = append(WordsAndVectorsByWeight1#wtYv,{prepend(j,(Words#(l-1))_i),Yv});
          BasesByWeight1#wtYv = B;
          WordsOfLengthl = append(WordsOfLengthl,prepend(j,(Words#(l-1))_i));
        )
      );
      print concatenate("Length ",toString(l)," complete. ",toString(#WordsOfLengthl)," new words found") << endl;
      Words#l = WordsOfLengthl;       
    );
    -- The minor given by the rows corresponding to VLB patterns of weight mu
    -- will be invertible
    -- Invert it to express these VLB basis vectors as linear combinations of words
    VLBWords := new MutableHashTable from {};
    weightMuIndices:={};
    M:={};
    WordsOfWeightMu:={};
    L:={};
    VLBWordsMu:={};
    VLBIndices:={};
    for mu in keys(WD) do (
        weightMuIndices = select(#Vlambdaweights, i -> Vlambdaweights_i==mu);
        M = (matrixFromColumns(BasesByWeight1#mu))^weightMuIndices;
        A = inverse(M);
        WordsOfWeightMu = apply(WordsAndVectorsByWeight1#mu, p -> p_0);
        L = entries transpose inverse M;
        VLBWordsMu = apply(L, i -> lieAlgebraModuleElement(V,delete(null,apply(#i, j -> if i_j!=0 then {WordsOfWeightMu_j,i_j}))));
    -- Put these into VLBWords
        VLBIndices = entries transpose((matrixFromColumns(BasesByWeight1#mu))*A);
        VLBIndices = apply(VLBIndices, v -> first select(#v, i -> v_i!=0));
        for k from 0 to #VLBIndices-1 do (
            VLBWords#(VLBIndices_k) = VLBWordsMu_k
        );
    );
    apply(#(keys VLBWords), i -> VLBWords#i)
)


isomorphismOfRepresentations = method(
    TypicalValue=>Matrix
);

-- gives matrix P such that P*(L2_i)*Pinv == L1_i
-- or Pinv*(L1_i)*P = L2_i
isomorphismOfRepresentations(LieAlgebraRepresentation,LieAlgebraRepresentation) := (rho1, rho2) -> (
    V:=rho1#"Module";
    if rho2#"Module"!=V then error "The representations are not isomorphic";
    if not isIrreducible(V) then error "Not implemented yet for reducible representations";
    LAB1:=rho1#"Basis";
    LAB2:=rho2#"Basis";
    if LAB1#"BasisElements"!=LAB2#"BasisElements" then error "Not implemented yet when the representations have different Lie algebra bases";
    lambda:=first keys(V#"DecompositionIntoIrreducibles");
    vlambda:=weightMuHighestWeightVectorsInW(lambda,rho1);
    LOMaps:=apply(LAB1#"LoweringOperatorIndices", i -> (rho1#"RepresentationMatrices")_i);
    basisWords:=basisWordsFromMatrixGenerators(rho2);
    act:=(X,f) -> X*f;
    P:=matrixFromColumns(apply(basisWords, w -> applyWord(w,vlambda,act,LOMaps)));
    -- Check this before returning it
    Pinv := inverse P;
    L1:=rho1#"RepresentationMatrices";
    L2:=rho2#"RepresentationMatrices";
    g:=LAB1#"LieAlgebra";
    if not all(dim g, i -> L2_i == Pinv*(L1_i)*P) then error "Isomorphism not found";
    P
);


