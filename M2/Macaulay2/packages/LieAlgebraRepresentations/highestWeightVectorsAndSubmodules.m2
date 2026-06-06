-*
saveListLineByLine = (L,Lstr) -> (
    fn:=openOut concatenate(Lstr,".m2");
    fn << concatenate(Lstr, " = {") << endl;
    for i from 0 to #L-2 do (
        fn << concatenate(toString(L_i),",") << endl
    );
    fn << toString(last L) << endl;
    fn << "};" << endl;
    close fn
);
R = ring(V20020inW8W2Std_0);
S = QQ[P_0..P_6434];
fRS = map(S,R,gens S);
LS = apply(V20020inW8W2Std, g -> fRS(g));
saveListLineByLine(LS,"V20020inW8W2Std");
*-


saveListAsFunction = (L,Lstr,argstr) -> (
    fn:=openOut concatenate(Lstr,".m2");
    fn << concatenate(Lstr, " = ",argstr," -> {") << endl;
    for i from 0 to #L-2 do (
        fn << concatenate(toString(L_i),",") << endl
    );
    fn << toString(last L) << endl;
    fn << "};" << endl;
    close fn
);



-- Restrict a sparse matrix to a subdomain and subcodomain

restrictRaisingOperatoritoWtmuSpace = (rho,i,mu) -> (
    W:=rho#"Module";
    LAB:=rho#"Basis";
    L:=rho#"RepresentationMatrices";
    ROI:=LAB#"RaisingOperatorIndices";
    weightShift:=(LAB#"Weights")_(ROI_i);
    Wweights:=representationWeights(rho);
    domainIndices:=select(dim W, i -> Wweights_i==mu);
    codomainIndices:=select(dim W, i -> Wweights_i==mu+weightShift);
    M:=L_(ROI_i);
    M_domainIndices^codomainIndices
);


-- First, a generic function for finding highest weight vectors in any representation

weightMuHighestWeightVectorsInW = method(
    TypicalValue=>Matrix
);    

weightMuHighestWeightVectorsInW(List,LieAlgebraRepresentation) := (mu,rho) -> (
    W:=rho#"Module";
    LAB:=rho#"Basis";
    L:=rho#"RepresentationMatrices";
    ROI:=LAB#"RaisingOperatorIndices";
    K:=entries gens intersect apply(#ROI, i -> ker restrictRaisingOperatoritoWtmuSpace(rho,i,mu));
    Wweights:=representationWeights(rho);
    domainIndices:=select(dim W, i -> Wweights_i==mu);
    z:=apply(#(K_0), i -> 0);
    matrix apply(dim W, i -> if member(i,domainIndices) then K_(position(domainIndices,x->x==i)) else z)
);

-- It's also useful to have functions that find the highest weight vectors
-- of weight mu in Symd W without fully computing Symd W




monomialFactors = (m) -> (
    e:=flatten exponents(m);
    flatten apply(#e, i -> apply(e_i, j -> i))
);




csOnDegdMonomial = memoize((X,Xstar,m) -> (
    s:=0;
    d:=first degree(m);
    p:=apply(monomialFactors(m), i -> (ring m)_i);
    q:={};
    for a from 0 to #X-1 do (
	-- Compute Xstar_a(m)
	for i from 0 to d-1 do (
	    for j from 0 to d-1 do (
		s = s + product apply(d, k -> if i==k and j==k then (X_a)((Xstar_a)(p_k)) else if i==k and j!=k then (X_a)(p_k) else if i!=k and j==k then (Xstar_a)(p_k) else p_k)
	    )
	)
    );
    s
));

csOnDegdPoly = (X,Xstar,f) -> (
    sum apply(terms f, t -> leadCoefficient(t)*csOnDegdMonomial(X,Xstar,leadMonomial(t)))
);


scale = (f) -> (
    if f==0 then return f;
    c:=flatten entries last coefficients(f);
    c = apply(c, i -> lift(i,QQ));
    g:=gcd(c);
    (1/g)*f
);

-*
The following function might offer a faster approach than weightMuHighestWeightVectorsInSymdW, especially if 
using sparse matrices. However, for now, we leave it unexported
*-

casimirMatrixForWtMuSpaceInSymdW = method(
    TypicalValue=>Matrix
);    


casimirMatrixForWtMuSpaceInSymdW(LieAlgebraRepresentation,List,ZZ):= (rhoW,mu,d) -> (
    LAB:=rhoW#"Basis";
    g:=LAB#"LieAlgebra";
    W:=rhoW#"Module";
    WRepMats := rhoW#"RepresentationMatrices";
    print toString("Constructing the Casimir operator...") << endl;
    Wweights:=representationWeights(rhoW);
    dtuples:=apply(#Wweights, i -> {i});
    for i from 2 to d do (
        dtuples = flatten apply(#Wweights, i -> delete(null,apply(dtuples, t -> if i<=t_0 then prepend(i,t))))
    );
    wtmutuples:=select(dtuples, t -> sum(apply(t, j -> Wweights_j))==mu);
    B:=getSymbol "B";
    R:=QQ[B_0..B_(dim(W)-1),MonomialOrder=>Lex];
    dbf:=makeDualBasisFunction(LAB);
    Wstar := dbf(WRepMats);
    WImageList := apply(WRepMats, M -> flatten entries((vars R)*M));
    WstarImageList:= apply(Wstar, M -> flatten entries((vars R)*M));
    X := apply(dim g, i -> map(R,R,WImageList_i));
    Xstar := apply(dim g, i -> map(R,R,WstarImageList_i));
    WtmuMonBasis := apply(wtmutuples, p -> product apply(p, j -> R_j));
    f:=0;
    cf:=0;
    c:=0;
    r:={};
    L:=for j from 0 to #WtmuMonBasis-1 list (
	f = WtmuMonBasis_j;
        cf = csOnDegdPoly(X,Xstar,f);
	r = {};
	for i from 0 to #WtmuMonBasis-1 do (
	    c = coefficient(WtmuMonBasis_i,cf);
	    if c!=0 then r = append(r,(i,j)=>c)
        );
        r
    );
    L
);



testAgainstUTBasis = (f0,B) -> (
    if #B==0 then return {f0};
    if f0==0 then return B;
    c:=0;
    g:=0;
    f:=f0;
    for j from 0 to #B-1 do ( 
      g = B_j;	 
      c=coefficient(leadMonomial(g),f)/leadCoefficient(g);
      f = f - c*g
    );
    if f==0 then return B else return append(B,f)  
);

-- The following function isn't getting used, but we
-- leave it unexported in case it turns out to be useful
-- in the future
writeInUTBasis = (f0,B) -> (
    if f0==0 then return apply(#B, i -> 0);
    c:=0;
    g:=0;
    f:=f0;
    answer:={};
    for j from 0 to #B-1 do ( 
        g = B_j;	 
        c=coefficient(leadMonomial(g),f)/leadCoefficient(g);
        answer = append(answer,c);
        f = f - c*g
    );
    if not(f==0) then error "This did not work";
    answer  
);



weightMuHighestWeightVectorsInSymdW = method(
    TypicalValue=>RingElement
);

weightMuHighestWeightVectorsInSymdW(List,ZZ,LieAlgebraRepresentation):= (mu,d,rhoW) -> (
    LAB:=rhoW#"Basis";
    g:=LAB#"LieAlgebra";
    W:=rhoW#"Module";
    WRepMats := rhoW#"RepresentationMatrices";
    print toString("Constructing the Casimir operator...") << endl;
    Wweights:=representationWeights(rhoW);
    dtuples:=apply(#Wweights, i -> {i});
    for i from 2 to d do (
        dtuples = flatten apply(#Wweights, i -> delete(null,apply(dtuples, t -> if i<=t_0 then prepend(i,t))))
    );
    wtmutuples:=select(dtuples, t -> sum(apply(t, j -> Wweights_j))==mu);
    B:=getSymbol "B";
    R:=QQ[B_0..B_(dim(W)-1),MonomialOrder=>Lex];
    dbf:=makeDualBasisFunction(LAB);
    Wstar := dbf(WRepMats);
    WImageList := apply(WRepMats, M -> flatten entries((vars R)*M));
    WstarImageList:= apply(Wstar, M -> flatten entries((vars R)*M));
    X := apply(dim g, i -> map(R,R,WImageList_i));
    Xstar := apply(dim g, i -> map(R,R,WstarImageList_i));
    WtmuMonBasis := apply(wtmutuples, p -> product apply(p, j -> R_j));
    T:=symmetricPower(d,W);
    HighestWtsT:=keys(T#"DecompositionIntoIrreducibles");
    EV:=reverse sort unique apply(HighestWtsT, v -> casimirScalar(irreducibleLieAlgebraModule(v,g)));
    csmu:=casimirScalar(irreducibleLieAlgebraModule(mu,g));
    otherEVs := select(EV, x -> x!=csmu);
    print concatenate("Other EVs: ",toString(otherEVs)) << endl;
    print toString("Beginning projections...") << endl;
    hwvs:={};
    L:={};
    pf:=0;
    f:=0;
    j:=-1;
    while #hwvs<(T#"DecompositionIntoIrreducibles")#mu do (
	j=j+1;
	print concatenate("    j=",toString(j),":") << endl;
        f = WtmuMonBasis_j;
        for c in otherEVs do (
            f = scale(csOnDegdPoly(X,Xstar,f)-c*f);
	    if f==0 then break;
            print concatenate("        EV ",toString(c)," complete") << endl;
        );
        if f== 0 then continue;
        if csOnDegdPoly(X,Xstar,f)!=csmu*f then error "f does not have the correct eigenvalue";
	print concatenate("    #hwvs=",toString(#hwvs)) << endl;
	L = testAgainstUTBasis(f,hwvs);
	if #L > #hwvs then (
	    hwvs = L
	);
    );	
    hwvs
);




-- Here are a bunch of internal functions that are used to evaluate words
-- in the lowering operators on the highest weight vector

actOnMonomial = (X,m) -> (
    --if first(degree(m))>2 then error "Only implemented for degrees 1 and 2";
    mf:=monomialFactors(m);
    R:=ring(m);
    --if first(degree(m))==1 then return (X(R_(mf_0)));
    --if first(degree(m))==2 then return (X(R_(mf_0))*(R_(mf_1)) + (R_(mf_0))*(X(R_(mf_1))));
    sum apply(#mf, i -> product(#mf, j -> if i==j then X(R_(mf_j)) else R_(mf_j)))
);



act = memoize((X,f) -> (
    if f==0_(ring f) then return f;
    T:=terms(f);
    sum apply(T, t -> leadCoefficient(t)*actOnMonomial(X,leadMonomial(t)))
));



applyTerm = (t,v,actInstance,LoweringOperators) -> (
    c:=t_1;
    w:=t_0;
    if w=={} then return v;
    x:=reverse(w);
    u:=v;
    for i from 0 to #x-1 do (
	u = actInstance(LoweringOperators_(x_i),u);
	if u==0 then return u;
    );
    c*u    
);



applyWord = (w,v,actInstance,LoweringOperators) -> (
    T:=w#"Terms";
    sum apply(T, t -> applyTerm(t,v,actInstance,LoweringOperators))
);


VInSymdW = method(
    Options=>{"SaveAsFunction"=>""},
    TypicalValue=>List
);



VInSymdW(LieAlgebraRepresentation,ZZ,LieAlgebraRepresentation,Matrix) := o -> (rhoV,d,rhoW,hwv) -> (
    V:=rhoV#"Module";
    LABV:=rhoV#"Basis";
    LV:=rhoV#"RepresentationMatrices";
    W:=rhoW#"Module";
    LABW:=rhoW#"Basis";
    LW:=rhoW#"RepresentationMatrices";
    -- Check that they use the same basis of g
    if LABV#"BasisElements"!= LABW#"BasisElements" then error "V and W do not use the same basis";
    LAB:=LABV;
    n:=dim W;
    B:=getSymbol "B";
    R:=QQ[apply(n, i -> B_i),MonomialOrder=>Lex];
    Wweights:=representationWeights(rhoW);
    R.cache = new CacheTable from {"Weights"=>Wweights};
    LOMaps:={};
    --if instance(LW_0,SparseMatrix) then (
    if false then (
        -- LOMaps=apply(LAB#"LoweringOperatorIndices", i -> ringMap(R,R,LW_i))
	LOMaps = LOMaps
    ) else (
        LOMaps=apply(LAB#"LoweringOperatorIndices", i -> map(R,R,LW_i))
    );
    basisWords:=basisWordsFromMatrixGenerators(rhoV);
    hwvR := ( (basis(d,R))*(hwv) )_(0,0);
    --apply(basisWords, w -> applyWord(w,hwvR,act,LOMaps))
    w:={};
    returnValue:=for i from 0 to #basisWords-1 list (
	w = basisWords_i;
	print toString(i) << endl;
	applyWord(w,hwvR,act,LOMaps)
    );
    if o#"SaveAsFunction"!="" then (
         saveListAsFunction(returnValue,o#"SaveAsFunction","B")
    );
    returnValue
)


VInSymdW(LieAlgebraRepresentation,ZZ,LieAlgebraRepresentation,RingElement) := o -> (rhoV,d,rhoW,hwv) -> (
    V:=rhoV#"Module";
    CBV:=rhoV#"Basis";
    LV:=rhoV#"RepresentationMatrices";
    W:=rhoW#"Module";
    CBW:=rhoW#"Basis";
    LW:=rhoW#"RepresentationMatrices";
    -- Check that they use the same basis of g
    if CBV#"BasisElements" != CBW#"BasisElements" then error "V and W do not use the same basis";
    CB:=CBV;
    n:=dim W;
    R:=ring(hwv);
    LOMaps:={};
   -- if instance(LW_0,SparseMatrix) then (
   --     LOMaps=apply(CB#"LoweringOperatorIndices", i -> ringMap(R,R,LW_i))
   -- ) else (
    LOMaps=apply(CB#"LoweringOperatorIndices", i -> map(R,R,LW_i));
    --);
    basisWords:=basisWordsFromMatrixGenerators(rhoV);
    --apply(basisWords, w -> applyWord(w,hwv,act,LOMaps))
    w:={};
    returnValue:=for i from 0 to #basisWords-1 list (
	w = basisWords_i;
	print toString(i) << endl;
	applyWord(w,hwv,act,LOMaps)
    );
    if o#"SaveAsFunction"!="" then (
         saveListAsFunction(returnValue,o#"SaveAsFunction","B")
    );
    returnValue
)



-*

R = ring(V20020inW8W2Std_0);
S = QQ[P_0..P_6434];
fRS = map(S,R,gens S);
LS = apply(V20020inW8W2Std, g -> fRS(g));
saveListLineByLine(LS,"V20020inW8W2Std");


saveListAsFunction = (L,Lstr,argstr) -> (
    fn:=openOut concatenate(Lstr,".m2");
    fn << concatenate(Lstr, " = ",argstr," -> {") << endl;
    for i from 0 to #L-2 do (
        fn << concatenate(toString(L_i),",") << endl
    );
    fn << toString(last L) << endl;
    fn << "};" << endl;
    close fn
);
*-


VInWedgekW = method(
    Options=>{"SaveAsFunction"=>""},
    TypicalValue=>List
);    

VInWedgekW(LieAlgebraRepresentation,ZZ,LieAlgebraRepresentation,Matrix) := o -> (rhoV,k,rhoW,hwv) -> (
    V:=rhoV#"Module";
    LABV:=rhoV#"Basis";
    LV:=rhoV#"RepresentationMatrices";
    W:=rhoW#"Module";
    LABW:=rhoW#"Basis";
    LW:=rhoW#"RepresentationMatrices";
    -- Check that they use the same basis of g
    if LABV#"BasisElements" != LABW#"BasisElements" then error "V and W do not use the same basis";
    LAB:=LABV;
    WedgekW:=exteriorPower(k,rhoW);
    n:=dim W;
    Bk := subsets(apply(n, i -> i),k);
    p:=getSymbol "p";
    R:=QQ[apply(Bk, i -> p_i),MonomialOrder=>Lex];
    m:=LAB#"LieAlgebra"#"LieAlgebraRank";
    Wweights:=representationWeights(rhoW);
    Wedgekweights := apply(Bk, s -> sum apply(s, j -> Wweights_j));
    R.cache = new CacheTable from {"Weights"=>Wedgekweights};
    LWedgekW:=WedgekW#"RepresentationMatrices";
    LOMaps:=apply(LAB#"LoweringOperatorIndices", i -> map(R,R,LWedgekW_i));
    basisWords:=basisWordsFromMatrixGenerators(rhoV);
    hwvR := ( (vars R)*(hwv) )_(0,0);
    returnValue:=apply(basisWords, w -> applyWord(w,hwvR,act,LOMaps));
    if o#"SaveAsFunction"!="" then (
	P := getSymbol "P";
        S := QQ[apply(numgens R, i -> P_i)];
        fRS := map(S,R,gens S);
        LS := apply(returnValue, g -> fRS(g));
        saveListAsFunction(LS,o#"SaveAsFunction","P")
    );    
    returnValue
);



csOnDeg2PureTensor = memoize((cas1,cas2,XftensorXstarg,XstarftensorXg,m) -> (
    (cas1+cas2)*m + sum apply(#XftensorXstarg, i -> (XftensorXstarg_i)(m) + (XstarftensorXg_i)(m))
));

csOnDeg2Tensor = (cas1,cas2,XftensorXstarg,XstarftensorXg,f) -> (
    sum apply(terms f, t -> leadCoefficient(t)*csOnDeg2PureTensor(cas1,cas2,XftensorXstarg,XstarftensorXg,leadMonomial(t)))
);


weightNuHighestWeightVectorsInVtensorW = method(
    TypicalValue=>RingElement
);


weightNuHighestWeightVectorsInVtensorW(List,LieAlgebraRepresentation,LieAlgebraRepresentation):= (nu,rhoV,rhoW) -> (
    LAB:=rhoV#"Basis";
    g:=LAB#"LieAlgebra";
    V:=rhoV#"Module";
    VRepMats := rhoV#"RepresentationMatrices";
    W:=rhoW#"Module";
    WRepMats := rhoW#"RepresentationMatrices";
    weightsV := representationWeights(rhoV);
    weightsW := representationWeights(rhoW);
    print toString("Constructing the Casimir operator...") << endl;
    wtnupairs := {};
    for i0 from 0 to dim(V)-1 do (
        for i1 from 0 to dim(W)-1 do (
            if weightsV_i0+weightsW_i1==nu then wtnupairs=append(wtnupairs,{i0,i1})
        )
    );
    A:=getSymbol "A";
    B:=getSymbol "B";
    R:=QQ[A_0..A_(dim(V)-1),B_0..B_(dim(W)-1),MonomialOrder=>Lex];
    dbf:=makeDualBasisFunction(LAB);
    Vstar := dbf(VRepMats);
    Wstar := dbf(WRepMats);
    cas1 := casimirScalar(V);
    cas2 := casimirScalar(W);
    Vvars:=matrix {apply(dim V, i -> R_i)};
    Wvars:=matrix {apply(dim W, i -> R_(i+dim V))};
    VImageList := apply(VRepMats, M -> flatten entries(Vvars*M));
    WImageList:= apply(WRepMats, M -> flatten entries(Wvars*M));
    VstarImageList:= apply(Vstar, M -> flatten entries(Vvars*M));
    WstarImageList := apply(Wstar, M -> flatten entries(Wvars*M));
    XftensorXstarg := apply(dim g, i -> map(R,R,join(VImageList_i,WstarImageList_i)));
    XstarftensorXg := apply(dim g, i -> map(R,R,join(VstarImageList_i,WImageList_i)));
    WtnuTensorBasis := apply(wtnupairs, p -> R_(p_0)*R_((dim V)+(p_1)));
    T:=V**W;
    HighestWtsT:=keys(T#"DecompositionIntoIrreducibles");
    EV:=reverse sort unique apply(HighestWtsT, v -> casimirScalar(irreducibleLieAlgebraModule(v,g)));
    csnu:=casimirScalar(irreducibleLieAlgebraModule(nu,g));
    otherEVs := select(EV, x -> x!=csnu);
    print concatenate("Other EVs: ",toString(otherEVs)) << endl;
    print toString("Beginning projections...") << endl;
    hwvs:={};
    L:={};
    pf:=0;
    f:=0;
    j:=-1;
    while #hwvs<(T#"DecompositionIntoIrreducibles")#nu do (
	j=j+1;
	print concatenate("    j=",toString(j),":") << endl;
        f = WtnuTensorBasis_j;
        for c in otherEVs do (
            f = scale(csOnDeg2Tensor(cas1,cas2,XftensorXstarg,XstarftensorXg,f)-c*f);
	    if f==0 then break;
            print concatenate("        EV ",toString(c)," complete") << endl;
        );
        if f== 0 then continue;
        if csOnDeg2Tensor(cas1,cas2,XftensorXstarg,XftensorXstarg,f)!=csnu*f then error "f does not have the correct eigenvalue";
	print concatenate("    #hwvs=",toString(#hwvs)) << endl;
	L = testAgainstUTBasis(f,hwvs);
	if #L > #hwvs then (
	    hwvs = L
	);
    );	
    hwvs    
);


UInVtensorW = method(
    Options=>{"SaveAsFunction"=>""},
    TypicalValue=>List
);

UInVtensorW(LieAlgebraRepresentation,LieAlgebraRepresentation,LieAlgebraRepresentation,Matrix) := o -> (rhoU,rhoV,rhoW,hwv) -> (
    U:=rhoU#"Module";
    LABU:=rhoU#"Basis";
    LU:=rhoU#"RepresentationMatrices";    
    V:=rhoV#"Module";
    LABV:=rhoV#"Basis";
    LV:=rhoV#"RepresentationMatrices";
    W:=rhoW#"Module";
    LABW:=rhoW#"Basis";
    LW:=rhoW#"RepresentationMatrices";
    -- Check that they use the same basis of g
    if LABU#"BasisElements"!= LABV#"BasisElements" then error "U and V do not use the same basis";
    if LABU#"BasisElements" != LABW#"BasisElements" then error "U and W do not use the same basis";
    LAB:=LABU;
    n1:=dim V;
    n2:=dim W;
    A:=getSymbol "A";
    B:=getSymbol "B";
    R:=QQ[join(apply(n1, i -> A_i),apply(n2, i -> B_i)),MonomialOrder=>Lex];
    domainPairs:= flatten apply(n1, i -> apply(n2, j -> {i,j}));
    hwvR:=sum apply(#domainPairs, i -> hwv_(i,0)*R_((domainPairs_i)_0)*R_(n1+((domainPairs_i)_1)));
    Vweights:=representationWeights(rhoV);
    Wweights:=representationWeights(rhoW);
    R.cache = new CacheTable from {"Weights"=>join(Vweights,Wweights)};
    LOMaps:=apply(LAB#"LoweringOperatorIndices", i->  map(R,R,(LV_i)++(LW_i)));
    basisWords:=basisWordsFromMatrixGenerators(rhoU);
    returnValue:=apply(basisWords, w -> applyWord(w,hwvR,act,LOMaps));
    if o#"SaveAsFunction"!="" then (
         saveListAsFunction(returnValue,o#"SaveAsFunction","(A,B)")
    );
    returnValue
)


UInVtensorW(LieAlgebraRepresentation,LieAlgebraRepresentation,LieAlgebraRepresentation,RingElement) := o -> (rhoU,rhoV,rhoW,hwvR) -> (
    U:=rhoU#"Module";
    LABU:=rhoU#"Basis";
    LU:=rhoU#"RepresentationMatrices";    
    V:=rhoV#"Module";
    LABV:=rhoV#"Basis";
    LV:=rhoV#"RepresentationMatrices";
    W:=rhoW#"Module";
    LABW:=rhoW#"Basis";
    LW:=rhoW#"RepresentationMatrices";
    -- Check that they use the same basis of g
    if LABU#"BasisElements" != LABV#"BasisElements" then error "U and V do not use the same basis";
    if LABU#"BasisElements" != LABW#"BasisElements" then error "U and W do not use the same basis";
    LAB:=LABU;
    n1:=dim V;
    n2:=dim W;
    R:=ring(hwvR);
    LOMaps:=apply(LAB#"LoweringOperatorIndices", i->  map(R,R,(LV_i)++(LW_i)));
    basisWords:=basisWordsFromMatrixGenerators(rhoU);
    w:={};
    returnValue:=for i from 0 to #basisWords-1 list (
	w = basisWords_i;
	print toString(i) << endl;
	applyWord(w,hwvR,act,LOMaps)
    );
    if o#"SaveAsFunction"!="" then (
         saveListAsFunction(returnValue,o#"SaveAsFunction","(A,B)")
    );
    returnValue
)




