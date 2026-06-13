 altSumFace = method();
altSumFace(SimplicialModule,ZZ) := (S,n) -> (sum(0..n,i->(-1)^i*(S.dd)_(n,i)))

altSumFace(Complex,ZZ) := (C,n) -> (altSumFace(simplicialModule(C,n),n))

--this function computes the naive normalization
--of a simplicial object, with is just the complex built from the
--modules of the simplicial object equipped with differential which is just the alternating sum of the face maps.
naiveNorm = method();
naiveNorm(SimplicialModule,ZZ) := (S,n) -> (complex for i from 1 to n list altSumFace(S,i))

naiveNorm(SimplicialModule) := S -> naiveNorm(S, S.topDegree)

naiveNorm(Complex,ZZ) := (C,n) -> (naiveNorm(simplicialModule(C,n),n))



--we assume that S is Gamma(C) for some complex C here
--sym = method();
--sym(ZZ,Matrix) := (d,M) -> (
-*symMult = method();
symMult(List,ZZ,Ring) := (L,d,Q) -> (Qn := Q[z_1..z_d];
    M1 := tensor(for i in L list basis(i,Qn));
    M2 := basis(sum L,Qn);
    sub(M1//M2,Q)
    )*-

--this function constructs the Dold-Puppe extension of the Schur functor to the category
--of chain complexes. The list L should be a partition
schurMap = method(Options => {Degeneracy => false,TopDegree => null});
schurMap(List,SimplicialModule) := SimplicialModule => opts -> (lambda,S) -> (tdeg := topDegree S;
    --C := S.complex;
    -- fastSchurSparse handles face/degeneracy maps that are column-sparse
    -- (at most one nonzero per column); falls back to generic schur otherwise.
    fastOrFallback := f -> (
        r := fastSchurSparse(lambda, f);
        if r =!= null then r else schur(lambda, f)
        );
    L := hashTable for i to tdeg list i => schurModule(lambda,combineSFactors(S,i));
    H1 := applyValues(S.dd.map, fastOrFallback);
    if opts.Degeneracy === true then H2 := applyValues(S.ss.map, fastOrFallback);
    --print("we made it");
    if opts.Degeneracy === true then return simplicialModule(L,H1,H2,tdeg);
    simplicialModule(L,H1,tdeg)
    )

schurMap(List,SimplicialModuleMap) := SimplicialModuleMap => opts -> (lambda,phi) -> (
    S1 := source phi;
    S2 := target phi;
    if instance((keys phi.map)#0,Sequence) then error "expected SimplicialModuleMap to have singly graded indices";
    fastOrFallback := v -> (
        r := fastSchurSparse(lambda, v);
        if r =!= null then r else schur(lambda, v)
        );
    map(schurMap(lambda,S2),schurMap(lambda,S1),applyValues(phi.map, fastOrFallback),Degree => degree phi)
    )

schurMap(List,Complex) := Complex => opts -> (lambda,C) -> (S := if opts.TopDegree =!= null then simplicialModule(C,opts.TopDegree)
    else simplicialModule(C,(sum lambda)*length(C));
    Sn := schurMap(lambda,S);
    normalize(Sn)
    )

schurMap(List,ComplexMap) := ComplexMap => opts -> (lambda,phi) -> (phin := if opts.TopDegree =!= null then simplicialModule(phi,opts.TopDegree)
    else simplicialModule(phi,(sum lambda)*(max(length(source phi),length target phi)));
    phik := schurMap(lambda,phin);
    normalize(phik)
    )

-*sym = method(Options => {Degeneracy => false,TopDegree => null});
sym(ZZ,Matrix) := Matrix => opts -> (d,phi) -> (Q := ring phi;
    n := rank source phi;
    m := rank target phi;
    phitens := tensor((d:phi));
    M1 := symMult(toList(d:1),n,Q);
    M2 := symMult(toList(d:1),m,Q);
    matrix entries transpose ((matrix entries transpose(M2*phitens))//transpose(M1))
    )


sym(ZZ,SimplicialModule) := SimplicialModule => opts -> (d,S) -> (tdeg := topDegree S;
    --C := S.complex;
    L = hashTable for i to tdeg list i => symmetricPower(d,combineSFactors(S,i));
    H1 = hashTable for i in keys (S.dd.map) list i => map(symmetricPower(d,target (S.dd)_i),symmetricPower(d,source (S.dd)_i),sym(d,((S.dd)_i)));
    if opts.Degeneracy === true then H2 = hashTable for i in keys (S.ss.map) list i => sym(d,((S.ss)_i));
    --print("we made it");
    if opts.Degeneracy === true then return simplicialModule(L,H1,H2,tdeg);
    simplicialModule(L,H1,tdeg)
    )

sym(ZZ,SimplicialModuleMap) := SimplicialModuleMap => opts -> (d,phi) -> (
    S1 := source phi;
    S2 := target phi;
    if instance((keys phi.map)#0,Sequence) then error "expected SimplicialModuleMap to have singly graded indices";
    map(sym(d,S2),sym(d,S1),new HashTable from for i to max(topDegree S1,topDegree S2) list i => sym(d,phi_i),Degree => degree phi)
    )

sym(ZZ,Complex) := Complex => opts -> (d,C) -> (if opts.TopDegree =!= null then S = simplicialModule(C,opts.TopDegree)
    else S = simplicialModule(C,d*length(C));
    Sn := sym(d,S);
    normalize(Sn)
    )

sym(ZZ,ComplexMap) := ComplexMap => opts -> (d,phi) -> (if opts.TopDegree =!= null then phin = simplicialModuleMap(phi,opts.TopDegree)
    else phin = simplicialModuleMap(phi,d*(max(length(source phi),length target phi)));
    phik := sym(d,phin);
    normalize(phik)
    )*-


--this function computes the Dold-Puppe extension of the exterior power functor
--to the category of chain complexes. The integer d is the degree of the exterior power
extPower = method(Options => {Degeneracy=>false,TopDegree => null})
extPower(ZZ,SimplicialModule) := SimplicialModule => opts -> (d,S) -> (tdeg := topDegree S;
    --C := S.complex;
    -- fastExteriorPowerSparse handles the common case where every column
    -- has at most one nonzero entry (all d_k for k>0 and all degeneracies
    -- coming from the Dold-Kan construction); it falls back to M2's
    -- generic exteriorPower when the matrix is dense.
    fastOrFallback := f -> (
        r := fastExteriorPowerSparse(d, f);
        if r =!= null then r else exteriorPower(d, f)
        );
    L := hashTable for i to tdeg list i => exteriorPower(d,combineSFactors(S,i));
    H1 := applyValues(S.dd.map, fastOrFallback);
    H2 := if opts.Degeneracy === true then applyValues(S.ss.map, fastOrFallback);
    --print("we made it");
    if opts.Degeneracy === true then return simplicialModule(L,H1,H2,tdeg);
    simplicialModule(L,H1,tdeg)
    )

extPower(ZZ,SimplicialModuleMap) := SimplicialModuleMap => opts -> (d,phi) -> (
    S1 := source phi;
    S2 := target phi;
    if instance((keys phi.map)#0,Sequence) then error "expected SimplicialModuleMap to have singly graded indices";
    map(extPower(d,S2),extPower(d,S1),applyValues(phi.map, v -> exteriorPower(d, v)),Degree => degree phi)
    )

extPower(ZZ,Complex) := Complex => opts -> (d,C) -> (S := if opts.TopDegree =!= null then simplicialModule(C,opts.TopDegree)
    else simplicialModule(C,d*length(C));
    Sn := extPower(d,S);
    normalize(Sn)
    )

extPower(ZZ,ComplexMap) := ComplexMap => opts -> (d,phi) -> (phin := if opts.TopDegree =!= null then simplicialModule(phi,opts.TopDegree)
    else simplicialModule(phi,d*(max(length(source phi),length target phi)));
    phik := extPower(d,phin);
    normalize(phik)
    )

--need to make this smarter: it should cache components so you can recover them easily

--This function computes the simplicial tensor product and caches the direct sum
--indices so that the user can easily access components of the resulting face maps
--on particular direct summands of the tensor product
simplicialTensor =  method(Options => {Degeneracy=>false,TopDegree => null})
simplicialTensor(List) := SimplicialModule => opts -> T -> (if instance(T_0,SimplicialModule) then (
	degens := all(T, i->i.?ss);
	tdeg := max apply(T,i->topDegree i);
	L := hashTable for i to tdeg list i => tensorwithComponents(apply(T,s->s_i));
	H1 := applyPairs((T_0).dd.map, (i, v) -> (i, map(L#(i_0-1),L#(i_0),tensor(apply(T,s->s.dd_i)))));
	H2 := if opts.Degeneracy === true or degens then applyPairs((T_0).ss.map, (i, v) -> (i, tensorwithComponents(apply(T,s->s.ss_i))));
	if opts.Degeneracy === true or degens then return simplicialModule(L,H1,H2,tdeg);
        return simplicialModule(L,H1,tdeg);
	);
    tLength := max apply(T,j->try length j else length source j);
    if instance(T_0,Complex) then return normalize simplicialTensor(apply(T,j->simplicialModule(j,length(T)*tLength)));
    if instance(T_0, ComplexMap) then return normalize tensor(apply(T, j -> simplicialModule(j, length(T)*tLength)));
    )

simplicialTensor(SimplicialModule,SimplicialModule) := SimplicialModule => opts -> (S,T) -> (simplicialTensor({S,T}))

simplicialTensor(ComplexMap, ComplexMap) := ComplexMap => opts -> (f,g) -> simplicialTensor({f,g})
    
simplicialTensor(ZZ,SimplicialModule) := SimplicialModule => opts -> (d,S) -> (simplicialTensor(toList(d:S),opts))

simplicialTensor(ZZ,Complex) := Complex => opts -> (d,C) -> (simplicialTensor(toList(d:C)))

simplicialTensor(Complex,Complex) := Complex => opts -> (C,D) -> (simplicialTensor({C,D}))


SimplicialModule ** SimplicialModule := SimplicialModule => (S,T) -> (simplicialTensor(S,T))
    
hhh = method();
hhh(Complex) := C -> (sum toList apply(0..length C,i->length HH_i(C)))

degenMorphisms = method()
degenMorphisms(SimplicialModule,ZZ,ZZ) := (S,n,k) -> (Cn := naiveNorm(S,n);
    map(Cn,Cn,i->(S.ss)_(i,k),Degree => 1)
    )

degenMorphisms(Complex,ZZ,ZZ) := (C,n,k) -> (degenMorphisms(simplicialModule(C,n),n,k))

faceMorphisms = method()
faceMorphisms(SimplicialModule,ZZ,ZZ) := (S,n,k) -> (Cn := naiveNorm(S,n);
    map(Cn,Cn,i->(S.dd)_(i,k),Degree => -1)
    )

faceMorphisms(Complex,ZZ,ZZ) := (C,n,k) -> (faceMorphisms(simplicialModule(C,n),n,k))

makeNormMap = method();
makeNormMap(SimplicialModule,ZZ) := (S,d) -> (
    K2 := intersect(for i from 1 to d list ker S.dd_(d,i));
    K1 := if d>1 then intersect(for i from 1 to d-1 list ker S.dd_(d-1,i)) else
    if d ==1 then S_0;
    I1 := if K1==0 then map(S_(d-1),(ring S)^0,0)
    else inducedMap(S_(d-1),K1);
    I2 := if K2==0 then map(S_d,(ring S)^0,0)
    else inducedMap(S_(d),K2);
    if I1 == 0 or I2 == 0 then return map(source I1,source I2,0);
    ((S.dd_(d,0)*I2)//I1)
    )

makeNormMap(SimplicialModuleMap,ZZ) := (phi,d) -> (
    S1 := source phi;
    S2 := target phi;
    if d==0 then return  phi_0;
    K1 := intersect( for i from 1 to d list ker S1.dd_(d,i));
    K2 := intersect( for i from 1 to d list ker S2.dd_(d,i));
    inducedMap(K2,K1,phi_d)
    ) 

--this is the normalization functor from the category of simplicial modules
--back to the category of nonnegatively-graded chain complexes.
--The dispatcher `normalize = method(Options => true)` lives in SchurFunctors,
--where `normalize Filling` is also installed; we share that method here so the
--two packages cooperate on a single symbol.
normalize(SimplicialModule,ZZ) := Complex => {CheckSum => true, CheckComplex => true} >> opts -> (S,d) -> (
    if opts.CheckComplex and S.?complex then return naiveTruncation(S.complex,0,d);
    n := length components S;
    if opts.CheckSum and n>1 then return directSum for i to n-1 list normalize((components S)_i,d);
    complex for i from 1 to d list makeNormMap(S,i))

normalize(SimplicialModule) := Complex => {CheckSum => true, CheckComplex => true} >> opts -> S -> (if S.?complex then return normalize(S,S.complexLength,opts);
    normalize(S,S.topDegree,opts)
    )

normalize(SimplicialModuleMap,ZZ) := ComplexMap => {CheckSum => true, CheckComplex => true} >> opts -> (phi,d) -> (
    if opts.CheckComplex and phi.cache.?complexMap then return naiveTruncation(phi.cache.complexMap,0,d);
    n := length components phi;
    if opts.CheckSum and n>1 then return directSum for i to n-1 list normalize((components phi)_i,d);
    src := source phi;
    trg := target phi;
    C1 := normalize(src,d,opts);
    C2 := normalize(trg,d,opts);
    map(C2,C1,new HashTable from for i to max(max C1,max C2) list i => (if (f:=makeNormMap(phi,i)) == 0 then continue else map(C2_i, C1_i,  f)))
    )

normalize(SimplicialModuleMap) := ComplexMap => {CheckSum => true, CheckComplex => true} >> opts ->  phi -> (d := max(topDegree source phi,topDegree target phi);
    normalize(phi,d,opts)
    )


--this function computes the image of the 2nd exterior power into the tensor product
exteriorInclusion = method();
exteriorInclusion(Module) := M -> (
    inducedMap(M**M,image(id_M**id_M-tensorCommutativity(M,M)))
    )


exteriorInclusion(SimplicialModule) := S -> (cS := forgetComplex S;
    w2S := extPower(2,S);
    T := S**S;
    map(T,w2S,applyValues(cS.module, m -> dual wedgeProduct(1,1,dual m)))
    )

exteriorInclusion(Complex,ZZ) := (C,d) -> (normalize exteriorInclusion(simplicialModule(C,d)))
    
exteriorInclusion(Complex) := C -> (exteriorInclusion(C,length C))

--computes the image of the surjection from the simplicial tensor product
--onto the second symmetric power of a complex
symmetricQuotient = method();
symmetricQuotient(Module) := M -> (
    phi := exteriorInclusion(M);
    inducedMap(coker phi,ambient coker phi)
    )

symmetricQuotient(SimplicialModule) := S -> (
    phi := exteriorInclusion(S);
    inducedMap(coker phi,target phi)
    )

symmetricQuotient(Complex,ZZ) := (C,d) -> (normalize symmetricQuotient(simplicialModule(C,d)))

symmetricQuotient(Complex) := C -> (symmetricQuotient(C,length C))


--this computes the long exact sequence of homology induced by the canonical short
--exact sequence of complexes $0 --> \wedge^2 --> T^2 --> Sym^2 --> 0$
tensorLES = method();
tensorLES(Complex,ZZ) := (C,d) -> (phi1 := exteriorInclusion(C,d);
    phi2 := inducedMap(coker phi1,target phi1);
    longExactSequence(prune phi2,prune phi1, Concentration => LengthLimit => d-1)
    )



posComps = (n,d) -> (compositions(d, n-d))/(i -> i + toList(d : 1))

surjectionBijection = L -> (
    n := length L - 1;
    d := sum L - 1;
    lo := 0;
    H := new MutableHashTable from {};
    for i from 0 to n do (
	for j from lo to lo + L_i-1 do (
	    H#(j) = i;
	    );
	lo = lo + L_i;
	);
    new HashTable from H
    )

summandSurjection = method()
summandSurjection(ZZ,ZZ) := (n,d) -> (
    L := sort posComps(n+1,d+1);
    L/surjectionBijection
    )

    







