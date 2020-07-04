
newPackage(
       "SparseResultants",
        Version => "0.9", 
        Date => "June 13, 2020",
        Headline => "computations with sparse resultants",
        Authors => {{Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com"}},
        PackageExports => {"Resultants"},
        DebuggingMode => false
)

export{"sparseResultant", "sparseDiscriminant", "exponentsMatrix", "genericLaurentPolynomials", "denseResultant", "denseDiscriminant", "genericMultihomogeneousPolynomial"}

SPARSERESULTANT := local SPARSERESULTANT;

SparseResultant = new Type of HashTable;

char SparseResultant := (R) -> R#"characteristic";

SparseResultant SPACE Matrix := (R,F) -> (R#"evaluation") F;

SparseResultant SPACE VisibleList := (R,F) -> (R#"evaluation") toList(F);

SparseResultant SPACE Thing := (R,F) -> R (sequence F);

net SparseResultant := (R) -> net("sparse ")|net(if R#"Unmixed" then "unmixed" else "mixed")|net(" resultant associated to ")|net(R#"exponents")|net(" over ")|net(if char R == 0 then ZZ else ZZ/(char R));

sparseResultant = method(Dispatch => Thing, Options => {Unmixed => null, CoefficientRing => ZZ});

sparseResultant Thing := o -> I -> ( 
    I = toList sequence I;
    if #I == 1 then if instance(first I,VisibleList) then I = toList first I;
    if o.Unmixed =!= null then if not instance(o.Unmixed,Boolean) then error "Unmixed option accepts a boolean value";
    if o.CoefficientRing =!= ZZ then (try assert(o.CoefficientRing === ZZ/(char o.CoefficientRing)) else error "CoefficientRing option accepts ZZ or a ring of the form ZZ/p with p prime");
    if unique apply(I,i -> instance(i,Matrix)) === {true} then (
        if unique apply(I,i -> ring i === ZZ) === {true} then (
            if # I > 1 then (
                if o.Unmixed === true then error "the input 'Unmixed=>true' is inconsistent with the other ones";
                return sparseMixedResultant(I,char o.CoefficientRing); 
            ) else (
                if o.Unmixed === false then error "the input 'Unmixed=>false' is inconsistent with the other ones";
                return sparseUnmixedResultant(first I,char o.CoefficientRing);
            );
        );
    );
    if #I == 1 then if instance(first I,Matrix) then (
        if o.CoefficientRing =!= ZZ then error "the CoefficientRing option cannot be used when the polynomials are given in input (the right coefficient ring is taken automatically)";
        if o.Unmixed === true then return sparseUnmixedResultant(first I) else return sparseMixedResultant(first I);
    );
    if unique apply(I,i -> instance(i,RingElement)) === {true} then (
        if o.CoefficientRing =!= ZZ then error "the CoefficientRing option cannot be used when the polynomials are given in input (the right coefficient ring is taken automatically)";
        if o.Unmixed === true then return sparseUnmixedResultant(I) else return sparseMixedResultant(I);
    );
    error("expected one of the following:"|newline|"-- one or more matrices of integers to represent the exponent vectors of the monomials, or"|newline|"-- a list/sequence/row matrix of n+1 Laurent polynomials in n variables");
);

sparseUnmixedResultant = method();

sparseUnmixedResultant (Matrix,ZZ) := (M,ch) -> (
    if ring M =!= ZZ then error "expected a matrix over ZZ, the exponent vectors must be integers";
    M = trimExponents M;
    if instance(SPARSERESULTANT_(M,0),SparseResultant) then return SPARSERESULTANT_(M,0);
    if instance(SPARSERESULTANT_(M,ch),SparseResultant) then return SPARSERESULTANT_(M,ch);    
    n := numRows M;
    l := numColumns M;
    if l-1 < n then error "hypothesis not satisfied by the set of monomials (there are too few monomials)";
    if l-1 == n then return sparseMixedResultant(toList(l:M),ch);    
    f := mapFromMons(M,ch);
    w := dualizedChowForm f;
    val := method();
    val (Matrix) := (L) -> (
        v := gens ring L;
        if not (isPolynomialRing ring L and numRows L == 1 and numColumns L == n+1 and #v == n) then error ("expected a row matrix (or list) of "|toString(n+1)|" Laurent polynomials in "|toString(n)|" variables");
        if ch > 0 then if char coefficientRing ring L != ch then error("expected polynomials in characteristic "|toString(ch));
        mm := matrix {apply(entries transpose M,u -> product(n,i -> v_i^(u_i)))};
        A := transpose sub(last coefficients(L,Monomials=>mm),coefficientRing ring L);
        if sub(A,ring L) * (transpose mm) - (transpose L) != 0 then error("make sure that the involved monomials are in "|toString(flatten entries mm));
        s := map(ring A,ring w,apply(subsets(l,n+1),m -> det submatrix(A,m)));
        s w
    );
    val (List) := (L) -> (
        try Lm := matrix {L} else error ("expected a list (or row matrix) of "|toString(n+1)|" Laurent polynomials in "|toString(n)|" variables");
        val Lm
    );
    SPARSERESULTANT_(M,ch) = new SparseResultant from {
        "exponents" => M,
        "characteristic" => ch,
        "evaluation" => val,
        "Unmixed" => true,
        "map" => f,
        "dualizedChowForm" => w,
        "universalRes" => null        
    };
    SPARSERESULTANT_(M,ch)
);

sparseUnmixedResultant (Matrix) := (M) -> (
    if not (isPolynomialRing ring M and numRows M == 1) then error "expected a row matrix (or list) of Laurent polynomials";
    x := gens ring M;
    if numColumns M === #x then (
        d := unique apply(flatten entries monomials M,m -> first degree m);
        if #d == 1 then (
            t := local t;
            R := (coefficientRing ring M)[t_1..t_(#x - 1),Inverses=>(options ring M)#Inverses,MonomialOrder=>(if (options ring M)#Inverses then Lex else GRevLex)];
            s := map(R,ring M,matrix{{1}}|vars R);
            M' := s M;
            if unique apply(flatten entries M',m -> first degree m) === d then (
                <<"--warning: "|newline|"--the input of sparseResultant should be n+1 non-homogeneous polynomials in n variables;"|newline|"--a dishomogenization of the input polynomials was performed successfully."<<endl;
                return sparseUnmixedResultant M';
            );
        );
    );
    if numColumns M =!= #x + 1 then error "expected the number of polynomials to be one more than the number of variables";
    N := transpose matrix sort unique flatten apply(flatten entries M,exponents);
    Res := sparseUnmixedResultant(N,char coefficientRing ring M);
    return (Res M);
);

sparseUnmixedResultant (List) := (L) -> (
    try assert isPolynomialRing ring matrix {L} else error "expected a list (or row matrix) of Laurent polynomials";
    sparseUnmixedResultant matrix {L}
);

sparseMixedResultant = method();

sparseMixedResultant (List,ZZ) := (lM,ch) -> (
    try n := numRows first lM else error "expected a list of matrices";
    for M in lM do (
        if not instance(M,Matrix) then error "expected a list of matrices";
        if ring M =!= ZZ then error "expected a list of matrices over ZZ";
        if n =!= numRows M then error "the matrices must have the same number of rows";
    );
    if #lM =!= n+1 then error("expected "|toString(n+1)|" matrices, one more than the common number of rows of the matrices");
    lM = trimExponents lM;
    isUnmixed := # unique lM == 1;
    if isUnmixed then if numColumns(first lM) != n+1 then return sparseUnmixedResultant(first lM,ch);
    if instance(SPARSERESULTANT_(lM,0),SparseResultant) then return SPARSERESULTANT_(lM,0);
    if instance(SPARSERESULTANT_(lM,ch),SparseResultant) then return SPARSERESULTANT_(lM,ch);
    lM0 := apply(lM,m -> m - transpose matrix toList((numColumns m):apply(entries matrix pack(numColumns m,apply(flatten entries m,e -> if e>0 then 0 else e)),min)));
    lM0 = apply(lM0,m -> m - transpose matrix toList((numColumns m):apply(entries m,min)));
    t := local t; a := local a;
    KK := if ch == 0 then QQ else ZZ/ch;
    R := KK[t_1..t_n,flatten for i to n list for j to numColumns(lM_i)-1 list a_(i,j),MonomialOrder=>Eliminate n];
    I := ideal apply(n+1,i -> (matrix{apply(entries transpose lM0_i,u -> product(n,i -> t_(i+1)^(u_i)))} * transpose(matrix{{a_(i,0)..a_(i,numColumns(lM_i)-1)}}))_(0,0));
    for i from 1 to n do I = saturate(I,t_i);
    D := deepSplice apply(n+1,i -> (numColumns lM_i) : (entries diagonalMatrix toList((n+1):1))_i);
    S := KK[flatten entries submatrix'(vars R,{0..n-1}),Degrees=>D];
    Res := trim sub(ideal selectInSubring(1,gens gb(I,SubringLimit=>1)),S); 
    Res = Res_0;
    val := method();
    val (Matrix) := (L) -> (
        v := gens ring L;
        if not (isPolynomialRing ring L and numRows L == 1 and numColumns L == n+1 and #v == n) then error ("expected a row matrix (or list) of "|toString(n+1)|" Laurent polynomials in "|toString(n)|" variables");
        if ch > 0 then if char coefficientRing ring L != ch then error("expected polynomials in characteristic "|toString(ch));
        mm := apply(n+1,i -> matrix{apply(entries transpose lM_i,u -> product(n,i -> (v_i)^(u_i)))});
        A := apply(n+1,i -> transpose sub(last coefficients(submatrix(L,{i}),Monomials=>mm_i),coefficientRing ring L));
        for i to n do (
            if sub(A_i,ring L) * (transpose mm_i) - submatrix(L,{i}) != 0 then error("make sure that the involved monomials in the polynomial number "|toString(i)|" are in the list "|toString(flatten entries mm_i));
        );
        sub(Res,flatten for i to n list for j to numColumns(lM_i)-1 list a_(i,j) => (A_i)_(0,j))
    );
    val (List) := (L) -> (
        try Lm := matrix {L} else error ("expected a list (or row matrix) of "|toString(n+1)|" Laurent polynomials in "|toString(n)|" variables");
        val Lm
    );
    SPARSERESULTANT_(lM,ch) = new SparseResultant from {
        "exponents" => if isUnmixed then first lM else lM,
        "characteristic" => ch,
        "evaluation" => val,
        "Unmixed" => isUnmixed,
        "map" => null,
        "dualizedChowForm" => null,
        "universalRes" => Res
    };
    SPARSERESULTANT_(lM,ch)
);

sparseMixedResultant (Matrix) := (M) -> (
    if not (isPolynomialRing ring M and numRows M == 1) then error "expected a row matrix (or list) of Laurent polynomials";
    x := gens ring M;
    if numColumns M === #x then (
        d := apply(flatten entries M,m -> unique apply(flatten entries monomials m,m' -> first degree m'));
        if #select(d,i -> #i != 1) == 0 then (
            t := local t;
            R := (coefficientRing ring M)[t_1..t_(#x - 1),Inverses=>(options ring M)#Inverses,MonomialOrder=>(if (options ring M)#Inverses then Lex else GRevLex)];
            s := map(R,ring M,matrix{{1}}|vars R);
            M' := s M;
            if apply(flatten entries M',m -> first degree m) == flatten d then (
                <<"--warning: "|newline|"--the input of sparseResultant should be n+1 non-homogeneous polynomials in n variables;"|newline|"--a dishomogenization of the input polynomials was performed successfully."<<endl;
                return sparseMixedResultant M';
            );
        );
    );
    if numColumns M =!= #x + 1 then error "expected the number of polynomials to be one more than the number of variables";
    N := apply(flatten entries M,l -> transpose matrix sort exponents l);
    Res := sparseMixedResultant(N,char coefficientRing ring M);
    return (Res M);
);

sparseMixedResultant (List) := (L) -> (
    try assert isPolynomialRing ring matrix {L} else error "expected a list (or row matrix) of Laurent polynomials";
    sparseMixedResultant matrix {L}
);

denseResultant = method(Dispatch => Thing, Options => {CoefficientRing => ZZ, Algorithm => "Poisson"});

denseResultant Thing := o -> I -> ( 
    I = toList sequence I;
    if #I == 1 then if instance(first I,VisibleList) then I = toList first I;
    if unique apply(I,i -> instance(i,ZZ)) === {true} then (
        if o.Algorithm =!= "Poisson" then error "the Algorithm option can be only used when the polynomials are given in input"; 
        return sparseResultant(exponentsMatrix genericLaurentPolynomials I,CoefficientRing=>o.CoefficientRing);
    );
    if #I == 1 then if instance(first I,Matrix) then if numRows first I == 1 then I = flatten entries first I;
    if unique apply(I,i -> instance(i,RingElement)) === {true} then (
        R := ring first I;
        if isPolynomialRing R and unique apply(I,i -> ring i === R) === {true} then (
            if o.CoefficientRing =!= ZZ then error "the CoefficientRing option cannot be used when the polynomials are given in input (the right coefficient ring is taken automatically)";
            return affineResultant(I,Algorithm => o.Algorithm);
        );
    );   
    error("expected a sequence of n+1 integers or a list/sequence/row matrix of n+1 Laurent polynomials in n variables");
);

SPARSEDISCRIMINANT := local SPARSEDISCRIMINANT;

SparseDiscriminant = new Type of HashTable;

char SparseDiscriminant := (D) -> D#"characteristic";

SparseDiscriminant SPACE Thing := (D,F) -> (D#"evaluation") F;

net SparseDiscriminant := (D) -> net("sparse discriminant associated to ")|net(D#"exponents")|net(" over ")|net(if char D == 0 then ZZ else ZZ/(char D));

sparseDiscriminant = method(Options => {CoefficientRing => ZZ});

sparseDiscriminant (Matrix) := o -> (M) -> (
    if o.CoefficientRing =!= ZZ then (try assert(o.CoefficientRing === ZZ/(char o.CoefficientRing)) else error "CoefficientRing option accepts ZZ or a ring of the form ZZ/p with p prime");
    ch := char (o.CoefficientRing);
    if ring M =!= ZZ then error "expected a matrix over ZZ, the exponent vectors must be integers";
    M = trimExponents M;
    if instance(SPARSEDISCRIMINANT_(M,0),SparseDiscriminant) then return SPARSEDISCRIMINANT_(M,0);
    if instance(SPARSEDISCRIMINANT_(M,ch),SparseDiscriminant) then return SPARSEDISCRIMINANT_(M,ch);    
    n := numRows M;
    l := numColumns M;
    if l-1 <= n then error "hypothesis not satisfied by the set of monomials (there are too few monomials)";
    f := mapFromMons(M,ch);    
    w := dualvariety(trim kernel f);
    w = if numgens w == 1 then w_0 else 1_(ring w);
    val := method();
    val (RingElement) := (L) -> (
        v := gens ring L;
        if not (isPolynomialRing ring L and #v == n) then error ("expected a (Laurent) polynomial in "|toString(n)|" variables");
        if ch > 0 then if char coefficientRing ring L != ch then error("expected a polynomial in characteristic "|toString(ch));
        mm := matrix {apply(entries transpose M,u -> product(n,i -> v_i^(u_i)))};
        A := transpose sub(last coefficients(matrix{{L}},Monomials=>mm),coefficientRing ring L);
        if sub(A,ring L) * (transpose mm) - matrix{{L}} != 0 then error("make sure that the involved monomials are in "|toString(flatten entries mm));
        s := map(ring A,ring w,A);
        s w
    );
    val (Thing) := (L) -> error ("expected a (Laurent) polynomial in "|toString(n)|" variables");
    SPARSEDISCRIMINANT_(M,ch) = new SparseDiscriminant from {
        "exponents" => M,
        "characteristic" => ch,
        "evaluation" => val,
        "map" => f,
        "dualVar" => w    
    };
    SPARSEDISCRIMINANT_(M,ch)
);

sparseDiscriminant (RingElement) := o -> (M) -> (
    if o.CoefficientRing =!= ZZ then error "the CoefficientRing option cannot be used when the polynomial is given in input (the right coefficient ring is taken automatically)";
    if not isPolynomialRing ring M then error "expected a (Laurent) polynomial";
    N := transpose matrix sort exponents M;
    ch := char coefficientRing ring M;
    Disc := sparseDiscriminant(N,CoefficientRing => if ch == 0 then ZZ else ZZ/ch);
    return (Disc M);
);

denseDiscriminant = method(Options => {CoefficientRing => ZZ, Algorithm => "Poisson"});

denseDiscriminant (ZZ,ZZ) := o -> (d,n) -> (
    if n<=0 then error "the number of variables must be positive";
    if d<=0 then error "the degree must be positive";
    if o.Algorithm =!= "Poisson" then error "option not allowed";
    sparseDiscriminant(exponentsMatrix first genericLaurentPolynomials prepend(d,n:0),CoefficientRing=>o.CoefficientRing)
);

denseDiscriminant (RingElement) := o -> (f) -> (
    if o.CoefficientRing =!= ZZ then error "the CoefficientRing option cannot be used when the polynomial is given in input (the right coefficient ring is taken automatically)";
    if not isPolynomialRing ring f then error "expected a (Laurent) polynomial";
    affineDiscriminant(f,Algorithm=>o.Algorithm)
);

mapFromMons = method();
mapFromMons (Matrix,ZZ) := (M,ch) -> (
    n := numRows M;
    l := numColumns M;
    t := local t;
    KK := if ch == 0 then QQ else ZZ/ch;
    R := KK[t_0..t_n];
    R' := frac R;
    t' := gens R'; 
    F' := apply(entries transpose M,u -> product(n,i -> t'_(i+1)^(u_i)));
    F := flatten entries lift((lcm apply(F',denominator))*matrix{F'},R);
    d := first max apply(F,degree);
    F = apply(F,f -> t_0^(d-(first degree f)) * f);
    gcdF := gcd F;
    F = apply(F,fe -> first quotientRemainder(fe,gcdF));
    T := local T;
    S := Grass(0,l-1,KK,Variable=>T);
    map(R,S,F) 
);    

trimExponents = method();
trimExponents (Matrix) := (M) -> transpose matrix sort unique entries transpose M;
trimExponents (List) := (L) -> apply(L,M -> trimExponents M);

dualvariety = method(TypicalValue => Ideal);
dualvariety (Ideal) := (I) -> (
    if not (isPolynomialRing ring I and isHomogeneous I) then error "expected a homogeneous ideal in a polynomial ring";
    K := coefficientRing ring I;
    c := codim I;
    x := gens ring I;
    n := #x - 1;
    local J; local J'; local R; local j;
    U := for i to n list (
        R = K[delete(x_i,x)];
        J = I; j = 0;
        while j < numgens J and c < numgens J do (
            J' = ideal submatrix'(gens J,{j});
            if sub(sub(J,x_i=>1),R) == sub(sub(J',x_i=>1),R) then J = J' else j = j+1;
        );
        if numgens J <= c then break {(i,J)};
        (i,J)
    );
    iI := first U;
    for u from 1 to #U-1 do if numgens last U_u < numgens last iI then iI = U_u;
    i := first iI;
    F := gens last iI;
    (u,a,b) := (local u,local a,local b);
    m := numgens source F;
    Ru := K[u_1..u_m,a_0..a_(i-1),a_(i+1)..a_n,b_0..b_n,MonomialOrder => Eliminate (m+n)];
    s := map(Ru,ring F,submatrix(vars Ru,{m .. m+i-1})|matrix{{1}}|submatrix(vars Ru,{m+i .. m+n-1}));
    A := ideal(s F) + ideal(submatrix(vars Ru,{m+n .. m+2*n}) - submatrix(vars Ru,{0 .. m-1}) * (s transpose jacobian F));
    trim sub(sub(ideal selectInSubring(1,gens gb A),K[b_0..b_n]),vars ring F)
);

dualizedChowForm = method(TypicalValue => RingElement, Options => {AffineChartGrass => true}); 
dualizedChowForm (RingMap) := o -> (phi) -> (
   -- Input: phi:P^r-->P^n monomial unirational parameterization of a variety X subset P^n of dimension r
   -- Output: the dualized Chow form of im(phi) on GG(r,P^n*) = GG(n-r-1,P^n)
   if not isPolynomialRing target phi then error "the target of the ring map needs to be a polynomial ring";
   if not isPolynomialRing source phi then error "the source of the ring map needs to be a polynomial ring";
   K := coefficientRing source phi; 
   if not (K === coefficientRing target phi) then error "different coefficient rings encountered";
   if not isField K then error "the coefficient ring needs to be a field";
   F := transpose submatrix(matrix phi,{0..(numgens source phi -1)});
   if not (isHomogeneous ideal F and # unique degrees ideal F == 1) then error "the map needs to be defined by homogeneous polynomials of the same degree";
   n := numgens source phi -1;
   r := numgens target phi -1;
   kerPhi := kernel phi;
   if dim kerPhi =!= r+1 then error("hypothesis not satisfied by the set of monomials (the dimension of the associated toric variety is less than "|toString(r)|")");
   mnr := o.AffineChartGrass;
   if mnr === true then mnr = (random toList(0..n))_{0..r};
   try assert(ring matrix{mnr} === ZZ and min mnr >=0 and max mnr <=n and # unique mnr == r+1 and # mnr == r+1) else error("bad value for option AffineChartGrass: expected either 'true' or list of "|toString(r+1)|" distinct integers beetween 0 and "|toString(n)); 
   mnr = sort mnr; 
   x := local x; u := local u;
   R := K[x_0..x_r,u_(0,0)..u_(r,n),MonomialOrder=>Eliminate(r+1),Degrees=>{r+1:{1,0},(r+1)*(n+1):{0,1}}];
   U := transpose genericMatrix(R,u_(0,0),n+1,r+1);
   Ru := K[u_(0,0)..u_(r,n)]; 
   R = K[x_0..x_r,flatten entries submatrix'(U,mnr),MonomialOrder=>Eliminate(r+1),Degrees=>{r+1:{1,0},(r+1)*(n-r):{0,1}}];
   Ru = K[flatten entries submatrix'(U,mnr)];
   U = sub(sub(U,flatten for i to r list for j to r list U_(i,mnr_j) => (if i == j then 1 else 0)),R);
   t := gens target phi;
   eF := apply(flatten entries F,e -> select(r+1,i -> degree(t_i,e)>0));
   m := 0; for i from 1 to n do if # eF_i < # eF_m then m = i;
   jj := first eF_m;
   R = K[flatten entries submatrix'(vars R,{jj}),MonomialOrder=>Eliminate r,Degrees=>drop(degrees R,{jj,jj})]; 
   Sub := map(R,target phi,submatrix(vars R,{0..jj-1})|matrix{{1}}|submatrix(vars R,{jj..r-1}));
   Sub' := map(Ru,R,matrix{toList(r:0_K)} | vars Ru);
   U = sub(U,R);
   Inc := ideal(U * (Sub F)); 
   if # eF_m > 1 then Inc = saturate(Inc,Sub ideal product apply(drop(eF_m,1),i -> t_i));
   Z := trim Sub' ideal selectInSubring(1,gens gb(Inc,SubringLimit=>1)); 
   G := Grass(r,n,K,Variable=>(source phi));
   f := map(Ru/Z,G,gens minors(r+1,Sub' U));
   Zs := kernel'(degree kerPhi,f);
   Zs = trim sub(ideal homogenize(sub(Zs_1,ambient ring Zs),1+sub(Zs_0,ambient ring Zs)),ring Zs);
   if numgens Zs =!= 1 or Zs_0 == 1 then error "something went wrong";
   Zs_0
);

kernel' = method();
kernel' (ZZ,RingMap) := (d,f) -> (
   local I;
   for i from 1 do (
       I = trim kernel(f,SubringLimit=>i);
       if degrees I == {{1},{d}} then return I;
   );
);

exponentsMatrix = method(Dispatch => Thing, Options => {Unmixed => null});
exponentsMatrix Sequence := o -> F -> ( 
    F = sequence F;
    try (M := matrix {toList F}; assert isPolynomialRing ring M) else error "expected one or more (Laurent) polynomials in the same ring";
    if o.Unmixed =!= null then if not instance(o.Unmixed,Boolean) then error "Unmixed option accepts a boolean value";
    if o.Unmixed === true then return transpose matrix sort unique flatten apply(flatten entries M,exponents);
    N := apply(toSequence flatten entries M,l -> transpose matrix sort exponents l);
    if # unique N == 1 then return first N else return N;
);    
exponentsMatrix VisibleList := o -> F -> exponentsMatrix(toSequence F,Unmixed=>o.Unmixed);
exponentsMatrix RingElement := o -> F -> exponentsMatrix({F},Unmixed=>o.Unmixed);

genericLaurentPolynomials = method(Dispatch => Thing, Options => {CoefficientRing => ZZ});
genericLaurentPolynomials Sequence := o -> lM -> ( 
    lM = sequence lM;
    if # select(lM,i -> not instance(i,ZZ)) == 0 then (
        if min lM < 0 then error "expected nonnegative integers, the degrees of the generic polynomials";
        N := exponentsMatrix genericPolynomials toList lM;
        if not instance(N,Sequence) then N = #lM : N;
        return genericLaurentPolynomials(apply(N,m -> submatrix'(m,{0},)),CoefficientRing=>o.CoefficientRing);
    );
    try n := numRows first lM else error "expected a list of matrices";
    for M in lM do (
        if not instance(M,Matrix) then error "expected a list of matrices";
        if ring M =!= ZZ then error "expected a list of matrices over ZZ";
        if n =!= numRows M then error "the matrices must have the same number of rows";
    );
    if #lM =!= n+1 then error("expected "|toString(n+1)|" matrices, one more than the common number of rows of the matrices");
    lM = apply(lM,m -> trimExponents m);
    K := o.CoefficientRing;
    if not instance(K,Ring) then error "CoefficientRing option expects a ring";
    opts := if min apply(n+1,i -> min flatten entries lM_i) < 0 then (MonomialOrder=>Lex,Inverses=>true) else (MonomialOrder=>GRevLex,Inverses=>false);
    local vi;
    A := apply(n+1,i -> (vi = vars i; K[vi_0..vi_(numColumns(lM_i)-1),opts_0,opts_1]));
    R := A_0; for i from 1 to n do R = R**A_i;
    R = K[gens R,opts_0,opts_1];
    x := local x; P := R[x_1..x_n,opts_0,opts_1];
    apply(0..n,i -> (matrix{apply(entries transpose lM_i,u -> product(n,i -> x_(i+1)^(u_i)))} * transpose(sub(vars A_i,R)))_(0,0))
);    
genericLaurentPolynomials VisibleList := o -> lM -> genericLaurentPolynomials(toSequence lM,CoefficientRing=>o.CoefficientRing);

genericMultihomogeneousPolynomial = method(Options => {CoefficientRing => ZZ});
genericMultihomogeneousPolynomial (VisibleList,VisibleList) := o -> (k,d) -> ( 
    d = toList d; k = toList k;
    n := #k;
    if n != #d then error "expected two lists of the same length";
    if n == 0 or #select(d|k,i -> not instance(i,ZZ)) > 0 then error "expected two lists of integers"; 
    if #select(d|k,i -> i < 0) > 0 then error "expected two lists of nonnegative integers"; 
    if #select(k,i -> i <= 0) > 0 then error "the number of variables must be positive"; 
    K := o.CoefficientRing;
    if not instance(K,Ring) then error "CoefficientRing option expects a ring";
    a := local a;
    A := K[a_(n:0) .. a_(apply(0 .. n-1,i->binomial(k_i-1+d_i,k_i-1)-1))];
    local x;
    R := apply(n,i -> (x = vars(23+i); A[x_0..x_(k_i-1)]));
    P := R_0; for i from 1 to n-1 do P = P ** R_i;
    mm := apply(n,i -> flatten entries sub(gens (ideal vars R_i)^(d_i),P));
    local b;
    sum(gens A,e -> (b = last baseName e; e * product(n,i -> (mm_i)_(b_i))))
);

beginDocumentation() 

document { 
    Key => SparseResultants, 
    Headline => "computations with sparse resultants", 
    PARA{"This package provides the methods ",TO "sparseResultant"," and ",TO "sparseDiscriminant"," to calculate sparse resultants and sparse discriminants. See also the package ",TO Resultants,", which includes methods to calculate dense resultants and dense discriminants."},
    PARA{"For the definitions, see one of the following books:"}, 
    PARA{"(1) ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"},", by David A. Cox, John Little, Donal O'shea; "},
    PARA{"(2) ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky."},
    Caveat => {"Currently, the algorithms implemented are based on elimination via Gröbner basis methods. This should be significantly improved by implementing more specialized algorithms."}
}

document { 
    Key => {sparseResultant,(sparseResultant,Thing),[sparseResultant,Unmixed],[sparseResultant,CoefficientRing]}, 
    Headline => "sparse resultant (A-resultant)", 
    Usage => "sparseResultant A", 
    Inputs => {"A" => Sequence => {"or a ",TO List," of ",TEX///$n+1$///," matrices ",TEX///$A_0,\ldots,A_n$///," over ",TEX///$\mathbb{Z}$///," and with ",TEX///$n$///," rows to represent the exponent vectors of Laurent polynomials ",TEX///$f_0,\ldots,f_n$///," in ", TEX///$n$///," variables, ",TEX///$f_i = \sum_{\omega\in A_i} a_{\omega} x^{\omega}$///,". In the unmixed case, that is when ",TEX///$A_0=\cdots=A_n$///,", it is enough to pass just one of the matrices. Moreover, if one wants to perform computations only in characteristic ",TEX///$p>0$///,", then it is recommended to use the option ",TT "CoefficientRing=>ZZ/p","."}}, 
    Outputs => {{"the universal sparse resultant associated to ",TEX///$A$///,", that is, a function that to a ", TO Sequence," (or a ",TO List,", or a row matrix) of ", TEX///$n+1$///," Laurent polynomials ", TEX///$f_0,\ldots,f_n$///," in ", TEX///$n$///," variables over a coefficient ring ",TEX///$K$///,", involving only monomials from ",TEX///$A$///,", associates the sparse resultant of the polynomials ",TEX///$f_0,\ldots,f_n$///,", which is an element of ",TEX///$K$///,"."}}, 
    PARA {"Alternatively, one can apply the method directly to the list of Laurent polynomials ",TEX///$f_0,\ldots,f_n$///,". In this case, the matrices ",TEX///$A_0,\ldots,A_n$///, " are automatically determined by ",TO "exponentsMatrix",". If you want require that ",TEX///$A_0=\cdots=A_n$///,", then use the option ",TT "Unmixed=>true"," (this could be faster). Below we consider some examples."},
    PARA {"In the first example, we calculate the sparse (mixed) resultant associated to the three sets of monomials ",TEX///$(1,x y,x^2 y,x),(y,x^2 y^2,x^2 y,x),(1,y,x y,x)$///,". Then we evaluate it at the three polynomials ",TEX///$f = c_{(1,1)}+c_{(1,2)} x y+c_{(1,3)} x^2 y+c_{(1,4)} x, g = c_{(2,1)} y+c_{(2,2)} x^2 y^2+c_{(2,3)} x^2 y+c_{(2,4)} x, h = c_{(3,1)}+c_{(3,2)} y+c_{(3,3)} x y+c_{(3,4)} x$///,"."},
    EXAMPLE {
        "time Res = sparseResultant(matrix{{0,1,1,2},{0,0,1,1}},matrix{{0,1,2,2},{1,0,1,2}},matrix{{0,0,1,1},{0,1,0,1}})",
        "use QQ[c_(1,1)..c_(3,4)][x,y];",
        "(f,g,h) = (c_(1,1)+c_(1,2)*x*y+c_(1,3)*x^2*y+c_(1,4)*x, c_(2,1)*y+c_(2,2)*x^2*y^2+c_(2,3)*x^2*y+c_(2,4)*x, c_(3,1)+c_(3,2)*y+c_(3,3)*x*y+c_(3,4)*x)",
        "time Res(f,g,h)",
        "assert(Res(f,g,h) == sparseResultant(f,g,h))"
    },
    PARA {"In the second example, we calculate the sparse unmixed resultant associated to the set of monomials ",TEX///$(1,x,y,xy)$///,". Then we evaluate it at the three polynomials ",TEX///$f = a_0 + a_1 x + a_2 y + a_3 x y, g = b_0 + b_1 x + b_2 y + b_3 x y, h = c_0 + c_1 x + c_2 y + c_3 x y$///,". Moreover, we perform all the computation over ",TEX///$\mathbb{Z}/3331$///,"."},    
    EXAMPLE {
        "time Res = sparseResultant(matrix{{0,0,1,1},{0,1,0,1}},CoefficientRing=>ZZ/3331)",
        "use ZZ/3331[a_0..a_3,b_0..b_3,c_0..c_3][x,y];",
        "(f,g,h) = (a_0 + a_1*x + a_2*y + a_3*x*y, b_0 + b_1*x + b_2*y + b_3*x*y, c_0 + c_1*x + c_2*y + c_3*x*y)",
        "time Res(f,g,h)",
        "assert(Res(f,g,h) == sparseResultant(f,g,h))"
    },
    PARA {"In the third and last example, we compare the sparse mixed resultant and the sparse unmixed resultant of three generic polynomials of degrees 1,2,2 in two variables."},
    EXAMPLE {
        "(f,g,h) = genericLaurentPolynomials(1,2,2)",
        "time (MixedRes,UnmixedRes) = (sparseResultant(f,g,h),sparseResultant(f,g,h,Unmixed=>true))",
        "quotientRemainder(UnmixedRes,MixedRes)",
        "assert((denseResultant(2,2,2))(f,g,h) == UnmixedRes and (denseResultant(1,2,2))(f,g,h) == MixedRes)"
    },
    SeeAlso => {sparseDiscriminant, denseResultant, resultant, genericLaurentPolynomials} 
}

document { 
    Key => {sparseDiscriminant,(sparseDiscriminant,Matrix),(sparseDiscriminant,RingElement),[sparseDiscriminant,CoefficientRing]}, 
    Headline => "sparse discriminant (A-discriminant)", 
    Usage => "sparseDiscriminant A", 
    Inputs => {"A" => Matrix => {"a matrix over ",TEX///$\mathbb{Z}$///," with ",TEX///$n$///," rows to represent the exponent vectors of a Laurent polynomial ",TEX///$f$///," in ", TEX///$n$///," variables, ",TEX///$f = \sum_{\omega\in A} a_{\omega} x^{\omega}$///,". If one wants to perform computations only in characteristic ",TEX///$p>0$///,", then it is recommended to use the option ",TT "CoefficientRing=>ZZ/p","."}}, 
    Outputs => {{"the universal sparse discriminant associated to ",TEX///$A$///,", that is, a function that to a Laurent polynomial ", TEX///$f$///," in ", TEX///$n$///," variables over a coefficient ring ",TEX///$K$///,", involving only monomials from ",TEX///$A$///,", associates the sparse discriminant of ",TEX///$f$///,", which is an element of ",TEX///$K$///,"."}}, 
    PARA {"Alternatively, one can apply the method directly to the Laurent polynomial ",TEX///$f$///,". In this case, the matrix ",TEX///$A$///," is automatically determined by ",TO "exponentsMatrix","."},
    PARA {"As an example, we now calculate the sparse discriminant of a generic trilinear form on ",TEX///$\mathbb{P}^1\times\mathbb{P}^2\times \mathbb{P}^1$///,", that is, the hyperdeterminant of a generic three-dimensional matrix of size ",TEX///$2\times 3\times 2$///,"."},
    EXAMPLE {
        "f = genericMultihomogeneousPolynomial((2,3,2),(1,1,1))",
        "time sparseDiscriminant f",
        "A = exponentsMatrix f",
        "Disc = sparseDiscriminant A",
        "assert(Disc f == sparseDiscriminant f)"
    },
    SeeAlso => {sparseResultant, denseDiscriminant, discriminant, genericMultihomogeneousPolynomial}
}

undocumented {(exponentsMatrix,VisibleList),(exponentsMatrix,RingElement)}
document { 
    Key => {exponentsMatrix,(exponentsMatrix,Sequence),[exponentsMatrix,Unmixed]}, 
    Headline => "exponents in one or more polynomials", 
    Usage => "exponentsMatrix(f_0,f_1,...)", 
    Inputs => {{TT "(f_0,f_1,...)"," one or more Laurent polynomials in the same ring."}},
    Outputs => {{TT "(A_0,A_1,...)"," a sequence of matrices such that the columns of ",TT "A_i"," are the ",TO exponents," of ",TT "f_i",". If either the option ",TT "Unmixed"," is set to true or all the matrices ",TT"A_i"," are equal, then just one matrix is returned."}},
    EXAMPLE {
        "QQ[x,y,z];",
        "f = (x^2 - 7 + x*y*z^11 + y, -3*y^7*z + x^3*y + 5*x^2);",
        "exponentsMatrix(f_0)",
        "exponentsMatrix(f_0,f_1)",
        "exponentsMatrix(f_0,f_1,Unmixed=>true)"
    },
    SeeAlso => {exponents, sparseResultant, genericLaurentPolynomials}
}

undocumented {(genericLaurentPolynomials,VisibleList)}
document { 
    Key => {genericLaurentPolynomials,(genericLaurentPolynomials,Sequence),[genericLaurentPolynomials,CoefficientRing]}, 
    Headline => "generic Laurent polynomials", 
    Usage => "genericLaurentPolynomials A", 
    Inputs => {"A" => Sequence => {"or a ",TO List," of ",TEX///$n+1$///," matrices ",TEX///$A_0,\ldots,A_n$///," over ",TEX///$\mathbb{Z}$///," and with ",TEX///$n$///," rows to represent the exponent vectors of Laurent polynomials ",TEX///$f_0,\ldots,f_n$///," in ", TEX///$n$///," variables. For the dense case, one can just pass the sequence of the degrees of ",TEX///$f_0,\ldots,f_n$///,"."}}, 
    Outputs => {{"the generic Laurent polynomials ",TEX///$f_0,\ldots,f_n$///," in the ring ",TEX///$\mathbb{Z}[a_0,a_1,\ldots,b_0,b_1,\ldots][x_1,\ldots,x_n]$///,", involving only the monomials from ",TEX///$A$///,"."}},
    PARA{"This method helps to construct special types of sparse resultants, see for instance ",TO "denseResultant","."},
    EXAMPLE {
        "M = (matrix{{2,3,4,5},{0,2,1,0}},matrix{{1,-1,0,2,3},{-2,0,-7,-1,0}},matrix{{-1,0,6},{-2,1,3}})",
        "genericLaurentPolynomials M",
        "genericLaurentPolynomials (2,3,1)",
    },
    SeeAlso => {sparseResultant, exponentsMatrix}
}

document { 
    Key => {genericMultihomogeneousPolynomial,(genericMultihomogeneousPolynomial,VisibleList,VisibleList),[genericMultihomogeneousPolynomial,CoefficientRing]}, 
    Headline => "generic multi-homogeneous polynomial", 
    Usage => "genericMultihomogeneousPolynomial((k_1,...,k_n),(d_1,...,d_n))", 
    Inputs => {{TT"(k_1,...,k_n)",", a sequence of positive integers to indicate ",TEX///$n$///," sets of variables ",TEX///$X_1,\ldots,X_n$///," with ",TEX///$#(X_i) = k_i$///,"."},
               {TT"(d_1,...,d_n)",", a sequence of nonnegative integers."}},
    Outputs => {{"the generic multi-homogeneous polynomial of multi-degree ",TEX///$(d_1,\ldots,d_n)$///," in the above sets of variables."}},
    PARA{"This method helps to construct special types of sparse discriminants. For instance, the hyperdeterminant of a generic ",TEX///$(k_1\times\cdots\times k_n)$///,"-matrix can be obtained with the code: ", TT "sparseDiscriminant genericMultihomogeneousPolynomial((k_1,...,k_n),(1,...,1))","."},
    EXAMPLE {
        "genericMultihomogeneousPolynomial((2,2,3),(1,1,1))",
        "genericMultihomogeneousPolynomial((2,3),(3,1))",
        "genericMultihomogeneousPolynomial((2,2),(1,1),CoefficientRing=>ZZ/33331)"
    },
    SeeAlso => {sparseDiscriminant, exponentsMatrix}
}

document { 
    Key => {denseResultant,(denseResultant,Thing),[denseResultant,Algorithm],[denseResultant,CoefficientRing]}, 
    Headline => "dense resultant (classical resultant)", 
    Usage => "denseResultant(d_0,d_1,...)"|newline|"denseResultant(f_0,f_1,...)", 
    Inputs => {{TT"(d_0,d_1,...)",", a sequence of integers."},
               {TT"(f_0,f_1,...)",", a sequence of ",TEX///$n+1$///," Laurent polynomials in ",TEX///$n$///," variables."}}, 
    Outputs => {{"for ",TT "(d_0,d_1,...)",", this is the same of ", TO sparseResultant," ",TO exponentsMatrix," ",TO genericLaurentPolynomials, TT"(d_0,d_1,...)",";"},
                {"for ",TT "(f_0,f_1,...)",", this is the same of ", TO affineResultant,TT"{f_0,f_1,...}","."}}, 
    EXAMPLE {
        "(f0,f1,f2) = genericLaurentPolynomials(1,2,2)",
        "time denseResultant(f0,f1,f2); -- using Poisson formula",
        "time denseResultant(f0,f1,f2,Algorithm=>\"Macaulay\"); -- using Macaulay formula",
        "time (denseResultant(1,2,2)) (f0,f1,f2); -- using sparseResultant",
        "assert(o2 == o3 and o3 == o4)"
    },
    SeeAlso => {sparseResultant, affineResultant, denseDiscriminant, exponentsMatrix, genericLaurentPolynomials}
}

document { 
    Key => {denseDiscriminant,(denseDiscriminant,ZZ,ZZ),(denseDiscriminant,RingElement),[denseDiscriminant,Algorithm],[denseDiscriminant,CoefficientRing]}, 
    Headline => "dense discriminant (classical discriminant)", 
    Usage => "denseDiscriminant(d,n)"|newline|"denseDiscriminant f", 
    Inputs => {{TT"(d,n)",", a pair of two integers representing, respectively, the degree and the number of variables of a Laurent polynomial ",TEX///$f$///,"."},
               "f" => RingElement => {"a Laurent polynomial of degree ",TEX///$d$///," in ",TEX///$n$///," variables."}}, 
    Outputs => {{"for ",TT "(d,n)",", this is the same of ", TO sparseDiscriminant," ", TO exponentsMatrix, EM "(generic polynomial of degree d in n variables);"},
                {"for ",TT "f",", this is the same of ", TO affineDiscriminant,TT"(f)","."}}, 
    EXAMPLE {
        "(d,n) := (2,3);",
        "time Disc = denseDiscriminant(d,n)",
        "f = first genericLaurentPolynomials prepend(d,n:0)",
        "assert(Disc(f) == denseDiscriminant(f))"
    },
    SeeAlso => {sparseDiscriminant, affineDiscriminant, denseResultant, exponentsMatrix, genericLaurentPolynomials}
}


TEST ///
 -- p. 318 Cox-Little-Shea
M = matrix {{0,1,0,1},{0,0,1,1}};
R := QQ[a_0..a_3,b_0..b_3,c_0..c_3][s,t];
f = a_0 + a_1*s + a_2*t + a_3*s*t;
g = b_0 + b_1*s + b_2*t + b_3*s*t;
h = c_0 + c_1*s + c_2*t + c_3*s*t;
Res = sparseResultant M;
assert(Res(f,g,h) == det matrix {
{a_0,a_1,a_2,a_3,0,0},
{b_0,b_1,b_2,b_3,0,0},
{c_0,c_1,c_2,c_3,0,0},
{0,a_0,0,a_2,a_1,a_3},
{0,b_0,0,b_2,b_1,b_3},
{0,c_0,0,c_2,c_1,c_3}})
assert(Res(f,g,h) == sparseResultant {f,g,h})
///

TEST ///
M = matrix{{0,1,0,1,2,0},
           {0,0,1,1,0,2}};
time U = sparseResultant M
F = matrix{toList genericPolynomials {2,2,2}};
F = flatten entries sub(sub(F,first gens ring F => 1),(coefficientRing ring F)[(gens ring F)_{1..(numgens ring F -1)}])
assert(U F === affineResultant F)
--
G = genericPolynomials((1,1,2),ZZ/33331);
assert(sparseResultant G === resultant G)
x = gens ring G_0;
assert(sparseResultant(x_1^2 * x_2 * G_0,x_1^3 * G_1,x_2 * G_2) === sparseResultant G)
R := newRing(coefficientRing ring G_0,Inverses=>true,MonomialOrder=>Lex)[gens ring G_0,Inverses=>true,MonomialOrder=>Lex]
x = gens R;
G = flatten entries sub(matrix{G},R)
assert(sparseResultant(x_2^(-2)*x_1^(-3) * G_0,x_1^3 *x_1^(-1) * G_1,x_2^4 * x_1^(-4) * G_2) === sparseResultant G)
--
G = genericPolynomials((2,2,2),ZZ/33331);
assert(sparseResultant G === resultant G)
x = gens ring G_0;
assert(sparseResultant(x_1^2 * x_2 * G_0,x_1^2 * x_2 *  G_1,x_1^2 * x_2 * G_2) === sparseResultant G)
R := newRing(coefficientRing ring G_0,Inverses=>true,MonomialOrder=>Lex)[gens ring G_0,Inverses=>true,MonomialOrder=>Lex]
x = gens R;
G = flatten entries sub(matrix{G},R)
assert(sparseResultant(x_2^(-2)*x_1^(-3) * G_0,x_2^(-2)*x_1^(-3) * G_1,x_2^(-2)*x_1^(-3) * G_2) === sparseResultant G)
///

TEST ///
F = genericLaurentPolynomials({1,1},CoefficientRing=>ZZ/33331);
elapsedTime R = denseResultant F;
elapsedTime S = sparseResultant F;
assert(R == -S)
--
F = genericLaurentPolynomials({1,1,1},CoefficientRing=>ZZ/33331);
elapsedTime R = denseResultant F;
elapsedTime S = sparseResultant F;
assert(R == S)
--
F = genericLaurentPolynomials((1,1,1,1),CoefficientRing=>ZZ/33331);
elapsedTime R = denseResultant F;
elapsedTime S = sparseResultant F;
assert(R == -S)
--
F = genericLaurentPolynomials((1,2,2),CoefficientRing=>ZZ/33331);
elapsedTime R = denseResultant F;
elapsedTime S = sparseResultant F;
assert(R == S)
--
F = genericLaurentPolynomials((1,1,2,1),CoefficientRing=>ZZ/33331);
elapsedTime R = denseResultant F;
elapsedTime S = sparseResultant F;
assert(R == S)
--
F = genericLaurentPolynomials((3,4),CoefficientRing=>ZZ/33331);
elapsedTime R = denseResultant F;
elapsedTime S = sparseResultant F;
assert(R == S)
--
F = genericLaurentPolynomials([3,1,1],CoefficientRing=>ZZ/33331);
elapsedTime R = denseResultant F;
elapsedTime S = sparseResultant F;
assert(R == S)
///

TEST /// -- Corollary 2.2, p. 256 [GKZ]
property = (M) -> (
    M = transpose matrix M;
    K := ZZ/33331; n := numRows M;
    t := local t; R := K[t_1..t_n];
    f := apply(entries transpose M,u -> product(n,i -> t_(i+1)^(u_i)));
    d := first max apply(f,degree);
    R' := K[t_0..t_n];
    F := apply(f,u -> t_0^(d-(first degree u)) * sub(u,R'));
    phi := map(R',Grass(0,numColumns(M)-1,K),F); 
    <<"phi: PP^"<<n<<" --> PP^"<<numgens source phi<<", degree of image: "<<degree kernel phi<<endl;
    A := matrix for i to n list for j to n list random(K);
    <<"det A = "<<det A<<endl;
    Res := sparseResultant(M,CoefficientRing=>K);
    <<Res<<endl;
    pols := apply(n+1,i -> sum(f,u -> random(K) * u));
    pols' := flatten entries (A*(transpose matrix{pols}));
    Res(pols') == (det A)^(degree kernel phi) * Res(pols)
);
assert property {{0,0,0},{1,0,0},{0,1,0},{0,0,1},{0,1,2}};
assert property {{0,0,0},{1,0,0},{1,1,0},{0,1,0},{0,0,1},{0,1,2}};
assert property {{0,0,0},{1,0,0},{1,1,0},{0,1,0},{0,0,2},{1,0,1}};
assert property {{1,0,0,1},{1,1,0,1},{0,1,0,0},{0,0,2,0},{1,0,1,0}};
assert property {{3,0},{0,3},{4,5},{4,2}};
///

TEST ///
F = first genericPolynomials({3,-1},ZZ/33331)
assert(sparseDiscriminant F == -discriminant F)
F = first genericPolynomials({4,-1},ZZ/33331)
assert(sparseDiscriminant F == discriminant F)
F = first genericPolynomials({5,-1},ZZ/33331)
assert(sparseDiscriminant F == discriminant F)
F = first genericPolynomials({2,-1,-1},ZZ/33331)
assert(sparseDiscriminant F == -discriminant F)
F = first genericPolynomials({2,-1,-1,-1},ZZ/33331)
assert(sparseDiscriminant F == discriminant F)
--
F = first genericLaurentPolynomials({3,0},CoefficientRing=>ZZ/33331)
assert(sparseDiscriminant F == -denseDiscriminant F)
F = first genericLaurentPolynomials({4,0},CoefficientRing=>ZZ/33331)
assert(sparseDiscriminant F == denseDiscriminant F)
F = first genericLaurentPolynomials({5,0},CoefficientRing=>ZZ/33331)
assert(sparseDiscriminant F == denseDiscriminant F)
F = first genericLaurentPolynomials({2,0,0},CoefficientRing=>ZZ/33331)
assert(sparseDiscriminant F == -denseDiscriminant F)
F = first genericLaurentPolynomials({2,0,0,0},CoefficientRing=>ZZ/33331)
assert(sparseDiscriminant F == denseDiscriminant F)
///

TEST /// -- "Cayley trick"
F = matrix{toList genericLaurentPolynomials((2,3),CoefficientRing=>ZZ/33331)}
R = (coefficientRing ring F)[gens ring F,y];
f = flatten entries sub(F,R)
assert(sparseResultant(F) == -sparseDiscriminant(f_1 + y*f_0))
--
F = matrix{toList genericLaurentPolynomials((3,3),CoefficientRing=>ZZ/33331)}
R = (coefficientRing ring F)[gens ring F,y];
f = flatten entries sub(F,R)
assert(sparseResultant(F) == -sparseDiscriminant(f_1 + y*f_0))
--
F = matrix{toList genericLaurentPolynomials((1,1,2),CoefficientRing=>ZZ/33331)}
R = (coefficientRing ring F)[gens ring F,y1,y2];
f = flatten entries sub(F,R)
assert(sparseResultant(F) == sparseDiscriminant(f_2 + y1*f_0 + y2*f_1))
///

