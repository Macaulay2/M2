
-*
   Copyright 2020, Giovanni Staglianò.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

newPackage(
       "SparseResultants",
        Version => "1.2", 
        Date => "July 8, 2021",
        Headline => "computations with sparse resultants",
        Authors => {{Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com"}},
	Keywords => {"Commutative Algebra"},
        PackageExports => {"Resultants"},
        DebuggingMode => false,
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "A package for computations with sparse resultants",
	     "acceptance date" => "5 May 2021",
	     "published article URI" => "https://msp.org/jsag/2021/11-1/p07.xhtml",
	     "published article DOI" => "10.2140/jsag.2021.11.61",
	     "published code URI" => "https://msp.org/jsag/2021/11-1/jsag-v11-n1-x07-SparseResultants.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/SparseResultants.m2",
	     "release at publication" => "4b0b826f08857b22cf17aebf9c56257ff44d8946",	    -- git commit number in hex
	     "version at publication" => "1.1",
	     "volume number" => "11",
	     "volume URI" => "https://msp.org/jsag/2021/11-1/"
	     }
)

export{"sparseResultant", "SparseResultant", "sparseDiscriminant", "SparseDiscriminant",
       "denseResultant", "denseDiscriminant",
       "exponentsMatrix", "genericLaurentPolynomials", "genericMultihomogeneousPolynomial",
       "MultidimensionalMatrix", "multidimensionalMatrix", "permute", "shape", "reverseShape", "sortShape", "sylvesterMatrix", "degreeDeterminant", "flattening",
       "randomMultidimensionalMatrix", "genericMultidimensionalMatrix", "genericSymmetricMultidimensionalMatrix", "genericSkewMultidimensionalMatrix"}

hasAttribute = value Core#"private dictionary"#"hasAttribute";
getAttribute = value Core#"private dictionary"#"getAttribute";
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary";

SPARSERESULTANT := local SPARSERESULTANT;

SparseResultant = new Type of HashTable;

globalAssignment SparseResultant;

char SparseResultant := (R) -> R#"characteristic";

exponents SparseResultant := (R) -> R#"exponents";

SparseResultant Matrix := (R,F) -> (R#"evaluation") F;

SparseResultant VisibleList := (R,F) -> (R#"evaluation") toList(F);

SparseResultant Thing := (R,F) -> R (sequence F);

toString SparseResultant := net SparseResultant := R -> (
    if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
    else "-*An example of sparse resultant*-"
);

SparseResultant#{Standard,AfterPrint} = SparseResultant#{Standard,AfterNoPrint} = (R) -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class R << " ("; 
    << "sparse " << (if R#"Unmixed" then "unmixed" else "mixed") << " resultant associated to " << R#"exponents"; 
    if char R > 0 then (<< " over " << toString(ZZ/(char R)));
    << ")" << endl;
);

describe SparseResultant := (R) -> (
    str := "sparse "|(if R#"Unmixed" then "unmixed" else "mixed")|" resultant associated to "|net(R#"exponents");
    if char R > 0 then str = str|" over "|toString(ZZ/char R);
    str = toString(str)|newline;
    W := R#"dualizedChowForm";
    if W =!= null then (
        f := R#"map";
        str = str|"associated monomial map: PP^"|toString(numgens target f -1)|" --> PP^"|toString(numgens source f -1)|newline;
        X := trim kernel f;
        str = str|"associated toric variety: (dim = "|toString(max(dim X -1,-1))|", degree = "|toString(degree X)|", degrees gens = "|toString(flatten degrees X)|")"|newline;
        str = str|"dualized Chow form: polynomial of degree "|toString(first degree W)|" in the coordinate ring of GG"|toString take(Grass ring W,2)|" with "|toString(# terms W)|" terms";
    ) else (
        E := R#"universalRes";
        str = str|"expansion: polynomial of degree "|toString(degree E)|" in "|toString(numgens ring E)|" variables with "|toString(# terms E)|" terms";
    );
    str
);

sparseResultant = method(TypicalValue => SparseResultant, Dispatch => Thing, Options => {Unmixed => null, CoefficientRing => ZZ});

sparseResultant Thing := o -> I -> ( 
    I = toList sequence I;
    if #I == 1 then if instance(first I,VisibleList) then I = toList first I;
    if o.Unmixed =!= null then if not instance(o.Unmixed,Boolean) then error "Unmixed option accepts a boolean value";
    if o.CoefficientRing =!= ZZ then (try assert(o.CoefficientRing === ZZ/(char o.CoefficientRing)) else error "CoefficientRing option accepts ZZ or a ring of the form ZZ/p with p prime");
    if all(I,i -> instance(i,Matrix) and ring i === ZZ) then (
        if # I > 1 then (
            if o.Unmixed === true then error "the input 'Unmixed=>true' is inconsistent with the other ones";
            return sparseMixedResultant(I,char o.CoefficientRing); 
        ) else (
            if o.Unmixed === false then error "the input 'Unmixed=>false' is inconsistent with the other ones";
            return sparseUnmixedResultant(first I,char o.CoefficientRing);
        );
    );
    if all(I,i -> instance(i,RingElement)) or (#I == 1 and instance(first I,Matrix)) then (
        if isPolynomialRing ring first I then if o.CoefficientRing =!= ZZ then error "the CoefficientRing option cannot be used when the polynomials are given in input (the right coefficient ring is taken automatically)";
        if instance(first I,Matrix) then I = first I;
        if o.Unmixed === true then (return sparseUnmixedResultant I) else (return sparseMixedResultant I);
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
                <<"--warning: " << endl <<"--the input of sparseResultant should be n+1 non-homogeneous polynomials in n variables;" << endl <<"--a dishomogenization of the input polynomials was performed successfully."<<endl;
                return sparseUnmixedResultant M';
            );
        );
    );
    if numColumns M =!= #x + 1 then error "expected the number of polynomials to be 1 plus the number of variables";
    N := transpose matrix sort unique flatten apply(flatten entries M,exponents);
    Res := sparseUnmixedResultant(N,char coefficientRing ring M);
    return (Res M);
);

sparseUnmixedResultant (List) := (L) -> (
    if not (all(L,l -> instance(l,RingElement)) and all(L,l -> ring l === ring first L) and isPolynomialRing ring first L) then error "expected a list (or row matrix) of Laurent polynomials";
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
            if sub(A_i,ring L) * (transpose mm_i) - submatrix(L,{i}) != 0 then error("make sure that the involved monomials in the polynomial number "|toString(i+1)|" are in the list "|toString(flatten entries mm_i));
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
                <<"--warning: " << endl <<"--the input of sparseResultant should be n+1 non-homogeneous polynomials in n variables;"<< endl <<"--a dishomogenization of the input polynomials was performed successfully."<<endl;
                return sparseMixedResultant M';
            );
        );
    );
    if numColumns M =!= #x + 1 then error "expected the number of polynomials to be 1 plus the number of variables";
    N := apply(flatten entries M,l -> transpose matrix sort exponents l);
    Res := sparseMixedResultant(N,char coefficientRing ring M);
    return (Res M);
);

sparseMixedResultant (List) := (L) -> (
    if not (all(L,l -> instance(l,RingElement)) and all(L,l -> ring l === ring first L) and isPolynomialRing ring first L) then error "expected a list (or row matrix) of Laurent polynomials";
    sparseMixedResultant matrix {L}
);

denseResultant = method(Dispatch => Thing, Options => {CoefficientRing => ZZ, Algorithm => "Poisson"});

denseResultant Thing := o -> I -> ( 
    I = toList sequence I;
    if #I == 1 then if instance(first I,VisibleList) then I = toList first I;
    if all(I,i -> instance(i,ZZ)) then (
        if o.Algorithm =!= "Poisson" then error "the Algorithm option can be only used when the polynomials are given in input"; 
        return sparseResultant(exponentsMatrix genericLaurentPolynomials I,CoefficientRing=>o.CoefficientRing);
    );
    if #I == 1 then if instance(first I,Matrix) then if numRows first I == 1 then I = flatten entries first I;
    if all(I,i -> instance(i,RingElement)) then (
        R := ring first I;
        if isPolynomialRing R and all(I,i -> ring i === R) then (
            if o.CoefficientRing =!= ZZ then error "the CoefficientRing option cannot be used when the polynomials are given in input (the right coefficient ring is taken automatically)";
            return affineResultant(I,Algorithm=>o.Algorithm);
        );
    );   
    error("expected a sequence of n+1 integers or a list/sequence/row matrix of n+1 Laurent polynomials in n variables");
);

SPARSEDISCRIMINANT := local SPARSEDISCRIMINANT;

SparseDiscriminant = new Type of HashTable;

globalAssignment SparseDiscriminant;

char SparseDiscriminant := (D) -> D#"characteristic";

exponents SparseDiscriminant := (D) -> D#"exponents";

SparseDiscriminant Thing := (D,F) -> (D#"evaluation") F;

toString SparseDiscriminant := net SparseDiscriminant := D -> (
    if hasAttribute(D,ReverseDictionary) then toString getAttribute(D,ReverseDictionary)
    else "-*An example of sparse discriminant*-"
);

SparseDiscriminant#{Standard,AfterPrint} = SparseDiscriminant#{Standard,AfterNoPrint} = (D) -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class D << " ("; 
    << "sparse discriminant associated to " << D#"exponents"; 
    if char D > 0 then (<< " over " << toString(ZZ/(char D)));
    << ")" << endl;
);

describe SparseDiscriminant := (D) -> (
    str := "sparse discriminant associated to "|net(D#"exponents");
    if char D > 0 then str = str|" over "|toString(ZZ/char D);
    str = toString(str)|newline;
    f := D#"map";
    str = str|"associated monomial map: PP^"|toString(numgens target f -1)|" --> PP^"|toString(numgens source f -1)|newline;
    Y := D#"dualVar";
    str = str|"dual variety of the associated toric variety: hypersurface in PP^"|toString(numgens ring Y -1)|" of degree "|toString(first degree Y)|" defined by a polynomial with "|toString(# terms Y)|" terms";
    str
);

sparseDiscriminant = method(TypicalValue => SparseDiscriminant, Options => {CoefficientRing => ZZ});

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
    w = if numgens w == 1 then w_0 else error("the sparse discriminant associated to "|(toString M)|" does not exist");
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
   try assert(ring matrix{mnr} === ZZ and min mnr >=0 and max mnr <=n and # unique mnr == r+1 and # mnr == r+1) else error("bad value for option AffineChartGrass: expected either 'true' or list of "|toString(r+1)|" distinct integers between 0 and "|toString(n)); 
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
genericLaurentPolynomials Sequence := o -> lM -> memGenLaurPols(lM,o.CoefficientRing);
genericLaurentPolynomials VisibleList := o -> lM -> memGenLaurPols(toSequence lM,o.CoefficientRing);

memGenLaurPols = memoize(
    (lM,K) -> (
        if all(lM,i -> instance(i,ZZ)) then (
            if min lM < 0 then error "expected nonnegative integers, the degrees of the generic polynomials";
            N := exponentsMatrix genericPolynomials toList lM;
            if not instance(N,Sequence) then N = #lM : N;
            return memGenLaurPols(apply(N,m -> submatrix'(m,{0},)),K);
        );
        try n := numRows first lM else error "expected a list of matrices";
        for M in lM do (
            if not instance(M,Matrix) then error "expected a list of matrices";
            if ring M =!= ZZ then error "expected a list of matrices over ZZ";
            if n =!= numRows M then error "the matrices must have the same number of rows";
        );
        if #lM =!= n+1 then error("expected "|toString(n+1)|" matrices, one more than the common number of rows of the matrices");
        lM = apply(lM,m -> trimExponents m);
        if not instance(K,Ring) then error "CoefficientRing option expects a ring";
        opts := if min apply(n+1,i -> min flatten entries lM_i) < 0 then (MonomialOrder=>Lex,Inverses=>true) else (MonomialOrder=>GRevLex,Inverses=>false);
        local vi;
        A := apply(n+1,i -> (vi = vars i; K[vi_0..vi_(numColumns(lM_i)-1),opts_0,opts_1]));
        R := A_0; for i from 1 to n do R = R**A_i;
        R = K[gens R,opts_0,opts_1];
        x := local x; P := R[x_1..x_n,opts_0,opts_1];
        apply(0..n,i -> (matrix{apply(entries transpose lM_i,u -> product(n,i -> x_(i+1)^(u_i)))} * transpose(sub(vars A_i,R)))_(0,0))
    )
);

genericMultihomogeneousPolynomial = method(Options => {CoefficientRing => ZZ, Variable => "a"});
genericMultihomogeneousPolynomial (VisibleList,VisibleList) := o -> (k,d) -> memGenMultHomPols(k,d,o.CoefficientRing,o.Variable);

memGenMultHomPols = memoize(
    (k,d,K,var) -> (
        d = toList d; k = toList k;
        n := #k;
        if n != #d then error "expected two lists of the same length";
        if n == 0 or #select(d|k,i -> not instance(i,ZZ)) > 0 then error "expected two lists of integers"; 
        if #select(d|k,i -> i < 0) > 0 then error "expected two lists of nonnegative integers"; 
        if #select(k,i -> i <= 0) > 0 then error "the number of variables must be positive"; 
        if not instance(K,Ring) then error "CoefficientRing option expects a ring";
        a := if instance(var,Symbol) then var else if instance(var,String) then getSymbol(var) else error "Variable option expects a symbol";
        A := K[a_(n:0) .. a_(apply(0 .. n-1,i->binomial(k_i-1+d_i,k_i-1)-1))];
        local x;
        R := apply(n,i -> (x = vars(23+i); A[x_0..x_(k_i-1)]));
        P := R_0; for i from 1 to n-1 do P = P ** R_i;
        mm := apply(n,i -> flatten entries sub(gens (ideal vars R_i)^(d_i),P));
        local b;
        sum(gens A,e -> (b = last baseName e; e * product(n,i -> (mm_i)_(b_i))))
    )
);

-- Hyperdeterminants --

MultidimensionalMatrix = new Type of HashTable;

MultidimensionalMatrix.synonym = "multidimensional matrix";

MultidimensionalMatrix#{Standard,AfterPrint} = MultidimensionalMatrix#{Standard,AfterNoPrint} = M -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << dim M << "-dimensional matrix of shape " << printedShape M << " over " << ring M << endl;
);

MultidimensionalMatrix.Wrap = x -> wrap(printWidth,"-", net x);

net MultidimensionalMatrix := M -> net entries M;

multidimensionalMatrix = method(TypicalValue => MultidimensionalMatrix);

multidimensionalMatrix (List) := (L) -> (
    X := makeRing L;
    n := apply(X,i -> #i),
    F := makeMultilinearForm(L,X);
    L' := makeHyperrectangularArray(F,X);
    new MultidimensionalMatrix from {
        symbol cache => new CacheTable,
        "shape" => n,
        "entries" => L',
        "multilinearForm" => F,
        "varsMultilinearForm" => X,
        "ring" => coefficientRing ring F  
    }
);

multidimensionalMatrix (RingElement) := (F) -> (
    R := ring F;
    if not isPolynomialRing R then error "expected a polynomial";
    x := gens R;
    h := # unique apply(x,degree);
    X := apply(entries diagonalMatrix toList(h:1),d -> select(x,u -> take(degree u,h) == d));
    if flatten X =!= x then error "expected a polynomial ring with the (Z^n)-grading where the degree of each variable is a standard basis vector";
    if take(degree F,h) =!= toList(h : 1) then error "expected a polynomial of multidegree (1,1,...,1)";
    if not all(terms F,m -> unique take(degree m,h) == {1}) then error "expected a multilinear form";
    K := coefficientRing R;
    n := apply(X,i -> #i); 
    X' := gensRing(K,n);
    R' := ring first first X';
    F' := sub(F,vars R');
    L := makeHyperrectangularArray(F',X');
    new MultidimensionalMatrix from {
        symbol cache => new CacheTable,
        "shape" => n,
        "entries" => L,
        "multilinearForm" => F',
        "varsMultilinearForm" => X',
        "ring" => K  
    }
);

multidimensionalMatrix (Matrix) := (M) -> multidimensionalMatrix entries M;

matrix (MultidimensionalMatrix) := o -> (M) -> (if dim M != 2 then error "expected a two-dimensional matrix"; matrix entries M);

shape = method();
shape (MultidimensionalMatrix) := (M) -> M#"shape";

printedShape = method();
printedShape (MultidimensionalMatrix) := (M) -> (    
    n := shape M;
    concatenate(for i to #n-2 list (toString n_i)|" x ")|(toString last n)
);

dim (MultidimensionalMatrix) := (M) -> # shape M;

entries (MultidimensionalMatrix) := (M) -> M#"entries";

MultidimensionalMatrix _ Sequence := (M,l) -> (
    if #l != dim M then error("expected a sequence of length "|toString(dim M));
    m := entries M;
    for i in l do m = m_i;
    m
);

ring (MultidimensionalMatrix) := (M) -> M#"ring";

sub (MultidimensionalMatrix,Ring) := (M,R) -> applyToEntries(M,a -> sub(a,R));

RingElement * MultidimensionalMatrix := (a,M) -> applyToEntries(M,m -> a*m);

ZZ * MultidimensionalMatrix := (a,M) -> applyToEntries(M,m -> a*m);

QQ * MultidimensionalMatrix := (a,M) -> (
    if denominator a != 1 then if char ring M != 0 then error "it is not allowed to multiply a rational number with a multidimensional matrix over a ring of positive characteristic"; 
    applyToEntries(M,m -> a*m)
);

MultidimensionalMatrix * MultidimensionalMatrix := (A,B) -> (
    kr := last shape A;
    if kr != first shape B then error "expected two matrices of shapes k_1x...xk_r and l_1x...xl_s with k_r==l_1";
    r := dim A;
    s := dim B;
    e := l -> sum(kr,h -> A_(append(take(l,r-1),h)) * B_(prepend(h,take(l,-(s-1)))));
    shapeAB := take(shape A,r-1)|take(shape B,-(s-1));
    m := apply((# shapeAB : 0) .. toSequence apply(shapeAB,i -> i-1),e); 
    for i in reverse shapeAB do m = pack(i,m);
    multidimensionalMatrix first m
);

- MultidimensionalMatrix := (M) -> (-1) * M;

MultidimensionalMatrix - MultidimensionalMatrix := (M,N) -> (
    if shape M != shape N then error "expected matrix of the same shape";
    multidimensionalMatrix((entries M) - (entries N))
);

+ MultidimensionalMatrix := (M) -> M;

MultidimensionalMatrix + MultidimensionalMatrix := (M,N) -> (
    if shape M != shape N then error "expected matrix of the same shape";
    multidimensionalMatrix((entries M) + (entries N))
);

MultidimensionalMatrix == MultidimensionalMatrix := (M,N) -> (
    if shape M != shape N then return false;
    if ring M =!= ring N then return false;
    M#"multilinearForm" == N#"multilinearForm"
);

MultidimensionalMatrix == ZZ := (M,n) -> (
    if n != 0 then error "encountered integer other than 0 in comparison with a multidimensional matrix";
    L := entries M;
    for i from 1 to dim M -1 do L = flatten L;
    matrix{L} == 0
);

ZZ == MultidimensionalMatrix := (n,M) -> M == n;

MultidimensionalMatrix ! := (M) -> M#"multilinearForm";

permute = method();
permute (MultidimensionalMatrix,List) := (M,l) -> (
    if M.cache#?("permute",l) then return M.cache#("permute",l);
    if not all(l,i -> instance(i,ZZ)) then error "expected a list of integers";
    d := dim M;
    if sort l != toList(0 .. d-1) then error("expected a permutation of the set "|toString toList(0 .. d-1));
    X := M#"varsMultilinearForm"; 
    X' := for i to d-1 list X_(l_i);
    K := ring M;
    D := entries diagonalMatrix toList(d : 1);
    R := K[flatten X',Degrees=>apply(d,i -> #(X'_i) : D_i)];
    M.cache#("permute",l) = multidimensionalMatrix sub(M#"multilinearForm",R)
);

reverseShape = method();
reverseShape (MultidimensionalMatrix) := (M) -> permute(M,reverse toList(0 .. dim M -1));

sortShape = method();
sortShape (MultidimensionalMatrix) := (M) -> permute(M,apply(sort apply(dim M,i -> ((shape M)_i,i)),last));

makeRing = method();
makeRing (List) := (L) -> (
    if #L == 0 then error "expected a nonempty list";
    n := {#L};
    L' := first L;
    while instance(L',List) do (
        if #L' == 0 then error "expected nonempty lists";
        n = prepend(#L',n);
        L' = first L';
    );
    for i to #n-2 do L = flatten L;
    K := ring matrix{L};
    gensRing(K,reverse n)
);

gensRing = memoize(
    (K,n) -> (
        x := apply(#n,i -> getSymbol("x"|toString(i)));
        X := apply(#n,i -> toList((x_i)_0 .. (x_i)_(n_i-1)));
        d := entries diagonalMatrix toList(#n : 1);
        R := K[flatten X,Degrees=>apply(#n,i -> n_i : d_i)];
        apply(X,X0->apply(X0,u -> u_R))
    )
);

makeMultilinearForm = method();
makeMultilinearForm (List,List) := (L,X) -> (
    if not instance(first L,List) then return ((matrix{first X})*transpose(matrix{L}))_(0,0);
    X' := take(X,-(#X-1));
    return makeMultilinearForm(apply(L,l -> makeMultilinearForm(l,X')),X);
);

makeHyperrectangularArray = method();
makeHyperrectangularArray (RingElement,List) := (F,X) -> (
    K := coefficientRing ring F;
    n := #X - 1;
    R := K[X_n]; for i from 1 to n do R = R[X_(n-i)];
    coeffs := (G) -> flatten entries sub(last coefficients(matrix G,Monomials=>vars ring G),coefficientRing ring G);
    C := sub(F,R);
    for i to n do C = applyToEntries(C,coeffs);
    return C;
);

applyToEntries = method();
applyToEntries (Thing,Function) := (M,f) -> if instance(M,List) then apply(M,m -> applyToEntries(m,f)) else f(M);
applyToEntries (MultidimensionalMatrix,Function) := (M,f) -> multidimensionalMatrix applyToEntries(entries M,f);

genericMultidimensionalMatrix = method(TypicalValue => MultidimensionalMatrix, Dispatch => Thing, Options => {CoefficientRing => ZZ, Variable => "a"});
genericMultidimensionalMatrix Sequence := o -> n -> multidimensionalMatrix genericMultihomogeneousPolynomial(n,#n:1,CoefficientRing=>o.CoefficientRing,Variable=>o.Variable);
genericMultidimensionalMatrix VisibleList := o -> n -> genericMultidimensionalMatrix(toSequence n,CoefficientRing=>o.CoefficientRing,Variable=>o.Variable);

randomMultidimensionalMatrix = method(TypicalValue => MultidimensionalMatrix, Dispatch => Thing, Options => {CoefficientRing => ZZ, MaximalRank => null});
randomMultidimensionalMatrix Sequence := o -> n -> (
    R := ring first first gensRing(o.CoefficientRing,toList n);
    N := toList(#n : 1);
    if o.MaximalRank === null then return multidimensionalMatrix random(N,R);
    if not(instance(o.MaximalRank,ZZ) and o.MaximalRank > 0) then error "MaximalRank option expects a positive integer";
    sum(o.MaximalRank,i -> multidimensionalMatrix product apply(entries diagonalMatrix N,d -> random(d,R))) 
);
randomMultidimensionalMatrix VisibleList := o -> n -> randomMultidimensionalMatrix(toSequence n,CoefficientRing=>o.CoefficientRing,MaximalRank=>o.MaximalRank);

removeOneDim = method();
removeOneDim MultidimensionalMatrix := (cacheValue "removeOneDim") (M -> (
    if all(shape M,i -> i == 1) then return multidimensionalMatrix {coefficient(product gens ring M#"multilinearForm",M#"multilinearForm")};
    X := select(M#"varsMultilinearForm",e -> #e > 1);
    R := (ring M)[flatten X];
    P := sub(sub(M#"multilinearForm",apply(select(M#"varsMultilinearForm",e -> #e == 1),t -> (first t)=>1)),R);
    X = apply(X,X0->apply(X0,u -> sub(u,R)));
    multidimensionalMatrix makeHyperrectangularArray(P,X)
));

canApplySchlafli = method(Options => {Strategy => null});
canApplySchlafli (List) := o -> (n) -> (
    if #n == 1 then return (true,0);
    if #n == 2 then if n_0 == n_1 then return (true,0) else return (false,-1);
    if o.Strategy =!= "forceSchlafliMethod" and o.Strategy =!= "Dense_forceSchlafliMethod" and o.Strategy =!= "NotDense_forceSchlafliMethod" then if #n == 3 and #unique n <= 2 then (
        b := first commonest n;
        a := n_0; if a == b then (a = n_1; if a == b then a = n_2);
        if a >= 4 then return (false,-1);
    );
    k := apply(n,i -> i-1);
    if 2*max(k) > sum k then return (false,-1);
    for i to #n-1 do if first canApplySchlafli(n_(toList delete(i,0..#n-1)),Strategy=>o.Strategy) then return (true,i);
    return (false,-1);
);

schlafliMethod = method(Options => {Strategy => null});
schlafliMethod (MultidimensionalMatrix) := o -> M -> (
    if dim M <= 2 then return determinant(M,Strategy=>o.Strategy);
    (b,i0) := canApplySchlafli(shape M,Strategy=>o.Strategy);
    if not b then error "Schlafli's method cannot be applied";
    F := M#"multilinearForm";
    K := ring M;
    X := M#"varsMultilinearForm";  
    Xi0 := X_(toList delete(i0,0..(dim M -1)));
    D := entries diagonalMatrix toList(dim(M)-1 : 1);   
    G := sub(F,(K[X_i0])[flatten Xi0,Degrees=>apply(dim(M)-1,i -> #(Xi0_i) : D_i)]);
    N := multidimensionalMatrix G;
    detN := schlafliMethod(N,Strategy=>o.Strategy);
    if detN == 0 then return 0_(ring M);
    d := first degree detN;
    if d != degreeDeterminant apply(Xi0,u -> #u) then error "got wrong value for the degree of the determinant";
    s := #(X_i0)-1;
    detN' := sub(sub(detN,first gens ring detN => 1),K[take(gens ring detN,{1,s})]);
    if d != first degree detN' then error "unhomogenization failed";
    Disc := affineDiscriminant;
    if o.Strategy === "Dense" or o.Strategy === "Dense_forceSchlafliMethod" then Disc = denseDiscriminant(d,s);
    if o.Strategy === null or o.Strategy === "forceSchlafliMethod" then if numgens K > 0 then Disc = denseDiscriminant(d,s);
    Disc detN'
);

determinant (MultidimensionalMatrix) := o -> (M) -> (
    if o.Strategy =!= null and o.Strategy =!= "Dense" and o.Strategy =!= "NotDense" and o.Strategy =!= "forceSchlafliMethod" and o.Strategy =!= "Dense_forceSchlafliMethod" and o.Strategy =!= "NotDense_forceSchlafliMethod" then error "allowed strategies are \"Dense\", \"NotDense\", \"forceSchlafliMethod\", \"Dense_forceSchlafliMethod\", and \"NotDense_forceSchlafliMethod\"";
    n := shape M;
    k := apply(n,i -> i-1);
    if 2*max(k) > sum k then error("the determinant for matrices of shape "|(printedShape M)|" does not exist");
    if n == {1} then return first entries M;
    if min n == 1 then return determinant(removeOneDim M,Strategy=>o.Strategy);
    if #n == 2 then return determinant(matrix entries M,Strategy=>null);
    if #n == 3 then (
        if first canApplySchlafli(n,Strategy=>o.Strategy) then return schlafliMethod(M,Strategy=>o.Strategy);
        if sort n === {2,3,4} then return det234 M;
        if sort n === {2,4,5} then return det245 M;
    );
    if n == {2,2,2,2} then return schlafliMethod(M,Strategy=>o.Strategy);
    if o.Strategy === "forceSchlafliMethod" or o.Strategy === "Dense_forceSchlafliMethod" or o.Strategy === "NotDense_forceSchlafliMethod" then return schlafliMethod(M,Strategy=>o.Strategy);
    if 2*max(k) == sum k then return det sylvesterMatrix M;
    K := if char ring M == 0 then ZZ else ZZ/(char ring M);
    A := sparseDiscriminant(exponentsMatrix genericMultihomogeneousPolynomial(n,toList(#n:1),CoefficientRing=>K),CoefficientRing=>K);
    A (M#"multilinearForm")
);

det234 = (M) -> ( -- determinant of shape 2x3x4
    if shape M =!= {2,3,4} then if sort shape M == {2,3,4} then M = sortShape M;
    assert(shape M == {2,3,4});
    K := ring M;
    p := local p;
    ringG35 := K[p_(0,1,2,3),p_(0,1,2,4),p_(0,1,3,4),p_(0,2,3,4),p_(1,2,3,4),p_(0,1,2,5),p_(0,1,3,5),p_(0,2,3,5),p_(1,2,3,5),p_(0,1,4,5),p_(0,2,4,5),p_(1,2,4,5),p_(0,3,4,5),p_(1,3,4,5),p_(2,3,4,5)];
    W := p_(0,1,2,5)*p_(0,1,4,5)*p_(0,3,4,5)-p_(0,1,2,4)*p_(0,2,4,5)*p_(0,3,4,5)-p_(0,1,2,5)*p_(0,1,3,5)*p_(1,3,4,5)+p_(0,1,2,4)*p_(0,2,3,5)*p_(1,3,4,5)-p_(0,1,2,3)*p_(1,2,3,5)*p_(1,3,4,5)+p_(0,1,2,3)*p_(0,2,4,5)*p_(1,3,4,5)-p_(0,1,2,4)*p_(0,2,3,4)*p_(2,3,4,5)+p_(0,1,2,3)*p_(1,2,3,4)*p_(2,3,4,5)+p_(0,1,2,4)*p_(0,1,3,5)*p_(2,3,4,5)-2*p_(0,1,2,3)*p_(0,1,4,5)*p_(2,3,4,5);
    g := gens ringG35;
    N := transpose matrix flatten entries M;
    mm := apply(subsets(6,4),m -> det submatrix(N,m));
    sub(W,apply(15,j -> g_j => mm_j))
);

det245 = (M) -> ( -- determinant of shape 2x4x5
    if shape M =!= {2,4,5} then if sort shape M == {2,4,5} then M = sortShape M;
    assert(shape M == {2,4,5});
    K := ring M;
    p := local p;
    ringG47 := K[p_(0,1,2,3,4),p_(0,1,2,3,5),p_(0,1,2,4,5),p_(0,1,3,4,5),p_(0,2,3,4,5),p_(1,2,3,4,5),p_(0,1,2,3,6),p_(0,1,2,4,6),p_(0,1,3,4,6),p_(0,2,3,4,6),p_(1,2,3,4,6),p_(0,1,2,5,6),p_(0,1,3,5,6),p_(0,2,3,5,6),p_(1,2,3,5,6),p_(0,1,4,5,6),p_(0,2,4,5,6),p_(1,2,4,5,6),p_(0,3,4,5,6),p_(1,3,4,5,6),p_(2,3,4,5,6),p_(0,1,2,3,7),p_(0,1,2,4,7),p_(0,1,3,4,7),p_(0,2,3,4,7),p_(1,2,3,4,7),p_(0,1,2,5,7),p_(0,1,3,5,7),p_(0,2,3,5,7),p_(1,2,3,5,7),p_(0,1,4,5,7),p_(0,2,4,5,7),p_(1,2,4,5,7),p_(0,3,4,5,7),p_(1,3,4,5,7),p_(2,3,4,5,7),p_(0,1,2,6,7),p_(0,1,3,6,7),p_(0,2,3,6,7),p_(1,2,3,6,7),p_(0,1,4,6,7),p_(0,2,4,6,7),p_(1,2,4,6,7),p_(0,3,4,6,7),p_(1,3,4,6,7),p_(2,3,4,6,7),p_(0,1,5,6,7),p_(0,2,5,6,7),p_(1,2,5,6,7),p_(0,3,5,6,7),p_(1,3,5,6,7),p_(2,3,5,6,7),p_(0,4,5,6,7),p_(1,4,5,6,7),p_(2,4,5,6,7),p_(3,4,5,6,7)];
    -- code: W = dualizedChowForm map parametrize rationalMap map(QQ[x_0,x_1,y_0,y_1,y_2,y_3,Degrees=>{2:{1,0},4:{0,1}}],QQ[p_(0,0),p_(0,1),p_(0,2),p_(0,3),p_(1,0),p_(1,1),p_(1,2),p_(1,3)],{x_0*y_0,x_0*y_1,x_0*y_2,x_0*y_3,x_1*y_0,x_1*y_1,x_1*y_2,x_1*y_3});
    W := p_(0,1,2,3,7)*p_(0,1,2,6,7)*p_(0,1,5,6,7)*p_(0,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,3,6,7)*p_(0,1,5,6,7)*p_(0,4,5,6,7)-p_(0,1,2,3,7)*p_(0,1,2,5,7)*p_(0,2,5,6,7)*p_(0,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,3,5,7)*p_(0,2,5,6,7)*p_(0,4,5,6,7)-p_(0,1,2,3,5)*p_(0,2,3,5,7)*p_(0,2,5,6,7)*p_(0,4,5,6,7)+p_(0,1,2,3,5)*p_(0,1,3,6,7)*p_(0,2,5,6,7)*p_(0,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,3,5,6)*p_(0,3,5,6,7)*p_(0,4,5,6,7)+p_(0,1,2,3,5)*p_(0,2,3,5,6)*p_(0,3,5,6,7)*p_(0,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,2,5,7)*p_(0,3,5,6,7)*p_(0,4,5,6,7)-2*p_(0,1,2,3,5)*p_(0,1,2,6,7)*p_(0,3,5,6,7)*p_(0,4,5,6,7)-p_(0,1,2,3,7)*p_(0,1,2,6,7)*p_(0,1,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,3,6,7)*p_(0,1,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,7)*p_(0,1,2,5,7)*p_(0,2,4,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,3,5,7)*p_(0,2,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,5)*p_(0,2,3,5,7)*p_(0,2,4,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,5)*p_(0,1,3,6,7)*p_(0,2,4,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,7)*p_(0,1,2,4,7)*p_(1,2,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,3,4,7)*p_(1,2,4,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,5)*p_(0,2,3,4,7)*p_(1,2,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,4)*p_(1,2,3,4,7)*p_(1,2,4,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,4)*p_(0,2,3,5,7)*p_(1,2,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,4)*p_(0,1,3,6,7)*p_(1,2,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,3,5,6)*p_(0,3,4,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,5)*p_(0,2,3,5,6)*p_(0,3,4,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,2,5,7)*p_(0,3,4,6,7)*p_(1,4,5,6,7)+2*p_(0,1,2,3,5)*p_(0,1,2,6,7)*p_(0,3,4,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,3,4,6)*p_(1,3,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,5)*p_(0,2,3,4,6)*p_(1,3,4,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,4)*p_(1,2,3,4,6)*p_(1,3,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,4)*p_(0,2,3,5,6)*p_(1,3,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,2,4,7)*p_(1,3,4,6,7)*p_(1,4,5,6,7)-2*p_(0,1,2,3,4)*p_(0,1,2,6,7)*p_(1,3,4,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,7)*p_(0,1,2,4,7)*p_(0,2,5,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,3,4,7)*p_(0,2,5,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,5)*p_(0,2,3,4,7)*p_(0,2,5,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,4)*p_(0,2,3,5,7)*p_(0,2,5,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,4)*p_(0,1,3,6,7)*p_(0,2,5,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,4)*p_(0,2,3,4,7)*p_(1,2,5,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,3,4,6)*p_(0,3,5,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,5)*p_(0,2,3,4,6)*p_(0,3,5,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,4)*p_(0,2,3,5,6)*p_(0,3,5,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,2,4,7)*p_(0,3,5,6,7)*p_(1,4,5,6,7)+2*p_(0,1,2,3,4)*p_(0,1,2,6,7)*p_(0,3,5,6,7)*p_(1,4,5,6,7)+p_(0,1,2,3,4)*p_(0,2,3,4,6)*p_(1,3,5,6,7)*p_(1,4,5,6,7)-p_(0,1,2,3,7)*p_(0,1,2,5,7)*p_(0,2,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,3,5,7)*p_(0,2,4,5,7)*p_(2,4,5,6,7)-p_(0,1,2,3,5)*p_(0,2,3,5,7)*p_(0,2,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,7)*p_(0,1,2,4,7)*p_(1,2,4,5,7)*p_(2,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,3,4,7)*p_(1,2,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,5)*p_(0,2,3,4,7)*p_(1,2,4,5,7)*p_(2,4,5,6,7)-p_(0,1,2,3,4)*p_(1,2,3,4,7)*p_(1,2,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,4)*p_(0,2,3,5,7)*p_(1,2,4,5,7)*p_(2,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,3,5,6)*p_(0,3,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,5)*p_(0,2,3,5,6)*p_(0,3,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,2,5,7)*p_(0,3,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,3,4,6)*p_(1,3,4,5,7)*p_(2,4,5,6,7)-p_(0,1,2,3,5)*p_(0,2,3,4,6)*p_(1,3,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,4)*p_(1,2,3,4,6)*p_(1,3,4,5,7)*p_(2,4,5,6,7)-p_(0,1,2,3,4)*p_(0,2,3,5,6)*p_(1,3,4,5,7)*p_(2,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,2,4,7)*p_(1,3,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,5)*p_(0,2,3,4,5)*p_(2,3,4,5,7)*p_(2,4,5,6,7)-p_(0,1,2,3,4)*p_(1,2,3,4,5)*p_(2,3,4,5,7)*p_(2,4,5,6,7)-p_(0,1,2,3,5)*p_(0,1,3,4,6)*p_(2,3,4,5,7)*p_(2,4,5,6,7)+2*p_(0,1,2,3,4)*p_(0,1,3,5,6)*p_(2,3,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,5)*p_(0,1,2,4,7)*p_(2,3,4,5,7)*p_(2,4,5,6,7)-2*p_(0,1,2,3,4)*p_(0,1,2,5,7)*p_(2,3,4,5,7)*p_(2,4,5,6,7)+p_(0,1,2,3,7)*p_(0,1,2,5,7)*p_(0,1,4,6,7)*p_(2,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,3,5,7)*p_(0,1,4,6,7)*p_(2,4,5,6,7)+p_(0,1,2,3,5)*p_(0,1,3,5,7)*p_(0,2,4,6,7)*p_(2,4,5,6,7)-p_(0,1,2,3,4)*p_(0,1,3,5,7)*p_(1,2,4,6,7)*p_(2,4,5,6,7)-2*p_(0,1,2,3,5)*p_(0,1,2,5,7)*p_(0,3,4,6,7)*p_(2,4,5,6,7)-p_(0,1,2,3,5)*p_(0,2,3,4,5)*p_(1,3,4,6,7)*p_(2,4,5,6,7)+p_(0,1,2,3,4)*p_(1,2,3,4,5)*p_(1,3,4,6,7)*p_(2,4,5,6,7)+p_(0,1,2,3,5)*p_(0,1,3,4,6)*p_(1,3,4,6,7)*p_(2,4,5,6,7)-2*p_(0,1,2,3,4)*p_(0,1,3,5,6)*p_(1,3,4,6,7)*p_(2,4,5,6,7)-p_(0,1,2,3,5)*p_(0,1,2,4,7)*p_(1,3,4,6,7)*p_(2,4,5,6,7)+4*p_(0,1,2,3,4)*p_(0,1,2,5,7)*p_(1,3,4,6,7)*p_(2,4,5,6,7)-2*p_(0,1,2,3,7)*p_(0,1,2,4,7)*p_(0,1,5,6,7)*p_(2,4,5,6,7)+2*p_(0,1,2,3,6)*p_(0,1,3,4,7)*p_(0,1,5,6,7)*p_(2,4,5,6,7)+p_(0,1,2,3,4)*p_(0,1,3,6,7)*p_(0,1,5,6,7)*p_(2,4,5,6,7)-2*p_(0,1,2,3,5)*p_(0,1,3,4,7)*p_(0,2,5,6,7)*p_(2,4,5,6,7)-p_(0,1,2,3,4)*p_(0,1,3,5,7)*p_(0,2,5,6,7)*p_(2,4,5,6,7)+2*p_(0,1,2,3,4)*p_(0,1,3,4,7)*p_(1,2,5,6,7)*p_(2,4,5,6,7)+p_(0,1,2,3,5)*p_(0,2,3,4,5)*p_(0,3,5,6,7)*p_(2,4,5,6,7)-p_(0,1,2,3,5)*p_(0,1,3,4,6)*p_(0,3,5,6,7)*p_(2,4,5,6,7)+2*p_(0,1,2,3,4)*p_(0,1,3,5,6)*p_(0,3,5,6,7)*p_(2,4,5,6,7)+4*p_(0,1,2,3,5)*p_(0,1,2,4,7)*p_(0,3,5,6,7)*p_(2,4,5,6,7)-p_(0,1,2,3,4)*p_(0,1,2,5,7)*p_(0,3,5,6,7)*p_(2,4,5,6,7)-p_(0,1,2,3,4)*p_(0,2,3,4,5)*p_(1,3,5,6,7)*p_(2,4,5,6,7)-p_(0,1,2,3,4)*p_(0,1,3,4,6)*p_(1,3,5,6,7)*p_(2,4,5,6,7)-2*p_(0,1,2,3,4)*p_(0,1,2,4,7)*p_(1,3,5,6,7)*p_(2,4,5,6,7)+p_(0,1,2,3,4)*p_(0,1,3,4,5)*p_(2,3,5,6,7)*p_(2,4,5,6,7)-3*p_(0,1,2,3,5)*p_(0,1,2,3,7)*p_(0,4,5,6,7)*p_(2,4,5,6,7)+3*p_(0,1,2,3,4)*p_(0,1,2,3,7)*p_(1,4,5,6,7)*p_(2,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,3,5,6)*p_(0,3,4,5,6)*p_(3,4,5,6,7)-p_(0,1,2,3,5)*p_(0,2,3,5,6)*p_(0,3,4,5,6)*p_(3,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,3,4,6)*p_(1,3,4,5,6)*p_(3,4,5,6,7)+p_(0,1,2,3,5)*p_(0,2,3,4,6)*p_(1,3,4,5,6)*p_(3,4,5,6,7)-p_(0,1,2,3,4)*p_(1,2,3,4,6)*p_(1,3,4,5,6)*p_(3,4,5,6,7)+p_(0,1,2,3,4)*p_(0,2,3,5,6)*p_(1,3,4,5,6)*p_(3,4,5,6,7)-p_(0,1,2,3,5)*p_(0,2,3,4,5)*p_(2,3,4,5,6)*p_(3,4,5,6,7)+p_(0,1,2,3,4)*p_(1,2,3,4,5)*p_(2,3,4,5,6)*p_(3,4,5,6,7)+p_(0,1,2,3,5)*p_(0,1,3,4,6)*p_(2,3,4,5,6)*p_(3,4,5,6,7)-2*p_(0,1,2,3,4)*p_(0,1,3,5,6)*p_(2,3,4,5,6)*p_(3,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,3,5,6)*p_(0,2,4,5,7)*p_(3,4,5,6,7)+p_(0,1,2,3,5)*p_(0,2,3,5,6)*p_(0,2,4,5,7)*p_(3,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,2,5,7)*p_(0,2,4,5,7)*p_(3,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,3,4,6)*p_(1,2,4,5,7)*p_(3,4,5,6,7)-p_(0,1,2,3,5)*p_(0,2,3,4,6)*p_(1,2,4,5,7)*p_(3,4,5,6,7)+p_(0,1,2,3,4)*p_(1,2,3,4,6)*p_(1,2,4,5,7)*p_(3,4,5,6,7)-p_(0,1,2,3,4)*p_(0,2,3,5,6)*p_(1,2,4,5,7)*p_(3,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,2,4,7)*p_(1,2,4,5,7)*p_(3,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,2,5,6)*p_(0,3,4,5,7)*p_(3,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,2,4,6)*p_(1,3,4,5,7)*p_(3,4,5,6,7)-p_(0,1,2,3,5)*p_(0,1,2,4,6)*p_(2,3,4,5,7)*p_(3,4,5,6,7)+2*p_(0,1,2,3,4)*p_(0,1,2,5,6)*p_(2,3,4,5,7)*p_(3,4,5,6,7)+p_(0,1,2,3,6)*p_(0,1,3,5,6)*p_(0,1,4,6,7)*p_(3,4,5,6,7)-p_(0,1,2,3,6)*p_(0,1,2,5,7)*p_(0,1,4,6,7)*p_(3,4,5,6,7)+p_(0,1,2,3,5)*p_(0,1,2,6,7)*p_(0,1,4,6,7)*p_(3,4,5,6,7)-p_(0,1,2,3,5)*p_(0,1,3,5,6)*p_(0,2,4,6,7)*p_(3,4,5,6,7)-p_(0,1,2,3,5)*p_(0,1,2,5,7)*p_(0,2,4,6,7)*p_(3,4,5,6,7)+2*p_(0,1,2,3,5)*p_(0,2,3,4,5)*p_(1,2,4,6,7)*p_(3,4,5,6,7)-2*p_(0,1,2,3,4)*p_(1,2,3,4,5)*p_(1,2,4,6,7)*p_(3,4,5,6,7)-2*p_(0,1,2,3,5)*p_(0,1,3,4,6)*p_(1,2,4,6,7)*p_(3,4,5,6,7)+4*p_(0,1,2,3,4)*p_(0,1,3,5,6)*p_(1,2,4,6,7)*p_(3,4,5,6,7)+2*p_(0,1,2,3,5)*p_(0,1,2,4,7)*p_(1,2,4,6,7)*p_(3,4,5,6,7)-2*p_(0,1,2,3,4)*p_(0,1,2,5,7)*p_(1,2,4,6,7)*p_(3,4,5,6,7)+2*p_(0,1,2,3,5)*p_(0,1,2,5,6)*p_(0,3,4,6,7)*p_(3,4,5,6,7)-p_(0,1,2,3,5)*p_(0,1,2,4,6)*p_(1,3,4,6,7)*p_(3,4,5,6,7)-p_(0,1,2,3,4)*p_(0,1,2,5,6)*p_(1,3,4,6,7)*p_(3,4,5,6,7)+p_(0,1,2,3,5)*p_(0,1,2,4,5)*p_(2,3,4,6,7)*p_(3,4,5,6,7)-2*p_(0,1,2,3,6)*p_(0,1,3,4,6)*p_(0,1,5,6,7)*p_(3,4,5,6,7)+2*p_(0,1,2,3,6)*p_(0,1,2,4,7)*p_(0,1,5,6,7)*p_(3,4,5,6,7)-3*p_(0,1,2,3,4)*p_(0,1,2,6,7)*p_(0,1,5,6,7)*p_(3,4,5,6,7)-2*p_(0,1,2,3,5)*p_(0,2,3,4,5)*p_(0,2,5,6,7)*p_(3,4,5,6,7)+4*p_(0,1,2,3,5)*p_(0,1,3,4,6)*p_(0,2,5,6,7)*p_(3,4,5,6,7)-2*p_(0,1,2,3,4)*p_(0,1,3,5,6)*p_(0,2,5,6,7)*p_(3,4,5,6,7)-p_(0,1,2,3,5)*p_(0,1,2,4,7)*p_(0,2,5,6,7)*p_(3,4,5,6,7)+3*p_(0,1,2,3,4)*p_(0,1,2,5,7)*p_(0,2,5,6,7)*p_(3,4,5,6,7)+2*p_(0,1,2,3,4)*p_(0,2,3,4,5)*p_(1,2,5,6,7)*p_(3,4,5,6,7)-p_(0,1,2,3,4)*p_(0,1,3,4,6)*p_(1,2,5,6,7)*p_(3,4,5,6,7)-2*p_(0,1,2,3,4)*p_(0,1,2,4,7)*p_(1,2,5,6,7)*p_(3,4,5,6,7)-2*p_(0,1,2,3,5)*p_(0,1,2,4,6)*p_(0,3,5,6,7)*p_(3,4,5,6,7)-2*p_(0,1,2,3,4)*p_(0,1,2,5,6)*p_(0,3,5,6,7)*p_(3,4,5,6,7)+3*p_(0,1,2,3,4)*p_(0,1,2,4,6)*p_(1,3,5,6,7)*p_(3,4,5,6,7)-3*p_(0,1,2,3,4)*p_(0,1,2,4,5)*p_(2,3,5,6,7)*p_(3,4,5,6,7)+3*p_(0,1,2,3,5)*p_(0,1,2,3,6)*p_(0,4,5,6,7)*p_(3,4,5,6,7)-3*p_(0,1,2,3,4)*p_(0,1,2,3,6)*p_(1,4,5,6,7)*p_(3,4,5,6,7)+3*p_(0,1,2,3,4)*p_(0,1,2,3,5)*p_(2,4,5,6,7)*p_(3,4,5,6,7); 
    g := gens ringG47;
    N := transpose matrix flatten entries M;
    mm := apply(subsets(8,5),m -> det submatrix(N,m));
    sub(W,apply(56,j -> g_j => mm_j))
);

sylvesterMatrix = method(TypicalValue => Matrix);
sylvesterMatrix (MultidimensionalMatrix) := (M) -> (
    -- see p. 459 in [Gelfand-Kapranov-Zelevinsky]
    n := shape M;
    k := apply(n,i -> i-1);
    r := dim M -1;
    i0 := maxPosition k;
    if 2*k_i0 != sum k then error "expected a multidimensional matrix of boundary shape";
    K := ring M;
    F := M#"multilinearForm";
    X := M#"varsMultilinearForm";
    R := (K[flatten(take(X,i0) | take(X,-(r-i0)))])[X_i0];
    f := flatten entries sub(last coefficients(sub(F,R),Monomials=>vars R),coefficientRing R);
    x := apply(take(X,i0) | take(X,-(r-i0)),u -> flatten entries sub(matrix{u},coefficientRing R));
    k' := take(k,i0) | take(k,-(r-i0));
    m := for j from 1 to r list sum take(k',j-1);
    S := flatten entries gens product(r,i -> (ideal x_i)^(m_i));
    T := gens product(r,i -> (ideal x_i)^(m_i+1));
    D := matrix flatten for i to k_i0 list apply(S,s -> flatten entries sub(last coefficients(s*f_i,Monomials=>T),K));
    if not(numRows D == numColumns D and numRows D == lift((k_i0+1)! / product(k',i -> i!),ZZ)) then error "something went wrong";
    D
);

degreeDeterminant = method();
degreeDeterminant (List) := (L) -> (
    if not all(L,i -> instance(i,ZZ)) then error "expected a list of integers";
    if min L <= 0 then error "expected a list of positive integers";
    L = apply(L,i -> i-1);
    n := #L;
    i0 := maxPosition L;
    L' := take(L,i0) | take(L,-(n-1-i0));
    if L_i0 == sum L' then return lift((L_i0 + 1)! / product(L',i -> i!),ZZ);
    x := local x;
    R := ZZ[x_1..x_n];
    X := local X;
    RX := R[X];
    P := product apply(gens R,u -> X-u);
    S := 1 - sum(1..n,i -> (i-1) * (-1)^i * coefficient(X^(n-i),P));
    m := product(1..n,i -> x_i^(L_(i-1)));
    R' := R/ideal(apply(1..n,i -> x_i^(1+L_(i-1))));
    coefficient(m,(sub(S,R'))^(-2))
);

symmetricMultidimensionalMatrix = method(TypicalValue => MultidimensionalMatrix);
symmetricMultidimensionalMatrix (RingElement) := (F) -> (
    if not(isPolynomialRing ring F and isHomogeneous F) then error "expected a homogeneous polynomial";
    K := coefficientRing ring F;
    y := local y;
    n := numgens ring F -1;
    S := K[y_0..y_n];
    F' := sub(F,vars S);
    d := first degree F';
    x := local x; 
    R := K[x_(0,0) .. x_(d-1,n)];
    j := map(S,R,apply(gens R,u -> y_(last last baseName u)));
    X := entries transpose genericMatrix(R,n+1,d);
    mm := flatten entries gens product apply(X,ideal);
    G := sum(mm,m -> m * coefficient(j m,F'));
    multidimensionalMatrix makeHyperrectangularArray(G,X)
);

isSymmetric = method(TypicalValue => Boolean);
isSymmetric (MultidimensionalMatrix) := (M) -> (
    for s in permutations(#(M#"shape")) do (
        -- <<"-- testing permutation "<<s<<endl;
        if not(M == permute(M,s)) then return false; 
    );
    return true;
);

genericSymmetricMultidimensionalMatrix = method(TypicalValue => MultidimensionalMatrix, Options => {CoefficientRing => QQ, Variable => "a"});
genericSymmetricMultidimensionalMatrix (ZZ,ZZ) := o -> (d,n) -> (
    if not(d > 0 and n > 0) then error "expected two positive integers";
    K := Grass(0,binomial(d+n-1,n-1)-1,o.CoefficientRing,Variable=>o.Variable);
    X := local X;
    R := K[X_0 .. X_(n-1)];
    F := ((vars K) * transpose gens (ideal vars R)^d)_(0,0);
    symmetricMultidimensionalMatrix F
);

sgn = (L) -> (n := sub(product(subsets(0..#L-1,2),I->(L_(I_1)-L_(I_0))/(I_1-I_0)),ZZ); if n > 0 then 1 else if n < 0 then -1 else 0);

skewSymmetricMultidimensionalMatrix = method(TypicalValue => MultidimensionalMatrix);
skewSymmetricMultidimensionalMatrix (RingElement) := (F) -> (
    if not(isPolynomialRing ring F and isHomogeneous F) then error "expected a homogeneous polynomial";
    K := coefficientRing ring F;
    y := local y;
    n := numgens ring F -1;
    S := K[y_0..y_n];
    F' := sub(F,vars S);
    d := first degree F';
    x := local x; 
    R := K[x_(0,0) .. x_(d-1,n)];
    j := map(S,R,apply(gens R,u -> y_(last last baseName u)));
    X := entries transpose genericMatrix(R,n+1,d);
    mm := flatten entries gens product apply(X,ideal);
    G := sum(mm,m -> (sgn apply(toList factor m,u -> last last baseName u#0)) * m * coefficient(j m,F'));
    multidimensionalMatrix makeHyperrectangularArray(G,X)
);

isSkewSymmetric = method(TypicalValue => Boolean);
isSkewSymmetric (MultidimensionalMatrix) := (M) -> (
    for s in permutations(#(M#"shape")) do (
        -- <<"-- testing permutation "<<s<<", sign: "<<sgn s<<endl;
        if not((sgn s) * M == permute(M,s)) then return false; 
    );
    return true;
);

genericSkewMultidimensionalMatrix = method(TypicalValue => MultidimensionalMatrix, Options => {CoefficientRing => QQ, Variable => "a"});
genericSkewMultidimensionalMatrix (ZZ,ZZ) := o -> (d,n) -> (
    if not(d > 0 and n > 0 and d <= n) then error "expected two positive integers d,n with d <= n";
    K := Grass(0,binomial(n,d)-1,o.CoefficientRing,Variable=>o.Variable);
    X := local X;
    R := K[X_0 .. X_(n-1)];
    F := ((vars K) * transpose matrix{apply(subsets(set gens R,d),u -> product toList u)})_(0,0);
    skewSymmetricMultidimensionalMatrix F
);

-- end Hyperdeterminants --

flattening = method(TypicalValue => Matrix);
flattening (List,MultidimensionalMatrix) := (n,A) -> (
    if A.cache#?("flattening",n) then return A.cache#("flattening",n);
    if #n > 0 and not(all(n,i -> instance(i,ZZ)) and min n >= 0 and max n <= dim A -1 and # unique n == #n) 
    then error("expected a subset of {0,1,...,n-1}, where n = "|toString(dim A)|" is the dimension of the matrix");
    n = sort n;
    if A.cache#?("flattening",n) then return A.cache#("flattening",n);
    m := select(dim A,i -> not member(i,n));
    K := ring A;
    x := A#"varsMultilinearForm";
    N := if #n == 0 then matrix 1_K else gens product apply(x_n,ideal);
    M := if #m == 0 then matrix 1_K else gens product apply(x_m,ideal);
    mons := flatten((transpose M) * N);
    L := gens product apply(gensRing(K,{product (shape A)_n,product (shape A)_m}),ideal);
    F := A#"multilinearForm";
    A.cache#("flattening",n) = matrix multidimensionalMatrix (L * sub(last coefficients(F,Monomials=>mons),K))_(0,0)
);
flattening (ZZ,MultidimensionalMatrix) := (n,A) -> flattening({n},A);

flattenings = method();
flattenings MultidimensionalMatrix := A -> (
    n := toList(0 .. dim A -1);
    S := apply(select(subsets(subsets n,2),s -> # first s > 0 and # last s > 0 and sort flatten s == n),first);
    apply(S,s -> flattening(s,A))
);

rank MultidimensionalMatrix := A -> (
    if min shape A == 1 then return rank removeOneDim A;
    if A == 0 then return 0;
    if all(dim A,i -> rank flattening(i,A) <= 1) then return 1;
    max apply(flattenings A,rank)
);

beginDocumentation() 

document { 
    Key => SparseResultants, 
    Headline => "computations with sparse resultants", 
    PARA{"This package provides the methods ",TO "sparseResultant"," and ",TO "sparseDiscriminant"," to calculate sparse resultants and sparse discriminants. As an application, the method ",TO (determinant,MultidimensionalMatrix)," is also provided. See also the package ",TO Resultants,", which includes methods to calculate dense resultants and dense discriminants."},
    PARA{"For the definitions, see one of the following books:"}, 
    PARA{"(1) ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky;"},
    PARA{"(2) ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"},", by David A. Cox, John Little, Donal O'shea."},
    Caveat => {"Currently, most of the algorithms implemented are based on elimination via Gröbner basis methods. This should be significantly improved by implementing more specialized algorithms (see e.g. [",HREF{"http://link.springer.com/book/10.1007%2Fb138611","2"},", Chapter 7,Section 6])."}
}

document { 
    Key => {sparseResultant,(sparseResultant,Thing),[sparseResultant,Unmixed],[sparseResultant,CoefficientRing]}, 
    Headline => "sparse resultant (A-resultant)", 
    Usage => "sparseResultant A", 
    Inputs => {"A" => Sequence => {"or a ",TO2 {List,"list"}," of ",TEX///$n+1$///," matrices ",TEX///$A_0,\ldots,A_n$///," over ",TEX///$\mathbb{Z}$///," and with ",TEX///$n$///," rows to represent the exponent vectors of Laurent polynomials ",TEX///$f_0,\ldots,f_n$///," in ", TEX///$n$///," variables, ",TEX///$f_i = \sum_{\omega\in A_i} a_{i,\omega} x^{\omega}$///,". In the unmixed case, that is when ",TEX///$A_0=\cdots=A_n$///,", it is enough to pass just one of the matrices. Moreover, if one wants to perform computations only in characteristic ",TEX///$p>0$///,", then it is recommended to use the option ",TT "CoefficientRing=>ZZ/p","."}}, 
    Outputs => {SparseResultant => {"the universal sparse resultant associated to ",TEX///$A$///,", that is, a function that to a ", TO2 {Sequence,"sequence"}," (or a ",TO2 {List,"list"},", or a row ", TO2 {Matrix,"matrix"},") of ", TEX///$n+1$///," Laurent polynomials ", TEX///$f_0,\ldots,f_n$///," in ", TEX///$n$///," variables over a coefficient ring ",TEX///$K$///,", involving only monomials from ",TEX///$A$///,", associates the sparse resultant of the polynomials ",TEX///$f_0,\ldots,f_n$///,", which is an element of ",TEX///$K$///,"."}}, 
    PARA {"Alternatively, one can apply the method directly to the list of Laurent polynomials ",TEX///$f_0,\ldots,f_n$///,". In this case, the matrices ",TEX///$A_0,\ldots,A_n$///, " are automatically determined by ",TO "exponentsMatrix",". If you want require that ",TEX///$A_0=\cdots=A_n$///,", then use the option ",TT "Unmixed=>true"," (this could be faster). Below we consider some examples."},
    PARA {"In the first example, we calculate the sparse (mixed) resultant associated to the three sets of monomials ",TEX///$(1,x y,x^2 y,x),(y,x^2 y^2,x^2 y,x),(1,y,x y,x)$///,". Then we evaluate it at the three polynomials ",TEX///$f = c_{(1,1)}+c_{(1,2)} x y+c_{(1,3)} x^2 y+c_{(1,4)} x, g = c_{(2,1)} y+c_{(2,2)} x^2 y^2+c_{(2,3)} x^2 y+c_{(2,4)} x, h = c_{(3,1)}+c_{(3,2)} y+c_{(3,3)} x y+c_{(3,4)} x$///,"."},
    EXAMPLE {
        "time Res = sparseResultant(matrix{{0,1,1,2},{0,0,1,1}},matrix{{0,1,2,2},{1,0,1,2}},matrix{{0,0,1,1},{0,1,0,1}})",
        "QQ[c_(1,1)..c_(3,4)][x,y];",
        "(f,g,h) = (c_(1,1)+c_(1,2)*x*y+c_(1,3)*x^2*y+c_(1,4)*x, c_(2,1)*y+c_(2,2)*x^2*y^2+c_(2,3)*x^2*y+c_(2,4)*x, c_(3,1)+c_(3,2)*y+c_(3,3)*x*y+c_(3,4)*x)",
        "time Res(f,g,h)",
        "assert(Res(f,g,h) == sparseResultant(f,g,h))"
    },
    PARA {"In the second example, we calculate the sparse unmixed resultant associated to the set of monomials ",TEX///$(1,x,y,xy)$///,". Then we evaluate it at the three polynomials ",TEX///$f = a_0 + a_1 x + a_2 y + a_3 x y, g = b_0 + b_1 x + b_2 y + b_3 x y, h = c_0 + c_1 x + c_2 y + c_3 x y$///,". Moreover, we perform all the computation over ",TEX///$\mathbb{Z}/3331$///,"."},    
    EXAMPLE {
        "time Res = sparseResultant(matrix{{0,0,1,1},{0,1,0,1}},CoefficientRing=>ZZ/3331);",
        "ZZ/3331[a_0..a_3,b_0..b_3,c_0..c_3][x,y];",
        "(f,g,h) = (a_0 + a_1*x + a_2*y + a_3*x*y, b_0 + b_1*x + b_2*y + b_3*x*y, c_0 + c_1*x + c_2*y + c_3*x*y)",
        "time Res(f,g,h)",
        "assert(Res(f,g,h) == sparseResultant(f,g,h))"
    },
    PARA {"In the third and last example, we compare the sparse mixed resultant and the sparse unmixed resultant of three generic polynomials of degrees 1,2,2 in two variables."},
    EXAMPLE {
        "(f,g,h) = genericLaurentPolynomials(1,2,2)",
        "time (MixedRes,UnmixedRes) = (sparseResultant(f,g,h),sparseResultant(f,g,h,Unmixed=>true));",
        "quotientRemainder(UnmixedRes,MixedRes)",
        "assert((denseResultant(2,2,2))(f,g,h) == UnmixedRes and (denseResultant(1,2,2))(f,g,h) == MixedRes)"
    },
    SeeAlso => {sparseDiscriminant, denseResultant, resultant, genericLaurentPolynomials} 
}

document { 
    Key => {"SparseResultant"}, 
    Headline => "the class of all sparse resultants", 
    PARA {"An object of this class is created by the method ",TO sparseResultant,", when the input is given by ",TEX///$n+1$///," integral matrices ",TEX///$A_0,\ldots,A_n$///," with ",TEX///$n$///," rows. Such an object behaves like a function that to ", TEX///$n+1$///," Laurent polynomials ", TEX///$f_0,\ldots,f_n$///," in ", TEX///$n$///," variables ",TEX///$x=(x_1,\ldots,x_n)$///,", with ",TEX///$f_i = \sum_{\omega\in \{columns\ of\ A_i\}} a_{i,\omega} x^{\omega}$///,", associates their sparse resultant ",TEX///$Res_{A_0,\ldots,A_n}(f_0,\ldots,f_n)$///,", which is a polynomial in the coefficients ",TEX///$a_{i,\omega}$///,". An error is thrown if the polynomials ",TEX///$f_i$///," do not have the correct form."}
}

document { 
    Key => {(exponents,SparseResultant)}, 
    Headline => "get the matrices of exponents", 
    Usage => "exponents R", 
    Inputs => {"R" => SparseResultant},
    Outputs => {{"the matrices of exponents from which the sparse resultant ",TT"R"," is created"}},
    EXAMPLE {
        "R = denseResultant(2,2,1);",
        "M = exponents R",
        "assert(R === sparseResultant M)"
     },
     SeeAlso => {exponentsMatrix}
}

document { 
    Key => {(char,SparseResultant)}, 
    Headline => "get the characteristic", 
    Usage => "char R", 
    Inputs => {"R" => SparseResultant},
    Outputs => {ZZ => {"the characteristic of the ring of the polynomial representing ",TT"R"}},
    EXAMPLE {
        "R = denseResultant(2,2,1,CoefficientRing=>ZZ/331);",
        "char R",
        "char denseResultant(2,2,1)"
     }
}

document { 
    Key => {(symbol SPACE, SparseResultant, Thing)}, 
    Headline => "evaluate a sparse resultant", 
    Usage => "R(f)", 
    Inputs => {"R" => SparseResultant => {"associated to ",TEX///$n+1$///," integral matrices ",TEX///$A_0,\ldots,A_n$///," with ",TEX///$n$///," rows."},
              {TEX///$n+1$///," Laurent polynomials ",TEX///$f = (f_0,\ldots,f_n)$///," in ", TEX///$n$///," variables ",TEX///$x=(x_1,\ldots,x_n)$///,", with ",TEX///$f_i = \sum_{\omega\in \{columns\ of\ A_i\}} a_{i,\omega} x^{\omega}$///,"."}},
    Outputs => {RingElement => {"the ",TEX///$(A_0,\ldots,A_n)$///,"-resultant of ",TEX///$f_0,\ldots,f_n$///,"."}},
    EXAMPLE {
        "R = denseResultant(2,3);",
        "f = genericLaurentPolynomials(2,3)",
        "R(f)"
     },
     SeeAlso => {sparseResultant}
}

undocumented {(describe, SparseResultant), (toString, SparseResultant), (net, SparseResultant), (symbol SPACE, SparseResultant, VisibleList), (symbol SPACE, SparseResultant, Matrix)}

document { 
    Key => {sparseDiscriminant,(sparseDiscriminant,Matrix),(sparseDiscriminant,RingElement),[sparseDiscriminant,CoefficientRing]}, 
    Headline => "sparse discriminant (A-discriminant)", 
    Usage => "sparseDiscriminant A", 
    Inputs => {"A" => Matrix => {"a matrix over ",TEX///$\mathbb{Z}$///," with ",TEX///$n$///," rows to represent the exponent vectors of a Laurent polynomial ",TEX///$f$///," in ", TEX///$n$///," variables, ",TEX///$f = \sum_{\omega\in A} a_{\omega} x^{\omega}$///,". If one wants to perform computations only in characteristic ",TEX///$p>0$///,", then it is recommended to use the option ",TT "CoefficientRing=>ZZ/p","."}}, 
    Outputs => {SparseDiscriminant => {"the universal sparse discriminant associated to ",TEX///$A$///,", that is, a function that to a Laurent polynomial ", TEX///$f$///," in ", TEX///$n$///," variables over a coefficient ring ",TEX///$K$///,", involving only monomials from ",TEX///$A$///,", associates the sparse discriminant of ",TEX///$f$///,", which is an element of ",TEX///$K$///,"."}}, 
    PARA {"Alternatively, one can apply the method directly to the Laurent polynomial ",TEX///$f$///,". In this case, the matrix ",TEX///$A$///," is automatically determined by ",TO "exponentsMatrix","."},
    PARA {"As an example, we now calculate the sparse discriminant of a generic trilinear form on ",TEX///$\mathbb{P}^1\times\mathbb{P}^2\times \mathbb{P}^1$///,", that is, the hyperdeterminant of a generic three-dimensional matrix of shape ",TEX///$2\times 3\times 2$///,"."},
    EXAMPLE {
        "f = genericMultihomogeneousPolynomial((2,3,2),(1,1,1))",
        "time sparseDiscriminant f",
        "A = exponentsMatrix f",
        "Disc = sparseDiscriminant A",
        "assert(Disc f == sparseDiscriminant f)"
    },
    SeeAlso => {sparseResultant, denseDiscriminant, discriminant, genericMultihomogeneousPolynomial, (determinant,MultidimensionalMatrix)}
}

document { 
    Key => {"SparseDiscriminant"}, 
    Headline => "the class of all sparse discriminants", 
    PARA {"An object of this class is created by the method ",TO sparseDiscriminant,", when the input is an integral matrix ",TEX///$A$///," with ",TEX///$n$///," rows. Such an object behaves like a function that to a Laurent polynomial ", TEX///$f$///," in ", TEX///$n$///," variables ",TEX///$x=(x_1,\ldots,x_n)$///,", with ",TEX///$f = \sum_{\omega\in \{columns\ of\ A\}} a_{\omega} x^{\omega}$///,", associates its sparse discriminant ",TEX///$Disc_{A}(f)$///,", which is a polynomial in the coefficients ",TEX///$a_{\omega}$///,". An error is thrown if the polynomial ",TEX///$f$///," does not have the correct form."}
}

document { 
    Key => {(exponents,SparseDiscriminant)}, 
    Headline => "get the matrix of exponents", 
    Usage => "exponents D", 
    Inputs => {"D" => SparseDiscriminant},
    Outputs => {{"the matrix of exponents from which the sparse discriminant ",TT"D"," is created"}},
    EXAMPLE {
        "D = denseDiscriminant(2,2);",
        "M = exponents D",
        "assert(D === sparseDiscriminant M)"
     },
     SeeAlso => {exponentsMatrix}
}

document { 
    Key => {(char,SparseDiscriminant)}, 
    Headline => "get the characteristic", 
    Usage => "char D", 
    Inputs => {"D" => SparseDiscriminant},
    Outputs => {ZZ => {"the characteristic of the ring of the polynomial representing ",TT"D"}},
    EXAMPLE {
        "D = denseDiscriminant(2,2,CoefficientRing=>ZZ/331);",
        "char D",
        "char denseDiscriminant(2,2)"
     }
}

document { 
    Key => {(symbol SPACE, SparseDiscriminant, Thing)}, 
    Headline => "evaluate a sparse discriminant", 
    Usage => "D(F)", 
    Inputs => {"D" => SparseDiscriminant => {"associated to an integral matrix ",TEX///$A$///," with ",TEX///$n$///," rows."},
              {"a Laurent polynomial ",TEX///$F = \sum_{\omega\in \{columns\ of\ A\}} a_{\omega} x^{\omega}$///," in ", TEX///$n$///," variables ",TEX///$x=(x_1,\ldots,x_n)$///,"."}},
    Outputs => {RingElement => {"the ",TEX///$A$///,"-discriminant of ",TEX///$F$///,"."}},
    EXAMPLE {
        "D = denseDiscriminant(2,2);",
        "QQ[a..f][x,y]; F = a*x^2+b*x*y+c*y^2+d*x+e*y+f",
        "D(F)"
     },
     SeeAlso => {sparseDiscriminant}
}

undocumented {(describe, SparseDiscriminant), (toString, SparseDiscriminant), (net, SparseDiscriminant)}

document { 
    Key => {exponentsMatrix,(exponentsMatrix,Sequence),[exponentsMatrix,Unmixed]}, 
    Headline => "exponents in one or more polynomials", 
    Usage => "exponentsMatrix(f_0,f_1,...)", 
    Inputs => {{TT "(f_0,f_1,...)"," one or more Laurent polynomials in the same ring."}},
    Outputs => {{TT "(A_0,A_1,...)"," a sequence of matrices such that the columns of ",TT "A_i"," are the ",TO exponents," of ",TT "f_i",". If either the option ",TT "Unmixed"," is set to true or all the matrices ",TT"A_i"," are equal, then just one matrix is returned whose columns are all the exponents occurring in at least one of the polynomials."}},
    EXAMPLE {
        "QQ[x,y,z];",
        "f = (x^2 - 7 + x*y*z^11 + y, -3*y^7*z + x^3*y + 5*x^2);",
        "exponentsMatrix(f_0)",
        "exponentsMatrix(f_0,f_1)",
        "exponentsMatrix(f_0,f_1,Unmixed=>true)"
    },
    SeeAlso => {exponents, sparseResultant, genericLaurentPolynomials}
}

undocumented {(exponentsMatrix,VisibleList),(exponentsMatrix,RingElement)}

document { 
    Key => {genericLaurentPolynomials,(genericLaurentPolynomials,Sequence),[genericLaurentPolynomials,CoefficientRing]}, 
    Headline => "generic (Laurent) polynomials", 
    Usage => "genericLaurentPolynomials A", 
    Inputs => {"A" => Sequence => {"or a ",TO2 {List,"list"}," of ",TEX///$n+1$///," matrices ",TEX///$A_0,\ldots,A_n$///," over ",TEX///$\mathbb{Z}$///," and with ",TEX///$n$///," rows to represent the exponent vectors of (Laurent) polynomials ",TEX///$f_0,\ldots,f_n$///," in ", TEX///$n$///," variables. For the dense case, one can pass just the sequence of degrees of ",TEX///$f_0,\ldots,f_n$///,"."}}, 
    Outputs => {{"the generic (Laurent) polynomials ",TEX///$f_0,\ldots,f_n$///," in the ring ",TEX///$\mathbb{Z}[a_0,a_1,\ldots,b_0,b_1,\ldots][x_1,\ldots,x_n]$///,", involving only the monomials from ",TEX///$A$///,". (Note that, if all the exponents are nonnegative, then the ambient polynomial ring is taken without inverses of variables, so that ",TEX///$f_0,\ldots,f_n$///," are ordinary polynomials.)"}},
    PARA{"This method helps to construct special types of sparse resultants, see for instance ",TO "denseResultant","."},
    EXAMPLE {
        "M = (matrix{{2,3,4,5},{0,2,1,0}},matrix{{1,-1,0,2,3},{-2,0,-7,-1,0}},matrix{{-1,0,6},{-2,1,3}})",
        "genericLaurentPolynomials M",
        "genericLaurentPolynomials (2,3,1)",
    },
    SeeAlso => {sparseResultant, exponentsMatrix}
}

undocumented {(genericLaurentPolynomials,VisibleList)}

document { 
    Key => {genericMultihomogeneousPolynomial,(genericMultihomogeneousPolynomial,VisibleList,VisibleList),[genericMultihomogeneousPolynomial,CoefficientRing],[genericMultihomogeneousPolynomial,Variable]}, 
    Headline => "generic multi-homogeneous polynomial", 
    Usage => "genericMultihomogeneousPolynomial((k_1,...,k_n),(d_1,...,d_n))", 
    Inputs => {{TT"(k_1,...,k_n)",", a sequence of positive integers to indicate ",TEX///$n$///," sets of variables ",TEX///$X_1,\ldots,X_n$///," with ",TEX///$\#(X_i) = k_i$///,"."},
               {TT"(d_1,...,d_n)",", a sequence of nonnegative integers."}},
    Outputs => {RingElement => {"the generic multi-homogeneous polynomial of multi-degree ",TEX///$(d_1,\ldots,d_n)$///," in the above sets of variables."}},
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
    Outputs => {{"for ",TT "(d_0,d_1,...)",", this is the same as ", TO sparseResultant," ",TO exponentsMatrix," ",TO genericLaurentPolynomials, TT"(d_0,d_1,...)",";"},
                {"for ",TT "(f_0,f_1,...)",", this is the same as ", TO affineResultant,TT"{f_0,f_1,...}","."}}, 
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
    Outputs => {{"for ",TT "(d,n)",", this is the same as ", TO sparseDiscriminant," ", TO exponentsMatrix, EM " \"generic polynomial of degree d in n variables\";"},
                {"for ",TT "f",", this is the same as ", TO affineDiscriminant,TT"(f)","."}}, 
    EXAMPLE {
        "(d,n) := (2,3);",
        "time Disc = denseDiscriminant(d,n)",
        "f = first genericLaurentPolynomials prepend(d,n:0)",
        "assert(Disc(f) == denseDiscriminant(f))"
    },
    SeeAlso => {sparseDiscriminant, affineDiscriminant, denseResultant, exponentsMatrix, genericLaurentPolynomials}
}

document { 
    Key => {multidimensionalMatrix,(multidimensionalMatrix,List)}, 
    Headline => "make a multidimensional matrix", 
    Usage => "multidimensionalMatrix L", 
    Inputs => {"L" => List => {"a list of nested lists representing a hyper-rectangular array of ring elements."}},
    Outputs => {MultidimensionalMatrix => {"the corresponding multidimensional matrix."}},
    EXAMPLE {
        "multidimensionalMatrix {{0, 5}, {9, 3}, {7, 2}}",
        "multidimensionalMatrix {{{1, 0}, {4, 3}}, {{3, 1}, {5, 9}}}",
        "multidimensionalMatrix {{{7/3, 8, 0}, {6, 8, 3}}, {{3, 8, 2}, {9, 2, 4}}, {{0, 2, 9}, {1, 9, 5}}, {{2, 8, 4}, {9, 7, 7}}}"
    },
    SeeAlso => {(multidimensionalMatrix,RingElement),MultidimensionalMatrix,(det,MultidimensionalMatrix),randomMultidimensionalMatrix,genericMultidimensionalMatrix} 
}

document { 
    Key => {(multidimensionalMatrix,RingElement)}, 
    Headline => "make a multidimensional matrix from a multilinear form", 
    Usage => "multidimensionalMatrix F", 
    Inputs => {"F" => RingElement => {"a homogeneous polynomial of multidegree ",TEX///$(1,\ldots,1)$///," in a polynomial ring ",TEX///$R$///," with the ",TEX///$\mathbb{Z}^n$///,"-grading where the degree of each variable is a standard basis vector; in other words, ",TEX///$R$///," is the homogeneous coordinate ring of a product of ",TEX///$n$///," projective spaces."}},
    Outputs => {MultidimensionalMatrix => {"the ",TEX///$n$///,"-dimensional matrix having as entries the coefficients of ",TEX///$F$///,"."}},
    PARA {"If ",TEX///$F$///," is a multilinear form on ",TEX///$\mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}$///,", then the shape of the corresponding matrix is ",TEX///$(k_1+1)\times(k_2+1)\times\cdots\times(k_n+1)$///,". You can use ",TO permute," to rearrange the dimensions."},
    EXAMPLE {
        "R = ZZ[x_1..x_3,y_1..y_4,z_1..z_2,Degrees=>{3:{1,0,0},4:{0,1,0},2:{0,0,1}}];",
        "F = random({1,1,1},R)",
        "M = multidimensionalMatrix F"
    },
    PARA {"The inverse operation can be obtained using the command \"",TT"!","\" as follows:"},
    EXAMPLE {
        "F' = M!",
        "assert(M === multidimensionalMatrix F')",
        "assert(sub(F',vars ring F) === F)",
    },
    SeeAlso => {(multidimensionalMatrix,List),MultidimensionalMatrix} 
}
undocumented {(symbol !,MultidimensionalMatrix)}

document { 
    Key => {MultidimensionalMatrix}, 
    Headline => "the class of all multidimensional matrices", 
    PARA{"A multidimensional matrix is a hyper-rectangular array of ring elements."}, 
    SeeAlso => {"multidimensionalMatrix"}
}

document { 
    Key => {permute,(permute,MultidimensionalMatrix,List)}, 
    Headline => "permute the dimensions of a multidimensional matrix", 
    Usage => "permute(M,s)", 
    Inputs => {"M" => MultidimensionalMatrix => {"an ",TEX///$n$///,"-dimensional matrix."},
               "s" => List => {"a permutation of the set ",TEX///$\{0,1,\ldots,n-1\}$///,"."}},
    Outputs => {MultidimensionalMatrix => {"with the same entries as the input, but with the dimensions re-ordered according to the specified permutation."}},
    PARA {"According to the definition given on page 449 of the book ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", the output of this method is ",TEX///$s'(M)$///,", where ",TEX///$s'$///," is the inverse permutation of ",TEX///$s$///,"."},
    EXAMPLE {
        "M = genericMultidimensionalMatrix {4,3,2}",
        "permute(M,{1,0,2})",
        "permute(M,{2,0,1})"
     },
     SeeAlso => {reverseShape, sortShape, transpose}
}

document { 
    Key => {reverseShape,(reverseShape,MultidimensionalMatrix)}, 
    Headline => "reverse the dimensions of a multidimensional matrix", 
    Usage => "reverseShape M", 
    Inputs => {"M" => MultidimensionalMatrix => {"an ",TEX///$n$///,"-dimensional matrix."}},
    Outputs => {MultidimensionalMatrix => {"the same as ",TO permute,TT"(M,{n-1,...,1,0})","."}},
    EXAMPLE {
        "M = genericMultidimensionalMatrix {4,3,2}",
        "reverseShape M",
        "assert(M === reverseShape reverseShape M)"
     },
     SeeAlso => {permute, sortShape, transpose}
}

document { 
    Key => {sortShape,(sortShape,MultidimensionalMatrix)}, 
    Headline => "sort the dimensions of a multidimensional matrix", 
    Usage => "sortShape M", 
    Inputs => {"M" => MultidimensionalMatrix},
    Outputs => {MultidimensionalMatrix => {"the same as ",TO permute,TT"(M,s)",", where ",TT"s"," is the sorting permutation of the shape of ",TT"M","."}},
    EXAMPLE {
        "M = genericMultidimensionalMatrix {4,1,3,2}",
        "sortShape M",
        "assert(sortShape M === permute(M,{1,3,2,0}))"
     },
     SeeAlso => {permute, reverseShape, transpose}
}

document { 
    Key => {(symbol +,MultidimensionalMatrix,MultidimensionalMatrix)}, 
    Headline => "sum of two multidimensional matrices of the same shape", 
    Usage => "M + N", 
    Inputs => {"M" => MultidimensionalMatrix,
               "N" => MultidimensionalMatrix},
    Outputs => {MultidimensionalMatrix => {"the sum ",TEX///$M+N$///}},
    EXAMPLE {
        "M = multidimensionalMatrix {{{1, 7}, {3, 6}, {1, 6}}, {{3, 5}, {4, 4}, {8, 0}}}",
        "N = multidimensionalMatrix {{{1, 7}, {6, 5}, {5, 5}}, {{4, 3}, {3, 8}, {4, 1}}}",
        "M + N"
     }
}
undocumented {(symbol +,MultidimensionalMatrix)}

document { 
    Key => {(symbol -,MultidimensionalMatrix,MultidimensionalMatrix)}, 
    Headline => "difference of two multidimensional matrices of the same shape", 
    Usage => "M - N", 
    Inputs => {"M" => MultidimensionalMatrix,
               "N" => MultidimensionalMatrix},
    Outputs => {MultidimensionalMatrix => {"the difference ",TEX///$M-N$///}},
    EXAMPLE {
        "M = multidimensionalMatrix {{{1, 7}, {3, 6}, {1, 6}}, {{3, 5}, {4, 4}, {8, 0}}}",
        "N = multidimensionalMatrix {{{1, 7}, {6, 5}, {5, 5}}, {{4, 3}, {3, 8}, {4, 1}}}",
        "M - N",
        "-M"
     }
}
undocumented {(symbol -,MultidimensionalMatrix)}

document { 
    Key => {(symbol *,RingElement,MultidimensionalMatrix)}, 
    Headline => "product of a scalar with a multidimensional matrix", 
    Usage => "e * N", 
    Inputs => {"e" => RingElement,
               "M" => MultidimensionalMatrix},
    Outputs => {MultidimensionalMatrix => {"the product ",TEX///$e*M$///}},
    EXAMPLE {
        "M = multidimensionalMatrix {{{1, 7}, {3, 6}, {1, 6}}, {{3, 5}, {4, 4}, {8, 0}}}",
        "2 * M",
        "(3/2) * M"
     },
     SeeAlso => {(symbol *,MultidimensionalMatrix,MultidimensionalMatrix)}
}
undocumented{(symbol *,ZZ,MultidimensionalMatrix),(symbol *,QQ,MultidimensionalMatrix)}

document { 
    Key => {(symbol *,MultidimensionalMatrix,MultidimensionalMatrix)}, 
    Headline => "product of multidimensional matrices", 
    Usage => "M * N", 
    Inputs => {"M" => MultidimensionalMatrix => {"a multidimensional matrix of shape ",TEX///$k_1\times\cdots\times k_r$///},
               "N" => MultidimensionalMatrix => {"a multidimensional matrix of shape ",TEX///$l_1\times\cdots\times l_s$///," with ",TEX///$k_r = l_1$///}},
    Outputs => {MultidimensionalMatrix => {"the convolution (or product) ",TEX///$M * N$///,", which is a multidimensional matrix of shape ",TEX///$k_1\times\cdots\times k_{r-1}\times l_2\times\cdots\times l_s$///}},
    EXAMPLE {
        "M = randomMultidimensionalMatrix {4,3}",
        "N = randomMultidimensionalMatrix {3,2}",
        "M * N",
        "N' = randomMultidimensionalMatrix {3,2,4}",
        "M * N'"
     },
     SeeAlso => {(symbol *,RingElement,MultidimensionalMatrix)}
}

undocumented {(net,MultidimensionalMatrix),(symbol ==,ZZ,MultidimensionalMatrix),(symbol ==,MultidimensionalMatrix,ZZ),(sub,MultidimensionalMatrix,Ring),
              (matrix,MultidimensionalMatrix),(multidimensionalMatrix,Matrix)}

document { 
    Key => {(symbol ==,MultidimensionalMatrix,MultidimensionalMatrix)}, 
    Headline => "equality of two multidimensional matrices", 
    Usage => "M == N", 
    Inputs => {"M" => MultidimensionalMatrix,
               "N" => MultidimensionalMatrix},
    Outputs => {Boolean => {"true or false, depending on whether ",TEX///$M$///," and ",TEX///$N$///, " are equal."}},
    EXAMPLE {
        "M = multidimensionalMatrix {{7, 6}, {9, 3}, {1, 7}}",
        "N = multidimensionalMatrix {{7, 6}, {9, 4}, {1, 7}}",
        "N' = multidimensionalMatrix {{7, 6}, {9, 3}, {1, 7}}",
        "M == N",
        "M == N'",
        "M - N' == 0"
     }
}

document { 
    Key => {(symbol _,MultidimensionalMatrix,Sequence)}, 
    Headline => "get entry of multidimensional matrix", 
    Usage => "M_l", 
    Inputs => {"M" => MultidimensionalMatrix => {"an ",TEX///$n$///,"-dimensional matrix."},
               "l" => Sequence => {"a sequence of ",TEX///$n$///," nonnegative integers."}},
    Outputs => {RingElement => {"the ",TEX///$l$///,"-th entry of the matrix ",TEX///$M$///," (note that indexes start from 0)."}},
    EXAMPLE {
        "M = genericMultidimensionalMatrix {2,3,2}",
        "M_(1,2,0)"
     }
}

document { 
    Key => {(determinant,MultidimensionalMatrix)},
    Headline => "hyperdeterminant of a multidimensional matrix", 
    Usage => "det M", 
    Inputs => {"M" => MultidimensionalMatrix},
    Outputs => {RingElement => {"the hyperdeterminant of ",TEX///$M$///}},
    PARA {"This is calculated using Schlafli's method where it is known to work. Use an optional input as ",TT "Strategy=>\"forceSchlafliMethod\""," to try to force this approach (but without ensuring the correctness of the calculation). For matrices of boundary shape, the calculation passes through ",TO sylvesterMatrix,". For details, see the Chapter 14 in the book ", HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},"."},
    EXAMPLE {
        "M = randomMultidimensionalMatrix(2,2,2,2)",
        "time det M",
        "M = randomMultidimensionalMatrix(2,2,2,2,5)",
        "time det M"
     },
     SeeAlso => {MultidimensionalMatrix, degreeDeterminant, sparseDiscriminant, sylvesterMatrix}
}

document { 
    Key => {degreeDeterminant,(degreeDeterminant,List)}, 
    Headline => "degree of the hyperdeterminant of a generic multidimensional matrix", 
    Usage => "degreeDeterminant n", 
    Inputs => {"n" => List => {"a list of positive integers ",TEX///$n=\{n_1,n_2,\ldots\}$///}},
    Outputs => {ZZ => {"the degree of the hyperdeterminant of a generic multidimensional matrix of shape ",TEX///$n_1\times n_2\times\ldots$///}},
    EXAMPLE {
        "n = {2,3,2}",
        "time degreeDeterminant n",
        "M = genericMultidimensionalMatrix n;",
        "time degree determinant M"
     },
     SeeAlso => {(determinant,MultidimensionalMatrix), genericMultidimensionalMatrix}
}

document { 
    Key => {genericMultidimensionalMatrix,(genericMultidimensionalMatrix,VisibleList),[genericMultidimensionalMatrix,CoefficientRing],[genericMultidimensionalMatrix,Variable]}, 
    Headline => "make a generic multidimensional matrix of variables", 
    Usage => "genericMultidimensionalMatrix(d_1,...,d_n)", 
    Inputs => {{TT"(d_1,...,d_n)",", a sequence of positive integers."}},
    Outputs => {{"the generic ",TO2{MultidimensionalMatrix,"multidimensional matrix"}," of shape ",TEX///$d_1\times\cdots\times d_n$///,"."}},
    EXAMPLE {
        "genericMultidimensionalMatrix(2,4,3)",
        "genericMultidimensionalMatrix((2,2,3),CoefficientRing=>ZZ/101)",
        "genericMultidimensionalMatrix((2,1,3),CoefficientRing=>ZZ/101,Variable=>\"b\")"
    },
    SeeAlso => {randomMultidimensionalMatrix,genericSymmetricMultidimensionalMatrix,genericSkewMultidimensionalMatrix}
}

document { 
    Key => {randomMultidimensionalMatrix,(randomMultidimensionalMatrix,VisibleList),[randomMultidimensionalMatrix,CoefficientRing],[randomMultidimensionalMatrix,MaximalRank]}, 
    Headline => "random multidimensional matrix", 
    Usage => "randomMultidimensionalMatrix(d_1,...,d_n)", 
    Inputs => {{TT"(d_1,...,d_n)",", a sequence of positive integers."}},
    Outputs => {{"a random ",TO2{MultidimensionalMatrix,"multidimensional matrix"}," of shape ",TEX///$d_1\times\cdots\times d_n$///," over the ring specified by the option ",TO2 {[randomMultidimensionalMatrix,CoefficientRing],"CoefficientRing"}," (the default ring is ",TO ZZ,")."}},
    EXAMPLE {
        "randomMultidimensionalMatrix(2,4,3)",
        "randomMultidimensionalMatrix((2,2,3),CoefficientRing=>ZZ/101)"
    },
    SeeAlso => {genericMultidimensionalMatrix}
}
undocumented{(genericMultidimensionalMatrix,Sequence),(randomMultidimensionalMatrix,Sequence)}

document { 
    Key => {(entries,MultidimensionalMatrix)}, 
    Headline => "lists the entries of a multidimensional matrix", 
    Usage => "entries M", 
    Inputs => {"M" => MultidimensionalMatrix},
    Outputs => {List => {"the hyperrectangular nested list of the entries of ",TT"M"}},
    EXAMPLE {
        "M = randomMultidimensionalMatrix {3,3}",
        "entries M",
        "M = randomMultidimensionalMatrix {2,2,3}",
        "entries M"
     }
}

document { 
    Key => {(ring,MultidimensionalMatrix)}, 
    Headline => "ring of a multidimensional matrix", 
    Usage => "ring M", 
    Inputs => {"M" => MultidimensionalMatrix},
    Outputs => {Ring => {"the common ring of the entries of ",TT"M"}},
    EXAMPLE {
        "M = multidimensionalMatrix {{{3, 8}, {3, 6}}, {{5, 1}, {4, 2}}}",
        "ring M",
        "M = multidimensionalMatrix {{{7, 2/3}, {1/7, 3}}, {{5/8, 8/9}, {1/3, 5}}}",
        "ring M"
     }
}

document { 
    Key => {shape, (shape,MultidimensionalMatrix)}, 
    Headline => "shape of a multidimensional matrix", 
    Usage => "shape M", 
    Inputs => {"M" => MultidimensionalMatrix => {"an ",TEX///$n$///,"-dimensional matrix of shape ",TEX///$k_1\times\cdots\times k_n$///}},
    Outputs => {List => {"the list of integers ",TEX///$\{k_1, \ldots, k_n\}$///}},
    EXAMPLE {
        "M = multidimensionalMatrix {{{0, 8, 3}, {7, 3, 2}, {2, 7, 0}, {4, 8, 4}}, {{0, 8, 1}, {3, 1, 0}, {4, 7, 4}, {0, 6, 9}}}",
        "shape M"
     },
     SeeAlso => {(dim,MultidimensionalMatrix)}
}

document { 
    Key => {(dim,MultidimensionalMatrix)}, 
    Headline => "dimension of a multidimensional matrix", 
    Usage => "dim M", 
    Inputs => {"M" => MultidimensionalMatrix => {"an ",TEX///$n$///,"-dimensional matrix of shape ",TEX///$k_1\times\cdots\times k_n$///}},
    Outputs => {ZZ => {"the integer ",TEX///$n$///}},
    EXAMPLE {
        "M = multidimensionalMatrix {{{0, 8, 3}, {7, 3, 2}, {2, 7, 0}, {4, 8, 4}}, {{0, 8, 1}, {3, 1, 0}, {4, 7, 4}, {0, 6, 9}}}",
        "dim M"
     },
     SeeAlso => {(shape,MultidimensionalMatrix)}
}

document { 
    Key => {genericSymmetricMultidimensionalMatrix,(genericSymmetricMultidimensionalMatrix,ZZ,ZZ),[genericSymmetricMultidimensionalMatrix,CoefficientRing],[genericSymmetricMultidimensionalMatrix,Variable]}, 
    Headline => "make a generic symmetric multidimensional matrix of variables", 
    Usage => "genericSymmetricMultidimensionalMatrix(n,d)", 
    Inputs => {{TT"(n,d)",", two positive integers."}},
    Outputs => {{"the generic symmetric ",TO2{MultidimensionalMatrix,"multidimensional matrix"}," of shape ",TEX///$d\times\cdots\times d$///," (",TEX///$n$///," times)."}},
    PARA {"An ",TEX///$n$///,"-dimensional matrix ",TEX///$M$///," is symmetric if for every permutation ",TEX///$s$///," of the set ",TEX///$\{0,\ldots,n-1\}$///," we have ",TO permute,TT"(M,s) == M","."},
    EXAMPLE {
        "genericSymmetricMultidimensionalMatrix(3,2)",
        "genericSymmetricMultidimensionalMatrix(3,2,CoefficientRing=>ZZ/101)",
        "genericSymmetricMultidimensionalMatrix(3,2,CoefficientRing=>ZZ/101,Variable=>\"b\")",
    },
    SeeAlso => {genericMultidimensionalMatrix,genericSkewMultidimensionalMatrix}
}

document { 
    Key => {genericSkewMultidimensionalMatrix,(genericSkewMultidimensionalMatrix,ZZ,ZZ),[genericSkewMultidimensionalMatrix,CoefficientRing],[genericSkewMultidimensionalMatrix,Variable]}, 
    Headline => "make a generic skew symmetric multidimensional matrix of variables", 
    Usage => "genericSkewMultidimensionalMatrix(n,d)", 
    Inputs => {{TT"(n,d)",", two positive integers."}},
    Outputs => {{"the generic skew symmetric ",TO2{MultidimensionalMatrix,"multidimensional matrix"}," of shape ",TEX///$d\times\cdots\times d$///," (",TEX///$n$///," times)."}},
    PARA {"An ",TEX///$n$///,"-dimensional matrix ",TEX///$M$///," is skew symmetric if for every permutation ",TEX///$s$///," of the set ",TEX///$\{0,\ldots,n-1\}$///," we have ",TO permute,TT"(M,s) == sign(s)*M","."},
    EXAMPLE {
        "genericSkewMultidimensionalMatrix(3,4)",
        "genericSkewMultidimensionalMatrix(3,4,CoefficientRing=>ZZ/101)",
        "genericSkewMultidimensionalMatrix(3,4,CoefficientRing=>ZZ/101,Variable=>\"b\")",
    },
    SeeAlso => {genericMultidimensionalMatrix,genericSymmetricMultidimensionalMatrix}
}

document { 
    Key => {sylvesterMatrix,(sylvesterMatrix,MultidimensionalMatrix)}, 
    Headline => "Sylvester-type matrix for the hyperdeterminant of a matrix of boundary shape", 
    Usage => "sylvesterMatrix M", 
    Inputs => {"M" => MultidimensionalMatrix => {"an ",TEX///$n$///,"-dimensional matrix of boundary shape ",TEX///$(k_1+1)\times\cdots\times (k_n+1)$///," (that is, ",TEX///$2 max\{k_1,\ldots,k_n\} = k_1+\ldots+k_n$///,")."}},
    Outputs => {Matrix => {"a particular square matrix whose determinant is ",TEX///$det(M)$///," (up to sign), introduced by Gelfand, Kapranov, and Zelevinsky."}},
    PARA{"This is an implementation of Theorem 3.3, Chapter 14, in ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},"."},
    EXAMPLE {
        "M = randomMultidimensionalMatrix {4,2,3}",
        "S = sylvesterMatrix M",
        "det M",
        "det S",
        "assert(oo == ooo or oo == -ooo)"
     },
     SeeAlso => {(determinant,MultidimensionalMatrix)}
}

document { 
    Key => {flattening,(flattening,List,MultidimensionalMatrix),(flattening,ZZ,MultidimensionalMatrix)}, 
    Headline => "flattening of a multidimensional matrix", 
    Usage => "flattening(s,M)", 
    Inputs => {"M" => MultidimensionalMatrix => {"an ",TEX///$n$///,"-dimensional matrix"},
               "s" => List => {"a subset of ",TT"{0,1,...,n-1}"}},
    Outputs => {Matrix => {"the flattening of ",TT"M"," corresponding to the partition ",TT"(s,{0,1,...,n-1} - s)"}},
    EXAMPLE {
        "M = randomMultidimensionalMatrix(2,4,3,2)",
        "s = {0,2};",
        "Ms = flattening(s,M)",
        "s' = {1,3};",
        "Ms' = flattening(s',M)",
        "assert(Ms == transpose Ms')",
     },
     PARA {"If the first argument is an integer ",TT"i",", it is interpreted as the list ",TT"{i}","."},
     EXAMPLE {
         "flattening(1,M)",
         "assert(oo == flattening({1},M))"
     },
     SeeAlso => {(rank,MultidimensionalMatrix)}
}

document { 
    Key => {(rank,MultidimensionalMatrix)}, 
    Headline => "about the border rank of a multidimensional matrix", 
    Usage => "rank M", 
    Inputs => {"M" => MultidimensionalMatrix},
    Outputs => {ZZ => {"a lower bound for the (border) rank of ",TT"M",", which is calculated as the maximum rank of all the ",TO2{flattening,"flattenings"}," of ",TT"M"}},
    PARA{"In some cases, we know that the returned integer ",TT"r = rank M"," is exactly the border rank of ",TT"M",". For instance, this is the case if ",TT"r<3"," by a result of ",HREF{"https://arxiv.org/abs/1011.5867v2","C. Raicu"},". In general, however, we obtain only a lower bound and it is not known how to calculate this rank exactly without resorting to elimination."},
    EXAMPLE {
        "M = randomMultidimensionalMatrix(2,4,3,2,MaximalRank=>2)",
        "rank M",
        "M' = randomMultidimensionalMatrix(2,4,2,1,3,CoefficientRing=>ZZ/65521,MaximalRank=>4)",
        "rank M'"
     },
     SeeAlso => {flattening,randomMultidimensionalMatrix}
}

-- Tests -- 

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
-- the following takes ~125 seconds on a system w/ an intel core i3-4150 cpu
-- (see https://github.com/Macaulay2/M2/issues/1914)
-- assert property {{1,0,0,1},{1,1,0,1},{0,1,0,0},{0,0,2,0},{1,0,1,0}};
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

TEST /// -- det
-- 2x2x2x2
M = multidimensionalMatrix {{{{1, 5}, {2, 8}}, {{6, 4}, {3, 0}}}, {{{0, 0}, {9, 7}}, {{8, 5}, {6, 2}}}}
assert(det M == 15443093137451945984)
M = multidimensionalMatrix {{{{0, 0}, {7, 3}}, {{4, 1}, {6, 2}}}, {{{0, 1}, {8, 3}}, {{3, 2}, {8, 9}}}}
assert(det M == -3171019184352000)
-- 3x3x3
M = multidimensionalMatrix {{{4, 1, 1}, {0, 4, 2}, {3, 6, 0}}, {{0, 6, 7}, {5, 1, 2}, {4, 1, 8}}, {{8, 7, 7}, {3, 2, 8}, {0, 7, 2}}}
assert(det M == -331148935009089093624820864272)
M = multidimensionalMatrix {{{9, 6, 1}, {0, 1, 8}, {0, 9, 2}}, {{0, 1, 8}, {7, 7, 5}, {9, 0, 4}}, {{3, 3, 8}, {8, 4, 8}, {8, 7, 3}}}
assert(det M == -295802129755280442875126108752)
-- 3x4x4
M = multidimensionalMatrix {{{3, 3, 5}, {8, 6, 1}, {1, 6, 6}, {1, 0, 4}}, {{5, 7, 7}, {0, 6, 8}, {6, 7, 4}, {4, 8, 9}}, {{9, 0, 1}, {6, 1, 5}, {9, 7, 1}, {4, 3, 4}}, {{0, 5, 2}, {5, 7, 5}, {6, 9, 1}, {4, 9, 7}}}
assert(det M == 79410647519045266081460676906532049366895749002036567996434812688220789304961231485162415744)
-- 4x3x4
M = multidimensionalMatrix {{{1, 3, 1, 8}, {5, 6, 5, 4}, {3, 7, 4, 5}}, {{7, 6, 5, 2}, {5, 7, 4, 0}, {8, 8, 6, 5}}, {{7, 3, 7, 4}, {4, 3, 5, 2}, {2, 4, 2, 7}}, {{2, 7, 8, 3}, {7, 1, 5, 4}, {3, 4, 8, 3}}}
assert(det M == -38294458590140101732130753793959707479707823954684704526689898232575574571008653766688)
-- 5x1x1x2x1x5x1
M = multidimensionalMatrix {{{{{{{6, 8, 4, 6, 5}}}, {{{6, 4, 1, 0, 6}}}}}, {{{{{7, 6, 2, 8, 7}}}, {{{3, 9, 2, 1, 7}}}}}, {{{{{0, 4, 5, 5, 5}}}, {{{1, 7, 6, 7, 4}}}}}, {{{{{4, 3, 9, 9, 1}}}, {{{7, 9, 5, 0, 2}}}}}, {{{{{0, 7, 7, 7, 4}}}, {{{8, 2, 6, 7, 4}}}}}}}
assert(det M == -1013820701536151127624817309100800)
///;

TEST /// -- convolution
A = randomMultidimensionalMatrix {3,5}
B = randomMultidimensionalMatrix {5,6}
assert(matrix entries(A*B) == (matrix entries A) * (matrix entries B))
--
A' = randomMultidimensionalMatrix {1,1,5,1,4}
B' = randomMultidimensionalMatrix {4,1,2,1}
A'B' = A' * B';
assert(shape A'B' == {1, 1, 5, 1, 1, 2, 1})
debug SparseResultants;
assert(matrix entries removeOneDim A'B' == (matrix entries removeOneDim A') * (matrix entries removeOneDim B'))
--
assert(shape((randomMultidimensionalMatrix {5,2,2}) * (randomMultidimensionalMatrix {2,3,4,2})) == {5,2,3,4,2})
assert(shape((randomMultidimensionalMatrix({5,2,1,2},CoefficientRing=>ZZ/3331)) * (randomMultidimensionalMatrix({2,3,4,2,1},CoefficientRing=>ZZ/3331))) == {5,2,1,3,4,2,1})
--
equalUpToSign = (A,B) -> A == B or A == -B;
--
detConvolution = method();
detConvolution (MultidimensionalMatrix,MultidimensionalMatrix) := (A,B) -> (
    Na := degreeDeterminant shape A;
    Nb := degreeDeterminant shape B;
    l := first shape B;
    detA := det A;
    assert equalUpToSign(detA,det sylvesterMatrix A);
    detB := det B;
    assert equalUpToSign(detB,det sylvesterMatrix B);
    AB := A*B;
    detAB := det AB;
    assert equalUpToSign(detAB,det sylvesterMatrix AB);
    assert(detA^Nb * detB^Na == detAB^l);
    <<"formula: "<<detAB<<"^"<<l<<" = "<<detA<<"^"<<Nb<<" * "<<detB<<"^"<<Na<<endl;
);
detConvolution (List,List) := (l,m) -> detConvolution(randomMultidimensionalMatrix l,randomMultidimensionalMatrix m);
detConvolution({3,3},{3,3})
detConvolution({1,4,1,4},{4,1,4})
detConvolution({2,2,3},{3,3})
detConvolution({3,2,2},{2,2})
-- det 2x3x4 --
-- assert(60600 == # terms det genericMultidimensionalMatrix {2,3,4})
detConvolution({2,3,4},{4,4})
detConvolution({3,2,4},{4,4})
detConvolution({2,4,3},{3,3})
detConvolution({4,2,3},{3,3})
detConvolution({3,4,2},{2,2})
detConvolution({4,3,2},{2,2})
detConvolution({2,2},{2,3,4})
detConvolution({2,2},{2,4,3})
detConvolution({3,3},{3,2,4})
detConvolution({3,3},{3,4,2})
detConvolution({4,4},{4,2,3})
detConvolution({4,4},{4,3,2})
-- det 2x4x5 --
detConvolution({2,4,5},{5,5})
detConvolution({4,2,5},{5,5})
detConvolution({2,5,4},{4,4})
detConvolution({5,2,4},{4,4})
detConvolution({4,5,2},{2,2})
detConvolution({5,4,2},{2,2})
detConvolution({2,2},{2,4,5})
detConvolution({2,2},{2,5,4})
detConvolution({4,4},{4,2,5})
detConvolution({4,4},{4,5,2})
detConvolution({5,5},{5,2,4})
detConvolution({5,5},{5,4,2})
-- 
time detConvolution({2,2,2,4},{4,2,5})
///;

TEST /// -- permute
A = randomMultidimensionalMatrix {3,4}
assert(matrix entries permute(A,{1,0}) == transpose matrix entries A)
-- 
perm = (L,s) -> (assert(#L == #s); for i to #L-1 list L_(s_i));
M = genericMultidimensionalMatrix {2,5,3,1,2};
assert(last baseName M_(1,4,2,0,1) == (1,4,2,0,1) and last baseName M_(1,3,2,0,1) == (1,3,2,0,1) and last baseName M_(1,0,0,0,1) == (1,0,0,0,1) and last baseName M_(0,0,0,0,0) == (0,0,0,0,0))
s = {1,0,4,2,3}
s' = {1,0,3,4,2}
sM = permute(M,s);
assert(permute(sM,s') == M and sM != M)
assert(shape sM == perm(shape M,s))
for i to 15 do (
    a = (random(0,4),random(0,1),random(0,1),random(0,2),random(0,0));
    << a <<endl;
    assert(sM_a == M_(a_(s'_0),a_(s'_1),a_(s'_2),a_(s'_3),a_(s'_4)))
);
--
M = randomMultidimensionalMatrix({2,2,3},CoefficientRing=>ZZ/33331);
s = {1,2,0};
s' = {2,0,1};
sM = permute(M,s);
assert(permute(sM,s') == M and sM != M)
assert(shape sM == perm(shape M,s))
for i to 10 do (
    a = (random(0,1),random(0,2),random(0,1));
    << a <<endl;
    assert(sM_a == M_(a_(s'_0),a_(s'_1),a_(s'_2)))
);
assert(det M == det sM)
///

TEST ///
debug SparseResultants
A = genericSymmetricMultidimensionalMatrix(3,2)
assert(isSymmetric A)
B = genericSkewMultidimensionalMatrix(3,4)
assert(isSkewSymmetric B)
///

TEST ///
L = ((2,2,2,4),(2,2,3,6),(2,3,3,12),(2,3,4,12),(2,4,4,24),(2,4,5,20),(3,3,3,36),(3,3,4,48),(3,3,5,30),(3,4,4,108),(3,4,5,120),(4,4,4,272));
assert all(L,l -> degreeDeterminant(toList take(l,3)) == last l)
///

TEST ///
M = randomMultidimensionalMatrix {3,2,4}
F = M!
N = multidimensionalMatrix F
assert(M == N)
assert(M === N)
--
M = genericMultidimensionalMatrix {3,2,4}
F = M!
N = multidimensionalMatrix F
assert(M == N)
assert(M === N)
--
a := local a;
K := ZZ[a_0..a_10]
M = multidimensionalMatrix {{{1, 4*a_0^2-a_1, 9, 8*a_2}, {4, 4, 9, 8*a_4-7*a_2^3+1}}, {{4, 6*a_3+1, 2, 6}, {2, 1, 0, 1}}, {{8, 1, 7, 6}, {1, 7, 7, 8}}}
F = M!
N = multidimensionalMatrix F
assert(M == N)
assert(M === N)
--
K := QQ[a_0..a_10,Degrees=>{5:{1,0},6:{0,1}}]
M = multidimensionalMatrix {{{a_10^4, 4*a_0^2-a_1, 9, 8*a_2}, {4, 4, 9, 8*a_4-7*a_2^3+1}}, {{4, 6*a_3+1, 2, 6}, {2, 1, 0, 1}}, {{8, 1, 7, 6}, {1, 7, 7, 8}}}
F = M!
N = multidimensionalMatrix F
assert(M == N)
assert(M === N)
///;

end 

TEST ///
M = genericMultidimensionalMatrix {2,2,2,2}
time D = det M;
assert(degree D == {24})
assert(# terms D == 2894276)
///

