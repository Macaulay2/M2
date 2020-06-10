
newPackage(
       "SparseResultants",
        Version => "0.1", 
        Date => "June 8, 2020",
    	Headline => "computations with sparse resultants",
        Authors => {{Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com"}},
        PackageExports => {"Resultants"},
        DebuggingMode => false
);

export{"sparseResultant", "UnMixed"}

SPARSERESULTANT := local SPARSERESULTANT;

SparseResultant = new Type of HashTable;

char SparseResultant := (R) -> R#"characteristic";

SparseResultant SPACE Matrix := (R,F) -> (R#"evaluation") F;

SparseResultant SPACE VisibleList := (R,F) -> (R#"evaluation") toList(F);

net SparseResultant := (R) -> net("sparse ")|net(if R#"UnMixed" then "unmixed" else "mixed")|net(" resultant associated to ")|net(R#"exponents")|net(" over ")|net(if char R == 0 then ZZ else ZZ/(char R));

sparseResultant = method(Dispatch => Thing, Options => {UnMixed => null, CoefficientRing => ZZ});

sparseResultant Thing := o -> I -> ( 
    I = toList sequence I;
    if o.UnMixed =!= null then if not instance(o.UnMixed,Boolean) then error "UnMixed option accepts a boolean value";
    if o.CoefficientRing =!= ZZ then (try assert(o.CoefficientRing === ZZ/(char o.CoefficientRing)) else error "CoefficientRing option accepts ZZ or a ring of the form ZZ/p with p prime");
    if unique apply(I,i -> instance(i,Matrix)) === {true} then (
        if unique apply(I,i -> ring i === ZZ) === {true} then (
            if # I > 1 then (
                if o.UnMixed === true then error "the input 'UnMixed=>true' is inconsistent with the other ones";
                return sparseMixedResultant(I,char o.CoefficientRing); 
            ) else (
                if o.UnMixed === false then error "the input 'UnMixed=>false' is inconsistent with the other ones";
                return sparseUnmixedResultant(first I,char o.CoefficientRing);
            );
        );
    );
    if unique apply(I,i -> instance(i,RingElement)) === {true} then (
        return sparseResultant(I,UnMixed=>o.UnMixed,CoefficientRing=>o.CoefficientRing);
    );
    if #I == 1 then (
        I' := I_0;
        if instance(I',VisibleList) then I' = toList I';
        if instance(I',Matrix) or instance(I',List) then (
            if o.CoefficientRing =!= ZZ then error "the CoefficientRing option cannot be used when the polynomials are given in input (the right coefficient ring is taken automatically)";
            if o.UnMixed === true then return sparseUnmixedResultant(I') else return sparseMixedResultant(I');
        );
    );
    error("expected one of the following:"|newline|"-- one or more matrices of integers to represent the exponent vectors of the monomials, or"|newline|"-- a list/sequence/row matrix of n+1 Laurent polynomials in n variables");
);

sparseUnmixedResultant = method();

sparseUnmixedResultant (Matrix,ZZ) := (M,ch) -> (
    if ring M =!= ZZ then error "the exponents must be integers";
    M = trimExponents M;
    if instance(SPARSERESULTANT_(M,0),SparseResultant) then return SPARSERESULTANT_(M,0);
    if instance(SPARSERESULTANT_(M,ch),SparseResultant) then return SPARSERESULTANT_(M,ch);    
    n := numRows M;
    l := numColumns M;
    if l-1 < n then error "hypothesis not satisfied by the set of monomials (there are too few monomials)";
    if l-1 == n then return sparseMixedResultant(toList(l:M),ch);
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
    f := map(R,S,F); 
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
        "UnMixed" => true,
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
            R := (coefficientRing ring M)[t_1..t_(#x - 1),Inverses=>(options ring M)#Inverses,,MonomialOrder=>(if (options ring M)#Inverses then Lex else GRevLex)];
            s := map(R,ring M,matrix{{1}}|vars R);
            M' := s M;
            if unique apply(flatten entries M',m -> first degree m) === d then (
                <<"--warning: "|newline|"--the input of sparseResultant should be n+1 non-homogeneous polynomials in n variables;"|newline|"--a dishomogenization of the input polynomials was performed successfully."<<endl;
                return sparseUnmixedResultant M';
            );
        );
    );
    if numColumns M =!= #x + 1 then error "expected the number of polynomials to be one more than the number of variables";
    N := transpose matrix sort unique flatten apply(flatten entries M,l -> apply(terms l,t -> apply(x,u -> degree(u,t))));
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
    isUnMixed := # unique lM == 1;
    if isUnMixed then if numColumns(first lM) != n+1 then return sparseUnmixedResultant(first lM,ch);
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
        "exponents" => if isUnMixed then first lM else lM,
        "characteristic" => ch,
        "evaluation" => val,
        "UnMixed" => isUnMixed,
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
    N := apply(flatten entries M,l -> transpose matrix sort apply(terms l,t -> apply(x,u -> degree(u,t))));
    Res := sparseMixedResultant(N,char coefficientRing ring M);
    return (Res M);
);

sparseMixedResultant (List) := (L) -> (
    try assert isPolynomialRing ring matrix {L} else error "expected a list (or row matrix) of Laurent polynomials";
    sparseMixedResultant matrix {L}
);

trimExponents = method();
trimExponents (Matrix) := (M) -> transpose matrix sort unique entries transpose M;
trimExponents (List) := (L) -> apply(L,M -> trimExponents M);

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

beginDocumentation() 

document { 
    Key => SparseResultants, 
    Headline => "computations with sparse resultants", 
    PARA{"This package provides the method ",TO "sparseResultant"," to calculate sparse resultants. Dense resultants can be instead calculated using methods from the package ",TO Resultants,"."},
    PARA{"For the definitions, see one of the following books:"}, 
    PARA{"(1) ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"},", by David A. Cox, John Little, Donal O'shea; "},
    PARA{"(2) ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky."},
    Caveat => {"This package will likely be significantly improved in future versions."}
}

document { 
    Key => {sparseResultant,(sparseResultant,Thing),UnMixed,[sparseResultant,UnMixed],[sparseResultant,CoefficientRing]}, 
    Headline => "sparse resultant", 
    Usage => "sparseResultant M", 
    Inputs => {"M" => Sequence => {"a sequence or list of ",TEX///$n+1$///," matrices ",TEX///$M_0,\ldots,M_n$///," over ",TEX///$\mathbb{Z}$///," and with ",TEX///$n$///," rows to represent the exponent vectors of Laurent polynomials ",TEX///$f_0,\ldots,f_n$///," in ", TEX///$n$///," variables. In the unmixed case, that is when ",TEX///$M_0=\cdots=M_n$///,", it is enough to pass just one of the matrices. If one wants to perform computations only in characteristic ",TEX///$p>0$///,", 
    then it is recommended to use the option ",TT "CoefficientRing=>ZZ/p","."}}, 
    Outputs => {{"the universal sparse resultant associated to ",TEX///$M$///,", that is, an object that to a ", TO Sequence," (or a ",TO List,", or a row matrix) of ", TEX///$n+1$///," Laurent polynomials ", TEX///$f_0,\ldots,f_n$///," in ", TEX///$n$///," variables over a coefficient ring ",TEX///$K$///,", involving only monomials from ",TEX///$M$///,", associates the sparse resultant of the polynomials ",TEX///$f_0,\ldots,f_n$///,", which is an element of ",TEX///$K$///,"."}}, 
    PARA {"Alternatively, one can apply the method directly to the list of Laurent polynomials ",TEX///$f_0,\ldots,f_n$///,". In this case, the matrices ",TEX///$M_0,\ldots,M_n$///, " are automatically and optimally determined. If you want require that ",TEX///$M_0=\cdots=M_n$///,", then use the option ",TT "UnMixed=>true"," (this could be faster)."},
    PARA {"Below we consider some examples."},
    PARA {"In the first example, we calculate the sparse (mixed) resultant associated to the three sets of monomials ",TEX///$(1,s,st),(1,s,t,st),(1,s,t,st,s^2)$///,". Then we evaluate it at the three polynomials ",TEX///$f = a_0 + a_1 s + a_2 s t, g = b_0 + b_1 s + b_2 t + b_3 s t, h = c_0 + c_1 s + c_2 t + c_3 s t + c_4 s^2$///,"."},
    EXAMPLE {
        "Res = sparseResultant(matrix{{0,1,1},{0,0,1}},matrix{{0,0,1,1},{0,1,0,1}},matrix{{0,0,1,1,2},{0,1,0,1,0}})",
        "use QQ[a_0..a_2,b_0..b_3,c_0..c_4][s,t];",
        "(f,g,h) = (a_0 + a_1*s + a_2*s*t, b_0 + b_1*s + b_2*t + b_3*s*t, c_0 + c_1*s + c_2*t + c_3*s*t + c_4*s^2)",
        "Res(f,g,h)",
        "sparseResultant(f,g,h)",
        "assert(oo === ooo)"
    },
    PARA {"In the second example, we calculate the sparse unmixed resultant associated to the set of monomials ",TEX///$(1,s,t,st)$///,". Then we evaluate it at the three polynomials ",TEX///$f = a_0 + a_1 s + a_2 t + a_3 s t, g = b_0 + b_1 s + b_2 t + b_3 s t, h = c_0 + c_1 s + c_2 t + c_3 s t$///,". Moreover, we perform all the computation over ",TEX///$\mathbb{Z}/3331$///,"."},    
    EXAMPLE {
        "Res = sparseResultant(matrix{{0,0,1,1},{0,1,0,1}},CoefficientRing=>ZZ/3331)",
        "use ZZ/3331[a_0..a_3,b_0..b_3,c_0..c_3][s,t];",
        "(f,g,h) = (a_0 + a_1*s + a_2*t + a_3*s*t, b_0 + b_1*s + b_2*t + b_3*s*t, c_0 + c_1*s + c_2*t + c_3*s*t)",
        "Res(f,g,h)",
        "sparseResultant(f,g,h)",
        "assert(oo === ooo)"
    },
    PARA {"In the third example, we calculate the sparse resultant of three generic polynomials of degree two in two variables, that is, the dense resultant of three ternary quadrics. We then show that it has 21.894 terms."},
    EXAMPLE {
        "use QQ[a_0,a_1,a_2,a_3,a_4,a_5,b_0,b_1,b_2,b_3,b_4,b_5,c_0,c_1,c_2,c_3,c_4,c_5][x,y];",
        "(f,g,h) = (a_0+a_1*x+a_2*y+a_3*x^2+a_4*x*y+a_5*y^2,b_0+b_1*x+b_2*y+b_3*x^2+b_4*x*y+b_5*y^2,c_0+c_1*x+c_2*y+c_3*x^2+c_4*x*y+c_5*y^2)",
        "time # terms sparseResultant(f,g,h)"
    },
    PARA {"In this last example, we compare the sparse mixed resultant, the sparse unmixed resultant, and the dense resultant of two univariate polynomials of degree 3 and 5."},
    EXAMPLE {
        "use QQ[a_0..a_7][t];",
        "(f,g) = (a_4*t^3+(a_3^2-2*a_2)*t^2+a_0*a_1, (a_5+2*a_6)*t^5+(3*a_4+a_2^2)*t^2+a_0*a_7*t)",
        "time mixRes = sparseResultant(f,g); -- mixed resultant",
        "time unmixRes = sparseResultant(f,g,UnMixed=>true); -- unmixed resultant",
        "time denRes = affineResultant {f,g}; -- dense resultant",
        "quotientRemainder(unmixRes,denRes),quotientRemainder(denRes,mixRes)"
    },
    SeeAlso => {resultant, affineResultant} 
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
F = matrix{genericPolynomials {2,2,2}};
F = flatten entries sub(sub(F,first gens ring F => 1),(coefficientRing ring F)[(gens ring F)_{1..(numgens ring F -1)}])
assert(U F === affineResultant F)
--
G = genericPolynomials({1,1,2},ZZ/33331);
assert(sparseResultant G === resultant G)
x = gens ring G_0;
assert(sparseResultant(x_1^2 * x_2 * G_0,x_1^3 * G_1,x_2 * G_2) === sparseResultant G)
R := newRing(coefficientRing ring G_0,Inverses=>true,MonomialOrder=>Lex)[gens ring G_0,Inverses=>true,MonomialOrder=>Lex]
x = gens R;
G = flatten entries sub(matrix{G},R)
assert(sparseResultant(x_2^(-2)*x_1^(-3) * G_0,x_1^3 *x_1^(-1) * G_1,x_2^4 * x_1^(-4) * G_2) === sparseResultant G)
--
G = genericPolynomials({2,2,2},ZZ/33331);
assert(sparseResultant G === resultant G)
x = gens ring G_0;
assert(sparseResultant(x_1^2 * x_2 * G_0,x_1^2 * x_2 *  G_1,x_1^2 * x_2 * G_2) === sparseResultant G)
R := newRing(coefficientRing ring G_0,Inverses=>true,MonomialOrder=>Lex)[gens ring G_0,Inverses=>true,MonomialOrder=>Lex]
x = gens R;
G = flatten entries sub(matrix{G},R)
assert(sparseResultant(x_2^(-2)*x_1^(-3) * G_0,x_2^(-2)*x_1^(-3) * G_1,x_2^(-2)*x_1^(-3) * G_2) === sparseResultant G)
///

TEST ///
F = genericPolynomials({1,1},ZZ/33331);
elapsedTime R = resultant F;
elapsedTime S = sparseResultant F;
assert(R == -S)
--
F = genericPolynomials({1,1,1},ZZ/33331);
elapsedTime R = resultant F;
elapsedTime S = sparseResultant F;
assert(R == S)
--
F = genericPolynomials({1,1,1,1},ZZ/33331);
elapsedTime R = resultant F;
elapsedTime S = sparseResultant F;
assert(R == -S)
--
F = genericPolynomials({1,2,2},ZZ/33331);
elapsedTime R = resultant F;
elapsedTime S = sparseResultant F;
assert(R == S)
--
F = genericPolynomials({1,1,2,1},ZZ/33331);
elapsedTime R = resultant F;
elapsedTime S = sparseResultant F;
assert(R == S)
--
F = genericPolynomials({3,4},ZZ/33331);
elapsedTime R = resultant F;
elapsedTime S = sparseResultant F;
assert(R == S)
--
F = genericPolynomials({3,1,1},ZZ/33331);
elapsedTime R = resultant F;
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

