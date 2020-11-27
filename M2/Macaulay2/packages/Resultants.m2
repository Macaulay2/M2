
newPackage(
       "Resultants",
	Version => "1.2.2", 
    	Date => "May 10, 2019",
    	Authors => {{Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com"}},
    	Headline => "resultants, discriminants, and Chow forms",
	Keywords => {"Commutative Algebra"},
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "A package for computations with classical resultants",
	     "acceptance date" => "18 May 2018",
	     "published article URI" => "https://msp.org/jsag/2018/8-1/p03.xhtml",
	     "published article DOI" => "10.2140/jsag.2018.8.21",
	     "published code URI" => "https://msp.org/jsag/2018/8-1/jsag-v8-n1-x03-Resultants.m2",
	     "repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/Resultants.m2",
	     "release at publication" => "61c93a6aaf9d6bf0dd11440339145703ce3d824b",	    -- git commit number in hex
	     "version at publication" => "1.2.1",
	     "volume number" => "8",
	     "volume URI" => "https://msp.org/jsag/2018/8-1/"
	     }
)

export{
       "resultant",
       "discriminant",
       "affineResultant",
       "affineDiscriminant",
       "genericPolynomials",
       "veronese",
       "macaulayFormula",
       "fromPluckerToStiefel",
       "Grass",
       "dualize",
       "tangentialChowForm", 
       "chowForm",
       "hurwitzForm",
       "chowEquations",
       "cayleyTrick", 
       "Duality",
       "AffineChartGrass",
       "AffineChartProj",
       "dualVariety",
       "AssumeOrdinary",
       "isCoisotropic",
       "conormalVariety",
       "isInCoisotropic",
       "plucker",
       "SingularLocus"
};

----------------------------------------------------------------------------------
----------------------- MultipolynomialResultats ---------------------------------
----------------------------------------------------------------------------------
    
resultant = method(TypicalValue => RingElement, Options => {Algorithm => "Poisson"});
    
resultant (Matrix) := o -> (F) -> (
    if numgens target F != 1 then error "expected a matrix with one row";
    if not isPolynomialRing ring F then error "the base ring must be a polynomial ring";
    n := numgens source F -1;
    if n+1 != numgens ring F then error("the number of polynomials must be equal to the number of variables, but got " | toString(numgens source F) | " polynomials and " | toString(numgens ring F) | " variables");
    if o.Algorithm =!= "Poisson" and o.Algorithm =!= "Poisson2" and o.Algorithm =!= "Macaulay" and o.Algorithm =!= "Macaulay2" then error "bad value for option Algorithm; possible values are \"Poisson\", \"Poisson2\", \"Macaulay\", and \"Macaulay2\"";         
    K := coefficientRing ring F;
    x := local x;
    Pn := K[x_0..x_n];
    F = sub(F,vars Pn); F' := F;
    d := apply(flatten entries F,ee->first degree ee);
    if not isField K then (K' := frac K; Pn' := K'[x_0..x_n]; F' = sub(F,Pn'));
    if not isHomogeneous ideal F' then error("expected homogeneous polynomials");
    if o.Algorithm === "Macaulay" then (if (min d > -1 and sum(d) > n) then return MacaulayResultant(F,false) else <<"--warning: ignored option Algorithm=>\"Macaulay\""<<endl);
    if o.Algorithm === "Poisson2" then return interpolateRes(F,"Poisson");
    if o.Algorithm === "Macaulay2" then return interpolateRes(F,"Macaulay");
    R := PoissonFormula F';
    if R != 0 then (
        if isField K then return R;
        if isUnit denominator R then return (denominator R)^(-1) * (numerator R) else return R;
    );
    if dim ideal F' > 0 then sub(0,K) else resultant(wobble F,Algorithm=>"Poisson") 
);

resultant (List) := o -> (s) -> resultant(matrix{s},Algorithm=>o.Algorithm);
    
PoissonFormula = method();
PoissonFormula (Matrix) := (F) -> (
    -- Theorem 3.4, p. 96 of [David A. Cox and John Little and Donal O'shea - Using Algebraic Geometry - (2005)]
    n := numgens source F -1;
    K := coefficientRing ring F;
    x := local x;
    R := K[x_0..x_n];
    F = sub(F,vars R);
    d := apply(flatten entries F,ee->first degree ee);
    if n == 0 then return leadCoefficient F_(0,0);
    if n == 1 then if min d > 0 then return standardRes(F,false);
    if d === {2,2,2} then return Res222 F; 
    xn := x_n; S := K[x_0..x_(n-1)];
    f := sub(sub(F,{xn=>1}),S);
    Fbar := sub(sub(submatrix'(F,,{n}),{xn=>0}),S);
    Res0 := PoissonFormula Fbar;
    if Res0 == 0 then return Res0;
    A := S/ideal(submatrix'(f,,{n}));
    bs := basis(A,Limit=>(product d_{0..(#d-2)}));  
    entriesBs := flatten entries bs; 
    if not apply(entriesBs,leadMonomial) === entriesBs then error "internal method expected to receive a monomial basis, but received something else";
    mf := sub(submatrix(f,{n}),A);
    mf = sub(last coefficients(mf*bs,Monomials=>bs),K);
    Res0^(d_n) * det(mf)
);

MacaulayResultant = method();
MacaulayResultant (Matrix,Boolean) := (F,onlyMatrices) -> (
    -- Theorem 4.9, p. 108 of [David A. Cox and John Little and Donal O'shea - Using Algebraic Geometry - (2005)]
    K := coefficientRing ring F;
    n := numgens source F -1;
    d := apply(flatten entries F,ee->first degree ee);
    x := gens ring F;
    mons := flatten entries gens (ideal x)^(sum(d)-n);
    eqs := {}; nonReducedMons := {}; q := local q; r := local r; divs := local divs;
    for i to #mons -1 do (
          divs = {};
          for j to n do (
                (q,r) = quotientRemainder(mons_i,x_j^(d_j));
                if r == 0 then divs = append(divs,j);
                if divs == {j} then eqs = append(eqs,q*F_(0,j));
                if #divs >= 2 then (nonReducedMons = append(nonReducedMons,i); break);
          );
    );
    Mn := sub(transpose (coefficients(matrix{eqs},Monomials=>mons))_1,K);
    Mn' := submatrix(Mn,nonReducedMons,nonReducedMons);
    if onlyMatrices then return (Mn,Mn');
    Dn := det Mn;
    Dn' := det Mn';
    if Dn' == 0 then (
          F' := F;
          if not isField K then F' = sub(F',frac(K)[x]);
          if dim ideal F' > 0 then return sub(0,K) else return MacaulayResultant(wobble F,false);
    );
    resF := Dn/Dn';
    if ring resF === K then return resF;
    if isUnit denominator resF then return (denominator resF)^(-1) * (numerator resF) else return resF;
);   

interpolateRes = (F,Alg) -> (    
    R := coefficientRing ring F; 
    if not (isPolynomialRing R and numgens R > 0) then error "interpolation not possible: expected coefficient ring to be polynomial";
    if not isHomogeneous ideal F then error "interpolation not possible: expected homogeneous polynomial(s)";
    K := coefficientRing R; 
    if not isField K then K = frac K;
    n := numgens source F -1;
    x := local x; Rx := R[x_0..x_n]; Kx := K[x_0..x_n];
    F = sub(F,vars Rx);
    d := sum(0..n,i -> (degree F_(0,i))_1 * product first entries submatrix'(matrix{apply(flatten entries F,ee -> first degree ee)},,{i}));
    if d == 0 then error "interpolation not possible: expected polynomials with nonconstant coefficients";
    points := sub(matrix apply(flatten entries gens (ideal gens R)^d,G->apply(gens R,t->degree(t,G))),K); 
    M := points|transpose matrix{apply(entries points,p->resultant(sub(sub(F,apply(numgens R,i -> R_i=>p_i)),Kx),Algorithm=>Alg))};
    if K === coefficientRing R then return interpolate(M,R,d);
    Res := interpolate(M,K[gens R],d);
    l := lcm apply(flatten entries sub(last coefficients Res,coefficientRing ring Res),u->denominator u);
    if isUnit l then sub(l^(-1) * (l * Res),vars R) else sub(l * Res,vars R)
);

wobble = method();
wobble (Matrix) := (F) -> (  
    R := ring F;
    K := coefficientRing R;
    n := numgens R -1;
    A := matrix 0; d := 0; while not isUnit d do (A = matrix for i to n list for j to n list sub(random(0,1),K); d = det A);
    A = d^(-1) * (matrix A_0) | submatrix'(A,,{0});
    Sub := map(R,R,transpose(A*(transpose vars R)));
    Sub(F)
);

standardRes = method();
standardRes (Matrix,Boolean) := (F,onlyMatrix) -> (
    -- Determinant of the Sylvester matrix
    K := coefficientRing ring F;
    (x,y) := toSequence gens ring F;
    (l,m) := (first degree F_(0,0),first degree F_(0,1));
    A := sub(transpose (coefficients(matrix{{F_(0,0)}}, Monomials => for i to l list x^(l-i) * y^i))_1,K);   
    B := sub(transpose (coefficients(matrix{{F_(0,1)}}, Monomials => for i to m list x^(m-i) * y^i))_1,K);   
    A' := transpose(A|matrix{toList(m-1:0_K)});
    B' := transpose(B|matrix{toList(l-1:0_K)});
    Sylvester := A';
    for i from 1 to m-1 do (
        A' = matrix{{0_K}} || submatrix'(A',{l+m-1},);
        Sylvester = Sylvester|A'
    );
    Sylvester = Sylvester|B';
    for i from 1 to l-1 do (
        B' = matrix{{0_K}} || submatrix'(B',{l+m-1},);
        Sylvester = Sylvester|B'
    );
    if onlyMatrix then Sylvester else det Sylvester
);

-- Res222' = (F) -> ( -- not longer used
--    -- resultant of three ternary quadrics
--    -- p. 89 of [David A. Cox and John Little and Donal O'shea - Using Algebraic Geometry (2005)]
--    assert(char coefficientRing ring F =!= 2);
--    K := coefficientRing ring F;
--    (x,y,z) := toSequence gens ring F;
--    M := {x^2,y^2,z^2,x*y,x*z,y*z};
--    J := transpose jacobian matrix{{det jacobian F}};
--    A := sub(transpose (coefficients(F,Monomials=>M))_1,K);
--    B := sub(transpose (coefficients(J,Monomials=>M))_1,K);
--    d := -1/sub(512,K)*det(A||B);
--    try lift(d,K) else d
-- );

Res222 = (F) -> (
   K := coefficientRing ring F;
   y := local y;
   ringG25 := K[y_(0,1,2),y_(0,1,3),y_(0,2,3),y_(1,2,3),y_(0,1,4),y_(0,2,4),y_(1,2,4),y_(0,3,4),y_(1,3,4),y_(2,3,4),y_(0,1,5),y_(0,2,5),y_(1,2,5),y_(0,3,5),y_(1,3,5),y_(2,3,5),y_(0,4,5),y_(1,4,5),y_(2,4,5),y_(3,4,5)];
     -- code: "chow"<<toString dualize chowForm(trim kernel map(QQ[t_0..t_2],QQ[x_0..x_5],gens (ideal(t_0..t_2))^2),Variable=>y)<<close;
   W := y_(0,3,5)^4+y_(0,2,4)*y_(0,3,5)^2*y_(1,3,5)-2*y_(0,1,5)*y_(0,3,5)^2*y_(1,3,5)+y_(0,1,5)^2*y_(1,3,5)^2+y_(0,2,3)*y_(0,2,5)*y_(1,3,5)^2-y_(0,1,4)*y_(0,2,5)*y_(1,3,5)^2+y_(0,1,2)*y_(1,2,5)*y_(1,3,5)^2-y_(0,2,4)*y_(0,3,4)*y_(0,3,5)*y_(2,3,5)+2*y_(0,2,3)*y_(0,3,5)^2*y_(2,3,5)+y_(0,1,4)*y_(0,3,5)^2*y_(2,3,5)-y_(0,2,3)*y_(0,2,4)*y_(1,3,5)*y_(2,3,5)+y_(0,1,4)*y_(0,2,4)*y_(1,3,5)*y_(2,3,5)-y_(0,1,2)*y_(1,2,4)*y_(1,3,5)*y_(2,3,5)-y_(0,1,4)*y_(0,1,5)*y_(1,3,5)*y_(2,3,5)+3*y_(0,1,2)*y_(0,3,5)*y_(1,3,5)*y_(2,3,5)+y_(0,2,3)^2*y_(2,3,5)^2+y_(0,1,2)*y_(1,2,3)*y_(2,3,5)^2-y_(0,1,3)*y_(0,2,4)*y_(2,3,5)^2-y_(0,1,2)*y_(0,3,4)*y_(2,3,5)^2+y_(0,1,3)*y_(0,1,5)*y_(2,3,5)^2-2*y_(0,3,4)*y_(0,3,5)^2*y_(0,4,5)+y_(0,3,4)^2*y_(0,4,5)^2-y_(0,2,4)*y_(0,3,4)*y_(0,3,5)*y_(1,4,5)-2*y_(0,2,3)*y_(0,3,5)^2*y_(1,4,5)+3*y_(0,1,4)*y_(0,3,5)^2*y_(1,4,5)-y_(0,2,3)*y_(0,2,4)*y_(1,3,5)*y_(1,4,5)+y_(0,1,4)*y_(0,2,4)*y_(1,3,5)*y_(1,4,5)-y_(0,1,2)*y_(1,2,4)*y_(1,3,5)*y_(1,4,5)-y_(0,1,4)*y_(0,1,5)*y_(1,3,5)*y_(1,4,5)-y_(0,1,2)*y_(0,3,5)*y_(1,3,5)*y_(1,4,5)+2*y_(0,2,3)*y_(0,3,4)*y_(0,4,5)*y_(1,4,5)-y_(0,1,4)*y_(0,3,4)*y_(0,4,5)*y_(1,4,5)-2*y_(0,1,3)*y_(0,3,5)*y_(0,4,5)*y_(1,4,5)+y_(0,2,3)^2*y_(1,4,5)^2+y_(0,1,2)*y_(1,2,3)*y_(1,4,5)^2-y_(0,1,3)*y_(0,2,4)*y_(1,4,5)^2+2*y_(0,1,2)*y_(0,3,4)*y_(1,4,5)^2+y_(0,1,3)*y_(0,1,5)*y_(1,4,5)^2+y_(0,2,4)*y_(0,3,4)^2*y_(2,4,5)+y_(0,2,3)*y_(0,2,4)*y_(1,3,4)*y_(2,4,5)-y_(0,1,4)*y_(0,2,4)*y_(1,3,4)*y_(2,4,5)+y_(0,1,2)*y_(1,2,4)*y_(1,3,4)*y_(2,4,5)-y_(0,2,3)^2*y_(2,3,4)*y_(2,4,5)-y_(0,1,2)*y_(1,2,3)*y_(2,3,4)*y_(2,4,5)+y_(0,1,3)*y_(0,2,4)*y_(2,3,4)*y_(2,4,5)+y_(0,1,2)*y_(0,3,4)*y_(2,3,4)*y_(2,4,5)-2*y_(0,2,3)*y_(0,3,4)*y_(0,3,5)*y_(2,4,5)-y_(0,1,4)*y_(0,3,4)*y_(0,3,5)*y_(2,4,5)-2*y_(0,1,3)*y_(0,3,5)^2*y_(2,4,5)+y_(0,1,4)^2*y_(1,3,5)*y_(2,4,5)-3*y_(0,1,2)*y_(0,3,4)*y_(1,3,5)*y_(2,4,5)-y_(0,1,3)*y_(0,1,4)*y_(2,3,5)*y_(2,4,5)+2*y_(0,1,3)*y_(0,3,4)*y_(0,4,5)*y_(2,4,5)-y_(0,1,3)*y_(0,1,4)*y_(1,4,5)*y_(2,4,5)+y_(0,1,3)^2*y_(2,4,5)^2-2*y_(0,2,3)^2*y_(1,2,5)*y_(3,4,5)-2*y_(0,1,2)*y_(1,2,3)*y_(1,2,5)*y_(3,4,5)+2*y_(0,1,3)*y_(0,2,4)*y_(1,2,5)*y_(3,4,5)-2*y_(0,1,3)*y_(0,1,5)*y_(1,2,5)*y_(3,4,5)-y_(0,2,3)*y_(0,2,4)*y_(0,3,5)*y_(3,4,5)-2*y_(0,1,4)*y_(0,1,5)*y_(0,3,5)*y_(3,4,5)+8*y_(0,1,3)*y_(0,2,5)*y_(0,3,5)*y_(3,4,5)-3*y_(0,1,2)*y_(0,3,5)^2*y_(3,4,5)+2*y_(0,1,2)*y_(0,1,5)*y_(1,3,5)*y_(3,4,5)+y_(0,1,2)*y_(0,2,3)*y_(2,3,5)*y_(3,4,5)-y_(0,1,2)*y_(0,1,4)*y_(2,3,5)*y_(3,4,5)+2*y_(0,2,3)^2*y_(0,4,5)*y_(3,4,5)+y_(0,1,4)^2*y_(0,4,5)*y_(3,4,5)-2*y_(0,1,3)*y_(0,2,4)*y_(0,4,5)*y_(3,4,5)-2*y_(0,1,3)*y_(0,1,5)*y_(0,4,5)*y_(3,4,5)+2*y_(0,1,2)*y_(0,2,3)*y_(1,4,5)*y_(3,4,5)-2*y_(0,1,2)*y_(0,1,4)*y_(1,4,5)*y_(3,4,5)+y_(0,1,2)*y_(0,1,3)*y_(2,4,5)*y_(3,4,5)+y_(0,1,2)^2*y_(3,4,5)^2;
   g := gens ringG25;
   M := transpose sub(last coefficients(F,Monomials=>(gens (ideal vars ring F)^2)),K);
   mm := apply(subsets(6,3),m -> det submatrix(M,m));
   sub(W,apply(20,j -> g_j => mm_j))
);

discriminant = method(TypicalValue => RingElement, Options => {Algorithm => "Poisson"});
    
discriminant RingElement := o -> (G) -> (
    if not (isPolynomialRing ring G) then error "expected a homogeneous polynomial";   
--  if not (isHomogeneous G) then error "expected a homogeneous polynomial";   
    n := numgens ring G;
    d := first degree G;
    a := lift(((d-1)^n - (-1)^n)/d,ZZ);
    resG := resultant(transpose jacobian matrix{{G}},Algorithm=>o.Algorithm);
    try return lift(resG/(d^a),ring resG) else (try (q := first quotientRemainder(resG,d^a); assert(resG == q*d^a); return q) else (<<"--warning: the returned discriminant value is only correct up to a non-zero multiplicative constant"<<endl; return resG;));
);

genericPolynomials = method(TypicalValue => List);
genericPolynomials (VisibleList,Ring) := (D,K) -> (
   D = toList D;
   try assert(ring matrix {D} === ZZ) else error "expected list of integers";
   local vi;
   A := apply(#D,i -> (vi = vars i; K[vi_0..vi_(binomial(#D-1+D_i,D_i)-1)]));
   R := A_0; for i from 1 to #D-1 do R = R**A_i;
   R = K[gens R];
   x := local x; P := R[x_0..x_(#D-1)];
   apply(#D,i -> (sub(vars A_i,R) * transpose gens (ideal vars P)^(D_i))_(0,0) )
); 
genericPolynomials (List) := (D) -> genericPolynomials(D,QQ);

interpolate = method(TypicalValue => RingElement)
interpolate (Matrix,PolynomialRing,ZZ) := (M,R,d) -> (
    --  input: M=matrix{{a_(0,0),...,a_(n,0),b_0},...,{a_(0,N),...,a_(n,N),b_N}}
    --  output: homogeneous polynomial P of degree d s.t. P(a_(0,i),...,a_(n,i))=b_i
    Det := (i,M) -> sum(0..(numgens source M -1), j -> (-1)^(i+j) * M_(i,j) * det submatrix'(M,{i},{j}));
    n := numgens source M -2;   
    N := numgens target M -1;
    K := coefficientRing R;
    mons := gens (ideal gens R)^d;
    if not (isField K and K === ring M and n+1 === numgens R and N+1 === numgens source mons) then error "invalid input data for interpolation";
    S := matrix apply(N+1,i -> first entries ((map(K,R,submatrix(M,{i},0..n))) mons));
    detS := det S; if detS == 0 then error "interpolation failed";
    B := flatten entries submatrix(M,,{n+1})/detS;
    sum(0..N,i -> B_i * Det(i,submatrix'(S,{i..N},)||mons||submatrix'(S,{0..i},)))
);

macaulayFormula = method();
macaulayFormula (Matrix) := (F) -> (
    if numgens target F != 1 then error "expected a matrix with one row";
    if not isPolynomialRing ring F then error "the base ring must be a polynomial ring";
    if numgens source F != numgens ring F then error("the number of variables in the ring must be equal to the number of entries of the matrix, but got " | toString(numgens ring F) | " variables and " | toString(numgens source F) | " entries");
    d := apply(flatten entries F,ee->first degree ee);
    if not (min d > -1 and sum(d) > numgens ring F -1) then error("method not applicable: the degrees got are "|toString(toSequence d));
    MacaulayResultant(F,true)
);
macaulayFormula (List) := (s) -> macaulayFormula matrix {s};

-- sylvesterMatrix = method(TypicalValue => Matrix); -- for now unused
-- sylvesterMatrix (Matrix) := (F) -> (
--     if not (numgens target F == 1 and numgens source F == 2) then error "expected a matrix with one row and two columns or a list of two elements";
--     if not (isPolynomialRing ring F and numgens ring F == 2) then error "the base ring must be a polynomial ring with two variables";
--     standardRes(F,true)
-- );
-- sylvesterMatrix (List) := (s) -> sylvesterMatrix matrix {s};

affineResultant = method(TypicalValue => RingElement, Options => {Algorithm => "Poisson"});
    
affineResultant (Matrix) := o -> (F) -> (
    if numgens target F != 1 then error "expected a matrix with one row";
    if not isPolynomialRing ring F then error "the base ring must be a polynomial ring";
    n := numgens ring F;
    if n+1 != numgens source F then error("the number of polynomials must be one more than the number of variables, but got " | toString(numgens source F) | " polynomials and " | toString(numgens ring F) | " variables");
    K := coefficientRing ring F;
    x := local x;
    An := K[x_1..x_n];
    Pn := K[x_0..x_n];
    F = sub(sub(F,vars An),Pn);
    F' := matrix {apply(flatten entries F,f -> sum(terms f,t -> x_0^((first degree f)-(first degree t)) * t))};
    resultant(F',Algorithm => o.Algorithm)
);

affineResultant (List) := o -> (s) -> affineResultant(matrix{s},Algorithm=>o.Algorithm);

affineDiscriminant = method(TypicalValue => RingElement, Options => {Algorithm => "Poisson"});
    
affineDiscriminant RingElement := o -> (G) -> (
    if not (isPolynomialRing ring G) then error "expected a polynomial";  
    n := numgens ring G;
    K := coefficientRing ring G;
    x := local x;
    An := K[x_1..x_n];
    Pn := K[x_0..x_n];
    G = sub(sub(G,vars An),Pn);
    G' := sum(terms G,t -> x_0^((first degree G) - (first degree t)) * t);
    discriminant(G',Algorithm=>o.Algorithm)
);

----------------------------------------------------------------------------------
---------------------------- chowForms -------------------------------------------
----------------------------------------------------------------------------------

GG := local GG;

Grass = method(TypicalValue => QuotientRing, Options => {Variable => "p"});

Grass (ZZ,ZZ,Ring) := o -> (k,n,KK) -> (
   pp := getVariable o.Variable;
   if not isField KK then error "expected a field";
   if (class GG_(k,n,KK,pp) === QuotientRing or class GG_(k,n,KK,pp) === PolynomialRing) then return GG_(k,n,KK,pp); 
   J := Grassmannian(k,n,CoefficientRing=>KK,Variable=>pp);
   GG_(k,n,KK,pp)=(ring J)/J;
   -- <<"-- created Grassmannian G("<<k<<","<<n<<") over "<<toString(KK)<<" with variable "<<pp<<endl;
   GG_(k,n,KK,pp)
);

Grass (ZZ,ZZ) := o -> (k,n) -> Grass(k,n,QQ,Variable=>o.Variable);

Grass (Ring) := o -> (R) -> ( -- undocumented
   try (k,n,KK,p) := detectGrassmannian R else error "unable to detect Grassmannian ring";
   -- << "-- Grassmannian of "|toString(k)|"-dimensional linear subspaces of PP^"|toString(n)|" over "|toString(KK) << endl;
   (k,n,KK,Variable=>p)
);

SS := local SS;

SegreRing = method(TypicalValue => Sequence, Options => {Variable => "p"});

SegreRing (ZZ,ZZ,Ring) := o -> (k,n,KK) -> (
   pp := getVariable o.Variable;
   if class SS_(k,n,KK,pp) === Sequence then return SS_(k,n,KK,pp); 
   R := KK[pp_(0,0)..pp_(k,n)]; 
   M := genericMatrix(R,n+1,k+1);
   SS_(k,n,KK,pp) = (R,M);
   -- <<"-- created ambient ring of PP^"<<k<<" x PP^"<<n<<" subset PP^"<<(n+1)*(k+1)-1<<" over "<<toString(KK)<<" with variable "<<pp<<endl;
   SS_(k,n,KK,pp)
);

getVariable = method(TypicalValue => Symbol);
getVariable (Symbol) := (x) -> x;
getVariable (String) := (x) -> getSymbol x;
getVariable (IndexedVariableTable) := (x) -> baseName x;
getVariable (IndexedVariable) := (x) -> first x;
getVariable (PolynomialRing) := (R) -> getVariable baseName first gens R;
getVariable (Thing) := (x) -> error "expected symbol or string";

detectGrassmannian = method(TypicalValue => Sequence);
detectGrassmannian (PolynomialRing) := (G) -> ( -- thanks to Federico Galetto 
    -- input: ambient ring of G(k,n) 
    -- output: (k,n)
    k := (length sequence last baseName first gens G) - 1;
    n := last sequence last baseName last gens G;
    if binomial(n+1,k+1) =!= numgens G then error "unable to detect Grassmannian ring";
    return (k,n,coefficientRing G,getVariable G)
);
detectGrassmannian (QuotientRing) := (G) -> (
   (k,n,KK,p) := detectGrassmannian ambient G;
   if sub(ideal Grass(k,n,KK,Variable=>p),vars ambient G) != ideal G then error "unable to detect Grassmannian ring";
   return (k,n,KK,p)
);

duality = method(TypicalValue => RingMap); -- p. 94 [Gelfand, Kapranov, Zelevinsky - Discriminants, resultants, and multidimensional determinants, 1994]

sign = (permutation) -> sub(product(subsets(0..#permutation-1,2),I->(permutation_(I_1)-permutation_(I_0))/(I_1-I_0)),ZZ); -- thanks to Nivaldo Medeiros 
tosequence = (L) -> if #L != 1 then toSequence L else L_0;

duality(PolynomialRing) := (R) -> (  -- returns the map R:=G(k,P^n) ---> G(n-k-1,P^n*)
   (k,n,KK,p) := detectGrassmannian R; 
   G := ambient Grass(k,n,KK,Variable=>p);
   G' := ambient Grass(n-k-1,n,KK,Variable=>p);  
   L := for U in subsets(set(0..n),n-k) list sign( (sort toList(set(0..n)-U)) | sort toList U)  *  (p_(tosequence sort toList(set(0..n)-U)))_G;
   return(map(R,G,vars R) * map(G,G',L));
);

duality(QuotientRing) := (R) -> ( -- returns the map R:=G(k,P^n) ---> G(n-k-1,P^n*)
   (k,n,K,p) := detectGrassmannian R;
   Gkn := Grass(k,n,K,Variable=>p);
   f := map(Gkn,Grass(n-k-1,n,K,Variable=>p),matrix duality ambient Gkn); 
   return(map(R,Gkn,vars R) * f);
);

dualize = method()
dualize(RingElement) := (F) -> (inverse duality ring F) F;
dualize(Matrix) := (F) -> (inverse duality ring F) F;
dualize(Ideal) := (F) -> (inverse duality ring F) F;
dualize(RingMap) := (phi) -> phi*duality(source phi);
dualize(VisibleList) := (L) -> apply(L,dualize);
dualize(Ring) := (R) -> source duality R;

tangentialChowForm = method(TypicalValue => RingElement, Options => {Variable => null, Duality => null, AffineChartGrass => true, AssumeOrdinary => null, AffineChartProj => true, SingularLocus => null}); 

tangentialChowForm (Ideal,ZZ,ZZ) := o -> (I,s,l) -> (
   -- Input: I ideal of X = V(I) subset P^n of dimension d
   -- Output: ideal of a subvariety of GG(n-l-1,P^n*) = GG(l,P^n) 
   if not isPolynomialRing ring I then error "expected ideal in a polynomial ring";
   if not isHomogeneous I then error "expected a homogeneous ideal";
   if s<0 then error "expected a nonnegative integer";
   p := if o.Variable === null then getVariable ring I else getVariable o.Variable;
   K := coefficientRing ring I; 
   n := numgens ring I -1;
   d := dim I -1;
   useDuality := o.Duality;
   if useDuality === null then useDuality = n-l <= l+1;
   if class useDuality =!= Boolean then error "expected true or false for option Duality";
   r := if useDuality then n-l-1 else l; 
   if l >= n or l <=-1 then return 1_(Grass(l,n,K,Variable=>p));
   mnr := o.AffineChartGrass;
   if mnr === true then mnr = (random toList(0..n))_{0..r};
   if mnr =!= false then (try assert(ring matrix{mnr} === ZZ and min mnr >=0 and max mnr <=n and # unique mnr == r+1 and # mnr == r+1) else error("bad value for option AffineChartGrass: expected either boolean value or list of "|toString(r+1)|" distinct integers beetween 0 and "|toString(n))); 
   if mnr =!= false then mnr = sort mnr; 
   if (class o.AssumeOrdinary =!= Boolean and o.AssumeOrdinary =!= null) then error "expected true or false for option AssumeOrdinary";
   limitNumGens := if (o.AssumeOrdinary === true or (o.AssumeOrdinary === null and s==0 and l==n-d-1+s)) then 1 else infinity;  
   x := local x; t := local t; u := local u;
   R := if useDuality then K[x_0..x_n,u_(0,0)..u_(r,n),MonomialOrder=>Eliminate(n+1)] else K[x_0..x_n,t_0..t_r,u_(0,0)..u_(r,n),MonomialOrder=>Eliminate(n+r+2)];
   U := genericMatrix(R,u_(0,0),n+1,r+1);
   if useDuality then U = transpose U; 
   Ru := K[u_(0,0)..u_(r,n)]; 
   if mnr =!= false then (
      if useDuality then (
         R = K[x_0..x_n,flatten entries submatrix'(U,mnr),MonomialOrder=>Eliminate(n+1)];
         Ru = K[flatten entries submatrix'(U,mnr)];
         U = sub(sub(U,flatten for i to r list for j to r list U_(i,mnr_j) => (if i == j then 1 else 0)),R)
      ) else (
         R = K[x_0..x_n,t_0..t_r,flatten entries submatrix'(transpose U,mnr),MonomialOrder=>Eliminate(n+r+2)];
         Ru = K[flatten entries submatrix'(transpose U,mnr)];
         U = sub(sub(U,flatten for i to r list for j to r list U_(mnr_i,j) => (if i == j then 1 else 0)),R)
      );
   );
   Sub := map(R,ring I,{x_0..x_n});
   Sub' := map(Ru,R,matrix{toList((numgens R - numgens Ru):0_K)} | vars Ru); 
   Irr := ideal(x_0..x_n);
   Inc := if useDuality then (Sub I)+ideal(U*transpose(gens Irr)) else (Sub I)+ideal(U*transpose(matrix{toList(t_0..t_r)})-transpose(gens Irr));
   if s>0 then (
       J := transpose jacobian I; 
       singLocus := if o.SingularLocus === null then trim(I+minors(n-d,J)) else o.SingularLocus;
       if not(isIdeal singLocus and ring singLocus === ring I and isHomogeneous singLocus) then error("bad value for option SingularLocus: expected the ideal of the singular locus");
       if dim singLocus -1 >= 0 then Irr = Sub singLocus; 
       J = Sub J; 
       Inc = if useDuality then Inc+minors(n-s+1,J||U) else Inc+minors(r-s+1,J*U)
   ); 
   jj := o.AffineChartProj;
   if jj === true then jj = random(n+1); 
   if jj =!= false then (try assert(class jj === ZZ and jj >=0 and jj <=n) else error("bad value for option AffineChartProj: expected either boolean value or integer beetween 0 and "|toString(n))); 
   if s>0 and Irr =!= ideal(x_0..x_n) then jj = false;
   if jj === false then Inc = saturate(Inc,Irr) else ( 
       tt := x_jj;
       VarsR := flatten entries submatrix'(vars R,{jj});
       R = if useDuality then K[VarsR,MonomialOrder=>Eliminate n] else K[VarsR,MonomialOrder=>Eliminate (n+r+1)]; 
       Inc = sub(sub(Inc,tt=>1),R); 
       U = sub(U,R);
       Sub' = map(Ru,R,submatrix'(matrix Sub',{jj}))
   );
   Z := trim Sub' ideal selectInSubring(1,gens gb(Inc,SubringLimit=>limitNumGens)); 
   G := Grass(r,n,K,Variable=>p);
   f := map(Ru/Z,G,gens minors(r+1,Sub' U));
   if mnr =!= false then limitNumGens = infinity; 
   Zs := kernel(f,SubringLimit=>limitNumGens);
   if mnr =!= false then Zs = homogenize(Zs,(p_(tosequence mnr))_G); 
   if useDuality then Zs = dualize Zs;
   Zs = trim Zs;
   if Zs == 1 and (o.AffineChartGrass =!= false or o.AffineChartProj =!= false) then (
        -- <<"-- rerun tangentialChowForm() with options AffineChartGrass=>false, AffineChartProj=>false... \n";
        return tangentialChowForm(I,s,l,Variable=>o.Variable,Duality=>o.Duality,AffineChartGrass=>false,AssumeOrdinary=>o.AssumeOrdinary,AffineChartProj=>false,SingularLocus=>o.SingularLocus);
   ); 
   -- <<"-- subvariety of GG("<<l<<","<<n<<") like CH_"<<s<<", expected dimension: "<<toString(-s^2+(d-n+l)*s+n*l-l^2+d)<<", real dimension: "<<dim Zs -1 <<", codimension in Grass: "<<codim Zs; (if l == n-d-1+s then <<" (classical associated subvariety)"); <<endl;
   if numgens Zs == 1 then Zs_0 else Zs
);

tangentialChowForm (Ideal,ZZ) := o -> (I,s) -> tangentialChowForm(I,s,codim I -1 + s,Variable=>o.Variable,Duality=>o.Duality,AffineChartGrass=>o.AffineChartGrass,AssumeOrdinary=>o.AssumeOrdinary,AffineChartProj=>o.AffineChartProj,SingularLocus=>o.SingularLocus);

chowForm = method(TypicalValue => RingElement, Options => {Variable => null, Duality => null, AffineChartGrass => true, AffineChartProj => true}); 

chowForm (Ideal) := o -> (I) -> tangentialChowForm(I,0,Variable=>o.Variable,Duality=>o.Duality,AffineChartGrass=>o.AffineChartGrass,AffineChartProj=>o.AffineChartProj);

chowForm (RingMap) := o -> (phi) -> ( -- undocumented
   -- Input: an isomorphism phi:P^r ---> X \subset P^n
   -- Output: form defining the hypersurface Z_0(X) subset GG(r,P^n*) = GG(n-r-1,P^n)
   if not isPolynomialRing target phi then error "the target of the ring map needs to be a polynomial ring";
   if not ((isPolynomialRing source phi or isQuotientRing source phi) and isPolynomialRing ambient source phi and isHomogeneous ideal source phi) then error("the source of the ring map needs to be either a polynomial ring or a quotient of a polynomial ring by a homogeneous ideal");
   K := coefficientRing ambient source phi; 
   if not (K === coefficientRing target phi) then error "different coefficient rings encountered";
   if not isField K then error "the coefficient ring needs to be a field";
   F := submatrix(matrix phi,{0..(numgens source phi -1)});
   if not (isHomogeneous ideal F and # unique degrees ideal F == 1) then error "the map needs to be defined by homogeneous polynomials of the same degree";
   p := if o.Variable === null then getVariable ambient source phi else getVariable o.Variable;
   if (o.AffineChartGrass =!= true or o.AffineChartProj =!= true or o.Duality =!= null) then error "option not available with chowForm(RingMap); you can use it with chowForm(Ideal)"; 
   n := numgens ambient source phi -1;
   r := numgens target phi -1;
   x := local x; u := local u;
   R := K[x_1..x_r,u_(0,0)..u_(r,n-r-1),MonomialOrder=>Eliminate r];
   U := (transpose genericMatrix(R,u_(0,0),n-r,r+1)) | sub(matrix for i to r list for j to r list if i == j then 1 else 0,R);
   F = (map(R,target phi,{1,x_1..x_r})) transpose F;
   Ru := K[u_(0,0)..u_(r,n-r-1)];
   Z := sub(ideal selectInSubring(1,gens gb(ideal(U*F),SubringLimit=>1)),Ru); 
   G := Grass(r,n,K,Variable=>p);
   f := map(Ru/Z,G,gens minors(r+1,sub(U,Ru)));
   Zs := kernel(f,SubringLimit=>2);
   Zs = trim dualize homogenize(Zs,(p_((n-r)..n))_G); 
   if numgens Zs == 1 then Zs_0 else Zs
);

hurwitzForm = method(TypicalValue => RingElement, Options => {Variable => null, Duality => null, AffineChartGrass => true, AffineChartProj => true, SingularLocus => null}); 

hurwitzForm (Ideal) := o -> (I) -> (
   I = trim I;
   if unique degrees I === {{1}} then error "expected a non-linear ideal";
   tangentialChowForm(I,1,Variable=>o.Variable,Duality=>o.Duality,AffineChartGrass=>o.AffineChartGrass,AffineChartProj=>o.AffineChartProj,AssumeOrdinary=>true,SingularLocus=>o.SingularLocus)
);

cayleyTrick = method (Options => {Variable => null,Duality => false});

cayleyTrick (Ideal,ZZ) := o -> (I,b) -> (
   -- input: ideal I of a subvariety X subset P^n, b integer s.t. def X <= b <= dim X
   -- output: (P^b x X),(P^b x X)^*
   if not isPolynomialRing ring I then error "expected ideal in a polynomial ring";
   if not isHomogeneous I then error "expected a homogeneous ideal";
   if b < 0 and b != -190181 then error "expected a nonnegative integer";
   if o.Duality then (I' := dualVariety I; d' := codim I' -1) else d := dim I -1;
   if b == -190181 then (if o.Duality then b = d' else b = d);
   a := if o.Duality
             then b - d' -- a := b - def(X)
             else d - b; -- a := dim(X) - b
   if a < 0 then error "integer outside the allowed range";
   n := numgens ring I -1;
   K := coefficientRing ring I;
   (R,M) := SegreRing(b,n,K,Variable=>(if o.Variable === null then getVariable ring I else getVariable o.Variable));
   t := local t; y := local y;
   PbPn := K[y_0..y_b,t_0..t_n];
   s := map(PbPn,R,flatten for i to b list for j to n list y_i*t_j);
   W := trim preimage(s,(map(PbPn,ring I,{t_0..t_n})) I);
   Z := if o.Duality then ideal tangentialChowForm(I',a) else dualize ideal tangentialChowForm(I,a);
   f := map(R,ring Z,gens minors(b+1,M));
   (W,f Z)
);

cayleyTrick (Ideal) := o -> (I) -> cayleyTrick(I,-190181,Variable=>o.Variable,Duality=>o.Duality); -- undocumented

chowEquations = method(TypicalValue => Ideal, Options => {Variable => null});

chowEquations (RingElement) := o -> (W) -> ( 
   (k,n,KK,x) := detectGrassmannian ambient ring W;
   k = n-k-1;   -- W in G(n-k-1,n), k=dim X 
   if o.Variable =!= null then x = getVariable o.Variable;
   Pn := Grass(0,n,KK,Variable=>x);
   s := local s; 
   R := Pn[s_(0,0)..s_(n-k-2,n)];   
   M := if n-k-1>0 then transpose(genericMatrix(R,n+1,n-k-1))||sub(vars Pn,R) else sub(vars Pn,R);
   mm := minors(n-k,M);
   pp := gens ring W;
   P := sub(W,for i to #pp -1 list pp_i => mm_i);
   trim sub(ideal last coefficients matrix{{P}},Pn)
);

chowEquations (RingElement,ZZ) := o -> (W,s) -> ( -- undocumented  
   -- recover the variety X from its tangential Chow form W=Z_s(X)
   W = dualize W;
   (k,n,KK,x) := detectGrassmannian ring W;
   k = k+s;   -- W in G(k-s,n), k=dim X 
   if not (s>=0 and s<=k) then error("wrong integer value for second argument of chowEquations");
   if o.Variable =!= null then x = getVariable o.Variable;
   Pn := Grass(0,n,KK,Variable=>x);
   psi := first projectionMap(ring W,AffineChartGrass=>false);
   Z := psi W;
   f := map((ring Z)/Z,ring Z,transpose jacobian matrix Z) * map(ring Z,Pn,submatrix(vars ring Z,0..n));
   trim kernel f
);

conormalVariety = method(TypicalValue => Ideal, Options => {Variable => null, Strategy => "Saturate", SingularLocus => null});

conormalVariety (Ideal,Matrix) := o -> (I,D) -> ( 
   if not isPolynomialRing ring I then error "expected ideal in a polynomial ring";
   if not isHomogeneous I then error "expected a homogeneous ideal";
   if ring D === ZZ then D = sub(D,coefficientRing ring I);
   if not(numgens target D == numgens source D and numgens target D === numgens ring I and ring D === coefficientRing ring I) then error("expected a square matrix of order "|toString(numgens ring I)|" over "|toString(coefficientRing ring I));
   Sing := o.SingularLocus;
   if Sing =!= null then (
       if not(isIdeal Sing and ring Sing === ring I and isHomogeneous Sing) then error("bad value for option SingularLocus: expected the ideal of the singular locus");
       if (numgens Sing == 1 and Sing == 1) then Sing = ideal vars ring I;
   );
   C := if o.Strategy === "Eliminate" then conormalVarietyElim(I,D) else if o.Strategy === "Saturate" then conormalVarietySat(I,D,SingularLocus=>Sing) else error "bad value for option Strategy; possible values are \"Eliminate\" and \"Saturate\"";
   R := first SegreRing(1,numgens ring I -1,coefficientRing ring I,Variable=>(if o.Variable === null then getVariable ring I else getVariable o.Variable));
   R = newRing(R,Degrees=>{(numgens ring I):{1,0},(numgens ring I):{0,1}});
   sub(C,vars R)
);

conormalVariety (Ideal) := o -> (I) -> ( 
   n := numgens ring I -1;
   D := diagonalMatrix toList(n+1:1);
   conormalVariety(I,D,Variable=>o.Variable,Strategy=>o.Strategy,SingularLocus=>o.SingularLocus)
);

conormalVarietyElim = method(TypicalValue => Ideal);
conormalVarietyElim (Ideal,Matrix) := (I,D) -> (
   F := gens trim I;
   K := coefficientRing ring F;
   (u,a,b) := (local u,local a,local b);
   n := numgens ring F -1;
   m := numgens source F -1;
   Ru := K[u_0..u_m,a_0..a_n,b_0..b_n,MonomialOrder => Eliminate (m+1)];
   s := map(Ru,ring F,{a_0..a_n});
   A := ideal(s F) + ideal(matrix{toList(b_0..b_n)} * D - matrix{toList{u_0..u_m}} * (s transpose jacobian F));
   R := K[a_0..a_n,b_0..b_n,Degrees=>{(n+1):{1,0},(n+1):{0,1}}];
   trim sub(ideal selectInSubring(1,gens gb A),R)
);

conormalVarietySat = method(TypicalValue => Ideal, Options => {SingularLocus => null});
conormalVarietySat (Ideal,Matrix) := o -> (I,D) -> (
   F := gens trim I;
   K := coefficientRing ring F;
   (a,b) := (local a,local b);
   n := numgens ring F -1;
   m := numgens source F -1;
   R := K[a_0..a_n,b_0..b_n,Degrees=>{(n+1):{1,0},(n+1):{0,1}}];
   s := map(R,ring F,{a_0..a_n});
   J := s transpose jacobian F;
   c := codim ideal F;
   Sing := if o.SingularLocus === null then trim minors(c,J) else s(o.SingularLocus);
   saturate(ideal(s F) + minors(c+1,(matrix{toList(b_0..b_n)} * D) || J),Sing)
);

dualVariety = method(TypicalValue => Ideal, Options => {AssumeOrdinary => false, Strategy => null, SingularLocus => null}); 

dualVariety (Ideal) := o -> (I) -> (
   if o.Strategy =!= null then (
      C := conormalVariety(I,Strategy=>o.Strategy,SingularLocus=>o.SingularLocus);
      n := numgens ring I -1;
      R := newRing(ring C,MonomialOrder=>Eliminate (n+1));
      C = sub(C,vars R);
      s := map(ring I,R,matrix{toList(n+1:0)}|(vars ring I));
      return trim s ideal selectInSubring(1,gens gb(C,SubringLimit=>if o.AssumeOrdinary === true then 1 else infinity))
   );
   I' := dualize tangentialChowForm(I,dim I -1,AssumeOrdinary=>o.AssumeOrdinary,AffineChartGrass=>(not o.AssumeOrdinary),SingularLocus=>o.SingularLocus);
   if not isIdeal I' then I' = ideal I';
   sub(I',vars ring I)
);

dualVariety (RingMap) := o -> (phi) -> ( -- undocumented 
   -- Input: an isomorphism phi:P^n ---> X \subset P^m 
   -- Output: the dual variety of X 
   if not isPolynomialRing target phi then error "the target of the ring map needs to be a polynomial ring";
   if not ((isPolynomialRing source phi or isQuotientRing source phi) and isPolynomialRing ambient source phi and isHomogeneous ideal source phi) then error "the source of the ring map needs to be either a polynomial ring or a quotient of a polynomial ring by a homogeneous ideal";
   K := coefficientRing ambient source phi; 
   if not (K === coefficientRing target phi) then error "different coefficient rings encountered";
   if not isField K then error "the coefficient ring needs to be a field";
   F := submatrix(matrix phi,{0..(numgens source phi -1)});
   if not (isHomogeneous ideal F and # unique degrees ideal F == 1) then error "the map needs to be defined by homogeneous polynomials of the same degree";
   if (class o.AssumeOrdinary =!= Boolean and o.AssumeOrdinary =!= null) then error "expected true or false for option AssumeOrdinary";
   n := numgens target phi -1;
   m := numgens ambient source phi -1;
   x := local x; y := local y;
   R := K[x_1..x_n,y_0..y_m,MonomialOrder=>Eliminate n];
   jacF := (map(R,target phi,{1,x_1..x_n})) transpose jacobian F;
   J := ideal(matrix{{y_0..y_m}} * jacF);
   gbJ := if o.AssumeOrdinary === true then gb(J,SubringLimit=>1) else gb J;
   sub(sub(ideal selectInSubring(1,gens gbJ),K[y_0..y_m]),vars ambient source phi)
);

projectionMap = method(Options => {Variable => null, AffineChartGrass => true});

projectionMap (Ring,Boolean) := o -> (G,B) -> (
   (k,n,KK,p) := detectGrassmannian G;
   if o.Variable =!= null then p = getVariable o.Variable;
   (R,M) := SegreRing(k,n,KK,Variable=>p);
   psi := map(R,G,gens minors(k+1,M));
   mnr := o.AffineChartGrass;
   if mnr === false then return (psi,M);
   if mnr === true then mnr = (random toList(0..n))_{0..k};
   try assert(ring matrix{mnr} === ZZ and min mnr >=0 and max mnr <=n and # unique mnr == k+1 and # mnr == k+1) else error("bad value for option AffineChartGrass: expected either boolean value or list of "|toString(k+1)|" distinct integers beetween 0 and "|toString(n)); 
   mnr = sort mnr; 
   R = KK[flatten entries submatrix'(transpose M,mnr)];
   M = sub(sub(M,flatten for i to k list for j to k list M_(mnr_i,j) => (if i == j then 1 else 0)),R);
   psi = map(R,G,gens minors(k+1,M));
   if B then (psi,submatrix'(transpose M,mnr)) else (psi,M)
);

projectionMap (Ring) := o -> (G) -> projectionMap(G,true,Variable=>o.Variable,AffineChartGrass=>o.AffineChartGrass);

isCoisotropic = method(TypicalValue => Boolean, Options => {AffineChartGrass => true})

isCoisotropic (RingElement) := o -> (F) -> (
   if o.AffineChartGrass === false then error "value not allowed for option AffineChartGrass";
   (psi,M) := projectionMap(ring F,AffineChartGrass=>o.AffineChartGrass);
   (m,n) := (numgens target M -1,numgens source M -1);
   f := psi F;
   J := matrix for i to m list for j to n list diff(M_(i,j),f);
   sub(minors(2,J),(ring J)/f) == 0
);

fromPluckerToStiefel = method(Options => {Variable => null, AffineChartGrass => false})

fromPluckerToStiefel (RingElement) := o -> (I) -> (
   f := first projectionMap(ring I,Variable=>o.Variable,AffineChartGrass=>o.AffineChartGrass);
   f I
);

fromPluckerToStiefel (Matrix) := o -> (I) -> (
   f := first projectionMap(ring I,Variable=>o.Variable,AffineChartGrass=>o.AffineChartGrass);
   f I
);

fromPluckerToStiefel (Ideal) := o -> (I) -> (
   f := first projectionMap(ring I,Variable=>o.Variable,AffineChartGrass=>o.AffineChartGrass);
   saturate(f I,ideal matrix f)
);

veronese = method(TypicalValue => RingMap, Options => {Variable => null});
veronese (ZZ,ZZ,Ring) := o -> (n,d,K) -> (
   if n<=0 or d<=0 then error "expected positive integers";
   if not isField K then error "expected a field";
   t := "t"; x := "x";
   if class o.Variable === Sequence then if # o.Variable == 2 then (t = getVariable first o.Variable; x = getVariable last o.Variable) else error "expected a symbol";
   if o.Variable =!= null and class o.Variable =!= Sequence then (t = getVariable o.Variable; x = t);
   Pn := Grass(0,n,K,Variable=>t);
   PN := Grass(0,binomial(n+d,d)-1,K,Variable=>x);
   F := gens (ideal vars Pn)^d;
   map(Pn,PN,F)
);
veronese (ZZ,ZZ) := o -> (n,d) -> veronese(n,d,QQ,Variable=>o.Variable);

isInCoisotropic = method(TypicalValue => Boolean, Options => {Duality => null, SingularLocus => null}); 

isInCoisotropic (Ideal,Ideal) := o -> (L,I) -> (
   -- Input 1: L ideal of a linear subvariety of P^n of dimension l
   -- Input 2: I ideal of X = V(I) subset P^n of dimension d and codimension c
   -- Output: a Boolean value, whether L corresponds to a point of Z_s(X) subset GG(l,P^n) = GG(c-1+s,P^n) with s = l-c+1
   R := ring I;
   K := coefficientRing R; 
   if R =!= ring L then error "expected same ring";
   if not isPolynomialRing R then error "expected ideals in a polynomial ring";
   if not (isHomogeneous I and isHomogeneous L) then error "expected homogeneous ideals";
   if not unique degrees L == {{1}} then error "the first argument must be an ideal generated by linear forms";
   n := numgens R -1;
   c := codim I;
   d := n - c;
   l := dim L -1;
   s := l - c + 1;
   useDuality := o.Duality;
   if useDuality === null then useDuality = binomial(n+1,n-s+1) * binomial(numgens I + numgens L,n-s+1) <= binomial(numgens I,n-d) * binomial(l+1,n-d);
   if class useDuality =!= Boolean then error "expected true or false for option Duality";
   P := I + L;
   if s>0 then (
       U := transpose sub(last coefficients(gens L,Monomials=>vars R),K);
       if not useDuality then U = mingens kernel U;
       J := transpose jacobian I; 
       P = if useDuality then P + minors(n-s+1,J||U) else P + minors(n-d,J*U);  
       singLocus := if o.SingularLocus === null then trim(I+minors(c,J)) else o.SingularLocus;
       if not(isIdeal singLocus and ring singLocus === ring I and isHomogeneous singLocus) then error("bad value for option SingularLocus: expected the ideal of the singular locus");
       if dim (singLocus+P) -1 >= 0 then P = saturate(P,singLocus); 
   ); 
   dim P >= 1
);

plucker = method(TypicalValue => Ideal, Options => {Variable => null, AffineChartGrass => true})

plucker (Matrix) := o -> (M) -> ( -- undocumented
   k := numRows M -1;
   n := numColumns M -1;
   Gr := Grass(k,n,ring M,Variable=>if o.Variable === null then "p" else o.Variable);
   trim sub(minors(2,(vars ambient Gr)||matrix{apply(subsets(n+1,k+1),m -> det submatrix(M,m))}),Gr)
);

plucker (Ideal) := o -> (I) -> (
   if class ring I === QuotientRing then return varietySweptOutByLinearSpaces(I,Variable=>o.Variable,AffineChartGrass=>o.AffineChartGrass);
   if isPolynomialRing ring I then (
       if not isHomogeneous I then error "expected a homogeneous ideal";
       if unique degrees I =!= {{1}} then error "expected an ideal generated by linear forms (you could try using 'plucker(Ideal,ZZ)')";
       x := if o.Variable === null then getVariable ambient ring I else getVariable o.Variable;
       K := coefficientRing ring I;
       A := transpose mingens kernel transpose sub(last coefficients(gens I,Monomials=>gens ring I),K);
       return plucker(A,Variable=>x)
   ) else error "expected the ideal of a subvariety of a Grassmannian Grass(k,n) or of a k-dimensional linear subspace of P^n";
);

plucker (Ideal,ZZ) := o -> (I,k) -> fanoVariety(I,k,Variable=>o.Variable,AffineChartGrass=>o.AffineChartGrass);

varietySweptOutByLinearSpaces = method(Options => {Variable => null, AffineChartGrass => true});

varietySweptOutByLinearSpaces (Ideal) := o -> (I) -> (
   I' := dualize I;
   G := ring I';
   (k,n,KK,p) := detectGrassmannian G;
   x := if o.Variable === null then p else getVariable o.Variable;
   (f,M) := projectionMap(G,false,Variable=>x,AffineChartGrass=>o.AffineChartGrass);
   u := local u;
   R := KK[gens target f,u_0..u_n,MonomialOrder=>Eliminate (numgens target f)];
   J := sub(f(I'),R) + ideal(sub(transpose M,R)*(transpose matrix{{u_0..u_n}}));
   if o.AffineChartGrass === false then J = saturate(J,sub(ideal submatrix(matrix f,{0..(numgens source f -1)}),R));
   W := ideal selectInSubring(1,gens gb J); 
   W = trim sub(sub(W,KK[u_0..u_n]),vars Grass(0,n,KK,Variable=>x));
   if o.AffineChartGrass === true then if numgens W == 1 and W == 1 and dim I > 0 then (
       <<"-- rerunning 'plucker' using another random chart on the Grassmannian ('AffineChartGrass => true')"<<endl;
       return varietySweptOutByLinearSpaces(I,Variable=>x,AffineChartGrass=>true);
   );
   W
);

fanoVariety = method(Options => {Variable => null, AffineChartGrass => true});

fanoVariety (Ideal,ZZ) := o -> (I,k) -> (
   if not isPolynomialRing ring I then error "expected ideal in a polynomial ring";
   if not isHomogeneous I then error "expected a homogeneous ideal";
   n := numgens ring I -1;
   K := coefficientRing ring I;
   p := if o.Variable === null then getVariable ring I else getVariable o.Variable;
   G := Grass(k,n,K,Variable=>p);
   mnr := o.AffineChartGrass;
   if mnr === true then mnr = (random toList(0..n))_{0..k};
   (f,M) := projectionMap(G,false,Variable=>"fano",AffineChartGrass=>mnr);
   t := local t;
   R := (target f)[t_0..t_k];
   E := trim ideal sub(last coefficients gens sub(I,apply(gens ring I,flatten entries(M * transpose vars R),(g,m) -> g => m)),target f);
   f' := map((target f)/E,source f,submatrix(matrix f,{0..(numgens source f -1)}));
   F := kernel f';
   if mnr =!= false then F = homogenize(F,(p_(tosequence sort mnr))_G); 
   F = trim F;
   -- if o.AffineChartGrass === true then if numgens F == 1 and F == 1 then (<<"--warning: calculation performed using a random chart on the Grassmannian ('AffineChartGrass => "|toString(sort mnr)|"')"<<endl);
   F
);

----------------------------------------------------------------------------------
---------------------------- Documentation ---------------------------------------
----------------------------------------------------------------------------------
     
beginDocumentation() 
document { 
    Key => Resultants, 
    Headline => "resultants, discriminants, and Chow forms", 
    PARA{"This package provides methods to deal with resultants and discriminants of multivariate polynomials, and with higher associated subvarieties of irreducible projective varieties. The main methods are: ", TO "resultant",", ",TO "discriminant",", ", TO "chowForm",", ",TO "dualVariety",", and ",TO "tangentialChowForm",". For the mathematical theory, we refer to the following two books: ", HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"},", by David A. Cox, John Little, Donal O'shea; ", HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky. Other references for the theory of Chow forms are: ", HREF{"https://projecteuclid.org/euclid.dmj/1077305197","The equations defining Chow varieties"}, ", by M. L. Green and I. Morrison; ", HREF{"http://link.springer.com/article/10.1007/BF02567693","Multiplicative properties of projectively dual varieties"},", by J. Weyman and A. Zelevinsky; and the preprint ",HREF{"https://arxiv.org/abs/1607.05932","Coisotropic Hypersurfaces in the Grassmannian"}, ", by K. Kohn."},
}
document { 
    Key => {[resultant,Algorithm],[discriminant,Algorithm],[affineResultant,Algorithm],[affineDiscriminant,Algorithm]}, 
    Usage => "Algorithm => \"Poisson\"/\"Macaulay\"/\"Poisson2\"/\"Macaulay2\"", 
    PARA{"This is an option that determines which algorithm will be used to compute the ",TO resultant,". There are currently four algorithms implemented: "},
    PARA{TT "\"Poisson\""," (default) the resultant is computed, recursively, through the Poisson Formula (see [1, Theorem 3.4]);"},
    PARA{TT "\"Macaulay\""," the resultant is computed as a Macaulay resultant, i.e. as a ratio of two determinants (see [1, Theorem 4.9]);"},
    PARA{TT "\"Poisson2\""," and ",TT "\"Macaulay2\""," these are variants of the above ones using interpolation of multivariate polynomials."},
    PARA{"[1] David A. Cox, John Little, Donal O'shea - ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"}, ", Graduate Texts in Mathematics, Volume 185 (2005)."}, 
    EXAMPLE {
        "R = QQ[a,b][x,y,z,w]",
        "F = {(7/3)*x+(7/2)*y+z+2*w, ((10/7)*a+b)*x^2+(a+(5/4)*b)*x*y+(2*a+(1/2)*b)*y^2+((7/8)*a+(7/5)*b)*x*z+((3/4)*a+b)*y*z+((7/8)*a+(1/7)*b)*z^2+((5/7)*a+(4/3)*b)*x*w+(9*a+10*b)*y*w+((7/5)*a+(3/4)*b)*z*w+((4/3)*a+5*b)*w^2, ((1/2)*a+(7/5)*b)*x^3+((1/2)*a+10*b)*x^2*y+((8/9)*a+(3/5)*b)*x*y^2+(a+(7/6)*b)*y^3+((3/7)*a+(3/4)*b)*x^2*z+((1/3)*a+(9/10)*b)*x*y*z+((9/4)*a+b)*y^2*z+((1/6)*a+(1/5)*b)*x*z^2+(3*a+(5/2)*b)*y*z^2+((5/3)*a+(3/7)*b)*z^3+(a+b)*x^2*w+((4/5)*a+(5/4)*b)*x*y*w+((5/3)*a+(5/8)*b)*y^2*w+((3/2)*a+(1/6)*b)*x*z*w+((1/3)*a+(4/5)*b)*y*z*w+(9*a+(1/3)*b)*z^2*w+((7/3)*a+(5/4)*b)*x*w^2+(a+(3/4)*b)*y*w^2+((9/8)*a+(7/8)*b)*z*w^2+((9/7)*a+2*b)*w^3, 2*x+(1/4)*y+(8/3)*z+(4/5)*w}",
        "time resultant(F,Algorithm=>\"Poisson2\")",
        "time resultant(F,Algorithm=>\"Macaulay2\")",
        "time resultant(F,Algorithm=>\"Poisson\")",
        "time resultant(F,Algorithm=>\"Macaulay\")",
         "assert(o3 == o4 and o4 == o5 and o5 == o6)"
    },
    SeeAlso => {discriminant,resultant}
} 
document { 
    Key => {resultant,(resultant,Matrix),(resultant,List)}, 
    Headline => "multipolynomial resultant", 
    Usage => "resultant F", 
    Inputs => { "F" => Matrix => {"a row matrix whose entries are ", TEX///$n+1$///," homogeneous polynomials ", TEX///$F_0,\ldots,F_n$///," in ", TEX///$n+1$///," variables (or a ", TO List," to be interpreted as such a matrix)"}}, 
    Outputs => {{"the resultant of ",TEX///$F_0,\ldots,F_n$///}}, 
    PARA{"Let ",TEX///$F_0,\ldots,F_n$///," be ",TEX///$n+1$///," homogeneous polynomials in ",TEX///$n+1$///," variables ",TEX///$x_0,\ldots,x_n$///," over a commutative ring ",TEX///$K$///,". The resultant ",TEX///$R(F_0,\ldots,F_n)$///," is a certain polynomial in the coefficients of ",TEX///$F_0,\ldots,F_n$///,"; when ",TEX///$K$///," is an algebraically closed field, ",TEX///$R(F_0,\ldots,F_n)$///," vanishes if and only if ",TEX///$F_0,\ldots,F_n$///," have a common nontrivial root.  For the general theory, see one of the following: ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"},", by David A. Cox, John Little, Donal O'shea; ", HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky."},
    EXAMPLE { 
    "ZZ[t,u][x,y,z]",
    "F = {x^2+3*t*y*z-u*z^2,(t+3*u-1)*x-y,-t*x*y^3+t*x^2*y*z+u*z^4}",
    "time resultant F"
    },
    PARA{"Below we compute the general expression of the resultant of a linear, a quadratic and a cubic form on ",TEX///$\mathbb{P}^2$///,"."},
    EXAMPLE {
    "F = genericPolynomials({1,2,3},ZZ)",
    "time resultant F",
    },
    PARA{"On page 88 of ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"}," is stated that the resultant of three ternary quadrics, written out in its full glory, has 21.894 terms."},
    EXAMPLE {
    "F = genericPolynomials({2,2,2},ZZ)",
    "time # terms resultant F"
    },
    SeeAlso => {chowForm,discriminant} 
}
document { 
    Key => {discriminant,(discriminant,RingElement)}, 
    Headline => "resultant of the partial derivatives", 
    Usage => "discriminant F", 
    Inputs => { "F" => RingElement => {"a homogeneous polynomial"}}, 
    Outputs => {{"the discriminant of ",TT "F"}}, 
    PARA{"The discriminant of a homogeneous polynomial is defined, up to a scalar factor, as the ",TO resultant," of its partial derivatives. For the general theory, see one of the following: ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"},", by David A. Cox, John Little, Donal O'shea; ", HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky."},
    EXAMPLE { 
    "ZZ[a,b,c][x,y]; F = a*x^2+b*x*y+c*y^2",
    "time discriminant F",
    "ZZ[a,b,c,d][x,y]; F = a*x^3+b*x^2*y+c*x*y^2+d*y^3",
    "time discriminant F",
    },
    PARA{"The next example illustrates how computing the intersection of a pencil generated by two degree ",TEX///$d$///," forms ",TEX///$F(x_0,\ldots,x_n), G(x_0,\ldots,x_n)$///," with the discriminant hypersurface in the space of forms of degree ",TEX///$d$///," on ",TEX///$\mathbb{P}^n$///},
    EXAMPLE {
    "x=symbol x; R=ZZ/331[x_0..x_3]",
    "F=x_0^4+x_1^4+x_2^4+x_3^4",
    "G=x_0^4-x_0*x_1^3-x_2^4+x_2*x_3^3",
    "R'=ZZ/331[t_0,t_1][x_0..x_3];",
    "pencil=t_0*sub(F,R')+t_1*sub(G,R')",
    "time D=discriminant pencil",
    "factor D"
    },
    SeeAlso => {dualVariety,resultant} 
}
document { 
    Key => {affineResultant,(affineResultant,Matrix),(affineResultant,List)}, 
    Headline => "affine resultant", 
    Usage => "affineResultant f", 
    Inputs => { "f" => Matrix => {"a row matrix whose entries are ", TEX///$n+1$///," polynomials ", TEX///$f_0,\ldots,f_n$///," in ", TEX///$n$///," variables (or a ", TO List," to be interpreted as such a matrix)"}}, 
    Outputs => {{"the resultant of the polynomials obtained by homogenizing ",TEX///$f_0,\ldots,f_n$///," with respect to a new variable"}}, 
    EXAMPLE { 
    "ZZ[t,u][y,z]",
    "f = {3*t*y*z-u*z^2+1, -y+t+3*u-1, u*z^4-t*y^3+t*y*z}",
    "affineResultant f"
    },
    SeeAlso => {resultant,affineDiscriminant} 
}
document { 
    Key => {affineDiscriminant,(affineDiscriminant,RingElement)}, 
    Headline => "affine discriminant", 
    Usage => "affineDiscriminant f", 
    Inputs => { "f" => RingElement => {"a polynomial"}}, 
    Outputs => {{"the discriminant of the polynomial obtained by homogenizing ",TT "f"," with respect to a new variable"}}, 
    EXAMPLE { 
    "ZZ[a,b,c][x]; f = a*x^2+b*x+c",
    "affineDiscriminant f",
    "ZZ[a,b,c,d][x]; f = a*x^3+b*x^2+c*x+d",
    "affineDiscriminant f",
    },
    SeeAlso => {discriminant,affineResultant} 
}
document { 
    Key => {genericPolynomials,(genericPolynomials,VisibleList,Ring),(genericPolynomials,List)}, 
    Headline => "generic homogeneous polynomials", 
    Usage => "genericPolynomials(d,K) 
              genericPolynomials d",
    Inputs => { "d" => List => {TT"n+1"," integers ",TT"d_0,...,d_n"},
                "K" => Ring => {"optional, with default value ",TO QQ}}, 
    Outputs => { {TT"n+1"," generic homogeneous polynomials of degrees ",TT "d_0,...,d_n", " in the ring ",TT "K[a_0,a_1,...,b_0,b_1,...][x_0,...,x_n]"}},
    PARA{"This is an auxiliary method to build tests and examples. For instance, the two following codes have to produce the same polynomial up to a renaming of variables: 1) ",TT "resultant genericPolynomials((n+1):d,K)"," and 2) ",TT "fromPluckerToStiefel dualize chowForm veronese(n,d,K)","."},
    EXAMPLE {
         "genericPolynomials {1,2,3}",
         "first genericPolynomials({4,2,3},ZZ/101)",
         "first genericPolynomials({4,-1,-1},ZZ/101)"
    },
    SeeAlso => {veronese}
}
document { 
    Key => {macaulayFormula,(macaulayFormula,Matrix),(macaulayFormula,List)}, 
    Headline => "Macaulay formula for the resultant", 
    Usage => "macaulayFormula F", 
    Inputs => { "F" => Matrix => {"a row matrix whose entries are ", TEX///$n+1$///," homogeneous polynomials ", TEX///$F_0,\ldots,F_n$///," in ", TEX///$n+1$///," variables (or a ", TO List," to be interpreted as such a matrix)"}}, 
    Outputs => {{"a pair of two square matrices ",TEX///$(D,D')$///, " such that the ratio of their determinants ",TEX///$det(D)/det(D')$///," is the resultant of ",TEX///$F_0,\ldots,F_n$///}},
    PARA{"This formula is stated in Theorem 4.9 of ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"},", by David A. Cox, John Little, Donal O'shea."},
    EXAMPLE {
       "F = genericPolynomials {2,2,3}",
       "time (D,D') = macaulayFormula F",
       "F = {random(2,Grass(0,2)),random(2,Grass(0,2)),random(3,Grass(0,2))}",
       "time (D,D') = macaulayFormula F",
       "assert(det D == (resultant F) * (det D'))" 
    },
    SeeAlso => {resultant}
}
document { 
    Key => {veronese,(veronese,ZZ,ZZ,Ring),(veronese,ZZ,ZZ)}, 
    Headline => "Veronese embedding", 
    Usage => "veronese(n,d,K) 
              veronese(n,d)",
    Inputs => { "n" => ZZ,
                "d" => ZZ,
                "K" => Ring => {"optional, with default value ",TO QQ}}, 
    Outputs => { {"representing the ",TEX///$d$///,"-th Veronese embedding of ",TEX///$\mathbb{P}^n$///}},
    PARA{"This is an auxiliary method to build tests and examples. For instance, the two following codes have to produce the same polynomial up to a renaming of variables: 1) ",TT "resultant genericPolynomials((n+1):d,K)"," and 2) ",TT "fromPluckerToStiefel dualize chowForm veronese(n,d,K)","."},
    EXAMPLE {
         "veronese(1,4)",
         "veronese(1,4,Variable=>y)",
         "veronese(1,4,Variable=>(u,z))",
         "veronese(2,2,ZZ/101)"
    },
    SeeAlso => {genericPolynomials}
}
document { 
    Key => {tangentialChowForm,(tangentialChowForm,Ideal,ZZ)}, 
    Headline => "higher Chow forms of a projective variety", 
    Usage => "tangentialChowForm(I,s)",
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///,", say of dimension ", TEX///$k$///},
                "s" => ZZ }, 
    Outputs => {{"or an ", TO "Ideal", " (if there is more than one generator) in the coordinate ring of the Grassmannian ",TEX///$\mathbb{G}(n-k-1+s,\mathbb{P}^n)$///," in the Plucker embedding, representing the higher associated subvariety ",TEX///$Z_s(X)$///}}, 
    PARA{"For a projective variety ",TEX///$X\subset\mathbb{P}^n$///," of dimension ",TEX///$k$///, ", the ",TEX///$s$///,"-th associated subvariety ",TEX///$Z_s(X)\subset\mathbb{G}(n-k-1+s,\mathbb{P}^n)$///, " (also called tangential Chow form) is defined to be the closure of the set of ", TEX///$(n-k-1+s)$///, "-dimensional subspaces ",TEX///$L\subset \mathbb{P}^n$///, " such that ", TEX///$L\cap X\neq\emptyset$///, " and ",TEX///$dim(L\cap T_x(X))\geq s$///, " for some smooth point ",TEX///$x\in L\cap X$///, ", where ",TEX///$T_x(X)$///, " denotes the embedded tangent space to ", TEX///$X$///, " at ",TEX///$x$///,". In particular, ", TEX///$Z_0(X)\subset\mathbb{G}(n-k-1,\mathbb{P}^n)$///, " is defined by the Chow form of ", TEX///$X$///, ", while ",TEX///$Z_k(X)\subset\mathbb{G}(n-1,\mathbb{P}^n)$///, " is identified to the dual variety ", TEX///$X^{*}\subset\mathbb{P}^n^{*}=\mathbb{G}(0,\mathbb{P}^n^{*})$///, " via the duality of Grassmannians ", TEX///$\mathbb{G}(0,\mathbb{P}^n^{*})=\mathbb{G}(n-1,\mathbb{P}^n)$///,". For details we refer to the third chapter of ", HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky. "},
    PARA{"The algorithm used are standard, based on projections of suitable incidence varieties. Here are some of the options available that could speed up the computation."},    
    PARA{TT "Duality"," Taking into account the duality of Grassmannians, one can perform the computation in ", TEX///$\mathbb{G}(k-s,n)$///, " and then passing to ", TEX///$\mathbb{G}(n-k-1+s,n)$///, ". This is done by default when it seems advantageous."},   
    PARA{TT "AffineChartGrass", " If one of the standard coordinate charts on the Grassmannian is specified, then the internal computation is done on that chart. By default, a random chart is used. Set this to ",TT"false"," to not use any chart."},   
    PARA{TT "AffineChartProj", " This is quite similar to ",TT "AffineChartGrass",", but it allows to specify one of the standard coordinate charts on the projective space. You should set this to ",TT"false"," for working with reducible or degenerate varieties."},   
    PARA{TT "AssumeOrdinary", " Set this to ",TT"true"," if you know that ",TEX///$Z_s(X)$///," is a hypersurface (by default is already ",TT"true"," if ",TEX///$s=0$///,")."},
    EXAMPLE { 
          "-- cubic rational normal scroll surface in P^4=G(0,4)
use Grass(0,4,Variable=>p); S = minors(2,matrix{{p_0,p_2,p_3},{p_1,p_3,p_4}})",
          "-- 0-th associated hypersurface of S in G(1,4) (Chow form)
time tangentialChowForm(S,0)",
          "-- 1-th associated hypersurface of S in G(2,4)
time tangentialChowForm(S,1)",
          "-- 2-th associated hypersurface of S in G(3,4) (parameterizing tangent hyperplanes to S)
time tangentialChowForm(S,2)",
          "-- we get the dual hypersurface of S in G(0,4) by dualizing
time S' = ideal dualize tangentialChowForm(S,2)",
          "-- we then can recover S
time assert(dualize tangentialChowForm(S',3) == S)"
            },
    SeeAlso => {isCoisotropic,chowForm}
}
undocumented {(tangentialChowForm,Ideal,ZZ,ZZ)}
document { 
    Key => {chowForm,(chowForm,Ideal),(chowForm,RingMap)}, 
    Headline => "Chow form of a projective variety", 
    Usage => "chowForm I", 
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///}}, 
    Outputs => {{"the Chow form of ",TEX///$X$///," in the coordinate ring of the Grassmannian ",TEX///$\mathbb{G}(n-dim(X)-1,\mathbb{P}^n)$///," in the Plucker embedding"}},
    PARA{"This is the same as ",TT "tangentialChowForm(I,0)", ", see ",TO "tangentialChowForm",". Below, we compute the Chow form of the Veronese surface and then we compare it with the resultant of three ternary quadrics."},
    EXAMPLE {
         "-- Veronese surface in P^5
f = veronese(2,2,ZZ/3331); V = kernel f",
         "-- Chow form of V in Grass(2,5) (performing internal computations on an affine chart of the Grassmannian)
time ChowV = chowForm(V,AffineChartGrass=>{1,2,3})",
         "-- equivalently (but faster)...
time assert(ChowV === chowForm f)",
         "-- X-resultant of V
time Xres = fromPluckerToStiefel dualize ChowV;",
         "-- three generic ternary quadrics
F = genericPolynomials({2,2,2},ZZ/3331)",
         "-- resultant of the three forms
time resF = resultant F;",
         "assert(resF === sub(Xres,vars ring resF) and Xres === sub(resF,vars ring Xres))"
    },
    SeeAlso => {tangentialChowForm,hurwitzForm}
}
document { 
    Key => {hurwitzForm,(hurwitzForm,Ideal)}, 
    Headline => "Hurwitz form of a projective variety", 
    Usage => "hurwitzForm I", 
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a non-linear projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///}}, 
    Outputs => {{"the Hurwitz form of ",TEX///$X$///," in the coordinate ring of the Grassmannian ",TEX///$\mathbb{G}(n-dim(X),\mathbb{P}^n)$///," in the Plucker embedding"}},
    PARA{"This is the same as ",TT "tangentialChowForm(I,1)", ", see ",TO "tangentialChowForm","."},
    EXAMPLE {
         "Q = ideal random(2,Grass(0,4))",
         "time hurwitzForm Q"
    },
    SeeAlso => {tangentialChowForm,chowForm}
}
undocumented {(chowEquations,RingElement,ZZ)};
document { 
    Key => {chowEquations,(chowEquations,RingElement)}, 
    Headline => "Chow equations of a projective variety", 
    Usage => "chowEquations W", 
    Inputs => { "W" => RingElement => {"the Chow form of an irreducible projective variety ",TEX///$X\subset\mathbb{P}^n$///,}}, 
    Outputs => { {"generated by the Chow equations of ",TEX///$X$///}},
    PARA{"Given the Chow form ",TEX///$Z_0(X)\subset\mathbb{G}(n-k-1,n)$///," of an irreducible projective ",TEX///$k$///,"-dimensional variety ", TEX///$X\subset\mathbb{P}^n$///, ", one can recover a canonical system of equations, called Chow equations, that always define ",TEX///$X$///, " set-theoretically, and also scheme-theoretically whenever ",TEX///$X$///, " is smooth. For details, see chapter 3, section 2C of ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky."},
    EXAMPLE { 
          "P3 = Grass(0,3,ZZ/11,Variable=>x)",
          "-- an elliptic quartic curve
C = ideal(x_0^2+x_1^2+x_2^2+x_3^2,x_0*x_1+x_1*x_2+x_2*x_3)",
          "-- Chow equations of C
time eqsC = chowEquations chowForm C",
          "C == saturate eqsC",
          "-- a singular irreducible curve 
D = ideal(x_1^2-x_0*x_2,x_2^3-x_0*x_1*x_3,x_1*x_2^2-x_0^2*x_3)",
          "-- Chow equations of D
time eqsD = chowEquations chowForm D",
          "D == saturate eqsD",
          "D == radical eqsD"
    },
    PARA{"Actually, one can use ", TT "chowEquations", " to recover a variety ",TEX///$X$///," from some other of its tangential Chow forms as well. This is based on generalizations of the \"Cayley trick\", see ",HREF{"http://link.springer.com/article/10.1007/BF02567693","Multiplicative properties of projectively dual varieties"},", by J. Weyman and A. Zelevinsky; see also the preprint ", HREF{"https://arxiv.org/abs/1607.05932","Coisotropic Hypersurfaces in the Grassmannian"}, ", by K. Kohn. For instance, "},
    EXAMPLE { 
          " Q = ideal(x_0*x_1+x_2*x_3)",
          "-- tangential Chow forms of Q
time (W0,W1,W2) = (tangentialChowForm(Q,0),tangentialChowForm(Q,1),tangentialChowForm(Q,2))",
          "time (Q,Q,Q) == (chowEquations(W0,0),chowEquations(W1,1),chowEquations(W2,2))"
    },
    PARA{"Note that ",TT "chowEquations(W,0)", " is not the same as ",TT "chowEquations W","."}
}
undocumented {(cayleyTrick,Ideal)};
document { 
    Key => {cayleyTrick,(cayleyTrick,Ideal,ZZ)}, 
    Headline => "Cayley trick", 
    Usage => "cayleyTrick(I,k)", 
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///},
                "k" => ZZ => {"the dimension of ",TEX///$X$///,", or more generally an integer ",TEX///$k$///," not exceeding the dimension of ",TEX///$X$///," and not smaller than the dual defect of ",TEX///$X$///}}, 
    Outputs => {{"a pair ",TEX///$(J,J')$///, ", where ",TEX///$J$///, " is the defining ideal of the Segre product ", TEX///$X\times\mathbb{P}^k\subset\mathbb{P}^{(k+1)(n+1)-1}$///, ", while ", TEX///$J'$///, " is the principal ideal defining the dual hypersurface ", TEX///$(X\times\mathbb{P}^k)^{*}\subset{\mathbb{P^{(k+1)(n+1)-1}}}^{*}$///}},
    PARA{"Let ",TEX///$X\subset\mathbb{P}^n$///," be a ",TEX///$k$///,"-dimensional projective variety. Consider the product ", TEX///$W = X\times\mathbb{P}^k$///," as a subvariety of ",TEX///$\mathbb{P}(Mat(k+1,n+1))$///,", the projectivization of the space of ",TEX///$(k+1)\times (n+1)$///, "-matrices, and consider the projection ",TEX///$p:\mathbb{P}(Mat(k+1,n+1))--->\mathbb{G}(k,n)=\mathbb{G}(n-k-1,n)$///,". Then the \"Cayley trick\" states that the dual variety ",TEX///$W^*$///," of ", TEX///$W$///," equals the closure of ",TEX///$p^{-1}(Z_0(X))$///, ", where ", TEX///$Z_0(X)\subset\mathbb{G}(n-k-1,n)$///," is the Chow hypersurface of ",TEX///$X$///,". The defining form of ", TEX///$W^*$///," is also called the ", TEX///$X$///,"-resultant. For details and proof, see ",HREF{"http://link.springer.com/article/10.1007/BF02567693","Multiplicative properties of projectively dual varieties"},", by J. Weyman and A. Zelevinsky; see also the preprint ", HREF{"https://arxiv.org/abs/1607.05932","Coisotropic Hypersurfaces in the Grassmannian"}, ", by K. Kohn."},
    PARA{"In the example below, we apply the method to the quadric ",TEX///$\mathbb{P}^1\times\mathbb{P}^1\subset\mathbb{P}^3$///,"."},
    EXAMPLE { 
          "QQ[x_0..x_3]; P1xP1 = ideal(x_0*x_1-x_2*x_3)",
          "time (P1xP1xP2,P1xP1xP2') = cayleyTrick(P1xP1,2);"
    },
    PARA{"In the next example, we calculate the defining ideal of ",TEX///$\mathbb{P}^1\times\mathbb{P}^1\times\mathbb{P}^1\subset\mathbb{P}^7$///," and that of its dual variety."},
    EXAMPLE { 
          "time (P1xP1xP1,P1xP1xP1') = cayleyTrick(P1xP1,1)",
            },
    PARA{"If the option ",TT "Duality"," is set to ",TT"true",", then the method applies the so-called \"dual Cayley trick\"."},
    EXAMPLE { 
          "time cayleyTrick(P1xP1,1,Duality=>true);",
          "assert(oo == (P1xP1xP1,P1xP1xP1'))",
          "time cayleyTrick(P1xP1,2,Duality=>true);",
          "assert(oo == (P1xP1xP2,P1xP1xP2'))"
            },
    SeeAlso => {dualVariety}
}
document { 
    Key => {dualize,(dualize,RingElement),(dualize,Matrix),(dualize,Ideal),(dualize,RingMap),(dualize,VisibleList),(dualize,Ring)}, 
    Headline => "apply duality of Grassmannians", 
    Usage => "dualize f", 
    Inputs => { "f" => RingElement => {" or a ",TO "Matrix", ", or an ",TO "Ideal",", in the coordinate ring (resp. ambient ring) of a Grassmannian ",TEX///$\mathbb{G}(k,n)$///}}, 
    Outputs => {{"the image of ", TT "f", " in the coordinate ring (resp. ambient ring) of ", TEX///$\mathbb{G}(n-k-1,n)$///," via the duality of Grassmannians"}},
    PARA{"This method implements the natural identification ", TEX///$\mathbb{G}(k,\mathbb{P}^n)\to\mathbb{G}(n-k-1,{\mathbb{P}^n}^{*})$///,", which takes a subspace ", TEX///$L\in\mathbb{G}(k,\mathbb{P}^n)$///, " to its orthogonal complement ",TEX///$L^*\in\mathbb{G}(n-k-1,{\mathbb{P}^n}^*)$///,"."},
    EXAMPLE { 
          "P9 = ambient Grass(2,4,ZZ/13,Variable=>x)",
          "vars P9",
          "dualize vars P9",
          "F  = random(2,P9)",
          "dualize F",
          "F == dualize dualize F",
    }
}
document { 
    Key => {fromPluckerToStiefel,(fromPluckerToStiefel,RingElement),(fromPluckerToStiefel,Matrix),(fromPluckerToStiefel,Ideal)}, 
    Headline => "convert from Plucker coordinates to Stiefel coordinates", 
    Usage => "fromPluckerToStiefel f", 
    Inputs => { "f" => RingElement => {" or a ",TO "Matrix", ", or an ",TO "Ideal",", in the coordinate ring of a Grassmannian ",TEX///$\mathbb{G}(k,n)$///}}, 
    Outputs => {{"the representation of ",TT"f"," in the Stiefel coordinates of ",TEX///$\mathbb{G}(k,n)$///," (or in the affine coordinates if an affine chart is given with ",TO"AffineChartGrass",")"}},
    PARA{"This method can be used to compute the ",TEX///$X$///,"-resultant of a projective variety. Here, we compute the ",TEX///$X$///,"-resultant of the twisted cubic curve."},
    EXAMPLE { 
          "C = kernel veronese(1,3)",
          "time fromPluckerToStiefel dualize chowForm C",
          "time fromPluckerToStiefel(dualize chowForm C,AffineChartGrass=>{0,1})",
          "fromPluckerToStiefel(dualize chowForm C,AffineChartGrass=>{2,3},Variable=>a)"
    },
    PARA{"As another application, we check that the singular locus of the Chow form of the twisted cubic has dimension 2 (on each standard chart)."},
    EXAMPLE { 
          "w = chowForm C;",
          "time U = apply(subsets(4,2),s->ideal fromPluckerToStiefel(w,AffineChartGrass=>s))",
          "time apply(U,u->dim singularLocus u)"
    }
}
undocumented {(Grass,Ring)};
document { 
    Key => {Grass,(Grass,ZZ,ZZ),(Grass,ZZ,ZZ,Ring)}, 
    Headline => "coordinate ring of a Grassmannian", 
    Usage => "Grass(k,n)
              Grass(k,n,K)",
    Inputs => {"k" => ZZ ,
               "n" => ZZ ,
               "K" => Ring => {"optional with default value ",TO "QQ",", the coefficient ring to be used"}}, 
    Outputs => {{"the coordinate ring of the Grassmannian variety of all projective ",TEX///$k$///,"-planes in ",TEX///$\mathbb{P}^n$///}},
    PARA{"This method calls the method ", TO "Grassmannian", ", and ", TT "Grass(k,n,K,Variable=>p)", " can be considered equivalent to ", TT "quotient Grassmannian(k,n,Variable=>p,CoefficientRing=>K)", ". However, the method ",TT "Grass", " creates no more than an instance of ring for a given tuple ", TT "(k,n,K,p)","."}, 
    EXAMPLE { 
          "R = Grass(2,4,ZZ/11)",
          "R === Grass(2,4,ZZ/11)"
    },
    PARA{"In order to facilitate comparisons, the outputs of the methods ", TO "chowForm",", ",TO "hurwitzForm",", ",TO "tangentialChowForm",", ",TO "chowEquations",", and ",TO "dualize", " always lie in these rings."},
    EXAMPLE { 
          "L = trim ideal(random(1,Grass(0,3,ZZ/11,Variable=>x)),random(1,Grass(0,3,ZZ/11,Variable=>x)))",
          "w = chowForm L",
          "ring w === Grass(1,3,ZZ/11,Variable=>x)",
          "L' = chowEquations w",
          "ring L' === Grass(0,3,ZZ/11,Variable=>x)",
          "L''= chowEquations(w,Variable=>y)",
          "ring L'' === Grass(0,3,ZZ/11,Variable=>y)"
    }
}
document {
    Key => {[Grass,Variable],[tangentialChowForm,Variable],[chowForm,Variable],[chowEquations,Variable],[cayleyTrick,Variable],[fromPluckerToStiefel,Variable],[veronese,Variable],[hurwitzForm,Variable],[conormalVariety,Variable],[plucker,Variable]},
    Headline => "specify a name for a variable", 
    Usage => "Variable => x",
    PARA{"This is an option used to specify a symbol to be used as a name for the generator of the coordinate ring of the Grassmannian ",TO Grass,", or some other ring."},
    SeeAlso => {Grass}
} 
document {
    Key => {AssumeOrdinary, [tangentialChowForm,AssumeOrdinary],[dualVariety,AssumeOrdinary]},
    Headline => "whether the expected codimension is 1", 
    Usage => "AssumeOrdinary => true/false",
    "This is an option for ", TO "dual"," and ",TO "tangentialChowForm"," that controls the option ", TO "SubringLimit",", used internally with ", TO "kernel", " and ", TO "gb",". You can set this to ",TT"true"," when the expected output should represent a hypersurface.",
    SeeAlso => {dualVariety,tangentialChowForm}
} 
document {
    Key => {Duality, [tangentialChowForm,Duality],[chowForm,Duality],[cayleyTrick,Duality],[hurwitzForm,Duality],[isInCoisotropic,Duality]},
    Headline => "whether to use dual Plucker coordinates", 
    Usage => "Duality => true/false",
    "This is a option typically used to specify whether to perform internal computations using the dual Plucker coordinates.",
    SeeAlso => {chowForm,hurwitzForm,tangentialChowForm}
} 
document {
    Key => {AffineChartGrass, [tangentialChowForm,AffineChartGrass],[chowForm,AffineChartGrass], [isCoisotropic,AffineChartGrass],[fromPluckerToStiefel,AffineChartGrass],[hurwitzForm,AffineChartGrass],[plucker,AffineChartGrass]},
    Headline => "use an affine chart on the Grassmannian", 
    Usage => "AffineChartGrass => l", 
    PARA{"This is an optional input for methods that involve computations on Grassmannians. The argument ",TT "l"," can be a list of ",TEX///$k+1$///," distinct integers between 0 and ",TEX///$n$///,", corresponding to an affine chart ",TEX///$U$///," on the Grassmannian ",TEX///$\mathbb{G}(k,n)$///,". This indicates to the method that should perform internal computations using the chart ",TEX///$U$///,". Set this to ", TT "false"," (resp. ",TT "true",") to not use any chart (resp. to use a random chart)."},
    SeeAlso => {chowForm,fromPluckerToStiefel,hurwitzForm,isCoisotropic,tangentialChowForm}
} 
document {
    Key => {AffineChartProj, [tangentialChowForm,AffineChartProj],[chowForm,AffineChartProj],[hurwitzForm,AffineChartProj]},
    Headline => "use an affine chart on the projective space", 
    Usage => "AffineChartProj => l",
    "This is an optional input for some methods, and it works quite similar to ",TO "AffineChartGrass",". The argument ",TT "l"," can be an integer between 0 and ",TEX///$n$///,", corresponding to an affine chart ",TEX///$U$///," on the projective space ",TEX///$\mathbb{P}^n$///,". This indicates to the method that should perform internal computations using the chart ",TEX///$U$///,".",
    SeeAlso => {chowForm,hurwitzForm,tangentialChowForm}
} 
document { 
    Key => {dualVariety,(dualVariety,Ideal),(dualVariety,RingMap)}, 
    Headline => "projective dual variety", 
    Usage => "dualVariety I", 
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///}}, 
    Outputs => {{"the ideal of the projective dual variety ",TEX///$X^{*}\subset{\mathbb{P}^n}^{*}$///}},
    PARA{"This can be considered a shortcut for ",TT "dualize tangentialChowForm(I,dim I -1)","."}, 
    PARA{"Note that in characteristic 0 (or sufficiently large characteristic), the reflexivity theorem implies that if ",TT"I' == dualVariety I"," then ", TT"dualVariety I' == I",". Below, we verify the reflexivity theorem for the Veronese surface."},
    EXAMPLE { 
      "V = kernel veronese(2,2)",
      "time V' = dualVariety V",
      "time V == dualVariety V'"
    },
    PARA{"In the next example, we verify that the discriminant of a generic ternary cubic form coincides with the dual variety of the 3-th Veronese embedding of the plane, which is a hypersurface of degree 12 in ",TEX///$\mathbb{P}^9$///},
    EXAMPLE {
      "F = first genericPolynomials({3,-1,-1},ZZ/3331)",
      "time discF = ideal discriminant F;",
      "time Z = dualVariety(veronese(2,3,ZZ/3331),AssumeOrdinary=>true);",
      "discF == sub(Z,vars ring discF) and Z == sub(discF,vars ring Z)"
    },
   SeeAlso => {conormalVariety,discriminant}
}
document {
    Key => {[conormalVariety,Strategy],[dualVariety,Strategy]},
    PARA{"This is an option for ",TO conormalVariety," and ",TO dualVariety," which specifies which algorithm to use. One can choose between two strategies: \"Saturate\" and \"Eliminate\"."}
} 
undocumented {(conormalVariety,Ideal,Matrix)}
document { 
    Key => {conormalVariety,(conormalVariety,Ideal)}, 
    Headline => "conormal variety", 
    Usage => "conormalVariety I", 
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///}}, 
    Outputs => {{"the bihomogeneous ideal of the conormal variety ",TEX///$Con(X)\subset\mathbb{P}^n\times{\mathbb{P}^n}^{*}$///," of ",TEX///$X$///}},
    PARA{"The conormal variety ",TEX///$Con(X)$///," of a projective variety ",TEX///$X\subset\mathbb{P}^n$///," is the Zariski closure in ",TEX///$\mathbb{P}^n\times{\mathbb{P}^n}^{*}$///," of the set of tuples ",TEX///$(x,H)$///," where ",TEX///$x$///," is a regular point of ",TEX///$X$///," and ",TEX///$H$///," is a hyperplane in ",TEX///$\mathbb{P}^n$///," containing the embedded tangent space to ",TEX///$X$///," at ",TEX///$x$///,". The dual variety of ",TEX///$X$///," is the image of ",TEX///$Con(X)\subset\mathbb{P}^n\times{\mathbb{P}^n}^{*}$///," under projection onto the second factor ",TEX///${\mathbb{P}^n}^{*}$///,"."}, 
    EXAMPLE { 
      "X = kernel veronese(1,3)",
      "conormalVariety X"
    },
   SeeAlso => {dualVariety}
}
document { 
    Key => {isCoisotropic,(isCoisotropic,RingElement)}, 
    Headline => "whether a hypersurface of a Grassmannian is a tangential Chow form", 
    Usage => "isCoisotropic w",
    Inputs => { "w" => RingElement => {" representing a hypersurface of a Grassmannian"}}, 
    Outputs => { {"whether ",TT"w", " is a tangential Chow form of some projective variety"}},
    PARA{"The algorithm implemented is based on Proposition 3.12 in Chapter 4 of ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky."},
    EXAMPLE { 
          "-- first tangential Chow form of a random quadric in P^3
w = tangentialChowForm(ideal random(2,Grass(0,3)),1)",
          "time isCoisotropic w",
          "-- random quadric in G(1,3)
w' = random(2,Grass(1,3))",
          "time isCoisotropic w'"
    }
}
document { 
    Key => {isInCoisotropic,(isInCoisotropic,Ideal,Ideal)},
    Headline => "test membership in a coisotropic hypersurface", 
    Usage => "isInCoisotropic(L,I)", 
    Inputs => {"L" => Ideal => {"the ideal of a ",TEX///$k$///,"-dimensional linear subspace ",TEX///$V(L)\subset\mathbb{P}^n$///,", which corresponds to a point ",TEX///$p_L\in\mathbb{G}(k,\mathbb{P}^n)$///},
               "I" => Ideal => {"the ideal of a ",TEX///$d$///,"-dimensional projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///}},
    Outputs => {Boolean => {"whether ",TEX///$p_L$///," belongs to the ",TEX///$s$///,"-th associated subvariety ",TEX///$Z_s(X)\subset\mathbb{G}(k,\mathbb{P}^n)$///," with ",TEX///$s = k+d+1-n$///}},
    PARA{"This is equivalent to ",TT "isSubset(ideal tangentialChowForm(I,s),plucker L)",", but it does not compute the tangential Chow form."},
    EXAMPLE {
         "use Grass(0,5,ZZ/33331,Variable=>x)",
         "I = minors(2,matrix {{x_0,x_1,x_3,x_4},{x_1,x_2,x_4,x_5}}) -- rational normal scroll surface",
         "L = ideal(x_1-12385*x_2-16397*x_3-7761*x_4+827*x_5,x_0+2162*x_2-8686*x_3+2380*x_4+9482*x_5) -- linear 3-dimensional subspace",
         "time isInCoisotropic(L,I) -- whether L belongs to Z_1(V(I))"
    },
    SeeAlso => {tangentialChowForm,plucker}
}
document { 
    Key => {plucker,(plucker,Ideal),(plucker,Ideal,ZZ)}, 
    Headline => "get the Plucker coordinates of a linear subspace", 
    Usage => "plucker L
              plucker p", 
    Inputs => {"L" => Ideal => {"the ideal of a ",TEX///$k$///,"-dimensional linear subspace ",TEX///$V(L)\subset\mathbb{P}^n$///},
               "p" => Ideal => {"the ideal of the corresponding point of ",TEX///V(L)///," in the Grassmannian ",TEX///$\mathbb{G}(k,\mathbb{P}^n)$///," (embedded via the Plucker embedding)"}}, 
    Outputs => {{TT"p"," if the input is ",TT"L",", and ",TT"L"," if the input is ",TT"p"}},
    EXAMPLE {
         "P4 = Grass(0,4,ZZ/33331,Variable=>x); G'1'4 = Grass(1,4,ZZ/33331,Variable=>x);",
         "L = trim ideal apply(3,i->random(1,P4))  -- a line in P^4",
         "time p = plucker L",
         "time L' = plucker p",
         "assert(L' == L)" 
    },
    PARA{"More generally, if the input is the ideal of a subvariety ",TEX///$Y\subset\mathbb{G}(k,\mathbb{P}^n)$///,", then the method returns the ideal of the variety ",TEX///$W\subset\mathbb{P}^n$///," swept out by the linear spaces corresponding to points of ",TEX///$Y$///,". As an example, we now compute a surface scroll ",TEX///$W\subset\mathbb{P}^4$///," over an elliptic curve ",TEX///$Y\subset\mathbb{G}(1,\mathbb{P}^4)$///,"."},
    EXAMPLE {
         "Y = ideal apply(5,i->random(1,G'1'4)); -- an elliptic curve",
         "time W = plucker Y; -- surface swept out by the lines of Y",
         "(codim W,degree W)" 
    },
    PARA{"In this example, we can recover the subvariety ",TEX///$Y\subset\mathbb{G}(k,\mathbb{P}^n)$///," by computing the Fano variety of ",TEX///$k$///,"-planes contained in ",TEX///$W$///,"."},
    EXAMPLE {
         "time Y' = plucker(W,1); -- variety of lines contained in W",
         "assert(Y' == Y)"
    },
    PARA{EM "Warning",": Notice that, by default, the computation is done on a randomly chosen affine chart on the Grassmannian. To change this behavior, you can use the ",TO "AffineChartGrass"," option."}
}
undocumented {(plucker,Matrix)}
document {
    Key => {SingularLocus,[tangentialChowForm,SingularLocus],[hurwitzForm,SingularLocus],[conormalVariety,SingularLocus],[dualVariety,SingularLocus],[isInCoisotropic,SingularLocus]},
    Headline => "pass the singular locus of the variety", 
    Usage => "SingularLocus => X", 
    PARA{"Some methods, like ",TO tangentialChowForm," and ",TO conormalVariety,", must calculate the singular locus of the input variety, which can be hard to obtain. Sometimes one knows this locus and this option allows to inform Macaulay2."}
}

----------------------------------------------------------------------------------
------------------------------- Tests --------------------------------------------
----------------------------------------------------------------------------------

TEST /// -- Resultants vs X-resultants
testAB = (n,d,K) -> (
   time A = resultant genericPolynomials((n+1):d,K);
   time B = fromPluckerToStiefel dualize chowForm veronese(n,d,K);
   A === sub(B,vars ring A) and B === sub(A,vars ring B)
);
testAB' = (n,d,K) -> (
   time A = resultant genericPolynomials((n+1):d,K);
   time B = fromPluckerToStiefel dualize chowForm kernel veronese(n,d,K);
   A === sub(B,vars ring A) and B === sub(A,vars ring B)
);
assert testAB(1,2,QQ)
assert testAB(1,3,ZZ/3331)
assert testAB(1,4,ZZ/101)
assert testAB(2,2,ZZ/190181)
assert testAB'(1,2,QQ)
assert testAB'(1,3,ZZ/3331)
assert testAB'(1,4,ZZ/101)
-- assert testAB'(2,2,ZZ/190181)
///

TEST /// 
genericResultant = {Algorithm => "Poisson"} >> o -> (d,K) -> ( 
    F:=matrix{genericPolynomials(d,K)};
    resultant(F,Algorithm=>o.Algorithm)
);
a:=local a; ring112=ZZ[a_(0,0),a_(0,1),a_(0,2),a_(1,0),a_(1,1),a_(1,2),a_(2,0),a_(2,1),a_(2,2),a_(2,3),a_(2,4),a_(2,5)];
res112=a_(0,0)^2*a_(1,1)^2*a_(2,5)-a_(0,0)^2*a_(1,1)*a_(1,2)*a_(2,4)+a_(0,0)^2*a_(1,2)^2*a_(2,3)-2*a_(0,0)*a_(0,1)*a_(1,0)*a_(1,1)*a_(2,5)+a_(0,0)*a_(0,1)*a_(1,0)*a_(1,2)*a_(2,4)+a_(0,0)*a_(0,1)*a_(1,1)*a_(1,2)*a_(2,2)-a_(0,0)*a_(0,1)*a_(1,2)^2*a_(2,1)+a_(0,1)^2*a_(1,0)^2*a_(2,5)-a_(0,1)^2*a_(1,0)*a_(1,2)*a_(2,2)+a_(0,1)^2*a_(1,2)^2*a_(2,0)+a_(0,0)*a_(0,2)*a_(1,0)*a_(1,1)*a_(2,4)-a_(0,0)*a_(0,2)*a_(1,1)^2*a_(2,2)-2*a_(0,0)*a_(0,2)*a_(1,0)*a_(1,2)*a_(2,3)+a_(0,0)*a_(0,2)*a_(1,1)*a_(1,2)*a_(2,1)-a_(0,1)*a_(0,2)*a_(1,0)^2*a_(2,4)+a_(0,1)*a_(0,2)*a_(1,0)*a_(1,1)*a_(2,2)+a_(0,1)*a_(0,2)*a_(1,0)*a_(1,2)*a_(2,1)-2*a_(0,1)*a_(0,2)*a_(1,1)*a_(1,2)*a_(2,0)+a_(0,2)^2*a_(1,0)^2*a_(2,3)-a_(0,2)^2*a_(1,0)*a_(1,1)*a_(2,1)+a_(0,2)^2*a_(1,1)^2*a_(2,0);
assert(sub(genericResultant((1,1,2),ZZ,Algorithm=>"Poisson"),vars ring112) == res112);
ResM=genericResultant((1,3,1),ZZ,Algorithm=>"Macaulay"); ResP=genericResultant((1,3,1),ZZ,Algorithm=>"Poisson"); ResP=sub(ResP,vars ring ResM);
assert(ResM == ResP)
ResM=genericResultant((2,1,2),ZZ,Algorithm=>"Macaulay"); ResP=genericResultant((2,1,2),ZZ,Algorithm=>"Poisson"); ResP=sub(ResP,vars ring ResM);
assert(ResM == ResP)
///

TEST ///
invariance = (d,K,alg) -> (
    n:=#d -1; x:=local x; R:=K[x_0..x_n];
    F:=matrix({for i to n list sub(random(d_i,ZZ[x_0..x_n]),R)});
    A:=matrix for i to n list for j to n list sub(random(-10,10),K); 
    Sub:=map(R,R,transpose(A*(transpose vars R)));
    F':=Sub(F);
    (resultant(F',Algorithm=>alg)) == (det A)^(product toList d) * (resultant(F,Algorithm=>alg))
);
assert(invariance((4,6),ZZ,"Poisson")) 
assert(invariance((1,2,3),ZZ,"Poisson")) 
assert(invariance((2,3,5),QQ,"Poisson")) 
assert(invariance((1,1,2),ZZ[z],"Poisson")) 
assert(invariance((3,3,1,1,2),ZZ/331,"Poisson"))
assert(invariance((4,6),ZZ,"Macaulay")) 
assert(invariance((1,2,3),ZZ,"Macaulay")) 
assert(invariance((2,3,5),QQ,"Macaulay")) 
assert(invariance((1,1,2),ZZ[z],"Macaulay")) 
assert(invariance((3,3,1,1,2),ZZ/331,"Macaulay"))
/// 
    
TEST ///
PoissonVsMacaulay = (d,K) -> (
    n:=#d -1; x:=local x; R:=K[x_0..x_n];
    F:=matrix({for i to n list try random(d_i,R) else sub(random(d_i,ZZ[x_0..x_n]),R)});
    poiRes:=resultant(F,Algorithm=>"Poisson");
    macRes:=resultant(F,Algorithm=>"Macaulay");
    poiRes == macRes
);
assert(PoissonVsMacaulay((4,6),ZZ)) 
assert(PoissonVsMacaulay((1,2,3),ZZ)) 
assert(PoissonVsMacaulay((2,3,5),QQ)) 
assert(PoissonVsMacaulay((1,1,2),ZZ[z])) 
assert(PoissonVsMacaulay((3,3,1,1,2),ZZ/331))
assert(PoissonVsMacaulay((3,2,2,2),ZZ/33331))
/// 

TEST /// 
genericDiscriminant = {Algorithm => "Poisson"} >> o -> (d,n,K) -> ( 
    F:=first genericPolynomials(prepend(d,toList(n:(-1))),K);
    discriminant(F,Algorithm=>o.Algorithm)
);
compareDiscriminants = (d,n,K) -> (
    D1:=genericDiscriminant(d,n,K,Algorithm=>"Poisson");
    D1':=sub(genericDiscriminant(d,n,K,Algorithm=>"Macaulay"),vars ring D1);
    D2:=sub((dualVariety kernel veronese(n,d,K))_0,vars ring D1);
    ideal(D2) == ideal(D1) and ideal(D1) == ideal(D1')
);
assert compareDiscriminants(2,1,QQ) 
assert compareDiscriminants(4,1,QQ) 
assert compareDiscriminants(5,1,ZZ/33331) 
assert compareDiscriminants(2,2,ZZ/101)
assert compareDiscriminants(3,1,GF 5^5) 
/// 

TEST ///
randomPols = (K,m,e,d) -> ( -- homogeneous polynomials in K[a_0..a_m][x_0..x_n]
  n:=#d-1; assert(#d==#e);
  a:=local a; x:=local x;
  R:=K[a_0..a_m,x_0..x_n,Degrees=>{(m+1):{1,0},(n+1):{0,1}}];   
  R':=K[a_0..a_m][x_0..x_n];
  toList apply(0..n,i->sub(random({e_i,d_i},R),R'))
)
checkInterpolate = (K,m,e,d) -> (
   F:=randomPols(K,m,e,d);
   <<"F="<<F<<endl;
   time R1 := resultant(F,Algorithm=>"Poisson2");
   time R2 := resultant(F,Algorithm=>"Macaulay2");
   time R3 := resultant(F,Algorithm=>"Poisson");
   time R4 := resultant(F,Algorithm=>"Macaulay");
   <<"Res="<<R1<<endl;
   assert(R1 == R2 and R1 == R3 and R1 == R4);
);
checkInterpolate(QQ,1,{1,1},{1,2});
checkInterpolate(GF(331^2),1,{1,1},{1,2});
checkInterpolate(QQ,1,{1,3},{1,2});
checkInterpolate(QQ,1,{1,2},{3,2})
checkInterpolate(ZZ/3331,1,{2,3},{3,4});
checkInterpolate(ZZ,2,{0,1},{3,2});
checkInterpolate(ZZ/3331,1,{2,0},{2,2});
checkInterpolate(ZZ,1,{1,3,0},{1,2,2});
checkInterpolate(ZZ,0,{1,3,0},{1,2,2});
checkInterpolate(ZZ,1,{1,0,0,2},{1,2,1,1});
G=first randomPols(ZZ,1,{1,0,0},{3,0,0})
assert(discriminant(G,Algorithm=>"Poisson2") == discriminant(G,Algorithm=>"Macaulay2"))
///

TEST ///
pencil = (d,n,K) -> (
F:=random(d,Grass(0,n,K));
G:=random(d,Grass(0,n,K));
t:=local t;
x:=local x;
R:=(coefficientRing ring F)[t_0,t_1][x_0..x_(numgens ring F -1)];
G=t_0*sub(F,vars R)+t_1*sub(G,vars R);
time T1 = discriminant(G,Algorithm=>"Poisson2");
time T2 = discriminant(G,Algorithm=>"Macaulay2");
time T3 = discriminant G;
assert(T1 == T2 and T1 == T3)
);
pencil(2,2,QQ)
pencil(4,1,GF(101^3))
pencil(2,4,ZZ/331)
pencil(3,2,ZZ/331)
///

TEST ///
-- testing dualize
assert (dualize ideal Grass(1,4) == ideal Grass(2,4))
assert (dualize ideal Grass(0,4,ZZ/331) == ideal Grass(3,4,ZZ/331))
assert (dualize ideal Grass(3,5,Variable=>T) == ideal Grass(1,5,Variable=>T))
assert (dualize Grassmannian(2,6,CoefficientRing=>(ZZ/11),Variable=>r) == ideal Grass(3,6,ZZ/11,Variable=>r))
///

TEST ///
-- testing cayleyTrick
testCT = (U,k) -> (
  time (X,Y):=cayleyTrick(U,k);
  time (X',Y'):=cayleyTrick(U,k,Duality=>true);
  if not (X == X' and Y == Y') then return false;
  if numgens Y != 1 then return false;
  f:=map((ring Y)/Y,ring Y,transpose jacobian Y);
  f X == 0
);
P3=ZZ/331[x_0..x_3]
assert testCT(minors(2,matrix{{x_0..x_2},{x_1..x_3}}),1)
assert testCT(minors(2,matrix{{x_0..x_2},{x_1..x_3}}),0)
assert testCT(ideal random(1,P3),2)
assert testCT(ideal random(2,P3),2)
assert testCT(ideal random(2,P3),1)
assert testCT(ideal random(2,P3),0)
P2=QQ[t_0,t_1,t_2]
C=ideal(t_0^3+t_1^3+t_2^3)
(W,W')=cayleyTrick C;
assert(W' == dualVariety W) 
P4=ZZ/3331[t_0..t_4]
S = minors(2,matrix {{t_0,t_2,t_3},{t_1,t_3,t_4}})
-- assert testCT(S,2)
assert testCT(S,1)
assert testCT(S,0)
///

TEST ///
-- testing chowEquations 
P3=Grass(0,3,Variable=>y)
C=minors(2,matrix{{y_0..y_2},{y_1..y_3}})
assert(C == saturate chowEquations chowForm C)
///

TEST /// 
-- testing tangentialChowForm and dualVariety
checkOptions = (I,s) -> (
   test:=true;
   time Z:=tangentialChowForm(I,s);
   time test =          Z == tangentialChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>false,Duality=>false);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>false,Duality=>true);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>true, Duality=>false);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>true, Duality=>true);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>false,Duality=>false);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>false,Duality=>true);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>true, Duality=>false);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>true, Duality=>true);
   test
);
-- rational normal scroll surface S(1,2) subset P^4
P4 = Grass(0,4,ZZ/3331,Variable=>x)
G14= Grass(1,4,ZZ/3331,Variable=>x)
X=minors(2,matrix{{x_0,x_2,x_3},{x_1,x_3,x_4}})
Z=x_(1,3)^2*x_(2,3)-x_(1,2)*x_(1,3)*x_(2,4)-x_(0,3)*x_(1,3)*x_(2,4)+x_(0,2)*x_(1,4)*x_(2,4)+x_(1,2)^2*x_(3,4)+x_(0,3)^2*x_(3,4)-2*x_(0,1)*x_(2,3)*x_(3,4)-x_(0,2)*x_(0,4)*x_(3,4)
assert (Z == chowForm X)
assert checkOptions(X,0)
assert (X == dualVariety dualVariety X)
assert (X == dualVariety dualVariety(X,AssumeOrdinary=>true))
-- cone in P^4 over twisted cubic
X=minors(2,matrix{{x_1,x_2,x_3},{x_2,x_3,x_4}})
Z=x_(2,3)^3-x_(1,3)*x_(2,3)*x_(2,4)+x_(1,2)*x_(2,4)^2+x_(1,3)^2*x_(3,4)-2*x_(1,2)*x_(2,3)*x_(3,4)-x_(1,2)*x_(1,4)*x_(3,4)
assert (Z == chowForm X)
assert checkOptions(X,0)
assert (X == dualVariety dualVariety X)
-- quadratic Fermat hypersurfaces 
isQuadricFermat = (X) -> (R:=ring X; x:=gens R; n:=#x-1; ideal(sum for i to n list x_i^2) == ideal X);
use Grass(0,5,ZZ/3331,Variable=>p)
Q = ideal sum for i to 5 list p_i^2
assert isQuadricFermat tangentialChowForm(Q,0)
assert isQuadricFermat tangentialChowForm(Q,1)
assert isQuadricFermat tangentialChowForm(Q,2)
assert isQuadricFermat tangentialChowForm(Q,3)
assert isQuadricFermat tangentialChowForm(Q,4)
assert checkOptions(X,3)
assert checkOptions(X,4)
///

TEST ///
testGCT = (X) -> (
   -- checking: dualize Z_(dimX-s)(X) == Z_(s-defX)(X*)
   X':=dualVariety X; dimX:=dim X -1; defX:=codim X' -1; Z:=local Z; cond:=true;
   <<"dim X = "<<dimX<<", def X = "<<defX<<endl;
   for s from defX to dimX do (
      Z = (dualize tangentialChowForm(X,dimX-s),tangentialChowForm(X',s-defX));
      cond = cond and ideal(Z_0) == ideal(Z_1);
      <<"s="<<s<<", (Z_"<<dimX-s<<"(X))' == Z_"<<s-defX<<"(X*) : "<<cond<<endl 
   );
   cond
);
-- X=S(1,2) subset P^4
P4=ZZ/101[x_0..x_4];
X=minors(2,matrix{{x_0,x_2,x_3},{x_1,x_3,x_4}})
assert testGCT X
-- cone over twisted cubic
Y=minors(2,matrix{{x_1,x_2,x_3},{x_2,x_3,x_4}})
assert testGCT Y
-- hyperplane
assert testGCT ideal(random(1,P4))
-- quadric hypersurface 
assert testGCT ideal(random(2,P4))
///

TEST /// -- isInCoisotropic, plucker
X = ideal discriminant first genericPolynomials({5,-1},ZZ/33331);
a = gens ring X
L0 = ideal(a_4-14781*a_5,a_3-5288*a_5,a_2-6663*a_5,a_1-12692*a_5,a_0+10313*a_5)
assert isInCoisotropic(L0,X)
L1 = ideal(a_3+3444*a_4+3577*a_5,a_2-14948*a_4-12150*a_5,a_1+736*a_4-9767*a_5,a_0-510*a_4+11589*a_5)
assert isInCoisotropic(L1,X)
L2 = ideal(a_2+16333*a_3-11898*a_4+10734*a_5,a_1+13244*a_3-9161*a_4-3453*a_5,a_0+10968*a_3+4779*a_4+11380*a_5)
assert isInCoisotropic(L2,X)
L3 = ideal(a_1-13474*a_2-9875*a_3-3510*a_4-2182*a_5,a_0+13025*a_2+4558*a_3+14456*a_4+4223*a_5)
assert isInCoisotropic(L3,X)
L4 = ideal(a_0-7097*a_1+4268*a_2+7883*a_3-16233*a_4+13665*a_5)
assert isInCoisotropic(L4,X)
(dim L0-1,dim L1-1,dim L2-1,dim L3-1,dim L4-1)
--
x = gens Grass(0,5,ZZ/33331,Variable=>"x");
I = minors(2,matrix {{x_0, x_1, x_3, x_4}, {x_1, x_2, x_4, x_5}})
L0 = ideal(x_2+11868*x_3+8744*x_4+10580*x_5,x_1-12066*x_3-1923*x_4-4079*x_5,x_0-15158*x_3+9079*x_4-6322*x_5)
assert isInCoisotropic(L0,I,Duality=>true)
assert isInCoisotropic(L0,I,Duality=>false)
L1 = ideal(x_1-12385*x_2-16397*x_3-7761*x_4+827*x_5,x_0+2162*x_2-8686*x_3+2380*x_4+9482*x_5)
assert isInCoisotropic(L1,I,Duality=>true)
assert isInCoisotropic(L1,I,Duality=>false)
L2 = ideal(x_0+8023*x_1+10732*x_2+96*x_3-9744*x_4-11142*x_5)
assert isInCoisotropic(L2,I,Duality=>true)
assert isInCoisotropic(L2,I,Duality=>false)
--
R := Grass(0,6,ZZ/33331,Variable=>"aa")
L = ideal apply(3,i -> random(1,R))
(dim L -1,codim L)
assert last (time p = plucker L,time L' = plucker p,time p' = plucker L',L' == L and p' == p)
L = ideal apply(5,i -> random(1,R))
(dim L -1,codim L)
assert last (time p = plucker L,time L' = plucker p,time p' = plucker L',L' == L and p' == p)
///

end

